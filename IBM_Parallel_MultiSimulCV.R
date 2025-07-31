# ================================================================================== #
# 1. SETUP: PACKAGES, DATA, AND MODEL TERMS
# ================================================================================== #

# Packages ####
# Added future, future.apply, and progressr for parallel processing
packages <- c("caret", "doParallel", "dplyr", "devtools", "ggplot2", "mdatools", 
              "mgcv", "MuMIn", "purrr", "ranger", "stringr", "tidyr", "viridis", 
              "xgboost", "future", "future.apply", "progressr")
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  utils::install.packages(pkgs = packages[!installed_packages])
}
invisible(lapply(packages, library, character.only = TRUE))
rm(installed_packages, packages)

# Load dataframe ========================================================================
# Ensure the path is correct for your environment
ibm <- readRDS("RDS_dataframes/IBM_proc_filter.RDS") %>%
  select(-c(haul, date_collected, read_age, test_age, final_age, scan_name, timestamp, file_name, session_title, file_path))
ages <- read_xlsx("metadata/ibm_ages_07302025.xlsx") %>% 
  select(-c(avg_age, hatch_est, length, area, percent_affected, structure_weight)) %>%
  mutate(
    # Calculate the mean of age1, age2, and age3 for each row
    avg_age = rowMeans(select(., age1, age2, age3), na.rm = TRUE),
    # Calculate hatch estimate
    hatch_est = julian_date - avg_age
  ) %>% 
  # remove rows with NA for age1
  filter(!is.na(age1))

df <- left_join(
  ages, 
  ibm, 
  by = c("specimen" = "specimen")
)

# remove missing scan specimen, only use NEW scans
df <- df %>%
  filter(specimen != 425970, run_number == 2)
df <- df[complete.cases(df$avg_age), ]
rm(ages, ibm)

# Dredge to find top 5 models ============================================================
pca_temp <- mdatools::pca(df[, 30:ncol(df)])
pc_df <- data.frame(PC1 = rep(0, nrow(df)))
for (i in 1:10) {
  pc_df[, paste0("PC", i)] <- pca_temp$res$cal$scores[, i]
}
pc_df <- cbind(pc_df, df)

global_lm <- lm(data = pc_df, avg_age ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10)
global_gam <- gam(data = pc_df, avg_age ~ s(PC1,k = 4) + s(PC2,k = 4) + s(PC3,k = 4) + s(PC4,k = 4) + s(PC5,k = 4) + s(PC6,k = 4) + s(PC7, k = 4) + s(PC8, k = 4) + s(PC9, k = 4) + s(PC10, k = 4))

options(na.action = "na.fail")
dredge_lm <- dredge(global_lm)
top10_lm <- get.models(dredge_lm, subset = 1:10)
dredge_gam <- dredge(global_gam)
top10_gam <- get.models(dredge_gam, subset = 1:10)

terms_lm <- list()
terms_gam <- list()
for(i in 1:10){
  terms_lm[[i]] <- top10_lm[[i]]$terms
  terms_gam[[i]] <- top10_gam[[i]]$formula
}
rm(global_gam, global_lm, top10_gam, top10_lm, pc_df, pca_temp, dredge_gam, dredge_lm, i)


# Functions for splits and metrics =================================================
generate_multiple_splits <- function(data = df, n_splits) {
  all_splits <- list()
  for (split_id in 1:n_splits) {
    all_splits[[split_id]] <- caret::createFolds(data$avg_age, k = 10, list = TRUE, returnTrain = FALSE)
  }
  return(all_splits)
}
calculate_rpd <- function(observed, predicted) {
  sd_observed <- sd(observed)
  rmse <- sqrt(mean((observed - predicted)^2))
  rpd <- sd_observed / rmse
  return(rpd)
}
calculate_bias <- function(observed, predicted) {
  bias <- mean(predicted - observed)
  return(bias)
}


# Tuning (Your original tuning code) ==============================================
# NOTE: This section remains unchanged and runs serially before the main simulation.
# If you have already run this and have the `best_params_` objects, you can skip this part.
# ================================================================================= #
# ================================================================================= #
# Tuning  ##### 
# =============================================================== #
# ================================================================================= #
# ================================================================================= #
## XGB ####
# 1. Set up the parallel backend
cl <- makePSOCKcluster(parallel::detectCores() - 1)
registerDoParallel(cl)

# 2. Define  full hyperparameter grid
my_full_grid <- expand.grid(
  nrounds = c(100, 300, 500, 800, 1600, 2000),
  eta = c(0.01, 0.05, 0.1, 0.2, 0.3),
  max_depth = c(1, 2, 3, 4, 6, 8),
  min_child_weight = c(1, 2, 4, 6, 8, 10, 15, 20, 30), 
  colsample_bytree = c(0.2, 0.3, 0.4, 0.6, 0.8),
  gamma = c(0, 0.1, 1, 5), 
  subsample = c(0.2, 0.4, 0.6, 0.8, 1)
)

# randomly select 1000 rows from the full grid
set.seed(6)
my_random_grid <- my_full_grid[sample(1:nrow(my_full_grid), 100), ]

# 3. Define the training control for 10-fold CV
train_control <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 1,
  search = "grid", 
  allowParallel = TRUE
)

Sys.time() # show start time
xgb_tuned_model <- train(
  x = as.matrix(df[, 30:ncol(df)]),
  y = df$avg_age,
  method = "xgbTree",
  trControl = train_control,
  tuneGrid = my_random_grid,
  verbose = FALSE # Suppress output during tuning
) 
Sys.time() # show end time - 
stopCluster(cl) # stop parallel backend

((best_params_xgb <- xgb_tuned_model$bestTune)) # store best tuning params and print

# > ((best_params_xgb <- xgb_tuned_model$bestTune)) # store best tuning params and print
# nrounds max_depth eta gamma colsample_bytree
# 64     100         8 0.1     1              0.6
# min_child_weight subsample
# 64                4       0.4

# ==================================================================================== #
## Random Forest####
# ==================================================================================== #

# 1. Set up the parallel backend to speed up the process
cl <- makePSOCKcluster(parallel::detectCores() - 1)
registerDoParallel(cl)

# 2. Define the hyperparameter grid
tuning_grid_rf <- expand.grid(
  mtry = c(floor(sqrt(ncol(df[, 30:ncol(df)])) * 0.1),
           floor(sqrt(ncol(df[, 30:ncol(df)])) * 0.2),
           floor(ncol(df[, 30:ncol(df)]) / 3),
           floor(ncol(df[, 30:ncol(df)]) / 2)),
  min.node.size = c(1, 3, 5, 15, 25, 35),
  splitrule = "variance" # Required for regression with ranger in caret
)

# 3. Define the training control for 3 repeats of 10-fold CV
train_control <- trainControl(
  method = "repeatedcv",
  number = 10,       # 10 folds
  repeats = 1,       
  search = "grid",  
  allowParallel = TRUE
)

Sys.time()
rf_tuned_model <- train(
  x = df[, 30:ncol(df)],
  y = df$avg_age,
  method = "ranger",        # Use the ranger package for Random Forest
  trControl = train_control,
  tuneGrid = tuning_grid_rf,
  importance = 'permutation', # Calculate variable importance on the final model
  num.trees = 1000
)
Sys.time() # 1 minute to run the full 24 combination grid
stopCluster(cl)
(best_params_rf <- rf_tuned_model$bestTune)
# 
# > (best_params_rf <- rf_tuned_model$bestTune)
# mtry splitrule min.node.size
# 14  311  variance             3

# ====================================================================================#
# Your original model functions =====================================================
# run_lm_models, run_gam_models, etc. These are used inside the parallel function.
# ... (All your run_..._models and combine/extract functions are assumed to be here) ...
# ... I am including them below for a fully self-contained script ...

run_lm_models <- function(cal, test, terms_lm) {
  splits_results_lm <- data.frame()
  all_predictions <- list()
  for (i in 1:10) {
    calibrate <- cal[[i]]; testing <- test[[i]]
    fold_results <- data.frame(Fold = i, Model = paste0("Linear ", 1:10), R2 = numeric(10), RMSE = numeric(10), RPD = numeric(10), Bias = numeric(10), PercentRMSE = numeric(10))
    fold_preds <- list(specimen_number = testing$specimen, actual = testing$avg_age, model_preds = vector("list", 10))
    for (j in 1:10) {
      mod <- lm(data = calibrate, terms_lm[[j]]); preds <- predict(mod, newdata = testing)
      fold_results$RMSE[j] <- caret::RMSE(pred = preds, obs = testing[, "avg_age"])
      fold_results$PercentRMSE[j] <- fold_results$RMSE[j] / max(testing$avg_age) * 100
      RSS <- sum((testing$avg_age - preds)^2); TSS <- sum((testing$avg_age - mean(testing$avg_age))^2)
      fold_results$R2[j] <- 1 - (RSS / TSS)
      fold_results$RPD[j] <- calculate_rpd(testing$avg_age, preds)
      fold_results$Bias[j] <- calculate_bias(testing$avg_age, preds)
      fold_preds$model_preds[[j]] <- preds
    }
    splits_results_lm <- rbind(splits_results_lm, fold_results)
    all_predictions[[i]] <- fold_preds
  }
  splits_results_lm$ModelType <- "LM"
  return(list(results = splits_results_lm, predictions = all_predictions))
}
run_gam_models <- function(cal, test, terms_gam) {
  splits_results_gam <- data.frame()
  all_predictions <- list()
  for (i in 1:10) {
    calibrate <- cal[[i]]; testing <- test[[i]]
    fold_results <- data.frame(Fold = i, Model = paste0("GAM ", 1:10), R2 = numeric(10), RMSE = numeric(10), RPD = numeric(10), Bias = numeric(10), PercentRMSE = numeric(10))
    fold_preds <- list(specimen_number = testing$specimen, actual = testing$avg_age, model_preds = vector("list", 10))
    for (j in 1:10) {
      mod <- gam(data = calibrate, terms_gam[[j]], method = "REML"); preds <- predict(mod, newdata = testing)
      fold_results$RMSE[j] <- caret::RMSE(pred = preds, obs = testing[, "avg_age"])
      fold_results$PercentRMSE[j] <- fold_results$RMSE[j] / max(testing$avg_age) * 100
      RSS <- sum((testing$avg_age - preds)^2); TSS <- sum((testing$avg_age - mean(testing$avg_age))^2)
      fold_results$R2[j] <- 1 - (RSS / TSS)
      fold_results$RPD[j] <- calculate_rpd(testing$avg_age, preds)
      fold_results$Bias[j] <- calculate_bias(testing$avg_age, preds)
      fold_preds$model_preds[[j]] <- preds
    }
    splits_results_gam <- rbind(splits_results_gam, fold_results)
    all_predictions[[i]] <- fold_preds
  }
  splits_results_gam$ModelType <- "GAM"
  pc_counts_gam <- sapply(terms_gam, function(formula) { stringr::str_count(as.character(formula)[3], "PC\\d+") })
  components_map <- data.frame(Model = paste0("GAM ", 1:10), Components = pc_counts_gam)
  splits_results_gam <- merge(splits_results_gam, components_map, by = "Model", all.x = TRUE)
  return(list(results = splits_results_gam, predictions = all_predictions))
}
run_pls_models <- function(cal, test) {
  splits_results_pls <- data.frame()
  all_predictions <- list(); all_importance_fold <- list()
  for (i in 1:10) {
    calibrate <- cal[[i]]; testing <- test[[i]]
    fold_results <- data.frame(Fold = i, Model = c("PLS", "PLS - VIP"), R2 = numeric(2), RMSE = numeric(2), RPD = numeric(2), Bias = numeric(2), PercentRMSE = numeric(2), Components = numeric(2))
    fold_preds <- list(specimen_number = testing$specimen, actual = testing$avg_age, pls_pred = numeric(length(testing$avg_age)), vip_pred = numeric(length(testing$avg_age)))
    mod <- mdatools::pls(calibrate[, 40:ncol(calibrate)], calibrate[, "avg_age"], scale = F, center = T, x.test = testing[, 40:ncol(testing)], y.test = testing[, "avg_age"]); ncomp <- mod$ncomp.selected
    wavenumbers <- as.numeric(colnames(calibrate[, 40:ncol(calibrate)])); vip_scores <- vipscores(mod)
    importance_df <- data.frame(fold = i, method = "PLS-VIP", wavenumber = wavenumbers, importance = vip_scores)
    all_importance_fold[[i]] <- importance_df
    fold_results$R2[1] <- mod$testres$r2[[ncomp]]; fold_results$RMSE[1] <- mod$testres$rmse[[ncomp]]; fold_results$RPD[1] <- mod$testres$rpd[[ncomp]]; fold_results$Bias[1] <- mod$testres$bias[[ncomp]]
    fold_results$PercentRMSE[1] <- mod$testres$rmse[[ncomp]] / max(testing$avg_age) * 100
    fold_results$Components[1] <- ncomp; fold_preds$pls_pred <- mod$testres$y.pred[, ncomp, ]
    vip <- as.data.frame(vipscores(mod))
    mod <- mdatools::pls(calibrate[, 40:ncol(calibrate)], calibrate[, "avg_age"], scale = F, center = T, x.test = testing[, 40:ncol(testing)], y.test = testing[, "avg_age"], exclcols = vip$V1 < 0.5); ncomp <- mod$ncomp.selected
    fold_results$R2[2] <- mod$testres$r2[[ncomp]]; fold_results$RMSE[2] <- mod$testres$rmse[[ncomp]]; fold_results$RPD[2] <- mod$testres$rpd[[ncomp]]; fold_results$Bias[2] <- mod$testres$bias[[ncomp]]
    fold_results$PercentRMSE[2] <- mod$testres$rmse[[ncomp]] / max(testing$avg_age) * 100
    fold_results$Components[2] <- ncomp; fold_preds$vip_pred <- mod$testres$y.pred[, ncomp, ]
    splits_results_pls <- rbind(splits_results_pls, fold_results)
    all_predictions[[i]] <- fold_preds
  }
  splits_results_pls$ModelType <- "PLS"
  final_importance_df <- dplyr::bind_rows(all_importance_fold)
  return(list(results = splits_results_pls, predictions = all_predictions, importance = final_importance_df))
}
run_xgb_models <- function(cal, test, best_params_xgb) {
  xgb_results_df <- data.frame(); all_predictions <- list(); all_importance_fold <- list()
  for (i in 1:10) {
    calibrate <- cal[[i]]; testing <- test[[i]]
    x_train <- as.matrix(calibrate[, 40:ncol(calibrate)]); x_test <- as.matrix(testing[, 40:ncol(calibrate)])
    y_train <- calibrate[, "avg_age"]; y_test <- testing[, "avg_age"]
    dtrain <- xgb.DMatrix(data = x_train, label = y_train); dtest <- xgb.DMatrix(data = x_test, label = y_test)
    params <- list(objective = "reg:squarederror", booster = "gbtree", eta = 0.1, max_depth = best_params_xgb$max_depth, min_child_weight = best_params_xgb$min_child_weight, subsample = best_params_xgb$subsample, gamma = best_params_xgb$gamma, colsample_bytree = best_params_xgb$colsample_bytree, nthread = parallel::detectCores() - 1)
    xgb_model <- xgb.train(params = params, data = dtrain, nrounds = 1000, watchlist = list(train = dtrain, test = dtest), early_stopping_rounds = 20, verbose = 0)
    wavenumbers_char <- colnames(x_train); importance_matrix <- xgb.importance(model = xgb_model)
    importance_df <- data.frame(Feature = wavenumbers_char) %>% left_join(importance_matrix, by = "Feature") %>% mutate(fold = i, method = "XGBoost", wavenumber = as.numeric(Feature), importance = ifelse(is.na(Gain), 0, Gain)) %>% select(fold, method, wavenumber, importance)
    all_importance_fold[[i]] <- importance_df
    preds <- predict(xgb_model, dtest)
    fold_preds <- list(specimen_number = testing$specimen, actual = y_test, xgb_pred = preds)
    r2 <- cor(preds, y_test)^2; rmse_val <- sqrt(mean((preds - y_test)^2))
    fold_results <- data.frame(Fold = i, Model = "XGB", R2 = r2, RMSE = rmse_val, RPD = calculate_rpd(y_test, preds), Bias = calculate_bias(y_test, preds), PercentRMSE = rmse_val / max(y_test) * 100, Components = xgb_model$best_iteration)
    xgb_results_df <- rbind(xgb_results_df, fold_results)
    all_predictions[[i]] <- fold_preds
  }
  final_importance_df <- dplyr::bind_rows(all_importance_fold)
  xgb_results_df$ModelType <- "XGB"
  return(list(results = xgb_results_df, predictions = all_predictions, importance = final_importance_df))
}
run_rf_models <- function(cal, test, best_params_rf) {
  splits_results_rf <- data.frame(); all_predictions <- list(); all_importance_fold <- list()
  for (i in 1:10) {
    calibrate <- cal[[i]]; testing <- test[[i]]
    fold_preds <- list(specimen_number = testing$specimen, actual = testing$avg_age, rf_pred = numeric(length(testing$avg_age)))
    fold_results <- data.frame(Fold = i, Model = "RF", R2 = numeric(1), RMSE = numeric(1), RPD = numeric(1), Bias = numeric(1), PercentRMSE = numeric(1))
    mod <- ranger(x = calibrate[, 40:ncol(calibrate)], y = calibrate$avg_age, mtry = best_params_rf$mtry, min.node.size = best_params_rf$min.node.size, seed = 6, importance = 'permutation')
    wavenumbers <- as.numeric(colnames(calibrate[, 40:ncol(calibrate)])); importance_scores <- ranger::importance(mod)
    importance_df <- data.frame(fold = i, method = "Random Forest", wavenumber = wavenumbers, importance = importance_scores)
    all_importance_fold[[i]] <- importance_df
    preds <- predict(mod, data = testing)$predictions; fold_preds$rf_pred <- preds
    RSS <- sum((testing$avg_age - preds)^2); TSS <- sum((testing$avg_age - mean(testing$avg_age))^2)
    fold_results$R2 <- 1 - (RSS / TSS)
    fold_results$RMSE <- caret::RMSE(pred = preds, obs = testing[, "avg_age"])
    fold_results$RPD <- calculate_rpd(testing$avg_age, preds)
    fold_results$Bias <- calculate_bias(testing$avg_age, preds)
    fold_results$PercentRMSE <- fold_results$RMSE / max(testing$avg_age) * 100
    fold_results$Components <- 500
    splits_results_rf <- rbind(splits_results_rf, fold_results)
    all_predictions[[i]] <- fold_preds
  }
  final_importance_df <- dplyr::bind_rows(all_importance_fold)
  splits_results_rf$ModelType <- "RF"
  return(list(results = splits_results_rf, predictions = all_predictions, importance = final_importance_df))
}
combine_all_results <- function(model_results_list) {
  all_dfs <- lapply(model_results_list, function(x) { if ("results" %in% names(x)) x$results else x })
  all_cols <- unique(unlist(lapply(all_dfs, colnames)))
  standardized_dfs <- lapply(all_dfs, function(df) {
    missing_cols <- setdiff(all_cols, colnames(df))
    for (col in missing_cols) df[[col]] <- NA
    df[, all_cols]
  })
  do.call(rbind, standardized_dfs)
}
extract_all_predictions <- function(model_results, split_set) {
  all_preds <- list()
  for (model_type in names(model_results)) {
    pred_list <- model_results[[model_type]]$predictions
    for (fold in seq_along(pred_list)) {
      fold_preds <- pred_list[[fold]]
      if (model_type == "lm") {
        for (variant in 1:10) { all_preds[[paste("lm", split_set, fold, variant)]] <- data.frame(split_set = split_set, fold = fold, model_type = "LM", model_variant = paste("LM", variant), specimen_number = fold_preds$specimen_number, actual = fold_preds$actual, predicted = fold_preds$model_preds[[variant]]) }
      } else if (model_type == "gam") {
        for (variant in 1:10) { all_preds[[paste("gam", split_set, fold, variant)]] <- data.frame(split_set = split_set, fold = fold, model_type = "GAM", model_variant = paste("GAM", variant), specimen_number = fold_preds$specimen_number, actual = fold_preds$actual, predicted = fold_preds$model_preds[[variant]]) }
      } else if (model_type == "pls") {
        if (!is.null(fold_preds$pls_pred)) { all_preds[[paste("pls_standard", split_set, fold)]] <- data.frame(split_set = split_set, fold = fold, model_type = "PLS", model_variant = "PLS", specimen_number = fold_preds$specimen_number, actual = fold_preds$actual, predicted = fold_preds$pls_pred) }
        if (!is.null(fold_preds$vip_pred)) { all_preds[[paste("pls_vip", split_set, fold)]] <- data.frame(split_set = split_set, fold = fold, model_type = "PLS", model_variant = "PLS-VIP", specimen_number = fold_preds$specimen_number, actual = fold_preds$actual, predicted = fold_preds$vip_pred) }
      } else if (model_type == "rf") {
        all_preds[[paste("rf", split_set, fold)]] <- data.frame(split_set = split_set, fold = fold, model_type = "RF", model_variant = "RF", specimen_number = fold_preds$specimen_number, actual = fold_preds$actual, predicted = fold_preds$rf_pred)
      } else if (model_type == "xgb") {
        all_preds[[paste("xgb", split_set, fold)]] <- data.frame(split_set = split_set, fold = fold, model_type = "XGB", model_variant = "XGBoost", specimen_number = fold_preds$specimen_number, actual = fold_preds$actual, predicted = fold_preds$xgb_pred)
      }
    }
  }
  final_predictions <- do.call(rbind, all_preds)
  if (!is.null(final_predictions) && nrow(final_predictions) > 0) { rownames(final_predictions) <- NULL }
  return(final_predictions)
}


# ================================================================================== #
# 2. PARALLEL EXECUTION OF COMPLEX MODELS
# ================================================================================== #

# A. Define the function to process a single split (body of your original for-loop)
process_single_split <- function(split_set, all_splits_data, df_data, p) {
  
  # Signal a progress update
  p(sprintf("Split %g", split_set))
  
  # Create cal/test data
  splits <- all_splits_data[[split_set]]
  cal <- test <- vector("list", 10)
  pca_importance_split <- list()
  
  for (i in 1:10) {
    # Use "nzv" to automatically remove zero-variance columns, preventing crashes
    pc.mod <- preProcess(df_data[-splits[[i]], -c(1:29)], method = c("nzv", "center", "pca"), pcaComp = 10)
    
    cal[[i]] <- cbind(predict(pc.mod, df_data[-splits[[i]], -c(1:29)]), df_data[-splits[[i]], ])
    test[[i]] <- cbind(predict(pc.mod, df_data[splits[[i]], -c(1:29)]), df_data[splits[[i]], ])
    
    # Ensure there are loadings to process
    if (!is.null(pc.mod$rotation)) {
      pca_loadings <- pc.mod$rotation
      # Adjust variance calculation if nzv removed some components
      num_pcs_retained <- min(10, ncol(pca_loadings))
      variances <- pc.mod$std[1:num_pcs_retained]^2
      prop_variance <- variances / sum(variances)
      weighted_importance <- abs(pca_loadings[, 1:num_pcs_retained]) %*% prop_variance
      
      pca_importance_fold <- data.frame(
        fold = i,
        method = "PCA Loadings",
        wavenumber = as.numeric(rownames(pca_loadings)),
        importance = weighted_importance[, 1]
      )
      pca_importance_split[[i]] <- pca_importance_fold
    }
  }
  
  # Run models and store results
  # Note: `terms_lm` and other objects are found in the global environment by the future workers
  model_results <- list(
    lm = run_lm_models(cal, test, terms_lm),
    gam = run_gam_models(cal, test, terms_gam),
    pls = run_pls_models(cal, test),
    rf = run_rf_models(cal, test, best_params_rf),
    xgb = run_xgb_models(cal, test, best_params_xgb)
  )
  
  # Extract importance data
  pca_importance_df <- dplyr::bind_rows(pca_importance_split)
  pls_importance_df <- model_results$pls$importance
  rf_importance_df <- model_results$rf$importance
  xgb_importance_df <- model_results$xgb$importance
  
  importance_for_this_split <- dplyr::bind_rows(
    pca_importance_df, pls_importance_df, rf_importance_df, xgb_importance_df
  )
  if(nrow(importance_for_this_split) > 0) {
    importance_for_this_split$split_set <- split_set
  }
  
  # Add split set ID to results
  for (m in names(model_results)) {
    model_results[[m]]$results$SplitSet <- split_set
  }
  
  # Extract predictions
  predictions_for_this_split <- extract_all_predictions(model_results, split_set)
  
  # Return all results for this split in a list
  return(list(
    results = model_results,
    predictions = predictions_for_this_split,
    importance = importance_for_this_split
  ))
}


# B. Configure and run the parallel job
n_splits <- 500
set.seed(6)
all_splits <- generate_multiple_splits(df, n_splits)

# Set up parallel backend using 'multisession' (works on all OS)
plan(multisession, workers = parallel::detectCores() - 1)

cat("Starting parallel processing of", n_splits, "splits...\n")
Sys.time()

# Set up progress bar
handlers(
  handler_progress(
    format = "[:bar] :percent | elapsed: :elapsed | eta: :eta",
    width = 60
  )
)

# Use with_progress to enable the bar
with_progress({
  p <- progressor(steps = n_splits)
  # Run the process in parallel using future_lapply
  # future.seed = TRUE makes random processes (like in ranger) reproducible
  parallel_results_list <- future_lapply(
    1:n_splits, 
    FUN = function(i) process_single_split(i, all_splits_data = all_splits, df_data = df, p = p),
    future.seed = TRUE
  )
})

Sys.time()
plan(sequential) # Shut down parallel workers
# 15 minutes vs 1:10 or so non-parallel...!!!!!!


# C. Combine results from the parallel run
cat("Aggregating results from parallel runs...\n")
all_results <- lapply(parallel_results_list, function(x) x$results)
all_predictions_list <- lapply(parallel_results_list, function(x) x$predictions)
all_importance_data <- lapply(parallel_results_list, function(x) x$importance)

# Combine metrics, predictions, and importance data into final dataframes
final_results <- combine_all_results(unlist(all_results, recursive = FALSE))
final_predictions <- do.call(rbind, all_predictions_list)
final_importance_data <- dplyr::bind_rows(all_importance_data)

# Calculate mean metrics for each simulation
all_results_means <- final_results %>%
  group_by(SplitSet, Model, ModelType) %>%
  summarize(across(c(R2, RMSE, RPD, Bias, PercentRMSE, Components), mean, na.rm = TRUE), .groups = "drop")

# ================================================================================= #
# 3. SIMPLE MODELS (PARALLELIZED) AND FINAL COMBINATION
# ================================================================================= #

# Filter data and generate splits (same as your original code)
df_simple <- df[complete.cases(df$structure_weight), ]
all_splits_simple <- generate_multiple_splits(df_simple, n_splits)


## 1. Define the "Worker" Function for a Single Split
# This function contains the logic from one iteration of your original loop
process_single_split_simple <- function(split_num, data, splits_list, p) {
  
  # Signal a progress update
  p(sprintf("Simple Split %g", split_num))
  
  # Get the current split
  current_split <- splits_list[[split_num]]
  
  # Initialize results for this specific split
  split_results_simple <- data.frame()
  split_predictions_simple_fold <- list()
  
  # Process each of the 10 folds (this part is fast, so we don't parallelize it)
  for (i in 1:10) {
    test_indices <- current_split[[i]]
    testing <- data[test_indices, ]
    calibrate <- data[-test_indices, ]
    
    fold_preds <- list(
      specimen_number = testing$specimen, 
      actual = testing$avg_age, 
      simple_lm_pred = numeric(length(testing$avg_age)),
      simple_gam_pred = numeric(length(testing$avg_age))
    )
    
    fold_results <- data.frame(
      Split = split_num, Fold = i, Model = c("Simple lm", "Simple gam"),
      R2 = numeric(2), RMSE = numeric(2), RPD = numeric(2),
      Bias = numeric(2), PercentRMSE = numeric(2)
    )
    
    # Simple linear model
    mod_lm <- lm(data = calibrate, avg_age ~ length + structure_weight, na.action = na.omit)
    preds_lm <- predict(mod_lm, newdata = testing)
    fold_preds$simple_lm_pred <- preds_lm
    
    # Calculate metrics for LM
    fold_results$RMSE[1] <- caret::RMSE(pred = preds_lm, obs = testing$avg_age)
    fold_results$PercentRMSE[1] <- fold_results$RMSE[1] / max(testing$avg_age) * 100
    RSS_lm <- sum((testing$avg_age - preds_lm)^2); TSS_lm <- sum((testing$avg_age - mean(testing$avg_age))^2)
    fold_results$R2[1] <- 1 - (RSS_lm / TSS_lm)
    fold_results$RPD[1] <- calculate_rpd(testing$avg_age, preds_lm)
    fold_results$Bias[1] <- calculate_bias(testing$avg_age, preds_lm)
    
    # Simple GAM model
    mod_gam <- gam(data = calibrate, avg_age ~ s(length, k = 4) + s(structure_weight, k = 4), method = "REML")
    preds_gam <- predict(mod_gam, newdata = testing)
    fold_preds$simple_gam_pred <- preds_gam
    
    # Calculate metrics for GAM
    fold_results$RMSE[2] <- caret::RMSE(pred = preds_gam, obs = testing$avg_age)
    fold_results$PercentRMSE[2] <- fold_results$RMSE[2] / max(testing$avg_age) * 100
    RSS_gam <- sum((testing$avg_age - preds_gam)^2); TSS_gam <- sum((testing$avg_age - mean(testing$avg_age))^2)
    fold_results$R2[2] <- 1 - (RSS_gam / TSS_gam)
    fold_results$RPD[2] <- calculate_rpd(testing$avg_age, preds_gam)
    fold_results$Bias[2] <- calculate_bias(testing$avg_age, preds_gam)
    
    # Add Components and ModelType
    fold_results$Components <- 3
    fold_results$ModelType <- "Simple"
    
    # Append fold data to the data for this split
    split_results_simple <- rbind(split_results_simple, fold_results)
    split_predictions_simple_fold[[i]] <- fold_preds
  }
  
  # Return both results and predictions for this single split
  return(list(results = split_results_simple, predictions = split_predictions_simple_fold))
}


## 2. Run the Simple Models in Parallel
cat("Starting parallel processing of", n_splits, "simple model splits...\n")
plan(multisession, workers = availableCores() - 1) # Set up parallel backend
Sys.time()

with_progress({
  p <- progressor(steps = n_splits)
  # Run the process in parallel
  parallel_results_simple <- future_lapply(
    1:n_splits,
    FUN = function(i) process_single_split_simple(i, data = df_simple, splits_list = all_splits_simple, p = p),
    future.seed = TRUE # Ensures reproducibility
  )
})

Sys.time()
plan(sequential) # Shut down parallel workers


## 3. Combine Results and Post-Process
cat("Aggregating results from simple models...\n")

# Extract the results and predictions from the list returned by the parallel run
simple_metrics_list <- lapply(parallel_results_simple, function(x) x$results)
simple_predictions <- lapply(parallel_results_simple, function(x) x$predictions)

# Combine into single data frames
simple_metrics <- dplyr::bind_rows(simple_metrics_list)

# Your original code for calculating means and combining results works perfectly here
simple_results_means <- simple_metrics %>%
  group_by(Split, Model) %>%
  summarize(
    across(c(R2, RMSE, RPD, Bias, PercentRMSE, Components), mean, na.rm = TRUE), 
    ModelType = "Simple", 
    .groups = "drop"
  ) %>%
  rename(SplitSet = Split)

# Combine with the complex model results
all_results_means_final <- rbind(all_results_means, simple_results_means)

# Your original code for formatting predictions also works perfectly
simple_predictions_formatted <- list()
for (split_set in seq_along(simple_predictions)) {
  for (fold in seq_along(simple_predictions[[split_set]])) {
    fold_preds <- simple_predictions[[split_set]][[fold]]
    # Simple LM predictions
    simple_predictions_formatted[[paste("simple_lm", split_set, fold)]] <- data.frame(
      split_set = split_set, fold = fold, model_type = "Simple", model_variant = "Simple LM",
      specimen_number = fold_preds$specimen_number, actual = fold_preds$actual, predicted = fold_preds$simple_lm_pred
    )
    # Simple GAM predictions
    simple_predictions_formatted[[paste("simple_gam", split_set, fold)]] <- data.frame(
      split_set = split_set, fold = fold, model_type = "Simple", model_variant = "Simple GAM",
      specimen_number = fold_preds$specimen_number, actual = fold_preds$actual, predicted = fold_preds$simple_gam_pred
    )
  }
}
simple_predictions_df <- do.call(rbind, simple_predictions_formatted)
rownames(simple_predictions_df) <- NULL

# Combine with complex model predictions
all_predictions_final <- rbind(final_predictions, simple_predictions_df)

# Save final objects
saveRDS(all_results_means_final, paste0("RDS_dataframes/IBM_all_results_means_parallel", Sys.Date(), ".RDS"))
saveRDS(all_predictions_final, paste0("RDS_dataframes/IBM_all_predictions_parallel", Sys.Date(), ".RDS"))
saveRDS(final_importance_data, paste0("RDS_dataframes/IBM_final_importance_data_parallel", Sys.Date(), ".RDS"))
