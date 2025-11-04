# Sys.setenv(OMP_NUM_THREADS = 1)
# Sys.setenv(MKL_NUM_THREADS = 1)
# Sys.setenv(OPENBLAS_NUM_THREADS = 1)  # Since you're using OpenBLAS


# ================================================================================== #
# 1. SETUP: PACKAGES, DATA, AND MODEL TERMS
# ================================================================================== #

# Packages #####################################################################
packages <- c("caret", "doParallel", "dplyr", "devtools", "ggplot2", "mdatools", 
              "mgcv", "MuMIn", "purrr", "ranger", "stringr", "tidyr", "viridis", 
              "xgboost", "future", "future.apply", "progressr", "furrr")
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  utils::install.packages(pkgs = packages[!installed_packages])
}
invisible(lapply(packages, library, character.only = TRUE))
rm(installed_packages, packages)

################################################################################

# Load dataframe ===============================================================

################################################################################

df <- readRDS("RDS_dataframes/LPW_scan_avg_proc.RDS")
df <- df[complete.cases(df$read_age), ]
df <- df %>% filter(specimen != 53, specimen != 74)



# IF YOU WANT TO REMOVE >7500 WAVENUMBER USE BELOW ******#$#)($#)($@*#)$(@*#$)
# Convert names to numeric, NAs are created for non-numeric names
# numeric_names <- suppressWarnings(as.numeric(names(df)))
# Keep columns that are NOT numbers OR are numbers <= 7500
# df <- df[, is.na(numeric_names) | numeric_names <= 7500] 

################################################################################

# Dredge to find top 5 models ==================================================

################################################################################

pca_temp <- mdatools::pca(df[, 21:ncol(df)])
pc_df <- data.frame(PC1 = rep(0, nrow(df)))
for (i in 1:10) {
  pc_df[, paste0("PC", i)] <- pca_temp$res$cal$scores[, i]
}
pc_df <- cbind(pc_df, df)

global_lm <- lm(data = pc_df, read_age ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10)
global_gam <- gam(data = pc_df, read_age ~ s(PC1,k = 4) + s(PC2,k = 4) + s(PC3,k = 4) + s(PC4,k = 4) + s(PC5,k = 4) + s(PC6,k = 4) + s(PC7, k = 4) + s(PC8, k = 4) + s(PC9, k = 4) + s(PC10, k = 4))

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

pc_counts_lm <- sapply(terms_lm, function(term) {
  length(attr(term, "term.labels"))
})

pc_counts_gam <- sapply(terms_gam, function(formula) {
  formula_str <- as.character(formula)[3]
  stringr::str_count(formula_str, "PC\\d+")
})


rm(global_gam, global_lm, top10_gam, top10_lm, pc_df, pca_temp, dredge_gam, dredge_lm, i)

################################################################################

# Functions for splits and metrics =================================================

################################################################################

generate_multiple_splits <- function(data = df, n_splits) {
  all_splits <- list()
  for (split_id in 1:n_splits) {
    all_splits[[split_id]] <- caret::createFolds(data$read_age, k = 10, list = TRUE, returnTrain = FALSE)
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

################################################################################

# Tuning  ======================================================================

################################################################################

# XGB Tuning ####
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
set.seed(6)
my_random_grid <- my_full_grid[sample(1:nrow(my_full_grid), 1000), ]
train_control <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 1,
  search = "grid",
  allowParallel = TRUE
)
Sys.time() # show start time
xgb_tuned_model <- train(
  x = as.matrix(df[, 21:ncol(df)]),
  y = df$read_age,
  method = "xgbTree",
  trControl = train_control,
  tuneGrid = my_random_grid,
  nthread = 1,
  verbose = FALSE # Suppress output during tuning
)
Sys.time()
stopCluster(cl)
(best_params_xgb <- xgb_tuned_model$bestTune)



# 10/03/2025 
# filtered, 100 / 162,000. ~ 1 minute
# nrounds max_depth  eta gamma colsample_bytree min_child_weight subsample
# 32    2000         3 0.05     1              0.2                1       0.2

# filtered, 1000 / 162,000, 8 minutes
# nrounds max_depth eta gamma colsample_bytree min_child_weight subsample
# 741     100         6 0.2     0              0.6                6       0.8

# 10/03/2025 
# all waves, 100 / 162,000. ~  minutes
# nrounds max_depth  eta gamma colsample_bytree min_child_weight subsample
# 1     500         1 0.01   0.1              0.6                8       0.6
# all waves, 1000 / 162,000. ~  17 minutes
# nrounds max_depth  eta gamma colsample_bytree min_child_weight subsample
# 257     300         2 0.05     5              0.4                8         1

# ON NEW PC: OUTLIERS REMOVED, all waves, 1000 ~ 7 minutes
# nrounds max_depth  eta gamma colsample_bytree min_child_weight subsample
# 348    2000         6 0.05   0.1              0.8                8         1

# AFTER OpenBLAS: outliers removed, all waves, 1000 ~ 6.5 mins
# nrounds max_depth  eta gamma colsample_bytree min_child_weight subsample
# 348    2000         6 0.05   0.1              0.8                8         1

# OPENBLAS, outliers remove, all waves, 1000, undervolt -20mv ~ 6:22
# nrounds max_depth  eta gamma colsample_bytree  min_child_weight subsample
# 348    2000         6 0.05   0.1              0.8                8         1


################################################################################
# previous parameters # 
# best_params_xgb <- data.frame(nrounds = 300, max_depth = 2, eta = 0.05, gamma = 5, colsample_bytree = 0.4, min_child_weight = 8, subsample = 1)



# previous parameters: remove outliers, 1000 hyperparameters
# best_params_xgb <- data.frame(nrounds = 2000, max_depth = 6, eta = 0.05, gamma = 1, colsample_bytree = 0.8, min_child_weight = 8, subsample = 1)
################################################################################



################################################################################

## Random Forest Tuning ####

################################################################################

cl <- makePSOCKcluster(parallel::detectCores() - 1)
registerDoParallel(cl)
tuning_grid_rf <- expand.grid(
  mtry = c(floor(sqrt(ncol(df[, 21:ncol(df)])) * 0.1),
           floor(sqrt(ncol(df[, 21:ncol(df)])) * 0.2),
           floor(ncol(df[, 21:ncol(df)]) / 3),
           floor(ncol(df[, 21:ncol(df)]) / 2)),
  min.node.size = c(1, 3, 5, 15, 25, 35),
  splitrule = "variance" # Required for regression with ranger in caret
)
train_control <- trainControl(
  method = "repeatedcv",
  number = 10,       # 10 folds
  repeats = 1,
  search = "grid",
  allowParallel = TRUE
)
Sys.time()
rf_tuned_model <- train(
  x = df[, 21:ncol(df)],
  y = df$read_age,
  method = "ranger",        # Use the ranger package for Random Forest
  trControl = train_control,
  tuneGrid = tuning_grid_rf,
  num.threads = 1,
  importance = 'permutation', # Calculate variable importance on the final model
  num.trees = 1000
)
Sys.time()
stopCluster(cl)
(best_params_rf <- rf_tuned_model$bestTune)



################################################################################
#   mtry splitrule min.node.size
# 23  218  variance            25

# previous parameters # 
# best_params_rf <- data.frame(
#   mtry = 466,
#   min.node.size = 15,
#   splitrule = "variance"
# )

# OUTLIERS REMOVED, ALL WAVES: 
# mtry splitrule min.node.size
# 16  311  variance            15


# OUTLIERS REMOVED
# best_params_rf <- data.frame(
#   mtry = 311,
#   min.node.size = 15,
#   splitrule = "variance"
# )

################################################################################


################################################################################

# model functions ==============================================================

################################################################################

run_lm_models <- function(cal, test, terms_lm, pc_counts_lm) {
  # Added pc_counts_lm
  splits_results_lm <- data.frame()
  all_predictions <- list()
  for (i in 1:10) {
    calibrate <- cal[[i]]
    testing <- test[[i]]
    fold_results <- data.frame(
      Fold = i,
      Model = paste0("Linear ", 1:10),
      R2 = numeric(10),
      RMSE = numeric(10),
      RPD = numeric(10),
      Bias = numeric(10),
      PercentRMSE = numeric(10),
      Components = numeric(10)
    )
    fold_preds <- list(
      specimen_number = testing$specimen,
      actual = testing$read_age,
      model_preds = vector("list", 10),
      model_comps = vector("list", 10)
    ) # Added model_comps
    for (j in 1:10) {
      mod <- lm(data = calibrate, terms_lm[[j]])
      preds <- predict(mod, newdata = testing)
      # results
      fold_results$RMSE[j] <- caret::RMSE(pred = preds, obs = testing$read_age)
      fold_results$PercentRMSE[j] <- fold_results$RMSE[j] / max(testing$read_age) * 100
      RSS <- sum((testing$read_age - preds)^2)
      TSS <- sum((testing$read_age - mean(testing$read_age))^2)
      fold_results$R2[j] <- 1 - (RSS / TSS)
      fold_results$RPD[j] <- calculate_rpd(testing$read_age, preds)
      fold_results$Bias[j] <- calculate_bias(testing$read_age, preds)
      fold_results$Components[j] <- pc_counts_lm[j] # Store component count in results
      # predictions
      fold_preds$model_preds[[j]] <- preds
      fold_preds$model_comps[[j]] <- pc_counts_lm[j] # Store component count for predictions
    }
    splits_results_lm <- rbind(splits_results_lm, fold_results)
    all_predictions[[i]] <- fold_preds
  }
  splits_results_lm$ModelType <- "LM"
  return(list(results = splits_results_lm, predictions = all_predictions))
}
################################################################################
run_gam_models <- function(cal, test, terms_gam, pc_counts_gam) {
  # Added pc_counts_gam
  splits_results_gam <- data.frame()
  all_predictions <- list()
  for (i in 1:10) {
    calibrate <- cal[[i]]
    testing <- test[[i]]
    fold_results <- data.frame(
      Fold = i,
      Model = paste0("GAM ", 1:10),
      R2 = numeric(10),
      RMSE = numeric(10),
      RPD = numeric(10),
      Bias = numeric(10),
      PercentRMSE = numeric(10),
      Components = numeric(10)
    )
    fold_preds <- list(
      specimen_number = testing$specimen,
      actual = testing$read_age,
      model_preds = vector("list", 10),
      model_comps = vector("list", 10)
    ) # Added model_comps
    for (j in 1:10) {
      mod <- gam(data = calibrate, terms_gam[[j]], method = "REML")
      preds <- predict(mod, newdata = testing)
      fold_results$RMSE[j] <- caret::RMSE(pred = preds, obs = testing$read_age)
      fold_results$PercentRMSE[j] <- fold_results$RMSE[j] / max(testing$read_age) * 100
      RSS <- sum((testing$read_age - preds)^2)
      TSS <- sum((testing$read_age - mean(testing$read_age))^2)
      fold_results$R2[j] <- 1 - (RSS / TSS)
      fold_results$RPD[j] <- calculate_rpd(testing$read_age, preds)
      fold_results$Bias[j] <- calculate_bias(testing$read_age, preds)
      fold_results$Components[j] <- pc_counts_gam[j] # Store component count in results
      fold_preds$model_preds[[j]] <- preds
      fold_preds$model_comps[[j]] <- pc_counts_gam[j] # Store component count for predictions
    }
    splits_results_gam <- rbind(splits_results_gam, fold_results)
    all_predictions[[i]] <- fold_preds
  }
  splits_results_gam$ModelType <- "GAM"
  return(list(results = splits_results_gam, predictions = all_predictions))
}
################################################################################
# --- UPDATED run_pls_models ---
run_pls_models <- function(cal, test) {
  splits_results_pls <- data.frame()
  all_predictions <- list()
  all_importance_fold <- list()
  for (i in 1:10) {
    calibrate <- cal[[i]]
    testing <- test[[i]]
    fold_results <- data.frame(
      Fold = i,
      Model = c("PLS", "PLS - VIP"),
      R2 = numeric(2),
      RMSE = numeric(2),
      RPD = numeric(2),
      Bias = numeric(2),
      PercentRMSE = numeric(2),
      Components = numeric(2)
    )
    fold_preds <- list(
      specimen_number = testing$specimen,
      actual = testing$read_age,
      pls_pred = numeric(length(testing$read_age)),
      vip_pred = numeric(length(testing$read_age)),
      pls_ncomp = NA_real_,
      vip_ncomp = NA_real_
    ) # Added ncomp storage
    mod <- mdatools::pls(
      calibrate[, 31:ncol(calibrate)],
      calibrate[, "read_age"],
      cv = 1,
      scale = F,
      center = T,
      x.test = testing[, 31:ncol(testing)],
      y.test = testing[, "read_age"]
    )
    ncomp <- mod$ncomp.selected
    wavenumbers <- as.numeric(colnames(calibrate[, 31:ncol(calibrate)]))
    vip_scores <- vipscores(mod)
    importance_df <- data.frame(
      fold = i,
      method = "PLS-VIP",
      wavenumber = wavenumbers,
      importance = vip_scores
    )
    all_importance_fold[[i]] <- importance_df
    fold_results$R2[1] <- mod$testres$r2[[ncomp]]
    fold_results$RMSE[1] <- mod$testres$rmse[[ncomp]]
    fold_results$RPD[1] <- mod$testres$rpd[[ncomp]]
    fold_results$Bias[1] <- mod$testres$bias[[ncomp]]
    fold_results$PercentRMSE[1] <- mod$testres$rmse[[ncomp]] / max(testing$read_age) * 100
    fold_results$Components[1] <- ncomp
    fold_preds$pls_pred <- mod$testres$y.pred[, ncomp, ]
    fold_preds$pls_ncomp <- ncomp # Store ncomp for predictions
    vip <- as.data.frame(vipscores(mod))
    mod <- mdatools::pls(
      calibrate[, 31:ncol(calibrate)],
      calibrate[, "read_age"],
      scale = F,
      center = T,
      cv = 1,
      x.test = testing[, 31:ncol(testing)],
      y.test = testing[, "read_age"],
      exclcols = vip$V1 < 0.5
    )
    ncomp <- mod$ncomp.selected
    fold_results$R2[2] <- mod$testres$r2[[ncomp]]
    fold_results$RMSE[2] <- mod$testres$rmse[[ncomp]]
    fold_results$RPD[2] <- mod$testres$rpd[[ncomp]]
    fold_results$Bias[2] <- mod$testres$bias[[ncomp]]
    fold_results$PercentRMSE[2] <- mod$testres$rmse[[ncomp]] / max(testing$read_age) * 100
    fold_results$Components[2] <- ncomp
    fold_preds$vip_pred <- mod$testres$y.pred[, ncomp, ]
    fold_preds$vip_ncomp <- ncomp # Store ncomp for predictions
    splits_results_pls <- rbind(splits_results_pls, fold_results)
    all_predictions[[i]] <- fold_preds
  }
  splits_results_pls$ModelType <- "PLS"
  final_importance_df <- dplyr::bind_rows(all_importance_fold)
  return(
    list(
      results = splits_results_pls,
      predictions = all_predictions,
      importance = final_importance_df
    )
  )
}
################################################################################
run_xgb_models <- function(cal, test, best_params_xgb) {
  xgb_results_df <- data.frame()
  all_predictions <- list()
  all_importance_fold <- list()
  for (i in 1:10) {
    calibrate <- cal[[i]]
    testing <- test[[i]]
    x_train <- as.matrix(calibrate[, 31:ncol(calibrate)])
    x_test <- as.matrix(testing[, 31:ncol(calibrate)])
    y_train <- calibrate$read_age
    y_test <- testing$read_age
    dtrain <- xgb.DMatrix(data = x_train, label = y_train)
    dtest <- xgb.DMatrix(data = x_test, label = y_test)
    params <- list(
      objective = "reg:squarederror",
      booster = "gbtree",
      eta = best_params_xgb$eta,
      max_depth = best_params_xgb$max_depth,
      min_child_weight = best_params_xgb$min_child_weight,
      subsample = best_params_xgb$subsample,
      gamma = best_params_xgb$gamma,
      colsample_bytree = best_params_xgb$colsample_bytree,
      nthread = 1
    ) # Set nthread to 1
    xgb_model <- xgb.train(
      params = params,
      data = dtrain,
      nrounds = best_params_xgb$nrounds,
      watchlist = list(train = dtrain, test = dtest),
      early_stopping_rounds = 20,
      verbose = 0
    )
    wavenumbers_char <- colnames(x_train)
    importance_matrix <- xgb.importance(model = xgb_model)
    importance_df <- data.frame(Feature = wavenumbers_char) %>% left_join(importance_matrix, by = "Feature") %>% mutate(
      fold = i,
      method = "XGBoost",
      wavenumber = as.numeric(Feature),
      importance = ifelse(is.na(Gain), 0, Gain)
    ) %>% select(fold, method, wavenumber, importance)
    all_importance_fold[[i]] <- importance_df
    preds <- predict(xgb_model, dtest)
    fold_preds <- list(
      specimen_number = testing$specimen,
      actual = y_test,
      xgb_pred = preds,
      components = NA_real_
    ) # Added components
    r2 <- cor(preds, y_test)^2
    rmse_val <- sqrt(mean((preds - y_test)^2))
    fold_results <- data.frame(
      Fold = i,
      Model = "XGB",
      R2 = r2,
      RMSE = rmse_val,
      RPD = calculate_rpd(y_test, preds),
      Bias = calculate_bias(y_test, preds),
      PercentRMSE = rmse_val / max(y_test) * 100,
      Components = NA_real_
    ) # Added components
    xgb_results_df <- rbind(xgb_results_df, fold_results)
    all_predictions[[i]] <- fold_preds
  }
  final_importance_df <- dplyr::bind_rows(all_importance_fold)
  xgb_results_df$ModelType <- "XGB"
  return(
    list(
      results = xgb_results_df,
      predictions = all_predictions,
      importance = final_importance_df
    )
  )
}
################################################################################
run_rf_models <- function(cal, test, best_params_rf) {
  splits_results_rf <- data.frame()
  all_predictions <- list()
  all_importance_fold <- list()
  for (i in 1:10) {
    calibrate <- cal[[i]]
    testing <- test[[i]]
    fold_preds <- list(
      specimen_number = testing$specimen,
      actual = testing$read_age,
      rf_pred = numeric(length(testing$read_age)),
      components = NA_real_
    ) # Added components
    fold_results <- data.frame(
      Fold = i,
      Model = "RF",
      R2 = numeric(1),
      RMSE = numeric(1),
      RPD = numeric(1),
      Bias = numeric(1),
      PercentRMSE = numeric(1),
      Components = NA_real_
    ) # Added components
    mod <- ranger(
      x = calibrate[, 31:ncol(calibrate)],
      y = calibrate$read_age,
      mtry = best_params_rf$mtry,
      min.node.size = best_params_rf$min.node.size,
      seed = 6,
      importance = 'permutation', 
      num.threads = 1
    )
    wavenumbers <- as.numeric(colnames(calibrate[, 31:ncol(calibrate)]))
    importance_scores <- ranger::importance(mod)
    importance_df <- data.frame(
      fold = i,
      method = "Random Forest",
      wavenumber = wavenumbers,
      importance = importance_scores
    )
    all_importance_fold[[i]] <- importance_df
    preds <- predict(mod, data = testing)$predictions
    fold_preds$rf_pred <- preds
    RSS <- sum((testing$read_age - preds)^2)
    TSS <- sum((testing$read_age - mean(testing$read_age))^2)
    fold_results$R2 <- 1 - (RSS / TSS)
    fold_results$RMSE <- caret::RMSE(pred = preds, obs = testing$read_age)
    fold_results$RPD <- calculate_rpd(testing$read_age, preds)
    fold_results$Bias <- calculate_bias(testing$read_age, preds)
    fold_results$PercentRMSE <- fold_results$RMSE / max(testing$read_age) * 100
    splits_results_rf <- rbind(splits_results_rf, fold_results)
    all_predictions[[i]] <- fold_preds
  }
  final_importance_df <- dplyr::bind_rows(all_importance_fold)
  splits_results_rf$ModelType <- "RF"
  return(
    list(
      results = splits_results_rf,
      predictions = all_predictions,
      importance = final_importance_df
    )
  )
}


################################################################################

# results and predictions

################################################################################

combine_all_results <- function(model_results_list) {
  all_dfs <- lapply(model_results_list, function(x) {
    if ("results" %in% names(x))
      x$results
    else
      x
  })
  all_cols <- unique(unlist(lapply(all_dfs, colnames)))
  standardized_dfs <- lapply(all_dfs, function(df) {
    missing_cols <- setdiff(all_cols, colnames(df))
    for (col in missing_cols)
      df[[col]] <- NA
    df[, all_cols]
  })
  do.call(rbind, standardized_dfs)
}
################################################################################
extract_all_predictions <- function(model_results, split_set) {
  all_preds <- list()
  for (model_type in names(model_results)) {
    pred_list <- model_results[[model_type]]$predictions
    for (fold in seq_along(pred_list)) {
      fold_preds <- pred_list[[fold]]
      base_df <- data.frame(
        split_set = split_set,
        fold = fold,
        specimen_number = fold_preds$specimen_number,
        actual = fold_preds$actual
      )
      if (model_type %in% c("lm", "gam")) {
        for (variant in 1:10) {
          variant_df <- base_df
          variant_df$model_type <- toupper(model_type)
          variant_df$model_variant <- paste(toupper(model_type), variant)
          variant_df$predicted <- fold_preds$model_preds[[variant]]
          variant_df$components <- fold_preds$model_comps[[variant]]
          all_preds[[length(all_preds) + 1]] <- variant_df
        }
      } else if (model_type == "pls") {
        pls_df <- base_df
        pls_df$model_type <- "PLS"
        pls_df$model_variant <- "PLS"
        pls_df$predicted <- fold_preds$pls_pred
        pls_df$components <- fold_preds$pls_ncomp
        all_preds[[length(all_preds) + 1]] <- pls_df
        vip_df <- base_df
        vip_df$model_type <- "PLS"
        vip_df$model_variant <- "PLS-VIP"
        vip_df$predicted <- fold_preds$vip_pred
        vip_df$components <- fold_preds$vip_ncomp
        all_preds[[length(all_preds) + 1]] <- vip_df
      } else if (model_type == "rf") {
        rf_df <- base_df
        rf_df$model_type <- "RF"
        rf_df$model_variant <- "RF"
        rf_df$predicted <- fold_preds$rf_pred
        rf_df$components <- fold_preds$components
        all_preds[[length(all_preds) + 1]] <- rf_df
      } else if (model_type == "xgb") {
        xgb_df <- base_df
        xgb_df$model_type <- "XGB"
        xgb_df$model_variant <- "XGBoost"
        xgb_df$predicted <- fold_preds$xgb_pred
        xgb_df$components <- fold_preds$components
        all_preds[[length(all_preds) + 1]] <- xgb_df
      }
    }
  }
  return(bind_rows(all_preds))
}


# ================================================================================== #
# 2. PARALLEL EXECUTION OF COMPLEX MODELS
# ================================================================================== #


# A. Define the function to process a single split 
#    We use `...` to capture all necessary variables and pass them cleanly.
process_single_split_optimized <- function(
    split_set, 
    all_splits_data, 
    df_data, 
    terms_lm, 
    pc_counts_lm, 
    terms_gam, 
    pc_counts_gam, 
    best_params_rf, 
    best_params_xgb
) {
  # This function body remains largely the same as your original:
  
  # Create cal/test data
  splits <- all_splits_data[[split_set]]
  cal <- test <- vector("list", 10)
  pca_importance_split <- list()
  
  for (i in 1:10) {
    # Use "nzv" to automatically remove zero-variance columns, preventing crashes
    # NOTE: df_data[, -c(1:20)] is hardcoded, check if this is correct for your data!
    # Assuming 21:ncol(df) is the feature range.
    feature_cols_index <- 21:ncol(df_data)
    
    pc.mod <- caret::preProcess(df_data[-splits[[i]], feature_cols_index], method = c("nzv", "center", "pca"), pcaComp = 10)
    
    cal[[i]] <- cbind(predict(pc.mod, df_data[-splits[[i]], feature_cols_index]), df_data[-splits[[i]], ])
    test[[i]] <- cbind(predict(pc.mod, df_data[splits[[i]], feature_cols_index]), df_data[splits[[i]], ])
    
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
  model_results <- list(
    lm = run_lm_models(cal, test, terms_lm, pc_counts_lm),
    gam = run_gam_models(cal, test, terms_gam, pc_counts_gam), 
    # NOTE: run_pls_models, run_xgb_models, and run_rf_models use hardcoded 
    # indices like 31:ncol(calibrate). Ensure these indices are correct 
    # if your column structure changes after cbind in the cal/test creation.
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


################################################################################

# B. Configure and run the parallel job

################################################################################

# B. Configure and run the parallel job (Optimized)

n_splits <- 500
set.seed(6)
all_splits <- generate_multiple_splits(df, n_splits)

# 1. Choose the number of workers: 
#    A 9800X3D has 8 physical cores and 16 logical threads. 
#    multisession/multicore workers should map well to physical cores.
#    Using a large number of workers (e.g., all 16) can sometimes be better, 
#    but start with 8 or 10.
num_workers <- parallel::detectCores()


Sys.setenv(OMP_NUM_THREADS = 1, 
           R_RETHRADS = 1,
           MKL_NUM_THREADS = 1)

# 2. Set the parallel plan (multisession works on all OS, multicore is Linux/Mac only)
plan(multisession, workers = num_workers) 

cat("Starting parallel processing of", n_splits, "splits with", num_workers, "workers...\n")
start_time <- Sys.time()

# Set up progress bar
handlers(
  handler_progress(
    format = "[:bar] :percent | elapsed: :elapsed | eta: :eta",
    width = 60
  )
)

# 3. Define the iterable list (Split IDs)
split_ids <- 1:n_splits

# 4. Run the process in parallel using future_map
#    We explicitly pass the necessary global objects to the workers.
parallel_results_list <- future_map(
  .x = split_ids,
  .f = function(i) {
    # Call the optimized function
    process_single_split_optimized(
      split_set = i,
      all_splits_data = all_splits,
      df_data = df,
      terms_lm = terms_lm, 
      pc_counts_lm = pc_counts_lm, 
      terms_gam = terms_gam, 
      pc_counts_gam = pc_counts_gam, 
      best_params_rf = best_params_rf, 
      best_params_xgb = best_params_xgb
    )
  },
  .options = furrr_options(seed = TRUE, packages = c("caret", "mgcv", "mdatools", "ranger", "xgboost", "dplyr", "stringr")),
  .progress = TRUE # Use progressr handler
)

end_time <- Sys.time()
print(end_time - start_time)
plan(sequential) # Shut down parallel workers


# NEW COMPUTER, UNDERVOLTED 20MV, UPDATED MATH LIBRARY:
# outliers removed, LOOCV, 16 workers
# Time for 500 iterations: Time difference of 1.048784 hours with fixed 1000 boosting rounds (forgot to ref tune)

# outliers remove, LOOCV, 500, actually 2000 boosting rounds, 16 workers
# > print(end_time - start_time)
Time difference of 1.082068 hours

################################################################################

# combine results

################################################################################

# C. Combine results from the parallel run
cat("Aggregating results from parallel runs...\n")
all_results <- lapply(parallel_results_list, function(x) x$results)
all_predictions_list <- lapply(parallel_results_list, function(x) x$predictions)
all_importance_data <- lapply(parallel_results_list, function(x) x$importance)

# Combine metrics, predictions, and importance data into final dataframes
final_results <- combine_all_results(unlist(all_results, recursive = FALSE))
final_predictions <- do.call(rbind, all_predictions_list)
final_importance_data <- dplyr::bind_rows(all_importance_data)


# try different version....>!
all_results_means <- final_results %>%
  group_by(SplitSet, Model, ModelType) %>%
  summarize(
    R2 = mean(R2),
    RMSE = mean(RMSE),
    RPD = mean(RPD),
    Bias = mean(Bias),
    PercentRMSE = mean(PercentRMSE),
    # KEY CHANGE: Calculate Min and Max, then combine into a range string
    Min_Components = min(Components, na.rm = TRUE),
    Max_Components = max(Components, na.rm = TRUE),
    N = n()
  ) %>%
  ungroup() %>%
  # Create a clean string for your final table
  mutate(
    Component_Range = case_when(
      is.infinite(Min_Components) ~ NA_character_, # Handle cases with all NAs
      Min_Components == Max_Components ~ as.character(Min_Components),
      TRUE ~ paste(Min_Components, Max_Components, sep = "-")
    )
  )



# ================================================================================= #
# 3. SIMPLE MODELS (PARALLELIZED) AND FINAL COMBINATION
# ================================================================================= #

# Filter data and generate splits
df_simple <- df[complete.cases(df$structure_weight), ]
set.seed(6)
all_splits_simple <- generate_multiple_splits(df_simple, n_splits)


## 1. Define the "Worker" Function for a Single Split
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
    actual = testing$read_age, 
    simple_lm_pred = numeric(length(testing$read_age)),
    simple_gam_pred = numeric(length(testing$read_age)), 
    simple_lm_ncomp = 3,
    simple_gam_ncomp = 3
  )
  
  fold_results <- data.frame(
    Split = split_num, Fold = i, Model = c("Simple lm", "Simple gam"),
    R2 = numeric(2), RMSE = numeric(2), RPD = numeric(2),
    Bias = numeric(2), PercentRMSE = numeric(2)
  )
  
  # Simple linear model
  mod_lm <- lm(data = calibrate, read_age ~ length + structure_weight + weight, na.action = na.omit)
  preds_lm <- predict(mod_lm, newdata = testing)
  fold_preds$simple_lm_pred <- preds_lm
  
  # Calculate metrics for LM
  fold_results$RMSE[1] <- caret::RMSE(pred = preds_lm, obs = testing$read_age)
  fold_results$PercentRMSE[1] <- fold_results$RMSE[1] / max(testing$read_age) * 100
  RSS_lm <- sum((testing$read_age - preds_lm)^2); TSS_lm <- sum((testing$read_age - mean(testing$read_age))^2)
  fold_results$R2[1] <- 1 - (RSS_lm / TSS_lm)
  fold_results$RPD[1] <- calculate_rpd(testing$read_age, preds_lm)
  fold_results$Bias[1] <- calculate_bias(testing$read_age, preds_lm)
  
  # Simple GAM model
  mod_gam <- gam(data = calibrate, read_age ~ s(length, k = 4) + s(structure_weight, k = 4) + s(weight, k = 4), method = "REML")
  preds_gam <- predict(mod_gam, newdata = testing)
  fold_preds$simple_gam_pred <- preds_gam
  
  # Calculate metrics for GAM
  fold_results$RMSE[2] <- caret::RMSE(pred = preds_gam, obs = testing$read_age)
  fold_results$PercentRMSE[2] <- fold_results$RMSE[2] / max(testing$read_age) * 100
  RSS_gam <- sum((testing$read_age - preds_gam)^2); TSS_gam <- sum((testing$read_age - mean(testing$read_age))^2)
  fold_results$R2[2] <- 1 - (RSS_gam / TSS_gam)
  fold_results$RPD[2] <- calculate_rpd(testing$read_age, preds_gam)
  fold_results$Bias[2] <- calculate_bias(testing$read_age, preds_gam)
  
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

################################################################################

## 2. Run the Simple Models in Parallel

################################################################################

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

################################################################################

## 3. Combine Results and Post-Process

################################################################################

cat("Aggregating results from simple models...\n")

# Extract the results and predictions from the list returned by the parallel run
simple_metrics_list <- lapply(parallel_results_simple, function(x) x$results)
simple_predictions <- lapply(parallel_results_simple, function(x) x$predictions)

# Combine into single data frames
simple_metrics <- dplyr::bind_rows(simple_metrics_list)

simple_results_means <- simple_metrics %>%
  group_by(Split, Model) %>%
  summarize(
    R2 = mean(R2),
    RMSE = mean(RMSE),
    RPD = mean(RPD),
    Bias = mean(Bias),
    PercentRMSE = mean(PercentRMSE),
    Components = mean(Components),
    N = n(),
    ModelType = "Simple"
  ) %>%
  rename(
    SplitSet = Split
  ) %>%
  ungroup() %>% 
  rename(Min_Components = Components) %>%
  # 2. Create the other two necessary component columns. Since it's always 3, this is simple.
  mutate(
    Max_Components = Min_Components,
    Component_Range = as.character(Min_Components)
  ) %>%
  # 3. Select and reorder the columns to perfectly match the all_results_means dataframe
  select(
    SplitSet, 
    Model, 
    ModelType, 
    R2, 
    RMSE, 
    RPD, 
    Bias, 
    PercentRMSE, 
    Min_Components, 
    Max_Components, 
    N, 
    Component_Range
  )

# Combine with the complex model results
all_results_means_final <- rbind(all_results_means, simple_results_means)

# format predictions

simple_predictions_formatted <- list()
for (split_set in seq_along(simple_predictions)) {
  for (fold in seq_along(simple_predictions[[split_set]])) {
    fold_preds <- simple_predictions[[split_set]][[fold]]
    # Simple LM predictions
    simple_predictions_formatted[[paste("simple_lm", split_set, fold)]] <- data.frame(
      split_set = split_set, 
      fold = fold, 
      model_type = "Simple", 
      model_variant = "Simple LM",
      specimen_number = fold_preds$specimen_number, 
      actual = fold_preds$actual, 
      predicted = fold_preds$simple_lm_pred, 
      components = 3
    )
    # Simple GAM predictions
    simple_predictions_formatted[[paste("simple_gam", split_set, fold)]] <- data.frame(
      split_set = split_set, 
      fold = fold, 
      model_type = "Simple", 
      model_variant = "Simple GAM",
      specimen_number = fold_preds$specimen_number, 
      actual = fold_preds$actual, 
      predicted = fold_preds$simple_gam_pred,
      components = 3
    )
  }
}
simple_predictions_df <- do.call(rbind, simple_predictions_formatted)
rownames(simple_predictions_df) <- NULL

# Combine with complex model predictions
all_predictions_final <- rbind(final_predictions, simple_predictions_df)


################################################################################

# Save final objects

################################################################################


saveRDS(all_results_means_final, paste0("RDS_dataframes/all_results_means_parallel_LOOCV_2000_", Sys.Date(), ".RDS"))
saveRDS(all_predictions_final, paste0("RDS_dataframes/all_predictions_parallel_LOOCV_2000_", Sys.Date(), ".RDS"))
saveRDS(final_importance_data, paste0("RDS_dataframes/final_importance_data_parallel_LOOCV_2000_", Sys.Date(), ".RDS"))


# IF FILTERED WAVENUMBERS
# saveRDS(all_results_means_final, paste0("RDS_dataframes/filtered_all_results_means_parallel", Sys.Date(), ".RDS"))
# saveRDS(all_predictions_final, paste0("RDS_dataframes/filtered_all_predictions_parallel", Sys.Date(), ".RDS"))
# saveRDS(final_importance_data, paste0("RDS_dataframes/filtered_final_importance_data_parallel", Sys.Date(), ".RDS"))


# saveRDS(all_results_means_final, paste0("RDS_dataframes/all_results_means_parallel", Sys.Date(), ".RDS"))
# saveRDS(all_predictions_final, paste0("RDS_dataframes/all_predictions_parallel", Sys.Date(), ".RDS"))
# saveRDS(final_importance_data, paste0("RDS_dataframes/final_importance_data_parallel", Sys.Date(), ".RDS"))
