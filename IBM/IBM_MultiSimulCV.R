# Packages####
packages <- c("caret", "doParallel", "dplyr","devtools", "dplyr","ggplot2", "janitor", "mdatools", "mgcv", "MuMIn", "purrr", "ranger", "readxl","stringr","tidyr", "viridis", "xgboost")
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  utils::install.packages(pkgs = packages[!installed_packages])
}
invisible(lapply(packages, library, character.only = TRUE)) # load all packages in list
rm(installed_packages, packages)

# icp_ms <- readRDS("RDS_dataframes/ICP-MS_updated.RDS")
ibm <- readRDS("RDS_dataframes/IBM_proc_filter.RDS") %>%
  select(-c(haul, date_collected, read_age, test_age, final_age, scan_name, timestamp, file_name, session_title, file_path))
ages <- read_xlsx("metadata/ibm_ages_10032025.xlsx") %>% 
  select(-c(avg_age, hatch_est)) %>%
  mutate(
    # Calculate the mean of age1, age2, and age3 for each row
    avg_age = rowMeans(select(., age1, age2, age3), na.rm = TRUE),
    # Calculate hatch estimate
    hatch_est = julian_date - avg_age
  ) %>% 
  # remove rows with NA for age1
  filter(!is.na(age1)) %>%
  # Group by row to perform row-wise operations
  rowwise() %>%
  # Calculate the standard deviation of the age estimates for each specimen
  mutate(
    age_sd = sd(c(age1, age2, age3), na.rm = TRUE)
  ) %>%
  # Ungroup to return to normal dataframe operations
  ungroup() %>%
  # Calculate the coefficient of variation (CV) in percent
  mutate(
    age_cv_percent = (age_sd / avg_age) * 100
  ) %>%
  # Filter to keep rows with a CV of 10% or less, or where CV is not applicable
  filter(age_cv_percent <= 10 | is.na(age_cv_percent)) %>% 
  select(-age_sd, age_cv_percent)



df <- left_join(
  ages, 
  ibm, 
  by = c("specimen" = "specimen")
)

# remove missing scan specimen, only use NEW scans
df <- df %>%
  filter(specimen != 425970, run_number == 2)

rm(ages, ibm)

# # Group by specimen and count the number of distinct runs
# run_counts <- ibm_with_ages %>%
#   group_by(specimen) %>%
#   summarise(
#     number_of_runs = n_distinct(run_number),
#     .groups = 'drop' # Recommended to drop the grouping after summarising
#   )
# 
# # Filter for any specimens that do NOT have exactly 2 runs
# specimens_with_issues <- run_counts %>%
#   filter(number_of_runs != 2)
# 
# # Check the result
# if (nrow(specimens_with_issues) == 0) {
#   cat("Success! All specimens have exactly two runs. ✅\n")
# } else {
#   cat("The following specimens do not have two runs:\n")
#   print(specimens_with_issues)
# }

# 425970 is missing scans, won't use.... :[]


names(df)
# wavenumbers are in columns 30:962

pca_temp <- mdatools::pca(df[, 30:ncol(df)])

pc_df <- data.frame(PC1 = rep(0,nrow(df)))
for (i in 1:10) {
  pc_df[, paste0("PC", i)] <- pca_temp$res$cal$scores[, i]
  rm(i)
}
pc_df <- cbind(pc_df,df)
global_lm <- lm(data = pc_df, avg_age ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10)
global_gam <- gam(data = pc_df, avg_age ~ s(PC1,k = 4) + s(PC2,k = 4) + s(PC3,k = 4) + s(PC4,k = 4) + s(PC5,k = 4) + s(PC6,k = 4) + s(PC7, k = 4) + s(PC8, k = 4) + s(PC9, k = 4) + s(PC10, k = 4))

options(na.action = "na.fail")
dredge_lm <- dredge(global_lm)
top10_lm <- get.models((dredge_lm), subset = 1:10)
dredge_gam <- dredge(global_gam)
top10_gam <- get.models(dredge_gam, subset = 1:10)
terms_lm <- list()
terms_gam <- list()

for(i in 1:10){
  terms_lm[[i]] <- top10_lm[[i]]$terms # extract terms for for-loop equation = usage
  terms_gam[[i]] <- top10_gam[[i]]$formula
}
rm(global_gam, global_lm, top10_gam, top10_lm, pc_df, pca_temp, dredge_gam,dredge_lm, i)




generate_multiple_splits <- function(data = df, n_splits) {
  all_splits <- list()
  for (split_id in 1:n_splits) {
    all_splits[[split_id]] <- caret::createFolds(data$avg_age, k = 10, list = TRUE, returnTrain = FALSE) # 10-fold CV
  }
  return(all_splits)
}
calculate_rpd <- function(observed, predicted) {
  # Standard deviation of observed values
  sd_observed <- sd(observed)
  # Root Mean Square Error
  rmse <- sqrt(mean((observed - predicted)^2))
  # RPD
  rpd <- sd_observed / rmse
  return(rpd)
}
calculate_bias <- function(observed, predicted) {
  bias <- mean(predicted - observed)
  return(bias)
}


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

# ====================================================================================#
# ====================================================================================#
# Functions for each model type ======================================================
# ====================================================================================#
# ====================================================================================#
run_lm_models <- function(cal, test, terms_lm) {
  splits_results_lm <- data.frame()
  all_predictions <- list()  # To store predictions for all folds
  for (i in 1:10) { # 10-fold CV
    calibrate <- cal[[i]]
    testing <- test[[i]]
    fold_results <- data.frame(
      Fold = i,
      Model = paste0("Linear ", 1:10),
      R2 = numeric(10),
      RMSE = numeric(10),
      RPD = numeric(10),
      Bias = numeric(10),
      PercentRMSE = numeric(10)
    )
    # Initialize prediction storage for this fold
    fold_preds <- list(
      specimen_number = testing$specimen,
      actual = testing$avg_age,
      model_preds = vector("list", 10)  # One slot per model combination
    )
    
    for (j in 1:10) {
      mod <- lm(data = calibrate, terms_lm[[j]])
      preds <- predict(mod, newdata = testing)
      
      # Store metrics
      fold_results$RMSE[j] <- caret::RMSE(pred = preds, obs = testing[, "avg_age"])
      fold_results$PercentRMSE[j] <- fold_results$RMSE[j] / max(testing$avg_age) * 100
      RSS <- sum((testing$avg_age - preds)^2)
      TSS <- sum((testing$avg_age - mean(testing$avg_age))^2)
      fold_results$R2[j] <- 1 - (RSS / TSS)
      fold_results$RPD[j] <- calculate_rpd(testing$avg_age, preds)
      fold_results$Bias[j] <- calculate_bias(testing$avg_age, preds)
      
      # Store predictions
      fold_preds$model_preds[[j]] <- preds
    }
    
    splits_results_lm <- rbind(splits_results_lm, fold_results)
    all_predictions[[i]] <- fold_preds
  }
  
  splits_results_lm$ModelType <- "LM"
  return(list(results = splits_results_lm, predictions = all_predictions))
}
# ================================================================================#
# ================================================================================#
run_gam_models <- function(cal, test, terms_gam) {
  splits_results_gam <- data.frame()
  all_predictions <- list()  # To store predictions for all folds
  
  for (i in 1:10) { # 10-fold CV
    calibrate <- cal[[i]]
    testing <- test[[i]]
    
    fold_results <- data.frame(
      Fold = i,
      Model = paste0("GAM ", 1:10),
      R2 = numeric(10),
      RMSE = numeric(10),
      RPD = numeric(10),
      Bias = numeric(10),
      PercentRMSE = numeric(10)
    )
    
    # Initialize prediction storage for this fold
    fold_preds <- list(
      specimen_number = testing$specimen,
      actual = testing$avg_age,
      model_preds = vector("list", 10)  # One slot per model combination
    )
    
    for (j in 1:10) {
      mod <- gam(data = calibrate, terms_gam[[j]], method = "REML")
      preds <- predict(mod, newdata = testing)
      
      # Store metrics
      fold_results$RMSE[j] <- caret::RMSE(pred = preds, obs = testing[, "avg_age"])
      fold_results$PercentRMSE[j] <- fold_results$RMSE[j] / max(testing$avg_age) * 100
      RSS <- sum((testing$avg_age - preds)^2)
      TSS <- sum((testing$avg_age - mean(testing$avg_age))^2)
      fold_results$R2[j] <- 1 - (RSS / TSS)
      fold_results$RPD[j] <- calculate_rpd(testing$avg_age, preds)
      fold_results$Bias[j] <- calculate_bias(testing$avg_age, preds)
      
      # Store predictions
      fold_preds$model_preds[[j]] <- preds
    }
    
    splits_results_gam <- rbind(splits_results_gam, fold_results)
    all_predictions[[i]] <- fold_preds
  }
  
  splits_results_gam$ModelType <- "GAM"
  
  # Add PC counts
  pc_counts_gam <- sapply(terms_gam, function(formula) {
    formula_str <- as.character(formula)[3]
    pc_count <- stringr::str_count(formula_str, "PC\\d+")
    return(pc_count)
  })
  
  components_map <- data.frame(
    Model = paste0("GAM ", 1:10),
    Components = pc_counts_gam
  )
  
  splits_results_gam <- merge(splits_results_gam, components_map, by = "Model", all.x = TRUE)
  
  return(list(results = splits_results_gam, predictions = all_predictions))
}
# ================================================================================#
# ================================================================================#
run_pls_models <- function(cal, test) {
  splits_results_pls <- data.frame()
  all_predictions <- list()  # To store predictions separately
  all_importance_fold <- list() # List to store importance from each fold
  
  for (i in 1:10) { # 10-fold CV
    calibrate <- cal[[i]]
    testing <- test[[i]]
    
    # Initialize fold results (without prediction columns)
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
    
    # Store predictions for this fold
    fold_preds <- list(
      specimen_number = testing$specimen,
      actual = testing$avg_age,
      pls_pred = numeric(length(testing$avg_age)),
      vip_pred = numeric(length(testing$avg_age))
    )
    
    # PLS model
    mod <- mdatools::pls(calibrate[, 40:ncol(calibrate)], calibrate[, "avg_age"],
                         scale = F, center = T, cv = 1,
                         x.test = testing[, 40:ncol(testing)],
                         y.test = testing[, "avg_age"])
    ncomp <- mod$ncomp.selected
    
    
    # Wavenumber data
    wavenumbers <- as.numeric(colnames(calibrate[, 40:ncol(calibrate)]))
    vip_scores <- vipscores(mod)
    importance_df <- data.frame(
      fold = i,
      method = "PLS-VIP",
      wavenumber = wavenumbers,
      importance = vip_scores
    )
    all_importance_fold[[i]] <- importance_df
    
    
    # Store metrics
    fold_results$R2[1] <- mod$testres$r2[[ncomp]]
    fold_results$RMSE[1] <- mod$testres$rmse[[ncomp]]
    fold_results$RPD[1] <- mod$testres$rpd[[ncomp]]
    fold_results$Bias[1] <- mod$testres$bias[[ncomp]]
    fold_results$PercentRMSE[1] <- mod$testres$rmse[[ncomp]] / max(testing$avg_age) * 100
    fold_results$Components[1] <- ncomp
    
    # Store predictions
    fold_preds$pls_pred <- mod$testres$y.pred[, ncomp,]
    
    # VIP model #
    vip <- as.data.frame(vipscores(mod))
    mod <- mdatools::pls(calibrate[, 40:ncol(calibrate)], calibrate[, "avg_age"],
                         scale = F, center = T, cv = 1,
                         x.test = testing[, 40:ncol(testing)],
                         y.test = testing[, "avg_age"],
                         exclcols = vip$V1 < 0.5)
    ncomp <- mod$ncomp.selected
    
    # Store metrics
    fold_results$R2[2] <- mod$testres$r2[[ncomp]]
    fold_results$RMSE[2] <- mod$testres$rmse[[ncomp]]
    fold_results$RPD[2] <- mod$testres$rpd[[ncomp]]
    fold_results$Bias[2] <- mod$testres$bias[[ncomp]]
    fold_results$PercentRMSE[2] <- mod$testres$rmse[[ncomp]] / max(testing$avg_age) * 100
    fold_results$Components[2] <- ncomp
    
    # Store predictions
    fold_preds$vip_pred <- mod$testres$y.pred[, ncomp,]
    
    # Append to results
    splits_results_pls <- rbind(splits_results_pls, fold_results)
    all_predictions[[i]] <- fold_preds
  }
  
  splits_results_pls$ModelType <- "PLS"
  final_importance_df <- dplyr::bind_rows(all_importance_fold)
  return(list(results = splits_results_pls, predictions = all_predictions, importance = final_importance_df))
}
# ================================================================================#
# ================================================================================#
run_xgb_models <- function(cal, test) {
  xgb_results_df <- data.frame()
  all_predictions <- list()
  all_importance_fold <- list()
  
  for (i in 1:10) { # 10-fold CV
    calibrate <- cal[[i]]
    testing <- test[[i]]
    
    # Extract features and target
    x_train <- as.matrix(calibrate[, 40:ncol(calibrate)])
    x_test <- as.matrix(testing[, 40:ncol(calibrate)])
    y_train <- calibrate[, "avg_age"]
    y_test <- testing[, "avg_age"]
    
    # Convert to DMatrix format
    dtrain <- xgb.DMatrix(data = x_train, label = y_train)
    dtest <- xgb.DMatrix(data = x_test, label = y_test)
    
    # Set XGBoost parameters
    params <- list(
      objective = "reg:squarederror",
      booster = "gbtree",
      eta = 0.1, # Fixed small learning rate
      max_depth = best_params_xgb$max_depth,
      min_child_weight = best_params_xgb$min_child_weight,
      subsample = best_params_xgb$subsample,
      gamma = best_params_xgb$gamma,
      colsample_bytree = best_params_xgb$colsample_bytree,
      nthread = parallel::detectCores() - 1
    )
    
    # Train the model
    xgb_model <- xgb.train(
      params = params,
      data = dtrain,
      nrounds = 1000,
      watchlist = list(train = dtrain, test = dtest),
      early_stopping_rounds = 20,
      verbose = 0
    )
    
    # wavenumbers
    wavenumbers_char <- colnames(x_train)
    importance_matrix <- xgb.importance(model = xgb_model)
    # Create a full importance data frame, ensuring all wavenumbers are included
    importance_df <- data.frame(Feature = wavenumbers_char) %>%
      left_join(importance_matrix, by = "Feature") %>%
      mutate(
        fold = i,
        method = "XGBoost",
        wavenumber = as.numeric(Feature),
        importance = ifelse(is.na(Gain), 0, Gain) # Use Gain, default to 0 if not used
      ) %>%
      select(fold, method, wavenumber, importance)
    
    all_importance_fold[[i]] <- importance_df
    
    # Make predictions
    preds <- predict(xgb_model, dtest)
    
    # Store predictions for this fold
    fold_preds <- list(
      specimen_number = testing$specimen,
      actual = y_test,
      xgb_pred = preds
    )
    
    # Calculate metrics
    r2 <- cor(preds, y_test)^2
    rmse_val <- sqrt(mean((preds - y_test)^2))
    
    # Create results for this fold
    fold_results <- data.frame(
      Fold = i,
      Model = "XGB",
      R2 = r2,
      RMSE = rmse_val,
      RPD = calculate_rpd(y_test, preds),
      Bias = calculate_bias(y_test, preds),
      PercentRMSE = rmse_val / max(y_test) * 100,
      Components = xgb_model$best_iteration
    )
    
    # Append results
    xgb_results_df <- rbind(xgb_results_df, fold_results)
    all_predictions[[i]] <- fold_preds
  }
  
  final_importance_df <- dplyr::bind_rows(all_importance_fold)
  xgb_results_df$ModelType <- "XGB"
  return(list(results = xgb_results_df, predictions = all_predictions, importance = final_importance_df))
}
# ================================================================================#
# ================================================================================#
run_rf_models <- function(cal, test) {
  splits_results_rf <- data.frame()
  all_predictions <- list()
  all_importance_fold <- list() 
  
  for (i in 1:10) { # 10-fold CV
    calibrate <- cal[[i]]
    testing <- test[[i]]
    
    # Initialize prediction storage
    fold_preds <- list(
      specimen_number = testing$specimen,
      actual = testing$avg_age,
      rf_pred = numeric(length(testing$avg_age))
    )
    
    fold_results <- data.frame(
      Fold = i,
      Model = "RF",
      R2 = numeric(1),
      RMSE = numeric(1),
      RPD = numeric(1),
      Bias = numeric(1),
      PercentRMSE = numeric(1)
    )
    # base RF model without tuning
    mod <- ranger(
      x = calibrate[, 40:ncol(calibrate)],
      y = calibrate$avg_age,
      mtry = best_params_rf$mtry,
      min.node.size = best_params_rf$min.node.size,
      seed = 6,
      importance = 'permutation' # Calculate variable importance
    )
    
    # Wavenumbers
    wavenumbers <- as.numeric(colnames(calibrate[, 40:ncol(calibrate)]))
    importance_scores <- ranger::importance(mod)
    importance_df <- data.frame(
      fold = i,
      method = "Random Forest",
      wavenumber = wavenumbers,
      importance = importance_scores
    )
    all_importance_fold[[i]] <- importance_df
    
    
    # Make predictions
    preds <- predict(mod, data = testing)$predictions
    fold_preds$rf_pred <- preds
    
    # Calculate metrics
    RSS <- sum((testing$avg_age - preds)^2)
    TSS <- sum((testing$avg_age - mean(testing$avg_age))^2)
    
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
  return(list(results = splits_results_rf, predictions = all_predictions,  importance = final_importance_df))
}
# ================================================================================#
# ================================================================================#
# Functions to combine results from all models and extract predictions=============
# ================================================================================#
# ================================================================================#
combine_all_results <- function(model_results_list) {
  # Extract results dataframes from each model
  all_dfs <- lapply(model_results_list, function(x) {
    if ("results" %in% names(x)) x$results else x
  })
  
  # Standardize columns
  all_cols <- unique(unlist(lapply(all_dfs, colnames)))
  standardized_dfs <- lapply(all_dfs, function(df) {
    missing_cols <- setdiff(all_cols, colnames(df))
    for (col in missing_cols) df[[col]] <- NA
    df[, all_cols]
  })
  
  # Combine and return
  do.call(rbind, standardized_dfs)
}

# Modified extract_all_predictions function
extract_all_predictions <- function(model_results, split_set) { # Added split_set argument
  # Initialize list to store all predictions
  all_preds <- list()
  
  # Process each model type in this split
  for (model_type in names(model_results)) {
    # Skip if no predictions available
    # if (is.null(model_results[[model_type]]$predictions)) next # Changed from results_list to model_results
    
    # Get predictions for all folds of this model type
    pred_list <- model_results[[model_type]]$predictions # Changed from results_list to model_results
    
    # Process each fold
    for (fold in seq_along(pred_list)) {
      fold_preds <- pred_list[[fold]]
      
      # Handle each model type's prediction structure differently
      if (model_type == "lm") {
        # Linear models with 10 variants
        for (variant in 1:10) {
          all_preds[[paste("lm", split_set, fold, variant)]] <- data.frame(
            split_set = split_set, # Use the passed split_set argument
            fold = fold,
            model_type = "LM",
            model_variant = paste("LM", variant),
            specimen_number = fold_preds$specimen_number, # Assuming specimen_number is included
            actual = fold_preds$actual,
            predicted = fold_preds$model_preds[[variant]]
          )
        }
      }
      else if (model_type == "gam") {
        # GAM models with 10 variants
        for (variant in 1:10) {
          all_preds[[paste("gam", split_set, fold, variant)]] <- data.frame(
            split_set = split_set, # Use the passed split_set argument
            fold = fold,
            model_type = "GAM",
            model_variant = paste("GAM", variant),
            specimen_number = fold_preds$specimen_number, # Assuming specimen_number is included
            actual = fold_preds$actual,
            predicted = fold_preds$model_preds[[variant]]
          )
        }
      }
      else if (model_type == "pls") {
        # Standard PLS model
        if (!is.null(fold_preds$pls_pred) && length(fold_preds$pls_pred) == length(fold_preds$actual)) {
          all_preds[[paste("pls_standard", split_set, fold)]] <- data.frame(
            split_set = split_set, # Use the passed split_set argument
            fold = fold,
            model_type = "PLS", # Model type remains PLS
            model_variant = "PLS", # Variant indicates standard PLS
            specimen_number = fold_preds$specimen_number, # Assuming specimen_number is included
            actual = fold_preds$actual,
            predicted = fold_preds$pls_pred
          )
        }
        
        # PLS-VIP model
        if (!is.null(fold_preds$vip_pred) && length(fold_preds$vip_pred) == length(fold_preds$actual)) {
          all_preds[[paste("pls_vip", split_set, fold)]] <- data.frame(
            split_set = split_set, # Use the passed split_set argument
            fold = fold,
            model_type = "PLS", # Model type can remain PLS
            model_variant = "PLS-VIP", # Variant distinguishes VIP
            specimen_number = fold_preds$specimen_number, # Assuming specimen_number is included
            actual = fold_preds$actual,
            predicted = fold_preds$vip_pred
          )
        }
      }
      else if (model_type == "rf") {
        # Random Forest
        all_preds[[paste("rf", split_set, fold)]] <- data.frame(
          split_set = split_set, # Use the passed split_set argument
          fold = fold,
          model_type = "RF",
          model_variant = "RF",
          specimen_number = fold_preds$specimen_number, # Assuming specimen_number is included
          actual = fold_preds$actual,
          predicted = fold_preds$rf_pred
        )
      }
      else if (model_type == "xgb") {
        # XGBoost model
        all_preds[[paste("xgb", split_set, fold)]] <- data.frame(
          split_set = split_set, # Use the passed split_set argument
          fold = fold,
          model_type = "XGB",
          model_variant = "XGBoost",
          specimen_number = fold_preds$specimen_number, # Assuming specimen_number is included
          actual = fold_preds$actual,
          predicted = fold_preds$xgb_pred
        )
      }
    }
  }
  
  # Combine all predictions into one dataframe
  final_predictions <- do.call(rbind, all_preds)
  if (!is.null(final_predictions) && nrow(final_predictions) > 0) {
    rownames(final_predictions) <- NULL
  }
  
  return(final_predictions)
}
# ================================================================================#
# ================================================================================#
# RUN MODELS ==============================
# ================================================================================#
# ================================================================================#


n_splits <- 10
set.seed(6)
all_splits <- generate_multiple_splits(df, n_splits)
all_results <- list()
all_predictions <- list()
all_importance_data <- list()
Sys.time()

for (split_set in 1:n_splits) {
  cat("Processing split set", split_set, "of", n_splits, "\n")
  
  # Create cal/test data (your existing code)
  splits <- all_splits[[split_set]]
  cal <- test <- vector("list", 10)
  pca_importance_split <- list()
  
  
  for (i in 1:10) {
    pc.mod <- preProcess(df[-splits[[i]], -c(1:29)], method = c("pca","center"), pcaComp = 10)
    cal[[i]] <- cbind(predict(pc.mod, df[-splits[[i]], -c(1:29)]), df[-splits[[i]], ])
    test[[i]] <- cbind(predict(pc.mod, df[splits[[i]], -c(1:29)]), df[splits[[i]], ])
    
    pca_loadings <- pc.mod$rotation
    variances <- pc.mod$std[1:10]^2
    
    # 2. Calculate the proportion of variance explained by each PC
    prop_variance <- variances / sum(variances)
    
    # 3. Calculate the importance score weighted by the proportion of variance
    # The %*% operator efficiently multiplies and sums the values for each wavenumber
    weighted_importance <- abs(pca_loadings[, 1:10]) %*% prop_variance
    
    # 4. Create the final data frame for this fold
    pca_importance_fold <- data.frame(
      fold = i,
      method = "PCA Loadings",
      wavenumber = as.numeric(rownames(pca_loadings)),
      importance = weighted_importance[, 1] # The result is a one-column matrix
    )
    
    pca_importance_split[[i]] <- pca_importance_fold
  }
  
  # Run models and store results
  model_results <- list(
    lm = run_lm_models(cal, test, terms_lm),
    gam = run_gam_models(cal, test, terms_gam),
    pls = run_pls_models(cal, test),
    rf = run_rf_models(cal, test),
    xgb = run_xgb_models(cal, test)
  )
  
  # importance
  # 1. Get PCA importance from the list we created
  pca_importance_df <- dplyr::bind_rows(pca_importance_split)
  
  # 2. Extract importance DFs from the model_results list
  pls_importance_df <- model_results$pls$importance
  rf_importance_df <- model_results$rf$importance
  xgb_importance_df <- model_results$xgb$importance
  
  # 3. Add the split_set ID to each importance dataframe
  pca_importance_df$split_set <- split_set
  pls_importance_df$split_set <- split_set
  rf_importance_df$split_set <- split_set
  xgb_importance_df$split_set <- split_set
  
  # 4. Bind them all together for this split_set and add to the master list
  all_importance_data[[split_set]] <- dplyr::bind_rows(
    pca_importance_df,
    pls_importance_df,
    rf_importance_df,
    xgb_importance_df
  )
  
  # Add split set ID
  for (m in names(model_results)) {
    model_results[[m]]$results$SplitSet <- split_set
  }
  
  # Store results and predictions
  all_results[[split_set]] <- model_results
  all_predictions[[split_set]] <- extract_all_predictions(model_results, split_set) # 
}

Sys.time() 

# Combine metrics from all models
final_results <- combine_all_results(unlist(all_results, recursive = FALSE))
final_predictions <- do.call(rbind, all_predictions)
final_importance_data <- dplyr::bind_rows(all_importance_data)


saveRDS(final_importance_data, paste0("RDS_dataframes/IBM_importance_", Sys.Date(), ".RDS"))

# ================================================================================#
# ================================================================================#
# calculate mean metrics for each simulation ======================================
# ================================================================================#
# ================================================================================#
all_results_means <- final_results %>%
  group_by(SplitSet, Model, ModelType) %>%
  summarize(
    R2 = mean(R2),
    RMSE = mean(RMSE),
    RPD = mean(RPD),
    Bias = mean(Bias),
    PercentRMSE = mean(PercentRMSE),
    Components = mean(Components),
    N = n()
  ) %>%
  ungroup()
rm(i,m,split_set,splits,cal,test, model_results, pc.mod)
# =================================================================================#
# =================================================================================#
# Simple Models  ========
# =================================================================================#
# =================================================================================#
df_simple <- df[complete.cases(df$structure_weight), ] # filter missing structure weight specimens

# HAVE WEIGHTS FOR ALL SPECIMENs, SICK
set.seed(6)
all_splits_simple <- generate_multiple_splits(df_simple, n_splits) # generate new splits with missing specimens

data <- df_simple
splits_list <- all_splits_simple
n_splits <- 10

run_multiple_splits_simple_models <- function(data, splits_list, n_splits = 10) {
  # Create a dataframe to store all results
  all_results_simple <- data.frame()
  all_predictions_simple <- list() # To store predictions for all splits and folds
  
  # Loop through each split
  for (split_num in 1:n_splits) {
    cat("Processing split", split_num, "of", n_splits, "\n")
    
    # Get the current split
    current_split <- splits_list[[split_num]]
    
    # Initialize results dataframe for this split
    split_results_simple <- data.frame()
    split_predictions_simple <- list() # To store predictions for all folds in this split
    
    # Process each fold
    for (i in 1:10) {
      i = 1
      # Get fold name and test indices
      fold_name <- names(current_split)[i]
      test_indices <- current_split[[i]]
      
      # Create test and calibration datasets
      testing <- data[test_indices, ]
      calibrate <- data[-test_indices, ]
      
      # Initialize prediction storage for this fold
      fold_preds <- list(
        specimen_number = testing$specimen,
        actual = testing$avg_age,
        simple_lm_pred = numeric(length(testing$avg_age)),
        simple_gam_pred = numeric(length(testing$avg_age))
      )
      
      # Create dataframe for this fold's results
      fold_results <- data.frame(
        Split = split_num,
        Fold = i,
        Model = c("Simple lm", "Simple gam"),
        R2 = numeric(2),
        RMSE = numeric(2),
        RPD = numeric(2),
        Bias = numeric(2),
        PercentRMSE = numeric(2)
      )
      
      # Simple linear model
      mod <- lm(data = calibrate, avg_age ~ length + structure_weight)
      preds_lm <- predict(mod, newdata = testing)
      fold_preds$simple_lm_pred <- preds_lm
      
      # Calculate metrics for LM
      fold_results$RMSE[1] <- caret::RMSE(pred = preds_lm, obs = testing[["avg_age"]])
      fold_results$PercentRMSE[1] <- fold_results$RMSE[1] / max(testing$avg_age) * 100
      RSS_lm <- sum((testing$avg_age - preds_lm)^2)
      TSS_lm <- sum((testing$avg_age - mean(testing$avg_age))^2)
      fold_results$R2[1] <- 1 - (RSS_lm / TSS_lm)
      fold_results$RPD[1] <- calculate_rpd(testing$avg_age, preds_lm)
      fold_results$Bias[1] <- calculate_bias(testing$avg_age, preds_lm)
      
      # Simple GAM model
      mod <- gam(data = calibrate, avg_age ~ s(length, k = 4) + s(structure_weight, k = 4), method = "REML")
      preds_gam <- predict(mod, newdata = testing)
      fold_preds$simple_gam_pred <- preds_gam
      
      # Calculate metrics for GAM
      fold_results$RMSE[2] <- caret::RMSE(pred = preds_gam, obs = testing[["avg_age"]])
      fold_results$PercentRMSE[2] <- fold_results$RMSE[2] / max(testing$avg_age) * 100
      RSS_gam <- sum((testing$avg_age - preds_gam)^2)
      TSS_gam <- sum((testing$avg_age - mean(testing$avg_age))^2)
      fold_results$R2[2] <- 1 - (RSS_gam / TSS_gam)
      fold_results$RPD[2] <- calculate_rpd(testing$avg_age, preds_gam)
      fold_results$Bias[2] <- calculate_bias(testing$avg_age, preds_gam)
      
      # Add Components
      fold_results$Components <- 2  # length, structure_weight, weight
      fold_results$ModelType <- "Simple"
      
      # Append to split results
      split_results_simple <- rbind(split_results_simple, fold_results)
      
      # Store predictions for this fold
      split_predictions_simple[[i]] <- fold_preds
    }
    
    # Append this split's results to all results
    all_results_simple <- rbind(all_results_simple, split_results_simple)
    
    # Store predictions for this split
    all_predictions_simple[[split_num]] <- split_predictions_simple
    
  }
  
  return(list(results = all_results_simple, predictions = all_predictions_simple))
}


# RUN SIMPLE MODELS ####

simple_results <- run_multiple_splits_simple_models(
  data = df_simple,
  splits_list = all_splits_simple,
  n_splits = 10
)

simple_metrics <- simple_results$results
simple_predictions <- simple_results$predictions

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
  ungroup()

# Combine all results ####


all_results_means <- rbind(all_results_means, simple_results_means)
# rm(simple_results, simple_metrics, simple_results_means)

# Convert simple_predictions list to a dataframe
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
      predicted = fold_preds$simple_lm_pred
    )
    
    # Simple GAM predictions
    simple_predictions_formatted[[paste("simple_gam", split_set, fold)]] <- data.frame(
      split_set = split_set,
      fold = fold,
      model_type = "Simple",
      model_variant = "Simple GAM",
      specimen_number = fold_preds$specimen_number,
      actual = fold_preds$actual,
      predicted = fold_preds$simple_gam_pred
    )
  }
}
simple_predictions_formatted <- do.call(rbind, simple_predictions_formatted)
rownames(simple_predictions_formatted) <- NULL
all_predictions <- rbind(final_predictions, simple_predictions_formatted)
rm(simple_predictions_formatted, simple_predictions, final_predictions)
# 

saveRDS(all_results_means, paste0("RDS_dataframes/IBM_all_result_means_", Sys.Date(), ".RDS"))
saveRDS(all_predictions, paste0("RDS_dataframes/IBM_all_predictions_", Sys.Date(), ".RDS"))
