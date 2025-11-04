# Packages####
packages <- c("caret", "doParallel", "dplyr","devtools", "dplyr","ggplot2", "mdatools", "mgcv", "MuMIn", "purrr", "ranger","stringr","tidyr", "viridis", "xgboost")
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  utils::install.packages(pkgs = packages[!installed_packages])
}
invisible(lapply(packages, library, character.only = TRUE)) # load all packages in list
rm(installed_packages, packages)
# Load dataframe ========================================================================
df <- readRDS("RDS_dataframes/LPW_scan_avg_proc.RDS")
df <- df[complete.cases(df$read_age), ]


# filter outliers 53, 74; outliers IDed in PCA of FT-NIRS
df <- df %>% filter(specimen != 53, specimen != 74)


# Dredge to find top 5 models ============================================================
pca_temp <- mdatools::pca(df[, 21:ncol(df)])

pc_df <- data.frame(PC1 = rep(0,nrow(df)))
for (i in 1:10) {
  pc_df[, paste0("PC", i)] <- pca_temp$res$cal$scores[, i]
  rm(i)
}
pc_df <- cbind(pc_df,df)
global_lm <- lm(data = pc_df, read_age ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10)
global_gam <- gam(data = pc_df, read_age ~ s(PC1,k = 4) + s(PC2,k = 4) + s(PC3,k = 4) + s(PC4,k = 4) + s(PC5,k = 4) + s(PC6,k = 4) + s(PC7, k = 4) + s(PC8, k = 4) + s(PC9, k = 4) + s(PC10, k = 4))

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

# Find number of PC's for each of 10 models: 
pc_counts_lm <- sapply(terms_lm, function(term) {
  # The 'term.labels' attribute contains the names of the predictor variables.
  # We just need to count how many there are.
  length(attr(term, "term.labels"))
})

# find num. comps for GAM:
pc_counts_gam <- sapply(terms_gam, function(formula) {
  # Your original method for GAMs is correct and effective
  formula_str <- as.character(formula)[3]
  stringr::str_count(formula_str, "PC\\d+")
})



rm(global_gam, global_lm, top10_gam, top10_lm, pc_df, pca_temp, dredge_gam,dredge_lm, i)
# ================================================================================== #
# ================================================================================== #
# Functions for splits and metrics ================================================= 
# ================================================================================== #
# ================================================================================== #
generate_multiple_splits <- function(data = df, n_splits) {
  all_splits <- list()
  for (split_id in 1:n_splits) {
    all_splits[[split_id]] <- caret::createFolds(data$read_age, k = 10, list = TRUE, returnTrain = FALSE) # 10-fold CV
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
# XGB ####
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
my_random_grid <- my_full_grid[sample(1:nrow(my_full_grid), 1000), ]

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
  x = as.matrix(df[, 21:ncol(df)]),
  y = df$read_age,
  method = "xgbTree",
  trControl = train_control,
  tuneGrid = my_random_grid,
  nthread = 1,
  verbose = FALSE # Suppress output during tuning
)
Sys.time() # show end time - approximately 14 minutes with 1000 random combinations and single 10-fold CV.
stopCluster(cl) # stop parallel backend

((best_params_xgb <- xgb_tuned_model$bestTune)) # store best tuning params and print

best_params_xgb <- data.frame(nround = 300, max_depth = 2, eta = 0.05, gamma = 5, colsample_bytree = 0.4, min_child_weight = 8, subsample = 1)
# nrounds   max_depth  eta    gamma colsample_bytree
#  300         2       0.05     5         0.4
# min_child_weight subsample
#        8           1

# TWO AT MAX VALUE - SUBSAMPLE AND GAMMA, ITS OK I GUESS?

# ==================================================================================== #
## Random Forest####
# ==================================================================================== #
# 
# # 1. Set up the parallel backend to speed up the process
# cl <- makePSOCKcluster(parallel::detectCores() - 1)
# registerDoParallel(cl)
# 
# # 2. Define the hyperparameter grid
# tuning_grid_rf <- expand.grid(
#   mtry = c(floor(sqrt(ncol(df[, 21:ncol(df)])) * 0.1),
#            floor(sqrt(ncol(df[, 21:ncol(df)])) * 0.2),
#            floor(ncol(df[, 21:ncol(df)]) / 3),
#            floor(ncol(df[, 21:ncol(df)]) / 2)),
#   min.node.size = c(1, 3, 5, 15, 25, 35),
#   splitrule = "variance" # Required for regression with ranger in caret
# )
# 
# # 3. Define the training control for 3 repeats of 10-fold CV
# train_control <- trainControl(
#   method = "repeatedcv",
#   number = 10,       # 10 folds
#   repeats = 1,       
#   search = "grid",  
#   allowParallel = TRUE
# )
# 
# Sys.time()
# rf_tuned_model <- train(
#   x = df[, 21:ncol(df)],
#   y = df$read_age,
#   method = "ranger",        # Use the ranger package for Random Forest
#   trControl = train_control,
#   tuneGrid = tuning_grid_rf,
#   importance = 'permutation', # Calculate variable importance on the final model
#   num.trees = 1000
# )
# Sys.time() # 1 minute to run the full 24 combination grid
# stopCluster(cl)
# (best_params_rf <- rf_tuned_model$bestTune)

best_params_rf <- data.frame(
  mtry = 466, 
  min.node.size = 15
)

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
      PercentRMSE = numeric(10),
      Components = numeric(10)
    )
    # Initialize prediction storage for this fold
    fold_preds <- list(
      specimen_number = testing$specimen,
      actual = testing$read_age,
      model_preds = vector("list", 10),  # One slot per model combination
      model_comps = vector("list", 10)
      )
    
    for (j in 1:10) {
      mod <- lm(data = calibrate, terms_lm[[j]])
      preds <- predict(mod, newdata = testing)
      
      # Store metrics
      fold_results$RMSE[j] <- caret::RMSE(pred = preds, obs = testing[, "read_age"])
      fold_results$PercentRMSE[j] <- fold_results$RMSE[j] / max(testing$read_age) * 100
      RSS <- sum((testing$read_age - preds)^2)
      TSS <- sum((testing$read_age - mean(testing$read_age))^2)
      fold_results$R2[j] <- 1 - (RSS / TSS)
      fold_results$RPD[j] <- calculate_rpd(testing$read_age, preds)
      fold_results$Bias[j] <- calculate_bias(testing$read_age, preds)
      fold_results$Components[j] <- pc_counts_lm[j]
      
      # Store predictions
      fold_preds$model_preds[[j]] <- preds
      fold_preds$model_comps[[j]] <- pc_counts_lm[j]
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
      PercentRMSE = numeric(10),
      Components = numeric(10)
    )
    
    # Initialize prediction storage for this fold
    fold_preds <- list(
      specimen_number = testing$specimen,
      actual = testing$read_age,
      model_preds = vector("list", 10),
      model_comps = vector("list", 10)
    )
    
    for (j in 1:10) {
      mod <- gam(data = calibrate, terms_gam[[j]], method = "REML")
      preds <- predict(mod, newdata = testing)
      
      # Store metrics
      fold_results$RMSE[j] <- caret::RMSE(pred = preds, obs = testing[, "read_age"])
      fold_results$PercentRMSE[j] <- fold_results$RMSE[j] / max(testing$read_age) * 100
      RSS <- sum((testing$read_age - preds)^2)
      TSS <- sum((testing$read_age - mean(testing$read_age))^2)
      fold_results$R2[j] <- 1 - (RSS / TSS)
      fold_results$RPD[j] <- calculate_rpd(testing$read_age, preds)
      fold_results$Bias[j] <- calculate_bias(testing$read_age, preds)
      fold_results$Components[j] <- pc_counts_gam[j]
      
      # Store predictions
      fold_preds$model_preds[[j]] <- preds
      fold_preds$model_comps[[j]] <- pc_counts_gam[j]
    }
    
    splits_results_gam <- rbind(splits_results_gam, fold_results)
    all_predictions[[i]] <- fold_preds
  }
  
  splits_results_gam$ModelType <- "GAM"
  
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
      actual = testing$read_age,
      pls_pred = numeric(length(testing$read_age)),
      vip_pred = numeric(length(testing$read_age)),
      pls_ncomp = NA_real_,
      vip_ncomp = NA_real_  
    )
    
    # PLS model
    mod <- mdatools::pls(calibrate[, 31:ncol(calibrate)], calibrate[, "read_age"],
                         scale = F, center = T, cv=1, 
                         x.test = testing[, 31:ncol(testing)],
                         y.test = testing[, "read_age"])
    ncomp <- mod$ncomp.selected
    
    
    # Wavenumber data
    wavenumbers <- as.numeric(colnames(calibrate[, 31:ncol(calibrate)]))
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
    fold_results$PercentRMSE[1] <- mod$testres$rmse[[ncomp]] / max(testing$read_age) * 100
    fold_results$Components[1] <- ncomp
    
    # Store predictions
    fold_preds$pls_pred <- mod$testres$y.pred[, ncomp,]
    fold_preds$pls_ncomp <- ncomp
    
    # VIP model #
    vip <- as.data.frame(vipscores(mod))
    mod <- mdatools::pls(calibrate[, 31:ncol(calibrate)], calibrate[, "read_age"],
                         scale = F, center = T, cv=1,
                         x.test = testing[, 31:ncol(testing)],
                         y.test = testing[, "read_age"],
                         exclcols = vip$V1 < 0.5)
    ncomp <- mod$ncomp.selected
    
    # Store metrics
    fold_results$R2[2] <- mod$testres$r2[[ncomp]]
    fold_results$RMSE[2] <- mod$testres$rmse[[ncomp]]
    fold_results$RPD[2] <- mod$testres$rpd[[ncomp]]
    fold_results$Bias[2] <- mod$testres$bias[[ncomp]]
    fold_results$PercentRMSE[2] <- mod$testres$rmse[[ncomp]] / max(testing$read_age) * 100
    fold_results$Components[2] <- ncomp
    
    # Store predictions
    fold_preds$vip_pred <- mod$testres$y.pred[, ncomp,]
    fold_preds$vip_ncomp <- ncomp
    
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
    x_train <- as.matrix(calibrate[, 31:ncol(calibrate)])
    x_test <- as.matrix(testing[, 31:ncol(calibrate)])
    y_train <- calibrate[, "read_age"]
    y_test <- testing[, "read_age"]
    
    # Convert to DMatrix format
    dtrain <- xgb.DMatrix(data = x_train, label = y_train)
    dtest <- xgb.DMatrix(data = x_test, label = y_test)
    
    # Set XGBoost parameters
    params <- list(
      objective = "reg:squarederror",
      booster = "gbtree",
      eta = best_params_xgb$eta, 
      nthread = 1,
      max_depth = best_params_xgb$max_depth,
      min_child_weight = best_params_xgb$min_child_weight,
      subsample = best_params_xgb$subsample,
      gamma = best_params_xgb$gamma,
      colsample_bytree = best_params_xgb$colsample_bytree
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
      xgb_pred = preds,
      components = NA_real_ 
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
      Components <- NA_real_ 
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
      actual = testing$read_age,
      rf_pred = numeric(length(testing$read_age)),
      components = NA_real_ 
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
      x = calibrate[, 31:ncol(calibrate)],
      y = calibrate$read_age,
      mtry = best_params_rf$mtry,
      min.node.size = best_params_rf$min.node.size,
      seed = 6,
      importance = 'permutation' # Calculate variable importance
    )
    
    # Wavenumbers
    wavenumbers <- as.numeric(colnames(calibrate[, 31:ncol(calibrate)]))
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
    RSS <- sum((testing$read_age - preds)^2)
    TSS <- sum((testing$read_age - mean(testing$read_age))^2)
    
    fold_results$R2 <- 1 - (RSS / TSS)
    fold_results$RMSE <- caret::RMSE(pred = preds, obs = testing[, "read_age"])
    fold_results$RPD <- calculate_rpd(testing$read_age, preds)
    fold_results$Bias <- calculate_bias(testing$read_age, preds)
    fold_results$PercentRMSE <- fold_results$RMSE / max(testing$read_age) * 100
    fold_results$Components <- NA_real_ 
    
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

extract_all_predictions <- function(model_results, split_set) {
  all_preds <- list()
  
  # Process each model type
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
      
      # Handle each model's unique structure
      if (model_type %in% c("lm", "gam")) {
        for (variant in 1:10) {
          variant_df <- base_df
          variant_df$model_type <- toupper(model_type)
          variant_df$model_variant <- paste(toupper(model_type), variant)
          variant_df$predicted <- fold_preds$model_preds[[variant]]
          variant_df$components <- fold_preds$model_comps[[variant]] # Get component count
          all_preds[[length(all_preds) + 1]] <- variant_df
        }
      } else if (model_type == "pls") {
        # Standard PLS
        pls_df <- base_df
        pls_df$model_type <- "PLS"
        pls_df$model_variant <- "PLS"
        pls_df$predicted <- fold_preds$pls_pred
        pls_df$components <- fold_preds$pls_ncomp # Get PLS components
        all_preds[[length(all_preds) + 1]] <- pls_df
        # PLS-VIP
        vip_df <- base_df
        vip_df$model_type <- "PLS"
        vip_df$model_variant <- "PLS-VIP"
        vip_df$predicted <- fold_preds$vip_pred
        vip_df$components <- fold_preds$vip_ncomp # Get VIP components
        all_preds[[length(all_preds) + 1]] <- vip_df
      } else if (model_type == "rf") {
        rf_df <- base_df
        rf_df$model_type <- "RF"
        rf_df$model_variant <- "RF"
        rf_df$predicted <- fold_preds$rf_pred
        rf_df$components <- fold_preds$components # Get NA
        all_preds[[length(all_preds) + 1]] <- rf_df
      } else if (model_type == "xgb") {
        xgb_df <- base_df
        xgb_df$model_type <- "XGB"
        xgb_df$model_variant <- "XGBoost"
        xgb_df$predicted <- fold_preds$xgb_pred
        xgb_df$components <- fold_preds$components # Get NA
        all_preds[[length(all_preds) + 1]] <- xgb_df
      }
    }
  }
  
  return(bind_rows(all_preds))
}
# ================================================================================#
# ================================================================================#
# RUN MODELS ==============================
# ================================================================================#
# ================================================================================#

################################################################################
# RUN MODELS (PARALLELIZED)
################################################################################

library(future)
library(future.apply)

# Prepare for parallel backend
n_cores <- parallel::detectCores(logical = FALSE)
options(mc.cores = n_cores)
Sys.unsetenv("R_PARALLELLY_AVAILABLE_CORES")
options(parallelly.maxWorkers.localhost = Inf)

# Prevent over-threading inside models
Sys.setenv(OMP_NUM_THREADS = 1, MKL_NUM_THREADS = 1)
if (requireNamespace("RhpcBLASctl", quietly = TRUE)) {
  RhpcBLASctl::blas_set_num_threads(1)
}

plan(multisession, workers = parallel::detectCores(logical = TRUE))

n_splits <- 500
set.seed(6)
all_splits <- generate_multiple_splits(df, n_splits)

cat("Starting parallel processing of", n_splits, "splits across", n_cores - 1, "workers...\n")
start_time <- Sys.time()

parallel_results <- future_lapply(
  1:n_splits,
  FUN = function(split_set) {
    library(caret)
    library(dplyr)
    library(mdatools)
    library(ranger)
    library(xgboost)
    library(mgcv)
  
    cat("Processing split set", split_set, "of", n_splits, "\n")
    
    splits <- all_splits[[split_set]]
    cal <- test <- vector("list", 10)
    pca_importance_split <- list()
    
    # PCA preprocessing per fold
    for (i in 1:10) {
      pc.mod <- preProcess(df[-splits[[i]], -c(1:20)], method = c("pca", "center"), pcaComp = 10)
      cal[[i]] <- cbind(predict(pc.mod, df[-splits[[i]], -c(1:20)]), df[-splits[[i]], ])
      test[[i]] <- cbind(predict(pc.mod, df[splits[[i]], -c(1:20)]), df[splits[[i]], ])
      
      pca_loadings <- pc.mod$rotation
      variances <- pc.mod$std[1:10]^2
      prop_variance <- variances / sum(variances)
      weighted_importance <- abs(pca_loadings[, 1:10]) %*% prop_variance
      
      pca_importance_split[[i]] <- data.frame(
        fold = i,
        method = "PCA Loadings",
        wavenumber = as.numeric(rownames(pca_loadings)),
        importance = weighted_importance[, 1]
      )
    }
    
    # Run all model types
    model_results <- list(
      lm  = run_lm_models(cal, test, terms_lm),
      gam = run_gam_models(cal, test, terms_gam),
      pls = run_pls_models(cal, test),
      rf  = run_rf_models(cal, test),
      xgb = run_xgb_models(cal, test)
    )
    
    # Collect importance
    pca_importance_df <- dplyr::bind_rows(pca_importance_split)
    pls_importance_df <- model_results$pls$importance
    rf_importance_df  <- model_results$rf$importance
    xgb_importance_df <- model_results$xgb$importance
    
    # Add split_set ID
    for (df_imp in list(pca_importance_df, pls_importance_df, rf_importance_df, xgb_importance_df)) {
      df_imp$split_set <- split_set
    }
    
    combined_importance <- dplyr::bind_rows(
      pca_importance_df,
      pls_importance_df,
      rf_importance_df,
      xgb_importance_df
    )
    
    # Add split ID to results
    for (m in names(model_results)) {
      model_results[[m]]$results$SplitSet <- split_set
    }
    
    # Collect everything
    list(
      split_set = split_set,
      model_results = model_results,
      predictions = extract_all_predictions(model_results, split_set),
      importance = combined_importance
    )
  },
  future.seed = TRUE,
  future.packages = c("caret", "dplyr", "ranger", "xgboost", "mdatools", "mgcv")
)

plan(sequential)
end_time <- Sys.time()

cat("✅ Parallel processing complete in",
    round(difftime(end_time, start_time, units = "mins"), 2), "minutes.\n")

# Combine results
all_results       <- lapply(parallel_results, `[[`, "model_results")
all_predictions   <- lapply(parallel_results, `[[`, "predictions")
all_importance_data <- lapply(parallel_results, `[[`, "importance")



# Combine metrics from all models
final_results <- combine_all_results(unlist(all_results, recursive = FALSE))
final_predictions <- do.call(rbind, all_predictions)
final_importance_data <- dplyr::bind_rows(all_importance_data)


# saveRDS(final_importance_data, paste0("RDS_dataframes/final_importance_data_", Sys.Date(), ".RDS"))

# ================================================================================#
# ================================================================================#
# calculate mean metrics for each simulation ======================================
# ================================================================================#
# ================================================================================#
final_results <- final_results %>% select(-`Components....NA_real_`)

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
rm(i,m,split_set,splits,cal,test, model_results, pc.mod)
# =================================================================================#
# =================================================================================#
# Simple Models  ========
# =================================================================================#
# =================================================================================#
df_simple <- df[complete.cases(df$structure_weight), ] # filter missing structure weight specimens
set.seed(6)
all_splits_simple <- generate_multiple_splits(df_simple, n_splits) # generate new splits with missing specimens

run_multiple_splits_simple_models <- function(data, splits_list, n_splits = 500) {
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
      # Get fold name and test indices
      fold_name <- names(current_split)[i]
      test_indices <- current_split[[i]]

      # Create test and calibration datasets
      testing <- data[test_indices, ]
      calibrate <- data[-test_indices, ]

      # Initialize prediction storage for this fold
      fold_preds <- list(
        specimen_number = testing$specimen,
        actual = testing$read_age,
        simple_lm_pred = numeric(length(testing$read_age)),
        simple_gam_pred = numeric(length(testing$read_age)),
        simple_lm_ncomp = 3,
        simple_gam_ncomp = 3
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
      mod <- lm(data = calibrate, read_age ~ length + structure_weight + weight, na.action = na.omit)
      preds_lm <- predict(mod, newdata = testing)
      fold_preds$simple_lm_pred <- preds_lm

      # Calculate metrics for LM
      fold_results$RMSE[1] <- caret::RMSE(pred = preds_lm, obs = testing[, "read_age"])
      fold_results$PercentRMSE[1] <- fold_results$RMSE[1] / max(testing$read_age) * 100
      RSS_lm <- sum((testing$read_age - preds_lm)^2)
      TSS_lm <- sum((testing$read_age - mean(testing$read_age))^2)
      fold_results$R2[1] <- 1 - (RSS_lm / TSS_lm)
      fold_results$RPD[1] <- calculate_rpd(testing$read_age, preds_lm)
      fold_results$Bias[1] <- calculate_bias(testing$read_age, preds_lm)

      # Simple GAM model
      mod <- gam(data = calibrate, read_age ~ s(length, k = 4) + s(structure_weight, k = 4) + s(weight, k = 4), method = "REML")
      preds_gam <- predict(mod, newdata = testing)
      fold_preds$simple_gam_pred <- preds_gam

      # Calculate metrics for GAM
      fold_results$RMSE[2] <- caret::RMSE(pred = preds_gam, obs = testing[, "read_age"])
      fold_results$PercentRMSE[2] <- fold_results$RMSE[2] / max(testing$read_age) * 100
      RSS_gam <- sum((testing$read_age - preds_gam)^2)
      TSS_gam <- sum((testing$read_age - mean(testing$read_age))^2)
      fold_results$R2[2] <- 1 - (RSS_gam / TSS_gam)
      fold_results$RPD[2] <- calculate_rpd(testing$read_age, preds_gam)
      fold_results$Bias[2] <- calculate_bias(testing$read_age, preds_gam)

      # Add Components
      fold_results$Components <- 3  # length, structure_weight, weight
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
  n_splits = 500
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
      predicted = fold_preds$simple_lm_pred,
      components = fold_preds$simple_lm_ncomp
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
      components = fold_preds$simple_gam_ncomp
    )
  }
}
simple_predictions_formatted <- do.call(rbind, simple_predictions_formatted)
rownames(simple_predictions_formatted) <- NULL
all_predictions <- rbind(final_predictions, simple_predictions_formatted)
rm(simple_predictions_formatted, simple_predictions, final_predictions)
# 

# saveRDS(all_results_means, paste0("RDS_dataframes/all_results_means_", Sys.Date(), ".RDS"))
# saveRDS(all_predictions, paste0("RDS_dataframes/all_predictions_", Sys.Date(), ".RDS"))
# 

# `=`=`=`=`=`=`=`=`=`=`=` ####
































# Pick best LM and GAM model from 10 based on mean RMSE #####

# LOAD DATA # 

df <- readRDS("RDS_dataframes/LPW_scan_avg_proc.RDS")
df <- df[complete.cases(df$read_age), ]
all_results_means <- readRDS("RDS_dataframes/all_results_means_parallel2025-07-21.RDS")
all_predictions <- readRDS("RDS_dataframes/all_predictions_parallel2025-07-21.RDS")
final_importance_data <- readRDS("RDS_dataframes/final_importance_data_parallel2025-07-21.RDS")

# Find the best LM (named "Linear") and GAM models from the results
best_models <- all_results_means %>%
  filter(ModelType %in% c("LM", "GAM")) %>%
  group_by(ModelType) %>%
  slice_min(order_by = RMSE, n = 1) %>%
  ungroup()

# Extract the specific model names (e.g., "Linear 9", "GAM 7")
best_lm_from_results <- best_models$Model[best_models$ModelType == "LM"]
best_gam_from_results <- best_models$Model[best_models$ModelType == "GAM"]

# **Correctly translate names** to match the `all_predictions` dataframe
# This converts "Linear 9" to "LM 9"
best_lm_in_preds <- gsub("Linear", "LM", best_lm_from_results)
best_gam_in_preds <- best_gam_from_results # GAM names are already consistent


# --- Step 2: Define Models to Keep and Final Plotting Order ---

# Create the definitive list of model variants to keep.
models_to_keep <- c(
  best_lm_in_preds,      # The best LM, now correctly named e.g., "LM 9"
  best_gam_in_preds,     # The best GAM, e.g., "GAM 7"
  "PLS-VIP",
  "RF",
  "XGBoost",             # Use the name as it appears in all_predictions
  "Simple LM",
  "Simple GAM"
)

# Define the desired final order for models and categories
final_model_order <- c("LM", "GAM", "PLS (VIP)", "XGB", "RF", "LM (Simple)", "GAM (Simple)")
final_modeltype_order <- c("PCA", "PLS", "ML", "Simple")


# --- Step 3: Filter and Relabel in a Single Pipeline ---

all_predictions <- all_predictions %>%
  # 1. Keep only the models in your `models_to_keep` list
  filter(model_variant %in% models_to_keep) %>%
  
  # 2. Rename variants for final display and re-categorize model types
  mutate(
    # Find and rename the best models using their names from `all_predictions`
    model_variant = case_when(
      model_variant == best_lm_in_preds  ~ "LM",
      model_variant == best_gam_in_preds ~ "GAM",
      model_variant == "PLS-VIP"         ~ "PLS (VIP)",
      model_variant == "XGBoost"         ~ "XGB", # Standardize XGBoost to XGB
      model_variant == "Simple LM"       ~ "LM (Simple)",
      model_variant == "Simple GAM"      ~ "GAM (Simple)",
      TRUE                               ~ as.character(model_variant)
    ),
    # Re-categorize the model types based on the new, final model names
    model_type = case_when(
      model_variant %in% c("LM", "GAM")   ~ "PCA",
      model_variant %in% c("XGB", "RF")    ~ "ML",
      model_variant == "PLS (VIP)"        ~ "PLS",
      model_variant %in% c("LM (Simple)", "GAM (Simple)") ~ "Simple",
      TRUE                                ~ as.character(model_type)
    )
  ) %>%
  
  # 3. Apply the desired factor ordering for plotting
  mutate(
    model_variant = factor(model_variant, levels = final_model_order),
    model_type = factor(model_type, levels = final_modeltype_order)
  )

# View the cleaned and ordered final dataframe
unique(final_predictions$model_variant) # Should show the final order of models
unique(final_predictions$model_type) # Should show the final order of model types

rm(best_models, best_lm_from_results, best_gam_from_results,
   best_lm_in_preds, best_gam_in_preds, models_to_keep, final_model_order,
   final_modeltype_order)
# =================================================================================#
# =================================================================================#
# Calc hatch date ==================================================================
# =================================================================================#
# =================================================================================#
all_predictions <- all_predictions %>%
  left_join(df %>% select(specimen, hatch_date), by = c("specimen_number" = "specimen"))

all_predictions <- all_predictions %>%
  left_join(df %>% select(specimen, sample_date), by = c("specimen_number" = "specimen"))


all_predictions$predhatch <- all_predictions$sample_date - all_predictions$predicted

# calc median hatch for each split, by model type


hatch_dates <- all_predictions %>%
  group_by(specimen_number, model_variant) %>%
  summarise(
    median_hatch = median(predhatch, na.rm = TRUE)
  ) %>%
  ungroup()



# K-S TEST ####

# Get the vector of original "true" hatch dates from your reference dataframe
# We will match specimens later to ensure a fair comparison for each model
original_data <- df %>%
  select(specimen_number = specimen, original_hatch_date = hatch_date)

# Get the list of unique model variants to loop through
all_variants <- unique(hatch_dates$model_variant)

# --- Run the K-S test for each model variant ---

ks_results <- map_dfr(all_variants, ~{
  # Filter estimates for the current model variant
  estimates_subset <- hatch_dates %>%
    filter(model_variant == .x)
  
  # Get the corresponding original hatch dates ONLY for the specimens in this subset
  originals_subset <- original_data %>%
    filter(specimen_number %in% estimates_subset$specimen_number)
  
  # Run the K-S test
  ks_test <- ks.test(estimates_subset$median_hatch, originals_subset$original_hatch_date)
  
  # Return a one-row tibble with the results
  tibble(
    model_variant = .x,
    D_statistic = ks_test$statistic,
    p_value = ks_test$p.value
  )
})

# Print the summary table, ordered by the D statistic (best to worst)
print(ks_results %>% arrange(D_statistic))

# --- Reshape data for plotting ---

# Get original dates, ready to be merged
original_plot_data <- hatch_dates %>%
  select(specimen_number, model_variant) %>%
  left_join(original_data, by = "specimen_number") %>%
  rename(hatch_date = original_hatch_date) %>%
  mutate(source = "Original")

# Get estimated dates
estimated_plot_data <- hatch_dates %>%
  rename(hatch_date = median_hatch) %>%
  mutate(source = "Estimate")

# Combine into one dataframe
combined_plot_data <- bind_rows(original_plot_data, estimated_plot_data)

# --- Create the faceted plot ---

ggplot(combined_plot_data, aes(x = hatch_date, color = source)) +
  stat_ecdf(geom = "step", linewidth = .6) +
  facet_wrap(~ model_variant, ncol = 4) +
  scale_color_manual(values = c("Estimate" = "red", "Original" = "blue")) +
  labs(
    title = "ECDF of Original vs. Estimated Hatch Dates",
    x = "Hatch Date",
    y = "Cumulative Probability",
    color = "Data Source"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")




# Get the original hatch dates for reference
original_dates <- df %>%
  filter(specimen %in% hatch_dates$specimen_number) %>%
  pull(hatch_date)

# Create a function to calculate the ECDF difference for a given set of estimates
calculate_ecdf_diff <- function(estimate_dates, original_dates) {
  # Create a common grid of x-values to evaluate both ECDFs on
  x_grid <- sort(unique(c(estimate_dates, original_dates)))
  
  # Calculate the ECDF values at each point on the grid
  ecdf_estimate <- ecdf(estimate_dates)(x_grid)
  ecdf_original <- ecdf(original_dates)(x_grid)
  
  # Return a tibble with the grid and the difference
  tibble(
    hatch_date = x_grid,
    # CORRECTED THIS LINE: edf_original -> ecdf_original
    ecdf_difference = ecdf_estimate - ecdf_original 
  )
}

# Apply this function to each model variant
ecdf_diff_data <- hatch_dates %>%
  group_by(model_variant) %>%
  summarise(
    diff_data = list(calculate_ecdf_diff(median_hatch, original_dates)),
    .groups = "drop"
  ) %>%
  tidyr::unnest(diff_data)

# Join the K-S statistics for annotation
ecdf_diff_data <- ecdf_diff_data %>%
  left_join(ks_results, by = "model_variant") %>%
  mutate(
    ks_label = sprintf("D = %.3f", D_statistic) # Format the label
  )

# --- 2. Create the Difference Plot ---
# --- 1. Define the correct color palette (as used in your boxplot) ---
color_palette <- c(
  "PCA"    = "#4477AA",  # Red-ish
  "PLS"    = "#AA3377",  # Green
  "ML"     = "#228833",  # Cyan
  "Simple" = "#CCBB44"   # Purple
)

# --- 2. Add 'ModelType' to the ECDF data ---
# Create a lookup table from your cleaned boxplot data
model_type_lookup <- all_results_cleaned %>%
  select(Model, ModelType) %>%
  distinct()

# Join the ModelType onto the ecdf_diff_data
ecdf_diff_data_colored <- ecdf_diff_data %>%
  left_join(model_type_lookup, by = c("model_variant" = "Model"))

# --- 3. Create the Final Plot ---
ggplot(ecdf_diff_data_colored, aes(x = hatch_date, y = ecdf_difference)) +
  # Add a reference line at y=0
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  
  # Draw the difference curve, coloring by ModelType
  # We still need `group = model_variant` to draw each line separately in its facet
  geom_step(aes(color = ModelType, group = model_variant), linewidth = 1) +
  
  # Add the K-S statistic as a label in each facet
  geom_text(
    aes(label = ks_label),
    x = -Inf, y = Inf,
    hjust = -0.1, vjust = 1.5,
    size = 3,
    check_overlap = TRUE
  ) +
  
  # Apply the matched color palette
  scale_color_manual(values = color_palette, name = "Model Type") +
  
  facet_wrap(~ model_variant, ncol = 4) +
  labs(
    title = "ECDF difference of hatch dates",
    subtitle = "Deviation from the zero-line indicates model error",
    x = "Hatch Date",
    y = "ECDF Difference"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom", # Add the legend to the bottom
    strip.background = element_rect(fill = "gray90")
  )

# # ====================================================================================#
# # ====================================================================================#
# Other Figures =======================================================================
# # ====================================================================================#
# # ====================================================================================#
# all_results_means <- readRDS("RDS_dataframes/all_results_means_2025-07-21.RDS")
# all_predictions <- readRDS("RDS_dataframes/all_predictions_07022025.RDS")
# final_importance_data <- readRDS("RDS_dataframes/final_importance_data_07022025.RDS")


# 1. Programmatically find the single best LM and GAM model
best_pca_models <- all_results_means %>%
  filter(ModelType %in% c("LM", "GAM")) %>%
  group_by(Model, ModelType) %>%
  summarise(Overall_Mean_RMSE = mean(RMSE, na.rm = TRUE), .groups = "drop") %>%
  group_by(ModelType) %>%
  slice_min(order_by = Overall_Mean_RMSE, n = 1)

best_pca_model_names <- best_pca_models$Model

# 2. Define the desired final order for the columns
final_model_order <- c("LM", "GAM", "PLS (VIP)", "XGB", "RF", "LM (Simple)", "GAM (Simple)")
final_modeltype_order <- c("PCA", "PLS", "ML", "Simple")

# 3. Create the final cleaned data frame in a single pipeline
all_results_cleaned <- all_results_means %>%
  # Keep rows that are either one of the best PCA models OR a non-PCA model type
  filter(Model %in% best_pca_model_names | !ModelType %in% c("LM", "GAM")) %>%
  
  # Also filter out the base "PLS" model, keeping only "PLS - VIP"
  filter(Model != "PLS") %>%
  
  # Rename and recategorize everything in one step
  mutate(
    Model = case_when(
      Model %in% best_pca_model_names & ModelType == "LM" ~ "LM",
      Model %in% best_pca_model_names & ModelType == "GAM" ~ "GAM",
      Model == "PLS - VIP" ~ "PLS (VIP)",
      Model == "Simple lm" ~ "LM (Simple)",
      Model == "Simple gam" ~ "GAM (Simple)",
      TRUE ~ as.character(Model)
    ),
    ModelType = case_when(
      ModelType %in% c("GAM", "LM") ~ "PCA",
      ModelType %in% c("XGB", "RF") ~ "ML",
      TRUE ~ as.character(ModelType)
    )
  ) %>%
  
  # Convert both columns to factors with your specified order for plotting
  mutate(
    Model = factor(Model, levels = final_model_order),
    ModelType = factor(ModelType, levels = final_modeltype_order)
  )

color_palette <- c('#4477AA', '#AA3377', "#228833", '#CCBB44')

# tol_default <- c('#4477AA', '#EE6677', '#228833', '#CCBB44', '#66CCEE', '#AA3377', '#BBBBBB')
# tol_contrast <- c("#FFFFFF", '#004488', '#DDAA33', '#BB5566', "#000000")
# tol_vibrant <- c('#EE7733', '#0077BB', '#33BBEE', '#EE3377', '#CC3311', '#009988', '#BBBBBB')
# tol_muted <- c( '#CC6677', '#332288', '#DDCC77', '#117733', '#88CCEE', '#882255', '#44AA99', '#999933', '#AA4499', "bad_data", '#DDDDDD')
# tol_medium <- c( '#6699CC', '#004488', '#EECC66', '#994455', '#997700', '#EE99AA')

# RMSE
ggplot(all_results_cleaned, aes(x = Model, y = RMSE, fill = ModelType)) +
  geom_boxplot(alpha = 0.7,width = 0.5) +
  theme_bw(base_size = 14) +
  scale_fill_manual(values = color_palette) +
  scale_x_discrete(labels = function(x) str_replace(x, " \\(", "\n(")) +
  labs(
    x = "Model",
    y = "RMSE (days)",
    fill = "Model Type"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

## R2 ###
ggplot(all_results_cleaned
       , aes(x = Model, y = R2, fill = ModelType)) +
  geom_boxplot(alpha = 0.7,width = 0.5) +
  scale_x_discrete(labels = function(x) str_replace(x, " \\(", "\n(")) +
  theme_bw(base_size = 14) +
  scale_fill_manual(values = color_palette) +
  labs(
    x = "Model",
    y = expression(R^2),
    fill = "Model Type"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5,)
  )

# Bias
ggplot(all_results_cleaned, aes(x = Model, y = Bias, fill = ModelType)) +
  geom_boxplot(alpha = 0.7,width = 0.5) +
  theme_bw(base_size = 14) +
  scale_fill_manual(values = color_palette) +
  scale_x_discrete(labels = function(x) str_replace(x, " \\(", "\n(")) +
  labs(
    x = "Model",
    y = "Bias (Days)",
    fill = "Model Type"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5)
  ) +
  geom_hline(yintercept = 0, color = "gray40", linewidth = 1, linetype = 2)

# RPD
ggplot(all_results_cleaned, aes(x = Model, y = RPD, fill = ModelType)) +
  geom_boxplot(alpha = 0.8,width = 0.5) +
  theme_bw(base_size = 14) +
  scale_fill_manual(values = color_palette) +
  scale_x_discrete(labels = function(x) str_replace(x, " \\(", "\n(")) +
  labs(
    x = "Model",
    y = "RPD",
    fill = "Model Type"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

library(cowplot)
library(gridExtra)
library(grid)
library(stringr)

# --- Step 1: Create a temporary plot just to extract its legend ---
legend_plot <- ggplot(all_results_cleaned, aes(x = Model, y = RMSE, fill = ModelType)) +
  geom_boxplot() +
  scale_fill_manual(values = color_palette) +
  theme_bw(base_size = 11) +
  labs(fill = "Model Type") +
  # Use theme() to format the legend's appearance before extracting it
  theme(legend.position = "bottom", legend.box = "horizontal")

# --- Step 2: Extract the legend ---
shared_legend <- get_legend(legend_plot)

# --- Step 3: Create your four plots and FORCE them to have no legend ---

p_rmse <- ggplot(all_results_cleaned, aes(x = Model, y = RMSE, fill = ModelType)) +
  geom_boxplot(alpha = 0.7, width = 0.5, outlier.size = 1) + 
  theme_bw(base_size = 11) +
  scale_fill_manual(values = color_palette) +
  scale_x_discrete(labels = function(x) str_replace(x, " \\(", "\n(")) +
  labs(y = "RMSE (days)") +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), legend.position = "none")

p_r2 <- ggplot(all_results_cleaned, aes(x = Model, y = R2, fill = ModelType)) +
  geom_boxplot(alpha = 0.7, width = 0.5, outlier.size = 1) +
  scale_x_discrete(labels = function(x) str_replace(x, " \\(", "\n(")) +
  theme_bw(base_size = 11) +
  scale_fill_manual(values = color_palette) +
  labs(y = expression(R^2)) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), legend.position = "none")

p_bias <- ggplot(all_results_cleaned, aes(x = Model, y = Bias, fill = ModelType)) +
  geom_boxplot(alpha = 0.7, width = 0.5, outlier.size = 1) +
  theme_bw(base_size = 11) +
  scale_fill_manual(values = color_palette) +
  scale_x_discrete(labels = function(x) str_replace(x, " \\(", "\n(")) +
  geom_hline(yintercept = 0, color = "gray40", linewidth = 1, linetype = 2) +
  labs(x = NULL, y = "Bias (Days)") +
  theme(legend.position = "none")

p_rpd <- ggplot(all_results_cleaned, aes(x = Model, y = RPD, fill = ModelType)) +
  geom_boxplot(alpha = 0.8, width = 0.5, outlier.size = 1) +
  theme_bw(base_size = 11) +
  scale_fill_manual(values = color_palette) +
  scale_x_discrete(labels = function(x) str_replace(x, " \\(", "\n(")) +
  labs(x = NULL, y = "RPD") +
  theme(legend.position = "none")

# --- Step 4: Manually arrange the plots, title, and legend with gridExtra ---
grid.arrange(
  arrangeGrob(p_rmse, p_r2, p_bias, p_rpd, nrow = 2),
  shared_legend,
  nrow = 2,
  heights = c(10, 1)
)


# # % RMSE
# ggplot(all_results_cleaned, aes(x = Model, y = PercentRMSE, fill = ModelType)) +
#   geom_boxplot(alpha = 0.7,width = 0.5) +
#   theme_bw(base_size = 14) +
#   scale_fill_manual(values = color_palette) +
#   scale_x_discrete(labels = function(x) str_replace(x, " \\(", "\n(")) +
#   labs(
#     x = "Model",
#     y = "% RMSE",
#     fill = "Model Type"
#   ) +
#   theme(
#     plot.title = element_text(hjust = 0.5)
#   )


# # =====================================================================================#
# # =====================================================================================#
## Spectra raw vs unprocessed #### =========================================================
# # =====================================================================================#
# # =====================================================================================#
# 
# 
# df_long <- pivot_longer(df, cols = -c(1:20))
# df_long <- df_long %>% rename(., "wavenumber" = "name") # rename name column to wavenumber for clarification
# df_long$wavenumber <- as.numeric(as.character(df_long$wavenumber)) # change class of wavenumber variable to a numeric
# 
# 
# df_unproc <- readRDS("RDS_dataframes/LPW_scan_avg_unproc.RDS")
# df_unproc <- df_unproc[complete.cases(df_unproc$read_age), ]
# df_unproc_long <- pivot_longer(df_unproc, cols = -c(1:20))
# df_unproc_long <- df_unproc_long %>% rename(., "wavenumber" = "name") 
# df_unproc_long$wavenumber <- as.numeric(as.character(df_unproc_long$wavenumber)) # change class of wavenumber variable to a numeric
# 
# # raw
# ggplot() +
#   geom_path(
#     data = df_unproc_long,
#     aes(x = wavenumber, y = value, color = read_age, group = file_name), linewidth = .5
#   ) + 
#   scale_x_reverse() + 
#   labs(y = "Raw Absorbance", x = expression(paste("Wavenumber ", cm^-1)), color = "Read Age (days)") + 
#   scale_color_viridis() + 
#   theme_bw()
# 
# # preprocessed
# ggplot() +
#   geom_path(
#     data = df_long,
#     aes(x = wavenumber, y = value, color = read_age, group = file_name), linewidth = .5
#   ) + 
#   scale_x_reverse() + 
#   labs(y = "Preprocessed Absorbance", x = expression(paste("Wavenumber ", cm^-1)), color = "Read Age (days)") + 
#   scale_color_viridis()  + 
#   theme_bw()

# =====================================================================================#
# =====================================================================================#
# IMPORTANCE FIGURES ####


# Figure most informative wavenumbers from PLS, PCA and Random Forest/XBG


# 1. Define the desired final names and their order
final_method_order <- c(
  "Loadings (PCA)",
  "VIP Score (PLS)",
  "Gain (XGB)",
  "Permutation (RF)"
)

# 2. Rename and reorder the 'method' column
final_importance_data <- final_importance_data %>%
  mutate(
    # Rename values using case_when()
    method = case_when(
      method == "PCA Loadings"  ~ "Loadings (PCA)",
      method == "PLS-VIP"       ~ "VIP Score (PLS)",
      method == "XGBoost"       ~ "Gain (XGB)",
      method == "Random Forest" ~ "Permutation (RF)",
      TRUE                      ~ as.character(method) # Fallback for any other values
    ),
    # Convert to a factor to set the specific order for plotting
    method = factor(method, levels = final_method_order)
  )



plot_summary_total_variance <- final_importance_data %>%
  group_by(method, wavenumber) %>%
  summarise(
    final_mean = mean(importance, na.rm = TRUE),
    upper_bound = max(importance, na.rm = TRUE),
    lower_bound = min(importance, na.rm = TRUE),
    .groups = 'drop'
  )

library(patchwork)

color_palette <- c('#4477AA', '#AA3377', '#CCBB44', "#228833")

# Define the data for each plot
pca_data <- filter(plot_summary_total_variance, method == "Loadings (PCA)")
pls_data <- filter(plot_summary_total_variance, method == "VIP Score (PLS)")
xgb_data <- filter(plot_summary_total_variance, method == "Gain (XGB)")
rf_data  <- filter(plot_summary_total_variance, method == "Permutation (RF)")
# Top three plots
p1 <- ggplot(pca_data, aes(x = wavenumber)) +
  geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound), fill = "#4477AA", alpha = 0.5) +
  geom_line(aes(y = final_mean), color = "#4477AA") +
  labs(y = "Loadings\n(PCA)", x = NULL) + # Use \n for a new line
  theme_bw() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_text(angle = 0, vjust = 0.5) # Rotate label to horizontal
  )

p2 <- ggplot(pls_data, aes(x = wavenumber)) +
  geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound), fill = "#AA3377", alpha = 0.5) +
  geom_line(aes(y = final_mean), color = "#AA3377") +
  labs(y = "VIP Score\n(PLS)", x = NULL) +
  theme_bw() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_text(angle = 0, vjust = 0.5)
  )

p3 <- ggplot(xgb_data, aes(x = wavenumber)) +
  geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound), fill = "#228833", alpha = 0.5) +
  geom_line(aes(y = final_mean), color = "#228833") +
  labs(y = "Gain\n(XGB)", x = NULL) +
  theme_bw() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_text(angle = 0, vjust = 0.5)
  )

# Bottom plot
p4 <- ggplot(rf_data, aes(x = wavenumber)) +
  geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound), fill = "#228833", alpha = 0.5) +
  geom_line(aes(y = final_mean), color = "#228833") +
  labs(y = "Permutation\n(RF)", x = expression(paste("Wavenumber (cm"^{-1}, ")"))) +
  theme_bw() +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5))

# --- 2. Combine the Plots (No change here) ---

# Combine the plots and add the centered annotation
(p1 / p2 / p3 / p4) +
  plot_annotation(
    title = "Wavenumber importance across model methods",
    subtitle = "Ribbons show range of importance values across all 500 10-fold CV iterations",
    theme = theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
  ) &
  scale_x_reverse()

# collect loadings of each PC individually ####

# n_splits <- 5
# set.seed(6)
# all_splits <- generate_multiple_splits(df, n_splits)
# 
# # --- MODIFIED: Initialize master lists ---
# all_results <- list()
# all_predictions <- list()
# all_importance_data <- list()
# all_pca_loadings_data <- list() # NEW: Dedicated list for the wide-format PC loadings
# 
# Sys.time()
# for (split_set in 1:n_splits) {
#   cat("Processing split set", split_set, "of", n_splits, "\n")
#   
#   splits <- all_splits[[split_set]]
#   cal <- test <- vector("list", 10)
#   
#   # --- MODIFIED: Renamed temp list for clarity ---
#   pca_loadings_split <- list() 
#   
#   for (i in 1:10) {
#     pc.mod <- preProcess(df[-splits[[i]], -c(1:20)], method = c("pca","center"), pcaComp = 10)
#     cal[[i]] <- cbind(predict(pc.mod, df[-splits[[i]], -c(1:20)]), df[-splits[[i]], ])
#     test[[i]] <- cbind(predict(pc.mod, df[splits[[i]], -c(1:20)]), df[splits[[i]], ])
#     
#     # --- MODIFIED: This block now only collects the detailed PC loadings ---
#     pca_loadings <- pc.mod$rotation
#     loadings_df <- as.data.frame(pca_loadings[, 1:10])
#     loadings_df$fold <- i
#     loadings_df$wavenumber <- as.numeric(rownames(pca_loadings))
#     pca_loadings_split[[i]] <- loadings_df
#   }
#   
#   # Run models and store results
#   model_results <- list(
#     lm = run_lm_models(cal, test, terms_lm),
#     gam = run_gam_models(cal, test, terms_gam),
#     pls = run_pls_models(cal, test),
#     rf = run_rf_models(cal, test),
#     xgb = run_xgb_models(cal, test)
#   )
#   
#   # --- MODIFIED: Separate collection for PCA loadings and other importance metrics ---
#   
#   # 1. Handle the PCA LOADINGS data
#   pca_loadings_df <- dplyr::bind_rows(pca_loadings_split)
#   pca_loadings_df$split_set <- split_set
#   all_pca_loadings_data[[split_set]] <- pca_loadings_df # Add to its own master list
#   
#   # 2. Handle the other IMPORTANCE metrics
#   pls_importance_df <- model_results$pls$importance
#   rf_importance_df <- model_results$rf$importance
#   xgb_importance_df <- model_results$xgb$importance
#   
#   pls_importance_df$split_set <- split_set
#   rf_importance_df$split_set <- split_set
#   xgb_importance_df$split_set <- split_set
#   
#   # Bind them together and add to the importance master list
#   all_importance_data[[split_set]] <- dplyr::bind_rows(
#     pls_importance_df,
#     rf_importance_df,
#     xgb_importance_df
#   )
#   
#   # --- (Rest of your loop for results and predictions is unchanged) ---
#   for (m in names(model_results)) {
#     model_results[[m]]$results$SplitSet <- split_set
#   }
#   all_results[[split_set]] <- model_results
#   all_predictions[[split_set]] <- extract_all_predictions(model_results, split_set)
# }
# Sys.time()
# 
# 
# 
# 
# final_results <- combine_all_results(unlist(all_results, recursive = FALSE))
# final_predictions <- do.call(rbind, all_predictions)
# final_importance_data <- dplyr::bind_rows(all_importance_data)
# final_pca_loadings <- dplyr::bind_rows(all_pca_loadings_data) # NEW: Create the final PCA loadings data frame
# 
# 
# 
# 
# # Summarize and plot the PCA loadings
# final_pca_loadings %>%
#   # 1. Calculate the mean loading for each PC at each wavenumber
#   group_by(wavenumber) %>%
#   summarise(across(starts_with("PC"), mean, na.rm = TRUE)) %>%
#   ungroup() %>%
#   
#   # 2. Reshape data from wide to long format for plotting
#   pivot_longer(
#     cols = PC1:PC10, # Select only the first 5 PCs
#     names_to = "component",
#     values_to = "loading"
#   ) %>%
#   
#   # 3. Create the plot
#   ggplot(aes(x = wavenumber, y = loading, color = component)) +
#   geom_line() +
#   scale_x_reverse() +
#   
#   # 4. Stack the plots in a single column
#   facet_wrap(~ component, ncol = 2) +
#   
#   labs(
#     title = "Mean PCA Loadings for First 5 Components",
#     subtitle = "Averaged across 5 simulations",
#     x = expression(paste("Wavenumber (cm"^{-1}, ")")),
#     y = "Mean Loading"
#   ) +
#   theme_bw() +
#   theme(legend.position = "none")




# Old Figures ####
# hatch date distr. for each model type separately
# ggplot(hatch_dates, aes(x = median_hatch)) +
#   geom_histogram(binwidth = 3) +
#   facet_wrap(~model_variant) +
#   labs(title = "Distribution of Predicted Hatch Dates by Model Variant",
#        x = "Predicted Hatch Date (days)",
#        y = "Count") +
#   theme_minimal() +
#   scale_fill_brewer(palette = "Set1") +
#   theme(legend.position = "bottom")

#
# # Prepare data for plotting: gather predicted and actual hatch dates into a long format
# plot_data <- all_predictions %>%
#   select(model_variant, predhatch, hatch_date) %>%
#   pivot_longer(cols = c(predhatch, hatch_date), names_to = "date_type", values_to = "hatch_date_value") %>%
#   mutate(date_type = factor(date_type, levels = c("hatch_date", "predhatch"), labels = c("Actual", "Predicted"))) # Relabel for clarity
#
# # Create the overlayed density plots
# ggplot(plot_data, aes(x = hatch_date_value, fill = date_type)) +
#   geom_density(alpha = 0.5) +
#   facet_wrap(~ model_variant, scales = "free") + # Facet by model_variant, free scales if needed
#   labs(x = "Hatch Date",
#        y = "Density",
#        fill = "Hatch Dates") +
#   theme_minimal() +
#   scale_fill_manual(values = c("Actual" = "black", "Predicted" = "red")) # Set colors as requested
#

# 
# 
# ggplot(plot_summary_normalized, aes(x = wavenumber, group = method, color = method, fill = method)) +
#   geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound), alpha = 0.25, linetype = "dotted") +
#   geom_line(aes(y = final_mean), linewidth = 0.8) +
#   scale_x_reverse() +
#   # Note: scales are now "fixed" to the [0, 1] range for direct comparison
#   facet_wrap(~ method, ncol = 1) +
#   labs(
#     title = "Normalized Wavenumber Importance",
#     subtitle = "Calculated across 500 simulations",
#     x = expression(paste("Wavenumber (cm"^{-1}, ")")),
#     y = "Normalized Importance Score"
#   ) +
#   theme_bw() +
#   theme(legend.position = "none")


# 
# ggplot(plot_summary_normalized, aes(x = wavenumber, group = method, color = method, fill = method)) +
#   geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound), alpha = 0.25, linetype = "dotted") +
#   geom_line(aes(y = final_mean), linewidth = 0.8) +
#   scale_x_reverse() +
#   # Note: scales are now "fixed" to the [0, 1] range for direct comparison
#   facet_wrap(~ method, ncol = 1) +
#   labs(
#     title = "Normalized Wavenumber Importance",
#     subtitle = "Calculated across 500 simulations",
#     x = expression(paste("Wavenumber (cm"^{-1}, ")")),
#     y = "Normalized Importance Score"
#   ) +
#   theme_bw() +
#   # only show wavenumbers that are between 7500-6000 cm-1
#   scale_x_reverse(limits = c(9000, 6000)) + 
#   theme(legend.position = "none")




