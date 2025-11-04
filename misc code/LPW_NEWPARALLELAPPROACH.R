# Load necessary libraries for this approach
packages <- c("caret", "doParallel", "dplyr", "devtools", "ggplot2", "mdatools", 
              "mgcv", "MuMIn", "purrr", "ranger", "stringr", "tidyr", "viridis", 
              "xgboost", "future", "future.apply", "progressr", "furrr",)
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  utils::install.packages(pkgs = packages[!installed_packages])
}
invisible(lapply(packages, library, character.only = TRUE))
rm(installed_packages, packages)

df <- readRDS("RDS_dataframes/LPW_scan_avg_proc.RDS")
df <- df[complete.cases(df$read_age), ]
df <- df %>% filter(specimen != 53, specimen != 74) # remove outliers



# IF YOU WANT TO REMOVE >7500 WAVENUMBER USE BELOW ******#$#)($#)($@*#)$(@*#$)
# Convert names to numeric, NAs are created for non-numeric names
# numeric_names <- suppressWarnings(as.numeric(names(df)))
# Keep columns that are NOT numbers OR are numbers <= 7500
# df <- df[, is.na(numeric_names) | numeric_names <= 7500] 

################################################################################
# Dredge to find top 10 models for LM and GAM  =================================
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
# PCA Loadings Function
################################################################################

# Function to calculate PCA loadings for a specific set of PCs
calculate_pca_importance_for_model <- function(pca_rotation, pc_indices, model_name) {
  
  # Sum absolute loadings across only the PCs used in this model
  # Equal weighting for each PC used
  importance <- rowSums(abs(pca_rotation[, pc_indices, drop = FALSE])) / length(pc_indices)
  
  return(importance)
}

# Extract PC indices for each model
get_pc_indices_lm <- function(term) {
  labels <- attr(term, "term.labels")
  as.numeric(str_extract(labels, "\\d+"))
}
get_pc_indices_gam <- function(formula) {
  formula_str <- as.character(formula)[3]
  pc_matches <- str_extract_all(formula_str, "PC\\d+")[[1]]
  as.numeric(str_extract(pc_matches, "\\d+"))
}

# Get the PC usage for each model
pcs_used_lm <- get_pc_indices_lm(terms_lm)
pcs_used_gam <- get_pc_indices_gam(terms_gam)

# Count how many times each PC appears across the top 10 models
pc_frequency_lm <- table(unlist(pcs_used_lm)) / 10  # Proportion of models using each PC
pc_frequency_gam <- table(unlist(pcs_used_gam)) / 10


################################################################################
# Other Functions
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





# print("PC frequency in LM models:")
# print(pc_frequency_lm)
# print("PC frequency in GAM models:")
# print(pc_frequency_gam)
# 
# 
# 
# # Function to calculate weighted PCA loadings
# calculate_weighted_pca_loadings <- function(pca_rotation, pc_frequency, method_name) {
#   
#   # Initialize importance scores
#   n_vars <- nrow(pca_rotation)
#   weighted_importance <- numeric(n_vars)
#   
#   # Weight loadings by PC frequency
#   for (pc_num in names(pc_frequency)) {
#     pc_idx <- as.numeric(pc_num)
#     weight <- as.numeric(pc_frequency[pc_num])
#     
#     # Add weighted absolute loadings for this PC
#     weighted_importance <- weighted_importance + 
#       (abs(pca_rotation[, pc_idx]) * weight)
#   }
#   
#   return(weighted_importance)
# }
# 


# previous parameters: remove outliers, 1000 hyperparameters
# best_params_xgb <- data.frame(nrounds = 2000, max_depth = 6, eta = 0.05, gamma = 1, colsample_bytree = 0.8, min_child_weight = 8, subsample = 1)

# OUTLIERS REMOVED
# best_params_rf <- data.frame(
#   mtry = 311,
#   min.node.size = 15,
#   splitrule = "variance"
# )



# WITH EARLY STOPPING = 20, 2000 rounds max

# best_params_xgb <- data.frame(max_depth = 8, eta = 0.01, gamma = 1, colsample_bytree = 0.8, min_child_weight = 8, subsample = 1)
# 
# best_params_rf <- data.frame(mtry = 311, min.node.size = 15,splitrule = "variance")
# 

################################################################################
# Functions for each model
################################################################################

run_lm_models_single <- function(calibrate, testing, fold_id, terms_lm, pc_counts_lm) {
  fold_results <- data.frame(
    Fold = fold_id, # Use the fold_id passed into the function
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
  )
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
  fold_results$ModelType <- "LM"
  return(list(results = fold_results, predictions = fold_preds))
}
################################################################################
run_gam_models_single <- function(calibrate, testing, fold_id, terms_gam, pc_counts_gam) {
  fold_results <- data.frame(
      Fold = fold_id,
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
    fold_results$ModelType <- "GAM"
    return(list(results = fold_results, predictions = fold_preds))
}
################################################################################
run_pls_models_fast <- function(calibrate, testing, ncomp_pls) {
  mod_pls <- mdatools::pls(
    calibrate[, 31:ncol(calibrate)],
    calibrate[, "read_age"],
    cv = NULL,           # NO internal cross-validation
    ncomp = ncomp_pls,     # USE THE PRE-CALCULATED VALUE
    scale = FALSE, 
    center = TRUE,
    x.test = testing[, 31:ncol(testing)],
    y.test = testing[, "read_age"]
  )
  
  # --- Model 2: PLS with VIP Selection ---
  # Get VIP scores from the first model
  vip <- as.data.frame(vipscores(mod_pls))
  
  # Run the VIP model, but calculate ncomp for this separately.
  mod_vip <- mdatools::pls(
    calibrate[, 31:ncol(calibrate)],
    calibrate[, "read_age"],
    cv = 1,  # LOOCV for VIP model
    ncomp = 10, # look across first 10 comps first best number
    scale = FALSE, 
    center = TRUE,
    x.test = testing[, 31:ncol(testing)],
    y.test = testing[, "read_age"],
    exclcols = vip$V1 < 0.5 # filter < 0.5 VIP score
  )
  vip.comps <- mod_vip$ncomp.selected
  
  # --- Assemble Results for this Fold ---
  
  results_df <- data.frame(
    Model = c("PLS", "PLS - VIP"),
    R2 = c(mod_pls$testres$r2[[ncomp_pls]], mod_vip$testres$r2[[vip.comps]]),
    RMSE = c(mod_pls$testres$rmse[[ncomp_pls]], mod_vip$testres$rmse[[vip.comps]]),
    RPD = c(mod_pls$testres$rpd[[ncomp_pls]], mod_vip$testres$rpd[[vip.comps]]),
    Bias = c(mod_pls$testres$bias[[ncomp_pls]], mod_vip$testres$bias[[vip.comps]]),
    PercentRMSE = c(
      mod_pls$testres$rmse[[ncomp_pls]] / max(testing$read_age) * 100,
      mod_vip$testres$rmse[[vip.comps]] / max(testing$read_age) * 100),
    Components = c(ncomp_pls, vip.comps),
    ModelType = "PLS"
  )
  
  # 2. Create the predictions list
  predictions_list <- list(
    specimen_number = testing$specimen,
    actual = testing$read_age,
    pls_pred = mod_pls$testres$y.pred[, ncomp_pls, ],
    vip_pred = mod_vip$testres$y.pred[, vip.comps, ],
    pls_ncomp = ncomp_pls,
    vip_ncomp = vip.comps
  )
  
  # 3. Create the importance data frame
  wavenumbers <- as.numeric(colnames(calibrate[, 31:ncol(calibrate)]))
  vip_scores <- vipscores(mod_pls) # Use VIP scores from the initial PLS model
  
  importance_df <- data.frame(
    # Fold and SplitSet IDs will be added later
    method = "PLS-VIP",
    wavenumber = wavenumbers,
    importance = vip_scores
  )
  
  # --- Return all outputs in the required list structure ---
  return(
    list(
      results = results_df,
      predictions = predictions_list,
      importance = importance_df
    )
  )
}
################################################################################
run_xgb_models_single <- function(calibrate, testing, fold_id, best_params_xgb) {
  
  # 1. PREPARE DATA for this fold
  # =================================================================
  # We only need DMatrix objects for the full calibrate and test sets
  x_calibrate <- as.matrix(calibrate[, 31:ncol(calibrate)])
  y_calibrate <- calibrate$read_age
  dtrain_full <- xgb.DMatrix(data = x_calibrate, label = y_calibrate)
  
  x_test <- as.matrix(testing[, 31:ncol(testing)])
  y_test <- testing$read_age
  dtest <- xgb.DMatrix(data = x_test, label = y_test)
  
  # 2. FIND OPTIMAL NROUNDS using internal CV on the calibration set
  # =================================================================
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
  )
  
  # Use xgb.cv to find the best number of rounds for this specific training fold
  xgb_cv_model <- xgb.cv(
    params = params,
    data = dtrain_full,
    nrounds = 2000,
    nfold = 5,
    early_stopping_rounds = 20,
    metrics = "rmse",
    verbose = 0
  )
  
  # Get the best number of rounds from the internal CV
  best_nrounds_for_fold <- xgb_cv_model$best_iteration
  
  # 3. TRAIN FINAL MODEL on the full calibration set
  # =================================================================
  final_xgb_model <- xgb.train(
    params = params,
    data = dtrain_full,
    nrounds = best_nrounds_for_fold, # use optimized number of rounds from above ^ 
    verbose = 0
  )
  
  # 4. GATHER RESULTS (using the final, untouched test set)
  # =================================================================
  
  preds <- predict(final_xgb_model, dtest)
  
  # The rest of your code remains the same...
  predictions_list <- list(
    specimen_number = testing$specimen,
    actual = y_test,
    xgb_pred = preds,
    components = NA_real_ 
  )
  
  r2 <- cor(preds, y_test)^2
  rmse_val <- sqrt(mean((preds - y_test)^2))
  
  results_df <- data.frame(
    Fold = fold_id,
    Model = "XGB",
    R2 = r2,
    RMSE = rmse_val,
    RPD = calculate_rpd(y_test, preds),
    Bias = calculate_bias(y_test, preds),
    PercentRMSE = rmse_val / max(y_test) * 100,
    Components = NA_real_,
    ModelType = "XGB"
  )
  
  wavenumbers_char <- colnames(x_calibrate)
  importance_matrix <- xgb.importance(model = final_xgb_model)
  
  importance_df <- data.frame(Feature = wavenumbers_char) %>%
    left_join(importance_matrix, by = "Feature") %>%
    mutate(
      method = "XGBoost",
      wavenumber = as.numeric(Feature),
      importance = ifelse(is.na(Gain), 0, Gain)
    ) %>%
    select(method, wavenumber, importance)
  
  return(
    list(
      results = results_df,
      predictions = predictions_list,
      importance = importance_df
    )
  )
}
################################################################################
run_rf_models_single <- function(calibrate, testing, fold_id, best_params_rf) {
  
  # 1. TRAIN THE MODEL for this single fold
  # =================================================================
  mod <- ranger(
    x = calibrate[, 31:ncol(calibrate)],
    y = calibrate$read_age,
    mtry = best_params_rf$mtry,
    min.node.size = best_params_rf$min.node.size,
    seed = 6,
    importance = 'permutation', 
    num.threads = 1
  )
  
  # 2. GATHER RESULTS for this single fold
  # =================================================================
  
  # A. Predictions
  preds <- predict(mod, data = testing)$predictions
  predictions_list <- list(
    specimen_number = testing$specimen,
    actual = testing$read_age,
    rf_pred = preds,
    components = NA_real_
  )
  
  # B. Results Data Frame (single row)
  RSS <- sum((testing$read_age - preds)^2)
  TSS <- sum((testing$read_age - mean(testing$read_age))^2)
  
  results_df <- data.frame(
    Fold = fold_id,
    Model = "RF",
    R2 = 1 - (RSS / TSS),
    RMSE = caret::RMSE(pred = preds, obs = testing$read_age),
    RPD = calculate_rpd(testing$read_age, preds),
    Bias = calculate_bias(testing$read_age, preds),
    PercentRMSE = caret::RMSE(pred = preds, obs = testing$read_age) / max(testing$read_age) * 100,
    Components = NA_real_,
    ModelType = "RF"
  )
  
  # C. Importance Data Frame
  wavenumbers <- as.numeric(colnames(calibrate[, 31:ncol(calibrate)]))
  importance_scores <- ranger::importance(mod)
  
  importance_df <- data.frame(
    method = "Random Forest",
    wavenumber = wavenumbers,
    importance = importance_scores
  )
  
  # 3. RETURN EVERYTHING in the standard list structure
  # =================================================================
  return(
    list(
      results = results_df,
      predictions = predictions_list,
      importance = importance_df
    )
  )
}

################################################################################
# results and predictions
################################################################################

# all the 'results' data frames from each element.
combine_final_results <- function(parallel_output) {
  map_dfr(parallel_output, "results")
}

# This function does the same for the 'predictions' data frames.
combine_final_predictions <- function(parallel_output) {
  map_dfr(parallel_output, "predictions")
}

# This function does the same for the 'importance' data frames.
combine_final_importance <- function(parallel_output) {
  map_dfr(parallel_output, "importance")
}

################################################################################
# Function to extract predictions
################################################################################
extract_all_predictions_single <- function(model_results_for_one_fold) {
  
  # A helper to create the base prediction data frame
  create_base_df <- function(preds_list) {
    tibble(
      specimen_number = preds_list$specimen_number,
      actual = preds_list$actual
    )
  }
  
  # --- Process each model type ---
  
  # LM & GAM (have 10 variants each)
  lm_preds <- model_results_for_one_fold$lm$predictions
  gam_preds <- model_results_for_one_fold$gam$predictions
  
  lm_df <- map_dfr(1:10, ~{
    create_base_df(lm_preds) %>%
      mutate(
        model_type = "LM",
        model_variant = paste("LM", .x),
        predicted = lm_preds$model_preds[[.x]],
        components = lm_preds$model_comps[[.x]]
      )
  })
  
  gam_df <- map_dfr(1:10, ~{
    create_base_df(gam_preds) %>%
      mutate(
        model_type = "GAM",
        model_variant = paste("GAM", .x),
        predicted = gam_preds$model_preds[[.x]],
        components = gam_preds$model_comps[[.x]]
      )
  })
  
  # PLS (has 2 variants: PLS and PLS-VIP)
  pls_preds <- model_results_for_one_fold$pls$predictions
  pls_df <- create_base_df(pls_preds) %>%
    mutate(
      model_type = "PLS",
      model_variant = "PLS",
      predicted = pls_preds$pls_pred,
      components = pls_preds$pls_ncomp
    )
  vip_df <- create_base_df(pls_preds) %>%
    mutate(
      model_type = "PLS",
      model_variant = "PLS-VIP",
      predicted = pls_preds$vip_pred,
      components = pls_preds$vip_ncomp
    )
  
  # RF & XGB (have 1 variant each)
  rf_preds <- model_results_for_one_fold$rf$predictions
  rf_df <- create_base_df(rf_preds) %>%
    mutate(
      model_type = "RF",
      model_variant = "RF",
      predicted = rf_preds$rf_pred,
      components = rf_preds$components
    )
  
  xgb_preds <- model_results_for_one_fold$xgb$predictions
  xgb_df <- create_base_df(xgb_preds) %>%
    mutate(
      model_type = "XGB",
      model_variant = "XGBoost",
      predicted = xgb_preds$xgb_pred,
      components = xgb_preds$components
    )
  
  # Combine everything into one tidy data frame
  bind_rows(lm_df, gam_df, pls_df, vip_df, rf_df, xgb_df)
}

################################################################################
# Function to run all models
################################################################################
run_analysis_for_fold <- function(job_row, 
                                  all_data, 
                                  terms_lm, pc_counts_lm, 
                                  terms_gam, pc_counts_gam, 
                                  best_params_rf, best_params_xgb) {
  
  # 1. PREPARE DATA
  calibrate_raw <- all_data[job_row$train_indices[[1]], ]
  testing_raw <- all_data[job_row$test_indices[[1]], ]
  feature_cols_index <- 21:ncol(all_data)
  
  # 2. PCA PRE-PROCESSING
  pc.mod <- caret::preProcess(
    calibrate_raw[, feature_cols_index], 
    method = c("center", "pca"),  # No nzv
    pcaComp = 10
  )
  
  calibrate <- cbind(predict(pc.mod, calibrate_raw[, feature_cols_index]), calibrate_raw)
  testing <- cbind(predict(pc.mod, testing_raw[, feature_cols_index]), testing_raw)
  
  # 2b. EXTRACT PCA LOADINGS FOR EACH MODEL
  pca_rotation <- pc.mod$rotation
  wavenumbers <- as.numeric(colnames(calibrate_raw[, feature_cols_index]))
  
  # Create importance dataframe for each LM model (10 models)
  pca_importance_lm_list <- map(1:10, function(i) {
    pc_indices <- get_pc_indices_lm(terms_lm[[i]])
    importance <- calculate_pca_importance_for_model(pca_rotation, pc_indices, paste0("Linear ", i))
    
    data.frame(
      method = paste0("PCA-LM", i),  # Unique identifier for each model
      model_name = paste0("Linear ", i),  # Match the model name in results
      wavenumber = wavenumbers,
      importance = importance,
      n_components = length(pc_indices),  # Track how many PCs used
      components_used = paste(pc_indices, collapse = ",")  # Which PCs
    )
  })
  
  # Create importance dataframe for each GAM model (10 models)
  pca_importance_gam_list <- map(1:10, function(i) {
    pc_indices <- get_pc_indices_gam(terms_gam[[i]])
    importance <- calculate_pca_importance_for_model(pca_rotation, pc_indices, paste0("GAM ", i))
    
    data.frame(
      method = paste0("PCA-GAM", i),
      model_name = paste0("GAM ", i),
      wavenumber = wavenumbers,
      importance = importance,
      n_components = length(pc_indices),
      components_used = paste(pc_indices, collapse = ",")
    )
  })
  
  # Combine all PCA importance dataframes
  pca_importance_all <- bind_rows(
    pca_importance_lm_list,
    pca_importance_gam_list
  )
  
  # 3. RUN ALL MODELS
  model_results <- list(
    lm = run_lm_models_single(calibrate, testing, job_row$fold_id, terms_lm, pc_counts_lm),
    gam = run_gam_models_single(calibrate, testing, job_row$fold_id, terms_gam, pc_counts_gam),
    pls = run_pls_models_fast(calibrate, testing, job_row$optimal_ncomp),
    rf = run_rf_models_single(calibrate, testing, job_row$fold_id, best_params_rf),
    xgb = run_xgb_models_single(calibrate, testing, job_row$fold_id, best_params_xgb)
  )
  
  # 4. GATHER & LABEL RESULTS
  importance_for_this_fold <- bind_rows(
    map(model_results, "importance"),
    list(pca_importance_all)  # Add all 20 PCA importance profiles
  )
  
  results_for_this_fold <- bind_rows(map(model_results, "results"))
  predictions_for_this_fold <- extract_all_predictions_single(model_results)
  
  # Add identifiers
  results_for_this_fold$SplitSet <- job_row$split_id
  importance_for_this_fold$SplitSet <- job_row$split_id
  importance_for_this_fold$Fold <- job_row$fold_id
  predictions_for_this_fold$split_set <- job_row$split_id
  predictions_for_this_fold$fold <- job_row$fold_id
  
  return(list(
    results = results_for_this_fold,
    predictions = predictions_for_this_fold,
    importance = importance_for_this_fold
  ))
}

################################################################################
# Generate k-fold splits
################################################################################
# 1. Generate all data splits
n_splits <- 500
set.seed(6)
all_splits <- generate_multiple_splits(df, n_splits)

# 2. Create the "Job Manifest": A tibble with one row for each of the 5,000 folds
job_manifest <- map_dfr(1:n_splits, function(split_num) {
  folds_for_split <- all_splits[[split_num]]
  map_dfr(1:10, function(fold_num) {
    tibble(
      split_id = split_num,
      fold_id = fold_num,
      train_indices = list(unlist(folds_for_split[-fold_num])),
      test_indices = list(folds_for_split[[fold_num]])
    )
  })
})

# ================================================================================== #
# Find optimal `ncomp` for all k-fold PLS models beforehand
# ================================================================================== #

# Worker function: Takes training indices, runs PLS LOOCV, returns optimal ncomp
find_optimal_ncomp <- function(train_idx, data) {
  calibrate <- data[train_idx, ]
  
  mod <- mdatools::pls(
    calibrate[, 21:ncol(calibrate)], 
    calibrate$read_age,
    cv = 1, # The demanding LOOCV
    ncomp = 10, # Cap the max components to test
    scale = FALSE,
    center = TRUE
  )
  
  return(mod$ncomp.selected)
}

# --- Configure and Run Parallel Job for Stage 1 ---
Sys.setenv(OMP_NUM_THREADS = 1, OPENBLAS_NUM_THREADS = 1)

# 16 workers seemed to be best for this but may swap back to 8?
plan(multisession, workers = 8) 

# Set up the progress bar
handlers(
  handler_progress(
    format = "[:bar] :percent | elapsed: :elapsed | eta: :eta",
    width = 60,
    clear = FALSE
  )
)

cat("\n--- STAGE 1: Finding optimal ncomp for", nrow(job_manifest), "folds... ---\n")
start_time_stage1 <- Sys.time()

# Run the job with a progress bar
with_progress({
  optimal_ncomps <- future_map_dbl(
    .x = job_manifest$train_indices,
    .f = ~ find_optimal_ncomp(.x, data = df),
    .options = furrr_options(seed = TRUE, packages = "mdatools"),
    .progress = TRUE
  )
})

end_time_stage1 <- Sys.time()

plan(sequential)
cat("--- Stage 1 finished! ---\n")
print(end_time_stage1 - start_time_stage1)


# 29.90707 mins for everythang with 16 workers
# Time difference of 28.26037 mins

# Add the calculated optimal ncomp values to our job manifest
job_manifest$optimal_ncomp <- optimal_ncomps


# ================================================================================== #
# Run Analysis
# ================================================================================== #

Sys.setenv(OMP_NUM_THREADS = 1, OPENBLAS_NUM_THREADS = 1)
plan(multisession, workers = 8) 

# Configure a better progress bar with more information


handlers(
  handler_cli(
    format = paste0(
      "{cli::pb_spin} {cli::pb_bar} {cli::pb_percent} | ",
      "{cli::pb_current}/{cli::pb_total} | ",
      "elapsed: {cli::pb_elapsed} | eta: {cli::pb_eta}"
    ),
    clear = FALSE,
    show_after = 0
  )
)

cat("\n--- STAGE 2: Running full analysis for", nrow(job_manifest), "folds... ---\n")
start_time_stage2 <- Sys.time()


# Then in your parallel call:
with_progress({
  p <- progressor(steps = nrow(job_manifest))
  
  parallel_results_list <- future_map(
    .x = 1:nrow(job_manifest),
    .f = ~ {
      result <- run_analysis_for_fold(
        job_row = job_manifest[.x, ],
        all_data = df,
        terms_lm = terms_lm, pc_counts_lm = pc_counts_lm,
        terms_gam = terms_gam, pc_counts_gam = pc_counts_gam,
        best_params_rf = best_params_rf, 
        best_params_xgb = best_params_xgb
      )
      p()
      result
    },
    .options = furrr_options(
      seed = TRUE, 
      packages = c("caret", "mgcv", "mdatools", "ranger", "xgboost", "dplyr", "purrr", "stringr")
    )
  )
})


end_time_stage2 <- Sys.time()
cat("--- Stage 2 finished! ---\n")
print(end_time_stage2 - start_time_stage2)
plan(sequential)


# saveRDS(parallel_results_list, "NEWPARALLELRUN_ALL.RDS")

# Time difference of 1.43189 hours for 5-fold nround selection in XGB, LOOCV of PLS models


# Running again, appears to be closer to 1.1 hr? Using 8 workers, 1 thread each
# Time difference of 1.225348 hours



# 
# 
# with_progress({
#   p <- progressor(steps = nrow(job_manifest))  # Initialize progressor
#   
#   parallel_results_list <- future_map(
#     .x = 1:nrow(job_manifest),
#     .f = ~ {
#       result <- run_analysis_for_fold(
#         job_row = job_manifest[.x, ],
#         all_data = df,
#         terms_lm = terms_lm, pc_counts_lm = pc_counts_lm,
#         terms_gam = terms_gam, pc_counts_gam = pc_counts_gam,
#         best_params_rf = best_params_rf, best_params_xgb = best_params_xgb
#       )
#       p()  # Update progress after each fold completes
#       result
#     },
#     .options = furrr_options(seed = TRUE, packages = c("caret", "mgcv", "mdatools", "ranger", "xgboost", "dplyr", "purrr"))
#   )
# })


# The parallel plan is still active from Stage 1.
# 
# Sys.setenv(OMP_NUM_THREADS = 1, OPENBLAS_NUM_THREADS = 1)
# 
# plan(multisession, workers = 8) 
# 
# cat("\n--- STAGE 2: Running full analysis for", nrow(job_manifest), "folds... ---\n")
# start_time_stage2 <- Sys.time()
# 
# with_progress({
#   parallel_results_list <- future_map(
#     .x = 1:nrow(job_manifest),
#     .f = ~ run_analysis_for_fold(
#       job_row = job_manifest[.x, ],
#       all_data = df,
#       terms_lm = terms_lm, pc_counts_lm = pc_counts_lm,
#       terms_gam = terms_gam, pc_counts_gam = pc_counts_gam,
#       best_params_rf = best_params_rf, best_params_xgb = best_params_xgb
#     ),
#     .options = furrr_options(seed = TRUE, packages = c("caret", "mgcv", "mdatools", "ranger", "xgboost", "dplyr", "purrr")),
#     .progress = TRUE
#   )
# })
# 
# end_time_stage2 <- Sys.time()
# cat("--- Stage 2 finished! ---\n")
# print(end_time_stage2 - start_time_stage2)
# 
# # Shut down the parallel workers
# plan(sequential)


# ================================================================================== #
# Combine Results
# ================================================================================== #

cat("\n---  Aggregating final results... ---\n")

# Use the efficient map_dfr functions to combine the results
final_results_df <- map_dfr(parallel_results_list, "results")
final_predictions_df <- map_dfr(parallel_results_list, "predictions")
final_importance_df <- map_dfr(parallel_results_list, "importance")

cat("--- Analysis Complete! ---\n")

# Glimpse the final, tidy data frames
glimpse(final_results_df)
glimpse(final_predictions_df)
glimpse(final_importance_df)



# try different version....>!
all_results_means <- final_results_df %>%
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

# ================================================================================== #
# SIMPLE MDOELS
# ================================================================================== #
  
# Filter data and generate splits

df_simple <- readRDS("RDS_dataframes/LPW_scan_avg_proc.RDS")
df_simple <- df_simple[complete.cases(df_simple$read_age), ]
df_simple <- df_simple[complete.cases(df_simple$structure_weight), ]
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

# Run Simple Models in Parallel

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

# Combine All Results 

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
all_predictions_final <- rbind(final_predictions_df, simple_predictions_df)



################################################################################

# Save Results 

################################################################################
###### :

saveRDS(all_results_means_final, paste0("RDS_dataframes/all_results_means_parallel_updated_", Sys.Date(), ".RDS"))
saveRDS(all_predictions_final, paste0("RDS_dataframes/all_predictions_parallel_updated_", Sys.Date(), ".RDS"))
saveRDS(final_importance_df, paste0("RDS_dataframes/final_importance_data_parallel_updated_", Sys.Date(), ".RDS"))
















# Parallelized function to process a single fold
process_single_fold_pca <- function(fold_idx, job_manifest, all_data, 
                                    terms_lm, terms_gam, 
                                    wavenumbers, feature_cols_index) {
  
  calibrate_raw <- all_data[job_manifest$train_indices[[fold_idx]], ]
  
  pc.mod <- caret::preProcess(
    calibrate_raw[, feature_cols_index], 
    method = c("center", "pca"),
    pcaComp = 10
  )
  
  pca_rotation <- pc.mod$rotation
  
  # Generate for all 10 LM models
  lm_importance <- map_dfr(1:10, function(model_idx) {
    pc_indices <- get_pc_indices_lm(terms_lm[[model_idx]])
    importance <- calculate_pca_importance_for_model(pca_rotation, pc_indices, 
                                                     paste0("Linear ", model_idx))
    
    data.frame(
      method = paste0("PCA-LM", model_idx),
      model_name = paste0("Linear ", model_idx),
      wavenumber = wavenumbers,
      importance = importance,
      # n_components = length(pc_indices),
      # components_used = paste(pc_indices, collapse = ","),
      SplitSet = job_manifest$split_id[fold_idx],
      Fold = job_manifest$fold_id[fold_idx]
    )
  })
  
  # Generate for all 10 GAM models
  gam_importance <- map_dfr(1:10, function(model_idx) {
    pc_indices <- get_pc_indices_gam(terms_gam[[model_idx]])
    importance <- calculate_pca_importance_for_model(pca_rotation, pc_indices, 
                                                     paste0("GAM ", model_idx))
    
    data.frame(
      method = paste0("PCA-GAM", model_idx),
      model_name = paste0("GAM ", model_idx),
      wavenumber = wavenumbers,
      importance = importance,
      # n_components = length(pc_indices),
      # components_used = paste(pc_indices, collapse = ","),
      SplitSet = job_manifest$split_id[fold_idx],
      Fold = job_manifest$fold_id[fold_idx]
    )
  })
  
  bind_rows(lm_importance, gam_importance)
}




# Main parallelized function
generate_pca_loadings_per_model_parallel <- function(job_manifest, all_data, 
                                                     terms_lm, terms_gam) {
  
  feature_cols_index <- 21:ncol(all_data)
  wavenumbers <- as.numeric(colnames(all_data[, feature_cols_index]))
  
  cat("\n--- Generating PCA loadings for", nrow(job_manifest), "folds (parallel) ---\n")

  
  # Set up progress bar
  handlers(
    handler_progress(
      format = "[:bar] :percent | :current/:total | eta: :eta",
      width = 60,
      clear = FALSE
    )
  )
  
  # Run in parallel with progress
  with_progress({
    p <- progressor(steps = nrow(job_manifest))
    
    pca_loadings_list <- future_map_dfr(
      .x = 1:nrow(job_manifest),
      .f = ~ {
        result <- process_single_fold_pca(
          fold_idx = .x,
          job_manifest = job_manifest,
          all_data = all_data,
          terms_lm = terms_lm,
          terms_gam = terms_gam,
          wavenumbers = wavenumbers,
          feature_cols_index = feature_cols_index
        )
        p()
        result
      },
      .options = furrr_options(
        seed = TRUE, 
        packages = c("caret", "dplyr", "purrr", "stringr")
      )
    )
  })
  
  return(pca_loadings_list)
}
# Run it (parallelized!)
cat("Generating model-specific PCA loadings (parallel)...\n")
# Set up parallel processing
Sys.setenv(OMP_NUM_THREADS = 1, OPENBLAS_NUM_THREADS = 1)
plan(multisession, workers = 8)
start_time <- Sys.time()

pca_loadings_data <- generate_pca_loadings_per_model_parallel(
  job_manifest, df, terms_lm, terms_gam
)

end_time <- Sys.time()
cat("✓ PCA loadings generation complete!\n")
print(end_time - start_time)
# Shut down parallel workers
plan(sequential)


final_importance_data$model_name <- NA
# pca_loadings_data <- pca_loadings_data %>% select(-n_components, -components_used)

# Add to existing importance data
cat("Merging with existing importance data...\n")
final_importance_data_complete <- bind_rows(
  final_importance_data,
  pca_loadings_data
)

# Save
saveRDS(final_importance_data_complete, "final_importance_data_parallel_GOODNOW_2025-10-16.RDS")

cat("✓ Complete! Added", nrow(pca_loadings_data), "rows\n")
cat("  (", 20 * nrow(job_manifest), "PCA importance profiles total)\n")
cat("  Saved to:", output_file, "\n")