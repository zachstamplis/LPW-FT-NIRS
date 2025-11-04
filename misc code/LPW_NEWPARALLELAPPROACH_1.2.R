# Load necessary libraries for this approach
packages <- c("caret", "doParallel", "dplyr", "devtools", "ggplot2", "mdatools", 
              "mgcv", "MuMIn", "purrr", "ranger", "stringr", "tidyr", "viridis", 
              "xgboost", "future", "future.apply", "progressr", "furrr")
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
# This function calculates variable importance for LM and GAM models based on PCA loadings.
# It sums the absolute loadings of the original wavenumbers onto the Principal Components (PCs)
# that are used in a specific model. This provides a measure of how much each wavenumber
# contributes to the PCs that are deemed important by the LM/GAM model, making it
# comparable to other importance methods by showing which original features drive the model.
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


# previous parameters: remove outliers, 1000 hyperparameters
best_params_xgb <- data.frame(nrounds = 2000, max_depth = 6, eta = 0.05, gamma = 1, colsample_bytree = 0.8, min_child_weight = 8, subsample = 1)

# OUTLIERS REMOVED
best_params_rf <- data.frame(
  mtry = 311,
  min.node.size = 15,
  splitrule = "variance"
)

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

# Standardized importance extraction function
standardize_importance_output <- function(importance_df, fold_id, split_id) {
  # Ensure all importance dataframes have the same core columns
  importance_df %>%
    select(method, wavenumber, importance) %>%
    mutate(
      Fold = fold_id,
      SplitSet = split_id
    )
}

# NEW: Summarize importance per split instead of storing every fold
summarize_importance_per_split <- function(importance_df) {
  importance_df %>%
    group_by(method, wavenumber, SplitSet) %>%
    summarise(
      mean_importance = mean(importance, na.rm = TRUE),
      sd_importance = sd(importance, na.rm = TRUE),
      min_importance = min(importance, na.rm = TRUE),
      max_importance = max(importance, na.rm = TRUE),
      .groups = "drop"
    )
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
    method = c("center", "pca"),
    pcaComp = 10
  )
  
  calibrate <- cbind(predict(pc.mod, calibrate_raw[, feature_cols_index]), calibrate_raw)
  testing <- cbind(predict(pc.mod, testing_raw[, feature_cols_index]), testing_raw)
  
  # 3. RUN ALL MODELS
  model_results <- list(
    lm = run_lm_models_single(calibrate, testing, job_row$fold_id, terms_lm, pc_counts_lm),
    gam = run_gam_models_single(calibrate, testing, job_row$fold_id, terms_gam, pc_counts_gam),
    pls = run_pls_models_fast(calibrate, testing, job_row$optimal_ncomp),
    rf = run_rf_models_single(calibrate, testing, job_row$fold_id, best_params_rf),
    xgb = run_xgb_models_single(calibrate, testing, job_row$fold_id, best_params_xgb)
  )
  
  # 4. GATHER & STANDARDIZE IMPORTANCE DATA
  
  # 4a. PCA Loadings for LM & GAM models
  pca_rotation <- pc.mod$rotation
  wavenumbers <- as.numeric(colnames(calibrate_raw[, feature_cols_index]))
  
  pca_importance_lm <- map_dfr(1:10, ~{
    pc_indices <- get_pc_indices_lm(terms_lm[[.x]])
    data.frame(
      method = paste0("PCA-LM", .x),
      wavenumber = wavenumbers,
      importance = calculate_pca_importance_for_model(pca_rotation, pc_indices, paste0("Linear ", .x))
    )
  })
  
  pca_importance_gam <- map_dfr(1:10, ~{
    pc_indices <- get_pc_indices_gam(terms_gam[[.x]])
    data.frame(
      method = paste0("PCA-GAM", .x),
      wavenumber = wavenumbers,
      importance = calculate_pca_importance_for_model(pca_rotation, pc_indices, paste0("GAM ", .x))
    )
  })
  
  # 4b. Combine all importance data
  importance_for_this_fold <- bind_rows(
    model_results$pls$importance,
    model_results$rf$importance,
    model_results$xgb$importance,
    pca_importance_lm,
    pca_importance_gam
  ) %>%
    mutate(
      Fold = job_row$fold_id,
      SplitSet = job_row$split_id
    )
  
  # 5. GATHER RESULTS & PREDICTIONS
  results_for_this_fold <- bind_rows(map(model_results, "results"))
  predictions_for_this_fold <- extract_all_predictions_single(model_results)
  
  # Add identifiers for traceability
  results_for_this_fold$SplitSet <- job_row$split_id
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
n_splits <- 50 # Changed from 500 to 50 as per user's test
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
plan(multisession, workers = 16) 

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

# Define the splits to run over
split_sets_to_run <- unique(job_manifest$split_id)

with_progress({
  p <- progressor(steps = length(split_sets_to_run))
  
  # PARALLELIZE OVER THE 500 SPLITSETS (NOT 5000 FOLDS)
  # This is more memory-efficient as it summarizes importance within each worker.
  parallel_results_list <- future_map(
    .x = split_sets_to_run,
    .f = ~ {
      current_split_id <- .x
      
      # Get the 10 rows from the job manifest for this SplitSet
      jobs_for_split <- job_manifest %>% dplyr::filter(split_id == current_split_id)
      
      # Process all 10 folds for this SplitSet sequentially (inside the worker)
      results_list_for_split <- purrr::map(1:nrow(jobs_for_split), function(j) {
        run_analysis_for_fold(
          job_row = jobs_for_split[j, ],
          all_data = df,
          terms_lm = terms_lm, pc_counts_lm = pc_counts_lm,
          terms_gam = terms_gam, pc_counts_gam = pc_counts_gam,
          best_params_rf = best_params_rf, 
          best_params_xgb = best_params_xgb
        )
      })
      
      # --- AGGREGATION (CRITICAL STEP) ---
      
      # A. Combine Results/Predictions (Keep raw, as they are not huge)
      results_raw <- purrr::map_dfr(results_list_for_split, "results")
      predictions_raw <- purrr::map_dfr(results_list_for_split, "predictions")
      
      # B. Combine and Summarize Importance (The Memory Fix)
      importance_raw <- purrr::map_dfr(results_list_for_split, "importance")
      
      importance_summary <- importance_raw %>%
        dplyr::group_by(method, wavenumber, SplitSet) %>%
        dplyr::summarise(
          mean_importance = mean(importance, na.rm = TRUE),
          sd_importance = sd(importance, na.rm = TRUE),
          min_importance = min(importance, na.rm = TRUE),
          max_importance = max(importance, na.rm = TRUE),
          q025 = quantile(importance, 0.025, na.rm = TRUE),
          q975 = quantile(importance, 0.975, na.rm = TRUE),
          .groups = "drop"
        )
      
      # Clean up intermediate raw importance data
      rm(importance_raw, results_list_for_split)
      gc()
      
      p() # Update progress
      
      # Return the raw results/predictions and the summarized importance
      return(list(
        results = results_raw, 
        predictions = predictions_raw, 
        importance = importance_summary
      ))
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

# ================================================================================== #
# STAGE 3: AGGREGATE AND SUMMARIZE RESULTS
# ================================================================================== #

cat("\n---  Aggregating final results... ---\n")

# Extract the already-summarized data from the parallel list
final_results_df <- map_dfr(parallel_results_list, "results")
final_predictions_df <- map_dfr(parallel_results_list, "predictions")
final_importance_summary <- map_dfr(parallel_results_list, "importance")

# Clear the large list object to free memory
rm(parallel_results_list)
gc()

# Summarize the results from the complex models
all_results_means <- final_results_df %>%
  group_by(SplitSet, Model, ModelType) %>%
  summarize(
    R2 = mean(R2),
    RMSE = mean(RMSE),
    RPD = mean(RPD),
    Bias = mean(Bias),
    PercentRMSE = mean(PercentRMSE),
    # Handle cases where all Components are NA (e.g., RF, XGB) to avoid warnings
    Min_Components = if(all(is.na(Components))) NA_integer_ else min(Components, na.rm = TRUE),
    Max_Components = if(all(is.na(Components))) NA_integer_ else max(Components, na.rm = TRUE),
    N = n(),
    .groups = "drop"
  ) %>%
  mutate(
    Component_Range = case_when(
      is.na(Min_Components) ~ NA_character_,
      Min_Components == Max_Components ~ as.character(Min_Components),
      TRUE ~ paste(Min_Components, Max_Components, sep = "-")
    )
  )

# ================================================================================== #
# STAGE 4: SIMPLE MODELS
# ================================================================================== #

# Filter data for simple models (requires structure_weight) and generate splits
# Reload and prepare data for simple models to include all specimens with weight data
df_simple <- readRDS("RDS_dataframes/LPW_scan_avg_proc.RDS")
df_simple <- df_simple[complete.cases(df_simple$read_age), ]
df_simple <- df_simple[complete.cases(df_simple$structure_weight), ]
set.seed(6)
all_splits_simple <- generate_multiple_splits(df_simple, n_splits)

## Worker function for a single split of simple models
process_single_split_simple <- function(split_num, data, splits_list, p) {
  p(sprintf("Simple Split %g", split_num))
  current_split <- splits_list[[split_num]]
  
  # Process each of the 10 folds sequentially
  map_dfr(1:10, function(fold_num) {
    test_indices <- current_split[[fold_num]]
    testing_data <- data[test_indices, ]
    calibrate_data <- data[-test_indices, ]
    
    # Simple linear model
    mod_lm <- lm(data = calibrate_data, read_age ~ length + structure_weight + weight)
    preds_lm <- predict(mod_lm, newdata = testing_data)
    
    # Simple GAM model
    mod_gam <- gam(data = calibrate_data, read_age ~ s(length, k = 4) + s(structure_weight, k = 4) + s(weight, k = 4), method = "REML")
    preds_gam <- predict(mod_gam, newdata = testing_data)
    
    # Combine results and predictions for the fold
    bind_rows(
      tibble(Model = "Simple lm", predicted = preds_lm),
      tibble(Model = "Simple gam", predicted = preds_gam)
    ) %>%
      mutate(
        Split = split_num,
        Fold = fold_num,
        specimen_number = testing_data$specimen,
        actual = testing_data$read_age
      )
  })
}

# --- Run Simple Models in Parallel ---
cat("\n--- Running", n_splits, "simple model splits... ---\n")
plan(multisession, workers = availableCores() - 1)
with_progress({
  p <- progressor(steps = n_splits)
  simple_models_output <- future_map_dfr(
    1:n_splits,
    ~process_single_split_simple(.x, data = df_simple, splits_list = all_splits_simple, p = p),
    .options = furrr_options(seed = TRUE, packages = c("dplyr", "mgcv", "purrr"))
  )
})
plan(sequential)

# ================================================================================== #
# STAGE 5: FINAL AGGREGATION AND SAVE
# ================================================================================== #

# --- Summarize Simple Model Metrics ---
simple_results_means <- simple_models_output %>%
  group_by(Split, Model) %>%
  summarise(
    R2 = cor(actual, predicted)^2,
    RMSE = sqrt(mean((actual - predicted)^2)),
    RPD = sd(actual) / RMSE,
    Bias = mean(predicted - actual),
    PercentRMSE = RMSE / max(actual) * 100,
    .groups = "drop"
  ) %>%
  mutate(
    ModelType = "Simple",
    Min_Components = 3,
    Max_Components = 3,
    Component_Range = "3",
    N = 10 # 10 folds per split
  ) %>%
  rename(SplitSet = Split)

# --- Combine All Predictions ---
simple_predictions_df <- simple_models_output %>%
  select(
    split_set = Split,
    fold = Fold,
    model_variant = Model,
    specimen_number,
    actual,
    predicted
  ) %>%
  mutate(
    model_type = "Simple",
    components = 3
  )

all_predictions_final <- bind_rows(final_predictions_df, simple_predictions_df)

# --- Combine All Result Summaries ---
all_results_means_final <- bind_rows(all_results_means, simple_results_means)

# ================================================================================== #
# STAGE 6: SAVE RESULTS
# ================================================================================== #

cat("\n--- Saving final results... ---\n")

saveRDS(all_results_means_final, paste0("RDS_dataframes/all_results_means_parallel_updated_TEST", Sys.Date(), ".RDS"))
saveRDS(all_predictions_final, paste0("RDS_dataframes/all_predictions_parallel_updated_TEST", Sys.Date(), ".RDS"))
saveRDS(final_importance_summary, paste0("RDS_dataframes/final_importance_data_parallel_updated_TEST", Sys.Date(), ".RDS"))

cat("--- Script finished successfully! ---\n")
