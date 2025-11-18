# =============================================================================
#
# FT-NIRS MODELING PIPELINE - COMPLETE SINGLE-FILE SCRIPT
#
# =============================================================================

# --- 1. SETUP & CONFIGURATION ---
# =============================================================================

# --- A. Load Libraries ---
cat("--- Loading Libraries ---\n")
packages <- c("caret", "dplyr", "mgcv", "MuMIn", "purrr", "ranger", "stringr",
              "xgboost", "future", "furrr", "progressr", "mdatools", "cli")

installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}
invisible(lapply(packages, library, character.only = TRUE))


# --- B. CONTROL PANEL: CHOOSE WHICH MODELS TO RUN ---
# Set these to TRUE to run a model set, or FALSE to skip it.
RUN_PCA_MODELS  <- TRUE  # Includes PCA-LM and PCA-GAM
RUN_PLS_MODELS  <- TRUE
RUN_RF_MODEL    <- TRUE
RUN_XGB_MODEL   <- TRUE
RUN_SIMPLE_MODELS <- TRUE # Uses a different dataset, run separately if needed


# --- C. Global Parameters ---
N_SPLITS <- 500 # Number of different train/test data splits (e.g., 500)
N_FOLDS  <- 10   # K-folds for CV
N_CORES_PREP <- 8 # Cores for pre-computation (e.g., PLS ncomp)
N_CORES_MAIN <- 8 # Cores for the main analysis loop
set.seed(6)


# --- D. Load and Prepare Data ---
cat("--- Loading and Preparing Data ---\n")

# df <- readRDS("RDS_dataframes/IBM_SGpreproc.RDS")
df <- readRDS("RDS_dataframes/LPW_scan_avg_proc.RDS")

# Define key column names
RESPONSE_VAR    <- "read_age"
SPECIMEN_ID_VAR <- "specimen"
# This dynamically finds all columns that are numbers (your spectral data)
SPECTRAL_COLS   <- names(df)[grepl("^\\d", names(df))]

# Pre-processing
df <- df[complete.cases(df[[RESPONSE_VAR]]), ]

# --- E. Define Model Hyperparameters ---
BEST_PARAMS_XGB <- readRDS("Chapter 1/LPW_best_xgb_params_tuned_2025-10-31.RDS")
BEST_PARAMS_RF <- readRDS("Chapter 1/LPW_best_RF_params_tuned_2025-10-31.RDS")
BEST_PARAMS_RF <- BEST_PARAMS_RF$bestTune

# --- 2. FUNCTION DEFINITIONS ---
# =============================================================================
cat("--- Defining Helper Functions ---\n")

# --- Data Splitting & Metrics ---
generate_multiple_splits <- function(data, n_splits, k_folds = 10) {
  purrr::map(1:n_splits, ~caret::createFolds(data[[RESPONSE_VAR]], k = k_folds, list = TRUE, returnTrain = FALSE))
}
calculate_rpd <- function(observed, predicted) { sd(observed) / sqrt(mean((observed - predicted)^2)) }
calculate_bias <- function(observed, predicted) { mean(predicted - observed) }

# --- PCA Helper Functions ---
calculate_pca_vector_length <- function(pca_rotation, pc_indices) {
  if (length(pc_indices) == 0 || is.null(pc_indices)) return(rep(0, nrow(pca_rotation)))
  valid_indices <- pc_indices[!is.na(pc_indices)]
  if (length(valid_indices) == 0) return(rep(0, nrow(pca_rotation)))
  loadings_subset <- pca_rotation[, valid_indices, drop = FALSE]
  sqrt(rowSums(loadings_subset^2))
}
get_pc_indices_lm <- function(term) { as.numeric(stringr::str_extract(attr(term, "term.labels"), "\\d+")) }
get_pc_indices_gam <- function(formula) {
  pc_matches <- stringr::str_extract_all(as.character(formula)[3], "PC\\d+")[[1]]
  as.numeric(stringr::str_extract(pc_matches, "\\d+"))
}


extract_all_predictions_single <- function(model_results_for_one_fold) {
  
  # Helper to create a base data frame for predictions
  create_base_df <- function(preds_list) {
    tibble(
      specimen_number = preds_list$specimen_number,
      actual = preds_list$actual
    )
  }
  
  # List to hold the data frames for each model type that was run
  all_pred_dfs <- list()
  
  # --- Process each model type ONLY if its results exist ---
  
  if (!is.null(model_results_for_one_fold$lm)) {
    lm_preds <- model_results_for_one_fold$lm$predictions
    all_pred_dfs$lm <- map_dfr(1:10, ~{
      create_base_df(lm_preds) %>%
        mutate(model_type = "LM", model_variant = paste("LM", .x), predicted = lm_preds$model_preds[[.x]], components = lm_preds$model_comps[[.x]])
    })
  }
  
  if (!is.null(model_results_for_one_fold$gam)) {
    gam_preds <- model_results_for_one_fold$gam$predictions
    all_pred_dfs$gam <- map_dfr(1:10, ~{
      create_base_df(gam_preds) %>%
        mutate(model_type = "GAM", model_variant = paste("GAM", .x), predicted = gam_preds$model_preds[[.x]], components = gam_preds$model_comps[[.x]])
    })
  }
  
  if (!is.null(model_results_for_one_fold$pls)) {
    pls_preds <- model_results_for_one_fold$pls$predictions
    all_pred_dfs$pls <- create_base_df(pls_preds) %>%
      mutate(model_type = "PLS", model_variant = "PLS", predicted = pls_preds$pls_pred, components = pls_preds$pls_ncomp)
    all_pred_dfs$vip <- create_base_df(pls_preds) %>%
      mutate(model_type = "PLS", model_variant = "PLS-VIP", predicted = pls_preds$vip_pred, components = pls_preds$vip_ncomp)
  }
  
  if (!is.null(model_results_for_one_fold$rf)) {
    rf_preds <- model_results_for_one_fold$rf$predictions
    all_pred_dfs$rf <- create_base_df(rf_preds) %>%
      mutate(model_type = "RF", model_variant = "RF", predicted = rf_preds$rf_pred, components = rf_preds$components)
  }
  
  if (!is.null(model_results_for_one_fold$xgb)) {
    xgb_preds <- model_results_for_one_fold$xgb$predictions
    all_pred_dfs$xgb <- create_base_df(xgb_preds) %>%
      mutate(model_type = "XGB", model_variant = "XGBoost", predicted = xgb_preds$xgb_pred, components = xgb_preds$components)
  }
  
  # Combine all available prediction data frames
  bind_rows(all_pred_dfs)
}


# --- New Wrapper Function for Memory-Efficient Parallelization ---
process_split_set <- function(current_split_id, job_manifest, all_data, ...) {
  
  # 1. Get the 10 jobs (folds) for this specific split set
  jobs_for_split <- job_manifest %>% dplyr::filter(split_id == current_split_id)
  
  # 2. Run the analysis for all 10 folds sequentially INSIDE this worker
  # This returns a list of 10 outputs (one for each fold)
  fold_outputs_list <- purrr::map(1:nrow(jobs_for_split), function(j) {
    do.call(run_analysis_for_fold, c(list(job_row = jobs_for_split[j, ]), list(all_data=all_data, ...)))
  })
  
  # 3. AGGREGATE THE RESULTS (THE MEMORY SAVING STEP)
  
  # a. Combine and summarize performance metrics by taking the mean across the 10 folds
  results_summary <- purrr::map_dfr(fold_outputs_list, "results") %>%
    dplyr::group_by(Model, ModelType, SplitSet) %>%
    dplyr::summarise(
      across(c(R2, RMSE, RPD, Bias, PercentRMSE), mean, na.rm = TRUE),
      # --- NEW: Calculate range and mean of components ---
      Mean_Components = mean(Components, na.rm = TRUE),
      Min_Components = min(Components, na.rm = TRUE),
      Max_Components = max(Components, na.rm = TRUE),
      # --- End new ---
      .groups = "drop"
    )
  
  # b. Combine and summarize importance data by taking the mean across the 10 folds
  importance_summary <- purrr::map_dfr(fold_outputs_list, "importance") %>%
    dplyr::group_by(method, wavenumber, SplitSet) %>%
    dplyr::summarise(
      mean_importance = mean(importance, na.rm = TRUE),
      .groups = "drop"
    )
  
  # c. Combine all raw predictions (NO aggregation here, as requested)
  predictions_raw <- purrr::map_dfr(fold_outputs_list, "predictions")
  
  # 4. Return a list containing the two small summary tables and one raw prediction table
  return(list(
    results = results_summary,
    importance = importance_summary,
    predictions = predictions_raw
  ))
}





# --- Individual Model Runner Functions ---
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
run_pls_models_fast <- function(calibrate, testing, ncomp_pls) {
  mod_pls <- mdatools::pls(
    calibrate[, SPECTRAL_COLS],
    calibrate[, "read_age"],
    cv = NULL,           # NO internal cross-validation
    ncomp = ncomp_pls,     # USE THE PRE-CALCULATED VALUE
    scale = FALSE, 
    center = TRUE,
    x.test = testing[, SPECTRAL_COLS],
    y.test = testing[, "read_age"]
  )
  
  # --- Model 2: PLS with VIP Selection ---
  # Get VIP scores from the first model
  vip <- as.data.frame(vipscores(mod_pls))
  
  # Run the VIP model, but calculate ncomp for this separately.
  mod_vip <- mdatools::pls(
    calibrate[, SPECTRAL_COLS],
    calibrate[, "read_age"],
    cv = 1,  # LOOCV for VIP model
    ncomp = 10, # look across first 10 comps
    scale = FALSE, 
    center = TRUE,
    x.test = testing[, SPECTRAL_COLS],
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
  wavenumbers <- as.numeric(colnames(calibrate[, SPECTRAL_COLS]))
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
run_xgb_models_single <- function(calibrate, testing, fold_id, best_params_xgb) {
  
  # 1. PREPARE DATA for this fold
  # =================================================================
  # We only need DMatrix objects for the full calibrate and test sets
  x_calibrate <- as.matrix(calibrate[, SPECTRAL_COLS])
  y_calibrate <- calibrate$read_age
  dtrain_full <- xgb.DMatrix(data = x_calibrate, label = y_calibrate)
  
  x_test <- as.matrix(testing[, SPECTRAL_COLS])
  y_test <- testing$read_age
  dtest <- xgb.DMatrix(data = x_test, label = y_test)
  
  # 2. SET MODEL PARAMETERS using pre-tuned values
  # =================================================================
  
  # Get the stable, pre-calculated nrounds from your tuning script
  # We round it just in case 'mean_nrounds' is not an integer
  best_nrounds_for_fold <- round(best_params_xgb$mean_nrounds)
  
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
  
  
  RSS <- sum((testing$read_age - preds)^2)
  TSS <- sum((testing$read_age - mean(testing$read_age))^2)
  r2 = 1 - (RSS / TSS)
  rmse_val <- caret::RMSE(pred = preds, obs = testing$read_age)
  
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

run_rf_models_single <- function(calibrate, testing, fold_id, best_params_rf) {
  
  # 1. TRAIN THE MODEL for this single fold
  # =================================================================
  mod <- ranger(
    x = calibrate[, SPECTRAL_COLS],
    y = calibrate$read_age,
    mtry = best_params_rf$mtry,
    min.node.size = best_params_rf$min.node.size,
    seed = 6,
    importance = 'permutation', 
    num.trees = 1000,
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
  wavenumbers <- as.numeric(colnames(calibrate[, SPECTRAL_COLS]))
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

# --- Master Analysis Function for One Fold ---
run_analysis_for_fold <- function(job_row, all_data, ...) {
  args <- list(...)
  
  # 1. Prepare Data for this Fold
  train_indices <- job_row$train_indices[[1]]
  test_indices  <- job_row$test_indices[[1]]
  calibrate_raw <- all_data[train_indices, ]
  testing_raw   <- all_data[test_indices, ]
  
  # Initialize containers
  model_outputs <- list()
  importance_list <- list()
  
  # 2. Run Models based on Control Panel Switches
  
  # --- PCA Models ---
  if (RUN_PCA_MODELS) {
    pc.mod <- caret::preProcess(calibrate_raw[, SPECTRAL_COLS], method = c("center", "pca"), pcaComp = 10)
    calibrate_pca <- cbind(predict(pc.mod, calibrate_raw[, SPECTRAL_COLS]), calibrate_raw)
    testing_pca <- cbind(predict(pc.mod, testing_raw[, SPECTRAL_COLS]), testing_raw)
    
    lm_out <- run_lm_models_single(calibrate_pca, testing_pca, job_row$fold_id, args$terms_lm, args$pc_counts_lm)
    gam_out <- run_gam_models_single(calibrate_pca, testing_pca, job_row$fold_id, args$terms_gam, args$pc_counts_gam)
    model_outputs <- c(model_outputs, list(lm=lm_out, gam=gam_out))
    
    # Calculate PCA loading importance
    pca_rotation <- pc.mod$rotation
    wavenumbers <- as.numeric(rownames(pca_rotation))
    
    pca_importance_lm <- map_dfr(1:10, ~{
      pc_indices <- get_pc_indices_lm(args$terms_lm[[.x]])
      data.frame(method = paste0("PCA-LM", .x), wavenumber = wavenumbers, importance = calculate_pca_vector_length(pca_rotation, pc_indices))
    })
    pca_importance_gam <- map_dfr(1:10, ~{
      pc_indices <- get_pc_indices_gam(args$terms_gam[[.x]])
      data.frame(method = paste0("PCA-GAM", .x), wavenumber = wavenumbers, importance = calculate_pca_vector_length(pca_rotation, pc_indices))
    })
    importance_list <- c(importance_list, list(pca_lm=pca_importance_lm, pca_gam=pca_importance_gam))
  }
  
  # --- PLS Models ---
  if (RUN_PLS_MODELS) {
    pls_out <- run_pls_models_fast(calibrate_raw, testing_raw, job_row$optimal_ncomp)
    model_outputs$pls <- pls_out
    importance_list$pls <- pls_out$importance
  }
  
  # --- RF Model ---
  if (RUN_RF_MODEL) {
    rf_out <- run_rf_models_single(calibrate_raw, testing_raw, job_row$fold_id, BEST_PARAMS_RF)
    model_outputs$rf <- rf_out
    importance_list$rf <- rf_out$importance
  }
  
  # --- XGBoost Model ---
  if (RUN_XGB_MODEL) {
    xgb_out <- run_xgb_models_single(calibrate_raw, testing_raw, job_row$fold_id, BEST_PARAMS_XGB)
    model_outputs$xgb <- xgb_out
    importance_list$xgb <- xgb_out$importance
  }
  
  # 3. Combine and Return All Results for the Fold
  results_for_fold <- map_dfr(model_outputs, "results")
  predictions_for_fold <- extract_all_predictions_single(model_outputs) # Assumes you have this function
  importance_for_fold <- bind_rows(importance_list)
  
  # Add identifiers
  results_for_fold$SplitSet <- job_row$split_id
  predictions_for_fold$split_set <- job_row$split_id
  predictions_for_fold$fold <- job_row$fold_id
  importance_for_fold$SplitSet <- job_row$split_id
  importance_for_fold$Fold <- job_row$fold_id
  
  return(list(results = results_for_fold, predictions = predictions_for_fold, importance = importance_for_fold))
}


# --- 3. PRE-COMPUTATION & MODEL SELECTION ---
# =============================================================================

set.seed(6)

# --- A. Create the Job Manifest for All Folds ---
cat("--- Generating Job Manifest ---\n")
all_splits <- generate_multiple_splits(df, n_splits = N_SPLITS, k_folds = N_FOLDS)
job_manifest <- map_dfr(1:N_SPLITS, function(split_num) {
  folds_for_split <- all_splits[[split_num]]
  map_dfr(1:N_FOLDS, function(fold_num) {
    tibble(
      split_id = split_num,
      fold_id = fold_num,
      train_indices = list(unlist(folds_for_split[-fold_num])),
      test_indices = list(folds_for_split[[fold_num]])
    )
  })
})

# --- B. PCA-LM & GAM: Find Top 10 Models with Dredge ---
if (RUN_PCA_MODELS) {
  cat("--- Finding Top 10 PCA-based Models using Dredge ---\n")
  
  pca_temp <- mdatools::pca(df[, SPECTRAL_COLS], ncomp = 10, center = TRUE, scale = FALSE)
  pc_scores <- pca_temp$res$cal$scores
  colnames(pc_scores) <- paste0("PC", 1:10)
  model_data <- cbind(df[, RESPONSE_VAR, drop = FALSE], pc_scores)
  
  global_lm <- lm(read_age ~ ., data = model_data, na.action = "na.fail")
  global_gam <- mgcv::gam(data = model_data, read_age ~ s(PC1,k = 4) + s(PC2,k = 4) + s(PC3,k = 4) + s(PC4,k = 4) + s(PC5,k = 4) + s(PC6,k = 4) + s(PC7, k = 4) + s(PC8, k = 4) + s(PC9, k = 4) + s(PC10, k = 4), na.action = "na.fail"
  )
  
  top10_lm_models <- get.models(dredge(global_lm), subset = 1:10)
  top10_gam_models <- get.models(dredge(global_gam), subset = 1:10)
  
  # Extract terms/formulas and component counts for use in the parallel loop
  terms_lm <- lapply(top10_lm_models, function(m) m$terms)
  terms_gam <- lapply(top10_gam_models, function(m) m$formula)
  pc_counts_lm <- sapply(terms_lm, function(term) length(attr(term, "term.labels")))
  pc_counts_gam <- sapply(terms_gam, function(form) stringr::str_count(as.character(form)[3], "PC\\d+"))
  
  cat("✅ Top 10 LM and GAM models identified.\n")
}



### UNCOMMENT TO SAVE PC'S USED
# # Direct access to the 'formula' element for all GAM models
# gam_formulas_direct <- lapply(top10_gam_models, `[[`, "formula")
# # 1. Get the formulas (from the previous step)
# gam_formulas <- lapply(top10_gam_models, formula)
# # 2. Extract and format the predictor names for each model
# gam_predictors_list <- lapply(names(gam_formulas), function(model_id) {
#   # Get the terms object from the formula
#   model_terms <- terms(gam_formulas[[model_id]])
# 
#   # Get all predictor names (e.g., "s(PC1, k = 4)")
#   predictors <- attr(model_terms, "term.labels")
# 
#   # Extract just the "PC1", "PC3", etc.
#   pc_parts <- stringr::str_extract(predictors, "PC\\d+")
# 
#   # Extract just the numbers "1", "3", etc.
#   pc_numbers <- gsub("PC", "", pc_parts)
# 
#   # Create a row for the data frame
#   data.frame(
#     Model_ID = model_id,
#     Predictors = paste(pc_numbers, collapse = ", ") # Combine all into a single string
#   )
# })
# # 3. Combine the list of data frames into a single table
# gam_predictors_table <- do.call(rbind, gam_predictors_list)
# # Print the resulting table
# print("Top 10 GAM Models Predictor Table:")
# print(gam_predictors_table)
# 
# 
# # 1. Get the formulas (from the previous step)
# lm_formulas <- lapply(top10_lm_models, formula)
# 
# # 2. Extract and format the predictor names for each model
# lm_predictors_list <- lapply(names(lm_formulas), function(model_id) {
#   # Get the terms object from the formula
#   model_terms <- terms(lm_formulas[[model_id]])
# 
#   # Get all predictor names (e.g., "PC1", "PC3")
#   predictors <- attr(model_terms, "term.labels")
# 
#   # Extract just the numbers "1", "3", etc.
#   pc_numbers <- gsub("PC", "", predictors)
# 
#   # Create a row for the data frame
#   data.frame(
#     Model_ID = model_id,
#     Predictors = paste(pc_numbers, collapse = ", ") # Combine all into a single string
#   )
# })
# 
# # 3. Combine the list of data frames into a single table
# lm_predictors_table <- do.call(rbind, lm_predictors_list)
# 
# # Print the resulting table
# print("Top 10 LM Models Predictor Table:")
# print(lm_predictors_table)
# 
# timestamp <- format(Sys.Date(), "%Y-%m-%d")
# saveRDS(gam_predictors_table, paste0("Chapter 1/", "LPW_", "GAM_PCsused_", timestamp, ".RDS"))
# saveRDS(lm_predictors_table,  paste0("Chapter 1/", "LPW_", "LM_PCsused_", timestamp, ".RDS"))





Sys.setenv(OMP_NUM_THREADS = 1, OPENBLAS_NUM_THREADS = 1)
Sys.time()
# --- C. PLS: Pre-calculate Optimal NCOMP for Each Fold ---
if (RUN_PLS_MODELS) {
  cat("--- Pre-calculating optimal PLS components for", nrow(job_manifest), "folds... ---\n")
  
  # Define the function needed for the calculation
  find_optimal_ncomp <- function(train_idx, data) {
    calibrate <- data[train_idx, ]
    mod <- mdatools::pls(
      calibrate[, SPECTRAL_COLS], 
      calibrate[[RESPONSE_VAR]],
      cv = 1,
      ncomp = 10,
      scale = FALSE,
      center = TRUE
    )
    return(mod$ncomp.selected)
  }
  
  # Set up the parallel plan
  plan(multisession, workers = N_CORES_PREP)
  
  # Use future_map_dbl directly on the individual tasks
  # This lets furrr handle load balancing and provides a simple progress bar
  optimal_ncomps <- future_map_dbl(
    .x = job_manifest$train_indices,
    .f = ~ find_optimal_ncomp(.x, data = df),
    .options = furrr_options(seed = TRUE, packages = "mdatools"),
    .progress = TRUE  # Use furrr's efficient, built-in progress bar
  )
  
  # Return to sequential processing
  plan(sequential)
  
  # Assign the results directly
  job_manifest$optimal_ncomp <- optimal_ncomps
  cat("\n✅ Optimal PLS components calculated.\n")
}
Sys.time()
timestamp <- format(Sys.Date(), "%Y-%m-%d")
saveRDS(job_manifest, paste0("Chapter 1/", "job_manifest_", "LPW_", timestamp, ".RDS"))

# 42 minutes to run with 57 specimens.....
# 32 mins run again....???? Oh I changed no recalculating nrounds of boosting

# --- 4. MAIN ANALYSIS: RUN ALL MODELS IN PARALLEL ---
# =============================================================================

Sys.setenv(OMP_NUM_THREADS = 1, OPENBLAS_NUM_THREADS = 1)
cat("--- Starting Main Analysis for", N_SPLITS, "Split Sets ---\n")

# Prepare arguments once
analysis_args <- list(
  job_manifest = job_manifest,
  all_data = df, 
  terms_lm = if(exists("terms_lm")) terms_lm else NULL, 
  pc_counts_lm = if(exists("pc_counts_lm")) pc_counts_lm else NULL, 
  terms_gam = if(exists("terms_gam")) terms_gam else NULL, 
  pc_counts_gam = if(exists("pc_counts_gam")) pc_counts_gam else NULL
)

plan(multisession, workers = N_CORES_MAIN)
Sys.time()
# The parallel loop now iterates over each SPLIT_ID
# We removed handlers() and with_progress() and added .progress = TRUE
parallel_results_list <- future_map(
  .x = unique(job_manifest$split_id),
  .f = ~ do.call(process_split_set, c(list(current_split_id = .x), analysis_args)),
  .options = furrr_options(seed = TRUE, packages = c("caret", "mgcv", "mdatools", "ranger", "xgboost", "dplyr", "purrr", "stringr")),
  .progress = TRUE
)
Sys.time()
plan(sequential)
cat("\n✅ Main analysis complete.\n")

# FULL RUN TIME 35 MINUTES 

# --- 5. AGGREGATE RESULTS & SAVE ---
# =============================================================================
cat("--- Aggregating and Saving Final Results ---\n")

# Data is already summarized per SplitSet, just bind the rows
final_results_summary <- map_dfr(parallel_results_list, "results")
final_importance_summary <- map_dfr(parallel_results_list, "importance")
final_predictions_df <- map_dfr(parallel_results_list, "predictions")

# Clean up the large parallel object immediately to free memory
# rm(parallel_results_list)
gc()


temp1 <- final_results_summary
temp2 <- final_importance_summary
temp3 <- final_predictions_df

# --- 6. SIMPLE MODELS (OPTIONAL) ---
# =============================================================================
if (RUN_SIMPLE_MODELS) {
  cat("\n--- Running Simple Models Analysis ---\n")
  
  # --- A. Prepare Data and Splits for Simple Models ---
  df_simple <- readRDS("RDS_dataframes/LPW_scan_avg_proc.RDS")
  df_simple <- df_simple[complete.cases(df_simple$read_age), ]
  df_simple <- df_simple[complete.cases(df_simple$structure_weight), ]
  set.seed(6)
  all_splits_simple <- generate_multiple_splits(df_simple, n_splits = N_SPLITS)
  
  # --- B. Worker Function ---
  process_single_split_simple <- function(split_num, data, splits_list) {
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
      
      # Combine results for the fold
      bind_rows(
        tibble(Model = "Simple lm", predicted = preds_lm),
        tibble(Model = "Simple gam", predicted = preds_gam)
      ) %>%
        mutate(
          SplitSet = split_num,
          Fold = fold_num,
          specimen_number = rep(testing_data$specimen, 2),
          actual = rep(testing_data$read_age, 2)
        )
    })
  }
  
  # --- C. Run Models in Parallel ---
  cat("--- Running", N_SPLITS, "simple model splits... ---\n")
  plan(multisession, workers = N_CORES_MAIN)
  handlers(handler_progress(format = "[:bar] :percent | ETA: :eta"))
  
  with_progress({
    simple_models_output <- future_map_dfr(
      1:N_SPLITS,
      ~process_single_split_simple(.x, data = df_simple, splits_list = all_splits_simple),
      .options = furrr_options(seed = TRUE, packages = c("dplyr", "mgcv", "purrr"))
    )
  })
  plan(sequential)
  cat("\n✅ Simple models analysis complete.\n")
  
  # --- D. Calculate Performance Metrics and Combine Results ---
  cat("--- Integrating simple model results... ---\n")
  
  # 1. Calculate performance metrics for each model and split
  simple_results_summary <- simple_models_output %>%
    group_by(Model, SplitSet) %>%
    summarise(
      # Calculate Sums of Squares
      RSS = sum((actual - predicted)^2),
      TSS = sum((actual - mean(actual))^2),
      n = n(),
      
      # Compute Metrics using RSS/TSS (Standard definition)
      RMSE = sqrt(RSS / n),
      R2 = 1 - (RSS / TSS),
      RPD = sd(actual) / sqrt(RSS / n),
      Bias = mean(predicted - actual),
      PercentRMSE = (sqrt(RSS / n) / max(actual)) * 100,
      .groups = "drop"
    ) %>%
    select(-RSS, -TSS, -n) %>% # Remove helper columns
    mutate(
      ModelType = "Simple",
      # Add component columns to match the schema of the main analysis results
      Components = 3,
      Mean_Components = 3,
      Min_Components = 3,
      Max_Components = 3
    )
  
  # 2. Format the predictions to match the complex models' output
  simple_predictions_df <- simple_models_output %>%
    select(
      split_set = SplitSet,
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
  
  # 3. Combine with the main results dataframes
  final_results_summary <- bind_rows(final_results_summary, simple_results_summary)
  final_predictions_df <- bind_rows(final_predictions_df, simple_predictions_df)
  
  cat("✅ Simple model results integrated.\n")
  # # --- D. Calculate Performance Metrics and Combine Results ---
  # cat("--- Integrating simple model results... ---\n")
  # 
  # # 1. Calculate performance metrics for each model and split
  # simple_results_summary <- simple_models_output %>%
  #   group_by(Model, SplitSet) %>%
  #   summarise(
  #     R2 = cor(actual, predicted)^2,
  #     RMSE = sqrt(mean((actual - predicted)^2)),
  #     RPD = sd(actual) / RMSE,
  #     Bias = mean(predicted - actual),
  #     PercentRMSE = RMSE / max(actual) * 100,
  #     .groups = "drop"
  #   ) %>%
  #   mutate(
  #     ModelType = "Simple",
  #     Components = 3 # length + structure_weight + weight
  #   )
  # 
  # # 2. Format the predictions to match the complex models' output
  # simple_predictions_df <- simple_models_output %>%
  #   select(
  #     split_set = SplitSet,
  #     fold = Fold,
  #     model_variant = Model,
  #     specimen_number,
  #     actual,
  #     predicted
  #   ) %>%
  #   mutate(
  #     model_type = "Simple",
  #     components = 3
  #   )
  # 
  # # 3. Combine with the main results dataframes
  # final_results_summary <- bind_rows(final_results_summary, simple_results_summary)
  # final_predictions_df <- bind_rows(final_predictions_df, simple_predictions_df)
  # 
  # cat("✅ Simple model results integrated.\n")
}

# --- 7. FINAL SAVE ---
# =============================================================================
# IMPORTANT: The save commands are now here, at the very end of the script,
# to ensure simple models are included if they were run.

cat("\n--- Saving All Final Results ---\n")

timestamp <- format(Sys.Date(), "%Y-%m-%d")
saveRDS(final_results_summary, paste0("Chapter 1/", "LPW", "_SUMMARY_all_models_", timestamp, ".RDS"))
saveRDS(final_predictions_df, paste0("Chapter 1/", "LPW", "_PREDICTIONS_all_models_", timestamp, ".RDS"))
saveRDS(final_importance_summary, paste0("Chapter 1/", "LPW", "_IMPORTANCE_all_models_", timestamp, ".RDS"))

cat("✅ All results saved.\n")
cat("\n--- Script finished successfully! ---\n")
