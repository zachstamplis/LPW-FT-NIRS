# ================================================================================= #
# IMPROVED XGBoost Hyperparameter Tuning with Holdout Validation
# ================================================================================= #


library(parallel)
library(doSNOW)
library(xgboost)


# df <- readRDS("RDS_dataframes/IBM_SGpreproc.RDS")
# df <- df[complete.cases(df[["read_age"]]), ]

df <- readRDS("RDS_dataframes/LPW_scan_avg_proc_UPDATED.RDS")
df <- df[complete.cases(df$read_age), ]
spec_cols   <- names(df)[grepl("^\\d", names(df))]

# --- 1. Prepare full dataset for nested CV ---
x_all <- as.matrix(df[, spec_cols])
y_all <- df$read_age

# --- 2. Same focused grid ---
search_grid <- expand.grid(
  colsample_bytree = c(0.2, 0.3, 0.4, 0.6, 0.8),
  eta = c(0.01, 0.05, 0.1, 0.2, 0.3),
  gamma = c(0, 0.1, 1, 5),
  max_depth = c(1, 2, 3, 4, 6, 8),
  min_child_weight = c(1, 2, 4, 6, 8, 10, 20, 30),
  subsample = c(0.2, 0.4, 0.6, 0.8, 1)
)

set.seed(6)
my_random_grid <- search_grid[sample(1:nrow(search_grid), 3000), ]

cat("Grid size:", nrow(my_random_grid), "combinations\n")
cat("Dataset size:", nrow(x_all), "\n\n")


#### MULTI CV

# --- 1. Setup (assuming df, spec_cols, my_random_grid exist) ---
num_repeats <- 3 
all_tuning_results <- list() # <-- CHANGE 1: Initialize a list to hold all results

Sys.time()

# --- 2. Main Loop for Repeated CV ---
for (rep in 1:num_repeats) {
  cat("\n===============================\n")
  cat("🔁 Starting CV repeat", rep, "of", num_repeats, "\n")
  cat("===============================\n")
  
  # Set a unique random seed each run
  set.seed(6 + rep)
  
  # --- 3. Set up parallel backend ---
  cl <- makePSOCKcluster(16)
  registerDoSNOW(cl)
  
  pb <- txtProgressBar(max = nrow(my_random_grid), style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  
  # --- 4. CV-based tuning loop (same as before) ---
  tuning_results <- foreach(
    i = 1:nrow(my_random_grid),
    .combine = 'rbind',
    .packages = 'xgboost',
    .options.snow = opts
  ) %dopar% {
    
    dtrain <- xgb.DMatrix(data = as.matrix(df[, spec_cols]), label = df$read_age)
    
    params <- list(
      objective = "reg:squarederror",
      booster = "gbtree",
      nthread = 1,
      colsample_bytree = my_random_grid$colsample_bytree[i],
      eta = my_random_grid$eta[i],
      gamma = my_random_grid$gamma[i],
      max_depth = my_random_grid$max_depth[i],
      min_child_weight = my_random_grid$min_child_weight[i],
      subsample = my_random_grid$subsample[i]
    )
    
    xgb_cv <- xgb.cv(
      params = params,
      data = dtrain,
      nrounds = 1000,
      nfold = 10,
      early_stopping_rounds = 20,
      metrics = "rmse",
      verbose = FALSE
    )
    
    data.frame(
      iteration = i,
      cv_best_nrounds = xgb_cv$best_iteration,
      cv_rmse = xgb_cv$evaluation_log$test_rmse_mean[xgb_cv$best_iteration],
      my_random_grid[i, ]
    )
  }
  
  close(pb)
  stopCluster(cl)
  
  # --- 5. Store results from this run ---
  all_tuning_results[[rep]] <- tuning_results # <-- CHANGE 2: Store the entire data frame
  
  # The old code for finding the best row and saving RDS is no longer needed here.
}

Sys.time()

# Combine the list of data frames into one large data frame
final_results <- bind_rows(all_tuning_results)

# Define the columns that identify a unique hyperparameter set
hyperparam_cols <- c( "colsample_bytree", "eta", "gamma", "max_depth", "min_child_weight", 
                      "subsample")

# Group by hyperparameters, summarize, and sort by mean_rmse
agg_results <- final_results %>%
  group_by(across(all_of(hyperparam_cols))) %>%
  summarise(
    mean_rmse = mean(cv_rmse),
    sd_rmse = sd(cv_rmse),
    mean_nrounds = mean(cv_best_nrounds),
    .groups = 'drop' # Recommended to ungroup after summarising
  ) %>%
  arrange(mean_rmse)

# --- 7. View the top results ---
best_params_xgb <- print(head(agg_results, 1))


timestamp <- format(Sys.Date(), "%Y-%m-%d")
saveRDS(best_params_xgb, paste0("LPW_best_xgb_params_tuned_", timestamp, ".RDS"))
saveRDS(agg_results, paste0("LPW_xgb_tuning_results_full_", timestamp, ".RDS"))














# 
# 
# # --- 3. Parallel backend ---
# cl <- makePSOCKcluster(parallel::detectCores()-1)
# registerDoSNOW(cl)
# 
# pb <- txtProgressBar(max = nrow(my_random_grid), style = 3)
# progress <- function(n) setTxtProgressBar(pb, n)
# opts <- list(progress = progress)
# 
# # --- 4. Internal CV tuning ---
# cat("Starting XGBoost internal CV tuning...\n")
# start_time <- Sys.time()
# 
# tuning_results <- foreach(
#   i = 1:nrow(my_random_grid),
#   .combine = 'rbind',
#   .packages = 'xgboost',
#   .options.snow = opts
# ) %dopar% {
#   
#   dtrain <- xgb.DMatrix(data = x_all, label = y_all)
#   
#   params <- list(
#     objective = "reg:squarederror",
#     booster = "gbtree",
#     eta = my_random_grid$eta[i],
#     max_depth = my_random_grid$max_depth[i],
#     min_child_weight = my_random_grid$min_child_weight[i],
#     subsample = my_random_grid$subsample[i],
#     gamma = my_random_grid$gamma[i],
#     colsample_bytree = my_random_grid$colsample_bytree[i],
#     nthread = 1
#   )
#   
#   xgb_cv_model <- xgb.cv(
#     params = params,
#     data = dtrain,
#     nrounds = 1000,
#     nfold = 10, 
#     early_stopping_rounds = 20,
#     metrics = "rmse",
#     verbose = FALSE
#   )
#   
#   data.frame(
#     iteration = i,
#     best_nrounds = xgb_cv_model$best_iteration,
#     cv_rmse = xgb_cv_model$evaluation_log$test_rmse_mean[xgb_cv_model$best_iteration],
#     my_random_grid[i, ]
#   )
# }
# 
# close(pb)
# end_time <- Sys.time()
# stopCluster(cl)
# 
# cat("\nTuning finished.\n")
# print(end_time - start_time)
# 
# # --- 5. Best params by CV RMSE ---
# cat("\n=== Best Hyperparameters (by CV RMSE) ===\n")
# (best_params_row <- tuning_results[which.min(tuning_results$cv_rmse), ])
# 
# 
# # --- 6. Top 10 ---
# cat("\n=== Top 10 Parameter Sets ===\n")
# (top_10 <- tuning_results %>%
#   arrange(cv_rmse) %>%
#   head(10))
# 
# # --- 7. Final model CV on full data with best params ---
# cat("\n=== Final 10-fold CV with Best Params ===\n")
# 
# dtrain_full <- xgb.DMatrix(data = x_all, label = y_all)
# 
# final_params <- list(
#   objective = "reg:squarederror",
#   booster = "gbtree",
#   eta = best_params_row$eta,
#   max_depth = best_params_row$max_depth,
#   min_child_weight = best_params_row$min_child_weight,
#   subsample = best_params_row$subsample,
#   gamma = best_params_row$gamma,
#   colsample_bytree = best_params_row$colsample_bytree,
#   nthread = 1
# )
# 
# final_cv <- xgb.cv(
#   params = final_params,
#   data = dtrain_full,
#   nrounds = 1000,
#   nfold = 10,
#   early_stopping_rounds = 20,
#   metrics = "rmse",
#   verbose = TRUE
# )
# 
# cat("\nFinal 10-fold CV RMSE:", final_cv$evaluation_log$test_rmse_mean[final_cv$best_iteration], "\n")
# cat("Optimal nrounds:", final_cv$best_iteration, "\n")
# 
# # --- 8. Save results ---
# (best_params_xgb <- data.frame(
#   nrounds = final_cv$best_iteration,
#   max_depth = best_params_row$max_depth,
#   eta = best_params_row$eta,
#   gamma = best_params_row$gamma,
#   colsample_bytree = best_params_row$colsample_bytree,
#   min_child_weight = best_params_row$min_child_weight,
#   subsample = best_params_row$subsample
# ))
# 
# timestamp <- format(Sys.Date(), "%Y-%m-%d")
# saveRDS(best_params_xgb, paste0("IBM_best_xgb_params_tuned.RDS", timestamp, ".RDS"))
# saveRDS(tuning_results, paste0("_BM_xgb_tuning_results_full", timestamp, ".RDS"))
# 
# cat("\n✓ Internal 10-fold CV tuning complete! Best parameters saved.\n")






















library(doParallel)

# --- 1. Set up the Parallel Backend ---
# Find the number of available cores, leave one free for the OS
cores_to_use <- 8
cat("Using", cores_to_use, "cores for parallel processing...\n")

# Create the cluster object
cl <- makePSOCKcluster(cores_to_use)

# *** THIS IS THE CRITICAL STEP ***
# Register the cluster as the parallel backend
registerDoParallel(cl)


# --- 2. Define the Hyperparameter Grid ---
# NOTE: Your original comment mentioned 'sample.fraction' but it wasn't in the grid.
# I have kept the grid as it was in your code.
tuning_grid_rf <- expand.grid(
  mtry = c(floor(sqrt(ncol(df[, spec_cols])) * 0.1),
           floor(sqrt(ncol(df[, spec_cols])) * 0.2),
           floor(ncol(df[, spec_cols]) / 3),
           floor(ncol(df[, spec_cols]) / 2)),
  min.node.size = c(1, 3, 5, 15, 25, 35),
  splitrule = "variance" # Required for regression with ranger in caret
)

p <- 933 # number of wavenumbers
tuning_grid_rf <- tuning_grid_rf %>%
  filter(mtry > 0, mtry <= p) %>%
  unique()

cat("Tuning grid size:", nrow(tuning_grid_rf), "combinations\n")

# --- 3. Define the Training Control ---
train_control <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 3,
  search = "grid",
  allowParallel = TRUE, # This tells caret to look for the registered backend
  verboseIter = TRUE
)

Sys.time()
# --- 4. Train the Model ---
cat("\nStarting Random Forest tuning...\n")

# Use a tryCatch block to ensure the cluster is always stopped
rf_tuned_model <- NULL
tryCatch({
  rf_tuned_model <- train(
    x = df[, spec_cols],
    y = df$read_age,
    method = "ranger",
    trControl = train_control,
    tuneGrid = tuning_grid_rf,
    importance = 'permutation',
    num.trees = 1000,
    # Let ranger use the parallel backend for its own internal threading if needed
    # But the main parallelization is handled by caret distributing CV folds
    num.threads = 1
  )
}, error = function(e) {
  # Print the error
  cat("An error occurred during training: ", e$message, "\n")
}, finally = {
  # --- 5. IMPORTANT: Stop the Cluster ---
  # This releases the cores back to your system.
  # The 'finally' block ensures this runs even if the training fails.
  cat("Stopping the parallel backend...\n")
  stopCluster(cl)
  # It's also good practice to deregister it, although stopCluster usually suffices
  registerDoSEQ()
})


Sys.time()
# --- 6. View Results ---
if (!is.null(rf_tuned_model)) {
  print(rf_tuned_model)
  plot(rf_tuned_model)
}
rf_tuned_model$bestTune
saveRDS(rf_tuned_model, paste0("LPW_best_RF_params_tuned_", timestamp, ".RDS"))

# # RANDOM FOREST #
# # Load necessary libraries
# library(caret)
# library(doParallel)
# library(ranger)
# library(dplyr) 
# 
# # --- 1. Prepare Data and Define Constants ---
# # Assuming 'df' is available and correctly structured
# p <- 933
# 
# # --- 2. Set up the parallel backend (Skipped for brevity, assume 'cl' is active) ---
# # ... (your existing parallel setup) ...
# 
# # --- 3. Define the Explicit Hyperparameter Grid ---
# 
# tuning_grid_rf <- expand.grid(
#   mtry = c(floor(sqrt(ncol(df[, spec_cols])) * 0.1),
#            floor(sqrt(ncol(df[, spec_cols])) * 0.2),
#            floor(ncol(df[, spec_cols]) / 3),
#            floor(ncol(df[, spec_cols]) / 2)),
#   min.node.size = c(1, 3, 5, 15, 25, 35),
#   splitrule = "variance" # Required for regression with ranger in caret
# )
# 
# # Filter the grid
# tuning_grid_rf <- tuning_grid_rf %>%
#   filter(mtry > 0, mtry <= p) %>%
#   unique()
# 
# cat("Tuning grid size:", nrow(tuning_grid_rf), "combinations\n")
# # Expected size: 3 mtry * 4 min.node.size * 3 sample.fraction = 36 combinations
# 
# # --- 4. Define the Training Control (5 Repeats of 10-Fold CV) ---
# train_control <- trainControl(
#   method = "repeatedcv",
#   number = 10,
#   repeats = 5,
#   search = "grid",
#   allowParallel = TRUE,
#   # Progress Bar
#   verboseIter = TRUE 
# )
# 
# # --- 5. Train the Model ---
# cat("\nStarting Random Forest tuning...\n")
# 
# # NOTE: Since 'sample.fraction' is now in the tuneGrid, do NOT pass it here.
# rf_tuned_model <- train(
#   x = df[, spec_cols],
#   y = df$read_age,
#   method = "ranger",
#   trControl = train_control,
#   tuneGrid = tuning_grid_rf, # The full 4-column grid is passed here
#   importance = 'permutation',
#   num.trees = 3000, 
#   num.threads = 1 
# )
# 
# # ... (Finalization and Reporting) ...
# 
