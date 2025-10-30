# ================================================================================= #
# IMPROVED XGBoost Hyperparameter Tuning with Holdout Validation
# ================================================================================= #

library(doSNOW)
library(xgboost)

# --- 1. Prepare full dataset for nested CV ---
set.seed(6)

x_all <- as.matrix(df[, 21:ncol(df)])
y_all <- df$read_age

# --- 2. Same focused grid ---
focused_grid <- expand.grid(
  eta = c(0.01, 0.05, 0.1, 0.2, 0.3),
  max_depth = c(1, 2, 3, 4, 6, 8),
  min_child_weight = c(1, 2, 4, 6, 8, 10, 15, 20, 30),
  colsample_bytree = c(0.2, 0.3, 0.4, 0.6, 0.8),
  gamma = c(0, 0.1, 1, 5),
  subsample = c(0.2, 0.4, 0.6, 0.8, 1)
)

set.seed(6)
my_random_grid <- focused_grid[sample(1:nrow(focused_grid), 3000), ]

cat("Grid size:", nrow(my_random_grid), "combinations\n")
cat("Dataset size:", nrow(x_all), "\n\n")

# --- 3. Parallel backend ---
cl <- makePSOCKcluster(parallel::detectCores() - 1)
registerDoSNOW(cl)

pb <- txtProgressBar(max = nrow(my_random_grid), style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

# --- 4. Internal CV tuning ---
cat("Starting XGBoost internal CV tuning...\n")
start_time <- Sys.time()

tuning_results <- foreach(
  i = 1:nrow(my_random_grid),
  .combine = 'rbind',
  .packages = 'xgboost',
  .options.snow = opts
) %dopar% {
  
  dtrain <- xgb.DMatrix(data = x_all, label = y_all)
  
  params <- list(
    objective = "reg:squarederror",
    booster = "gbtree",
    eta = my_random_grid$eta[i],
    max_depth = my_random_grid$max_depth[i],
    min_child_weight = my_random_grid$min_child_weight[i],
    subsample = my_random_grid$subsample[i],
    gamma = my_random_grid$gamma[i],
    colsample_bytree = my_random_grid$colsample_bytree[i],
    nthread = 1
  )
  
  xgb_cv_model <- xgb.cv(
    params = params,
    data = dtrain,
    nrounds = 1000,
    nfold = 10, 
    early_stopping_rounds = 30,
    metrics = "rmse",
    verbose = FALSE
  )
  
  data.frame(
    iteration = i,
    best_nrounds = xgb_cv_model$best_iteration,
    cv_rmse = xgb_cv_model$evaluation_log$test_rmse_mean[xgb_cv_model$best_iteration],
    my_random_grid[i, ]
  )
}

close(pb)
end_time <- Sys.time()
stopCluster(cl)

cat("\nTuning finished.\n")
print(end_time - start_time)

# --- 5. Best params by CV RMSE ---
cat("\n=== Best Hyperparameters (by CV RMSE) ===\n")
(best_params_row <- tuning_results[which.min(tuning_results$cv_rmse), ])


# --- 6. Top 10 ---
cat("\n=== Top 10 Parameter Sets ===\n")
(top_10 <- tuning_results %>%
  arrange(cv_rmse) %>%
  head(10))

# --- 7. Final model CV on full data with best params ---
cat("\n=== Final 10-fold CV with Best Params ===\n")

dtrain_full <- xgb.DMatrix(data = x_all, label = y_all)

final_params <- list(
  objective = "reg:squarederror",
  booster = "gbtree",
  eta = best_params_row$eta,
  max_depth = best_params_row$max_depth,
  min_child_weight = best_params_row$min_child_weight,
  subsample = best_params_row$subsample,
  gamma = best_params_row$gamma,
  colsample_bytree = best_params_row$colsample_bytree,
  nthread = parallel::detectCores() - 1
)

final_cv <- xgb.cv(
  params = final_params,
  data = dtrain_full,
  nrounds = 1500,
  nfold = 10,
  early_stopping_rounds = 30,
  metrics = "rmse",
  verbose = TRUE
)

cat("\nFinal 10-fold CV RMSE:", final_cv$evaluation_log$test_rmse_mean[final_cv$best_iteration], "\n")
cat("Optimal nrounds:", final_cv$best_iteration, "\n")

# --- 8. Save results ---
best_params_xgb <- data.frame(
  nrounds = final_cv$best_iteration,
  max_depth = best_params_row$max_depth,
  eta = best_params_row$eta,
  gamma = best_params_row$gamma,
  colsample_bytree = best_params_row$colsample_bytree,
  min_child_weight = best_params_row$min_child_weight,
  subsample = best_params_row$subsample
)

saveRDS(best_params_xgb, "best_xgb_params_tuned.RDS")
saveRDS(tuning_results, "xgb_tuning_results_full.RDS")

cat("\n✓ Internal 10-fold CV tuning complete! Best parameters saved.\n")




#### MULTI CV

# === SETUP ===
num_repeats <- 5     # number of repeated 10-fold CV runs
repeat_results <- list()

for (rep in 1:num_repeats) {
  cat("\n===============================\n")
  cat("🔁 Starting CV repeat", rep, "of", num_repeats, "\n")
  cat("===============================\n")
  
  # set a unique random seed each run
  set.seed(1000 + rep)
  
  # --- 3. Set up parallel backend ---
  cl <- makePSOCKcluster(parallel::detectCores() - 1)
  registerDoSNOW(cl)
  
  pb <- txtProgressBar(max = nrow(my_random_grid), style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  
  # --- 4. CV-based tuning loop (same as before, no holdout set now) ---
  tuning_results <- foreach(
    i = 1:nrow(my_random_grid),
    .combine = 'rbind',
    .packages = 'xgboost',
    .options.snow = opts
  ) %dopar% {
    
    dtrain <- xgb.DMatrix(data = as.matrix(df[, 21:ncol(df)]), label = df$read_age)
    
    params <- list(
      objective = "reg:squarederror",
      booster = "gbtree",
      eta = my_random_grid$eta[i],
      max_depth = my_random_grid$max_depth[i],
      min_child_weight = my_random_grid$min_child_weight[i],
      subsample = my_random_grid$subsample[i],
      gamma = my_random_grid$gamma[i],
      colsample_bytree = my_random_grid$colsample_bytree[i],
      nthread = 1
    )
    
    xgb_cv <- xgb.cv(
      params = params,
      data = dtrain,
      nrounds = 1000,
      nfold = 10,
      early_stopping_rounds = 30,
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
  
  # --- 5. Find best params this run ---
  best_row <- tuning_results[which.min(tuning_results$cv_rmse), ]
  repeat_results[[rep]] <- best_row
  
  cat("\nBest params for repeat", rep, ":\n")
  print(best_row)
  
  # Optional: save each run’s results
  saveRDS(tuning_results, paste0("xgb_tuning_repeat_", rep, ".RDS"))
}

all_results <- bind_rows(all_runs)

# Average RMSE across repeats for each parameter combination
agg_results <- all_results %>%
  group_by(
    eta, max_depth, min_child_weight,
    subsample, gamma, colsample_bytree
  ) %>%
  summarise(
    mean_rmse = mean(cv_rmse),
    sd_rmse   = sd(cv_rmse),
    mean_nrounds = mean(best_nrounds),
    .groups = "drop"
  ) %>%
  arrange(mean_rmse)

cat("\n\n=== AVERAGED RESULTS ACROSS REPEATED 10-FOLD CVs ===\n")
print(head(agg_results, 10))

# Best averaged parameter set
best_tune <- agg_results[which.min(agg_results$mean_rmse), ]
cat("\n\n=== BEST AVERAGED PARAMETER SET ===\n")
print(best_tune)

# Optional: Save the aggregated summary
saveRDS(agg_results, "xgb_tuning_aggregated_results.RDS")

cat("\n✓ Repeated CV tuning complete — averaged results saved.\n")




file_list <- paste0("TUNING/xgb_tuning_repeat_", 1:num_repeats, ".RDS")

all_results <- bind_rows(lapply(file_list, readRDS))

# --- 2. Corrected Aggregation and Analysis ---

# NOTE: The column name for the best number of rounds is 'cv_best_nrounds' in the tuning loop,
#       but your aggregation summary used 'best_nrounds'. This must be corrected.

# Average RMSE across repeats for each parameter combination
agg_results <- all_results %>%
  # Corrected the column names to match the tuning_results data frame
  group_by(
    eta, max_depth, min_child_weight,
    subsample, gamma, colsample_bytree
  ) %>%
  summarise(
    mean_rmse = mean(cv_rmse),
    sd_rmse   = sd(cv_rmse),
    mean_nrounds = mean(cv_best_nrounds), # Corrected column name from 'best_nrounds'
    .groups = "drop"
  ) %>%
  arrange(mean_rmse)

cat("\n\n=== AVERAGED RESULTS ACROSS REPEATED 10-FOLD CVs ===\n")
print(head(agg_results, 10))

# Best averaged parameter set
best_avg <- agg_results[which.min(agg_results$mean_rmse)+1, ] # +1 since the second rank seemed best for being conservative-ish
cat("\n\n=== BEST AVERAGED PARAMETER SET ===\n")
print(best_avg)



# going to opt for a slightly more conservative model (rank 2)



# Optional: Save the aggregated summary
saveRDS(agg_results, "xgb_tuning_aggregated_results.RDS")

cat("\n✓ Repeated CV tuning complete — averaged results saved.\n")











# RANDOM FOREST #
# Load necessary libraries
library(caret)
library(doParallel)
library(ranger)
library(dplyr) 

# --- 1. Prepare Data and Define Constants ---
# Assuming 'df' is available and correctly structured
p <- ncol(df) - 20 

# --- 2. Set up the parallel backend (Skipped for brevity, assume 'cl' is active) ---
# ... (your existing parallel setup) ...

# --- 3. Define the Explicit Hyperparameter Grid ---

tuning_grid_rf <- expand.grid(
  mtry = c(floor(sqrt(ncol(df[, 21:ncol(df)])) * 0.1),
           floor(sqrt(ncol(df[, 21:ncol(df)])) * 0.2),
           floor(ncol(df[, 21:ncol(df)]) / 3),
           floor(ncol(df[, 21:ncol(df)]) / 2)),
  min.node.size = c(1, 3, 5, 15, 25, 35),
  splitrule = "variance" # Required for regression with ranger in caret
)

# Filter the grid
tuning_grid_rf <- tuning_grid_rf %>%
  filter(mtry > 0, mtry <= p) %>%
  unique()

cat("Tuning grid size:", nrow(tuning_grid_rf), "combinations\n")
# Expected size: 3 mtry * 4 min.node.size * 3 sample.fraction = 36 combinations

# --- 4. Define the Training Control (5 Repeats of 10-Fold CV) ---
train_control <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 5,
  search = "grid",
  allowParallel = TRUE,
  # Progress Bar
  verboseIter = TRUE 
)

# --- 5. Train the Model ---
cat("\nStarting Random Forest tuning...\n")

# NOTE: Since 'sample.fraction' is now in the tuneGrid, do NOT pass it here.
rf_tuned_model <- train(
  x = df[, 21:ncol(df)],
  y = df$read_age,
  method = "ranger",
  trControl = train_control,
  tuneGrid = tuning_grid_rf, # The full 4-column grid is passed here
  importance = 'permutation',
  num.trees = 3000, 
  num.threads = 1 
)

# ... (Finalization and Reporting) ...

