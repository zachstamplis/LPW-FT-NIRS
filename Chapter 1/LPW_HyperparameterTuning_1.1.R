# ================================================================================= #
# IMPROVED XGBoost Hyperparameter Tuning with Holdout Validation
# ================================================================================= #

library(caret)
library(parallel)
library(doSNOW)
library(xgboost)


# df <- readRDS("RDS_dataframes/IBM_SGpreproc.RDS")
# df <- df[complete.cases(df[["read_age"]]), ]

df <- readRDS("RDS_dataframes/LPW_scan_avg_proc.RDS")
df <- df[, is.na(as.numeric(names(df))) | (as.numeric(names(df)) <= 7500)]
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
  cl <- makePSOCKcluster(8)
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
# ~ 15 minutes for 3 repeated 10-fold CV.  

# WITH FILTERED WAVES: 


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
saveRDS(best_params_xgb, paste0("Model Results/LPW_best_xgb_params_tuned_", timestamp, ".RDS"))
saveRDS(agg_results, paste0("Model Results/LPW_xgb_tuning_results_full_", timestamp, ".RDS"))













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

p <- as.numeric(length(spec_cols)) # number of wavenumbers
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
# under a minute to run 10-fold CV 3 times

# --- 6. View Results ---
if (!is.null(rf_tuned_model)) {
  print(rf_tuned_model)
  plot(rf_tuned_model)
}
rf_tuned_model$bestTune
saveRDS(rf_tuned_model, paste0("Model Results/LPW_best_RF_params_tuned_", timestamp, ".RDS"))

