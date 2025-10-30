# --- New XGBoost Tuning with Early Stopping ---


library(doSNOW) # CHANGED: Use doSNOW instead of doParallel
install.packages("doSNOW")
# 1. Prepare your data
# Make sure your feature matrix and label are ready
x_matrix <- as.matrix(df[, 21:ncol(df)])
y_label <- df$read_age


my_full_grid <- expand.grid(
  eta = c(0.01, 0.05, 0.1, 0.2, 0.3),
  max_depth = c(1, 2, 3, 4, 6, 8),
  min_child_weight = c(1, 2, 4, 6, 8, 10, 15, 20, 30),
  colsample_bytree = c(0.2, 0.3, 0.4, 0.6, 0.8),
  gamma = c(0, 0.1, 1, 5),
  subsample = c(0.2, 0.4, 0.6, 0.8, 1)
)


# Note: I've reduced the grid size for this example. 
# Your larger grid is fine, but this will be faster to demonstrate.
set.seed(6)
my_random_grid <- my_full_grid[sample(1:nrow(my_full_grid), 1000), ] # Tune 100 random combos

# 3. Set up a parallel backend
# Using doParallel with foreach is a great way to parallelize this loop
cl <- makePSOCKcluster(parallel::detectCores() - 1)
registerDoParallel(cl)

# 4. Loop through the grid and run xgb.cv for each parameter set
cat("Starting XGBoost tuning with early stopping...\n")
start_time <- Sys.time()

# The `foreach` loop will run in parallel
tuning_results <- foreach(
  i = 1:nrow(my_random_grid), 
  .combine = 'rbind', 
  .packages = 'xgboost'
) %dopar% {
  dtrain <- xgb.DMatrix(data = x_matrix, label = y_label)
  
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
  
  # Run cross-validation with early stopping
  xgb_cv_model <- xgb.cv(
    params = params,
    data = dtrain,
    nrounds = 2000,             # A high number, we'll stop early
    nfold = 10,                  # 10-fold CV
    early_stopping_rounds = 30, # Stop if test RMSE doesn't improve for 20 rounds
    metrics = "rmse",
    verbose = FALSE
  )
  
  # Return a data frame with the results for this iteration
  data.frame(
    iteration = i,
    best_nrounds = xgb_cv_model$best_iteration,
    best_rmse = xgb_cv_model$evaluation_log$test_rmse_mean[xgb_cv_model$best_iteration],
    my_random_grid[i, ]
  )
}

end_time <- Sys.time()
stopCluster(cl)

cat("Tuning finished.\n")
print(end_time - start_time)

# 5. Find the best overall hyperparameters
best_params_row <- tuning_results[which.min(tuning_results$best_rmse), ]

# Print the best parameters, including the optimal nrounds
print("Best Hyperparameters Found:")
print(best_params_row)

# Now you can use these parameters to train your final model


# 1000 random of my grid, early stop 20;

# Time difference of 1.546848 mins

# iteration best_nrounds best_rmse  eta max_depth min_child_weight colsample_bytree gamma
# 25526       183          610  9.855534 0.01         8                8              0.8     1
# subsample
# 25526         1


##########################################################################################


# 1000, early stop 30

# Time difference of 1.788754 mins

# iteration best_nrounds best_rmse  eta max_depth min_child_weight colsample_bytree gamma
# 25526       183         1013  9.672725 0.01         8                8              0.8     1
# subsample
# 25526         1


##########################################################################################
##########################################################################################
##########################################################################################

# 5000 random of my grid

# Time difference of 7.529913 mins

# iteration best_nrounds best_rmse  eta max_depth min_child_weight colsample_bytree gamma
# 23592       839          110  9.505089 0.05         3                6              0.4   0.1
# subsample
# 23592         1


##########################################################################################



# ===================================================================
# SECTION 1: SETUP - Install and Load Libraries
# ===================================================================
# install.packages(c("future", "furrr", "mdatools", "tictoc", "ggplot2", "dplyr", "progressr", "purrr"))

# Load libraries
library(future)
library(furrr)
library(mdatools)
library(tictoc)
library(ggplot2)
library(dplyr)
library(progressr)
library(purrr) # For map functions

# Use progressr to show progress bars in future_map
handlers(global = TRUE)
handlers("progress")


# ===================================================================
# SECTION 2: LOAD DATA & DEFINE FUNCTIONS
# ===================================================================

# --- Helper function to generate CV splits ---
# A simple function to create multiple sets of 10-fold CV indices.
# generate_multiple_splits <- function(data, n_sets) {
#   n_rows <- nrow(data)
#   map(1:n_sets, function(i) {
#     # Create one full set of 10 folds
#     shuffled_indices <- sample(1:n_rows)
#     split(shuffled_indices, cut(seq_along(shuffled_indices), 10, labels = FALSE))
#   })
# }

# --- Your core worker function ---
find_optimal_ncomp <- function(train_idx, data) {
  calibrate <- data[train_idx, ]
  mod <- mdatools::pls(
    calibrate[, 21:ncol(calibrate)],
    calibrate$read_age,
    cv = 1, # LOOCV
    scale = FALSE,
    center = TRUE,
    ncomp = 10
  )
  return(mod$ncomp.selected)
}

# --- Create the Job Manifest for Benchmarking ---
# For the benchmark, we'll generate 10 sets of 10-fold CV for a total of 100 jobs.
# This is enough for stable timing without a long setup.
N_SPLITS_BENCHMARK <- 3
set.seed(6) # for reproducibility
all_splits <- generate_multiple_splits(df, N_SPLITS_BENCHMARK)

job_manifest <- map_dfr(1:N_SPLITS_BENCHMARK, function(split_num) {
  folds_for_split <- all_splits[[split_num]]
  map_dfr(1:10, function(fold_num) {
    tibble(
      split_id = split_num,
      fold_id = fold_num,
      train_indices = list(unlist(folds_for_split[-fold_num]))
      # We don't need test_indices for this specific benchmark
    )
  })
})

cat(sprintf("✅ Setup complete. Job manifest created with %d jobs.\n", nrow(job_manifest)))


# ===================================================================
# SECTION 3: DEFINE BENCHMARKING GRID (This section remains the same)
# ===================================================================
# Detect physical cores for setting a reasonable limit on workers
# max_workers <- parallel::detectCores(logical = FALSE)
max_workers <- 16

# Define the grid of parameters to test
benchmark_grid <- expand.grid(
  threads = c(1, 2, 4),
  workers = seq(6, max_workers, by = 1)
)

# Also test the "system default" setting by unsetting the variables
benchmark_grid <- bind_rows(
  benchmark_grid,
  tibble(threads = NA, workers = seq(6, max_workers, by = 1))
)

# Optional: Avoid extreme oversubscription
total_logical_cores <- parallel::detectCores(logical = TRUE)
benchmark_grid <- benchmark_grid %>%
  filter(is.na(threads) | (workers * threads) <= (total_logical_cores * 1.5)) %>%
  arrange(workers, threads)

print("--- Combinations to be tested ---")
print(benchmark_grid)

# List to store timing results
all_results <- list()


# ===================================================================
# SECTION 4: RUN THE BENCHMARK LOOP
# ===================================================================
cat("\n--- 🚀 Starting Benchmark ---\n")

for (i in 1:nrow(benchmark_grid)) {
  n_threads <- benchmark_grid$threads[i]
  n_workers <- benchmark_grid$workers[i]
  
  # Configure Environment
  Sys.unsetenv("OMP_NUM_THREADS"); Sys.unsetenv("OPENBLAS_NUM_THREADS")
  
  if (is.na(n_threads)) {
    cat(sprintf("\n--- Testing: %d Workers | Threads: System Default ---\n", n_workers))
  } else {
    cat(sprintf("\n--- Testing: %d Workers | Threads: %d ---\n", n_workers, n_threads))
    Sys.setenv(OMP_NUM_THREADS = n_threads, OPENBLAS_NUM_THREADS = n_threads)
  }
  
  plan(multisession, workers = n_workers)
  
  # --- Run and Time the Job ---
  tictoc::tic()
  with_progress({
    # *** KEY CHANGE IS HERE: We iterate over the manifest column ***
    results <- future_map_dbl(
      .x = job_manifest$train_indices,
      .f = ~ find_optimal_ncomp(.x, data = df),
      .options = furrr_options(seed = TRUE, packages = "mdatools")
    )
  })
  elapsed_time <- tictoc::toc(quiet = TRUE)
  elapsed_seconds <- as.numeric(elapsed_time$toc - elapsed_time$tic)
  
  # Store Results
  all_results[[i]] <- tibble(
    workers = n_workers,
    threads = ifelse(is.na(n_threads), "Default", as.character(n_threads)),
    time_sec = elapsed_seconds
  )
  
  # Clean up
  plan(sequential); gc()
}

cat("\n--- ✅ Benchmark Finished ---\n")


# ===================================================================
# SECTION 5: ANALYZE AND VISUALIZE (This section remains the same)
# ===================================================================
# Combine and arrange results
benchmark_results <- bind_rows(all_results) %>%
  mutate(threads = factor(threads, levels = c("1", "2", "4", "8", "Default"))) %>%
  arrange(time_sec)

cat("\n--- 📊 Benchmark Results (Fastest to Slowest) ---\n")
print(benchmark_results)

# Find the best result for the plot's title
best_result <- benchmark_results %>% top_n(-1, time_sec)

# Create the Plot
p <- ggplot(benchmark_results, aes(x = workers, y = time_sec, color = threads, group = threads)) +
  geom_line(linewidth = 1.2, alpha = 0.8) +
  geom_point(size = 3) +
  geom_point(data = best_result, aes(x = workers, y = time_sec), color = "red", size = 5, shape = 8, stroke = 1.5) +
  scale_x_continuous(breaks = seq(0, max(benchmark_results$workers), by = 2)) +
  labs(
    title = "PLS Model Parallel Benchmark",
    subtitle = paste0("Optimal time: ", round(best_result$time_sec, 2), "s (", best_result$workers, " workers, ", best_result$threads, " threads)"),
    x = "Number of Parallel Workers (R Sessions)",
    y = "Total Time (seconds)",
    color = "Threads per Worker"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top", plot.title = element_text(face = "bold"))

print(p)






# 13 worker, 1 thread was best but marginally
# 16 worker, 1 thread is the one I guess.  Just gonna be slow, nothing to do about it.....?

