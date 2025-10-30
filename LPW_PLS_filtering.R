# ================================================================= #
# 1. SETUP: PACKAGES, DATA, AND CONSTANTS
# ================================================================= #

# Load necessary packages
library(mdatools)
library(dplyr)
library(ggplot2)
library(foreach)
library(doParallel)

# Load your processed dataframe
df <- readRDS("RDS_dataframes/LPW_scan_avg_proc.RDS")
df <- df[complete.cases(df$read_age), ]

# Define constants
N_REPEATS <- 5
K_FOLDS   <- 10
RESPONSE_VAR <- "read_age"
PREDICTOR_VARS <- 21:ncol(df)

# ================================================================= #
# 2. PARALLEL EXECUTION
# ================================================================= #

# Set up parallel backend
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)
cat(paste("Starting parallel job on", detectCores() - 1, "cores...\n"))

# The parallel loop iterates through the 5 REPEATS
all_results_list <- foreach(
  i = 1:N_REPEATS,
  .packages = c("mdatools", "dplyr"),
  .combine = 'rbind' # Automatically combine the results from each repeat
) %dopar% {
  
  # Create a new set of 10 folds for this repeat
  set.seed(123 + i)
  folds <- caret::createFolds(df[[RESPONSE_VAR]], k = K_FOLDS, list = TRUE)
  
  # List to store results for each of the 10 folds in this repeat
  repeat_results_list <- list()
  
  # The inner loop iterates through the 10 FOLDS
  for (k in 1:K_FOLDS) {
    cal_indices <- unlist(folds[-k])
    val_indices <- folds[[k]]
    
    cal_data <- df[cal_indices, ]; val_data <- df[val_indices, ]
    cal_x <- cal_data[, PREDICTOR_VARS]; cal_y <- cal_data[[RESPONSE_VAR]]
    val_x <- val_data[, PREDICTOR_VARS]; val_y <- val_data[[RESPONSE_VAR]]
    
    # Train Baseline PLS Model (using LOOCV) to get VIP scores
    m_baseline <- pls(cal_x, cal_y, cv = 1)
    ncomp_baseline <- m_baseline$ncomp.selected
    
    # Calculate ONLY VIP scores from the baseline model
    wavenumbers <- as.numeric(colnames(cal_x))
    vips <- vipscores(m_baseline, ncomp = ncomp_baseline)[, 1]
    
    # Create lists of wavenumbers to exclude for VIP only
    excl_vip0.5   <- wavenumbers[vips < 0.5]
    excl_vip1.0   <- wavenumbers[vips < 1.0]
    
    # Define a smaller list of models to run
    models_to_run <- list(
      "Baseline"  = NULL, 
      "VIP < 0.5" = excl_vip0.5, 
      "VIP < 1.0" = excl_vip1.0
    )
    
    fold_results_list <- list()
    for (model_name in names(models_to_run)) {
      result <- tryCatch({
        excl_list <- models_to_run[[model_name]]
        
        # Train and validate in a single step
        m_current <- pls(cal_x, cal_y, cv = 1, x.test = val_x, y.test = val_y, exclcols = excl_list)
        ncomp_current <- m_current$ncomp.selected
        
        # Extract results directly from the model's testres object
        data.frame(
          Repeat = i, Fold = k, Method = model_name,
          RMSE = m_current$testres$rmse[ncomp_current],
          R2 = m_current$testres$r2[ncomp_current],
          ncomp = ncomp_current,
          nvars = ncol(cal_x) - length(excl_list)
        )
      }, error = function(e) {
        # Error handling remains as a safeguard
        data.frame(Repeat = i, Fold = k, Method = model_name, RMSE = NA, R2 = NA, ncomp = NA, nvars = NA)
      })
      fold_results_list[[model_name]] <- result
    }
    repeat_results_list[[k]] <- bind_rows(fold_results_list)
  }
  # Combine the 10 fold results for this repeat
  bind_rows(repeat_results_list)
}

# Stop the parallel cluster
stopCluster(cl)
cat("Parallel job finished.\n")

# ================================================================= #
# 3. AGGREGATE AND VISUALIZE RESULTS
# ================================================================= #

final_results <- all_results_list

# Summarize the performance of each method
summary_stats <- final_results %>%
  group_by(Method) %>%
  summarise(
    Mean_RMSE = mean(RMSE, na.rm = TRUE), SD_RMSE = sd(RMSE, na.rm = TRUE),
    Mean_R2 = mean(R2, na.rm = TRUE), Mean_ncomp = mean(ncomp, na.rm = TRUE),
    Mean_nvars = mean(nvars, na.rm = TRUE)
  ) %>%
  arrange(Mean_RMSE)

cat("\n--- Summary of Model Performance ---\n")
print(summary_stats)

# Create a boxplot to visualize the distribution of RMSE
ggplot(final_results, aes(x = reorder(Method, RMSE, median, na.rm = TRUE), y = RMSE, fill = Method)) +
  geom_boxplot(alpha = 0.8) +
  labs(
    title = "Comparison of PLS Wavenumber Filtering Methods",
    subtitle = "VIP Filtering Only",
    x = "Filtering Method", y = "RMSE (Days)"
  ) +
  theme_bw() +
  theme(legend.position = "none")