# ====================================================================================
# SCRIPT TO AVERAGE PLS RESULTS AND TEST MULTIPLE VIP THRESHOLDS
# ====================================================================================

# -- Step 1: Load packages and define parameters ----
library(mdatools)
library(dplyr)
library(caret)

# !!! EDIT THIS VECTOR to test different VIP score cutoffs !!!
vip_thresholds_to_test <- c(0.5, 1.0)

# Load your preprocessed dataframe
tryCatch({
  df <- readRDS("RDS_dataframes/LPW_scan_avg_proc.RDS")
}, error = function(e) {
  stop("Error: Data file not found. Please check the path to 'LPW_scan_avg_proc.RDS'")
})

df <- df[complete.cases(df$read_age), ]
SPECTRA_START_COL <- 21

# ====================================================================================
# -- Step 2: Set up a single 10-fold cross-validation ----
# ====================================================================================

set.seed(6) # for reproducibility
folds <- createFolds(df$read_age, k = 10, list = TRUE, returnTrain = FALSE)

# ====================================================================================
# -- Step 3: Loop through folds and VIP thresholds to get results ----
# ====================================================================================

all_fold_results <- list()

cat("Starting 10-fold cross-validation for PLS models...\n")

for (i in 1:10) {
  cat("  Processing Fold", i, "...\n")
  
  # Define training and testing sets for this fold
  test_indices <- folds[[i]]
  train_df <- df[-test_indices, ]
  test_df  <- df[test_indices, ]
  
  # Extract predictor (X) and response (y) variables
  x_train <- train_df[, SPECTRA_START_COL:ncol(train_df)]
  y_train <- train_df$read_age
  x_test <- test_df[, SPECTRA_START_COL:ncol(test_df)]
  y_test <- test_df$read_age
  
  # A. Run the standard PLS model (this is our baseline for the fold)
  pls_standard_model <- mdatools::pls(x_train, y_train, scale = FALSE, center = TRUE,
                                      x.test = x_test, y.test = y_test)
  
  ncomp_std <- pls_standard_model$ncomp.selected
  plot(pls_standard_model)
}
