# -----------------------------------------------------------------------------
# Stand-Alone PCA Importance Calculation (V2)
# -----------------------------------------------------------------------------
# This script is adapted to work with specific list structures for model terms
# and a manifest tibble containing training indices.

# --- 1. SETUP: Load libraries and define model objects ---
library(tidyverse)
library(dplyr)
library(purrr)

# --- !! ACTION REQUIRED !! ---
# Update these file paths to match your project structure
manifest_file <- "job_manifest.RDS" # Your job manifest tibble
raw_data_file <- "raw_data.RDS"     # Your raw spectral data

# Load the required data objects
job_manifest <- readRDS(manifest_file)
raw_data <- df

# --- Recreating your exact model lists ---
# This section pastes your provided model structures directly into the script.
top_10_gam_formulas <- list(
  as.formula(read_age ~ s(PC1, k = 4) + s(PC2, k = 4) + s(PC3, k = 4) + s(PC5, k = 4) + s(PC8, k = 4) + 1),
  as.formula(read_age ~ s(PC1, k = 4) + s(PC3, k = 4) + s(PC5, k = 4) + s(PC8, k = 4) + s(PC9, k = 4) + 1),
  as.formula(read_age ~ s(PC1, k = 4) + s(PC2, k = 4) + s(PC3, k = 4) + s(PC5, k = 4) + s(PC8, k = 4) + s(PC9, k = 4) + 1),
  as.formula(read_age ~ s(PC1, k = 4) + s(PC2, k = 4) + s(PC3, k = 4) + s(PC5, k = 4) + s(PC7, k = 4) + s(PC8, k = 4) + 1),
  as.formula(read_age ~ s(PC1, k = 4) + s(PC3, k = 4) + s(PC4, k = 4) + s(PC5, k = 4) + s(PC8, k = 4) + s(PC9, k = 4) + 1),
  as.formula(read_age ~ s(PC1, k = 4) + s(PC2, k = 4) + s(PC3, k = 4) + s(PC4, k = 4) + s(PC5, k = 4) + s(PC8, k = 4) + 1),
  as.formula(read_age ~ s(PC1, k = 4) + s(PC2, k = 4) + s(PC3, k = 4) + s(PC5, k = 4) + s(PC6, k = 4) + s(PC8, k = 4) + 1),
  as.formula(read_age ~ s(PC1, k = 4) + s(PC3, k = 4) + s(PC5, k = 4) + s(PC8, k = 4) + 1),
  as.formula(read_age ~ s(PC1, k = 4) + s(PC10, k = 4) + s(PC2, k = 4) + s(PC3, k = 4) + s(PC5, k = 4) + s(PC8, k = 4) + 1),
  as.formula(read_age ~ s(PC1, k = 4) + s(PC3, k = 4) + s(PC5, k = 4) + s(PC6, k = 4) + s(PC8, k = 4) + s(PC9, k = 4) + 1)
)

# NOTE: This creates a list of 'terms' objects, just like your input
top_10_lm_terms <- list(
  terms(read_age ~ PC1 + PC2 + PC3 + PC5 + PC8 + 1),
  terms(read_age ~ PC1 + PC3 + PC5 + PC8 + 1),
  terms(read_age ~ PC1 + PC2 + PC3 + PC5 + PC8 + PC9 + 1),
  terms(read_age ~ PC1 + PC10 + PC2 + PC3 + PC5 + PC8 + 1),
  terms(read_age ~ PC1 + PC2 + PC3 + PC5 + PC7 + PC8 + 1),
  terms(read_age ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC8 + 1),
  terms(read_age ~ PC1 + PC2 + PC3 + PC5 + PC6 + PC8 + 1),
  terms(read_age ~ PC1 + PC3 + PC5 + PC8 + PC9 + 1),
  terms(read_age ~ PC1 + PC10 + PC3 + PC5 + PC8 + 1),
  terms(read_age ~ PC1 + PC3 + PC5 + PC7 + PC8 + 1)
)


# --- 2. HELPER FUNCTIONS (ADAPTED FOR YOUR DATA) ---

# **NEW version for 'terms' objects**
# Extracts PC numbers by accessing the 'term.labels' attribute directly
get_pc_indices_lm <- function(term_obj) {
  labels <- attr(term_obj, "term.labels")
  as.numeric(stringr::str_extract(labels, "\\d+"))
}

# This function works as-is for the 'formula' objects in the GAM list
get_pc_indices_gam <- function(form) {
  formula_str <- as.character(form)[3]
  pc_matches <- stringr::str_extract_all(formula_str, "PC\\d+")[[1]]
  as.numeric(stringr::str_extract(pc_matches, "\\d+"))
}

# This function is unchanged. It calculates importance using the vector length.
calculate_pca_vector_length <- function(pca_rotation, pc_indices) {
  # Handle cases where a PC might not exist (e.g., if max PCs < 10)
  valid_indices <- pc_indices[pc_indices <= ncol(pca_rotation)]
  if(length(valid_indices) == 0) return(rep(0, nrow(pca_rotation)))
  
  loadings_subset <- pca_rotation[, valid_indices, drop = FALSE]
  importance <- sqrt(rowSums(loadings_subset^2))
  return(importance)
}


# --- 3. MAIN PROCESSING: Iterate through the manifest ---
message("Starting PCA importance calculation for ", nrow(job_manifest), " folds...")


library(dplyr)
library(purrr)


# 1. Get unique SplitSets to iterate over
unique_split_ids <- unique(job_manifest$split_id)

# -----------------------------------------------------------
# 1. SETUP FOR PARALLEL PROCESSING
# -----------------------------------------------------------
library(doParallel)
library(foreach)
library(dplyr)
library(purrr) # Already used, needed for map_dfr inside the loop

# Determine the number of cores to use (e.g., all but one)
n_cores <- 8

# Register the parallel backend
cl <- makeCluster(n_cores)
registerDoParallel(cl)

message(paste("Running PCA importance calculation on", n_cores, "cores..."))


all_pca_importance <- foreach(
  current_split_id = unique_split_ids,
  .combine = 'rbind',
  .packages = c("dplyr", "purrr", "stringr", "tibble") # Packages needed inside the loop
) %dopar% {
  
  # Filter job_manifest to only include folds for the current SplitSet
  split_manifest <- job_manifest %>% filter(split_id == current_split_id)
  
  # Inner loop iterates over FOLDS within the current SplitSet
  # Use 'map_dfr' here as the inner loop is typically fast enough not to require further parallelization
  all_fold_results <- purrr::map_dfr(1:nrow(split_manifest), function(i) {
    
    job_row <- split_manifest[i, ]
    
    train_indices <- job_row$train_indices[[1]]
    train_data <- raw_data[train_indices, ]
    
    # [... Standard Feature/PCA Calculation Block ...]
    metadata_cols <- c("specimen", "length", "weight", "structure_weight", 
                       "read_age", "expected_age", "average_grade", "sample_date",
                       "hatch_date", "percent_affected", "other_problem", "broken",
                       "side", "common_name", "run_number", "scan_name", 
                       "file_name", "session_title", "comments", "file_path")
    feature_cols <- names(train_data)[sapply(train_data, is.numeric)]
    feature_cols <- setdiff(feature_cols, metadata_cols)
    train_spectra <- train_data[, feature_cols]
    
    pca_model <- prcomp(train_spectra, center = TRUE, scale. = TRUE)
    pca_rotation <- pca_model$rotation
    wavenumbers <- as.numeric(rownames(pca_rotation))
    
    # Calculate importance for all LM and GAM models (same as before)
    pca_importance_lm <- purrr::map_dfr(1:length(top_10_lm_terms), ~{
      term_obj <- top_10_lm_terms[[.x]]
      pc_indices <- get_pc_indices_lm(term_obj)
      tibble(
        method = paste0("PCA-LM", .x),
        wavenumber = wavenumbers,
        importance = calculate_pca_vector_length(pca_rotation, pc_indices)
      )
    })
    
    pca_importance_gam <- purrr::map_dfr(1:length(top_10_gam_formulas), ~{
      form <- top_10_gam_formulas[[.x]]
      pc_indices <- get_pc_indices_gam(form)
      tibble(
        method = paste0("PCA-GAM", .x),
        wavenumber = wavenumbers,
        importance = calculate_pca_vector_length(pca_rotation, pc_indices)
      )
    })
    
    # Return results for this single fold
    bind_rows(pca_importance_lm, pca_importance_gam)
    
  }, .progress = FALSE) # Disable progress bar inside map_dfr for cleaner parallel output
  
  # 3. SUMMARISE results ACROSS ALL FOLDS for the current SplitSet
  averaged_split_result <- all_fold_results %>%
    group_by(method, wavenumber) %>%
    summarise(
      mean_importance = mean(importance, na.rm = TRUE),
      sd_importance = sd(importance, na.rm = TRUE),
      min_importance = min(importance, na.rm = TRUE),
      max_importance = max(importance, na.rm = TRUE),
      q025 = quantile(importance, probs = 0.025, na.rm = TRUE),
      q975 = quantile(importance, probs = 0.975, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      SplitSet = current_split_id
    )
  
  # Return the single, summarised result for this SplitSet
  return(averaged_split_result)
  
} # End of foreach loop

# -----------------------------------------------------------
# 3. CLEANUP
# -----------------------------------------------------------
# STOP the cluster when done
stopCluster(cl)

old_importance <- readRDS("RDS_dataframes/final_importance_data_USEME_NEW_2025-10-20.RDS")

message("PCA importance calculation complete.")

# The final 'all_pca_importance' is now much smaller, containing only the
# averaged importance for each SplitSet.


filtered_importance_data <- old_importance %>%
  filter(!str_detect(method, "^PCA-(GAM|LM)"))


# This creates your final, updated importance data frame.
final_importance_data_updated <- bind_rows(
  filtered_importance_data,
  all_pca_importance
)

saveRDS(final_importance_data_updated, "IMPORTANCE_UPDATED_USEME_10212025.RDS")

# Optional: Reassign the updated data back to the original variable name
final_importance_data <- final_importance_data_updated
