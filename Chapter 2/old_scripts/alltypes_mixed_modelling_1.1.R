# 1. --- SETUP ---
# Load all necessary libraries
library(mdatools)
library(dplyr)
library(prospectr)
library(caret)
library(mgcv)      # For GAM models
library(MuMIn)     # For dredge()
library(tidyr)
library(ggplot2)
library(patchwork) # For combining plots
library(ggridges)
library(purrr)

# Helper function for consistent metrics
calculate_metrics <- function(actual, predicted, model_name) {
  # Return NA if predictions are bad (e.g., all NA or constant)
  if(all(is.na(predicted)) || sd(predicted, na.rm = TRUE) == 0) {
    return(tibble(ModelType = model_name, R2 = NA, RMSE = NA, RPD = NA, Bias = NA))
  }
  
  R2 <- cor(actual, predicted, use = "pairwise.complete.obs")^2
  RMSE <- caret::RMSE(pred = predicted, obs = actual, na.rm = TRUE)
  RPD <- sd(actual, na.rm = TRUE) / RMSE
  Bias <- mean(predicted - actual, na.rm = TRUE)
  
  tibble(ModelType = model_name, R2, RMSE, RPD, Bias)
}


################################################################################
# 2. --- LOAD DATA ---
################################################################################

# Load BOTH dataframes
df_raw <- readRDS("RDS_dataframes/combined_IBM_LPW_raw.RDS")
df_snvsg <- readRDS("RDS_dataframes/combined_IBM_LPW_experimental_SNVandSG.RDS")

df_raw <- df_raw %>% filter(!is.na(read_age))
df_snvsg <- df_snvsg %>% filter(!is.na(read_age))

# Define spectral columns once
speccols_raw <- names(df_raw)[grepl("^\\d", names(df_raw))]
speccols_snvsg <- names(df_snvsg)[grepl("^\\d", names(df_snvsg))]


################################################################################
# 3. --- GLOBAL MODEL SELECTION (DREDGE) ---
################################################################################
# This is run once on the full dataset to determine the *best formula*
# for PCA-based models, which will then be used in CV.

message("--- Finding Best PCA-based Model Formulas using dredge() ---")

sg_matrix_for_dredge <- savitzkyGolay(as.matrix(df_raw[, speccols_raw]), m = 1, p = 3, w = 17)
pca_for_dredge <- mdatools::pca(sg_matrix_for_dredge, ncomp = 10, center = TRUE, scale = FALSE)
pc_scores_for_dredge <- as.data.frame(pca_for_dredge$res$cal$scores)
colnames(pc_scores_for_dredge) <- paste0("PC", 1:10)

dredge_data <- cbind(read_age = df_raw$read_age, pc_scores_for_dredge)

global_lm <- lm(read_age ~ ., data = dredge_data, na.action = "na.fail")
global_gam <- gam(read_age ~ s(PC1, k=4) + s(PC2, k=4) + s(PC3, k=4) + s(PC4, k=4) + s(PC5, k=4) + 
                    s(PC6, k=4) + s(PC7, k=4) + s(PC8, k=4) + s(PC9, k=4) + s(PC10, k=4), 
                  data = dredge_data, na.action = "na.fail", method = "REML")

best_lm_model <- get.models(dredge(global_lm), subset = 1)[[1]]
best_gam_model <- get.models(dredge(global_gam), subset = 1)[[1]]
best_lm_formula <- formula(best_lm_model)
best_gam_formula <- formula(best_gam_model)

message("✅ Best LM formula found: ", deparse(best_lm_formula))
message("✅ Best GAM formula found: ", deparse(best_gam_formula))


################################################################################
# 4. --- REUSABLE MODELING FUNCTIONS ---
################################################################################

#' Helper function to create title pages in the PDF
#' @param title_text The text to display.
make_title_page <- function(title_text) {
  plot.new()
  text(x = 0.5, y = 0.5, labels = title_text, cex = 1.5, font = 2)
}

#' Runs the full suite of PLS models.
#'
#' @param cal_raw Raw calibration data (for SG models).
#' @param test_raw Raw test data.
#' @param cal_snvsg SNV+SG processed calibration data.
#' @param test_snvsg SNV+SG processed test data.
#' @param fold_label A label (like Fold number) for plot titles.
#' @param plot_diagnostics Boolean. If TRUE, generates `plot(m)` diagnostics.
#' @return A tibble with results for all 5 PLS model types.
run_all_pls_models <- function(cal_raw, test_raw, cal_snvsg, test_snvsg, fold_label = "", plot_diagnostics = FALSE) {
  
  results_list <- list()
  
  # MODEL 1: SNV + SG
  tryCatch({
    message("  Building SNV + SG model...")
    m <- pls(x = as.matrix(cal_snvsg[, speccols_snvsg]), y = as.matrix(cal_snvsg[, "read_age"]),
             x.test = as.matrix(test_snvsg[, speccols_snvsg]), y.test = as.matrix(test_snvsg[, "read_age"]),
             cv = 1, scale = F, center = T)
    if(plot_diagnostics) { make_title_page(paste(fold_label, "\n\nModel: SNV + SG")); plot(m) }
    ncomp <- m$ncomp.selected
    results_list[["SNV+SG"]] <- tibble(ModelType = "SNV+SG", nComp = ncomp, R2 = m$res$test$r2[, ncomp], RMSE = m$res$test$rmse[, ncomp], Bias = m$res$test$bias[, ncomp], RPD = m$res$test$rpd[, ncomp])
  }, error = function(e) { message(paste("    ERROR in SNV+SG model:", e$message)) })
  
  # Data Prep for subsequent PLS models
  cal_sg_matrix <- savitzkyGolay(as.matrix(cal_raw[, speccols_raw]), m = 1, p = 3, w = 17)
  test_sg_matrix <- savitzkyGolay(as.matrix(test_raw[, speccols_raw]), m = 1, p = 3, w = 17)
  speccols_sg <- colnames(cal_sg_matrix)
  
  # MODEL 2: SG Only
  tryCatch({
    message("  Building SG Only model...")
    m <- pls(x = cal_sg_matrix, y = as.matrix(cal_raw[, "read_age"]),
             x.test = test_sg_matrix, y.test = as.matrix(test_raw[, "read_age"]),
             cv = 1, scale = F, center = T)
    if(plot_diagnostics) { make_title_page(paste(fold_label, "\n\nModel: SG Only")); plot(m) }
    ncomp <- m$ncomp.selected
    results_list[["SG Only"]] <- tibble(ModelType = "SG Only", nComp = ncomp, R2 = m$res$test$r2[, ncomp], RMSE = m$res$test$rmse[, ncomp], Bias = m$res$test$bias[, ncomp], RPD = m$res$test$rpd[, ncomp])
  }, error = function(e) { message(paste("    ERROR in SG Only model:", e$message)) })
  
  # MODEL 3: Filtered SG
  m_filtsg <- NULL
  tryCatch({
    message("  Building Filtered SG model...")
    cols_to_remove <- speccols_sg[as.numeric(speccols_sg) > 7500]
    speccols_filtsg <- setdiff(speccols_sg, cols_to_remove)
    m_filtsg <- pls(x = cal_sg_matrix[, speccols_filtsg], y = as.matrix(cal_raw[, "read_age"]),
                    x.test = test_sg_matrix[, speccols_filtsg], y.test = as.matrix(test_raw[, "read_age"]),
                    cv = 1, scale = F, center = T)
    if(plot_diagnostics) { make_title_page(paste(fold_label, "\n\nModel: Filtered SG")); plot(m_filtsg) }
    ncomp <- m_filtsg$ncomp.selected
    results_list[["Filtered SG"]] <- tibble(ModelType = "Filtered SG", nComp = ncomp, R2 = m_filtsg$res$test$r2[, ncomp], RMSE = m_filtsg$res$test$rmse[, ncomp], Bias = m_filtsg$res$test$bias[, ncomp], RPD = m_filtsg$res$test$rpd[, ncomp])
  }, error = function(e) { message(paste("    ERROR in Filtered SG model:", e$message)) })
  
  # MODELS 4 & 5: VIP Selection
  if (!is.null(m_filtsg)) {
    vip <- as.data.frame(vipscores(m_filtsg))
    tryCatch({
      message("  Building VIP > 0.5 model...")
      m <- pls(x = cal_sg_matrix[, speccols_filtsg], y = as.matrix(cal_raw[, "read_age"]),
               x.test = test_sg_matrix[, speccols_filtsg], y.test = as.matrix(test_raw[, "read_age"]),
               cv = 1, scale = F, center = T, exclcols = vip$V1 < 0.5)
      if(plot_diagnostics) { make_title_page(paste(fold_label, "\n\nModel: VIP > 0.5")); plot(m) }
      ncomp <- m$ncomp.selected
      results_list[["VIP > 0.5"]] <- tibble(ModelType = "VIP > 0.5", nComp = ncomp, R2 = m$res$test$r2[, ncomp], RMSE = m$res$test$rmse[, ncomp], Bias = m$res$test$bias[, ncomp], RPD = m$res$test$rpd[, ncomp])
    }, error = function(e) { message(paste("    ERROR in VIP > 0.5 model:", e$message)) })
    tryCatch({
      message("  Building VIP > 1.0 model...")
      m <- pls(x = cal_sg_matrix[, speccols_filtsg], y = as.matrix(cal_raw[, "read_age"]),
               x.test = test_sg_matrix[, speccols_filtsg], y.test = as.matrix(test_raw[, "read_age"]),
               cv = 1, scale = F, center = T, exclcols = vip$V1 < 1.0)
      if(plot_diagnostics) { make_title_page(paste(fold_label, "\n\nModel: VIP > 1.0")); plot(m) }
      ncomp <- m$ncomp.selected
      results_list[["VIP > 1.0"]] <- tibble(ModelType = "VIP > 1.0", nComp = ncomp, R2 = m$res$test$r2[, ncomp], RMSE = m$res$test$rmse[, ncomp], Bias = m$res$test$bias[, ncomp], RPD = m$res$test$rpd[, ncomp])
    }, error = function(e) { message(paste("    ERROR in VIP > 1.0 model:", e$message)) })
  }
  
  return(bind_rows(results_list))
}


#' Runs the PCA-based LM and GAM models.
#'
#' @param cal_raw Raw calibration data.
#' @param test_raw Raw test data.
#' @param lm_formula The formula for the PCA-LM model (from dredge).
#' @param gam_formula The formula for the PCA-GAM model (from dredge).
#' @return A tibble with results for PCA-LM and PCA-GAM.
run_pca_models <- function(cal_raw, test_raw, lm_formula, gam_formula) {
  message("  Building PCA-based models...")
  results_list <- list()
  
  tryCatch({
    # 1. Apply SG filter
    cal_sg_fold <- savitzkyGolay(as.matrix(cal_raw[, speccols_raw]), m = 1, p = 3, w = 17)
    test_sg_fold <- savitzkyGolay(as.matrix(test_raw[, speccols_raw]), m = 1, p = 3, w = 17)
    
    # 2. Build PCA on CALIBRATION data ONLY
    pca_model_fold <- mdatools::pca(cal_sg_fold, ncomp = 10, center = TRUE, scale = FALSE)
    
    # 3. Get scores for cal set and PREDICT scores for test set
    cal_pcs <- as.data.frame(pca_model_fold$res$cal$scores); colnames(cal_pcs) <- paste0("PC", 1:10)
    test_pcs <- as.data.frame(predict(pca_model_fold, test_sg_fold)$scores); colnames(test_pcs) <- paste0("PC", 1:10)
    
    # 4. Combine with response variable
    cal_pca_df <- cbind(read_age = cal_raw$read_age, cal_pcs)
    test_pca_df <- cbind(read_age = test_raw$read_age, test_pcs)
    
    # 5. Train and evaluate PCA-LM
    mod_pca_lm <- lm(lm_formula, data = cal_pca_df)
    preds_pca_lm <- predict(mod_pca_lm, newdata = test_pca_df)
    results_list[["PCA-LM"]] <- calculate_metrics(test_pca_df$read_age, preds_pca_lm, "PCA-LM")
    
    # 6. Train and evaluate PCA-GAM
    mod_pca_gam <- gam(gam_formula, data = cal_pca_df, method = "REML")
    preds_pca_gam <- predict(mod_pca_gam, newdata = test_pca_df)
    results_list[["PCA-GAM"]] <- calculate_metrics(test_pca_df$read_age, preds_pca_gam, "PCA-GAM")
    
  }, error = function(e) { message(paste("    ERROR in PCA models:", e$message)) })
  
  return(bind_rows(results_list))
}


#' Runs the simple LM and GAM models.
#'
#' @param cal_simple Calibration data (with length, structure_weight).
#' @param test_simple Test data.
#' @return A tibble with results for Simple-LM and Simple-GAM.
run_simple_models <- function(cal_simple, test_simple) {
  message("  Building simple models...")
  results_list <- list()
  
  tryCatch({
    if(nrow(cal_simple) < 10 || nrow(test_simple) < 1) {
      message("    Skipping Simple models (insufficient data after NA removal)...")
      return(tibble()) # Return empty tibble
    }
    
    # 1. Train and evaluate Simple-LM
    mod_simple_lm <- lm(read_age ~ length + structure_weight, data = cal_simple)
    preds_simple_lm <- predict(mod_simple_lm, newdata = test_simple)
    results_list[["Simple-LM"]] <- calculate_metrics(test_simple$read_age, preds_simple_lm, "Simple-LM")
    
    # 2. Train and evaluate Simple-GAM
    mod_simple_gam <- gam(read_age ~ s(length, k=4) + s(structure_weight, k=4), data = cal_simple, method = "REML")
    preds_simple_gam <- predict(mod_simple_gam, newdata = test_simple)
    results_list[["Simple-GAM"]] <- calculate_metrics(test_simple$read_age, preds_simple_gam, "Simple-GAM")
    
  }, error = function(e) { message(paste("    ERROR in Simple models:", e$message)) })
  
  return(bind_rows(results_list))
}


################################################################################
#
# ANALYSIS 1: 10-FOLD CROSS-VALIDATION (COMBINED REGIONS)
#
################################################################################

################################################################################
# 5. --- PREPARE 10-FOLD STRATIFIED DATA SPLITS (CORRECTED) ---
################################################################################

k <- 10 # Number of folds
df_raw$unique_row_id <- 1:nrow(df_raw); df_snvsg$unique_row_id <- 1:nrow(df_snvsg)
message("\n--- Generating stratified folds for SPECTRAL models ---")

raw_cal_list <- vector("list", k); raw_test_list <- vector("list", k)
list_of_regional_dfs <- split(df_raw, df_raw$region)

for (region_name in names(list_of_regional_dfs)) {
  current_region_df <- list_of_regional_dfs[[region_name]]
  
  # --- FIX 1 ---
  # Create folds on the ROW INDICES (1:n) stratified by read_age
  test_indices_for_region <- createFolds(
    y = current_region_df$read_age, # Stratify by this
    k = k, 
    list = TRUE, 
    returnTrain = FALSE
  )
  
  # Now, use these indices to subset the dataframe
  for (i in 1:k) {
    fold_indices <- test_indices_for_region[[i]]
    
    # Check if the fold is empty (which can happen if n < k)
    if (length(fold_indices) > 0) {
      regional_test_df <- current_region_df[fold_indices, ]
      regional_cal_df <- current_region_df[-fold_indices, ]
    } else {
      # If fold is empty, create empty DFs to avoid errors
      regional_test_df <- current_region_df[0, ] 
      regional_cal_df <- current_region_df[0, ]
    }
    
    raw_test_list[[i]] <- bind_rows(raw_test_list[[i]], regional_test_df)
    raw_cal_list[[i]] <- bind_rows(raw_cal_list[[i]], regional_cal_df)
  }
}
snvsg_cal_list <- lapply(raw_cal_list, function(df) df_snvsg %>% filter(unique_row_id %in% df$unique_row_id))
snvsg_test_list <- lapply(raw_test_list, function(df) df_snvsg %>% filter(unique_row_id %in% df$unique_row_id))
message("✅ Spectral model data lists created successfully.")


# ---
message("\n--- Generating stratified folds for SIMPLE models ---")
df_simple <- df_raw %>% 
  select(region, read_age, length, structure_weight, unique_row_id) %>% 
  filter(!is.na(length) & !is.na(structure_weight))

simple_cal_list <- vector("list", k); simple_test_list <- vector("list", k)
list_of_regional_dfs_simple <- split(df_simple, df_simple$region)

for (region_name in names(list_of_regional_dfs_simple)) {
  current_region_df <- list_of_regional_dfs_simple[[region_name]]
  
  # --- FIX 2 (Same logic as above) ---
  test_indices_for_region <- createFolds(
    y = current_region_df$read_age, # Stratify by this
    k = k, 
    list = TRUE, 
    returnTrain = FALSE
  )
  
  for (i in 1:k) {
    fold_indices <- test_indices_for_region[[i]]
    
    if (length(fold_indices) > 0) {
      regional_test_df <- current_region_df[fold_indices, ]
      regional_cal_df <- current_region_df[-fold_indices, ]
    } else {
      regional_test_df <- current_region_df[0, ]
      regional_cal_df <- current_region_df[0, ]
    }
    
    simple_test_list[[i]] <- bind_rows(simple_test_list[[i]], regional_test_df)
    simple_cal_list[[i]] <- bind_rows(simple_cal_list[[i]], regional_cal_df)
  }
}
message("✅ Simple model data lists created successfully.")

################################################################################
# 6. --- MAIN 10-FOLD CV MODELING LOOP (REFACTORED) ---
################################################################################

if (!dir.exists("pls_plots_final_10fold")) {
  dir.create("pls_plots_final_10fold")
}
pdf("pls_plots_final_10fold/allregions_10foldcv_plsplots.pdf", width = 8.5, height = 11)

# List to hold all results tibbles
all_fold_results_list <- list()

# The main outer loop
for (i in 1:k) {
  message(paste0("\n--- Processing Fold ", i, "/", k, " ---"))
  
  # 1. Get data for the current fold
  cal_raw <- raw_cal_list[[i]]; test_raw <- raw_test_list[[i]]
  cal_snvsg <- snvsg_cal_list[[i]]; test_snvsg <- snvsg_test_list[[i]]
  cal_simple <- simple_cal_list[[i]]; test_simple <- simple_test_list[[i]]
  
  fold_label_text <- paste("Fold:", i)
  
  # 2. Run all model blocks using the functions
  pls_results <- run_all_pls_models(cal_raw, test_raw, cal_snvsg, test_snvsg, 
                                    fold_label = fold_label_text, plot_diagnostics = TRUE)
  
  pca_results <- run_pca_models(cal_raw, test_raw, 
                                best_lm_formula, best_gam_formula)
  
  simple_results <- run_simple_models(cal_simple, test_simple)
  
  # 3. Combine results for this fold and add Fold ID
  all_fold_results_list[[i]] <- bind_rows(pls_results, pca_results, simple_results) %>%
    mutate(Fold = i)
  
} # End of the main for loop

dev.off()

# Combine all results from the list into one big dataframe
all_fold_results <- bind_rows(all_fold_results_list)


################################################################################
# 7. --- AGGREGATE AND VISUALIZE 10-FOLD CV RESULTS ---
################################################################################

# --- A. Summary Statistics Table ---
summary_stats <- all_fold_results %>%
  group_by(ModelType) %>%
  summarise(
    Avg_R2 = mean(R2, na.rm = TRUE), SD_R2 = sd(R2, na.rm = TRUE),
    Avg_RMSE = mean(RMSE, na.rm = TRUE), SD_RMSE = sd(RMSE, na.rm = TRUE),
    Avg_RPD = mean(RPD, na.rm = TRUE), SD_RPD = sd(RPD, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(desc(Avg_R2))

message("\n--- Average Performance Across 10 Folds ---")
print(as.data.frame(summary_stats))

write.csv(all_fold_results, "allregions_10foldcv_raw.csv", row.names = FALSE)
write.csv(summary_stats, "allregions_10foldcv_summary.csv", row.names = FALSE)


# --- B. Performance Visualization Plot ---
# This addresses your request to "combine performance plots"
model_order <- summary_stats$ModelType

plot_10fold_performance <- all_fold_results %>%
  filter(!is.na(RMSE)) %>%
  mutate(ModelType = factor(ModelType, levels = rev(model_order))) %>%
  ggplot(aes(x = ModelType, y = RMSE, fill = ModelType)) +
  geom_boxplot(show.legend = FALSE) +
  geom_point(position = position_jitter(width = 0.1), alpha = 0.5, show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Model Performance Across 10-Fold Cross-Validation",
    subtitle = "Models trained and tested on combined regional data",
    y = expression("Test Set R"^2),
    x = "Model Type"
  ) +
  theme_minimal(base_size = 14) +
  theme(panel.grid.major.y = element_blank())

ggsave("allregions_10fold_boxplot.pdf", plot = plot_10fold_performance, width = 10, height = 8)
print(plot_10fold_performance)


################################################################################
# 8. --- DATA SPLIT DISTRIBUTION PLOTS ---
################################################################################

################################################################################
################################################################################
# Histogram for splits: length/read_age by region:
################################################################################
###0#############################################################################
# 3. --- CREATE THE HISTOGRAM PLOTS (SPLIT BY VARIABLE) ---
message("Generating split histogram plots...")

# --- SPECTRAL MODELS ---

# PLOT 1A: Spectral Models (AGE)
plot_spectral_age <- combined_splits_spectral %>%
  filter(Variable == "Fish Age (days)") %>%
  ggplot(aes(x = Value, fill = SplitType)) +
  geom_histogram(bins = 30, alpha = 0.7, position = "identity", aes(y = ..density..)) +
  
  # Facet by region (rows) and Fold (cols)
  facet_grid(region ~ Fold, scales = "free") +
  
  scale_fill_manual(values = c("Calibration" = "#D55E00", "Test" = "#0072B2")) +
  labs(
    title = "Data Distributions for SPECTRAL Models (Fish Age)",
    subtitle = "Comparing Test vs. Calibration Splits for Each Region Across 10 CV Folds",
    x = "Fish Age (days)", y = "Density", fill = "Split Type"
  ) +
  theme_bw(base_size = 12) +
  theme(strip.text = element_text(face = "bold"), 
        legend.position = "top")

# PLOT 1B: Spectral Models (LENGTH)
plot_spectral_length <- combined_splits_spectral %>%
  filter(Variable == "Fish Length (mm)") %>%
  ggplot(aes(x = Value, fill = SplitType)) +
  geom_histogram(bins = 30, alpha = 0.7, position = "identity", aes(y = ..density..)) +
  
  # Facet by region (rows) and Fold (cols)
  facet_grid(region ~ Fold, scales = "free") +
  
  scale_fill_manual(values = c("Calibration" = "#D55E00", "Test" = "#0072B2")) +
  labs(
    title = "Data Distributions for SPECTRAL Models (Fish Length)",
    subtitle = "Comparing Test vs. Calibration Splits for Each Region Across 10 CV Folds",
    x = "Fish Length (mm)", y = "Density", fill = "Split Type"
  ) +
  theme_bw(base_size = 12) +
  theme(strip.text = element_text(face = "bold"), 
        legend.position = "top")

# --- SIMPLE MODELS ---

# PLOT 2A: Simple Models (AGE)
plot_simple_age <- combined_splits_simple %>%
  filter(Variable == "Fish Age (days)") %>%
  ggplot(aes(x = Value, fill = SplitType)) +
  geom_histogram(bins = 30, alpha = 0.7, position = "identity", aes(y = ..density..)) +
  
  # Facet by region (rows) and Fold (cols)
  facet_grid(region ~ Fold, scales = "free") +
  
  scale_fill_manual(values = c("Calibration" = "#D55E00", "Test" = "#0072B2")) +
  labs(
    title = "Data Distributions for SIMPLE Models (Fish Age)",
    subtitle = "Comparing Test vs. Calibration Splits for Each Region Across 10 CV Folds",
    x = "Fish Age (days)", y = "Density", fill = "Split Type"
  ) +
  theme_bw(base_size = 12) +
  theme(strip.text = element_text(face = "bold"), 
        legend.position = "top")

# PLOT 2B: Simple Models (LENGTH)
plot_simple_length <- combined_splits_simple %>%
  filter(Variable == "Fish Length (mm)") %>%
  ggplot(aes(x = Value, fill = SplitType)) +
  geom_histogram(bins = 30, alpha = 0.7, position = "identity", aes(y = ..density..)) +
  
  # Facet by region (rows) and Fold (cols)
  facet_grid(region ~ Fold, scales = "free") +
  
  scale_fill_manual(values = c("Calibration" = "#D55E00", "Test" = "#0072B2")) +
  labs(
    title = "Data Distributions for SIMPLE Models (Fish Length)",
    subtitle = "Comparing Test vs. Calibration Splits for Each Region Across 10 CV Folds",
    x = "Fish Length (mm)", y = "Density", fill = "Split Type"
  ) +
  theme_bw(base_size = 12) +
  theme(strip.text = element_text(face = "bold"), 
        legend.position = "top")


# 4. --- SAVE ALL 4 PLOTS TO A SINGLE 4-PAGE PDF ---
message("Saving all 4 plots to a single PDF...")

# I recommend saving to a NEW file name to avoid PDF caching issues
pdf("allregions_10foldcv_HISTOGRAM_4page.pdf", width = 18, height = 10)
print(plot_spectral_age)    # Page 1
print(plot_spectral_length) # Page 2
print(plot_simple_age)      # Page 3
print(plot_simple_length)   # Page 4
dev.off() # This closes the PDF device and saves the file

message("✅ Four-page histogram plot saved to allregions_10foldcv_HISTOGRAM_4page.pdf")


################################################################################
################################################################################
# investigating splits of length/read_age
################################################################################
################################################################################
# # useful for seeing if particular splits got a bad mix of specimens
# 
# # 1. --- SETUP ---
# # Ensure the combined_splits_df from the ridgeline plot code is in your environment.
# # If not, you can recreate it with this code:
# message("Preparing data for summary tables...")
# cal_df_long <- map_dfr(raw_cal_list, ~ as.data.frame(.x), .id = "Fold") %>%
#   mutate(SplitType = "Calibration", Fold = as.integer(Fold))
# test_df_long <- map_dfr(raw_test_list, ~ as.data.frame(.x), .id = "Fold") %>%
#   mutate(SplitType = "Test", Fold = as.integer(Fold))
# combined_splits_df <- bind_rows(cal_df_long, test_df_long) %>%
#   select(Fold, SplitType, region, read_age, length) %>%
#   pivot_longer(cols = c(read_age, length), names_to = "Variable", values_to = "Value")
# 
# # 2. --- CALCULATE AND FORMAT SUMMARY STATISTICS ---
# # This uses dplyr to group the data and calculate stats for each group
# summary_table <- combined_splits_df %>%
#   group_by(Variable, Fold, region, SplitType) %>%
#   summarise(
#     N = n(), # Count of samples
#     Mean = mean(Value, na.rm = TRUE),
#     Median = median(Value, na.rm = TRUE),
#     StdDev = sd(Value, na.rm = TRUE),
#     Min = min(Value, na.rm = TRUE),
#     Max = max(Value, na.rm = TRUE),
#     .groups = "drop" # Ungroup after summarising
#   ) %>%
#   # Pivot the table to make comparison easier
#   pivot_wider(
#     names_from = SplitType,
#     values_from = c(N, Mean, Median, StdDev, Min, Max)
#   ) %>%
#   # Reorder columns for clarity
#   select(
#     Variable, Fold, region, N_Calibration, N_Test, 
#     Mean_Calibration, Mean_Test, Median_Calibration, Median_Test,
#     StdDev_Calibration, StdDev_Test, Min_Calibration, Min_Test,
#     Max_Calibration, Max_Test
#   ) %>%
#   # Arrange the data for easy viewing
#   arrange(Variable, Fold, region)
# 
# message("✅ Summary table created successfully.")
# 
# # 3. --- VIEW RESULTS AND SAVE ---
# 
# # View a specific, problematic example to see the numerical difference
# # Let's look at the 'read_age' data for LPW in Fold 2, which looked uneven in the plot
# message("\n--- Example: Summary for read_age in Fold 2 (LPW) ---")
# print(
#   summary_table %>% 
#     filter(Variable == "read_age", Fold == 2, region == "LPW")
# )
# 
# # You can view the entire table in RStudio
# # View(summary_table)
# summary_table$maxdiff <- abs(summary_table$Max_Calibration - summary_table$Max_Test)
# summary_table$mindiff <- abs(summary_table$Min_Calibration - summary_table$Min_Test)
# summary_table$meandiff <- abs(summary_table$Mean_Calibration - summary_table$Mean_Test)
# # Save the full table to a CSV file for further inspection in Excel or another program
# write.csv(summary_table, "allregions_10foldcv_split_summary_statistics.csv", row.names = FALSE)
# message("\nFull summary table saved to allregions_10foldcv_split_summary_statistics.csv")
# 
# 
# # Ensure the summary_table is in your environment from the previous step.
# # If not, you will need to re-run the code that generates it.
# 
# # Now, we'll reshape the data into a "long" format for ggplot
# diff_long <- summary_table %>%
#   # Select only the columns we need
#   select(Variable, Fold, region, maxdiff, mindiff, meandiff) %>%
#   # Use pivot_longer to stack the difference columns
#   pivot_longer(
#     cols = c(maxdiff, mindiff, meandiff),
#     names_to = "DifferenceType",
#     values_to = "DifferenceValue"
#   ) %>%
#   # Clean up the names for nicer plot labels
#   mutate(
#     DifferenceType = recode(
#       DifferenceType,
#       "meandiff" = "Mean Difference",
#       "mindiff" = "Min Value Difference",
#       "maxdiff" = "Max Value Difference"
#     ),
#     # Make Fold a factor to ensure bars are plotted for each fold
#     Fold = as.factor(Fold)
#   )
# 
# # 3. --- CREATE THE FACETED BAR PLOT ---
# # This plot will show the magnitude of the difference for each fold.
# # Higher bars indicate a larger disparity between the test and calibration set.
# difference_plot <- ggplot(diff_long, aes(x = Fold, y = DifferenceValue)) +
#   # geom_col() creates the bar chart
#   geom_col(show.legend = FALSE) +
#   
#   # Create a grid of plots: one for each combination of Region, Variable, and Difference Type
#   facet_grid(DifferenceType ~ region + Variable, scales = "free_y") +
#   
#   # Add informative labels
#   labs(
#     title = "Disparity Between Calibration and Test Sets Across CV Folds",
#     subtitle = "Comparing Absolute Differences in Mean, Min, and Max Values",
#     x = "Cross-Validation Fold",
#     y = "Absolute Difference"
#   ) +
#   
#   # Use a clean theme
#   theme_bw(base_size = 14) +
#   theme(
#     strip.text = element_text(face = "bold", size = 10),
#     axis.text.x = element_text(size = 10),
#     panel.spacing.x = unit(1.5, "lines") # Add some space between variable groups
#   )
# 
# # 4. --- SAVE AND DISPLAY THE PLOT ---
# ggsave(
#   "allregions_10foldcv_splitsbarplot.pdf",
#   plot = difference_plot,
#   width = 16,
#   height = 9,
#   units = "in",
#   dpi = 600
# )
# 
# message("✅ Bar chart of split differences saved to allregions_10foldcv_splitsbarplot.pdf")
# 
# # Display the plot
# print(difference_plot)
# 
# 
# 
# 
# 

################################################################################
################################################################################
# ridgeplots but combined, not by region
################################################################################
################################################################################
# 1. --- SETUP ---
# Load libraries
library(dplyr)
library(purrr)
library(ggplot2)
library(ggridges)

# Ensure the combined_splits_df from the previous step is in your environment.
# This dataframe already contains all the data we need, structured correctly.
# If you don't have it, you can recreate it:
cal_df_long <- map_dfr(raw_cal_list, ~ as.data.frame(.x), .id = "Fold") %>%
  mutate(SplitType = "Calibration", Fold = as.integer(Fold))
test_df_long <- map_dfr(raw_test_list, ~ as.data.frame(.x), .id = "Fold") %>%
  mutate(SplitType = "Test", Fold = as.integer(Fold))
combined_splits_df <- bind_rows(cal_df_long, test_df_long) %>%
  select(Fold, SplitType, region, read_age, length) %>%
  pivot_longer(cols = c(read_age, length), names_to = "Variable", values_to = "Value") %>%
  mutate(
    Variable = ifelse(Variable == "read_age", "Fish Age (days)", "Fish Length (mm)"),
    SplitType = factor(SplitType, levels = c("Test", "Calibration"))
  )


# 2. --- CREATE THE RIDGELINE PLOT (AGGREGATED FOLDS) ---
message("Generating ridgeline plot for combined folds...")

# We need to make Fold a factor and reverse it so Fold 1 is at the top
combined_splits_df$Fold_factor <- factor(combined_splits_df$Fold, levels = rev(1:10))

combined_fold_plot <- ggplot(combined_splits_df, aes(x = Value, y = Fold_factor, fill = SplitType)) +
  geom_density_ridges(alpha = 0.7, scale = 0.9) +
  
  # Facet by Variable only (Age vs. Length)
  facet_wrap(~ Variable, scales = "free_x") +
  
  scale_fill_manual(values = c("Calibration" = "#D55E00", "Test" = "#0072B2")) +
  
  labs(
    title = "Distribution of Combined Data Across 10 CV Folds",
    subtitle = "Comparing the final Test vs. Calibration splits for the entire dataset",
    x = "Value (Age in Days or Length in mm)",
    y = "Cross-Validation Fold",
    fill = "Split Type"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(face = "bold"),
    legend.position = "top"
  )

# 3. --- SAVE AND DISPLAY THE PLOT ---
ggsave(
  "allregions_10foldcv_ridgeline_regionscombined.pdf",
  plot = combined_fold_plot,
  width = 12,
  height = 8,
  units = "in"
)

message("✅ Plot of combined fold distributions saved to allregions_10foldcv_ridgeline_regionscombined.pdf")
print(combined_fold_plot)

# 1. --- CALCULATE AGGREGATED SUMMARY STATISTICS ---
message("\nCalculating summary statistics for combined folds...")

combined_summary_table <- combined_splits_df %>%
  # Group by Variable and Fold, ignoring region
  group_by(Variable, Fold, SplitType) %>%
  summarise(
    N = n(),
    Mean = mean(Value, na.rm = TRUE),
    Median = median(Value, na.rm = TRUE),
    StdDev = sd(Value, na.rm = TRUE),
    Min = min(Value, na.rm = TRUE),
    Max = max(Value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = SplitType,
    values_from = c(N, Mean, Median, StdDev, Min, Max)
  ) %>%
  # Calculate the difference columns
  mutate(
    max_diff = abs(Max_Calibration - Max_Test),
    min_diff = abs(Min_Calibration - Min_Test),
    mean_diff = abs(Mean_Calibration - Mean_Test)
  ) %>%
  arrange(Variable, Fold)

# 2. --- VIEW AND SAVE ---
message("✅ Combined summary table created successfully.")
print(as.data.frame(combined_summary_table))

write.csv(combined_summary_table, "allregions_10foldcv_splitsummary_combined.csv", row.names = FALSE)
message("\nFull summary table saved to allregions_10foldcv_splitsummary_combined.csv")


################################################################################
#
# ANALYSIS 2: REGION-TO-REGION VALIDATION
#
################################################################################

################################################################################
# 9. --- MAIN REGION-TO-REGION MODELING LOOP (REFACTORED) ---
################################################################################
# This is the section where you wanted the PLS models added.
# We can now call the same functions we used in the 10-fold CV.

unique_regions <- unique(df_raw$region)
region_results_list <- list() # Use a list for efficiency

# --- The Nested Loops ---
for (cal_region in unique_regions) {
  message(paste("\n----- STARTING CALIBRATION REGION:", cal_region, "-----"))
  
  # Prepare CALIBRATION data for all model types
  cal_df_raw <- df_raw %>% filter(region == !!cal_region)
  if(nrow(cal_df_raw) < 10) { message("  Skipping cal region (insufficient data)..."); next }
  cal_df_snvsg <- df_snvsg %>% filter(region == !!cal_region)
  cal_df_simple <- cal_df_raw %>% select(read_age, length, structure_weight) %>% filter(!is.na(length) & !is.na(structure_weight))
  
  # Inner loop for VALIDATION set
  for (val_region in unique_regions) {
    message(paste("  Validating against:", val_region))
    
    # Prepare VALIDATION data
    val_df_raw <- df_raw %>% filter(region == !!val_region)
    if(nrow(val_df_raw) < 1) { message("    Skipping val region (insufficient data)..."); next }
    val_df_snvsg <- df_snvsg %>% filter(region == !!val_region)
    val_df_simple <- val_df_raw %>% select(read_age, length, structure_weight) %>% filter(!is.na(length) & !is.na(structure_weight))
    
    # --- A. PLS MODELS ---
    # We call the same function, but with plot_diagnostics = FALSE
    pls_results <- run_all_pls_models(cal_df_raw, val_df_raw, 
                                      cal_df_snvsg, val_df_snvsg, 
                                      plot_diagnostics = FALSE)
    
    # --- B. PCA-BASED MODELS (LM & GAM) ---
    pca_results <- run_pca_models(cal_df_raw, val_df_raw, 
                                  best_lm_formula, best_gam_formula)
    
    # --- C. SIMPLE MODELS (LM & GAM) ---
    simple_results <- run_simple_models(cal_df_simple, val_df_simple)
    
    # 4. Combine results and add region labels
    loop_results <- bind_rows(pls_results, pca_results, simple_results) %>%
      mutate(Cal_Region = cal_region, Val_Region = val_region)
    
    region_results_list[[paste(cal_region, val_region)]] <- loop_results
    
  } # End inner loop
} # End outer loop

# Combine all region-to-region results
region_results_summary <- bind_rows(region_results_list)


################################################################################
# 10. --- AGGREGATE AND VISUALIZE REGION-TO-REGION RESULTS ---
################################################################################

# --- A. Summary Statistics Table ---
# Your old code `pls_results <- read.csv(...)` is no longer needed!
# The `region_results_summary` dataframe now contains ALL model results.

final_results <- region_results_summary %>%
  arrange(Cal_Region, Val_Region, desc(R2))

message("\n--- Final Performance Summary (Region-to-Region) ---")
print(as_tibble(final_results), n = 50) # Print more rows

write.csv(final_results, "region_testcal_performance_summary.csv", row.names = FALSE)
message("\nFull summary table saved to region_testcal_performance_summary.csv")


# --- B. Performance Visualization (Heatmap) ---

# 1. Check the range of your RMSE values
summary(final_results$RMSE)

# 2. If you have huge outliers, use this code to cap the color scale
#    (e.g., at 30 days) while still showing the true value in the text.

plot_region_heatmap <- final_results %>%
  filter(!is.na(RMSE)) %>%
  # Create a new column for visualization, capping the error at 30
  mutate(RMSE_viz = pmin(RMSE, 30)) %>% 
  
  # Fill using the CAPPED value
  ggplot(aes(x = Val_Region, y = Cal_Region, fill = RMSE_viz)) +
  geom_tile(color = "white") +
  
  # Label using the TRUE value
  geom_text(aes(label = round(RMSE, 1)), size = 2.5, color = "black") +
  
  # We can now use scale_fill_gradient2 to get a nice "white" midpoint
  scale_fill_gradient2(low = "darkgreen", mid = "white", high = "darkred",
                       midpoint = 15, # e.g., 15 days is our "medium" error
                       name = "RMSE (days)\n(Color scale capped at 30)") +
  
  facet_wrap(~ ModelType, ncol = 3) +
  labs(
    title = "Region-to-Region Model Transferability (by RMSE)",
    subtitle = "Models trained on 'Calibration Region' (y-axis) and tested on 'Validation Region' (x-axis). Text shows RMSE.",
    x = "Validation Region",
    y = "Calibration Region"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold", size = 12),
    legend.position = "bottom"
  )

print(plot_region_heatmap)
ggsave("region_testcal__heatmap_RMSE.pdf", plot = plot_region_heatmap, width = 12, height = 14)











final_results <- read.csv("Chapter 2/mixed_NEW/region_testcal_performance_summary.csv")
################################################################################
# 11. --- SIMPLIFIED 2x2 SUMMARY PLOT (FOR 3-MINUTE THESIS) ---
################################################################################
# This code block takes your detailed 'final_results' and creates the
# simplified 2x2 "Lab vs. Wild" conceptual summary plot.

message("\n--- Generating Simplified 2x2 'Lab vs. Wild' Summary Plot ---")

# 1. Create the 2x2 summary data
# We'll define "Lab" as 'LPW' and "Wild" as everything else.
summary_2x2 <- final_results %>%
  filter(!is.na(RMSE)) %>%
  
  # Step 1: Create the 'Lab' vs 'Wild' categories
  mutate(
    Cal_Type = if_else(Cal_Region == "LPW", "Lab Fish", "Wild Fish"),
    Val_Type = if_else(Val_Region == "LPW", "Lab Fish", "Wild Fish")
  ) %>%
  
  # Step 2: Group by these new categories and get the average RMSE
  group_by(ModelType, Cal_Type, Val_Type) %>%
  summarise(
    Avg_RMSE = mean(RMSE, na.rm = TRUE),
    .groups = 'drop' # Ungroup
  ) %>%
  
  # Step 3: Set the factor levels to control plot order
  # We want "Lab Fish" on the bottom row (like 'LPW' in the original)
  # and "Lab Fish" in the first column (like 'LPW' in the original)
  mutate(
    Cal_Type = factor(Cal_Type, levels = c("Wild Fish", "Lab Fish")),
    Val_Type = factor(Val_Type, levels = c("Lab Fish", "Wild Fish"))
  )

# 2. Plot the new 2x2 heatmap, FILTERED FOR "SG Only"
plot_2x2_heatmap_sg_only <- summary_2x2 %>%
  
  # Filter for the single model you want to plot
  filter(ModelType == "SG Only") %>%
  
  ggplot(aes(x = Val_Type, y = Cal_Type, fill = Avg_RMSE)) +
  
  # Add a thick white border to separate the 4 cells
  geom_tile(color = "white", size = 2) +
  
  # Add the text labels, rounded to 1 decimal
  geom_text(
    aes(label = sprintf("RMSE\n%.1f", Avg_RMSE)), 
    size = 10, # Interior text size
    color = "black",
    fontface = "bold"
  ) +
  
  # Use scale_fill_gradient2 (which accepts 'midpoint')
  # and a professional, colorblind-friendly Blue-White-Red palette.
  scale_fill_gradient2(
    low = "#0571b0",  # Dark Blue (Good)
    mid = "white",
    high = "#ca0020", # Dark Red (Bad)
    midpoint = 15,
    limit = c(5, 30),
    oob = scales::squish,
    name = "Avg. RMSE (days)"
  ) +
  
  # Add axis titles back
  labs(
    x = "Test Model On:",
    y = "Train Model On:"
  ) +
  
  # Use a minimal theme and make axis text bold
  theme_minimal(base_size = 30) + # Base font size
  theme(
    legend.position = "none", # Remove legend
    axis.text.x = element_text(size = 25, color = "black"), 
    axis.text.y = element_text(size = 25, color = "black", angle = 90, vjust = 0.5, hjust = 0.5), # Rotate Y axis text and center it
    panel.grid = element_blank() # Remove gridlines
  )

# 3. Print and save the new, single plot
print(plot_2x2_heatmap_sg_only)
ggsave("region_testcal__heatmap_2x2_SG_Only.pdf", plot = plot_2x2_heatmap_sg_only, width = 7, height = 7)

message("\n✅ Simplified 2x2 'SG Only' plot saved to region_testcal__heatmap_2x2_SG_Only.pdf")