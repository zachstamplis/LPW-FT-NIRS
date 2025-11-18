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


################################################################################
################################################################################
# Load Data
################################################################################
################################################################################


# Load BOTH dataframes
df_raw <- readRDS("RDS_dataframes/combined_IBM_LPW_raw.RDS")
df_snvsg <- readRDS("RDS_dataframes/combined_IBM_LPW_experimental_SNVandSG.RDS")

df_raw <- df_raw %>% filter(!is.na(read_age))
df_snvsg <- df_snvsg %>% filter(!is.na(read_age))


################################################################################
################################################################################
# 10-fold CV, all models, combined regions for test/cal
################################################################################
################################################################################

# --- Define constants and prepare for outputs ---
k <- 10 # Number of folds
if (!dir.exists("pls_plots_final_10fold")) {
  dir.create("pls_plots_final_10fold")
}
# Expanded results summary to hold all model types
results_summary <- data.frame()


################################################################################
################################################################################
# Dredge 
################################################################################
################################################################################

# 2. --- PCA MODEL SELECTION (CORRECTED: Renaming Columns) ---
message("--- Finding Best PCA-based Model Formulas using dredge() ---")

speccols_raw <- names(df_raw)[grepl("^\\d", names(df_raw))]
message("Applying Savitzky-Golay filter to full dataset for model selection...")
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
################################################################################
# Stratify splits per region (ensure even splits of read_age for each region)
################################################################################
################################################################################
# 3. --- PREPARE STRATIFIED DATA SPLITS --- This ensures even splits of read_age for EACH region instead of for the whole dataset
# These models are using ALL regions combined, no region specific splits for test/cal

df_raw$unique_row_id <- 1:nrow(df_raw); df_snvsg$unique_row_id <- 1:nrow(df_snvsg)
message("\n--- Generating stratified folds for SPECTRAL models ---")
raw_cal_list <- vector("list", k); raw_test_list <- vector("list", k)
list_of_regional_dfs <- split(df_raw, df_raw$region)
for (region_name in names(list_of_regional_dfs)) {
  current_region_df <- list_of_regional_dfs[[region_name]]
  test_indices_for_region <- createFolds(current_region_df$read_age, k = k, returnTrain = FALSE, list = TRUE)
  regional_test_dfs <- lapply(test_indices_for_region, function(indices) current_region_df[indices, ])
  regional_cal_dfs <- lapply(test_indices_for_region, function(indices) current_region_df[-indices, ])
  for (i in seq_along(regional_test_dfs)) {
    raw_test_list[[i]] <- bind_rows(raw_test_list[[i]], regional_test_dfs[[i]])
    raw_cal_list[[i]] <- bind_rows(raw_cal_list[[i]], regional_cal_dfs[[i]])
  }
}
snvsg_cal_list <- lapply(raw_cal_list, function(df) df_snvsg %>% filter(unique_row_id %in% df$unique_row_id))
snvsg_test_list <- lapply(raw_test_list, function(df) df_snvsg %>% filter(unique_row_id %in% df$unique_row_id))
message("✅ Spectral model data lists created successfully.")
message("\n--- Generating stratified folds for SIMPLE models ---")
df_simple <- df_raw %>% select(region, read_age, length, structure_weight, unique_row_id) %>% filter(!is.na(length) & !is.na(structure_weight))
simple_cal_list <- vector("list", k); simple_test_list <- vector("list", k)
list_of_regional_dfs_simple <- split(df_simple, df_simple$region)
for (region_name in names(list_of_regional_dfs_simple)) {
  current_region_df <- list_of_regional_dfs_simple[[region_name]]
  test_indices_for_region <- createFolds(current_region_df$read_age, k = k, returnTrain = FALSE, list = TRUE)
  regional_test_dfs <- lapply(test_indices_for_region, function(indices) current_region_df[indices, ])
  regional_cal_dfs <- lapply(test_indices_for_region, function(indices) current_region_df[-indices, ])
  for (i in seq_along(regional_test_dfs)) {
    simple_test_list[[i]] <- bind_rows(simple_test_list[[i]], regional_test_dfs[[i]])
    simple_cal_list[[i]] <- bind_rows(simple_cal_list[[i]], regional_cal_dfs[[i]])
  }
}
message("✅ Simple model data lists created successfully.")



################################################################################
################################################################################
# Main Modelling Loop
################################################################################
################################################################################

# 4. --- MAIN 10-FOLD CV MODELING LOOP ---
calculate_metrics <- function(actual, predicted) {
  tibble(
    R2 = cor(actual, predicted, use = "pairwise.complete.obs")^2,
    RMSE = caret::RMSE(pred = predicted, obs = actual),
    RPD = sd(actual, na.rm = TRUE) / RMSE,
    Bias = mean(predicted - actual, na.rm = TRUE)
  )
}

pdf("pls_plots_final_10fold/all_models_10fold_cv_plots.pdf", width = 8.5, height = 11)

# The main outer loop
for (i in 1:k) {
  message(paste0("\n--- Processing Fold ", i, "/", k, " ---"))
  
  cal_raw <- raw_cal_list[[i]]; test_raw <- raw_test_list[[i]]
  cal_snvsg <- snvsg_cal_list[[i]]; test_snvsg <- snvsg_test_list[[i]]
  cal_simple <- simple_cal_list[[i]]; test_simple <- simple_test_list[[i]]
  
  make_title_page <- function(title_text) {
    plot.new(); text(x = 0.5, y = 0.5, labels = title_text, cex = 1.5, font = 2)
  }
  
  # --- A. PLS MODELS (CORRECTED: No inner loop) ---
  
  # MODEL 1: SNV + SG
  tryCatch({
    message("  Building SNV + SG model...")
    speccols_snvsg <- names(cal_snvsg)[grepl("^\\d", names(cal_snvsg))]
    m <- pls(x = as.matrix(cal_snvsg[, speccols_snvsg]), y = as.matrix(cal_snvsg[, "read_age"]),
             x.test = as.matrix(test_snvsg[, speccols_snvsg]), y.test = as.matrix(test_snvsg[, "read_age"]),
             cv = 1, scale = F, center = T)
    make_title_page(paste("Fold:", i, "\n\nModel: SNV + SG")); plot(m)
    ncomp <- m$ncomp.selected
    results_summary <- bind_rows(results_summary, data.frame(Fold = i, ModelType = "SNV+SG", nComp = ncomp, R2 = m$res$test$r2[, ncomp], RMSE = m$res$test$rmse[, ncomp], Bias = m$res$test$bias[, ncomp], RPD = m$res$test$rpd[, ncomp]))
  }, error = function(e) { message(paste("    ERROR in SNV+SG model:", e$message)) })
  
  # Data Prep for subsequent PLS models
  speccols_raw <- names(cal_raw)[grepl("^\\d", names(cal_raw))]
  cal_sg_matrix <- savitzkyGolay(as.matrix(cal_raw[, speccols_raw]), m = 1, p = 3, w = 17)
  test_sg_matrix <- savitzkyGolay(as.matrix(test_raw[, speccols_raw]), m = 1, p = 3, w = 17)
  speccols_sg <- colnames(cal_sg_matrix)
  
  # MODEL 2: SG Only
  tryCatch({
    message("  Building SG Only model...")
    m <- pls(x = cal_sg_matrix, y = as.matrix(cal_raw[, "read_age"]),
             x.test = test_sg_matrix, y.test = as.matrix(test_raw[, "read_age"]),
             cv = 1, scale = F, center = T)
    make_title_page(paste("Fold:", i, "\n\nModel: SG Only")); plot(m)
    ncomp <- m$ncomp.selected
    results_summary <- bind_rows(results_summary, data.frame(Fold = i, ModelType = "SG Only", nComp = ncomp, R2 = m$res$test$r2[, ncomp], RMSE = m$res$test$rmse[, ncomp], Bias = m$res$test$bias[, ncomp], RPD = m$res$test$rpd[, ncomp]))
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
    make_title_page(paste("Fold:", i, "\n\nModel: Filtered SG")); plot(m_filtsg)
    ncomp <- m_filtsg$ncomp.selected
    results_summary <- bind_rows(results_summary, data.frame(Fold = i, ModelType = "Filtered SG", nComp = ncomp, R2 = m_filtsg$res$test$r2[, ncomp], RMSE = m_filtsg$res$test$rmse[, ncomp], Bias = m_filtsg$res$test$bias[, ncomp], RPD = m_filtsg$res$test$rpd[, ncomp]))
  }, error = function(e) { message(paste("    ERROR in Filtered SG model:", e$message)) })
  
  # MODELS 4 & 5: VIP Selection
  if (!is.null(m_filtsg)) {
    vip <- as.data.frame(vipscores(m_filtsg))
    tryCatch({
      message("  Building VIP > 0.5 model...")
      m <- pls(x = cal_sg_matrix[, speccols_filtsg], y = as.matrix(cal_raw[, "read_age"]),
               x.test = test_sg_matrix[, speccols_filtsg], y.test = as.matrix(test_raw[, "read_age"]),
               cv = 1, scale = F, center = T, exclcols = vip$V1 < 0.5)
      make_title_page(paste("Fold:", i, "\n\nModel: VIP > 0.5")); plot(m)
      ncomp <- m$ncomp.selected
      results_summary <- bind_rows(results_summary, data.frame(Fold = i, ModelType = "VIP > 0.5", nComp = ncomp, R2 = m$res$test$r2[, ncomp], RMSE = m$res$test$rmse[, ncomp], Bias = m$res$test$bias[, ncomp], RPD = m$res$test$rpd[, ncomp]))
    }, error = function(e) { message(paste("    ERROR in VIP > 0.5 model:", e$message)) })
    tryCatch({
      message("  Building VIP > 1.0 model...")
      m <- pls(x = cal_sg_matrix[, speccols_filtsg], y = as.matrix(cal_raw[, "read_age"]),
               x.test = test_sg_matrix[, speccols_filtsg], y.test = as.matrix(test_raw[, "read_age"]),
               cv = 1, scale = F, center = T, exclcols = vip$V1 < 1.0)
      make_title_page(paste("Fold:", i, "\n\nModel: VIP > 1.0")); plot(m)
      ncomp <- m$ncomp.selected
      results_summary <- bind_rows(results_summary, data.frame(Fold = i, ModelType = "VIP > 1.0", nComp = ncomp, R2 = m$res$test$r2[, ncomp], RMSE = m$res$test$rmse[, ncomp], Bias = m$res$test$bias[, ncomp], RPD = m$res$test$rpd[, ncomp]))
    }, error = function(e) { message(paste("    ERROR in VIP > 1.0 model:", e$message)) })
  }
  
  # --- B. PCA-BASED MODELS (LM & GAM) ---
  message("  Building PCA-based models...")
  # This diagram illustrates the correct, non-leaky way to handle PCA in cross-validation.
  
  tryCatch({
    cal_sg_fold <- savitzkyGolay(as.matrix(cal_raw[, speccols_raw]), m = 1, p = 3, w = 17)
    test_sg_fold <- savitzkyGolay(as.matrix(test_raw[, speccols_raw]), m = 1, p = 3, w = 17)
    pca_model_fold <- mdatools::pca(cal_sg_fold, ncomp = 10, center = TRUE, scale = FALSE)
    cal_pcs <- as.data.frame(pca_model_fold$res$cal$scores); colnames(cal_pcs) <- paste0("PC", 1:10)
    test_pcs <- as.data.frame(predict(pca_model_fold, test_sg_fold)$scores); colnames(test_pcs) <- paste0("PC", 1:10)
    cal_pca_df <- cbind(read_age = cal_raw$read_age, cal_pcs)
    test_pca_df <- cbind(read_age = test_raw$read_age, test_pcs)
    mod_pca_lm <- lm(best_lm_formula, data = cal_pca_df)
    preds_pca_lm <- predict(mod_pca_lm, newdata = test_pca_df)
    results_pca_lm <- calculate_metrics(test_pca_df$read_age, preds_pca_lm) %>% mutate(Fold = i, ModelType = "PCA-LM", Model = "PCA-LM")
    mod_pca_gam <- gam(best_gam_formula, data = cal_pca_df, method = "REML")
    preds_pca_gam <- predict(mod_pca_gam, newdata = test_pca_df)
    results_pca_gam <- calculate_metrics(test_pca_df$read_age, preds_pca_gam) %>% mutate(Fold = i, ModelType = "PCA-GAM", Model = "PCA-GAM")
    results_summary <- bind_rows(results_summary, results_pca_lm, results_pca_gam)
  }, error = function(e) { message(paste("    ERROR in PCA models:", e$message)) })
  
  # --- C. SIMPLE MODELS (LM & GAM) ---
  message("  Building simple models...")
  tryCatch({
    mod_simple_lm <- lm(read_age ~ length + structure_weight, data = cal_simple)
    preds_simple_lm <- predict(mod_simple_lm, newdata = test_simple)
    results_simple_lm <- calculate_metrics(test_simple$read_age, preds_simple_lm) %>% mutate(Fold = i, ModelType = "Simple-LM", Model = "Simple-LM")
    mod_simple_gam <- gam(read_age ~ s(length, k=4) + s(structure_weight, k=4), data = cal_simple, method = "REML")
    preds_simple_gam <- predict(mod_simple_gam, newdata = test_simple)
    results_simple_gam <- calculate_metrics(test_simple$read_age, preds_simple_gam) %>% mutate(Fold = i, ModelType = "Simple-GAM", Model = "Simple-GAM")
    results_summary <- bind_rows(results_summary, results_simple_lm, results_simple_gam)
  }, error = function(e) { message(paste("    ERROR in Simple models:", e$message)) })
  
} # End of the main for loop

dev.off()

################################################################################
################################################################################
# Aggregate results 
################################################################################
################################################################################


# 5. --- VIEW AND SAVE AGGREGATED RESULTS ---
summary_stats <- results_summary %>%
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

write.csv(results_summary, "ALL_MODELS_10fold_cv_results_raw.csv", row.names = FALSE)
write.csv(summary_stats, "ALL_MODELS_10fold_cv_results_summary.csv", row.names = FALSE)


################################################################################
################################################################################
# Ridge Plots for splits: length/read_age by region:
################################################################################
################################################################################

# 1. --- SETUP ---
# Load libraries for data wrangling and plotting
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(ggridges)

# Ensure your data lists from the full script are in the environment
# raw_cal_list, raw_test_list, simple_cal_list, simple_test_list

# 2. --- DATA PREPARATION FOR PLOTTING ---
message("Preparing data for visualization...")

# --- Part A: Prepare data for the SPECTRAL model splits (as before) ---
cal_df_long_spectral <- map_dfr(raw_cal_list, ~ as.data.frame(.x), .id = "Fold") %>%
  mutate(SplitType = "Calibration", Fold = as.integer(Fold))

test_df_long_spectral <- map_dfr(raw_test_list, ~ as.data.frame(.x), .id = "Fold") %>%
  mutate(SplitType = "Test", Fold = as.integer(Fold))

combined_splits_spectral <- bind_rows(cal_df_long_spectral, test_df_long_spectral) %>%
  select(Fold, SplitType, region, read_age, length) %>%
  pivot_longer(cols = c(read_age, length), names_to = "Variable", values_to = "Value") %>%
  mutate(
    Variable = ifelse(Variable == "read_age", "Fish Age (days)", "Fish Length (mm)"),
    SplitType = factor(SplitType, levels = c("Test", "Calibration"))
  )

# --- Part B: Prepare data for the SIMPLE model splits ---
cal_df_long_simple <- map_dfr(simple_cal_list, ~ as.data.frame(.x), .id = "Fold") %>%
  mutate(SplitType = "Calibration", Fold = as.integer(Fold))

test_df_long_simple <- map_dfr(simple_test_list, ~ as.data.frame(.x), .id = "Fold") %>%
  mutate(SplitType = "Test", Fold = as.integer(Fold))

combined_splits_simple <- bind_rows(cal_df_long_simple, test_df_long_simple) %>%
  select(Fold, SplitType, region, read_age, length) %>%
  pivot_longer(cols = c(read_age, length), names_to = "Variable", values_to = "Value") %>%
  mutate(
    Variable = ifelse(Variable == "read_age", "Fish Age (days)", "Fish Length (mm)"),
    SplitType = factor(SplitType, levels = c("Test", "Calibration"))
  )

message("✅ Data wrangling complete for both split types.")


# 3. --- CREATE THE RIDGELINE PLOTS ---
message("Generating ridgeline plots...")

# PLOT 1: For Spectral Models
plot_spectral_splits <- ggplot(combined_splits_spectral, aes(x = Value, y = region, fill = SplitType)) +
  geom_density_ridges(alpha = 0.7, scale = 0.9) +
  facet_grid(Variable ~ Fold, scales = "free_x") +
  scale_fill_manual(values = c("Calibration" = "#D55E00", "Test" = "#0072B2")) +
  labs(
    title = "Data Distributions for SPECTRAL Models (PLS, PCA-GAM, etc.)",
    subtitle = "Comparing Test vs. Calibration Splits for Each Region Across 10 CV Folds",
    x = "Value (Age in Days or Length in mm)", y = "Region", fill = "Split Type"
  ) +
  theme_minimal(base_size = 14) +
  theme(strip.text = element_text(face = "bold"), legend.position = "top")

# PLOT 2: For Simple Models
plot_simple_splits <- ggplot(combined_splits_simple, aes(x = Value, y = region, fill = SplitType)) +
  geom_density_ridges(alpha = 0.7, scale = 0.9) +
  facet_grid(Variable ~ Fold, scales = "free_x") +
  scale_fill_manual(values = c("Calibration" = "#D55E00", "Test" = "#0072B2")) +
  labs(
    title = "Data Distributions for SIMPLE Models (using length & structure_weight)",
    subtitle = "Comparing Test vs. Calibration Splits for Each Region Across 10 CV Folds (after removing NAs)",
    x = "Value (Age in Days or Length in mm)", y = "Region", fill = "Split Type"
  ) +
  theme_minimal(base_size = 14) +
  theme(strip.text = element_text(face = "bold"), legend.position = "top")


# 4. --- SAVE BOTH PLOTS TO A SINGLE 2-PAGE PDF ---
message("Saving both plots to a single PDF...")
pdf("cv_split_distributions_COMPLETE.pdf", width = 18, height = 10)
print(plot_spectral_splits) # This prints the first plot to page 1
print(plot_simple_splits)   # This prints the second plot to page 2
dev.off() # This closes the PDF device and saves the file

message("✅ Two-page plot saved to cv_split_distributions_COMPLETE.pdf")


################################################################################
################################################################################
# HISTOGRAMS FOR EACH REGION: AGE AND LEGNTH
################################################################################
################################################################################
# # Ensure the raw, combined dataframe is loaded and cleaned
# 
# df_raw <- readRDS("RDS_dataframes/combined_IBM_LPW_raw.RDS")
# df_raw <- df_raw %>% filter(!is.na(read_age), !is.na(length))
# 
# 
# # 2. --- DATA PREPARATION FOR PLOTTING ---
# # To create faceted plots, we need to convert the data from a "wide" format
# # to a "long" format. This stacks the 'read_age' and 'length' columns.
# df_long <- df_raw %>%
#   # Select only the columns of interest
#   select(region, read_age, length) %>%
#   # Use pivot_longer to reshape the data
#   pivot_longer(
#     cols = c(read_age, length),
#     names_to = "Variable",
#     values_to = "Value"
#   ) %>%
#   # Clean up the variable names for nicer plot labels
#   mutate(
#     Variable = ifelse(Variable == "read_age", "Fish Age (days)", "Fish Length (mm)")
#   )
# 
# 
# # 3. --- CREATE THE FACETED HISTOGRAM PLOT ---
# # This plot will have a grid of panels (facets).
# # Rows will be for Age and Length.
# # Columns will be for each of the 5 Regions.
# distribution_plot <- ggplot(df_long, aes(x = Value)) +
#   # geom_histogram() creates the bar plot. `bins=20` is a good starting point.
#   geom_histogram(aes(y = ..density..), bins = 30, fill = "#0072B2", color = "white", alpha = 0.7) +
#   # geom_density() overlays a smooth line to better show the shape
#   geom_density(color = "black", size = 1) +
#   
#   # This is the key function that creates the grid of plots.
#   # `scales = "free"` is CRITICAL. It allows each panel to have its own
#   # independent x and y axes, which is necessary since age and length have different scales.
#   facet_grid(Variable ~ region, scales = "free") +
#   
#   # Add informative labels
#   labs(
#     title = "Distribution of Fish Age and Length by Region",
#     subtitle = "Comparing the underlying data structure for each sampling location",
#     x = "Value (Age in Days or Length in mm)",
#     y = "Density"
#   ) +
#   
#   # Use a clean theme for readability
#   theme_bw(base_size = 14) +
#   theme(
#     strip.text = element_text(face = "bold"),
#     axis.text.x = element_text(angle = 45, hjust = 1)
#   )
# 
# # 4. --- SAVE AND DISPLAY THE PLOT ---
# ggsave(
#   "regional_distributions.pdf",
#   plot = distribution_plot,
#   width = 12,
#   height = 8,
#   units = "in"
# )
# 
# message("✅ Plot saved to regional_distributions.pdf")
# 
# # To display the plot in RStudio viewer as well:
# print(distribution_plot)

################################################################################
################################################################################
# Histogram with x-axis for both length AND age
################################################################################
################################################################################

# 1. --- SETUP ---
# Load the necessary libraries, including patchwork for combining plots


# --- DATA PREPARATION (from previous step) ---
df_long <- df_raw %>%
  select(region, read_age, length) %>%
  pivot_longer(
    cols = c(read_age, length),
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  mutate(
    Variable = ifelse(Variable == "read_age", "Fish Age (days)", "Fish Length (mm)")
  )

# PLOT A: Fish Age
age_plot <- df_long %>%
  filter(Variable == "Fish Age (days)") %>%
  ggplot(aes(x = Value)) +
  geom_histogram(aes(y = ..density..), bins = 20, fill = "#0072B2", color = "white", alpha = 0.7) +
  geom_density(color = "black", size = 1) +
  facet_wrap(~ region, scales = "free_y") + # Facet by region
  labs(
    x = "Fish Age (days)", # Specific label for this plot
    y = "Density"
  ) +
  theme_bw(base_size = 14) +
  theme(strip.text = element_text(face = "bold"))

# PLOT B: Fish Length
length_plot <- df_long %>%
  filter(Variable == "Fish Length (mm)") %>%
  ggplot(aes(x = Value)) +
  geom_histogram(aes(y = ..density..), bins = 20, fill = "#D55E00", color = "white", alpha = 0.7) +
  geom_density(color = "black", size = 1) +
  facet_wrap(~ region, scales = "free_y") + # Facet by region
  labs(
    x = "Fish Length (mm)", # Specific label for this plot
    y = "Density"
  ) +
  theme_bw(base_size = 14) +
  theme(strip.text = element_text(face = "bold"))

# 3. --- COMBINE AND SAVE THE PLOTS ---

# Use patchwork to stack the plots vertically (age_plot on top of length_plot)
combined_plot <- age_plot / length_plot

# Add a single, overarching title to the combined plot
final_plot <- combined_plot + 
  plot_annotation(
    title = "Distribution of Fish Age and Length by Region",
    subtitle = "Comparing the underlying data structure for each sampling location"
  )

# Save the final combined plot
ggsave(
  "regional_distributions_CORRECTED.pdf",
  plot = final_plot,
  width = 15,
  height = 8,
  units = "in"
)

message("✅ Corrected plot saved to regional_distributions_CORRECTED.pdf")

# Display the plot
print(final_plot)



################################################################################
################################################################################
# investigating splits of length/read_age
################################################################################
################################################################################
# useful for seeing if particular splits got a bad mix of specimens

# 1. --- SETUP ---
# Ensure the combined_splits_df from the ridgeline plot code is in your environment.
# If not, you can recreate it with this code:
message("Preparing data for summary tables...")
cal_df_long <- map_dfr(raw_cal_list, ~ as.data.frame(.x), .id = "Fold") %>%
  mutate(SplitType = "Calibration", Fold = as.integer(Fold))
test_df_long <- map_dfr(raw_test_list, ~ as.data.frame(.x), .id = "Fold") %>%
  mutate(SplitType = "Test", Fold = as.integer(Fold))
combined_splits_df <- bind_rows(cal_df_long, test_df_long) %>%
  select(Fold, SplitType, region, read_age, length) %>%
  pivot_longer(cols = c(read_age, length), names_to = "Variable", values_to = "Value")

# 2. --- CALCULATE AND FORMAT SUMMARY STATISTICS ---
# This uses dplyr to group the data and calculate stats for each group
summary_table <- combined_splits_df %>%
  group_by(Variable, Fold, region, SplitType) %>%
  summarise(
    N = n(), # Count of samples
    Mean = mean(Value, na.rm = TRUE),
    Median = median(Value, na.rm = TRUE),
    StdDev = sd(Value, na.rm = TRUE),
    Min = min(Value, na.rm = TRUE),
    Max = max(Value, na.rm = TRUE),
    .groups = "drop" # Ungroup after summarising
  ) %>%
  # Pivot the table to make comparison easier
  pivot_wider(
    names_from = SplitType,
    values_from = c(N, Mean, Median, StdDev, Min, Max)
  ) %>%
  # Reorder columns for clarity
  select(
    Variable, Fold, region, N_Calibration, N_Test, 
    Mean_Calibration, Mean_Test, Median_Calibration, Median_Test,
    StdDev_Calibration, StdDev_Test, Min_Calibration, Min_Test,
    Max_Calibration, Max_Test
  ) %>%
  # Arrange the data for easy viewing
  arrange(Variable, Fold, region)

message("✅ Summary table created successfully.")

# 3. --- VIEW RESULTS AND SAVE ---

# View a specific, problematic example to see the numerical difference
# Let's look at the 'read_age' data for LPW in Fold 2, which looked uneven in the plot
message("\n--- Example: Summary for read_age in Fold 2 (LPW) ---")
print(
  summary_table %>% 
    filter(Variable == "read_age", Fold == 2, region == "LPW")
)

# You can view the entire table in RStudio
# View(summary_table)
summary_table$maxdiff <- abs(summary_table$Max_Calibration - summary_table$Max_Test)
summary_table$mindiff <- abs(summary_table$Min_Calibration - summary_table$Min_Test)
summary_table$meandiff <- abs(summary_table$Mean_Calibration - summary_table$Mean_Test)
# Save the full table to a CSV file for further inspection in Excel or another program
write.csv(summary_table, "cv_split_summary_statistics.csv", row.names = FALSE)
message("\nFull summary table saved to cv_split_summary_statistics.csv")


# Ensure the summary_table is in your environment from the previous step.
# If not, you will need to re-run the code that generates it.

# Now, we'll reshape the data into a "long" format for ggplot
diff_long <- summary_table %>%
  # Select only the columns we need
  select(Variable, Fold, region, maxdiff, mindiff, meandiff) %>%
  # Use pivot_longer to stack the difference columns
  pivot_longer(
    cols = c(maxdiff, mindiff, meandiff),
    names_to = "DifferenceType",
    values_to = "DifferenceValue"
  ) %>%
  # Clean up the names for nicer plot labels
  mutate(
    DifferenceType = recode(
      DifferenceType,
      "meandiff" = "Mean Difference",
      "mindiff" = "Min Value Difference",
      "maxdiff" = "Max Value Difference"
    ),
    # Make Fold a factor to ensure bars are plotted for each fold
    Fold = as.factor(Fold)
  )

# 3. --- CREATE THE FACETED BAR PLOT ---
# This plot will show the magnitude of the difference for each fold.
# Higher bars indicate a larger disparity between the test and calibration set.
difference_plot <- ggplot(diff_long, aes(x = Fold, y = DifferenceValue)) +
  # geom_col() creates the bar chart
  geom_col(show.legend = FALSE) +
  
  # Create a grid of plots: one for each combination of Region, Variable, and Difference Type
  facet_grid(DifferenceType ~ region + Variable, scales = "free_y") +
  
  # Add informative labels
  labs(
    title = "Disparity Between Calibration and Test Sets Across CV Folds",
    subtitle = "Comparing Absolute Differences in Mean, Min, and Max Values",
    x = "Cross-Validation Fold",
    y = "Absolute Difference"
  ) +
  
  # Use a clean theme
  theme_bw(base_size = 14) +
  theme(
    strip.text = element_text(face = "bold", size = 10),
    axis.text.x = element_text(size = 10),
    panel.spacing.x = unit(1.5, "lines") # Add some space between variable groups
  )

# 4. --- SAVE AND DISPLAY THE PLOT ---
ggsave(
  "cv_split_differences_barchart.pdf",
  plot = difference_plot,
  width = 16,
  height = 9,
  units = "in",
  dpi = 600
)

message("✅ Bar chart of split differences saved to cv_split_differences_barchart.pdf")

# Display the plot
print(difference_plot)








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
  "cv_split_distributions_COMBINED.pdf",
  plot = combined_fold_plot,
  width = 12,
  height = 8,
  units = "in"
)

message("✅ Plot of combined fold distributions saved to cv_split_distributions_COMBINED.pdf")
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

write.csv(combined_summary_table, "cv_split_summary_statistics_COMBINED.csv", row.names = FALSE)
message("\nFull summary table saved to cv_split_summary_statistics_COMBINED.csv")























################################################################################
################################################################################
# REGION TEST/CAL SPLITS, INCLUDING PCA AND SIMPLE
################################################################################
################################################################################
# 1. --- SETUP ---
# Load all necessary libraries
library(mdatools)
library(dplyr)
library(prospectr)
library(mgcv)      # For GAM models
library(MuMIn)     # For dredge()

# Load BOTH dataframes
df_raw <- readRDS("RDS_dataframes/combined_IBM_LPW_raw.RDS")
df_snvsg <- readRDS("RDS_dataframes/combined_IBM_LPW_experimental_SNVandSG.RDS")
df_raw <- df_raw %>% filter(!is.na(read_age))
df_snvsg <- df_snvsg %>% filter(!is.na(read_age))

# --- Define constants and prepare for outputs ---
unique_regions <- unique(df_raw$region)
speccols_raw <- names(df_raw)[grepl("^\\d", names(df_raw))]
results_summary <- data.frame() # Initialize an empty dataframe to hold all results


# 2. --- PCA MODEL SELECTION (using dredge on the full dataset) ---
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
message("✅ Best formulas for PCA-LM and PCA-GAM identified.")


# 3. --- MAIN REGION-TO-REGION MODELING LOOP ---
# This helper function will calculate all metrics consistently
calculate_metrics <- function(actual, predicted) {
  tibble(
    R2 = cor(actual, predicted, use = "pairwise.complete.obs")^2,
    RMSE = caret::RMSE(pred = predicted, obs = actual),
    RPD = sd(actual, na.rm = TRUE) / RMSE,
    Bias = mean(predicted - actual, na.rm = TRUE)
  )
}

# --- The Nested Loops ---
for (cal_region in unique_regions) {
  message(paste("\n----- STARTING CALIBRATION REGION:", cal_region, "-----"))
  
  # Prepare CALIBRATION data for all model types
  cal_df_raw <- df_raw %>% filter(region == !!cal_region)
  if(nrow(cal_df_raw) < 10) { message("  Skipping cal region (insufficient data)..."); next }
  cal_df_snvsg <- df_snvsg %>% filter(region == !!cal_region)
  
  # Inner loop for VALIDATION set
  for (val_region in unique_regions) {
    # We can also test a region against itself to get an idea of "best-case" performance
    # if (cal_region == val_region) { next } 
    message(paste("  Validating against:", val_region))
    
    # Prepare VALIDATION data
    val_df_raw <- df_raw %>% filter(region == !!val_region)
    if(nrow(val_df_raw) < 10) { message("    Skipping val region (insufficient data)..."); next }
    val_df_snvsg <- df_snvsg %>% filter(region == !!val_region)
    
    # --- A. PLS MODELS ---
    # (This section is adapted from your working code)
    # ...
    
    # --- B. PCA-BASED MODELS (LM & GAM) ---
    tryCatch({
      # 1. Apply SG filter
      cal_sg_fold <- savitzkyGolay(as.matrix(cal_df_raw[, speccols_raw]), m = 1, p = 3, w = 17)
      val_sg_fold <- savitzkyGolay(as.matrix(val_df_raw[, speccols_raw]), m = 1, p = 3, w = 17)
      
      # 2. Build PCA on CALIBRATION data ONLY
      pca_model_fold <- mdatools::pca(cal_sg_fold, ncomp = 10, center = TRUE, scale = FALSE)
      
      # 3. Get scores for cal set and PREDICT scores for val set
      cal_pcs <- as.data.frame(pca_model_fold$res$cal$scores); colnames(cal_pcs) <- paste0("PC", 1:10)
      val_pcs <- as.data.frame(predict(pca_model_fold, val_sg_fold)$scores); colnames(val_pcs) <- paste0("PC", 1:10)
      
      # 4. Combine with response variable
      cal_pca_df <- cbind(read_age = cal_df_raw$read_age, cal_pcs)
      val_pca_df <- cbind(read_age = val_df_raw$read_age, val_pcs)
      
      # 5. Train and evaluate PCA-LM
      mod_pca_lm <- lm(best_lm_formula, data = cal_pca_df)
      preds_pca_lm <- predict(mod_pca_lm, newdata = val_pca_df)
      results_pca_lm <- calculate_metrics(val_pca_df$read_age, preds_pca_lm) %>%
        mutate(Cal_Region = cal_region, Val_Region = val_region, ModelType = "PCA-LM")
      
      # 6. Train and evaluate PCA-GAM
      mod_pca_gam <- gam(best_gam_formula, data = cal_pca_df, method = "REML")
      preds_pca_gam <- predict(mod_pca_gam, newdata = val_pca_df)
      results_pca_gam <- calculate_metrics(val_pca_df$read_age, preds_pca_gam) %>%
        mutate(Cal_Region = cal_region, Val_Region = val_region, ModelType = "PCA-GAM")
      
      results_summary <- bind_rows(results_summary, results_pca_lm, results_pca_gam)
      
    }, error = function(e) { message(paste("    ERROR in PCA models:", e$message)) })
    
    # --- C. SIMPLE MODELS (LM & GAM) ---
    tryCatch({
      # 1. Prepare data for simple models (filter for complete cases)
      cal_simple <- cal_df_raw %>% select(read_age, length, structure_weight) %>% filter(!is.na(length) & !is.na(structure_weight))
      val_simple <- val_df_raw %>% select(read_age, length, structure_weight) %>% filter(!is.na(length) & !is.na(structure_weight))
      
      if(nrow(cal_simple) < 10 || nrow(val_simple) < 1) {
        message("    Skipping Simple models (insufficient data after NA removal)...")
      } else {
        # 2. Train and evaluate Simple-LM
        mod_simple_lm <- lm(read_age ~ length + structure_weight, data = cal_simple)
        preds_simple_lm <- predict(mod_simple_lm, newdata = val_simple)
        results_simple_lm <- calculate_metrics(val_simple$read_age, preds_simple_lm) %>%
          mutate(Cal_Region = cal_region, Val_Region = val_region, ModelType = "Simple-LM")
        
        # 3. Train and evaluate Simple-GAM
        mod_simple_gam <- gam(read_age ~ s(length, k=4) + s(structure_weight, k=4), data = cal_simple, method = "REML")
        preds_simple_gam <- predict(mod_simple_gam, newdata = val_simple)
        results_simple_gam <- calculate_metrics(val_simple$read_age, preds_simple_gam) %>%
          mutate(Cal_Region = cal_region, Val_Region = val_region, ModelType = "Simple-GAM")
        
        results_summary <- bind_rows(results_summary, results_simple_lm, results_simple_gam)
      }
    }, error = function(e) { message(paste("    ERROR in Simple models:", e$message)) })
    
  } # End inner loop
} # End outer loop


# 4. --- VIEW AND SAVE RESULTS ---
# For clarity, let's combine the PLS results with the new results
pls_results <- read.csv("Chapter 2/pls_model_paired_performance_summary_FINAL.csv") # Assuming this file exists from your previous run
final_results <- bind_rows(pls_results, results_summary) %>%
  arrange(Cal_Region, Val_Region, ModelType)

message("\n--- Final Performance Summary (Region-to-Region) ---")
print(as_tibble(final_results), n = 50) # Print more rows

write.csv(final_results, "ALL_MODELS_paired_performance_summary.csv", row.names = FALSE)
message("\nFull summary table saved to ALL_MODELS_paired_performance_summary.csv")