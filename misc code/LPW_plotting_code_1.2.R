#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# 1. Setup: Libraries and Global Definitions ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Load all necessary libraries
library(ggplot2)
library(dplyr)
library(stringr)
library(patchwork)
library(gridExtra)
library(cowplot)
library(purrr)
library(gt)
library(data.table)

# Define the color palette used across multiple plots
color_palette <- c(
  "PCA"    = "#4477AA",  # Red-ish
  "PLS"    = "#AA3377",  # Green
  "ML"     = "#228833",  # Cyan
  "Simple" = "#CCBB44"   # Purple
)


# LOAD DATA # 

df <- readRDS("RDS_dataframes/LPW_scan_avg_proc.RDS")
df <- df[complete.cases(df$read_age), ]
df <- df %>% filter(specimen != 53, specimen != 74)


all_results_means <- readRDS(file.choose())
all_predictions <- readRDS(file.choose())
final_importance_data <- readRDS(file.choose())

all_results_means <- all_results_means_final
all_predictions <- all_predictions_final
final_importance_data <- final_importance_summary

rm(all_results_means_final, all_predictions_final, final_importance_summary)
# all_importance_data <- all_importance
# final_importance_data <- final_importance_data %>% filter(method %in% c("PLS-VIP", "Random Forest", "XGBoost"))
# saveRDS(final_importance_data, "RDS_dataframes/final_importance_data_CLEANED_10202025.RDS")
# all_results_means <- readRDS("RDS_dataframes/all_results_means_parallel_LOOCV_2025-10-14.RDS")
# all_predictions <- readRDS("RDS_dataframes/all_predictions_parallel_LOOCV_temp2025-10-14.RDS")
# final_importance_data <- readRDS("RDS_dataframes/final_importance_data_parallel_LOOCV_temp2025-10-14.RDS")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# 2. Global Data Preparation ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Find the single best LM and GAM model from 'all_results_means'
best_pca_models <- all_results_means %>%
  filter(ModelType %in% c("LM", "GAM")) %>% # filter for GAM and LM only
  group_by(Model, ModelType) %>% # group by LM/GAM 1-10
  summarise(Overall_Mean_RMSE = mean(RMSE, na.rm = TRUE), .groups = "drop") %>% # find RMSE average
  group_by(ModelType) %>% # group again by GAM/LM
  slice_min(order_by = Overall_Mean_RMSE, n = 1) # show lowest RMSE

# Now, we extract the names of the winning model variants to use for filtering
best_model_variants <- best_pca_models$Model
# Define best models 
best_gam <- best_pca_models %>% filter(ModelType == "GAM") %>% select(Model) %>% pull()
best_lm <- best_pca_models %>% filter(ModelType == "LM") %>% select(Model) %>% pull()


# Reorder models
final_model_order <- c("LM", "GAM", "PLS", "PLS (VIP)", "XGB", "RF", "LM (Simple)", "GAM (Simple)")
final_modeltype_order <- c("PCA", "PLS", "ML", "Simple")

# Create the cleaned data frame for Figure 1 from 'all_results_means'
all_results_cleaned <- all_results_means %>%
  filter(Model %in% best_pca_models$Model | !ModelType %in% c("LM", "GAM")) %>%
  mutate(
    Model = case_when(
      Model %in% best_pca_models$Model & ModelType == "LM" ~ "LM",
      Model %in% best_pca_models$Model & ModelType == "GAM" ~ "GAM",
      Model == "PLS" ~ "PLS",
      Model == "PLS - VIP" ~ "PLS (VIP)",
      Model == "Simple lm" ~ "LM (Simple)",
      Model == "Simple gam" ~ "GAM (Simple)",
      TRUE ~ as.character(Model)
    ),
    ModelType = case_when(
      ModelType %in% c("GAM", "LM") ~ "PCA",
      ModelType %in% c("XGB", "RF") ~ "ML",
      TRUE ~ as.character(ModelType)
    )
  ) %>%
  mutate(
    Model = factor(Model, levels = final_model_order),
    ModelType = factor(ModelType, levels = final_modeltype_order)
  )


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# 3. Figure 1: Model Performance Metrics (Boxplots) ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

create_boxplot <- function(data, y_var, y_lab, show_x_axis = FALSE) {
  p <- ggplot(data, aes(x = Model, y = {{ y_var }}, fill = ModelType)) +
    geom_boxplot(alpha = 0.8,
                 width = 0.5,
                 outlier.size = 1) +
    scale_fill_manual(values = color_palette) +
    scale_x_discrete(
      labels = function(x)
        str_replace(x, " \\(", "\n(")
    ) +
    labs(y = y_lab, x = NULL) +
    theme_bw(base_size = 13.5) +
    theme(legend.position = "none")
  if (!show_x_axis) {
    p <- p + theme(axis.title.x = element_blank(), axis.text.x = element_blank())
  }
  return(p)
}
p_rmse <- create_boxplot(all_results_cleaned, RMSE, "RMSE (Days)")
p_r2 <- create_boxplot(all_results_cleaned, R2, expression(R^2))
p_bias <- create_boxplot(all_results_cleaned, Bias, "Bias (Days)", show_x_axis = TRUE) +
  geom_hline(
    yintercept = 0,
    color = "gray40",
    linewidth = 1,
    linetype = 2
  )
p_rpd <- create_boxplot(all_results_cleaned, RPD, "RPD", show_x_axis = TRUE)
legend_plot <- ggplot(all_results_cleaned, aes(x = Model, y = RMSE, fill = ModelType)) +
  geom_boxplot() + scale_fill_manual(values = color_palette, name = "Model Type") + theme(legend.position = "bottom")
shared_legend <- get_legend(legend_plot)

# Create the composite plot object
composite_plot <- arrangeGrob(
  arrangeGrob(p_rmse, p_r2, p_bias, p_rpd, nrow = 2),
  shared_legend,
  nrow = 2,
  heights = c(10, 1)
)

# Display in RStudio
grid.arrange(composite_plot)

# Save as high-resolution PNG (recommended for Word)
ggsave(
  filename = "model_comparison_boxplots.png",
  plot = composite_plot,
  width = 12,        # Width in inches
  height = 8,        # Height in inches
  dpi = 600,         # 600 DPI is publication quality
  bg = "white"       # White background
)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# 4. Figure 2: Wavenumber Importance (Stacked Line/Ribbon) ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# The best models are:
# best_lm: "Linear 6" -> corresponds to "PCA-LM6" in the importance data
# best_gam: "GAM 6"    -> corresponds to "PCA-GAM6" in the importance data

# Extract the component number from the best model strings
lm_comp <- sub("Linear ", "", best_lm) # Extracts "6"
gam_comp <- sub("GAM ", "", best_gam) # Extracts "6"

# Define the exact original method names to select
best_lm_method <- paste0("PCA-LM", lm_comp)
best_gam_method <- paste0("PCA-GAM", gam_comp)

# Updated method order for consistency
final_method_order_importance <- c(
  paste0("Loadings (", best_lm, ")"),
  paste0("Loadings (", best_gam, ")"),
  "VIP Score (PLS)",
  "Gain (XGB)",
  "Permutation (RF)"
)

# Aggregate and relabel (using available variables: mean_importance, q025, q975)
importance_summary <- final_importance_data %>%
  mutate(
    # Rename the specific winning PCA models and the others
    method = case_when(
      method == best_lm_method ~ paste0("Loadings (", best_lm, ")"),
      method == best_gam_method ~ paste0("Loadings (", best_gam, ")"),
      method == "PLS-VIP" ~ "VIP Score (PLS)",
      method == "XGBoost" ~ "Gain (XGB)",
      method == "Random Forest" ~ "Permutation (RF)",
      TRUE ~ as.character(method)
    )
  ) %>%
  # Filter only the five required methods
  filter(method %in% final_method_order_importance) %>%
  group_by(method, wavenumber) %>%
  summarise(
    # Use the existing mean_importance for the line plot (final_mean)
    final_mean = mean(mean_importance, na.rm = TRUE),
    # Use the q025 and q975 columns from the original data for the ribbon
    # Since final_importance_data is already aggregated, we take the mean of the bounds
    lower_bound = mean(q025, na.rm = TRUE),
    upper_bound = mean(q975, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Convert to factor for plot ordering
  mutate(method = factor(method, levels = final_method_order_importance))

# Extract data for each method
lm_data  <- filter(importance_summary, method == paste0("Loadings (", best_lm, ")"))
gam_data <- filter(importance_summary, method == paste0("Loadings (", best_gam, ")"))
pls_data <- filter(importance_summary, method == "VIP Score (PLS)")
xgb_data <- filter(importance_summary, method == "Gain (XGB)")
rf_data  <- filter(importance_summary, method == "Permutation (RF)")








# Create plots (no changes needed here, as the data names are now correct)
p1 <- ggplot(lm_data, aes(x = wavenumber)) +
  geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound), fill = "#4477AA", alpha = 0.5) +
  geom_line(aes(y = final_mean), color = "#4477AA") +
  labs(y = paste0("Loadings\n(", best_lm, ")"), x = NULL) + 
  theme_bw() + 
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(), 
        axis.title.y = element_text(angle = 0, vjust = 0.5))

p2 <- ggplot(gam_data, aes(x = wavenumber)) +
  geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound), fill = "#4477AA", alpha = 0.5) +
  geom_line(aes(y = final_mean), color = "#4477AA") +
  labs(y = paste0("Loadings\n(", best_gam, ")"), x = NULL) + 
  theme_bw() + 
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(), 
        axis.title.y = element_text(angle = 0, vjust = 0.5))

p3 <- ggplot(pls_data, aes(x = wavenumber)) +
  geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound), fill = "#AA3377", alpha = 0.5) +
  geom_line(aes(y = final_mean), color = "#AA3377") +
  labs(y = "VIP Score\n(PLS)", x = NULL) + 
  theme_bw() + 
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(), 
        axis.title.y = element_text(angle = 0, vjust = 0.5))

p4 <- ggplot(xgb_data, aes(x = wavenumber)) +
  geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound), fill = "#228833", alpha = 0.5) +
  geom_line(aes(y = final_mean), color = "#228833") +
  labs(y = "Gain\n(XGB)", x = NULL) + 
  theme_bw() + 
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(), 
        axis.title.y = element_text(angle = 0, vjust = 0.5))

p5 <- ggplot(rf_data, aes(x = wavenumber)) +
  geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound), fill = "#228833", alpha = 0.5) +
  geom_line(aes(y = final_mean), color = "#228833") +
  labs(y = "Permutation\n(RF)", x = expression(paste("Wavenumber (cm"^{-1}, ")"))) + 
  theme_bw() + 
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5))

# Combine plots
(p1 / p2 / p3 / p4 / p5) +
  plot_annotation(
    title = "Wavenumber importance across model methods",
    subtitle = "Ribbons show range of importance values across all 500 10-fold CV iterations",
    theme = theme(plot.title = element_text(hjust = 0.5), 
                  plot.subtitle = element_text(hjust = 0.5))
  ) & scale_x_reverse()



# Clean up
rm(p1, p2, p3, p4, p5, p1_ci, p2_ci, p3_ci, p4_ci, p5_ci,
   lm_data, gam_data, pls_data, xgb_data, rf_data, 
   importance_summary, final_method_order_importance,
   best_lm, best_gam)

# 
# final_method_order_importance <- c("Loadings (PCA)", "VIP Score (PLS)", "Gain (XGB)", "Permutation (RF)")
# importance_summary <- final_importance_data %>%
#   mutate(
#     method = case_when(
#       method == "PCA Loadings"  ~ "Loadings (PCA)",
#       method == "PLS-VIP"       ~ "VIP Score (PLS)",
#       method == "XGBoost"       ~ "Gain (XGB)",
#       method == "Random Forest" ~ "Permutation (RF)",
#       TRUE ~ as.character(method)
#     ),
#     method = factor(method, levels = final_method_order_importance)
#   ) %>%
#   group_by(method, wavenumber) %>%
#   summarise(
#     final_mean = mean(importance, na.rm = TRUE),
#     lower_bound = min(importance, na.rm = TRUE),
#     upper_bound = max(importance, na.rm = TRUE),
#     lower_CI = quantile(importance, 0.025, na.rm = TRUE),
#     upper_CI = quantile(importance, 0.975, na.rm = TRUE),
#     .groups = "drop"
#   )
# pca_data <- filter(importance_summary, method == "Loadings (PCA)")
# pls_data <- filter(importance_summary, method == "VIP Score (PLS)")
# xgb_data <- filter(importance_summary, method == "Gain (XGB)")
# rf_data  <- filter(importance_summary, method == "Permutation (RF)")
# 
# p1 <- ggplot(pca_data, aes(x = wavenumber)) +
#   geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound), fill = "#4477AA", alpha = 0.5) +
#   geom_line(aes(y = final_mean), color = "#4477AA") +
#   labs(y = "Loadings\n(PCA)", x = NULL) + theme_bw() + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.y = element_text(angle = 0, vjust = 0.5))
# p2 <- ggplot(pls_data, aes(x = wavenumber)) +
#   geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound), fill = "#AA3377", alpha = 0.5) +
#   geom_line(aes(y = final_mean), color = "#AA3377") +
#   labs(y = "VIP Score\n(PLS)", x = NULL) + theme_bw() + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.y = element_text(angle = 0, vjust = 0.5))
# p3 <- ggplot(xgb_data, aes(x = wavenumber)) +
#   geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound), fill = "#228833", alpha = 0.5) +
#   geom_line(aes(y = final_mean), color = "#228833") +
#   labs(y = "Gain\n(XGB)", x = NULL) + theme_bw() + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.y = element_text(angle = 0, vjust = 0.5))
# p4 <- ggplot(rf_data, aes(x = wavenumber)) +
#   geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound), fill = "#228833", alpha = 0.5) +
#   geom_line(aes(y = final_mean), color = "#228833") +
#   labs(y = "Permutation\n(RF)", x = expression(paste("Wavenumber (cm"^{-1}, ")"))) + theme_bw() + theme(axis.title.y = element_text(angle = 0, vjust = 0.5))
# (p1 / p2 / p3 / p4) +
#   plot_annotation(
#     title = "Wavenumber importance across model methods",
#     subtitle = "Ribbons show range of importance values across all 500 10-fold CV iterations",
#     theme = theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))) & scale_x_reverse()
# 
# 
# # 95% CI
# 
# p1 <- ggplot(pca_data, aes(x = wavenumber)) +
#   geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI), fill = "#4477AA", alpha = 0.5) +
#   geom_line(aes(y = final_mean), color = "#4477AA") +
#   labs(y = "Loadings\n(PCA)", x = NULL) + theme_bw() + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.y = element_text(angle = 0, vjust = 0.5))
# p2 <- ggplot(pls_data, aes(x = wavenumber)) +
#   geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI), fill = "#AA3377", alpha = 0.5) +
#   geom_line(aes(y = final_mean), color = "#AA3377") +
#   labs(y = "VIP Score\n(PLS)", x = NULL) + theme_bw() + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.y = element_text(angle = 0, vjust = 0.5))
# p3 <- ggplot(xgb_data, aes(x = wavenumber)) +
#   geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI), fill = "#228833", alpha = 0.5) +
#   geom_line(aes(y = final_mean), color = "#228833") +
#   labs(y = "Gain\n(XGB)", x = NULL) + theme_bw() + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.y = element_text(angle = 0, vjust = 0.5))
# p4 <- ggplot(rf_data, aes(x = wavenumber)) +
#   geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI), fill = "#228833", alpha = 0.5) +
#   geom_line(aes(y = final_mean), color = "#228833") +
#   labs(y = "Permutation\n(RF)", x = expression(paste("Wavenumber (cm"^{-1}, ")"))) + theme_bw() + theme(axis.title.y = element_text(angle = 0, vjust = 0.5))
# (p1 / p2 / p3 / p4) +
#   plot_annotation(
#     title = "Wavenumber importance across model methods",
#     subtitle = "Ribbons show 95% CI of importance values across all 500 10-fold CV iterations",
#     theme = theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))) & scale_x_reverse()
# 
# 
# rm(p1, p2, p3, p4, pca_data, pls_data, xgb_data, rf_data, importance_summary, final_method_order_importance)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# 5. Figure 3: ECDF Difference Plot (CORRECTED ORDER) ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# --- Prepare data for K-S Test and ECDF Plot ---
# (The first part of the data prep is unchanged)
hatch_estimates <- all_predictions %>%
  left_join(df %>% select(specimen, sample_date), by = c("specimen_number" = "specimen")) %>%
  mutate(predhatch = sample_date - predicted) %>%
  group_by(specimen_number, model_variant) %>%
  summarise(median_hatch = median(predhatch, na.rm = TRUE), .groups = "drop")

best_lm_in_preds <- str_replace(best_pca_models$Model[best_pca_models$ModelType == "LM"], "Linear", "LM")
best_gam_in_preds <- best_pca_models$Model[best_pca_models$ModelType == "GAM"]

hatch_estimates_cleaned <- hatch_estimates %>%
  mutate(
    Model = case_when(
      model_variant == best_lm_in_preds  ~ "LM",
      model_variant == best_gam_in_preds  ~ "GAM",
      # ADDED: A rule for the plain "PLS" model
      model_variant == "PLS"             ~ "PLS", 
      model_variant == "PLS-VIP"         ~ "PLS (VIP)",
      model_variant == "XGBoost"         ~ "XGB",
      model_variant == "RF"              ~ "RF",
      # CORRECTED: Matched the lowercase 'gam' and 'lm' from your data
      model_variant == "Simple gam"      ~ "GAM (Simple)",
      model_variant == "Simple lm"      ~ "LM (Simple)",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Model)) %>%
  # Make sure "PLS" is included in your final_model_order vector
  mutate(Model = factor(Model, levels = final_model_order))

original_data <- df %>%
  select(specimen_number = specimen, original_hatch_date = hatch_date)

# --- MODIFICATION 1: Extract p-value along with D-statistic ---

ks_results <- map_dfr(levels(hatch_estimates_cleaned$Model), ~{
  # Subset the estimated hatch dates for the current model
  estimates_subset <- hatch_estimates_cleaned %>% filter(Model == .x)
  
  # Subset the original hatch dates for the corresponding specimens
  originals_subset <- original_data %>% 
    filter(specimen_number %in% estimates_subset$specimen_number)
  
  # Check if both subsets have enough data for ks.test (e.g., more than 1 observation)
  if (nrow(estimates_subset) > 1 && nrow(originals_subset) > 1) {
    # Perform the Kolmogorov-Smirnov test
    ks_test <- ks.test(estimates_subset$median_hatch, originals_subset$original_hatch_date)
    
    # Return the results
    return(tibble(
      Model = .x,
      D_statistic = ks_test$statistic,
      p_value = ks_test$p.value
    ))
  } else {
    # If not enough data, return a row with NA for statistics
    warning(paste("Not enough data for KS test for Model:", .x))
    return(tibble(
      Model = .x,
      D_statistic = NA_real_, # Use NA_real_ for numeric NA
      p_value = NA_real_
    ))
  }
})
# ks_results <- map_dfr(levels(hatch_estimates_cleaned$Model), ~{
#   estimates_subset <- hatch_estimates_cleaned %>% filter(Model == .x)
#   originals_subset <- original_data %>% filter(specimen_number %in% estimates_subset$specimen_number)
#   ks_test <- ks.test(estimates_subset$median_hatch, originals_subset$original_hatch_date)
#   tibble(
#     Model = .x,
#     D_statistic = ks_test$statistic,
#     p_value = ks_test$p.value # <-- ADD THIS LINE
#   )
# })

original_dates <- df %>%
  filter(specimen %in% hatch_estimates_cleaned$specimen_number) %>%
  pull(hatch_date)

calculate_ecdf_diff <- function(estimate_dates, original_dates) {
  x_grid <- sort(unique(c(estimate_dates, original_dates)))
  ecdf_estimate <- ecdf(estimate_dates)(x_grid)
  ecdf_original <- ecdf(original_dates)(x_grid)
  tibble(hatch_date = x_grid, ecdf_difference = ecdf_estimate - ecdf_original)
}

model_type_lookup <- all_results_cleaned %>% select(Model, ModelType) %>% distinct()

ecdf_differences <- hatch_estimates_cleaned %>%
  group_by(Model) %>%
  summarise(diff_data = list(calculate_ecdf_diff(median_hatch, original_dates)), .groups = "drop") %>%
  tidyr::unnest(diff_data) %>%
  left_join(ks_results, by = "Model") %>%
  left_join(model_type_lookup, by = "Model") %>%
  # --- MODIFICATION 2: Update the label to include the p-value ---
  mutate(
    # Format p-value for readability (e.g., show "p < 0.001" for very small values)
    p_label = if_else(p_value < 0.001, "p < 0.001", sprintf("p = %.3f", p_value)),
    ks_label = sprintf("D = %.3f\n%s", D_statistic, p_label) # <-- UPDATE THIS LINE
  ) %>%
  # Re-apply the factor levels to ensure correct plot order
  mutate(Model = factor(Model, levels = final_model_order))


# --- Create and print the final, correct plot ---
ggplot(ecdf_differences, aes(x = hatch_date, y = ecdf_difference)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_step(aes(color = ModelType, group = Model), linewidth = .9) +
  geom_text(aes(label = ks_label), x = -Inf, y = Inf, hjust = -0.1, vjust = 1.2, # Adjusted vjust for two lines
            size = 3, check_overlap = TRUE, lineheight = .9) +
  scale_color_manual(values = color_palette, name = "Model Type") +
  facet_wrap(~ Model, ncol = 4) +
  labs(
    title = "ECDF Difference (Estimate - Original) of Hatch Dates",
    subtitle = "Deviation from the zero-line indicates model error. D and p-values from K-S test shown.",
    x = "Hatch Date",
    y = "ECDF Difference"
  ) +
  theme_bw() +
  theme(legend.position = "bottom", strip.background = element_rect(fill = "gray90"))


# --- Clean up environment ---
rm(hatch_estimates, hatch_estimates_cleaned, original_data, ks_results, 
   original_dates, calculate_ecdf_diff, model_type_lookup, ecdf_differences,
   best_lm_in_preds, best_gam_in_preds)







################################################################################
################################################################################
# Model performance comparison
################################################################################
################################################################################


# Calculate and rank models by their median RMSE
performance_rankings <- all_results_cleaned %>%
  group_by(Model) %>%
  summarise(Median_RMSE = median(RMSE, na.rm = TRUE)) %>%
  arrange(Median_RMSE) # Arrange from lowest (best) to highest (worst)

print("--- Model Rankings by Overall Performance (Median RMSE) ---")
print(performance_rankings)

# First, create a "wide" dataframe where each row is an iteration and each column is a model
wide_results <- all_results_cleaned %>%
  select(Model, RMSE, SplitSet) %>%
  pivot_wider(names_from = Model, values_from = RMSE)

# For each row (iteration), find the column name (model) with the minimum RMSE
# Note: `.[-1]` is used to exclude the 'iteration' column from the min calculation
winners <- apply(wide_results[, -1], 1, function(row) {
  names(row)[which.min(row)]
})


# Count the wins for each model
consistency_rankings <- as.data.frame(table(winners)) %>%
  rename(Model = winners, Win_Count = Freq) %>%
  arrange(desc(Win_Count)) # Arrange from most wins to fewest

print("--- Model Rankings by Consistency (Number of Wins out of 500) ---")
print(consistency_rankings)


library(rstatix)
library(ggpubr)
library(dplyr)

# Assuming the rows in `all_results_cleaned` correspond to the same CV iteration
# We add an ID to represent the "pairing" variable.
# If you have an iteration column already, use that instead.
# all_results_cleaned <- all_results_cleaned %>%
#   group_by(Model) %>%
#   mutate(iteration = row_number()) %>%
#   ungroup()

# 1. Perform the Friedman Test (Omnibus Test) for RMSE
friedman_test_result <- all_results_cleaned %>%
  friedman_test(RMSE ~ Model | SplitSet)

print(friedman_test_result)
# A significant p-value (e.g., p < 0.05) indicates that at least one model's
# RMSE distribution is different from the others.

# 2. Perform Pairwise Wilcoxon Signed-Rank Tests (Post-Hoc)
# This compares every model against every other model.
pwc_results <- all_results_cleaned %>%
  wilcox_test(
    RMSE ~ Model,
    paired = TRUE,
    p.adjust.method = "holm" # Holm-Bonferroni correction
  )

print(pwc_results)







# R code to calculate the CV of RMSE for each model - indicates stability
stability_analysis <- all_results_cleaned %>%
  group_by(Model) %>%
  summarise(
    Mean_RMSE = mean(RMSE, na.rm = TRUE),
    SD_RMSE = sd(RMSE, na.rm = TRUE),
    CV_RMSE = (SD_RMSE / Mean_RMSE) * 100  # CV as a percentage
  ) %>%
  arrange(CV_RMSE) # Sort by most stable (lowest CV)

print(stability_analysis)




# Assuming 'all_predictions' has 'predicted' and 'read_age' columns
# You might want to average the predictions for each fish across the 500 iterations first
# Define the final order and color palette for consistency
final_model_order <- c("LM", "GAM", "PLS (VIP)", "XGB", "RF", "LM (Simple)", "GAM (Simple)")
color_palette <- c(
  "PCA"    = "#4477AA",
  "PLS"    = "#AA3377",
  "ML"     = "#228833",
  "Simple" = "#CCBB44"
)

# Filter the main predictions dataframe and clean up model names
predictions_for_plot <- all_predictions %>%
  filter(
    model_variant %in% best_model_variants | !model_type %in% c("LM", "GAM")
  ) %>%
  mutate(
    Model = case_when(
      model_variant %in% best_model_variants & model_type == "LM" ~ "LM",
      model_variant %in% best_model_variants & model_type == "GAM" ~ "GAM",
      model_variant == "PLS-VIP"                                   ~ "PLS (VIP)",
      model_variant == "XGBoost"                                   ~ "XGB",
      model_variant == "RF"                                        ~ "RF",
      model_variant == "Simple LM"                                 ~ "LM (Simple)",
      model_variant == "Simple GAM"                                ~ "GAM (Simple)",
      TRUE                                                         ~ NA_character_
    ),
    ModelType = case_when(
      Model %in% c("LM", "GAM")                   ~ "PCA",
      Model == "PLS (VIP)"                        ~ "PLS",
      Model %in% c("XGB", "RF")                   ~ "ML",
      Model %in% c("LM (Simple)", "GAM (Simple)") ~ "Simple",
      TRUE                                        ~ NA_character_
    )
  ) %>%
  filter(!is.na(Model)) %>%
  mutate(Model = factor(Model, levels = final_model_order))

# Calculate the average prediction and residual for each fish
avg_predictions <- predictions_for_plot %>%
  group_by(specimen_number, Model, ModelType, actual) %>%
  summarise(avg_predicted_age = mean(predicted, na.rm = TRUE), .groups = 'drop') %>%
  mutate(residual = avg_predicted_age - actual)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# 3. Create the Final Plot ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

ggplot(avg_predictions, aes(x = actual, y = residual)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_point(aes(color = ModelType), alpha = 0.6, size = 1.5) +
  geom_smooth(method = "loess", se = FALSE, color = "#002244", linewidth = 1) +
  scale_color_manual(values = color_palette, name = "Model Type") +
  facet_wrap(~ Model, ncol = 3) +
  labs(
    x = "Actual Age (Days)",
    y = "Prediction Error (Predicted - Actual)"
  ) +
  theme_bw(base_size = 16) +
  theme(legend.position = "bottom", strip.background = element_rect(fill = "gray90"))




################################################################################
################################################################################
# MODEL PERFORMANCE TABLES
################################################################################
################################################################################











# Define your color palette
color_palette <- c(
  "PCA"    = "#4477AA", # Linear and GAM models
  "PLS"    = "#AA3377", # PLS models
  "ML"     = "#228833", # RF and XGB models
  "Simple" = "#CCBB44"  # Simple LM and GAM
)

# Prepare the data with the corrected logic
table_data_full <- all_results_means %>%
  mutate(
    # Ensure model names are "Linear X" to match your image
    Model = str_replace(Model, "^LM ", "Linear "),
    Model = str_replace(Model, "Simple lm", "Simple LM"),
    Model = str_replace(Model, "Simple gam", "Simple GAM"),
    # CORRECTED: This now properly detects "Linear" and "GAM" models
    ModelType = case_when(
      str_detect(Model, "Linear|GAM ") ~ "PCA",
      str_detect(Model, "PLS") ~ "PLS",
      Model %in% c("XGB", "RF") ~ "ML",
      str_detect(Model, "Simple") ~ "Simple",
      TRUE ~ as.character(ModelType)
    )
  ) %>%
  group_by(Model, ModelType) %>%
  summarise(across(c(RMSE, R2, Bias, RPD), ~mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  arrange(RMSE)

max_abs_bias_full <- max(abs(table_data_full$Bias))

# Create the gt object
table_to_export_full <- table_data_full %>%
  select(-ModelType) %>%
  gt() %>%
  data_color(columns = RMSE, palette = c("#63BE7B", "#FFEB84", "#F8696B")) %>%
  data_color(columns = R2, palette = c("#F8696B", "#FFEB84", "#63BE7B")) %>%
  data_color(columns = Bias, palette = c("#F8696B", "#63BE7B", "#F8696B"), domain = c(-max_abs_bias_full, 0, max_abs_bias_full)) %>%
  data_color(columns = RPD, palette = c("#F8696B", "#FFEB84", "#63BE7B")) %>%
  fmt_number(columns = c(RMSE, R2, Bias, RPD), decimals = 3) %>%
  tab_options(column_labels.border.bottom.width = px(2), column_labels.border.bottom.color = "black", table_body.hlines.color = "#ededed")

# Loop through to color the Model cells
for (type_name in names(color_palette)) {
  table_to_export_full <- table_to_export_full %>%
    tab_style(
      style = cell_fill(color = color_palette[[type_name]], alpha = 0.5),
      locations = cells_body(columns = Model, rows = table_data_full$ModelType == type_name)
    )
}


table_to_export_full

table_to_export_full %>%
  gtsave("results_all.html")


# Define your color palette
color_palette <- c(
  "PCA"    = "#4477AA", # MLR and GAM models
  "PLS"    = "#AA3377", # PLS models
  "ML"     = "#228833", # RF and XGB models
  "Simple" = "#CCBB44"  # Simple LM and GAM
)

# --- 1. Find the single best LM (MLR) model ---
best_mlr <- all_results_means %>%
  filter(ModelType == "LM") %>%
  group_by(Model, ModelType) %>%
  summarise(across(c(RMSE, R2, Bias, RPD), ~mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  slice_min(order_by = RMSE, n = 1)

# --- 2. Find the single best GAM model ---
best_gam <- all_results_means %>%
  filter(ModelType == "GAM") %>%
  group_by(Model, ModelType) %>%
  summarise(across(c(RMSE, R2, Bias, RPD), ~mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  slice_min(order_by = RMSE, n = 1)

# --- 3. Summarize all other model types ---
other_models <- all_results_means %>%
  filter(!ModelType %in% c("LM", "GAM")) %>%
  group_by(Model, ModelType) %>%
  summarise(across(c(RMSE, R2, Bias, RPD), ~mean(.x, na.rm = TRUE)), .groups = "drop")

# --- 4. Combine and apply final renaming for the table ---
table_data_filtered <- bind_rows(best_mlr, best_gam, other_models) %>%
  mutate(
    # Rename the selected models to their general names
    Model = case_when(
      ModelType == "LM" ~ "MLR",
      ModelType == "GAM" ~ "GAM",
      Model == "Simple lm" ~ "Simple LM",
      Model == "Simple gam" ~ "Simple GAM",
      TRUE ~ Model
    ),
    # Re-assign ModelType for coloring
    ModelType = case_when(
      ModelType %in% c("LM", "GAM") ~ "PCA",
      ModelType %in% c("RF", "XGB") ~ "ML",
      TRUE ~ as.character(ModelType)
    )
  ) %>%
  arrange(RMSE)

max_abs_bias_filtered <- max(abs(table_data_filtered$Bias))

# Define your color palette
color_palette <- c(
  "PCA"    = "#4477AA", # MLR and GAM models
  "PLS"    = "#AA3377", # PLS models
  "ML"     = "#228833", # RF and XGB models
  "Simple" = "#CCBB44"  # Simple LM and GAM
)

# --- 1. Find the single best LM (MLR) model ---
best_mlr <- all_results_means %>%
  filter(ModelType == "LM") %>%
  group_by(Model, ModelType) %>%
  summarise(across(c(RMSE, R2, Bias, RPD), ~mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  slice_min(order_by = RMSE, n = 1)

# --- 2. Find the single best GAM model ---
best_gam <- all_results_means %>%
  filter(ModelType == "GAM") %>%
  group_by(Model, ModelType) %>%
  summarise(across(c(RMSE, R2, Bias, RPD), ~mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  slice_min(order_by = RMSE, n = 1)

# --- 3. Summarize all other model types ---
other_models <- all_results_means %>%
  filter(!ModelType %in% c("LM", "GAM")) %>%
  group_by(Model, ModelType) %>%
  summarise(across(c(RMSE, R2, Bias, RPD), ~mean(.x, na.rm = TRUE)), .groups = "drop")

# --- 4. Combine and apply final renaming for the table ---
table_data_filtered <- bind_rows(best_mlr, best_gam, other_models) %>%
  mutate(
    Model = case_when(
      ModelType == "LM" ~ "MLR",
      ModelType == "GAM" ~ "GAM",
      Model == "Simple lm" ~ "Simple LM",
      Model == "Simple gam" ~ "Simple GAM",
      TRUE ~ Model
    ),
    ModelType = case_when(
      ModelType %in% c("LM", "GAM") ~ "PCA",
      ModelType %in% c("RF", "XGB") ~ "ML",
      TRUE ~ as.character(ModelType)
    )
  ) %>%
  arrange(RMSE)

max_abs_bias_filtered <- max(abs(table_data_filtered$Bias))

# --- 5. Create the gt object ---
filtered_table_final <- table_data_filtered %>%
  select(-ModelType) %>%
  gt() %>%
  data_color(columns = RMSE, palette = c("#63BE7B", "#FFEB84", "#F8696B")) %>%
  # CORRECTED LINE: Added the missing quote before #63BE7B
  data_color(columns = R2, palette = c("#F8696B", "#FFEB84", "#63BE7B")) %>%
  data_color(columns = Bias, palette = c("#F8696B", "#63BE7B", "#F8696B"), domain = c(-max_abs_bias_filtered, 0, max_abs_bias_filtered)) %>%
  data_color(columns = RPD, palette = c("#F8696B", "#FFEB84", "#63BE7B")) %>%
  fmt_number(columns = c(RMSE, R2, Bias, RPD), decimals = 3) %>%
  tab_options(column_labels.border.bottom.width = px(2), column_labels.border.bottom.color = "black", table_body.hlines.color = "#ededed")

# --- 6. Loop to color the Model cells ---
for (type_name in names(color_palette)) {
  filtered_table_final <- filtered_table_final %>%
    tab_style(
      style = cell_fill(color = color_palette[[type_name]], alpha = 0.5),
      locations = cells_body(columns = Model, rows = table_data_filtered$ModelType == type_name)
    )
}

# View the final table in RStudio
filtered_table_final


filtered_table_final %>%
  gtsave("results_filtered.html")
