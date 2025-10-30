# Load all necessary libraries
library(ggplot2)
library(dplyr)
library(stringr)
library(patchwork)
library(gridExtra)
library(cowplot)
library(purrr)
library(readxl)


ibm <- readRDS("RDS_dataframes/IBM_proc_filter.RDS") %>%
  select(-c(haul, date_collected, read_age, test_age, final_age, scan_name, timestamp, file_name, session_title, file_path))
ages <- read_xlsx("metadata/ibm_ages_10032025.xlsx") %>% 
  select(-c(avg_age, hatch_est)) %>%
  mutate(
    # Calculate the mean of age1, age2, and age3 for each row
    avg_age = rowMeans(select(., age1, age2, age3), na.rm = TRUE),
    # Calculate hatch estimate
    hatch_est = julian_date - avg_age
  ) %>% 
  # remove rows with NA for age1
  filter(!is.na(age1)) %>%
  # Group by row to perform row-wise operations
  rowwise() %>%
  # Calculate the standard deviation of the age estimates for each specimen
  mutate(
    age_sd = sd(c(age1, age2, age3), na.rm = TRUE)
  ) %>%
  # Ungroup to return to normal dataframe operations
  ungroup() %>%
  # Calculate the coefficient of variation (CV) in percent
  mutate(
    age_cv_percent = (age_sd / avg_age) * 100
  ) %>%
  # Filter to keep rows with a CV of 10% or less, or where CV is not applicable
  filter(age_cv_percent <= 10 | is.na(age_cv_percent)) %>% 
  select(-age_sd, -age_cv_percent)

df <- left_join(
  ages,
  ibm,
  by = c("specimen" = "specimen")
) %>%
  filter(specimen != 425970, run_number == 2)



rm(ages, ibm)


# Define the color palette used across multiple plots
color_palette <- c(
  "PCA"    = "#4477AA",  # Red-ish
  "PLS"    = "#AA3377",  # Green
  "ML"     = "#228833",  # Cyan
  "Simple" = "#CCBB44"   # Purple
)


# LOAD DATA # 

all_results_means <- readRDS("RDS_dataframes/IBM_filt_all_results_means_parallel2025-10-03.RDS")
all_predictions <- readRDS("RDS_dataframes/IBM_filt_all_predictions_parallel2025-10-03.RDS")
final_importance_data <- readRDS("RDS_dataframes/IBM_filt_final_importance_data_parallel2025-10-03.RDS")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# 2. Global Data Preparation ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Programmatically find the single best LM and GAM model from 'all_results_means'
best_pca_models <- all_results_means %>%
  filter(ModelType %in% c("LM", "GAM")) %>%
  group_by(Model, ModelType) %>%
  summarise(Overall_Mean_RMSE = mean(RMSE, na.rm = TRUE), .groups = "drop") %>%
  group_by(ModelType) %>%
  slice_min(order_by = Overall_Mean_RMSE, n = 1)

# Define the desired final order for models and categories
final_model_order <- c("LM", "GAM", "PLS (VIP)", "XGB", "RF", "LM (Simple)", "GAM (Simple)")
final_modeltype_order <- c("PCA", "PLS", "ML", "Simple")

# Create the cleaned data frame for Figure 1 from 'all_results_means'
all_results_cleaned <- all_results_means %>%
  filter(Model %in% best_pca_models$Model | !ModelType %in% c("LM", "GAM")) %>%
  filter(Model != "PLS") %>%
  mutate(
    Model = case_when(
      Model %in% best_pca_models$Model & ModelType == "LM" ~ "LM",
      Model %in% best_pca_models$Model & ModelType == "GAM" ~ "GAM",
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
    geom_boxplot(alpha = 0.8, width = 0.5, outlier.size = 1) +
    scale_fill_manual(values = color_palette) +
    scale_x_discrete(labels = function(x) str_replace(x, " \\(", "\n(")) +
    labs(y = y_lab, x = NULL) +
    theme_bw(base_size = 11) +
    theme(legend.position = "none")
  if (!show_x_axis) { p <- p + theme(axis.title.x = element_blank(), axis.text.x = element_blank()) }
  return(p)
}
p_rmse <- create_boxplot(all_results_cleaned, RMSE, "RMSE (days)")
p_r2 <- create_boxplot(all_results_cleaned, R2, expression(R^2))
p_bias <- create_boxplot(all_results_cleaned, Bias, "Bias (Days)", show_x_axis = TRUE) +
  geom_hline(yintercept = 0, color = "gray40", linewidth = 1, linetype = 2)
p_rpd <- create_boxplot(all_results_cleaned, RPD, "RPD", show_x_axis = TRUE)
legend_plot <- ggplot(all_results_cleaned, aes(x = Model, y = RMSE, fill = ModelType)) +
  geom_boxplot() + scale_fill_manual(values = color_palette, name = "Model Type") + theme(legend.position = "bottom")
shared_legend <- get_legend(legend_plot)
grid.arrange(arrangeGrob(p_rmse, p_r2, p_bias, p_rpd, nrow = 2), shared_legend, nrow = 2, heights = c(10, 1))
rm(p_rmse, p_r2, p_bias, p_rpd, legend_plot, shared_legend, create_boxplot)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# 4. Figure 2: Wavenumber Importance (Stacked Line/Ribbon) ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

final_method_order_importance <- c("Loadings (PCA)", "VIP Score (PLS)", "Gain (XGB)", "Permutation (RF)")
importance_summary <- final_importance_data %>%
  mutate(
    method = case_when(
      method == "PCA Loadings"  ~ "Loadings (PCA)",
      method == "PLS-VIP"       ~ "VIP Score (PLS)",
      method == "XGBoost"       ~ "Gain (XGB)",
      method == "Random Forest" ~ "Permutation (RF)",
      TRUE ~ as.character(method)
    ),
    method = factor(method, levels = final_method_order_importance)
  ) %>%
  group_by(method, wavenumber) %>%
  summarise(
    final_mean = mean(importance, na.rm = TRUE),
    lower_bound = min(importance, na.rm = TRUE),
    upper_bound = max(importance, na.rm = TRUE),
    lower_CI = quantile(importance, 0.025, na.rm = TRUE),
    upper_CI = quantile(importance, 0.975, na.rm = TRUE),
    .groups = "drop"
  )
pca_data <- filter(importance_summary, method == "Loadings (PCA)")
pls_data <- filter(importance_summary, method == "VIP Score (PLS)")
xgb_data <- filter(importance_summary, method == "Gain (XGB)")
rf_data  <- filter(importance_summary, method == "Permutation (RF)")

p1 <- ggplot(pca_data, aes(x = wavenumber)) +
  geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound), fill = "#4477AA", alpha = 0.5) +
  geom_line(aes(y = final_mean), color = "#4477AA") +
  labs(y = "Loadings\n(PCA)", x = NULL) + theme_bw() + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.y = element_text(angle = 0, vjust = 0.5))
p2 <- ggplot(pls_data, aes(x = wavenumber)) +
  geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound), fill = "#AA3377", alpha = 0.5) +
  geom_line(aes(y = final_mean), color = "#AA3377") +
  labs(y = "VIP Score\n(PLS)", x = NULL) + theme_bw() + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.y = element_text(angle = 0, vjust = 0.5))
p3 <- ggplot(xgb_data, aes(x = wavenumber)) +
  geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound), fill = "#228833", alpha = 0.5) +
  geom_line(aes(y = final_mean), color = "#228833") +
  labs(y = "Gain\n(XGB)", x = NULL) + theme_bw() + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.y = element_text(angle = 0, vjust = 0.5))
p4 <- ggplot(rf_data, aes(x = wavenumber)) +
  geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound), fill = "#228833", alpha = 0.5) +
  geom_line(aes(y = final_mean), color = "#228833") +
  labs(y = "Permutation\n(RF)", x = expression(paste("Wavenumber (cm"^{-1}, ")"))) + theme_bw() + theme(axis.title.y = element_text(angle = 0, vjust = 0.5))
(p1 / p2 / p3 / p4) +
  plot_annotation(
    title = "Wavenumber importance across model methods",
    subtitle = "Ribbons show range of importance values across all 500 10-fold CV iterations",
    theme = theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))) & scale_x_reverse()


# 95% CI

p1 <- ggplot(pca_data, aes(x = wavenumber)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI), fill = "#4477AA", alpha = 0.5) +
  geom_line(aes(y = final_mean), color = "#4477AA") +
  labs(y = "Loadings\n(PCA)", x = NULL) + theme_bw() + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.y = element_text(angle = 0, vjust = 0.5))
p2 <- ggplot(pls_data, aes(x = wavenumber)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI), fill = "#AA3377", alpha = 0.5) +
  geom_line(aes(y = final_mean), color = "#AA3377") +
  labs(y = "VIP Score\n(PLS)", x = NULL) + theme_bw() + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.y = element_text(angle = 0, vjust = 0.5))
p3 <- ggplot(xgb_data, aes(x = wavenumber)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI), fill = "#228833", alpha = 0.5) +
  geom_line(aes(y = final_mean), color = "#228833") +
  labs(y = "Gain\n(XGB)", x = NULL) + theme_bw() + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.y = element_text(angle = 0, vjust = 0.5))
p4 <- ggplot(rf_data, aes(x = wavenumber)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI), fill = "#228833", alpha = 0.5) +
  geom_line(aes(y = final_mean), color = "#228833") +
  labs(y = "Permutation\n(RF)", x = expression(paste("Wavenumber (cm"^{-1}, ")"))) + theme_bw() + theme(axis.title.y = element_text(angle = 0, vjust = 0.5))
(p1 / p2 / p3 / p4) +
  plot_annotation(
    title = "Wavenumber importance across model methods",
    subtitle = "Ribbons show 95% CI of importance values across all 500 10-fold CV iterations",
    theme = theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))) & scale_x_reverse()


rm(p1, p2, p3, p4, pca_data, pls_data, xgb_data, rf_data, importance_summary, final_method_order_importance)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# 5. Figure 3: ECDF Difference Plot (CORRECTED ORDER) ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# --- Prepare data for K-S Test and ECDF Plot ---
# (The first part of the data prep is unchanged)
hatch_estimates <- all_predictions %>%
  left_join(df %>% select(specimen, julian_date), by = c("specimen_number" = "specimen")) %>%
  mutate(predhatch = julian_date - predicted) %>%
  group_by(specimen_number, model_variant) %>%
  summarise(median_hatch = median(predhatch, na.rm = TRUE), .groups = "drop")

best_lm_in_preds <- str_replace(best_pca_models$Model[best_pca_models$ModelType == "LM"], "Linear", "LM")
best_gam_in_preds <- best_pca_models$Model[best_pca_models$ModelType == "GAM"]

hatch_estimates_cleaned <- hatch_estimates %>%
  mutate(
    Model = case_when(
      model_variant == best_lm_in_preds  ~ "LM",
      model_variant == best_gam_in_preds  ~ "GAM",
      model_variant == "PLS-VIP"          ~ "PLS (VIP)",
      model_variant == "XGBoost"          ~ "XGB",
      model_variant == "RF"               ~ "RF",
      model_variant == "Simple LM"        ~ "LM (Simple)",
      model_variant == "Simple GAM"       ~ "GAM (Simple)",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Model)) %>%
  mutate(Model = factor(Model, levels = final_model_order))

original_data <- df %>%
  select(specimen_number = specimen, original_hatch_date = hatch_est)

ks_results <- map_dfr(levels(hatch_estimates_cleaned$Model), ~{
  estimates_subset <- hatch_estimates_cleaned %>% filter(Model == .x)
  originals_subset <- original_data %>% filter(specimen_number %in% estimates_subset$specimen_number)
  ks_test <- ks.test(estimates_subset$median_hatch, originals_subset$original_hatch_date)
  tibble(Model = .x, D_statistic = ks_test$statistic)
})

original_dates <- df %>%
  filter(specimen %in% hatch_estimates_cleaned$specimen_number) %>%
  pull(hatch_est)

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
  mutate(ks_label = sprintf("D = %.3f", D_statistic)) %>%
  
  # --- FIX: Re-apply the factor levels to ensure correct plot order ---
  mutate(Model = factor(Model, levels = final_model_order))


# --- Create and print the final, correct plot ---
ggplot(ecdf_differences, aes(x = hatch_date, y = ecdf_difference)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_step(aes(color = ModelType, group = Model), linewidth = .7) +
  geom_text(aes(label = ks_label), x = -Inf, y = Inf, hjust = -0.1, vjust = 1.5,
            size = 3, check_overlap = TRUE) +
  scale_color_manual(values = color_palette, name = "Model Type") +
  facet_wrap(~ Model, ncol = 4) +
  labs(
    title = "ECDF Difference (Estimate - Original) of Hatch Dates",
    subtitle = "Deviation from the zero-line indicates model error",
    x = "Hatch Date",
    y = "ECDF Difference"
  ) +
  theme_bw() +
  theme(legend.position = "bottom", strip.background = element_rect(fill = "gray90"))

# --- Clean up environment ---
rm(hatch_estimates, hatch_estimates_cleaned, original_data, ks_results, 
   original_dates, calculate_ecdf_diff, model_type_lookup, ecdf_differences,
   best_lm_in_preds, best_gam_in_preds)
