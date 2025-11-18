# Load necessary libraries
library(ggplot2)
library(dplyr)
library(stringr)
library(patchwork)
library(gridExtra)
library(cowplot)
library(purrr)
library(gt)
library(data.table)
library(prospectr)
library(tidyverse)
library(viridis)

dfmeta_LPW <- readRDS("RDS_dataframes/LPW_dfmeta.RDS") # RAW FT-NIRS, all scans, no preprocess yet 
# Define your color palette
color_palette <- c(
  "PCA"    = "#4477AA", # Linear and GAM models
  "PLS"    = "#AA3377", # PLS models
  "ML"     = "#228833", # RF and XGB models
  "Simple" = "#CCBB44"  # Simple LM and GAM
)
timestamp <- format(Sys.Date(), "%Y-%m-%d")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Figure: Spectra: unproc. vs proc. ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


# --- 1. Recreate Averaged & Processed Dataframes ---
# This section is from your RMD file to ensure the data is ready.

# Filter for scans 2, 3, and 4
scan_2 <- dfmeta_LPW %>% dplyr::filter(run_number == 2)
scan_3 <- dfmeta_LPW %>% dplyr::filter(run_number == 3)
scan_4 <- dfmeta_LPW %>% dplyr::filter(run_number == 4)

# Create the averaged dataframe
scan_avg <- bind_cols(NULL, scan_2[, 1:20])
scan_avg <- bind_cols(scan_avg, (scan_2[, 21:ncol(scan_2)] + scan_3[, 21:ncol(scan_3)] + scan_4[, 21:ncol(scan_4)]) / 3)

# Create the pre-processed (Savitzky-Golay) dataframe
scan_avg_proc <- cbind(scan_avg[, 1:20],
                       savitzkyGolay(scan_avg[, 21:length(scan_avg)], m = 1, p = 3, w = 17))

# --- 2. Prepare Data for Plotting ---

# Filter for specimens that have a read_age and pivot to long format
scan_avg_long <- scan_avg %>%
  filter(!is.na(read_age)) %>%
  pivot_longer(
    cols = -c(1:20),
    names_to = "wavenumber",
    values_to = "absorbance"
  ) %>%
  mutate(wavenumber = as.numeric(wavenumber))

scan_proc_long <- scan_avg_proc %>%
  filter(!is.na(read_age)) %>%
  pivot_longer(
    cols = -c(1:20),
    names_to = "wavenumber",
    values_to = "absorbance"
  ) %>%
  mutate(wavenumber = as.numeric(wavenumber))

# --- 3. Create the Individual Plots ---

# Top Plot: Raw Averaged Spectra
p1 <- ggplot(scan_avg_long, aes(x = wavenumber, y = absorbance, group = specimen, color = read_age)) +
  geom_line(alpha = 0.6, linewidth = .6) +
  scale_x_reverse() +
  scale_color_viridis() +
  labs(
    y = "Raw Absorbance",
    color = "Age (days)"
  ) +
  theme_bw(base_size = 15) +
  theme(
    axis.title.x = element_blank(), # Remove x-axis title for the top plot
    axis.text.x = element_blank(),  # Remove x-axis labels for the top plot
    axis.ticks.x = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none" # Hide legend to create a shared one later
  )

# Bottom Plot: Pre-processed Spectra
p2 <- ggplot(scan_proc_long, aes(x = wavenumber, y = absorbance, group = specimen, color = read_age)) +
  geom_line(alpha = 0.6, linewidth = .6) +
  scale_x_reverse() +
  scale_color_viridis() +
  labs(
    y = "Preprocessed Absorbance",
    x = expression(paste("Wavenumber (", cm^-1, ")")),
    color = "Age (days)"
  ) +
  theme_bw(base_size = 15) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    legend.text = element_text(size = 14), # Adjust legend item text size
    legend.title = element_text(size = 14) # Adjust legend title size
  )

# --- 4. Combine Plots with Patchwork ---

# Use the patchwork package to combine plots and add a shared legend
final_plot <- (p1 / p2) + plot_layout(guides = 'collect')

# Display the final plot
final_plot



# --- Save the plot as a TIFF file (Recommended for publication) ---
# The ggsave function saves the last plot that was displayed by default.
# We specify the plot object explicitly for clarity.


# 
# 
# ggsave(
#   filename = paste0("Model Results/", "LPW", "_Raw_vs_Preprocessed_Spectra_", timestamp, ".tiff"), # The name of the file
#   plot = final_plot,                           # The plot object to save
#   device = "tiff",                             # The file format
#   width = 8.5,                                   # Width of the plot in inches
#   height = 6,                                  # Height of the plot in inches
#   dpi = 600,                                   # Resolution in Dots Per Inch
#   compression = "lzw"                          # A good lossless compression
# )
# 

# --- Alternative: Save as a PNG file ---
# PNG is also a great option, widely used and good quality.
# ggsave(
#   filename = paste0("Model Results/", "LPW", "_Raw_vs_Preprocessed_Spectra_", timestamp, ".png"),
#   plot = final_plot,
#   device = "png",
#   width = 8.5,
#   height = 6,
#   dpi = 600
# )
# 




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Figure: PCA FOR OUTLIERS ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Load necessary libraries
library(mdatools)
library(ggplot2)
library(viridis)
library(ggrepel)
library(dplyr)


pca_data_with_na <- readRDS("RDS_dataframes/LPW_scan_avg_proc.RDS")

# --- 1. Prepare and Clean the Data (No Changes) ---
# Assumes 'scan_avg_proc' is in your environment
# pca_data_with_na <- scan_avg_proc %>% filter(!is.na(read_age))

metadata <- pca_data_with_na[, 1:20]
spectral_data <- pca_data_with_na[, 21:ncol(pca_data_with_na)]

spectral_data_cleaned <- spectral_data %>%
  select(where(~ !any(is.na(.))))

# --- 2. Run PCA (No Changes) ---
# This step was successful for you
pca_results <- pca(spectral_data_cleaned, scale = TRUE)

# --- 3. Create Dataframes (No Change) ---
# This dataframe has ALL points (aged and unaged)
scores_df <- data.frame(
  PC1 = pca_results$calres$scores[, 1],  
  PC2 = pca_results$calres$scores[, 2],  
  specimen = metadata$specimen,
  read_age = metadata$read_age
)

# This dataframe has ONLY the aged points
aged_data <- scores_df %>% filter(!is.na(read_age))

# (Variance variables are the same)
pc1_variance <- round(pca_results$calres$expvar[1], 1)
pc2_variance <- round(pca_results$calres$expvar[2], 1)


# --- 4. Generate the Plot [REVISED] ---
PCA_outliers <- ggplot(scores_df, aes(x = PC1, y = PC2)) +
  
  # Ellipse (using all points)
  stat_ellipse(
    type = "norm", level = 0.99, geom = "polygon",
    alpha = 0.3, fill = "grey80" # Your darker fill
  ) +
  
  # Points (all points)
  geom_point(
    # [FIX 1] Use 'read_age' as a numeric variable, NOT a factor
    aes(color = read_age), 
    size = 3, alpha = 0.8
  ) +
  
  # Labels (aged points only)
  geom_text_repel(
    data = aged_data, 
    aes(label = specimen), 
    size = 3, max.overlaps = 15
  ) +
  
  # [FIX 2] Use the default continuous scale.
  # 'na.value' will still color the unaged points grey.
  scale_color_viridis(option = "D", na.value = "grey50") +
  
  labs(
    title = "PCA of Pre-processed Spectra for Outlier Detection",
    subtitle = "All specimens shown; aged specimens are labeled and colored",
    x = paste0("PC1 (", pc1_variance, "% variance explained)"),
    y = paste0("PC2 (", pc2_variance, "% variance explained)"),
    color = "Age (days)" # Legend title
  ) +
  theme_bw() +
  guides(fill = "none") 

PCA_outliers
# --- Alternative: Save as a PNG file ---
# PNG is also a great option, widely used and good quality.
# ggsave(
#   filename = paste0("Chapter 1/", "LPW", "_PCA_outliers_", timestamp, ".png"),
#   plot = PCA_outliers,
#   device = "png",
#   width = 7,
#   height = 6,
#   dpi = 600
# )
rm(pca_data_with_na)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Figure: AGE READ BIAS PLOTS ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
df <- readRDS("RDS_dataframes/LPW_scan_avg_proc.RDS")
df <- df[complete.cases(df$read_age), ]
age_reads <- read_csv("metadata/ages_LPW.csv")

ba_data_1_2 <- age_reads %>%
  filter(!is.na(`Age 1`) & !is.na(`Age 2`)) %>%
  mutate(
    difference = `Age 1` - `Age 2`,
    mean_age = (`Age 1` + `Age 2`) / 2
  ) %>%
  select(specimen, difference, mean_age)

# Calculate the mean difference (Bias) and its standard deviation
mean_diff_1_2 <- mean(ba_data_1_2$difference)
sd_diff_1_2 <- sd(ba_data_1_2$difference)

# Calculate the Limits of Agreement (LoA)
LoA_upper_1_2 <- mean_diff_1_2 + 1.96 * sd_diff_1_2
LoA_lower_1_2 <- mean_diff_1_2 - 1.96 * sd_diff_1_2


ba_plot_1_2_straight <- ggplot(ba_data_1_2, aes(x = mean_age, y = difference)) +
  # 1. Scatter points
  geom_point(shape = 21, size = 3, fill = "white", color = "black") +
  
  # 2. Reference Line at Y=0 (Red dashed line)
  geom_hline(yintercept = 0, color = "black", size = 0.8) +
  
  # 3. Mean Difference Line (Blue straight line - Bias)
  geom_hline(yintercept = mean_diff_1_2, linetype = "dashed", color = "red", size = 1) +
  
  # 4. Limits of Agreement (LoA) as dashed grey lines
  geom_hline(yintercept = LoA_upper_1_2, linetype = "dashed", color = "grey50", size = 0.8) +
  geom_hline(yintercept = LoA_lower_1_2, linetype = "dashed", color = "grey50", size = 0.8) +
  
  # If you want a confidence interval around the *mean difference* (blue line)
  # using a linear model (straight line), you can use geom_smooth:
  geom_smooth(method = "lm", color = "blue", fill = "gray", size = 0.5, se = TRUE) +
  
  
  # 5. Labels and Theme
  labs(
    x = "Mean Age (days)",
    y = "Age 1 - Age 2 (days)"
  ) +
  theme_bw(base_size = 18) +
  theme(panel.grid.minor = element_blank())

# Display the plot
print(ba_plot_1_2_straight)




ba_data_2_3 <- age_reads %>%
  filter(!is.na(`Age 2`) & !is.na(`Age 3`)) %>%
  mutate(
    difference = `Age 2` - `Age 3`,
    mean_age = (`Age 2` + `Age 3`) / 2
  ) %>%
  select(specimen, difference, mean_age)

# Calculate the mean difference (Bias) and its standard deviation
mean_diff_2_3 <- mean(ba_data_2_3$difference)
sd_diff_2_3 <- sd(ba_data_2_3$difference)

# Calculate the Limits of Agreement (LoA)
LoA_upper_2_3 <- mean_diff_2_3 + 1.96 * sd_diff_2_3
LoA_lower_2_3 <- mean_diff_2_3 - 1.96 * sd_diff_2_3


ba_plot_2_3_straight <- ggplot(ba_data_2_3, aes(x = mean_age, y = difference)) +
  # 1. Scatter points
  geom_point(shape = 21, size = 3, fill = "white", color = "black") +
  
  # 2. Reference Line at Y=0 (Red dashed line)
  geom_hline(yintercept = 0, color = "black", size = 0.8) +
  
  # 3. Mean Difference Line (Blue straight line - Bias)
  geom_hline(yintercept = mean_diff_2_3, linetype = "dashed", color = "red", size = 1) +
  
  # 4. Limits of Agreement (LoA) as dashed grey lines
  geom_hline(yintercept = LoA_upper_2_3, linetype = "dashed", color = "grey50", size = 0.8) +
  geom_hline(yintercept = LoA_lower_2_3, linetype = "dashed", color = "grey50", size = 0.8) +
  
  # If you want a confidence interval around the *mean difference* (blue line)
  # using a linear model (straight line), you can use geom_smooth:
  geom_smooth(method = "lm", color = "blue", fill = "gray", size = 0.5, se = TRUE) +
  
  
  # 5. Labels and Theme
  labs(
    x = "Mean Age (days)",
    y = "Age 2 - Age 3 (days)"
  ) +
  theme_bw(base_size = 18) +
  theme(panel.grid.minor = element_blank())

# Display the plot
print(ba_plot_2_3_straight)



ba_data_1_3 <- age_reads %>%
  filter(!is.na(`Age 1`) & !is.na(`Age 3`)) %>%
  mutate(
    difference = `Age 1` - `Age 3`,
    mean_age = (`Age 1` + `Age 3`) / 2
  ) %>%
  select(specimen, difference, mean_age)

# Calculate the mean difference (Bias) and its standard deviation
mean_diff_1_3 <- mean(ba_data_1_3$difference)
sd_diff_1_3 <- sd(ba_data_1_3$difference)

# Calculate the Limits of Agreement (LoA)
LoA_upper_1_3 <- mean_diff_1_3 + 1.96 * sd_diff_1_3
LoA_lower_1_3 <- mean_diff_1_3 - 1.96 * sd_diff_1_3


ba_plot_1_3_straight <- ggplot(ba_data_1_3, aes(x = mean_age, y = difference)) +
  # 1. Scatter points
  geom_point(shape = 21, size = 3, fill = "white", color = "black") +
  
  # 2. Reference Line at Y=0 (Red dashed line)
  geom_hline(yintercept = 0, color = "black", size = 0.8) +
  
  # 3. Mean Difference Line (Blue straight line - Bias)
  geom_hline(yintercept = mean_diff_1_3, linetype = "dashed", color = "red", size = 1) +
  
  # 4. Limits of Agreement (LoA) as dashed grey lines
  geom_hline(yintercept = LoA_upper_1_3, linetype = "dashed", color = "grey50", size = 0.8) +
  geom_hline(yintercept = LoA_lower_1_3, linetype = "dashed", color = "grey50", size = 0.8) +
  
  # If you want a confidence interval around the *mean difference* (blue line)
  # using a linear model (straight line), you can use geom_smooth:
  geom_smooth(method = "lm", color = "blue", fill = "gray", size = 0.5, se = TRUE) +
  
  
  # 5. Labels and Theme
  labs(
    x = "Mean Age (days)",
    y = "Age 1 - Age 3 (days)"
  ) +
  theme_bw(base_size = 18) +
  theme(panel.grid.minor = element_blank())

# Display the plot
print(ba_plot_1_3_straight)





combined_ba_plot <- ba_plot_1_2_straight + ba_plot_2_3_straight + ba_plot_1_3_straight

# To display the plot
print(combined_ba_plot)

# Save the combined plot to a PNG file
# ggsave(
#   filename = paste0("Model Results/", "LPW", "_ageread_bias_", timestamp, ".png"),  # Desired filename
#   plot = combined_ba_plot,                       # The combined plot object
#   width = 15,                                    # Width in inches (adjust as needed for 3 plots)
#   height = 5,                                    # Height in inches
#   units = "in",                                  # Specify units for width/height
#   dpi = 600                                      # High resolution for publication (300-600 is standard)
# )

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Figure: MODEL PERFORMANCE ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
all_results_means <- readRDS("Chapter 1/Newest Run/data/LPW_SUMMARY_all_models_2025-11-17.RDS")

# all_results_means <- readRDS("Chapter 1/Run_10_31_2025/LPW_SUMMARY_all_models_2025-10-31.RDS")
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

create_boxplot <- function(data, y_var, y_lab, show_x_axis = FALSE) {
  # Quoting the variable for robust NSE handling
  y_var_q <- rlang::enquo(y_var)
  
  # The main aesthetic mapping for the plot (used by geom_boxplot)
  boxplot_aes <- aes(x = Model, y = !!y_var_q, fill = ModelType)
  
  # Custom function to extract outliers (the 'y' values geom_boxplot calculates)
  # This uses the same logic as geom_boxplot to determine outliers
  get_outliers <- function(x) {
    if (length(x) == 0) return(NULL)
    stats <- as.numeric(boxplot.stats(x)$out)
    return(stats)
  }
  
  p <- ggplot(data, boxplot_aes) +
    # 1. Boxplot with black outline and NO default outliers
    geom_boxplot(
      outlier.shape = NA,
      color = "black",
      width = 0.5,
      alpha = 0.8
    ) +
    # 2. Add jittered points for outliers using stat_summary
    stat_summary(
      fun.data = function(x) {
        # Create a data frame for the outliers only
        data.frame(y = get_outliers(x))
      },
      # The geom to draw the outliers as points
      geom = "point", 
      # Apply jitter to the points
      position = position_jitter(width = 0.075, height = 0),
      alpha = 0.7,
      size = 1.3,
      color = "black", 
      show.legend = FALSE
    ) +
    # ... rest of the code ...
    scale_fill_manual(values = color_palette) +
    scale_x_discrete(
      labels = function(x)
        str_replace(x, " \\(", "\n(")
    ) +
    labs(y = y_lab, x = NULL) +
    theme_bw(base_size = 15) +
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
  geom_boxplot() + 
  scale_fill_manual(values = color_palette, name = "Model Type") + 
  theme(
    legend.position = "bottom",
    # Set a negative top margin to pull the legend box closer to the plot panel
    # The value '-5.5' is an example; adjust as needed.
    legend.margin = margin(t = -12, unit = "pt"),
    strip.background = element_rect(fill = "gray90"), 
    legend.text = element_text(size = 15.5), # Adjust legend item text size
    legend.title = element_text(size = 15.5) # Adjust legend title size
  )

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
# ggsave(
#   filename = paste0("Model Results/", "LPW", "_model_comparison_boxplots_jittered_", timestamp, ".png"),
#   plot = composite_plot,
#   width = 12,        # Width in inches
#   height = 8,        # Height in inches
#   dpi = 600,         # 600 DPI is publication quality
#   bg = "white"       # White background
# )

# unjittered

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
    theme_bw(base_size = 15) +
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
  geom_boxplot() + 
  scale_fill_manual(values = color_palette, name = "Model Type") + 
  theme(
    legend.position = "bottom",
    # Set a negative top margin to pull the legend box closer to the plot panel
    # The value '-5.5' is an example; adjust as needed.
    legend.margin = margin(t = -12, unit = "pt"),
    strip.background = element_rect(fill = "gray90"), 
    legend.text = element_text(size = 15.5), # Adjust legend item text size
    legend.title = element_text(size = 15.5) # Adjust legend title size
  )
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
# ggsave(
#   filename = paste0("Model Results/", "LPW", "_model_comparison_boxplots_unjittered_", timestamp, ".png"),
#   plot = composite_plot,
#   width = 12,        # Width in inches
#   height = 8,        # Height in inches
#   dpi = 600,         # 600 DPI is publication quality
#   bg = "white"       # White background
# )



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Figure: IMPORTANCE DATA ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


# Extract the component number from the best model strings (needed for identifying the original PCA methods)
lm_comp <- sub("Linear ", "", best_lm) 
gam_comp <- sub("GAM ", "", best_gam) 

# Define the exact original method names to select
best_lm_method <- paste0("PCA-LM", lm_comp)
best_gam_method <- paste0("PCA-GAM", gam_comp)

# Updated method order for consistency - ONLY FOUR METHODS
final_method_order_importance <- c(
  "PCA Loadings (LM)",
  "PCA Loadings (GAM)",
  "VIP Score (PLS)",
  "Gain (XGB)",
  "Permutation (RF)"
)

final_importance_data <- readRDS("Chapter 1/Newest Run/data/LPW_IMPORTANCE_all_models_2025-11-17.RDS")
# final_importance_data <- readRDS("Chapter 1/Run_10_31_2025/LPW_IMPORTANCE_all_models_2025-10-31.RDS")

importance_summary <- final_importance_data %>%
  mutate(
    method = case_when(
      method == best_lm_method  ~ "PCA Loadings (LM)",
      method == best_gam_method ~ "PCA Loadings (GAM)",
      method == "PLS-VIP" ~ "VIP Score (PLS)",
      method == "XGBoost" ~ "Gain (XGB)",
      method == "Random Forest" ~ "Permutation (RF)",
      TRUE ~ as.character(method)
    )
  ) %>%
  # Filter only the four required methods
  filter(method %in% final_method_order_importance) %>%
  group_by(method, wavenumber) %>%
  summarise(
    # 1. Calculates the grand mean (mean of the 500 'mean_importance' values)
    final_mean = mean(mean_importance, na.rm = TRUE),
    lower_bound = min(mean_importance, na.rm = TRUE),
    upper_bound = max(mean_importance, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Convert to factor for plot ordering
  mutate(method = factor(method, levels = final_method_order_importance))

# ----------------------------------------------------------- #
## Data Extraction for Plotting #
# ----------------------------------------------------------- #

# Extract data for the four final methods
lm_pca_data <- filter(importance_summary, method == "PCA Loadings (LM)")
gam_pca_data <- filter(importance_summary, method == "PCA Loadings (GAM)")
pls_data <- filter(importance_summary, method == "VIP Score (PLS)")
xgb_data <- filter(importance_summary, method == "Gain (XGB)")
rf_data  <- filter(importance_summary, method == "Permutation (RF)")

# ----------------------------------------------------------- #
# Plot Generation (Five Plots) 
# ----------------------------------------------------------- #

# --- NEW: p1: PCA Loadings (LM) ---
p1_lm <- ggplot(lm_pca_data, aes(x = wavenumber)) +
  geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound), fill = "#4477AA", alpha = 0.5) +
  geom_line(aes(y = final_mean), color = "#4477AA") +
  labs(y = "PCA Loadings\n(LM)", x = NULL) +
  theme_bw(base_size = 15) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(angle = 90, vjust = 0.5))

# --- NEW: p2: PCA Loadings (GAM) ---
p2_gam <- ggplot(gam_pca_data, aes(x = wavenumber)) +
  geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound), fill = "#4477AA", alpha = 0.5) +
  geom_line(aes(y = final_mean), color = "#4477AA") +
  labs(y = "PCA Loadings\n(GAM)", x = NULL) +
  theme_bw(base_size = 15) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(angle = 90, vjust = 0.5))

# --- MODIFIED: p3: VIP Score (PLS) ---
p3_pls <- ggplot(pls_data, aes(x = wavenumber)) +
  geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound), fill = "#AA3377", alpha = 0.5) +
  geom_line(aes(y = final_mean), color = "#AA3377") +
  labs(y = "VIP Score\n(PLS)", x = NULL) +
  theme_bw(base_size = 15) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(angle = 90, vjust = 0.5))

# --- MODIFIED: p4: Gain (XGB) ---
p4_xgb <- ggplot(xgb_data, aes(x = wavenumber)) +
  geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound), fill = "#228833", alpha = 0.5) +
  geom_line(aes(y = final_mean), color = "#228833") +
  labs(y = "Gain\n(XGB)", x = NULL) +
  theme_bw(base_size = 15) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(angle = 90, vjust = 0.5))

# --- MODIFIED: p5: Permutation (RF) - includes the x-axis label ---
p5_rf <- ggplot(rf_data, aes(x = wavenumber)) +
  geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound), fill = "#228833", alpha = 0.5) +
  geom_line(aes(y = final_mean), color = "#228833") +
  labs(y = "Permutation\n(RF)", x = expression(paste("Wavenumber (cm"^{-1}, ")"))) +
  theme_bw(base_size = 15) +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5))

# ----------------------------------------------------------- #
# Combine and Display #
# ----------------------------------------------------------- #

# --- MODIFIED: Combine the five plots vertically ---
final_plot <- (p1_lm / p2_gam / p3_pls / p4_xgb / p5_rf) & 
  scale_x_reverse(expand = expansion(mult = 0, add = 50))

final_plot
# 
# ggsave(
#   filename = paste0("Model Results/", "LPW", "_waveimportance_range_", timestamp, ".png"),
#   plot = final_plot,
#   width = 10,
#   height = 12, # <-- Increased height for 5 plots
#   dpi = 600
# )
# 
# 









## ALTERNATIVE: Overlay of spectra ####
scan_proc_long <- df %>%
  filter(!is.na(read_age)) %>%
  pivot_longer(
    cols = -c(1:20),
    names_to = "wavenumber",
    values_to = "absorbance"
  ) %>%
  mutate(wavenumber = as.numeric(wavenumber))
specimen_48_proc_long <- scan_proc_long %>% filter(specimen == 48)

# Get the range of the preprocessed absorbance data for the secondary axis scale
# This is necessary to map the absorbance values (y) to the importance values (x)
ABS_MIN <- min(specimen_48_proc_long$absorbance, na.rm = TRUE)
ABS_MAX <- max(specimen_48_proc_long$absorbance, na.rm = TRUE)
ABS_RANGE <- ABS_MAX - ABS_MIN
ABS_CENTER <- (ABS_MAX + ABS_MIN) / 2

# We need a scaling function to map the Absorbance (A) to the Importance scale (I).
# Since the Importance scale is different for each plot, we will map A to a 
# normalized space (e.g., 0 to 1) and then map the Importance scale to the same space.

# --- 1. Define Universal Spectra Overlay Function ---

add_spectra_overlay <- function(plot_obj, importance_data) {
  # 1. Calculate the Importance Scale Range for the current plot
  I_MIN <- min(importance_data$final_mean, na.rm = TRUE)
  I_MAX <- max(importance_data$final_mean, na.rm = TRUE)
  I_RANGE <- I_MAX - I_MIN
  
  # If I_RANGE is near zero (flat line), default to a safe scale (e.g., I_MAX)
  if (I_RANGE < 1e-6) {
    I_RANGE <- I_MAX
  }
  
  # 2. Calculate the Scaling Factor (m_abs) and Center Offset (c_abs) 
  #    to map Absorbance (A) to the Importance Scale (I).
  #    We want the Absorbance line to fit within the Importance plot's Y range.
  #    Let's aim to have the Absorbance line cover ~80% of the Importance Y range.
  
  # Target Absorbance Range on the Importance Scale (I_MAX - I_MIN) * 0.8
  TARGET_A_RANGE_ON_I <- I_RANGE * 0.8
  
  # Scaling factor: maps the Absorbance Range to the Target Importance Range
  m_abs <- TARGET_A_RANGE_ON_I / ABS_RANGE 
  
  # Center Offset: centers the Absorbance line on the Importance Center
  I_CENTER <- (I_MAX + I_MIN) / 2
  A_CENTER <- (ABS_MAX + ABS_MIN) / 2
  
  c_abs <- I_CENTER - (m_abs * A_CENTER)
  
  # 3. Modify the original plot object
  plot_obj + 
    # Add the spectra line using the calculated scaling
    geom_line(
      data = specimen_48_proc_long,
      aes(y = absorbance * m_abs + c_abs),
      color = "grey", 
      alpha = .6, 
      linewidth = 0.7
    ) +
    # Add the secondary Y-axis, but hide all its elements
    scale_y_continuous(
      # The primary scale is for importance (from original plot)
      name = plot_obj$labels$y,
      # Secondary axis transformation (maps Importance back to Absorbance)
      sec.axis = sec_axis(
        trans = ~ (. - c_abs) / m_abs, 
        name = "Preprocessed Absorbance (Hidden)",
        breaks = NULL # Hide ticks
      )
    ) +
    theme_bw(base_size = 15) + 
    # Theme adjustments to explicitly hide the secondary axis title and ticks
    theme(
      axis.title.y.right = element_blank(), # Hide secondary axis title
      axis.text.y.right = element_blank(),  # Hide secondary axis labels
      axis.ticks.y.right = element_blank()  # Hide secondary axis ticks
    )
}

# --- 2. Create the Final Overlaid Plots ---

p1_lm_final <- add_spectra_overlay(p1_lm, lm_pca_data) + theme(
  axis.title.x = element_blank(), 
  axis.text.x = element_blank(), 
  axis.ticks.x = element_blank(),
  axis.line.x = element_blank(), 
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank()
)
p2_gam_final <- add_spectra_overlay(p2_gam, gam_pca_data) + theme(
  axis.title.x = element_blank(), 
  axis.text.x = element_blank(), 
  axis.ticks.x = element_blank(),
  axis.line.x = element_blank(), 
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank()
)
p3_pls_final <- add_spectra_overlay(p3_pls, pls_data) + theme(
  axis.title.x = element_blank(), 
  axis.text.x = element_blank(), 
  axis.ticks.x = element_blank(),
  axis.line.x = element_blank(), 
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank()
)
p4_xgb_final <- add_spectra_overlay(p4_xgb, xgb_data) + theme(
  axis.title.x = element_blank(), 
  axis.text.x = element_blank(), 
  axis.ticks.x = element_blank(),
  axis.line.x = element_blank(), 
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank()
)
p5_rf_final <- add_spectra_overlay(p5_rf, rf_data) + theme(
  axis.title.x = element_text(size = 15), 
  axis.text.x = element_text(size = 15), 
  axis.ticks.x = element_line(),         
  axis.line.x = element_line(),          
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank()
)

# --- 3. Combine and Display ---

final_combined_plot_outline <- (p1_lm_final / p2_gam_final / p3_pls_final / p4_xgb_final / p5_rf_final) & 
  scale_x_reverse(expand = expansion(mult = 0, add = 50)) 

print(final_combined_plot_outline)

# ggsave(
#   filename = paste0("Model Results/", "LPW", "_waveimportance_overlay_", timestamp, ".png"),
#   plot = final_combined_plot_outline,
#   width = 10,
#   height = 12, # <-- Increased height for 5 plots
#   dpi = 600
# )

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Figure: Age Predictions ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

all_predictions <- readRDS("Chapter 1/Newest Run/data/LPW_PREDICTIONS_all_models_2025-11-17.RDS")
# all_predictions <- readRDS("Chapter 1/Run_10_31_2025/LPW_PREDICTIONS_all_models_2025-10-31.RDS")


# Define the required constants (assuming they are correctly sourced from elsewhere)
final_model_order <- c("LM",
                       "GAM",
                       "PLS",
                       "PLS (VIP)",
                       "XGB",
                       "RF",
                       "LM (Simple)",
                       "GAM (Simple)")

# 1. Define the complete list of model_variant names you want to keep
# FIX: Use the confirmed model variant string "LM 6" instead of the incorrect "Linear 6"

required_variants <- c(
  # Best PCA Models
  "LM 3",
  "GAM 9",
  # Other Models
  "PLS",
  "PLS-VIP",
  "RF",
  "XGBoost",
  "Simple lm",
  "Simple gam"
)

# Filter the main predictions dataframe and clean up model names
predictions_for_plot <- all_predictions %>%
  # FIX: Filter using the correct variant list, including "LM 6"
  filter(model_variant %in% required_variants) %>%
  mutate(
    Model = case_when(
      # FIX: Map the confirmed best LM variant "LM 6" to the final name "LM"
      model_variant == "LM 3" ~ "LM",
      model_variant == "GAM 9" ~ "GAM",
      
      # Map PLS Models
      model_variant == "PLS"                                      ~ "PLS",
      model_variant == "PLS-VIP"                                   ~ "PLS (VIP)",
      
      # Map ML Models
      model_variant == "XGBoost"                                   ~ "XGB",
      model_variant == "RF"                                        ~ "RF",
      
      # Map Simple Models (with correct capitalization)
      model_variant == "Simple lm"                                 ~ "LM (Simple)",
      model_variant == "Simple gam"                                ~ "GAM (Simple)",
      
      TRUE                                                         ~ NA_character_
    ),
    ModelType = case_when(
      Model %in% c("LM", "GAM")                   ~ "PCA",
      Model %in% c("PLS", "PLS (VIP)")            ~ "PLS",
      Model %in% c("XGB", "RF")                   ~ "ML",
      Model %in% c("LM (Simple)", "GAM (Simple)") ~ "Simple",
      TRUE                                        ~ NA_character_
    )
  ) %>%
  filter(!is.na(Model)) %>%
  mutate(Model = factor(Model, levels = final_model_order))

# Calculate the average prediction and residual for each fish (unchanged)
avg_predictions <- predictions_for_plot %>%
  group_by(specimen_number, Model, ModelType, actual) %>%
  summarise(avg_predicted_age = median(predicted, na.rm = TRUE), .groups = 'drop') %>%
  mutate(residual = avg_predicted_age - actual)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# 3. Create the Final Plot ----#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


# 1:1 line for predictions
preds_1_1 <- ggplot(avg_predictions, aes(x = actual, y = avg_predicted_age)) +
  geom_abline(slope = 1, linetype = "dashed", color = "black") +
  geom_point(aes(color = ModelType), alpha = 0.75, size = 1.5) +
  # geom_smooth(method = "lm", se = T, color = "#002244", linewidth = 1, alpha = 0.4) +
  scale_color_manual(values = color_palette, name = "Model Type") +
  # This will now display all 8 models, including LM
  facet_wrap(~ Model, ncol = 3) + 
  labs(
    x = "Thin Section Age (Days)",
    y = "Predicted Age (Days)"
  ) +
  theme_bw(base_size = 18) +
  theme(
    legend.position = "bottom",
    # Set a negative top margin to pull the legend box closer to the plot panel
    # The value '-5.5' is an example; adjust as needed.
    legend.margin = margin(t = -5.5, unit = "pt"),
    strip.background = element_rect(fill = "gray90")
  )


preds_1_1

ggsave(
  filename = paste0("Model Results/", "LPW", "_age_predictions_1_1_line", timestamp, ".png"), # Desired file name
  plot = preds_1_1,                              # The plot object to save
  width = 8,                                     # Width in inches (adjust as needed)
  height = 8,                                    # Height in inches (adjust as needed for 4 vertical plots)
  dpi = 600                                      # High resolution (300 is standard for print)
)





final_plot <- ggplot(avg_predictions, aes(x = actual, y = residual)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_point(aes(color = ModelType), alpha = 0.75, size = 1.5) +
  # geom_smooth(method = "lm", se = T, color = "#002244", linewidth = 1, alpha = 0.4) +
  scale_color_manual(values = color_palette, name = "Model Type") +
  # This will now display all 8 models, including LM
  facet_wrap(~ Model, ncol = 3) + 
  labs(
    x = "Actual Age (Days)",
    y = "Prediction Error (Predicted - Actual)"
  ) +
  theme_bw(base_size = 18) +
  theme(
    legend.position = "bottom",
    # Set a negative top margin to pull the legend box closer to the plot panel
    # The value '-5.5' is an example; adjust as needed.
    legend.margin = margin(t = -5.5, unit = "pt"),
    strip.background = element_rect(fill = "gray90")
  )


final_plot

# ggsave(
#   filename = paste0("Model Results/", "LPW", "_age_predictions", timestamp, ".png"), # Desired file name
#   plot = final_plot,                              # The plot object to save
#   width = 8,                                     # Width in inches (adjust as needed)
#   height = 8,                                    # Height in inches (adjust as needed for 4 vertical plots)
#   dpi = 600                                      # High resolution (300 is standard for print)
# )
# 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Figure: Hatch ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


df <- readRDS("RDS_dataframes/LPW_scan_avg_proc.RDS")
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
      # The best PCA models (LM/GAM X)
      model_variant == best_lm_in_preds  ~ "LM",
      model_variant == best_gam_in_preds  ~ "GAM",
      
      # PLS Models
      model_variant == "PLS"              ~ "PLS",        # <- Includes the bare PLS model
      model_variant == "PLS-VIP"          ~ "PLS (VIP)",  # <- Includes the PLS-VIP model
      
      # ML Models
      model_variant == "XGBoost"          ~ "XGB",
      model_variant == "RF"               ~ "RF",
      
      # Simple Models (FIXED CAPITALIZATION to match unique(all_predictions$model_variant))
      model_variant == "Simple lm"        ~ "LM (Simple)",
      model_variant == "Simple gam"       ~ "GAM (Simple)",
      
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Model)) %>%
  # Filtered data now only contains models that exist AND were mapped above.
  # The 'PLS' factor level is now actually populated, but we'll stick with the 
  # robust solution for KS test calculation anyway.
  mutate(Model = factor(Model, levels = final_model_order))

original_data <- df %>%
  select(specimen_number = specimen, original_hatch_date = hatch_date)

# --- MODIFICATION 1: Extract p-value along with D-statistic ---
ks_results <- map_dfr(levels(hatch_estimates_cleaned$Model), ~{
  estimates_subset <- hatch_estimates_cleaned %>% filter(Model == .x)
  originals_subset <- original_data %>% filter(specimen_number %in% estimates_subset$specimen_number)
  ks_test <- ks.test(estimates_subset$median_hatch, originals_subset$original_hatch_date)
  tibble(
    Model = .x,
    D_statistic = ks_test$statistic,
    p_value = ks_test$p.value # <-- ADD THIS LINE
  )
})

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
hatch <- ggplot(ecdf_differences, aes(x = hatch_date, y = ecdf_difference)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_step(aes(color = ModelType, group = Model), linewidth = .9) +
  geom_text(aes(label = ks_label), x = -Inf, y = Inf, hjust = -0.1, vjust = 1.2, # Adjusted vjust for two lines
            size = 4, check_overlap = TRUE, lineheight = .9) +
  scale_color_manual(values = color_palette, name = "Model Type") +
  facet_wrap(~ Model, ncol = 4) +
  labs(
    # title = "ECDF Difference (Estimate - Original) of Hatch Dates",
    # subtitle = "Deviation from the zero-line indicates model error. D and p-values from K-S test shown.",
    x = "Hatch Date",
    y = "ECDF Difference"
  ) +
  theme_bw(base_size = 18) +
  theme(
    legend.position = "bottom",
    # Set a negative top margin to pull the legend box closer to the plot panel
    # The value '-5.5' is an example; adjust as needed.
    legend.margin = margin(t = -5.5, unit = "pt"),
    strip.background = element_rect(fill = "gray90")
  )



hatch
# 
# ggsave(
#   filename = paste0("Model Results/", "LPW", "_hatch_comparison_", timestamp, ".png"),
#   plot = hatch,
#   width = 12,        # Width in inches
#   height = 8,        # Height in inches
#   dpi = 600,         # 600 DPI is publication quality
#   bg = "white"       # White background
# )
# 


# --- Clean up environment ---
rm(hatch_estimates, hatch_estimates_cleaned, original_data, ks_results, 
   original_dates, calculate_ecdf_diff, model_type_lookup, ecdf_differences,
   best_lm_in_preds, best_gam_in_preds)




# --- START: New Hatch Date Density Comparison Plot ---
# message("\n--- Generating Density Plot for 'True Hatch Date' vs 'PLS (VIP)' ---")
# 
# # 1. Get the "True" Hatch Dates from the main dataframe
# #    We use 'original_data' which was already created for the KS test
# true_hatch_dates_df <- original_data %>%
#   select(hatch_date = original_hatch_date) %>%
#   filter(!is.na(hatch_date)) %>%
#   mutate(Source = "Thin-Section")
# 
# # 2. Get the median *predicted hatch dates* for the "PLS-VIP" model
# #    We use 'hatch_estimates_cleaned' which has the median_hatch
# pls_vip_hatch_preds <- hatch_estimates_cleaned %>%
#   filter(Model == "PLS (VIP)") %>%
#   select(hatch_date = median_hatch) %>%
#   mutate(Source = "FT-NIRS Prediction")
# 
# # 3. Combine the two dataframes
# hatch_date_data <- bind_rows(true_hatch_dates_df, pls_vip_hatch_preds) %>%
#   mutate(Source = factor(Source, levels = c("Thin-Section", "FT-NIRS Prediction")))
# 
# # 4. --- NEW: Create custom date breaks for 1st and 15th ---
# # Get the date range from the data
# # This assumes hatch_date_data$hatch_date is numeric (days since epoch)
# min_date_numeric <- min(hatch_date_data$hatch_date, na.rm = TRUE)
# max_date_numeric <- max(hatch_date_data$hatch_date, na.rm = TRUE)
# 
# # --- FIX: Convert numeric dates back to Date objects for lubridate ---
# # The error occurs because floor_date() needs a Date object, not a number.
# # We assume the numeric is days since the "1970-01-01" origin.
# min_date <- as.Date(min_date_numeric, origin = "1970-01-01")
# max_date <- as.Date(max_date_numeric, origin = "1970-01-01")
# 
# # Create a sequence of all days in the full month range
# all_days <- seq(from = floor_date(min_date, "month"), 
#                 to = ceiling_date(max_date, "month"), 
#                 by = "day")
# 
# # Filter for days that are 1 or 15
# custom_date_breaks <- all_days[day(all_days) == 1 | day(all_days) == 15]
# 
# # --- FIX: Filter to START from the beginning of the month ---
# # This was filtering from the *actual* min date (e.g., Mar 7)
# # Now it filters from the *start of the month* (e.g., Mar 1)
# custom_date_breaks <- custom_date_breaks[custom_date_breaks >= floor_date(min_date, "month") & 
#                                            custom_date_breaks <= ceiling_date(max_date, "month")]
# 
# # If no breaks are found (e.g., very short range), add at least the min and max
# if (length(custom_date_breaks) == 0) {
#   custom_date_breaks <- c(min_date, max_date)
# }
# 
# 
# # 5. Create the density plot (was step 4)
# hatch_date_comparison_plot <- ggplot(hatch_date_data, aes(x = hatch_date, fill = Source)) +
#   geom_density(alpha = 0.6, position = "identity") +
#   
#   # Format the x-axis to show dates (e.g., "Mar 01")
#   # --- MODIFIED: Use custom date breaks and add limits ---
#   scale_x_date(
#     labels = scales::date_format("%b %d"), 
#     breaks = custom_date_breaks,
#     # Force the axis limits to match the breaks
#     limits = c(min(custom_date_breaks), max(custom_date_breaks)) 
#   ) +
#   
#   scale_fill_manual(values = c("Thin-Section" = "#ca0020", "FT-NIRS Prediction" = "#6ba2c2")) +
#   labs(
#     # title = "Distribution of True vs. PLS (VIP) Estimated Hatch Dates", # Title removed as requested
#     x = "Hatch Date",
#     y = "",
#     fill = "Age Source"
#   ) +
#   theme_bw(base_size = 30) +
#   theme(
#     legend.position = "bottom",
#     # Pull legend closer to x-axis
#     legend.margin = margin(t = -15, unit = "pt"), 
#     # Remove Y-axis text and ticks
#     axis.text.y = element_blank(),
#     axis.ticks.y = element_blank(),
#     # Increase X-axis text size and rotate
#     axis.text.x = element_text(angle = 45, hjust = 1, size = 30) 
#   )
# 
# print(hatch_date_comparison_plot)

# 6. Save the plot (was step 5)
# ggsave(
#   filename = paste0("LPW", "_hatch_date_density_comparison_", timestamp, ".png"),
#   plot = hatch_date_comparison_plot,
#   width = 10,
#   height = 7,
#   dpi = 600,
#   bg = "white"
# )

message("\n✅ Hatch date density comparison plot saved.")


# --- END: New Hatch Date Density Comparison Plot ---



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Tables: MODEL PERFORMANCE ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

## Colored - Full ####

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
  arrange(Model)

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

# table_to_export_full %>%
#   gtsave("results_all.html")


## Colored - Filtered ####


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

# --- 5. Create the gt object ---
filtered_table_final <- table_data_filtered %>%
  select(-ModelType) %>%
  gt() %>%
  data_color(columns = RMSE, palette = c("#63BE7B", "#FFEB84", "#F8696B")) %>%
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

# # Save the final table
# filtered_table_final %>%
#   gtsave("results_filtered.html")

## B/W - Full & Filtered ####

# Create the UNCOLORED gt object
table_to_export_full_bw <- table_data_full %>%
  select(-ModelType) %>%
  gt() %>%
  # Apply a neutral/white background to the Model column where colors were before
  tab_style(
    style = cell_fill(color = "white"),
    locations = cells_body(columns = Model)
  ) %>%
  # Use default number formatting (no heatmap coloring)
  fmt_number(columns = c(RMSE, R2, Bias, RPD), decimals = 3) %>%
  # Apply border options
  tab_options(
    column_labels.border.bottom.width = px(2),
    column_labels.border.bottom.color = "black",
    table_body.hlines.color = "#ededed"
  )
table_to_export_full_bw

# --- 2. UNCOLORED Filtered Table (equivalent to filtered_table_final) ---

# Create the UNCOLORED gt object
filtered_table_final_bw <- table_data_filtered %>%
  select(-ModelType) %>%
  gt() %>%
  # Apply a neutral/white background to the Model column where colors were before
  tab_style(
    style = cell_fill(color = "white"),
    locations = cells_body(columns = Model)
  ) %>%
  # Use default number formatting (no heatmap coloring)
  fmt_number(columns = c(RMSE, R2, Bias, RPD), decimals = 3) %>%
  # Apply border options
  tab_options(
    column_labels.border.bottom.width = px(2),
    column_labels.border.bottom.color = "black",
    table_body.hlines.color = "#ededed"
  )

filtered_table_final_bw

# You can save these B/W tables using gtsave:
gtsave(table_to_export_full_bw, paste0("Model Results/", "LPW", "_results_all_bw_", timestamp, ".html"))
gtsave(filtered_table_final_bw, paste0("Model Results/", "LPW", "_results_filtered_bw_", timestamp, ".html"))



# PLS COMPONENTS USED:
temp <- all_results_means %>% filter(Model == "PLS - VIP")
range(temp$Min_Components)
range(temp$Max_Components)



# AGE BIAS

agereads <- read.csv("Chapter 1/age_bias_esther.csv")
names(agereads)







library(readxl)
library(tidyverse)
library(FSA)
library(lubridate) # Helps with date handling

library(FSA)
library(dplyr)

# 1. Calculate Row Means & Filter
# We assume your dataframe is named 'agereads'
data_prep <- agereads %>%
  rowwise() %>%
  mutate(
    # Calculate mean of your 3 potential reads (ignoring NAs)
    My_Mean = mean(c(y.age1, y.age2, y.age3), na.rm = TRUE),
    
    # Calculate mean of expert's potential reads
    Exp_Mean = mean(c(x.age1, x.age2, x.age3), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  # Filter: Keep only specimens where BOTH you and expert have an age
  filter(!is.nan(My_Mean) & !is.nan(Exp_Mean))




# 1. Create the ageBias object
# Formula: NonReference (You) ~ Reference (Expert)
ab_model <- ageBias(My_Mean ~ Exp_Mean, 
                    data = data_prep,
                    ref.lab = "Experienced Reader Age (days)",
                    nref.lab = "First Author Age (days)")

# 2. Plot using the Campana style
# 'col.CIsig = "red"' makes the error bar red if you are significantly biased
# 'show.n = TRUE' puts the sample size number above the x-axis
plotAB(ab_model,
       what = "Campana",
       col.CIsig = "red",
       pch.mean.sig = 21, # Open circle for biased means (standard convention)
       show.n = F)

# Calculate precision across ALL columns provided in the formula
# This will include your replicates and the expert's replicates
ap_model <- agePrecision(~ y.age1 + y.age2 + y.age3 + x.age1 + x.age2 + x.age3, 
                         data = data_prep)

# View the summary stats
# Look for 'ACV' (Average Coefficient of Variation)
summary(ap_model, what = "precision")

