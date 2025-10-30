# Load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(prospectr)
library(patchwork) # For combining plots
library(viridis)

dfmeta_LPW <- readRDS("RDS_dataframes/LPW_dfmeta.RDS") # RAW FT-NIRS, all scans, no preprocess yet 

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
  geom_line(alpha = 0.7) +
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
  geom_line(alpha = 0.7) +
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
    panel.grid.minor = element_blank()
  )

# --- 4. Combine Plots with Patchwork ---

# Use the patchwork package to combine plots and add a shared legend
final_plot <- (p1 / p2) + plot_layout(guides = 'collect')

# Display the final plot
final_plot


# Load the ggplot2 library if it's not already loaded
library(ggplot2)

# --- Save the plot as a TIFF file (Recommended for publication) ---
# The ggsave function saves the last plot that was displayed by default.
# We specify the plot object explicitly for clarity.

ggsave(
  filename = "Raw_vs_Preprocessed_Spectra.tiff", # The name of the file
  plot = final_plot,                           # The plot object to save
  device = "tiff",                             # The file format
  width = 7,                                   # Width of the plot in inches
  height = 6,                                  # Height of the plot in inches
  dpi = 300,                                   # Resolution in Dots Per Inch
  compression = "lzw"                          # A good lossless compression
)


# --- Alternative: Save as a PNG file ---
# PNG is also a great option, widely used and good quality.
ggsave(
  filename = "Raw_vs_Preprocessed_Spectra.png",
  plot = final_plot,
  device = "png",
  width = 7,
  height = 6,
  dpi = 300
)






##### PCA FOR OUTLIERS ######
# Load necessary libraries
library(mdatools)
library(ggplot2)
library(viridis)
library(ggrepel)
library(dplyr)

# --- 1. Prepare and Clean the Data (No Changes) ---
# Assumes 'scan_avg_proc' is in your environment
pca_data_with_na <- scan_avg_proc %>%
  filter(!is.na(read_age))

metadata <- pca_data_with_na[, 1:20]
spectral_data <- pca_data_with_na[, 21:ncol(pca_data_with_na)]

spectral_data_cleaned <- spectral_data %>%
  select(where(~ !any(is.na(.))))

# --- 2. Run PCA (No Changes) ---
# This step was successful for you
pca_results <- pca(spectral_data_cleaned, scale = TRUE)

# --- 3. Create Dataframe for Plotting [FINAL CORRECTION] ---
# We now use the correct path to the scores matrix and column indices
scores_df <- data.frame(
  PC1 = pca_results$calres$scores[, 1],  # First column of the scores
  PC2 = pca_results$calres$scores[, 2],  # Second column of the scores
  specimen = metadata$specimen,
  read_age = metadata$read_age
)

# Use the correct path for explained variance
pc1_variance <- round(pca_results$calres$expvar[1], 1)
pc2_variance <- round(pca_results$calres$expvar[2], 1)


# --- 4. Generate the Plot (No Changes) ---
PCA_outliers <- ggplot(scores_df, aes(x = PC1, y = PC2)) +
  stat_ellipse(
    type = "norm", level = 0.95, geom = "polygon",
    alpha = 0.1, aes(fill = read_age)
  ) +
  geom_point(aes(color = read_age), size = 3, alpha = 0.8) +
  geom_text_repel(aes(label = specimen), size = 3, max.overlaps = 15) +
  scale_color_viridis(option = "D") +
  scale_fill_viridis(option = "D") +
  labs(
    title = "PCA of Pre-processed Spectra for Outlier Detection",
    subtitle = "Points are labeled by specimen ID and colored by age",
    x = paste0("PC1 (", pc1_variance, "% variance explained)"),
    y = paste0("PC2 (", pc2_variance, "% variance explained)"),
    color = "Age (days)",
    fill = "Age (days)"
  ) +
  theme_bw() +
  guides(fill = "none")

PCA_outliers

# --- Alternative: Save as a PNG file ---
# PNG is also a great option, widely used and good quality.
ggsave(
  filename = "PCA_outliers.png",
  plot = PCA_outliers,
  device = "png",
  width = 7,
  height = 6,
  dpi = 300
)


# average length = 

# 53 was large, 177 mm
# 77 also large, 182
# 74 wasn't particularly large; 107,


# Load necessary libraries
library(ggplot2)
library(dplyr)
library(viridis)
library(patchwork)

# --- 1. Define Outliers and Prepare Data ---
# This assumes 'scan_avg_long' and 'scan_proc_long' are already in your environment.

# Define the specimen IDs for the outliers
outlier_specimens <- c(53, 77, 74)

# Split the RAW data into normal and outlier groups
scan_avg_long_normal <- scan_avg_long %>% filter(!specimen %in% outlier_specimens)
scan_avg_long_outliers <- scan_avg_long %>% filter(specimen %in% outlier_specimens)

# Split the PROCESSED data into normal and outlier groups
scan_proc_long_normal <- scan_proc_long %>% filter(!specimen %in% outlier_specimens)
scan_proc_long_outliers <- scan_proc_long %>% filter(specimen %in% outlier_specimens)


# --- 2. Create the Updated Plots ---

# Top Plot: Raw Averaged Spectra with Outliers in Black
p1 <- ggplot() +
  # Layer 1: Normal data, colored by age
  geom_line(data = scan_avg_long_normal,
            aes(x = wavenumber, y = absorbance, group = specimen, color = read_age),
            alpha = 0.6) +
  # Layer 2: Outlier data, thicker and black
  geom_line(data = scan_avg_long_outliers,
            aes(x = wavenumber, y = absorbance, group = specimen),
            color = "red",
            linewidth = 0.9) +
  scale_x_reverse() +
  scale_color_viridis() +
  labs(y = "Raw Absorbance", color = "Age (days)") +
  theme_bw(base_size = 15) +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

# Bottom Plot: Pre-processed Spectra with Outliers in Black
p2 <- ggplot() +
  # Layer 1: Normal data, colored by age
  geom_line(data = scan_proc_long_normal,
            aes(x = wavenumber, y = absorbance, group = specimen, color = read_age),
            alpha = 0.6) +
  # Layer 2: Outlier data, thicker and black
  geom_line(data = scan_proc_long_outliers,
            aes(x = wavenumber, y = absorbance, group = specimen),
            color = "red",
            linewidth = 0.9) +
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
    panel.grid.minor = element_blank()
  )

# --- 3. Combine and Display the Final Plot ---
(p1 / p2) + plot_layout(guides = 'collect')






# This is the most important part. You must be transparent about this process in your methods section.
# 
# Create a subsection called something like "Data Filtering and Outlier Removal." In this section, you should state clearly:
#   
#   "Prior to model calibration, the pre-processed spectral data was examined for statistical outliers using Principal Component Analysis (PCA). Several specimens (e.g., 53, 56, 74, 77, 118) were identified as lying outside the 95% confidence ellipse of the main data cluster (see Fig. X). While some of these specimens corresponded to the largest fish in the dataset, others did not, suggesting their spectral profiles were unique for reasons other than size alone. To create a more robust and representative calibration model, these outlying specimens were removed from the dataset prior to analysis."
# 
# This statement does three crucial things:
#   
#   It states what you did (removed specific specimens).
# 
# It justifies why you did it (they were statistical outliers identified by PCA).
# 
# It shows you've thought critically about the result (acknowledging the mixed relationship with fish size).





# COMPARE OUTLIERS AND NOT
# --- 1. Setup: Load Libraries and Data ---
# Load necessary packages
library(dplyr)
library(mdatools)
library(ggplot2)
library(tidyr)
library(caret) # Explicitly load for createFolds

# Load your pre-processed data
df_full <- readRDS("RDS_dataframes/LPW_scan_avg_proc.RDS")

# --- 2. Data Preparation (No Changes) ---

df_with_outliers <- df_full %>%
  filter(!is.na(read_age))

specimens_to_remove <- c(53,74, 77)
df_without_outliers <- df_full %>%
  filter(!is.na(read_age)) %>%
  filter(!specimen %in% specimens_to_remove)

# --- 3. The Core Function: A SINGLE 10-Fold CV ---
# This function is now simplified to be non-parallel.

run_single_10_fold_cv <- function(dataset) {
  
  spectral_start_col <- 21
  
  cat("Processing dataset with", nrow(dataset), "samples...\n")
  
  # Create ONE set of 10 folds
  set.seed(123) # Use a fixed seed for reproducibility
  folds <- createFolds(dataset$read_age, k = 10, list = TRUE)
  
  # Store RMSE for each of the 10 folds
  fold_results <- list()
  
  # Use a standard for loop
  for (j in 1:10) {
    cat("  - Processing fold", j, "of 10\n")
    
    test_indices <- folds[[j]]
    calibrate_df <- dataset[-test_indices, ]
    testing_df <- dataset[test_indices, ]
    
    train_spectra <- calibrate_df[, spectral_start_col:ncol(calibrate_df)]
    train_spectra_cleaned <- train_spectra %>% select(where(~ !any(is.na(.))))
    clean_colnames <- colnames(train_spectra_cleaned)
    
    mod_vip <- mdatools::pls(
      train_spectra_cleaned, calibrate_df$read_age, cv = 1, scale = TRUE
    )
    
    vip_scores <- vipscores(mod_vip)
    cols_to_exclude <- vip_scores < 0.5
    
    mod_final <- mdatools::pls(
      train_spectra_cleaned, calibrate_df$read_age, scale = TRUE, cv = 1,
      x.test = testing_df[, clean_colnames], y.test = testing_df$read_age,
      exclcols = cols_to_exclude
    )
    
    ncomp <- mod_final$ncomp.selected
    # Important: Check if ncomp is valid before trying to access results
    if (length(ncomp) > 0 && !is.na(ncomp)) {
      fold_results[[j]] <- data.frame(Fold = j, RMSE = mod_final$testres$rmse[ncomp])
    } else {
      # Handle cases where no component is selected (can happen in rare cases)
      fold_results[[j]] <- data.frame(Fold = j, RMSE = NA)
      cat("    ! Warning: No component selected in fold", j, "\n")
    }
  }
  
  # Combine results and return
  return(bind_rows(fold_results))
}


# --- 4. Execution: Run the Simple Analysis ---

results_with_outliers <- run_single_10_fold_cv(df_with_outliers)
results_with_outliers$Group <- "Outliers Included"

results_without_outliers <- run_single_10_fold_cv(df_without_outliers)
results_without_outliers$Group <- "Outliers Excluded"


# --- 5. Comparison and Summary ---

comparison_results <- bind_rows(results_with_outliers, results_without_outliers)

# Generate a boxplot for visual comparison of the 10 folds
ggplot(comparison_results, aes(x = Group, y = RMSE, fill = Group)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.1, alpha = 0.5, height = 0) +
  scale_fill_manual(values = c("Outliers Included" = "#EE6677", "Outliers Excluded" = "#4477AA")) +
  labs(
    title = "PLS Model Performance: Outlier Impact",
    subtitle = "Results from a single 10-fold cross-validation",
    x = "Dataset",
    y = "RMSE (days)",
    fill = "Group"
  ) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none")

# Print a summary table
summary_table <- comparison_results %>%
  group_by(Group) %>%
  summarise(
    Average_RMSE = mean(RMSE, na.rm = TRUE),
    Std_Dev_RMSE = sd(RMSE, na.rm = TRUE)
  )

print(summary_table)


















#### PCA INCLUDING UNAGED


# Load necessary libraries
library(mdatools)
library(ggplot2)
library(viridis)
library(ggrepel)
library(dplyr)

# --- 1. Prepare and Clean ALL Spectral Data ---
# Assumes 'scan_avg_proc' is in your environment.
# KEY CHANGE: We are NOT filtering for read_age here.
# We use the entire dataframe to run the PCA.
all_pca_data <- scan_avg_proc

# Separate metadata from the full spectral data
all_metadata <- all_pca_data[, 1:20]
all_spectral_data <- all_pca_data[, 21:ncol(all_pca_data)]

# Clean NA columns from the Savitzky-Golay filter
all_spectral_data_cleaned <- all_spectral_data %>%
  select(where(~ !any(is.na(.))))

# --- 2. Run PCA on the Full Dataset ---
pca_results_all <- pca(all_spectral_data_cleaned, scale = TRUE)

# --- 3. Create Dataframe for Plotting ---
# This dataframe will include all 122 specimens.
# The 'read_age' column will have NAs for un-aged fish.
scores_df_all <- data.frame(
  PC1 = pca_results_all$calres$scores[, 1],
  PC2 = pca_results_all$calres$scores[, 2],
  specimen = all_metadata$specimen,
  read_age = all_metadata$read_age
)

# Create a separate dataframe JUST for the labels of aged fish
labels_df <- scores_df_all %>%
  filter(!is.na(read_age))

# Extract variance explained
pc1_variance_all <- round(pca_results_all$calres$expvar[1], 1)
pc2_variance_all <- round(pca_results_all$calres$expvar[2], 1)


# --- 4. Generate the Plot ---
# This plot will show all points, but only label the aged ones.
PCA_outliers_all_data <- ggplot(scores_df_all, aes(x = PC1, y = PC2)) +
  # Draw a single 95% confidence ellipse around ALL data points
  stat_ellipse(type = "norm", level = 0.95, geom = "polygon", alpha = 0.1, fill = "grey") +
  
  # Plot all points. Un-aged fish will be grey by default.
  geom_point(aes(color = read_age), size = 3, alpha = 0.7) +
  
  # KEY CHANGE: Use the 'labels_df' to only add text for aged fish
  geom_text_repel(data = labels_df, aes(label = specimen), size = 3.5, max.overlaps = 20) +
  
  # Use the viridis color scale (it handles NA values gracefully)
  scale_color_viridis(option = "D", na.value = "grey50") +
  
  labs(
    title = "PCA of All 122 Specimens for Outlier Detection",
    subtitle = "Only aged specimens are labeled. Un-aged specimens are shown in grey.",
    x = paste0("PC1 (", pc1_variance_all, "% variance explained)"),
    y = paste0("PC2 (", pc2_variance_all, "% variance explained)"),
    color = "Age (days)"
  ) +
  theme_bw(base_size = 14)

# Display the plot
PCA_outliers_all_data

# --- 5. Save the Plot ---
ggsave(
  filename = "PCA_outliers_all_data.png",
  plot = PCA_outliers_all_data,
  device = "png",
  width = 8,
  height = 7,
  dpi = 300
)



