# Load necessary libraries
library(mdatools)
library(ggplot2)
library(viridis)
library(ggrepel) # For non-overlapping text labels
library(dplyr)

# --- 1. Prepare the Data ---
# This assumes 'scan_avg_proc' from the previous step is in your environment.




# Filter out specimens without an age, as they can't be plotted by age.
# Also, select only the spectral data for the PCA.
pca_data <- scan_avg_proc %>%
  filter(!is.na(read_age))

# --- 2. Run PCA ---
# Run PCA on the processed spectral data columns, scaling the data.
# The spectral data starts at column 21.
pca_results <- pca(pca_data[, 21:ncol(pca_data)], scale = TRUE)

# --- 3. Create a Dataframe for Plotting ---
# Combine PC scores with the metadata needed for the plot (specimen ID and age).
scores_df <- as.data.frame(pca_results$scores) %>%
  mutate(
    specimen = pca_data$specimen,
    read_age = pca_data$read_age
  )

# Extract the variance explained by the first two PCs for axis labels
pc1_variance <- round(pca_results$calres$expvar[1], 1)
pc2_variance <- round(pca_results$calres$expvar[2], 1)

# --- 4. Generate the Plot ---
ggplot(scores_df, aes(x = PC1, y = PC2)) +
  # Add confidence ellipses around the data
  stat_ellipse(
    type = "norm",
    level = 0.95, # 95% confidence interval
    geom = "polygon",
    alpha = 0.1,
    aes(fill = read_age)
  ) +
  # Add the points, colored by age
  geom_point(aes(color = read_age), size = 3, alpha = 0.8) +
  # Add labels for each point (specimen ID)
  geom_text_repel(aes(label = specimen), size = 3, max.overlaps = 15) +
  # Use the viridis color scale
  scale_color_viridis(option = "D") +
  scale_fill_viridis(option = "D") + # For the ellipse fill
  # Add informative labels
  labs(
    title = "PCA of Pre-processed Spectra for Outlier Detection",
    subtitle = "Points are labeled by specimen ID and colored by age",
    x = paste0("PC1 (", pc1_variance, "% variance explained)"),
    y = paste0("PC2 (", pc2_variance, "% variance explained)"),
    color = "Age (days)",
    fill = "Age (days)"
  ) +
  theme_bw() +
  guides(fill = "none") # Remove the legend for the ellipse fill