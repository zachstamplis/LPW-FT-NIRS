# --- 0. Load Required Packages ---
# Make sure you have these installed first:
# install.packages(c("tidyverse", "zoo", "factoextra", "vegan", "pls"))

library(tidyverse)
library(zoo)        
library(factoextra) 
library(vegan)      
library(pls)        

# --- Take a snapshot of the environment before we start ---
initial_objects <- ls()

# --- 1. Data Preparation: Load, Classify, and Smooth ICP-MS Data ---

cat("--- Preparing ICP-MS Data ---\n")

# -- Load Raw Data --
icpms_raw_df <- readRDS("RDS_dataframes/ICP-MS_meta_09032025.RDS")
marked_cores <- read_csv("otolith_core_regions_2025-09-04.csv", 
                         col_types = cols(specimen = "c", transect = "c"))
ftnirs_raw_df <- readRDS('/Users/zachstamplis/Desktop/Thesis and Otoliths/Github/LPW_FT-NIRS/RDS_dataframes/IBM_proc_filter.RDS')
ftnirs_raw_df <- ftnirs_raw_df %>% filter(run_number == 2)
# -- Classify Otolith Regions --
df_classified <- icpms_raw_df %>% 
  mutate(Sr87_86 = Sr87_ppm / Sr86_ppm,
         transect = as.character(transect)) %>%
  filter(!specimen %in% c("oto05", "oto12", "oto14", "oto40")) %>%
  left_join(marked_cores, by = c("specimen", "transect"))

df_final <- df_classified %>%
  group_by(specimen, transect) %>%
  filter(!is.na(core_start_time), time >= core_start_time) %>%
  mutate(
    region = case_when(
      time >= (max(time, na.rm = TRUE) - 5) ~ "edge",
      time >= core_start_time & time <= core_end_time ~ "core",
      TRUE ~ "middle"
    ),
    region = factor(region, levels = c("core", "middle", "edge"))
  ) %>%
  ungroup() %>%
  select(-core_start_time, -core_end_time)

# -- Apply Smoothing --
smoothing_window_size <- 5 
element_columns <- df_final %>%
  select(where(is.numeric), -any_of(c("time", "grade", "specimen_number", "transect"))) %>%
  names()

df_smoothed <- df_final %>%
  group_by(specimen, transect) %>%
  arrange(time, .by_group = TRUE) %>% 
  mutate(
    across(
      .cols = all_of(element_columns),
      .fns = ~ zoo::rollmean(.x, k = smoothing_window_size, fill = NA, align = "center")
    )
  ) %>%
  na.omit() %>%
  ungroup()

# -- Calculate Final Averages --
specimen_region_averages_smoothed <- df_smoothed %>%
  group_by(specimen, specimen_number, area, region) %>%
  summarise(
    across(
      .cols = where(is.numeric) & !any_of(c("time", "grade", "specimen_number", "transect")),
      .fns = ~mean(.x, na.rm = TRUE)
    ),
    .groups = 'drop'
  )

# --- 2. Statistical Analysis: ANOVA for Regional and Zone Differences ---

cat("\n--- Performing ANOVA on Key Elements ---\n")

anova_data <- specimen_region_averages_smoothed %>%
  mutate(
    zone = case_when(
      grepl("Kodiak", area) ~ "Kodiak",
      grepl("Shumagins", area) ~ "Shumagins",
      TRUE ~ as.character(area)
    )
  )

elements_to_test <- c("Li7_ppm", "Mg24_ppm", "Mn55_ppm", "Sr88_ppm", "Ba138_ppm", "Sr87_86")

for (element in elements_to_test) {
  cat(paste("\n--- Two-Way ANOVA Results for:", element, "---\n"))
  formula <- as.formula(paste("`", element, "` ~ zone * region", sep = ""))
  anova_model <- aov(formula, data = anova_data)
  print(summary(anova_model))
}


# --- 3. FT-NIRS Integration & PCA ---

cat("\n--- Integrating with FT-NIRS Data and Performing PCA ---\n")

# -- Reshape Otolith Data for Joining --
# CORRECTED: Ensures 'area' and 'zone' are both kept for the join.
otolith_wide <- anova_data %>% 
  select(specimen_number, zone, area, region, all_of(elements_to_test)) %>%
  pivot_wider(
    id_cols = c(specimen_number, zone, area),
    names_from = region,
    values_from = all_of(elements_to_test),
    names_glue = "{.value}_{region}"
  )

# -- Join Datasets and Clean Up Columns --
# This now works because 'area' is correctly included in 'otolith_wide'
combined_df <- ftnirs_raw_df %>%
  rename(specimen_number = specimen) %>%
  inner_join(otolith_wide, by = "specimen_number") %>%
  # Your code to remove junk columns and reorder
  select(-c(haul, read_age:file_path)) %>% 
  select(specimen_number, length:region, zone, area, Li7_ppm_core:Sr87_86_edge, everything())

cat("ICP-MS and FT-NIRS data successfully combined.\n")

# -- Create a clean, complete dataset for analysis --
spectral_cols <- names(combined_df)[grepl("^[0-9.]+$", names(combined_df))]
non_zero_var_cols <- which(apply(combined_df[, spectral_cols], 2, var, na.rm = TRUE) > 1e-10)
spectral_cols_filtered <- names(non_zero_var_cols)

# -- Perform PCA and Create Plot (No Vectors) --
pca_data <- combined_df %>%
  select(specimen_number, zone, all_of(spectral_cols_filtered)) %>%
  na.omit()

pca_results <- prcomp(pca_data[, spectral_cols_filtered], scale. = TRUE)

pca_plot <- fviz_pca_ind(pca_results, 
                         habillage = pca_data$zone, 
                         addEllipses = TRUE, ellipse.level = 0.95,
                         ggtheme = theme_minimal(),
                         title = "PCA of FT-NIRS Spectra by Geographic Zone")

cat("PCA plot created. Printing plot...\n")
print(pca_plot)


cat("\n--- Building PLS Models for All Regional Elements ---\n")

# # Make sure you have mdatools installed first:
# # install.packages("mdatools")
# library(mdatools)
# 
# # Get the names of all the regional element columns to use as response variables
# element_cols_to_predict <- names(combined_df)[grepl("_core$|_middle$|_edge$", names(combined_df))]
# 
# # Get the names of the spectral data columns to use as predictors
# spectral_cols_filtered <- names(combined_df)[grepl("^[0-9.]+$", names(combined_df))]
# 
# # Create a directory to save the PLS plots
# output_dir_pls <- "PLS_model_plots"
# if (!dir.exists(output_dir_pls)) {
#   dir.create(output_dir_pls)
# }
# 
# # Loop through each element variable to build and plot a PLS model
# for (element in element_cols_to_predict) {
#   
#   cat(paste("\n--- Building PLS model for:", element, "---\n"))
#   
#   # Create a clean dataframe with no NAs for the current element and spectra
#   pls_data <- combined_df %>%
#     select(all_of(element), all_of(spectral_cols_filtered)) %>%
#     na.omit()
# 
#   # Define the predictors (X) and the response (y)
#   X <- pls_data %>% select(all_of(spectral_cols_filtered)) %>% as.matrix()
#   y <- pls_data[[element]]
# 
#   # Build the PLS model with cross-validation
#   # We'll test up to 15 components
#   model <- pls(X, y, ncomp = 15, cv = 10)
# 
#   # Find the optimal number of components based on RMSE
#   ncomp_optimal <- model$cvres$ncomp.selected
#   
#   cat(paste("Optimal number of components for", element, "is:", ncomp_optimal, "\n"))
# 
#   # Set the optimal number of components for the final model plots
#   model$ncomp.selected <- ncomp_optimal
# 
#   # --- Generate and Save the Diagnostic Plots ---
#   
#   # Define the filename for the plot
#   file_name <- file.path(output_dir_pls, paste0("pls_plot_", element, ".png"))
#   
#   # Open a PNG device to save the plot
#   png(filename = file_name, width = 10, height = 8, units = "in", res = 300)
#   
#   # Create the 4-in-1 plot from mdatools
#   plot(model, main = paste("PLS Model Performance for", element))
#   
#   # Close the PNG device
#   dev.off()
#   
#   cat("Saved PLS plot:", file_name, "\n")
# }
# 
# cat("\nPLS modeling complete. All plots have been saved to the '", output_dir_pls, "' folder. ✅\n")

# --- 4. Predictive Modeling: PLS Including Zone as a Predictor ---

cat("\n--- Building PLS Models for All Regional Elements with Zone as a Predictor ---\n")

# Make sure you have mdatools installed first:
# install.packages("mdatools")
library(mdatools)

# Get the names of all the regional element columns to use as response variables
element_cols_to_predict <- names(combined_df)[grepl("_core$|_middle$|_edge$", names(combined_df))]

# Get the names of the spectral data columns to use as predictors
spectral_cols_filtered <- names(combined_df)[grepl("^[0-9.]+$", names(combined_df))]

# Create a directory to save the PLS plots
output_dir_pls <- "PLS_model_plots_with_zone"
if (!dir.exists(output_dir_pls)) {
  dir.create(output_dir_pls)
}

# Loop through each element variable to build and plot a PLS model
for (element in element_cols_to_predict) {
  
  cat(paste("\n--- Building PLS model for:", element, "---\n"))
  
  # --- This is the key change ---
  # We create a predictor matrix 'X' that includes both the spectra and the 'zone' variable.
  # The model.matrix() function automatically converts the categorical 'zone' column 
  # into numeric dummy variables (0s and 1s) that the PLS model can use.
  
  # 1. Create a dataframe of all predictors
  predictors_df <- combined_df %>%
    select(zone, all_of(spectral_cols_filtered))
  
  # 2. Convert to a model matrix, creating dummy variables for 'zone'
  X <- model.matrix(~ . - 1, data = predictors_df) # '-1' removes the intercept column
  
  # 3. Define the response variable (y)
  y <- combined_df[[element]]
  
  # Build the PLS model with cross-validation
  model <- pls(X, y, ncomp = 15, cv = 10, scale = TRUE) # Scaling is important
  
  # Find and set the optimal number of components
  ncomp_optimal <- model$cvres$ncomp.selected
  model$ncomp.selected <- ncomp_optimal
  
  cat(paste("Optimal number of components for", element, "is:", ncomp_optimal, "\n"))
  
  # --- Generate and Save the Diagnostic Plots ---
  file_name <- file.path(output_dir_pls, paste0("pls_plot_", element, ".png"))
  
  png(filename = file_name, width = 10, height = 8, units = "in", res = 300)
  plot(model, main = paste("PLS Performance for", element, "(including Zone)"))
  dev.off()
  
  cat("Saved PLS plot:", file_name, "\n")
}

cat("\nPLS modeling complete. All plots have been saved to the '", output_dir_pls, "' folder. ✅\n")


# --- 5. Final Environment Cleanup ---

# List of final objects you want to keep
objects_to_keep <- c(
  "icpms_raw_df", 
  "ftnirs_raw_df", 
  "combined_df"
)

# Get all objects created by the script
all_objects <- ls()
objects_to_remove <- setdiff(all_objects, c(initial_objects, objects_to_keep))

# Remove the intermediate objects if there are any to remove
if (length(objects_to_remove) > 0) {
  rm(list = objects_to_remove)
}

cat("\nEnvironment cleaned. Remaining objects:\n")



###### #OHETOIEHROIEJRO:KAJWSLDKJASPJKLD




library(vegan)
library(factoextra)

# Get all element column names from your combined_df
element_cols_all <- names(combined_df)[grepl("_core$|_middle$|_edge$", names(combined_df))]

# Get the spectral data column names
spectral_cols_filtered <- names(combined_df)[grepl("^[0-9.]+$", names(combined_df))]

# Create a clean dataframe for this analysis
analysis_df <- combined_df %>%
  select(specimen_number, zone, all_of(spectral_cols_filtered), all_of(element_cols_all)) %>%
  na.omit()

# Perform PCA once on the spectral data
pca_results <- prcomp(analysis_df[, spectral_cols_filtered], scale. = TRUE)

# Loop through each region to create a separate plot
regions_to_plot <- c("core", "middle", "edge")

for (current_region in regions_to_plot) {
  
  element_cols_for_region <- element_cols_all[grepl(paste0("_", current_region, "$"), element_cols_all)]
  
  # Fit only the elements for the current region onto the PCA
  env_fit <- envfit(pca_results, analysis_df[, element_cols_for_region], na.rm = TRUE)
  
  pca_biplot <- fviz_pca_ind(pca_results, 
                             habillage = analysis_df$zone, 
                             addEllipses = TRUE, ellipse.level = 0.95,
                             ggtheme = theme_minimal(),
                             title = paste("PCA of FT-NIRS Spectra with", toupper(current_region), "Element Vectors"))
  
  sig_arrows_scores <- vegan::scores(env_fit, "vectors")
  sig_arrows_df <- as.data.frame(sig_arrows_scores[env_fit$vectors$pvals < 0.05, , drop = FALSE])
  
  if(nrow(sig_arrows_df) > 0){
    pca_biplot_with_vectors <- pca_biplot +
      geom_segment(data = sig_arrows_df * 5, 
                   aes(x = 0, y = 0, xend = PC1, yend = PC2),
                   arrow = arrow(length = unit(0.2, "cm")), color = "black", inherit.aes = FALSE) +
      geom_text(data = sig_arrows_df * 5.5, 
                aes(x = PC1, y = PC2, label = rownames(sig_arrows_df)),
                color = "black", fontface = "bold", inherit.aes = FALSE)
  } else {
    pca_biplot_with_vectors <- pca_biplot
  }
  
  print(pca_biplot_with_vectors)
}




library(mdatools)

# Get the list of spectral column names
spectral_cols_filtered <- names(combined_df)[grepl("^[0-9.]+$", names(combined_df))]

# Create a clean dataframe for this specific model
pls_data <- combined_df %>%
  select(Ba138_ppm_core, zone, all_of(spectral_cols_filtered)) %>%
  na.omit()

# Define predictors (X) and response (y)
predictors_df <- pls_data %>% select(zone, all_of(spectral_cols_filtered))
X <- model.matrix(~ . - 1, data = predictors_df)
y <- pls_data$Ba138_ppm_core

# Build the PLS model
model <- pls(X, y, ncomp = 15, cv = 10, scale = TRUE)
model$ncomp.selected <- model$cvres$ncomp.selected

# --- Plot the VIP scores ---
# This plot shows which wavelengths are most important for predicting the element
plotVIPScores(model, main = "VIP Scores for Predicting Ba138_ppm_core")





library(vegan)

# Get the names of the element and spectral columns
element_cols_all <- names(combined_df)[grepl("_core$|_middle$|_edge$", names(combined_df))]
spectral_cols_filtered <- names(combined_df)[grepl("^[0-9.]+$", names(combined_df))]

# Create a clean dataframe with no NAs
cca_data <- combined_df %>%
  select(all_of(element_cols_all), all_of(spectral_cols_filtered)) %>%
  na.omit()

# Separate the two datasets
elements_matrix <- cca_data[, element_cols_all]
spectra_matrix <- cca_data[, spectral_cols_filtered]

# Run the CCA
cca_result <- cca(elements_matrix, spectra_matrix)

# Test the significance of the overall relationship
# A small p-value (< 0.05) means the relationship is statistically significant
print(anova(cca_result))

# Plot the results
# Samples will be shown as points, elements and spectra as text
# If they are close together on the plot, they are related.
plot(cca_result, display = c("sites", "species", "bp"))





cat("\n--- Correlating Principal Components with Elemental Data ---\n")

# Load the Hmisc package, which has a more robust correlation testing function
# install.packages("Hmisc")
library(Hmisc)

# -- This first part of the script is the same --
spectral_cols <- names(combined_df)[grepl("^[0-9.]+$", names(combined_df))]
element_cols <- names(combined_df)[grepl("_core$|_middle$|_edge$", names(combined_df))]

non_zero_var_cols <- which(apply(combined_df[, spectral_cols], 2, var, na.rm = TRUE) > 1e-10)
spectral_cols_filtered <- names(non_zero_var_cols)

analysis_df <- combined_df %>%
  select(specimen_number, zone, all_of(spectral_cols_filtered), all_of(element_cols)) %>%
  na.omit()

pca_results <- prcomp(analysis_df[, spectral_cols_filtered], scale. = TRUE)
pc_scores <- as.data.frame(pca_results$x[, 1:5])
correlation_df <- bind_cols(analysis_df %>% select(all_of(element_cols)), pc_scores)

# --- THIS IS THE ROBUST, CORRECTED SECTION ---

# 1. Clean up the long element names as you specified
cleaned_names_df <- correlation_df %>%
  rename_with(~ str_remove(., "[0-9]+_ppm")) %>%
  rename_with(~ str_replace(., "_core", " (Core)")) %>%
  rename_with(~ str_replace(., "_middle", " (Mid)")) %>%
  rename_with(~ str_replace(., "_edge", " (Edge)")) %>%
  rename_with(~ str_replace(., "Sr87_86", "SrRatio"))

# 2. Calculate correlation and p-value matrices using a robust method
# Hmisc::rcorr() requires a matrix input and is very stable.
rcorr_results <- Hmisc::rcorr(as.matrix(cleaned_names_df))
cor_matrix <- rcorr_results$r
p_matrix <- rcorr_results$P

# 3. SUBSET both matrices robustly
cleaned_element_names <- names(cleaned_names_df)[!grepl("^PC", names(cleaned_names_df))]
pc_names <- names(pc_scores)

# The `drop = FALSE` argument is the critical fix that prevents the error.
cor_subset <- cor_matrix[cleaned_element_names, pc_names, drop = FALSE]
p_mat_subset <- p_matrix[cleaned_element_names, pc_names, drop = FALSE]

# 4. Create the final plot
cat("\n--- Generating Focused Correlation Plot with Significance Testing ---\n")

corrplot(t(cor_subset), 
         method = "color",
         addCoef.col = "black",
         tl.col = "black",
         tl.cex = 0.8,
         number.cex = 0.7,
         title = "\n\nCorrelation between Elements and FT-NIRS Principal Components",
         mar = c(0,0,2,0),
         
         # Arguments to show non-significant correlations with an "X"
         p.mat = t(p_mat_subset),
         sig.level = 0.05,
         insig = "pch",
         pch.col = "grey40",
         pch.cex = 3.5
)

cat("Focused correlation plot generated.\n")