# --- 1. Load Libraries and Data Paths ---
# Required libraries
library(dplyr)
library(tidyr)
library(stringr)
library(pls)
library(vegan)      # Loaded for potential future use (like rda/cca) and for the ecosystem
library(ggplot2)
library(ggrepel)
library(broom)
library(purrr)
library(prospectr) # For Savitzky-Golay
# library(mdatools) # No longer needed for PCA
library(CCP)      # For CCA significance testing

# Define file paths (adjust these if running locally)
ICPMS_path <- "C:/Users/poopm/Thesis Writing/Github/LPW_FT-NIRS/RDS_dataframes/ICPMS_filtered_GOOD_USEME.RDS"
FTNIRS_path <- "C:/Users/poopm/Thesis Writing/Github/LPW_FT-NIRS/RDS_dataframes/combined_IBM_LPW_raw.RDS"

# Load Data
ICPMS_raw <- readRDS(file = ICPMS_path)
FTNIRS_raw <- readRDS(FTNIRS_path)


# --- 2. Prepare ICP-MS Data (Y-matrix) ---

# Define the *original* column names and *clean* names
elements_of_interest <- c(
  "Mg24_ppm", 
  "Mn55_ppm", 
  "Li7_ppm", 
  "Zn66_ppm", 
  "Sr88_ppm", 
  "Ba138_ppm"
)

elements_clean <- c("Mg", "Mn", "Li", "Zn", "Sr", "Ba")

# Summarize ICP-MS data by averaging transects per region
ICPMS_region_avg <- ICPMS_raw %>%
  # Filter out rows with NA in 'specimen' or 'oto_region' before grouping
  filter(!is.na(specimen), !is.na(oto_region)) %>%
  group_by(specimen, oto_region) %>%
  summarize(
    across(all_of(elements_of_interest), \(x) median(x, na.rm = TRUE), .names = "{.col}"),
    .groups = "drop"
  )

# Rename columns to clean elemental names *before* pivoting
ICPMS_region_avg <- ICPMS_region_avg %>%
  rename_with(~str_remove_all(.x, "[0-9]+|_ppm"), .cols = all_of(elements_of_interest))

# Pivot wider to create the Y-matrix structure
ICPMS_wide_region <- ICPMS_region_avg %>%
  pivot_wider(
    names_from = oto_region,
    values_from = all_of(elements_clean),
    names_sep = "_"
  ) %>%
  # Ensure 'specimen' is clean for joining
  mutate(specimen = as.factor(specimen))

# Identify specimens with complete ICP-MS data
complete_specimens_ICPMS <- ICPMS_wide_region %>%
  drop_na() %>%
  pull(specimen)

cat(paste("Total specimens with complete ICP-MS data:", length(complete_specimens_ICPMS), "\n"))


# --- 3. Prepare FT-NIRS Data (X-matrix) ---

# 3a. Filter FT-NIRS data to include only the specimens that passed the ICP-MS filtering
FTNIRS_filtered <- FTNIRS_raw %>%
  filter(region != "LPW") %>% # Select only IBM specimens
  mutate(specimen = as.factor(specimen)) %>%
  filter(specimen %in% complete_specimens_ICPMS)

cat(paste("Filtered FT-NIRS data to match ICP-MS data:", nrow(FTNIRS_filtered), "rows.\n"))

# 3b. Apply Savitzky-Golay filter
speccols <- names(FTNIRS_filtered)[grepl("^\\d", names(FTNIRS_filtered))]
metacols <- setdiff(names(FTNIRS_filtered), speccols)

FTNIRS_processed <- cbind(
  FTNIRS_filtered[, metacols],
  # Applying SG filter (m=1, p=5, w=17 from your original script)
  savitzkyGolay(as.matrix(FTNIRS_filtered[speccols]), m = 1, p = 5, w = 17)
)
speccols_processed <- names(FTNIRS_processed)[grepl("^\\d", names(FTNIRS_processed))]


# 3c. Run PCA on the processed FT-NIRS spectra using prcomp (standard base R function)
X_data_for_pca <- FTNIRS_processed[speccols_processed]
pca_FTNIRS <- prcomp(X_data_for_pca, center = TRUE, scale = TRUE)

# Calculate cumulative variance explained from prcomp output (sdev)
# variance = sdev^2 / sum(sdev^2)
var_explained <- pca_FTNIRS$sdev^2 / sum(pca_FTNIRS$sdev^2)
cumulative_variance <- cumsum(var_explained)

# Determine the number of PCs for 90% variance or use maximum 20
num_pcs_90_perc <- which(cumulative_variance >= 0.9)[1]
# Ensure we don't try to use more PCs than available (ncol(pca_FTNIRS$x))
num_pcs_to_use <- min(num_pcs_90_perc, 25, ncol(pca_FTNIRS$x))

cat(paste("\nPCA Results (using prcomp):\n"))
cat(paste("PCs needed for 90% variance:", num_pcs_90_perc, "\n"))
cat(paste("Using", num_pcs_to_use, "Principal Components for CCA (max 25).\n"))


# Extract PC scores from prcomp output ($x)
pca_df <- data.frame(
  specimen = FTNIRS_processed$specimen,
  pca_FTNIRS$x[, 1:num_pcs_to_use] # $x holds the PC scores
)
# Rename columns
colnames(pca_df)[2:(num_pcs_to_use + 1)] <- paste0("PC", 1:num_pcs_to_use)


# --- 4. Final Data Alignment and Matrix Creation ---

# Join PCA scores (X) with ICP-MS data (Y)
final_analysis_data <- inner_join(pca_df, ICPMS_wide_region, by = "specimen")
# The use of 'complete_specimens_ICPMS' already ensured no NAs, but we keep this join structure.


# Create the final X and Y matrices
X_matrix_pca <- final_analysis_data %>%
  dplyr::select(starts_with("PC")) %>%
  as.matrix()

Y_matrix_elements <- final_analysis_data %>%
  dplyr::select(-specimen, -starts_with("PC")) %>%
  as.matrix()

# Scale the Y matrix (essential for standardizing elemental concentrations)
Y_matrix_scaled <- scale(Y_matrix_elements)


# --- 5. Run Canonical Correlation Analysis (CCA) ---
# X = FT-NIRS PCs (Predictors), Y = Scaled ICP-MS Data (Responses)
cca_result <- cancor(X_matrix_pca, Y_matrix_scaled)


# --- 6. Results and Output ---

cat("\n======================================================\n")
cat("          CANONICAL CORRELATION ANALYSIS RESULTS        \n")
cat("======================================================\n")

# 6a. Canonical Correlations
cat("\n6a. Canonical Correlations (Correlations between Canonical Variates):\n")
canonical_correlations <- data.frame(
  Canonical_Pair = 1:length(cca_result$cor),
  Correlation = cca_result$cor
)
print(canonical_correlations)


# 6b. Significance Testing (Wilks' Lambda)
cat("\n6b. Significance Test for Canonical Variates (Wilks' Lambda):\n")
# Requires the CCP library
# N = sample size, p = number of X variables, q = number of Y variables
p.asym(cca_result$cor,
                     N = nrow(X_matrix_pca),
                     p = ncol(X_matrix_pca),
                     q = ncol(Y_matrix_scaled),
                     tstat = "Wilks")

# # Display the Wilks' Lambda test results
# # The first row tests the significance of all pairs, the second tests pairs 2 onwards, etc.
# print(cca_p_test)

# 6c. Elemental Loadings for the most significant pair (CV1)
cat("\n6c. Elemental Loadings (Importance) for Canonical Variate 1 (CV1):\n")

# Extract the coefficients (weights) for the Y-variables (Elements) for the first CV
cv1_y_loadings <- data.frame(
  Element_Region = rownames(cca_result$ycoef),
  Loading_CV1 = cca_result$ycoef[, 1]
)

# Calculate the standardized (correlation) coefficients for better interpretability
# These are the correlations between the original variables and the canonical variate
# Standardized coefficients = Y_matrix_scaled %*% cca_result$ycoef
cv1_y_correlations <- cor(Y_matrix_scaled, cca_result$ycan[, 1])
cv1_y_correlations_df <- data.frame(
  Element_Region = rownames(cv1_y_correlations),
  Correlation_CV1 = cv1_y_correlations[, 1]
)
# Merge and sort by absolute correlation to show importance
element_importance <- cv1_y_correlations_df %>%
  arrange(desc(abs(Correlation_CV1)))
print(element_importance)






correl_test <- function(varnum){
  cv_y_correlations <- cor(Y_matrix_scaled, cca_result$ycan[, varnum])
  cv_y_correlations_df <- data.frame(
    Element_Region = rownames(cv_y_correlations),
    Correlation_CV = cv_y_correlations[, varnum]
  )
  # Merge and sort by absolute correlation to show importance
  element_importance <- cv_y_correlations_df %>%
    dplyr::arrange(desc(abs(Correlation_CV)))
  print(element_importance)
}

correl_test(varnum =1)

for (i in 1:18){
  print(paste0("Canonical Variate # ", i))
  correl_test(varnum = i)
}
