library(dplyr)
library(lubridate)
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
library(readxl)

IBM <- readRDS("RDS_dataframes/IBM_dfmeta_scan2_raw.RDS")
LPW <- readRDS("RDS_dataframes/LPW_scan_avg_unproc.RDS")


# specs_IBM <- names(raw_IBM)[grepl("^\\d", names(raw_IBM))]
# specs_LPW <- names(raw_LPW)[grepl("^\\d", names(raw_LPW))]


# create combined LPW and IBM dataframe
# LPW <- readRDS("RDS_dataframes/LPW_scan_avg_proc_UPDATED.RDS")
LPW$region <- "LPW"
LPW <- LPW %>% select(-c(expected_age, average_grade, percent_affected, other_problem, broken, side, common_name, run_number, scan_name, file_name, session_title, comments, file_path))
LPW <- LPW %>% select(specimen, region, length, weight, structure_weight, sample_date, hatch_date, read_age, everything())


# IBM #
# IBM <- readRDS("RDS_dataframes/IBM_SGpreproc.RDS")
IBM <- IBM %>% select(-c(haul, latitude, longitude, test_age, final_age, run_number, scan_name, comments, unscannable, percent_affected, other_problem, crystallized, broken, side, timestamp, file_name, session_title, file_path))
IBM$weight <- NA
IBM$date_collected <- as.Date(IBM$date_collected)
IBM$date_collected <- yday(IBM$date_collected)
IBM <- IBM %>%
  rename(sample_date = date_collected)
IBM$hatch_date <- IBM$sample_date - IBM$read_age
IBM <- IBM %>% select(specimen, region, length, weight, structure_weight, sample_date, hatch_date, read_age, everything())
region_meta <- read_xlsx("metadata/IBM_region_specs.xlsx")
IBM <- IBM %>%
  inner_join(region_meta, by = "specimen") %>%
  select(-region.x, region = region.y) %>%
  select(specimen, region, length, weight, structure_weight, sample_date, hatch_date, read_age, everything())
rm(region_meta)

df_combined_raw <- rbind(LPW, IBM)
df_combined_raw <- temp

temp <- df_combined_raw
# saveRDS(df_combined_raw, "combined_IBM_LPW_raw.RDS")
# rm(IBM, LPW)



### SAVITZKY GOLAY FILTERING ###

# find spec cols
spec_col <- function(df) {
  names(df)[grepl("^\\d", names(df))]
}

make_long <- function(df, speccols) {
  df_long <- df %>%
    tidyr::pivot_longer(cols = speccols) %>%
    # rename(wavenumber = name) %>%
    # mutate(wavenumber = as.numeric(as.character(wavenumber)))
    mutate(name = as.numeric(as.character(name)))
  return(df_long)
}


spec.fig <- function(mydf, color) {
  # 1. Quoting to allow column name use later
  color_col <- rlang::enquo(color) 

  # 2. Sort the data by the grouping variable (specimen) and the x-axis (name)
  mydf_sorted <- mydf %>% 
    dplyr::arrange(specimen, name) 
  
  # 3. Plot the sorted data
  ggplot(mydf_sorted) +
    geom_path(aes(x = name, y = value, color = !!color_col, group = specimen)) +
    scale_x_reverse() +
    scale_color_viridis() +
    labs(y = "Preprocessed absorbance", x = expression(paste("Wavenumber ", cm^-1))) + 
    theme_bw()
}

# Note: Requires dplyr and tidyr
sg_plotting <- function(df, speccols, color, m, p, w) {
  metacols <- setdiff(names(df), speccols)
  sg_matrix <- savitzkyGolay(df[, speccols], m = m, p = p, w = w)
  
  # FIX: Dynamically determine the correct subset of original names
  n_orig <- length(speccols)
  p_rem <- (n_orig - ncol(sg_matrix)) / 2
  new_speccols <- if (p_rem > 0) speccols[(p_rem + 1) : (n_orig - p_rem)] else speccols
  
  names(sg_matrix) <- new_speccols
  
  dftempproc_long <- dplyr::bind_cols(df[, metacols], as.data.frame(sg_matrix)) %>%
    tidyr::pivot_longer(cols = tidyr::all_of(new_speccols), names_to = "name", values_to = "value") %>%
    dplyr::mutate(name = as.numeric(as.character(name)))
  
  spec.fig(mydf = dftempproc_long, color = {{ color }}) +
    ggtitle(paste("diff = ", {{ m }}, "poly = ", {{ p }}, "window = ", {{ w }}))
}

quickproc <- function(df, speccols, m, p, w) {
  metacols <- setdiff(names(df), speccols)
  sg_matrix <- savitzkyGolay(df[, speccols], m = m, p = p, w = w)
  
  # FIX: Dynamically determine the correct subset of original names
  n_orig <- length(speccols)
  p_rem <- (n_orig - ncol(sg_matrix)) / 2
  new_speccols <- if (p_rem > 0) speccols[(p_rem + 1) : (n_orig - p_rem)] else speccols
  
  names(sg_matrix) <- new_speccols
  
  # Global assignments (using <<- as requested)
  temp_proc <<- dplyr::bind_cols(df[, metacols], as.data.frame(sg_matrix))
  
  temp_proc_long <<- temp_proc %>%
    tidyr::pivot_longer(cols = tidyr::all_of(new_speccols), names_to = "name", values_to = "value") %>%
    dplyr::mutate(name = as.numeric(as.character(name)))
}


# TRY SNV and PRPROCESSING
speccols <- names(df_combined_raw)[grepl("^\\d", names(df_combined_raw))]


speccols_IBM <- names(IBM)[grepl("^\\d", names(IBM))]
metacols_IBM <- setdiff(names(IBM), speccols_IBM)
speccols_LPW <- names(LPW)[grepl("^\\d", names(LPW))]
metacols_LPW <- setdiff(names(LPW), speccols_LPW)

##          IBM            ##
# SNV
testing1 <- prep.snv(as.matrix(IBM[,speccols_IBM]))
testing1 <- as.data.frame(unlist(testing1))
testing1 <- cbind(IBM[,metacols_IBM], testing1)
testing1_long <- make_long(testing1, speccols_IBM)
testing1_long <- testing1_long %>%
  filter(complete.cases(read_age))
spec.fig(testing1_long, read_age)


# SNV + SG(1,3,17)
testing2 <- quickproc(testing1,speccols_IBM, 1,3,17)
testing2 <- temp_proc
speccols_IBM_sg <- names(testing2)[grepl("^\\d", names(testing2))]

testing2_long <- make_long(testing2, speccols_IBM_sg)
testing2_long <- testing2_long %>%
  filter(complete.cases(read_age))
# quickproc(df_combined_raw, speccols, m = 1, p = 3, w = 17)
spec.fig(testing2_long, read_age)




##          LPW            ##
# SNV
testing3 <- prep.snv(as.matrix(LPW[,speccols_LPW]))
testing3 <- as.data.frame(unlist(testing3))
testing3 <- cbind(LPW[,metacols_LPW], testing3)
testing3_long <- make_long(testing3, speccols_LPW)
testing3_long <- testing3_long %>%
  filter(complete.cases(read_age))
spec.fig(testing3_long, read_age)


# SNV + SG(1,3,17)
testing4 <- quickproc(testing3,speccols_LPW, 1,3,17)
testing4 <- temp_proc
speccols_LPW_sg <- names(testing4)[grepl("^\\d", names(testing4))]

testing4_long <- make_long(testing4, speccols_LPW_sg)
testing4_long <- testing4_long %>%
  filter(complete.cases(read_age))
# quickproc(df_combined_raw, speccols, m = 1, p = 3, w = 17)
spec.fig(testing4_long, read_age)







# LETS TRY COMBINING 2 AND 4

combine_test <- rbind(testing2, testing4)
speccols_combined <- spec_col(combine_test)
combine_test_long <- make_long(combine_test, speccols_combined)
combine_test_long <- combine_test_long %>%
  filter(complete.cases(read_age))


spec.fig(combine_test_long, read_age)



df_combined_raw <- df_combined_raw %>%
  filter(complete.cases(read_age))



quickproc(df_combined_raw, speccols, m = 1, p = 3, w = 17)
# df_proc <- temp_proc
# saveRDS(df_proc, "RDS_dataframes/combined_IBM_LPW_SG_filt.RDS")



spec.fig(temp_proc_long %>% filter(region == "W Shumagins"), read_age)
spec.fig(temp_proc_long, read_age)
spec.fig(temp_proc_long, region) +  scale_color_viridis(discrete = T)

# testing1 <- prep.snv(as.matrix(dfmeta_IBM[,25:ncol(dfmeta_IBM)]))
# 








saveRDS(combine_test,"RDS_dataframes/combined_IBM_LPW_experimental_SNVandSG.RDS")
































































####### NEW VERSION FOR FILTERING WITH WAVE CUTOFFS, SNV AND SG

# ---
# R Script: FT-NIRS Preprocessing Pipeline
# ---
#
# This script provides functions to preprocess FT-NIRS spectral data.
#
# Key Functions:
# 1. preprocess_spectra: The main function. Applies (in order):
#    - Wavenumber cutoff filtering
#    - Standard Normal Variate (SNV)
#    - Savitzky-Golay (SG) filtering
# 2. spec.fig: Your plotting function (unchanged).
# 3. plot_processed_spectra: A helper function to pivot the processed
#    data and plot it using spec.fig.
#
# Required Libraries:
# Please ensure you have these installed: install.packages(c("dplyr", "tidyr", "prospectr", "ggplot2", "viridis", "rlang"))
# ---

library(dplyr)
library(tidyr)
library(prospectr)  # For savitzkyGolay() and prep.snv()
library(ggplot2)
library(viridis)
library(rlang)      # For enquo() in plotting

#' Preprocess FT-NIRS Spectra
#'
#' Applies a sequence of preprocessing steps to a spectral dataframe.
#' The steps are applied in this order:
#' 1. Wavenumber cutoff (removes columns > cutoff)
#' 2. Standard Normal Variate (SNV)
#' 3. Savitzky-Golay (SG) filter
#'
#' @param df The input dataframe. Must contain metadata and spectral columns.
#' @param all_speccols A character vector of all column names that
#'   represent spectral data (wavenumbers).
#' @param wn_cutoff A numeric value. Any wavenumber (column name) greater
#'   than this value will be removed. Default is NULL (no cutoff).
#' @param apply_snv Logical. If TRUE, applies SNV transformation.
#'   Default is FALSE.
#' @param apply_sg Logical. If TRUE, applies Savitzky-Golay filter.
#'   Default is FALSE.
#' @param sg_m Integer. The differentiation order for SG filter.
#'   Default is 0.
#' @param sg_p Integer. The polynomial order for SG filter.
#'   Default is 2.
#' @param sg_w Integer. The window size for SG filter (must be odd).
#'   Default is 11.
#'
#' @return A dataframe in wide format with the transformations applied
#'   to the spectral columns. Metadata columns are preserved.
#'
preprocess_spectra <- function(df,
                               all_speccols,
                               wn_cutoff = NULL,
                               apply_sg_smooth = FALSE,
                               sg_smooth_m = 1,
                               sg_smooth_p = 3,
                               sg_smooth_w = 17,
                               apply_snv = FALSE,
                               apply_sg_deriv = FALSE,
                               sg_deriv_m = 1,
                               sg_deriv_p = 3,
                               sg_deriv_w = 17) {
  
  # 1. Identify metadata and spectral columns from the original dataframe
  metacols <- setdiff(names(df), all_speccols)
  current_speccols <- all_speccols
  
  # Keep a copy of the metadata
  meta_df <- df[, metacols, drop = FALSE]
  
  # Start with the raw spectral data
  processed_spectra <- df[, current_speccols, drop = FALSE]
  
  # ---
  # Step 1: Wavenumber Cutoff
  # ---
  if (!is.null(wn_cutoff)) {
    cat("Applying wavenumber cutoff: >", wn_cutoff, "\n")
    
    # Convert column names to numeric for comparison
    numeric_wavenumbers <- as.numeric(current_speccols)
    
    # Find columns to keep (those <= cutoff)
    cols_to_keep_mask <- numeric_wavenumbers <= wn_cutoff
    
    if (sum(cols_to_keep_mask) == 0) {
      stop("Wavenumber cutoff resulted in 0 spectral columns. Check your cutoff value.")
    }
    
    # Update the list of spectral columns
    current_speccols <- current_speccols[cols_to_keep_mask]
    
    # Filter the spectral data
    processed_spectra <- processed_spectra[, current_speccols, drop = FALSE]
    
    cat("  New spectral range:", range(as.numeric(current_speccols)), "\n")
  }
  
  # ---
  # Step 2: Savitzky-Golay (SG) Smoothing (m=0)
  # ---
  if (apply_sg_smooth) {
    cat("Applying Savitzky-Golay Smoothing (m=0, p=", sg_smooth_p, ", w=", sg_smooth_w, ")...\n")
    
    # Store the column names *before* applying SG
    pre_sg_speccols <- colnames(processed_spectra)
    
    # Apply the SG filter (m=0 for smoothing)
    sg_matrix <- savitzkyGolay(as.matrix(processed_spectra), m = sg_smooth_m, p = sg_smooth_p, w = sg_smooth_w)
    
    # --- Handle edge clipping ---
    n_orig <- length(pre_sg_speccols)
    n_new <- ncol(sg_matrix)
    p_rem <- (n_orig - n_new) / 2
    
    if (p_rem < 0) stop("SG smoothing returned *more* columns than input.")
    
    new_speccols <- if (p_rem > 0) {
      pre_sg_speccols[(p_rem + 1):(n_orig - p_rem)]
    } else {
      pre_sg_speccols
    }
    # --- End clipping ---
    
    colnames(sg_matrix) <- new_speccols
    processed_spectra <- as.data.frame(sg_matrix)
    
    cat("  New spectral range after SG smoothing:", range(as.numeric(new_speccols)), "\n")
  }
  
  # ---
  # Step 3: Standard Normal Variate (SNV)
  # ---
  if (apply_snv) {
    cat("Applying SNV...\n")
    
    # Store column names before conversion
    snv_input_colnames <- colnames(processed_spectra)
    
    # prep.snv expects a matrix.
    processed_spectra_matrix <- as.matrix(processed_spectra)
    
    # prep.snv returns a matrix, so we convert back to data.frame
    processed_spectra <- as.data.frame(prep.snv(processed_spectra_matrix))
    
    # Re-apply column names, as they can be lost in matrix conversion
    colnames(processed_spectra) <- snv_input_colnames
  }
  
  # ---
  # Step 4: Savitzky-Golay (SG) Derivative
  # ---
  if (apply_sg_deriv) {
    cat("Applying Savitzky-Golay Derivative (m=", sg_deriv_m, ", p=", sg_deriv_p, ", w=", sg_deriv_w, ")...\n")
    
    # Store the column names *before* applying SG
    pre_sg_speccols <- colnames(processed_spectra)
    
    # Apply the SG filter
    sg_matrix <- savitzkyGolay(as.matrix(processed_spectra), m = sg_deriv_m, p = sg_deriv_p, w = sg_deriv_w)
    
    # --- Handle edge clipping ---
    n_orig <- length(pre_sg_speccols)
    n_new <- ncol(sg_matrix)
    p_rem <- (n_orig - n_new) / 2
    
    if (p_rem < 0) stop("SG derivative returned *more* columns than input.")
    
    new_speccols <- if (p_rem > 0) {
      pre_sg_speccols[(p_rem + 1):(n_orig - p_rem)]
    } else {
      pre_sg_speccols
    }
    # --- End clipping ---
    
    colnames(sg_matrix) <- new_speccols
    processed_spectra <- as.data.frame(sg_matrix)
    
    cat("  New spectral range after SG derivative:", range(as.numeric(new_speccols)), "\n")
  }
  
  # ---
  # Final Step: Recombine metadata and processed spectra
  # ---
  final_df <- dplyr::bind_cols(meta_df, processed_spectra)
  
  cat("Preprocessing complete.\n")
  return(final_df)
}
#' Plot Spectra (Your Function)
#'
#' Plots spectral data that is in long format.
#'
#' @param mydf A long-format dataframe with columns 'specimen', 'name' (wavenumber),
#'   'value' (absorbance), and the column specified in 'color'.
#' @param color The metadata column to use for coloring the spectra (e.g., age, site).
spec.fig <- function(mydf, color) {
  # 1. Quoting to allow column name use later
  color_col <- rlang::enquo(color)
  
  # 2. Sort the data by the grouping variable (specimen) and the x-axis (name)
  #    Sorting by 'name' (wavenumber) is crucial for geom_path()
  mydf_sorted <- mydf %>%
    dplyr::arrange(specimen, name)
  
  # 3. Plot the sorted data
  ggplot(mydf_sorted, aes(x = name, y = value, color = !!color_col, group = specimen)) +
    geom_path(alpha = 0.7) + # Added alpha for better visibility
    scale_x_reverse() +
    scale_color_viridis() +
    labs(y = "Preprocessed absorbance", x = expression(paste("Wavenumber ", cm^-1))) +
    theme_bw() # Added a clean theme
}


#' Helper to Pivot and Plot
#'
#' Takes a wide-format processed dataframe, pivots it, and calls spec.fig.
#'
#' @param processed_df The wide-format dataframe returned by `preprocess_spectra`.
#' @param color The unquoted column name to use for coloring (e.g., age).
#' @return A ggplot object.


plot_processed_spectra <- function(processed_df, color) {
  
  # Identify spectral columns (all columns that are not metadata)
  # This assumes 'specimen' is a key metadata column.
  # A more robust way is to find all numeric-like column names again.
  speccols <- names(processed_df)[grepl("^\\d", names(processed_df))]
  
  if (length(speccols) == 0) {
    stop("Could not find any spectral columns (names starting with digits) in the processed data.")
  }
  
  # Pivot the data to long format
  df_long <- processed_df %>%
    tidyr::pivot_longer(
      cols = tidyr::all_of(speccols),
      names_to = "name",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      # Ensure wavenumber ('name') is numeric for plotting
      name = as.numeric(as.character(name))
    )
  
  # Call the plotting function
  spec.fig(mydf = df_long, color = {{ color }})
}






library(plotly)



df_combined_raw <- readRDS("RDS_dataframes/combined_IBM_LPW_raw.RDS")
# filter for read_age using complete.cases()
df_combined_raw <- df_combined_raw %>%
  filter(complete.cases(read_age))

speccols <- names(df_combined_raw)[grepl("^\\d", names(df_combined_raw))]
# ---
# --- RUN PREPROCESSING ---
# ---

# Example 1: Plot Raw Data (colored by age)
# You will see 'IBM' (dark blue) are flat, 'LPW' (yellow) have signal
plot_raw <- plot_processed_spectra(df_combined_raw, color = read_age) +
  ggtitle("Raw Spectra (Colored by Site)")
print(plot_raw)


# Example 2: Apply SNV only (The problem you saw)
# This will show the "zig-zag" noise amplification
df_snv <- preprocess_spectra(df_combined_raw, speccols,
                             apply_snv = TRUE)

plot_snv <- plot_processed_spectra(df_snv, color = read_age) +
  ggtitle("SNV Only (Problem Visualized)")
print(plot_snv)


# Example 3: *** THE FIX ***
# Apply SG Smoothing FIRST, then apply SNV
df_fix <- preprocess_spectra(df_combined_raw, speccols,
                             apply_sg_smooth = TRUE,
                             sg_smooth_m = 0,
                             sg_smooth_p = 3,
                             sg_smooth_w = 17,
                             apply_snv = TRUE)

plot_fix <- plot_processed_spectra(df_fix, color = read_age) +
  ggtitle("FIX: SG Smooth -> SNV")
print(plot_fix)
ggplotly(plot_fix)

# Example 4: Full Pipeline: Smooth -> SNV -> Derivative
df_all <- preprocess_spectra(df_combined_raw, speccols,
                             wn_cutoff = 10000,
                             apply_sg_smooth = TRUE, sg_smooth_w = 21,
                             apply_snv = TRUE,
                             apply_sg_deriv = TRUE, sg_deriv_m = 1, sg_deriv_w = 15)

plot_all <- plot_processed_spectra(df_all, color = read_age) +
  ggtitle("Cutoff -> Smooth -> SNV -> 1st Derivative")
print(plot_all)
