# ================================================================= #
# 1. SETUP: PACKAGES, DATA, AND CONSTANTS
# ================================================================= #
library(mdatools)
library(dplyr)
library(ggplot2)
library(foreach)
library(doParallel)

df <- readRDS("RDS_dataframes/LPW_scan_avg_proc.RDS")
df <- df[complete.cases(df$read_age), ]

N_REPEATS <- 10
K_FOLDS   <- 10
RESPONSE_VAR <- "read_age"
PREDICTOR_VARS <- 21:ncol(df)

# ================================================================= #
# 2. PARALLEL EXECUTION
# ================================================================= #
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)
cat(paste("Starting parallel job on", detectCores() - 1, "cores...\n"))

all_results_list <- foreach(
  i = 1:N_REPEATS,
  .packages = c("mdatools", "dplyr"),
  .combine = 'rbind'
) %dopar% {
  
  set.seed(123 + i)
  folds <- caret::createFolds(df[[RESPONSE_VAR]], k = K_FOLDS, list = TRUE)
  repeat_results_list <- list()
  
  for (k in 1:K_FOLDS) {
    cal_indices <- unlist(folds[-k])
    val_indices <- folds[[k]]
    
    cal_data <- df[cal_indices, ]; val_data <- df[val_indices, ]
    cal_x <- cal_data[, PREDICTOR_VARS]; cal_y <- cal_data[[RESPONSE_VAR]]
    val_x <- val_data[, PREDICTOR_VARS]; val_y <- val_data[[RESPONSE_VAR]]
    
    fold_results_list <- list()
    
    m_baseline <- pls(cal_x, cal_y, cv = 10, x.test = val_x, y.test = val_y, scale = F, center = T)
    
    if (!is.null(m_baseline) && !is.null(m_baseline$ncomp.selected) && m_baseline$ncomp.selected > 0) {
      ncomp_baseline <- m_baseline$ncomp.selected
      
      fold_results_list[['Baseline']] <- data.frame(
        Repeat = i, Fold = k, Method = "Baseline",
        RMSE = m_baseline$testres$rmse[ncomp_baseline], 
        R2 = m_baseline$testres$r2[ncomp_baseline],
        ncomp = ncomp_baseline, nvars = ncol(cal_x)
      )
      
      # --- All metrics calculated EXACTLY as per documentation and your examples ---
      vips <- vipscores(m_baseline)[, 1]
      sr_scores <- selratio(m_baseline, ncomp = ncomp_baseline)
      jk_pvals <- m_baseline$coeffs$p.values[, ncomp_baseline, 1]
      
      models_to_run <- list(
        "VIP < 0.5"   = vips < 0.5,
        "VIP < 1.0"   = vips < 1.0,
        "SR < 0.05"   = sr_scores < 0.05,
        "SR < 0.25"   = sr_scores < 0.25,
        "JK p > 0.05" = jk_pvals > 0.05
      )
      
      for (model_name in names(models_to_run)) {
        excl_list <- models_to_run[[model_name]]
        m_current <- pls(cal_x, cal_y, cv = 10, x.test = val_x, y.test = val_y, exclcols = excl_list, scale = F, center = T)
        ncomp_current <- m_current$ncomp.selected
        
        fold_results_list[[model_name]] <- data.frame(
          Repeat = i, Fold = k, Method = model_name,
          RMSE = m_current$testres$rmse[ncomp_current], 
          R2 = m_current$testres$r2[ncomp_current],
          ncomp = ncomp_current, nvars = ncol(cal_x) - length(which(excl_list))
        )
      }
    } else {
      all_methods <- c("Baseline", "VIP < 0.5", "VIP < 1.0", "SR < 0.05", "SR < 0.25", "JK p > 0.05")
      for(m_name in all_methods) {
        fold_results_list[[m_name]] <- data.frame(Repeat = i, Fold = k, Method = m_name, RMSE = NA, R2 = NA, ncomp = NA, nvars = NA)
      }
    }
    repeat_results_list[[k]] <- bind_rows(fold_results_list)
  }
  bind_rows(repeat_results_list)
}

stopCluster(cl)
cat("Parallel job finished.\n")

# ================================================================= #
# 3. AGGREGATE AND VISUALIZE RESULTS
# ================================================================= #
final_results <- all_results_list

summary_stats <- final_results %>%
  group_by(Method) %>%
  summarise(
    Mean_RMSE = mean(RMSE, na.rm = TRUE), SD_RMSE = sd(RMSE, na.rm = TRUE),
    Mean_R2 = mean(R2, na.rm = TRUE), Mean_ncomp = mean(ncomp, na.rm = TRUE),
    Mean_nvars = mean(nvars, na.rm = TRUE)
  ) %>%
  arrange(Mean_RMSE)

cat("\n--- Summary of Model Performance ---\n")
print(summary_stats)

ggplot(final_results, aes(x = reorder(Method, RMSE, median, na.rm = TRUE), y = RMSE, fill = Method)) +
  geom_boxplot(alpha = 0.8) +
  labs(
    title = "Comparison of PLS Wavenumber Filtering Methods",
    x = "Filtering Method", y = "RMSE (Days)"
  ) +
  theme_bw() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))

















# ================================================================= #
# 1. SETUP: PACKAGES, DATA, AND PRE-FILTERING
# ================================================================= #

# Load necessary packages
library(mdatools)
library(dplyr)
library(ggplot2)

# Load your processed dataframe
df <- readRDS("RDS_dataframes/LPW_scan_avg_proc.RDS")
df <- df[complete.cases(df$read_age), ]

# --- NEW: Pre-filter the dataframe to remove wavenumbers > 7500 ---
cat("Pre-filtering data: removing wavenumbers > 7500 cm-1...\n")
original_wavenumber_count <- ncol(df) - 20
numeric_names <- suppressWarnings(as.numeric(names(df)))
df <- df[, is.na(numeric_names) | numeric_names <= 7500]
filtered_wavenumber_count <- ncol(df) - 20
cat(paste("Original wavenumbers:", original_wavenumber_count, "| Wavenumbers after filtering:", filtered_wavenumber_count, "\n\n"))
# --- End of new section ---

# Define constants
N_REPEATS <- 5
K_FOLDS   <- 10
RESPONSE_VAR <- "read_age"
PREDICTOR_VARS <- 21:ncol(df) # This will adapt automatically to the new df

# ================================================================= #
# 2. SERIAL EXECUTION
# ================================================================= #

all_results_list <- list()

cat("Starting serial job on the pre-filtered dataset...\n")

# Outer loop for the 5 REPEATS
for (i in 1:N_REPEATS) {
  
  set.seed(123 + i)
  folds <- caret::createFolds(df[[RESPONSE_VAR]], k = K_FOLDS, list = TRUE)
  
  # Inner loop for the 10 FOLDS
  for (k in 1:K_FOLDS) {
    
    cat(paste("Running Repeat:", i, "| Fold:", k, "\n"))
    
    cal_indices <- unlist(folds[-k])
    val_indices <- folds[[k]]
    
    cal_data <- df[cal_indices, ]; val_data <- df[val_indices, ]
    cal_x <- cal_data[, PREDICTOR_VARS]; cal_y <- cal_data[[RESPONSE_VAR]]
    val_x <- val_data[, PREDICTOR_VARS]; val_y <- val_data[[RESPONSE_VAR]]
    
    fold_results_list <- list()
    
    m_baseline <- tryCatch({
      pls(cal_x, cal_y, cv = 10, x.test = val_x, y.test = val_y, scale = F, center = T)
    }, error = function(e) { NULL })
    
    if (!is.null(m_baseline) && !is.null(m_baseline$ncomp.selected) && m_baseline$ncomp.selected > 0) {
      ncomp_baseline <- m_baseline$ncomp.selected
      
      fold_results_list[['Baseline']] <- data.frame(
        Repeat = i, Fold = k, Method = "Baseline",
        RMSE = m_baseline$testres$rmse[ncomp_baseline], 
        R2 = m_baseline$testres$r2[ncomp_baseline],
        ncomp = ncomp_baseline, nvars = ncol(cal_x)
      )
      
      vips <- vipscores(m_baseline)[, 1]
      sr_scores <- selratio(m_baseline, ncomp = ncomp_baseline)
      jk_pvals <- m_baseline$coeffs$p.values[, ncomp_baseline, 1]
      
      models_to_run <- list(
        "VIP < 0.5"   = vips < 0.5,
        "VIP < 1.0"   = vips < 1.0,
        "SR < 0.05"   = sr_scores < 0.05,
        "SR < 0.25"   = sr_scores < 0.25,
        "JK p > 0.05" = jk_pvals > 0.05
      )
      
      for (model_name in names(models_to_run)) {
        excl_list <- models_to_run[[model_name]]
        m_current <- pls(cal_x, cal_y, cv = 10, x.test = val_x, y.test = val_y, exclcols = excl_list, scale = F, center = T)
        ncomp_current <- m_current$ncomp.selected
        
        fold_results_list[[model_name]] <- data.frame(
          Repeat = i, Fold = k, Method = model_name,
          RMSE = m_current$testres$rmse[ncomp_current], 
          R2 = m_current$testres$r2[ncomp_current],
          ncomp = ncomp_current, nvars = ncol(cal_x) - length(which(excl_list))
        )
      }
    } else {
      all_methods <- c("Baseline", "VIP < 0.5", "VIP < 1.0", "SR < 0.05", "SR < 0.25", "JK p > 0.05")
      for(m_name in all_methods) {
        fold_results_list[[m_name]] <- data.frame(Repeat = i, Fold = k, Method = m_name, RMSE = NA, R2 = NA, ncomp = NA, nvars = NA)
      }
    }
    all_results_list[[paste0(i, "-", k)]] <- bind_rows(fold_results_list)
  }
}

cat("Serial job finished.\n")

# ================================================================= #
# 3. AGGREGATE AND VISUALIZE RESULTS
# ================================================================= #
final_results <- bind_rows(all_results_list)

summary_stats <- final_results %>%
  group_by(Method) %>%
  summarise(
    Mean_RMSE = mean(RMSE, na.rm = TRUE), SD_RMSE = sd(RMSE, na.rm = TRUE),
    Mean_R2 = mean(R2, na.rm = TRUE), Mean_ncomp = mean(ncomp, na.rm = TRUE),
    Mean_nvars = mean(nvars, na.rm = TRUE)
  ) %>%
  arrange(Mean_RMSE)

cat("\n--- Summary of Model Performance (Wavenumbers <= 7500) ---\n")
print(summary_stats)

ggplot(final_results, aes(x = reorder(Method, RMSE, median, na.rm = TRUE), y = RMSE, fill = Method)) +
  geom_boxplot(alpha = 0.8) +
  labs(
    title = "Comparison of PLS Filtering Methods (Wavenumbers <= 7500)",
    x = "Filtering Method", y = "RMSE (Days)"
  ) +
  theme_bw() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))
