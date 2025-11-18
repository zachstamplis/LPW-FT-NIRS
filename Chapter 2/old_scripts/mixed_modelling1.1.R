library(mdatools)
library(tidyverse)
library(prospectr)
df_raw <- readRDS("RDS_dataframes/combined_IBM_LPW_raw.RDS")
speccols <- names(df_raw)[grepl("^\\d", names(df_raw))]
metacols <- setdiff(names(df_raw), speccols)

# SNV + SG

df_snvsg <- readRDS("RDS_dataframes/combined_IBM_LPW_experimental_SNVandSG.RDS")
df_snvsg <- df_snvsg %>% filter(!is.na(read_age))
speccols_snvsg <- names(df_snvsg)[grepl("^\\d", names(df_snvsg))]
# metacols <- setdiff(names(df_snvsg), speccols_snvsg)
pls_snvsg <- pls(x = as.matrix(df_snvsg[,speccols_snvsg]), 
                 y = as.matrix(df_snvsg[,"read_age"]), 
                 cv = 1, scale = F, center = T)
plot(pls_snvsg)

# ONLY SAVITZKY GOLAY #
df_sg <- savitzkyGolay(as.matrix(df_raw[speccols]), m = 1, p = 3, w = 17)
df_sg <- cbind(df_raw[,metacols],df_sg)
df_sg <- df_sg %>% filter(!is.na(read_age))
speccols_sg <- names(df_sg)[grepl("^\\d", names(df_sg))]
pls_sg <- pls(x = as.matrix(df_sg[,speccols_sg]), 
              y = as.matrix(df_sg[,"read_age"]), 
              cv = 1, scale = F, center = T)
plot(pls_sg, main = "POOP")


# remove > 7500 and SG #
df_filtsg <- savitzkyGolay(as.matrix(df_raw[speccols]), m = 1, p = 3, w = 17)
df_filtsg <- cbind(df_raw[,metacols],df_filtsg)
df_filtsg <- df_filtsg %>% filter(!is.na(read_age))
# 1. Get the names of the spectral columns you want to remove
cols_to_remove <- speccols_sg[as.numeric(speccols_sg) > 7500]
# 2. Use dplyr::select() to remove them from the dataframe
# The "-" sign means remove, and all_of() is used when you have a character vector of names.
df_filtsg <- df_filtsg %>% 
  select(-all_of(cols_to_remove))
rm(cols_to_remove)
speccols_filtsg <- names(df_filtsg)[grepl("^\\d", names(df_filtsg))]
pls_filtsg <- pls(x = as.matrix(df_filtsg[,speccols_filtsg]), 
                  y = as.matrix(df_filtsg[,"read_age"]), 
                  cv = 1, scale = F, center = T)
plot(pls_filtsg)


# now filter VIP < 0.5 and 1.0
vip <- as.data.frame(vipscores(pls_filtsg))
mod_vip05 <- pls(x = as.matrix(df_filtsg[,speccols_filtsg]), 
           y = as.matrix(df_filtsg[,"read_age"]), 
           scale = F, center = T,
           info = "Length Prediction Model",
           exclcols = vip$V1 < 0.5,
           cv = 1
)
plot(mod_vip05)

mod_vip10 <- pls(x = as.matrix(df_filtsg[,speccols_filtsg]), 
                 y = as.matrix(df_filtsg[,"read_age"]), 
                 scale = F, center = T,
                 info = "Length Prediction Model",
                 exclcols = vip$V1 < 1,
                 cv = 1
)
plot(mod_vip10)




#### Now by region ####





df_raw <- readRDS("RDS_dataframes/combined_IBM_LPW_raw.RDS")
unique_regions <- unique(df_raw$region)

# df_raw <- df_raw %>% filter(region == "W Shumagins")

speccols <- names(df_raw)[grepl("^\\d", names(df_raw))]
metacols <- setdiff(names(df_raw), speccols)

# SNV + SG

df_snvsg <- readRDS("RDS_dataframes/combined_IBM_LPW_experimental_SNVandSG.RDS")
df_snvsg <- df_snvsg %>% filter(region == "E Shumagins")
df_snvsg <- df_snvsg %>% filter(!is.na(read_age))
speccols_snvsg <- names(df_snvsg)[grepl("^\\d", names(df_snvsg))]
# metacols <- setdiff(names(df_snvsg), speccols_snvsg)
pls_snvsg <- pls(x = as.matrix(df_snvsg[,speccols_snvsg]), 
                 y = as.matrix(df_snvsg[,"read_age"]), 
                 cv = 1, scale = F, center = T)
plot(pls_snvsg)

# ONLY SAVITZKY GOLAY #
df_sg <- savitzkyGolay(as.matrix(df_raw[speccols]), m = 1, p = 3, w = 17)
df_sg <- cbind(df_raw[,metacols],df_sg)
df_sg <- df_sg %>% filter(!is.na(read_age))
speccols_sg <- names(df_sg)[grepl("^\\d", names(df_sg))]
pls_sg <- pls(x = as.matrix(df_sg[,speccols_sg]), 
              y = as.matrix(df_sg[,"read_age"]), 
              cv = 1, scale = F, center = T)
plot(pls_sg)


# remove > 7500 and SG #
df_filtsg <- savitzkyGolay(as.matrix(df_raw[speccols]), m = 1, p = 3, w = 17)
df_filtsg <- cbind(df_raw[,metacols],df_filtsg)
df_filtsg <- df_filtsg %>% filter(!is.na(read_age))
# 1. Get the names of the spectral columns you want to remove
cols_to_remove <- speccols_sg[as.numeric(speccols_sg) > 7500]
# 2. Use dplyr::select() to remove them from the dataframe
# The "-" sign means remove, and all_of() is used when you have a character vector of names.
df_filtsg <- df_filtsg %>% 
  select(-all_of(cols_to_remove))
rm(cols_to_remove)
speccols_filtsg <- names(df_filtsg)[grepl("^\\d", names(df_filtsg))]
pls_filtsg <- pls(x = as.matrix(df_filtsg[,speccols_filtsg]), 
                  y = as.matrix(df_filtsg[,"read_age"]), 
                  cv = 1, scale = F, center = T)
plot(pls_filtsg)


# now filter VIP < 0.5 and 1.0
vip <- as.data.frame(vipscores(pls_filtsg))
mod_vip05 <- pls(x = as.matrix(df_filtsg[,speccols_filtsg]), 
                 y = as.matrix(df_filtsg[,"read_age"]), 
                 scale = F, center = T,
                 info = "Length Prediction Model",
                 exclcols = vip$V1 < 0.5,
                 cv = 1
)
plot(mod_vip05)

mod_vip10 <- pls(x = as.matrix(df_filtsg[,speccols_filtsg]), 
                 y = as.matrix(df_filtsg[,"read_age"]), 
                 scale = F, center = T,
                 info = "Length Prediction Model",
                 exclcols = vip$V1 < 1,
                 cv = 1
)
plot(mod_vip10)






# FOR LOOP VERSION:


# 1. --- SETUP ---
# Load necessary libraries
library(mdatools)
library(dplyr)
library(prospectr) # For savitzkyGolay

# Load your raw data (assuming it's in a sub-folder)
# Make sure the paths are correct for your project structure
df_raw <- readRDS("RDS_dataframes/combined_IBM_LPW_raw.RDS")
df_snvsg <- readRDS("RDS_dataframes/combined_IBM_LPW_experimental_SNVandSG.RDS")

# Get unique regions to loop over
unique_regions <- unique(df_raw$region)

# Define spectral and metadata columns from the raw data
speccols <- names(df_raw)[grepl("^\\d", names(df_raw))]
metacols <- setdiff(names(df_raw), speccols)

# Create a directory to save the plots. The 'if' statement prevents errors if it already exists.
if (!dir.exists("pls_plots")) {
  dir.create("pls_plots")
}

# Create an empty data frame to store results from all models
results_summary <- data.frame(
  Region = character(),
  ModelType = character(),
  nComp = integer(),
  R2 = numeric(),
  RMSE = numeric(),
  Bias = numeric(),
  RPD = numeric(),
  stringsAsFactors = FALSE
)


# 2. --- THE FOR LOOP ---
# Loop over each unique region
for (current_region in unique_regions) {
  
  message(paste("Processing models for region:", current_region))
  
  # --- DATA PREPARATION FOR THE CURRENT REGION ---
  
  # Filter the main dataframes for the current region
  # The '!!' is important when using a variable within a dplyr pipe
  region_df_raw <- df_raw %>% filter(region == !!current_region, !is.na(read_age))
  region_df_snvsg <- df_snvsg %>% filter(region == !!current_region, !is.na(read_age))
  
  # Skip this region if it has too few samples to model (e.g., less than 10)
  if(nrow(region_df_raw) < 10) {
    message(paste("  Skipping", current_region, "due to insufficient data (", nrow(region_df_raw), "samples)."))
    next # 'next' skips to the next iteration of the loop
  }
  
  # --- MODEL 1: SNV + Savitzky-Golay ---
  message("  Building SNV + SG model...")
  speccols_snvsg <- names(region_df_snvsg)[grepl("^\\d", names(region_df_snvsg))]
  
  m_snvsg <- pls(
    x = as.matrix(region_df_snvsg[, speccols_snvsg]),
    y = as.matrix(region_df_snvsg[, "read_age"]),
    cv = 1, scale = F, center = T, info = paste(current_region, "SNV+SG")
  )
  
  # Save plot
  pdf(paste0("pls_plots/", current_region, "_1_SNV_SG.pdf"))
  plot(m_snvsg, main = paste(current_region, "- SNV+SG Model"))
  dev.off()
  
  # Extract results
  ncomp <- m_snvsg$ncomp.selected
  results_summary <- rbind(results_summary, data.frame(
    Region = current_region,
    ModelType = "SNV+SG",
    nComp = ncomp,
    R2 = m_snvsg$res$cv$r2[, ncomp],
    RMSE = m_snvsg$res$cv$rmse[, ncomp],
    Bias = m_snvsg$res$cv$bias[, ncomp],
    RPD = m_snvsg$res$cv$rpd[, ncomp]
  ))
  
  
  # --- MODEL 2: Savitzky-Golay Only ---
  message("  Building SG Only model...")
  df_sg <- savitzkyGolay(as.matrix(region_df_raw[speccols]), m = 1, p = 3, w = 17)
  df_sg <- cbind(region_df_raw[, metacols], df_sg)
  speccols_sg <- names(df_sg)[grepl("^\\d", names(df_sg))]
  
  m_sg <- pls(
    x = as.matrix(df_sg[, speccols_sg]),
    y = as.matrix(df_sg[, "read_age"]),
    cv = 1, scale = F, center = T, info = paste(current_region, "SG Only")
  )
  
  # Save plot
  pdf(paste0("pls_plots/", current_region, "_2_SG_Only.pdf"))
  plot(m_sg, main = paste(current_region, "- SG Only Model"))
  dev.off()
  
  # Extract results
  ncomp <- m_sg$ncomp.selected
  results_summary <- rbind(results_summary, data.frame(
    Region = current_region,
    ModelType = "SG Only",
    nComp = ncomp,
    R2 = m_sg$res$cv$r2[, ncomp],
    RMSE = m_sg$res$cv$rmse[, ncomp],
    Bias = m_sg$res$cv$bias[, ncomp],
    RPD = m_sg$res$cv$rpd[, ncomp]
  ))
  
  
  # --- MODEL 3: Filtered (>7500nm) + Savitzky-Golay ---
  message("  Building Filtered SG model...")
  # We can reuse df_sg from the previous step
  cols_to_remove <- speccols_sg[as.numeric(speccols_sg) > 7500]
  df_filtsg <- df_sg %>% select(-all_of(cols_to_remove))
  speccols_filtsg <- names(df_filtsg)[grepl("^\\d", names(df_filtsg))]
  
  m_filtsg <- pls(
    x = as.matrix(df_filtsg[, speccols_filtsg]),
    y = as.matrix(df_filtsg[, "read_age"]),
    cv = 1, scale = F, center = T, info = paste(current_region, "Filtered SG")
  )
  
  # Save plot
  pdf(paste0("pls_plots/", current_region, "_3_Filtered_SG.pdf"))
  plot(m_filtsg, main = paste(current_region, "- Filtered SG Model"))
  dev.off()
  
  # Extract results
  ncomp <- m_filtsg$ncomp.selected
  results_summary <- rbind(results_summary, data.frame(
    Region = current_region,
    ModelType = "Filtered SG",
    nComp = ncomp,
    R2 = m_filtsg$res$cv$r2[, ncomp],
    RMSE = m_filtsg$res$cv$rmse[, ncomp],
    Bias = m_filtsg$res$cv$bias[, ncomp],
    RPD = m_filtsg$res$cv$rpd[, ncomp]
  ))
  
  # Calculate VIP scores from this model for the next two steps
  vip <- as.data.frame(vipscores(m_filtsg))
  
  
  # --- MODEL 4: Filtered SG + VIP > 0.5 ---
  message("  Building VIP > 0.5 model...")
  m_vip05 <- pls(
    x = as.matrix(df_filtsg[, speccols_filtsg]),
    y = as.matrix(df_filtsg[, "read_age"]),
    scale = F, center = T, cv = 1,
    info = paste(current_region, "VIP > 0.5"),
    exclcols = vip$V1 < 0.5
  )
  
  # Save plot
  pdf(paste0("pls_plots/", current_region, "_4_VIP05.pdf"))
  plot(m_vip05, main = paste(current_region, "- VIP > 0.5 Model"))
  dev.off()
  
  # Extract results
  ncomp <- m_vip05$ncomp.selected
  results_summary <- rbind(results_summary, data.frame(
    Region = current_region,
    ModelType = "VIP > 0.5",
    nComp = ncomp,
    R2 = m_vip05$res$cv$r2[, ncomp],
    RMSE = m_vip05$res$cv$rmse[, ncomp],
    Bias = m_vip05$res$cv$bias[, ncomp],
    RPD = m_vip05$res$cv$rpd[, ncomp]
  ))
  
  
  # --- MODEL 5: Filtered SG + VIP > 1.0 ---
  message("  Building VIP > 1.0 model...")
  m_vip10 <- pls(
    x = as.matrix(df_filtsg[, speccols_filtsg]),
    y = as.matrix(df_filtsg[, "read_age"]),
    scale = F, center = T, cv = 1,
    info = paste(current_region, "VIP > 1.0"),
    exclcols = vip$V1 < 1.0
  )
  
  # Save plot
  pdf(paste0("pls_plots/", current_region, "_5_VIP10.pdf"))
  plot(m_vip10, main = paste(current_region, "- VIP > 1.0 Model"))
  dev.off()
  
  # Extract results
  ncomp <- m_vip10$ncomp.selected
  results_summary <- rbind(results_summary, data.frame(
    Region = current_region,
    ModelType = "VIP > 1.0",
    nComp = ncomp,
    R2 = m_vip10$res$cv$r2[, ncomp],
    RMSE = m_vip10$res$cv$rmse[, ncomp],
    Bias = m_vip10$res$cv$bias[, ncomp],
    RPD = m_vip10$res$cv$rpd[, ncomp]
  ))
  
} # End of the for loop

# 3. --- VIEW AND SAVE RESULTS ---
# Print the final summary table to the console
print(results_summary)

# Optionally, save the summary table to a CSV file for later use
write.csv(results_summary, "pls_model_performance_summary.csv", row.names = FALSE)













# NOW SEPERATE REGIONS FOR TEST/CALIBRATION


# 1. --- SETUP ---
# Load necessary libraries
library(mdatools)
library(dplyr)
library(prospectr)

# Load your raw data
df_raw <- readRDS("RDS_dataframes/combined_IBM_LPW_raw.RDS")
df_snvsg <- readRDS("RDS_dataframes/combined_IBM_LPW_experimental_SNVandSG.RDS")

# Get unique regions to loop over
unique_regions <- unique(df_raw$region)

# Define base spectral and metadata columns
speccols <- names(df_raw)[grepl("^\\d", names(df_raw))]
metacols <- setdiff(names(df_raw), speccols)

# Create an empty data frame to store results
results_summary <- data.frame(
  Cal_Region = character(), Val_Region = character(), ModelType = character(),
  nComp = integer(), R2 = numeric(), RMSE = numeric(), Bias = numeric(),
  RPD = numeric(), stringsAsFactors = FALSE
)

# --- ROBUST PLOTTING SETUP ---
# Ensure all graphics devices are closed before starting a new one.
while (!is.null(dev.list())) {
  dev.off()
}

# Open the single PDF file for ALL plots
pdf("all_paired_model_plots_FINAL.pdf", width = 8.5, height = 11)


# 2. --- THE NESTED FOR LOOPS ---
for (cal_region in unique_regions) {
  message(paste("----- STARTING CALIBRATION REGION:", cal_region, "-----"))
  
  # Prepare CALIBRATION data
  cal_df_raw <- df_raw %>% filter(region == !!cal_region, !is.na(read_age))
  if(nrow(cal_df_raw) < 10) { message("  Skipping cal region..."); next }
  cal_df_snvsg <- df_snvsg %>% filter(region == !!cal_region, !is.na(read_age))
  speccols_snvsg <- names(cal_df_snvsg)[grepl("^\\d", names(cal_df_snvsg))]
  Xc_snvsg <- as.matrix(cal_df_snvsg[, speccols_snvsg])
  Yc_snvsg <- as.matrix(cal_df_snvsg[, "read_age"])
  cal_sg_matrix <- savitzkyGolay(as.matrix(cal_df_raw[speccols]), m = 1, p = 3, w = 17)
  cal_df_sg <- cbind(cal_df_raw[, metacols], cal_sg_matrix)
  speccols_sg <- colnames(cal_sg_matrix)
  Xc_sg <- as.matrix(cal_df_sg[, speccols_sg])
  Yc_sg <- as.matrix(cal_df_sg[, "read_age"])
  cols_to_remove <- speccols_sg[as.numeric(speccols_sg) > 7500]
  speccols_filtsg <- setdiff(speccols_sg, cols_to_remove)
  Xc_filtsg <- as.matrix(cal_df_sg[, speccols_filtsg])
  Yc_filtsg <- as.matrix(cal_df_sg[, "read_age"])
  
  # Inner loop for VALIDATION set
  for (val_region in unique_regions) {
    if (cal_region == val_region) { next }
    message(paste("  Validating against:", val_region))
    
    # Prepare VALIDATION data
    val_df_raw <- df_raw %>% filter(region == !!val_region, !is.na(read_age))
    if(nrow(val_df_raw) < 10) { message("    Skipping val region..."); next }
    val_df_snvsg <- df_snvsg %>% filter(region == !!val_region, !is.na(read_age))
    Xt_snvsg <- as.matrix(val_df_snvsg[, speccols_snvsg])
    Yt_snvsg <- as.matrix(val_df_snvsg[, "read_age"])
    val_sg_matrix <- savitzkyGolay(as.matrix(val_df_raw[speccols]), m = 1, p = 3, w = 17)
    Xt_sg <- as.matrix(cbind(val_df_raw[, metacols], val_sg_matrix)[, speccols_sg])
    Yt_sg <- as.matrix(val_df_raw[, "read_age"])
    Xt_filtsg <- as.matrix(cbind(val_df_raw[, metacols], val_sg_matrix)[, speccols_filtsg])
    Yt_filtsg <- as.matrix(val_df_raw[, "read_age"])
    
    # This is a helper function to avoid repeating the title page code
    make_title_page <- function(title_text) {
      plot.new() # Creates a blank page
      # Places the text in the center of the blank page
      text(x = 0.5, y = 0.5, labels = title_text, cex = 1.5, font = 2)
    }
    
    # --- MODELS ---
    # The structure for each model is now:
    # 1. Build the model.
    # 2. Call make_title_page() to create a labeled page.
    # 3. Call plot() to create the 4-panel plot on the next page.
    # 4. Extract results.
    
    # --- MODEL 1: SNV + SG ---
    tryCatch({
      m <- pls(x = Xc_snvsg, y = Yc_snvsg, cv = 1, scale = F, center = T, x.test = Xt_snvsg, y.test = Yt_snvsg)
      make_title_page(paste("Calibration:", cal_region, "\nValidation:", val_region, "\n\nModel: SNV + SG"))
      plot(m)
      ncomp <- m$ncomp.selected
      results_summary <- rbind(results_summary, data.frame(
        Cal_Region = cal_region, Val_Region = val_region, ModelType = "SNV+SG", nComp = ncomp,
        R2 = m$res$test$r2[, ncomp], RMSE = m$res$test$rmse[, ncomp],
        Bias = m$res$test$bias[, ncomp], RPD = m$res$test$rpd[, ncomp]))
    }, error = function(e) { message(paste("    ERROR in SNV+SG:", e$message)) })
    
    # --- MODEL 2: SG Only ---
    tryCatch({
      m <- pls(x = Xc_sg, y = Yc_sg, cv = 1, scale = F, center = T, x.test = Xt_sg, y.test = Yt_sg)
      make_title_page(paste("Calibration:", cal_region, "\nValidation:", val_region, "\n\nModel: SG Only"))
      plot(m)
      ncomp <- m$ncomp.selected
      results_summary <- rbind(results_summary, data.frame(
        Cal_Region = cal_region, Val_Region = val_region, ModelType = "SG Only", nComp = ncomp,
        R2 = m$res$test$r2[, ncomp], RMSE = m$res$test$rmse[, ncomp],
        Bias = m$res$test$bias[, ncomp], RPD = m$res$test$rpd[, ncomp]))
    }, error = function(e) { message(paste("    ERROR in SG Only:", e$message)) })
    
    # --- MODEL 3: Filtered SG ---
    m_filtsg <- NULL
    tryCatch({
      m_filtsg <- pls(x = Xc_filtsg, y = Yc_filtsg, cv = 1, scale = F, center = T, x.test = Xt_filtsg, y.test = Yt_filtsg)
      make_title_page(paste("Calibration:", cal_region, "\nValidation:", val_region, "\n\nModel: Filtered SG"))
      plot(m_filtsg)
      ncomp <- m_filtsg$ncomp.selected
      results_summary <- rbind(results_summary, data.frame(
        Cal_Region = cal_region, Val_Region = val_region, ModelType = "Filtered SG", nComp = ncomp,
        R2 = m_filtsg$res$test$r2[, ncomp], RMSE = m_filtsg$res$test$rmse[, ncomp],
        Bias = m_filtsg$res$test$bias[, ncomp], RPD = m_filtsg$res$test$rpd[, ncomp]))
    }, error = function(e) { message(paste("    ERROR in Filtered SG:", e$message)) })
    
    # --- VIP Models ---
    if (!is.null(m_filtsg)) {
      vip <- as.data.frame(vipscores(m_filtsg))

      tryCatch({
        m <- pls(x = Xc_filtsg, y = Yc_filtsg, cv = 1, scale = F, center = T, x.test = Xt_filtsg, y.test = Yt_filtsg, exclcols = vip$V1 < 0.5)
        make_title_page(paste("Calibration:", cal_region, "\nValidation:", val_region, "\n\nModel: VIP > 0.5"))
        plot(m)
        ncomp <- m$ncomp.selected
        results_summary <- rbind(results_summary, data.frame(
          Cal_Region = cal_region, Val_Region = val_region, ModelType = "VIP > 0.5", nComp = ncomp,
          R2 = m$res$test$r2[, ncomp], RMSE = m$res$test$rmse[, ncomp],
          Bias = m$res$test$bias[, ncomp], RPD = m$res$test$rpd[, ncomp]))
      }, error = function(e) { message(paste("    ERROR in VIP > 0.5:", e$message)) })

      tryCatch({
        m <- pls(x = Xc_filtsg, y = Yc_filtsg, cv = 1, scale = F, center = T, x.test = Xt_filtsg, y.test = Yt_filtsg, exclcols = vip$V1 < 1.0)
        make_title_page(paste("Calibration:", cal_region, "\nValidation:", val_region, "\n\nModel: VIP > 1.0"))
        plot(m)
        ncomp <- m$ncomp.selected
        results_summary <- rbind(results_summary, data.frame(
          Cal_Region = cal_region, Val_Region = val_region, ModelType = "VIP > 1.0", nComp = ncomp,
          R2 = m$res$test$r2[, ncomp], RMSE = m$res$test$rmse[, ncomp],
          Bias = m$res$test$bias[, ncomp], RPD = m$res$test$rpd[, ncomp]))
      }, error = function(e) { message(paste("    ERROR in VIP > 1.0:", e$message)) })
    }
  } # End inner loop
} # End outer loop

# Close the PDF device. This is the critical step that writes the file.
dev.off()
message("All plots saved to 'all_paired_model_plots_FINAL.pdf'")

# 3. --- VIEW AND SAVE RESULTS ---
print(results_summary)
write.csv(results_summary, "pls_model_paired_performance_summary_FINAL.csv", row.names = FALSE)






































# 1. --- SETUP ---
# Load necessary libraries
library(mdatools)
library(dplyr)
library(prospectr)
library(caret)

# Load BOTH dataframes. We'll use the indices from df_raw to split both.
df_raw <- readRDS("RDS_dataframes/combined_IBM_LPW_raw.RDS")
df_snvsg <- readRDS("RDS_dataframes/combined_IBM_LPW_experimental_SNVandSG.RDS")

# CRITICAL: Ensure both dataframes have the exact same rows in the same order
df_raw <- df_raw %>% filter(!is.na(read_age))
df_snvsg <- df_snvsg %>% filter(!is.na(read_age))

# --- Get unique regions and define constants ---
unique_regions <- unique(df_raw$region)
k <- 10 # Number of folds

# --- Prepare for outputs ---
if (!dir.exists("pls_plots_final_10fold")) {
  dir.create("pls_plots_final_10fold")
}
results_summary <- data.frame(
  Fold = integer(), ModelType = character(), nComp = integer(), R2 = numeric(),
  RMSE = numeric(), Bias = numeric(), RPD = numeric(), stringsAsFactors = FALSE
)

# 2. --- CREATE STRATIFIED DATA LISTS (FINAL CORRECTED METHOD) ---

# CRITICAL FIX: Add a guaranteed unique identifier to BOTH dataframes *immediately* after loading.
# This ensures the ID exists for all subsequent steps. Using a row-based ID assumes
# that after filtering, row 5 in df_raw is the same specimen as row 5 in df_snvsg.
# If you have a real specimen ID column, using that is even more robust.
df_raw$unique_row_id <- 1:nrow(df_raw)
df_snvsg$unique_row_id <- 1:nrow(df_snvsg)

message("Generating stratified fold lists using a direct dataframe-based method...")
k <- 10

# Initialize the master lists for the RAW data
combined_cal_list_raw <- vector("list", k)
combined_test_list_raw <- vector("list", k)

# Start with the list of regional dataframes from df_raw
list_of_regional_dfs <- split(df_raw, df_raw$region)

# Loop through each REGION's dataframe
for (region_name in names(list_of_regional_dfs)) {
  
  message(paste("... Processing splits for region:", region_name))
  current_region_df <- list_of_regional_dfs[[region_name]]
  
  # Create the 10 sets of TEST indices for THIS REGION ONLY
  test_indices_for_region <- createFolds(current_region_df$read_age, k = k, returnTrain = FALSE, list = TRUE)
  
  # Create lists of the actual dataframes for this region's folds
  regional_test_dfs <- lapply(test_indices_for_region, function(indices) current_region_df[indices, ])
  regional_cal_dfs <- lapply(test_indices_for_region, function(indices) current_region_df[-indices, ])
  
  # Merge these regional lists into the master lists for the RAW data
  for (i in seq_along(regional_test_dfs)) {
    combined_test_list_raw[[i]] <- bind_rows(combined_test_list_raw[[i]], regional_test_dfs[[i]])
    combined_cal_list_raw[[i]] <- bind_rows(combined_cal_list_raw[[i]], regional_cal_dfs[[i]])
  }
}
message("Raw data lists created successfully.")

# NOW, create the corresponding lists for the SNV+SG dataframe.
# This will work because the dataframes in combined_cal_list_raw now contain the 'unique_row_id' column.
message("Creating corresponding SNV+SG data lists by matching unique_row_id...")
snvsg_cal_list <- lapply(combined_cal_list_raw, function(df) {
  # For each raw dataframe, filter the main snvsg dataframe to get the matching rows
  df_snvsg %>% filter(unique_row_id %in% df$unique_row_id)
})
snvsg_test_list <- lapply(combined_test_list_raw, function(df) {
  df_snvsg %>% filter(unique_row_id %in% df$unique_row_id)
})
message("All data lists created successfully and are guaranteed to be aligned.")

# You can now rename the raw lists for the modeling loop if you wish
raw_cal_list <- combined_cal_list_raw
raw_test_list <- combined_test_list_raw

# Optional: clean up the temporary ID column if you don't need it later
raw_cal_list <- lapply(raw_cal_list, function(df) select(df, -unique_row_id))
raw_test_list <- lapply(raw_test_list, function(df) select(df, -unique_row_id))
snvsg_cal_list <- lapply(snvsg_cal_list, function(df) select(df, -unique_row_id))
snvsg_test_list <- lapply(snvsg_test_list, function(df) select(df, -unique_row_id))








# 3. --- MAIN 10-FOLD CV MODELING LOOP ---
# This loop will now take the pre-made lists of dataframes and process them one fold at a time.
# For each fold, it builds all 5 model types, generates plots, and records the results.


pdf("pls_plots_final_10fold/all_models_10fold_cv_plots.pdf", width = 8.5, height = 11)

for (i in 1:k) {
  message(paste0("\n--- Processing Fold ", i, "/", k, " ---"))
  
  # Get the data for the current fold from our lists
  cal_raw <- raw_cal_list[[i]]
  test_raw <- raw_test_list[[i]]
  cal_snvsg <- snvsg_cal_list[[i]]
  test_snvsg <- snvsg_test_list[[i]]
  
  make_title_page <- function(title_text) {
    plot.new(); text(x = 0.5, y = 0.5, labels = title_text, cex = 1.5, font = 2)
  }
  
  # --- MODEL 1: SNV + SG (using pre-processed data) ---
  tryCatch({
    message("  Building SNV + SG model...")
    speccols_snvsg <- names(cal_snvsg)[grepl("^\\d", names(cal_snvsg))]
    m <- pls(
      x = as.matrix(cal_snvsg[, speccols_snvsg]), y = as.matrix(cal_snvsg[, "read_age"]),
      x.test = as.matrix(test_snvsg[, speccols_snvsg]), y.test = as.matrix(test_snvsg[, "read_age"]),
      cv = 1, scale = F, center = T
    )
    make_title_page(paste("Fold:", i, "\n\nModel: SNV + SG"))
    plot(m)
    ncomp <- m$ncomp.selected
    results_summary <- rbind(results_summary, data.frame(
      Fold = i, ModelType = "SNV+SG", nComp = ncomp, R2 = m$res$test$r2[, ncomp],
      RMSE = m$res$test$rmse[, ncomp], Bias = m$res$test$bias[, ncomp], RPD = m$res$test$rpd[, ncomp]
    ))
  }, error = function(e) { message(paste("    ERROR in SNV+SG model:", e$message)) })
  
  # --- Data Prep for Models 2-5 (from raw data) ---
  speccols_raw <- names(cal_raw)[grepl("^\\d", names(cal_raw))]
  cal_sg_matrix <- savitzkyGolay(as.matrix(cal_raw[, speccols_raw]), m = 1, p = 3, w = 17)
  test_sg_matrix <- savitzkyGolay(as.matrix(test_raw[, speccols_raw]), m = 1, p = 3, w = 17)
  speccols_sg <- colnames(cal_sg_matrix)
  
  # --- MODEL 2: SG Only ---
  tryCatch({
    message("  Building SG Only model...")
    m <- pls(
      x = cal_sg_matrix, y = as.matrix(cal_raw[, "read_age"]),
      x.test = test_sg_matrix, y.test = as.matrix(test_raw[, "read_age"]),
      cv = 1, scale = F, center = T
    )
    make_title_page(paste("Fold:", i, "\n\nModel: SG Only"))
    plot(m)
    ncomp <- m$ncomp.selected
    results_summary <- rbind(results_summary, data.frame(
      Fold = i, ModelType = "SG Only", nComp = ncomp, R2 = m$res$test$r2[, ncomp],
      RMSE = m$res$test$rmse[, ncomp], Bias = m$res$test$bias[, ncomp], RPD = m$res$test$rpd[, ncomp]
    ))
  }, error = function(e) { message(paste("    ERROR in SG Only model:", e$message)) })
  
  # --- MODEL 3: Filtered SG (>7500nm) ---
  m_filtsg <- NULL
  tryCatch({
    message("  Building Filtered SG model...")
    cols_to_remove <- speccols_sg[as.numeric(speccols_sg) > 7500]
    speccols_filtsg <- setdiff(speccols_sg, cols_to_remove)
    
    m_filtsg <- pls(
      x = cal_sg_matrix[, speccols_filtsg], y = as.matrix(cal_raw[, "read_age"]),
      x.test = test_sg_matrix[, speccols_filtsg], y.test = as.matrix(test_raw[, "read_age"]),
      cv = 1, scale = F, center = T
    )
    make_title_page(paste("Fold:", i, "\n\nModel: Filtered SG"))
    plot(m_filtsg)
    ncomp <- m_filtsg$ncomp.selected
    results_summary <- rbind(results_summary, data.frame(
      Fold = i, ModelType = "Filtered SG", nComp = ncomp, R2 = m_filtsg$res$test$r2[, ncomp],
      RMSE = m_filtsg$res$test$rmse[, ncomp], Bias = m_filtsg$res$test$bias[, ncomp], RPD = m_filtsg$res$test$rpd[, ncomp]
    ))
  }, error = function(e) { message(paste("    ERROR in Filtered SG model:", e$message)) })
  
  # --- MODELS 4 & 5: VIP Selection (based on Model 3) ---
  if (!is.null(m_filtsg)) {
    vip <- as.data.frame(vipscores(m_filtsg))
    
    
    # Model 4: VIP > 0.5
    tryCatch({
      message("  Building VIP > 0.5 model...")
      m <- pls(
        x = cal_sg_matrix[, speccols_filtsg], y = as.matrix(cal_raw[, "read_age"]),
        x.test = test_sg_matrix[, speccols_filtsg], y.test = as.matrix(test_raw[, "read_age"]),
        cv = 1, scale = F, center = T, exclcols = vip$V1 < 0.5
      )
      make_title_page(paste("Fold:", i, "\n\nModel: VIP > 0.5"))
      plot(m)
      ncomp <- m$ncomp.selected
      results_summary <- rbind(results_summary, data.frame(
        Fold = i, ModelType = "VIP > 0.5", nComp = ncomp, R2 = m$res$test$r2[, ncomp],
        RMSE = m$res$test$rmse[, ncomp], Bias = m$res$test$bias[, ncomp], RPD = m$res$test$rpd[, ncomp]
      ))
    }, error = function(e) { message(paste("    ERROR in VIP > 0.5 model:", e$message)) })
    
    # Model 5: VIP > 1.0
    tryCatch({
      message("  Building VIP > 1.0 model...")
      m <- pls(
        x = cal_sg_matrix[, speccols_filtsg], y = as.matrix(cal_raw[, "read_age"]),
        x.test = test_sg_matrix[, speccols_filtsg], y.test = as.matrix(test_raw[, "read_age"]),
        cv = 1, scale = F, center = T, exclcols = vip$V1 < 1.0
      )
      make_title_page(paste("Fold:", i, "\n\nModel: VIP > 1.0"))
      plot(m)
      ncomp <- m$ncomp.selected
      results_summary <- rbind(results_summary, data.frame(
        Fold = i, ModelType = "VIP > 1.0", nComp = ncomp, R2 = m$res$test$r2[, ncomp],
        RMSE = m$res$test$rmse[, ncomp], Bias = m$res$test$bias[, ncomp], RPD = m$res$test$rpd[, ncomp]
      ))
    }, error = function(e) { message(paste("    ERROR in VIP > 1.0 model:", e$message)) })
  }
} # End of the main for loop

dev.off()
message("\nAll plots saved to 'pls_plots_final_10fold/all_models_10fold_cv_plots.pdf'")


# 4. --- VIEW AND SAVE AGGREGATED RESULTS ---
summary_stats <- results_summary %>%
  group_by(ModelType) %>%
  summarise(
    Avg_R2 = mean(R2, na.rm = TRUE), SD_R2 = sd(R2, na.rm = TRUE),
    Avg_RMSE = mean(RMSE, na.rm = TRUE), SD_RMSE = sd(RMSE, na.rm = TRUE),
    Avg_RPD = mean(RPD, na.rm = TRUE), SD_RPD = sd(RPD, na.rm = TRUE),
    .groups = 'drop'
  )

message("\n--- Average Performance Across 10 Folds ---")
print(as.data.frame(summary_stats))

write.csv(results_summary, "pls_10fold_cv_results_raw.csv", row.names = FALSE)
write.csv(summary_stats, "pls_10fold_cv_results_summary.csv", row.names = FALSE)






