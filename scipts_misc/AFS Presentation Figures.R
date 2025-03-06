
############################### spectra and length ################################################


df <- readRDS("RDS_dataframes/LPW_scan_avg_proc.RDS")
df_long <- pivot_longer(df, cols = `11472`:`4016`, names_to = "name", values_to = "value")
df_long$name <- as.numeric(df_long$name)


ggplot(df_long, aes(x = name, y = value, group = specimen, color = length)) + 
  geom_path() + 
  scale_x_reverse() + 
  scale_color_viridis() + 
  labs(color = "Fork Length (mm)",
       y = "Raw absorbance", x = "") + 
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 15),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 17)) 
# +  
  # geom_vline(xintercept = 7500, col = "red", linewidth = 2, linetype = "dashed")

library(ggthemes)
# NEED TO FIX THE FUCKING LEGEND
spectra_length <- ggplot(df_long, aes(x = name, y = value, group = specimen, color = length)) + 
  geom_path() + 
  scale_x_reverse(expand = c(0, 0)) + 
  scale_color_viridis() + 
  labs(color = "Fork Length (mm)") + 
  labs(y = "Preprocessed absorbance", x = expression(paste("Wavenumber ", cm^-1))) +
  theme_stata(base_size = 18,scheme = "s1mono") + 
  theme(legend.text = element_text(size = 11),
        legend.title = element_text(size = 18, vjust = 1),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 13),
        legend.position = "inside",
        legend.position.inside = c(.01,.99),
        legend.justification = c(0, 1),
        legend.box.background = element_rect(fill = "transparent", color = NA),
        legend.key = element_rect(fill = "transparent", color = NA)) 
  
spectra_length
ggsave("spectra_length.png", spectra_length, width = 10, height = 5.625, dpi = 300)


################################### Grow over time and sample date ################################################


theme(
  legend.text = element_text(angle = 90, vjust = -.01),
  # Make axis titles the same size as legend title
  axis.title = element_text(size = 12),
  legend.title = element_text(size = 12),
  # Optional: adjust axis text size if needed
  axis.text = element_text(size = 10)
)

# Load required packages
library(ggplot2)
library(dplyr)
library(lubridate)

# Convert Julian days to dates in a non-leap year (e.g., 2021)
df <- df %>%
  mutate(date = as.Date(sample_date - 1, origin = "2020-01-01"))

# Create the plot with proper date formatting
growth_over_time <- ggplot(df, aes(x = date, y = length)) +
  geom_point(size = 3, alpha = 0.9) +
  geom_smooth(method = "lm", se = T, color = "black") +
  scale_x_date(
    name = "Month",
    date_labels = "%b",
    date_breaks = "1 month",
    expand = c(0.01, 0)
  ) +
  labs(
    y = "Fork Length (mm)"
  ) +
  theme_stata(base_size = 34,scheme = "s1mono")
ggsave("growth_over_time.png", growth_over_time, width = 10, height = 5.625, dpi = 300)






###### MAP

library("ggplot2")
# theme_set(theme_bw())
library("sf")
library(ggthemes)
library(ggrepel)
library("rnaturalearth")
library("rnaturalearthdata")
library(rnaturalearthhires)
library (ggspatial)
library(cowplot)

world <- ne_countries(scale = 10, returnclass = "sf")
worldbig <- ne_countries(scale = "medium", returnclass = "sf")


sites <- read.csv("/Users/zachstamplis/Desktop/Thesis and Otoliths/Coding/IBM/IBM_sites.csv")

places <- data.frame (
  lat = c(56.384167, 57.790001),
  lon = c(-134.6433, -152.407227),
  city = c("Little Port Walter", ""))



################################################  LPW ONLY CLEANER FROM AI ##########################################
# Load required packages if not already loaded
library(ggplot2)
library(ggrepel)
library(sf)
library(cowplot)

# Create the main Southeast Alaska map
se_alaska.map <- ggplot(data = world) +
  # Base map with no graticules
  geom_sf(color = "black", fill = "#E5F0EA") +
  
  # Coordinates for Southeast Alaska
  coord_sf(
    xlim = c(-140, -129),
    ylim = c(54.5, 60),
    expand = FALSE, 
    label_axes = ""
  ) + 
  
  # Add cities as points
  geom_point(
    data = cities,
    aes(x = lon, y = lat, color = is_lpw, size = is_lpw)
  ) +
  
  # Add city labels with backgrounds and larger text
  geom_label_repel(
    data = cities,
    aes(x = lon, y = lat, label = name, color = is_lpw),
    size = 10,
    box.padding = 0.5,
    min.segment.length = 0.2,
    max.overlaps = 20,
    nudge_x = -1,
    nudge_y = -0.2,
    fill = alpha("white", 0.7),
    label.padding = unit(0.15, "lines"),
    label.r = unit(0.15, "lines"),
    segment.color = "black"
  ) +
  
  # Set colors and sizes
  scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red")) +
  scale_size_manual(values = c("FALSE" = 2, "TRUE" = 3)) +
  
  # Theme with white background and no border
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    text = element_text(family = "serif", size = 18),
    legend.position = "none"
  )

# Create the inset map of all of Alaska with proper annotation
alaska_inset <- ggplot(data = worldbig) +
  geom_sf(color = "black", fill = "#E5F0EA") +
  coord_sf(
    xlim = c(-180, -129),  # Full Alaska longitude range
    ylim = c(51, 72),      # Full Alaska latitude range
    expand = FALSE, 
    label_axes = ""
  ) + 
  # Use annotate instead of geom_rect to avoid the warning
  annotate(
    "rect",
    xmin = -137, xmax = -129,
    ymin = 54.5, ymax = 60,
    fill = NA, 
    color = "red", 
    linewidth = 1
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)  # Add border
  )

# Combine the main map with a better sized inset in the bottom left
combined_map <- ggdraw(se_alaska.map) +
  draw_plot(
    alaska_inset,
    x = 0.65,      # Position from left
    y = 0.65,      # Position from bottom
    width = 0.33,  # Medium size width
    height = 0.33  # Medium size height
  )
combined_map
# Save the combined map
ggsave(
  "se_alaska_map_with_inset.png",
  combined_map,
  width = 6.5,
  height = 6,
  dpi = 300,
  bg = "white"
)



################################################  MODEL PERFORMANCE  ################################################


library(ggplot2)
library(dplyr)
library(gridExtra)
library(caret)

# Function to calculate metrics per fold, then average
calculate_metrics_by_fold <- function(all_predictions) {
  # Split predictions by model and fold
  metrics_by_fold <- all_predictions %>%
    group_by(model, fold) %>%
    summarize(
      RMSE = caret::RMSE(pred = predicted, obs = observed),
      RSS = sum((observed - predicted)^2),
      TSS = sum((observed - mean(observed))^2),
      R2 = 1 - (RSS / TSS),
      .groups = "drop"
    )
  
  # Average metrics across folds for each model
  avg_metrics <- metrics_by_fold %>%
    group_by(model) %>%
    summarize(
      RMSE = mean(RMSE),
      R2 = mean(R2),
      .groups = "drop"
    ) %>%
    arrange(RMSE)
  
  return(avg_metrics)
}

# Function to extract LM and GAM predictions by fold
extract_lm_gam_predictions <- function(mods.lm, mods.gam, test_data_list) {
  # Create a list to store all predictions
  all_preds <- list()
  
  # Extract LM predictions
  lm_preds <- list()
  for(i in 1:10) {  # For each fold
    fold_preds <- list()
    for(j in 1:5) {  # For each LM model
      model <- mods.lm[[i]][[j]]
      # Use tryCatch to handle potential errors
      tryCatch({
        preds <- predict(model, newdata = test[[i]])
        fold_preds[[j]] <- data.frame(
          observed = test[[i]]$read_age,
          predicted = preds,
          model = paste0("Linear ", j),
          fold = i
        )
      }, error = function(e) {
        message("Error in LM model ", j, " fold ", i, ": ", e$message)
      })
    }
    if(length(fold_preds) > 0) {
      lm_preds[[i]] <- do.call(rbind, fold_preds)
    }
  }
  if(length(lm_preds) > 0) {
    all_preds$lm <- do.call(rbind, lm_preds)
  }
  
  # Extract GAM predictions
  gam_preds <- list()
  for(i in 1:10) {  # For each fold
    fold_preds <- list()
    for(j in 1:5) {  # For each GAM model
      model <- mods.gam[[i]][[j]]
      tryCatch({
        preds <- predict(model, newdata = test[[i]])
        fold_preds[[j]] <- data.frame(
          observed = test[[i]]$read_age,
          predicted = preds,
          model = paste0("GAM ", j),
          fold = i
        )
      }, error = function(e) {
        message("Error in GAM model ", j, " fold ", i, ": ", e$message)
      })
    }
    if(length(fold_preds) > 0) {
      gam_preds[[i]] <- do.call(rbind, fold_preds)
    }
  }
  if(length(gam_preds) > 0) {
    all_preds$gam <- do.call(rbind, gam_preds)
  }
  
  # Combine all predictions
  all_predictions <- do.call(rbind, all_preds)
  return(all_predictions)
}

# Function to extract PLS predictions
extract_pls_predictions <- function(mods.pls, mods.vip) {
  # For PLS models
  pls1_preds <- list()
  for(i in 1:10) {
    # Check if testres exists
    if(!is.null(mods.pls[[i]]$testres)) {
      ncomp <- mods.pls[[i]]$ncomp.selected
      
      # Extract predictions for the selected number of components
      dim_pred <- dim(mods.pls[[i]]$testres$y.pred)
      if(length(dim_pred) == 3) {
        preds <- mods.pls[[i]]$testres$y.pred[, ncomp, 1]
      } else if(length(dim_pred) == 2) {
        preds <- mods.pls[[i]]$testres$y.pred[, ncomp]
      } else {
        preds <- mods.pls[[i]]$testres$y.pred
      }
      
      # Extract observed values
      dim_ref <- dim(mods.pls[[i]]$testres$y.ref)
      if(length(dim_ref) == 2) {
        observed <- mods.pls[[i]]$testres$y.ref[, 1]
      } else {
        observed <- mods.pls[[i]]$testres$y.ref
      }
      
      pls1_preds[[i]] <- data.frame(
        observed = observed,
        predicted = preds,
        model = "PLS",
        fold = i
      )
    }
  }
  
  if(length(pls1_preds) > 0) {
    pls1_combined <- do.call(rbind, pls1_preds)
  } else {
    pls1_combined <- data.frame(observed = numeric(), predicted = numeric(), 
                                model = character(), fold = numeric())
  }
  
  # For VIP models
  pls2_preds <- list()
  for(i in 1:10) {
    if(!is.null(mods.vip[[i]]$testres)) {
      ncomp <- mods.vip[[i]]$ncomp.selected
      
      dim_pred <- dim(mods.vip[[i]]$testres$y.pred)
      if(length(dim_pred) == 3) {
        preds <- mods.vip[[i]]$testres$y.pred[, ncomp, 1]
      } else if(length(dim_pred) == 2) {
        preds <- mods.vip[[i]]$testres$y.pred[, ncomp]
      } else {
        preds <- mods.vip[[i]]$testres$y.pred
      }
      
      dim_ref <- dim(mods.vip[[i]]$testres$y.ref)
      if(length(dim_ref) == 2) {
        observed <- mods.vip[[i]]$testres$y.ref[, 1]
      } else {
        observed <- mods.vip[[i]]$testres$y.ref
      }
      
      pls2_preds[[i]] <- data.frame(
        observed = observed,
        predicted = preds,
        model = "VIP",
        fold = i
      )
    }
  }
  
  if(length(pls2_preds) > 0) {
    pls2_combined <- do.call(rbind, pls2_preds)
  } else {
    pls2_combined <- data.frame(observed = numeric(), predicted = numeric(), 
                                model = character(), fold = numeric())
  }
  
  # Combine both PLS models
  all_pls <- rbind(pls1_combined, pls2_combined)
  return(all_pls)
}

# Function to extract predictions from simple models
extract_simple_predictions <- function(mods.simple, test_data_list) {
  # Create lists to store predictions
  lm_preds <- list()
  gam_preds <- list()
  
  # Extract LM predictions
  for(i in 1:10) {
    # Get the model and test data
    model <- mods.simple$lm[[i]]
    test_data <- test_data_list[[i]]
    
    # Filter test data to remove NAs in structure_weight
    test_data <- test_data[complete.cases(test_data$structure_weight), ]
    
    # Make predictions
    tryCatch({
      preds <- predict(model, newdata = test_data)
      lm_preds[[i]] <- data.frame(
        observed = test_data$read_age,
        predicted = preds,
        model = "Simple LM",
        fold = i
      )
    }, error = function(e) {
      message("Error in Simple LM model fold ", i, ": ", e$message)
    })
  }
  
  # Extract GAM predictions
  for(i in 1:10) {
    # Get the model and test data
    model <- mods.simple$gam[[i]]
    test_data <- test_data_list[[i]]
    
    # Filter test data to remove NAs in structure_weight
    test_data <- test_data[complete.cases(test_data$structure_weight), ]
    
    # Make predictions
    tryCatch({
      preds <- predict(model, newdata = test_data)
      gam_preds[[i]] <- data.frame(
        observed = test_data$read_age,
        predicted = preds,
        model = "Simple GAM",
        fold = i
      )
    }, error = function(e) {
      message("Error in Simple GAM model fold ", i, ": ", e$message)
    })
  }
  
  # Combine predictions
  lm_combined <- do.call(rbind, lm_preds)
  gam_combined <- do.call(rbind, gam_preds)
  all_simple_preds <- rbind(lm_combined, gam_combined)
  
  return(all_simple_preds)
}

# Function to create the combined performance plot
create_combined_plot <- function(all_predictions, metrics) {
  # Create a faceted plot
  p <- ggplot(all_predictions, aes(x = observed, y = predicted)) +
    geom_point(alpha = 0.5) +
    geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
    facet_wrap(~ model, ncol = 4) +
    labs(
      x = "Observed Age",
      y = "Predicted Age",
      title = "Observed vs Predicted Ages for All Models"
    ) +
    theme_bw() +
    geom_text(
      data = metrics,
      aes(
        x = min(all_predictions$observed, na.rm = TRUE) + 0.1 * diff(range(all_predictions$observed, na.rm = TRUE)),
        y = max(all_predictions$predicted, na.rm = TRUE) - 0.1 * diff(range(all_predictions$predicted, na.rm = TRUE)),
        label = sprintf("RMSE: %.2f\nR²: %.2f", RMSE, R2)
      ),
      hjust = 0,
      vjust = 1,
      size = 3
    )
  
  return(p)
}

# Function to create individual plots for each model
create_individual_plots <- function(all_predictions, metrics) {
  # Get unique model names
  model_names <- unique(all_predictions$model)
  
  # Create a list to store individual plots
  individual_plots <- list()
  
  # Create a plot for each model
  for(model_name in model_names) {
    # Filter data for this model
    model_data <- all_predictions %>% filter(model == model_name)
    model_metric <- metrics %>% filter(model == model_name)
    
    # Create plot
    p <- ggplot(model_data, aes(x = observed, y = predicted)) +
      geom_point(alpha = 0.7) +
      geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
      labs(
        x = "Observed Age",
        y = "Predicted Age",
        title = paste("Observed vs Predicted Ages -", model_name)
      ) +
      theme_bw() +
      geom_text(
        data = model_metric,
        aes(
          x = min(model_data$observed, na.rm = TRUE) + 0.1 * diff(range(model_data$observed, na.rm = TRUE)),
          y = max(model_data$predicted, na.rm = TRUE) - 0.1 * diff(range(model_data$predicted, na.rm = TRUE)),
          label = sprintf("RMSE: %.2f\nR²: %.2f", RMSE, R2)
        ),
        hjust = 0,
        vjust = 1,
        size = 4
      )
    
    # Store the plot
    individual_plots[[model_name]] <- p
  }
  
  return(individual_plots)
}



# Main execution code - updated to include simple models
# 1. Extract LM and GAM predictions
lm_gam_predictions <- extract_lm_gam_predictions(mods.lm, mods.gam, test)

# 2. Extract PLS predictions
pls_predictions <- extract_pls_predictions(mods.pls, mods.vip)

# 3. Extract simple model predictions
simple_predictions <- extract_simple_predictions(mods.simple, test)

# 4. Combine all predictions
all_predictions <- rbind(lm_gam_predictions, pls_predictions, simple_predictions)

# 5. Calculate metrics by fold, then average
calculated_metrics <- calculate_metrics_by_fold(all_predictions)

# 6. Create combined visualization
combined_plot <- create_combined_plot(all_predictions, calculated_metrics)

# 7. Create individual plots for each model
individual_plots <- create_individual_plots(all_predictions, calculated_metrics)

# 8. Display the combined plot
print(combined_plot)

# 9. View the metrics
print(calculated_metrics)

# 10. Save the combined plot
ggsave("model_performance_combined.png", combined_plot, width = 12, height = 8)

# 11. Save individual plots
for(model_name in names(individual_plots)) {
  # Replace spaces with underscores for filenames
  filename <- paste0("model_performance_", gsub(" ", "_", model_name), ".png")
  ggsave(filename, individual_plots[[model_name]], width = 6, height = 5)
}
