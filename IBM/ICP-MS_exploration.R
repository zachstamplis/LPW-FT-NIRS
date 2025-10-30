# --- Load Libraries ---
library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)
library(patchwork)
library(cowplot)
library(magick)

df <- readRDS("RDS_dataframes/ICP-MS_meta_09032025.RDS")

df <- df %>% mutate(Sr87_86 = Sr87_ppm / Sr86_ppm)

# remove transects that missed core for now
bad_otos <- c("oto05", "oto12", "oto14", "oto40")
df <- df %>% filter(!specimen %in% bad_otos)

# This list includes the recommended choices from the table above.
elements_to_keep <- c(
  "Ca43", "Li7", "B11", "Mg24", "Si29", "Mn55", "Fe56", "Co59", "Ni60",
  "Cu63", # Recommended Copper isotope
  "Zn66", "Rb85",
  "Sr87_86", "Sr88", # Recommended Strontium isotope for concentration
  "Cd111", "Ba138", "Ce140",
  "PbTotal" # Recommended Lead value, removing other lead isotopes
  # "Th232" # seems like junk right now......
)

#################################################################
##  1. DEFINE THE PLOTTING FUNCTION
#################################################################

create_specimen_plot <- function(specimen_id, source_df) {
  
  # --- A. Prepare Data for the given specimen_id ---
  plot_data <- source_df %>%
    pivot_longer(
      cols = Ca43:Sr87_86,
      names_to = "element",
      values_to = "concentration"
    ) %>%
    mutate(element = sub("_ppm", "", element)) %>%
    filter(element %in% elements_to_keep) %>%
    filter(
      specimen == specimen_id,
      !is.na(concentration)
    ) %>%
    mutate(grade_rank = dense_rank(grade)) %>%
    mutate(n_rank1 = n_distinct(transect[grade_rank == 1])) %>%
    filter(
      grade_rank == 1 | (grade_rank == 2 & n_rank1 == 1)
    ) %>%
    mutate(plot_pos = dense_rank(transect)) %>%
    mutate(transect_label = paste0("Transect ", transect, " [Grade: ", grade, "]")) %>%
    group_by(element, transect) %>%
    arrange(time) %>%
    mutate(
      elapsed_time = time - min(time),
      elapsed_time_next = lead(elapsed_time, default = last(elapsed_time) + median(diff(elapsed_time)))
    ) %>%
    ungroup() %>%
    group_by(element) %>%
    mutate(concentration_percentile = cume_dist(concentration) * 100) %>%
    ungroup()
  
  if (nrow(plot_data) == 0) {
    print(paste("Skipping", specimen_id, "- No data available after filtering."))
    return(NULL)
  }
  
  # --- B. Automatically generate the main title ---
  title_info <- plot_data %>%
    distinct(transect_label) %>%
    arrange(transect_label) %>%
    pull(transect_label) %>%
    paste(collapse = " & ")
  main_title <- paste0("Specimen: ", specimen_id, " - ", title_info)
  
  # --- C. Extract a Shared Legend ---
  legend_plot <- ggplot(plot_data) +
    geom_tile(aes(x = elapsed_time, y = plot_pos, fill = concentration_percentile)) +
    scale_fill_viridis_c(name = "Percentile", limits = c(0, 100), breaks = c(0, 25, 50, 75, 100)) +
    theme(legend.position = "bottom", legend.direction = "horizontal", legend.key.width = unit(2, "cm"))
  shared_legend <- get_legend(legend_plot)
  
  # --- D. Loop to Create Sub-plots ---
  plot_list <- list()
  elements_to_plot <- sort(unique(plot_data$element))
  for (elem in elements_to_plot) {
    plot_subset <- plot_data %>% filter(element == elem)
    p <- ggplot(plot_subset, aes(fill = concentration_percentile)) +
      geom_rect(aes(
        xmin = elapsed_time, xmax = elapsed_time_next,
        ymin = plot_pos - 0.45, ymax = plot_pos + 0.45
      )) +
      scale_fill_viridis_c(limits = c(0, 100)) +
      # Simplify the scale_y_reverse call, as labels will be hidden
      scale_y_reverse() +
      labs(title = elem) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5),
        legend.position = "none",
        axis.title = element_blank(),

        # Hide all axis text (x and y) again
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank()
      )
    plot_list[[elem]] <- p
  }
  
  # --- E. Combine All Plots and Save ---
  final_grid <- wrap_plots(plot_list, ncol = 5)
  final_plot_with_legend <- final_grid / shared_legend +
    plot_layout(heights = c(1, 0.05)) +
    plot_annotation(title = main_title)
  
  output_filename <- paste0("plot_", specimen_id, ".png")
  ggsave(output_filename, final_plot_with_legend, width = 12, height = 5, dpi = 300)
  print(paste("Saved plot:", output_filename))
}


#################################################################
##  2. GET SPECIMEN LIST AND RUN THE LOOP
#################################################################

# Get a unique list of all specimens in your dataframe
specimen_list <- unique(df$specimen)

# Loop through each specimen name and call the function
for(id in specimen_list) {
  create_specimen_plot(specimen_id = id, source_df = df)
}

#################################################################
##  3. MAKE SINGLE PDF OUT OF ALL PNGs
#################################################################
# --- 1. Get a list of all your PNG plot files ---
# This pattern finds all files starting with "plot_" and ending with ".png"
# 'full.names = TRUE' ensures R knows the full path to the files
png_files <- list.files(
  pattern = "plot_.*\\.png$",
  full.names = TRUE
)

# Optional: Sort the files so they appear in a logical order in the PDF
png_files <- sort(png_files)


# --- 2. Read all the PNG files into a single object ---
# image_read() creates a stack of all your plots
image_stack <- image_read(png_files)


# --- 3. Write the stack of images to a single PDF file ---
image_write(
  image_stack,
  path = "All_Otolith_Plots.pdf",
  format = "pdf"
)

print("PDF created successfully: All_Otolith_Plots.pdf")





#################################################################
##  4. PLOTTING AND PLAYING
#################################################################


######### 
temp <- df %>% filter(specimen == "oto01", transect == "1")

ggplot(df %>% filter(specimen == "oto01", transect == "1"), aes(x = time, y = Ba138_ppm)) + 
  geom_line()
ggplot(df %>% filter(specimen == "oto01", transect == "1"), aes(x = time, y = Mg24_ppm)) + 
  geom_line()

temp <- df %>% filter(grade == "1" | grade == "1.5")
otos <- unique(temp$specimen)

for (spec in otos) {
  data <- df %>% filter(specimen == spec, grade == "1" | grade == "1.5")
  transects <- unique(data$transect)
  print(ggplot(data, aes(x = time, y = Ba138_ppm)) + 
          geom_line() + 
          ggtitle(paste("spec #", spec)) + 
          facet_wrap(~transect) + 
          theme_bw()
  )
}
ggplot(df %>% filter(specimen == "oto03", transect == "1"), aes(x = time, y = Ba138_ppm)) + 
  geom_line()

ggplot(df %>% filter(specimen == "oto21", transect == "2"), aes(x = time, y = Ba138_ppm)) + 
  geom_line() + 
  geom_vline(xintercept = c(15.2795, 31.206), color = "red") + 
  ggtitle("specimen 21-2")
ggplot(df %>% filter(specimen == "oto21", transect == "2"), aes(x = time, y = Mg24_ppm)) + 
  geom_line() + 
  geom_vline(xintercept = c(15.2795, 31.206), color = "red")
ggplot(df %>% filter(specimen == "oto21", transect == "2"), aes(x = time, y = Mn55_ppm)) + 
  geom_line() + 
  geom_vline(xintercept = c(15.2795, 31.206), color = "red")

# seems to be the "core" is the region immediately after the drop in Ba

# try another, 22-3

ggplot(df %>% filter(specimen == "oto22", transect == "3"), aes(x = time, y = Ba138_ppm)) + 
  geom_line() + 
  geom_vline(xintercept = c(32.106, 60.847), color = "red")+ 
  ggtitle("specimen 22-3")

ggplot(df %>% filter(specimen == "oto22", transect == "3"), aes(x = time, y = Mg24_ppm)) + 
  geom_line() + 
  geom_vline(xintercept = c(32.106, 60.847), color = "red")

ggplot(df %>% filter(specimen == "oto22", transect == "3"), aes(x = time, y = Mn55_ppm)) + 
  geom_line() + 
  geom_vline(xintercept = c(32.106, 60.847), color = "red")



# one final one, a nice specimen, 19-2

ggplot(df %>% filter(specimen == "oto19", transect == "2"), aes(x = time, y = Ba138_ppm)) + 
  geom_line() + 
  geom_vline(xintercept = c(57.696, 110.545), color = "red")+ 
  ggtitle("specimen 19-2")
ggplot(df %>% filter(specimen == "oto19", transect == "2"), aes(x = time, y = Mg24_ppm)) + 
  geom_line() + 
  geom_vline(xintercept = c(57.696, 110.545), color = "red")
ggplot(df %>% filter(specimen == "oto19", transect == "2"), aes(x = time, y = Mn55_ppm)) + 
  geom_line() + 
  geom_vline(xintercept = c(57.696, 110.545), color = "red")
