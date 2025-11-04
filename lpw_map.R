library("ggplot2")
library("sf")
library(ggthemes)
library(ggrepel)
library("rnaturalearth")
library("rnaturalearthdata")
library(ggspatial)
library(cowplot)
library(tidyverse)

sites <- read.csv("metadata/IBM_metadata.csv")
df_raw <- readRDS("RDS_dataframes/combined_IBM_LPW_raw.RDS")

# Filter 'sites' to keep only rows where sites$specimen is found in df_raw$specimen
sites_filtered <- sites[sites$specimen %in% df_raw$specimen, ] %>% filter(run_number == 2)


# --- 1. Load Libraries ---

# Set a clean default theme
theme_set(theme_bw())

# --- 2. Load Geographic Data ---
world <- ne_countries(scale = "medium", returnclass = "sf")

# --- 3. Filter Your Data ---
# (Assuming 'sites' and 'df_raw' are already in your environment)
sites_filtered <- sites[sites$specimen %in% df_raw$specimen, ]

# --- 4. Create Data for Labels (SPLIT) ---

# Labels to plot directly (no lines)
labels_static <- data.frame(
  lon = c(-152.5, -159.0), # Kodiak & Shumagins
  lat = c(57.0, 54.5),
  label = c("Kodiak Island", "Shumagin Islands")
)

# Label to repel (with a line)
label_repel <- data.frame(
  lon = -134.6433, # LPW
  lat = 56.384,
  label = "LPW"
)

alaska_map_final <- ggplot(data = world) +
  
  # Base map land features
  geom_sf(color = "black", fill = "gray80") +
  
  # Set coordinates
  coord_sf(xlim = c(-166.5, -130), ylim = c(53, 63), expand = FALSE) +
  
  # Add your sample locations (NOW WITH LEGEND MAPPING)
  geom_point(
    data = sites_filtered,
    aes(x = longitude, y = latitude, color = "Sample Site"), # <-- Mapped color
    shape = 4, # 'x'
    size = 5,
    alpha = .7,
    stroke = 1.2
  ) +
  
  # --- NEW: Manually set the color and legend key shape ---
  scale_color_manual(
    name = NULL, 
    values = c("Sample Site" = "red")
  ) +
  guides(color = guide_legend(override.aes = list(
    shape = 4,
    size = 5,
    stroke = 1.2
  ))) +
  # --- End new legend code ---
  
  # Add STATIC labels (no lines)
  geom_label(
    data = labels_static,
    aes(x = lon, y = lat, label = label),
    size = 8,
    color = "black",
    fill = alpha("white", 0.8)
  ) +
  
  # Add REPELLED label for LPW (with line)
  geom_label_repel(
    data = label_repel,
    aes(x = lon, y = lat, label = label),
    size = 8,
    color = "black",
    fill = alpha("white", 0.8),
    box.padding = 1,
    nudge_x = -2.5,
    nudge_y = -1.0,
    segment.color = "black",
    segment.size = 0.8,
    min.segment.length = 0
  ) +
  
  # Remove all axis titles, text, and gridlines
  labs(x = NULL, y = NULL) +
  theme(
    panel.background = element_rect(fill = "aliceblue"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    
    # --- NEW LEGEND POSITION & STYLE ---
    legend.position = c(0.6, 0.08), # Adjust as needed
    legend.background = element_rect(fill = alpha("white", 1.0)),
    legend.key = element_rect(fill = "transparent"),
    legend.text = element_text(size = 20)
  )


# --- 6. View and Save the Map ---

# To view in RStudio
print(alaska_map_final)

# To save the map
ggsave(
  "alaska_powerpoint_map_final.png",
  alaska_map_final,
  width = 11,
  height = 8.5,
  bg = "transparent",
  dpi = 600
)

