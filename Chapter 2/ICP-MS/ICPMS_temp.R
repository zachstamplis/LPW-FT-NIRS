library(dplyr)
library(tidyr)
library(stringr) # For str_remove_all
library(pls)       # For plsr
library(pheatmap)  # For heatmap
library(vegan)     # For rda
library(ggplot2)
library(ggrepel)
library(broom)     # For model summaries
library(purrr)     # For map_dfr
library(prospectr)
library(mdatools)

ICPMS <- readRDS(file = "C:/Users/poopm/Thesis Writing/Github/LPW_FT-NIRS/RDS_dataframes/ICPMS_filtered_GOOD_USEME.RDS")

FTNIRS <- readRDS("C:/Users/poopm/Thesis Writing/Github/LPW_FT-NIRS/RDS_dataframes/combined_IBM_LPW_raw.RDS")
FTNIRS <- FTNIRS %>% filter(region != "LPW")# select only IBM specimensof combined dataframe

# apply savitzky golay 1,5,17 filter
speccols <- names(FTNIRS)[grepl("^\\d", names(FTNIRS))]
metacols <- setdiff(names(FTNIRS), speccols)
FTNIRS <- cbind(FTNIRS[,metacols],
                savitzkyGolay(as.matrix(FTNIRS[speccols]), m = 1, p = 5, w = 17)
)
speccols <- names(FTNIRS)[grepl("^\\d", names(FTNIRS))] # reselect the FT-NIRS columns
# perform PCA on all FT-NIRS columns, store alongside specimen
pca_FTNIRS <- mdatools::pca(FTNIRS[speccols], center = TRUE, scale = TRUE, )
# pca dataframe with first 20 PC's
pca_df <- data.frame(specimen = FTNIRS$specimen, 
                     pca_FTNIRS$calres$scores[,1:20])
# clean up column names
colnames(pca_df)[2:21] <- paste0("PC", 1:20)




# Define the *original* column names you want to select
elements_of_interest <- c(
  "Ba138_ppm",
  "Co59_ppm",
  # "Fe56_ppm", # this is too low for detection I think? I have negative values
  "Li7_ppm", 
  "Mg24_ppm", 
  "Mn55_ppm", 
  "Ni60_ppm",
  "Rb85_ppm",
  "Sr86_ppm",
  "Sr87_ppm",
  "Sr88_ppm",  # Selecting Sr88 sepecifically, though others may be usedful?
  "Zn66_ppm"
)

# Define the *clean* names for the pivot step later
elements_clean <- c("Ba", "Co", # "Fe",
                    "Li", "Mg", "Mn", "Ni", "Rb", "Sr86", "Sr87", "Sr88", "Zn")

# Create the summary, find median of each transect
ICPMS_overall_avg <- ICPMS %>%
  group_by(specimen, transect) %>%
  summarize(
    across(all_of(elements_of_interest), \(x) mean(x, na.rm = TRUE))
  ) %>%
  group_by(specimen) %>%
  summarize(
    across(all_of(elements_of_interest), \(x) mean(x, na.rm = T))
  ) %>%
  ungroup() # Always good to ungroup after summarize

rename_map <- setNames(elements_of_interest, elements_clean)

ICPMS_overall_avg <- ICPMS_overall_avg %>%
  rename(any_of(rename_map))

pca_df_clean_overall <- inner_join(pca_df, ICPMS_overall_avg, by = "specimen") %>%
  na.omit()



# add in `region` information from FT-NIRS df, match by specimen
ICPMS_overall_avg <- ICPMS_overall_avg %>% left_join(FTNIRS %>% dplyr::select(specimen, region), by = "specimen")
# make ICPMS long for elements for plotting
ICPMS_long_overall <- ICPMS_overall_avg %>% pivot_longer(cols = Ba:Zn, 
                                                         names_to = "element", 
                                                         values_to = "concentration")

# Define your custom order
region_order <- c("E Shumagins", "W Shumagins", "N Kodiak", "S Kodiak")

# Apply the factor levels
ICPMS_long_overall <- ICPMS_long_overall %>%
  mutate(region = factor(region, levels = region_order))

ggplot(ICPMS_long_overall, aes(x = region, y = concentration)) + 
  geom_boxplot() + 
  facet_wrap(~ element, scales = "free_y") +
  theme_bw()


unique(ICPMS_overall_avg$region)




# NOW BY REGION

# Create the summary, find median of each transect
ICPMS_region_avg <- ICPMS %>%
  group_by(specimen, oto_region) %>%
  summarize(
    across(all_of(elements_of_interest), \(x) mean(x, na.rm = TRUE))
  ) %>%
  ungroup() # Always good to ungroup after summarize

# *** THE KEY FIX: Rename columns *before* pivoting ***
ICPMS_region_avg <- ICPMS_region_avg  %>%
  rename(any_of(rename_map)) %>% left_join(FTNIRS %>% dplyr::select(specimen, region), by = "specimen")


# # Pivot wider using the new, clean names
# ICPMS_wide_region <- ICPMS_region_avg %>%
#   pivot_wider(
#     names_from = oto_region,
#     values_from = all_of(elements_clean), # Use the clean names
#     names_sep = "_"                      # Makes "Mg_core", "Mg_middle", etc.
#   )


ICPMS_long_region <- ICPMS_region_avg %>% pivot_longer(cols = Ba:Zn, 
                                                         names_to = "element", 
                                                         values_to = "concentration")

# Join with PCA data (pca_df)
# We use na.omit() to ensure all models run
pca_df_clean <- inner_join(pca_df, ICPMS_wide_region, by = "specimen") %>%
  na.omit()

# Apply the factor levels
ICPMS_long_region <- ICPMS_long_overall %>%
  mutate(region = factor(region, levels = region_order))

ggplot(ICPMS_long_region, aes(x = oto_region, y = concentration)) + 
  geom_boxplot() + 
  facet_grid(element ~ region , scales = "free_y") +
  theme_bw()

# --- 3. Create Final X and Y Variables ---