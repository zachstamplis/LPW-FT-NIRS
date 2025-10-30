# --- 1. Preamble: Load necessary packages ---
library(dplyr)
library(readr)

# --- 2. Load Your Data ---
# IMPORTANT: Replace these file paths with the actual locations of your files.

# Load your main otolith dataset
# Example: df_raw <- read.csv("path/to/your/full_otolith_data.csv")
df_raw <- df # Using the sample 'df' from the app for this example

# Load the CSV file with your manually marked core regions
# This is the file you downloaded from the Shiny app.
# Example: marked_cores <- read_csv("path/to/your/otolith_core_regions.csv")
marked_cores <- read_csv("otolith_core_regions_2025-09-04.csv", col_types = cols(
  specimen = "c",
  transect = "c",
  core_start_time = "d",
  core_end_time = "d"
))


# --- 3. Join Marked Regions with Main Data ---
# Example: df_raw <- read.csv("path/to/your/full_otolith_data.csv")
df_raw <- df # Using the sample 'df' from the app for this example

# Load the CSV file with your manually marked core regions
# This is the file you downloaded from the Shiny app.
# Example: marked_cores <- read_csv("path/to/your/otolith_core_regions.csv")
marked_cores <- read_csv("otolith_core_regions_2025-09-04.csv", col_types = cols(
  specimen = "c",
  transect = "c",
  core_start_time = "d",
  core_end_time = "d"
))


# --- 3. Join Marked Regions with Main Data ---

# We'll use a left_join to add the core_start_time and core_end_time
# to every row of the corresponding specimen and transect in your main data.
# Note: We ensure 'transect' is a character in both for a clean join.
df_classified <- df_raw %>%
  mutate(transect = as.character(transect)) %>%
  left_join(marked_cores, by = c("specimen", "transect"))


# --- 4. Classify Each Point into a Region ---

# Now, we create the 'region' column based on your rules.
# The case_when() function checks these rules in order for each row.
df_final <- df_classified %>%
  # We must group by specimen and transect for the following operations
  group_by(specimen, transect) %>%
  # For each transect, snip off all data points occurring before the marked core region.
  # This will also implicitly remove any transects that were not in the marked_cores CSV.
  filter(time >= core_start_time) %>%
  mutate(
    region = case_when(
      # Rule 1: The 'edge' is the last 5 seconds of the transect.
      time >= (max(time, na.rm = TRUE) - 5) ~ "edge",
      
      # Rule 2: The 'core' is between the start and end times from your CSV.
      # This will only apply to rows that have start/end times (i.e., were marked).
      !is.na(core_start_time) & time >= core_start_time & time <= core_end_time ~ "core",
      
      # Rule 3: Anything else is the 'middle'.
      TRUE ~ "middle"
    ),
    # Optional: Convert the region column to a factor for easier plotting
    region = factor(region, levels = c("core", "middle", "edge"))
  ) %>%
  # Ungroup to prevent accidental grouped operations later
  ungroup() %>%
  # Optional: Remove the temporary start/end time columns if you don't need them
  select(-core_start_time, -core_end_time)


# --- 5. Verification ---
# It's always good practice to check the result.

# Print the first few rows of the final, classified dataframe
print(head(df_final))

# See a summary of how many data points are in each region
print(
  df_final %>%
    count(specimen, transect, region)
)


# --- 6. (Optional) Save the Final Classified Data ---
# You can now save this complete dataset for future analysis.
# write_csv(df_final, "classified_otolith_data.csv")

