library(readxl)
library(tidyverse)
library(FSA)
library(lubridate) # Helps with date handling
ages <- read_xlsx("metadata/LPW_ages_new.xlsx")
ages <- ages %>%
  # remove rows with NA in age1
  filter(!is.na(age1)) %>%
  filter(!is.na(age2)) %>%
  rowwise() %>%
  mutate(
    # Combine the ages for the current row into a single vector
    my_reads = list(c(age1, age2, age3)),
    # Calculate %CV, handling cases with one or zero reads
    cv_percent = (sd(my_reads, na.rm = TRUE) / mean(my_reads, na.rm = TRUE)) * 100
  ) %>%
  ungroup() %>% # Exit row-wise mode
  select(-my_reads) # Remove the temporary list column

# temp <- ages %>% filter(cv_percent < 10)


# --- Step 1: Define the User's Age ('my_final_age') ---
# Create a single age for YOUR reads based on your rules.
# This step is performed first on the original wide-format data.
ages_with_my_age <- ages %>%
  rowwise() %>%
  mutate(
    # If age_to_use is available, use it.
    # Otherwise, calculate the mean of your other reads.
    my_final_age = if_else(
      !is.na(age_to_use),
      as.double(age_to_use),
      mean(c(age1, age2, age3), na.rm = TRUE)
    )
  ) %>% 
  ungroup() # Exit row-wise mode

# --- Step 2: Reshape Alternative Reads and Apply Filters ---
# Pivot ONLY the alternative reader columns and then filter them.
comparison_df <- ages_with_my_age %>%
  pivot_longer(
    cols = starts_with("alt_"),
    names_to = c(".value", "read_num"),
    names_pattern = "alt_([a-z]+)(\\d+)", # Extracts "age", "grade", "reader"
    values_drop_na = TRUE # Crucial for handling missing reads
  )

# --- Step 3: Calculate Bland-Altman Metrics ---
# Calculate the mean and difference using your final age and the filtered alt ages.
comparison_df <- comparison_df %>%
  mutate(
    mean_age = (my_final_age + age) / 2,
    diff_age = my_final_age - age
  ) %>%
  # Rename for clarity in the plotting step
  rename(alt_reader = reader, alt_age = age)

# --- Step 4: Calculate Overall Statistics ---
# These are the statistics for the horizontal lines, calculated on the filtered data.
mean_diff <- mean(comparison_df$diff_age, na.rm = TRUE)
std_diff  <- sd(comparison_df$diff_age, na.rm = TRUE)
upper_loa <- mean_diff + 1.96 * std_diff
lower_loa <- mean_diff - 1.96 * std_diff

# --- Step 5: Create the Enhanced Plot ---
bland_altman_plot_final <- ggplot(comparison_df, aes(x = mean_age, y = diff_age)) +
  
  # Add horizontal lines for overall bias and limits of agreement
  geom_hline(aes(yintercept = mean_diff, linetype = "Mean Difference (Bias)"), color = "red") +
  geom_hline(aes(yintercept = upper_loa, linetype = "Limits of Agreement"), color = "black") +
  geom_hline(aes(yintercept = lower_loa), color = "black") +
  
  # Add the points for each reader
  geom_point(aes(color = alt_reader), alpha = 0.6, size = 2.5) +
  
  # Add a GAM smooth line and confidence ribbon for each reader
  geom_smooth(aes(color = alt_reader, fill = alt_reader),
              method = "gam",
              formula = y ~ s(x, bs = "tp", k = 3),
              alpha = 0.2) +
  
  # Define the appearance of the linetypes
  scale_linetype_manual(name = "Agreement Metrics", values = c("Mean Difference (Bias)" = "dashed", "Limits of Agreement" = "solid")) +
  
  # Customize labels and theme
  labs(
    title = "Bland-Altman Plot with Reader-Specific Trends",
    x = "Mean of Ages",
    y = "Difference in Ages (My Age - Alt Age)",
    color = "Alternative Reader",
    fill = "Alternative Reader"
  ) +
  theme_bw()

# --- Step 6: Display the Plot ---
print(bland_altman_plot_final)




# --- Step 7: Perform Symmetry Tests ---

# Get a list of the unique alternative readers in your data
unique_readers <- unique(comparison_df$alt_reader)


for (current_reader in unique_readers) {
  
  # Prepare a temporary data frame with only the current reader's data
  reader_data <- comparison_df %>%
    filter(alt_reader == current_reader)
  
  # Run the age precision analysis
  precision_results <- agePrecision(~ alt_age + my_final_age, data = reader_data)
  
  # Print the results to the console with a clear header
  cat("========================================================\n")
  cat("Symmetry Test Results for:", current_reader, "\n")
  cat("========================================================\n")
  
  # CORRECTED LINE: The default summary includes the symmetry tests
  print(summary(precision_results))
  
  cat("\n\n") # Add some space before the next reader's results
}






# --- Step 1: Reshape Data to Long Format ---
# This converts the data so that each row is a single age reading for a specimen.
long_ages <- ages %>%
  # Select only the columns we need for this task
  select(specimen, starts_with("age"), starts_with("date")) %>%
  pivot_longer(
    cols = -specimen,
    names_to = c(".value", "read_set"),
    names_pattern = "([a-z]+)(\\d+)", # Splits "age1" into "age" and "1"
    values_drop_na = TRUE # Removes any sets that don't have both an age and a date
  )

# --- Step 2: Determine Chronological Order of Reads ---
# For each specimen, we sort the reads by date and assign a rank.
chrono_reads <- long_ages %>%
  group_by(specimen) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(chrono_rank = row_number()) %>%
  ungroup()

# --- Step 3: Create a Wide Data Frame with Chronological Ages ---
# This makes it easy to create pairs for comparison.
wide_chrono_ages <- chrono_reads %>%
  select(specimen, chrono_rank, age) %>%
  pivot_wider(
    names_from = chrono_rank,
    values_from = age,
    names_prefix = "age_read_"
  )

# --- Step 4: Generate All Comparison Pairs ---
# We create three separate data frames for each comparison and then bind them together.
pair_1_vs_2 <- wide_chrono_ages %>%
  select(age_A = age_read_1, age_B = age_read_2) %>%
  mutate(comparison = "1st vs 2nd Read")

pair_1_vs_3 <- wide_chrono_ages %>%
  select(age_A = age_read_1, age_B = age_read_3) %>%
  mutate(comparison = "1st vs 3rd Read")

pair_2_vs_3 <- wide_chrono_ages %>%
  select(age_A = age_read_2, age_B = age_read_3) %>%
  mutate(comparison = "2nd vs 3rd Read")

# Combine all pairs into one data frame and remove rows with missing data
all_pairs <- bind_rows(pair_1_vs_2, pair_1_vs_3, pair_2_vs_3) %>%
  drop_na(age_A, age_B)

# --- Step 5: Calculate Bland-Altman Metrics for Each Pair ---
bland_altman_data <- all_pairs %>%
  mutate(
    mean_age = (age_A + age_B) / 2,
    diff_age = age_A - age_B # Difference = (Earlier Read - Later Read)
  )

# --- Step 6: Calculate Summary Statistics for Each Panel ---
# This is necessary so each plot facet gets its own correct mean and LoA lines.
summary_stats <- bland_altman_data %>%
  group_by(comparison) %>%
  summarise(
    n = n(),
    mean_diff = mean(diff_age),
    sd_diff   = sd(diff_age),
    upper_loa = mean_diff + 1.96 * sd_diff,
    lower_loa = mean_diff - 1.96 * sd_diff
  )

# Print the summary stats to the console
print(summary_stats)

# --- Step 7: Create the Faceted Bland-Altman Plot ---
intra_reader_plot_with_smooth <- ggplot(bland_altman_data, aes(x = mean_age, y = diff_age)) +
  geom_point(alpha = 0.7, color = "black") +
  # --- UPDATED: Add a trend line WITH a confidence ribbon ---
  geom_smooth(method = "lm", se = TRUE, color = "darkgreen", fill = "darkgreen", alpha = 0.2) +
  
  # Add the mean and Limits of Agreement lines
  geom_hline(data = summary_stats, aes(yintercept = mean_diff), color = "red", linewidth = 1) +
  geom_hline(data = summary_stats, aes(yintercept = upper_loa), color = "blue", linetype = "dashed") +
  geom_hline(data = summary_stats, aes(yintercept = lower_loa), color = "blue", linetype = "dashed") +
  
  # Use facet_wrap to create a separate plot for each comparison
  facet_wrap(~ comparison, scales = "free") +
  
  labs(
    title = "Intra-Reader Bias Assessment",
    subtitle = "Comparing repeated age reads in chronological order",
    x = "Mean of Ages",
    y = "Difference between Ages (Earlier - Later)"
  ) +
  theme_bw()

# Display the plot
print(intra_reader_plot_with_smooth)








# Load the FSA library
library(FSA)
library(tidyverse)

# Your previous code to create 'comparison_df' should be run first.
# Ensure 'my_final_age' and 'alt_age' columns exist.

# --- Analysis Loop using the ageBias Function ---

# Get a list of unique alternative readers
unique_readers <- unique(comparison_df$alt_reader)

# Loop through each alternative reader
for (current_reader in unique_readers) {
  
  # Prepare a data frame with only the current reader's data
  reader_data <- comparison_df %>%
    filter(alt_reader == current_reader)
  
  # --- 1. Run the ageBias analysis ---
  # The formula is nref ~ ref, so alt_age ~ my_final_age
  bias_results <- ageBias(
    alt_age ~ my_final_age, 
    data = reader_data,
    ref.lab = "My Final Age",
    nref.lab = current_reader
  )
  
  # --- 2. Print Headers and Summaries ---
  cat("========================================================\n")
  cat("Comprehensive Bias Analysis for:", current_reader, "\n")
  cat("========================================================\n\n")
  
  cat("--- Age-Agreement Table ---\n")
  print(summary(bias_results, what = "table"))
  cat("\n")
  
  cat("--- Tests of Symmetry ---\n")
  print(summary(bias_results, what = "symmetry"))
  cat("\n\n")
  
  # --- 3. Generate the Bland-Altman Plot ---
  # The plot function for ageBias can create this directly with xvals = "mean"
  plot(
    bias_results, 
    xvals = "mean",
    xlab = "Mean of Ages",
    ylab = paste("Difference (", current_reader, " - My Age)"),
    main = paste("Bland-Altman Plot: My Ages vs.", current_reader)
  )
}




# Load necessary libraries if you haven't already
library(FSA)
library(tidyverse)

# --- Step 1: Prepare Chronologically Ordered Data ---
# This reuses the logic from our previous work to get your reads in order.
# It starts with your original 'ages' dataframe.
my_chrono_reads_wide <- ages %>%
  select(specimen, starts_with("age"), starts_with("date")) %>%
  pivot_longer(
    cols = -c(specimen, starts_with("alt"), age_to_use),
    names_to = c(".value", "read_set"),
    names_pattern = "([a-z]+)(\\d+)",
    values_drop_na = TRUE
  ) %>%
  group_by(specimen) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(chrono_rank = row_number()) %>%
  ungroup() %>%
  select(specimen, chrono_rank, age) %>%
  pivot_wider(
    names_from = chrono_rank,
    values_from = age,
    names_prefix = "age_read_"
  )

# --- Step 2: Loop Through Each Comparison Pair ---

# Define the pairs we want to compare
comparison_pairs <- list(
  c(ref = 1, nref = 2),
  c(ref = 1, nref = 3),
  c(ref = 2, nref = 3)
)

# Loop through the list of pairs
for (pair in comparison_pairs) {
  ref_col_name <- paste0("age_read_", pair["ref"])
  nref_col_name <- paste0("age_read_", pair["nref"])
  
  # Prepare data for this specific pair, removing NAs
  paired_data <- my_chrono_reads_wide %>%
    select(ref_age = all_of(ref_col_name), nref_age = all_of(nref_col_name)) %>%
    drop_na()
  
  # Skip if there's no data for this pair
  if (nrow(paired_data) < 2) {
    cat("========================================================\n")
    cat("Not enough data for comparison between Read", pair["ref"], "and Read", pair["nref"], "\n")
    cat("========================================================\n\n")
    next
  }
  
  # Run the ageBias analysis
  bias_results <- ageBias(
    nref_age ~ ref_age,
    data = paired_data,
    ref.lab = paste(pair["ref"], "st/nd Read"),
    nref.lab = paste(pair["nref"], "rd/th Read")
  )
  
  # Print headers and summaries
  cat("========================================================\n")
  cat("Intra-Reader Bias: Read", pair["ref"], "vs. Read", pair["nref"], "\n")
  cat("========================================================\n\n")
  
  cat("--- Age-Agreement Table ---\n")
  print(summary(bias_results, what = "table"))
  cat("\n")
  
  cat("--- Tests of Symmetry ---\n")
  print(summary(bias_results, what = "symmetry"))
  cat("\n\n")
  
  # Generate the Bland-Altman Plot
  plot(
    bias_results,
    xvals = "mean",
    xlab = "Mean of Ages",
    ylab = paste("Difference (Read", pair["nref"], " - Read", pair["ref"], ")"),
    main = paste("Bland-Altman Plot: Read", pair["ref"], "vs. Read", pair["nref"])
  )
}


