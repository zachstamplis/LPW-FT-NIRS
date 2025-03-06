# Produced via Claude AI models to quickly check bias in ages vs FT-NIRS ages

# Visualizing Bias in Otolith Age Determination for FT-NIRS Integration: Expert Guidelines

## 1. Modified Age Bias Plots for Multiple Readings

When working with multiple readings and no designated reference age, treat the mean age as your best reference point:

```{r}
library(ggplot2)
library(dplyr)
library(tidyr)

df <- readRDS("RDS_dataframes/LPW_scan_avg_proc.RDS")
df <- df[complete.cases(df$read_age), ]
df <- df %>% filter(specimen != 77)
pca_temp <- pca(df[, 21:ncol(df), ])
pc_df <- data.frame(PC1 = rep(0,nrow(df)))
for (i in 1:10) {
  pc_df[, paste0("PC", i)] <- pca_temp$res$cal$scores[, i]
  rm(i)
}
pc_df <- cbind(pc_df,df)
mod <- gam(read_age ~ s(PC1, k = 4) + s(PC3, k = 4) + s(PC4, k = 4) + s(PC6, k =4), data = pc_df)
pc_df <- cbind(preds = mod$fitted.values, pc_df)
pc_df <- pc_df[complete.cases(pc_df$preds), ]
df <- read.csv("ages_LPW.csv")
# Left join to preserve all rows in df
df <- df %>% 
  left_join(pc_df %>% select(specimen, preds), by = "specimen")

# Calculate mean age for each specimen
mean_ages <- df %>%
  rowwise() %>%
  mutate(
    readings_count = sum(!is.na(c(Age.1, Age.2, Age.3))),
    mean_age = mean(c(Age.1, Age.2, Age.3), na.rm=TRUE),
    sd_age = sd(c(Age.1, Age.2, Age.3), na.rm=TRUE),
    cv = ifelse(readings_count > 1, sd_age/mean_age*100, NA)
  ) %>%
  filter(!is.na(mean_age))

# Create long format data for plotting
long_data <- mean_ages %>%
  select(specimen, Age.1, Age.2, Age.3, mean_age, cv, readings_count) %>%
  pivot_longer(cols=c(Age.1, Age.2, Age.3), 
               names_to="Reading", 
               values_to="Age",
               values_drop_na=TRUE)

# Create enhanced age bias plot
ggplot(long_data, aes(x=mean_age, y=Age, color=Reading)) +
  geom_point(alpha=0.7) +
  geom_abline(intercept=0, slope=1, linetype="dashed", color="darkred") +
  geom_smooth(method="lm", se=FALSE) +
  labs(
    title="Age Bias Plot for Multiple Otolith Readings",
    subtitle="Using mean age as reference point",
    x="Mean Age (days)",
    y="Individual Reading (days)"
  ) +
  theme_bw() +
  annotate("text", x=min(long_data$mean_age, na.rm=TRUE), y=max(long_data$Age, na.rm=TRUE),
           label=paste("Overall APE =", round(4.42, 2), "%"), 
           hjust=0, vjust=1, size=4)
```

## 2. Bland-Altman Plots for Reading Pairs

This approach examines agreement between reading pairs and identifies age-dependent bias patterns:

```{r}
# Create data frames for each reading pair
pairs_1_2 <- df %>% 
  filter(!is.na(Age.1) & !is.na(Age.2)) %>%
  mutate(
    mean_age = (Age.1 + Age.2)/2,
    diff = Age.1 - Age.2,
    pair = "Reading 1 vs 2"
  )

pairs_1_3 <- df %>% 
  filter(!is.na(Age.1) & !is.na(Age.3)) %>%
  mutate(
    mean_age = (Age.1 + Age.3)/2,
    diff = Age.1 - Age.3,
    pair = "Reading 1 vs 3"
  )

pairs_2_3 <- df %>% 
  filter(!is.na(Age.2) & !is.na(Age.3)) %>%
  mutate(
    mean_age = (Age.2 + Age.3)/2,
    diff = Age.2 - Age.3,
    pair = "Reading 2 vs 3"
  )

# Combine all pairs
all_pairs <- bind_rows(pairs_1_2, pairs_1_3, pairs_2_3)

# Calculate limits of agreement for each pair
pair_stats <- all_pairs %>%
  group_by(pair) %>%
  summarize(
    n = n(),
    mean_diff = mean(diff, na.rm=TRUE),
    sd_diff = sd(diff, na.rm=TRUE),
    upper = mean_diff + 1.96*sd_diff,
    lower = mean_diff - 1.96*sd_diff
  )

# Create enhanced Bland-Altman plot
ggplot(all_pairs, aes(x=mean_age, y=diff, color=pair)) +
  geom_point(alpha=0.7) +
  geom_hline(yintercept=0, linetype="dashed") +
  geom_smooth(method="loess", se=TRUE, span=1) +
  facet_wrap(~pair) +
  geom_hline(data=pair_stats, aes(yintercept=mean_diff), color="blue") +
  geom_hline(data=pair_stats, aes(yintercept=upper), linetype="dotted", color="red") +
  geom_hline(data=pair_stats, aes(yintercept=lower), linetype="dotted", color="red") +
  labs(
    title="Bland-Altman Plots for Otolith Reading Pairs",
    x="Mean Age of Pair (days)",
    y="Age Difference (days)",
    color="Reading Pair"
  ) +
  theme_bw() +
  geom_text(data=pair_stats, aes(x=Inf, y=Inf, 
                                 label=paste0("n = ", n, "\nMean diff = ", round(mean_diff, 1))),
            hjust=1.1, vjust=1.5, size=3)
```

## 3. Precision-by-Age-Group Visualization

This visualization is critical for understanding how aging precision varies across age classes:

```{r}
# Create age groups
mean_ages <- mean_ages %>%
  mutate(age_group = cut(mean_age, 
                         breaks=seq(100, 225, by=25),
                         labels=paste0(seq(100, 200, by=25), "-", seq(124, 224, by=25))))

# Calculate precision metrics by age group
age_group_stats <- mean_ages %>%
  filter(!is.na(cv)) %>%
  group_by(age_group) %>%
  summarize(
    n = n(),
    mean_cv = mean(cv, na.rm=TRUE),
    median_cv = median(cv, na.rm=TRUE),
    se_cv = sd(cv, na.rm=TRUE)/sqrt(n())
  )

# Create precision by age group plot
ggplot(age_group_stats, aes(x=age_group, y=mean_cv)) +
  geom_col(fill="steelblue", width=0.7) +
  geom_errorbar(aes(ymin=mean_cv-se_cv, ymax=mean_cv+se_cv), width=0.2) +
  geom_text(aes(y=0.5, label=paste("n =", n)), color="white", fontface="bold") +
  geom_hline(yintercept=5, linetype="dashed", color="red") +
  labs(
    title="Coefficient of Variation by Age Group",
    subtitle="Red line indicates 5% CV threshold commonly used in daily aging studies",
    x="Age Group (days)",
    y="Mean CV (%)"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45, hjust=1))
```

## 4. Integrated FT-NIRS Validation Plot

This visualization directly addresses your need to integrate manual aging precision with FT-NIRS predictions:

```{r}
# Assuming you have NIRS predictions in 'nirs_data' with specimen and preds columns
# Merge with manual reading statistics
combined_data <- mean_ages
combined_data <- combined_data[complete.cases(combined_data$preds), ]
# Create integrated validation plot
ggplot(combined_data, aes(x=mean_age, y=preds)) +
  geom_point(aes(color=cv, size=readings_count), alpha=0.8) +
  geom_abline(intercept=0, slope=1, linetype="dashed", color="red") +
  geom_smooth(method="lm", color="blue", se=TRUE) +
  scale_color_viridis_c(option="plasma", direction=-1, 
                       limits=c(0, max(combined_data$cv, na.rm=TRUE))) +
  labs(
    title="FT-NIRS Predicted Age vs. Manual Mean Age",
    subtitle=paste("R² =", round(summary(lm(preds ~ mean_age, data=combined_data))$r.squared, 3)),
    x="Manual Mean Age (days)",
    y="NIRS Predicted Age (days)",
    color="Manual CV (%)",
    size="Reading Count"
  ) +
  theme_bw() +
  annotate("text", x=min(combined_data$mean_age, na.rm=TRUE), 
           y=max(combined_data$preds, na.rm=TRUE),
           label=paste("RMSE =", 
                      round(sqrt(mean((combined_data$preds - combined_data$mean_age)^2, na.rm=TRUE)), 1),
                      "days"),
           hjust=0, vjust=1)
```

## 5. Precision-Weighted Residual Analysis

This advanced approach weights the importance of NIRS prediction errors by the precision of manual readings:

```{r}
# Calculate weighted residuals
combined_data <- combined_data %>%
  mutate(
    nirs_residual = preds - mean_age,
    weight = ifelse(is.na(cv) | cv == 0, 1, 1/cv)  # Higher weight for more precise manual readings
  )

# Create weighted residual plot
ggplot(combined_data, aes(x=mean_age, y=nirs_residual)) +
  geom_point(aes(size=weight, color=readings_count), alpha=0.7) +
  geom_hline(yintercept=0, linetype="dashed", color="red") +
  geom_smooth(aes(weight=weight), method="loess", span=0.75, se=TRUE) +
  scale_size_continuous(name="Precision Weight", range=c(1, 5)) +
  labs(
    title="Precision-Weighted FT-NIRS Prediction Residuals",
    subtitle="Larger points indicate more precise manual readings (lower CV)",
    x="Manual Mean Age (days)",
    y="NIRS Residual (Predicted - Manual, days)",
    color="Reading Count"
  ) +
  theme_bw()
```

## 6. Multi-component Reliability Assessment

This visualization presents a comprehensive view of both manual reading reliability and NIRS prediction performance:

```{r}
# Calculate key metrics for both manual readings and NIRS predictions
reliability_data <- data.frame(
  Component = rep(c("Manual Readings", "NIRS Predictions"), each=3),
  Metric = c("APE (%)", "CV (%)", "Mean Diff (days)", 
             "RMSE (days)", "MAE (days)", "Bias (days)"),
  Value = c(
    4.42,  # Your reported APE
    mean(combined_data$cv, na.rm=TRUE),  # Mean CV
    mean(abs(all_pairs$diff), na.rm=TRUE),  # Mean absolute difference between readings
    sqrt(mean((combined_data$preds - combined_data$mean_age)^2, na.rm=TRUE)),  # RMSE
    mean(abs(combined_data$preds - combined_data$mean_age), na.rm=TRUE),  # MAE
    mean(combined_data$preds - combined_data$mean_age, na.rm=TRUE)  # Bias
  )
)

# Create multi-component reliability plot
ggplot(reliability_data, aes(x=Metric, y=Value, fill=Component)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_text(aes(label=round(Value, 2)), position=position_dodge(width=0.9), vjust=-0.3) +
  labs(
    title="Reliability Assessment of Manual Readings and FT-NIRS Predictions",
    x=NULL,
    y="Value",
    fill="Component"
  ) +
  scale_fill_brewer(palette="Set1") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45, hjust=1))
```

## 7. Age Group Comparison Matrix

This visualization provides a comprehensive view of manual and NIRS aging performance across age groups:

```{r}
# Add age groups to combined data
combined_data <- combined_data %>%
  mutate(age_group = cut(mean_age, 
                         breaks=seq(100, 225, by=25),
                         labels=paste0(seq(100, 200, by=25), "-", seq(124, 224, by=25))))

# Calculate metrics by age group
age_comparison <- combined_data %>%
  group_by(age_group) %>%
  summarize(
    n = n(),
    manual_cv = mean(cv, na.rm=TRUE),
    nirs_rmse = sqrt(mean((preds - mean_age)^2, na.rm=TRUE)),
    nirs_r2 = summary(lm(preds ~ mean_age))$r.squared,
    nirs_bias = mean(preds - mean_age, na.rm=TRUE)
  ) %>%
  gather(key="metric", value="value", -age_group, -n)

# Create age group comparison matrix
ggplot(age_comparison, aes(x=age_group, y=value, fill=metric)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_text(aes(label=round(value, 2)), position=position_dodge(width=0.9), 
            vjust=-0.3, size=3) +
  facet_wrap(~metric, scales="free_y") +
  labs(
    title="Age Group Comparison: Manual Reading Precision vs. NIRS Performance",
    x="Age Group (days)",
    y="Value"
  ) +
  scale_fill_brewer(palette="Set2") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle=45, hjust=1),
    legend.position = "none",
    strip.background = element_rect(fill="lightblue")
  )
```

## Best Practices for Publication-Quality Visualizations

1. **Report APE prominently**: Your APE of 4.42% indicates good precision for daily aging and should be featured in your visualizations.

2. **Use the mean age as reference**: When lacking designated reference ages, the mean of multiple readings provides the best approximation of "true" age.

3. **Weight by precision**: In FT-NIRS validation, give more weight to specimens with more precise manual readings.

4. **Stratify by age group**: Both aging precision and NIRS performance often vary with fish age.

5. **Show sample sizes**: Always indicate the number of specimens in each analysis group.

6. **Use consistent terminology**: Clearly distinguish between manual reading precision metrics (APE, CV) and NIRS prediction performance metrics (RMSE, R²).

7. **Document limitations**: Acknowledge the absence of validated reference ages in figure captions and text.

These specialized visualization approaches will maximize the value of your current dataset structure while providing robust validation for your FT-NIRS age prediction models. They follow current best practices in fisheries science and are suitable for high-impact journal publications.


_-----------------------------------------------------------------------------------------------------------------------





```{r}

library(FSA)
library(dplyr)

# Assuming data frame with columns: specimen_id, reading1, reading2 
# (where reading1 and reading2 are your own repeated counts)

df <- read.csv("ages_LPW.csv")
?agePrecision
# Calculate within-reader precision
self_precision <- agePrecision(~Age.1 + Age.2 + Age.3, data=df)
summary(self_precision, what = c("precision", "difference", "absolute difference", "details"))

# Extract key metrics
cv <- self_precision$CV  # Coefficient of variation
ape <- self_precision$APE  # Average percent error
cv
ape

# ___________ PLOTS ____________

library(ggplot2)
library(dplyr)

# Calculate differences for each pair where both readings exist
df_diff <- df %>%
  mutate(
    diff_1_2 = Age.1 - Age.2,
    diff_1_3 = Age.1 - Age.3,
    diff_2_3 = Age.2 - Age.3
  )

# Combine all differences into one column for plotting
all_diffs <- c(
  df_diff$diff_1_2[!is.na(df_diff$diff_1_2)],
  df_diff$diff_1_3[!is.na(df_diff$diff_1_3)],
  df_diff$diff_2_3[!is.na(df_diff$diff_2_3)]
)

# Create histogram of all age differences
ggplot(data.frame(difference = all_diffs), aes(x = difference)) +
  geom_histogram(binwidth = 2, fill = "steelblue", color = "black") +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", size = 1) +
  labs(
    title = "Distribution of Age Differences Between Repeated Readings",
    x = "Age Difference (days)",
    y = "Frequency"
  ) +
  theme_bw()

# Calculate mean age for specimens with multiple readings
df_mean <- df %>%
  rowwise() %>%
  mutate(mean_age = mean(c(Age.1, Age.2, Age.3), na.rm = TRUE)) %>%
  filter(!is.na(mean_age))

# Create age-bias plot for first vs. second reading
ggplot(df_mean, aes(x = Age.1, y = Age.2)) +
  geom_point(alpha = 0.7) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(
    title = "Age Bias Plot: First vs. Second Reading",
    x = "First Reading (days)",
    y = "Second Reading (days)"
  ) +
  theme_bw()



# OTHER PLOTS MAYBE 

library(ggplot2)
library(dplyr)

# Create paired datasets for comparison
pairs_1_2 <- df %>% 
  filter(!is.na(Age.1) & !is.na(Age.2)) %>%
  select(specimen, Age.1, Age.2)

pairs_1_3 <- df %>%
  filter(!is.na(Age.1) & !is.na(Age.3)) %>%
  select(specimen, Age.1, Age.3)

pairs_2_3 <- df %>%
  filter(!is.na(Age.2) & !is.na(Age.3)) %>%
  select(specimen, Age.2, Age.3)

# Create age bias plot for Reading 1 vs Reading 2
ggplot(pairs_1_2, aes(x=Age.1, y=Age.2)) +
  geom_point(alpha=0.7) +
  geom_abline(intercept=0, slope=1, linetype="dashed", color="red") +
  geom_smooth(method="lm", se=FALSE, color="blue") +
  labs(
    title="Age Comparison: Reading 1 vs Reading 2",
    x="Reading 1 (days)",
    y="Reading 2 (days)"
  ) +
  theme_bw() +
  annotate("text", x=min(pairs_1_2$Age.1), y=max(pairs_1_2$Age.2),
           label=paste("n =", nrow(pairs_1_2)), hjust=0, vjust=1)



```

# Specialized Visualization Methods for Otolith Aging Bias with Multiple Readings

As a fisheries aging expert, Ill provide tailored visualization approaches specifically designed for your dataset structure where you have 1-3 readings per specimen without reference ages. These methods represent best practices in the field for self-consistency analysis.

## 1. Pairwise Age Comparison Plots

This approach is ideal for your dataset structure, comparing each pair of readings directly:

```{r}

library(ggplot2)
library(dplyr)

# Create paired datasets for comparison
pairs_1_2 <- df %>% 
  filter(!is.na(Age.1) & !is.na(Age.2)) %>%
  select(specimen, Age.1, Age.2)

pairs_1_3 <- df %>%
  filter(!is.na(Age.1) & !is.na(Age.3)) %>%
  select(specimen, Age.1, Age.3)

pairs_2_3 <- df %>%
  filter(!is.na(Age.2) & !is.na(Age.3)) %>%
  select(specimen, Age.2, Age.3)

# Create age bias plot for Reading 1 vs Reading 2
ggplot(pairs_1_2, aes(x=Age.1, y=Age.2)) +
  geom_point(alpha=0.7) +
  geom_abline(intercept=0, slope=1, linetype="dashed", color="red") +
  geom_smooth(method="lm", se=FALSE, color="blue") +
  labs(
    title="Age Comparison: Reading 1 vs Reading 2",
    x="Reading 1 (days)",
    y="Reading 2 (days)"
  ) +
  theme_bw() +
  annotate("text", x=min(pairs_1_2$Age.1), y=max(pairs_1_2$Age.2),
           label=paste("n =", nrow(pairs_1_2)), hjust=0, vjust=1)
```

## 2. Multi-panel Comparison Matrix

This creates a comprehensive view of all reading pairs:

```{r}
library(ggplot2)
library(patchwork)

# Function to create comparison plot
create_comparison_plot <- function(data, x_var, y_var, x_lab, y_lab) {
  data_subset <- data %>% filter(!is.na(!!sym(x_var)) & !is.na(!!sym(y_var)))
  
  # Calculate regression and correlation
  fit <- lm(formula(paste(y_var, "~", x_var)), data=data_subset)
  r_value <- cor(data_subset[[x_var]], data_subset[[y_var]], use="complete.obs")
  
  ggplot(data_subset, aes_string(x=x_var, y=y_var)) +
    geom_point(alpha=0.7) +
    geom_abline(intercept=0, slope=1, linetype="dashed", color="red") +
    geom_smooth(method="lm", se=FALSE, color="blue") +
    labs(
      x=x_lab,
      y=y_lab,
      subtitle=paste("n =", nrow(data_subset), "| r =", round(r_value, 3))
    ) +
    theme_bw()
}

# Create the three plots
p1 <- create_comparison_plot(df, "Age.1", "Age.2", "Reading 1 (days)", "Reading 2 (days)")
p2 <- create_comparison_plot(df, "Age.1", "Age.3", "Reading 1 (days)", "Reading 3 (days)")
p3 <- create_comparison_plot(df, "Age.2", "Age.3", "Reading 2 (days)", "Reading 3 (days)")

# Combine plots
(p1 + p2) / (p3 + plot_spacer()) +
  plot_annotation(title="Multi-panel Age Comparison Matrix")
```

## 3. Difference vs. Mean Plots (Modified Bland-Altman)

Ideal for showing how differences relate to the magnitude of measurements:

```{r}
library(tidyr)

# Create long-format dataset for all paired comparisons
pairs_all <- df %>%
  rowwise() %>%
  mutate(
    # Calculate pairwise differences and means
    diff_1_2 = ifelse(!is.na(Age.1) & !is.na(Age.2), Age.1 - Age.2, NA),
    mean_1_2 = ifelse(!is.na(Age.1) & !is.na(Age.2), (Age.1 + Age.2)/2, NA),
    diff_1_3 = ifelse(!is.na(Age.1) & !is.na(Age.3), Age.1 - Age.3, NA),
    mean_1_3 = ifelse(!is.na(Age.1) & !is.na(Age.3), (Age.1 + Age.3)/2, NA),
    diff_2_3 = ifelse(!is.na(Age.2) & !is.na(Age.3), Age.2 - Age.3, NA),
    mean_2_3 = ifelse(!is.na(Age.2) & !is.na(Age.3), (Age.2 + Age.3)/2, NA)
  ) %>%
  ungroup()

# Convert to long format for plotting
diff_long <- pairs_all %>%
  select(specimen, contains("diff_"), contains("mean_")) %>%
  pivot_longer(
    cols = -specimen,
    names_to = c(".value", "comparison"),
    names_pattern = "(diff|mean)_(.*)"
  ) %>%
  filter(!is.na(diff)) %>%
  mutate(comparison = case_when(
    comparison == "1_2" ~ "Reading 1 vs 2",
    comparison == "1_3" ~ "Reading 1 vs 3",
    comparison == "2_3" ~ "Reading 2 vs 3"
  ))

# Calculate statistics for reference lines
diff_stats <- diff_long %>%
  group_by(comparison) %>%
  summarize(
    mean_diff = mean(diff, na.rm=TRUE),
    sd_diff = sd(diff, na.rm=TRUE),
    upper_lim = mean_diff + 1.96*sd_diff,
    lower_lim = mean_diff - 1.96*sd_diff,
    n = n()
  )

# Create Bland-Altman style plot
ggplot(diff_long, aes(x=mean, y=diff, color=comparison)) +
  geom_point(alpha=0.7) +
  geom_hline(yintercept=0, linetype="dashed") +
  geom_smooth(method="loess", se=TRUE, alpha=0.2) +
  facet_wrap(~comparison) +
  geom_hline(data=diff_stats, aes(yintercept=mean_diff), color="blue") +
  geom_hline(data=diff_stats, aes(yintercept=upper_lim), linetype="dotted", color="red") +
  geom_hline(data=diff_stats, aes(yintercept=lower_lim), linetype="dotted", color="red") +
  labs(
    title="Difference vs. Mean Age Plots",
    x="Mean Age (days)",
    y="Age Difference (days)",
    color="Comparison"
  ) +
  theme_bw() +
  geom_text(data=diff_stats, aes(x=Inf, y=Inf, 
                               label=paste0("n = ", n, "\nMean diff = ", round(mean_diff, 1))),
           hjust=1.1, vjust=1.5, size=3)
```

## 4. Precision by Age Group Visualization

Shows how aging precision varies across the age spectrum:

```{r}
# Calculate mean age and CV for each specimen with multiple readings
precision_data <- df %>%
  rowwise() %>%
  mutate(
    readings = sum(!is.na(c(Age.1, Age.2, Age.3))),
    mean_age = mean(c(Age.1, Age.2, Age.3), na.rm=TRUE),
    sd_age = sd(c(Age.1, Age.2, Age.3), na.rm=TRUE),
    cv = ifelse(readings > 1, sd_age/mean_age*100, NA),
    age_group = cut(mean_age, 
                   breaks=seq(100, 250, by=25),
                   labels=paste0(seq(100, 225, by=25), "-", seq(124, 249, by=25)))
  ) %>%
  filter(readings > 1) %>%
  ungroup()

# Visualize precision by age group
ggplot(precision_data, aes(x=age_group, y=cv)) +
  geom_boxplot(fill="lightblue", alpha=0.7) +
  geom_jitter(width=0.2, alpha=0.5) +
  geom_hline(yintercept=5, linetype="dashed", color="red") +
  labs(
    title="Aging Precision by Age Group",
    subtitle="Red line indicates 5% CV threshold",
    x="Age Group (days)",
    y="Coefficient of Variation (%)"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45, hjust=1))
```

## 5. Interactive Discrepancy Explorer

For identifying specific specimens with high discrepancies:

```{r}
library(plotly)

# Calculate absolute percent error for each specimen
discrepancy_data <- df %>%
  rowwise() %>%
  mutate(
    readings = sum(!is.na(c(Age.1, Age.2, Age.3))),
    mean_age = mean(c(Age.1, Age.2, Age.3), na.rm=TRUE),
    max_diff = ifelse(readings > 1, 
                     max(abs(c(Age.1-mean_age, Age.2-mean_age, Age.3-mean_age)), na.rm=TRUE),
                     NA),
    percent_error = (max_diff/mean_age)*100
  ) %>%
  filter(readings > 1) %>%
  arrange(desc(percent_error))

# Create interactive plot
p <- ggplot(discrepancy_data, aes(x=mean_age, y=percent_error, 
                                text=paste("Specimen:", specimen,
                                          "\nMean age:", round(mean_age, 1),
                                          "\nReadings:", readings,
                                          "\nAge.1:", Age.1,
                                          "\nAge.2:", Age.2,
                                          "\nAge.3:", Age.3))) +
  geom_point(aes(size=readings, color=percent_error)) +
  scale_color_gradient(low="blue", high="red") +
  geom_hline(yintercept=5, linetype="dashed") +
  labs(
    title="Specimens by Aging Discrepancy",
    x="Mean Age (days)",
    y="Maximum Percent Error (%)",
    size="Number of Readings",
    color="Percent Error"
  ) +
  theme_bw()

# Convert to interactive plot
ggplotly(p, tooltip="text")
```

## 6. Cumulative Distribution of Differences

Shows the proportion of specimens falling within different levels of agreement:

```{r}
# Calculate absolute differences for all pairs
all_diffs <- c(
  abs(df$Age.1 - df$Age.2)[!is.na(df$Age.1) & !is.na(df$Age.2)],
  abs(df$Age.1 - df$Age.3)[!is.na(df$Age.1) & !is.na(df$Age.3)],
  abs(df$Age.2 - df$Age.3)[!is.na(df$Age.2) & !is.na(df$Age.3)]
)

# Create data frame for plotting
diff_df <- data.frame(abs_diff = all_diffs)

# Plot cumulative distribution
ggplot(diff_df, aes(x=abs_diff)) +
  stat_ecdf(geom="step", color="blue", size=1) +
  geom_vline(xintercept=c(5, 10, 15), linetype="dashed", color="red") +
  annotate("text", x=c(5, 10, 15), y=c(0.05, 0.05, 0.05), 
           label=c("5 days", "10 days", "15 days"), 
           hjust=-0.1, color="red") +
  labs(
    title="Cumulative Distribution of Absolute Age Differences",
    x="Absolute Difference Between Readings (days)",
    y="Cumulative Proportion"
  ) +
  theme_bw()
```

## 7. Heatmap of Reading Agreement

Visualizes the distribution of age differences:

```{r}
# Calculate all pairwise differences
all_pair_diffs <- bind_rows(
  df %>% 
    filter(!is.na(Age.1) & !is.na(Age.2)) %>%
    mutate(diff = Age.1 - Age.2, pair = "Reading 1-2"),
  df %>% 
    filter(!is.na(Age.1) & !is.na(Age.3)) %>%
    mutate(diff = Age.1 - Age.3, pair = "Reading 1-3"),
  df %>% 
    filter(!is.na(Age.2) & !is.na(Age.3)) %>%
    mutate(diff = Age.2 - Age.3, pair = "Reading 2-3")
)

# Bin the differences
all_pair_diffs <- all_pair_diffs %>%
  mutate(diff_bin = cut(diff, 
                       breaks=seq(-40, 40, by=5),
                       labels=paste0(seq(-40, 35, by=5), " to ", seq(-36, 39, by=5))))

# Count occurrences
diff_counts <- all_pair_diffs %>%
  count(pair, diff_bin) %>%
  complete(pair, diff_bin, fill = list(n = 0))

# Create heatmap
ggplot(diff_counts, aes(x=diff_bin, y=pair, fill=n)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(
    title="Distribution of Age Differences Between Readings",
    x="Age Difference (days)",
    y="Reading Pair",
    fill="Count"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5))
```

## Implementation Recommendations

1. **Start with the pairwise comparison plots** to visualize the overall relationship between readings

2. **Use the difference vs. mean plots** to identify any systematic bias related to fish age

3. **Implement the precision by age group visualization** to determine if certain age ranges are more challenging to interpret

4. **Create the discrepancy explorer** to identify specific specimens that may need re-examination

5. **Consider running these visualizations separately** for different cohorts or collection periods if applicable

These specialized visualization approaches are designed to maximize the information from your specific dataset structure with 1-3 readings per specimen and no reference ages. They follow current best practices in fisheries science for self-consistency analysis and provide a comprehensive evaluation of your aging precision.



