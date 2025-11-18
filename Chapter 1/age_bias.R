agereads <- read.csv("age_bias_esther.csv")
names(agereads)







library(readxl)
library(tidyverse)
library(FSA)
library(lubridate) # Helps with date handling

library(FSA)
library(dplyr)

# 1. Calculate Row Means & Filter
# We assume your dataframe is named 'agereads'
data_prep <- agereads %>%
  rowwise() %>%
  mutate(
    # Calculate mean of your 3 potential reads (ignoring NAs)
    My_Mean = mean(c(y.age1, y.age2, y.age3), na.rm = TRUE),
    
    # Calculate mean of expert's potential reads
    Exp_Mean = mean(c(x.age1, x.age2, x.age3), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  # Filter: Keep only specimens where BOTH you and expert have an age
  filter(!is.nan(My_Mean) & !is.nan(Exp_Mean))




# 1. Create the ageBias object
# Formula: NonReference (You) ~ Reference (Expert)
ab_model <- ageBias(My_Mean ~ Exp_Mean, 
                    data = data_prep,
                    ref.lab = "Experienced Reader Age (days)",
                    nref.lab = "First Author Age (days)")

# 2. Plot using the Campana style
# 'col.CIsig = "red"' makes the error bar red if you are significantly biased
# 'show.n = TRUE' puts the sample size number above the x-axis
plotAB(ab_model,
       what = "Campana",
       col.CIsig = "red",
       pch.mean.sig = 21, # Open circle for biased means (standard convention)
       show.n = F)

# Calculate precision across ALL columns provided in the formula
# This will include your replicates and the expert's replicates
ap_model <- agePrecision(~ y.age1 + y.age2 + y.age3 + x.age1 + x.age2 + x.age3, 
                         data = data_prep)

# View the summary stats
# Look for 'ACV' (Average Coefficient of Variation)
summary(ap_model, what = "precision")

