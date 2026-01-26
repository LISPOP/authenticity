# install packages if necessary
devtools::install_github("sjkiss/cesdata2")
#devtools::install_github("sjikss/cesdata2") - don't use, won't run

# load libraries
library(cesdata2)
library(tidyverse)
library(haven)
library(here)
library(labelled)
#load the dataset called ces25b
data("ces25b")

# the module variables are in two batches
# The first is a set of questions on housing and social class.
# We may not use these.

ces25b %>% 
  select(kiss_module_Q1:kiss_module_Q9_3) %>% 
  var_label()

# second set, specifically about authenticity is 

ces25b %>% 
  select(kiss_module_Q10_1:kiss_module_lispop_5_6) %>% 
  var_label()




------------------------


# Load required packages
library(dplyr)
library(labelled)
library(crosstable)
library(flextable)

# Create recoded variables with exact coding from codebook
ces <- ces25b %>%
  select(kiss_module_lispop_2, cps25_genderid, cps25_education) %>%
  mutate(
    # Recode gender into 2 categories (Male/Female only)
    # From codebook: 1 = A man, 2 = A woman, 3 = Non-binary, 4 = Another gender
    gender_2cat = case_when(
      cps25_genderid == 1 ~ "Male",
      cps25_genderid == 2 ~ "Female",
      cps25_genderid %in% c(3, 4) ~ NA_character_,  # Recode 3 and 4 as NA
      TRUE ~ NA_character_
    ),
    gender_2cat = factor(gender_2cat, levels = c("Male", "Female")),
    
    # Recode lispop variable based on codebook
    # kiss_module_lispop_2: 1=Strongly disagree, 2=Somewhat disagree, 
    # 3=Neither agree nor disagree, 4=Somewhat agree, 5=Strongly agree, 
    # 6=Don't know/Prefer not to answer
    lispop_3cat = case_when(
      kiss_module_lispop_2 %in% c(4, 5) ~ "Agree",        # Somewhat agree + Strongly agree
      kiss_module_lispop_2 == 3 ~ "Neutral",              # Neither agree nor disagree
      kiss_module_lispop_2 %in% c(1, 2) ~ "Disagree",     # Strongly + Somewhat disagree
      kiss_module_lispop_2 == 6 ~ "DK/Refused",           # Don't know/Prefer not to answer
      TRUE ~ NA_character_
    ),
    lispop_3cat = factor(lispop_3cat, 
                         levels = c("Disagree", "Neutral", "Agree", "DK/Refused")),
    
    # Create simplified education categories
    # From codebook: 1=No schooling, 2=Some elementary, 3=Completed elementary,
    # 4=Some secondary/high school, 5=Completed secondary/high school,
    # 6=Some technical/college, 7=Completed technical/college,
    # 8=Some university, 9=Bachelor's, 10=Master's, 11=Professional/doctorate,
    # 12=Don't know/Prefer not to answer
    education_3cat = case_when(
      cps25_education %in% 1:5 ~ "High school or less",
      cps25_education %in% 6:7 ~ "Some post-secondary",
      cps25_education %in% 8:11 ~ "University degree",
      cps25_education == 12 ~ "DK/Refused",
      TRUE ~ NA_character_
    ),
    education_3cat = factor(education_3cat,
                            levels = c("High school or less", 
                                       "Some post-secondary", 
                                       "University degree",
                                       "DK/Refused"))
  )

# Check distributions
table(ces$gender_2cat, useNA = "always")
table(ces$lispop_3cat, useNA = "always")
table(ces$education_3cat, useNA = "always")

# OPTION 1: Stacked tables (cleanest presentation)
# Create gender table
ct_gender <- crosstable(
  ces,
  cols = lispop_3cat,        # DV in columns
  by = gender_2cat,          # IV in rows (gender)
  total = "both",
  percent_pattern = "{p_row}% (n={n})",
  percent_digits = 1
) %>%
  mutate(Variable = "Gender", .before = 1)

# Create education table
ct_education <- crosstable(
  ces,
  cols = lispop_3cat,        # DV in columns
  by = education_3cat,       # IV in rows (education)
  total = "both",
  percent_pattern = "{p_row}% (n={n})",
  percent_digits = 1
) %>%
  mutate(Variable = "Education", .before = 1)

# Combine tables
ct_stacked <- bind_rows(ct_gender, ct_education)

# Create flextable
ft_stacked <- ct_stacked %>%
  as_flextable(keep_id = TRUE) %>%
  set_header_labels(
    Variable = "Independent Variable",
    gender_2cat = "Gender",
    education_3cat = "Education",
    .variable = "Response",
    total = "Total"
  ) %>%
  autofit()

ft_stacked

# OPTION 2: Single crosstable with both IVs
# This creates one table with gender AND education as separate row groups
ct_combined <- crosstable(
  ces,
  cols = lispop_3cat,                    # DV in columns
  by = c(gender_2cat, education_3cat),   # Both IVs in rows
  total = "both",
  percent_pattern = "{p_row}% (n={n})",
  percent_digits = 1
)

ft_combined <- ct_combined %>%
  as_flextable(keep_id = TRUE) %>%
  autofit()

ft_combined

# OPTION 3: Just gender table (as originally requested)
ct_gender_simple <- crosstable(
  ces,
  cols = lispop_3cat,           # DV in columns
  by = gender_2cat,             # IV in rows
  total = "both",
  percent_pattern = "{p_row}% (n={n})",
  percent_digits = 1
)

ft_gender <- ct_gender_simple %>%
  as_flextable(keep_id = TRUE) %>%
  set_header_labels(
    gender_2cat = "Gender",
    .variable = "Lispop Response",
    Disagree = "Disagree (%)",
    Neutral = "Neutral (%)",
    Agree = "Agree (%)",
    `DK/Refused` = "DK/Refused (%)",
    total = "Total"
  ) %>%
  bold(part = "header") %>%
  autofit()

ft_gender

# OPTION 4: For a more detailed analysis - create both 2x3 and collapsed Agree/Disagree
# First create collapsed lispop variable (Agree vs Disagree, excluding neutrals)
ces <- ces %>%
  mutate(
    lispop_agree_disagree = case_when(
      kiss_module_lispop_2 %in% c(4, 5) ~ "Agree",
      kiss_module_lispop_2 %in% c(1, 2) ~ "Disagree",
      TRUE ~ NA_character_  # Exclude neutrals and DK/Refused for this analysis
    ),
    lispop_agree_disagree = factor(lispop_agree_disagree, 
                                   levels = c("Disagree", "Agree"))
  )

# Create table with collapsed lispop variable
ct_collapsed <- crosstable(
  ces,
  cols = lispop_agree_disagree,  # Binary DV in columns
  by = c(gender_2cat, education_3cat),  # Both IVs in rows
  total = "both",
  percent_pattern = "{p_row}% (n={n})",
  percent_digits = 1
)

ft_collapsed <- ct_collapsed %>%
  as_flextable(keep_id = TRUE) %>%
  autofit()

ft_collapsed

# Print counts for verification
cat("\n=== GENDER DISTRIBUTION ===\n")
print(table(ces$gender_2cat, useNA = "always"))

cat("\n=== LISPOP RESPONSE DISTRIBUTION ===\n")
print(table(ces$lispop_3cat, useNA = "always"))

cat("\n=== EDUCATION DISTRIBUTION ===\n")
print(table(ces$education_3cat, useNA = "always"))

# Calculate sample sizes
cat("\n=== SAMPLE SIZES ===\n")
cat("Total observations:", nrow(ces), "\n")
cat("Observations with valid gender:", sum(!is.na(ces$gender_2cat)), "\n")
cat("Observations with valid lispop response:", sum(!is.na(ces$lispop_3cat)), "\n")
cat("Observations with valid education:", sum(!is.na(ces$education_3cat)), "\n")
