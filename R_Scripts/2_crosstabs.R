#this line runs the data import and recode script
source("R_Scripts/1_data_import.R")
# OPTION 1: Comprehensive stacked table with all IVs
# Create gender table
ct_gender <- crosstable(
  ces,
  by = Truth,
  cols = gender_2cat,
  total = "both",
  percent_pattern = "{p_row}% (n={n})",
  percent_digits = 1
) %>%
  mutate(Variable = "Gender", .before = 1)
ct_gender
# Create education table
ct_education <- crosstable(
  ces,
  by = Truth,
 cols = education_3cat,
  total = "both",
  percent_pattern = "{p_row}% (n={n})",
  percent_digits = 1
) %>%
  mutate(Variable = "Education", .before = 1)
ct_education
# Create religiosity table
ct_religiosity <- crosstable(
  ces,
  by = Truth,
  cols = religiosity,
  total = "both",
  percent_pattern = "{p_row}% (n={n})",
  percent_digits = 1
) %>%
  mutate(Variable = "Religiosity", .before = 1)
ct_religiosity

# Create age table
ct_age <- crosstable(
  ces,
  by = Truth,
  cols = age_5cat,
  total = "both",
  percent_pattern = "{p_row}% (n={n})",
  percent_digits = 1
) %>%
  mutate(Variable = "Age", .before = 1)
ct_age

# Combine all four tables
ct_all_ivs <- bind_rows(ct_gender, ct_education, ct_religiosity, ct_age)

# Select only the columns you want (exclude "label" column)
ct_all_ivs_clean <- ct_all_ivs %>%
  select(-label)

# Create flextable without the label column
ft_all <- ct_all_ivs_clean %>%
  as_flextable(keep_id = FALSE) %>%
  set_header_labels(
    Variable = "Independent Variable",
    gender_2cat = "Gender",
    education_3cat = "Education",
    religiosity = "Religiosity",
    age_5cat = "Age",
    .variable = "Lispop Response",
    total = "Total"
  ) %>%
  autofit()

ft_all



 
