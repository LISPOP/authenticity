#this line runs the data import and recode script
source("R_Scripts/1_data_import.R")
# OPTION 1: Comprehensive stacked table with all IVs
# Create gender table
ct_gender <- crosstable(
  ces,
  by = lispop_3cat,
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
  by = lispop_3cat,
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
  by = lispop_3cat,
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
  by = lispop_3cat,
  cols = age_5cat,
  total = "both",
  percent_pattern = "{p_row}% (n={n})",
  percent_digits = 1
) %>%
  mutate(Variable = "Age", .before = 1)
ct_age

# Combine all four tables
ct_all_ivs <- bind_rows(ct_gender, ct_education, ct_religiosity, ct_age)

# Create flextable
ft_all <- ct_all_ivs %>%
  as_flextable(keep_id = TRUE) %>%
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




 
