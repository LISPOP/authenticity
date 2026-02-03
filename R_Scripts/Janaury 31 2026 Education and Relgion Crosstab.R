# Load required packages
library(dplyr)
library(labelled)
library(crosstable)
library(flextable)
library(tidyr)


# OPTION 1: Comprehensive stacked table with all IVs
# Create gender table
ct_gender <- crosstable(
  ces,
  cols = lispop_3cat,
  by = gender_2cat,
  total = "both",
  percent_pattern = "{p_row}% (n={n})",
  percent_digits = 1
) %>%
  mutate(Variable = "Gender", .before = 1)

# Create education table
ct_education <- crosstable(
  ces,
  cols = lispop_3cat,
  by = education_3cat,
  total = "both",
  percent_pattern = "{p_row}% (n={n})",
  percent_digits = 1
) %>%
  mutate(Variable = "Education", .before = 1)

# Create religiosity table
ct_religiosity <- crosstable(
  ces,
  cols = lispop_3cat,
  by = religiosity,
  total = "both",
  percent_pattern = "{p_row}% (n={n})",
  percent_digits = 1
) %>%
  mutate(Variable = "Religiosity", .before = 1)

# Combine all three tables
ct_all_ivs <- bind_rows(ct_gender, ct_education, ct_religiosity)

# Create flextable
ft_all <- ct_all_ivs %>%
  as_flextable(keep_id = TRUE) %>%
  set_header_labels(
    Variable = "Independent Variable",
    gender_2cat = "Gender",
    education_3cat = "Education",
    religiosity = "Religiosity",
    .variable = "Lispop Response",
    total = "Total"
  ) %>%
  autofit()

ft_all

# OPTION 2: Education x Religiosity cross-tab (as requested)
# First, let's create a cross-tab of education by religiosity
ct_educ_relig <- crosstable(
  ces,
  cols = religiosity,        # DV in columns (or could switch based on your hypothesis)
  by = education_3cat,       # IV in rows
  total = "both",
  percent_pattern = "{p_row}% (n={n})",
  percent_digits = 1
)

ft_educ_relig <- ct_educ_relig %>%
  as_flextable(keep_id = TRUE) %>%
  set_header_labels(
    education_3cat = "Education Level",
    religiosity = "Religiosity",
    `Very important` = "Very important (%)",
    `Somewhat important` = "Somewhat important (%)",
    `Not very important` = "Not very important (%)",
    `Not important at all` = "Not important at all (%)",
    `DK/Refused` = "DK/Refused (%)",
    total = "Total"
  ) %>%
  bold(part = "header") %>%
  autofit()

ft_educ_relig

# OPTION 3: Three-way cross-tab (Education x Religiosity x Lispop response)
# Create combined category for education and religiosity
ces <- ces %>%
  mutate(
    educ_relig_combo = paste(education_3cat, religiosity, sep = " - "),
    educ_relig_combo = factor(educ_relig_combo)
  )

ct_educ_relig_lispop <- crosstable(
  ces,
  cols = lispop_3cat,          # DV in columns
  by = educ_relig_combo,       # Combined IV in rows
  total = "both",
  percent_pattern = "{p_row}% (n={n})",
  percent_digits = 1
)

ft_educ_relig_lispop <- ct_educ_relig_lispop %>%
  as_flextable(keep_id = TRUE) %>%
  autofit()

ft_educ_relig_lispop

# OPTION 4: Separate tables for clearer presentation
# Table 1: Education distribution by Religiosity categories
ct_educ_by_relig <- crosstable(
  ces,
  cols = education_3cat,      # DV in columns
  by = religiosity,           # IV in rows
  total = "both",
  percent_pattern = "{p_row}% (n={n})",
  percent_digits = 1
)

ft_educ_by_relig <- ct_educ_by_relig %>%
  as_flextable(keep_id = TRUE) %>%
  set_header_labels(
    religiosity = "Religiosity",
    education_3cat = "Education Level",
    `High school or less` = "HS or less (%)",
    `Some post-secondary` = "Some post-sec (%)",
    `University degree` = "Univ degree (%)",
    `DK/Refused` = "DK/Refused (%)",
    total = "Total"
  ) %>%
  bold(part = "header") %>%
  autofit()

ft_educ_by_relig

# Table 2: Religiosity distribution by Education categories
ct_relig_by_educ <- crosstable(
  ces,
  cols = religiosity,         # DV in columns
  by = education_3cat,        # IV in rows
  total = "both",
  percent_pattern = "{p_row}% (n={n})",
  percent_digits = 1
)

ft_relig_by_educ <- ct_relig_by_educ %>%
  as_flextable(keep_id = TRUE) %>%
  set_header_labels(
    education_3cat = "Education Level",
    religiosity = "Religiosity",
    `Very important` = "Very important (%)",
    `Somewhat important` = "Somewhat important (%)",
    `Not very important` = "Not very important (%)",
    `Not important at all` = "Not important at all (%)",
    `DK/Refused` = "DK/Refused (%)",
    total = "Total"
  ) %>%
  bold(part = "header") %>%
  autofit()

ft_relig_by_educ

# OPTION 5: Create a summary statistics table
summary_stats <- ces %>%
  group_by(education_3cat, religiosity) %>%
  summarise(
    n = n(),
    .groups = 'drop'
  ) %>%
  group_by(education_3cat) %>%
  mutate(
    pct = round(n / sum(n) * 100, 1)
  ) %>%
  ungroup()

# Create a formatted summary table
summary_table <- summary_stats %>%
  mutate(
    display = paste0(pct, "% (n=", n, ")")
  ) %>%
  select(education_3cat, religiosity, display) %>%
  pivot_wider(
    names_from = religiosity,
    values_from = display,
    values_fill = "0% (n=0)"
  )

# Display as flextable
ft_summary <- flextable(summary_table) %>%
  set_header_labels(
    education_3cat = "Education Level"
  ) %>%
  bold(part = "header") %>%
  autofit()

ft_summary

# Print distributions for verification
cat("\n=== VARIABLE DISTRIBUTIONS ===\n")

cat("\nGender:\n")
print(table(ces$gender_2cat, useNA = "always"))

cat("\nEducation:\n")
print(table(ces$education_3cat, useNA = "always"))

cat("\nReligiosity:\n")
print(table(ces$religiosity, useNA = "always"))

cat("\nLispop Response:\n")
print(table(ces$lispop_3cat, useNA = "always"))

# Calculate sample sizes
cat("\n=== SAMPLE SIZES ===\n")
cat("Total observations:", nrow(ces), "\n")
cat("Observations with valid gender:", sum(!is.na(ces$gender_2cat)), "\n")
cat("Observations with valid education:", sum(!is.na(ces$education_3cat)), "\n")
cat("Observations with valid religiosity:", sum(!is.na(ces$religiosity)), "\n")
cat("Observations with valid lispop response:", sum(!is.na(ces$lispop_3cat)), "\n")

# Cross-tab of education and religiosity (raw counts)
cat("\n=== EDUCATION × RELIGIOSITY CROSS-TAB (COUNTS) ===\n")
print(table(ces$education_3cat, ces$religiosity, useNA = "always"))

# Cross-tab of education and religiosity (row percentages)
cat("\n=== EDUCATION × RELIGIOSITY CROSS-TAB (ROW %) ===\n")
print(prop.table(table(ces$education_3cat, ces$religiosity), 1) * 100)
