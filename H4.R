#this line runs the data import and recode script
source("R_Scripts/1_data_import.R")

#H4 - Incivility climate: Feeling deterred by identity-based attacks (kiss_module_lispop_1) predicts greater support for “telling it like it is” (lispop_2) and belief that authenticity wins votes (lispop_3).
#Call kiss_module_lipop_1 something like in 1_data_import. R 
# crossstab with the categorical version of lispop_2 and lispop_3

library(crosstable)
library(forcats)
library(flextable)
# Crosstab with categorical factors
#truth
tab_truth <- crosstable(
  kiss_module_lispop_1 ~ Truth,
  data = ces,
  percent_pattern = "{n} ({p_col}%)",
  test = TRUE
)
ft_truth <- as_flextable(tab_truth)
ft_truth

#ordinary
tab_ordinary <- crosstable(
  kiss_module_lispop_1 ~ Ordinary,
  data = ces,
  percent_pattern = "{n} ({p_col}%)",
  test = TRUE
)

ft_ordinary <- as_flextable(tab_ordinary)
ft_ordinary