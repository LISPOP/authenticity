# H7.R
rm(list = ls())

source("R_Scripts/1_data_import.R")

library(dplyr)
library(crosstable)
library(flextable)

#Overwriting the H7 recode in this script
#Assumes: 1=Strongly disagree, 2=Somewhat disagree, 3=Neutral, 4=Somewhat agree, 5=Strongly agree, 6=DK/Refused
ces <- ces %>%
  mutate(
    opinion_leaders = case_when(
      kiss_module_lispop_4 %in% c(4, 5) ~ "Agree",
      kiss_module_lispop_4 == 3 ~ "Neutral",
      kiss_module_lispop_4 %in% c(1, 2) ~ "Disagree",
      kiss_module_lispop_4 == 6 ~ NA_character_,
      TRUE ~ NA_character_
    ),
    opinion_leaders = factor(opinion_leaders, levels = c("Disagree", "Neutral", "Agree"))
  )

table(ces$opinion_leaders, useNA = "ifany")
table(ces$Truth, useNA = "ifany")
table(ces$Ordinary, useNA = "ifany")

#Crosstab 1: opinion_leaders x Truth (lispop_2 recode)
ct_truth <- crosstable(
  ces,
  cols = Truth,
  by = opinion_leaders,
  total = "both",
  percent_pattern = "{p_row}% (n={n})",
  percent_digits = 1
)

ft_truth <- ct_truth %>%
  as_flextable(keep_id = TRUE) %>%
  autofit()

ft_truth

#Crosstab 2: opinion_leaders x Ordinary (lispop_3 recode)
ct_ordinary <- crosstable(
  ces,
  cols = Ordinary,
  by = opinion_leaders,
  total = "both",
  percent_pattern = "{p_row}% (n={n})",
  percent_digits = 1
)

ft_ordinary <- ct_ordinary %>%
  as_flextable(keep_id = TRUE) %>%
  autofit()

print(ft_ordinary)