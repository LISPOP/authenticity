#H9.R

rm(list = ls())

source("R_Scripts/1_data_import.R")

library(dplyr)
library(crosstable)
library(flextable)

#Recode voting likelihood
#Recode: 1:2 and 6 => "Likely / certain"
#3:4 and 7 => "Unlikely / certain not"
#Everything else => NA
ces <- ces %>%
  mutate(
    likely_vote = case_when(
      ces25b_v_likely %in% c(1, 2, 6) ~ "Likely / certain",
      ces25b_v_likely %in% c(3, 4, 7) ~ "Unlikely / certain not",
      TRUE ~ NA_character_
    ),
    likely_vote = factor(likely_vote, levels = c("Likely / certain", "Unlikely / certain not"))
  )

table(ces$likely_vote, useNA = "ifany")
table(ces$Truth, useNA = "ifany")
table(ces$Ordinary, useNA = "ifany")

#Crosstab 1: likely_vote x Truth
ct_truth <- crosstable(
  ces,
  cols = Truth,
  by = likely_vote,
  total = "both",
  percent_pattern = "{p_row}% (n={n})",
  percent_digits = 1
)

ft_truth <- ct_truth %>%
  as_flextable(keep_id = TRUE) %>%
  autofit()

ft_truth

#Crosstab 2: likely_vote x Ordinary
ct_ordinary <- crosstable(
  ces,
  cols = Ordinary,
  by = likely_vote,
  total = "both",
  percent_pattern = "{p_row}% (n={n})",
  percent_digits = 1
)

ft_ordinary <- ct_ordinary %>%
  as_flextable(keep_id = TRUE) %>%
  autofit()

ft_ordinary