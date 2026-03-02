# H8.R
rm(list = ls())

source("R_Scripts/1_data_import.R")

library(dplyr)
library(crosstable)
library(flextable)

table(ces$partyid, useNA = "ifany")
table(ces$Truth, useNA = "ifany")
table(ces$Ordinary, useNA = "ifany")

#Crosstab 1: partyid x Truth
ct_truth_counts <- crosstable(ces, partyid, Truth, total = "both")  # counts
ct_truth_counts %>% as_flextable()

ct_truth_rowpct <- crosstable(
  ces, partyid, Truth,
  percent_pattern = "{row}% ({n})",  # show row % with counts
  total = "both"
)
ct_truth_rowpct %>% as_flextable()

#Crosstab 2: partyid x Ordinary
ct_ordinary_counts <- crosstable(ces, partyid, Ordinary, total = "both")
ct_ordinary_counts %>% as_flextable()

ct_ordinary_rowpct <- crosstable(
  ces, partyid, Ordinary,
  percent_pattern = "{row}% ({n})",
  total = "both"
)
ct_ordinary_rowpct %>% as_flextable()