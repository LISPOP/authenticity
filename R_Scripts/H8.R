# H8.R
source("R_Scripts/1_data_import.R")

library(dplyr)
library(crosstable)
library(flextable)

table(ces$partyid, useNA = "ifany")
table(ces$Truth, useNA = "ifany")
table(ces$Ordinary, useNA = "ifany")
ces$partyid
var_label(ces$partyid)<-c("Party ID")
#Crosstab 1: partyid x Truth

ct_truth_counts <- 
  crosstable(ces, as_factor(partyid)~Truth)  # counts
ct_truth_counts %>% as_flextable()

ct_truth_rowpct <- crosstable(
  ces, as_factor(partyid)~Truth,
  percent_pattern = "{p_row}% (n={n})",  # show row % with counts
  total = "both"
)
ct_truth_rowpct %>% as_flextable() %>% 
  save_as_docx(path=here("Tables/H8_1.docx"))

#Crosstab 2: partyid x Ordinary
ct_ordinary_counts <- crosstable(ces,as_factor(partyid)~Ordinary, total = "both")
ct_ordinary_counts %>% as_flextable()

ct_ordinary_rowpct <- crosstable(
  ces, as_factor(partyid)~Ordinary,
  percent_pattern = "{p_row}% (n={n})",
  total = "both"
)
ct_ordinary_rowpct %>% as_flextable() %>% 
  save_as_docx(path=here("Tables/H8_2.docx"))
