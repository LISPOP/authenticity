#H9.R
source("R_Scripts/1_data_import.R")

library(dplyr)
library(crosstable)
library(flextable)


table(ces$likely_vote, useNA = "ifany")
table(ces$Truth, useNA = "ifany")
table(ces$Ordinary, useNA = "ifany")

#Crosstab 1: likely_vote x Truth
ct_truth <- crosstable(
  subset(ces, !is.na(likely_vote)),
likely_vote~Truth,
  total = "row",
  percent_pattern = "{p_row}% (n={n})",
  percent_digits = 1
)
ct_truth
ft_truth <- ct_truth %>%
  as_flextable() %>%
  autofit()%>% 
  save_as_docx(., path=here("Tables/H9_1.docx"))

ft_truth

#Crosstab 2: likely_vote x Ordinary
ct_ordinary <- crosstable(
  subset(ces, !is.na(likely_vote)),
  likely_vote~Ordinary,
  total = "row",
  percent_pattern = "{p_row}% (n={n})",
  percent_digits = 1
)

ft_ordinary <- ct_ordinary %>%
  as_flextable() %>%
  autofit() 

ft_ordinary %>% 
  save_as_docx(., path=here("Tables/H9_2.docx"))
