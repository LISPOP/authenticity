# H7.R
source("R_Scripts/1_data_import.R")

library(dplyr)
library(crosstable)
library(flextable)

#Overwriting the H7 recode in this script


table(ces$opinion_leaders, useNA = "ifany")
table(ces$Truth, useNA = "ifany")
table(ces$Ordinary, useNA = "ifany")

#Crosstab 1: opinion_leaders x Truth (lispop_2 recode)
ct_truth <- crosstable(
  ces,
opinion_leaders~Truth,
  percent_pattern = "{p_row}% (n={n})",
total="row",
  percent_digits = 1, test=T
)

ft_truth <- ct_truth %>%
  as_flextable()

ft_truth %>% 
  save_as_docx(., path=here("Tables/H7_1.docx"))

#Crosstab 2: opinion_leaders x Ordinary (lispop_3 recode)
ct_ordinary <- crosstable(
  ces,
opinion_leaders~Ordinary,
  total = "row",
  percent_pattern = "{p_row}% (n={n})",
  percent_digits = 1
)


ft_ordinary <- ct_ordinary %>%
  as_flextable() %>%
  autofit()%>% 
  save_as_docx(., path=here("Tables/H7_2.docx"))

print(ft_ordinary)
