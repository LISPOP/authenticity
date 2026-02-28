#this line runs the data import and recode script
source("R_Scripts/1_data_import.R")

#H6 - Biggest concern about running for public office. Using Kiss_module_lispop_5(1) would predict more support for plain talking politicians (lispop_2).

#I have renamed kiss 5_1 to be speak_mind and included it in the select command. Please crosstab

#Crosstab speak_mind with lispop_2
ces %>%
  crosstable(speak_mind ~ Truth,
             percent_pattern = "{n} ({p_col}%)") %>%
  as_flextable()
