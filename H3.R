#this line runs the data import and recode script
source("R_Scripts/1_data_import.R")

#H3 - Class conflict and authenticity. Usking Kiss module Q4 (perceiving class conflict) and stronger class closeness (Q3) should increase demand for plain talking politicians (lispop_2).
#Hi Jenny: I have already recoded these variables in ces25b. Q4 is called "class_conflict" and Q3 is called "class_closeness".
#To use these effectively in a crosstab you will need to wrap each in as_factor(). I have gone and added those variables to the select() command in 1_data_import.


#class conflict x ordinary (lispop_3 categorical)
ces %>%
  crosstable(as_factor(class_conflict) ~ Ordinary,
             percent_pattern = "{n} ({p_col}%)") %>%
  as_flextable()

#or
ces %>%
  crosstable(as_factor(class_conflict) ~ Ordinary,
             percent_pattern = "{n} ({p_col}%)",
             test = TRUE) %>%
  as_flextable()

#class closeness x truth (lispop_2 categorical)
ces %>%
  crosstable(as_factor(class_close) ~ Truth,
             percent_pattern = "{n} ({p_col}%)") %>%
  as_flextable()
#or 
ces %>%
  crosstable(as_factor(class_close) ~ Truth,
             percent_pattern = "{n} ({p_col}%)",
             test = TRUE) %>%
  as_flextable()