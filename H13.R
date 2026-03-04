#this line runs the data import and recode script
source("R_Scripts/1_data_import.R")

#might have to recode to reduce categories; 
#I'II leave it up to. you rename the variable news_consumption
#Crosstab the variable cps25_news_consume with truth and ordinary

# H13 Crosstabs: News Consumption × Truth and Ordinary

# Crosstab: news_consumption_cat × Truth
crosstable(ces, 
           news_consumption_cat~Truth, 
           percent_pattern="{p_row} (n={n})") %>% 
  as_flextable() %>% 
  save_as_docx(., path=here("Tables/h13_1.docx"))
crosstable(ces, 
           news_consumption_cat~Ordinary, 
           percent_pattern="{p_row} (n={n})") %>% 
  as_flextable() %>% 
  save_as_docx(., path=here("Tables/h13_2.docx"))
