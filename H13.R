#this line runs the data import and recode script
source("R_Scripts/1_data_import.R")

#might have to recode to reduce categories; 
#I'II leave it up to. you rename the variable news_consumption
#Crosstab the variable cps25_news_consume with truth and ordinary

# H13 Crosstabs: News Consumption × Truth and Ordinary

# Crosstab: news_consumption_cat × Truth
tab_truth <- table(ces$news_consumption_cat, ces$Truth, useNA = "ifany")
tab_truth

# Row percentages
prop.table(tab_truth, margin = 1)

# Chi-square test
chisq.test(tab_truth)

# Crosstab: news_consumption_cat × Ordinary
tab_ordinary <- table(ces$news_consumption_cat, ces$Ordinary, useNA = "ifany")
tab_ordinary

# Row percentages
prop.table(tab_ordinary, margin = 1)

# Chi-square test
chisq.test(tab_ordinary)

