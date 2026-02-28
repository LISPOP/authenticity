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

# or

library(flextable)
library(dplyr)

# Crosstab: news_consumption_cat × Truth
tab_truth <- table(ces$news_consumption_cat, ces$Truth, useNA = "ifany")
tab_truth_df <- as.data.frame.matrix(tab_truth)

# Add row totals
tab_truth_df <- tab_truth_df %>%
  mutate(Total = rowSums(.))

# Add row percentages
tab_truth_pct <- prop.table(tab_truth, margin = 1) * 100
tab_truth_df <- cbind(tab_truth_df, Percent_Disagree = round(tab_truth_pct[, "Disagree"],1),
                      Percent_Neutral = round(tab_truth_pct[, "Neutral"],1),
                      Percent_Agree = round(tab_truth_pct[, "Agree"],1))

# Create flextable
flextable(tab_truth_df) %>%
  set_header_labels(Disagree = "Disagree (count)",
                    Neutral = "Neutral (count)",
                    Agree = "Agree (count)",
                    Total = "Total") %>%
  add_header_row(values = c("Truth Support by News Consumption"), colwidths = ncol(tab_truth_df)) %>%
  autofit()

# Crosstab: news_consumption_cat × Ordinary
tab_ordinary <- table(ces$news_consumption_cat, ces$Ordinary, useNA = "ifany")
tab_ordinary_df <- as.data.frame.matrix(tab_ordinary)

# Add row totals
tab_ordinary_df <- tab_ordinary_df %>%
  mutate(Total = rowSums(.))

# Add row percentages
tab_ordinary_pct <- prop.table(tab_ordinary, margin = 1) * 100
tab_ordinary_df <- cbind(tab_ordinary_df, Percent_Disagree = round(tab_ordinary_pct[, "Disagree"],1),
                         Percent_Neutral = round(tab_ordinary_pct[, "Neutral"],1),
                         Percent_Agree = round(tab_ordinary_pct[, "Agree"],1))

# Create flextable
flextable(tab_ordinary_df) %>%
  set_header_labels(Disagree = "Disagree (count)",
                    Neutral = "Neutral (count)",
                    Agree = "Agree (count)",
                    Total = "Total") %>%
  add_header_row(values = c("Ordinary Support by News Consumption"), colwidths = ncol(tab_ordinary_df)) %>%
  autofit()