library(cesdata2)
data("ces25b")


# load libraries
library(cesdata2)
library(tidyverse)
library(haven)
library(here)
library(labelled)
#load the dataset called ces25b
data("ces25b")


ces25b %>% 
  select(kiss_module_Q1:kiss_module_Q9_3) %>% 
  var_label()

# second set, specifically about authenticity is 

ces25b %>% 
  select(kiss_module_Q10_1:kiss_module_lispop_5_6) %>% 
  var_label()


#Clean Variables
# Notes:
#   - kiss_module_lispop_2: 6 (DK/PNTA) -> NA
#   - cps25_lead_rating_23-25: -99 -> NA
#   - cps25_fed_gov_sat: recode 5 -> 2.5

library(dplyr)


# 1) Clean variables

ces_clean <- ces25b %>%
  mutate(
    
    # LISPOP variable
    kiss_module_lispop_2 = as.numeric(kiss_module_lispop_2),
    kiss_module_lispop_2 = na_if(kiss_module_lispop_2, 6),
    
    # Feeling thermometers
    cps25_lead_rating_23 = na_if(as.numeric(cps25_lead_rating_23), -99), # Carney
    cps25_lead_rating_24 = na_if(as.numeric(cps25_lead_rating_24), -99), # Poilievre
    cps25_lead_rating_25 = na_if(as.numeric(cps25_lead_rating_25), -99), # Singh
    
    # Trudeau government satisfaction
    cps25_fed_gov_sat = as.numeric(cps25_fed_gov_sat),
    cps25_fed_gov_sat = ifelse(cps25_fed_gov_sat == 5, 2.5, cps25_fed_gov_sat)
  )


# 2) Sanity checks
summary(ces_clean$kiss_module_lispop_2)
summary(ces_clean$cps25_lead_rating_23)

# summary(ces_clean$cps25_lead_rating_24)
# summary(ces_clean$cps25_lead_rating_25)
# summary(ces_clean$cps25_fed_gov_sat)


# 3) Correlations
vars <- c(
  "kiss_module_lispop_2",
  "cps25_lead_rating_23",
  "cps25_lead_rating_24",
  "cps25_lead_rating_25",
  "cps25_fed_gov_sat"
)

cor_matrix <- cor(
  ces_clean[, vars],
  use = "pairwise.complete.obs"
)

print(cor_matrix)

# 4) Individual correlations with p-values
outcomes <- vars[-1]

results <- lapply(outcomes, function(v) {
  
  test <- cor.test(
    ces_clean$kiss_module_lispop_2,
    ces_clean[[v]],
    use = "complete.obs"
  )
  
  ces25b(
    outcome = v,
    r = unname(test$estimate),
    p_value = test$p.value,
    n_complete_cases = sum(complete.cases(ces_clean$kiss_module_lispop_2, ces_clean[[v]]))
  )
}) %>%
  bind_rows()

print(results)
