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
    authenticity_numeric = as.numeric(kiss_module_lispop_2),
    authenticity_numeric = na_if(authenticity_numeric, 6),
    
    # Feeling thermometers
    carney_approval = na_if(as.numeric(cps25_lead_rating_23), -99), # Carney
    poilievre_approval = na_if(as.numeric(cps25_lead_rating_24), -99), # Poilievre
    singh_approval = na_if(as.numeric(cps25_lead_rating_25), -99), # Singh
    
    # Trudeau government satisfaction
    dissatisfaction_federal = as.numeric(cps25_fed_gov_sat),
    dissatisfaction_federal = ifelse(cps25_fed_gov_sat == 5, 2.5, dissatisfaction_federal),
    #ideology
    ideology=ideology
  )

ces25b$cps25
# 2) Sanity checks
summary(ces_clean$authenticity_numeric)
summary(ces_clean$carney_approval)

# summary(ces_clean$poilievre_approval)
# summary(ces_clean$singh_approval)
# summary(ces_clean$dissatisfaction_federal)


# 3) Correlations
vars <- c(
  "authenticity_numeric",
  "carney_approval",
  "poilievre_approval",
  "singh_approval",
  "dissatisfaction_federal", "ideology"
)

cor_matrix <- cor(
  ces_clean[, vars],
  use = "pairwise.complete.obs"
)

print(cor_matrix)
# 
# # 4) Individual correlations with p-values
# outcomes <- vars[-1]
# 
# results <- lapply(outcomes, function(v) {
#   
#   test <- cor.test(
#     ces_clean$authenticity_numeric,
#     ces_clean[[v]],
#     use = "complete.obs"
#   )
#   
#   
#   ces25b(
#     outcome = v,
#     r = unname(test$estimate),
#     p_value = test$p.value,
#     n_complete_cases = sum(complete.cases(ces_clean$authenticity_numeric, ces_clean[[v]]))
#   )
# }) %>%
#   bind_rows()
# 
# print(results)
library(tidyverse)

vars_x <- c("carney_approval", "poilievre_approval", "singh_approval", "ideology", "dissatisfaction_federal")

ces_long <- ces_clean %>%
  select(authenticity_numeric, all_of(vars_x)) %>%
  pivot_longer(
    cols = all_of(vars_x),
    names_to = "leader",
    values_to = "approval"
  )

ggplot(ces_long, aes(x = approval, y = authenticity_numeric)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "firebrick") +
  facet_wrap(~ leader, scales = "free_x") +
  labs(
    title = "Authenticity vs Leader Approval",
    x = "Leader Approval",
    y = "Authenticity (numeric)"
  ) +
  theme_minimal(base_size = 12)
ggsave(here("Plots/scatterplots.png"), width=12, height=8,dpi=300)
