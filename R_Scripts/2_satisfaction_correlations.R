source("R_Scripts/1_data_import.R")

#Clean Variables
# Notes:
#   - kiss_module_lispop_2: 6 (DK/PNTA) -> NA
#   - cps25_lead_rating_23-25: -99 -> NA
#   - cps25_fed_gov_sat: recode 5 -> 2.5

# 1) Clean variables




# 3) Correlations
vars <- c(
  "truth_numeric",
  "carney_approval",
  "poilievre_approval",
  "singh_approval",
  "dissatisfaction_federal", "ideology"
)

cor_matrix <- cor(
  ces[, vars],
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
#     ces_clean$truth_numeric,
#     ces_clean[[v]],
#     use = "complete.obs"
#   )
#   
#   
#   ces25b(
#     outcome = v,
#     r = unname(test$estimate),
#     p_value = test$p.value,
#     n_complete_cases = sum(complete.cases(ces_clean$truth_numeric, ces_clean[[v]]))
#   )
# }) %>%
#   bind_rows()
# 
# print(results)
library(tidyverse)

vars_x <- c("carney_approval", "poilievre_approval", "singh_approval", "ideology", "dissatisfaction_federal")

ces_long <- ces %>%
  select(truth_numeric, all_of(vars_x)) %>%
  pivot_longer(
    cols = all_of(vars_x),
    names_to = "leader",
    values_to = "approval"
  )

ggplot(ces_long, aes(x = approval, y = truth_numeric)) +
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
