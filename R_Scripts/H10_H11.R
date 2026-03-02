#H10_H11.R

rm(list = ls())

source("R_Scripts/1_data_import.R")

library(dplyr)
library(ggplot2)

#Variables are numeric
ces <- ces %>%
  mutate(
    party_average = as.numeric(party_average),
    leader_average = as.numeric(leader_average),
    truth_numeric = as.numeric(truth_numeric),
    ordinary_numeric = as.numeric(ordinary_numeric)
  )

#H10
# Party dislike (party_average) -> support for lispop_2 (truth_numeric)

cor_party_truth <- cor(
  ces$party_average,
  ces$truth_numeric,
  use = "pairwise.complete.obs"
)

cor_party_truth

# Scatterplot
ggplot(ces, aes(x = party_average, y = truth_numeric)) +
  geom_point(na.rm = TRUE) +
  geom_smooth(method = "lm", se = FALSE, na.rm = TRUE) +
  labs(
    title = paste0("H10: Party Average vs Truth | r = ", round(cor_party_truth, 3)),
    x = "Party Thermometer Average",
    y = "Truth (lispop_2 numeric)"
  )

#H11
# Leader dislike (leader_average) -> support for lispop_3 (ordinary_numeric)

cor_leader_ordinary <- cor(
  ces$leader_average,
  ces$ordinary_numeric,
  use = "pairwise.complete.obs"
)

cor_leader_ordinary

# Scatterplot
ggplot(ces, aes(x = leader_average, y = ordinary_numeric)) +
  geom_point(na.rm = TRUE) +
  geom_smooth(method = "lm", se = FALSE, na.rm = TRUE) +
  labs(
    title = paste0("H11: Leader Average vs Ordinary | r = ", round(cor_leader_ordinary, 3)),
    x = "Leader Thermometer Average",
    y = "Ordinary (lispop_3 numeric)"
  )