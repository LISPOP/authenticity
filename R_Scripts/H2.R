# H2.R
rm(list = ls())

source("R_Scripts/1_data_import.R")

library(dplyr)
library(ggplot2)

ces <- ces %>%
  mutate(
    q10_1 = na_if(as.numeric(kiss_module_Q10_1), 6),
    q10_2 = na_if(as.numeric(kiss_module_Q10_2), 6),
    q10_3 = na_if(as.numeric(kiss_module_Q10_3), 6),
    
    q10_composite = rowMeans(
      dplyr::select(., q10_1, q10_2, q10_3),
      na.rm = TRUE
    ),
    
    # If respondent misses all three items its converted to NA
    q10_composite = ifelse(is.nan(q10_composite), NA, q10_composite)
  )

#Correlations
cor_truth <- cor(ces$q10_composite, ces$truth_numeric, use = "pairwise.complete.obs")
cor_ordinary <- cor(ces$q10_composite, ces$ordinary_numeric, use = "pairwise.complete.obs")

cor_truth
cor_ordinary

#Scatterplots
# Composite vs truth_numeric
ggplot(ces, aes(x = q10_composite, y = truth_numeric)) +
  geom_point(na.rm = TRUE) +
  geom_smooth(method = "lm", se = FALSE, na.rm = TRUE) +
  labs(
    title = paste0("H2: Q10 Composite vs Truth (numeric) | r = ", round(cor_truth, 3)),
    x = "Q10 Composite (mean of Q10_1–Q10_3)",
    y = "Truth (truth_numeric)"
  )

# Composite vs ordinary_numeric
ggplot(ces, aes(x = q10_composite, y = ordinary_numeric)) +
  geom_point(na.rm = TRUE) +
  geom_smooth(method = "lm", se = FALSE, na.rm = TRUE) +
  labs(
    title = paste0("H2: Q10 Composite vs Ordinary (numeric) | r = ", round(cor_ordinary, 3)),
    x = "Q10 Composite (mean of Q10_1–Q10_3)",
    y = "Ordinary (ordinary_numeric)"
  )



#Political Efficacy Correlations
ces <- ces %>%
  mutate(
    political_efficacy = as.numeric(political_efficacy),
    truth_numeric = as.numeric(truth_numeric),
    ordinary_numeric = as.numeric(ordinary_numeric)
  )

#Correlations
cor_eff_truth <- cor(
  ces$political_efficacy,
  ces$truth_numeric,
  use = "pairwise.complete.obs"
)

cor_eff_ordinary <- cor(
  ces$political_efficacy,
  ces$ordinary_numeric,
  use = "pairwise.complete.obs"
)

cor_eff_truth
cor_eff_ordinary

#Scatterplot 1
ggplot(ces, aes(x = political_efficacy, y = truth_numeric)) +
  geom_point(na.rm = TRUE) +
  geom_smooth(method = "lm", se = FALSE, na.rm = TRUE) +
  labs(
    title = paste0("H2: Political Efficacy vs Truth | r = ",
                   round(cor_eff_truth, 3)),
    x = "Political Efficacy",
    y = "Truth (lispop_2 numeric)"
  )

#Scatterplot 2
ggplot(ces, aes(x = political_efficacy, y = ordinary_numeric)) +
  geom_point(na.rm = TRUE) +
  geom_smooth(method = "lm", se = FALSE, na.rm = TRUE) +
  labs(
    title = paste0("H2: Political Efficacy vs Ordinary | r = ",
                   round(cor_eff_ordinary, 3)),
    x = "Political Efficacy",
    y = "Ordinary (lispop_3 numeric)"
  )
