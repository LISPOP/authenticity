

source("R_Scripts/1_data_import.R")

# Tests whether education attenuates the relationship between age and lispop 2 question

# Create Clean Analysis Dataset
  analysis_data <- ces %>%
  filter(
    !is.na(age_5cat),
    !is.na(education_3cat),
    !is.na(truth),
    truth != "DK/Refused",
    education_3cat != "DK/Refused"
  ) %>%
  droplevels()

# Create Binary Outcome Variable
analysis_data <- analysis_data %>%
  mutate(truth_binary = ifelse(truth == "Agree", 1, 0))

# Model 1 — Effect of Age Only
model1 <- glm(truth_binary ~ age_5cat,
              data = analysis_data,
              family = binomial)

summary(model1)

# Model 2 — Age + Education
model2 <- glm(truth_binary ~ age_5cat + education_3cat,
              data = analysis_data,
              family = binomial)

summary(model2)

# Convert Coefficients to Odds Ratios
exp(coef(model1))
exp(coef(model2))

---------------
  
#graph
library(ggplot2)

ggplot(analysis_data,
       aes(x = age_5cat, fill = truth)) +
  geom_bar(position = "fill") +
  facet_wrap(~ education_3cat) +
  theme_minimal()

---------------
  
#ideology
  
# clean data
  
  analysis_data <- ces %>%
  filter(
    !is.na(age_5cat),
    !is.na(ideology_group),
    !is.na(truth),
    truth != "DK/Refused"
  ) %>%
  droplevels()


analysis_data <- analysis_data %>%
  mutate(truth_binary = ifelse(truth == "Agree", 1, 0))

# Model 1 — Effect of Age Only
model1 <- glm(truth_binary ~ age_5cat,
              data = analysis_data,
              family = binomial)

summary(model1)

# Model 2 — Age + Ideology
model2 <- glm(truth_binary ~ age_5cat + ideology_group,
              data = analysis_data,
              family = binomial)

summary(model2)

# Convert coefficients to odds ratios
exp(coef(model1))
exp(coef(model2))

# graph

library(ggplot2)

ggplot(analysis_data,
       aes(x = age_5cat, fill = truth)) +
  geom_bar(position = "fill") +
  facet_wrap(~ ideology_group) +
  theme_minimal() +
  labs(
    x = "Age Group",
    y = "Proportion Agree",
    fill = "Response"
  )






