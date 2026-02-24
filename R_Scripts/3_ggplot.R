
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


  
#graph
library(ggplot2)

ggplot(analysis_data,
       aes(x = age_5cat, fill = truth)) +
  geom_bar(position = "fill") +
  facet_wrap(~ education_3cat) +
  theme_minimal()

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



# model 3
model3 <- glm(truth_binary ~ age_5cat * ideology_group,
                data = analysis_data,
                family = binomial)
summary(model3)

# Convert coefficients to odds ratios
exp(coef(model2))
exp(coef(model3))

#install.packages("ggeffects")
library(ggeffects)
library(ggplot2)

pred <- ggpredict(model3, terms = c("age_5cat", "ideology_group"))

ggplot(pred, aes(x = x, y = predicted, color = group)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    x = "Age Group",
    y = "Predicted Probability of Agreeing",
    color = "Ideology",
    title = "Predicted Agreement by Age and Ideology"
  ) +
  theme_minimal()



  
  # gender
  
  analysis_data <- ces %>%
  filter(
    !is.na(age_5cat),
    !is.na(ideology_group),
    !is.na(gender_2cat),
    !is.na(truth),
    truth != "DK/Refused"
  ) %>%
  mutate(truth_binary = ifelse(truth == "Agree", 1, 0)) %>%
  droplevels()


truth_binary ~ age_5cat + ideology_group + gender_2cat

model3 <- glm(truth_binary ~ age_5cat + ideology_group + gender_2cat,
              data = analysis_data,
              family = binomial)

summary(model3)
exp(coef(model3))

#graoh

library(ggplot2)
library(dplyr)

# Ensure gender is a factor
analysis_data$gender_2cat <- factor(analysis_data$gender_2cat)

# Compute mean predicted probability by age, gender, ideology
summary_data <- analysis_data %>%
  group_by(age_5cat, gender_2cat, ideology_group) %>%
  summarize(
    mean_prob = mean(pred_prob),
    sd_prob = sd(pred_prob),
    n = n(),
    se = sd_prob / sqrt(n)
  ) %>%
  ungroup()

# Plot with color
ggplot(summary_data, aes(x = age_5cat, y = mean_prob, fill = gender_2cat)) +
  geom_col(position = position_dodge(width = 0.7)) +
  geom_errorbar(aes(ymin = mean_prob - se, ymax = mean_prob + se),
                width = 0.2,
                position = position_dodge(width = 0.7)) +
  facet_wrap(~ ideology_group) +
  labs(
    title = "Predicted Probability of Agree by Age, Gender, and Ideology",
    x = "Age Category",
    y = "Predicted Probability of Agree",
    fill = "Gender"
  ) +
  theme_minimal()
