
source("R_Scripts/1_data_import.R")

# Tests whether education attenuates the relationship between age and lispop 2 question

# Create Clean Analysis Dataset
analysis_data <- ces %>%
  mutate(degree = as.factor(degree)) %>%
  filter(
    !is.na(age_5cat),
    !is.na(degree),
    !is.na(Truth),
    Truth != "DK/Refused",
    degree != "DK/Refused"
  ) %>%
  droplevels()

# Create Binary Outcome Variable
analysis_data <- analysis_data %>%
  mutate(Truth_binary = ifelse(Truth == "Agree", 1, 0))

# Model 1 — Effect of Age Only
model1 <- glm(Truth_binary ~ age_5cat,
              data = analysis_data,
              family = binomial)

summary(model1)

# Model 2 — Age + Education
model2 <- glm(Truth_binary ~ age_5cat + education_3cat,
              data = analysis_data,
              family = binomial)

summary(model2)

# Convert Coefficients to Odds Ratios
exp(coef(model1))
exp(coef(model2))

library(ggplot2)

ggplot(analysis_data,
       aes(x = age_5cat, fill = Truth)) +
  geom_bar(position = "fill") +
  facet_wrap(~ education_3cat) +
  theme_minimal()


#ideology
  
# clean data
  
  analysis_data <- ces %>%
  filter(
    !is.na(age_5cat),
    !is.na(ideology_group),
    !is.na(Truth),
    Truth != "DK/Refused"
  ) %>%
  droplevels()


analysis_data <- analysis_data %>%
  mutate(Truth_binary = ifelse(Truth == "Agree", 1, 0))

# Model 1 — Effect of Age Only
model1 <- glm(Truth_binary ~ age_5cat,
              data = analysis_data,
              family = binomial)

summary(model1)

# Model 2 — Age + Ideology
model2 <- glm(Truth_binary ~ age_5cat + ideology_group,
              data = analysis_data,
              family = binomial)

summary(model2)

# Convert coefficients to odds ratios
exp(coef(model1))
exp(coef(model2))

# graph

library(ggplot2)

ggplot(analysis_data,
       aes(x = age_5cat, fill = Truth)) +
  geom_bar(position = "fill") +
  facet_wrap(~ ideology_group) +
  theme_minimal() +
  labs(
    x = "Age Group",
    y = "Proportion Agree",
    fill = "Response"
  )



# model 3
model3 <- glm(Truth_binary ~ age_5cat * ideology_group,
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
    !is.na(Truth),
    Truth != "DK/Refused"
  ) %>%
  mutate(Truth_binary = ifelse(Truth == "Agree", 1, 0)) %>%
  droplevels()


Truth_binary ~ age_5cat + ideology_group + gender_2cat

model3 <- glm(Truth_binary ~ age_5cat + ideology_group + gender_2cat,
              data = analysis_data,
              family = binomial)

summary(model3)
exp(coef(model3))

#graph

library(ggplot2)
library(dplyr)

# Ensure gender is a factor
analysis_data$gender_2cat <- factor(analysis_data$gender_2cat)
# create pred_prob
analysis_data$pred_prob <- predict(model3, type = "response")

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

#Ideology recode
analysis_data <- ces %>%
  mutate(
    ideology_num = case_when(
      ideology == "Unsure" ~ 5,
      TRUE ~ as.numeric(as.character(ideology))
    ),
    age_num = as.numeric(age),   # assuming raw age variable is `age`
    Truth_binary = ifelse(Truth == "Agree", 1, 0)
  ) %>%
  filter(
    !is.na(age_num),
    !is.na(ideology_num),
    !is.na(Truth_binary)
  )

#Model age and ideology
model_age_ideo_int <- glm(
  Truth_binary ~ age_num * ideology_num,
  data = analysis_data,
  family = binomial
)

summary(model_age_ideo_int)
exp(coef(model_age_ideo_int))  # odds ratios

#Mean Centre
analysis_data <- analysis_data %>%
  mutate(
    age_c = scale(age_num, center = TRUE, scale = FALSE),
    ideology_c = scale(ideology_num, center = TRUE, scale = FALSE)
  )

model_age_ideo_int <- glm(
  Truth_binary ~ age_c * ideology_c,
  data = analysis_data,
  family = binomial
)

summary(model_age_ideo_int)
exp(coef(model_age_ideo_int))

#Education clean 
analysis_data <- ces %>%
  mutate(
    age_num = as.numeric(age),
    Truth_binary = ifelse(Truth == "Agree", 1, 0)
  ) %>%
  filter(
    !is.na(age_num),
    !is.na(degree),
    !is.na(Truth_binary)
  ) %>%
  droplevels()
#Model Age and Education
model_age_edu_int <- glm(
  Truth_binary ~ age_num * degree,
  data = analysis_data,
  family = binomial
)

summary(model_age_edu_int)
exp(coef(model_age_edu_int))

# Education plot

library(ggeffects)
library(ggplot2)

pred_edu <- ggpredict(model_age_edu_int,
                      terms = c("age_num [all]", "degree"))

ggplot(pred_edu,
       aes(x = x, y = predicted, color = group)) +
  geom_line(size = 1.2) +
  labs(
    x = "Age",
    y = "Predicted Probability of Agree",
    color = "Education"
  ) +
  theme_minimal()


# attempt at marginal effects

install.packages("marginaleffects")  # if needed
library(marginaleffects)
library(ggplot2)

model_age_edu_int <- glm(
  Truth_binary ~ age_num * degree,
  data = analysis_data,
  family = binomial
)

pred_edu <- predictions(
  model_age_edu_int,
  newdata = datagrid(
    age_num = seq(min(analysis_data$age_num),
                  max(analysis_data$age_num),
                  by = 1),
    degree = unique(analysis_data$degree)
  )
)

ggplot(pred_edu,
       aes(x = age_num,
           y = estimate,
           color = degree)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low,
                  ymax = conf.high,
                  fill = degree),
              alpha = 0.2,
              color = NA) +
  labs(
    x = "Age",
    y = "Predicted Probability of Agree",
    color = "Education",
    fill = "Education"
  ) +
  theme_minimal()

slopes_edu <- slopes(
  model_age_edu_int,
  variables = "age_num",
  newdata = datagrid(
    degree = unique(analysis_data$degree)
  )
)

slopes_edu

