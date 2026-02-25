
#install devtools
#install.packages('devtools')
#install cesdata2
#devtools::install_github("sjkiss/cesdata2")
#Check that it worked
library(cesdata2)
data("ces25b")

# install packages if necessary
# First install packages
#install.packages("dplyr")
#install.packages("labelled")
#install.packages("crosstable")
#install.packages("flextable")
#devtools::install_github("sjkiss/cesdata2")


# load libraries
library(cesdata2)
library(tidyverse)
library(haven)
library(here)
library(labelled)
library(flextable)
library(crosstable)  # Add this line
#load the dataset called ces25b
data("ces25b")

# the module variables are in two batches
# The first is a set of questions on housing and social class.
# We may not use these.

ces25b %>% 
  select(kiss_module_Q1:kiss_module_Q9_3) %>% 
  var_label()

# second set, specifically about authenticity is 

ces25b %>% 
  select(kiss_module_Q10_1:kiss_module_lispop_5_6) %>% 
  var_label()
#### Select variables ####
# add any variables we deal with in here. 
ces25b %>%
  select(contains("kiss")&-contains("DO"), 
         cps25_genderid, cps25_education, cps25_rel_imp, ideology, age, contains("lead_rating"), contains("fed_gov_sat"))->ces 


ces<- ces%>%
  mutate(
    # LISPOP variable
    truth_numeric = as.numeric(kiss_module_lispop_2),
    truth_numeric = na_if(truth_numeric, 6),
    
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



# summary(ces_clean$poilievre_approval)
# summary(ces_clean$singh_approval)
# summary(ces_clean$dissatisfaction_federal)
# Now we can use just the dataset ces
#### Recodes ####
# Create recoded variables with exact coding from codebook
# Let's make one chain per recode, it will be easier to track

# Gender 
# This works fine. 
ces %>% 
  mutate(
    # Recode gender into 2 categories (Male/Female only)
    gender_2cat = case_when(
      cps25_genderid == 1 ~ "Male",
      cps25_genderid == 2 ~ "Female",
      cps25_genderid %in% c(3, 4) ~ NA_character_,
      TRUE ~ NA_character_
    ),
    gender_2cat = factor(gender_2cat, levels = c("Male", "Female")))->ces

# This works fine , except let's pick a more meaningful variable name
# What about truth?

ces %>% 
  mutate(
    # Recode lispop variable
    truth = case_when(
      kiss_module_lispop_2 %in% c(4, 5) ~ "Agree",
      kiss_module_lispop_2 == 3 ~ "Neutral",
      kiss_module_lispop_2 %in% c(1, 2) ~ "Disagree",
      kiss_module_lispop_2 == 6 ~ "DK/Refused",
      TRUE ~ NA_character_
    ),
    truth = factor(truth, 
                         levels = c("Disagree", "Neutral", "Agree", "DK/Refused")))->ces
    
# Education
# this works great 
ces %>% 
  mutate(
    # Create simplified education categories
    education_3cat = case_when(
      cps25_education %in% 1:5 ~ "High school or less",
      cps25_education %in% 6:7 ~ "Some post-secondary",
      cps25_education %in% 8:11 ~ "University degree",
      cps25_education == 12 ~ "DK/Refused",
      TRUE ~ NA_character_
    ),
    education_3cat = factor(education_3cat,
                            levels = c("High school or less", 
                                       "Some post-secondary", 
                                       "University degree",
                                       "DK/Refused")))->ces

# Religiosity
#this works
ces %>% 
  mutate(
    # Recode religiosity (cps25_rel_imp from codebook)
    # 1 = Very important, 2 = Somewhat important, 3 = Not very important,
    # 4 = Not important at all, 5 = Don't know/Prefer not to answer
    religiosity = case_when(
      cps25_rel_imp == 1 ~ "Very important",
      cps25_rel_imp == 2 ~ "Somewhat important",
      cps25_rel_imp == 3 ~ "Not very important",
      cps25_rel_imp == 4 ~ "Not important at all",
      cps25_rel_imp == 5 ~ "DK/Refused",
      TRUE ~ NA_character_
    ),
    religiosity = factor(religiosity,
                         levels = c("Very important", "Somewhat important",
                                    "Not very important", "Not important at all",
                                    "DK/Refused")))->ces

table(ces25b$cps25_rel_imp, useNA = "ifany")
#Age 

ces %>% 
  mutate(
    age_5cat = case_when(
      age >= 18 & age <= 34 ~ "18-34",
      age >= 35 & age <= 44 ~ "35-44",
      age >= 45 & age <= 54 ~ "45-54",
      age >= 55 & age <= 64 ~ "55-64",
      age >= 65 ~ "65+",
      TRUE ~ NA_character_
    ),
    age_5cat = factor(age_5cat,
                      levels = c("18-34","35-44","45-54","55-64","65+"))
  ) -> ces


#ideology

ces <- ces %>% 
  mutate(
    ideology_group = case_when(
      ideology %in% 0:0.3 ~ "Left",
      ideology %in% 0.4:0.6 ~ "Centre",
      ideology %in% 0.7:1 ~ "Right",
      TRUE ~ NA_character_
    ),
    ideology_group = factor(ideology_group,
                            levels = c("Left", "Centre", "Right"))
  )
