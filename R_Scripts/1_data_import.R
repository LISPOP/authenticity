##### Please run these commands first!!!!!!
# You have to reinstall cesdata2
#remove.packages("cesdata2")
#install devtools
#install.packages('devtools')
#install cesdata2
#devtools::install_github("sjkiss/cesdata2")
#Check that it worked
library(cesdata2)
data("ces25b")

# install packages if necessary
# First install packages
# install.packages("dplyr")
# install.packages("labelled")
# install.packages("crosstable")
# install.packages("flextable")
# devtools::install_github("sjkiss/cesdata2")


# load libraries
library(cesdata2)
library(tidyverse)
library(haven)
library(here)
library(labelled)
library(flextable)
library(crosstable)  # Add this line


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
  rowwise() %>%
  mutate(leader_average=mean(c_across(liberal_leader:ppc_leader), na.rm=T)) %>%
  ungroup()->ces25b
ces <- ces25b %>%
  select(contains("kiss")&-contains("DO"), 
         cps25_genderid, cps25_education, cps25_news_cons, degree, cps25_rel_imp, ideology, age, contains("lead_rating"), contains("fed_gov_sat"), class_conflict, class_close, party_average, leader_average, speak_mind, partyid, political_efficacy, class_close, kiss_module_lispop_1, cps25_v_likely)

ces<- ces%>%
  mutate(
    # LISPOP variable
    climate_numeric = (kiss_module_lispop_1),
    climate_numeric = as.numeric(climate_numeric, 6),
    truth_numeric = as.numeric(kiss_module_lispop_2),
    truth_numeric = na_if(truth_numeric, 6),
    ordinary_numeric = as.numeric(kiss_module_lispop_3),
    ordinary_numeric = na_if(ordinary_numeric, 6),
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



ces %>% 
  mutate(
    # Recode lispop variable
    Ordinary = case_when(
      kiss_module_lispop_1 %in% c(4, 5) ~ "Agree",
      kiss_module_lispop_1 == 3 ~ "Neutral",
      kiss_module_lispop_1 %in% c(1, 2) ~ "Disagree",
      kiss_module_lispop_1 == 6 ~ "Neutral",
      TRUE ~ NA_character_
    ),
    Ordinary = factor(Ordinary, 
                      levels = c("Disagree", "Neutral", "Agree")))->ces

# This works fine , except let's pick a more meaningful variable name
# What about truth?

ces %>% 
  mutate(
    # Recode lispop variable
    Truth = case_when(
      kiss_module_lispop_2 %in% c(4, 5) ~ "Agree",
      kiss_module_lispop_2 == 3 ~ "Neutral",
      kiss_module_lispop_2 %in% c(1, 2) ~ "Disagree",
      kiss_module_lispop_2 == 6 ~ "Neutral",
      TRUE ~ NA_character_
    ),
    Truth = factor(Truth, 
                   levels = c("Disagree", "Neutral", "Agree")))->ces

ces %>% 
  mutate(
    # Recode lispop variable
    Civility = case_when(
      kiss_module_lispop_1 %in% c(4, 5) ~ "Agree",
      kiss_module_lispop_1 == 3 ~ "Neutral",
      kiss_module_lispop_1 %in% c(1, 2) ~ "Disagree",
      kiss_module_lispop_1 == 6 ~ "Neutral",
      TRUE ~ NA_character_
    ),
    Civility = factor(Civility, 
                   levels = c("Disagree", "Neutral", "Agree")))->ces
ces %>% 
  mutate(
    # Recode lispop variable
    Ordinary = case_when(
      kiss_module_lispop_3 %in% c(4, 5) ~ "Agree",
      kiss_module_lispop_3 == 3 ~ "Neutral",
      kiss_module_lispop_3 %in% c(1, 2) ~ "Disagree",
      kiss_module_lispop_3 == 6 ~ "Neutral",
      TRUE ~ NA_character_
    ),
    Ordinary = factor(Ordinary, 
                      levels = c("Disagree", "Neutral", "Agree")))->ces

# Education
# this works great 
ces %>% 
  mutate(
    # Create simplified education categories
    education_3cat = case_when(
      cps25_education %in% 1:5 ~ "High school or less",
      cps25_education %in% 6:7 ~ "Some post-secondary",
      cps25_education %in% 8:11 ~ "University degree",
      cps25_education == 12 ~ NA_character_,
      TRUE ~ NA_character_
    ),
    education_3cat = factor(education_3cat,
                            levels = c("High school or less", 
                                       "Some post-secondary", 
                                       "University degree")))->ces


# Religiosity
#this works
ces %>% 
  mutate(
    # Recode religiosity (cps25_rel_imp from codebook)
    # 1 = Very important, 2 = Somewhat important, 3 = Not very important,
    # 4 = Not important at all, 5 = Don't know/Prefer not to answer
    religiosity = case_when(
      cps25_rel_imp %in% c(1, 2) ~ "Very religious",              # 1 = Very important, 2 = Somewhat important
      cps25_rel_imp %in% c(3, 4) ~ "Not very religious",          # 3 = Not very important, 4 = Not important at all
      is.na(cps25_rel_imp) ~ "Atheist / no religion"  
    ),
    religiosity = factor(
      religiosity,
      levels = c("Very religious", "Not very religious", "Atheist / no religion")))->ces

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

#news consumption
ces <- ces %>%
  rename(news_consumption = cps25_news_cons) %>%
  mutate(
    news_consumption_cat = case_when(
      news_consumption %in% 1:2 ~ "Low",
      news_consumption %in% 3:5 ~ "Medium",
      news_consumption %in% 6:7 ~ "High",
      TRUE ~ NA_character_
    ),
    news_consumption_cat = factor(news_consumption_cat, levels = c("Low", "Medium", "High"))
  )

#### Favourable Leaders
#Assumes: 1=Strongly disagree, 2=Somewhat disagree, 3=Neutral, 4=Somewhat agree, 5=Strongly agree, 6=DK/Refused
ces <- ces %>%
  mutate(
    opinion_leaders = case_when(
      kiss_module_lispop_4 %in% c(3,4) ~ "Unfavourable",
      #kiss_module_lispop_4 == 3 ~ "Neutral",
      kiss_module_lispop_4 %in% c(1, 2) ~ "Favourable",
    #  kiss_module_lispop_4 == 5 ~ "Neutral",
      TRUE ~ NA_character_
    ),
    opinion_leaders = factor(opinion_leaders, levels = c("Unfavourable", "Favourable"))
  )

# Convert speak_mind to categorical
ces %>% 
  mutate(speak_mind=case_when(
    speak_mind==1~"Speak Mind",
    TRUE~"Other"
  ))->ces
levels(ces$speak_mind)<-c("Speak Mind" , "Other")

#####Recode voting likelihood
#Recode: 1:2 and 6 => "Likely / certain"
#3:4 and 7 => "Unlikely / certain not"
#Everything else => NA
ces <- ces %>%
  mutate(
    likely_vote = case_when(
      cps25_v_likely %in% c(1, 2, 6) ~ "Likely / certain",
      cps25_v_likely %in% c(3, 4, 7) ~ "Unlikely / certain not",
      TRUE ~ NA_character_
    ),
    likely_vote = factor(likely_vote, levels = c("Likely / certain", "Unlikely / certain not"))
  )

# Conservative Party ID
# Set theme for all graphs
theme_set(theme_minimal(base_size=24))
