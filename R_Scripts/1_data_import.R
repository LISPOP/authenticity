# install packages if necessary
devtools::install_github("sjkiss/cesdata2")
#devtools::install_github("sjikss/cesdata2") - don't use, won't run

# load libraries
library(cesdata2)
library(tidyverse)
library(haven)
library(here)
library(labelled)
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

----------
# Lispop 2 CROSS TABLE (gender and variable re-label)
  
library(dplyr)
library(labelled)
library(crosstable)

ces <- ces25b %>% 
  select(kiss_module_lispop_2, cps25_genderid) %>% 
  mutate(
    kiss_module_lispop_2 = as_factor(kiss_module_lispop_2, levels = "labels"),
    cps25_genderid = as_factor(cps25_genderid, levels = "labels")
  )

ct <- crosstable(
  ces,
  cols = kiss_module_lispop_2,
  by   = cps25_genderid,
  total = "both",
  percent_pattern = "{n} ({p_row}/{p_col})",  # total counts + row%/col%
  percent_digits = 1                           # no decimals or 1 decimal
)

ct

# cross table looks like from https://cran.r-project.org/web/packages/crosstable/vignettes/crosstable.html
ct %>% as_flextable(keep_id = TRUE)

# Cross table (variabel re-label)

library(dplyr)
library(labelled)
library(crosstable)

ces <- ces25b %>%
  select(kiss_module_lispop_2, cps25_genderid) %>%
  mutate(
    # Convert CES variable to factor
    kiss_module_lispop_2 = as_factor(kiss_module_lispop_2, levels = "labels"),
    
    # Recode into 3 categories
    lispop_3cat = case_when(
      kiss_module_lispop_2 %in% c(1, 2) ~ "Agree",
      kiss_module_lispop_2 %in% c(3, 6) ~ "Neutral",
      TRUE ~ NA_character_
    ),
    lispop_3cat = factor(lispop_3cat, levels = c("Agree", "Neutral")),
    
    # Convert gender to factor
    cps25_genderid = as_factor(cps25_genderid, levels = "labels")
  )
ct <- crosstable(
  ces,
  cols = lispop_3cat,
  by   = cps25_genderid,
  total = "both"  # shows row + column totals
)

ct
ct %>% as_flextable(keep_id = TRUE)




# from meeting, as reference*
ces25b %>% 
  select(kiss_module_lispop_2) %>% 
  as_factor ()
ces25b
names(ces25b)
ces25b$kiss_module_lispop_2
table(ces25b$kiss_module_lispop_2)
library(labelled)


------------------------------------

ces25b %>% 
  select(kiss_module_lispop_3) %>% 
  as_factor ()

ces25b
names(ces25b)
ces25b$kiss_module_lispop_3
table(ces25b$kiss_module_lispop_3)
library(labelled)


----------

ces25b %>% 
  select(cps25_v_likely) %>% 
  as_factor ()

  ces25b
  names(ces25b)
  ces25b$kiss_module_lispop_3
  table(ces25b$cps25_v_likely)
  library(labelled)
  
  
ces25b %>% 
  select(cps25_v_votechoice) %>% 
  as_factor ()

ct

ces25b %>% 
  select(cps25_genderid) %>% 
  as_factor ()

ces25b %>% 
  select(cps25_demsat) %>% 
  as_factor ()

ces25b %>% 
  select(cps25_v_advance) %>% 
  as_factor ()

ct

ces25b %>% 
  select(cps25_fed_gov_sat_2) %>% 
  as_factor ()


ces25b %>% 
  select(cps25_lead_rating) %>% 
  as_factor ()

ces25b %>% 
  select(cps25_own_fin_retro) %>% 
  as_factor ()
----------------------------------------------------------

# create a subset of questions that we are going to work with.
ces25b
names(ces25b)
ces25b$kiss_module_lispop_2
table(ces25b$kiss_module_lispop_2)
library(labelled)
ces25b %>% 
  select(kiss_module_lispop_2) ->test

#first convert lispop_2 to a factor
#then use car::Recode to recode factor levels
# or keep lispop2 as a labelled variable
# and use mutate(cae_when().) to create a new variable if lispop2 = 1 or 2, then "Agree", if 3 or 6 then "neutral" 
#
library(dplyr)
library(tidyr)
library(flextable)
library(labelled)

# Step 1: Recode lispop variable into 3 categories
ces25b <- ces25b %>%
  mutate(
    lispop_3cat = case_when(
      kiss_module_lispop_2 %in% c(1, 2) ~ "Agree",
      kiss_module_lispop_2 %in% c(3, 6) ~ "Neutral",
      TRUE ~ NA_character_
    ),
    lispop_3cat = factor(lispop_3cat, levels = c("Agree", "Neutral"))
  )

# Step 2: Crosstab with gender
ct <- ces25b %>%
  filter(!is.na(lispop_3cat), !is.na(cps25_genderid)) %>%  # remove NAs
  count(cps25_genderid, lispop_3cat) %>%                   # counts
  pivot_wider(
    names_from = lispop_3cat,
    values_from = n,
    values_fill = 0
  )

# Step 3: Convert to flextable
ct_flex <- flextable(ct)

# Print
ct_flex




----------------------------------------

  # First install packages
  install.packages("dplyr")
install.packages("labelled")
install.packages("crosstable")
install.packages("flextable")

# Then load them
library(dplyr)
library(labelled)
library(crosstable)
library(flextable)

# Then run the rest
ces <- ces25b %>%
  select(kiss_module_lispop_2, cps25_genderid) %>%
  mutate(
    lispop_3cat = case_when(
      kiss_module_lispop_2 %in% c(1, 2) ~ "Agree",
      kiss_module_lispop_2 %in% c(3, 6) ~ "Neutral",
      TRUE ~ NA_character_
    ),
    lispop_3cat = factor(lispop_3cat, levels = c("Agree", "Neutral")),
    
    gender_2cat = case_when(
      cps25_genderid == 1 ~ "Male",
      cps25_genderid == 2 ~ "Female",
      TRUE ~ NA_character_
    ),
    gender_2cat = factor(gender_2cat, levels = c("Male", "Female"))
  )

ct <- crosstable(
  ces,
  cols = lispop_3cat,
  by = gender_2cat,
  total = "both",
  percent_pattern = "{n} ({p_row}%)",
  percent_digits = 1
)

print(ct)
  
  
ct %>% as_flextable(keep_id = TRUE)
  


-------------------------
  
  
  
  

