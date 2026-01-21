# install packages if necessary
devtools::install_github("sjikss/cesdata2")

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
