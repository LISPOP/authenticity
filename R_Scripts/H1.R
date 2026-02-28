#H1
rm(list = ls())

source("1_data_import.R")

library(dplyr)
library(tidyr)
library(ggplot2)

#Frequency tables
table(ces$Truth, useNA = "ifany")
table(ces$Ordinary, useNA = "ifany")

#Bar graphs (frequencies)

ces_long <- ces %>%
  select(Truth, Ordinary) %>%
  pivot_longer(cols = c(Truth, Ordinary),
               names_to = "Variable",
               values_to = "Response") %>%
  filter(!is.na(Response))  # remove NAs for the graph

ggplot(ces_long, aes(x = Response)) +
  geom_bar() +                       # counts by default
  facet_wrap(~Variable, ncol = 2) +
  labs(
    title = "H1: Frequencies of Authenticity Proxy Items",
    x = "Response Category",
    y = "Frequency (count)"
  )