#H1
library(here)
source(here("R_Scripts/1_data_import.R"))

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
var_label(ces25b$kiss_module_lispop_3)

ces_long %>% 
  mutate(Label=case_when(
    Variable=="Truth"~"Politicians should just tell it like it is",
    Variable=="Ordinary"~"People are more likely to vote for politicians that appear ordinary"
    )) %>% 
ggplot(., aes(x = Response)) +
  geom_bar() +                       # counts by default
  facet_wrap(~str_wrap(Label, 30), ncol = 2) +
  labs(
    title = "",
    x = "Response Category",
    y = "Frequency (count)"
  )+theme(
    strip.text = element_text(hjust = 0) # Left-aligns the facet labels
  )
ggsave(filename=here("Plots/h1.png"), width=12, height=8)
