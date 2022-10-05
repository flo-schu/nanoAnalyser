# change working directory to path of source file ##############################
sourcedir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(sourcedir)
library(tidyverse)

rm(list =ls())

data <- readRDS("../data/rds/data_merged.rds")



# summary
daphniaperday <- data %>% 
  group_by(picture, vessel,date_picture,treatment,concentration) %>%
  summarize(n_individuals = sum(!is.na(value))) %>% 
  group_by(vessel, date_picture) %>% 
  mutate(maximum_n = max(n_individuals)) %>% 
  ungroup() %>% 
  mutate(is_maximum = case_when(n_individuals == maximum_n ~ "yes",
                                TRUE ~ "no")) %>% 
  arrange(treatment, vessel, date_picture) %>% 
  filter(is_maximum == "yes") %>% 
  group_by(vessel, date_picture, treatment, concentration) %>% 
  summarize(n_individuals = mean(n_individuals)) 



# errors?

potential_errors <- daphniaperday %>% 
  filter(n_individuals > 200)


# plotting
ggplot(daphniaperday, 
       aes(x = date_picture,
           y = n_individuals,
           group = vessel,
           color = treatment)) +
  geom_point()+
  geom_line()
    

ggsave("../plots/daphniaperday.pdf")







