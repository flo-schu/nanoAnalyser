# change working directory to path of source file ##############################
sourcedir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(sourcedir)
library(tidyverse)

rm(list =ls())

data <- readRDS("../data/rds/data_merged.rds")

# summary
# picture with maximum amount of Daphnia was selected
d <- data %>% 
  # remove vessels in which no daphnia populations developed
  filter(! vessel %in% c(12,14,15,27,30)) %>% 
  
  # remove vessels with extremely low daphnia populations
  filter(! vessel %in% c(24,25,33)) %>% 
  
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
  summarize(n_individuals = mean(n_individuals)) %>% 
  group_by(date_picture,concentration) %>% 
  summarise(abundance= mean(n_individuals),
            sd_abundance = sd(n_individuals),
            n_treatment = n()) %>% 
  mutate(se_abundance = sd_abundance / sqrt(n_treatment)) 
  


ggplot(d, 
       aes(x = concentration,
           y = abundance,
           group = date_picture)) +
  geom_point()+
  geom_errorbar(aes(x = concentration,
                    ymin = abundance - se_abundance,
                    ymax = abundance + se_abundance,
                    group = date_picture),
                alpha = .5, width = .1)+
  geom_line()+
  theme_few()+
  scale_x_log10()+
  xlab("Esfenvalerate [mg L-1]")+
  facet_wrap(~date_picture)


ggsave("../plots/abundance_concentration.jpg",
       width = 16, height = 16)
