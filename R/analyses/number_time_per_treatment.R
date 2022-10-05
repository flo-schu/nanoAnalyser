# change working directory to path of source file ##############################
sourcedir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(sourcedir)
library(tidyverse)
library(ggthemes)

rm(list =ls())

data <- readRDS("../data/rds/data_merged.rds")


# summary
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
  summarize(n_individuals = mean(n_individuals))


  
  
dsum <- d %>%   
  group_by(date_picture,concentration) %>% 
  summarise(abundance= mean(n_individuals),
            sd_abundance = sd(n_individuals),
            n_treatment = n()) %>% 
  mutate(se_abundance = sd_abundance / sqrt(n_treatment)) 

# plotting
ggplot(dsum, 
       aes(x = date_picture,
           y = abundance,
           group = concentration,
           color = concentration)) +
  geom_point()+
  geom_errorbar(aes(x = date_picture,
                    ymin = abundance - se_abundance,
                    ymax = abundance + se_abundance,
                    group = concentration,
                    color = concentration),
                alpha = .5)+
  geom_vline(xintercept = as.Date("2019-11-20"))+
  geom_line()+
  theme_few()+
  scale_color_gradient(
    low = "grey", high = "red",
    name = "Esfenvalerate \n mg L-1", trans = "log10",
    breaks = unique(d$concentration))+
  xlab("Date")+
  theme(
    legend.position = c(.1,.7)
  )
  
    

ggsave("../plots/abundance_time.jpg")







