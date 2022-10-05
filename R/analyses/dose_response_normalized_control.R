# change working directory to path of source file ##############################
# sourcedir <- dirname(rstudioapi::getActiveDocumentContext()$path)
# setwd(sourcedir)
# library(tidyverse)
# rm(list =ls())

data <- readRDS(file.path(data_dir, "rds/data_merged.rds"))

# summary
# picture with maximum amount of Daphnia was selected
d <- data %>% 
  # remove ids in which no daphnia populations developed
  filter(! id %in% uncolonized) %>% 
  
  # remove ids with extremely low daphnia populations
  filter(! id %in% extremely_low_pop) %>% 
  
  group_by(picture, id,date,treatment,concentration) %>%
  summarize(n_individuals = sum(!is.na(value))) %>% 
  
  group_by(id, date) %>% 
  mutate(maximum_n = max(n_individuals)) %>% 
  ungroup() %>% 
  mutate(is_maximum = case_when(n_individuals == maximum_n ~ "yes",
                                TRUE ~ "no")) %>%
  arrange(treatment, id, date) %>% 
  filter(is_maximum == "yes") %>% 
  group_by(id, date, treatment, concentration) %>% 
  summarize(n_individuals = mean(n_individuals)) %>% 
  group_by(date,concentration) %>% 
  summarise(abundance= mean(n_individuals),
            sd_abundance = sd(n_individuals),
            n_treatment = n()) %>% 
  mutate(se_abundance = sd_abundance / sqrt(n_treatment)) %>% 
  pivot_wider(id_cols = c(date), names_from = concentration,
              names_prefix = "conc_",
              values_from = abundance)
d_norm <- d %>% 
  mutate(control = conc_0) %>% 
  transmute_at(.funs = list(~./control), .vars = vars(2:9)) %>% 
  pivot_longer(cols = starts_with("conc_"),
               names_to = "concentration", values_to = "abundance",
               names_prefix = "conc_") %>% 
  mutate(concentration = as.numeric(concentration))


ggplot(d_norm, 
       aes(x = concentration,
           y = abundance,
           group = date)) +
  geom_point()+
  # geom_errorbar(aes(x = concentration,
  #                   ymin = abundance - se_abundance,
  #                   ymax = abundance + se_abundance,
  #                   group = date),
  #               alpha = .5, width = .1)+
  geom_line()+
  geom_hline(aes(yintercept = 1), linetype = 2)+
  theme_few()+
  scale_x_log10()+
  xlab("Esfenvalerate [mg L-1]")+
  facet_wrap(~date)

ggsave(file.path("plots", "abundance_normed_conc.jpg"), width=16, height = 16)
