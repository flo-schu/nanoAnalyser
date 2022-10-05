# change working directory to path of source file ##############################
# sourcedir <- dirname(rstudioapi::getActiveDocumentContext()$path)
# setwd(sourcedir)
# library(tidyverse)
# library(ggthemes)
# rm(list =ls())
require(ggplot2)
library(stringr)

data <- readRDS(file.path(data_dir, "rds/data_smoothed.rds"))

d <- data %>% filter(index == "n") %>% 
  mutate(plotgroup = paste(sizeclass, concentration, id, sep = "_")) 

for(conc in unique(d$concentration)){
    ggplot(dplyr::filter(d, concentration == conc), 
        aes(x=date, y= value,
            group = plotgroup,
            color = as.factor(id)))+
    geom_point()+
    geom_line()+
    geom_vline(xintercept = c(as.Date("2019-11-20"),as.Date("2020-01-22")),
             linetype = 2, color = "grey")+
    scale_color_discrete(name=paste0(conc, " µg L-1"))+
    xlab("Date")+
    ylab("Abundance")+
    theme_few()+
    theme(
        legend.position = c(.08,.75),
        legend.background = element_blank()
    )+
    facet_wrap(~sizeclass, scales = "fixed")+
    expand_limits(y = max(d$value, na.rm=TRUE))
    ggsave(file.path("plots", paste0("abundance_smoothed_time_", 
           str_pad(conc*1000, 5, pad="0"),".png")), width=16, height = 6)
}

davg <- d %>% group_by(concentration, sizeclass, date) %>% 
  summarize(abundance = mean(value, na.rm= TRUE)) %>% 
  mutate(plotgroup = paste(sizeclass, concentration, sep = "_")) 



ggplot(davg, 
       aes(x=date, y= abundance,
           group = plotgroup,
           color = concentration))+
  geom_point()+
  geom_line()+
  geom_vline(xintercept = c(as.Date("2019-11-20"),as.Date("2020-01-22")),
             linetype = 2, color = "grey")+
  scale_color_gradient(
    low = "grey", high = "red",
    name = "Esfenvalerate mg L-1", trans = "log10")+
  xlab("Date")+
  theme_few()+
  theme(
    legend.position = c(.08,.75),
    legend.background = element_blank()
  )+
  facet_wrap(~sizeclass, scales = "free_y")


ggsave(file.path("plots", "abundance_smoothed_time.png"), width=16, height = 6)

