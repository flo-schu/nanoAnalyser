# change working directory to path of source file ##############################
# sourcedir <- dirname(rstudioapi::getActiveDocumentContext()$path)
# setwd(sourcedir)
# library(tidyverse)
# library(ggthemes)
# rm(list =ls())

data <- readRDS(file.path(data_dir, "rds/structure_long_mod.rds"))
treatments <- read.csv(file.path(data_dir, "csv/treatments.csv"), stringsAsFactors = FALSE)

dat <- data %>% 
  # remove ids in which no daphnia populations developed
  filter(! id %in% uncolonized) %>% 
  
  # remove ids with extremely low daphnia populations
  filter(! id %in% extremely_low_pop) %>% 
  group_by(id, date) %>% 
  pivot_wider(id_cols = c(date), 
              names_from = c(sizeclass,id), 
              values_from = c(n, mean_length, mean_biomass), names_sep = "__")

loopmat <- dat[,2:ncol(dat)]

ma <- list()
cnames <- colnames(loopmat)

for(j in 1:ncol(loopmat)){
  temp <- t(loopmat[,j])
  timesteps <- list()
  for(i in 1:n_steps){
    timesteps[[i]] <- c(rep(0,i-1),temp[1:(nrow(loopmat)-i+1)])[-c(1:(n_steps/2))]
  }
  temp2 <-do.call(cbind,timesteps)
  ma[[cnames[j]]] <- apply(temp2, 1, mean, na.rm = TRUE)
}

ma <- do.call(cbind,ma) 
ma <- cbind(as.data.frame(dat[-c(1:(n_steps/2)),1]),ma)

d_smooth <- ma %>% 
  pivot_longer(cols = -date, names_to = c("index","sizeclass", "id"),
               names_sep = "__", values_to = "value", 
               names_ptypes = list(id = integer())) %>% 
  left_join(treatments, by = "id")

saveRDS(d_smooth, file.path(data_dir, "rds/data_smoothed.rds"))
write.csv(d_smooth, file.path(data_dir, "csv/data_smoothed.csv"))