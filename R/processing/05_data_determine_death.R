# change working directory to path of source file ##############################
# sourcedir <- dirname(rstudioapi::getActiveDocumentContext()$path)
# setwd(sourcedir)
# library(tidyverse)
# rm(list =ls())

data <- readRDS(file.path(data_dir, "rds/structure_long.rds"))
size <- readRDS(file.path(data_dir, "rds/size_individuals.rds"))

ab <- data %>% 
  # spread data to have a column for each tank
  group_by(id, date) %>% 
  summarize(abundance = sum(n, na.rm = TRUE)) %>% 
  pivot_wider(id_cols = date, names_from = id, 
              values_from = abundance, names_prefix = "v_")


# mark tanks as dead if they had 6 consecutive images without any organisms
deathpoint_tank <- list()
for(j in 1:ncol(ab[,-1])){
  temp <- t(ab[,j+1])
  timesteps <- list()
  for(i in 1:6){
    timesteps[[i]] <- c(rep(0,i-1),temp[1:(nrow(ab)-i+1)])[-c(1:3)]
  }
  temp2 <-do.call(cbind,timesteps)
  temp2 <- apply(temp2, 1, sum)
  if( any(temp2 <= 6)){
    deathpoint_tank[[j]] <- min(which(temp2 <= 6))
  } else{
    deathpoint_tank[[j]] <- as.numeric(NA)
  }
}

# set the respective metrics to zero where tank death was set
date_tankedeath <- do.call(c,deathpoint_tank)
date_match <- data.frame(end_id = 1:length(unique(size$date))) %>%
  mutate(date = unique(size$date))

tankdeath <- cbind(id = 1:40,
      end_id = date_tankedeath) %>% 
  as_tibble() %>% 
  left_join(date_match, by = "end_id") %>% 
  rename(end_date = date) %>% 
  select(-end_id) %>% 
  replace_na(list(end_date = format(Sys.time(),"%Y-%m-%d")))

dmod <- data %>% 
  left_join(tankdeath, by = "id") %>% 
  mutate(n = case_when(date > end_date ~ 0,
                       TRUE ~ as.numeric(n)),
         mean_length =  case_when(date > end_date ~ 0,
                                  TRUE ~ as.numeric(mean_length)),
         mean_biomass =  case_when(date > end_date ~ 0,
                                   TRUE ~ as.numeric(mean_biomass))) %>% 
  group_by(date, id)

# in dmod n, biomass and length are set to zero on dates after the "tank-death-date"
saveRDS(dmod, file.path(data_dir, "rds/structure_long_mod.rds"))
write.csv(dmod, file.path(data_dir, "csv/structure_long_mod.csv"))




