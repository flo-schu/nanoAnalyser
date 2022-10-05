data <- readRDS(file.path(data_dir, "rds/data_merged_removed_problems.rds"))
treatments <- read.csv(file.path(data_dir, "csv/treatments.csv"))

# 25.10. sollte wahrscheinlich rausgefiltert werden. Da scheint es noch
# massiv fehler zu geben

empty_grid <- expand.grid(date = unique(data$date),
                          id = 1:40,
                          sizeclass = c("adult","juvenile","neo"),
                          stringsAsFactors = FALSE) %>% 
  left_join(treatments, by = "id")



# summary
d <- data %>% 
  # filter( date != "2019-10-25") %>% 

  # select picture with the maximum amount of individuals on it of each
  # date x sample combination
  group_by(picture, id,date,treatment,concentration) %>%
  mutate(n_individuals = sum(!is.na(value))) %>% 
  group_by(id, date) %>% 
  mutate(maximum_n = max(n_individuals)) %>% 
  ungroup() %>% 
  mutate(is_maximum = case_when(n_individuals == maximum_n ~ "yes",
                                TRUE ~ "no")) %>% 
  arrange(treatment, id, date) %>% 
  filter(is_maximum == "yes") %>% 

  # still there can be more than one pic with the same number of individuals 
  # on it. So we do another screening for maximum size and again select the one
  # with maximum size on it. This choice is best, because the calculations are
  # based on the assumption that organisms stay closest to the camera, which 
  # would result to the maximum total size.
  group_by(id, date, treatment, concentration, picture_number) %>% 
  mutate(total_area = sum(value)) %>% 
  ungroup() %>% 
  group_by(id, date, treatment, concentration) %>% 
  mutate(max_area = max(total_area)) %>% 
  mutate(is_max_area = case_when(max_area == total_area ~ "yes",
                                TRUE ~ "no")) %>% 
  filter(is_max_area == "yes") %>% 

  # remove temporary columns
  select(-c( 
    maximum_n,max_area,total_area,
    is_max_area,is_maximum, picture_number)) %>% 
  
  # the conversion from detected_pixels to measured size is described 
  # in a technical document (<nanomodel/docs/KalibrierungErgebnisse.doc>)
  # and has been well calibrated
  # convert bodylength to biomass by formula from Kaarina's publication 
  # (10.1016/j.aquatox.2011.09.013), which cites the bodylength ~ biomass 
  # conversion originally published in Dumont et al., 1975
  # this formula requires the bodylength in µm and gives µg
  # TODO: Where does the size conversion value -> bodylength come from
  mutate(bodylength = sqrt(value / 35.5),
         biomass = 1.5 * 10 ^ -8 * (bodylength * 1000) ^ 2.84 / 1000) %>% 
  select(-value) %>% 

  # split individuals in size classes distributed as chosen by Kaarina. I don't 
  # find the published  values, but I suppose they don't matter much for
  # statistical analyses, but they would for modelling. 
  mutate(sizeclass = case_when(bodylength <= 1.68 ~ "neo",
                               bodylength <= 2.28 ~ "juvenile",
                               bodylength >  2.28 ~ "adult")) %>% 
  
  # remove superflous columns and add size classes
  ungroup() %>% 
  select(-c(treatment, concentration)) %>% 
  full_join(empty_grid, by = c("date", "id", "sizeclass"))


# this dataset is probably the most relevant for modelling as it contains all
# photographed individuals
saveRDS(d, file.path(data_dir, "rds/size_individuals.rds"))
write.csv(d, file.path(data_dir, "csv/size_individuals.csv"))


dsum <- d %>% 
  
  # summarize 
  group_by(date, id,treatment, concentration, n_individuals,sizeclass ) %>% 
  summarize(n = n(),
            mean_length = mean(bodylength),
            mean_biomass = mean(biomass)) %>% 

  # removes false n=1 counts which only come from completing the dataset. 
  # it sets those entries to NA, which is stupid, because they should be zero
  # TODO: set change as.numeric(NA) to 0
  mutate(n = case_when(is.na(mean_length) & n == 1 ~ as.numeric(NA),
                       TRUE ~ as.numeric(n)))


saveRDS(dsum, file.path(data_dir, "rds/structure_long.rds"))
write.csv(dsum, file.path(data_dir, "csv/structure_long.csv"))

# provide the same as wide table format
dsumwide <- dsum %>% 
  pivot_wider(id_cols = c(id,sizeclass,concentration,treatment),
              names_from = date,
              values_from = c(n,mean_length,mean_biomass)) %>% 
  arrange(id, sizeclass) %>% 
  complete(concentration, treatment, nesting(id = full_seq(id,1) ,
                                             sizeclass = c("neo","juvenile","adult")))
saveRDS(dsumwide, file.path(data_dir, "rds/structure_wide.rds"))
write.csv(dsumwide, file.path(data_dir, "csv/structure_wide.csv"))


# at this point we have some missing observations which may either result from
# no individuals or a missing foto series for a tank/date combination.
# This could to some extent be resolved by noting when tanks have gone empty
# The missing observations are going to be tried to be smoothed by a moving 
# average. This however can only work if the missing values only occurr in an 
# intermittent fashion and are not in a longer row.

# ab <- dsum %>% 
#   group_by(id, date) %>% 
#   summarize(abundance = sum(n, na.rm = TRUE)) %>% 
#   pivot_wider(id_cols = date, names_from = id, 
#               values_from = abundance, names_prefix = "v_")

# deathpoint_tank <- list()
# for(j in 1:ncol(ab[,-1])){
#   temp <- t(ab[,j+1])
#   timesteps <- list()
#   for(i in 1:6){
#     timesteps[[i]] <- c(rep(0,i-1),temp[1:(nrow(ab)-i+1)])[-c(1:3)]
#   }
#   temp2 <-do.call(cbind,timesteps)
#   temp2 <- apply(temp2, 1, sum)
#   if( any(temp2 <= 6)){
#     deathpoint_tank[[j]] <- min(which(temp2 <= 6))
#   } else{
#     deathpoint_tank[[j]] <- as.numeric(NA)
#   }
  
  
    
# }

# date_tankedeath <- do.call(c,deathpoint_tank)
# date_match <- data.frame(end_id = 1:length(unique(d$date))) %>%
#   mutate(date = unique(d$date))

# tankdeath <- cbind(id = 1:40,
#       end_id = date_tankedeath) %>% 
#   as_tibble() %>% 
#   left_join(date_match, by = "end_id") %>% 
#   rename(end_date = date) %>% 
#   select(-end_id) %>% 
#   replace_na(list(end_date = format(Sys.time(),"%Y-%m-%d")))

# dmod <- dsum %>% 
#   left_join(tankdeath, by = "id") %>% 
#   mutate(n = case_when(date > end_date ~ 0,
#                        TRUE ~ as.numeric(n)),
#          mean_length =  case_when(date > end_date ~ 0,
#                                   TRUE ~ as.numeric(mean_length)),
#          mean_biomass =  case_when(date > end_date ~ 0,
#                                    TRUE ~ as.numeric(mean_biomass))) %>% 
#   group_by(date, id)

# # in dmod n, biomass and length are set to zero on dates after the "tank-death-date"
# saveRDS(dmod, file.path(data_dir, "rds/structure_long_mod.rds"))
# write.csv(dmod, file.path(data_dir, "csv/structure_long_mod.csv"))




