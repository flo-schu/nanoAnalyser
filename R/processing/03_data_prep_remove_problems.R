data <- readRDS(file.path(data_dir, "rds/data_merged.rds"))

problem_check <- data %>% filter(
  (
    date == as.Date("2020-01-22") & picture_number %in% c(81:83) |
    date == as.Date("2020-01-24") & picture_number %in% c(81:86) 
  )
) 

# check if deletion is correct
unique(problem_check$id)
unique(problem_check$picture_number)
unique(problem_check$date)

d <- data %>% filter(
  !(
    date == as.Date("2020-01-22") & picture_number %in% c(81:83) |
    date == as.Date("2020-01-24") & picture_number %in% c(81:86) 
   )
) 

nrow(d) + nrow(problem_check) == nrow(data)

saveRDS(d, file.path(data_dir, "rds/data_merged_removed_problems.rds"))
write.csv(d, file.path(data_dir, "csv/data_merged_removed_problems.csv"))

