merge_data <- function(treatments_csv, data, output) {
    treatments <- read.csv(treatments_csv)

    # read rds file if not data is provided (useful for chaining functions)
    if (class(data) != "data.frame") {
        data <- readRDS(data)
    }


    full_df <- expand.grid(id = 1:40,
                           picture = 0:2,
                           date = unique(data$date)) %>%
      mutate(matchcolumn = paste(picture, id, date, sep="_"))

    transformed_dat <- data %>%
      mutate(matchcolumn = paste(picture, id, date, sep="_")) %>%
      ungroup() %>%
      select(-c(picture, id, date)) %>%
      full_join(full_df, by = "matchcolumn") %>%
      full_join(treatments, by = "id") %>%
      select(-matchcolumn)
      # replace_na(replace=list(value = 0))


    saveRDS(transformed_dat, output)
    write.csv(transformed_dat, output)

    return(transformed_dat)
}

