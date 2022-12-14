merge_data <- function(treatments_csv, data, output, n=40) {
    treatments <- read.csv(treatments_csv)

    # read rds file if not data is provided (useful for chaining functions)
    if (! is.data.frame(data)) {
        data <- read.csv(data)
    }


    full_df <- expand.grid(id = 1:n,
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

    write.csv(transformed_dat, output)

    return(transformed_dat)
}

