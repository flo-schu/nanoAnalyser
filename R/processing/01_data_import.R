require(dplyr)
require(tidyr)

import_nano <- function(
    path, fname, n,
    species="",
    analysis="motion",
    out=".",
    date_format="%Y%m%d") {

    ppv <- 3 # pictures per vessel

    vessel_matching <- expand.grid(
        picture = 0:(ppv - 1),
        id = 1:n
    ) %>%
        mutate(picture_number = 0:(ppv * n - 1))

    dat <- list()
    dates <- dir(path)

    for (i in dates) {
        if (is.na(as.Date.character(i, format = date_format))) next
        temp <- read.table(
            file.path(path, i, fname), sep = "\t", header = TRUE
        ) %>%
            as_tibble()


        temp2 <- temp[apply(temp, 1, sum) != 0, ]

        prepared_data <- temp2 %>%
            gather() %>%
            dplyr::filter(value != 0) %>%
            mutate(key = sub("X", "", key)) %>%
            rename(picture_number = key) %>%
            mutate(picture_number = as.integer(picture_number)) %>%
            full_join(vessel_matching, by = "picture_number") %>%
            mutate(date = as.Date(i, date_format)) %>%
            as.data.frame()

        dat[[i]] <- prepared_data

        rm(temp, prepared_data)
    }

    data <- do.call(rbind, dat)

    if (species != "") {
        data["species"] <- species
    }

    data["analysis"] <- analysis

    if (out == ".") {
        out <- file.path(path, "data.csv")
    }

    write.csv(data, out, row.names = FALSE)

    # identify error sources
    potential_errors <- data %>%
        dplyr::filter(value > 500) # %>%
    # .$date_picture %>%

    cat("\n==================== POTENTIAL ERRORS ===========================\n")
    print(potential_errors)
    cat("\n=================================================================\n")


    return(data)
}