prepare_size_daphnia <- function(treatments_csv, data, output, output_size, n=40) {
    treatments <- read.csv(treatments_csv)

    # read rds file if not data is provided (useful for chaining functions)
    if (! is.data.frame(data)) {
        data <- read.csv(data)
    }

    empty_grid <- expand.grid(
        date = unique(data$date),
        id = 1:n,
        sizeclass = c("adult","juvenile","neo"),
        stringsAsFactors = FALSE
    ) %>%
        left_join(treatments, by = "id")



    # summary
    d <- data %>%
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
    write.csv(d, output_size)


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


    write.csv(dsum, output)

    return(
      list(data=dsum, data_indivudal=d)
    )

}
