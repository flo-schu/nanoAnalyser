source("R/processing/01_data_import.R")
source("R/processing/02_data_merge.R")
source("R/processing/04_data_prep_size.R")
source("R/processing/05_data_determine_death.R")

import_nano <- import_nano
merge_data <- merge_data
calculate_size_daphnia <- prepare_size_daphnia
detect_population_death <- final_death
