require(dplyr)
require(tidyr)
require(optparse)
source("R/processing/01_data_import.R")

parser <- OptionParser()

parser <- add_option(
    parser, c("-p","--path"), type="character",
    help="path to the results of the image analysis with kaananoij",
    metavar="path")

parser <- add_option(
    parser, c("-s", "--species"), type="character", default="Daphnia",
    help="collect files for species [default %default]",
    metavar="name")

parser <- add_option(
    parser, c("-a", "--analysis"), type="character", default="motion",
    help="type of algorithm used in kaananoij (motion, edge) [default %default]",
    metavar="name")

parser <- add_option(
    parser, c("-n", "--number_nanocosms"), type="integer", default=1,
    help="Number of nanocosms in the experiment [default %default]",
    metavar="number")

parser <- add_option(
    parser, c("-o", "--output"), type="character", default="data/csv/data.csv",
    help="output path [default %default]",
    metavar="path")

args <- parse_args(parser)



process_nanocosm <- function() {
    data <- "test"
    source("R/processing/02_data_merge.R")
    source("R/processing/03_data_prep_remove_problems.R") # needs to be customized
    source("R/processing/04_data_prep_size.R")
    source("R/processing/05_data_determine_death.R")
}
