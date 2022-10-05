require(dplyr)
require(tidyr)
require(optparse)
source("R/processing/01_data_import.R")

parser <- OptionParser()

parser <- add_option(
    parser, c("-p", "--path"), type="character", default="data/image_results",
    help="path to the results of the image analysis with kaananoij [default %default]",
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
    parser, c("-o", "--output"), type="character", default=".",
    help="output path [default %default]",
    metavar="path")

# args <- parse_args(parser, args= c("--path", "data/image_results", "-s", "Daphnia", "-a", "motion", "-n", 40, "-o", "data/csv/data.csv"))
args <- parse_args(parser)



species <- args$species
collect_files <- list()

if (species == "Daphnia") {
    collect_files[[1]] <- "Erg_gross.txt"
} else if (species == "Culex") {
    collect_files[[1]] <- "#AbundImmo.txt"
    collect_files[[2]] <- "#AbundMobil.txt"
}


for (fname in collect_files) {
    data <- import_nano(
        path = args$path,
        fname = fname,  # data of pixel size
        n = args$number_nanocosms,
        species = species,
        out = args$output
    )
}
