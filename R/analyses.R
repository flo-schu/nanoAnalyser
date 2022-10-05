# plots and analyses -----------------------------------------------------------
# this is an example script for some basic analyses of the systems can be 
# factored into functions for use in the package

# library(ggthemes)
# library(ggplot2)
# library(tidyverse)

# plot_dir <- "plots"
# data_dir <- "data"

# # remove problematic data
# uncolonized <- c(12, 14, 15, 27, 30)
# extremely_low_pop <- c(24, 25, 33)

# # smoothing of data
# n_steps <- 8 # smoothing kernel (?)
# source("src/R/05_data_prep_smooth.R") # watch out some vessels are removed here!


# # plot time series of abundance of Daphnia Magna with different size classes
# # averaged over treatments and smoothed
# source("src/R/structure_abundance_time.R")

# # plot dose response curves normalized to control treatment
# source("src/R/dose_response_normalized_control.R")
