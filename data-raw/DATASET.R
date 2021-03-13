## code to prepare `DATASET` dataset goes here

ffr_fdr_sample <- read.csv('sample.csv')

usethis::use_data(ffr_fdr_sample, internal = TRUE, overwrite = TRUE)
