## code to prepare `DATASET` dataset goes here

ffr_fdr_sample <- read.csv('sample.csv')

usethis::use_data(ffr_fdr_sample, overwrite = TRUE)

hazard_accumulate_sample <- read.csv('hazard_accumulate.csv')

hazard_accumulate_sample$CALC_PROD_DT_ind <- substr(as.character(hazard_accumulate_sample$CALC_PROD_DT_ind), 3, 6)

usethis::use_data(hazard_accumulate_sample, overwrite = TRUE)
