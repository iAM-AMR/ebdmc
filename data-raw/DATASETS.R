
# Create exported data objects.
# Access via system.file("extdata", "filename.csv", package = "ebdmc", mustWork = TRUE)

library('here')
library('tidyverse')

beta_diversity_measures <- readr::read_csv(here::here("inst", "extdata", "beta_diversity_measures.csv"))
usethis::use_data(SHCKON2013_cgMLST, overwrite = TRUE)

SHCKON2013_cgMLST       <- readr::read_csv(here::here("inst", "extdata", "SHCKON2013_cgMLST.csv"))
usethis::use_data(SHCKON2013_cgMLST, overwrite = TRUE)

SHCKON2013_SNP          <- readr::read_csv(here::here("inst", "extdata", "SHCKON2013_SNP.csv"))
usethis::use_data(SHCKON2013_SNP, overwrite = TRUE)


