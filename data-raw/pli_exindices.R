#--make an example dataset
#--elicted data will have to be pre-processed, for sure

library(tidyverse)

a0 <-
  readr::read_csv("data-raw/interm_output/byhand_exloadindices.csv")

pli_exindices <- a0

usethis::use_data(pli_exindices, overwrite = TRUE)
pli_exindices %>% write_csv("data-raw/pli_exindices.csv")
