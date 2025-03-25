#--make an of the output from GetLoadIndex in order to have ex data to put through
#--XXX function
#--remeber to rerun when updating GetLoadIndex

a1 <- GetLoadIndex()

pli_exoutputdat <- a1

usethis::use_data(pli_exoutputdat, overwrite = TRUE)

pli_exdat %>% readr::write_csv("data-raw/pli_exoutputdat.csv")
