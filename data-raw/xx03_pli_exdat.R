#--make an example dataset
#--elicted data will have to be pre-processed, for sure

rm(list = ls())

a0 <-
  readxl::read_excel("data-raw/elicitations/ANT - compost lettuce.xlsx", skip = 5) %>%
  tidyr::fill(title, scenario) %>%
  dplyr::mutate(ai_conc = as.numeric(ai_conc),
                prod_amt = as.numeric(prod_amt))


# a1. fix units -----------------------------------------------------------

a1 <-
  a0 %>%
  dplyr::mutate(prodconc_gai_g = ai_conc/100,
                prodamt_g_ha = prod_amt * 1000) %>%
  dplyr::mutate(prodconc_gai_g = ifelse(is.na(prodconc_gai_g), 0, prodconc_gai_g),
                prodamt_g_ha = ifelse(is.na(prodamt_g_ha), 0, prodamt_g_ha),
                ai_name = ifelse(is.na(ai_name), "none", ai_name)) %>%
  dplyr::select(title, scenario, ai_name, prodconc_gai_g, prodamt_g_ha)

pli_exdat <- a1

usethis::use_data(pli_exdat, overwrite = TRUE)

pli_exdat %>% readr::write_csv("data-raw/pli_exdat.csv")
