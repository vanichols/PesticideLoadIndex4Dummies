#--make an example dataset
#--elicted data will have to be pre-processed, for sure


a0 <-
  readxl::read_excel("data-raw/elicitations/ANT - compost lettuce.xlsx", skip = 5) %>%
  tidyr::fill(title, scenario) %>%
  dplyr::mutate(ai_conc = as.numeric(ai_conc),
                prod_amt = as.numeric(prod_amt))


# a1. fix units -----------------------------------------------------------

a1 <-
  a0 %>%
  dplyr::mutate(ai_con_g_g = ai_conc/100,
                prod_amt_g_ha = prod_amt * 1000) %>%
  dplyr::mutate(ai_con_g_g = ifelse(is.na(ai_con_g_g), 0, ai_con_g_g),
                prod_amt_g_ha = ifelse(is.na(prod_amt_g_ha), 0, prod_amt_g_ha),
                ai_name = ifelse(is.na(ai_name), "none", ai_name)) %>%
  dplyr::select(title, scenario, ai_name, ai_con_g_g, prod_amt_g_ha)

pli_exdat <- a1

usethis::use_data(pli_exdat, overwrite = TRUE)
