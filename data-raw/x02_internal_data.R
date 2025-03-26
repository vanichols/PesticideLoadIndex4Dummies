# created 14 feb 2025
# purpose: get just the values we need and in a better format

library(tidyverse)

Sys.setenv(LANG = "en")
rm(list = ls())


# A. example input data ---------------------------------------------------
#--why is this internal data? It is pli_exdat 18 feb 2025

# a0 <-
#   readxl::read_excel("data-raw/elicitations/ANT - compost lettuce.xlsx", skip = 5) %>%
#   tidyr::fill(title, scenario) %>%
#   dplyr::mutate(ai_conc = as.numeric(ai_conc),
#          prod_amt = as.numeric(prod_amt))
#
#
# # a1. fix units -----------------------------------------------------------
#
# a1 <-
#   a0 %>%
#   dplyr::mutate(ai_con_g_g = ai_conc/100,
#          prod_amt_g_ha = prod_amt * 1000) %>%
#   dplyr::mutate(ai_con_g_g = ifelse(is.na(ai_con_g_g), 0, ai_con_g_g),
#          prod_amt_g_ha = ifelse(is.na(prod_amt_g_ha), 0, prod_amt_g_ha),
#          ai_name = ifelse(is.na(ai_name), "none", ai_name)) %>%
#   dplyr::select(title, scenario, ai_name, ai_con_g_g, prod_amt_g_ha)
#
# internal_antex <- a1


# B. fit and save models ------------------------------------------------

#--this is in progress
#--24 March 2025, starting trying to change these
#--imagine this sort of df:
# metric    xmin    xmax   int   slope
# DT50      0       30       0     0.004

# b1. bcf model -----------------------------------------------------------
#--bioconcentration

ref_bcf <-
  readxl::read_excel("data-raw/byhand_bcf-ref-values.xlsx", skip = 5) %>%
  mutate_at(c(2, 3, 4, 5), as.numeric) %>%
  fill(name)

#--place results will be stored
res_bcf <-
  ref_bcf %>%
  select(name, xmin, xmax) %>%
  mutate(segment = 1:n())

#--build the df to merge
b1res_holder <- NULL

#--work through each segment segment

#--I have no idea what the warning is, it works so ignore it I guess
for (i in 1:ncol(res_bcf)) {
  tmp.bcf1 <-
    ref_bcf %>%
    slice(i)

  tmp.bcf2 <- tibble(
    x = c(tmp.bcf1$xmin, tmp.bcf1$xmax),
    y = c(tmp.bcf1$ymin, tmp.bcf1$ymax)
  )

  tmp.int <- as.numeric(lm(y ~ x, data = tmp.bcf2)$coefficients[1])
  tmp.slp <- as.numeric(lm(y ~ x, data = tmp.bcf2)$coefficients[2])

  res <- tibble(segment = i,
                int = tmp.int,
                slp = tmp.slp)
  b1res_holder <-
    b1res_holder %>%
    rbind(res)

}

b1 <-
  res_bcf %>%
  left_join(b1res_holder)

tst <- b1 %>%
  filter(segment == 1)

50 * tst$slp + tst$int

internal_mod_bcf <- b1

# b2. soil degrad model -----------------------------------------------------------
#--soil degradation, dt50

ref_dt50 <-
  readxl::read_excel("data-raw/byhand_dt50-ref-values.xlsx", skip = 5) %>%
  mutate_at(c(2, 3, 4, 5), as.numeric) %>%
  fill(name)

#--place results will be stored
res_dt50 <-
  ref_dt50 %>%
  select(name, xmin, xmax) %>%
  mutate(segment = 1:n())

#--build the df to merge
b2res_holder <- NULL

#--work througNULL#--work through each segment segment
for (i in 1:ncol(res_dt50)){

  tmp.dt501 <-
    ref_dt50 %>%
    slice(i)

  tmp.dt502 <- tibble(x = c(tmp.dt501$xmin, tmp.dt501$xmax),
                     y = c(tmp.dt501$ymin, tmp.dt501$ymax))

  tmp.int <- as.numeric(lm(y ~ x, data = tmp.dt502)$coefficients[1])
  tmp.slp <- as.numeric(lm(y ~ x, data = tmp.dt502)$coefficients[2])

  res <- tibble(
    segment = i,
    int = tmp.int,
    slp = tmp.slp
  )
  b2res_holder <-
    b2res_holder %>%
    rbind(res)

}

b2 <-
  res_dt50 %>%
  left_join(b2res_holder)

tst <- b2 %>%
  filter(segment == 4)

370 * tst$slp + tst$int

internal_mod_dt50 <- b2

# b3. just an equation for ecotox vals --------------------------------------------------
#--this should simply be a calculator
# PLI = 1 / (value/reference)

internal_mod_ecotox <-
  readxl::read_excel("data-raw/byhand_ecotox-ref-values.xlsx", skip = 5) %>%
  select(1, 3)

# D. PPDB internal data ---------------------------------------------------

# d0. raw data ----------------------------------------------------------------

rdat <-
  readxl::read_excel("data-raw/ppdb/Requested-PPDB-format.xlsx") %>%
  janitor::clean_names()

rdat.names <- names(rdat)

#--note they send me an updated version every 3 months
#--do not worry about the warnings
draw <-
  readxl::read_excel("data-raw/ppdb/PPDB_Aarhus_University_24-07-15.xlsx") %>%
  janitor::clean_names()

draw.names <- names(draw)

#--some we asked for but didn't get? might be a replacement for them
#--for ex we asked for bioconcentration_factor_bcf_lkg
#--it is actually bioconcentration_factor_bcf_l_kg
setdiff(rdat.names, draw.names)

#--a lot we didn't ask for, _qb is an indicator of quality
setdiff(draw.names, rdat.names)


# d1. clean it up: 154,602 entries----------------------------------------------------------

#--qb

d1 <-
  draw %>%
  dplyr::mutate_all(as.character) %>%
  tidyr::pivot_longer(6:ncol(.)) %>%
  dplyr::mutate(value2 = as.numeric(value)) %>%
  dplyr::mutate_if(is.character, str_to_lower)

# d2. get rid of dupes: 154,592 ---------------------------------

d2 <-
  d1 %>%
  dplyr::distinct()


# d3. eliminate the _qb names, 103,068 ----------------------------------------------

d3 <-
  d2 %>%
  dplyr::filter(!grepl("_qb", name))



# d4. the _2 and = data, 26,371 ----------------------------------------------------

#--these seem safe to remove
tmp.d4 <-
  d3 %>%
  dplyr::filter(value == "=")

tmp.d4 <-
  d3 %>%
  dplyr::filter(grepl("_2$", name)) %>%
  dplyr::select(name)

d4 <-
  d3 %>%
  dplyr::filter(value != "=") %>%
  dplyr::filter(!grepl("_2$", name))

# d5. arrange and id within name-----------------------------------------------------------

d5 <-
  d4 %>%
  dplyr::group_by(name) %>%
  dplyr::arrange(value2, .by_group = TRUE)


d5.tmp <-
  d5 %>%
  dplyr::group_by(name) %>%
  dplyr::summarise(
    min = min(value2),
    mn = mean(value2),
    med = median(value2),
    max = max(value2)
  ) %>%
  dplyr::distinct()


#--these all have extreme values, the mean and median are nowhere close to each other
#--log transformation is probably needed, but won't work for 0
d5.tmp2 <-
  d5.tmp %>%
  dplyr::filter(!is.na(min))

#--only two parameters have a value of 0, and we don't actually use these in the danish pli, so don't worry about it
d5.tmp3 <-
  d5.tmp %>%
  dplyr::filter(min==0)

# d6. used to overwrite 0s-----------------------------------------

d6 <- d5

# d7. order within names ---------------------------------------------------

d7 <-
  d6 %>%
  dplyr::group_by(name) %>%
  dplyr::arrange(value2, .by_group = TRUE) %>%
  dplyr::mutate(n = 1:n())


d7 %>%
  ggplot(aes(n, value2)) +
  geom_point() +
  facet_wrap(~name, scales = "free") +
  scale_y_log10()


# d8. keep only the 'pli_listofmetrics', assign them to their PLI group ---------------------------------------

#--does not include scigrow
pli_listofmetrics <-
  readr::read_csv("data-raw/pli_listofmetrics.csv")

d8 <-
  pli_listofmetrics %>%
  dplyr::left_join(d7) %>%
  dplyr::arrange(n)


# d9. keep only certain substances/columns --------------------------------------------

#--this column is a bitch, no filtering by pesticide_type
d8 %>% pull(pesticide_type) %>% unique()

d8 %>%
  group_by(pesticide_type) %>%
  summarise(n = n()) %>%
  arrange(-n)

d9 <-
  d8 %>%
  dplyr::select(id, n, pli_cat, name, name_nice, substance, value.num = value2)

d9 %>%
  filter(pli_cat == "env_fate_load") %>% pull(name) %>% unique()

#--it is kind of big, this might not be the best way to do this
internal_ppdb <- d9




# write it ----------------------------------------------------------------

usethis::use_data(internal_ppdb,
                  #internal_antex,
                  internal_nicenames,
                  internal_mod_bcf,
                  internal_mod_dt50,
                  internal_mod_ecotox,
                  internal = TRUE, overwrite = TRUE)

