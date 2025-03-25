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

#--how do these minimum values compare to the references values from rainford?
#--for the ecotox ones
ref_eco <- readxl::read_excel("data-raw/byhand_ecotox-ref-values.xlsx", skip = 5)

#--NOTE: the reference values are higher than the minimum values, so I need to fix this in the 'fitting'
ref_eco %>%
  left_join(d5.tmp)

# d6. do nothing, used to overwrite 0s-----------------------------------------

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


# d8. assign them to their PLI group ---------------------------------------

d8 <-
  d7 %>%
  dplyr::select(name) %>%
  dplyr::distinct()


# 8a. environ fate load, lower is better ---------------------------------------------------

#--three soil degradation values, not clear which PLI uses
d8 %>%
  dplyr::filter(grepl("soil", name))

d7 %>%
  filter(grepl("soil", name)) %>%
  ggplot(aes(n, value2)) +
  geom_point(aes(color = name, shape = name), size = 3)

#--use the field days I guess, although things are sprayed in greenhouses

#--bioconcentration factor (BCF)

d8a <-
  d8 %>%
  dplyr::mutate(pli_cat = dplyr::case_when(
    #--Env Fate
    name == "soil_degradation_dt50_field_days" ~ "env_fate_load",
    name == "bioconcentration_factor_bcf_l_kg" ~ "env_fate_load",
    name == "sci_grow" ~ "env_fate_load",
    #---Env Tox
    #--short term (acute)
    name == "birds_acute_ld50_mg_kg" ~ "env_tox_load", #--birds
    name == "mammals_acute_oral_ld50_mg_kg" ~ "env_tox_load", #--mamals
    name == "temperate_freshwater_fish_acute_96hr_lc50_mg_l" ~ "env_tox_load", #--fish
    name == "temperate_freshwater_aquatic_invertebrates_acute_mg_l" ~ "env_tox_load", #--daphnia??
    name == "algae_acute_72hr_ec50_growth_mg_l" ~ "env_tox_load", #--algae
    name == "aquatic_plants_acute_7d_ec50_mg_l" ~ "env_tox_load", #--aquatic pl
    name == "earthworms_acute_14d_lc50_mg_kg" ~ "env_tox_load", #--earthworms
    name == "honeybees_contact_acute_ld50_ug_bee" ~ "env_tox_load", #--bees?
    #---long term (chronic)
    name == "temperate_freshwater_fish_chronic_21d_noec_mg_l" ~ "env_tox_load", #--fish
    name == "temperate_freshwater_aquatic_invertebrates_chronic_mg_l" ~ "env_tox_load", #--daphnia
    name == "earthworms_chronic_noec_reproduction_mg_kg" ~ "env_tox_load", #--worms
    TRUE ~ "DKDC" #--don't know don't care
  ))


d8 <-
  d8a %>%
  dplyr::filter(pli_cat != "DKDC")


# d9. combine pli cats and data --------------------------------------------

d9 <-
  d8 %>%
  dplyr::left_join(d7) %>%
  dplyr::select(n, pli_cat, name, substance, value.num = value2)

#--it is kind of big, this might not be the best way to do this
internal_ppdb <- d9

# C. make nice labels for indices -----------------------------------------

#--make sure it is in the right order
internal_ppdb %>%
  dplyr::select(name) %>%
  dplyr::distinct()

c1 <-
  tibble(
  name_nice = c(
  "Algae, acute",
  "Aquatic plants, acute",
  "Bioaccumulation",
  "Birds, acute",
  "Earthworms, acute",
  "Earthworms, chronic",
  "Honeybees, acute",
  "Mammals, acute",
  "Soil binding",
  "Soil stability",
  "Freshwater invertebrates, acute",
  "Freshwater invertebrates, chronic",
  "Freshwater fish, acute",
  "Freshwater fish, chronic"))

internal_nicenames <-
  internal_ppdb %>%
  dplyr::select(name) %>%
  dplyr::distinct() %>%
  bind_cols(c1)

# B. fit and save models ------------------------------------------------

#--this needs to change, needs to use reference values
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

#--work througNULL#--work through each segment segment
for (i in 1:ncol(res_bcf)){

  tmp.bcf1 <-
    ref_bcf %>%
    slice(i)

  tmp.bcf2 <- tibble(x = c(tmp.bcf1$xmin, tmp.bcf1$xmax),
                     y = c(tmp.bcf1$ymin, tmp.bcf1$ymax))

  tmp.int <- as.numeric(lm(y ~ x, data = tmp.bcf2)$coefficients[1])
  tmp.slp <- as.numeric(lm(y ~ x, data = tmp.bcf2)$coefficients[2])

  res <- tibble(
    segment = i,
    int = tmp.int,
    slp = tmp.slp
  )
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

# b3. linear for all others --------------------------------------------------

## I need to think about this. Higher is better for these,
#--so higher values than the reference value mean lower PLI

ref_eco <-
  readxl::read_excel("data-raw/byhand_ecotox-ref-values.xlsx", skip = 5) %>%
  mutate(xmin = 0,
         ymin = 0,
         xmax = reference_substance_value,
         ymax = 1) %>%
  select(name, xmin, ymin, xmax, ymax)

#--place results will be stored
res_eco <-
  ref_eco %>%
  select(name, xmin, xmax) %>%
  mutate(segment = 1)

#--build the df to merge
b3res_holder <- NULL

#--work through each metric individually

for (i in 1:ncol(res_eco)){

  tmp.eco1 <-
    ref_eco %>%
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









#################OLD
inds_refs2 <- intersect(inds, ref_eco %>% pull(name) %>% unique())

b3 <-
  ref_eco %>%
  filter(name == inds_refs2[1]) %>%
  select(-reference_substance_name) %>%
  bind_rows(
    internal_ppdb %>%
      filter(name == inds_refs2[1]) %>%
      filter(value.num == max(value.num)) %>%
      select(name, reference_substance_value = value.num) %>%
      mutate(pli_value = 0)
  )

b3
lm(pli_value ~ 1/reference_substance_value, data = b3)

# write it ----------------------------------------------------------------

usethis::use_data(internal_ppdb,
                  #internal_antex,
                  internal_nicenames,
                  internal_mod_bcf,
                  internal_mod_soil,
                  internal = TRUE, overwrite = TRUE)

