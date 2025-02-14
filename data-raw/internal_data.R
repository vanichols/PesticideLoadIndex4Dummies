# created 14 feb 2025
# purpose: get just the values we need and in a better format

library(tidyverse)

Sys.setenv(LANG = "en")
rm(list = ls())


# A. example input data ---------------------------------------------------

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

internal_antex <- a1


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
  dplyr::arrange(value2, .by_group = TRUE) %>%

  #--see what the lowest value that is NOT zero is, I think it is fine
  #--this is so we can take the logarithm to fit for scaling
  dplyr::filter(!value2 == 0)

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



# d6. take the second lowest value if it is 0 (98 substances impacted-----------------------------------------

#--find lowest value that is not 0
d6.tmp <-
  d5 %>%
  dplyr::filter(value2 != 0) %>%
  dplyr::group_by(name) %>%
  dplyr::summarise(value.tmp = min(value2))

d6 <-
  d5 %>%
  dplyr::left_join(d6.tmp) %>%
  dplyr::mutate(value4 = case_when(
    value2 == 0 ~ value.tmp,
    TRUE ~ value2
  ))

d6.tmp2 <-
  d6 %>%
  dplyr::filter(value2 != value4)


# d7. order within names ---------------------------------------------------

d7 <-
  d6 %>%
  dplyr::group_by(name) %>%
  dplyr::arrange(value4, .by_group = TRUE) %>%
  dplyr::mutate(n = 1:n())


d7 %>%
  ggplot(aes(n, value4)) +
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
  ggplot(aes(n, value4)) +
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
    TRUE ~ "DKDC"
  ))


d8 <-
  d8a %>%
  dplyr::filter(pli_cat != "DKDC")


# d9. combine pli cats and data --------------------------------------------

d9 <-
  d8 %>%
  dplyr::left_join(d7) %>%
  dplyr::select(-value2, -value.tmp, value.num = value4)

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

inds <- internal_ppdb %>% pull(name) %>% unique()

internal_mods <- list()

for(i in 1:length(inds)){

  #--soil degradation
  b0 <-
    internal_ppdb %>%
    mutate(liv = n/max(n),
           name = as.factor(name)) %>%
    filter(name == inds[i])

  my.mod <- nls(liv ~ SSlogis(log(value.num), Asym, mid, scal),
                      data = b0)


  internal_mods[[i]] <- my.mod

}



# write it ----------------------------------------------------------------

usethis::use_data(internal_ppdb,
                  internal_antex,
                  internal_nicenames,
                  internal_mods,
                  internal = TRUE, overwrite = TRUE)

