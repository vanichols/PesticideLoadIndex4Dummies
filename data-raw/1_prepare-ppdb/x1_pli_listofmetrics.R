#--starts by reading in things we have defined reference values for
#--no reference value means not included
#--this will show which metrics are included (maybe they will change)
#--note we aren't including sci grow right now
#--we need some 'human health' component, mammal toxicity for now
#--March 31 2025 - Added mammals_acute_oral_ld50_mg_kg_2
#--July 16 2025 - Using list of metrics from SUPPORT deliverable
#--July 31 2025 - this might need updated...not changing for now, will come back as I identify more variables I need


rm(list = ls())

library(tidyverse)

# A. BCF ref values -------------------------------------------------------

a1 <- system.file("extdata",
                  "byhand_bcf-ref-values.xlsx",
                  package = "PesticideLoadIndex4Dummies")

a2 <- readxl::read_excel(a1, skip = 5)

a3 <-
  a2 %>%
  fill(name) %>%
  select(name) %>%
  distinct()

  #--bioconcentration_factor_bcf_lkg


# B. DT50 refs-----------------------------------------------------------------
#--three soil degradation values, not clear which PLI uses
#--use the field days I guess, although things are sprayed in greenhouses
#--UPDATE: use the lab values


b1 <- system.file("extdata",
                  "byhand_dt50-ref-values.xlsx",
                  package = "PesticideLoadIndex4Dummies")

b2 <- readxl::read_excel(b1, skip = 5)

b3 <-
  b2 %>%
  fill(name) %>%
  select(name) %>%
  distinct()


# C. ecotox refs ----------------------------------------------------------

c1 <- system.file("extdata",
                  "byhand_ecotox-ref-values.xlsx",
                  package = "PesticideLoadIndex4Dummies")

c2 <- readxl::read_excel(c1, skip = 5)

c3 <-
  c2 %>%
  fill(name) %>%
  select(name) %>%
  distinct()


# D. mammalian tox ref ----------------------------------------------------

d <-
  a3 %>%
  bind_rows(b3) %>%
  bind_rows(c3) %>%
  add_row(name = "mammals_acute_oral_ld50_mg_kg")


# E. assign to pli type ------------------------------------------------------

e <-
  d %>%
  dplyr::mutate(pli_cat = dplyr::case_when(
  #--Env Fate
  name == "soil_degradation_dt50_field_days" ~ "env_fate_load",
  name == "bioconcentration_factor_bcf_l_kg" ~ "env_fate_load",
  #name == "sci_grow" ~ "env_fate_load",
  #--Human health
  name == "mammals_acute_oral_ld50_mg_kg" ~ "env_tox_load", #--mamals
  #---Env Tox
  #--short term (acute)
  name == "birds_acute_ld50_mg_kg" ~ "env_tox_load", #--birds
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



# F. make nice labels -----------------------------------------------------

f <-
  e %>%
  dplyr::mutate(name_nice = dplyr::case_when(
    #--Env Fate
    name == "soil_degradation_dt50_field_days" ~ "Soil persistence",
    name == "bioconcentration_factor_bcf_l_kg" ~ "Bioaccumulation",
    #name == "sci_grow" ~ "env_fate_load",
    #--Human health
    name == "mammals_acute_oral_ld50_mg_kg" ~ "Mammals oral, acute", #--worms
    #---Env Tox
    #--short term (acute)
    name == "birds_acute_ld50_mg_kg" ~ "Birds, acute", #--birds
    name == "temperate_freshwater_fish_acute_96hr_lc50_mg_l" ~ "Freshwater fish, acute", #--fish
    name == "temperate_freshwater_aquatic_invertebrates_acute_mg_l" ~ "Freshwater invertebrates, acute", #--daphnia??
    name == "algae_acute_72hr_ec50_growth_mg_l" ~ "Algae, acute", #--algae
    name == "aquatic_plants_acute_7d_ec50_mg_l" ~ "Aquatic plants, acute", #--aquatic pl
    name == "earthworms_acute_14d_lc50_mg_kg" ~ "Earthworms, acute", #--earthworms
    name == "honeybees_contact_acute_ld50_ug_bee" ~ "Honeybees, acute", #--bees?
    #---long term (chronic)
    name == "temperate_freshwater_fish_chronic_21d_noec_mg_l" ~ "Freshwater fish, chronic", #--fish
    name == "temperate_freshwater_aquatic_invertebrates_chronic_mg_l" ~ "Freshwater invertebrates, chronic", #--daphnia
    name == "earthworms_chronic_noec_reproduction_mg_kg" ~ "Earthworms, chronic", #--worms
    TRUE ~ "DKDC" #--don't know don't care
  ))


# write data --------------------------------------------------------------

pli_listofmetrics <- f

usethis::use_data(pli_listofmetrics, overwrite = TRUE)

pli_listofmetrics %>%
  write_csv("inst/pkgdata/pli_listofmetrics.csv")
