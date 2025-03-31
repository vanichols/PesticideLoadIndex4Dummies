#--show which metrics are included (maybe they will change)
#--update 31 march 2025, we need some 'human health' component, mammal toxicity?
#--Add mammals_acute_oral_ld50_mg_kg_2

rm(list = ls())

library(tidyverse)


# 1. which metrics -----------------------------------------------------------

a1 <-
  readxl::read_excel("data-raw/byhand_bcf-ref-values.xlsx", skip = 5) %>%
  fill(name) %>%
  select(name) %>%
  distinct()
#--bioconcentration_factor_bcf_lkg

#--three soil degradation values, not clear which PLI uses
#--use the field days I guess, although things are sprayed in greenhouses

a2 <-
  readxl::read_excel("data-raw/byhand_dt50-ref-values.xlsx", skip = 5) %>%
  fill(name) %>%
  select(name) %>%
  distinct()

a3 <-
  readxl::read_excel("data-raw/byhand_ecotox-ref-values.xlsx", skip = 5) %>%
  fill(name) %>%
  select(name) %>%
  distinct()

#--note we aren't including sci grow right now
#--31 march, add mammal toxicity to use a 'human health'

d4 <-
  a1 %>%
  bind_rows(a2) %>%
  bind_rows(a3) %>%
  add_row(name = "mammals_acute_oral_ld50_mg_kg_2")


# 2. assign to group ------------------------------------------------------

#--assign to a group
d5 <-
  d4 %>%
  dplyr::mutate(pli_cat = dplyr::case_when(
  #--Env Fate
  name == "soil_degradation_dt50_field_days" ~ "env_fate_load",
  name == "bioconcentration_factor_bcf_l_kg" ~ "env_fate_load",
  #name == "sci_grow" ~ "env_fate_load",
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
  name == "mammals_acute_oral_ld50_mg_kg_2" ~ "human_health_load", #--worms
  TRUE ~ "DKDC" #--don't know don't care
))



# 3. make nice labels -----------------------------------------------------
d6 <-
  d5 %>%
  dplyr::mutate(name_nice = dplyr::case_when(
    #--Env Fate
    name == "soil_degradation_dt50_field_days" ~ "Soil persistence",
    name == "bioconcentration_factor_bcf_l_kg" ~ "Bioaccumulation",
    #name == "sci_grow" ~ "env_fate_load",
    #---Env Tox
    #--short term (acute)
    name == "birds_acute_ld50_mg_kg" ~ "Birds, acute", #--birds
    name == "mammals_acute_oral_ld50_mg_kg" ~ "Mammals, acute", #--mamals
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
    name == "mammals_acute_oral_ld50_mg_kg_2" ~ "Mammals oral, acute", #--worms
    TRUE ~ "DKDC" #--don't know don't care
  ))


# write data --------------------------------------------------------------

pli_listofmetrics <- d6

usethis::use_data(pli_listofmetrics, overwrite = TRUE)

pli_listofmetrics %>% write_csv("data-raw/pli_listofmetrics.csv")
