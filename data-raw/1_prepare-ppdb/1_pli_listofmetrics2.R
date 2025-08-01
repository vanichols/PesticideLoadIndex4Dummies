#--July 31 2025 - Using list of metrics from SUPPORT deliverable
#--Aug 1 2025 - This is on pause bc I can't match the SUPPORT deliverable metrics to the PPDB columns. Emailed Vera.


rm(list = ls())

library(tidyverse)


#--look at column names
r1 <- system.file("extdata",
                  "PPDB_Aarhus_University_25-07-18.xlsx",
                  package = "PesticideLoadIndex4Dummies")

r2 <-
  readxl::read_excel(r1) %>%
  janitor::clean_names()

metrics <- colnames(r2)

cheat_sheet <-
  tibble(
  metric = metrics[6:76]
  ) %>%
  mutate(coln = 1:n(),
         coln2 = coln + 5) %>%
  arrange(metric)

# 1. fate metrics ---------------------------------------------------------

#--dt50soil - 8
cheat_sheet %>%
  filter(grepl("dt50", metric))
d1a <- metrics[8]

#--dt50watear - 22
cheat_sheet %>%
  filter(grepl("water_phase", metric))
d1b <- metrics[22]

#--scigrow - 76
cheat_sheet %>%
  filter(grepl("sci", metric))
d1c <- metrics[76]

#--koc - 18
cheat_sheet %>%
  filter(grepl("koc", metric))
d1d <- metrics[18]

#--bcf - 24
cheat_sheet %>%
  filter(grepl("bcf", metric))
d1e <- metrics[24]


d1 <-
  tibble(
    cat = "fate",
    metric = c(d1a, d1b, d1c, d1d, d1e)
  ) %>%
  mutate(subcat = case_when(
    metric == d1a ~ "persistance",
    metric == d1b ~ "persistance",
    metric == d1c ~ "leachability",
    metric == d1d ~ "mobility",
    metric == d1e ~ "bio concentration",

    TRUE ~ "xxx"
  ))

d1

# 2. ecotox metrics ---------------------------------------------------------

#----group 1

#--lc50_fish - 55
cheat_sheet %>%
  filter(grepl("lc50", metric))
d2a <- metrics[55]

# lc50_aqu_invert (assumed acute)
cheat_sheet %>%
  filter(grepl("inv", metric))
d2b <- metrics[61]

# lc50_sedimentorg ######## no idea what this one is....
# cheat_sheet %>%
#   filter(grepl("lc50", metric))
# d2c <- metrics[61]

# lc50_algae
cheat_sheet %>%
  filter(grepl("alg", metric))
d2b <- metrics[61]

# ec50_lemna


d1a <- metrics[8]

#--dt50watear - 22
d1b <- metrics[22]

#--scigrow - 71
d1c <- metrics[71]

#--koc - 13
d1d <- metrics[13]

#--bcf - 19
d1e <- metrics[19]


d1 <-
  tibble(
    cat = "fate",
    metric = c(d1a, d1b, d1c, d1d, d1e)
  )




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
