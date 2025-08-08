# PPDB internal data cleanup
# created 26 march 2025
# updated 8 aug 2025

library(tidyverse)

Sys.setenv(LANG = "en")
rm(list = ls())

# 1. raw data ----------------------------------------------------------------
#--note they send me an updated version every 3 months
#--do not worry about the warnings

d1a <- system.file("extdata",
                  "PPDB_Aarhus_University_25-08-08.xlsx",
                  package = "PesticideLoadIndex4Dummies")

d1b <- readxl::read_excel(d1a)

d1c <-
  d1b %>%
  janitor::clean_names()

d1 <- d1c


# 2. metrics we requested -------------------------------------------------
#--this is created in 1_pli_listofmetrics code

d2a <-
  read_csv("inst/pkgdata/pli_listofmetrics.csv")

d2b <-
  d2a %>%
  select(metric) %>%
  arrange(metric) %>%
  pull(metric)

d2c <-
  d1 %>%
  select(1:5,
         all_of(d2b))

d2 <- d2c


# 3. make it character and long -------------------------------------------

d3 <-
  d2 %>%
  select(id, substance, pesticide_type, 6:ncol(.)) %>%
  mutate_all(as.character) %>%
  pivot_longer(4:ncol(.))


# 4. numeric --------------------------------------------------------------

suppressWarnings(
  d4 <-
    d3 %>%
    mutate(value_num = as.numeric(value))
)


# 5. fix bioconcentration factor -----------------------------------

d5 <-
  d4 %>%
  mutate(value_num = dplyr::case_when(
    (name == "bioconcentration_factor_bcf_l_kg" & value == "low risk") ~ 0,
    (name == "bioconcentration_factor_bcf_l_kg" & value == "<1.0") ~ 1,
    (name == "bioconcentration_factor_bcf_l_kg" & value == "<4") ~ 4,
    (name == "bioconcentration_factor_bcf_l_kg" & value == "high risk") ~ 5674,
    TRUE ~ value_num)
    )


# 6. add pli category and order -----------------------------------------------------

d6a <-
  d2a %>%
  select(pli_cat = cat,
         name = metric) %>%
  left_join(d5, relationship = "many-to-many")

d6 <-
  d6a %>%
  select(id, pli_cat, name, substance, pesticide_type, value, value_num) %>%
  group_by(name) %>%
  dplyr::arrange(value_num, .by_group = TRUE)

tmp.d6 <-
  d6 %>%
  dplyr::group_by(name) %>%
  dplyr::summarise(
    min = min(value_num, na.rm = T),
    mn = mean(value_num, na.rm = T),
    med = median(value_num, na.rm = T),
    max = max(value_num, na.rm = T)
  ) %>%
  dplyr::distinct()

#--great, everyone has at least one value
tmp.d6


# write internal data -----------------------------------------------------

internal_tidyppdb <- d6

internal_tidyppdb %>%
  write_rds("inst/pkgdata/internal_ppdb.rds")
