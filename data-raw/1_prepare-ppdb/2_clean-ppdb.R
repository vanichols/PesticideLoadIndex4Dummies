# PPDB internal data cleanup
# created 26 march 2025

library(tidyverse)

Sys.setenv(LANG = "en")
rm(list = ls())

# A. raw data ----------------------------------------------------------------
#--note they send me an updated version every 3 months
#--do not worry about the warnings

a1 <- system.file("extdata",
                  "PPDB_Aarhus_University_24-07-15.xlsx",
                  package = "PesticideLoadIndex4Dummies")

a2 <- readxl::read_excel(a1)

a3 <-
  a2 %>%
  janitor::clean_names()

a <- a3

#--we got a lot we didn't specifically ask for, _qb is an indicator of quality

# B. clean it up: 154,602 entries----------------------------------------------------------

#--qb

b <-
  a %>%
  dplyr::mutate_all(as.character) %>%
  tidyr::pivot_longer(6:ncol(.)) %>%
  dplyr::mutate_if(is.character, str_to_lower)


# C. get rid of dupes: 154,592 ---------------------------------
#--weird

c <-
  b %>%
  dplyr::distinct()


# D. eliminate the _qb names, 103,068 ----------------------------------------------

d <-
  c %>%
  dplyr::filter(!grepl("_qb", name))


# E. the _2 and = data, 26,371 ----------------------------------------------------

#--these seem safe to remove
tmp.e <-
  d %>%
  dplyr::filter(value == "=")

tmp.e <-
  d %>%
  dplyr::filter(grepl("_2$", name)) %>%
  dplyr::select(name)

e <-
  d %>%
  dplyr::filter(value != "=") %>%
  dplyr::filter(!grepl("_2$", name))


# F. keep only the 'pli_listofmetrics', 12,821 ---------------------------------------

internal_listofmetrics <- readr::read_csv(system.file("pkgdata",
                                     "pli_listofmetrics.csv",
                                     package = "PesticideLoadIndex4Dummies"))

#--does not include scigrow

f <-
  e %>%
  filter(name %in% internal_listofmetrics$name)


# G. make value numeric ---------------------------------------------------

g1 <-
  f %>%
  dplyr::mutate(value2 = as.numeric(value))

#--look at the NAs in value2
#--it is only the bioconcentration_factor
tmp.g1 <-
  g1 %>%
  filter(is.na(value2))

tmp.g1 %>%
  pull(name) %>%
  unique()

g2 <-
  g1 %>%
  dplyr::mutate(value2 = dplyr::case_when(
    (name == "bioconcentration_factor_bcf_l_kg" & value == "low risk") ~ 0,
    (name == "bioconcentration_factor_bcf_l_kg" & value == "<1.0") ~ 1,
    (name == "bioconcentration_factor_bcf_l_kg" & value == "<4") ~ 4,
    (name == "bioconcentration_factor_bcf_l_kg" & value == "high risk") ~ 5674,
    TRUE ~ value2)
    )

tmp.g2 <-
  g2 %>%
  filter(is.na(value2))


g <- g2

# H. arrange and id within name-----------------------------------------------------------

h <-
  g %>%
  dplyr::group_by(name) %>%
  dplyr::arrange(value2, .by_group = TRUE)


tmp.h <-
  h %>%
  dplyr::group_by(name) %>%
  dplyr::summarise(
    min = min(value2),
    mn = mean(value2),
    med = median(value2),
    max = max(value2)
  ) %>%
  dplyr::distinct()

#--great, everyone has a value
tmp.h


# Z. clean up --------------------------------------------

#--this column is a bitch, no filtering by pesticide_type
h %>% pull(pesticide_type) %>% unique()

h %>%
  group_by(pesticide_type) %>%
  summarise(n = n()) %>%
  arrange(-n)

z <-
  h %>%
  left_join(internal_listofmetrics, relationship = "many-to-many") %>%
  dplyr::select(id, pli_cat, name, name_nice,
                substance, pesticide_type,
                value.num = value2)


# write internal data -----------------------------------------------------

internal_tidyppdb <- z

internal_tidyppdb %>%
  write_rds("inst/pkgdata/internal_ppdb.rds")
