#--created 12 aug 2025

library(tidyverse)

Sys.setenv(LANG = "en")
rm(list = ls())


# 1. data -------------------------------------------------

d1a <- system.file("pkgdata",
                  "internal_ppdb.rds",
                  package = "PesticideLoadIndex4Dummies")

d1b <- readr::read_rds(d1a)

d1 <-
  d1b %>%
  filter(pli_cat == "envfat")


# 2. equations -----------------------------------------------------

source("data-raw/2_calc-pli/util_envfat.R")


# 3. calculate each one separately------------------------------------------------------

d3.list <-
  d1 %>%
  pull(name) %>%
  unique()

#--BCF
d3a <-
  d1 %>%
  filter(name == d3.list[1]) %>%
  rowwise() %>%
  mutate(sc_value = ifelse(is.na(value_num), NA, CalcBCF(value_num)))


#--koc
d3b <-
  d1 %>%
  filter(name == d3.list[2]) %>%
  rowwise() %>%
  mutate(sc_value = ifelse(is.na(value_num), NA, CalcKOC(value_num)))

#--sci - looks really funny
d3c <-
  d1 %>%
  filter(name == d3.list[3]) %>%
  rowwise() %>%
  mutate(sc_value = ifelse(is.na(value_num), NA, CalcSCIGROW(value_num)))

#--dt50soil
d3d <-
  d1 %>%
  filter(name == d3.list[4]) %>%
  rowwise() %>%
  mutate(sc_value = ifelse(is.na(value_num), NA, CalcDT50soil(value_num)))

#--dt50water
d3e <-
  d1 %>%
  filter(name == d3.list[5]) %>%
  rowwise() %>%
  mutate(sc_value = ifelse(is.na(value_num), NA, CalcDT50water(value_num)))

d3 <-
  d3a %>%
  bind_rows(d3b) %>%
  bind_rows(d3c) %>%
  bind_rows(d3d) %>%
  bind_rows(d3e)

# 4. look at it ----------------------------------------------------------------

d3 %>%
  ggplot(aes(value_num, sc_value)) +
  geom_line() +
  facet_wrap(~name, scales = "free")


d3 %>%
  write_rds("inst/pkgdata/tidy_envfat.rds")
