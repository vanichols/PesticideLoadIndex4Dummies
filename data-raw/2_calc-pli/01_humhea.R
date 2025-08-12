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
  filter(pli_cat == "humhea")


# 2. reference values -----------------------------------------------------

d2a <-
  d1 %>%
  filter(pli_cat == "humhea") %>%
  select(name) %>%
  distinct()

d2b <-
  d2a %>%
  mutate(
    value_ref = case_when(
      name == "acceptable_daily_intake_adi_mg_kg_bw_d" ~ 0.0022,
      name == "acute_acceptable_operator_exposure_aaoel_mg_kg_bw_d" ~ 0.00234,
      name == "acute_reference_dose_a_rf_d_mg_kg_bw_d" ~ 0.005,
      name == "drinking_water_mac_ug_l" ~ 0.1,
      name == "mammals_acute_oral_ld50_mg_kg" ~ 63.3,
      name == "mammals_dermal_ld50_mg_kg" ~ 1027.5,#--this isn't right, should be dermal....
      TRUE ~ 9999

    )
  )

d2 <- d2b


# 3. join ref values ------------------------------------------------------

d3a <-
  d1 %>%
  left_join(d2) %>%
  mutate(sc_value = 1/(value_num/value_ref),
         pli = ifelse(sc_value > 1, 1, sc_value))

#--looks like Vera's!
d3a %>%
  ggplot(aes(value_num, sc_value2)) +
  geom_line() +
  scale_x_log10() +
  facet_wrap(~name, scales = "free")

d3 <-
  d3a %>%
  select(-value, -value_ref, -sc_value)

# 4. write it ----------------------------------------------------------------

d3 %>%
  write_rds("inst/pkgdata/tidy_humhea.rds")
