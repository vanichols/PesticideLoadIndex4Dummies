#--combine all the metrics into one db, get three PLI values for each substance
#--created 31 march 2025, trying to replace the internal thing

rm(list = ls())

library(tidyverse)


# 0. data -----------------------------------------------------------------

#--let's start with a smaller db
#--cyprodinil and glyphosate

d1 <-
  read_rds("data-raw/tidy_bcf-ppdb-plis.rds") %>%
  mutate(PLI_per_unit = ifelse(PLI_per_unit > 1, 1, PLI_per_unit)) #%>%
  #filter(id %in% c(199, 373, 331))

d2 <- read_rds("data-raw/tidy_dt50-ppdb-plis.rds") %>%
  mutate(PLI_per_unit = ifelse(PLI_per_unit > 1, 1, PLI_per_unit)) #%>%
#filter(id %in% c(199, 373, 331))

d3 <- read_rds("data-raw/tidy_ecotox-ppdb-plis.rds") %>%
  mutate(PLI_per_unit = ifelse(PLI_per_unit > 1, 1, PLI_per_unit)) #%>%
  #filter(id %in% c(199, 373, 331, 101))

d4 <- read_rds("data-raw/tidy_hh-ppdb-plis.rds") %>%
  mutate(PLI_per_unit = ifelse(PLI_per_unit > 1, 1, PLI_per_unit)) #%>%
  #filter(id %in% c(199, 373, 331))

#--we have a fate, hh and tox category

# A. fate metrics ---------------------------------------------------------


#--take average of bcf and dt50 (fates)
res_f <-
  d1 %>%
  bind_rows(d2) %>%
  group_by(id, pli_cat, substance) %>%
  summarise(PLI_per_unit = mean(PLI_per_unit, na.rm = T))


# B. human health ---------------------------------------------------------

#--only have one metric right now, take average if we add more
res_hh <-
  d4 %>%
  group_by(id, pli_cat, substance) %>%
  summarise(PLI_per_unit = mean(PLI_per_unit, na.rm = T))

# C. environmental tox ---------------------------------------------------------

#--oofffff
d3 %>%
  pull(name) %>%
  unique()

#--average over chronic and acute when we brought in that data

c1 <-
  d3 %>%
  filter(grepl("invertebrates", name)) %>%
  group_by(id, pli_cat, substance, pesticide_type) %>%
  summarise(PLI_per_unit = mean(PLI_per_unit, na.rm = T)) %>%
  mutate(name = "invertebrates")

c2 <-
  d3 %>%
  filter(grepl("fish", name)) %>%
  group_by(id, pli_cat, substance, pesticide_type) %>%
  summarise(PLI_per_unit = mean(PLI_per_unit, na.rm = T)) %>%
  mutate(name = "fish")

#--combine
c3 <-
  d3 %>%
  filter(!grepl("inverte|fish", name)) %>%
  bind_rows(c1) %>%
  bind_rows(c2)

#--average all of them? Take max value?
#--I think it is fair to take the maximum value

res_ecotox <-
  c3 %>%
  group_by(id, pli_cat, substance, pesticide_type) %>%
  summarise(PLI_per_unit = max(PLI_per_unit, na.rm = T))



# D. combine them ---------------------------------------------------------

res1 <-
  res_f %>%
  bind_rows(res_hh) %>%
  bind_rows(res_ecotox) %>%
  select(id, substance, pli_cat, PLI_per_unit) %>%
  pivot_wider(names_from = pli_cat, values_from = PLI_per_unit)

res2 <-
  res1 %>%
  ungroup() %>%
  mutate(env_mean = mean(env_fate_load, na.rm = T),
         hh_mean = mean(human_health_load, na.rm = T),
         eco_mean = mean(env_tox_load, na.rm = T)) %>%
  mutate(env_load = ifelse(is.na(env_fate_load), env_mean, env_fate_load),
         hh_load = ifelse(is.na(human_health_load), hh_mean, human_health_load),
         eco_load = ifelse(is.na(env_tox_load), eco_mean, env_tox_load)) %>%
  select(id, substance, env_load, hh_load, eco_load)


res2 %>%
  write_rds("data-raw/tidy_ppdb-all-3plis.rds")

