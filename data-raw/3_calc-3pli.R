#--combine all the metrics into one db, get three PLI values for each substance
#--created 31 march 2025
#--started cleanup 6 april 2025

rm(list = ls())

library(tidyverse)


# 0. data -----------------------------------------------------------------

#--let's start with a smaller db
#--cyprodinil and glyphosate

a0 <- system.file("pkgdata",
                  "tidy_ppdb-bcf-plis.rds",
                  package = "PesticideLoadIndex4Dummies")

bcf <-
  readr::read_rds(a0) %>%
  mutate(PLI_per_unit = ifelse(PLI_per_unit>1, 1, PLI_per_unit))


b0 <- system.file("pkgdata",
                  "tidy_ppdb-dt50-plis.rds",
                  package = "PesticideLoadIndex4Dummies")

dt50 <-
  readr::read_rds(b0) %>%
  mutate(PLI_per_unit = ifelse(PLI_per_unit>1, 1, PLI_per_unit))


c0 <- system.file("pkgdata",
                  "tidy_ppdb-ecotox-plis.rds",
                  package = "PesticideLoadIndex4Dummies")

et <-
  readr::read_rds(c0) %>%
  mutate(PLI_per_unit = ifelse(PLI_per_unit>1, 1, PLI_per_unit))


d0 <- system.file("pkgdata",
                  "tidy_ppdb-hh-plis.rds",
                  package = "PesticideLoadIndex4Dummies")

hh <-
  readr::read_rds(d0) %>%
  mutate(PLI_per_unit = ifelse(PLI_per_unit>1, 1, PLI_per_unit))



#--we have a fate, hh and tox category

# F. fate metrics ---------------------------------------------------------

#--bcf
bcf %>%
  ggplot(aes(PLI_per_unit)) +
  geom_histogram()

#--dt50
dt50 %>%
  ggplot(aes(PLI_per_unit)) +
  geom_histogram()

#--take average of bcf and dt50 (fates)
res_f <-
  bcf %>%
  bind_rows(dt50) %>%
  group_by(id, pli_cat, substance) %>%
  summarise(PLI_per_unit = mean(PLI_per_unit, na.rm = T))

#--no NAs
res_f %>%
  filter(is.na(PLI_per_unit))

#--that is a weird piling up around certain middle values

res_f %>%
  ggplot(aes(PLI_per_unit)) +
  geom_histogram()


# H. human health ---------------------------------------------------------

#--only have one metric right now, take average if we add more
res_hh <-
  hh %>%
  group_by(id, pli_cat, substance) %>%
  summarise(PLI_per_unit = mean(PLI_per_unit, na.rm = T))

# E. environmental tox ---------------------------------------------------------

#--oofffff
et %>%
  pull(name) %>%
  unique()

#--average over chronic and acute when we have both?

e1 <-
  et %>%
  filter(grepl("invertebrates", name)) %>%
  group_by(id, pli_cat, substance, pesticide_type) %>%
  summarise(PLI_per_unit = mean(PLI_per_unit, na.rm = T)) %>%
  mutate(name = "invertebrates")

e2 <-
  et %>%
  filter(grepl("fish", name)) %>%
  group_by(id, pli_cat, substance, pesticide_type) %>%
  summarise(PLI_per_unit = mean(PLI_per_unit, na.rm = T)) %>%
  mutate(name = "fish")

#--combine
e3 <-
  et %>%
  filter(!grepl("inverte|fish", name)) %>%
  bind_rows(e1) %>%
  bind_rows(e2)

#--average all of them? Take max value?
#--I think it is fair to take the maximum value

res_ecotox <-
  e3 %>%
  group_by(id, pli_cat, substance, pesticide_type) %>%
  summarise(PLI_per_unit = max(PLI_per_unit, na.rm = T))


# G. combine them ---------------------------------------------------------

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
  mutate(env_load2 = ifelse(is.na(env_fate_load), env_mean, env_fate_load),
         hh_load2 = ifelse(is.na(human_health_load), hh_mean, human_health_load),
         eco_load2 = ifelse(is.na(env_tox_load), eco_mean, env_tox_load)) %>%
  select(id, substance,
         env_load = env_fate_load,
         env_load2,
         hh_load = human_health_load,
         hh_load2,
         eco_load = env_tox_load,
         eco_load2)


# look at it --------------------------------------------------------------

#--something is wrong with env fate? less values?
res1 %>%
  pivot_longer(3:5) %>%
  filter(!is.na(value)) %>%
  group_by(name) %>%
  mutate(n = n(),
         name = paste0(name, " (", n, ")")) %>%
  ggplot(aes(value)) +
  geom_histogram(aes(fill = name), show.legend = F, color = "black") +
  labs(y = "PLI") +
  facet_wrap(~name) +
  coord_flip()

ggsave("man/figures/fig_pli-distributions.png", width = 12, height = 8)


res2 %>%
  write_rds("inst/pkgdata/tidy_ppdb-3plis.rds")

pli_ppdb3plis <- res2
usethis::use_data(pli_ppdb3plis, overwrite = TRUE)
