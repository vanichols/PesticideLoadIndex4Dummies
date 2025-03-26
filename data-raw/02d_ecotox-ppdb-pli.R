# created 26 march 2025
# replace internal data, just calc pli for every substance
# in trial phase, interal data is still in place and fxn uses it
#--separated by pli_category bc it takes a long time to run each one

library(tidyverse)

Sys.setenv(LANG = "en")
rm(list = ls())

# A. ecotox ref values -----------------------------------------------------------
#--lots of metrics

#--ref values from rainford et al.
a3 <-
  readxl::read_excel("data-raw/byhand_ecotox-ref-values.xlsx", skip = 5) %>%
  select(1, 3)


# E. calc PLI for every substance -----------------------------------------

d <- read_rds("data-raw/tidy_ppdb.rds")

metrics <-
  a3 %>%
  pull(name) %>%
  unique()

tmp.ecotox <-
  d %>%
  filter(name %in% metrics) %>%
  #filter(substance == "cyfluthrin") %>%
  left_join(a3, relationship = "many-to-many") %>%
  mutate(PLI_per_unit = 1 / (value.num/reference_substance_value))

res_ecotox <-
  tmp.ecotox %>%
  select(id, name, name_nice, pli_cat, substance, pesticide_type, value.num, PLI_per_unit)


#--check it
res_ecotox %>%
  ggplot(aes(PLI_per_unit)) +
  geom_histogram(bins = 1000)

#--there are a LOT of substances with values lower than the reference
#--keep the 'jesus that is bad' PLI values, deal with them as they come up?
res_ecotox %>%
  filter(PLI_per_unit > 1)



# write it ----------------------------------------------------------------

res_ecotox %>%
  write_rds("data-raw/tidy_ecotox-ppdb-plis.rds")
