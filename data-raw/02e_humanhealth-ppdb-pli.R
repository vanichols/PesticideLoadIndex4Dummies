# created 31 march 2025
# replace internal data, just calc pli for every substance
# in trial phase, interal data is still in place and fxn uses it
# adding mammalian toxicity, surrogate for human health

library(tidyverse)

Sys.setenv(LANG = "en")
rm(list = ls())


# 1. make reference value -------------------------------------------------

d <-
  read_rds("data-raw/tidy_ppdb.rds") %>%
  filter(name == "mammals_acute_oral_ld50_mg_kg")

summary(d$value.num)

d %>%
  filter(value.num < 380) %>%
  ggplot(aes(value.num)) +
  geom_histogram()

#--top 5%, assign cutoff as 22
d %>%
  arrange(value.num) %>% #--just to check
  slice_min(order_by = value.num, prop  = 0.05) %>%
  arrange(-value.num)

top5 <-
  d %>%
  arrange(value.num) %>% #--just to check
  slice_min(order_by = value.num, prop  = 0.05) %>%
  arrange(-value.num) %>%
  slice(1) %>%
  pull(value.num)

#--this is the value listed in rainford et al. I should be consistent in the source for the ref value I use
d2 <-
  d %>%
  mutate(PLI_per_unit = 1 / (value.num/2.5))


res_hh <-
  d2 %>%
  mutate(pli_cat = "human_health_load") %>%
  select(id, name, name_nice, pli_cat, substance, pesticide_type, value.num, PLI_per_unit)


#--check it, yeah the majority are really low...but that is how it is
res_hh %>%
  ggplot(aes(PLI_per_unit)) +
  geom_histogram(bins = 1000) +
  scale_x_continuous(limits = c(0, 1))


# write it ----------------------------------------------------------------

res_hh %>%
  write_rds("data-raw/tidy_hh-ppdb-plis.rds")
