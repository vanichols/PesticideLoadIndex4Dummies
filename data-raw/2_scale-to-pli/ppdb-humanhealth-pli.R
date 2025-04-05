# created 31 march 2025

library(tidyverse)

Sys.setenv(LANG = "en")
rm(list = ls())


# 1. make reference value -------------------------------------------------

b1 <- system.file("pkgdata",
                  "tidy_ppdb.rds",
                  package = "PesticideLoadIndex4Dummies")

tidyppdb <- readr::read_rds(b1)


d <-
  tidyppdb %>%
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

#--2.5 is the value listed in rainford et al. I should be consistent in the source for the ref value I use
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

res_hh %>%
  ggplot(aes(value.num, PLI_per_unit)) +
  geom_point() +
  scale_y_log10()


# make a figure -----------------------------------------------------------

p1 <-
  tibble(
    value.num = seq(1:1000)) %>%
  mutate(PLI_per_unit = 1 / (value.num/2.5),
         PLI_per_unit = ifelse(PLI_per_unit >1, 1, PLI_per_unit))

p1 %>%
  ggplot(aes(x = value.num, y = PLI_per_unit)) +
  geom_point() +
  geom_point(x = 2.5, y = 1, fill = "purple", size = 4, pch = 22) +
  labs(x = "\nMammalian LD50 value from PPDB\n",
       y = "\nPLI value (0-1)\n",
       caption = "Anchor point (purple square) defined by Rainford et al. 2022 based on God knows what")

ggsave("man/figures/fig_hh-scaling.png", width = 7, height = 6)


# write it ----------------------------------------------------------------

res_hh %>%
  mutate(PLI_per_unit = ifelse(PLI_per_unit>1, 1, PLI_per_unit)) %>%
  write_rds("inst/pkgdata/tidy_ppdb-hh-plis.rds")
