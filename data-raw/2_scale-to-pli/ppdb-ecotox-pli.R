# created 26 march 2025
# updated 31 march 2025, remove mammals_acute_oral_ld50
# updated 6 april 2025, cleaned up

library(tidyverse)
library(ggthemes)

Sys.setenv(LANG = "en")
rm(list = ls())

theme_set(theme_economist_white())

# A. ecotox ref values -----------------------------------------------------------


r1 <- system.file("extdata",
                  "byhand_ecotox-ref-values.xlsx",
                  package = "PesticideLoadIndex4Dummies")

r2 <- readxl::read_excel(r1, skip = 5)


r3 <-
  r2 %>%
  select(1, 3) %>%
  filter(name != "mammals_acute_oral_ld50_mg_kg")

r <- r3


# B. calc PLI for every substance -----------------------------------------

b1 <- system.file("pkgdata",
                  "tidy_ppdb.rds",
                  package = "PesticideLoadIndex4Dummies")

tidyppdb <- readr::read_rds(b1)

metrics <-
  r %>%
  pull(name) %>%
  unique()

#--keep the ecotox values
b2 <-
  tidyppdb %>%
  filter(name %in% metrics)

#--join to reference values by metric name
b3 <-
  b2 %>%
  left_join(r, by = "name", relationship = "many-to-many")

#--calc pli based on ref value
b4 <-
  b3 %>%
  mutate(PLI_per_unit = 1 / (value.num/reference_substance_value))

b <- b4

# visualize it ------------------------------------------------------------

b %>%
  mutate(pli = ifelse(PLI_per_unit > 1, 1, PLI_per_unit)) %>%
  ggplot(aes(pli)) +
  geom_histogram(fill = "green3", color = "green3", show.legend = F) +
  coord_flip() +
  labs(y = "\nNumber of substances with a given PLI value",
       x = "PLI value\n",
       title = "Majority of substances have very low Ecotox PLIs") +
  facet_wrap(~name_nice, scales = "free", labeller = label_wrap_gen(width = 15))


ggsave("man/figures/fig_ecotox-scaling.png", width = 12, height = 10)


# C. clean up -------------------------------------------------------------

c <-
  b %>%
  select(id, name, name_nice, pli_cat, substance, pesticide_type, value.num, PLI_per_unit)

#--there are like 50 substances with values lower than the reference
#--keep the 'jesus that is bad' PLI values, make max value 1 as needed
c %>%
  filter(PLI_per_unit > 1)


# write it ----------------------------------------------------------------

c %>%
  write_rds("inst/pkgdata/tidy_ppdb-ecotox-plis.rds")
