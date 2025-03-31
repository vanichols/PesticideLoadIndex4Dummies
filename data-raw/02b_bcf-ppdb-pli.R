# created 26 march 2025
# replace internal data, just calc pli for every substance
# BCF has like 300 missing data points (320), not sure how impactful this will be

library(tidyverse)
library(ggthemes)

Sys.setenv(LANG = "en")
rm(list = ls())
theme_set(theme_economist_white())

# A. interpolation models ------------------------------------------------

#--this is in progress
#--24 March 2025, starting trying to change these
#--imagine this sort of df:
# metric    xmin    xmax   int   slope
# DT50      0       30       0     0.004

# bcf model -----------------------------------------------------------
#--bioconcentration

ref_bcf <-
  readxl::read_excel("data-raw/byhand_bcf-ref-values.xlsx", skip = 5) %>%
  mutate_at(c(2, 3, 4, 5), as.numeric) %>%
  fill(name)

#--place results will be stored
astore_bcf <-
  ref_bcf %>%
  select(name, xmin, xmax) %>%
  mutate(segment = 1:n())

#--build the df to merge
a1res_holder <- NULL

#--work through each segment segment

#--I have no idea what the warning is, it works so ignore it I guess
for (i in 1:ncol(astore_bcf)) {

  tmp.bcf1 <-
    ref_bcf %>%
    slice(i)

  tmp.bcf2 <- tibble(
    x = c(tmp.bcf1$xmin, tmp.bcf1$xmax),
    y = c(tmp.bcf1$ymin, tmp.bcf1$ymax)
  )

  tmp.int <- as.numeric(lm(y ~ x, data = tmp.bcf2)$coefficients[1])
  tmp.slp <- as.numeric(lm(y ~ x, data = tmp.bcf2)$coefficients[2])

  tmp.res <- tibble(segment = i,
                int = tmp.int,
                slp = tmp.slp)
  a1res_holder <-
    a1res_holder %>%
    rbind(tmp.res)

}

#--need to increase the max bc there are lots of values higher than 5674
#--those will just be changed to a value of 1

a1 <-
  astore_bcf %>%
  left_join(a1res_holder) %>%
  mutate(xmax = ifelse(xmin == 5000, 100000, xmax))

tst <- a1 %>%
  filter(segment == 1)

50 * tst$slp + tst$int

tibble(
  value.num = seq(1:5674),
  name = "bioconcentration_factor_bcf_l_kg") %>%
  left_join(a1) %>%
  filter(value.num > xmin & value.num < xmax) %>%
  distinct() %>%
  mutate(PLI_per_unit = slp*value.num + int) %>%
  ggplot(aes(x = value.num, y = PLI_per_unit)) +
  geom_point() +
  labs(x = "BCF value from PPDB",
       y = "PLI value (0-1)",
       caption = "Anchor points defined by Rainford et al. 2022 based on policy-derived definitions")

ggsave("data-raw/fig_bcf-scaling.png", width = 6, height = 5)


# E. calc PLI for every substance -----------------------------------------

#--these require unique interpolations (piecewise)

# e1. BCF -----------------------------------------------------------------

d <- read_rds("data-raw/tidy_ppdb.rds")

#--955 substances
tmp.bcf <-
  d %>%
  filter(name == "bioconcentration_factor_bcf_l_kg")

#--start with segment #1

s1 <-
  tmp.bcf %>%
  filter(value.num <= (a1 %>% filter(segment == 1) %>% pull(xmax))) %>%
  left_join(a1 %>% filter(segment == 1)) %>%
  mutate(PLI_per_unit = slp*value.num + int) %>%
  select(id, name, name_nice, pli_cat, substance, value.num, PLI_per_unit)

s2 <-
  tmp.bcf %>%
  filter(value.num >= (a1 %>% filter(segment == 2) %>% pull(xmin))) %>%
  filter(value.num < (a1 %>% filter(segment == 2) %>% pull(xmax))) %>%
  left_join(a1 %>% filter(segment == 2)) %>%
  mutate(PLI_per_unit = slp*value.num + int) %>%
  select(id, name, name_nice, pli_cat, substance, value.num, PLI_per_unit)

#--there are some monster BCF values...
s3 <-
  tmp.bcf %>%
  filter(value.num >= (a1 %>% filter(segment == 3) %>% pull(xmin))) %>%
  left_join(a1 %>% filter(segment == 3)) %>%
  mutate(PLI_per_unit = slp*value.num + int,
         PLI_per_unit = ifelse(PLI_per_unit > 1, 1, PLI_per_unit)) %>%
  select(id, name, name_nice, pli_cat, substance, value.num, PLI_per_unit)


#--600, where did the other 300 go?
#--there are like 300 without a value for BCF
res_bcf <-
  s1 %>%
  bind_rows(s2) %>%
  bind_rows(s3)

hmm <- tmp.bcf %>% pull(id)

res_bcf

tmp.bcf %>%
  filter(id %in% setdiff(hmm, res_bcf %>% pull(id)))



# write it ----------------------------------------------------------------


res_bcf %>%
  write_rds("data-raw/tidy_bcf-ppdb-plis.rds")
