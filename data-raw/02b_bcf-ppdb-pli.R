# created 26 march 2025
# replace internal data, just calc pli for every substance
# in trial phase, interal data is still in place and fxn uses it
#--separated by pli_category bc it takes a long time to run each one

library(tidyverse)

Sys.setenv(LANG = "en")
rm(list = ls())

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
res_bcf <-
  ref_bcf %>%
  select(name, xmin, xmax) %>%
  mutate(segment = 1:n())

#--build the df to merge
a1res_holder <- NULL

#--work through each segment segment

#--I have no idea what the warning is, it works so ignore it I guess
for (i in 1:ncol(res_bcf)) {
  tmp.bcf1 <-
    ref_bcf %>%
    slice(i)

  tmp.bcf2 <- tibble(
    x = c(tmp.bcf1$xmin, tmp.bcf1$xmax),
    y = c(tmp.bcf1$ymin, tmp.bcf1$ymax)
  )

  tmp.int <- as.numeric(lm(y ~ x, data = tmp.bcf2)$coefficients[1])
  tmp.slp <- as.numeric(lm(y ~ x, data = tmp.bcf2)$coefficients[2])

  res <- tibble(segment = i,
                int = tmp.int,
                slp = tmp.slp)
  a1res_holder <-
    a1res_holder %>%
    rbind(res)

}

a1 <-
  res_bcf %>%
  left_join(a1res_holder)

tst <- a1 %>%
  filter(segment == 1)

50 * tst$slp + tst$int
d <- d9


# E. calc PLI for every substance -----------------------------------------

#--2,000 substances
e0 <-
  d %>%
  select(substance) %>%
  distinct()

#--work through the indicators one category at a time

#--these require unique interpolations (piecewise)

# e1. BCF -----------------------------------------------------------------

d <- read_rds("data-raw/tidy_ppdb.rds")

tmp.bcf <-
  d %>%
  filter(name == "bioconcentration_factor_bcf_l_kg")

res_bcf <- NULL

for (i in 1:nrow(tmp.bcf)){

  tmp.res_bcf <-
    tmp.bcf %>%
    slice(i) %>%
    left_join(a1) %>%
    filter(value.num > xmin & value.num < xmax) %>%
    mutate(PLI_per_unit = slp*value.num + int,
           proc_nu = i) %>%
    select(proc_nu, id, name, name_nice, pli_cat, substance, value.num, PLI_per_unit)

  res_bcf <- bind_rows(res_bcf, tmp.res_bcf)
}

res_bcf %>%
  write_rds("data-raw/tidy_bcf-ppdb-plis.rds")
