# created 26 march 2025
# replace internal data, just calc pli for every substance
# in trial phase, interal data is still in place and fxn uses it
#--separated by pli_category bc it takes a long time to run each one

library(tidyverse)

Sys.setenv(LANG = "en")
rm(list = ls())

# A. DT50 piecewise model -----------------------------------------------------------
#--soil degradation, dt50

#--ref values from rainford et al.
ref_dt50 <-
  readxl::read_excel("data-raw/byhand_dt50-ref-values.xlsx", skip = 5) %>%
  mutate_at(c(2, 3, 4, 5), as.numeric) %>%
  fill(name)

#--place results will be stored
res_dt50 <-
  ref_dt50 %>%
  select(name, xmin, xmax) %>%
  mutate(segment = 1:n())

#--build the df to merge
a2res_holder <- NULL

#--work througNULL#--work through each segment segment
for (i in 1:ncol(res_dt50)){

  tmp.dt501 <-
    ref_dt50 %>%
    slice(i)

  tmp.dt502 <- tibble(x = c(tmp.dt501$xmin, tmp.dt501$xmax),
                     y = c(tmp.dt501$ymin, tmp.dt501$ymax))

  tmp.int <- as.numeric(lm(y ~ x, data = tmp.dt502)$coefficients[1])
  tmp.slp <- as.numeric(lm(y ~ x, data = tmp.dt502)$coefficients[2])

  res <- tibble(
    segment = i,
    int = tmp.int,
    slp = tmp.slp
  )
  a2res_holder <-
    a2res_holder %>%
    rbind(res)

}

a2 <-
  res_dt50 %>%
  left_join(a2res_holder)

#--test it
tst <-
  a2 %>%
  filter(segment == 4)

370 * tst$slp + tst$int

# E. calc PLI for every substance -----------------------------------------

d <- read_rds("data-raw/tidy_ppdb.rds")

#--2,000 substances
e0 <-
  d %>%
  select(substance) %>%
  distinct()

#--dt50 requires unique interpolations (piecewise)

tmp.dt50 <-
  d %>%
  filter(name == "soil_degradation_dt50_field_days")

res_dt50 <- NULL

#--this runs through 600 substances (less data available compared to bcf)

for (i in 1:nrow(tmp.dt50)){

  tmp.res_dt50 <-
    tmp.dt50 %>%
    slice(i) %>%
    left_join(a2) %>%
    filter(value.num > xmin & value.num < xmax) %>%
    mutate(PLI_per_unit = slp*value.num + int,
           proc_nu = i) %>%
    select(proc_nu, id, name, name_nice, pli_cat, substance, value.num, PLI_per_unit)

  res_dt50 <- bind_rows(res_dt50, tmp.res_dt50)
}

#--check it
res_dt50

res_dt50 %>%
  write_rds("data-raw/tidy_dt50-ppdb-plis.rds")
