# created 26 march 2025
# replace internal data, just calc pli for every substance
# in trial phase, interal data is still in place and fxn uses it
#--separated by pli_category bc it takes a long time to run each one

library(tidyverse)

Sys.setenv(LANG = "en")
rm(list = ls())

theme_set(theme_economist_white())

# A. DT50 piecewise model -----------------------------------------------------------
#--soil degradation, dt50

#--ref values from rainford et al.
ref_dt50 <-
  readxl::read_excel("data-raw/byhand_dt50-ref-values.xlsx", skip = 5) %>%
  mutate_at(c(2, 3, 4, 5), as.numeric) %>%
  fill(name)

#--place results will be stored
astore_dt50 <-
  ref_dt50 %>%
  select(name, xmin, xmax) %>%
  mutate(segment = 1:n())

#--build the df to merge
a2res_holder <- NULL

#--work througNULL#--work through each segment segment
for (i in 1:ncol(astore_dt50)){

  tmp.dt501 <-
    ref_dt50 %>%
    slice(i)

  tmp.dt502 <- tibble(x = c(tmp.dt501$xmin, tmp.dt501$xmax),
                     y = c(tmp.dt501$ymin, tmp.dt501$ymax))

  tmp.int <- as.numeric(lm(y ~ x, data = tmp.dt502)$coefficients[1])
  tmp.slp <- as.numeric(lm(y ~ x, data = tmp.dt502)$coefficients[2])

  tmp.res <- tibble(
    segment = i,
    int = tmp.int,
    slp = tmp.slp
  )
  a2res_holder <-
    a2res_holder %>%
    rbind(tmp.res)

}

a1 <-
  astore_dt50 %>%
  left_join(a2res_holder)

#--test it
tst <-
  a1 %>%
  filter(segment == 4)

370 * tst$slp + tst$int

tibble(
  value.num = seq(1:5500),
  name = "soil_degradation_dt50_field_days") %>%
  left_join(a1) %>%
  filter(value.num > xmin & value.num < xmax) %>%
  distinct() %>%
  mutate(PLI_per_unit = slp*value.num + int) %>%
  ggplot(aes(x = value.num, y = PLI_per_unit)) +
  geom_point() +
  labs(x = "DT50 value from PPDB",
       y = "PLI value (0-1)",
       caption = "Anchor points defined by Rainford et al. 2022 based on policy-derived definitions")

ggsave("data-raw/fig_dt50-scaling.png", width = 6, height = 5)

# E. calc PLI for every substance -----------------------------------------

d <- read_rds("data-raw/tidy_ppdb.rds")

#--616 substances
tmp.dt50 <-
  d %>%
  filter(name == "soil_degradation_dt50_field_days")

#--start with segment #1

s1 <-
  tmp.dt50 %>%
  filter(value.num <= (a1 %>% filter(segment == 1) %>% pull(xmax))) %>%
  left_join(a1 %>% filter(segment == 1)) %>%
  mutate(PLI_per_unit = slp*value.num + int) %>%
  select(id, name, name_nice, pli_cat, substance, value.num, PLI_per_unit)

s2 <-
  tmp.dt50 %>%
  filter(value.num >= (a1 %>% filter(segment == 2) %>% pull(xmin))) %>%
  filter(value.num < (a1 %>% filter(segment == 2) %>% pull(xmax))) %>%
  left_join(a1 %>% filter(segment == 2)) %>%
  mutate(PLI_per_unit = slp*value.num + int) %>%
  select(id, name, name_nice, pli_cat, substance, value.num, PLI_per_unit)

#--there are some monster dt50 values of like 7000
s3 <-
  tmp.dt50 %>%
  filter(value.num >= (a1 %>% filter(segment == 3) %>% pull(xmin))) %>%
  left_join(a1 %>% filter(segment == 3)) %>%
  mutate(PLI_per_unit = slp*value.num + int,
         PLI_per_unit = ifelse(PLI_per_unit > 1, 1, PLI_per_unit)) %>%
  select(id, name, name_nice, pli_cat, substance, value.num, PLI_per_unit)


#--600, where did the other 300 go?
#--there are like 300 without a value for dt50
res_dt50 <-
  s1 %>%
  bind_rows(s2) %>%
  bind_rows(s3)

hmm <- tmp.dt50 %>% pull(id)

res_dt50

tmp.dt50 %>%
  filter(id %in% setdiff(hmm, res_dt50 %>% pull(id)))

#--check it
res_dt50

res_dt50 %>%
  write_rds("data-raw/tidy_dt50-ppdb-plis.rds")
