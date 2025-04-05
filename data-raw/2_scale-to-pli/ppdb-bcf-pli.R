# created 26 march 2025
# replace internal data, just calc pli for every substance

library(tidyverse)
library(ggthemes)

Sys.setenv(LANG = "en")
rm(list = ls())

theme_set(theme_economist_white())

# A. interpolation models ------------------------------------------------

#--24 March 2025, starting trying to change these
#--imagine this sort of df:
# metric    xmin    xmax   int   slope
# DT50      0       30       0     0.004

# bcf model -----------------------------------------------------------
#--bioconcentration


# 1. reference values -----------------------------------------------------

r1 <- system.file("extdata",
                  "byhand_bcf-ref-values.xlsx",
                  package = "PesticideLoadIndex4Dummies")

r2 <- readxl::read_excel(r1, skip = 5)


r3 <-
  r2 %>%
  mutate_at(c(2, 3, 4, 5), as.numeric) %>%
  fill(name)

r <- r3


# 2. assign segment -------------------------------------------------------

#--place results will be stored
dat_base <-
  r %>%
  select(name, xmin, xmax) %>%
  mutate(segment = 1:n())

#--build the df to merge
dat_empty <- NULL

#--work through each segment, fitting a new linear interpolation

#--I have no idea what the warning is, it works so ignore it I guess
for (i in 1:ncol(dat_base)) {

  tmp.bcf1 <-
    r %>%
    slice(i)

  tmp.bcf2 <-
    tibble(
    x = c(tmp.bcf1$xmin, tmp.bcf1$xmax),
    y = c(tmp.bcf1$ymin, tmp.bcf1$ymax)
  )

  tmp.int <- as.numeric(lm(y ~ x, data = tmp.bcf2)$coefficients[1])
  tmp.slp <- as.numeric(lm(y ~ x, data = tmp.bcf2)$coefficients[2])

  tmp.res <-
    tibble(segment = i,
                int = tmp.int,
                slp = tmp.slp)

  dat_empty <-
    dat_empty %>%
    rbind(tmp.res)

}

dat_empty

#--need to increase the max bc there are lots of values higher than 5674
#--those will just be changed to a value of 1

f1 <-
  dat_empty %>%
  left_join(dat_base) %>%
  mutate(xmax = ifelse(xmin == 5000, 100000, xmax))

tst <-
  f1 %>%
  filter(segment == 1)

50 * tst$slp + tst$int


# 3. make a visual of the fit--------------------------------------------------------

ref_pts <-
  r %>%
  select(value.num = xmin,
         PLI_per_unit = ymin) %>%
  bind_rows(
    r %>%
      select(value.num = xmax,
             PLI_per_unit = ymax)
  ) %>%
  distinct()


p1 <-
  tibble(
  value.num = seq(1:5674),
  name = "bioconcentration_factor_bcf_l_kg") %>%
  left_join(f1, relationship = "many-to-many") %>%
  filter(value.num > xmin & value.num < xmax) %>%
  distinct() %>%
  mutate(PLI_per_unit = slp*value.num + int)

p1 %>%
  ggplot(aes(x = value.num, y = PLI_per_unit)) +
  geom_point() +
  geom_point(data = ref_pts, aes(x = value.num, y = PLI_per_unit), color = "red", size = 4) +
  scale_x_continuous(breaks = c(0, 1000, 2000, 3000, 4000, 5000, 6000)) +
  labs(x = "\nBCF value from PPDB\n",
       y = "\nPLI value (0-1)\n",
       caption = "Anchor points (red dots) defined by Rainford et al. 2022 based on policy-derived definitions")

ggsave("man/figures/fig_bcf-scaling.png", width = 7, height = 6)


# B. calc PLI for every substance -----------------------------------------

b1 <- system.file("pkgdata",
                  "tidy_ppdb.rds",
                  package = "PesticideLoadIndex4Dummies")

tidyppdb <- readr::read_rds(b1)

#--955 substances
tmp.bcf <-
  tidyppdb %>%
  filter(name == "bioconcentration_factor_bcf_l_kg")

s1 <-
  tmp.bcf %>%
  #--if value is 100 or less
  filter(value.num <= (f1 %>% filter(segment == 1) %>% pull(xmax))) %>%
  left_join(f1 %>% filter(segment == 1)) %>%
  mutate(PLI_per_unit = slp*value.num + int) %>%
  select(id, name, name_nice, pli_cat, substance, value.num, PLI_per_unit)

s2 <-
  tmp.bcf %>%
  #--if it is greater than 100, but less than 5000
  filter(value.num > (f1 %>% filter(segment == 2) %>% pull(xmin))) %>%
  filter(value.num < (f1 %>% filter(segment == 2) %>% pull(xmax))) %>%
  left_join(f1 %>% filter(segment == 2)) %>%
  mutate(PLI_per_unit = slp*value.num + int) %>%
  select(id, name, name_nice, pli_cat, substance, value.num, PLI_per_unit)

#--there are some monster BCF values...
s3 <-
  tmp.bcf %>%
  #--if it is equal to or greater than 5000
  filter(value.num >= (f1 %>% filter(segment == 3) %>% pull(xmin))) %>%
  left_join(f1 %>% filter(segment == 3)) %>%
  mutate(PLI_per_unit = slp*value.num + int,
         #--if it is higher than 1, it is just 1
         PLI_per_unit = ifelse(PLI_per_unit > 1, 1, PLI_per_unit)) %>%
  select(id, name, name_nice, pli_cat, substance, value.num, PLI_per_unit)

#--why do I need this distinct?
res_bcf <-
  s1 %>%
  bind_rows(s2) %>%
  bind_rows(s3) %>%
  distinct()

res_bcf %>%
  distinct()

res_bcf %>%
  ggplot(aes(x = value.num, y = PLI_per_unit)) +
  geom_point() +
  geom_point(data = ref_pts, aes(x = value.num, y = PLI_per_unit), color = "red", size = 4) +
  coord_cartesian(xlim = c(0, 6000)) +
  #scale_x_continuous(breaks = c(0, 1000, 2000, 3000, 4000, 5000, 6000)) +
  labs(x = "\nBCF value from PPDB\n",
       y = "\nPLI value (0-1)\n",
       caption = "Anchor points (red dots) defined by Rainford et al. 2022 based on policy-derived definitions")


# write it ----------------------------------------------------------------

res_bcf %>%
  write_rds("inst/pkgdata/tidy_ppdb-bcf-plis.rds")
