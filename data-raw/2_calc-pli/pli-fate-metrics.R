# based on ppdb-dt50-pli, created 5 april 2025
# July 31 2025 - using the equations from Vera 250612_PLI_for_Support_second_round (002)
#--includes additional metrics beyond dt50

library(tidyverse)
library(patchwork)

Sys.setenv(LANG = "en")
rm(list = ls())


# 1. dt50 soil 1-10 000--------------------------------------------------------------------

CalcDT50soil <- function(x){
  f.res <- min(c(1, 0.114 * log(x + 0.961)))
  return(f.res)
}

d1 <-
  tibble(x = seq(0, 10000)) %>%
  rowwise() %>%
  mutate(pl = CalcDT50soil(x))



# 2. dt50 water 1 - 1 000--------------------------------------------------------------------

CalcDT50water <- function(x){
  f.res <- min(c(1, 0.146 * log(x + 0.948)))
  return(f.res)
}

d2 <-
  tibble(x = seq(0, 1000)) %>%
  rowwise() %>%
  mutate(pl = CalcDT50water(x))

# 3. sci-grow 1 - 7--------------------------------------------------------------------

CalcSCIGROW <- function(x){
  if(x < 0.1) {
    f.pli <- 3.3*x
  } else {
    if (x < 1) {
      f.pli <- 0.367 * x +0.293
    } else {
      f.pli <- min(c(1, 0.054 * x + 0.606))
    }
  }
return(f.pli)

}

d3 <-
  tibble(x = seq(0, 7)) %>%
  rowwise() %>%
  mutate(pl = CalcSCIGROW(x))

# 4. koc 1 - 4 000 --------------------------------------------------------------------

CalcKOC <- function(x){

  f.res <- max(c(0, -0.128 * log(0.146 * x + 1) + 0.995))
  return(f.res)

}

d4 <-
  tibble(x = seq(0, 4000)) %>%
  rowwise() %>%
  mutate(pl = CalcKOC(x))

# 5. bcf 0 - 5 100 --------------------------------------------------------------------

CalcBCF <- function(x){

  if(x < 5050) {
    f.pli <- 0.076 * log(x + 0.997)
  } else {
      f.pli <- 1
  }
  return(f.pli)
}

d5 <-
  tibble(x = seq(0, 5100)) %>%
  rowwise() %>%
  mutate(pl = CalcBCF(x))


# 6. make a visual of the fits--------------------------------------------------------

MakeFig <- function(x){
  x %>%
    ggplot(aes(x, pl)) +
    geom_line(size = 3, color = "orange3")
}


p1 <-
  MakeFig(d1) +
  labs(title = "DT50soil")

p2 <-
  MakeFig(d2) +
  labs(title = "DT50water")

p3 <-
  MakeFig(d3) +
  labs(title = "SCI-GROW")

p4 <-
  MakeFig(d4) +
  labs(title = "KOC")

p5 <-
  MakeFig(d4) +
  labs(title = "BCF")


(p1 + p2 + p3)/(p4 + p5)


ggsave("man/figures/fig_fate-metrics.png", width = 7, height = 6)



# 7. calculate fate pli for every substance -------------------------------

b1 <- system.file("pkgdata",
                  "tidy_ppdb.rds",
                  package = "PesticideLoadIndex4Dummies")

tidyppdb <- readr::read_rds(b1)

#--616 substances
tmp.dt50 <-
  tidyppdb  %>%
  filter(name %in%  "soil_degradation_dt50_field_days")

s1 <-
  tmp.dt50 %>%
  # 30 or less
  filter(value.num <= (f1 %>% filter(segment == 1) %>% pull(xmax))) %>%
  left_join(f1 %>% filter(segment == 1)) %>%
  mutate(PLI_per_unit = slp*value.num + int) %>%
  select(id, name, name_nice, pli_cat, substance, value.num, PLI_per_unit)

s2 <-
  tmp.dt50 %>%
  #over 30 to 100
  filter(value.num > (f1 %>% filter(segment == 2) %>% pull(xmin))) %>%
  filter(value.num <= (f1 %>% filter(segment == 2) %>% pull(xmax))) %>%
  left_join(f1 %>% filter(segment == 2)) %>%
  mutate(PLI_per_unit = slp*value.num + int) %>%
  select(id, name, name_nice, pli_cat, substance, value.num, PLI_per_unit)

#--there are some monster dt50 values of like 7000
s3 <-
  tmp.dt50 %>%
  #-over 100, to 365
  filter(value.num > (f1 %>% filter(segment == 3) %>% pull(xmin))) %>%
  filter(value.num <= (f1 %>% filter(segment == 3) %>% pull(xmax))) %>%
  left_join(f1 %>% filter(segment == 3)) %>%
  mutate(PLI_per_unit = slp*value.num + int) %>%
  select(id, name, name_nice, pli_cat, substance, value.num, PLI_per_unit)

s4 <-
  tmp.dt50 %>%
  #- over 365
  filter(value.num > (f1 %>% filter(segment == 4) %>% pull(xmin))) %>%
  left_join(f1 %>% filter(segment == 4)) %>%
  mutate(PLI_per_unit = slp*value.num + int,
         PLI_per_unit = ifelse(PLI_per_unit > 1, 1, PLI_per_unit)) %>%
  select(id, name, name_nice, pli_cat, substance, value.num, PLI_per_unit)


#--there are like 300 without a value for dt50?
res_dt50 <-
  s1 %>%
  bind_rows(s2) %>%
  bind_rows(s3) %>%
  bind_rows(s4)

res_dt50

res_dt50 %>%
  ggplot(aes(x = value.num, y = PLI_per_unit)) +
  geom_point() +
  geom_point(data = ref_pts, aes(x = value.num, y = PLI_per_unit), color = "red", size = 4) +
  coord_cartesian(xlim = c(0, 6000))
  #scale_x_continuous(breaks = c(0, 1000, 2000, 3000, 4000, 5000, 6000))

# write it ----------------------------------------------------------------

res_dt50 %>%
  write_rds("inst/pkgdata/tidy_ppdb-dt50-plis.rds")
