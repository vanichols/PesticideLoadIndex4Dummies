#--uses functions for envfat to make figs
#--for trouble shooting

library(tidyverse)
library(patchwork)

Sys.setenv(LANG = "en")
rm(list = ls())

source("data-raw/2_calc-pli/util_envfat.R")



# 1. data -----------------------------------------------------------------

d1 <-
  tibble(x = seq(0, 10000)) %>%
  rowwise() %>%
  mutate(pl = CalcDT50soil(x))

d2 <-
  tibble(x = seq(0, 1000)) %>%
  rowwise() %>%
  mutate(pl = CalcDT50water(x))

d3 <-
  tibble(x = seq(0, 7)) %>%
  rowwise() %>%
  mutate(pl = CalcSCIGROW(x))

d4 <-
  tibble(x = seq(0, 4000)) %>%
  rowwise() %>%
  mutate(pl = CalcKOC(x))

d5 <-
  tibble(x = seq(0, 5100)) %>%
  rowwise() %>%
  mutate(pl = CalcBCF(x))


# 2. make a visual of the fits--------------------------------------------------------

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


(p1 + p2 + p3)/(p4 + p5) & plot_annotation(title = "Environmental Fate (envfat) metrics")


ggsave("man/figures/fig_fate-metrics.png", width = 7, height = 6)


