# created 26 march 2025
# replace internal data, just calc pli for every substance
# in trial phase, interal data is still in place and fxn uses it
#--separated by pli_category bc it takes a long time to run each one

library(tidyverse)

Sys.setenv(LANG = "en")
rm(list = ls())

# D. PPDB internal data cleanup---------------------------------------------------

# d0. raw data ----------------------------------------------------------------

rdat <-
  readxl::read_excel("data-raw/ppdb/Requested-PPDB-format.xlsx") %>%
  janitor::clean_names()

rdat.names <- names(rdat)

#--note they send me an updated version every 3 months
#--do not worry about the warnings
draw <-
  readxl::read_excel("data-raw/ppdb/PPDB_Aarhus_University_24-07-15.xlsx") %>%
  janitor::clean_names()

draw.names <- names(draw)

#--some we asked for but didn't get? might be a replacement for them
#--for ex we asked for bioconcentration_factor_bcf_lkg
#--it is actually bioconcentration_factor_bcf_l_kg
setdiff(rdat.names, draw.names)

#--a lot we didn't ask for, _qb is an indicator of quality
setdiff(draw.names, rdat.names)


# d1. clean it up: 154,602 entries----------------------------------------------------------

#--qb

d1 <-
  draw %>%
  dplyr::mutate_all(as.character) %>%
  tidyr::pivot_longer(6:ncol(.)) %>%
  dplyr::mutate(value2 = as.numeric(value)) %>%
  dplyr::mutate_if(is.character, str_to_lower)

# d2. get rid of dupes: 154,592 ---------------------------------

d2 <-
  d1 %>%
  dplyr::distinct()


# d3. eliminate the _qb names, 103,068 ----------------------------------------------

d3 <-
  d2 %>%
  dplyr::filter(!grepl("_qb", name))



# d4. the _2 and = data, 26,371 ----------------------------------------------------

#--these seem safe to remove
tmp.d4 <-
  d3 %>%
  dplyr::filter(value == "=")

tmp.d4 <-
  d3 %>%
  dplyr::filter(grepl("_2$", name)) %>%
  dplyr::select(name)

d4 <-
  d3 %>%
  dplyr::filter(value != "=") %>%
  dplyr::filter(!grepl("_2$", name))

# d5. arrange and id within name-----------------------------------------------------------

d5 <-
  d4 %>%
  dplyr::group_by(name) %>%
  dplyr::arrange(value2, .by_group = TRUE)


d5.tmp <-
  d5 %>%
  dplyr::group_by(name) %>%
  dplyr::summarise(
    min = min(value2),
    mn = mean(value2),
    med = median(value2),
    max = max(value2)
  ) %>%
  dplyr::distinct()


#--these all have extreme values, the mean and median are nowhere close to each other
#--log transformation is probably needed, but won't work for 0
d5.tmp2 <-
  d5.tmp %>%
  dplyr::filter(!is.na(min))

#--only two parameters have a value of 0, and we don't actually use these in the danish pli, so don't worry about it
d5.tmp3 <-
  d5.tmp %>%
  dplyr::filter(min==0)

# d6. used to overwrite 0s-----------------------------------------

d6 <- d5

# d7. order within names ---------------------------------------------------

d7 <-
  d6 %>%
  dplyr::group_by(name) %>%
  dplyr::arrange(value2, .by_group = TRUE) %>%
  dplyr::mutate(n = 1:n())


# d7 %>%
#   ggplot(aes(n, value2)) +
#   geom_point() +
#   facet_wrap(~name, scales = "free") +
#   scale_y_log10()


# d8. keep only the 'pli_listofmetrics', assign them to their PLI group ---------------------------------------

#--does not include scigrow
pli_listofmetrics <-
  readr::read_csv("data-raw/pli_listofmetrics.csv")

d8 <-
  pli_listofmetrics %>%
  dplyr::left_join(d7) %>%
  dplyr::arrange(n)


# d9. keep only certain substances/columns --------------------------------------------

#--this column is a bitch, no filtering by pesticide_type
d8 %>% pull(pesticide_type) %>% unique()

d8 %>%
  group_by(pesticide_type) %>%
  summarise(n = n()) %>%
  arrange(-n)

d9 <-
  d8 %>%
  dplyr::select(id, n, pli_cat, name, name_nice,
                substance, pesticide_type,
                value.num = value2)

d9 %>%
  filter(pli_cat == "env_fate_load") %>% pull(name) %>% unique()

#--it is kind of big, this might not be the best way to do this
d <- d9

d %>%
  write_rds("data-raw/tidy_ppdb.rds")
