#--July 31 2025 - Using list of metrics from SUPPORT deliverable
#--I just ordered the ones I need, they shoud be correct
#--assign them to 'categories' here
#--need to be consistent about category names
#--envfat
#--ecotox
#--humhea

#--need to double check the column I chose is the numeric column...

#--there are four aggregation levels


rm(list = ls())

library(tidyverse)

#--note you have to build the package after adding a new file for it to appear
#--look at column names
r1 <- system.file("extdata",
                  "PPDB_Aarhus_University_25-08-08.xlsx",
                  package = "PesticideLoadIndex4Dummies")

r2 <-
  readxl::read_excel(r1) %>%
  janitor::clean_names()

r2small <- r2 %>% slice(c(1, 89, 160, 389))

metrics <- colnames(r2)

cheat_sheet <-
  tibble(
  metric = metrics[6:ncol(r2)]
  ) %>%
  mutate(coln = 1:n(),
         coln2 = coln + 5) %>%
  arrange(metric) %>%
  select(-coln)

cheat_sheet

# 1. envfat metrics ---------------------------------------------------------

#--dt50soil
cheat_sheet %>%
  filter(grepl("dt50", metric))
d1a <- metrics[8]

r2small %>%
  select(all_of(d1a))

#--dt50watear
cheat_sheet %>%
  filter(grepl("water_phase", metric))
d1b <- metrics[22]
r2small %>%
  select(all_of(d1b))

#--scigrow NOTE this is a charachter with E values, needs careful fixing ############################
cheat_sheet %>%
  filter(grepl("sci", metric))
d1c <- metrics[142]

r2small %>%
  select(all_of(d1c))

#--koc
cheat_sheet %>%
  filter(grepl("koc", metric))
d1d <- metrics[18]

r2small %>%
  select(all_of(d1d))


#--bcf
cheat_sheet %>%
  filter(grepl("bcf", metric))
d1e <- metrics[24]

r2small %>%
  select(all_of(d1e))

d1 <-
  tibble(
    cat = "envfat",
    metric = c(d1a, d1b, d1c, d1d, d1e)
  ) %>%
  mutate(ag1 = case_when(
    metric == d1a ~ "persistance",
    metric == d1b ~ "persistance",
    metric == d1c ~ "leachability",
    metric == d1d ~ "mobility",
    metric == d1e ~ "bio concentration",
    TRUE ~ "xxx"
  )) %>%
  mutate(ag2 = "fate",
         ag3 = "fate",
         ag4 = "fate")

d1


# 2. humhea metrics ---------------------------------------------------------

#--aoel
cheat_sheet %>%
  filter(grepl("acute", metric)) %>%
  filter(grepl("op", metric))
d2a <- metrics[48]

r2small %>%
  select(all_of(d2a))

r2 %>%
  select(all_of(d2a))

#--arfd --############### has weird full sentences like 'None allocated"
cheat_sheet %>%
  filter(grepl("acute", metric)) %>%
  filter(grepl("dose", metric))
d2b <- metrics[44]

r2small %>%
  select(all_of(d2b)) %>%
  rename("h" = 1) %>%
  mutate(new = as.numeric(h))

#--adi
cheat_sheet %>%
  filter(grepl("daily", metric))
d2c <- metrics[42]


r2small %>%
  select(all_of(d2c)) %>%
  rename("h" = 1) %>%
  mutate(new = as.numeric(h))

#--mac
cheat_sheet %>%
  filter(grepl("drink", metric))
d2d <- metrics[50]

r2 %>%
  select(all_of(d2d)) %>%
  rename("h" = 1) %>%
  mutate(new = as.numeric(h))

#--ld50_mam_oral
cheat_sheet %>%
  filter(grepl("mam", metric)) %>%
  filter(grepl("ora", metric))

d2e <- metrics[26]


r2small %>%
  select(contains(d2e))

r2small %>%
  select(all_of(d2e)) %>%
  rename("h" = 1) %>%
  mutate(new = as.numeric(h))


#--noeal_mam
cheat_sheet %>%
  filter(grepl("diet", metric))

r2 %>%
  select(contains("diet"))

d2f <- metrics[35]

r2small %>%
  select(all_of(d2f))


d2 <-
  tibble(
    cat = "humhea",
    metric = c(d2a, d2b, d2c, d2d, d2e, d2f)
  ) %>%
  mutate(ag1 = case_when(
    metric == d2a ~ "operators",
    metric == d2b ~ "operators",
    metric == d2c ~ "consumers",
    metric == d2d ~ "consumers",
    metric == d2e ~ "mammals",
    metric == d2f ~ "mammals",
    TRUE ~ "xxx"
  )) %>%
  mutate(ag2 = "human health",
         ag3 = "human health",
         ag4 = "human health")

d2

# 3. ecotox metrics ---------------------------------------------------------

#----group 1

#--lc50_fish - 55
cheat_sheet %>%
  filter(grepl("lc50", metric)) %>%
  filter(grepl("fish", metric))
d3a <- metrics[113]

# lc50_aqu_invert
cheat_sheet %>%
  filter(grepl("inv", metric))
d3b <- metrics[119]

# ld50_sedimentorg
cheat_sheet %>%
  filter(grepl("sed", metric)) %>%
  filter(grepl("acute", metric))
d3c <- metrics[126]

#--group 2

# lc50_algae
cheat_sheet %>%
  filter(grepl("alg", metric))
d3d <- metrics[140]

# ec50_lemna
cheat_sheet %>%
  filter(grepl("free", metric)) %>%
  filter(grepl("aqu", metric))
d3e <- metrics[133]

#--group 3

#--noec_fische
cheat_sheet %>%
  filter(grepl("fish", metric)) %>%
  filter(grepl("chr", metric))
d3f <- metrics[116]

#--noec_daphnia
cheat_sheet %>%
  filter(grepl("noec", metric)) %>%
  filter(grepl("inv", metric))
d3g <- metrics[123]

#--noec_sedimentorg
cheat_sheet %>%
  filter(grepl("sed", metric)) %>%
  filter(grepl("noec", metric))
d3h <- metrics[129]

#--group 4

#--lc50_earthworm
cheat_sheet %>%
  filter(grepl("earth", metric)) %>%
  filter(grepl("acute", metric))
d3i <- metrics[59]

#--noec_earthworm
cheat_sheet %>%
  filter(grepl("earth", metric)) %>%
  filter(grepl("noec", metric))
d3j <- metrics[62]

#--noec_other_soilorg ----check, 2 columns
cheat_sheet %>%
  filter(grepl("lc50", metric)) %>%
  filter(grepl("coll", metric))

tmp <-
  r2 %>%
  select(contains("coll"))

d3k <- metrics[84]

#--group 5

#--ld50_birds
cheat_sheet %>%
  filter(grepl("bir", metric)) %>%
  filter(grepl("acute", metric))
d3l <- metrics[52]

#--noel_birds
cheat_sheet %>%
  filter(grepl("bir", metric)) %>%
  filter(grepl("chr", metric))
d3m <- metrics[56]

#--group 6

#--ld50_mam_oral
cheat_sheet %>%
  filter(grepl("mam", metric)) %>%
  filter(grepl("acute", metric))
d3n <- metrics[26]

#--noeal_mam
cheat_sheet %>%
  filter(grepl("mam", metric)) %>%
  filter(grepl("chr", metric))
d3o <- metrics[40]

#--group 7

#--ld50_bee_cont
cheat_sheet %>%
  filter(grepl("honey", metric)) %>%
  filter(grepl("acute", metric))
d3p <- metrics[65]

#--ld50_bee_ora
cheat_sheet %>%
filter(grepl("honey", metric)) %>%
  filter(grepl("oral", metric))
d3q <- metrics[68]

#--ld50_bumble_cont
cheat_sheet %>%
  filter(grepl("bumb", metric)) %>%
  filter(grepl("cont", metric))
d3r <- metrics[71]

#--ld50_bumble_ora
cheat_sheet %>%
  filter(grepl("bumb", metric)) %>%
  filter(grepl("ora", metric))
d3s <- metrics[75]

#--ld50_mason_cont
cheat_sheet %>%
  filter(grepl("mas", metric)) %>%
  filter(grepl("cont", metric))
d3t <- metrics[79]

#--ld50_mason_ora
cheat_sheet %>%
  filter(grepl("mas", metric)) %>%
  filter(grepl("ora", metric))
d3u <- metrics[82]

#--group 8

#--lr50_pyri
cheat_sheet %>%
  filter(grepl("pred", metric)) %>%
  filter(grepl("mite", metric))
d3v <- metrics[109]

#--lr50_rhopa
cheat_sheet %>%
  filter(grepl("para", metric)) %>%
  filter(grepl("wasp", metric))
d3w <- metrics[105]

#--lr50_p cupreus
cheat_sheet %>%
  filter(grepl("ground", metric)) %>%
  filter(grepl("beet", metric))
d3x <- metrics[101]

#--lr50 c carnea
cheat_sheet %>%
  filter(grepl("lace", metric)) %>%
  filter(grepl("wing", metric))
d3y <- metrics[97]

#--lr50 c septemp
cheat_sheet %>%
  filter(grepl("lady", metric)) %>%
  filter(grepl("bir", metric))
d3z <- metrics[93]

#--er50 ntp1
cheat_sheet %>%
  filter(grepl("targ", metric)) %>%
  filter(grepl("vig", metric))
d3aa <- metrics[88]

#--er50 ntp2
cheat_sheet %>%
  filter(grepl("targ", metric)) %>%
  filter(grepl("seed", metric))
d3ab <- metrics[91]

#----------combine

d3ac <-
  tibble(
    cat = "ecotox",
    metric = c(d3a, d3b, d3c, d3d, d3e,
               d3f, d3g, d3h, d3i, d3j,
               d3k, d3l, d3m, d3n, d3o,
               d3p, d3q, d3r, d3s, d3t,
               d3u, d3v, d3w, d3x, d3y,
               d3z, d3aa, d3ab)
  )

#-----------add groups
d3ad <-
  d3ac %>%
  mutate(ag1 = case_when(
    #--non plants
    metric %in% c(d3a, d3b, d3c) ~ metric,
    #--algae
    metric %in% c(d3d, d3e) ~ "plants",
    #--non plants
    metric %in% c(d3f, d3g, d3h) ~ metric,
    #--soily
    metric %in% c(d3i, d3j, d3k) ~ metric,
    #--birds
    metric %in% c(d3l, d3m) ~ "birds",
    #--mammals
    metric %in% c(d3n, d3o) ~ "mammals",
    #--pollinators
    metric %in% c(d3p, d3q, d3r, d3s, d3t, d3u) ~ "pollinators",
    #--beneficials
    metric %in% c(d3v, d3w, d3x, d3y, d3z) ~ "beneficials",
    #--plants
    metric %in% c(d3aa, d3ab) ~ "ntplants",

  ))

d3ae <-
  d3ad %>%
  mutate(ag2 = case_when(
  ag1 %in% c(d3a, d3b, d3c, "plants") ~ "aquatic organisms acute",
  ag1 %in% c(d3f, d3g, d3h) ~ "aquatic organisms chronic",
  ag1 %in% c(d3i, d3j, d3k) ~ ag1,
  ag1 %in% c("birds", "mammals") ~ "vertebrates",
  ag1 %in% c("pollinators") ~ "pollinators",
  ag1 %in% c("beneficials") ~ "beneficials",
  ag1 %in% c("ntplants") ~ "ntplants",
  TRUE ~ "xxx"
))


d3af <-
  d3ae %>%
  mutate(ag3 = case_when(
    ag2 %in% c("aquatic organisms acute", "aquatic organisms chronic") ~ "aquatic organisms",
    ag2 %in% c(d3i, d3j, d3k) ~ "soil organisms",
    ag2 %in% c("vertebrates", "pollinators", "beneficials", "ntplants") ~ "terrestrial organisms",
    TRUE ~ "xxx"
  ))

d3ag <-
  d3af %>%
  mutate(ag4 = "ecotoxicity")

d3 <- d3ag

# write data --------------------------------------------------------------

pli_listofmetrics <- d3

usethis::use_data(pli_listofmetrics, overwrite = TRUE)

pli_listofmetrics %>%
  write_csv("inst/pkgdata/pli_listofmetrics.csv")
