#' Scale active ingredient indicator values to be 0-1, multiply by amount applied
#'
#' @param f_dat Active ingredient information, see pli_examdat
#' @returns A dataframe with the indicator, the active ingredient, the original value, and the index value
#' @export

#--using this website as a guide: https://rpubs.com/MarkusLoew/12164
#--I'm struggling with the units of the load, and multiplying it by the amount applied....
#--can use 'devtools::load_all()' to get access to things
#----REPLACED, I JUST CALCULATED THINGS DIRECTLY IN THE DB, NO NEED FOR A PACKAGE NOW


GetLoadIndex <- function(f_dat = pli_exdat){

  #--get the active ing we need values for
  a1 <-
    f_dat %>%
    dplyr::filter(ai_name != "none") %>%
    dplyr::pull(ai_name)

  #--filter the ppdb for only those ais
  d0 <-
    internal_ppdb %>%
    dplyr::mutate(liv = n/max(n)) %>% #--wtf is this for?
    dplyr::filter(substance %in% a1) %>%
    dplyr::select(pli_cat, name, substance, value.num)

  dall <- NULL

  #--work through the indicators one group at a time

  #--these require unique interpolations (not strictly linear)

  #--bcf
  tmp.bcf <-
    internal_mod_bcf %>%
    left_join(d0, relationship = "many-to-many") %>%
    arrange(substance)

  res_bcf <- NULL

  for (i in 1:length(a1)){

    tmp.res_bcf <-
      tmp.bcf %>%
      filter(substance == a1[i]) %>%
      filter(value.num > xmin & value.num < xmax) %>%
      mutate(PLI_per_unit = slp*value.num + int) %>%
      select(name, pli_cat, substance, PLI_per_unit)

    res_bcf <- bind_rows(res_bcf, tmp.res_bcf)
  }

  #--dt50
  tmp.dt50 <-
    internal_mod_dt50 %>%
    left_join(d0, relationship = "many-to-many") %>%
    arrange(substance)

  # i <- 1
  res_dt50 <- NULL

  for (i in 1:length(a1)){

    tmp.res_dt50 <-
      tmp.dt50 %>%
      filter(substance == a1[i]) %>%
      filter(value.num > xmin & value.num < xmax) %>%
      mutate(PLI_per_unit = slp*value.num + int) %>%
      select(name, pli_cat, substance, PLI_per_unit)

    res_dt50 <- bind_rows(res_dt50, tmp.res_dt50)
  }

  res_dt50

  #--ecotox
  tmp.ecotox <-
    internal_mod_ecotox %>%
    left_join(d0, relationship = "many-to-many") %>%
    arrange(substance)

  # i <- 1
  res_ecotox <- NULL

  for (i in 1:length(a1)){

    tmp.res_ecotox <-
      tmp.ecotox %>%
      filter(substance == a1[i]) %>%
      mutate(PLI_per_unit = 1 / (value.num/reference_substance_value)) %>%
      select(name, pli_cat, substance, PLI_per_unit)

    res_ecotox <- bind_rows(res_ecotox, tmp.res_ecotox)
  }

  res_ecotox

  #--take the maximum PLI value observed across categories and assign it
  res<-
    res_bcf %>%
    dplyr::bind_rows(res_dt50) %>%
    dplyr::bind_rows(res_ecotox) %>%
    dplyr::group_by(substance) %>%
    dplyr::summarise(max_PLI_per_unit = max(PLI_per_unit))

  #--multiply by amt applied, can see PLI per substance
  #--NOTE this assumes the ai amount is in grams, and the PLI per unit ai is per unit grams
  tmp.all <-
    f_dat %>%
    dplyr::mutate(ai_amt_g_ha = prodconc_gai_g * prodamt_g_ha) %>%
    dplyr::select(title, scenario, substance = ai_name, ai_amt_g_ha) %>%
      dplyr::left_join(res) %>%
      dplyr::mutate(
        max_PLI_per_unit = ifelse(is.na(max_PLI_per_unit), 0, max_PLI_per_unit),
        PLI = ai_amt_g_ha * max_PLI_per_unit)

  #--sum, grouped by scenario

  # dall3 <-
  #   tmp.all %>%
  #   group_by(title, scenario) %>%
  #   summarise(PLI = sum(PLI))


  return(tmp.all)

}

