#' Scale active ingredient indicator values to be 0-1
#'
#' @param f_dat Active ingredient information, see pli_examdat
#' @returns A dataframe with the indicator, the active ingredient, the original value, and the index value
#' @export

#--using this website as a guide: https://rpubs.com/MarkusLoew/12164

GetLoadIndex <- function(f_dat = pli_exdat){

  #--get the active ing we need values for
  a1 <-
    internal_antex %>%
    dplyr::filter(ai_name != "none") %>%
    dplyr::pull(ai_name)

  d0 <-
    internal_ppdb %>%
    dplyr::mutate(liv = n/max(n)) %>%
    dplyr::filter(substance %in% a1) %>%
    dplyr::select(pli_cat, name, substance, value.num)

  dall <- NULL

  #--work through the indicators one at a time
  inds <- internal_ppdb %>% dplyr::pull(name) %>% unique()

  for(i in 1:length(inds)){

    d.tmp1 <-
      d0 %>%
      dplyr::filter(name == inds[i])

    mod.tmp <- internal_mods[[i]]

    tmp.p <- predict(mod.tmp, d.tmp1)

    d.tmp2 <-
      d.tmp1 %>%
      dplyr::mutate(load_index = tmp.p)

    dall <- dplyr::bind_rows(dall, d.tmp2)

  }

  #--for env tox values, the higher the value the better, so swap it
  dall2 <-
    dall %>%
    dplyr::mutate(load_index2 = ifelse(pli_cat == "env_tox_load", 1-load_index, load_index))

  tmp.d5 <-
    f_dat %>%
    dplyr::mutate(ai_amt_g_ha = ai_con_g_g * prod_amt_g_ha) %>%
    dplyr::select(title, scenario, substance = ai_name, ai_amt_g_ha)

  dall3 <-
    tmp.d5 %>%
    dplyr::left_join(dall2) %>%
    dplyr::mutate(load_index2_ha = load_index2 * ai_amt_g_ha,
                  load_index2 = ifelse(is.na(load_index2), 0, load_index2),
                  load_index2_ha = ifelse(is.na(load_index2_ha), 0, load_index2_ha))


  return(dall3)

}

