#' Scale active ingredient indicator values to be 0-1
#'
#' @param f_dat Active ingredient information, see pli_examdat
#' @returns A dataframe with the indicator, the active ingredient, the original value, and the index value
#' @export

##--IN THE PROCESS OF BEING WRITTEN

VisualizeLoads <- function(f_dat = pli_exindices){

  f_dat %>%
    left_join(internal_nicenames) %>%
    ggplot2::ggplot(aes(substance, load_index2_ha)) +
    geom_jitter(aes(color = name_nice), size = 4, width = 0.25) +
    facet_grid(.~scenario, scales = "free_x")

  f_dat %>%
    left_join(internal_nicenames) %>%
    ggplot2::ggplot(aes(substance, load_index2)) +
    geom_jitter(aes(color = name_nice, shape = pli_cat), width = 0.25, size = 4) +
    facet_grid(pli_cat~scenario, scales = "free_x")

  # #--get the active ing we need values for
  # a1 <-
  #   internal_antex %>%
  #   dplyr::filter(ai_name != "none") %>%
  #   dplyr::pull(ai_name)
  #
  # d0 <-
  #   internal_ppdb %>%
  #   dplyr::mutate(liv = n/max(n)) %>%
  #   dplyr::filter(substance %in% a1) %>%
  #   dplyr::select(name, substance, value.num)
  #
  # dall <- NULL
  #
  # #--work through the indicators one at a time
  # inds <- internal_ppdb %>% dplyr::pull(name) %>% unique()
  #
  # for(i in 1:length(inds)){
  #
  #   d.tmp1 <-
  #     d0 %>%
  #     dplyr::filter(name == inds[i])
  #
  #   mod.tmp <- internal_mods[[i]]
  #
  #   tmp.p <- predict(mod.tmp, d.tmp1)
  #
  #   d.tmp2 <-
  #     d.tmp1 %>%
  #     dplyr::mutate(load_index = tmp.p)
  #
  #   dall <- dplyr::bind_rows(dall, d.tmp2)
  #
  # }

  return(pli_exindices)

}

