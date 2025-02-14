#' Example cleaned data from an elicitation
#'
#' A dataset containing the needed information to extract Load Indices
#'
#' @format A data frame with an expanding number of rows and 4 variables:
#' \describe{
#'   \item{title}{The title of the elicitation}
#'   \item{scenario}{Must be either CCP (Current Commercial Practice) or IPM (Integrated Pest Management)}
#'   \item{ai_name}{Active ingredient (ai) name, must match what is in the online PPDB search}
#'   \item{ai_con_g_g}{The concentration of the ai in the product used (grams ai per gram of product)}
#'   \item{prod_amt_g_ha}{Amount of product applied in grams per hectare}
#' }
"pli_exdat"

#' Example of calculated indices
#'
#' A dataset containing an example of output from the GetLoadIndex function
#'
#' @format A data frame with an expanding number of rows and 4 variables:
#' \describe{
#'   \item{pli_cat}{Either env_tox_load (environmental toxicity load) or env_fate_load (environmental fate load)}
#'   \item{name}{Name of one of the 14 indicators from the PPDB}
#'   \item{substance}{the active ingredient (ai) being evaluated}
#'   \item{value.num}{the value of the ai in a given indicator, in the units of that indicator}
#'   \item{load_index}{the value scaled (using a logarithmic model) to 0-1}
#'   \item{load_index2}{the load_index, adjusted so low is 'good' and high is 'bad'}
#' }
"pli_exindices"
