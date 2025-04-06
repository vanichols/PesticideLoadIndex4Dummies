#' Example cleaned data from an elicitation
#'
#' A dataset containing the needed information to extract Load Indices
#'
#' @format A data frame with an expanding number of rows and 4 variables:
#' \describe{
#'   \item{title}{The title of the elicitation}
#'   \item{package_title}{Obv}
#'   \item{ai_name}{Active ingredient (ai) name, must match what is in the online PPDB search}
#'   \item{ai_conc_in_prod}{The concentration of the ai in the product used}
#'   \item{ai_conc_units}{Units of concentration (must be g/kg or g/L or mL/kg or mL/L)}
#'   \item{amt_prod_app}{Units of concentration (must be g/kg or g/L or mL/kg or mL/L)}
#'   \item{amt_prod_app_units}{Units of concentration (must be g/kg or g/L or mL/kg or mL/L)}
#' }
"pli_exinput"
