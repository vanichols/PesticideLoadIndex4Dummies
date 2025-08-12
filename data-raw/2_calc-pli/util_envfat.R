# based on ppdb-dt50-pli, created 5 april 2025
# July 31 2025 - using the equations from Vera 250612_PLI_for_Support_second_round (002)
#--includes additional metrics beyond dt50

library(tidyverse)
library(patchwork)


# 1. dt50 soil 1-10 000--------------------------------------------------------------------

CalcDT50soil <- function(x){
  f.res <- min(c(1, 0.114 * log(x + 0.961)))
  return(f.res)
}


# 2. dt50 water 1 - 1 000--------------------------------------------------------------------

CalcDT50water <- function(x){
  f.res <- min(c(1, 0.146 * log(x + 0.948)))
  return(f.res)
}

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


# 4. koc 1 - 4 000 --------------------------------------------------------------------

CalcKOC <- function(x){

  f.res <- max(c(0, -0.128 * log(0.146 * x + 1) + 0.995))
  return(f.res)

}

# 5. bcf 0 - 5 100 --------------------------------------------------------------------

CalcBCF <- function(x){

  if(x < 5050) {
    f.pli <- 0.076 * log(x + 0.997)
  } else {
      f.pli <- 1
  }
  return(f.pli)
}

CalcBCF(54)
CalcBCF(699999)
