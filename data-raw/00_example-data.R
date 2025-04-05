#--Made an example data input by hand
#--has the 'desired' units of things (g/kg or g/L)


rm(list = ls())



# Z. example data  -----------------------------------------------------------


z1 <- system.file("extdata",
                  "ABC_peticides_fairy dust lettuce-EXAMPLE.xlsx",
                  package = "PesticideLoadIndex4Dummies")

z2 <- readxl::read_excel(z1)

#--externally available dataset
pli_exinput <- z2
usethis::use_data(pli_exinput, overwrite = TRUE)

