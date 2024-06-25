# remove validation language
removeValidationLanguage <- function(d, trait, valLang) {
  # set validation language to missing
  d[d$Language_ID == valLang & d$Parameter_ID == trait, "Value"] <- "?"
  return(d)
}
