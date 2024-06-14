# remove validation language
removeValidationLanguage <- function(gb, trait, valLang) {
  # set validation language to missing
  gb[gb$Language_ID == valLang & gb$Parameter_ID == trait, "Value"] <- "?"
  return(gb)
}
