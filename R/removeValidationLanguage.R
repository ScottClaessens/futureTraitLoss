# remove validation language
removeValidationLanguage <- function(values, trait, valLang) {
  # set validation language to missing
  values[
    values$Language_ID == valLang & values$Parameter_ID == trait, 
    "Value"
    ] <- "?"
  return(values)
}
