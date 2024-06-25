# write json file
writeDataJSON <- function(d, mcc, treesSubset, fileTrees, trait, id) {
  # if validation, remove validation language
  if (id != 0) {
    valLang <- getValidationLanguage(d, trait, id)
    d <- removeValidationLanguage(d, trait, valLang)
  }
  # get values for trait from n = 2383 languages
  values <- 
    d %>% 
    filter(Parameter_ID == trait) %>%
    transmute(
      Taxon = Language_ID,
      Value = Value
    )
  # get all n = 6636 languages
  values <-
    tibble(Taxon = mcc$tip.label) %>%
    left_join(values, by = "Taxon") %>%
    mutate(Value = ifelse(is.na(Value), "?", Value))
  # prepare json file
  out <- values$Value
  names(out) <- values$Taxon
  out <- 
    toJSON(out) %>%
    str_remove_all(fixed('\"')) %>%
    str_replace_all(fixed(":"), fixed("=")) %>%
    str_remove(fixed('{')) %>%
    str_remove(fixed('}'))
  filename <- paste0(trait, "_", id)
  out <- paste0(
    '{"data":"', out, '", ',
    '"treesFile":"', getwd(), "/data/subset.trees", '", ',
    '"logFile":"', getwd(), "/temp/log_", filename, '.log", ',
    '"treesOutFile":"', getwd(), "/temp/trees_", filename, '.trees", ',
    '"imputationsFile":"', getwd(), "/temp/imputations_", filename, '.trees"',
    '}')
  # write data to json file
  write(out, file = paste0("temp/", filename, ".json"))
  return(out)
}
