# write json file
writeDataJSON <- function(values, mcc, treesSubset, fileTrees, trait, type, id = NULL) {
  # if type = "validation", id must be set
  if (type == "validation" & is.null(id)) stop("Must set ID for validations.")
  # get values for trait from n = 2383 languages
  values <- 
    values %>% 
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
  filename <- paste0(trait, ifelse(type == "main", "", paste0("_", id)))
  out <- paste0(
    '{"data":"', out, '", ',
    '"treesFile":"', normalizePath("files/trees/subset.trees"), '", ',
    '"logFile":"', getwd(), "/out/", type, "/log/", filename, '.log", ',
    '"treesOutFile":"', getwd(), "/out/", type, "/trees/", filename, '.trees", ',
    '"imputationsFile":"', getwd(), "/out/", type, "/imputations/", filename, '.trees"',
    '}')
  # write data to json file
  write(out, file = paste0("files/json/", type, "/", filename, ".json"))
  return(out)
}
