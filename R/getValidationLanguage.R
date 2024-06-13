# get language for validation
getValidationLanguage <- function(values, trait, id) {
  # reduce dataset to only trait of interest
  values <- values[values$Parameter_ID == trait,]
  # randomly shuffle dataset
  set.seed(parse_number(trait))
  values <- values[sample(nrow(values)),]
  # get randomly ordered languages
  languages <-
    values %>%
    filter(Value %in% c("0","1")) %>%
    pull(Language_ID)
  # return language
  return(languages[id])
}
