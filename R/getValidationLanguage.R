# get language for validation
getValidationLanguage <- function(gb, trait, id) {
  # reduce dataset to only trait of interest
  gb <- gb[gb$Parameter_ID == trait,]
  # randomly shuffle dataset
  set.seed(parse_number(trait))
  gb <- gb[sample(nrow(gb)),]
  # get randomly ordered languages
  languages <-
    gb %>%
    filter(Value %in% c("0","1")) %>%
    pull(Language_ID)
  # return language
  ifelse(id == 0, NA, languages[id])
}
