# get language for validation
getValidationLanguage <- function(d, trait, id) {
  # reduce dataset to only trait of interest
  d <- d[d$Parameter_ID == trait,]
  # randomly shuffle dataset
  if (str_starts(trait, fixed("GB"))) set.seed(parse_number(trait))
  if (trait == "Endogamous")          set.seed(1) 
  if (trait == "Exogamous")           set.seed(2)  
  if (trait == "HunterGatherer")      set.seed(3) 
  if (trait == "Nomadic")             set.seed(4) 
  d <- d[sample(nrow(d)),]
  # get randomly ordered languages
  languages <-
    d %>%
    filter(Value %in% c("0","1")) %>%
    pull(Language_ID)
  # return language
  ifelse(id == 0, NA, languages[id])
}
