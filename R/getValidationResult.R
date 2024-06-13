# get validation result
getValidationResult <- function(trait, id, valLang, values, valImp, 
                                phyDistMat, phySignal) {
  # calculate distance from language to nearest observed tip
  observedTips <- 
    values %>% 
    # for current trait, excluding validation language, and 
    # excluding languages with missing data
    filter(Parameter_ID == trait & Language_ID != valLang & Value != "?") %>%
    # get language IDs
    pull(Language_ID)
  # calculate minimum phylogenetic distance to observed tip
  distNearestObs <- min(phyDistMat[valLang, observedTips])
  # get validation results
  tibble(
    trait = trait,
    id = id,
    lang = valLang,
    trueValue = as.numeric(values$Value[values$Language_ID == valLang & 
                                          values$Parameter_ID == trait]),
    imputedValue = valImp$Imputation[valImp$Language_ID == valLang],
    error = ifelse(trueValue == 0, imputedValue, 1 - imputedValue),
    langDistNearestObs = distNearestObs,
    traitCoverage = mean(values$Value[values$Parameter_ID == trait] != "?"),
    traitPropObs0 = mean(pull(filter(values, Value != "?" & 
                                       Parameter_ID == trait), Value) == "0"),
    traitPropObs1 = mean(pull(filter(values, Value != "?" & 
                                       Parameter_ID == trait), Value) == "1")
  ) %>%
    # add phylogenetic signal estimates from 10.1126/sciadv.adg6175
    left_join(phySignal, by = "trait")
}

