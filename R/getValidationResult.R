# get validation result
getValidationResult <- function(trait, gb, imp, phyDistMat, phySignal) {
  # create function to calculate distance from language to nearest observed tip
  calculateNearestPhyDistance <- function(trait, gb, valLang) {
    # get all observed tips
    observedTips <- 
      gb %>% 
      # for current trait, excluding validation language, and 
      # excluding languages with missing data
      filter(Parameter_ID == trait & Language_ID != valLang & Value != "?") %>%
      # get language IDs
      pull(Language_ID)
    # calculate minimum phylogenetic distance to observed tip
    min(phyDistMat[valLang, observedTips])
  } 
  # get validation languages and relevant data
  valLangs <-
    tibble(id = unique(imp$id)) %>%
    # validation languages only
    filter(id != 0) %>%
    mutate(
      # validation language
      valLang = map(
        id, function(x) getValidationLanguage(gb, trait, id = x)
        ),
      # phylogenetic distance to nearest neighbour
      langDistNearestObs = map(
        valLang, function(x) calculateNearestPhyDistance(trait, gb, valLang = x)
        )
      ) %>%
    unnest(c(valLang, langDistNearestObs))
    # calculate trait level stats
    traitCoverage <- mean(gb$Value[gb$Parameter_ID == trait] != "?")
    obsTraitValues <- 
      gb %>%
      filter(Value != "?" & Parameter_ID == trait) %>%
      pull(Value)
    traitPropObs0 <- mean(obsTraitValues == "0")
    traitPropObs1 <- mean(obsTraitValues == "1")
    # get validation results
    imp %>%
      left_join(valLangs, by = "id") %>%
      filter(Language_ID == valLang) %>%
      transmute(
        trait = Trait,
        id = id,
        N = N,
        lang = valLang,
        trueValue = Value,
        imputedValue = Imputation,
        error = ifelse(trueValue == 0, imputedValue, 1 - imputedValue),
        langDistNearestObs = langDistNearestObs,
        traitCoverage = traitCoverage,
        traitPropObs0 = traitPropObs0,
        traitPropObs1 = traitPropObs1
        ) %>%
      # add phylogenetic signal estimates from 10.1126/sciadv.adg6175
      left_join(phySignal, by = "trait")
}

