# calculate area under the curve
calculateAUC <- function(valResults) {
  valResults %>%
    group_by(trait) %>%
    summarise(
      # get area under curve for each trait
      auc = as.numeric(roc(response = trueValue, predictor = imputedValue)$auc),
      # return trait-level variables
      traitCoverage = mean(traitCoverage),
      traitPropObs0 = mean(traitPropObs0),
      traitPropObs1 = mean(traitPropObs1),
      traitPhySignal = mean(traitPhySignal)
    )
}
