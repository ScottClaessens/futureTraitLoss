# fit model to validation results: proportion of observed traits
fitValidationModel2 <- function(valResults) {
  valResults %>%
    mutate(
      # we took 90 posterior samples
      # error = how many posterior samples were incorrect?
      error = as.integer(error * 90),
      trueValue = factor(trueValue)
    ) %>%
    # fit model
    brm(
      data = .,
      formula = error | trials(90) ~ 0 + trueValue + 
        trueValue:traitPropObs1 + (1 | trait),
      family = binomial,
      prior = c(prior(normal(0, 1), class = b),
                prior(exponential(1), class = sd)),
      iter = 4000,
      warmup = 2000,
      control = list(adapt_delta = 0.99),
      seed = 1
    )
}
