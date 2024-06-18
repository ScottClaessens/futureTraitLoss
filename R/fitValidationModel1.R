# fit model to validation results: trait coverage
fitValidationModel1 <- function(valResults) {
  valResults %>%
    # error = how many posterior samples were incorrect?
    mutate(error = as.integer(error * N)) %>%
    # fit model
    brm(
      data = .,
      formula = error | trials(N) ~ 1 + traitCoverage + (1 | trait),
      family = binomial,
      prior = c(prior(normal(0, 1), class = Intercept),
                prior(normal(0, 1), class = b),
                prior(exponential(1), class = sd)),
      iter = 4000,
      warmup = 2000,
      control = list(adapt_delta = 0.99),
      seed = 1
    )
}
