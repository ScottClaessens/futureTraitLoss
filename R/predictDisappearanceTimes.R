predictDisappearanceTimes <- function(endanger, imp) {
  # fit model
  fit <- 
    endanger %>% 
    # calculate probabilities of NOT being sleeping (EGIDS >= 9)
    transmute(
      glottocode, EGIDS,
      probNotSleep_Present = 1 - (`P[Y=6]`    + `P[Y=7]`   ),
      probNotSleep_40      = 1 - (`P[Y=6].40` + `P[Y=7].40`),
      probNotSleep_80      = 1 - (`P[Y=6].80` + `P[Y=7].80`)
    ) %>% 
    # join imputed data
    right_join(imp, by = c("glottocode" = "Language_ID")) %>%
    # drop cases without matches
    drop_na(EGIDS:probNotSleep_80) %>%
    # filter to only non-sleeping languages in the present day
    filter(EGIDS < 6) %>%
    # then, for each trait
    group_by(Trait) %>%
    summarise(
      # get the expected number of non-sleeping languages with trait = 1
      num_2023 = sum(Imputation),
      num_2060 = sum(Imputation * probNotSleep_40),
      num_2100 = sum(Imputation * probNotSleep_80),
      n = n(),
      .groups = "drop"
    ) %>%
    # pivot longer
    pivot_longer(
      cols = starts_with("num_"),
      names_prefix = "num_",
      names_to = "year",
      values_to = "num"
    ) %>%
    mutate(
      present = as.integer(round(num)),
      year = (as.numeric(year) - 2023) / 100
    ) %>%
    # fit model of loss over time
    brm(
      formula = present | trials(n) ~ 1 + year + (1 + year | Trait),
      data = .,
      family = binomial,
      prior = c(
        prior(normal(0, 1), class = Intercept),
        prior(normal(0, 1), class = b),
        prior(exponential(1), class = sd)
      )
    )
  # what value does "year" have to be to get log odds of presence
  # down to -9.5 (basically absence)?
  # -9.5 = b0 + b1*year
  # year = (-9.5 - b0) / b1
  hypothesis(
    fit,
    hypothesis = "(((-9.5 - Intercept) / year) * 100) + 2023 = 0",
    scope = "ranef",
    group = "Trait"
  )$hypothesis %>%
    as_tibble() %>%
    dplyr::select(c(Group, Estimate, Est.Error, CI.Lower, CI.Upper)) %>%
    rename(Trait = Group)
}