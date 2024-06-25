# plot proportion in grambank and proportion in imputations
plotProportions <- function(endanger, imp) {
  out <-
    endanger %>% 
    # calculate probabilities of NOT being sleeping (EGIDS >= 9)
    transmute(
      glottocode, EGIDS,
      probNotSleep_Present = 1 - (`P[Y=6]`    + `P[Y=7]`   ),
      probNotSleep_40      = 1 - (`P[Y=6].40` + `P[Y=7].40`),
      probNotSleep_80      = 1 - (`P[Y=6].80` + `P[Y=7].80`)
    ) %>% 
    # join imputed grambank data
    right_join(
      filter(imp, str_starts(Trait, fixed("GB"))), 
      by = c("glottocode" = "Language_ID")
      ) %>%
    # drop cases without matches
    drop_na(EGIDS:probNotSleep_80) %>%
    group_by(Trait) %>%
    summarise(
      propGrambank        = sum(Value == "1") / sum(Value != "?"),
      propImputed_Current = mean(Imputation),
      propImputed_2060    = mean(Imputation * probNotSleep_40),
      propImputed_2100    = mean(Imputation * probNotSleep_80)
    ) %>%
    pivot_longer(
      cols = starts_with("propImputed"),
      names_prefix = "propImputed_",
      names_to = "Time"
    ) %>%
    mutate(Time = factor(Time, levels = c("Current", "2060", "2100"))) %>%
    ggplot(aes(x = propGrambank, y = value)) +
    geom_abline(intercept = 0, slope = 1, colour = "white") +
    geom_point(size = 1) +
    facet_grid(. ~ Time) +
    scale_x_continuous(
      name = "Proportion of languages with feature in Grambank",
      limits = c(0, 1)
    ) +
    scale_y_continuous(
      name = "Estimated proportion of\nlanguages with feature",
      limits = c(0, 1)
    ) +
    theme(
      panel.grid = element_blank(),
      panel.spacing = unit(1, "lines")
    )
  # save
  ggsave(out, filename = "plots/proportions.pdf", width = 7, height = 2.7)
  return(out)
}