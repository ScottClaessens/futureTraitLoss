plotTraitLoss <- function(endanger, imp) {
  # join imputations and endangerment probabilities
  d <-
    endanger %>%
    # grambank traits only
    filter(str_starts(trait, fixed("GB"))) %>%
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
    drop_na(EGIDS:probNotSleep_80)
  # plot feature loss
  pA <-
    d %>%
    # first, filter to only non-sleeping languages in the present day
    filter(EGIDS < 6) %>%
    # then, for each trait...
    group_by(Trait) %>%
    summarise(
      # get the expected number of non-sleeping languages with trait = 1
      num_2023 = sum(Imputation),
      num_2060 = sum(Imputation * probNotSleep_40),
      num_2100 = sum(Imputation * probNotSleep_80)
    ) %>%
    mutate(Trait = fct_reorder(Trait, desc(num_2023))) %>%
    ggplot() +
    geom_col(aes(x = Trait, y = num_2023), fill = "#cae6d3") +
    geom_col(aes(x = Trait, y = num_2100), fill = "#71a674") +
    scale_x_discrete(
      name = "Grambank features",
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      name = "Estimated number of\nlanguages with feature",
      expand = c(0, 0),
      limits = c(0, 6500),
      breaks = seq(0, 6000, by = 1000)
    ) +
    theme(
      panel.grid = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0))
    )
  # get data on macroareas
  macroareas <-
    read_csv(
      paste0(
        # from Grambank version 1.0.3
        "https://raw.githubusercontent.com/grambank/grambank/",
        "7ae000cf740f93cdb3e4ec67010668d6795337a9/cldf/languages.csv"
      ), 
      show_col_types = FALSE
    ) %>%
    dplyr::select(ID, Macroarea)
  # split by macroarea
  pB <-
    d %>%
    # filter to only non-sleeping languages in the present day
    filter(EGIDS < 6) %>%
    # link macroarea data
    left_join(macroareas, by = c("glottocode" = "ID")) %>%
    filter(!is.na(Macroarea)) %>%
    # then, for each trait and macroarea
    group_by(Trait, Macroarea) %>%
    summarise(
      # get the expected number of non-sleeping languages with trait = 1
      num_2023 = sum(Imputation),
      num_2060 = sum(Imputation * probNotSleep_40),
      num_2100 = sum(Imputation * probNotSleep_80),
      .groups = "drop"
    ) %>%
    # ordering for plot
    mutate(
      TraitMacroarea = paste(Trait, Macroarea),
      TraitMacroarea = fct_reorder(TraitMacroarea, desc(num_2023))
    ) %>%
    ggplot() +
    geom_col(aes(x = TraitMacroarea, y = num_2023), fill = "#cae6d3") +
    geom_col(aes(x = TraitMacroarea, y = num_2100), fill = "#71a674") +
    facet_wrap(
      vars(Macroarea),
      scales = "free"
    ) +
    scale_x_discrete(
      name = "Grambank features",
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      name = "Estimated number of\nlanguages with feature",
      expand = c(0, 0)
    ) +
    theme(
      panel.grid = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0))
    )
  # save plot
  out <- plot_grid(pA, pB, ncol = 1)
  ggsave(out, filename = "plots/loss.pdf", width = 6, height = 6)
  return(out)
}