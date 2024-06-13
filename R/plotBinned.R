# plot binned imputed values
plotBinned <- function(valResults) {
  # get binned imputed values
  p <-
    valResults %>%
    mutate(bin = floor(imputedValue * 10) / 10) %>%
    group_by(bin) %>%
    summarise(prop1 = mean(trueValue == 1)) %>%
    ggplot(aes(x = ifelse(bin == 1, 1, bin + 0.05), y = prop1)) +
    geom_point() +
    geom_line() +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    scale_x_continuous(name = "Binned imputation value (0.1 increments)",
                       breaks = seq(0, 1, by = 0.1), limits = c(0, 1)) +
    scale_y_continuous(name = "Proportion of true 1s", limits = c(0, 1),
                       breaks = seq(0, 1, by = 0.2)) +
    theme(panel.grid.minor.x = element_blank())
  # save
  ggsave(p, filename = "plots/validation/valBinned.pdf", width = 5, height = 5)
  return(p)
}
