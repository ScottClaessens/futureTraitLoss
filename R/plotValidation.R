# plot validation results
plotValidation <- function(valResults) {
  # plotting function
  plottingFun <- function(true) {
    valResults %>%
      # correct ordering for plot
      group_by(trait, trueValue) %>%
      mutate(medianError = median(error)) %>%
      ungroup() %>%
      mutate(trueValue = factor(trueValue)) %>%
      # filter to true value
      filter(trueValue == true) %>%
      # plot
      ggplot(aes(x = reorder(trait, medianError), y = error)) +
      geom_boxplot(outlier.shape = NA, colour = ifelse(true == 0, "#F8766D", "#619CFF")) +
      labs(x = "Grambank trait", y = "Error rate", colour = "True value",
           title = paste0("True value = ", true)) +
      theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank()
      )
  }
  p0 <- plottingFun(true = 0)
  p1 <- plottingFun(true = 1)
  # put together
  p <- plot_grid(p0, p1, ncol = 1)
  # save
  ggsave(p, filename = "plots/validation/validation.pdf", width = 7, height = 5)
  return(p)
}
