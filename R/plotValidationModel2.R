# plot validation model 2 with data
plotValidationModel2 <- function(valModel2) {
  # get model predictions
  p <- plot(
    conditional_effects(valModel2),
    points = TRUE,
    ask = FALSE,
    plot = FALSE
    )[[2]]
  p <- 
    p +
    scale_x_continuous(
      name = "Proportion of 1s in observed data", 
      limits = c(0, 1), 
      breaks = c(0, 0.25, 0.5, 0.75, 1)
      ) +
    scale_y_continuous(
      name = "Error rate",
      limits = c(0, 90), 
      labels = function(x) x / 90,
      breaks = c(0, 0.25, 0.5, 0.75, 1) * 90
      )
  # save plot
  ggsave(
    p,
    filename = "plots/validation/valModel2.pdf",
    width = 6,
    height = 6
    )
  return(p)
}
