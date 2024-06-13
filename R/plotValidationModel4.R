# plot validation model 4 with data
plotValidationModel4 <- function(valModel4) {
  # get model predictions
  p <- plot(
    conditional_effects(valModel4),
    points = TRUE,
    ask = FALSE,
    plot = FALSE
    )[[1]]
  p <- 
    p +
    scale_x_continuous(
      name = "Distance to nearest language with observed trait data",
      trans = "log",
      breaks = c(0.1, 1, 10)
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
    filename = "plots/validation/valModel4.pdf",
    width = 6,
    height = 6
    )
  return(p)
}
