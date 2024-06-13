# plot validation model 1 with data
plotValidationModel1 <- function(valModel1) {
  # get model predictions
  p <- plot(
    conditional_effects(valModel1),
    points = TRUE,
    ask = FALSE,
    plot = FALSE
    )[[1]]
  p <- 
    p +
    scale_x_continuous(
      name = "Trait coverage"
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
    filename = "plots/validation/valModel1.pdf",
    width = 6,
    height = 6
    )
  return(p)
}
