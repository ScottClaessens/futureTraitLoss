combineData <- function(gb, dplace) {
  # pivot dplace data longer
  dplace <- 
    pivot_longer(
      dplace,
      cols = !Language_ID,
      names_to = "Parameter_ID",
      values_to = "Value"
      )
  # bind datasets together
  bind_rows(gb, dplace)
}