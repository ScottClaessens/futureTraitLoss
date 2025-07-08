# get grambank data
getGBData <- function(mcc) {
  read_csv(
    paste0(
      # from Grambank version 1.0.3
      "https://raw.githubusercontent.com/grambank/grambank/",
      "7ae000cf740f93cdb3e4ec67010668d6795337a9/cldf/values.csv"
    ), 
    show_col_types = FALSE
    ) %>%
    # remove 262 languages not represented on the global tree
    filter(Language_ID %in% mcc$tip.label) %>%
    binarise()
}
