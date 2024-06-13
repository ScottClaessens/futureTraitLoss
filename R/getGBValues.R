# get grambank values
getGBValues <- function(fileValues, mcc) {
  read_csv(fileValues, show_col_types = FALSE) %>%
    # remove ~262 languages not represented on the global tree
    filter(Language_ID %in% mcc$tip.label)
}
