# get dplace data
getDPLACEData <- function(mcc) {
  # prepare language data to link dplace to edge tree
  link <- 
    read_csv(
      paste0(
        # from DPLACE version 3.0.0
        "https://raw.githubusercontent.com/D-PLACE/dplace-cldf/",
        "b68c021e302f46763a8d25a7a5b7ffc416ba2aa4/cldf/societies.csv"
      ), 
      show_col_types = FALSE
      ) %>%
    transmute(
      Soc_ID = ID,
      Language_ID = Glottocode
    )
  # get dplace data
  read_csv(
    paste0(
      # from DPLACE version 3.0.0
      "https://raw.githubusercontent.com/D-PLACE/dplace-cldf/",
      "b68c021e302f46763a8d25a7a5b7ffc416ba2aa4/cldf/data.csv"
    ), 
    show_col_types = FALSE
  ) %>%
    left_join(link, by = "Soc_ID") %>%
    # remove 396 languages not represented on the global tree
    filter(Language_ID %in% mcc$tip.label)
}
