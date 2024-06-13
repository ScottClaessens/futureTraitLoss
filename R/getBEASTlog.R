# get beast log file
getBEASTLog <- function(trait, fit, type, id) {
  parse_beast_tracelog_file(
    paste0(
      "out/",
      type,
      "/log/",
      paste0(
        trait,
        ifelse(type == "main", "", paste0("_", id))
        ), 
      ".log"
      )
  )
}
