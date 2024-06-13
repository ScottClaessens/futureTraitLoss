# clean up files
cleanUpFiles <- function(trait, plot, ess, type, id = NULL) {
  # get trait label
  trait <- paste0(trait, ifelse(type == "main", "", paste0("_", id)))
  # silently delete beast output files
  invisible(
    file.remove(
      paste0("temp/", trait, ".json"),
      paste0("temp/out_", trait, ".xml"),
      paste0("temp/imputations_", trait, ".trees"),
      paste0("temp/trees_", trait, ".trees"),
      paste0("temp/state_", trait, ".state"),
      paste0("temp/log_", trait, ".log")
    )
  )
}