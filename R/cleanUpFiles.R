# clean up files
cleanUpFiles <- function(trait, ess, id) {
  # get trait label
  trait <- paste0(trait, "_", id)
  # delete beast output files
  file.remove(
    paste0("temp/", trait, ".json"),
    paste0("temp/out_", trait, ".xml"),
    paste0("temp/imputations_", trait, ".trees"),
    paste0("temp/trees_", trait, ".trees"),
    paste0("temp/state_", trait, ".state"),
    paste0("temp/log_", trait, ".log")
  )
  # do not return anything
  invisible(NULL)
}