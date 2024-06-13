# fit beast model
fitBEAST <- function(trait, json, fileXML, type, id = NULL, numThreads = 1) {
  # set working directory
  system(paste0("cd ", getwd()))
  # get json file location
  fileJSON <-
    paste0(
      "files/json/", type, "/",
      paste0(trait, ifelse(type == "main", "", paste0("_", id))),
      ".json"
    )
  # write command
  command <- 
    paste0(
      # run beast
      "BEAST\\ 2.7.5/bin/beast ",
      # number of threads
      "-threads ", numThreads, " ",
      # json file
      "-DF ", fileJSON, " ",
      # out file
      "-DFout out/", type, "/out/", trait, ".xml ",
      # overwrite any existing files
      "-overwrite ",
      # state file
      "-statefile out/", type, "/state/", trait, ".state ",
      # xml file
      fileXML
    )
  # execute command
  system(command)
  return(paste0("Finished running at ", Sys.time()))
}
