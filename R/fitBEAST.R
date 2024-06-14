# fit beast model
fitBEAST <- function(trait, json, fileXML, id, numThreads = 1) {
  # trait label
  trait <- paste0(trait, "_", id)
  # get json file location
  fileJSON <- paste0("temp/", trait, ".json")
  # write command
  command <- 
    paste0(
      # run beast
      "BEAST.v2.7.6.Windows/BEAST/bat/beast.bat ",
      # number of threads
      "-threads ", numThreads, " ",
      # json file
      "-DF ", fileJSON, " ",
      # out file
      "-DFout temp/out_", trait, ".xml ",
      # overwrite any existing files
      "-overwrite ",
      # state file
      "-statefile temp/state_", trait, ".state ",
      # xml file
      fileXML
    )
  # execute commands
  system(command)
  return(paste0("Finished running at ", Sys.time()))
}
