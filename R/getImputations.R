# get imputations in tibble
getImputations <- function(d, mcc, treesSubset, fileTrees, 
                           fileXML, trait, id) {
  # write json file
  json <- writeDataJSON(d, mcc, treesSubset, fileTrees, trait, id)
  # fit beast model
  fit <- fitBEAST(fileXML, trait, id)
  # extract effective sample sizes
  ess <- 
    extractESS(trait, id) %>%
    rename_with(function(x) ifelse(x == "trait", "Trait", paste0("ess_", x)))
  # get tree with reconstructed tips
  tree <- read.beast(
    file = paste0("temp/imputations_", paste0(trait, "_", id), ".trees")
  )
  # remove 10% burn in
  nTrees <- length(tree)
  burnIn <- c(1, 1:((nTrees - 1) / 10) + 1)
  tree <- tree[-burnIn]
  # wrangle as tibble
  imp <- tibble()
  for (i in 1:length(tree)) {
    imp <- 
      bind_rows(
        imp, 
        as_tibble(tree[[i]]) %>%
          as.data.frame() %>%
          filter(!is.na(label)) %>% # tips only
          mutate(treeID = i) %>% 
          dplyr::select(treeID, everything())
      )
  }
  # summarise posterior tip reconstructions and match with real data
  values <- 
    d %>% 
    filter(Parameter_ID == trait) %>% 
    transmute(
      label = Language_ID, 
      Value = Value
    )
  out <-
    imp %>%
    group_by(label) %>%
    summarise(
      Imputation = mean(location),
      N = n()
      ) %>%
    left_join(values, by = "label") %>%
    mutate(
      Value = factor(ifelse(is.na(Value), "?", Value), levels = c("0","1","?")),
      Trait = trait,
      id = id
      ) %>%
    rename(Language_ID = label) %>%
    dplyr::select(Trait, id, everything()) %>%
    left_join(ess, by = "Trait")
  # clean up files after all computation is finished
  cleanUpFiles(trait, id)
  return(out)
}
