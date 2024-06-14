# get imputations in tibble
getImputations <- function(trait, fit, gb, id, ess) {
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
    gb %>% 
    filter(Parameter_ID == trait) %>% 
    transmute(
      label = Language_ID, 
      Value = Value
    )
  out <-
    imp %>%
    group_by(label) %>%
    summarise(Imputation = mean(location)) %>%
    left_join(values, by = "label") %>%
    mutate(
      Value = factor(ifelse(is.na(Value), "?", Value), levels = c("0","1","?")),
      Trait = trait,
      id = id
      ) %>%
    rename(Language_ID = label) %>%
    dplyr::select(Trait, id, everything())
  # clean up files after all computation is finished
  cleanUpFiles(trait, ess, id)
  return(out)
}
