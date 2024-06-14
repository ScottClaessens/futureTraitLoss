# load libraries and functions
options(tidyverse.quiet = TRUE)
library(crew)
library(targets)
library(tarchetypes)
library(tidyverse)
tar_source()

# set targets options
tar_option_set(
  packages = c("ape","brms","dplyr","cowplot","ggplot2","ggtree","ggtreeExtra",
               "phangorn","pROC","readr","rjson","tracerer","treeio")
  )

# get binary grambank traits
binaryGBTraits <- 
  read_csv(
    paste0(
      # Grambank version 1.0.3
      "https://raw.githubusercontent.com/grambank/grambank/",
      "7ae000cf740f93cdb3e4ec67010668d6795337a9/cldf/codes.csv"
      ),
    show_col_types = FALSE
    ) %>% 
  group_by(Parameter_ID) %>% 
  summarise(n = n()) %>% 
  filter(n == 2) %>% 
  pull(Parameter_ID)

# targets for grambank imputations
gbTargets <-
  tar_map(
    # iterate over binary traits
    values = expand_grid(trait = binaryGBTraits[1]),
    # write json file
    tar_target(json, writeDataJSON(gb, mcc, treesSubset, fileTrees, trait, id),
               pattern = map(id)),
    # fit model to data
    tar_target(fit, fitBEAST(trait, json, fileXML, id),
               pattern = map(json, id)),
    # extract effective sample size for posterior
    tar_target(ess, extractESS(trait, fit, id),
               pattern = map(fit, id)),
    # get imputation results in tibble
    tar_target(imp, getImputations(trait, fit, gb, id, ess),
               pattern = map(fit, id, ess)),
    # plot imputation results on tree
    tar_target(plot, plotImputations(trait, mcc, imp)),
    # get result for validations
    tar_target(valResult, getValidationResult(trait, gb, imp, phyDistMat, phySignal))
  )

# pipeline
list(
  
  ### 1. Load data
  
  # files
  tar_target(fileTrees, "data/edge6636-March-2023-no-metadata.trees", format = "file"),
  tar_target(fileXML, "xml/imputeTipsBEAST_multiTree_strictClock.xml", format = "file"),
  tar_target(filePhySignal, "data/phySignal.csv", format = "file"),
  # load posterior treeset
  tar_target(trees, read.nexus(fileTrees)),
  # random subset of n trees for analysis
  tar_target(treesSubset, getSubsetTrees(trees, n = 100)),
  # construct maximum clade credibility tree
  tar_target(mcc, mcc(trees)),
  # get phylogenetic distance matrix from mcc tree
  tar_target(phyDistMat, cophenetic.phylo(mcc)),
  # load grambank data
  tar_target(gb, getGBData(mcc)),
  # load dplace data
  tar_target(dplace, getDPLACEData(mcc)),
  # load phylogenetic signal data
  tar_target(phySignal, read_csv(filePhySignal, show_col_types = FALSE)),
  
  ### 2. Grambank imputations
  
  # ids to loop over (0 = main, 1:n = validations)
  tar_target(id, 0:2),
  ## run imputations
  gbTargets,
  # extract effective sample sizes
  tar_combine(ess, gbTargets[["ess"]]),
  # get validation results
  tar_combine(valResults, gbTargets[["valResult"]]),
  # plot validation results
  #tar_target(plotVal, plotValidation(valResults)),
  # calculate auc
  tar_target(auc, calculateAUC(valResults))
  # fit models analysing validation results
  #tar_target(valModel1, fitValidationModel1(valResults)),
  #tar_target(valModel2, fitValidationModel2(valResults)),
  #tar_target(valModel3, fitValidationModel3(valResults)),
  #tar_target(valModel4, fitValidationModel4(valResults)),
  ## plot models anlysing validation results
  #tar_target(plotValModel1, plotValidationModel1(valModel1)),
  #tar_target(plotValModel2, plotValidationModel2(valModel2)),
  #tar_target(plotValModel3, plotValidationModel3(valModel3)),
  #tar_target(plotValModel4, plotValidationModel4(valModel4)),
  ## plot binned imputed values
  #tar_target(plotValBin, plotBinned(valResults))
)
