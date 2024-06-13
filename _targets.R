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

# targets for main GB trait imputations
mainGBTargets <-
  tar_map(
    # iterate over binary traits
    values = tibble(trait = binaryGBTraits, type = "main"),
    names = "trait",
    # write json file
    tar_target(json, writeDataJSON(gb, mcc, treesSubset, 
                                   fileTrees, trait, type)),
    # fit model to data
    tar_target(fit, fitBEAST(trait, json, fileXML, type)),
    # extract effective sample size for posterior
    tar_target(ess, extractESS(trait, fit, type)),
    # get imputation results in tibble
    tar_target(imp, getImputations(trait, fit, gb, type)),
    # plot imputation results on tree
    tar_target(plot, plotImputations(trait, mcc, imp)),
    # clean up files to save storage
    # (include plot & ess to ensure this happens at the end)
    tar_target(cleanUp, cleanUpFiles(trait, plot, ess, type))
  )

# targets for GB validation
validationGBTargets <-
  tar_map(
    # iterate over binary traits
    values = expand_grid(trait = binaryGBTraits, type = "validation", id = 1:2),
    names = c("trait", "id"),
    # which language is to be removed in the validation?
    tar_target(valLang, getValidationLanguage(values, trait, id)),
    # remove validation language from dataset
    tar_target(valData, removeValidationLanguage(values, trait, valLang)),
    # write json file
    tar_target(valJson, writeDataJSON(valData, mcc, treesSubset, fileTrees, 
                                      trait, type, id)),
    # fit model to data
    tar_target(valFit, fitBEAST(trait, valJson, fileXML, type, id)),
    # get imputation results in tibble
    tar_target(valImp, getImputations(trait, valFit, valData, type, id)),
    # get validation results
    tar_target(valResult, getValidationResult(trait, id, valLang, values, 
                                              valImp, phyDistMat, phySignal))
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
  
  # run main imputations
  mainGBTargets,
  tar_combine(ess, mainGBTargets[["ess"]]),
  # run validations
  validationGBTargets,
  tar_combine(valResults, validationGBTargets[["valResult"]]),
  tar_target(plotVal, plotValidation(valResults)),
  # calculate auc
  tar_target(auc, calculateAUC(valResults)),
  # fit models analysing validation results
  tar_target(valModel1, fitValidationModel1(valResults)),
  tar_target(valModel2, fitValidationModel2(valResults)),
  tar_target(valModel3, fitValidationModel3(valResults)),
  tar_target(valModel4, fitValidationModel4(valResults)),
  # plot models anlysing validation results
  tar_target(plotValModel1, plotValidationModel1(valModel1)),
  tar_target(plotValModel2, plotValidationModel2(valModel2)),
  tar_target(plotValModel3, plotValidationModel3(valModel3)),
  tar_target(plotValModel4, plotValidationModel4(valModel4)),
  # plot binned imputed values
  tar_target(plotValBin, plotBinned(valResults))
)
