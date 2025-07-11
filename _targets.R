# load libraries and functions
options(tidyverse.quiet = TRUE)
library(crew)
library(targets)
library(tarchetypes)
library(tidyverse)
tar_source()

BEAST_COMMAND <- 'BEAST.v2.7.7.Windows/BEAST/bat/beast.bat'
BEAST_COMMAND <- 'beast' # -beagle -beagle_SSE'

NUMBER_OF_VALIDATIONS <- 50

# set targets options
tar_option_set(
  packages = c("ape","brms","dplyr","cowplot","ggplot2","ggtree","ggtreeExtra",
               "phangorn","pROC","readr","readxl","rjson","tracerer","treeio")
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
  pull(Parameter_ID)

# replace multistate variables with binarised variables.
# multistate_parameters and binary_parameters are set in binariseGB.R
binaryGBTraits <- binaryGBTraits[!binaryGBTraits %in% multistate_parameters]
binaryGBTraits <- c(binaryGBTraits, binary_parameters)


# targets for imputations
impTargets <-
  tar_map(
    # iterate over binary traits
    values = expand_grid(trait = binaryGBTraits),
    # fit models and get imputation results in tibble
    tar_target(imp, getImputations(d, mcc, treesSubset, fileTrees, 
                                   fileXML, trait, id), pattern = map(id)),
    # plot imputation results on tree
    tar_target(plot, plotImputations(trait, mcc, imp)),
    # get result for validations
    tar_target(valResult, getValidationResult(trait, d, imp, 
                                              phyDistMat, phySignal))
  )

# pipeline
list(
  
  ### 1. Load data
  
  # files
  tar_target(fileTrees, "data/edge6636-March-2023-no-metadata.trees", format = "file"),
  tar_target(fileXML, "xml/imputeTipsBEAST_multiTree_strictClock.xml", format = "file"),
  tar_target(filePhySignal, "data/phySignal.csv", format = "file"),
  tar_target(fileEndanger, "data/endanger.rds", format = "file"),
  tar_target(fileDPLACE, "data/dplaceEdgeTree.xlsx", format = "file"),
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
  tar_target(dplace, getDPLACEData(fileDPLACE, mcc)),
  # combine grambank and dplace data
  tar_target(d, combineData(gb, dplace)),
  # load phylogenetic signal data
  tar_target(phySignal, read_csv(filePhySignal, show_col_types = FALSE)),
  # load endangerment data
  tar_target(endanger, readRDS(fileEndanger)),
  
  ### 2. Imputations
  
  # ids to loop over (0 = main, 1:n = validations)
  tar_target(id, 0:NUMBER_OF_VALIDATIONS),
  ## run imputations
  impTargets,
  # combine results
  tar_combine(
    imp,
    impTargets[["imp"]],
    command = bind_rows(!!!.x) %>% filter(id == 0)
    ),
  tar_combine(valResults, impTargets[["valResult"]]),
  # plot validation results
  tar_target(plotVal, plotValidation(valResults)),
  # calculate auc
  tar_target(auc, calculateAUC(valResults)),
  # fit models analysing validation results
  tar_target(valModel1, fitValidationModel1(valResults)),
  tar_target(valModel2, fitValidationModel2(valResults)),
  tar_target(valModel3, fitValidationModel3(valResults)),
  tar_target(valModel4, fitValidationModel4(valResults)),
  # plot models analysing validation results
  tar_target(plotValModel1, plotValidationModel1(valModel1)),
  tar_target(plotValModel2, plotValidationModel2(valModel2)),
  tar_target(plotValModel3, plotValidationModel3(valModel3)),
  tar_target(plotValModel4, plotValidationModel4(valModel4)),
  # plot binned imputed values
  tar_target(plotValBin, plotBinned(valResults)),
  # plot trait loss
  tar_target(plotLoss, plotTraitLoss(endanger, imp)),
  # plot proportion in grambank and proportion in imputations
  tar_target(plotProp, plotProportions(endanger, imp)),
  # predict disappearance times
  tar_target(predDisappear, predictDisappearanceTimes(endanger, imp))
)
