# The future loss of linguistic and cultural traits

This project estimates the future loss of linguistic and cultural traits by
combining data from [Grambank](https://grambank.clld.org/) and 
[DPLACE](https://d-place.org/) with a global language tree (see 
[here](https://osf.io/preprints/socarxiv/f8tr6)) and estimates of future 
language loss (see [here](https://www.nature.com/articles/s41559-021-01604-y)).

## Getting started

### Installation guide

To run this code, you will first need to [install R](https://www.r-project.org/)
and install the following packages:

```R
install.packages(c(
    "ape","brms","dplyr","cowplot","ggplot2", "phangorn","pROC",
    "readr","readxl","rjson","tarchetypes",
    "targets","tidyverse","tracerer"
))

# treeio/ggtree/ggtreeExtra need to be installed like this:
if (!requireNamespace("BiocManager", quietly=TRUE))
    install.packages("BiocManager")
## BiocManager::install("BiocUpgrade") ## you may need this
BiocManager::install("treeio")
BiocManager::install("ggtree")
BiocManager::install("ggtreeExtra")
```

Then you will need to download BEAST2 and add it to this repository. See here 
for more details: https://www.beast2.org/

Finally, you will need to ensure that BEAST2 has the following packages 
installed: `FixedTreeAnalysis` and `BEAST_CLASSIC`. See here for details on how
to install packages: http://beast2.org/managing-packages/

### Executing code

1. Set the working directory to this code repository 
`setwd("myPath/futureLossTraits")`
2. Load the `targets` package with `library(targets)`
3. To run the analysis pipeline, run `tar_make()`
4. To load individual targets into your environment, run `tar_load()` etc.

## Help

Any issues, please email scott.claessens@gmail.com.

## Authors

Scott Claessens, scott.claessens@gmail.com
