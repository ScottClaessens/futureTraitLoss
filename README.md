# The future loss of linguistic and cultural traits

This project estimates the future loss of linguistic and cultural traits by
combining data from [Grambank](https://grambank.clld.org/) and 
[DPLACE](https://d-place.org/) with a global language tree (see 
[here](https://osf.io/preprints/socarxiv/f8tr6)) and estimates of future 
language loss (see [here](https://www.nature.com/articles/s41559-021-01604-y)).

## Getting started

### Installation guide

To run this code, you will need to [install R](https://www.r-project.org/) and
install the following packages:

```
install.packages(c("ape","brms","dplyr","cowplot","ggplot2",
                   "ggtree","ggtreeExtra","phangorn","pROC",
                   "readr","rjson","tarchetypes","targets",
                   "tidyverse","tracerer","treeio"))
```

### Executing code

1. Set the working directory to this code repository `setwd("myPath/futureLossTraits")`
2. Load the `targets` package with `library(targets)`
3. To run the analysis pipeline, run `tar_make()`
4. To load individual targets into your environment, run `tar_load()` etc.

## Help

Any issues, please email scott.claessens@gmail.com.

## Authors

Scott Claessens, scott.claessens@gmail.com
