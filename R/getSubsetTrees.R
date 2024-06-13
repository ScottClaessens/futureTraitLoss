# get subset of trees
getSubsetTrees <- function(trees, n = 100) {
  # total length
  nTrees <- length(trees)
  # sample n random trees
  ids <- sample(1:nTrees, size = n)
  # get trees subset
  out <- trees[ids]
  # save trees subset
  write.nexus(out, file = "files/trees/subset.trees")
  return(out)
}
