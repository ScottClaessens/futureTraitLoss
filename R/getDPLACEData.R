# get dplace data
getDPLACEData <- function(fileDPLACE, mcc) {
  # functions for converting variables
  convertForager  <- function(x) ifelse(is.na(x), "?", as.character(x))
  convertSubsist  <- function(x) ifelse(is.na(x), "?", ifelse(x == 2, "0", "1"))
  convertSettle   <- function(x) ifelse(is.na(x), "?", ifelse(x == 2, "0", "1"))
  convertEndogamy <- function(x) ifelse(is.na(x), "?", ifelse(x == 1, "1", "0"))
  convertExogamy  <- function(x) ifelse(is.na(x), "?", ifelse(x == 3, "1", "0"))
  # load data
  read_xlsx(path = fileDPLACE, na = "") %>%
    # rename variables
    transmute(
      Language_ID     = Glottocode,
      ForagerDefinite = convertForager(Forager_language_definite),   # 0 = no, 1 = yes
      ForagerPossible = convertForager(Forager_language_possible),   # 0 = no, 1 = yes
      ForagerComplete = convertForager(Forager_language_complete),   # 0 = no, 1 = yes
      HunterGatherer  = convertSubsist(D_Place_Subsistence),         # 0 = agriculture, 1 = hunt/gather/fish
      Nomadic         = convertSettle(D_Place_Settlement_Pattern),   # 0 = sedentary, 1 = (semi)nomadic
      Endogamous      = convertEndogamy(D_Place_Community_Marriage), # 0 = not endogamous, 1 = endogamous
      Exogamous       = convertExogamy(D_Place_Community_Marriage)   # 0 = not exogamous, 1 = exogamous
    ) %>%
    # change one language id to match tree
    mutate(
      Language_ID = ifelse(Language_ID == "iron1242", "osse1243", Language_ID)
    )
}
