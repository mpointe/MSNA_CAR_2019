# Cleaning data

setwd("C:/Users/REACH-RCA-AO/Documents/GitHub/MSNA_CAR_2019/input")

devtools::install_github("ellieallien/cleaninginspectoR", build_vignette = T)

# show packages as it is in GitHub -> to see vignettes
.libPaths()

library("cleaninginspectoR")

?cleaninginspectoR


testdf <- read.csv("./questionnaire_test_0705.csv", stringsAsFactors = F)
testdf %>% str

find_duplicates(testdf, duplicate.column.name = "X_uuid")
testdf$X_uuid


issues = cleaninginspectoR::inspect_all(testdf)


