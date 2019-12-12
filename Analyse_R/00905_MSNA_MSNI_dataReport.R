# setup
library(dplyr)
library(readr)
library(tidyr)
library(koboquest) # manage kobo questionnairs
library(parallel) # mclapply
library(kobostandards) # check inputs for inconsistencies
# devtools::install_github('mabafaba/kobostandards') 
library(xlsformfill) # generate fake data for kobo
#devtools::install_github('mabafaba/xlsformfill') 
library(hypegrammaR) # stats 4 complex samples
# install.packages("curl")
# install.packages("cran")
library(curl)
# devtools::install_github('ellieallien/hypegrammaR')
library(composr) # horziontal operations
#devtools::install_github('mabafaba/composr') 
library(parallel)
library(knitr)
library(surveyweights)
library(stringr)
library(srvyr)
if (!require("processx")) install.packages("processx")
library(surveyweights)
library(processx)
# install.packages("plotly")
library(plotly)
library(msni19)
# remotes::install_github("caldwellst/msni19")
# remotes::install_github("ellieallien/surveyweights")

#### READING LATEST DATAFRAME !
response_updated_cluster_msni <- read.csv(paste0("./output/msni/final/Dataset_MSNI_RAW_20191205.csv"), stringsAsFactors = TRUE)
# response_updated_cluster_msni_test <- read.csv(paste0("./output/msni/final/Dataset_MSNI_RAW_20191203.csv"), stringsAsFactors = TRUE)


# % par admin 2 au sein des HH en 4
response_updated_cluster_msni_4 = subset(response_updated_cluster_msni, response_updated_cluster_msni$msni_score_final == 4)

template_analysisplan_file <- "./input/analysisplan_template_msni_4.csv"

response_updated_cluster_msni$admin_0 <- "RCA"

analysisplan_admin_0 <- make_analysis_plan_template(df= response_updated_cluster_msni,
                                                    repeat.for.variable = "admin_0",
                                                    questionnaire = questionnaire,
                                                    hypothesis.type = "direct_reporting",
                                                    template_file = template_analysisplan_file)

analysisplan_admin_0 <- analysisplan_admin_0[!is.na(analysisplan_admin_0$dependent.variable.type),]

### CRUNCH
final_result_admin_0 <- from_analysisplan_map_to_output(data = response_updated_cluster_msni, 
                                                        analysisplan = analysisplan_admin_0, 
                                                        weighting = weighting_combined_2, 
                                                        cluster_variable_name = "clusters",
                                                        questionnaire = questionnaire)

summary.stats_admin0 <- final_result_admin_0$results %>%
  lapply(function(x){x$summary.statistic}) %>% do.call(rbind, .)%>%
  map_to_file(paste0("./output/msni/final/","summary_stats_admin0_RAW_",format(Sys.time(), "%Y%m%d"),"_msni_ALL_test2.csv"))


?Setviz::