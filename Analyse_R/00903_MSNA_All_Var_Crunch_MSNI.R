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
#source("functions/to_alphanumeric_lowercase.R") # function to standardise column headers (like check.names)
source("functions/analysisplan_factory.R")  # generate analysis plans
source("functions/remove_responses_from_sumstat.R")  # generate analysis plans
source("functions/format_hypothesis_test.R")

## Add the var you want to output into the analysis plan file 
template_analysisplan_file <- "./input/analysisplan_template_msni.csv"

# IF NEW VARIABLES TO OUTPUT, need to add into the questionnnaire :
# # # #load questionnaire inputs
#  questions <- read.csv("input/questionnaire_kobo_hh_combine_v4_FINAL_PourAnalyse_survey.csv",
#                       stringsAsFactors=F, check.names = F, encoding = "UTF-8")
# 
# choices <- read.csv("input/questionnaire_kobo_hh_combine_v4_FINAL_PourAnalyse_choices.csv",
#                     stringsAsFactors=F, check.names = F, encoding = "UTF-8")
# 
# choices$name <- gsub('[^ -~]', '', choices$name)
# questions$name <- gsub('[^ -~]', '', questions$name)
# questions$name <- tolower(questions$name)
# 
# ## CAREFUL : have some " " at the end of some options. Replace them with nothing :
# choices$list_name %<>% gsub(" ", "", .)
# 

#### READING LATEST DATAFRAME !
response_updated_cluster_msni <- read.csv(paste0("./output/msni/final/Dataset_MSNI_RAW_20191209.csv"), stringsAsFactors = TRUE)


#### ADMIN 0 ####
response_updated_cluster_msni$admin_0 <- "RCA"

analysisplan_admin_0 <- make_analysis_plan_template(df= response_updated_cluster_msni,
                                                    repeat.for.variable = "admin_0",
                                                    questionnaire = questionnaire,
                                                    hypothesis.type = "direct_reporting",
                                                    template_file = template_analysisplan_file)

analysisplan_admin_0 <- analysisplan_admin_0[!analysisplan_admin_0$dependent.variable %in% unwanted_cols,]
analysisplan_admin_0 <- analysisplan_admin_0[!is.na(analysisplan_admin_0$dependent.variable.type),]
# To analyse only one variable : 
# analysisplan_admin_0 <- analysisplan_admin_0[(analysisplan_admin_0$dependent.variable == "score_nfi_tot") ,]

### CRUNCH
final_result_admin_0 <- from_analysisplan_map_to_output(data = response_updated_cluster_msni, 
                                                        analysisplan = analysisplan_admin_0, 
                                                        weighting = weighting_combined, 
                                                        cluster_variable_name = "clusters",
                                                        questionnaire = questionnaire)

summary.stats_admin0 <- final_result_admin_0$results %>%
  lapply(function(x){x$summary.statistic}) %>% do.call(rbind, .)%>%
  map_to_file(paste0("./output/msni/final/","summary_stats_admin0_RAW_",format(Sys.time(), "%Y%m%d"),"_2.csv"))

### ADMIN 0 GROUPE
analysisplan_admin_0_grp <- make_analysis_plan_template(df= response_updated_cluster_msni,
                                                        questionnaire = questionnaire,
                                                        repeat.for.variable = "admin_0",
                                                        independent.variable = "ig_8_statut_groupe",
                                                        hypothesis.type = "group_difference",
                                                        template_file = template_analysisplan_file
)
analysisplan_admin_0_grp <- analysisplan_admin_0_grp[!analysisplan_admin_0_grp$dependent.variable %in% unwanted_cols,]
analysisplan_admin_0_grp <- analysisplan_admin_0_grp[!is.na(analysisplan_admin_0_grp$dependent.variable.type),]
# To analyse only one variable : 
# analysisplan_admin_0_grp <- analysisplan_admin_0_grp[(analysisplan_admin_0_grp$dependent.variable == "score_nfi_tot") ,]

final_result_admin_0_grp <- from_analysisplan_map_to_output(data = response_updated_cluster_msni, 
                                                            analysisplan = analysisplan_admin_0_grp, 
                                                            weighting = weighting_combined, 
                                                            cluster_variable_name = "clusters",
                                                            questionnaire = questionnaire)

summary.stats_admin0_grp <- final_result_admin_0_grp$results %>%
  lapply(function(x){x$summary.statistic}) %>% do.call(rbind, .)%>%
  map_to_file(paste0("./output/msni/final/","summary_stats_admin0_grp_RAW_",format(Sys.time(), "%Y%m%d"),"_2.csv"))


#### ADMIN 0 urbanrural
analysisplan_admin_0_urbanrural <- make_analysis_plan_template(df= response_updated_cluster_msni,
                                                               questionnaire = questionnaire,
                                                               repeat.for.variable = "admin_0",
                                                               independent.variable = "urban_rural",
                                                               hypothesis.type = "group_difference",
                                                               template_file = template_analysisplan_file
)
analysisplan_admin_0_urbanrural <- analysisplan_admin_0_urbanrural[!analysisplan_admin_0_urbanrural$dependent.variable %in% unwanted_cols,]
analysisplan_admin_0_urbanrural <- analysisplan_admin_0_urbanrural[!is.na(analysisplan_admin_0_urbanrural$dependent.variable.type),]
# To analyse only one variable : 
# analysisplan_admin_0_urbanrural <- analysisplan_admin_0_sexHHD[(analysisplan_admin_0_urbanrural$dependent.variable == "score_nfi_tot") ,]

final_result_admin_0_urbanrural <- from_analysisplan_map_to_output(data = response_updated_cluster_msni, 
                                                                   analysisplan = analysisplan_admin_0_urbanrural, 
                                                                   weighting = weighting_combined, 
                                                                   cluster_variable_name = "clusters",
                                                                   questionnaire = questionnaire)

summary.stats_admin0_urbanrural<- final_result_admin_0_urbanrural$results %>%
  lapply(function(x){x$summary.statistic}) %>% do.call(rbind, .)%>%
  map_to_file(paste0("./output/msni/final/","summary_stats_admin0_urbanrural_RAW_",format(Sys.time(), "%Y%m%d"),"_2.csv"))



#### ADMIN 0 GRP + urbanrural
analysisplan_admin_0_grp_urbanrural <- make_analysis_plan_template(df= response_updated_cluster_msni,
                                                               questionnaire = questionnaire,
                                                               repeat.for.variable = "admin_0",
                                                               independent.variable = "ig_8_statut_groupe_uran_rural",
                                                               hypothesis.type = "group_difference",
                                                               template_file = template_analysisplan_file
)
analysisplan_admin_0_grp_urbanrural <- analysisplan_admin_0_grp_urbanrural[!analysisplan_admin_0_grp_urbanrural$dependent.variable %in% unwanted_cols,]
analysisplan_admin_0_grp_urbanrural <- analysisplan_admin_0_grp_urbanrural[!is.na(analysisplan_admin_0_grp_urbanrural$dependent.variable.type),]

final_result_admin_0_grp_urbanrural <- from_analysisplan_map_to_output(data = response_updated_cluster_msni, 
                                                                   analysisplan = analysisplan_admin_0_grp_urbanrural, 
                                                                   weighting = weighting_combined, 
                                                                   cluster_variable_name = "clusters",
                                                                   questionnaire = questionnaire)

summary.stats_admin0_grp_urbanrural<- final_result_admin_0_grp_urbanrural$results %>%
  lapply(function(x){x$summary.statistic}) %>% do.call(rbind, .)%>%
  map_to_file(paste0("./output/msni/final/","summary_stats_admin0_grp_urbanrural_RAW_",format(Sys.time(), "%Y%m%d"),"_2.csv"))


#### ADMIN 1 ####
analysisplan_admin_1 <- make_analysis_plan_template(df= response_updated_cluster_msni,
                                                    repeat.for.variable = "admin_1",
                                                    questionnaire = questionnaire,
                                                    hypothesis.type = "direct_reporting",
                                                    template_file = template_analysisplan_file
)
analysisplan_admin_1 <- analysisplan_admin_1[!row.names(analysisplan_admin_1) %in% unwanted_cols,]
analysisplan_admin_1 <- analysisplan_admin_1[!is.na(analysisplan_admin_1$dependent.variable.type),]
# To analyse only one variable : 
# analysisplan_admin_1 <- analysisplan_admin_1[(analysisplan_admin_1$dependent.variable == "score_nfi_tot") ,]

final_result_admin_1 <- from_analysisplan_map_to_output(data = response_updated_cluster_msni, 
                                                        analysisplan = analysisplan_admin_1, 
                                                        weighting = weighting_combined, 
                                                        cluster_variable_name = "clusters",
                                                        questionnaire = questionnaire)

summary.stats_admin1 <- final_result_admin_1$results %>%
  lapply(function(x){x$summary.statistic}) %>% do.call(rbind, .)%>%
  map_to_file(paste0("./output/msni/final/","summary_stats_admin1_RAW_",format(Sys.time(), "%Y%m%d"),"_2.csv"))


#### ADMIN 1 GROUPE ####
analysisplan_admin_1_grp <- make_analysis_plan_template(df= response_updated_cluster_msni,
                                                        questionnaire = questionnaire,
                                                        repeat.for.variable = "admin_1",
                                                        independent.variable = "ig_8_statut_groupe",
                                                        hypothesis.type = "group_difference",
                                                        template_file = template_analysisplan_file
)
analysisplan_admin_1_grp <- analysisplan_admin_1_grp[!analysisplan_admin_1_grp$dependent.variable %in% unwanted_cols,]
analysisplan_admin_1_grp <- analysisplan_admin_1_grp[!is.na(analysisplan_admin_1_grp$dependent.variable.type),]
# To analyse only one variable : 
# analysisplan_admin_1_grp <- analysisplan_admin_1_grp[(analysisplan_admin_1_grp$dependent.variable == "score_nfi_tot") ,]

final_result_admin_1_grp <- from_analysisplan_map_to_output(data = response_updated_cluster_msni, 
                                                            analysisplan = analysisplan_admin_1_grp, 
                                                            weighting = weighting_combined, 
                                                            cluster_variable_name = "clusters",
                                                            questionnaire = questionnaire)

summary.stats_admin1_grp <- final_result_admin_1_grp$results %>%
  lapply(function(x){x$summary.statistic}) %>% do.call(rbind, .)%>%
  map_to_file(paste0("./output/msni/final/","summary_stats_admin1_grp_RAW_",format(Sys.time(), "%Y%m%d"),"_2.csv"))

#### ADMIN 1 Urban Rural ####
analysisplan_admin_1_urbanrural <- make_analysis_plan_template(df= response_updated_cluster_msni,
                                                               questionnaire = questionnaire,
                                                               repeat.for.variable = "admin_1",
                                                               independent.variable = "urban_rural",
                                                               hypothesis.type = "group_difference",
                                                               template_file = template_analysisplan_file
)
analysisplan_admin_1_urbanrural <- analysisplan_admin_1_urbanrural[!analysisplan_admin_1_urbanrural$dependent.variable %in% unwanted_cols,]
analysisplan_admin_1_urbanrural <- analysisplan_admin_1_urbanrural[!is.na(analysisplan_admin_1_urbanrural$dependent.variable.type),]
# To analyse only one variable : 
# analysisplan_admin_1_urbanrural <- analysisplan_admin_1_urbanrural[(analysisplan_admin_1_urbanrural$dependent.variable == "score_nfi_tot") ,]

final_result_admin_1_urbanrural <- from_analysisplan_map_to_output(data = response_updated_cluster_msni, 
                                                                   analysisplan = analysisplan_admin_1_urbanrural, 
                                                                   weighting = weighting_combined, 
                                                                   cluster_variable_name = "clusters",
                                                                   questionnaire = questionnaire)

summary.stats_admin1_urbanrural <- final_result_admin_1_urbanrural$results %>%
  lapply(function(x){x$summary.statistic}) %>% do.call(rbind, .)%>%
  map_to_file(paste0("./output/msni/final/","summary_stats_admin1_urbanrural_RAW_",format(Sys.time(), "%Y%m%d"),"_2.csv"))


#### ADMIN 2 ####
analysisplan_admin_2 <- make_analysis_plan_template(df= response_updated_cluster_msni,
                                                    repeat.for.variable = "admin_2",
                                                    questionnaire = questionnaire,
                                                    hypothesis.type = "direct_reporting",
                                                    template_file = template_analysisplan_file
)

analysisplan_admin_2 <- analysisplan_admin_2[!analysisplan_admin_2$dependent.variable %in% unwanted_cols,]
analysisplan_admin_2 <- analysisplan_admin_2[!is.na(analysisplan_admin_2$dependent.variable.type),]
# To analyse only one variable : 
# analysisplan_admin_2 <- analysisplan_admin_2[(analysisplan_admin_2$dependent.variable == "score_nfi_tot") ,]

final_result_admin_2 <- from_analysisplan_map_to_output(data = response_updated_cluster_msni, 
                                                        analysisplan = analysisplan_admin_2, 
                                                        weighting = weighting_combined, 
                                                        cluster_variable_name = "clusters",
                                                        questionnaire = questionnaire)

summary.stats_admin2 <- final_result_admin_2$results %>%
  lapply(function(x){x$summary.statistic}) %>% do.call(rbind, .)%>%
  map_to_file(paste0("./output/msni/final/","summary_stats_admin2_RAW_",format(Sys.time(), "%Y%m%d"),"_2.csv"))






### Have 3 final scripts:
##### LOADING Agg Tables ####
table_raw_admin0 <- read.csv(paste0("./output/msni/final/summary_stats_admin0_RAW_", format(Sys.time(), "%Y%m%d"),"_2.csv"), stringsAsFactors = F)
table_raw_admin0_grp <- read.csv(paste0("./output/msni/final/summary_stats_admin0_grp_RAW_", format(Sys.time(), "%Y%m%d"),"_2.csv"), stringsAsFactors = F)
table_raw_admin0_urbanrural <- read.csv(paste0("./output/msni/final/summary_stats_admin0_urbanrural_RAW_", format(Sys.time(), "%Y%m%d"),"_2.csv"), stringsAsFactors = F)
table_raw_admin0_grp_urbanrural <- read.csv(paste0("./output/msni/final/summary_stats_admin0_grp_urbanrural_RAW_", format(Sys.time(), "%Y%m%d"),"_2.csv"), stringsAsFactors = F)

table_raw_admin1 <- read.csv(paste0("./output/msni/final/summary_stats_admin1_RAW_", format(Sys.time(), "%Y%m%d"),"_2.csv"), stringsAsFactors = F)
table_raw_admin1_grp <- read.csv(paste0("./output/msni/final/summary_stats_admin1_grp_RAW_", format(Sys.time(), "%Y%m%d"),"_2.csv"), stringsAsFactors = F)
table_raw_admin1_urbanrural <- read.csv(paste0("./output/msni/final/summary_stats_admin1_urbanrural_RAW_", format(Sys.time(), "%Y%m%d"),"_2.csv"), stringsAsFactors = F)

table_raw_admin2 <- read.csv(paste0("./output/msni/final/summary_stats_admin2_RAW_", format(Sys.time(), "%Y%m%d"),"_2.csv"), stringsAsFactors = F)

##### BIND admin0 agg tables ####
full_table_admin0 <- table_raw_admin0%>%
  mutate(independent.var = "total", independent.var.value = "total")%>%
  rbind(table_raw_admin0_grp, table_raw_admin0_urbanrural, table_raw_admin0_grp_urbanrural)%>%
  select(dependent.var, dependent.var.value, independent.var, independent.var.value, repeat.var, repeat.var.value, numbers)

final_admin0_alldataframes <- full_table_admin0 

write.csv(final_admin0_alldataframes, paste0("./output/msni/final/admin_0_all_vars_aggregation", format(Sys.time(), "%Y%m%d"),"_2.csv"))


##### BIND admin1 agg tables ####
full_table_admin1 <- table_raw_admin1%>%
  mutate(independent.var = "total", independent.var.value = "total")%>%
  rbind(table_raw_admin1_grp, table_raw_admin1_urbanrural)%>%
  select(dependent.var, dependent.var.value, independent.var, independent.var.value, repeat.var, repeat.var.value, numbers)

final_admin1_alldataframes <- full_table_admin1 

write.csv(final_admin1_alldataframes, paste0("./output/msni/final/admin_1_all_vars_aggregation", format(Sys.time(), "%Y%m%d"),"_2.csv"))

##### BIND admin2 agg tables ####

full_table_admin2 <- table_raw_admin2%>%
  mutate(independent.var = "total", independent.var.value = "total")%>%
  select(dependent.var, dependent.var.value, independent.var, independent.var.value, repeat.var, repeat.var.value, numbers)

final_admin2_alldataframes <- full_table_admin2 

write.csv(final_admin2_alldataframes, paste0("./output/msni/final/admin_2_all_vars_aggregation", format(Sys.time(), "%Y%m%d"),"_2.csv"))


