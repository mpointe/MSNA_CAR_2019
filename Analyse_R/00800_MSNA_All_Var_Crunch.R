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


#### READING LATEST DATAFRAME FROM TODAY!!
response_updated_cluster <- read.csv(paste0("./output/REACH_CAR_dataset_HH_MSNA_20191212_weights_ok.csv"), stringsAsFactors = TRUE)
 
# response_updated_cluster_test <- response_updated_cluster %>%
#   mutate(
#     score_casserole = case_when(
#       nb_casseroles >= 0 & nb_casseroles <1 ~ 5,
#       nb_casseroles >= 1 & nb_casseroles <2 ~ 4,
#       nb_casseroles >= 2 & nb_casseroles <3 ~ 3.5,
#       nb_casseroles >= 3 & nb_casseroles <4 ~ 2,
#       nb_casseroles >= 4 & nb_casseroles <5 ~ 1,
#       nb_casseroles >= 5 ~ 0),
#     score_seau = case_when(
#       nb_seaux >= 0 & nb_seaux < 1 ~ 5,
#       nb_seaux >= 1 & nb_seaux < 2 ~ 3.5,
#       nb_seaux >= 2 & nb_seaux < 3 ~ 2.5,
#       nb_seaux >= 3 & nb_seaux < 4 ~ 1, ## !! Has been corrected (before: >=3 & <3, then 1 and > 3, then 0) HAS TO BE RERUNNED ! 
#       nb_seaux >= 4 ~ 0
#     )
#   )%>%
#   mutate(
#     score_nfi_tot = (score_bidons+ score_seau+ score_moust+ score_casserole+ score_couchage+ score_draps)/6,
#     sphere_nfi = if_else(is.na(volume_total_recipients+ nb_casseroles+ nb_couchage+ nb_draps+ nb_mousticaire), "NA",
#                          if_else(volume_total_recipients >= 30 & nb_casseroles >= 2 & nb_couchage >=3 & nb_draps >=3 & nb_mousticaire >= 3, "oui", "non")),
#     pin_score_nfi = if_else(score_nfi_tot >= 3.9, "5",
#                             if_else(score_nfi_tot >= 3, "4",
#                                     if_else(score_nfi_tot >= 2, "3",
#                                             if_else(score_nfi_tot >= 1, "2",
#                                                     if_else(score_nfi_tot <= 1 & score_nfi_tot >=0, "1",
#                                                             NA_character_)
#                                             ))))
#   )
# 

# template_analysisplan_file <- "./input/analysisplan_template_pin_score_nfi.csv"
template_analysisplan_file <- "./input/analysisplan_template_updated.csv"

unwanted_cols <- unique(tolower(c("x","start", "end", "today", "q0_2_date", "consensus_note", "village",  "q0_1_enqueteur","village_autre", "ig_11_IDP_RtL_autre", 
                                  "ig_14_IDP_cond_retour_autre","ig_16_Ret_Rapat_abri_origine_non_raison_autre", "ig_15_IDP_RtR_Ret_Rapat_autre",
                                  "sante_1_accouch_autre","sante_1_accouch_maison_autre","sante_2_soin_recu_autre",
                                  "sante_3_soin_non_recu_autre","sante_4_0_4_malades_autre",  "sante_5_5plus_malades_autre",
                                  "educ_4_handi_acces_autre",   "protect_10_autre",           "educ_5_ecole_acces_autre",
                                  "nfi_2_1_type_abri_autre",    "nfi_propr_abri_autre",       "mssc_2_source_rev_autre",
                                  "mssc_4_dep_6M_autre","secal_6_agric_raisons_autre","wash_1_source_boisson_autre",
                                  "wash_2_source_autre_usage_autre", "wash_9_insuff_raisons_certains_groupes_autre",  "wash_9_insuff_raisons_autre",
                                  "wash_15_insuff_raisons_certains_groupes_autre", "wash_15_insuff_raisons_autre","wash_20_autres_autre",
                                  "sante_5_deces_relation_autre", "sante_5_deces_cause_autre",  "protect_2_femmes_risque_autre",
                                  "protect_2_hommes_risque_autre","protect_3_filles_risque_autre","protect_3_garcons_risque_autre",
                                  "protect_5_1_travail_force_autre", "protect_8_2_autre","protect_13_autre",
                                  "aap_1_1_source_confiance_autre", "aap_2_1_type_information_autre", "aap_3_canal_information_enpersonne",
                                  "aap_3_canal_information_autre", "aap_4_retour_fournisseurs_aide_autre", "educ_6_reponse_autre",
                                  "nfi_7_assistance_autre",     "secal_13_reponse_autre",     "wash_22_autres_autre",
                                  "sante_7_reponse_autre",      "note_comm_end", "sante_1_accouch_maison_raison_autre", "sum_sante_1_accouch_autre"
)))

#### ADMIN 0 ####
response_updated_cluster$admin_0 <- "RCA"

analysisplan_admin_0 <- make_analysis_plan_template(df= response_updated_cluster,
                                                    repeat.for.variable = "admin_0",
                                                    questionnaire = questionnaire,
                                                    hypothesis.type = "direct_reporting",
                                                    template_file = template_analysisplan_file)

analysisplan_admin_0 <- analysisplan_admin_0[!analysisplan_admin_0$dependent.variable %in% unwanted_cols,]
analysisplan_admin_0 <- analysisplan_admin_0[!is.na(analysisplan_admin_0$dependent.variable.type),]
# To analyse only one variable : 
 # analysisplan_admin_0 <- analysisplan_admin_0[(analysisplan_admin_0$dependent.variable == "score_nfi_tot") ,]

running_timezz <- data.frame(matrix(ncol = 4, nrow = 6) )

names(running_timezz) <- c("Level", "Start", "End", "Running_time")

### CRUNCH
start_time_admin0 <- Sys.time()

final_result_admin_0 <- from_analysisplan_map_to_output(data = response_updated_cluster, 
                                                        analysisplan = analysisplan_admin_0, 
                                                        weighting = weighting_combined, 
                                                        cluster_variable_name = "clusters",
                                                        questionnaire = questionnaire)

end_time_admin0 <- Sys.time()

# summary.stats_admin0 <- final_result_admin_0$results %>%
#   lapply(function(x){x$summary.statistic}) %>% do.call(rbind, .)

summary.stats_admin0 <- final_result_admin_0$results %>%
  lapply(function(x){x$summary.statistic}) %>% do.call(rbind, .)%>%
  map_to_file(paste0("./output/tables/RAW/","summary_stats_admin0_RAW_",format(Sys.time(), "%Y%m%d"),".csv"))


### ADMIN 0 GROUPE

analysisplan_admin_0_grp <- make_analysis_plan_template(df= response_updated_cluster,
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

start_time_admin0_grp <- Sys.time()

final_result_admin_0_grp <- from_analysisplan_map_to_output(data = response_updated_cluster, 
                                                            analysisplan = analysisplan_admin_0_grp, 
                                                            weighting = weighting_combined, 
                                                            cluster_variable_name = "clusters",
                                                            questionnaire = questionnaire)

end_time_admin0_grp <- Sys.time()

summary.stats_admin0_grp <- final_result_admin_0_grp$results %>%
  lapply(function(x){x$summary.statistic}) %>% do.call(rbind, .)%>%
  map_to_file(paste0("./output/tables/RAW/","summary_stats_admin0_grp_RAW_",format(Sys.time(), "%Y%m%d"),".csv"))
  

#### ADMIN 0 sex HHD

analysisplan_admin_0_sexHHD <- make_analysis_plan_template(df= response_updated_cluster,
                                                        questionnaire = questionnaire,
                                                        repeat.for.variable = "admin_0",
                                                        independent.variable = "sexe_chef_menage",
                                                        hypothesis.type = "group_difference",
                                                        template_file = template_analysisplan_file
)
analysisplan_admin_0_sexHHD <- analysisplan_admin_0_sexHHD[!analysisplan_admin_0_sexHHD$dependent.variable %in% unwanted_cols,]
analysisplan_admin_0_sexHHD <- analysisplan_admin_0_sexHHD[!is.na(analysisplan_admin_0_sexHHD$dependent.variable.type),]
# To analyse only one variable : 
 # analysisplan_admin_0_sexHHD <- analysisplan_admin_0_sexHHD[(analysisplan_admin_0_sexHHD$dependent.variable == "score_nfi_tot") ,]


start_time_admin0_sexHHD <- Sys.time()

final_result_admin_0_sexHHD <- from_analysisplan_map_to_output(data = response_updated_cluster, 
                                                            analysisplan = analysisplan_admin_0_sexHHD, 
                                                            weighting = weighting_combined, 
                                                            cluster_variable_name = "clusters",
                                                            questionnaire = questionnaire)

end_time_admin0_sexHHD <- Sys.time()

summary.stats_admin0_sexHHD <- final_result_admin_0_sexHHD$results %>%
  lapply(function(x){x$summary.statistic}) %>% do.call(rbind, .)%>%
  map_to_file(paste0("./output/tables/RAW/","summary_stats_admin0_sexHHD_RAW_",format(Sys.time(), "%Y%m%d"),".csv"))


#### ADMIN 1 ####


analysisplan_admin_1 <- make_analysis_plan_template(df= response_updated_cluster,
                                                    repeat.for.variable = "admin_1",
                                                    questionnaire = questionnaire,
                                                    hypothesis.type = "direct_reporting",
                                                    template_file = template_analysisplan_file
)
analysisplan_admin_1 <- analysisplan_admin_1[!row.names(analysisplan_admin_1) %in% unwanted_cols,]
analysisplan_admin_1 <- analysisplan_admin_1[!is.na(analysisplan_admin_1$dependent.variable.type),]
# To analyse only one variable : 
 # analysisplan_admin_1 <- analysisplan_admin_1[(analysisplan_admin_1$dependent.variable == "score_nfi_tot") ,]

start_time_admin1 <- Sys.time()
final_result_admin_1 <- from_analysisplan_map_to_output(data = response_updated_cluster, 
                                                        analysisplan = analysisplan_admin_1, 
                                                        weighting = weighting_combined, 
                                                        cluster_variable_name = "clusters",
                                                        questionnaire = questionnaire)
end_time_admin1 <- Sys.time()

summary.stats_admin1 <- final_result_admin_1$results %>%
  lapply(function(x){x$summary.statistic}) %>% do.call(rbind, .)%>%
  map_to_file(paste0("./output/tables/RAW/","summary_stats_admin1_RAW_",format(Sys.time(), "%Y%m%d"),".csv"))
  

#### ADMIN 1 GROUPE ####

analysisplan_admin_1_grp <- make_analysis_plan_template(df= response_updated_cluster,
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

start_time_admin1_grp <- Sys.time()
final_result_admin_1_grp <- from_analysisplan_map_to_output(data = response_updated_cluster, 
                                                            analysisplan = analysisplan_admin_1_grp, 
                                                            weighting = weighting_combined, 
                                                            cluster_variable_name = "clusters",
                                                            questionnaire = questionnaire)

end_time_admin1_grp <- Sys.time()

summary.stats_admin1_grp <- final_result_admin_1_grp$results %>%
  lapply(function(x){x$summary.statistic}) %>% do.call(rbind, .)%>%
  map_to_file(paste0("./output/tables/RAW/","summary_stats_admin1_grp_RAW_",format(Sys.time(), "%Y%m%d"),".csv"))


#### ADMIN 2 ####
# response_updated_cluster = subset(response_updated_cluster, response_updated_cluster$admin2_labels == "Bangui")
# response_updated_cluster$admin_2 = if_else(response_updated_cluster$localite_final_labels_admin2 == "bangui3ebangui", "Bangui_3e", "Bangui_245678e")

analysisplan_admin_2 <- make_analysis_plan_template(df= response_updated_cluster,
                                                    repeat.for.variable = "admin_2",
                                                    questionnaire = questionnaire,
                                                    hypothesis.type = "direct_reporting",
                                                    template_file = template_analysisplan_file
)

analysisplan_admin_2 <- analysisplan_admin_2[!analysisplan_admin_2$dependent.variable %in% unwanted_cols,]
analysisplan_admin_2 <- analysisplan_admin_2[!is.na(analysisplan_admin_2$dependent.variable.type),]
# To analyse only one variable : 
 # analysisplan_admin_2 <- analysisplan_admin_2[(analysisplan_admin_2$dependent.variable == "score_nfi_tot") ,]

start_time_admin2 <- Sys.time()

final_result_admin_2 <- from_analysisplan_map_to_output(data = response_updated_cluster, 
                                                        analysisplan = analysisplan_admin_2, 
                                                        weighting = weighting_combined, 
                                                        cluster_variable_name = "clusters",
                                                        questionnaire = questionnaire)
end_time_admin2 <- Sys.time()

summary.stats_admin2 <- final_result_admin_2$results %>%
  lapply(function(x){x$summary.statistic}) %>% do.call(rbind, .)%>%
  map_to_file(paste0("./output/tables/RAW/","summary_stats_admin2_RAW_",format(Sys.time(), "%Y%m%d"),".csv"))

running_timezz <- data.frame(matrix(ncol = 4, nrow = 6) )

names(running_timezz) <- c("Level", "Start", "End", "Running_time")

running_timezz$Level <- c("admin0", "admin0_grp", "admin0_sexHHD", "admin1", "admin1_grp", "admin2")
running_timezz$Start <- c(start_time_admin0, start_time_admin0_grp, start_time_admin0_sexHHD, start_time_admin1, start_time_admin1_grp, start_time_admin2)
running_timezz$End <- c(end_time_admin0, end_time_admin0_grp, end_time_admin0_sexHHD, end_time_admin1, end_time_admin1_grp, end_time_admin2)
running_timezz <- running_timezz%>%
  mutate(Start = as.POSIXct(Start))%>%
  mutate(End = as.POSIXct(End))%>%
  mutate(Running_time = difftime(End, Start))

running_timezz                                 

