setwd("C:/Users/REACH-RCA-AO/Documents/GitHub/MSNA_CAR_2019/")
library(dplyr)
library(koboquest) # manage kobo questionnairs
library(parallel) # mclapply
library(kobostandards) # check inputs for inconsistencies
#devtools::install_github('mabafaba/kobostandards')
library(xlsformfill) # generate fake data for kobo
#devtools::install_github('mabafaba/xlsformfill')
library(hypegrammaR) # stats 4 complex samples
#devtools::install_github('ellieallien/hypegrammaR')
library(composr) # horziontal operations
library(parallel)
library(koboloops)

main <- read.csv("./input/questionnaire_07_0102.csv", stringsAsFactors = F)

library(mergekobodata)
### water
merge_kobo_data("./input/loop_bidons/", filename_pattern = ".csv", output_file = "./input/merged_water.csv", write_log = T)

water <- read.csv("./input/merged_water.csv", stringsAsFactors = F)

merge_kobo_data("./input/loop_hh_members/", filename_pattern = ".csv", output_file = "./input/merged_hh.csv", write_log = T)

hh_ind <- read.csv("./input/merged_hh.csv", stringsAsFactors = F)

main$sum_agegrp_0_17_hommes %>% table

hh_ind$educ_2_inscrit_18_19_level %>% table
hh_ind$educ_3_presence_18_19 %>% table

hh_ind$agegrp_4_18 %>% table
hh_ind$sexe_hh %>% table

hh_ind$educ_2_inscrit_18_19_level %>% table
table(hh_ind$educ_2_inscrit_18_19_level, hh_ind$age_hh)

## Create binary var age and gender
hh_ind$filles_4_6 = ifelse(hh_ind$sexe_hh == "femme" & hh_ind$age_hh > 3 & hh_ind$age_hh < 7, 1, 0)
hh_ind$filles_7_12 = ifelse(hh_ind$sexe_hh == "femme" & hh_ind$age_hh > 6 & hh_ind$age_hh < 13, 1, 0)
hh_ind$filles_13_18 = ifelse(hh_ind$sexe_hh == "femme" & hh_ind$age_hh > 12 & hh_ind$age_hh < 19, 1, 0)

hh_ind$garcons_4_6 = ifelse(hh_ind$sexe_hh == "homme" & hh_ind$age_hh > 3 & hh_ind$age_hh < 7, 1, 0)
hh_ind$garcons_7_12 = ifelse(hh_ind$sexe_hh == "homme" & hh_ind$age_hh > 6 & hh_ind$age_hh < 13, 1, 0)
hh_ind$garcons_13_18 = ifelse(hh_ind$sexe_hh == "homme" & hh_ind$age_hh > 12 & hh_ind$age_hh < 19, 1, 0)


## Create binary var attending or enrolled in school
hh_ind$filles_educ_4_6 = ifelse(hh_ind$filles_4_6 & (hh_ind$educ_2_inscrit_18_19_non == 1 | hh_ind$educ_3_presence_18_19_non == 1), 1, 0)
hh_ind$filles_educ_7_12 = ifelse(hh_ind$filles_7_12 & (hh_ind$educ_2_inscrit_18_19_non == 1 | hh_ind$educ_3_presence_18_19_non == 1), 1, 0)
hh_ind$filles_educ_13_18 = ifelse(hh_ind$filles_13_18 & (hh_ind$educ_2_inscrit_18_19_non == 1 | hh_ind$educ_3_presence_18_19_non == 1), 1, 0)

hh_ind$garcons_educ_4_6 = ifelse(hh_ind$garcons_4_6 & (hh_ind$educ_2_inscrit_18_19_non == 1 | hh_ind$educ_3_presence_18_19_non == 1), 1, 0)
hh_ind$garcons_educ_7_12 = ifelse(hh_ind$garcons_7_12 & (hh_ind$educ_2_inscrit_18_19_non == 1 | hh_ind$educ_3_presence_18_19_non == 1), 1, 0)
hh_ind$garcons_educ_13_18 = ifelse(hh_ind$garcons_13_18 & (hh_ind$educ_2_inscrit_18_19_non == 1 | hh_ind$educ_3_presence_18_19_non == 1), 1, 0)




?affect_loop_to_parent

main_updated <- affect_loop_to_parent(loop = hh_ind, parent = main, aggregate.function = sum, 
                                      variable.to.add = c(sum_filles_4_6 = "filles_4_6",
                                      sum_filles_7_12 = "filles_7_12",
                                      sum_filles_13_18 = "filles_13_18",
                                      sum_garcons_4_6 = "garcons_4_6",
                                      sum_garcons_7_12 = "garcons_7_12",
                                      sum_garcons_13_18 = "garcons_13_18",
                                      sum_filles_educ_4_6 = "filles_educ_4_6",
                                      sum_filles_educ_7_12 = "filles_educ_7_12",
                                      sum_filles_educ_13_18 = "filles_educ_13_18",
                                      sum_garcons_educ_4_6 = "garcons_educ_4_6",
                                      sum_garcons_educ_7_12 = "garcons_educ_7_12",
                                      sum_garcons_educ_13_18 = "garcons_educ_13_18"
                                      ))

main_updated$sum_filles_13_18 %>% table

