# setup

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
#devtools::install_github('mabafaba/composr') 
library(parallel)
library(knitr)

#source("functions/to_alphanumeric_lowercase.R") # function to standardise column headers (like check.names)
source("functions/analysisplan_factory.R")  # generate analysis plans
source("functions/remove_responses_from_sumstat.R")  # generate analysis plans
### source("SOME_NGA_SPECIFIC_FUNCTIONS")

# load questionnaire inputs
questions <- read.csv("input/questionnaire_kobo_hh_combine_v4_FINAL_survey_OK.csv", 
                      stringsAsFactors=F, check.names = F)

choices <- read.csv("input/questionnaire_kobo_hh_combine_v4_FINAL_choices_OK.csv", 
                    stringsAsFactors=F, check.names = F)

choices <- read.csv("input/questionnaire_kobo_hh_combine_v4_FINAL_TEST_choices.csv", 
                    stringsAsFactors=F, check.names = F)


## CAREFUL : have some " " at the end of some options. Replace them with notihng :
choices$list_name %<>% gsub(" ", "", .)

### remove choice une ligne en trop 
#choices <- choices[-252,]

#generate data
#response <- xlsform_fill(questions,choices,200)

response_dryrun <- xlsform_fill(questions,choices,200)

response_dryrun <- response_dryrun[,-523]

response_dryrun <- response_dryrun %>% as.data.frame

response_dryrun <- response_dryrun %>% mutate_if(is.factor, as.character)
#colnames = colnames(response_dryrun)
#response_dryrun = data.frame(matrix(unlist((response_dryrun)), ncol =length(response_dryrun), byrow=F), stringsAsFactors=FALSE)
#colnames(response_dryrun) = colnames
#machin <- as.data.frame(list_corr)
#list_corr$consensus_note = "oui"
#list_corr -> response_dryrun

#response_testBangui_0701 <- read.csv("input/questionnaire_test_OK_0701.csv", 
#                                stringsAsFactors=F, check.names = F)
#
#response_testBangui_0702 <- read.csv("input/questionnaire_test_OK_0702.csv", 
#                                     stringsAsFactors=F, check.names = F)
#

## merging Kobo data from 01/07 and 02/07

#devtools::install_github('mabafaba/mergekobodata')
#library(mergekobodata)
#
#?merge_kobo_data
#
#response_testBangui = merge_kobo_data("./input/questionnaire/", output_file = "./input/questionnaire_07_0102.csv" )
#loop_bidons_testBangui = merge_kobo_data("./input/loop_bidons/", output_file = "./input/loop_bidons_07_0102.csv" )
#loop_hh_members_testBangui = merge_kobo_data("./input/loop_hh_members/", output_file = "./input/loop_hh_members_07_0102.csv" )
#

#response_Bangui <- read.csv("./input/questionnaire_07_0102.csv", stringsAsFactors = F)

# test with hh loop added (need to run "loop_cleaning.R" file)
response = main_updated

to_alphanumeric_lowercase <-
function(x){tolower(gsub("[^a-zA-Z0-9_]", "\\.", x))}
names(response)<-to_alphanumeric_lowercase(names(response))

questionnaire <- load_questionnaire(data = response,
                                    questions = questions,
                                    choices = choices)


# check if choices for MC for variable "..." are ok in response
questionnaire$choices_for_select_multiple("protect_2_femmes_risque", response)
response$protect_2_femmes_risque_autre
response$protect_2_femmes_risque.tensions_communautes
response$protect_2_femmes_risque
choices$label[questionnaire$choices_for_select_multiple("wash_19_lavage_main_moyens", response)]
response$wash_19_lavage_main_moyens
questionnaire$choices_for_select_multiple("wash_19_lavage_main_moyens", response)
questionnaire$question_get_choice_labels(response$protect_2_femmes_risque, "protect_2_femmes_risque")
choices$list_name[questionnaire$choices_for_select_multiple("wash_19_lavage_main_moyens", response)]


# regroup Retourné & Rapatré as one category: 

response$ig_8_statut = ifelse(response$ig_8_statut == "retourne", "retourne_rapatrie",
                                ifelse(response$ig_8_statut == "rapatrie", "retourne_rapatrie", response$ig_8_statut))




# generate samplingframe
sampling.frame <- load_samplingframe(file = "./input/Copy of CAR_MSNA_Echantillonage_v4_final (00000002).csv")
sampling.frame$population = gsub(",", "", sampling.frame$population)
sampling.frame = sampling.frame[!is.na(sampling.frame$population),]

sampling.frame$population %<>% as.numeric

response$stratum_column = paste(response$admin_2, "_", response$ig_8_statut, sep="")

## From sampling frame -> no IDP en site sur Bangui... Remplacer par IDP FA
response$stratum_column = ifelse(response$stratum_column == "Bangui_IDP_site", "Bangui_IDP_FA", response$stratum_column)


# delete data from responses that are not in the sampling frame:
response = response[((response$stratum_column %in% sampling.frame$strata)),]
response = response[,-1]
#sampling.frame$strata
weighting <- map_to_weighting(sampling.frame = sampling.frame, 
                              data = response, 
                              sampling.frame.population.column ="population", 
                              sampling.frame.stratum.column = "strata",
                              data.stratum.column = "stratum_column")

### Debug fct : 
#debug(map_to_weighting)
#map_to_weighting
#debug(surveyweights::weighting_fun_from_samplingframe)
#surveyweights::weighting_fun_from_samplingframe
#undebug(surveyweights::weighting_fun_from_samplingframe)
#undebug(map_to_weighting)
### or use : 
#debugonce()
## and then do not need to undebug. 



# add cluster ids

#design <- map_to_design(data = response, cluster_variable_name = "cluster", weighting_function = weighting)
#design <- map_to_design(data = response, cluster_variable_name = "cluster", weighting_function = weighting)


analysisplan_admin_1 <- make_analysisplan_all_vars(df= response, 
                                           questionnaire = questionnaire, 
                                           repeat.for.variable = "admin_1", 
                                           independent.variable = "ig_8_statut", 
                                           hypothesis.type = "group_difference")


analysisplan_admin_2 <- make_analysisplan_all_vars(df= response, 
                                               questionnaire = questionnaire, 
                                               repeat.for.variable = "admin_2", 
                                               hypothesis.type = "direct_reporting")


#case <- map_to_case("group_difference", "categorical", "categorical")
#result <- map_to_result(data = response, 
#                        dependent.var = "ig_2_sexe", 
#                        independent.var = "ig_8_statut", 
#                        case = case, 
#                        cluster.variable.name = "cluster", 
#                        weighting = weighting, 
#                        questionnaire = questionnaire)


# Calculate the final results
final_result_admin_1 <- from_analysisplan_map_to_output(data = response, 
                                                analysisplan = analysisplan_admin_1, 
                                                weighting = weighting, 
                                   #             cluster_variable_name = "cluster",
                                                questionnaire = questionnaire)
final_result_admin_2 <- from_analysisplan_map_to_output(data = response, 
                                                analysisplan = analysisplan_admin_2, 
                                                weighting = weighting, 
                                                #             cluster_variable_name = "cluster",
                                                questionnaire = questionnaire)

#undebug(from_analysisplan_map_to_output)
from_analysisplan_map_to_output(data = response, 
                                analysisplan = analysisplan_admin_1, 
                                weighting = weighting, 
                                #             cluster_variable_name = "cluster",
                                questionnaire = questionnaire)
#?from_analysisplan_map_to_output

# Print a massive table with everything (summary stats and p values)
final_result$results %>% map_to_master_table(., filename= "./master_table.csv", questionnaire = questionnaire)


#case <- map_to_case("group_difference", "categorical" , "categorical")

summary.stats <- final_result$results %>% lapply(function(x){map_to_labeled(result = x, questionnaire = questionnaire)}) %>% 
  lapply(function(x){x$summary.statistic}) %>% do.call(rbind, .) %>% map_to_file("./summary_stats.csv")


final_result %>% map_to_template( questionnaire = questionnaire, dir = "./output", type = "visual", "report.html")

result %>% map_to_visualisation


analysisplan <- analysisplan[1:10]



# not sure if this function should be "user facing" or have some wrappers (@Bouke thoughts?)
# essentially it handles all the looping over different column values as hierarchies.
# then each result is visualised by a function passed here that decides how to render each individual result
# see ?hypegrammaR:::map_to_generic_hierarchical_html
hypegrammaR:::map_to_generic_hierarchical_html(final_result,
                                               render_result_with = hypegrammaR:::from_result_map_to_md_table,
                                               by_analysisplan_columns = c("dependent.var","repeat.var.value"),
                                               by_prefix =  c("",""),
                                               level = 2,
                                               questionnaire = questionnaire,
                                               label_varnames = TRUE,
                                               dir = "./output",
                                               filename = "summary_by_dependent_var_then_by_repeat_var.html")

browseURL("output/summary_by_dependent_var_then_by_repeat_var.html")

map_to_summary_table(results,"summarized_group.csv", questionnaire = questionnaire)



# not sure this is working correctly.. next on agenda (:
# big_table <- hypegrammaR:::map_to_datamerge(results$results, questionnaire = questionnaire, rows = "repeat.var.value")

