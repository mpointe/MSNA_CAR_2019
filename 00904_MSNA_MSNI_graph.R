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
if (!require("processx")) install.packages("processx")
library(surveyweights)
library(processx)
# install.packages("plotly")
library(plotly)
library(msni19)
 # remotes::install_github("caldwellst/msni19")
# remotes::install_github("ellieallien/surveyweights")



#### READING LATEST DATAFRAME !
response_updated_cluster_msni <- read.csv(paste0("./output/msni/final/Dataset_MSNI_RAW_20191210.csv"), stringsAsFactors = TRUE)
# response_updated_cluster_msni_test <- read.csv(paste0("./output/msni/final/Dataset_MSNI_RAW_20191203.csv"), stringsAsFactors = TRUE)




# chart
index_chart(response_updated_cluster_msni,
            group = "ig_8_statut_groupe",
            group_order = c("hote", "deplaces_FA", "deplaces_site", "retournes"),
            group_labels = c("Pop. non-diplacée", "PDIs en FA", "PDIs en site", "Retournés / Rapatriés"),
            index = "msni_score_final", 
            index_max = 4,
            weighting_function = weighting_combined,
            bar_graph = T,
            print_plot = T,
            plot_name = "msni_bar_2_2",
            path = "./output/msni/final/graph")
response_updated_cluster_msni$totpop = "pop_tot"
index_chart(response_updated_cluster_msni,
             group = "totpop",
            index = "msni_score_final", 
            index_max = 4,
            weighting_function = weighting_combined,
            bar_graph = T,
            # print_plot = T,
            plot_name = "msni_bar_pop_tot",
            path = "./output/msni/final/graph")

index_chart(response_updated_cluster_msni,
            group = "ig_8_statut_groupe_uran_rural",
            index = "msni_score_final", 
            index_max = 4,
            weighting_function = weighting_combined,
            bar_graph = T,
            print_plot = T,
            plot_name = "msni_bar_group_urban_rural",
            path = "./output/msni/final/graph")

sunburst_msni(response_updated_cluster_msni, msni = "msni_score_final", fsl_lsg = "fsl", health_lsg = "health", protection_lsg = "protection",
              shelter_lsg = "shelter", wash_lsg = "wash", capacity_gaps = "capacity_gaps", impact = "impact_final",
              fsl_wash_branch = T,
              # impact_branch = T,
              # health_prot_shelt_branch = T,
              msni_filter = c(3,4),
              weighting_function = weighting_combined,
              print_plot = T,
              plot_name = "msni_sunburst_HH_3+",
              path = "./output/msni/final/graph")
# language = "fr")

sunburst_msni(dplyr::filter(response_updated_cluster_msni, urban_rural == "urban"), 
              msni = "msni_score_final", fsl_lsg = "fsl", health_lsg = "health", protection_lsg = "protection",
              shelter_lsg = "shelter", wash_lsg = "wash", capacity_gaps = "capacity_gaps", impact = "impact_final",
              fsl_wash_branch = T,
              health_prot_shelt_branch = F,
              msni_filter = c(3,4),
              weighting_function = weighting_combined,
              print_plot = T,
              plot_name = "msni_sunburst_Urban_3+",
              path = "./output/msni/final/graph")

sunburst_msni(dplyr::filter(response_updated_cluster_msni, urban_rural == "rural"), 
              msni = "msni_score_final", fsl_lsg = "fsl", health_lsg = "health", protection_lsg = "protection",
              shelter_lsg = "shelter", wash_lsg = "wash", capacity_gaps = "capacity_gaps", impact = "impact_final",
              fsl_wash_branch = T,
              health_prot_shelt_branch = F,
              msni_filter = c(3,4),
              weighting_function = weighting_combined,
              print_plot = T,
              plot_name = "msni_sunburst_Rural_3+",
              path = "./output/msni/final/graph")

?msni19::radar_graph


# response_updated_cluster_msni <- response_updated_cluster_msni %>%
#   ungroup(ig_8_statut_groupe) 
# %>%
#   mutate(ig_8_statut_groupe = factor(ig_8_statut_groupe,levels = c("deplaces_FA","deplaces_site","hote", "returnees"),
#                       labels = c("deplaces_FA","deplaces_site","hote", "returnees") ) )



response_updated_cluster_msni <- response_updated_cluster_msni%>%
  ungroup()

# response_updated_cluster_msni <- ungroup(response_updated_cluster_msni)

radar_graph(response_updated_cluster_msni, lsg = c("education", "shelter", "fsl", "health", "protection", "wash"), #, "impact_final"),
            lsg_labels = c("Education",
               "Shelter\n& NFI",
               "Food \nSecurity",
               "Health",
               "Protection",
               "WASH"),
               # "Capacity\ngaps",
               # "Impact"),
            group = "ig_8_statut_groupe",
            group_order = c("hote", "deplaces_FA", "deplaces_site", "retournes"),
            group_labels = c("Non-displaced pop.", "Hosted IDPs", "IDPs on site", "Returnees / Repatriates"),
            weighting_function = weighting_combined,
            legend_position = "right",
            legend_text_size = 12,
            # print_plot = T,
            plot_name = "msni_radar_grp",
            path = "./output/msni/final/graph")

radar_graph(response_updated_cluster_msni, lsg = c("education", "shelter", "fsl", "health", "protection", "wash"),# "capacity_gaps", "impact_final"),
            lsg_labels = c("Education",
                           "Shelter\n& NFI",
                           "Food \nSecurity",
                           "Health",
                           "Protection",
                           "WASH"),
                           # "Capacity gaps",
                           # "Impact"),
                        group = "ig_8_statut_groupe_uran_rural",
            group_order = c("hote_rural", "hote_urban", "deplaces_FA_rural", "deplaces_FA_urban", 
                            "deplaces_site_rural", "deplaces_site_urban", "retournes_rural", "retournes_urban"),
            group_labels = c("Non-displaced pop. - rural", "Non-displaced pop. - urban", 
                             "Hosted IDPs - rural", "Hosted IDPs - urban", 
                             "IDPs on site - rural", "IDPs on site - urban", 
                             "Returnees / Repatriates - rural", "Returnees / Repatriates - urban"),
            weighting_function = weighting_combined,
            legend_position = "right",
            legend_text_size = 14,
            # print_plot = T,
            plot_name = "msni_radar_grp_urban_rural_noimp_nocap",
            path = "./output/msni/final/graph")

radar_graph(response_updated_cluster_msni, lsg = c("education", "shelter", "fsl", "health", "protection", "wash"),#,"capacity_gaps",  "impact_final"),
            lsg_labels = c("Education",
                           "Shelter\n& NFI",
                           "Food Security",
                           "Health",
                           "Protection",
                           "WASH"),
                           # "Capacity gaps",
                           # "Impact"),
            group = "urban_rural",
            group_order = c("rural", "urban"),
            group_labels = c("Rural areas", "Urban areas"),
            weighting_function = weighting_combined,
            legend_position = "right",
            # print_plot = T,
            plot_name = "msni_radar_urban_rural_noimp_cap",
            path = "./output/msni/final/graph")



remotes::install_github("ellieallien/Setviz")
library(Setviz)

?Setviz::plot_set_percentages


response_updated_cluster_msni <- response_updated_cluster_msni%>%
  mutate(
    Education = if_else(education >= 3, 1, 0),
    Shelter = if_else(shelter >= 3, 1, 0),
    Food_Security = if_else(fsl >= 3, 1, 0),
    Health = if_else(health >= 3, 1, 0),
    Protection = if_else(protection >= 3, 1, 0),
    WASH = if_else(wash >= 3, 1, 0),
    Impact = if_else(impact_final >= 3, 1, 0),
    Capacity_Gaps = if_else(capacity_gaps >= 3,1,0)
    )


plot_set_percentages(data = response_updated_cluster_msni,
                     varnames = c("Education", "Shelter", "Food_Security", "Health", "Protection", "WASH", "Impact", "Capacity_Gaps"),#,"capacity_gaps_plot", 
                     # label = c("Education","Shelter", "Food Security", "Health", "Protection", "WASH"),
                     weight_variable = "weights_sampling",
                       nintersects = 12,
                    mutually_exclusive_sets = TRUE,
                    exclude_unique = FALSE,
                    round_to_1_percent = FALSE)
plot_set_percentages(data = response_updated_cluster_msni,
                     varnames = c("Education", "Shelter", "Food_Security", "Health", "Protection", "WASH"),#, "Impact", "Capacity_Gaps")
                     # label = c("Education","Shelter", "Food Security", "Health", "Protection", "WASH"),
                     weight_variable = "weights_sampling",
                     nintersects = 12,
                     mutually_exclusive_sets = TRUE,
                     exclude_unique = FALSE,
                     round_to_1_percent = FALSE)
?plot_set_percentages
