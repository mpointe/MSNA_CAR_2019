response <- read.csv("output/MSNA_HH_Analysed_data.csv", stringsAsFactors = F, encoding = "UTF-8")

response_updated_cluster <- read.csv(paste0("./output/REACH_CAR_dataset_HH_MSNA_20190920.csv"), stringsAsFactors = FALSE)


## Check number of qst by group and loc:

# have retournÃ©s and rapatriÃ©s together:
response_updated_cluster$ig_8_statut_final = ifelse(response_updated_cluster$ig_8_statut == "hote", response_updated_cluster$ig_8_statut, 
                                            ifelse(response_updated_cluster$ig_8_statut == "IDP_FA",  response_updated_cluster$ig_8_statut, 
                                                   ifelse(response_updated_cluster$ig_8_statut == "IDP_site", response_updated_cluster$ig_8_statut, "Ret_Rap")))
data_clean_sampling_admin2 = as.data.frame(table(response_updated_cluster$admin_2))
data_clean_sampling_admin1 = as.data.frame.matrix(table(response_updated_cluster$admin_1, response_updated_cluster$ig_8_statut_final))

# Import data 
admin1quota <- read.csv("./input/MSNA_admin1quota_R.csv", stringsAsFactors = F)
admin1quota = admin1quota[-18,]
admin2quota <- read.csv("./input/MSNA_admin2quota_R.csv", stringsAsFactors = F)

admin1quota = admin1quota[order(admin1quota$Prefecture),]

checkquota1 = admin1quota
checkquota1$sampling_hote = data_clean_sampling_admin1$hote
checkquota1$sampling_IDPFA = data_clean_sampling_admin1$IDP_FA
checkquota1$sampling_IDPSite = data_clean_sampling_admin1$IDP_site
checkquota1$sampling_Retrap = data_clean_sampling_admin1$Ret_Rap

checkquota1 = checkquota1[,c(1,2,6,3,7,4,8,5,9)]


checkquota2 = admin2quota
checkquota2 = checkquota2[order(checkquota2$Sous_prefecture),]
data_clean_sampling_admin2 = data_clean_sampling_admin2[order(data_clean_sampling_admin2$Var1),]
checkquota2$sampling = data_clean_sampling_admin2

write.csv(checkquota1, "./output/checkquota1_2109.csv")
write.csv(checkquota2, "./output/checkquota2_2109.csv")





## visited localities to find urbal vs. rural split : 

response_updated_cluster <- read.csv(paste0("./output/REACH_CAR_dataset_HH_MSNA_20190927.csv"), stringsAsFactors = FALSE)

response_updated_cluster$localite_final_labels_admin2
response_updated_cluster$localite_final_labels_2
response_updated_cluster$localite_final_labels
response_updated_cluster$admin_2

response_updated_cluster$localites_visitees_labels


localite_visitee = as.data.frame(unique(response_updated_cluster$localite_final_labels_admin2))
for (i in 1:nrow(localite_visitee)){
  localite_visitee$localites_visitees_labels[i] = subset(response_updated_cluster, response_updated_cluster$localite_final_labels_admin2 == localite_visitee$`unique(response_updated_cluster$localite_final_labels_admin2)`[i])$localites_visitees_labels
  localite_visitee$admin2[i] = subset(response_updated_cluster, response_updated_cluster$localite_final_labels_admin2 == localite_visitee$`unique(response_updated_cluster$localite_final_labels_admin2)`[i])$admin_2
}
localite_visitee = as.data.frame(localite_visitee)


write.csv(localite_visitee, "./output/localite_visitee.csv")


subset(response_updated_cluster, response_updated_cluster$X_parent_index == 4512)$localite
subset(response_updated_cluster, response_updated_cluster$X_parent_index == 4512)$localite_final_labels_admin2
subset(response_updated_cluster, response_updated_cluster$X_parent_index == 1611)$localite_final_labels_admin2
subset(response_updated_cluster, response_updated_cluster$X_parent_index == 1621)$localite_final_labels_admin2

