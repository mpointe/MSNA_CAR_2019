# Mettre sa session R sur le chemin ou se trouve ce document
setwd("C:/Users/REACH-RCA-AO/Documents/GitHub/CAR_MSNA_HH_Analysis_FINAL/")

# install.packages("remotes") 
Sys.setenv("R_REMOTES_NO_ERRORS_FROM_WARNINGS"=TRUE)
# remotes::install_github('caldwellst/Setviz')
# install.packages("data.table")
library(data.table)

# devtools::install_github("mabafaba/msni19", build_vignettes = TRUE, build_opts = c() )
# devtools::install_github("caldwellst/msni19", build_vignettes = TRUE, build_opts = c() )
library(msni19) 
# devtools::install_github('sharonorengo/koboloops')
library(parallel) # mclapply
library(magrittr)
library(dplyr)
library(knitr)

# remotes::install_github('ellieallien/hypegrammaR')
# remotes::install_github('mabafaba/hypegrammaR')
library(hypegrammaR) # stats 4 complex samples

# remotes::install_github("mabafaba/koboquest")
library(koboquest) # manage kobo questionnairs

# remotes::install_github('mabafaba/kobostandards')
library(kobostandards) # check inputs for inconsistencies

# remotes::install_github('mabafaba/xlsformfill')
library(xlsformfill) # generate fake data for kobo

library(composr) # horziontal operations
# remotes::install_github('sharonorengo/koboloops')
library(koboloops)
# remotes::install_github('mabafaba/mergekobodata')
library(mergekobodata)
# remotes::install_github("ellieallien/cleaninginspectoR")
library(cleaninginspectoR)
#install.packages("vctrs")
library(vctrs)

# saveRDS(response_updated_cluster, "response_updated_cluster.Rda")
# saveRDS(response_updated_cluster_indiv, "response_updated_cluster_indiv.Rda")

response_updated_cluster <- read.csv(paste0("./output/REACH_CAR_dataset_HH_MSNA_20191209_weights_ok.csv"), stringsAsFactors = FALSE)


# Health:
response_updated_cluster_indiv <- read.csv(paste0("./output/REACH_CAR_dataset_Indiv_MSNA_20190925.csv"), stringsAsFactors = FALSE)
response_updated_cluster_indiv = subset(response_updated_cluster_indiv, is_in(response_updated_cluster_indiv$X_parent_index, response_updated_cluster$x_index))
response_updated_cluster_indiv$sante_4_0_4_diarrhee_tot_nonsoignee = ifelse(response_updated_cluster_indiv$sante_4_0_4_malades.diarrhee == 1
                                                                            & response_updated_cluster_indiv$agegrp_0_4 == 1
                                                                            & response_updated_cluster_indiv$sante_2_soin_recu != "cs",1,0)
response_updated_cluster_indiv$sante_5_5plus_malade_tot_nonsoignee = ifelse(response_updated_cluster_indiv$sante_5_5plus_malades != ""
                                                                            & (response_updated_cluster_indiv$agegrp_5_17 == 1 | response_updated_cluster_indiv$agegrp_18_59_tot == 1 | response_updated_cluster_indiv$agegrp_59plus_tot == 1)
                                                                            & response_updated_cluster_indiv$sante_2_soin_recu != "cs",1,0)

response_updated_cluster_indiv$sante_4_0_4_fievre_toux_tot_nonsoignee = ifelse((response_updated_cluster_indiv$sante_4_0_4_malades.fievre == 1 |
                                                                                  response_updated_cluster_indiv$sante_4_0_4_malades.toux == 1 |
                                                                                  response_updated_cluster_indiv$sante_4_0_4_malades.autre == 1)
                                                                            & response_updated_cluster_indiv$agegrp_0_4 == 1
                                                                            & response_updated_cluster_indiv$sante_2_soin_recu != "cs",1,0)  

response_updated_cluster_indiv$sante_2_malade_tot_soins_cs = ifelse(response_updated_cluster_indiv$sante_2_malade_oui == 1
                                                                            & response_updated_cluster_indiv$sante_2_soin_recu == "cs",1,0)

response_updated_cluster = response_updated_cluster_indiv %>% group_by(X_parent_index) %>%
  summarise(sum_sante_4_0_4_diarrhee_tot_nonsoignee = sum(sante_4_0_4_diarrhee_tot_nonsoignee),
            sum_sante_5_5plus_malade_tot_nonsoignee = sum(sante_5_5plus_malade_tot_nonsoignee),
            sum_sante_4_0_4_fievre_toux_tot_nonsoignee = sum(sante_4_0_4_fievre_toux_tot_nonsoignee),
            sum_sante_2_malade_tot_soins_cs = sum(sante_2_malade_tot_soins_cs)
            
  ) %>% 
  right_join(response_updated_cluster,by = c("X_parent_index"="x_index"))

#  Correct the age groups !! 
response_updated_cluster$sum_agegrp_0_4_ok = (response_updated_cluster$sum_agegrp_0_4_femmes+response_updated_cluster$sum_agegrp_0_4_hommes)
# table(response_updated_cluster$sum_agegrp_0_4 == response_updated_cluster$sum_agegrp_0_4_ok)
response_updated_cluster$ig_6_hh_membres_tot = if_else(is.na(response_updated_cluster$ig_6_hh_membres_tot), 
                                                       response_updated_cluster$sum_agegrp_0_17+response_updated_cluster$sum_agegrp_18plus,
                                                       response_updated_cluster$ig_6_hh_membres_tot)
# subset(response_updated_cluster, response_updated_cluster$ig_6_hh_membres_tot != (response_updated_cluster$sum_agegrp_0_17+response_updated_cluster$sum_agegrp_18plus))$X_parent_index
response_updated_cluster$ig_6_hh_membres_tot_ok = (response_updated_cluster$sum_agegrp_0_17+response_updated_cluster$sum_agegrp_18plus)
for(i in 1:nrow(response_updated_cluster)){
  if(response_updated_cluster$loop_without_resp[i] == 1 & response_updated_cluster$ig_1_age[i] < 19){
    response_updated_cluster$ig_6_hh_membres_tot_ok[i] = response_updated_cluster$ig_6_hh_membres_tot_ok[i] + 1
    response_updated_cluster$sum_agegrp_18plus_femmes[i] = ifelse(response_updated_cluster$ig_2_sexe[i] == "femme", response_updated_cluster$sum_agegrp_18plus_femmes[i] + 1, response_updated_cluster$sum_agegrp_18plus_femmes[i])
    response_updated_cluster$sum_agegrp_18plus_hommes[i] = ifelse(response_updated_cluster$ig_2_sexe[i] == "homme", response_updated_cluster$sum_agegrp_18plus_hommes[i] + 1, response_updated_cluster$sum_agegrp_18plus_hommes[i])
    response_updated_cluster$sum_ig_7_gr_vulnerable.nsp[i] = response_updated_cluster$sum_ig_7_gr_vulnerable.nsp[i] + 1
    response_updated_cluster$sum_sante_2_malade_nsp[i] = response_updated_cluster$sum_sante_2_malade_nsp[i] + 1
  } 
}
response_updated_cluster$sum_agegrp_18plus = response_updated_cluster$sum_agegrp_18plus_femmes + response_updated_cluster$sum_agegrp_18plus_hommes



response_updated_cluster <- response_updated_cluster%>%
  mutate(
    sum_sante_4_0_4_diarrhee_tot_nonsoignee = if_else(is.na(sum_sante_4_0_4_diarrhee_tot_nonsoignee), 0,
                                                                           sum_sante_4_0_4_diarrhee_tot_nonsoignee),
    sum_sante_4_0_4_fievre_toux_tot_nonsoignee = if_else(is.na(sum_sante_4_0_4_fievre_toux_tot_nonsoignee), 0,
                                                                              sum_sante_4_0_4_fievre_toux_tot_nonsoignee),
    sante_5_deces_cause = if_else(sante_5_deces_cause == "TRUE", "", sante_5_deces_cause),
    health = if_else(sante_5_deces_cause == "cause_animale" |
                                            sante_5_deces_cause == "maladie_diarr" |
                                            sante_5_deces_cause == "manque_nourrit" |
                                            sante_5_deces_cause == "acc_CA" |
                                            (!is.nan(sum_sante_4_0_4_diarrhee_tot_nonsoignee/sum_agegrp_0_4_ok) &
                                                sum_sante_4_0_4_diarrhee_tot_nonsoignee/sum_agegrp_0_4_ok > 0.45) |
                                            sum_sante_5_5plus_malade_tot_nonsoignee/
                                            (sum_agegrp_5_17 + sum_agegrp_18plus) > 0.6, "4", 
                                          if_else(sante_5_deces_cause == "acc_travail" |
                                                    sante_5_deces_cause == "maladie_autre" |
                                                    (!is.nan(sum_sante_4_0_4_diarrhee_tot_nonsoignee/sum_agegrp_0_4_ok) & 
                                                        sum_sante_4_0_4_diarrhee_tot_nonsoignee/sum_agegrp_0_4_ok > 0.2) |
                                                       (!is.nan(sum_sante_4_0_4_fievre_toux_tot_nonsoignee/sum_agegrp_0_4_ok) & 
                                                           sum_sante_4_0_4_fievre_toux_tot_nonsoignee/sum_agegrp_0_4_ok > 0.2) |
                                                    (sum_sante_5_5plus_malade_tot_nonsoignee/
                                                    (sum_agegrp_5_17 + sum_agegrp_18plus) > 0.4) |
                                                    (!is.na(sante_indicator_accouchement_non_assiste) & 
                                                       sante_indicator_accouchement_non_assiste == 1) , "3",
                                                  if_else(sante_5_deces_cause == "acc_route" |
                                                            sante_5_deces_cause == "catast_nat" |
                                                            sante_5_deces_cause == "autre" |
                                                            (!is.nan(sum_sante_4_0_4_diarrhee_tot_nonsoignee/sum_agegrp_0_4_ok) & 
                                                               sum_sante_4_0_4_diarrhee_tot_nonsoignee/sum_agegrp_0_4_ok > 0) |
                                                            (!is.nan(sum_sante_4_0_4_fievre_toux_tot_nonsoignee/sum_agegrp_0_4_ok) & 
                                                               sum_sante_4_0_4_fievre_toux_tot_nonsoignee/sum_agegrp_0_4_ok > 0) |
                                                            (sum_sante_5_5plus_malade_tot_nonsoignee/
                                                               (sum_agegrp_5_17 + sum_agegrp_18plus) > 0) |
                                                            sum_sante_2_malade_tot_soins_cs / ig_6_hh_membres_tot_ok > 0.5, "2",
                                                          if_else((sante_5_deces_cause == "" |
                                                                    sante_5_deces_cause == "naturelle") &
                                                                    (sum_sante_4_0_4_diarrhee_tot_nonsoignee/sum_agegrp_0_4_ok == 0 | 
                                                                       is.nan(sum_sante_4_0_4_diarrhee_tot_nonsoignee/sum_agegrp_0_4_ok)) &
                                                                    (sum_sante_4_0_4_fievre_toux_tot_nonsoignee/sum_agegrp_0_4_ok == 0 | 
                                                                       is.nan(sum_sante_4_0_4_fievre_toux_tot_nonsoignee/sum_agegrp_0_4_ok)) &
                                                                    sum_sante_5_5plus_malade_tot_nonsoignee/
                                                                    (sum_agegrp_5_17 + sum_agegrp_18plus) == 0 &
                                                                    sum_sante_2_malade_tot_soins_cs / ig_6_hh_membres_tot_ok <= 0.5 & 
                                                                    (sante_indicator_accouchement_assiste != 0 | 
                                                                       sante_indicator_accouchement_nsp == 1 |
                                                                       is.na(sante_indicator_accouchement_assiste) |
                                                                       is.na(sante_indicator_accouchement_non_assiste) |
                                                                       is.na(sante_indicator_accouchement_nsp) ), "1", "TO_BE_CHECKED"))))
    )
  


table(response_updated_cluster$health)
# sum(table(response_updated_cluster$health))


# Protection:
response_updated_cluster <- response_updated_cluster%>%
  mutate(
    risque_critique = ifelse((protect_2_femmes_risque.violence_sexuelles == 0 | is.na(protect_2_femmes_risque.violence_sexuelles)) & 
                               (protect_3_filles_risque.violence_sexuelles == 0 | is.na(protect_3_filles_risque.violence_sexuelles)) & 
                               (protect_2_hommes_risque.violence_sexuelles == 0 | is.na(protect_2_hommes_risque.violence_sexuelles)) & 
                               (protect_3_garcons_risque.violence_sexuelles == 0 | is.na(protect_3_garcons_risque.violence_sexuelles)) & 
                               (protect_2_femmes_risque.meurtre_diff_groupes == 0 | is.na(protect_2_femmes_risque.meurtre_diff_groupes)) & 
                               (protect_3_filles_risque.meurtre_diff_groupes == 0 | is.na(protect_3_filles_risque.meurtre_diff_groupes)) & 
                               (protect_2_hommes_risque.meurtre_diff_groupes == 0 | is.na(protect_2_hommes_risque.meurtre_diff_groupes)) & 
                               (protect_3_garcons_risque.meurtre_diff_groupes == 0 | is.na(protect_3_garcons_risque.meurtre_diff_groupes)) & 
                               (protect_2_femmes_risque.meurtre_meme_groupe == 0 | is.na(protect_2_femmes_risque.meurtre_meme_groupe)) & 
                               (protect_3_filles_risque.meurtre_meme_groupe == 0 | is.na(protect_3_filles_risque.meurtre_meme_groupe)) & 
                               (protect_2_hommes_risque.meurtre_meme_groupe == 0 | is.na(protect_2_hommes_risque.meurtre_meme_groupe)) & 
                               (protect_3_garcons_risque.meurtre_meme_groupe == 0 | is.na(protect_3_garcons_risque.meurtre_meme_groupe)), 0, 1),
    risque_severe.mariage_force = ifelse((protect_2_femmes_risque.mariage_force == 0 | is.na(protect_2_femmes_risque.mariage_force )) & 
                                           (protect_3_filles_risque.mariage_force == 0 | is.na(protect_3_filles_risque.mariage_force)) & 
                                           (protect_2_hommes_risque.mariage_force == 0 | is.na(protect_2_hommes_risque.mariage_force)) & 
                                           (protect_3_garcons_risque.mariage_force == 0 | is.na(protect_3_garcons_risque.mariage_force)), 0, 1),
    risque_severe.separation_famille = ifelse((protect_2_femmes_risque.separation_famille == 0 | is.na(protect_2_femmes_risque.separation_famille )) & 
                                                (protect_3_filles_risque.separation_famille == 0 | is.na(protect_3_filles_risque.separation_famille)) & 
                                                (protect_2_hommes_risque.separation_famille == 0 | is.na(protect_2_hommes_risque.separation_famille)) & 
                                                (protect_3_garcons_risque.separation_famille == 0 | is.na(protect_3_garcons_risque.separation_famille)), 0, 1),
    risque_severe.recrutement_force = ifelse((protect_2_femmes_risque.recrutement_force == 0 | is.na(protect_2_femmes_risque.recrutement_force )) & 
                                               (protect_3_filles_risque.recrutement_force == 0 | is.na(protect_3_filles_risque.recrutement_force)) & 
                                               (protect_2_hommes_risque.recrutement_force == 0 | is.na(protect_2_hommes_risque.recrutement_force)) & 
                                               (protect_3_garcons_risque.recrutement_force == 0 | is.na(protect_3_garcons_risque.recrutement_force)), 0, 1),
    risque_severe.travail_force = ifelse((protect_2_femmes_risque.travail_force == 0 | is.na(protect_2_femmes_risque.travail_force )) & 
                                           (protect_3_filles_risque.travail_force == 0 | is.na(protect_3_filles_risque.travail_force)) & 
                                           (protect_2_hommes_risque.travail_force == 0 | is.na(protect_2_hommes_risque.travail_force)) & 
                                           (protect_3_garcons_risque.travail_force == 0 | is.na(protect_3_garcons_risque.travail_force)), 0, 1),
    risque_severe.enlevement = ifelse((protect_2_femmes_risque.enlevement == 0 | is.na(protect_2_femmes_risque.enlevement )) & 
                                        (protect_3_filles_risque.enlevement == 0 | is.na(protect_3_filles_risque.enlevement)) & 
                                        (protect_2_hommes_risque.enlevement == 0 | is.na(protect_2_hommes_risque.enlevement)) & 
                                        (protect_3_garcons_risque.enlevement == 0 | is.na(protect_3_garcons_risque.enlevement)), 0, 1),
    risque_severe.enlevement_travail_force = ifelse((protect_2_femmes_risque.enlevement_travail_force == 0 | is.na(protect_2_femmes_risque.enlevement_travail_force )) & 
                                                      (protect_3_filles_risque.enlevement_travail_force == 0 | is.na(protect_3_filles_risque.enlevement_travail_force)) & 
                                                      (protect_2_hommes_risque.enlevement_travail_force == 0 | is.na(protect_2_hommes_risque.enlevement_travail_force)) & 
                                                      (protect_3_garcons_risque.enlevement_travail_force == 0 | is.na(protect_3_garcons_risque.enlevement_travail_force)), 0, 1),
    risque_severe.un = if_else(risque_severe.mariage_force + 
                                                      risque_severe.separation_famille +
                                                      risque_severe.recrutement_force +
                                                      risque_severe.travail_force + 
                                                      risque_severe.enlevement + 
                                                      risque_severe.enlevement_travail_force == 1, 1, 0) ,
    risque_severe.deuxplus = if_else(risque_severe.mariage_force + 
                                       risque_severe.separation_famille +
                                       risque_severe.recrutement_force +
                                       risque_severe.travail_force + 
                                       risque_severe.enlevement + 
                                       risque_severe.enlevement_travail_force > 1, 1, 0),
    risque_serieux.vol_betail = ifelse((protect_2_femmes_risque.vol_betail == 0 | is.na(protect_2_femmes_risque.vol_betail )) & 
                                         (protect_3_filles_risque.vol_betail == 0 | is.na(protect_3_filles_risque.vol_betail)) & 
                                         (protect_2_hommes_risque.vol_betail == 0 | is.na(protect_2_hommes_risque.vol_betail)) & 
                                         (protect_3_garcons_risque.vol_betail == 0 | is.na(protect_3_garcons_risque.vol_betail)), 0, 1),
    risque_serieux.pillage_criminalite = ifelse((protect_2_femmes_risque.pillage_criminalite == 0 | is.na(protect_2_femmes_risque.pillage_criminalite )) & 
                                                  (protect_3_filles_risque.pillage_criminalite == 0 | is.na(protect_3_filles_risque.pillage_criminalite)) & 
                                                  (protect_2_hommes_risque.pillage_criminalite == 0 | is.na(protect_2_hommes_risque.pillage_criminalite)) & 
                                                  (protect_3_garcons_risque.pillage_criminalite == 0 | is.na(protect_3_garcons_risque.pillage_criminalite)), 0, 1),
    risque_serieux.harcelement = ifelse((protect_2_femmes_risque.harcelement == 0 | is.na(protect_2_femmes_risque.harcelement )) & 
                                          (protect_3_filles_risque.harcelement == 0 | is.na(protect_3_filles_risque.harcelement)) & 
                                          (protect_2_hommes_risque.harcelement == 0 | is.na(protect_2_hommes_risque.harcelement)) & 
                                          (protect_3_garcons_risque.harcelement == 0 | is.na(protect_3_garcons_risque.harcelement)), 0, 1),
    risque_serieux.violence_voisins = ifelse((protect_2_femmes_risque.violence_voisins == 0 | is.na(protect_2_femmes_risque.violence_voisins )) & 
                                               (protect_3_filles_risque.violence_voisins == 0 | is.na(protect_3_filles_risque.violence_voisins)) & 
                                               (protect_2_hommes_risque.violence_voisins == 0 | is.na(protect_2_hommes_risque.violence_voisins)) & 
                                               (protect_3_garcons_risque.violence_voisins == 0 | is.na(protect_3_garcons_risque.violence_voisins)), 0, 1),
    risque_serieux.conflits_fonciers = ifelse((protect_2_femmes_risque.conflits_fonciers == 0 | is.na(protect_2_femmes_risque.conflits_fonciers )) & 
                                                (protect_3_filles_risque.conflits_fonciers == 0 | is.na(protect_3_filles_risque.conflits_fonciers)) & 
                                                (protect_2_hommes_risque.conflits_fonciers == 0 | is.na(protect_2_hommes_risque.conflits_fonciers)) & 
                                                (protect_3_garcons_risque.conflits_fonciers == 0 | is.na(protect_3_garcons_risque.conflits_fonciers)), 0, 1),
    risque_serieux.incident_transhumance = ifelse((protect_2_femmes_risque.incident_transhumance == 0 | is.na(protect_2_femmes_risque.incident_transhumance )) & 
                                                    (protect_3_filles_risque.incident_transhumance == 0 | is.na(protect_3_filles_risque.incident_transhumance)) & 
                                                    (protect_2_hommes_risque.incident_transhumance == 0 | is.na(protect_2_hommes_risque.incident_transhumance)) & 
                                                    (protect_3_garcons_risque.incident_transhumance == 0 | is.na(protect_3_garcons_risque.incident_transhumance)), 0, 1),
    risque_serieux.rumeurs_attaques = ifelse((protect_2_femmes_risque.rumeurs_attaques == 0 | is.na(protect_2_femmes_risque.rumeurs_attaques )) & 
                                               (protect_3_filles_risque.rumeurs_attaques == 0 | is.na(protect_3_filles_risque.rumeurs_attaques)) & 
                                               (protect_2_hommes_risque.rumeurs_attaques == 0 | is.na(protect_2_hommes_risque.rumeurs_attaques)) & 
                                               (protect_3_garcons_risque.rumeurs_attaques == 0 | is.na(protect_3_garcons_risque.rumeurs_attaques)), 0, 1),
    risque_serieux.tensions_communautes = ifelse((protect_2_femmes_risque.tensions_communautes == 0 | is.na(protect_2_femmes_risque.tensions_communautes )) & 
                                                   (protect_3_filles_risque.tensions_communautes == 0 | is.na(protect_3_filles_risque.tensions_communautes)) & 
                                                   (protect_2_hommes_risque.tensions_communautes == 0 | is.na(protect_2_hommes_risque.tensions_communautes)) & 
                                                   (protect_3_garcons_risque.tensions_communautes == 0 | is.na(protect_3_garcons_risque.tensions_communautes)), 0, 1),
    risque_serieux.un = if_else(risque_serieux.vol_betail + 
                                                      risque_serieux.pillage_criminalite +
                                                      risque_serieux.harcelement +
                                                      risque_serieux.violence_voisins + 
                                                      risque_serieux.conflits_fonciers + 
                                                      risque_serieux.incident_transhumance + 
                                                      risque_serieux.rumeurs_attaques + 
                                                      risque_serieux.tensions_communautes == 1, 1, 0),
    risque_serieux.deuxplus = if_else(risque_serieux.vol_betail + 
                                                       risque_serieux.pillage_criminalite +
                                                       risque_serieux.harcelement +
                                                       risque_serieux.violence_voisins + 
                                                       risque_serieux.conflits_fonciers + 
                                                       risque_serieux.incident_transhumance + 
                                                       risque_serieux.rumeurs_attaques + 
                                                       risque_serieux.tensions_communautes > 1, 1, 0) 
  )

response_updated_cluster <- response_updated_cluster%>%
  mutate(
    protection = if_else(((risque_critique == 1 | risque_severe.deuxplus == 1 ) &
                           protect_11 == "oui") | 
                           ((risque_critique == 1 | risque_severe.deuxplus == 1 | risque_severe.un == 1 | risque_serieux.deuxplus == 1) & 
                           (protect_13 == "aucun" | protect_13 == "nsp")), "4",
                         if_else((risque_critique == 1 | risque_severe.deuxplus == 1 | risque_severe.un == 1 | risque_serieux.deuxplus == 1) |
                                   (sum_protect_10.carriere >= 1 | sum_protect_10.construction >= 1 | sum_protect_10.prostitution >= 1 | sum_protect_10.recrutes >= 1) | 
                                   (protect_4 == "oui" & 
                                      (protect_5_travail_force.mines == 1 |
                                         protect_5_travail_force.construction == 1 |
                                         protect_5_travail_force.militaire == 1 | 
                                         protect_5_travail_force.prostitution == 1 | 
                                         protect_5_travail_force.drogue == 1 | 
                                         protect_5_travail_force.mendicite == 1 | 
                                         protect_5_travail_force.combats == 1 | 
                                         protect_5_travail_force.services_sexuels == 1 | 
                                         protect_5_travail_force.autres_actions_militaires == 1 | 
                                         protect_5_travail_force.soutien_ga == 1 | 
                                         protect_5_travail_force.espion == 1)) | 
                                   (risque_serieux.un == 1 & (protect_13 == "aucun" | protect_13 == "nsp")), "3", 
                                 if_else(risque_serieux.un == 1 |
                                           protect_11 == "oui" | 
                                           (sum_protect_10.agric >= 1 | 
                                              sum_protect_10.peche >= 1 | 
                                              sum_protect_10.elevage >= 1 | 
                                              sum_protect_10.petit_commerce >= 1 | 
                                              sum_protect_10.restauration >= 1 | 
                                              sum_protect_10.artisanat >= 1 | 
                                              sum_protect_10.travaux_domestiques >= 1 | 
                                              sum_protect_10.transport >= 1 | 
                                              sum_protect_10.autre >= 1 | 
                                              sum_protect_10.nsp >= 1 |
                                              sum_protect_9 >= 1 ) | 
                                           (protect_4 == "oui" & 
                                              (protect_5_travail_force.agriculture_forets == 1 |
                                                 protect_5_travail_force.peche == 1 |
                                                 protect_5_travail_force.indutrie == 1 |
                                                 protect_5_travail_force.commerce_automobile == 1 |
                                                 protect_5_travail_force.hotellerie_restauration == 1 |
                                                 protect_5_travail_force.arts == 1 |
                                                 protect_5_travail_force.services_personnels == 1 |
                                                 protect_5_travail_force.travail_domestique == 1 |
                                                 protect_5_travail_force.portier == 1 |
                                                 protect_5_travail_force.autre == 1 |
                                                 protect_5_travail_force.nsp == 1)) | 
                                           (protect_13 == "aucun" | protect_13 == "nsp") | 
                                           protect_4 == "nsp", "2", 
                                         if_else(risque_critique == 0 & 
                                                   risque_severe.deuxplus == 0 & 
                                                   risque_severe.un == 0 & 
                                                   risque_serieux.deuxplus == 0 & 
                                                   risque_serieux.un == 0 &
                                                   protect_4 == "non" & 
                                                   sum_protect_9 == 0 & 
                                                   (protect_13 != "aucun" | protect_13 != "nsp"), "1", "TOBECHECKED"))))
  )
                                            
table(response_updated_cluster$protection)


# FSL:
response_updated_cluster <- response_updated_cluster%>%
  mutate(
    fsl = if_else(hhs_severe == 1 | 
                    (hhs_moderate == 1 &
                       fcs_score_poor == 1 & 
                       rcsi_high == 1) , "4",
                  if_else((hhs_moderate == 1 &
                             fcs_score_poor == 1) | 
                            (hhs_moderate == 1 &
                               (fcs_score_borderline == 1 | fcs_score_acceptable == 1) & rcsi_high == 1) | 
                            (hhs_light == 1 &
                               fcs_score_poor == 1 & rcsi_high == 1) , "3", 
                          if_else((hhs_moderate == 1 &
                                     (fcs_score_borderline == 1 | fcs_score_acceptable == 1)) | 
                                    (hhs_light == 1 &
                                       fcs_score_poor == 1) | 
                                    (hhs_light == 1 &
                                       (fcs_score_borderline == 1 | fcs_score_acceptable == 1) &
                                       (rcsi_high == 1 | rcsi_med == 1)) , "2", 
                                  if_else(hhs_light == 1 &
                                            (fcs_score_borderline == 1 | fcs_score_acceptable == 1) & 
                                            rcsi_low == 1, "1", "TOBECHECKED")))))

table(response_updated_cluster$fsl)



# Shelter / NFI:
#  to do : check 'score casserole' in score_nfi vs. nb_casserole !! 
response_updated_cluster <- response_updated_cluster %>%
  mutate(
    score_casserole = case_when(
      nb_casseroles >= 0 & nb_casseroles <1 ~ 5,
      nb_casseroles >= 1 & nb_casseroles <2 ~ 4,
      nb_casseroles >= 2 & nb_casseroles <3 ~ 3.5,
      nb_casseroles >= 3 & nb_casseroles <4 ~ 2,
      nb_casseroles >= 4 & nb_casseroles <5 ~ 1,
      nb_casseroles >= 5 ~ 0),
    score_seau = case_when(
      nb_seaux >= 0 & nb_seaux < 1 ~ 5,
      nb_seaux >= 1 & nb_seaux < 2 ~ 3.5,
      nb_seaux >= 2 & nb_seaux < 3 ~ 2.5,
      nb_seaux >= 3 & nb_seaux < 4 ~ 1, ## !! Has been corrected (before: >=3 & <3, then 1 and > 3, then 0) HAS TO BE RERUNNED ! 
      nb_seaux >= 4 ~ 0
    )
  )%>%
  mutate(
      score_nfi_tot = (score_bidons+ score_seau+ score_moust+ score_casserole+ score_couchage+ score_draps)/6,
      sphere_nfi = if_else(is.na(volume_total_recipients+ nb_casseroles+ nb_couchage+ nb_draps+ nb_mousticaire), "NA",
                           if_else(volume_total_recipients >= 30 & nb_casseroles >= 2 & nb_couchage >=3 & nb_draps >=3 & nb_mousticaire >= 3, "oui", "non")),
      pin_score_nfi = if_else(score_nfi_tot >= 3.9, "5",
                              if_else(score_nfi_tot >= 3, "4",
                                      if_else(score_nfi_tot >= 2, "3",
                                              if_else(score_nfi_tot >= 1, "2",
                                                      if_else(score_nfi_tot <= 1 & score_nfi_tot >=0, "1",
                                                              NA_character_)
                                                      ))))
      )


response_updated_cluster <- response_updated_cluster%>%
  mutate(
    nfi_list_bache = if_else(bache == "oui", 1, 
                              if_else(bache == "non", 0, 0)),
    nfi_list_kit_cuisine = if_else(kit_cuisine == "complet", 1, 
                                   if_else(kit_cuisine == "incomp", 0.5, 
                                           if_else(kit_cuisine == "aucun", 0, 0))),
    nfi_list_habits = if_else(habits_femmes_enfants == "tous", 1, 
                               if_else(habits_femmes_enfants == "incomp", 0.5, 
                                       if_else(habits_femmes_enfants == "aucun", 0, 0))),
    nfi_list_doc_legale = if_else(doc_legale == "tous", 1, 
                                 if_else(doc_legale == "incomp", 0.5, 
                                         if_else(doc_legale == "aucun", 0, 0))),
    nfi_list_titre_prop = if_else(doc_propriete == "complet", 1, 
                                 if_else(doc_propriete == "incomp", 0.5, 
                                         if_else(doc_propriete == "aucun", 0, 0))),
    nfi_list_semence = if_else(semence == "suff", 1, 
                               if_else(semence == "insuff", 0.5, 
                                       if_else(semence == "aucun", 0, 0))),
    nfi_list_outils = if_else(outils_aratoires == "suff", 1, 
                              if_else(outils_aratoires == "insuff", 0.5, 
                                      if_else(outils_aratoires == "aucun", 0, 0))),
    nfi_list_cendres = if_else(cendres == "oui", 1, 
                               if_else(cendres == "non", 0, 0)),
    nfi_list_bidons_oui_non = if_else(bidons_oui_non == 1, 1, 
                                  if_else(bidons_cap == 0, 0, 0)),
    nfi_list_savons = if_else(savon_toilette_azur/ig_6_hh_membres_tot_ok >= 1, 1, 
                              if_else(savon_toilette_azur/ig_6_hh_membres_tot_ok  >= 0.5 , 0.5, 
                                      if_else(savon_toilette_azur/ig_6_hh_membres_tot_ok  < 0.5, 0, 0))),
    nfi_list_savons_lessive = if_else(savon_lessive/ig_6_hh_membres_tot_ok >= 1, 1, 
                                      if_else(savon_lessive/ig_6_hh_membres_tot_ok  >= 0.5 , 0.5, 
                                              if_else(savon_lessive/ig_6_hh_membres_tot_ok  < 0.5, 0, 0))),
    nfi_list_pots_bb = if_else(sum_agegrp_0_4 == 0, 1,
                               if_else(sum_agegrp_0_4 > 0 & pot >= 1, 1, 
                                       if_else(sum_agegrp_0_4 > 0 & pot < 1, 0, 0))),
    nfi_list_gobelets = if_else(gobelet/ig_6_hh_membres_tot_ok >= 1, 1, 
                                if_else(gobelet/ig_6_hh_membres_tot_ok  >= 0.5 , 0.5, 
                                        if_else(gobelet/ig_6_hh_membres_tot_ok  < 0.5, 0, 0))),
    nfi_list_serviette_hyg = if_else((sum_agegrp_13_18_filles+sum_agegrp_19_59_femmes+sum_agegrp_59plus_femmes) == 0, 1,
                                     if_else(serviette_hyg/(sum_agegrp_13_18_filles+sum_agegrp_19_59_femmes+sum_agegrp_59plus_femmes) >= 1, 1, 
                                             if_else(serviette_hyg/(sum_agegrp_13_18_filles+sum_agegrp_19_59_femmes+sum_agegrp_59plus_femmes) >= 0.5 , 0.5, 
                                                     if_else(serviette_hyg/(sum_agegrp_13_18_filles+sum_agegrp_19_59_femmes+sum_agegrp_59plus_femmes) < 0.5, 0, 0)))),
    nfi_list_seaux = if_else(nb_seaux >= 1, 1, 
                             if_else(nb_seaux < 1, 0, 0)),
    nfi_list_moustiquaire = if_else(nb_mousticaire/ig_6_hh_membres_tot_ok >= 1, 1, 
                                    if_else(nb_mousticaire/ig_6_hh_membres_tot_ok  >= 0.5 , 0.5, 
                                            if_else(nb_mousticaire/ig_6_hh_membres_tot_ok  < 0.5, 0, 0))),
    nfi_list_casseroles = if_else(nb_casseroles >= 1, 1, 
                                  if_else(nb_casseroles < 1, 0, 0)),
    nfi_list_lits = if_else(nb_couchage/ig_6_hh_membres_tot_ok >= 1, 1, 
                            if_else(nb_couchage/ig_6_hh_membres_tot_ok  >= 0.5 , 0.5, 
                                    if_else(nb_couchage/ig_6_hh_membres_tot_ok  < 0.5, 0, 0))),
    nfi_list_couv = if_else(nb_draps/ig_6_hh_membres_tot_ok >= 1, 1, 
                            if_else(nb_draps/ig_6_hh_membres_tot_ok  >= 0.5 , 0.5, 
                                    if_else(nb_draps/ig_6_hh_membres_tot_ok  < 0.5, 0, 0)))
  )


response_updated_cluster <- response_updated_cluster%>%
  mutate(
    nfi_list_cumul = nfi_list_bache + nfi_list_kit_cuisine + nfi_list_habits + nfi_list_doc_legale + nfi_list_titre_prop + nfi_list_semence +
                               nfi_list_outils + nfi_list_cendres + nfi_list_bidons_oui_non + nfi_list_savons + nfi_list_savons_lessive + nfi_list_pots_bb +
                               nfi_list_gobelets + nfi_list_serviette_hyg + nfi_list_seaux + nfi_list_moustiquaire + nfi_list_casseroles + nfi_list_lits+ nfi_list_couv
  )

               
response_updated_cluster <- response_updated_cluster%>%
  mutate(
    taille_abri_pp = if_else(is.na(nfi_4_personnes_abri), nfi_5_taille_abri/ig_6_hh_membres_tot_ok, taille_abri_pp),
    shelter = if_else(is.na(score_nfi_tot) | is.na(nfi_list_cumul), 
                      if_else(nfi_2_type_abri == "aucun" | 
                                nfi_2_type_abri == "abri_urgence", "4", 
                              if_else(((nfi_2_type_abri == "habitat_paille"| nfi_2_type_abri == "autre" | nfi_2_type_abri == "nsp") &  
                                        taille_abri_pp < 3.5 & nfi_propr_abri_propre != "oui") |
                                        (nfi_2_type_abri == "maison_dur" & nfi_propr_abri == "occupe"), "3", 
                                      if_else((nfi_2_type_abri == "habitat_paille" | nfi_2_type_abri == "autre" | nfi_2_type_abri == "nsp") &
                                                 taille_abri_pp < 3.5 & nfi_propr_abri_propre == "oui", "2", "1"))),
                      if_else(nfi_2_type_abri == "aucun" | 
                                nfi_2_type_abri == "abri_urgence" | 
                                score_nfi_tot >= 4.2 |
                                nfi_list_cumul < 3, "4",
                              if_else(((nfi_2_type_abri == "habitat_paille"| nfi_2_type_abri == "autre" | nfi_2_type_abri == "nsp") &  
                                         taille_abri_pp < 3.5 & nfi_propr_abri_propre != "oui") |
                                        (nfi_2_type_abri == "maison_dur" & nfi_propr_abri == "occupe") |
                                        score_nfi_tot >= 3.9 |
                                        nfi_list_cumul < 7, "3", 
                                      if_else(((nfi_2_type_abri == "habitat_paille" | nfi_2_type_abri == "autre" | nfi_2_type_abri == "nsp") & 
                                                 taille_abri_pp < 3.5 & nfi_propr_abri_propre == "oui") |
                                                score_nfi_tot >= 2 |
                                                nfi_list_cumul < 12, "2", 
                                              if_else((((nfi_2_type_abri == "habitat_paille" | nfi_2_type_abri == "autre" | nfi_2_type_abri == "nsp") & 
                                                          taille_abri_pp >= 3.5 ) | 
                                                         nfi_2_type_abri == "maison_dur") & 
                                                        (score_nfi_tot < 2 |
                                                        nfi_list_cumul >= 12), "1", "TOBECHECKED")))))
    )

(table(response_updated_cluster$shelter))


# Educ
# install.packages("matrixStats")
library(matrixStats)

response_updated_cluster <- response_updated_cluster%>%
  mutate(
    sum_age_grp_7_18 = sum_agegrp_7_12_filles + sum_agegrp_7_12_garcons + sum_agegrp_13_18_filles + sum_agegrp_13_18_garcons,
    taux_freq_scol = ifelse(sum_age_grp_7_18 == 0, NA, (sum_educ_3_presence_18_19.filles_7_12.6m_12m + sum_educ_3_presence_18_19.garcons_7_12.6m_12m +
                                                          sum_educ_3_presence_18_19.filles_7_12.12m + sum_educ_3_presence_18_19.garcons_7_12.12m + 
                                                          sum_educ_3_presence_18_19.filles_13_18.6m_12m + sum_educ_3_presence_18_19.garcons_13_18.6m_12m +
                                                          sum_educ_3_presence_18_19.filles_13_18.12m + sum_educ_3_presence_18_19.garcons_13_18.12m)/sum_age_grp_7_18),
    education = if_else(is.na(taux_freq_scol), "",
                                   if_else(taux_freq_scol > 0.8, "1",
                                           if_else(taux_freq_scol > 0.6, "2",
                                                   if_else(taux_freq_scol > 0.4, "3",
                                                           if_else(taux_freq_scol <= 0.4, "4", "TOBECHECKED"))))),
    )

table(response_updated_cluster$education)



### WASH 

response_updated_cluster <- response_updated_cluster %>%
  mutate(
    wash_cond_1_source = if_else(wash_1_source_boisson.cours_d_eau == 1 | wash_1_source_boisson.eau_de_pluie == 1, "surface",
                                 if_else(wash_1_source_boisson.source_non_amen == 1 | wash_1_source_boisson.puit_non_prot == 1 | 
                                           wash_1_source_boisson.autre == 1, "non_amelioree", 
                                         if_else(wash_1_source_boisson.eau_robinet == 1 | wash_1_source_boisson.forage_mec == 1 | 
                                                   wash_1_source_boisson.forage_man == 1 | wash_1_source_boisson.puit_prot == 1 | 
                                                   wash_1_source_boisson.bouteilles == 1 | wash_1_source_boisson.eau_camion == 1 | 
                                                   wash_1_source_boisson.source_amen == 1, "amelioree", "TOBECHECKED"))),
    wash_cond_1 = if_else(wash_cond_1_source == "amelioree" & wash_5_saison_seche == "oui" & wash_4_temps == "sur_place", "1",
                          if_else(wash_cond_1_source == "amelioree" & wash_5_saison_seche == "oui" &
                                    (wash_4_temps == "0_5_min" | wash_4_temps == "5_15_min" | wash_4_temps == "16_30_min"), "2", 
                                  if_else(wash_cond_1_source == "amelioree" , "3",  
                                          if_else(wash_cond_1_source == "non_amelioree" | wash_cond_1_source == "surface", "4", "TOBECHECKED")))),
    wash_cond_2 = if_else(is.na(volume_total_recipients_pp), "1",
                          if_else(volume_total_recipients_pp <= 9, "4", 
                                  if_else(volume_total_recipients_pp <= 15, "3", 
                                          if_else(volume_total_recipients_pp <= 50, "2", 
                                                  if_else(volume_total_recipients_pp > 50, "1", "TOBECHECKED"))))),
    wash_cond_3 = if_else(wash_10_infra_sanit == "latrine_acceptable" & (wash_11_sanit_partagee == "non" | wash_11_sanit_partagee == "nsp"), "1", 
                          if_else(wash_10_infra_sanit == "latrine_acceptable" & wash_11_sanit_partagee == "oui", "3", 
                                  if_else(wash_10_infra_sanit == "latrine_non_acceptable"  |
                                            wash_10_infra_sanit == "autre"  |
                                            wash_10_infra_sanit == "nsp"  |
                                            wash_10_infra_sanit == "defec_air_libre" | 
                                            wash_10_infra_sanit == "defec_air_libre_cours_d_eau" | 
                                            wash_10_infra_sanit == "defec_air_libre_zone_precise", "4", "TOBECHECKED"))),
    wash_18_nb_lavage_main_moment = wash_18_lavage_main_moment.apres_toilette + wash_18_lavage_main_moment.apres_nettoyage +
      wash_18_lavage_main_moment.avant_cuisiner + wash_18_lavage_main_moment.avant_manger + 
      wash_18_lavage_main_moment.avant_allaitement + wash_18_lavage_main_moment.apres_champs,
    wash_cond_4 = if_else((wash_18_nb_lavage_main_moment >= 3) & 
                            (wash_19_lavage_main_moyens.cendre == 1 | wash_19_lavage_main_moyens.savon == 1) , "1", 
                          if_else((wash_18_nb_lavage_main_moment < 3) & 
                                    (wash_19_lavage_main_moyens.cendre == 1 | wash_19_lavage_main_moyens.savon == 1), "2", 
                                  if_else((wash_18_nb_lavage_main_moment >= 3) & 
                                            (wash_19_lavage_main_moyens.cendre == 0 & wash_19_lavage_main_moyens.savon == 0 
                                             & wash_19_lavage_main_moyens.eau == 1), "3", 
                                             if_else((wash_18_nb_lavage_main_moment < 3) & 
                                                       (wash_19_lavage_main_moyens.cendre == 0 & wash_19_lavage_main_moyens.savon == 0 
                                                        & wash_19_lavage_main_moyens.eau == 1) , "4", 
                                                     if_else(wash_18_nb_lavage_main_moment < 3, "4", 
                                                             if_else(wash_18_nb_lavage_main_moment >= 3, "2",
                                                                     "TOBECHECKED"))))))
  )%>%
  mutate(
    taux_autre_eha = sum_sante_4_0_4_malades_autre_filles + sum_sante_4_0_4_malades_autre_garcons,
    taux_toux_eha = sum_sante_4_0_4_malades_toux_filles + sum_sante_4_0_4_malades_toux_garcons,
    taux_diarrhee_eha = sum_sante_4_0_4_malades_diarrhee_filles + sum_sante_4_0_4_malades_diarrhee_garcons,
    taux_fievre_eha = sum_sante_4_0_4_malades_fievre_filles + sum_sante_4_0_4_malades_fievre_garcons,
    taux_eha = (taux_autre_eha + taux_toux_eha + taux_diarrhee_eha + taux_fievre_eha) / sum_agegrp_0_4,
    taux_morbid_eha = ifelse(sum_agegrp_0_4 == 0, NA,
                             ifelse(taux_eha > 1, 1, taux_eha)),
    taux_morbid_eha_score = if_else(is.na(taux_morbid_eha), "",
                                    if_else(taux_morbid_eha == 0, "1",
                                            if_else(taux_morbid_eha <= 0.5, "3",
                                                    if_else(taux_morbid_eha <= 1, "4", "TOBECHECKED")))),
    wash_be_score_1 = taux_morbid_eha_score,
    wash_be_poid_1 = if_else(wash_12_sanit_partagee_par_genre == "non" | wash_13_sanit_partagee_espace_femmes == "non", 3, 0),
    wash_be_poid_2 = if_else(wash_4_temps == "31_plus_min" | wash_4_temps == "nsp" | wash_cond_1_source != "amelioree", 4, 0),
    wash_be_poid_3_a = if_else(wash_9_insuff_raisons_1 == "inacc_pr_certains_groupes" | wash_9_insuff_raisons_1 == "route_dangereux",3,0),
    wash_be_poid_3_b = if_else(wash_9_insuff_raisons_2 == "inacc_pr_certains_groupes" | wash_9_insuff_raisons_2 == "route_dangereux",2,0),
    wash_be_poid_3_c = if_else(wash_9_insuff_raisons_3 == "inacc_pr_certains_groupes" | wash_9_insuff_raisons_3 == "route_dangereux",1,0),
    wash_be_poid_4_a = if_else(wash_15_insuff_raisons_1 == "quantite_insuff" | wash_15_insuff_raisons_1 == "mixte" | 
                                 wash_15_insuff_raisons_1 == "trop_loin" | wash_15_insuff_raisons_1 == "trop_dangereux" | 
                                 wash_15_insuff_raisons_1 == "inacc_pr_certains_groupes", 4, 0),
    wash_be_poid_4_b = if_else(wash_15_insuff_raisons_2 == "quantite_insuff" | wash_15_insuff_raisons_2 == "mixte" | 
                                 wash_15_insuff_raisons_2 == "trop_loin" | wash_15_insuff_raisons_2 == "trop_dangereux" | 
                                 wash_15_insuff_raisons_2 == "inacc_pr_certains_groupes", 2, 0),
    wash_be_poid_4_c = if_else(wash_15_insuff_raisons_3 == "quantite_insuff" | wash_15_insuff_raisons_3 == "mixte" | 
                                 wash_15_insuff_raisons_3 == "trop_loin" | wash_15_insuff_raisons_3 == "trop_dangereux" | 
                                 wash_15_insuff_raisons_3 == "inacc_pr_certains_groupes", 1, 0),
    wash_be_poid_3_max = if_else(wash_be_poid_3_a > wash_be_poid_3_b, wash_be_poid_3_a,
                                 if_else(wash_be_poid_3_b > wash_be_poid_3_c, wash_be_poid_3_b,
                                         if_else(wash_be_poid_3_c > 0, wash_be_poid_3_c, 0))),
    wash_be_poid_4_max = if_else(wash_be_poid_4_a > wash_be_poid_4_b, wash_be_poid_4_a,
                                 if_else(wash_be_poid_4_b > wash_be_poid_4_c, wash_be_poid_4_b,
                                         if_else(wash_be_poid_4_c > 0, wash_be_poid_4_c, 0))),
    wash_be_indice = wash_be_poid_1 + wash_be_poid_2 + wash_be_poid_3_max + wash_be_poid_4_max,
    wash_be_score_2 = if_else(wash_be_indice < 4, "1",
                              if_else(wash_be_indice < 10, "3", 
                                      if_else(wash_be_indice < 15, "4", "TOBECHECKED")))
  )

cond_vie_wash_score = cbind(wash_cond_1 = as.numeric(response_updated_cluster$wash_cond_1), wash_cond_2 = as.numeric(response_updated_cluster$wash_cond_2),
                                                     wash_cond_3 = as.numeric(response_updated_cluster$wash_cond_3), wash_cond_4 = as.numeric(response_updated_cluster$wash_cond_4))
response_updated_cluster$cond_vie_wash_score = rowMedians(cond_vie_wash_score)

be_wash_score = cbind(wash_be_score_1 = as.numeric(response_updated_cluster$wash_be_score_1), wash_be_score_2 = as.numeric(response_updated_cluster$wash_be_score_2))
response_updated_cluster$be_wash_score = rowMedians(be_wash_score, na.rm = T)

wash_score = cbind(cond_vie = as.numeric(response_updated_cluster$cond_vie_wash_score), bien_etre = as.numeric(response_updated_cluster$be_wash_score))
response_updated_cluster$wash = floor(rowMedians(wash_score))

table(response_updated_cluster$wash)

### Capacity score 
response_updated_cluster <- response_updated_cluster %>%
  mutate(
    lcs_score = if_else(lcs_total == "urgence", "4", 
                        if_else(lcs_total == "crise", "3", 
                                ifelse(lcs_total == "stress", "2", 
                                       if_else(lcs_total == "minimal", "1", "TOBECHECKED")))),
    source_rev_dep_ha = if_else(mssc_2_source_rev_1 == "transferts_argent" & mssc_2_source_rev_2 == "", "4", 
                            if_else(mssc_2_source_rev_1 == "transferts_argent" | mssc_2_source_rev_2 == "transferts_argent" |
                                    mssc_2_source_rev_3 == "transferts_argent", "3", "1")),
    capacity_gaps = if_else(lcs_score == "4" | source_rev_dep_ha == "4", "4", 
                             if_else(lcs_score == "3" | source_rev_dep_ha == "3", "3", 
                                     if_else(lcs_score == "2" | source_rev_dep_ha == "2", "2",
                                             if_else(lcs_score == "1" | source_rev_dep_ha == "1", "1", "TOBECHECECKED"))))

  )
table(response_updated_cluster$capacity_gaps)


# Impact score
response_updated_cluster <- response_updated_cluster %>%
  mutate(
    # impact_1 = if_else(ig_8_statut_groupe != "hote", "3", "2"),
    
    # impact_2 = if_else((ig_8_statut_groupe == "deplaces_FA" | ig_8_statut_groupe == "deplaces_site") &
    #                      (ig_12_idp_length_idp != "12_mois_ou_plus" & ig_12_idp_length_idp != "5_12_mois" & ig_12_idp_length_idp != "nsp"), "4",
    #                    if_else(((ig_8_statut_groupe == "deplaces_FA" | ig_8_statut_groupe == "deplaces_site") &
    #                              (ig_12_idp_length_idp == "12_mois_ou_plus" | ig_12_idp_length_idp == "5_12_mois" | ig_12_idp_length_idp == "nsp")) |
    #                              (ig_8_statut_groupe == "retournes" & ig_16_ret_rapat_dest_finale_oui_non != "abri_origine") |
    #                              ig_9_hote_fa == "oui", "3",
    #                            if_else((ig_8_statut_groupe == "retournes" & ig_16_ret_rapat_dest_finale_oui_non == "abri_origine") |
    #                                      ig_8_statut_groupe == "hote", "2", "tbc"))),

    impact_final = if_else((ig_8_statut_groupe == "deplaces_FA" | ig_8_statut_groupe == "deplaces_site") |
                                 (ig_8_statut_groupe == "retournes" & ig_16_ret_rapat_dest_finale_oui_non != "abri_origine") |
                                 ig_9_hote_fa == "oui", "3",
                               if_else((ig_8_statut_groupe == "retournes" & ig_16_ret_rapat_dest_finale_oui_non == "abri_origine") |
                                         ig_8_statut_groupe == "hote", "2", "tbc"))
  )
    
# table(response_updated_cluster$impact_1)
# table(response_updated_cluster$impact_2)
table(response_updated_cluster$impact_final)

response_updated_cluster <- response_updated_cluster %>%
  mutate(
    education = as.numeric(education),
    fsl = as.numeric(fsl),
    health = as.numeric(health),
    protection = as.numeric(protection),
    shelter = as.numeric(shelter),
    wash = as.numeric(wash),
    capacity_gaps = as.numeric(capacity_gaps),
    # impact_1 = as.numeric(impact_1),
    # impact_2 = as.numeric(impact_2),
    impact_final = as.numeric(impact_final)
  )

    
# remotes::install_github("caldwellst/msni19",force = TRUE)
library(msni19)
# browseVignettes("msni19")


# if educ is na, then no children, then no need, so replace with 1:
response_updated_cluster$education = if_else(is.na(response_updated_cluster$education), 1, response_updated_cluster$education)

# Calculate the MSNI score (1 with no impact, 2 wiht the impact: 
response_updated_cluster <- response_updated_cluster %>%
  mutate(
    # msni_score_1 = msni(education_lsg = response_updated_cluster$education,
    #                    fsl_lsg = response_updated_cluster$fsl,
    #                health_lsg = response_updated_cluster$health,
    #                protection_lsg = response_updated_cluster$protection,
    #                shelter_lsg = response_updated_cluster$shelter,
    #                wash_lsg = response_updated_cluster$wash,
    #                capacity_gaps = response_updated_cluster$capacity_gaps,
    #                impact = response_updated_cluster$impact_1),
    # msni_score_2 = msni(education_lsg = response_updated_cluster$education,
    #                     fsl_lsg = response_updated_cluster$fsl,
    #                     health_lsg = response_updated_cluster$health,
    #                     protection_lsg = response_updated_cluster$protection,
    #                     shelter_lsg = response_updated_cluster$shelter,
    #                     wash_lsg = response_updated_cluster$wash,
    #                     capacity_gaps = response_updated_cluster$capacity_gaps,
    #                     impact = response_updated_cluster$impact_2),
    msni_score_final = msni(education_lsg = response_updated_cluster$education,
                        fsl_lsg = response_updated_cluster$fsl,
                        health_lsg = response_updated_cluster$health,
                        protection_lsg = response_updated_cluster$protection,
                        shelter_lsg = response_updated_cluster$shelter,
                        wash_lsg = response_updated_cluster$wash,
                        capacity_gaps = response_updated_cluster$capacity_gaps,
                        impact = response_updated_cluster$impact_final)
   )

# table(response_updated_cluster$msni_score_1)
# table(response_updated_cluster$msni_score_2)
table(response_updated_cluster$msni_score_final)

response_updated_cluster <- response_updated_cluster %>%
  mutate(
    education_cat = if_else(education == 4, "extreme", 
                            if_else(education == 3, "severe", 
                                    if_else(education == 2, "stresse", "aucun_min"))),
    fsl_cat = if_else(fsl == 4, "extreme", 
                      if_else(fsl == 3, "severe", 
                              if_else(fsl == 2, "stresse", "aucun_min"))),
    health_cat = if_else(health == 4, "extreme", 
                         if_else(health == 3, "severe", 
                                 if_else(health == 2, "stresse", "aucun_min"))),
    protection_cat = if_else(protection == 4, "extreme", 
                             if_else(protection == 3, "severe", 
                                     if_else(protection == 2, "stresse", "aucun_min"))),
    shelter_cat = if_else(shelter == 4, "extreme", 
                          if_else(shelter == 3, "severe", 
                                  if_else(shelter == 2, "stresse", "aucun_min"))),
    wash_cat = if_else(wash == 4, "extreme", 
                       if_else(wash == 3, "severe", 
                               if_else(wash == 2, "stresse", "aucun_min"))),
    capacity_gaps_cat = if_else(capacity_gaps == 4, "extreme", 
                                if_else(capacity_gaps == 3, "severe", 
                                        if_else(capacity_gaps == 2, "stresse", "aucun_min"))),
    # impact_cat_1 = if_else(impact_1 == 4, "extreme", 
    #                        if_else(impact_1 == 3, "severe", 
    #                                if_else(impact_1 == 2, "stresse", "aucun_min"))),
    # impact_cat_2 = if_else(impact_2 == 4, "extreme", 
    #                        if_else(impact_2 == 3, "severe", 
    #                                if_else(impact_2 == 2, "stresse", "aucun_min"))),
    impact_cat = if_else(impact_final == 4, "extreme", 
                           if_else(impact_final == 3, "severe", 
                                   if_else(impact_final == 2, "stresse", "aucun_min"))),
    # msni_score_cat_1 = if_else(msni_score_1 == 4, "extreme", 
    #                            if_else(msni_score_1 == 3, "severe", 
    #                                    if_else(msni_score_1 == 2, "stresse", "aucun_min"))),
    # msni_score_cat_2 = if_else(msni_score_2 == 4, "extreme", 
    #                            if_else(msni_score_2 == 3, "severe", 
    #                                    if_else(msni_score_2 == 2, "stresse", "aucun_min"))),
    msni_score_cat = if_else(msni_score_final == 4, "extreme", 
                               if_else(msni_score_final == 3, "severe", 
                                       if_else(msni_score_final == 2, "stresse", "aucun_min")))
    
    
  )





## Add the urban / rural var
urban_rural = read.csv("./input/MSNA_loc_urbainrural.csv", stringsAsFactors = F)

urban_rural$UrbanRural_final = if_else(urban_rural$UrbanRural_final == "rural" | urban_rural$UrbanRural_final == "Rural" |
                                         urban_rural$UrbanRural_final == "rural ", "rural",
                                       if_else(urban_rural$UrbanRural_final == "urban" | 
                                                 urban_rural$UrbanRural_final == "Urban", "urban", urban_rural$UrbanRural_final))

for (i in 1:nrow(response_updated_cluster)){
  response_updated_cluster$urban_rural[i] = subset(urban_rural, urban_rural$unique.response_updated_cluster.localite_final_labels_admin2. == response_updated_cluster$localite_final_labels_admin2[i])$UrbanRural_final
}



## Prepare weights: - Need to read sampling frame from code 00700 !!
# Add stratum column for the weights
response_updated_cluster$stratum_column <- paste(response_updated_cluster$admin_2, response_updated_cluster$ig_8_statut_groupe, sep = "_")

# # drivers msni = 4
# response_updated_cluster <- response_updated_cluster %>%
#   mutate(
#     health_prot_score = ifelse(health == 4 & protection == 4, "4_4",
#                            ifelse(health == 3 & protection == 4, "3_4", 
#                                   ifelse(health == 4 & protection == 3, "4_3",
#                                          ifelse(health == 3 & protection == 3, "3_3",  NA)))),
#     prot_shelter_score = ifelse(protection == 4 & shelter == 4, "4_4",
#                                 ifelse(protection == 3 & shelter == 4, "3_4", 
#                                        ifelse(protection == 4 & shelter == 3, "4_3",
#                                               ifelse(protection == 3 & shelter == 3, "3_3",  NA)))),
#     health_shelter_score = ifelse(health == 4 & shelter == 4, "4_4",
#                                ifelse(health == 3 & shelter == 4, "3_4", 
#                                       ifelse(health == 4 & shelter == 3, "4_3",
#                                              ifelse(health == 3 & shelter == 3, "3_3",  NA)))),
#     wash_fsl_score = ifelse(wash == 4 & fsl == 4, "4_4",
#                                   ifelse(wash == 3 & fsl == 4, "3_4", 
#                                          ifelse(wash == 4 & fsl == 3, "4_3",
#                                                 ifelse(wash == 3 & fsl == 3, "3_3",  NA)))),
#     wash_capgaps_score = ifelse(wash == 4 & capacity_gaps == 4, "4_4",
#                             ifelse(wash == 3 & capacity_gaps == 4, "3_4", 
#                                    ifelse(wash == 4 & capacity_gaps == 3, "4_3",
#                                           ifelse(wash == 3 & capacity_gaps == 3, "3_3",  NA)))),
#     fsl_capgaps_score = ifelse(fsl == 4 & capacity_gaps == 4, "4_4",
#                                 ifelse(fsl == 3 & capacity_gaps == 4, "3_4", 
#                                        ifelse(fsl == 4 & capacity_gaps == 3, "4_3",
#                                               ifelse(fsl == 3 & capacity_gaps == 3, "3_3",  NA))))
#   )

# % of households having a LSG severity score of at least 3 in one or more sectors
response_updated_cluster <- response_updated_cluster %>%
  mutate(
    lsg_sev_score_3 = if_else(education >= 3, "oui", 
                              if_else(shelter >= 3, "oui", 
                                      if_else(fsl >= 3, "oui",
                                              if_else(health >= 3, "oui",
                                                      if_else(protection >= 3, "oui", 
                                                              if_else(wash >= 3, "oui", 
                                                                      if_else(impact_final >= 3, "oui", "non")))))))
  )

response_updated_cluster <- response_updated_cluster%>%
  mutate(
    ig_8_statut_groupe_uran_rural = if_else(ig_8_statut_groupe == "hote" & urban_rural == "urban", "hote_urban",
                                            if_else(ig_8_statut_groupe == "deplaces_site" & urban_rural == "urban", "deplaces_site_urban",
                                                    if_else(ig_8_statut_groupe == "deplaces_FA" & urban_rural == "urban", "deplaces_FA_urban",
                                                            if_else(ig_8_statut_groupe == "retournes" & urban_rural == "urban", "retournes_urban",
                                                                    if_else(ig_8_statut_groupe == "hote" & urban_rural == "rural", "hote_rural",
                                                                            if_else(ig_8_statut_groupe == "deplaces_site" & urban_rural == "rural", "deplaces_site_rural",
                                                                                    if_else(ig_8_statut_groupe == "deplaces_FA" & urban_rural == "rural", "deplaces_FA_rural",
                                                                                            if_else(ig_8_statut_groupe == "retournes" & urban_rural == "rural", "retournes_rural", "tbc"))))))))
  )
# Prepare final output: 
# msni_results_1 = select(response_updated_cluster, c("weights_sampling", "clusters", "stratum_column", "X_parent_index", "ig_8_statut_groupe", "admin_1", "admin_2", 
#                                                     "urban_rural", "localite_final_labels_admin2",
#                                                   "education", "fsl", "health", "protection", "shelter", "wash", "capacity_gaps", "impact_1", "msni_score_1",
#                                                   "education_cat", "fsl_cat", "health_cat", "protection_cat", "shelter_cat", "wash_cat", "capacity_gaps_cat", "impact_cat_1",
#                                                   "msni_score_cat_1"))
#                                                   # "health_prot_score", "prot_shelter_score", "health_shelter_score",
#                                                   # "wash_fsl_score" , "wash_capgaps_score" ,"fsl_capgaps_score"))
# 
# msni_results_2 = select(response_updated_cluster, c("weights_sampling", "clusters", "stratum_column", "X_parent_index", "ig_8_statut_groupe", "admin_1", "admin_2",
#                                                     "urban_rural", "localite_final_labels_admin2",
#                                                   "education", "fsl", "health", "protection", "shelter", "wash", "capacity_gaps", "impact_2", "msni_score_2",
#                                                   "education_cat", "fsl_cat", "health_cat", "protection_cat", "shelter_cat", "wash_cat", "capacity_gaps_cat", "impact_cat_2",
#                                                   "msni_score_cat_2"))
#                                                   # "health_prot_score", "prot_shelter_score", "health_shelter_score",
#                                                   # "wash_fsl_score" , "wash_capgaps_score" ,"fsl_capgaps_score"))
msni_results_3 = select(response_updated_cluster, c("weights_sampling", "clusters", "stratum_column", "X_parent_index", "ig_8_statut_groupe", "admin_1", "admin_2",
                                                    "urban_rural", "ig_8_statut_groupe_uran_rural", "localite_final_labels_admin2",
                                                    "education", "fsl", "health", "protection", "shelter", "wash", "capacity_gaps", "impact_final", "msni_score_final",
                                                    "education_cat", "fsl_cat", "health_cat", "protection_cat", "shelter_cat", "wash_cat", "capacity_gaps_cat", "impact_cat",
                                                    "msni_score_cat", "lsg_sev_score_3"))




# write.csv(msni_results_1, paste0("./output/msni/","Dataset_MSNI_RAW_impact_1_",format(Sys.time(), "%Y%m%d"),".csv"))
# write.csv(msni_results_2, paste0("./output/msni/","Dataset_MSNI_RAW_impact_2_",format(Sys.time(), "%Y%m%d"),"_2.csv"))
write.csv(msni_results_3, paste0("./output/msni/final/","Dataset_MSNI_RAW_",format(Sys.time(), "%Y%m%d"),".csv"))
