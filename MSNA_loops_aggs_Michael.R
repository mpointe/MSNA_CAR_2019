setwd("~/Desktop/Nigeria/MSNA/Ready_Data")

if (!require(readxl)) install.packages('readxl')
library(readxl)

if (!require(ggplot2)) install.packages('ggplot2')
library(ggplot2)

if (!require(reshape2)) install.packages('reshape2')
library(reshape2)

if (!require(data.table)) install.packages('data.table')
library(data.table)

if (!require(magrittr)) install.packages('magrittr')
library(magrittr)

if (!require(scales)) install.packages('scales')
library(scales)

if (!require(dplyr)) install.packages('dplyr')
library(dplyr)

if (!require(plotly)) install.packages('plotly')
library(plotly)

if (!require(tibble)) install.packages('tibble')
library(tibble)

if (!require(plotly)) install.packages('plotly')
library(plotly)

if (!require(plyr)) install.packages('plyr')
library(plyr)

if (!require(tidyr)) install.packages('tidyr')
library(tidyr)

if (!require(stringr)) install.packages('stringr')
library(stringr)

if (!require(ggrepel)) install.packages('ggrepel')
library(ggrepel)

if (!require(reshape2)) install.packages('reshape2')
library(reshape2)

if (!require(assertr)) install.packages('assertr')
library(assertr)

######BEGIN#######
#LOAN MAIN CLEAN DATA
main_data <- read_excel("reach_nga_msna_clean_dataset_final.xlsx", sheet = "clean_hh_data")
main_data <- main_data %>% dplyr:: rename(uuid = `_uuid`)

#LOAD HOUSEHOLD DEMOGRAPHIC DATA
hh_demo_data <- read_excel("reach_nga_msna_clean_dataset_final.xlsx", sheet = "ind_hh_member_data")
hh_demo_data <- hh_demo_data %>% dplyr:: rename(uuid = `UNIQUE RECORD/HOUSEHOLD IDENTIFIER`)

#CONVERT AGE TO MONTHS
hh_demo_data$`AGE - months` <- ifelse(is.na( hh_demo_data$`AGE - months`),0, hh_demo_data$`AGE - months`)
hh_demo_data$age_in_months <- (as.numeric(hh_demo_data$`AGE - years`)*12) + hh_demo_data$`AGE - months`
hh_demo_data$onesz <- 1

#CLEAN COLNAMES -- LOWERCASE AND SPACES
names(hh_demo_data) <- names(hh_demo_data) %>%
  gsub("-", "", .) %>%
  gsub(" ", "_", .) %>%
  gsub("\\?", "", .) %>%
  gsub(",", "", .) %>%
  gsub("'", "", .) %>%
  gsub("__", "_", .)%>%
  gsub("\\(", "", .) %>%
  gsub("\\)", "", .) 
#FILTER ALL KIDS REPORTED AGE MORE THAN 18
hh_demo_data <- hh_demo_data %>% setNames(make.names(names(.), unique = TRUE)) %>% dplyr:: filter(age_in_months<216)
#REMOVE :other--text" COLUMNS
hh_demo_data <- hh_demo_data %>% dplyr:: select( -contains("Other_text"))
#SEX
sex_hh <- hh_demo_data %>% dcast( uuid~ SEX, fun.aggregate = sum,value.var= "onesz", na.rm=TRUE)
#NUMBER OF PEOPLE IN HOUSEHOLD
num_people_hh <- hh_demo_data %>% dcast( uuid~ onesz, fun.aggregate = sum,value.var= "onesz", na.rm=TRUE)
#AVERAGE AGE
avg_age <- hh_demo_data %>% dcast( uuid~ onesz, fun.aggregate = mean,value.var= "age_in_months", na.rm=TRUE)
avg_age[2] <- avg_age[2]/12
#COMBINE SEX/NUMBER/AGE
demo <- merge(num_people_hh, sex_hh, by="uuid", all.x=TRUE)
colnames(demo)[2] <- "number of children"
colnames(demo)[3] <- "girls"
colnames(demo)[4] <- "boys"
demo <- merge(demo, avg_age, by="uuid", all.x=TRUE)
colnames(demo)[5] <- "average_age" 

#LOOP TO PROCESS CATEGORICAL DEMOGRAPHIC DATA
#AGE SUBSET FROM HOUSEHOLD
measurement_subset <- hh_demo_data %>% dplyr:: select(uuid, Child_MUAC_Measurement_mm)
#REMOVE PROBLEM COLUMNS
hh_demo_data$AGE_GROUP_6_months_to_5_years.1<-NULL

#SELECT FIRST AND LAST CATEGORICAL DEMOGRAPHIC VARIABLES
first_demo_col <- grep("AGE_GROUP_0_to_5_years", colnames(hh_demo_data))
last_demo_col <- grep("What_is_the_current_INFORMAL_school_attendance_status_of_", colnames(hh_demo_data))
demo_cata <- list()
#LOOP
for(i in first_demo_col :last_demo_col){
  uuid_indicator <- as.formula(paste0("uuid","~", colnames(hh_demo_data)[i]))  
  cata <- hh_demo_data %>% dcast( uuid_indicator, fun.aggregate = mean,value.var= "onesz", na.rm=TRUE)
  question_name <- colnames(hh_demo_data)[i]
  names(cata)[2:ncol(cata)] <- paste0(question_name,"_", colnames(cata)[2:ncol(cata)])
  demo_cata[[i]] <- cata
}
demo_cata[sapply(demo_cata, is.null)] <- NULL
demo_cata2 <- as.data.frame(demo_cata)
demo_cata2 <- dplyr:: select(demo_cata2, -contains("uuid."))
#CHANGE 0/1 TO YES AND NO
names(demo_cata2) <- names(demo_cata2) %>%
  gsub("\\_0\\>", "_no", .) %>%
  gsub("\\_1\\>", "_yes", .)
demo_cata2 <- demo_cata2 %>% dplyr:: select( -contains("_no")) %>%
  dplyr:: select( -contains("_No")) %>%
  dplyr:: select( -contains("_98")) %>%
  dplyr:: select( -contains("_99")) %>%
  dplyr:: select( -contains("_NA"))
#NaN to ZERO
demo_cata2 <- demo_cata2 %>% mutate_all(~replace(., is.na(.), 0))
#REMOVE COLUMNS MADE FROM TEXT CATEGORY
demo_cata2 <- demo_cata2 %>% dplyr:: select( -contains("animal_milks_y")) %>%
  dplyr:: select( -contains("liquids_yesterday_during_the_day_or_night")) %>%
  dplyr:: select( -contains("Did_consume_breastmilk_in_any_other_of_the_following_ways_yesterday_during_the"))

#JOIN SEX/AGE/NUMBER & DEMOGRAPHICS
demo <- merge(demo, demo_cata2, by="uuid", all.x=TRUE)
demo <- dplyr:: select(demo, -contains("Child_MUAC_Measurement"))

##########MUAC MEASUREMENTS###########
####MEASUREMENTS INTO 1 COLUMN
measurement_subset$onesz  <- 1
mm_measures <- measurement_subset %>% dcast( uuid~Child_MUAC_Measurement_mm, fun.aggregate = mean,value.var= "onesz", na.rm=TRUE)
mm_measures <- mm_measures %>% mutate_all(~replace(., is.na(.), 0))
muac <- list()
for(j in 1: nrow(mm_measures)){
  measure_try <- mm_measures %>% dplyr:: filter(row_number() == j) 
  measure_try[grep("NA",colnames(measure_try))] <- NULL
  idx <- measure_try[1]
  if(rowSums(measure_try[2:ncol(measure_try)])>0){
    measure_try <- measure_try[which(measure_try[1,] > 0)]
    namez <-  names(measure_try)[which(measure_try == 1, arr.ind=T)[, "col"]]
    measure_try[2:ncol(measure_try)] <- namez
    measure_try[,1] <- NULL
    measure_try <- col_concat(measure_try, sep = ";")
    measure_try <- data.frame(idx, measure_try)
    colnames(measure_try)[2] <- "MUAC_measurement_mm"
  } else {
    MUAC_measurement_mm <- NA     #MAKE 2 COLUMNS--ONE EMPTY; 1 uuid
    measure_try <- data.frame(idx, MUAC_measurement_mm)
  }
  muac[[j]] <- measure_try
}
muac[sapply(muac, is.null)] <- NULL
muac_measures  <- do.call(rbind.data.frame, muac)

#COMBINED DEMOGRAPHIC AND MUAC MEASURES
ind_hh <- merge(demo, muac_measures, by ="uuid", all.x=TRUE)

###MUAC CRITERON
muac_crit <- list()
#LOOP
for(h in 1: nrow(ind_hh)){
  grab_muac <- ind_hh  %>% dplyr :: select(uuid,MUAC_measurement_mm) %>% dplyr:: filter(row_number() == h)
  muacs <- grab_muac %>% tidyr::separate(MUAC_measurement_mm, c("M1","M2","M3","M4","M5","M6","M7","M8","M9","M10","M11","M12","M13","M14","M15"),extra='drop')
  muacs <- as.data.frame(Filter(function(x)!all(is.na(x)),muacs) ) 
  if(ncol(muacs)==1){
    next 
  } else if (ncol(muacs)==2){
    #ADD VARIES
    muacs <- add_column(muacs, "NUMBER CHILDREN with SAM (MUAC <=115)" = NA)
    muacs <- add_column(muacs, "NUMBER CHILDREN with MAM (115mm >= MUAC <125mm)" = NA)
    muacs <- add_column(muacs, "NUMBER CHILDREN with GAM (MUAC <125mm)" = NA)
    muacs <- add_column(muacs, "NUMBER CHILDREN with NORMAL (MUAC >=125mm)" = NA)
    muacs$`NUMBER CHILDREN with SAM (MUAC <=115)` <- ifelse(muacs[,2]<=115, 1, muacs$`NUMBER CHILDREN with SAM (MUAC <=115)`)
    muacs$`NUMBER CHILDREN with MAM (115mm >= MUAC <125mm)` <- ifelse(muacs[,2]>=115 & muacs[,2]<125, 1, muacs$`NUMBER CHILDREN with MAM (115mm >= MUAC <125mm)`)
    muacs$`NUMBER CHILDREN with GAM (MUAC <125mm)` <- ifelse(muacs[,2]<125 , 1, muacs$`NUMBER CHILDREN with GAM (MUAC <125mm)` )
    muacs$`NUMBER CHILDREN with NORMAL (MUAC >=125mm)` <- ifelse(muacs[,2]>=125 , 1,  muacs$`NUMBER CHILDREN with NORMAL (MUAC >=125mm)`)
  } else  {
    muacs <- add_column(muacs, "NUMBER CHILDREN with SAM (MUAC <=115)" = NA)
    muacs <- add_column(muacs, "NUMBER CHILDREN with MAM (115mm >= MUAC <125mm)" = NA)
    muacs <- add_column(muacs, "NUMBER CHILDREN with GAM (MUAC <125mm)" = NA)
    muacs <- add_column(muacs, "NUMBER CHILDREN with NORMAL (MUAC >=125mm)" = NA)
    for(i in 2: ncol(muacs)){
      #ADD VARIES
      muacs$`NUMBER CHILDREN with SAM (MUAC <=115)` <- ifelse(muacs[,i]<=115, 1, muacs$`NUMBER CHILDREN with SAM (MUAC <=115)`)
      muacs$`NUMBER CHILDREN with MAM (115mm >= MUAC <125mm)` <- ifelse(muacs[,i]>=115 & muacs[,i]<125, 1, muacs$`NUMBER CHILDREN with MAM (115mm >= MUAC <125mm)`)
      muacs$`NUMBER CHILDREN with GAM (MUAC <125mm)` <- ifelse(muacs[,i]<125 , 1, muacs$`NUMBER CHILDREN with GAM (MUAC <125mm)` )
      muacs$`NUMBER CHILDREN with NORMAL (MUAC >=125mm)` <- ifelse(muacs[,i]>=125 , 1,  muacs$`NUMBER CHILDREN with NORMAL (MUAC >=125mm)`)
    }
  }
  muacs<- muacs %>% dplyr::select(uuid, "NUMBER CHILDREN with SAM (MUAC <=115)","NUMBER CHILDREN with MAM (115mm >= MUAC <125mm)","NUMBER CHILDREN with GAM (MUAC <125mm)","NUMBER CHILDREN with NORMAL (MUAC >=125mm)")
  muac_crit[[h]] <- muacs
}
muac_crit[sapply(muac_crit, is.null)] <- NULL
muac_crit_go  <- do.call(rbind.data.frame, muac_crit)

#COMBINED DEMOGRAPHIC AND MUAC INDICATORS
ind_hh <- merge(ind_hh, muac_crit_go, by ="uuid", all.x=TRUE)	


###WATER USAGE LOOP
ind_waterloop_data_data <- read_excel("reach_nga_msna_clean_dataset_final.xlsx", sheet = "ind_waterloop_data")
ind_waterloop_data_data <- ind_waterloop_data_data %>% dplyr:: rename(uuid = `_uuid`)
ind_waterloop_data_data$onesz <- 1

#CREATE INDICATOR == volume*refills TOTAL LITRES OF WATER YESTERDAY
ind_waterloop_data_data <- as.data.frame(append(ind_waterloop_data_data, list(volume_fill = ind_waterloop_data_data$volume_ct*ind_waterloop_data_data$num_filled_ct), after =  grep("cost_fill_ct" ,colnames(ind_waterloop_data_data))))
#SUM WATER COLLECTED
water_collected_total <- ind_waterloop_data_data %>% dcast( uuid~ onesz, fun.aggregate = sum,value.var= "volume_fill", na.rm=TRUE)
colnames(water_collected_total)[2] <- "TOTAL QUANTITY WATER COLLECTED PREVIOUS DAY"

#CREATE INDICATOR == cost*refills == TOTAL COST OF WATER YESTERDAY
ind_waterloop_data_data <- as.data.frame(append(ind_waterloop_data_data, list(cost_fill = ind_waterloop_data_data$num_filled_ct*ind_waterloop_data_data$cost_fill_ct), after =  grep("volume_fill" ,colnames(ind_waterloop_data_data))))
#TOTAL COST OF WATER PREVIOUS DAY
water_cost_total <- ind_waterloop_data_data %>% dcast( uuid~ onesz, fun.aggregate = sum,value.var= "cost_fill", na.rm=TRUE)
colnames(water_cost_total)[2] <- "TOTAL COST OF WATER PREVIOUS DAY"

#SUM WATER CONTAINERS
sum_water_containers <- ind_waterloop_data_data %>% dcast( uuid~ onesz, fun.aggregate = sum,value.var= "num_filled_ct", na.rm=TRUE)
colnames(sum_water_containers)[2] <- "Number water containers for collecting water"

#COMBINE ELEMENTS OF WATER LOOP
water_loop <- merge(water_cost_total, sum_water_containers, by="uuid", all.x=TRUE)
water_loop <- merge(water_loop, water_collected_total, by="uuid", all.x=TRUE)


#####MORE VARIABLES TO CREATE######
"NUMBER CHILDREN IN HOUSEHOLD ELIGIBLE FOR MEASLES VACCINE (AGES 9 months to 10 y"
"NUMBER CHILDREN IN HOUSEHOLD ELIGIBLE FOR POLIO or PENA VACCINES (Ages 0-5 years	"
"NUMBER AGE ELIGIBLE CHILDREN RECEIVED ANY MEASLES VACCINE"
"NUMBER AGE ELIGIBLE CHILDREN RECEIVED ANY POLIO VACCINE"
"NUMBER AGE ELIGIBLE CHILDREN RECEIVED ANY PENTA VACCINE"	
"NUMBER CHILDREN ELIGIBLE FOR EXCLUSIVE BREASTFEEDING (AGES 0-6 MONTHS)"	
"NUMBER AGE ELIGIBLE CHILDREN EXCLUSIVELY BREASTFEEDING"	
"NUMBER CHILDREN ELIGIBLE FOR MUAC (AGES 6-59 MONTHS)"	

#GAP
"NUMBER CHILDREN with OEDEMA	NUMBER CHILDRNE with SAM in OTP"

#HOW TO WEIGHT WATER -- LGA
#LGA-level, sum after multiply by weight
#HOW TO WEIGHT WATER -- STATE
#STATE-level, sum after multiply by weight







