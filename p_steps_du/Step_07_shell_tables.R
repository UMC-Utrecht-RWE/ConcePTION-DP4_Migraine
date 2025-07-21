
############################################################
#                                                          #
####                    Shell Tables                    ####
#                                                          #
############################################################

library(tidyverse)
library(data.table)
library(dplyr)



#Function for calculating 95% CI
scoreci <- function(success, total, level = 0.95, return_midpoint = FALSE) {
  zalpha <- abs(stats::qnorm((1 - level) / 2))
  estimate <- success / total
  bound <- (zalpha * ((estimate * (1 - estimate) + (zalpha**2) / (4 * total)) / total)**(1 / 2)) /
    (1 + (zalpha**2) / total)
  midpoint <- (estimate + (zalpha**2) / (2 * total)) / (1 + (zalpha**2) / total)
  
  conf.low  <- midpoint - bound
  conf.high <- midpoint + bound
  
  if(return_midpoint)
    data.frame(success, total, estimate, conf.low, conf.high, midpoint, level)
  else
    data.frame(success, total, estimate, conf.low, conf.high, level)
}

replace_na = function(x,val) ifelse(is.na(x),val,x)


# Loading data
# if(data_access_provider_name=="NIHW"){
#   preg_0 <- readRDS('g_intermediate/pregnancy_d3/pregnancy_d3_DU_b.rds')
# }else{
#   preg_0 <- readRDS('g_intermediate/pregnancy_d3/pregnancy_d3_DU.rds')  
# }

load_cov <- function(cov_name, medicine = NULL){
  
  if(!is.null(medicine)){
    return(readRDS(file.path(projectFolder, "g_intermediate", "medicines_d3", paste0(medicine, ".rds"))))
  }

  
  fixed_file = file.path(projectFolder, "g_intermediate", "diagnoses_d3", paste0(cov_name, "_fixed", ".rds"))
  cehckbox_file = file.path(projectFolder,  "g_intermediate", "diagnoses_d3", paste0(cov_name, "_checkbox", ".rds"))
  diagnoses_file = file.path(projectFolder, "g_intermediate", "diagnoses_d3", paste0(cov_name, "_diagnoses", ".rds"))
  
  if(file.exists(fixed_file)){
    fx = readRDS(fixed_file)
    fx$pregnancy_id = as.character(fx$pregnancy_id)
    fx$person_id = as.character(fx$person_id)
    fx$event_date = as.Date(fx$event_date)
  }else{
    fx = data.table(person_id = NA_character_, pregnancy_id =NA_character_, event_date = NA_Date_, variable_name = cov_name, value = NA_real_)
  }
  
  if(file.exists(cehckbox_file)){
    chk = readRDS(cehckbox_file)
    chk$pregnancy_id = as.character(chk$pregnancy_id)
    chk$person_id = as.character(chk$person_id)
    chk$event_date = as.Date(chk$event_date)
  }else{
    chk = data.table(person_id = NA_character_, pregnancy_id =NA_character_, event_date = NA_Date_, variable_name = cov_name)
  }
  
  if(file.exists(diagnoses_file)){
    diag = readRDS(diagnoses_file)
    diag$pregnancy_id = as.character(diag$pregnancy_id)
    diag$person_id = as.character(diag$person_id)
    diag$event_date = as.Date(diag$event_date)
  }else{
    diag = data.table(person_id = NA_character_, pregnancy_id =NA_character_, event_date = NA_Date_, variable_name = cov_name)
  }
  
  if("event_code" %in% colnames(fx)) setnames(fx, "event_code", "value")
  
  setnames(fx, "value", "fixed_value")
  fx$fixed_value = as.numeric(fx$fixed_value)
  fx[, chk := FALSE]
  chk[, fixed_value := fx$fixed_value[1]*NA][, chk:=TRUE]
  diag[, fixed_value :=fx$fixed_value[1]*NA][, chk:=TRUE]
  
  fx = fx[!is.na(person_id)]
  chk = chk[!is.na(person_id)]
  diag = diag[!is.na(person_id)]
  
  rbindlist(list(fx, chk, diag), use.names = TRUE, fill = TRUE)
}

triptans    = load_cov(medicine = 'TRIPTANS') 
paracetamol = load_cov(medicine = 'PARACETAMOL') 
codeine     = load_cov(medicine = 'CODEINE_PARACETAMOL') 
nsaid       = load_cov(medicine = 'NSAID') 
antinausea  = load_cov(medicine = 'ANTI_NAUSEA') 
prev_med    = load_cov(medicine = 'PREV_MIG_MED') 
cgrp        = load_cov(medicine = 'CGRP_MED') 

obesity     = load_cov('OW') 
weight      = load_cov('PBMI_w')
height      = load_cov('PBMI_h')
obesity2    = merge(weight, height, by = c("person_id", "pregnancy_id"), all = TRUE, allow.cartesian = TRUE)

height_mean = suppressWarnings(mean(height$fixed_value, na.rm = TRUE))
if(!is.na(height_mean) && height_mean>100) height$fixed_value = height$fixed_value/100

obesity2 = suppressWarnings(obesity2 %>% group_by(pregnancy_id) %>% summarise(
  person_id = person_id[1],
  fixed_value = max(fixed_value.x) / max(fixed_value.y^2),
  chk = FALSE*NA,
  event_date = max(max(event_date.x, na.rm = TRUE), max(event_date.y, na.rm = TRUE), na.rm = TRUE),
  variable_name = "OW"
))
obesity2 = as.data.table(obesity2)
obesity2[is.infinite(fixed_value), fixed_value:=NA_real_]
obesity = rbindlist(list(obesity, obesity2), use.names = TRUE, fill = TRUE)

parity      = load_cov('PARITY') 
HT          = load_cov('BLHT') 
diab        = load_cov('BLDM') 
dep         = load_cov('BLDEP') 



#Select population for the analyses (all migraineurs)
if(exists("DU_sensitivity_analysis")&&DU_sensitivity_analysis){
  preg_0      = readRDS('g_intermediate/pregnancy_d3/pregnancy_d3_DU_S.rds')  
  csv_path = "g_output/Drug utilisation/csv/sensitivity_analysis"
  preg_0[,MIG_S4:=NA]
}else{
  # if(exists("revised_sensitivity") && revised_sensitivity){
  #   preg_0      = readRDS('g_intermediate/pregnancy_d3/pregnancy_d3_DU_during2.rds')  
  # }else{
  #   preg_0      = readRDS('g_intermediate/pregnancy_d3/pregnancy_d3_DU_during.rds') 
  # }
  preg_0      = readRDS('g_intermediate/pregnancy_d3/pregnancy_d3_DU_during2.rds') 
  csv_path = "g_output/Drug utilisation/csv"
}

elig_prim <- table(preg_0$MIG_DxRx)
DUS_all <- preg_0[preg_0$MIG_DxRx==1, ] 
# min(preg_0$pregnancy_start_date)  #Report on earliest LMP date
# max(preg_0$pregnancy_start_date)  #Report on latest LMP date

if(!dir.exists(csv_path)){
  dir.create(csv_path, recursive = TRUE)
}

rm(elig_prim, elig_sens, preg_0)


DUS_all2 <- merge(DUS_all, triptans, by = "person_id", all.x = TRUE, allow.cartesian = TRUE) #many-to-many relationship
DUS_all2$gap_LMP_a <- difftime(DUS_all2$medicine_date, DUS_all2$pregnancy_start_date, units = "days")
DUS_all2$gap_del_a <- difftime(DUS_all2$medicine_date, DUS_all2$pregnancy_end_date, units = "days")
DUS_all2$trip_prior_0_12_a <- ifelse((DUS_all2$gap_LMP >= -365 & DUS_all2$gap_LMP <= -1), 1, 0)
DUS_all2$trip_prior_0_3_a <- ifelse((DUS_all2$gap_LMP >= -90 & DUS_all2$gap_LMP <= -1), 1, 0)
DUS_all2$trip_preg_a <- ifelse((DUS_all2$gap_LMP >= 0 & DUS_all2$gap_del <= 0), 1, 0)
DUS_all2$trip_T1_a <- ifelse((DUS_all2$gap_LMP >= 0 & DUS_all2$gap_LMP <= 97 & DUS_all2$gap_del<=0), 1, 0)
DUS_all2$trip_T2_a <- ifelse((DUS_all2$gap_LMP >= 98 & DUS_all2$gap_LMP <= 195 & DUS_all2$gap_del<=0), 1, 0)
DUS_all2$trip_T3_a <- ifelse((DUS_all2$gap_LMP >= 196 & DUS_all2$gap_del <= 0), 1, 0)
DUS_all2$trip_post_a <- ifelse((DUS_all2$gap_del >= 1 & DUS_all2$gap_del <= 90), 1, 0)
#number of prescriptions filled per time period by pregnancy
DUS_all3 = DUS_all2 %>%
  group_by(pregnancy_id) %>%
  summarize(trip_prior_0_12_n = sum(trip_prior_0_12_a))
DUS_all <- merge(DUS_all, DUS_all3, by = c("pregnancy_id"))
DUS_all3 = DUS_all2 %>%
  group_by(pregnancy_id) %>%
  summarize(trip_prior_0_3_n = sum(trip_prior_0_3_a))
DUS_all <- merge(DUS_all, DUS_all3, by = c("pregnancy_id"))
DUS_all3 = DUS_all2 %>%
  group_by(pregnancy_id) %>%
  summarize(trip_preg_n = sum(trip_preg_a))
DUS_all <- merge(DUS_all, DUS_all3, by = c("pregnancy_id"))
DUS_all3 = DUS_all2 %>%
  group_by(pregnancy_id) %>%
  summarize(trip_T1_n = sum(trip_T1_a))
DUS_all <- merge(DUS_all, DUS_all3, by = c("pregnancy_id"))
DUS_all3 = DUS_all2 %>%
  group_by(pregnancy_id) %>%
  summarize(trip_T2_n = sum(trip_T2_a))
DUS_all <- merge(DUS_all, DUS_all3, by = c("pregnancy_id"))
DUS_all3 = DUS_all2 %>%
  group_by(pregnancy_id) %>%
  summarize(trip_T3_n = sum(trip_T3_a))
DUS_all <- merge(DUS_all, DUS_all3, by = c("pregnancy_id"))
DUS_all3 = DUS_all2 %>%
  group_by(pregnancy_id) %>%
  summarize(trip_post_n = sum(trip_post_a))
DUS_all <- merge(DUS_all, DUS_all3, by = c("pregnancy_id"))
rm(DUS_all2)
rm(DUS_all3)
rm(triptans)

#Set denominators for trimesters in pregnancy
DUS_all$gest_age <- difftime(DUS_all$pregnancy_end_date, DUS_all$pregnancy_start_date, units = "days")
DUS_all$denom_T1 <- ifelse((DUS_all$gest_age >= 97), 1, 0)
DUS_all$denom_T2 <- ifelse((DUS_all$gest_age >= 195), 1, 0)
DUS_all$denom_T3 <- ifelse((DUS_all$gest_age >= 196), 1, 0)

#Dichotomized exposure per period: Primary exposure definition
DUS_all$trip_prior_0_12 <- ifelse((DUS_all$trip_prior_0_12_n >= 1), 1, 0)
DUS_all$trip_prior_0_3 <- ifelse((DUS_all$trip_prior_0_3_n >= 1), 1, 0)
DUS_all$trip_preg <- ifelse((DUS_all$trip_preg_n >= 1), 1, 0)
DUS_all$trip_T1 <- ifelse((DUS_all$trip_T1_n >= 1), 1, 0)
DUS_all[(denom_T1 == 0), trip_T1 := NA]
DUS_all$trip_T2 <- ifelse((DUS_all$trip_T2_n >= 1), 1, 0)
DUS_all[(denom_T2 == 0), trip_T2 := NA]
DUS_all$trip_T3 <- ifelse((DUS_all$trip_T3_n >= 1), 1, 0)
DUS_all[(denom_T3 == 0), trip_T3 := NA]
DUS_all$trip_post <- ifelse((DUS_all$trip_post_n >= 1), 1, 0)

#Dichotomized exposure per period: Conservative exposure definition
DUS_all$trip_prior_0_12_c <- ifelse((DUS_all$trip_prior_0_12_n >= 2), 1, 0)
DUS_all$trip_prior_0_3_c <- ifelse((DUS_all$trip_prior_0_3_n >= 2), 1, 0)
DUS_all$trip_preg_c <- ifelse((DUS_all$trip_preg_n >= 2), 1, 0)
DUS_all$trip_T1_c <- ifelse((DUS_all$trip_T1_n >= 2), 1, 0)
DUS_all[(denom_T1 == 0), trip_T1_c := NA]
DUS_all$trip_T2_c <- ifelse((DUS_all$trip_T2_n >= 2), 1, 0)
DUS_all[(denom_T2 == 0), trip_T2_c := NA]
DUS_all$trip_T3_c <- ifelse((DUS_all$trip_T3_n >= 2), 1, 0)
DUS_all[(denom_T3 == 0), trip_T3_c := NA]
DUS_all$trip_post_c <- ifelse((DUS_all$trip_post_n >= 2), 1, 0)

#Triptan use groups
DUS_all[is.na(trip_prior_0_12), trip_prior_0_12:=0]
DUS_all[is.na(trip_prior_0_3), trip_prior_0_3:=0]
DUS_all[is.na(trip_preg), trip_preg:=0]

DUS_all[((trip_prior_0_12 == 1 | trip_prior_0_3 == 1) & trip_preg == 0), trip_group := "Discontinuer"]
DUS_all[((trip_prior_0_12 == 0 & trip_prior_0_3 == 0) & trip_preg == 0), trip_group := "Non-user"]

DUS_all[((trip_prior_0_12 == 1 | trip_prior_0_3 == 1) & trip_preg == 1), trip_group := "Continuer"]
DUS_all[((trip_prior_0_12 == 0 & trip_prior_0_3 == 0) & trip_preg == 1), trip_group := "Initiator"]


############# Table 1 #########
cat("\nCreating Table 1 ...\n")

#Primary exposure definition
denom_all <- length(unique(DUS_all$pregnancy_id))
denom_T1 <- nrow(DUS_all[DUS_all$denom_T1 == 1, ])
denom_T2 <- nrow(DUS_all[DUS_all$denom_T2 == 1, ])
denom_T3 <- nrow(DUS_all[DUS_all$denom_T3 == 1, ])
triptans_prior_0_12 <- nrow(DUS_all[DUS_all$trip_prior_0_12 == 1, ])
triptans_prior_0_3 <- nrow(DUS_all[DUS_all$trip_prior_0_3 == 1, ])
triptans_preg <- nrow(DUS_all[DUS_all$trip_preg == 1, ])
triptans_T1 <- nrow(DUS_all[DUS_all$trip_T1 == 1, ])
triptans_T2 <- nrow(DUS_all[DUS_all$trip_T2 == 1, ])
triptans_T3 <- nrow(DUS_all[DUS_all$trip_T3 == 1, ])
triptans_post <- nrow(DUS_all[DUS_all$trip_post == 1, ])
triptans_prior_0_12_prev <- round((triptans_prior_0_12 / denom_all)*100, 3)
triptans_prior_0_3_prev <- round((triptans_prior_0_3 / denom_all)*100, 3)
triptans_preg_prev <- round((triptans_preg / denom_all)*100, 3)
triptans_T1_prev <- round((triptans_T1 / denom_T1)*100, 3)
triptans_T2_prev <- round((triptans_T2 / denom_T2)*100, 3)
triptans_T3_prev <- round((triptans_T3 / denom_T3)*100, 3)
triptans_post_prev <- round((triptans_post / denom_all)*100, 3)
tript_prior_0_12 <- scoreci(triptans_prior_0_12, denom_all)
tript_prior_0_3 <- scoreci(triptans_prior_0_3, denom_all)
tript_preg <- scoreci(triptans_preg, denom_all)
tript_T1 <- scoreci(triptans_T1, denom_T1)
tript_T2 <- scoreci(triptans_T2, denom_T2)
tript_T3 <- scoreci(triptans_T3, denom_T3)
tript_post <- scoreci(triptans_post, denom_all)
triptans_prior_0_12_low_CI <- round((tript_prior_0_12$conf.low)*100, 3)
triptans_prior_0_12_up_CI <- round((tript_prior_0_12$conf.high)*100, 3)
triptans_prior_0_3_low_CI <- round((tript_prior_0_3$conf.low)*100, 3)
triptans_prior_0_3_up_CI <- round((tript_prior_0_3$conf.high)*100, 3)
triptans_preg_low_CI <- round((tript_preg$conf.low)*100, 3)
triptans_preg_up_CI <- round((tript_preg$conf.high)*100, 3)
triptans_T1_low_CI <- round((tript_T1$conf.low)*100, 3)
triptans_T1_up_CI <- round((tript_T1$conf.high)*100, 3)
triptans_T2_low_CI <- round((tript_T2$conf.low)*100, 3)
triptans_T2_up_CI <- round((tript_T2$conf.high)*100, 3)
triptans_T3_low_CI <- round((tript_T3$conf.low)*100, 3)
triptans_T3_up_CI <- round((tript_T3$conf.high)*100, 3)
triptans_post_low_CI <- round((tript_post$conf.low)*100, 3)
triptans_post_up_CI <- round((tript_post$conf.high)*100, 3)

table1_prim <- data.frame(
  Time_period = c("0-12 months before LMP", "0-3 months before LMP", "Any time during pregnancy", "Trimester 1", "Trimester 2", "Trimester 3", "0-3 months after delivery"),
  N_pregnancies = c(denom_all, denom_all, denom_all, denom_T1, denom_T2, denom_T3, denom_all),
  N_users = c(triptans_prior_0_12, triptans_prior_0_3, triptans_preg, triptans_T1, triptans_T2, triptans_T3, triptans_post),
  Prevalence = c(triptans_prior_0_12_prev, triptans_prior_0_3_prev, triptans_preg_prev, triptans_T1_prev, triptans_T2_prev, triptans_T3_prev, triptans_post_prev),
  CI_lower = c(triptans_prior_0_12_low_CI, triptans_prior_0_3_low_CI, triptans_preg_low_CI, triptans_T1_low_CI, triptans_T2_low_CI, triptans_T3_low_CI, triptans_post_low_CI),
  CI_upper = c(triptans_prior_0_12_up_CI, triptans_prior_0_3_up_CI, triptans_preg_up_CI, triptans_T1_up_CI, triptans_T2_up_CI, triptans_T3_up_CI, triptans_post_up_CI)
)

write.csv(table1_prim, file.path(csv_path, "Table1_primary.csv"), row.names = FALSE)  ##Path to output folder

#Conservative exposure definition
triptans_prior_0_12_c <- nrow(DUS_all[DUS_all$trip_prior_0_12_c == 1, ])
triptans_prior_0_3_c <- nrow(DUS_all[DUS_all$trip_prior_0_3_c == 1, ])
triptans_preg_c <- nrow(DUS_all[DUS_all$trip_preg_c == 1, ])
triptans_T1_c <- nrow(DUS_all[DUS_all$trip_T1_c == 1, ])
triptans_T2_c <- nrow(DUS_all[DUS_all$trip_T2_c == 1, ])
triptans_T3_c <- nrow(DUS_all[DUS_all$trip_T3_c == 1, ])
triptans_post_c <- nrow(DUS_all[DUS_all$trip_post_c == 1, ])
triptans_prior_0_12_prev_c <- round((triptans_prior_0_12_c / denom_all)*100, 3)
triptans_prior_0_3_prev_c <- round((triptans_prior_0_3_c / denom_all)*100, 3)
triptans_preg_prev_c <- round((triptans_preg_c / denom_all)*100, 3)
triptans_T1_prev_c <- round((triptans_T1_c / denom_T1)*100, 3)
triptans_T2_prev_c <- round((triptans_T2_c / denom_T2)*100, 3)
triptans_T3_prev_c <- round((triptans_T3_c / denom_T3)*100, 3)
triptans_post_prev_c <- round((triptans_post_c / denom_all)*100, 3)
tript_prior_0_12_c <- scoreci(triptans_prior_0_12_c, denom_all)
tript_prior_0_3_c <- scoreci(triptans_prior_0_3_c, denom_all)
tript_preg_c <- scoreci(triptans_preg_c, denom_all)
tript_T1_c <- scoreci(triptans_T1_c, denom_T1)
tript_T2_c <- scoreci(triptans_T2_c, denom_T2)
tript_T3_c <- scoreci(triptans_T3_c, denom_T3)
tript_post_c <- scoreci(triptans_post_c, denom_all)
triptans_prior_0_12_low_CI_c <- round((tript_prior_0_12_c$conf.low)*100, 3)
triptans_prior_0_12_up_CI_c <- round((tript_prior_0_12_c$conf.high)*100, 3)
triptans_prior_0_3_low_CI_c <- round((tript_prior_0_3_c$conf.low)*100, 3)
triptans_prior_0_3_up_CI_c <- round((tript_prior_0_3_c$conf.high)*100, 3)
triptans_preg_low_CI_c <- round((tript_preg_c$conf.low)*100, 3)
triptans_preg_up_CI_c <- round((tript_preg_c$conf.high)*100, 3)
triptans_T1_low_CI_c <- round((tript_T1_c$conf.low)*100, 3)
triptans_T1_up_CI_c <- round((tript_T1_c$conf.high)*100, 3)
triptans_T2_low_CI_c <- round((tript_T2_c$conf.low)*100, 3)
triptans_T2_up_CI_c <- round((tript_T2_c$conf.high)*100, 3)
triptans_T3_low_CI_c <- round((tript_T3_c$conf.low)*100, 3)
triptans_T3_up_CI_c <- round((tript_T3_c$conf.high)*100, 3)
triptans_post_low_CI_c <- round((tript_post_c$conf.low)*100, 3)
triptans_post_up_CI_c <- round((tript_post_c$conf.high)*100, 3)

table1_cons <- data.frame(
  Time_period = c("0-12 months before LMP", "0-3 months before LMP", "Any time during pregnancy", "Trimester 1", "Trimester 2", "Trimester 3", "0-3 months after delivery"),
  N_pregnancies = c(denom_all, denom_all, denom_all, denom_T1, denom_T2, denom_T3, denom_all),
  N_users = c(triptans_prior_0_12_c, triptans_prior_0_3_c, triptans_preg_c, triptans_T1_c, triptans_T2_c, triptans_T3_c, triptans_post_c),
  Prevalence = c(triptans_prior_0_12_prev_c, triptans_prior_0_3_prev_c, triptans_preg_prev_c, triptans_T1_prev_c, triptans_T2_prev_c, triptans_T3_prev_c, triptans_post_prev_c),
  CI_lower = c(triptans_prior_0_12_low_CI_c, triptans_prior_0_3_low_CI_c, triptans_preg_low_CI_c, triptans_T1_low_CI_c, triptans_T2_low_CI_c, triptans_T3_low_CI_c, triptans_post_low_CI_c),
  CI_upper = c(triptans_prior_0_12_up_CI_c, triptans_prior_0_3_up_CI_c, triptans_preg_up_CI_c, triptans_T1_up_CI_c, triptans_T2_up_CI_c, triptans_T3_up_CI_c, triptans_post_up_CI_c)
)

write.csv(table1_cons, file.path(csv_path, "Table1_conservative.csv"), row.names = FALSE)  ##Path to output folder

rm(tript_post, tript_post_c, tript_preg, tript_preg_c, tript_prior_0_12, tript_prior_0_12_c, tript_prior_0_3, tript_prior_0_3_c, tript_T1, tript_T1_c, tript_T2, tript_T2_c, tript_T3, tript_T3_c)
rm(list = ls.str(mode = "numeric"))


############# Table 2 #########
cat("\nCreating Table 2 ...\n")

#Time period 2009-2012
denom_all_0912 <- nrow(DUS_all[DUS_all$year_group == "2009-2012", ])
denom_T1_0912 <- nrow(DUS_all[(DUS_all$year_group == "2009-2012" & DUS_all$denom_T1 == 1), ])
denom_T2_0912 <- nrow(DUS_all[(DUS_all$year_group == "2009-2012" & DUS_all$denom_T2 == 1), ])
denom_T3_0912 <- nrow(DUS_all[(DUS_all$year_group == "2009-2012" & DUS_all$denom_T3 == 1), ])

triptans_prior_0_12 <- nrow(DUS_all[DUS_all$year_group == "2009-2012" & DUS_all$trip_prior_0_12 == 1, ])
triptans_prior_0_3 <- nrow(DUS_all[DUS_all$year_group == "2009-2012" & DUS_all$trip_prior_0_3 == 1, ])
triptans_preg <- nrow(DUS_all[DUS_all$year_group == "2009-2012" & DUS_all$trip_preg == 1, ])
triptans_T1 <- nrow(DUS_all[DUS_all$year_group == "2009-2012" & DUS_all$trip_T1 == 1, ])
triptans_T2 <- nrow(DUS_all[DUS_all$year_group == "2009-2012" & DUS_all$trip_T2 == 1, ])
triptans_T3 <- nrow(DUS_all[DUS_all$year_group == "2009-2012" & DUS_all$trip_T3 == 1, ])
triptans_post <- nrow(DUS_all[DUS_all$year_group == "2009-2012" & DUS_all$trip_post == 1, ])
triptans_prior_0_12_prev <- round((triptans_prior_0_12 / denom_all_0912)*100, 3)
triptans_prior_0_3_prev <- round((triptans_prior_0_3 / denom_all_0912)*100, 3)
triptans_preg_prev <- round((triptans_preg / denom_all_0912)*100, 3)
triptans_T1_prev <- round((triptans_T1 / denom_T1_0912)*100, 3)
triptans_T2_prev <- round((triptans_T2 / denom_T2_0912)*100, 3)
triptans_T3_prev <- round((triptans_T3 / denom_T3_0912)*100, 3)
triptans_post_prev <- round((triptans_post / denom_all_0912)*100, 3)
tript_prior_0_12 <- scoreci(triptans_prior_0_12, denom_all_0912)
tript_prior_0_3 <- scoreci(triptans_prior_0_3, denom_all_0912)
tript_preg <- scoreci(triptans_preg, denom_all_0912)
tript_T1 <- scoreci(triptans_T1, denom_T1_0912)
tript_T2 <- scoreci(triptans_T2, denom_T2_0912)
tript_T3 <- scoreci(triptans_T3, denom_T3_0912)
tript_post <- scoreci(triptans_post, denom_all_0912)
triptans_prior_0_12_low_CI <- round((tript_prior_0_12$conf.low)*100, 3)
triptans_prior_0_12_up_CI <- round((tript_prior_0_12$conf.high)*100, 3)
triptans_prior_0_3_low_CI <- round((tript_prior_0_3$conf.low)*100, 3)
triptans_prior_0_3_up_CI <- round((tript_prior_0_3$conf.high)*100, 3)
triptans_preg_low_CI <- round((tript_preg$conf.low)*100, 3)
triptans_preg_up_CI <- round((tript_preg$conf.high)*100, 3)
triptans_T1_low_CI <- round((tript_T1$conf.low)*100, 3)
triptans_T1_up_CI <- round((tript_T1$conf.high)*100, 3)
triptans_T2_low_CI <- round((tript_T2$conf.low)*100, 3)
triptans_T2_up_CI <- round((tript_T2$conf.high)*100, 3)
triptans_T3_low_CI <- round((tript_T3$conf.low)*100, 3)
triptans_T3_up_CI <- round((tript_T3$conf.high)*100, 3)
triptans_post_low_CI <- round((tript_post$conf.low)*100, 3)
triptans_post_up_CI <- round((tript_post$conf.high)*100, 3)

table2_0912 <- data.frame(
  Time_period = c("0-12 months before LMP", "0-3 months before LMP", "Any time during pregnancy", "Trimester 1", "Trimester 2", "Trimester 3", "0-3 months after delivery"),
  N_pregnancies = c(denom_all_0912, denom_all_0912, denom_all_0912, denom_T1_0912, denom_T2_0912, denom_T3_0912, denom_all_0912),
  N_users = c(triptans_prior_0_12, triptans_prior_0_3, triptans_preg, triptans_T1, triptans_T2, triptans_T3, triptans_post),
  Prevalence = c(triptans_prior_0_12_prev, triptans_prior_0_3_prev, triptans_preg_prev, triptans_T1_prev, triptans_T2_prev, triptans_T3_prev, triptans_post_prev),
  CI_lower = c(triptans_prior_0_12_low_CI, triptans_prior_0_3_low_CI, triptans_preg_low_CI, triptans_T1_low_CI, triptans_T2_low_CI, triptans_T3_low_CI, triptans_post_low_CI),
  CI_upper = c(triptans_prior_0_12_up_CI, triptans_prior_0_3_up_CI, triptans_preg_up_CI, triptans_T1_up_CI, triptans_T2_up_CI, triptans_T3_up_CI, triptans_post_up_CI)
)

write.csv(table2_0912, file.path(csv_path, "Table2_2009_2012.csv"), row.names = FALSE)  ##Path to output folder

rm(tript_post, tript_preg, tript_prior_0_12, tript_prior_0_3, tript_T1, tript_T2, tript_T3)
rm(list = ls.str(mode = "numeric"))

#Time period 2013-2016
denom_all_1316 <- nrow(DUS_all[DUS_all$year_group == "2013-2016", ])
denom_T1_1316 <- nrow(DUS_all[(DUS_all$year_group == "2013-2016" & DUS_all$denom_T1 == 1), ])
denom_T2_1316 <- nrow(DUS_all[(DUS_all$year_group == "2013-2016" & DUS_all$denom_T2 == 1), ])
denom_T3_1316 <- nrow(DUS_all[(DUS_all$year_group == "2013-2016" & DUS_all$denom_T3 == 1), ])

triptans_prior_0_12 <- nrow(DUS_all[DUS_all$year_group == "2013-2016" & DUS_all$trip_prior_0_12 == 1, ])
triptans_prior_0_3 <- nrow(DUS_all[DUS_all$year_group == "2013-2016" & DUS_all$trip_prior_0_3 == 1, ])
triptans_preg <- nrow(DUS_all[DUS_all$year_group == "2013-2016" & DUS_all$trip_preg == 1, ])
triptans_T1 <- nrow(DUS_all[DUS_all$year_group == "2013-2016" & DUS_all$trip_T1 == 1, ])
triptans_T2 <- nrow(DUS_all[DUS_all$year_group == "2013-2016" & DUS_all$trip_T2 == 1, ])
triptans_T3 <- nrow(DUS_all[DUS_all$year_group == "2013-2016" & DUS_all$trip_T3 == 1, ])
triptans_post <- nrow(DUS_all[DUS_all$year_group == "2013-2016" & DUS_all$trip_post == 1, ])
triptans_prior_0_12_prev <- round((triptans_prior_0_12 / denom_all_1316)*100, 3)
triptans_prior_0_3_prev <- round((triptans_prior_0_3 / denom_all_1316)*100, 3)
triptans_preg_prev <- round((triptans_preg / denom_all_1316)*100, 3)
triptans_T1_prev <- round((triptans_T1 / denom_T1_1316)*100, 3)
triptans_T2_prev <- round((triptans_T2 / denom_T2_1316)*100, 3)
triptans_T3_prev <- round((triptans_T3 / denom_T3_1316)*100, 3)
triptans_post_prev <- round((triptans_post / denom_all_1316)*100, 3)
tript_prior_0_12 <- scoreci(triptans_prior_0_12, denom_all_1316)
tript_prior_0_3 <- scoreci(triptans_prior_0_3, denom_all_1316)
tript_preg <- scoreci(triptans_preg, denom_all_1316)
tript_T1 <- scoreci(triptans_T1, denom_T1_1316)
tript_T2 <- scoreci(triptans_T2, denom_T2_1316)
tript_T3 <- scoreci(triptans_T3, denom_T3_1316)
tript_post <- scoreci(triptans_post, denom_all_1316)
triptans_prior_0_12_low_CI <- round((tript_prior_0_12$conf.low)*100, 3)
triptans_prior_0_12_up_CI <- round((tript_prior_0_12$conf.high)*100, 3)
triptans_prior_0_3_low_CI <- round((tript_prior_0_3$conf.low)*100, 3)
triptans_prior_0_3_up_CI <- round((tript_prior_0_3$conf.high)*100, 3)
triptans_preg_low_CI <- round((tript_preg$conf.low)*100, 3)
triptans_preg_up_CI <- round((tript_preg$conf.high)*100, 3)
triptans_T1_low_CI <- round((tript_T1$conf.low)*100, 3)
triptans_T1_up_CI <- round((tript_T1$conf.high)*100, 3)
triptans_T2_low_CI <- round((tript_T2$conf.low)*100, 3)
triptans_T2_up_CI <- round((tript_T2$conf.high)*100, 3)
triptans_T3_low_CI <- round((tript_T3$conf.low)*100, 3)
triptans_T3_up_CI <- round((tript_T3$conf.high)*100, 3)
triptans_post_low_CI <- round((tript_post$conf.low)*100, 3)
triptans_post_up_CI <- round((tript_post$conf.high)*100, 3)

table2_1316 <- data.frame(
  Time_period = c("0-12 months before LMP", "0-3 months before LMP", "Any time during pregnancy", "Trimester 1", "Trimester 2", "Trimester 3", "0-3 months after delivery"),
  N_pregnancies = c(denom_all_1316, denom_all_1316, denom_all_1316, denom_T1_1316, denom_T2_1316, denom_T3_1316, denom_all_1316),
  N_users = c(triptans_prior_0_12, triptans_prior_0_3, triptans_preg, triptans_T1, triptans_T2, triptans_T3, triptans_post),
  Prevalence = c(triptans_prior_0_12_prev, triptans_prior_0_3_prev, triptans_preg_prev, triptans_T1_prev, triptans_T2_prev, triptans_T3_prev, triptans_post_prev),
  CI_lower = c(triptans_prior_0_12_low_CI, triptans_prior_0_3_low_CI, triptans_preg_low_CI, triptans_T1_low_CI, triptans_T2_low_CI, triptans_T3_low_CI, triptans_post_low_CI),
  CI_upper = c(triptans_prior_0_12_up_CI, triptans_prior_0_3_up_CI, triptans_preg_up_CI, triptans_T1_up_CI, triptans_T2_up_CI, triptans_T3_up_CI, triptans_post_up_CI)
)

write.csv(table2_1316, file.path(csv_path, "Table2_2013_2016.csv"), row.names = FALSE)  ##Path to output folder


rm(tript_post, tript_preg, tript_prior_0_12, tript_prior_0_3, tript_T1, tript_T2, tript_T3)
rm(list = ls.str(mode = "numeric"))

#Time period 2017-2020
denom_all_1720 <- nrow(DUS_all[DUS_all$year_group == "2017-2020", ])
denom_T1_1720 <- nrow(DUS_all[(DUS_all$year_group == "2017-2020" & DUS_all$denom_T1 == 1), ])
denom_T2_1720 <- nrow(DUS_all[(DUS_all$year_group == "2017-2020" & DUS_all$denom_T2 == 1), ])
denom_T3_1720 <- nrow(DUS_all[(DUS_all$year_group == "2017-2020" & DUS_all$denom_T3 == 1), ])

triptans_prior_0_12 <- nrow(DUS_all[DUS_all$year_group == "2017-2020" & DUS_all$trip_prior_0_12 == 1, ])
triptans_prior_0_3 <- nrow(DUS_all[DUS_all$year_group == "2017-2020" & DUS_all$trip_prior_0_3 == 1, ])
triptans_preg <- nrow(DUS_all[DUS_all$year_group == "2017-2020" & DUS_all$trip_preg == 1, ])
triptans_T1 <- nrow(DUS_all[DUS_all$year_group == "2017-2020" & DUS_all$trip_T1 == 1, ])
triptans_T2 <- nrow(DUS_all[DUS_all$year_group == "2017-2020" & DUS_all$trip_T2 == 1, ])
triptans_T3 <- nrow(DUS_all[DUS_all$year_group == "2017-2020" & DUS_all$trip_T3 == 1, ])
triptans_post <- nrow(DUS_all[DUS_all$year_group == "2017-2020" & DUS_all$trip_post == 1, ])
triptans_prior_0_12_prev <- round((triptans_prior_0_12 / denom_all_1720)*100, 3)
triptans_prior_0_3_prev <- round((triptans_prior_0_3 / denom_all_1720)*100, 3)
triptans_preg_prev <- round((triptans_preg / denom_all_1720)*100, 3)
triptans_T1_prev <- round((triptans_T1 / denom_T1_1720)*100, 3)
triptans_T2_prev <- round((triptans_T2 / denom_T2_1720)*100, 3)
triptans_T3_prev <- round((triptans_T3 / denom_T3_1720)*100, 3)
triptans_post_prev <- round((triptans_post / denom_all_1720)*100, 3)
tript_prior_0_12 <- scoreci(triptans_prior_0_12, denom_all_1720)
tript_prior_0_3 <- scoreci(triptans_prior_0_3, denom_all_1720)
tript_preg <- scoreci(triptans_preg, denom_all_1720)
tript_T1 <- scoreci(triptans_T1, denom_T1_1720)
tript_T2 <- scoreci(triptans_T2, denom_T2_1720)
tript_T3 <- scoreci(triptans_T3, denom_T3_1720)
tript_post <- scoreci(triptans_post, denom_all_1720)
triptans_prior_0_12_low_CI <- round((tript_prior_0_12$conf.low)*100, 3)
triptans_prior_0_12_up_CI <- round((tript_prior_0_12$conf.high)*100, 3)
triptans_prior_0_3_low_CI <- round((tript_prior_0_3$conf.low)*100, 3)
triptans_prior_0_3_up_CI <- round((tript_prior_0_3$conf.high)*100, 3)
triptans_preg_low_CI <- round((tript_preg$conf.low)*100, 3)
triptans_preg_up_CI <- round((tript_preg$conf.high)*100, 3)
triptans_T1_low_CI <- round((tript_T1$conf.low)*100, 3)
triptans_T1_up_CI <- round((tript_T1$conf.high)*100, 3)
triptans_T2_low_CI <- round((tript_T2$conf.low)*100, 3)
triptans_T2_up_CI <- round((tript_T2$conf.high)*100, 3)
triptans_T3_low_CI <- round((tript_T3$conf.low)*100, 3)
triptans_T3_up_CI <- round((tript_T3$conf.high)*100, 3)
triptans_post_low_CI <- round((tript_post$conf.low)*100, 3)
triptans_post_up_CI <- round((tript_post$conf.high)*100, 3)

table2_1720 <- data.frame(
  Time_period = c("0-12 months before LMP", "0-3 months before LMP", "Any time during pregnancy", "Trimester 1", "Trimester 2", "Trimester 3", "0-3 months after delivery"),
  N_pregnancies = c(denom_all_1720, denom_all_1720, denom_all_1720, denom_T1_1720, denom_T2_1720, denom_T3_1720, denom_all_1720),
  N_users = c(triptans_prior_0_12, triptans_prior_0_3, triptans_preg, triptans_T1, triptans_T2, triptans_T3, triptans_post),
  Prevalence = c(triptans_prior_0_12_prev, triptans_prior_0_3_prev, triptans_preg_prev, triptans_T1_prev, triptans_T2_prev, triptans_T3_prev, triptans_post_prev),
  CI_lower = c(triptans_prior_0_12_low_CI, triptans_prior_0_3_low_CI, triptans_preg_low_CI, triptans_T1_low_CI, triptans_T2_low_CI, triptans_T3_low_CI, triptans_post_low_CI),
  CI_upper = c(triptans_prior_0_12_up_CI, triptans_prior_0_3_up_CI, triptans_preg_up_CI, triptans_T1_up_CI, triptans_T2_up_CI, triptans_T3_up_CI, triptans_post_up_CI)
)

write.csv(table2_1720, file.path(csv_path, "Table2_2017_2020.csv"), row.names = FALSE)  ##Path to output folder


rm(tript_post, tript_preg, tript_prior_0_12, tript_prior_0_3, tript_T1, tript_T2, tript_T3)
rm(list = ls.str(mode = "numeric"))


############# Table 3 #########
cat("\nCreating Table 3 ...\n")

#Maternal age 15-24 years
denom_all_1524 <- nrow(DUS_all[DUS_all$maternal_age == "15-24", ])
denom_T1_1524 <- nrow(DUS_all[(DUS_all$maternal_age == "15-24" & DUS_all$denom_T1 == 1), ])
denom_T2_1524 <- nrow(DUS_all[(DUS_all$maternal_age == "15-24" & DUS_all$denom_T2 == 1), ])
denom_T3_1524 <- nrow(DUS_all[(DUS_all$maternal_age == "15-24" & DUS_all$denom_T3 == 1), ])

triptans_prior_0_12 <- nrow(DUS_all[DUS_all$maternal_age == "15-24" & DUS_all$trip_prior_0_12 == 1, ])
triptans_prior_0_3 <- nrow(DUS_all[DUS_all$maternal_age == "15-24" & DUS_all$trip_prior_0_3 == 1, ])
triptans_preg <- nrow(DUS_all[DUS_all$maternal_age == "15-24" & DUS_all$trip_preg == 1, ])
triptans_T1 <- nrow(DUS_all[DUS_all$maternal_age == "15-24" & DUS_all$trip_T1 == 1, ])
triptans_T2 <- nrow(DUS_all[DUS_all$maternal_age == "15-24" & DUS_all$trip_T2 == 1, ])
triptans_T3 <- nrow(DUS_all[DUS_all$maternal_age == "15-24" & DUS_all$trip_T3 == 1, ])
triptans_post <- nrow(DUS_all[DUS_all$maternal_age == "15-24" & DUS_all$trip_post == 1, ])
triptans_prior_0_12_prev <- round((triptans_prior_0_12 / denom_all_1524)*100, 3)
triptans_prior_0_3_prev <- round((triptans_prior_0_3 / denom_all_1524)*100, 3)
triptans_preg_prev <- round((triptans_preg / denom_all_1524)*100, 3)
triptans_T1_prev <- round((triptans_T1 / denom_T1_1524)*100, 3)
triptans_T2_prev <- round((triptans_T2 / denom_T2_1524)*100, 3)
triptans_T3_prev <- round((triptans_T3 / denom_T3_1524)*100, 3)
triptans_post_prev <- round((triptans_post / denom_all_1524)*100, 3)
tript_prior_0_12 <- scoreci(triptans_prior_0_12, denom_all_1524)
tript_prior_0_3 <- scoreci(triptans_prior_0_3, denom_all_1524)
tript_preg <- scoreci(triptans_preg, denom_all_1524)
tript_T1 <- scoreci(triptans_T1, denom_T1_1524)
tript_T2 <- scoreci(triptans_T2, denom_T2_1524)
tript_T3 <- scoreci(triptans_T3, denom_T3_1524)
tript_post <- scoreci(triptans_post, denom_all_1524)
triptans_prior_0_12_low_CI <- round((tript_prior_0_12$conf.low)*100, 3)
triptans_prior_0_12_up_CI <- round((tript_prior_0_12$conf.high)*100, 3)
triptans_prior_0_3_low_CI <- round((tript_prior_0_3$conf.low)*100, 3)
triptans_prior_0_3_up_CI <- round((tript_prior_0_3$conf.high)*100, 3)
triptans_preg_low_CI <- round((tript_preg$conf.low)*100, 3)
triptans_preg_up_CI <- round((tript_preg$conf.high)*100, 3)
triptans_T1_low_CI <- round((tript_T1$conf.low)*100, 3)
triptans_T1_up_CI <- round((tript_T1$conf.high)*100, 3)
triptans_T2_low_CI <- round((tript_T2$conf.low)*100, 3)
triptans_T2_up_CI <- round((tript_T2$conf.high)*100, 3)
triptans_T3_low_CI <- round((tript_T3$conf.low)*100, 3)
triptans_T3_up_CI <- round((tript_T3$conf.high)*100, 3)
triptans_post_low_CI <- round((tript_post$conf.low)*100, 3)
triptans_post_up_CI <- round((tript_post$conf.high)*100, 3)

table3_1524 <- data.frame(
  Time_period = c("0-12 months before LMP", "0-3 months before LMP", "Any time during pregnancy", "Trimester 1", "Trimester 2", "Trimester 3", "0-3 months after delivery"),
  N_pregnancies = c(denom_all_1524, denom_all_1524, denom_all_1524, denom_T1_1524, denom_T2_1524, denom_T3_1524, denom_all_1524),
  N_users = c(triptans_prior_0_12, triptans_prior_0_3, triptans_preg, triptans_T1, triptans_T2, triptans_T3, triptans_post),
  Prevalence = c(triptans_prior_0_12_prev, triptans_prior_0_3_prev, triptans_preg_prev, triptans_T1_prev, triptans_T2_prev, triptans_T3_prev, triptans_post_prev),
  CI_lower = c(triptans_prior_0_12_low_CI, triptans_prior_0_3_low_CI, triptans_preg_low_CI, triptans_T1_low_CI, triptans_T2_low_CI, triptans_T3_low_CI, triptans_post_low_CI),
  CI_upper = c(triptans_prior_0_12_up_CI, triptans_prior_0_3_up_CI, triptans_preg_up_CI, triptans_T1_up_CI, triptans_T2_up_CI, triptans_T3_up_CI, triptans_post_up_CI)
)

write.csv(table3_1524, file.path(csv_path, "Table3_15_24.csv"), row.names = FALSE)  ##Path to output folder


rm(tript_post, tript_preg, tript_prior_0_12, tript_prior_0_3, tript_T1, tript_T2, tript_T3)
rm(list = ls.str(mode = "numeric"))

#Maternal age 25-34 years
denom_all_2534 <- nrow(DUS_all[DUS_all$maternal_age == "25-34", ])
denom_T1_2534 <- nrow(DUS_all[(DUS_all$maternal_age == "25-34" & DUS_all$denom_T1 == 1), ])
denom_T2_2534 <- nrow(DUS_all[(DUS_all$maternal_age == "25-34" & DUS_all$denom_T2 == 1), ])
denom_T3_2534 <- nrow(DUS_all[(DUS_all$maternal_age == "25-34" & DUS_all$denom_T3 == 1), ])

triptans_prior_0_12 <- nrow(DUS_all[DUS_all$maternal_age == "25-34" & DUS_all$trip_prior_0_12 == 1, ])
triptans_prior_0_3 <- nrow(DUS_all[DUS_all$maternal_age == "25-34" & DUS_all$trip_prior_0_3 == 1, ])
triptans_preg <- nrow(DUS_all[DUS_all$maternal_age == "25-34" & DUS_all$trip_preg == 1, ])
triptans_T1 <- nrow(DUS_all[DUS_all$maternal_age == "25-34" & DUS_all$trip_T1 == 1, ])
triptans_T2 <- nrow(DUS_all[DUS_all$maternal_age == "25-34" & DUS_all$trip_T2 == 1, ])
triptans_T3 <- nrow(DUS_all[DUS_all$maternal_age == "25-34" & DUS_all$trip_T3 == 1, ])
triptans_post <- nrow(DUS_all[DUS_all$maternal_age == "25-34" & DUS_all$trip_post == 1, ])
triptans_prior_0_12_prev <- round((triptans_prior_0_12 / denom_all_2534)*100, 3)
triptans_prior_0_3_prev <- round((triptans_prior_0_3 / denom_all_2534)*100, 3)
triptans_preg_prev <- round((triptans_preg / denom_all_2534)*100, 3)
triptans_T1_prev <- round((triptans_T1 / denom_T1_2534)*100, 3)
triptans_T2_prev <- round((triptans_T2 / denom_T2_2534)*100, 3)
triptans_T3_prev <- round((triptans_T3 / denom_T3_2534)*100, 3)
triptans_post_prev <- round((triptans_post / denom_all_2534)*100, 3)
tript_prior_0_12 <- scoreci(triptans_prior_0_12, denom_all_2534)
tript_prior_0_3 <- scoreci(triptans_prior_0_3, denom_all_2534)
tript_preg <- scoreci(triptans_preg, denom_all_2534)
tript_T1 <- scoreci(triptans_T1, denom_T1_2534)
tript_T2 <- scoreci(triptans_T2, denom_T2_2534)
tript_T3 <- scoreci(triptans_T3, denom_T3_2534)
tript_post <- scoreci(triptans_post, denom_all_2534)
triptans_prior_0_12_low_CI <- round((tript_prior_0_12$conf.low)*100, 3)
triptans_prior_0_12_up_CI <- round((tript_prior_0_12$conf.high)*100, 3)
triptans_prior_0_3_low_CI <- round((tript_prior_0_3$conf.low)*100, 3)
triptans_prior_0_3_up_CI <- round((tript_prior_0_3$conf.high)*100, 3)
triptans_preg_low_CI <- round((tript_preg$conf.low)*100, 3)
triptans_preg_up_CI <- round((tript_preg$conf.high)*100, 3)
triptans_T1_low_CI <- round((tript_T1$conf.low)*100, 3)
triptans_T1_up_CI <- round((tript_T1$conf.high)*100, 3)
triptans_T2_low_CI <- round((tript_T2$conf.low)*100, 3)
triptans_T2_up_CI <- round((tript_T2$conf.high)*100, 3)
triptans_T3_low_CI <- round((tript_T3$conf.low)*100, 3)
triptans_T3_up_CI <- round((tript_T3$conf.high)*100, 3)
triptans_post_low_CI <- round((tript_post$conf.low)*100, 3)
triptans_post_up_CI <- round((tript_post$conf.high)*100, 3)

table3_2534 <- data.frame(
  Time_period = c("0-12 months before LMP", "0-3 months before LMP", "Any time during pregnancy", "Trimester 1", "Trimester 2", "Trimester 3", "0-3 months after delivery"),
  N_pregnancies = c(denom_all_2534, denom_all_2534, denom_all_2534, denom_T1_2534, denom_T2_2534, denom_T3_2534, denom_all_2534),
  N_users = c(triptans_prior_0_12, triptans_prior_0_3, triptans_preg, triptans_T1, triptans_T2, triptans_T3, triptans_post),
  Prevalence = c(triptans_prior_0_12_prev, triptans_prior_0_3_prev, triptans_preg_prev, triptans_T1_prev, triptans_T2_prev, triptans_T3_prev, triptans_post_prev),
  CI_lower = c(triptans_prior_0_12_low_CI, triptans_prior_0_3_low_CI, triptans_preg_low_CI, triptans_T1_low_CI, triptans_T2_low_CI, triptans_T3_low_CI, triptans_post_low_CI),
  CI_upper = c(triptans_prior_0_12_up_CI, triptans_prior_0_3_up_CI, triptans_preg_up_CI, triptans_T1_up_CI, triptans_T2_up_CI, triptans_T3_up_CI, triptans_post_up_CI)
)

write.csv(table3_2534, file.path(csv_path, "Table3_25_34.csv"), row.names = FALSE)  ##Path to output folder

rm(tript_post, tript_preg, tript_prior_0_12, tript_prior_0_3, tript_T1, tript_T2, tript_T3)
rm(list = ls.str(mode = "numeric"))

#Maternal age 35-49 years
denom_all_3549 <- nrow(DUS_all[DUS_all$maternal_age == "35+", ])
denom_T1_3549 <- nrow(DUS_all[(DUS_all$maternal_age == "35+" & DUS_all$denom_T1 == 1), ])
denom_T2_3549 <- nrow(DUS_all[(DUS_all$maternal_age == "35+" & DUS_all$denom_T2 == 1), ])
denom_T3_3549 <- nrow(DUS_all[(DUS_all$maternal_age == "35+" & DUS_all$denom_T3 == 1), ])

triptans_prior_0_12 <- nrow(DUS_all[DUS_all$maternal_age == "35+" & DUS_all$trip_prior_0_12 == 1, ])
triptans_prior_0_3 <- nrow(DUS_all[DUS_all$maternal_age == "35+" & DUS_all$trip_prior_0_3 == 1, ])
triptans_preg <- nrow(DUS_all[DUS_all$maternal_age == "35+" & DUS_all$trip_preg == 1, ])
triptans_T1 <- nrow(DUS_all[DUS_all$maternal_age == "35+" & DUS_all$trip_T1 == 1, ])
triptans_T2 <- nrow(DUS_all[DUS_all$maternal_age == "35+" & DUS_all$trip_T2 == 1, ])
triptans_T3 <- nrow(DUS_all[DUS_all$maternal_age == "35+" & DUS_all$trip_T3 == 1, ])
triptans_post <- nrow(DUS_all[DUS_all$maternal_age == "35+" & DUS_all$trip_post == 1, ])
triptans_prior_0_12_prev <- round((triptans_prior_0_12 / denom_all_3549)*100, 3)
triptans_prior_0_3_prev <- round((triptans_prior_0_3 / denom_all_3549)*100, 3)
triptans_preg_prev <- round((triptans_preg / denom_all_3549)*100, 3)
triptans_T1_prev <- round((triptans_T1 / denom_T1_3549)*100, 3)
triptans_T2_prev <- round((triptans_T2 / denom_T2_3549)*100, 3)
triptans_T3_prev <- round((triptans_T3 / denom_T3_3549)*100, 3)
triptans_post_prev <- round((triptans_post / denom_all_3549)*100, 3)
tript_prior_0_12 <- scoreci(triptans_prior_0_12, denom_all_3549)
tript_prior_0_3 <- scoreci(triptans_prior_0_3, denom_all_3549)
tript_preg <- scoreci(triptans_preg, denom_all_3549)
tript_T1 <- scoreci(triptans_T1, denom_T1_3549)
tript_T2 <- scoreci(triptans_T2, denom_T2_3549)
tript_T3 <- scoreci(triptans_T3, denom_T3_3549)
tript_post <- scoreci(triptans_post, denom_all_3549)
triptans_prior_0_12_low_CI <- round((tript_prior_0_12$conf.low)*100, 3)
triptans_prior_0_12_up_CI <- round((tript_prior_0_12$conf.high)*100, 3)
triptans_prior_0_3_low_CI <- round((tript_prior_0_3$conf.low)*100, 3)
triptans_prior_0_3_up_CI <- round((tript_prior_0_3$conf.high)*100, 3)
triptans_preg_low_CI <- round((tript_preg$conf.low)*100, 3)
triptans_preg_up_CI <- round((tript_preg$conf.high)*100, 3)
triptans_T1_low_CI <- round((tript_T1$conf.low)*100, 3)
triptans_T1_up_CI <- round((tript_T1$conf.high)*100, 3)
triptans_T2_low_CI <- round((tript_T2$conf.low)*100, 3)
triptans_T2_up_CI <- round((tript_T2$conf.high)*100, 3)
triptans_T3_low_CI <- round((tript_T3$conf.low)*100, 3)
triptans_T3_up_CI <- round((tript_T3$conf.high)*100, 3)
triptans_post_low_CI <- round((tript_post$conf.low)*100, 3)
triptans_post_up_CI <- round((tript_post$conf.high)*100, 3)

table3_3549 <- data.frame(
  Time_period = c("0-12 months before LMP", "0-3 months before LMP", "Any time during pregnancy", "Trimester 1", "Trimester 2", "Trimester 3", "0-3 months after delivery"),
  N_pregnancies = c(denom_all_3549, denom_all_3549, denom_all_3549, denom_T1_3549, denom_T2_3549, denom_T3_3549, denom_all_3549),
  N_users = c(triptans_prior_0_12, triptans_prior_0_3, triptans_preg, triptans_T1, triptans_T2, triptans_T3, triptans_post),
  Prevalence = c(triptans_prior_0_12_prev, triptans_prior_0_3_prev, triptans_preg_prev, triptans_T1_prev, triptans_T2_prev, triptans_T3_prev, triptans_post_prev),
  CI_lower = c(triptans_prior_0_12_low_CI, triptans_prior_0_3_low_CI, triptans_preg_low_CI, triptans_T1_low_CI, triptans_T2_low_CI, triptans_T3_low_CI, triptans_post_low_CI),
  CI_upper = c(triptans_prior_0_12_up_CI, triptans_prior_0_3_up_CI, triptans_preg_up_CI, triptans_T1_up_CI, triptans_T2_up_CI, triptans_T3_up_CI, triptans_post_up_CI)
)

write.csv(table3_3549, file.path(csv_path, "Table3_35_49.csv"), row.names = FALSE)  ##Path to output folder


rm(tript_post, tript_preg, tript_prior_0_12, tript_prior_0_3, tript_T1, tript_T2, tript_T3)
rm(list = ls.str(mode = "numeric"))


############# Table 4 #########
cat("\nCreating Table 4 ...\n")


#Paracetamol
DUS_PCM <- merge(DUS_all, paracetamol, by = "person_id", all.x = TRUE, allow.cartesian = TRUE) #many-to-many relationship
DUS_PCM$gap_LMP <- difftime(DUS_PCM$medicine_date, DUS_PCM$pregnancy_start_date, units = "days")
DUS_PCM$gap_del <- difftime(DUS_PCM$medicine_date, DUS_PCM$pregnancy_end_date, units = "days")
DUS_PCM$PCM_prior_a <- ifelse((DUS_PCM$gap_LMP >= -365 & DUS_PCM$gap_LMP <= -1), 1, 0)
DUS_PCM$PCM_preg_a <- ifelse((DUS_PCM$gap_LMP >= 0 & DUS_PCM$gap_del <= 0), 1, 0)
DUS_PCM$PCM_T1_a <- ifelse((DUS_PCM$gap_LMP >= 0 & DUS_PCM$gap_LMP <= 97 & DUS_PCM$gap_del<=0), 1, 0)
DUS_PCM$PCM_T2_a <- ifelse((DUS_PCM$gap_LMP >= 98 & DUS_PCM$gap_LMP <= 195 & DUS_PCM$gap_del<=0), 1, 0)
DUS_PCM$PCM_T3_a <- ifelse((DUS_PCM$gap_LMP >= 196 & DUS_PCM$gap_del <= 0), 1, 0)

DUS_PCM1 = DUS_PCM %>%
  group_by(pregnancy_id) %>%
  summarize(PCM_prior = max(PCM_prior_a))
DUS_all <- merge(DUS_all, DUS_PCM1, by = c("pregnancy_id"))
DUS_all$PCM_prior <- replace_na(DUS_all$PCM_prior, 0)
DUS_PCM1 = DUS_PCM %>%
  group_by(pregnancy_id) %>%
  summarize(PCM_preg = max(PCM_preg_a))
DUS_all <- merge(DUS_all, DUS_PCM1, by = c("pregnancy_id"))
DUS_all$PCM_preg <- replace_na(DUS_all$PCM_preg, 0)
DUS_PCM1 = DUS_PCM %>%
  group_by(pregnancy_id) %>%
  summarize(PCM_T1 = max(PCM_T1_a))
DUS_all <- merge(DUS_all, DUS_PCM1, by = c("pregnancy_id"))
DUS_all$PCM_T1 <- replace_na(DUS_all$PCM_T1, 0)
DUS_PCM1 = DUS_PCM %>%
  group_by(pregnancy_id) %>%
  summarize(PCM_T2 = max(PCM_T2_a))
DUS_all <- merge(DUS_all, DUS_PCM1, by = c("pregnancy_id"))
DUS_all$PCM_T2 <- replace_na(DUS_all$PCM_T2, 0)
DUS_PCM1 = DUS_PCM %>%
  group_by(pregnancy_id) %>%
  summarize(PCM_T3 = max(PCM_T3_a))
DUS_all <- merge(DUS_all, DUS_PCM1, by = c("pregnancy_id"))
DUS_all$PCM_T3 <- replace_na(DUS_all$PCM_T3, 0)
rm(DUS_PCM, DUS_PCM1, paracetamol)

#Codeine and Paracetamol
DUS_COD <- merge(DUS_all, codeine, by = "person_id", all.x = TRUE, allow.cartesian = TRUE) #many-to-many relationship
DUS_COD$gap_LMP <- difftime(DUS_COD$medicine_date, DUS_COD$pregnancy_start_date, units = "days")
DUS_COD$gap_del <- difftime(DUS_COD$medicine_date, DUS_COD$pregnancy_end_date, units = "days")
DUS_COD$COD_prior_a <- ifelse((DUS_COD$gap_LMP >= -365 & DUS_COD$gap_LMP <= -1), 1, 0)
DUS_COD$COD_preg_a <- ifelse((DUS_COD$gap_LMP >= 0 & DUS_COD$gap_del <= 0), 1, 0)
DUS_COD$COD_T1_a <- ifelse((DUS_COD$gap_LMP >= 0 & DUS_COD$gap_LMP <= 97 & DUS_COD$gap_del<=0), 1, 0)
DUS_COD$COD_T2_a <- ifelse((DUS_COD$gap_LMP >= 98 & DUS_COD$gap_LMP <= 195 & DUS_COD$gap_del<=0), 1, 0)
DUS_COD$COD_T3_a <- ifelse((DUS_COD$gap_LMP >= 196 & DUS_COD$gap_del <= 0), 1, 0)

DUS_COD1 = DUS_COD %>%
  group_by(pregnancy_id) %>%
  summarize(COD_prior = max(COD_prior_a))
DUS_all <- merge(DUS_all, DUS_COD1, by = c("pregnancy_id"))
DUS_all$COD_prior <- replace_na(DUS_all$COD_prior, 0)
DUS_COD1 = DUS_COD %>%
  group_by(pregnancy_id) %>%
  summarize(COD_preg = max(COD_preg_a))
DUS_all <- merge(DUS_all, DUS_COD1, by = c("pregnancy_id"))
DUS_all$COD_preg <- replace_na(DUS_all$COD_preg, 0)
DUS_COD1 = DUS_COD %>%
  group_by(pregnancy_id) %>%
  summarize(COD_T1 = max(COD_T1_a))
DUS_all <- merge(DUS_all, DUS_COD1, by = c("pregnancy_id"))
DUS_all$COD_T1 <- replace_na(DUS_all$COD_T1, 0)
DUS_COD1 = DUS_COD %>%
  group_by(pregnancy_id) %>%
  summarize(COD_T2 = max(COD_T2_a))
DUS_all <- merge(DUS_all, DUS_COD1, by = c("pregnancy_id"))
DUS_all$COD_T2 <- replace_na(DUS_all$COD_T2, 0)
DUS_COD1 = DUS_COD %>%
  group_by(pregnancy_id) %>%
  summarize(COD_T3 = max(COD_T3_a))
DUS_all <- merge(DUS_all, DUS_COD1, by = c("pregnancy_id"))
DUS_all$COD_T3 <- replace_na(DUS_all$COD_T3, 0)
rm(DUS_COD, DUS_COD1, codeine)

#NSAID
DUS_NSAID <- merge(DUS_all, nsaid, by = "person_id", all.x = TRUE, allow.cartesian = TRUE) #many-to-many relationship
DUS_NSAID$gap_LMP <- difftime(DUS_NSAID$medicine_date, DUS_NSAID$pregnancy_start_date, units = "days")
DUS_NSAID$gap_del <- difftime(DUS_NSAID$medicine_date, DUS_NSAID$pregnancy_end_date, units = "days")

DUS_NSAID$NSAID_prior_a <- ifelse((DUS_NSAID$gap_LMP >= -365 & DUS_NSAID$gap_LMP <= -1), 1, 0)
DUS_NSAID$NSAID_preg_a <- ifelse((DUS_NSAID$gap_LMP >= 0 & DUS_NSAID$gap_del <= 0), 1, 0)
DUS_NSAID$NSAID_T1_a <- ifelse((DUS_NSAID$gap_LMP >= 0 & DUS_NSAID$gap_LMP <= 97 & DUS_NSAID$gap_del<=0), 1, 0)
DUS_NSAID$NSAID_T2_a <- ifelse((DUS_NSAID$gap_LMP >= 98 & DUS_NSAID$gap_LMP <= 195 & DUS_NSAID$gap_del<=0), 1, 0)
DUS_NSAID$NSAID_T3_a <- ifelse((DUS_NSAID$gap_LMP >= 196 & DUS_NSAID$gap_del <= 0), 1, 0)

DUS_NSAID1 = DUS_NSAID %>%
  group_by(pregnancy_id) %>%
  summarize(NSAID_prior = max(NSAID_prior_a))
DUS_all <- merge(DUS_all, DUS_NSAID1, by = c("pregnancy_id"))
DUS_all$NSAID_prior <- replace_na(DUS_all$NSAID_prior, 0)
DUS_NSAID1 = DUS_NSAID %>%
  group_by(pregnancy_id) %>%
  summarize(NSAID_preg = max(NSAID_preg_a))
DUS_all <- merge(DUS_all, DUS_NSAID1, by = c("pregnancy_id"))
DUS_all$NSAID_preg <- replace_na(DUS_all$NSAID_preg, 0)
DUS_NSAID1 = DUS_NSAID %>%
  group_by(pregnancy_id) %>%
  summarize(NSAID_T1 = max(NSAID_T1_a))
DUS_all <- merge(DUS_all, DUS_NSAID1, by = c("pregnancy_id"))
DUS_all$NSAID_T1 <- replace_na(DUS_all$NSAID_T1, 0)
DUS_NSAID1 = DUS_NSAID %>%
  group_by(pregnancy_id) %>%
  summarize(NSAID_T2 = max(NSAID_T2_a))
DUS_all <- merge(DUS_all, DUS_NSAID1, by = c("pregnancy_id"))
DUS_all$NSAID_T2 <- replace_na(DUS_all$NSAID_T2, 0)
DUS_NSAID1 = DUS_NSAID %>%
  group_by(pregnancy_id) %>%
  summarize(NSAID_T3 = max(NSAID_T3_a))
DUS_all <- merge(DUS_all, DUS_NSAID1, by = c("pregnancy_id"))
DUS_all$NSAID_T3 <- replace_na(DUS_all$NSAID_T3, 0)
rm(DUS_NSAID, DUS_NSAID1, nsaid)

#Antinauseants
DUS_antinausea <- merge(DUS_all, antinausea, by = "person_id", all.x = TRUE, allow.cartesian = TRUE) #many-to-many relationship
DUS_antinausea$gap_LMP <- difftime(DUS_antinausea$medicine_date, DUS_antinausea$pregnancy_start_date, units = "days")
DUS_antinausea$gap_del <- difftime(DUS_antinausea$medicine_date, DUS_antinausea$pregnancy_end_date, units = "days")
DUS_antinausea$antinausea_prior_a <- ifelse((DUS_antinausea$gap_LMP >= -365 & DUS_antinausea$gap_LMP <= -1), 1, 0)
DUS_antinausea$antinausea_preg_a <- ifelse((DUS_antinausea$gap_LMP >= 0 & DUS_antinausea$gap_del <= 0), 1, 0)
DUS_antinausea$antinausea_T1_a <- ifelse((DUS_antinausea$gap_LMP >= 0 & DUS_antinausea$gap_LMP <= 97 & DUS_antinausea$gap_del<=0), 1, 0)
DUS_antinausea$antinausea_T2_a <- ifelse((DUS_antinausea$gap_LMP >= 98 & DUS_antinausea$gap_LMP <= 195 & DUS_antinausea$gap_del<=0), 1, 0)
DUS_antinausea$antinausea_T3_a <- ifelse((DUS_antinausea$gap_LMP >= 196 & DUS_antinausea$gap_del <= 0), 1, 0)

DUS_antinausea1 = DUS_antinausea %>%
  group_by(pregnancy_id) %>%
  summarize(antinausea_prior = max(antinausea_prior_a))
DUS_all <- merge(DUS_all, DUS_antinausea1, by = c("pregnancy_id"))
DUS_all$antinausea_prior <- replace_na(DUS_all$antinausea_prior, 0)
DUS_antinausea1 = DUS_antinausea %>%
  group_by(pregnancy_id) %>%
  summarize(antinausea_preg = max(antinausea_preg_a))
DUS_all <- merge(DUS_all, DUS_antinausea1, by = c("pregnancy_id"))
DUS_all$antinausea_preg <- replace_na(DUS_all$antinausea_preg, 0)
DUS_antinausea1 = DUS_antinausea %>%
  group_by(pregnancy_id) %>%
  summarize(antinausea_T1 = max(antinausea_T1_a))
DUS_all <- merge(DUS_all, DUS_antinausea1, by = c("pregnancy_id"))
DUS_all$antinausea_T1 <- replace_na(DUS_all$antinausea_T1, 0)
DUS_antinausea1 = DUS_antinausea %>%
  group_by(pregnancy_id) %>%
  summarize(antinausea_T2 = max(antinausea_T2_a))
DUS_all <- merge(DUS_all, DUS_antinausea1, by = c("pregnancy_id"))
DUS_all$antinausea_T2 <- replace_na(DUS_all$antinausea_T2, 0)
DUS_antinausea1 = DUS_antinausea %>%
  group_by(pregnancy_id) %>%
  summarize(antinausea_T3 = max(antinausea_T3_a))
DUS_all <- merge(DUS_all, DUS_antinausea1, by = c("pregnancy_id"))
DUS_all$antinausea_T3 <- replace_na(DUS_all$antinausea_T3, 0)
rm(DUS_antinausea, DUS_antinausea1, antinausea)

#Acute migraine treatment
DUS_all$acute_prior <- ifelse((DUS_all$PCM_prior == 1 | DUS_all$COD_prior == 1 | DUS_all$NSAID_prior == 1 | DUS_all$NSAID_prior == 1), 1, 0)
DUS_all$acute_preg <- ifelse((DUS_all$PCM_preg == 1 | DUS_all$COD_preg == 1 | DUS_all$NSAID_preg == 1 | DUS_all$NSAID_preg == 1), 1, 0)
DUS_all$acute_T1 <- ifelse((DUS_all$PCM_T1 == 1 | DUS_all$COD_T1 == 1 | DUS_all$NSAID_T1 == 1 | DUS_all$NSAID_T1 == 1), 1, 0)
DUS_all$acute_T2 <- ifelse((DUS_all$PCM_T2 == 1 | DUS_all$COD_T2 == 1 | DUS_all$NSAID_T2 == 1 | DUS_all$NSAID_T2 == 1), 1, 0)
DUS_all$acute_T3 <- ifelse((DUS_all$PCM_T3 == 1 | DUS_all$COD_T3 == 1 | DUS_all$NSAID_T3 == 1 | DUS_all$NSAID_T3 == 1), 1, 0)

#Metoprolol/propranolol
meto_prop <- prev_med[prev_med$atc_code == "C07AA05" | prev_med$atc_code == "C07AB02", ]
DUS_meto_prop <- merge(DUS_all, meto_prop, by = "person_id", all.x = TRUE, allow.cartesian = TRUE) #many-to-many relationship
DUS_meto_prop$gap_LMP <- difftime(DUS_meto_prop$medicine_date, DUS_meto_prop$pregnancy_start_date, units = "days")
DUS_meto_prop$gap_del <- difftime(DUS_meto_prop$medicine_date, DUS_meto_prop$pregnancy_end_date, units = "days")
DUS_meto_prop$meto_prop_prior_a <- ifelse((DUS_meto_prop$gap_LMP >= -365 & DUS_meto_prop$gap_LMP <= -1), 1, 0)
DUS_meto_prop$meto_prop_preg_a <- ifelse((DUS_meto_prop$gap_LMP >= 0 & DUS_meto_prop$gap_del <= 0), 1, 0)
DUS_meto_prop$meto_prop_T1_a <- ifelse((DUS_meto_prop$gap_LMP >= 0 & DUS_meto_prop$gap_LMP <= 97 & DUS_meto_prop$gap_del<=0), 1, 0)
DUS_meto_prop$meto_prop_T2_a <- ifelse((DUS_meto_prop$gap_LMP >= 98 & DUS_meto_prop$gap_LMP <= 195 & DUS_meto_prop$gap_del<=0), 1, 0)
DUS_meto_prop$meto_prop_T3_a <- ifelse((DUS_meto_prop$gap_LMP >= 196 & DUS_meto_prop$gap_del <= 0), 1, 0)

DUS_meto_prop1 = DUS_meto_prop %>%
  group_by(pregnancy_id) %>%
  summarize(meto_prop_prior = max(meto_prop_prior_a))
DUS_all <- merge(DUS_all, DUS_meto_prop1, by = c("pregnancy_id"))
DUS_all$meto_prop_prior <- replace_na(DUS_all$meto_prop_prior, 0)
DUS_meto_prop1 = DUS_meto_prop %>%
  group_by(pregnancy_id) %>%
  summarize(meto_prop_preg = max(meto_prop_preg_a))
DUS_all <- merge(DUS_all, DUS_meto_prop1, by = c("pregnancy_id"))
DUS_all$meto_prop_preg <- replace_na(DUS_all$meto_prop_preg, 0)
DUS_meto_prop1 = DUS_meto_prop %>%
  group_by(pregnancy_id) %>%
  summarize(meto_prop_T1 = max(meto_prop_T1_a))
DUS_all <- merge(DUS_all, DUS_meto_prop1, by = c("pregnancy_id"))
DUS_all$meto_prop_T1 <- replace_na(DUS_all$meto_prop_T1, 0)
DUS_meto_prop1 = DUS_meto_prop %>%
  group_by(pregnancy_id) %>%
  summarize(meto_prop_T2 = max(meto_prop_T2_a))
DUS_all <- merge(DUS_all, DUS_meto_prop1, by = c("pregnancy_id"))
DUS_all$meto_prop_T2 <- replace_na(DUS_all$meto_prop_T2, 0)
DUS_meto_prop1 = DUS_meto_prop %>%
  group_by(pregnancy_id) %>%
  summarize(meto_prop_T3 = max(meto_prop_T3_a))
DUS_all <- merge(DUS_all, DUS_meto_prop1, by = c("pregnancy_id"))
DUS_all$meto_prop_T3 <- replace_na(DUS_all$meto_prop_T3, 0)
rm(DUS_meto_prop, DUS_meto_prop1, meto_prop)

#Amitriptyline
ami <- prev_med[prev_med$atc_code == "N06AA09", ]
DUS_ami <- merge(DUS_all, ami, by = "person_id", all.x = TRUE, allow.cartesian = TRUE) #many-to-many relationship
DUS_ami$gap_LMP <- difftime(DUS_ami$medicine_date, DUS_ami$pregnancy_start_date, units = "days")
DUS_ami$gap_del <- difftime(DUS_ami$medicine_date, DUS_ami$pregnancy_end_date, units = "days")
DUS_ami$ami_prior_a <- ifelse((DUS_ami$gap_LMP >= -365 & DUS_ami$gap_LMP <= -1), 1, 0)
DUS_ami$ami_preg_a <- ifelse((DUS_ami$gap_LMP >= 0 & DUS_ami$gap_del <= 0), 1, 0)
DUS_ami$ami_T1_a <- ifelse((DUS_ami$gap_LMP >= 0 & DUS_ami$gap_LMP <= 97 & DUS_ami$gap_del<=0), 1, 0)
DUS_ami$ami_T2_a <- ifelse((DUS_ami$gap_LMP >= 98 & DUS_ami$gap_LMP <= 195 & DUS_ami$gap_del<=0), 1, 0)
DUS_ami$ami_T3_a <- ifelse((DUS_ami$gap_LMP >= 196 & DUS_ami$gap_del <= 0), 1, 0)

DUS_ami1 = DUS_ami %>%
  group_by(pregnancy_id) %>%
  summarize(ami_prior = max(ami_prior_a))
DUS_all <- merge(DUS_all, DUS_ami1, by = c("pregnancy_id"))
DUS_all$ami_prior <- replace_na(DUS_all$ami_prior, 0)
DUS_ami1 = DUS_ami %>%
  group_by(pregnancy_id) %>%
  summarize(ami_preg = max(ami_preg_a))
DUS_all <- merge(DUS_all, DUS_ami1, by = c("pregnancy_id"))
DUS_all$ami_preg <- replace_na(DUS_all$ami_preg, 0)
DUS_ami1 = DUS_ami %>%
  group_by(pregnancy_id) %>%
  summarize(ami_T1 = max(ami_T1_a))
DUS_all <- merge(DUS_all, DUS_ami1, by = c("pregnancy_id"))
DUS_all$ami_T1 <- replace_na(DUS_all$ami_T1, 0)
DUS_ami1 = DUS_ami %>%
  group_by(pregnancy_id) %>%
  summarize(ami_T2 = max(ami_T2_a))
DUS_all <- merge(DUS_all, DUS_ami1, by = c("pregnancy_id"))
DUS_all$ami_T2 <- replace_na(DUS_all$ami_T2, 0)
DUS_ami1 = DUS_ami %>%
  group_by(pregnancy_id) %>%
  summarize(ami_T3 = max(ami_T3_a))
DUS_all <- merge(DUS_all, DUS_ami1, by = c("pregnancy_id"))
DUS_all$ami_T3 <- replace_na(DUS_all$ami_T3, 0)
rm(DUS_ami, DUS_ami1, ami)

#Candesartan/lisinopril/verapamil
c_l_v <- prev_med[prev_med$atc_code == "C09CA06" | prev_med$atc_code == "C09AA03" | prev_med$atc_code == "C08DA01", ]
DUS_c_l_v <- merge(DUS_all, c_l_v, by = "person_id", all.x = TRUE, allow.cartesian = TRUE) #many-to-many relationship
DUS_c_l_v$gap_LMP <- difftime(DUS_c_l_v$medicine_date, DUS_c_l_v$pregnancy_start_date, units = "days")
DUS_c_l_v$gap_del <- difftime(DUS_c_l_v$medicine_date, DUS_c_l_v$pregnancy_end_date, units = "days")
DUS_c_l_v$c_l_v_prior_a <- ifelse((DUS_c_l_v$gap_LMP >= -365 & DUS_c_l_v$gap_LMP <= -1), 1, 0)
DUS_c_l_v$c_l_v_preg_a <- ifelse((DUS_c_l_v$gap_LMP >= 0 & DUS_c_l_v$gap_del <= 0), 1, 0)
DUS_c_l_v$c_l_v_T1_a <- ifelse((DUS_c_l_v$gap_LMP >= 0 & DUS_c_l_v$gap_LMP <= 97 & DUS_c_l_v$gap_del<=0), 1, 0)
DUS_c_l_v$c_l_v_T2_a <- ifelse((DUS_c_l_v$gap_LMP >= 98 & DUS_c_l_v$gap_LMP <= 195 & DUS_c_l_v$gap_del<=0), 1, 0)
DUS_c_l_v$c_l_v_T3_a <- ifelse((DUS_c_l_v$gap_LMP >= 196 & DUS_c_l_v$gap_del <= 0), 1, 0)

DUS_c_l_v1 = DUS_c_l_v %>%
  group_by(pregnancy_id) %>%
  summarize(c_l_v_prior = max(c_l_v_prior_a))
DUS_all <- merge(DUS_all, DUS_c_l_v1, by = c("pregnancy_id"))
DUS_all$c_l_v_prior <- replace_na(DUS_all$c_l_v_prior, 0)
DUS_c_l_v1 = DUS_c_l_v %>%
  group_by(pregnancy_id) %>%
  summarize(c_l_v_preg = max(c_l_v_preg_a))
DUS_all <- merge(DUS_all, DUS_c_l_v1, by = c("pregnancy_id"))
DUS_all$c_l_v_preg <- replace_na(DUS_all$c_l_v_preg, 0)
DUS_c_l_v1 = DUS_c_l_v %>%
  group_by(pregnancy_id) %>%
  summarize(c_l_v_T1 = max(c_l_v_T1_a))
DUS_all <- merge(DUS_all, DUS_c_l_v1, by = c("pregnancy_id"))
DUS_all$c_l_v_T1 <- replace_na(DUS_all$c_l_v_T1, 0)
DUS_c_l_v1 = DUS_c_l_v %>%
  group_by(pregnancy_id) %>%
  summarize(c_l_v_T2 = max(c_l_v_T2_a))
DUS_all <- merge(DUS_all, DUS_c_l_v1, by = c("pregnancy_id"))
DUS_all$c_l_v_T2 <- replace_na(DUS_all$c_l_v_T2, 0)
DUS_c_l_v1 = DUS_c_l_v %>%
  group_by(pregnancy_id) %>%
  summarize(c_l_v_T3 = max(c_l_v_T3_a))
DUS_all <- merge(DUS_all, DUS_c_l_v1, by = c("pregnancy_id"))
DUS_all$c_l_v_T3 <- replace_na(DUS_all$c_l_v_T3, 0)
rm(DUS_c_l_v, DUS_c_l_v1, c_l_v)

#Topiramate/valproic acid
top_val <- prev_med[prev_med$atc_code == "N03AX11" | prev_med$atc_code == "N03AG01", ]
DUS_top_val <- merge(DUS_all, top_val, by = "person_id", all.x = TRUE, allow.cartesian = TRUE) #many-to-many relationship
DUS_top_val$gap_LMP <- difftime(DUS_top_val$medicine_date, DUS_top_val$pregnancy_start_date, units = "days")
DUS_top_val$gap_del <- difftime(DUS_top_val$medicine_date, DUS_top_val$pregnancy_end_date, units = "days")
DUS_top_val$top_val_prior_a <- ifelse((DUS_top_val$gap_LMP >= -365 & DUS_top_val$gap_LMP <= -1), 1, 0)
DUS_top_val$top_val_preg_a <- ifelse((DUS_top_val$gap_LMP >= 0 & DUS_top_val$gap_del <= 0), 1, 0)
DUS_top_val$top_val_T1_a <- ifelse((DUS_top_val$gap_LMP >= 0 & DUS_top_val$gap_LMP <= 97 & DUS_top_val$gap_del<=0), 1, 0)
DUS_top_val$top_val_T2_a <- ifelse((DUS_top_val$gap_LMP >= 98 & DUS_top_val$gap_LMP <= 195 & DUS_top_val$gap_del<=0), 1, 0)
DUS_top_val$top_val_T3_a <- ifelse((DUS_top_val$gap_LMP >= 196 & DUS_top_val$gap_del <= 0), 1, 0)

DUS_top_val1 = DUS_top_val %>%
  group_by(pregnancy_id) %>%
  summarize(top_val_prior = max(top_val_prior_a))
DUS_all <- merge(DUS_all, DUS_top_val1, by = c("pregnancy_id"))
DUS_all$top_val_prior <- replace_na(DUS_all$top_val_prior, 0)
DUS_top_val1 = DUS_top_val %>%
  group_by(pregnancy_id) %>%
  summarize(top_val_preg = max(top_val_preg_a))
DUS_all <- merge(DUS_all, DUS_top_val1, by = c("pregnancy_id"))
DUS_all$top_val_preg <- replace_na(DUS_all$top_val_preg, 0)
DUS_top_val1 = DUS_top_val %>%
  group_by(pregnancy_id) %>%
  summarize(top_val_T1 = max(top_val_T1_a))
DUS_all <- merge(DUS_all, DUS_top_val1, by = c("pregnancy_id"))
DUS_all$top_val_T1 <- replace_na(DUS_all$top_val_T1, 0)
DUS_top_val1 = DUS_top_val %>%
  group_by(pregnancy_id) %>%
  summarize(top_val_T2 = max(top_val_T2_a))
DUS_all <- merge(DUS_all, DUS_top_val1, by = c("pregnancy_id"))
DUS_all$top_val_T2 <- replace_na(DUS_all$top_val_T2, 0)
DUS_top_val1 = DUS_top_val %>%
  group_by(pregnancy_id) %>%
  summarize(top_val_T3 = max(top_val_T3_a))
DUS_all <- merge(DUS_all, DUS_top_val1, by = c("pregnancy_id"))
DUS_all$top_val_T3 <- replace_na(DUS_all$top_val_T3, 0)
rm(DUS_top_val, DUS_top_val1, top_val)

#Botulinum toxin
bot <- prev_med[prev_med$atc_code == "M03AX01", ]
DUS_bot <- merge(DUS_all, bot, by = "person_id", all.x = TRUE, allow.cartesian = TRUE) #many-to-many relationship
DUS_bot$gap_LMP <- difftime(DUS_bot$medicine_date, DUS_bot$pregnancy_start_date, units = "days")
DUS_bot$gap_del <- difftime(DUS_bot$medicine_date, DUS_bot$pregnancy_end_date, units = "days")
DUS_bot$bot_prior_a <- ifelse((DUS_bot$gap_LMP >= -365 & DUS_bot$gap_LMP <= -1), 1, 0)
DUS_bot$bot_preg_a <- ifelse((DUS_bot$gap_LMP >= 0 & DUS_bot$gap_del <= 0), 1, 0)
DUS_bot$bot_T1_a <- ifelse((DUS_bot$gap_LMP >= 0 & DUS_bot$gap_LMP <= 97 & DUS_bot$gap_del<=0), 1, 0)
DUS_bot$bot_T2_a <- ifelse((DUS_bot$gap_LMP >= 98 & DUS_bot$gap_LMP <= 195 & DUS_bot$gap_del<=0), 1, 0)
DUS_bot$bot_T3_a <- ifelse((DUS_bot$gap_LMP >= 196 & DUS_bot$gap_del <= 0), 1, 0)

DUS_bot1 = DUS_bot %>%
  group_by(pregnancy_id) %>%
  summarize(bot_prior = max(bot_prior_a))
DUS_all <- merge(DUS_all, DUS_bot1, by = c("pregnancy_id"))
DUS_all$bot_prior <- replace_na(DUS_all$bot_prior, 0)
DUS_bot1 = DUS_bot %>%
  group_by(pregnancy_id) %>%
  summarize(bot_preg = max(bot_preg_a))
DUS_all <- merge(DUS_all, DUS_bot1, by = c("pregnancy_id"))
DUS_all$bot_preg <- replace_na(DUS_all$bot_preg, 0)
DUS_bot1 = DUS_bot %>%
  group_by(pregnancy_id) %>%
  summarize(bot_T1 = max(bot_T1_a))
DUS_all <- merge(DUS_all, DUS_bot1, by = c("pregnancy_id"))
DUS_all$bot_T1 <- replace_na(DUS_all$bot_T1, 0)
DUS_bot1 = DUS_bot %>%
  group_by(pregnancy_id) %>%
  summarize(bot_T2 = max(bot_T2_a))
DUS_all <- merge(DUS_all, DUS_bot1, by = c("pregnancy_id"))
DUS_all$bot_T2 <- replace_na(DUS_all$bot_T2, 0)
DUS_bot1 = DUS_bot %>%
  group_by(pregnancy_id) %>%
  summarize(bot_T3 = max(bot_T3_a))
DUS_all <- merge(DUS_all, DUS_bot1, by = c("pregnancy_id"))
DUS_all$bot_T3 <- replace_na(DUS_all$bot_T3, 0)
rm(DUS_bot, DUS_bot1, bot)

#Pizotifen/clonidine
piz_clo <- prev_med[prev_med$atc_code == "N02CX02" | prev_med$atc_code == "N02CX01", ]
DUS_piz_clo <- merge(DUS_all, piz_clo, by = "person_id", all.x = TRUE, allow.cartesian = TRUE) #many-to-many relationship
DUS_piz_clo$gap_LMP <- difftime(DUS_piz_clo$medicine_date, DUS_piz_clo$pregnancy_start_date, units = "days")
DUS_piz_clo$gap_del <- difftime(DUS_piz_clo$medicine_date, DUS_piz_clo$pregnancy_end_date, units = "days")
DUS_piz_clo$piz_clo_prior_a <- ifelse((DUS_piz_clo$gap_LMP >= -365 & DUS_piz_clo$gap_LMP <= -1), 1, 0)
DUS_piz_clo$piz_clo_preg_a <- ifelse((DUS_piz_clo$gap_LMP >= 0 & DUS_piz_clo$gap_del <= 0), 1, 0)
DUS_piz_clo$piz_clo_T1_a <- ifelse((DUS_piz_clo$gap_LMP >= 0 & DUS_piz_clo$gap_LMP <= 97 & DUS_piz_clo$gap_del<=0), 1, 0)
DUS_piz_clo$piz_clo_T2_a <- ifelse((DUS_piz_clo$gap_LMP >= 98 & DUS_piz_clo$gap_LMP <= 195 & DUS_piz_clo$gap_del<=0), 1, 0)
DUS_piz_clo$piz_clo_T3_a <- ifelse((DUS_piz_clo$gap_LMP >= 196 & DUS_piz_clo$gap_del <= 0), 1, 0)

DUS_piz_clo1 = DUS_piz_clo %>%
  group_by(pregnancy_id) %>%
  summarize(piz_clo_prior = max(piz_clo_prior_a))
DUS_all <- merge(DUS_all, DUS_piz_clo1, by = c("pregnancy_id"))
DUS_all$piz_clo_prior <- replace_na(DUS_all$piz_clo_prior, 0)
DUS_piz_clo1 = DUS_piz_clo %>%
  group_by(pregnancy_id) %>%
  summarize(piz_clo_preg = max(piz_clo_preg_a))
DUS_all <- merge(DUS_all, DUS_piz_clo1, by = c("pregnancy_id"))
DUS_all$piz_clo_preg <- replace_na(DUS_all$piz_clo_preg, 0)
DUS_piz_clo1 = DUS_piz_clo %>%
  group_by(pregnancy_id) %>%
  summarize(piz_clo_T1 = max(piz_clo_T1_a))
DUS_all <- merge(DUS_all, DUS_piz_clo1, by = c("pregnancy_id"))
DUS_all$piz_clo_T1 <- replace_na(DUS_all$piz_clo_T1, 0)
DUS_piz_clo1 = DUS_piz_clo %>%
  group_by(pregnancy_id) %>%
  summarize(piz_clo_T2 = max(piz_clo_T2_a))
DUS_all <- merge(DUS_all, DUS_piz_clo1, by = c("pregnancy_id"))
DUS_all$piz_clo_T2 <- replace_na(DUS_all$piz_clo_T2, 0)
DUS_piz_clo1 = DUS_piz_clo %>%
  group_by(pregnancy_id) %>%
  summarize(piz_clo_T3 = max(piz_clo_T3_a))
DUS_all <- merge(DUS_all, DUS_piz_clo1, by = c("pregnancy_id"))
DUS_all$piz_clo_T3 <- replace_na(DUS_all$piz_clo_T3, 0)
rm(DUS_piz_clo, DUS_piz_clo1, piz_clo)

#Preventive treatment
DUS_prev_med <- merge(DUS_all, prev_med, by = "person_id", all.x = TRUE, allow.cartesian = TRUE) #many-to-many relationship
DUS_prev_med$gap_LMP <- difftime(DUS_prev_med$medicine_date, DUS_prev_med$pregnancy_start_date, units = "days")
DUS_prev_med$gap_del <- difftime(DUS_prev_med$medicine_date, DUS_prev_med$pregnancy_end_date, units = "days")
DUS_prev_med$prev_med_prior_a <- ifelse((DUS_prev_med$gap_LMP >= -365 & DUS_prev_med$gap_LMP <= -1), 1, 0)
DUS_prev_med$prev_med_preg_a <- ifelse((DUS_prev_med$gap_LMP >= 0 & DUS_prev_med$gap_del <= 0), 1, 0)
DUS_prev_med$prev_med_T1_a <- ifelse((DUS_prev_med$gap_LMP >= 0 & DUS_prev_med$gap_LMP <= 97 & DUS_prev_med$gap_del<=0), 1, 0)
DUS_prev_med$prev_med_T2_a <- ifelse((DUS_prev_med$gap_LMP >= 98 & DUS_prev_med$gap_LMP <= 195 & DUS_prev_med$gap_del<=0), 1, 0)
DUS_prev_med$prev_med_T3_a <- ifelse((DUS_prev_med$gap_LMP >= 196 & DUS_prev_med$gap_del <= 0), 1, 0)

DUS_prev_med1 = DUS_prev_med %>%
  group_by(pregnancy_id) %>%
  summarize(prev_med_prior = max(prev_med_prior_a))
DUS_all <- merge(DUS_all, DUS_prev_med1, by = c("pregnancy_id"))
DUS_all$prev_med_prior <- replace_na(DUS_all$prev_med_prior, 0)
DUS_prev_med1 = DUS_prev_med %>%
  group_by(pregnancy_id) %>%
  summarize(prev_med_preg = max(prev_med_preg_a))
DUS_all <- merge(DUS_all, DUS_prev_med1, by = c("pregnancy_id"))
DUS_all$prev_med_preg <- replace_na(DUS_all$prev_med_preg, 0)
DUS_prev_med1 = DUS_prev_med %>%
  group_by(pregnancy_id) %>%
  summarize(prev_med_T1 = max(prev_med_T1_a))
DUS_all <- merge(DUS_all, DUS_prev_med1, by = c("pregnancy_id"))
DUS_all$prev_med_T1 <- replace_na(DUS_all$prev_med_T1, 0)
DUS_prev_med1 = DUS_prev_med %>%
  group_by(pregnancy_id) %>%
  summarize(prev_med_T2 = max(prev_med_T2_a))
DUS_all <- merge(DUS_all, DUS_prev_med1, by = c("pregnancy_id"))
DUS_all$prev_med_T2 <- replace_na(DUS_all$prev_med_T2, 0)
DUS_prev_med1 = DUS_prev_med %>%
  group_by(pregnancy_id) %>%
  summarize(prev_med_T3 = max(prev_med_T3_a))
DUS_all <- merge(DUS_all, DUS_prev_med1, by = c("pregnancy_id"))
DUS_all$prev_med_T3 <- replace_na(DUS_all$prev_med_T3, 0)
rm(DUS_prev_med, DUS_prev_med1, prev_med)

#Erenumab
ere <- cgrp[cgrp$atc_code == "N02CD01", ]
DUS_ere <- merge(DUS_all, ere, by = "person_id", all.x = TRUE, allow.cartesian = TRUE) #many-to-many relationship
DUS_ere$gap_LMP <- difftime(DUS_ere$medicine_date, DUS_ere$pregnancy_start_date, units = "days")
DUS_ere$gap_del <- difftime(DUS_ere$medicine_date, DUS_ere$pregnancy_end_date, units = "days")
DUS_ere$ere_prior_a <- ifelse((DUS_ere$gap_LMP >= -365 & DUS_ere$gap_LMP <= -1), 1, 0)
DUS_ere$ere_preg_a <- ifelse((DUS_ere$gap_LMP >= 0 & DUS_ere$gap_del <= 0), 1, 0)
DUS_ere$ere_T1_a <- ifelse((DUS_ere$gap_LMP >= 0 & DUS_ere$gap_LMP <= 97 & DUS_ere$gap_del<=0), 1, 0)
DUS_ere$ere_T2_a <- ifelse((DUS_ere$gap_LMP >= 98 & DUS_ere$gap_LMP <= 195 & DUS_ere$gap_del<=0), 1, 0)
DUS_ere$ere_T3_a <- ifelse((DUS_ere$gap_LMP >= 196 & DUS_ere$gap_del <= 0), 1, 0)

DUS_ere1 = DUS_ere %>%
  group_by(pregnancy_id) %>%
  summarize(ere_prior = max(ere_prior_a))
DUS_all <- merge(DUS_all, DUS_ere1, by = c("pregnancy_id"))
DUS_all$ere_prior <- replace_na(DUS_all$ere_prior, 0)
DUS_ere1 = DUS_ere %>%
  group_by(pregnancy_id) %>%
  summarize(ere_preg = max(ere_preg_a))
DUS_all <- merge(DUS_all, DUS_ere1, by = c("pregnancy_id"))
DUS_all$ere_preg <- replace_na(DUS_all$ere_preg, 0)
DUS_ere1 = DUS_ere %>%
  group_by(pregnancy_id) %>%
  summarize(ere_T1 = max(ere_T1_a))
DUS_all <- merge(DUS_all, DUS_ere1, by = c("pregnancy_id"))
DUS_all$ere_T1 <- replace_na(DUS_all$ere_T1, 0)
DUS_ere1 = DUS_ere %>%
  group_by(pregnancy_id) %>%
  summarize(ere_T2 = max(ere_T2_a))
DUS_all <- merge(DUS_all, DUS_ere1, by = c("pregnancy_id"))
DUS_all$ere_T2 <- replace_na(DUS_all$ere_T2, 0)
DUS_ere1 = DUS_ere %>%
  group_by(pregnancy_id) %>%
  summarize(ere_T3 = max(ere_T3_a))
DUS_all <- merge(DUS_all, DUS_ere1, by = c("pregnancy_id"))
DUS_all$ere_T3 <- replace_na(DUS_all$ere_T3, 0)
rm(DUS_ere, DUS_ere1, ere)

#Galkanezumab
gal <- cgrp[cgrp$atc_code == "N02CD02", ]
DUS_gal <- merge(DUS_all, gal, by = "person_id", all.x = TRUE, allow.cartesian = TRUE) #many-to-many relationship
DUS_gal$gap_LMP <- difftime(DUS_gal$medicine_date, DUS_gal$pregnancy_start_date, units = "days")
DUS_gal$gap_del <- difftime(DUS_gal$medicine_date, DUS_gal$pregnancy_end_date, units = "days")
DUS_gal$gal_prior_a <- ifelse((DUS_gal$gap_LMP >= -365 & DUS_gal$gap_LMP <= -1), 1, 0)
DUS_gal$gal_preg_a <- ifelse((DUS_gal$gap_LMP >= 0 & DUS_gal$gap_del <= 0), 1, 0)
DUS_gal$gal_T1_a <- ifelse((DUS_gal$gap_LMP >= 0 & DUS_gal$gap_LMP <= 97 & DUS_gal$gap_del<=0), 1, 0)
DUS_gal$gal_T2_a <- ifelse((DUS_gal$gap_LMP >= 98 & DUS_gal$gap_LMP <= 195 & DUS_gal$gap_del<=0), 1, 0)
DUS_gal$gal_T3_a <- ifelse((DUS_gal$gap_LMP >= 196 & DUS_gal$gap_del <= 0), 1, 0)

DUS_gal1 = DUS_gal %>%
  group_by(pregnancy_id) %>%
  summarize(gal_prior = max(gal_prior_a))
DUS_all <- merge(DUS_all, DUS_gal1, by = c("pregnancy_id"))
DUS_all$gal_prior <- replace_na(DUS_all$gal_prior, 0)
DUS_gal1 = DUS_gal %>%
  group_by(pregnancy_id) %>%
  summarize(gal_preg = max(gal_preg_a))
DUS_all <- merge(DUS_all, DUS_gal1, by = c("pregnancy_id"))
DUS_all$gal_preg <- replace_na(DUS_all$gal_preg, 0)
DUS_gal1 = DUS_gal %>%
  group_by(pregnancy_id) %>%
  summarize(gal_T1 = max(gal_T1_a))
DUS_all <- merge(DUS_all, DUS_gal1, by = c("pregnancy_id"))
DUS_all$gal_T1 <- replace_na(DUS_all$gal_T1, 0)
DUS_gal1 = DUS_gal %>%
  group_by(pregnancy_id) %>%
  summarize(gal_T2 = max(gal_T2_a))
DUS_all <- merge(DUS_all, DUS_gal1, by = c("pregnancy_id"))
DUS_all$gal_T2 <- replace_na(DUS_all$gal_T2, 0)
DUS_gal1 = DUS_gal %>%
  group_by(pregnancy_id) %>%
  summarize(gal_T3 = max(gal_T3_a))
DUS_all <- merge(DUS_all, DUS_gal1, by = c("pregnancy_id"))
DUS_all$gal_T3 <- replace_na(DUS_all$gal_T3, 0)
rm(DUS_gal, DUS_gal1, gal)

#Fremanezumab
fre <- cgrp[cgrp$atc_code == "N02CD03", ]
DUS_fre <- merge(DUS_all, fre, by = "person_id", all.x = TRUE, allow.cartesian = TRUE) #many-to-many relationship
DUS_fre$gap_LMP <- difftime(DUS_fre$medicine_date, DUS_fre$pregnancy_start_date, units = "days")
DUS_fre$gap_del <- difftime(DUS_fre$medicine_date, DUS_fre$pregnancy_end_date, units = "days")
DUS_fre$fre_prior_a <- ifelse((DUS_fre$gap_LMP >= -365 & DUS_fre$gap_LMP <= -1), 1, 0)
DUS_fre$fre_preg_a <- ifelse((DUS_fre$gap_LMP >= 0 & DUS_fre$gap_del <= 0), 1, 0)
DUS_fre$fre_T1_a <- ifelse((DUS_fre$gap_LMP >= 0 & DUS_fre$gap_LMP <= 97  & DUS_fre$gap_del<=0), 1, 0)
DUS_fre$fre_T2_a <- ifelse((DUS_fre$gap_LMP >= 98 & DUS_fre$gap_LMP <= 195  & DUS_fre$gap_del<=0), 1, 0)
DUS_fre$fre_T3_a <- ifelse((DUS_fre$gap_LMP >= 196 & DUS_fre$gap_del <= 0), 1, 0)

DUS_fre1 = DUS_fre %>%
  group_by(pregnancy_id) %>%
  summarize(fre_prior = max(fre_prior_a))
DUS_all <- merge(DUS_all, DUS_fre1, by = c("pregnancy_id"))
DUS_all$fre_prior <- replace_na(DUS_all$fre_prior, 0)
DUS_fre1 = DUS_fre %>%
  group_by(pregnancy_id) %>%
  summarize(fre_preg = max(fre_preg_a))
DUS_all <- merge(DUS_all, DUS_fre1, by = c("pregnancy_id"))
DUS_all$fre_preg <- replace_na(DUS_all$fre_preg, 0)
DUS_fre1 = DUS_fre %>%
  group_by(pregnancy_id) %>%
  summarize(fre_T1 = max(fre_T1_a))
DUS_all <- merge(DUS_all, DUS_fre1, by = c("pregnancy_id"))
DUS_all$fre_T1 <- replace_na(DUS_all$fre_T1, 0)
DUS_fre1 = DUS_fre %>%
  group_by(pregnancy_id) %>%
  summarize(fre_T2 = max(fre_T2_a))
DUS_all <- merge(DUS_all, DUS_fre1, by = c("pregnancy_id"))
DUS_all$fre_T2 <- replace_na(DUS_all$fre_T2, 0)
DUS_fre1 = DUS_fre %>%
  group_by(pregnancy_id) %>%
  summarize(fre_T3 = max(fre_T3_a))
DUS_all <- merge(DUS_all, DUS_fre1, by = c("pregnancy_id"))
DUS_all$fre_T3 <- replace_na(DUS_all$fre_T3, 0)
rm(DUS_fre, DUS_fre1, fre)

#CGRP
DUS_cgrp <- merge(DUS_all, cgrp, by = "person_id", all.x = TRUE, allow.cartesian = TRUE) #many-to-many relationship
DUS_cgrp$gap_LMP <- difftime(DUS_cgrp$medicine_date, DUS_cgrp$pregnancy_start_date, units = "days")
DUS_cgrp$gap_del <- difftime(DUS_cgrp$medicine_date, DUS_cgrp$pregnancy_end_date, units = "days")
DUS_cgrp$cgrp_prior_a <- ifelse((DUS_cgrp$gap_LMP >= -365 & DUS_cgrp$gap_LMP <= -1), 1, 0)
DUS_cgrp$cgrp_preg_a <- ifelse((DUS_cgrp$gap_LMP >= 0 & DUS_cgrp$gap_del <= 0), 1, 0)
DUS_cgrp$cgrp_T1_a <- ifelse((DUS_cgrp$gap_LMP >= 0 & DUS_cgrp$gap_LMP <= 97  & DUS_cgrp$gap_del<=0), 1, 0)
DUS_cgrp$cgrp_T2_a <- ifelse((DUS_cgrp$gap_LMP >= 98 & DUS_cgrp$gap_LMP <= 195  & DUS_cgrp$gap_del<=0), 1, 0)
DUS_cgrp$cgrp_T3_a <- ifelse((DUS_cgrp$gap_LMP >= 196 & DUS_cgrp$gap_del <= 0), 1, 0)

DUS_cgrp1 = DUS_cgrp %>%
  group_by(pregnancy_id) %>%
  summarize(cgrp_prior = max(cgrp_prior_a))
DUS_all <- merge(DUS_all, DUS_cgrp1, by = c("pregnancy_id"))
DUS_all$cgrp_prior <- replace_na(DUS_all$cgrp_prior, 0)
DUS_cgrp1 = DUS_cgrp %>%
  group_by(pregnancy_id) %>%
  summarize(cgrp_preg = max(cgrp_preg_a))
DUS_all <- merge(DUS_all, DUS_cgrp1, by = c("pregnancy_id"))
DUS_all$cgrp_preg <- replace_na(DUS_all$cgrp_preg, 0)
DUS_cgrp1 = DUS_cgrp %>%
  group_by(pregnancy_id) %>%
  summarize(cgrp_T1 = max(cgrp_T1_a))
DUS_all <- merge(DUS_all, DUS_cgrp1, by = c("pregnancy_id"))
DUS_all$cgrp_T1 <- replace_na(DUS_all$cgrp_T1, 0)
DUS_cgrp1 = DUS_cgrp %>%
  group_by(pregnancy_id) %>%
  summarize(cgrp_T2 = max(cgrp_T2_a))
DUS_all <- merge(DUS_all, DUS_cgrp1, by = c("pregnancy_id"))
DUS_all$cgrp_T2 <- replace_na(DUS_all$cgrp_T2, 0)
DUS_cgrp1 = DUS_cgrp %>%
  group_by(pregnancy_id) %>%
  summarize(cgrp_T3 = max(cgrp_T3_a))
DUS_all <- merge(DUS_all, DUS_cgrp1, by = c("pregnancy_id"))
DUS_all$cgrp_T3 <- replace_na(DUS_all$cgrp_T3, 0)
rm(DUS_cgrp, DUS_cgrp1, cgrp)

#Calculate cell counts for table 4 - total population
denom_all <- length(unique(DUS_all$pregnancy_id))
denom_T1 <- nrow(DUS_all[DUS_all$denom_T1 == 1, ])
acute_prior_n <- nrow(DUS_all[DUS_all$acute_prior == 1, ])
PCM_prior_n <- nrow(DUS_all[DUS_all$PCM_prior == 1, ])
COD_prior_n <- nrow(DUS_all[DUS_all$COD_prior == 1, ])
NSAID_prior_n <- nrow(DUS_all[DUS_all$NSAID_prior == 1, ])
antinausea_prior_n <- nrow(DUS_all[DUS_all$antinausea_prior == 1, ])
prev_med_prior_n <- nrow(DUS_all[DUS_all$prev_med_prior == 1, ])
meto_prop_prior_n <- nrow(DUS_all[DUS_all$meto_prop_prior == 1, ])
ami_prior_n <- nrow(DUS_all[DUS_all$ami_prior == 1, ])
c_l_v_prior_n <- nrow(DUS_all[DUS_all$c_l_v_prior == 1, ])
top_val_prior_n <- nrow(DUS_all[DUS_all$top_val_prior == 1, ])
bot_prior_n <- nrow(DUS_all[DUS_all$bot_prior == 1, ])
piz_clo_prior_n <- nrow(DUS_all[DUS_all$piz_clo_prior == 1, ])
cgrp_prior_n <- nrow(DUS_all[DUS_all$cgrp_prior == 1, ])
ere_prior_n <- nrow(DUS_all[DUS_all$ere_prior == 1, ])
gal_prior_n <- nrow(DUS_all[DUS_all$gal_prior == 1, ])
fre_prior_n <- nrow(DUS_all[DUS_all$fre_prior == 1, ])

acute_prior_prev <- round((acute_prior_n / denom_all)*100, 3)
PCM_prior_prev <- round((PCM_prior_n / denom_all)*100, 3)
COD_prior_prev <- round((COD_prior_n / denom_all)*100, 3)
NSAID_prior_prev <- round((NSAID_prior_n / denom_all)*100, 3)
antinausea_prior_prev <- round((antinausea_prior_n / denom_all)*100, 3)
prev_med_prior_prev <- round((prev_med_prior_n / denom_all)*100, 3)
meto_prop_prior_prev <- round((meto_prop_prior_n / denom_all)*100, 3)
ami_prior_prev <- round((ami_prior_n / denom_all)*100, 3)
c_l_v_prior_prev <- round((c_l_v_prior_n / denom_all)*100, 3)
top_val_prior_prev <- round((top_val_prior_n / denom_all)*100, 3)
bot_prior_prev <- round((bot_prior_n / denom_all)*100, 3)
piz_clo_prior_prev <- round((piz_clo_prior_n / denom_all)*100, 3)
cgrp_prior_prev <- round((cgrp_prior_n / denom_all)*100, 3)
ere_prior_prev <- round((ere_prior_n / denom_all)*100, 3)
gal_prior_prev <- round((gal_prior_n / denom_all)*100, 3)
fre_prior_prev <- round((fre_prior_n / denom_all)*100, 3)

acute_prior <- scoreci(acute_prior_n, denom_all)
PCM_prior <- scoreci(PCM_prior_n, denom_all)
COD_prior <- scoreci(COD_prior_n, denom_all)
NSAID_prior <- scoreci(NSAID_prior_n, denom_all)
antinausea_prior <- scoreci(antinausea_prior_n, denom_all)
prev_med_prior <- scoreci(prev_med_prior_n, denom_all)
meto_prop_prior <- scoreci(meto_prop_prior_n, denom_all)
ami_prior <- scoreci(ami_prior_n, denom_all)
c_l_v_prior <- scoreci(c_l_v_prior_n, denom_all)
top_val_prior <- scoreci(top_val_prior_n, denom_all)
bot_prior <- scoreci(bot_prior_n, denom_all)
piz_clo_prior <- scoreci(piz_clo_prior_n, denom_all)
cgrp_prior <- scoreci(cgrp_prior_n, denom_all)
ere_prior <- scoreci(ere_prior_n, denom_all)
gal_prior <- scoreci(gal_prior_n, denom_all)
fre_prior <- scoreci(fre_prior_n, denom_all)

acute_prior_low_CI <- round((acute_prior$conf.low)*100, 3)
acute_prior_up_CI <- round((acute_prior$conf.high)*100, 3)
PCM_prior_low_CI <- round((PCM_prior$conf.low)*100, 3)
PCM_prior_up_CI <- round((PCM_prior$conf.high)*100, 3)
COD_prior_low_CI <- round((COD_prior$conf.low)*100, 3)
COD_prior_up_CI <- round((COD_prior$conf.high)*100, 3)
NSAID_prior_low_CI <- round((NSAID_prior$conf.low)*100, 3)
NSAID_prior_up_CI <- round((NSAID_prior$conf.high)*100, 3)
antinausea_prior_low_CI <- round((antinausea_prior$conf.low)*100, 3)
antinausea_prior_up_CI <- round((antinausea_prior$conf.high)*100, 3)
prev_med_prior_low_CI <- round((prev_med_prior$conf.low)*100, 3)
prev_med_prior_up_CI <- round((prev_med_prior$conf.high)*100, 3)
meto_prop_prior_low_CI <- round((meto_prop_prior$conf.low)*100, 3)
meto_prop_prior_up_CI <- round((meto_prop_prior$conf.high)*100, 3)
ami_prior_low_CI <- round((ami_prior$conf.low)*100, 3)
ami_prior_up_CI <- round((ami_prior$conf.high)*100, 3)
c_l_v_prior_low_CI <- round((c_l_v_prior$conf.low)*100, 3)
c_l_v_prior_up_CI <- round((c_l_v_prior$conf.high)*100, 3)
top_val_prior_low_CI <- round((top_val_prior$conf.low)*100, 3)
top_val_prior_up_CI <- round((top_val_prior$conf.high)*100, 3)
bot_prior_low_CI <- round((bot_prior$conf.low)*100, 3)
bot_prior_up_CI <- round((bot_prior$conf.high)*100, 3)
piz_clo_prior_low_CI <- round((piz_clo_prior$conf.low)*100, 3)
piz_clo_prior_up_CI <- round((piz_clo_prior$conf.high)*100, 3)
cgrp_prior_low_CI <- round((cgrp_prior$conf.low)*100, 3)
cgrp_prior_up_CI <- round((cgrp_prior$conf.high)*100, 3)
ere_prior_low_CI <- round((ere_prior$conf.low)*100, 3)
ere_prior_up_CI <- round((ere_prior$conf.high)*100, 3)
gal_prior_low_CI <- round((gal_prior$conf.low)*100, 3)
gal_prior_up_CI <- round((gal_prior$conf.high)*100, 3)
fre_prior_low_CI <- round((fre_prior$conf.low)*100, 3)
fre_prior_up_CI <- round((fre_prior$conf.high)*100, 3)

acute_preg_n <- nrow(DUS_all[DUS_all$acute_preg == 1, ])
PCM_preg_n <- nrow(DUS_all[DUS_all$PCM_preg == 1, ])
COD_preg_n <- nrow(DUS_all[DUS_all$COD_preg == 1, ])
NSAID_preg_n <- nrow(DUS_all[DUS_all$NSAID_preg == 1, ])
antinausea_preg_n <- nrow(DUS_all[DUS_all$antinausea_preg == 1, ])
prev_med_preg_n <- nrow(DUS_all[DUS_all$prev_med_preg == 1, ])
meto_prop_preg_n <- nrow(DUS_all[DUS_all$meto_prop_preg == 1, ])
ami_preg_n <- nrow(DUS_all[DUS_all$ami_preg == 1, ])
c_l_v_preg_n <- nrow(DUS_all[DUS_all$c_l_v_preg == 1, ])
top_val_preg_n <- nrow(DUS_all[DUS_all$top_val_preg == 1, ])
bot_preg_n <- nrow(DUS_all[DUS_all$bot_preg == 1, ])
piz_clo_preg_n <- nrow(DUS_all[DUS_all$piz_clo_preg == 1, ])
cgrp_preg_n <- nrow(DUS_all[DUS_all$cgrp_preg == 1, ])
ere_preg_n <- nrow(DUS_all[DUS_all$ere_preg == 1, ])
gal_preg_n <- nrow(DUS_all[DUS_all$gal_preg == 1, ])
fre_preg_n <- nrow(DUS_all[DUS_all$fre_preg == 1, ])

acute_preg_prev <- round((acute_preg_n / denom_all)*100, 3)
PCM_preg_prev <- round((PCM_preg_n / denom_all)*100, 3)
COD_preg_prev <- round((COD_preg_n / denom_all)*100, 3)
NSAID_preg_prev <- round((NSAID_preg_n / denom_all)*100, 3)
antinausea_preg_prev <- round((antinausea_preg_n / denom_all)*100, 3)
prev_med_preg_prev <- round((prev_med_preg_n / denom_all)*100, 3)
meto_prop_preg_prev <- round((meto_prop_preg_n / denom_all)*100, 3)
ami_preg_prev <- round((ami_preg_n / denom_all)*100, 3)
c_l_v_preg_prev <- round((c_l_v_preg_n / denom_all)*100, 3)
top_val_preg_prev <- round((top_val_preg_n / denom_all)*100, 3)
bot_preg_prev <- round((bot_preg_n / denom_all)*100, 3)
piz_clo_preg_prev <- round((piz_clo_preg_n / denom_all)*100, 3)
cgrp_preg_prev <- round((cgrp_preg_n / denom_all)*100, 3)
ere_preg_prev <- round((ere_preg_n / denom_all)*100, 3)
gal_preg_prev <- round((gal_preg_n / denom_all)*100, 3)
fre_preg_prev <- round((fre_preg_n / denom_all)*100, 3)

acute_preg <- scoreci(acute_preg_n, denom_all)
PCM_preg <- scoreci(PCM_preg_n, denom_all)
COD_preg <- scoreci(COD_preg_n, denom_all)
NSAID_preg <- scoreci(NSAID_preg_n, denom_all)
antinausea_preg <- scoreci(antinausea_preg_n, denom_all)
prev_med_preg <- scoreci(prev_med_preg_n, denom_all)
meto_prop_preg <- scoreci(meto_prop_preg_n, denom_all)
ami_preg <- scoreci(ami_preg_n, denom_all)
c_l_v_preg <- scoreci(c_l_v_preg_n, denom_all)
top_val_preg <- scoreci(top_val_preg_n, denom_all)
bot_preg <- scoreci(bot_preg_n, denom_all)
piz_clo_preg <- scoreci(piz_clo_preg_n, denom_all)
cgrp_preg <- scoreci(cgrp_preg_n, denom_all)
ere_preg <- scoreci(ere_preg_n, denom_all)
gal_preg <- scoreci(gal_preg_n, denom_all)
fre_preg <- scoreci(fre_preg_n, denom_all)

acute_preg_low_CI <- round((acute_preg$conf.low)*100, 3)
acute_preg_up_CI <- round((acute_preg$conf.high)*100, 3)
PCM_preg_low_CI <- round((PCM_preg$conf.low)*100, 3)
PCM_preg_up_CI <- round((PCM_preg$conf.high)*100, 3)
COD_preg_low_CI <- round((COD_preg$conf.low)*100, 3)
COD_preg_up_CI <- round((COD_preg$conf.high)*100, 3)
NSAID_preg_low_CI <- round((NSAID_preg$conf.low)*100, 3)
NSAID_preg_up_CI <- round((NSAID_preg$conf.high)*100, 3)
antinausea_preg_low_CI <- round((antinausea_preg$conf.low)*100, 3)
antinausea_preg_up_CI <- round((antinausea_preg$conf.high)*100, 3)
prev_med_preg_low_CI <- round((prev_med_preg$conf.low)*100, 3)
prev_med_preg_up_CI <- round((prev_med_preg$conf.high)*100, 3)
meto_prop_preg_low_CI <- round((meto_prop_preg$conf.low)*100, 3)
meto_prop_preg_up_CI <- round((meto_prop_preg$conf.high)*100, 3)
ami_preg_low_CI <- round((ami_preg$conf.low)*100, 3)
ami_preg_up_CI <- round((ami_preg$conf.high)*100, 3)
c_l_v_preg_low_CI <- round((c_l_v_preg$conf.low)*100, 3)
c_l_v_preg_up_CI <- round((c_l_v_preg$conf.high)*100, 3)
top_val_preg_low_CI <- round((top_val_preg$conf.low)*100, 3)
top_val_preg_up_CI <- round((top_val_preg$conf.high)*100, 3)
bot_preg_low_CI <- round((bot_preg$conf.low)*100, 3)
bot_preg_up_CI <- round((bot_preg$conf.high)*100, 3)
piz_clo_preg_low_CI <- round((piz_clo_preg$conf.low)*100, 3)
piz_clo_preg_up_CI <- round((piz_clo_preg$conf.high)*100, 3)
cgrp_preg_low_CI <- round((cgrp_preg$conf.low)*100, 3)
cgrp_preg_up_CI <- round((cgrp_preg$conf.high)*100, 3)
ere_preg_low_CI <- round((ere_preg$conf.low)*100, 3)
ere_preg_up_CI <- round((ere_preg$conf.high)*100, 3)
gal_preg_low_CI <- round((gal_preg$conf.low)*100, 3)
gal_preg_up_CI <- round((gal_preg$conf.high)*100, 3)
fre_preg_low_CI <- round((fre_preg$conf.low)*100, 3)
fre_preg_up_CI <- round((fre_preg$conf.high)*100, 3)

acute_T1_n <- nrow(DUS_all[DUS_all$acute_T1 == 1, ])
PCM_T1_n <- nrow(DUS_all[DUS_all$PCM_T1 == 1, ])
COD_T1_n <- nrow(DUS_all[DUS_all$COD_T1 == 1, ])
NSAID_T1_n <- nrow(DUS_all[DUS_all$NSAID_T1 == 1, ])
antinausea_T1_n <- nrow(DUS_all[DUS_all$antinausea_T1 == 1, ])
prev_med_T1_n <- nrow(DUS_all[DUS_all$prev_med_T1 == 1, ])
meto_prop_T1_n <- nrow(DUS_all[DUS_all$meto_prop_T1 == 1, ])
ami_T1_n <- nrow(DUS_all[DUS_all$ami_T1 == 1, ])
c_l_v_T1_n <- nrow(DUS_all[DUS_all$c_l_v_T1 == 1, ])
top_val_T1_n <- nrow(DUS_all[DUS_all$top_val_T1 == 1, ])
bot_T1_n <- nrow(DUS_all[DUS_all$bot_T1 == 1, ])
piz_clo_T1_n <- nrow(DUS_all[DUS_all$piz_clo_T1 == 1, ])
cgrp_T1_n <- nrow(DUS_all[DUS_all$cgrp_T1 == 1, ])
ere_T1_n <- nrow(DUS_all[DUS_all$ere_T1 == 1, ])
gal_T1_n <- nrow(DUS_all[DUS_all$gal_T1 == 1, ])
fre_T1_n <- nrow(DUS_all[DUS_all$fre_T1 == 1, ])

acute_T1_prev <- round((acute_T1_n / denom_T1)*100, 3)
PCM_T1_prev <- round((PCM_T1_n / denom_T1)*100, 3)
COD_T1_prev <- round((COD_T1_n / denom_T1)*100, 3)
NSAID_T1_prev <- round((NSAID_T1_n / denom_T1)*100, 3)
antinausea_T1_prev <- round((antinausea_T1_n / denom_T1)*100, 3)
prev_med_T1_prev <- round((prev_med_T1_n / denom_T1)*100, 3)
meto_prop_T1_prev <- round((meto_prop_T1_n / denom_T1)*100, 3)
ami_T1_prev <- round((ami_T1_n / denom_T1)*100, 3)
c_l_v_T1_prev <- round((c_l_v_T1_n / denom_T1)*100, 3)
top_val_T1_prev <- round((top_val_T1_n / denom_T1)*100, 3)
bot_T1_prev <- round((bot_T1_n / denom_T1)*100, 3)
piz_clo_T1_prev <- round((piz_clo_T1_n / denom_T1)*100, 3)
cgrp_T1_prev <- round((cgrp_T1_n / denom_T1)*100, 3)
ere_T1_prev <- round((ere_T1_n / denom_T1)*100, 3)
gal_T1_prev <- round((gal_T1_n / denom_T1)*100, 3)
fre_T1_prev <- round((fre_T1_n / denom_T1)*100, 3)

acute_T1 <- scoreci(acute_T1_n, denom_T1)
PCM_T1 <- scoreci(PCM_T1_n, denom_T1)
COD_T1 <- scoreci(COD_T1_n, denom_T1)
NSAID_T1 <- scoreci(NSAID_T1_n, denom_T1)
antinausea_T1 <- scoreci(antinausea_T1_n, denom_T1)
prev_med_T1 <- scoreci(prev_med_T1_n, denom_T1)
meto_prop_T1 <- scoreci(meto_prop_T1_n, denom_T1)
ami_T1 <- scoreci(ami_T1_n, denom_T1)
c_l_v_T1 <- scoreci(c_l_v_T1_n, denom_T1)
top_val_T1 <- scoreci(top_val_T1_n, denom_T1)
bot_T1 <- scoreci(bot_T1_n, denom_T1)
piz_clo_T1 <- scoreci(piz_clo_T1_n, denom_T1)
cgrp_T1 <- scoreci(cgrp_T1_n, denom_T1)
ere_T1 <- scoreci(ere_T1_n, denom_T1)
gal_T1 <- scoreci(gal_T1_n, denom_T1)
fre_T1 <- scoreci(fre_T1_n, denom_T1)

acute_T1_low_CI <- round((acute_T1$conf.low)*100, 3)
acute_T1_up_CI <- round((acute_T1$conf.high)*100, 3)
PCM_T1_low_CI <- round((PCM_T1$conf.low)*100, 3)
PCM_T1_up_CI <- round((PCM_T1$conf.high)*100, 3)
COD_T1_low_CI <- round((COD_T1$conf.low)*100, 3)
COD_T1_up_CI <- round((COD_T1$conf.high)*100, 3)
NSAID_T1_low_CI <- round((NSAID_T1$conf.low)*100, 3)
NSAID_T1_up_CI <- round((NSAID_T1$conf.high)*100, 3)
antinausea_T1_low_CI <- round((antinausea_T1$conf.low)*100, 3)
antinausea_T1_up_CI <- round((antinausea_T1$conf.high)*100, 3)
prev_med_T1_low_CI <- round((prev_med_T1$conf.low)*100, 3)
prev_med_T1_up_CI <- round((prev_med_T1$conf.high)*100, 3)
meto_prop_T1_low_CI <- round((meto_prop_T1$conf.low)*100, 3)
meto_prop_T1_up_CI <- round((meto_prop_T1$conf.high)*100, 3)
ami_T1_low_CI <- round((ami_T1$conf.low)*100, 3)
ami_T1_up_CI <- round((ami_T1$conf.high)*100, 3)
c_l_v_T1_low_CI <- round((c_l_v_T1$conf.low)*100, 3)
c_l_v_T1_up_CI <- round((c_l_v_T1$conf.high)*100, 3)
top_val_T1_low_CI <- round((top_val_T1$conf.low)*100, 3)
top_val_T1_up_CI <- round((top_val_T1$conf.high)*100, 3)
bot_T1_low_CI <- round((bot_T1$conf.low)*100, 3)
bot_T1_up_CI <- round((bot_T1$conf.high)*100, 3)
piz_clo_T1_low_CI <- round((piz_clo_T1$conf.low)*100, 3)
piz_clo_T1_up_CI <- round((piz_clo_T1$conf.high)*100, 3)
cgrp_T1_low_CI <- round((cgrp_T1$conf.low)*100, 3)
cgrp_T1_up_CI <- round((cgrp_T1$conf.high)*100, 3)
ere_T1_low_CI <- round((ere_T1$conf.low)*100, 3)
ere_T1_up_CI <- round((ere_T1$conf.high)*100, 3)
gal_T1_low_CI <- round((gal_T1$conf.low)*100, 3)
gal_T1_up_CI <- round((gal_T1$conf.high)*100, 3)
fre_T1_low_CI <- round((fre_T1$conf.low)*100, 3)
fre_T1_up_CI <- round((fre_T1$conf.high)*100, 3)

table4_total <- data.frame(
  Time_period = c("Acute migraine treatment", "0-12 months before LMP", "Any time during pregnancy", "Trimester 1", "Paracetamol", "0-12 months before LMP", "Any time during pregnancy", "Trimester 1", "Codeine and paracetamol", "0-12 months before LMP", "Any time during pregnancy", "Trimester 1", "NSAIDs", "0-12 months before LMP", "Any time during pregnancy", "Trimester 1", "Antinauseants", "0-12 months before LMP", "Any time during pregnancy", "Trimester 1", "Preventive treatment", "0-12 months before LMP", "Any time during pregnancy", "Trimester 1", "Metoprolol/propranolol", "0-12 months before LMP", "Any time during pregnancy", "Trimester 1", "Amitriptyline", "0-12 months before LMP", "Any time during pregnancy", "Trimester 1", "Candesartan/lisinopril/verapamil", "0-12 months before LMP", "Any time during pregnancy", "Trimester 1", "Topiramate/valproic acid", "0-12 months before LMP", "Any time during pregnancy", "Trimester 1", "Botulinum toxin", "0-12 months before LMP", "Any time during pregnancy", "Trimester 1", "Pizotifen/clonidine", "0-12 months before LMP", "Any time during pregnancy", "Trimester 1", "CGRP antagonists", "0-12 months before LMP", "Any time during pregnancy", "Trimester 1", "Erenumab", "0-12 months before LMP", "Any time during pregnancy", "Trimester 1", "Galkanezumab", "0-12 months before LMP", "Any time during pregnancy", "Trimester 1", "Fremanezumab", "0-12 months before LMP", "Any time during pregnancy", "Trimester 1"),
  N_pregnancies = c("", denom_all, denom_all, denom_T1, "", denom_all, denom_all, denom_T1, "", denom_all, denom_all, denom_T1, "", denom_all, denom_all, denom_T1, "", denom_all, denom_all, denom_T1, "", denom_all, denom_all, denom_T1, "", denom_all, denom_all, denom_T1, "", denom_all, denom_all, denom_T1, "", denom_all, denom_all, denom_T1, "", denom_all, denom_all, denom_T1, "", denom_all, denom_all, denom_T1, "", denom_all, denom_all, denom_T1, "", denom_all, denom_all, denom_T1, "", denom_all, denom_all, denom_T1, "", denom_all, denom_all, denom_T1, "", denom_all, denom_all, denom_T1),
  N_users = c("", acute_prior_n, acute_preg_n, acute_T1_n, "", PCM_prior_n, PCM_preg_n, PCM_T1_n, "", COD_prior_n, COD_preg_n, COD_T1_n, "", NSAID_prior_n, NSAID_preg_n, NSAID_T1_n, "", antinausea_prior_n, antinausea_preg_n, antinausea_T1_n, "", prev_med_prior_n, prev_med_preg_n, prev_med_T1_n, "", meto_prop_prior_n, meto_prop_preg_n, meto_prop_T1_n, "", ami_prior_n, ami_preg_n, ami_T1_n, "", c_l_v_prior_n, c_l_v_preg_n, c_l_v_T1_n, "", top_val_prior_n, top_val_preg_n, top_val_T1_n, "", bot_prior_n, bot_preg_n, bot_T1_n, "", piz_clo_prior_n, piz_clo_preg_n, piz_clo_T1_n, "", cgrp_prior_n, cgrp_preg_n, cgrp_T1_n, "", ere_prior_n, ere_preg_n, ere_T1_n, "", gal_prior_n, gal_preg_n, gal_T1_n, "", fre_prior_n, fre_preg_n, fre_T1_n),
  Prevalence = c("", acute_prior_prev, acute_preg_prev, acute_T1_prev, "", PCM_prior_prev, PCM_preg_prev, PCM_T1_prev, "", COD_prior_prev, COD_preg_prev, COD_T1_prev, "", NSAID_prior_prev, NSAID_preg_prev, NSAID_T1_prev, "", antinausea_prior_prev, antinausea_preg_prev, antinausea_T1_prev, "", prev_med_prior_prev, prev_med_preg_prev, prev_med_T1_prev, "", meto_prop_prior_prev, meto_prop_preg_prev, meto_prop_T1_prev, "", ami_prior_prev, ami_preg_prev, ami_T1_prev, "", c_l_v_prior_prev, c_l_v_preg_prev, c_l_v_T1_prev, "", top_val_prior_prev, top_val_preg_prev, top_val_T1_prev, "", bot_prior_prev, bot_preg_prev, bot_T1_prev, "", piz_clo_prior_prev, piz_clo_preg_prev, piz_clo_T1_prev, "", cgrp_prior_prev, cgrp_preg_prev, cgrp_T1_prev, "", ere_prior_prev, ere_preg_prev, ere_T1_prev, "", gal_prior_prev, gal_preg_prev, gal_T1_prev, "", fre_prior_prev, fre_preg_prev, fre_T1_prev),
  CI_lower = c("", acute_prior_low_CI, acute_preg_low_CI, acute_T1_low_CI, "", PCM_prior_low_CI, PCM_preg_low_CI, PCM_T1_low_CI, "", COD_prior_low_CI, COD_preg_low_CI, COD_T1_low_CI, "", NSAID_prior_low_CI, NSAID_preg_low_CI, NSAID_T1_low_CI, "", antinausea_prior_low_CI, antinausea_preg_low_CI, antinausea_T1_low_CI, "", prev_med_prior_low_CI, prev_med_preg_low_CI, prev_med_T1_low_CI, "", meto_prop_prior_low_CI, meto_prop_preg_low_CI, meto_prop_T1_low_CI, "", ami_prior_low_CI, ami_preg_low_CI, ami_T1_low_CI, "", c_l_v_prior_low_CI, c_l_v_preg_low_CI, c_l_v_T1_low_CI, "", top_val_prior_low_CI, top_val_preg_low_CI, top_val_T1_low_CI, "", bot_prior_low_CI, bot_preg_low_CI, bot_T1_low_CI, "", piz_clo_prior_low_CI, piz_clo_preg_low_CI, piz_clo_T1_low_CI, "", cgrp_prior_low_CI, cgrp_preg_low_CI, cgrp_T1_low_CI, "", ere_prior_low_CI, ere_preg_low_CI, ere_T1_low_CI, "", gal_prior_low_CI, gal_preg_low_CI, gal_T1_low_CI, "", fre_prior_low_CI, fre_preg_low_CI, fre_T1_low_CI),
  CI_upper = c("", acute_prior_up_CI, acute_preg_up_CI, acute_T1_up_CI, "", PCM_prior_up_CI, PCM_preg_up_CI, PCM_T1_up_CI, "", COD_prior_up_CI, COD_preg_up_CI, COD_T1_up_CI, "", NSAID_prior_up_CI, NSAID_preg_up_CI, NSAID_T1_up_CI, "", antinausea_prior_up_CI, antinausea_preg_up_CI, antinausea_T1_up_CI, "", prev_med_prior_up_CI, prev_med_preg_up_CI, prev_med_T1_up_CI, "", meto_prop_prior_up_CI, meto_prop_preg_up_CI, meto_prop_T1_up_CI, "", ami_prior_up_CI, ami_preg_up_CI, ami_T1_up_CI, "", c_l_v_prior_up_CI, c_l_v_preg_up_CI, c_l_v_T1_up_CI, "", top_val_prior_up_CI, top_val_preg_up_CI, top_val_T1_up_CI, "", bot_prior_up_CI, bot_preg_up_CI, bot_T1_up_CI, "", piz_clo_prior_up_CI, piz_clo_preg_up_CI, piz_clo_T1_up_CI, "", cgrp_prior_up_CI, cgrp_preg_up_CI, cgrp_T1_up_CI, "", ere_prior_up_CI, ere_preg_up_CI, ere_T1_up_CI, "", gal_prior_up_CI, gal_preg_up_CI, gal_T1_up_CI, "", fre_prior_up_CI, fre_preg_up_CI, fre_T1_up_CI)
)

write.csv(table4_total, file.path(csv_path, "Table4_total.csv"), row.names = FALSE)  ##Path to output folder

# rm(list = ls.str(mode = "numeric"))
# rm(list = ls()[! ls() %in% c("dep", "diab", "DUS_all", "HT", "obesity", "replace_na", "scoreci")])

#Calculate cell counts for table 4 - continuers only
DUS_continuer <- DUS_all[DUS_all$trip_group == "Continuer" | DUS_all$trip_group == "Initiator", ]
denom_all <- length(unique(DUS_continuer$pregnancy_id))
denom_T1 <- nrow(DUS_continuer[DUS_continuer$denom_T1 == 1, ])
acute_prior_n <- nrow(DUS_continuer[DUS_continuer$acute_prior == 1, ])
PCM_prior_n <- nrow(DUS_continuer[DUS_continuer$PCM_prior == 1, ])
COD_prior_n <- nrow(DUS_continuer[DUS_continuer$COD_prior == 1, ])
NSAID_prior_n <- nrow(DUS_continuer[DUS_continuer$NSAID_prior == 1, ])
antinausea_prior_n <- nrow(DUS_continuer[DUS_continuer$antinausea_prior == 1, ])
prev_med_prior_n <- nrow(DUS_continuer[DUS_continuer$prev_med_prior == 1, ])
meto_prop_prior_n <- nrow(DUS_continuer[DUS_continuer$meto_prop_prior == 1, ])
ami_prior_n <- nrow(DUS_continuer[DUS_continuer$ami_prior == 1, ])
c_l_v_prior_n <- nrow(DUS_continuer[DUS_continuer$c_l_v_prior == 1, ])
top_val_prior_n <- nrow(DUS_continuer[DUS_continuer$top_val_prior == 1, ])
bot_prior_n <- nrow(DUS_continuer[DUS_continuer$bot_prior == 1, ])
piz_clo_prior_n <- nrow(DUS_continuer[DUS_continuer$piz_clo_prior == 1, ])
cgrp_prior_n <- nrow(DUS_continuer[DUS_continuer$cgrp_prior == 1, ])
ere_prior_n <- nrow(DUS_continuer[DUS_continuer$ere_prior == 1, ])
gal_prior_n <- nrow(DUS_continuer[DUS_continuer$gal_prior == 1, ])
fre_prior_n <- nrow(DUS_continuer[DUS_continuer$fre_prior == 1, ])

acute_prior_prev <- round((acute_prior_n / denom_all)*100, 3)
PCM_prior_prev <- round((PCM_prior_n / denom_all)*100, 3)
COD_prior_prev <- round((COD_prior_n / denom_all)*100, 3)
NSAID_prior_prev <- round((NSAID_prior_n / denom_all)*100, 3)
antinausea_prior_prev <- round((antinausea_prior_n / denom_all)*100, 3)
prev_med_prior_prev <- round((prev_med_prior_n / denom_all)*100, 3)
meto_prop_prior_prev <- round((meto_prop_prior_n / denom_all)*100, 3)
ami_prior_prev <- round((ami_prior_n / denom_all)*100, 3)
c_l_v_prior_prev <- round((c_l_v_prior_n / denom_all)*100, 3)
top_val_prior_prev <- round((top_val_prior_n / denom_all)*100, 3)
bot_prior_prev <- round((bot_prior_n / denom_all)*100, 3)
piz_clo_prior_prev <- round((piz_clo_prior_n / denom_all)*100, 3)
cgrp_prior_prev <- round((cgrp_prior_n / denom_all)*100, 3)
ere_prior_prev <- round((ere_prior_n / denom_all)*100, 3)
gal_prior_prev <- round((gal_prior_n / denom_all)*100, 3)
fre_prior_prev <- round((fre_prior_n / denom_all)*100, 3)

acute_prior <- scoreci(acute_prior_n, denom_all)
PCM_prior <- scoreci(PCM_prior_n, denom_all)
COD_prior <- scoreci(COD_prior_n, denom_all)
NSAID_prior <- scoreci(NSAID_prior_n, denom_all)
antinausea_prior <- scoreci(antinausea_prior_n, denom_all)
prev_med_prior <- scoreci(prev_med_prior_n, denom_all)
meto_prop_prior <- scoreci(meto_prop_prior_n, denom_all)
ami_prior <- scoreci(ami_prior_n, denom_all)
c_l_v_prior <- scoreci(c_l_v_prior_n, denom_all)
top_val_prior <- scoreci(top_val_prior_n, denom_all)
bot_prior <- scoreci(bot_prior_n, denom_all)
piz_clo_prior <- scoreci(piz_clo_prior_n, denom_all)
cgrp_prior <- scoreci(cgrp_prior_n, denom_all)
ere_prior <- scoreci(ere_prior_n, denom_all)
gal_prior <- scoreci(gal_prior_n, denom_all)
fre_prior <- scoreci(fre_prior_n, denom_all)

acute_prior_low_CI <- round((acute_prior$conf.low)*100, 3)
acute_prior_up_CI <- round((acute_prior$conf.high)*100, 3)
PCM_prior_low_CI <- round((PCM_prior$conf.low)*100, 3)
PCM_prior_up_CI <- round((PCM_prior$conf.high)*100, 3)
COD_prior_low_CI <- round((COD_prior$conf.low)*100, 3)
COD_prior_up_CI <- round((COD_prior$conf.high)*100, 3)
NSAID_prior_low_CI <- round((NSAID_prior$conf.low)*100, 3)
NSAID_prior_up_CI <- round((NSAID_prior$conf.high)*100, 3)
antinausea_prior_low_CI <- round((antinausea_prior$conf.low)*100, 3)
antinausea_prior_up_CI <- round((antinausea_prior$conf.high)*100, 3)
prev_med_prior_low_CI <- round((prev_med_prior$conf.low)*100, 3)
prev_med_prior_up_CI <- round((prev_med_prior$conf.high)*100, 3)
meto_prop_prior_low_CI <- round((meto_prop_prior$conf.low)*100, 3)
meto_prop_prior_up_CI <- round((meto_prop_prior$conf.high)*100, 3)
ami_prior_low_CI <- round((ami_prior$conf.low)*100, 3)
ami_prior_up_CI <- round((ami_prior$conf.high)*100, 3)
c_l_v_prior_low_CI <- round((c_l_v_prior$conf.low)*100, 3)
c_l_v_prior_up_CI <- round((c_l_v_prior$conf.high)*100, 3)
top_val_prior_low_CI <- round((top_val_prior$conf.low)*100, 3)
top_val_prior_up_CI <- round((top_val_prior$conf.high)*100, 3)
bot_prior_low_CI <- round((bot_prior$conf.low)*100, 3)
bot_prior_up_CI <- round((bot_prior$conf.high)*100, 3)
piz_clo_prior_low_CI <- round((piz_clo_prior$conf.low)*100, 3)
piz_clo_prior_up_CI <- round((piz_clo_prior$conf.high)*100, 3)
cgrp_prior_low_CI <- round((cgrp_prior$conf.low)*100, 3)
cgrp_prior_up_CI <- round((cgrp_prior$conf.high)*100, 3)
ere_prior_low_CI <- round((ere_prior$conf.low)*100, 3)
ere_prior_up_CI <- round((ere_prior$conf.high)*100, 3)
gal_prior_low_CI <- round((gal_prior$conf.low)*100, 3)
gal_prior_up_CI <- round((gal_prior$conf.high)*100, 3)
fre_prior_low_CI <- round((fre_prior$conf.low)*100, 3)
fre_prior_up_CI <- round((fre_prior$conf.high)*100, 3)

acute_preg_n <- nrow(DUS_continuer[DUS_continuer$acute_preg == 1, ])
PCM_preg_n <- nrow(DUS_continuer[DUS_continuer$PCM_preg == 1, ])
COD_preg_n <- nrow(DUS_continuer[DUS_continuer$COD_preg == 1, ])
NSAID_preg_n <- nrow(DUS_continuer[DUS_continuer$NSAID_preg == 1, ])
antinausea_preg_n <- nrow(DUS_continuer[DUS_continuer$antinausea_preg == 1, ])
prev_med_preg_n <- nrow(DUS_continuer[DUS_continuer$prev_med_preg == 1, ])
meto_prop_preg_n <- nrow(DUS_continuer[DUS_continuer$meto_prop_preg == 1, ])
ami_preg_n <- nrow(DUS_continuer[DUS_continuer$ami_preg == 1, ])
c_l_v_preg_n <- nrow(DUS_continuer[DUS_continuer$c_l_v_preg == 1, ])
top_val_preg_n <- nrow(DUS_continuer[DUS_continuer$top_val_preg == 1, ])
bot_preg_n <- nrow(DUS_continuer[DUS_continuer$bot_preg == 1, ])
piz_clo_preg_n <- nrow(DUS_continuer[DUS_continuer$piz_clo_preg == 1, ])
cgrp_preg_n <- nrow(DUS_continuer[DUS_continuer$cgrp_preg == 1, ])
ere_preg_n <- nrow(DUS_continuer[DUS_continuer$ere_preg == 1, ])
gal_preg_n <- nrow(DUS_continuer[DUS_continuer$gal_preg == 1, ])
fre_preg_n <- nrow(DUS_continuer[DUS_continuer$fre_preg == 1, ])

acute_preg_prev <- round((acute_preg_n / denom_all)*100, 3)
PCM_preg_prev <- round((PCM_preg_n / denom_all)*100, 3)
COD_preg_prev <- round((COD_preg_n / denom_all)*100, 3)
NSAID_preg_prev <- round((NSAID_preg_n / denom_all)*100, 3)
antinausea_preg_prev <- round((antinausea_preg_n / denom_all)*100, 3)
prev_med_preg_prev <- round((prev_med_preg_n / denom_all)*100, 3)
meto_prop_preg_prev <- round((meto_prop_preg_n / denom_all)*100, 3)
ami_preg_prev <- round((ami_preg_n / denom_all)*100, 3)
c_l_v_preg_prev <- round((c_l_v_preg_n / denom_all)*100, 3)
top_val_preg_prev <- round((top_val_preg_n / denom_all)*100, 3)
bot_preg_prev <- round((bot_preg_n / denom_all)*100, 3)
piz_clo_preg_prev <- round((piz_clo_preg_n / denom_all)*100, 3)
cgrp_preg_prev <- round((cgrp_preg_n / denom_all)*100, 3)
ere_preg_prev <- round((ere_preg_n / denom_all)*100, 3)
gal_preg_prev <- round((gal_preg_n / denom_all)*100, 3)
fre_preg_prev <- round((fre_preg_n / denom_all)*100, 3)

acute_preg <- scoreci(acute_preg_n, denom_all)
PCM_preg <- scoreci(PCM_preg_n, denom_all)
COD_preg <- scoreci(COD_preg_n, denom_all)
NSAID_preg <- scoreci(NSAID_preg_n, denom_all)
antinausea_preg <- scoreci(antinausea_preg_n, denom_all)
prev_med_preg <- scoreci(prev_med_preg_n, denom_all)
meto_prop_preg <- scoreci(meto_prop_preg_n, denom_all)
ami_preg <- scoreci(ami_preg_n, denom_all)
c_l_v_preg <- scoreci(c_l_v_preg_n, denom_all)
top_val_preg <- scoreci(top_val_preg_n, denom_all)
bot_preg <- scoreci(bot_preg_n, denom_all)
piz_clo_preg <- scoreci(piz_clo_preg_n, denom_all)
cgrp_preg <- scoreci(cgrp_preg_n, denom_all)
ere_preg <- scoreci(ere_preg_n, denom_all)
gal_preg <- scoreci(gal_preg_n, denom_all)
fre_preg <- scoreci(fre_preg_n, denom_all)

acute_preg_low_CI <- round((acute_preg$conf.low)*100, 3)
acute_preg_up_CI <- round((acute_preg$conf.high)*100, 3)
PCM_preg_low_CI <- round((PCM_preg$conf.low)*100, 3)
PCM_preg_up_CI <- round((PCM_preg$conf.high)*100, 3)
COD_preg_low_CI <- round((COD_preg$conf.low)*100, 3)
COD_preg_up_CI <- round((COD_preg$conf.high)*100, 3)
NSAID_preg_low_CI <- round((NSAID_preg$conf.low)*100, 3)
NSAID_preg_up_CI <- round((NSAID_preg$conf.high)*100, 3)
antinausea_preg_low_CI <- round((antinausea_preg$conf.low)*100, 3)
antinausea_preg_up_CI <- round((antinausea_preg$conf.high)*100, 3)
prev_med_preg_low_CI <- round((prev_med_preg$conf.low)*100, 3)
prev_med_preg_up_CI <- round((prev_med_preg$conf.high)*100, 3)
meto_prop_preg_low_CI <- round((meto_prop_preg$conf.low)*100, 3)
meto_prop_preg_up_CI <- round((meto_prop_preg$conf.high)*100, 3)
ami_preg_low_CI <- round((ami_preg$conf.low)*100, 3)
ami_preg_up_CI <- round((ami_preg$conf.high)*100, 3)
c_l_v_preg_low_CI <- round((c_l_v_preg$conf.low)*100, 3)
c_l_v_preg_up_CI <- round((c_l_v_preg$conf.high)*100, 3)
top_val_preg_low_CI <- round((top_val_preg$conf.low)*100, 3)
top_val_preg_up_CI <- round((top_val_preg$conf.high)*100, 3)
bot_preg_low_CI <- round((bot_preg$conf.low)*100, 3)
bot_preg_up_CI <- round((bot_preg$conf.high)*100, 3)
piz_clo_preg_low_CI <- round((piz_clo_preg$conf.low)*100, 3)
piz_clo_preg_up_CI <- round((piz_clo_preg$conf.high)*100, 3)
cgrp_preg_low_CI <- round((cgrp_preg$conf.low)*100, 3)
cgrp_preg_up_CI <- round((cgrp_preg$conf.high)*100, 3)
ere_preg_low_CI <- round((ere_preg$conf.low)*100, 3)
ere_preg_up_CI <- round((ere_preg$conf.high)*100, 3)
gal_preg_low_CI <- round((gal_preg$conf.low)*100, 3)
gal_preg_up_CI <- round((gal_preg$conf.high)*100, 3)
fre_preg_low_CI <- round((fre_preg$conf.low)*100, 3)
fre_preg_up_CI <- round((fre_preg$conf.high)*100, 3)

acute_T1_n <- nrow(DUS_continuer[DUS_continuer$acute_T1 == 1, ])
PCM_T1_n <- nrow(DUS_continuer[DUS_continuer$PCM_T1 == 1, ])
COD_T1_n <- nrow(DUS_continuer[DUS_continuer$COD_T1 == 1, ])
NSAID_T1_n <- nrow(DUS_continuer[DUS_continuer$NSAID_T1 == 1, ])
antinausea_T1_n <- nrow(DUS_continuer[DUS_continuer$antinausea_T1 == 1, ])
prev_med_T1_n <- nrow(DUS_continuer[DUS_continuer$prev_med_T1 == 1, ])
meto_prop_T1_n <- nrow(DUS_continuer[DUS_continuer$meto_prop_T1 == 1, ])
ami_T1_n <- nrow(DUS_continuer[DUS_continuer$ami_T1 == 1, ])
c_l_v_T1_n <- nrow(DUS_continuer[DUS_continuer$c_l_v_T1 == 1, ])
top_val_T1_n <- nrow(DUS_continuer[DUS_continuer$top_val_T1 == 1, ])
bot_T1_n <- nrow(DUS_continuer[DUS_continuer$bot_T1 == 1, ])
piz_clo_T1_n <- nrow(DUS_continuer[DUS_continuer$piz_clo_T1 == 1, ])
cgrp_T1_n <- nrow(DUS_continuer[DUS_continuer$cgrp_T1 == 1, ])
ere_T1_n <- nrow(DUS_continuer[DUS_continuer$ere_T1 == 1, ])
gal_T1_n <- nrow(DUS_continuer[DUS_continuer$gal_T1 == 1, ])
fre_T1_n <- nrow(DUS_continuer[DUS_continuer$fre_T1 == 1, ])

acute_T1_prev <- round((acute_T1_n / denom_T1)*100, 3)
PCM_T1_prev <- round((PCM_T1_n / denom_T1)*100, 3)
COD_T1_prev <- round((COD_T1_n / denom_T1)*100, 3)
NSAID_T1_prev <- round((NSAID_T1_n / denom_T1)*100, 3)
antinausea_T1_prev <- round((antinausea_T1_n / denom_T1)*100, 3)
prev_med_T1_prev <- round((prev_med_T1_n / denom_T1)*100, 3)
meto_prop_T1_prev <- round((meto_prop_T1_n / denom_T1)*100, 3)
ami_T1_prev <- round((ami_T1_n / denom_T1)*100, 3)
c_l_v_T1_prev <- round((c_l_v_T1_n / denom_T1)*100, 3)
top_val_T1_prev <- round((top_val_T1_n / denom_T1)*100, 3)
bot_T1_prev <- round((bot_T1_n / denom_T1)*100, 3)
piz_clo_T1_prev <- round((piz_clo_T1_n / denom_T1)*100, 3)
cgrp_T1_prev <- round((cgrp_T1_n / denom_T1)*100, 3)
ere_T1_prev <- round((ere_T1_n / denom_T1)*100, 3)
gal_T1_prev <- round((gal_T1_n / denom_T1)*100, 3)
fre_T1_prev <- round((fre_T1_n / denom_T1)*100, 3)

acute_T1 <- scoreci(acute_T1_n, denom_T1)
PCM_T1 <- scoreci(PCM_T1_n, denom_T1)
COD_T1 <- scoreci(COD_T1_n, denom_T1)
NSAID_T1 <- scoreci(NSAID_T1_n, denom_T1)
antinausea_T1 <- scoreci(antinausea_T1_n, denom_T1)
prev_med_T1 <- scoreci(prev_med_T1_n, denom_T1)
meto_prop_T1 <- scoreci(meto_prop_T1_n, denom_T1)
ami_T1 <- scoreci(ami_T1_n, denom_T1)
c_l_v_T1 <- scoreci(c_l_v_T1_n, denom_T1)
top_val_T1 <- scoreci(top_val_T1_n, denom_T1)
bot_T1 <- scoreci(bot_T1_n, denom_T1)
piz_clo_T1 <- scoreci(piz_clo_T1_n, denom_T1)
cgrp_T1 <- scoreci(cgrp_T1_n, denom_T1)
ere_T1 <- scoreci(ere_T1_n, denom_T1)
gal_T1 <- scoreci(gal_T1_n, denom_T1)
fre_T1 <- scoreci(fre_T1_n, denom_T1)

acute_T1_low_CI <- round((acute_T1$conf.low)*100, 3)
acute_T1_up_CI <- round((acute_T1$conf.high)*100, 3)
PCM_T1_low_CI <- round((PCM_T1$conf.low)*100, 3)
PCM_T1_up_CI <- round((PCM_T1$conf.high)*100, 3)
COD_T1_low_CI <- round((COD_T1$conf.low)*100, 3)
COD_T1_up_CI <- round((COD_T1$conf.high)*100, 3)
NSAID_T1_low_CI <- round((NSAID_T1$conf.low)*100, 3)
NSAID_T1_up_CI <- round((NSAID_T1$conf.high)*100, 3)
antinausea_T1_low_CI <- round((antinausea_T1$conf.low)*100, 3)
antinausea_T1_up_CI <- round((antinausea_T1$conf.high)*100, 3)
prev_med_T1_low_CI <- round((prev_med_T1$conf.low)*100, 3)
prev_med_T1_up_CI <- round((prev_med_T1$conf.high)*100, 3)
meto_prop_T1_low_CI <- round((meto_prop_T1$conf.low)*100, 3)
meto_prop_T1_up_CI <- round((meto_prop_T1$conf.high)*100, 3)
ami_T1_low_CI <- round((ami_T1$conf.low)*100, 3)
ami_T1_up_CI <- round((ami_T1$conf.high)*100, 3)
c_l_v_T1_low_CI <- round((c_l_v_T1$conf.low)*100, 3)
c_l_v_T1_up_CI <- round((c_l_v_T1$conf.high)*100, 3)
top_val_T1_low_CI <- round((top_val_T1$conf.low)*100, 3)
top_val_T1_up_CI <- round((top_val_T1$conf.high)*100, 3)
bot_T1_low_CI <- round((bot_T1$conf.low)*100, 3)
bot_T1_up_CI <- round((bot_T1$conf.high)*100, 3)
piz_clo_T1_low_CI <- round((piz_clo_T1$conf.low)*100, 3)
piz_clo_T1_up_CI <- round((piz_clo_T1$conf.high)*100, 3)
cgrp_T1_low_CI <- round((cgrp_T1$conf.low)*100, 3)
cgrp_T1_up_CI <- round((cgrp_T1$conf.high)*100, 3)
ere_T1_low_CI <- round((ere_T1$conf.low)*100, 3)
ere_T1_up_CI <- round((ere_T1$conf.high)*100, 3)
gal_T1_low_CI <- round((gal_T1$conf.low)*100, 3)
gal_T1_up_CI <- round((gal_T1$conf.high)*100, 3)
fre_T1_low_CI <- round((fre_T1$conf.low)*100, 3)
fre_T1_up_CI <- round((fre_T1$conf.high)*100, 3)

table4_continuer <- data.frame(
  Time_period = c("Acute migraine treatment", "0-12 months before LMP", "Any time during pregnancy", "Trimester 1", "Paracetamol", "0-12 months before LMP", "Any time during pregnancy", "Trimester 1", "Codeine and paracetamol", "0-12 months before LMP", "Any time during pregnancy", "Trimester 1", "NSAIDs", "0-12 months before LMP", "Any time during pregnancy", "Trimester 1", "Antinauseants", "0-12 months before LMP", "Any time during pregnancy", "Trimester 1", "Preventive treatment", "0-12 months before LMP", "Any time during pregnancy", "Trimester 1", "Metoprolol/propranolol", "0-12 months before LMP", "Any time during pregnancy", "Trimester 1", "Amitriptyline", "0-12 months before LMP", "Any time during pregnancy", "Trimester 1", "Candesartan/lisinopril/verapamil", "0-12 months before LMP", "Any time during pregnancy", "Trimester 1", "Topiramate/valproic acid", "0-12 months before LMP", "Any time during pregnancy", "Trimester 1", "Botulinum toxin", "0-12 months before LMP", "Any time during pregnancy", "Trimester 1", "Pizotifen/clonidine", "0-12 months before LMP", "Any time during pregnancy", "Trimester 1", "CGRP antagonists", "0-12 months before LMP", "Any time during pregnancy", "Trimester 1", "Erenumab", "0-12 months before LMP", "Any time during pregnancy", "Trimester 1", "Galkanezumab", "0-12 months before LMP", "Any time during pregnancy", "Trimester 1", "Fremanezumab", "0-12 months before LMP", "Any time during pregnancy", "Trimester 1"),
  N_pregnancies = c("", denom_all, denom_all, denom_T1, "", denom_all, denom_all, denom_T1, "", denom_all, denom_all, denom_T1, "", denom_all, denom_all, denom_T1, "", denom_all, denom_all, denom_T1, "", denom_all, denom_all, denom_T1, "", denom_all, denom_all, denom_T1, "", denom_all, denom_all, denom_T1, "", denom_all, denom_all, denom_T1, "", denom_all, denom_all, denom_T1, "", denom_all, denom_all, denom_T1, "", denom_all, denom_all, denom_T1, "", denom_all, denom_all, denom_T1, "", denom_all, denom_all, denom_T1, "", denom_all, denom_all, denom_T1, "", denom_all, denom_all, denom_T1),
  N_users = c("", acute_prior_n, acute_preg_n, acute_T1_n, "", PCM_prior_n, PCM_preg_n, PCM_T1_n, "", COD_prior_n, COD_preg_n, COD_T1_n, "", NSAID_prior_n, NSAID_preg_n, NSAID_T1_n, "", antinausea_prior_n, antinausea_preg_n, antinausea_T1_n, "", prev_med_prior_n, prev_med_preg_n, prev_med_T1_n, "", meto_prop_prior_n, meto_prop_preg_n, meto_prop_T1_n, "", ami_prior_n, ami_preg_n, ami_T1_n, "", c_l_v_prior_n, c_l_v_preg_n, c_l_v_T1_n, "", top_val_prior_n, top_val_preg_n, top_val_T1_n, "", bot_prior_n, bot_preg_n, bot_T1_n, "", piz_clo_prior_n, piz_clo_preg_n, piz_clo_T1_n, "", cgrp_prior_n, cgrp_preg_n, cgrp_T1_n, "", ere_prior_n, ere_preg_n, ere_T1_n, "", gal_prior_n, gal_preg_n, gal_T1_n, "", fre_prior_n, fre_preg_n, fre_T1_n),
  Prevalence = c("", acute_prior_prev, acute_preg_prev, acute_T1_prev, "", PCM_prior_prev, PCM_preg_prev, PCM_T1_prev, "", COD_prior_prev, COD_preg_prev, COD_T1_prev, "", NSAID_prior_prev, NSAID_preg_prev, NSAID_T1_prev, "", antinausea_prior_prev, antinausea_preg_prev, antinausea_T1_prev, "", prev_med_prior_prev, prev_med_preg_prev, prev_med_T1_prev, "", meto_prop_prior_prev, meto_prop_preg_prev, meto_prop_T1_prev, "", ami_prior_prev, ami_preg_prev, ami_T1_prev, "", c_l_v_prior_prev, c_l_v_preg_prev, c_l_v_T1_prev, "", top_val_prior_prev, top_val_preg_prev, top_val_T1_prev, "", bot_prior_prev, bot_preg_prev, bot_T1_prev, "", piz_clo_prior_prev, piz_clo_preg_prev, piz_clo_T1_prev, "", cgrp_prior_prev, cgrp_preg_prev, cgrp_T1_prev, "", ere_prior_prev, ere_preg_prev, ere_T1_prev, "", gal_prior_prev, gal_preg_prev, gal_T1_prev, "", fre_prior_prev, fre_preg_prev, fre_T1_prev),
  CI_lower = c("", acute_prior_low_CI, acute_preg_low_CI, acute_T1_low_CI, "", PCM_prior_low_CI, PCM_preg_low_CI, PCM_T1_low_CI, "", COD_prior_low_CI, COD_preg_low_CI, COD_T1_low_CI, "", NSAID_prior_low_CI, NSAID_preg_low_CI, NSAID_T1_low_CI, "", antinausea_prior_low_CI, antinausea_preg_low_CI, antinausea_T1_low_CI, "", prev_med_prior_low_CI, prev_med_preg_low_CI, prev_med_T1_low_CI, "", meto_prop_prior_low_CI, meto_prop_preg_low_CI, meto_prop_T1_low_CI, "", ami_prior_low_CI, ami_preg_low_CI, ami_T1_low_CI, "", c_l_v_prior_low_CI, c_l_v_preg_low_CI, c_l_v_T1_low_CI, "", top_val_prior_low_CI, top_val_preg_low_CI, top_val_T1_low_CI, "", bot_prior_low_CI, bot_preg_low_CI, bot_T1_low_CI, "", piz_clo_prior_low_CI, piz_clo_preg_low_CI, piz_clo_T1_low_CI, "", cgrp_prior_low_CI, cgrp_preg_low_CI, cgrp_T1_low_CI, "", ere_prior_low_CI, ere_preg_low_CI, ere_T1_low_CI, "", gal_prior_low_CI, gal_preg_low_CI, gal_T1_low_CI, "", fre_prior_low_CI, fre_preg_low_CI, fre_T1_low_CI),
  CI_upper = c("", acute_prior_up_CI, acute_preg_up_CI, acute_T1_up_CI, "", PCM_prior_up_CI, PCM_preg_up_CI, PCM_T1_up_CI, "", COD_prior_up_CI, COD_preg_up_CI, COD_T1_up_CI, "", NSAID_prior_up_CI, NSAID_preg_up_CI, NSAID_T1_up_CI, "", antinausea_prior_up_CI, antinausea_preg_up_CI, antinausea_T1_up_CI, "", prev_med_prior_up_CI, prev_med_preg_up_CI, prev_med_T1_up_CI, "", meto_prop_prior_up_CI, meto_prop_preg_up_CI, meto_prop_T1_up_CI, "", ami_prior_up_CI, ami_preg_up_CI, ami_T1_up_CI, "", c_l_v_prior_up_CI, c_l_v_preg_up_CI, c_l_v_T1_up_CI, "", top_val_prior_up_CI, top_val_preg_up_CI, top_val_T1_up_CI, "", bot_prior_up_CI, bot_preg_up_CI, bot_T1_up_CI, "", piz_clo_prior_up_CI, piz_clo_preg_up_CI, piz_clo_T1_up_CI, "", cgrp_prior_up_CI, cgrp_preg_up_CI, cgrp_T1_up_CI, "", ere_prior_up_CI, ere_preg_up_CI, ere_T1_up_CI, "", gal_prior_up_CI, gal_preg_up_CI, gal_T1_up_CI, "", fre_prior_up_CI, fre_preg_up_CI, fre_T1_up_CI)
)

write.csv(table4_continuer, file.path(csv_path, "Table4_continuer.csv"), row.names = FALSE)  ##Path to output folder

# rm(list = ls.str(mode = "numeric"))
# rm(list = ls()[! ls() %in% c("dep", "diab", "DUS_all", "HT", "obesity", "replace_na", "scoreci")])

#Calculate cell counts for table 4 - discontinuers only
DUS_discontinuer <- DUS_all[DUS_all$trip_group == "Discontinuer", ]
denom_all <- length(unique(DUS_discontinuer$pregnancy_id))
denom_T1 <- nrow(DUS_discontinuer[DUS_discontinuer$denom_T1 == 1, ])
acute_prior_n <- nrow(DUS_discontinuer[DUS_discontinuer$acute_prior == 1, ])
PCM_prior_n <- nrow(DUS_discontinuer[DUS_discontinuer$PCM_prior == 1, ])
COD_prior_n <- nrow(DUS_discontinuer[DUS_discontinuer$COD_prior == 1, ])
NSAID_prior_n <- nrow(DUS_discontinuer[DUS_discontinuer$NSAID_prior == 1, ])
antinausea_prior_n <- nrow(DUS_discontinuer[DUS_discontinuer$antinausea_prior == 1, ])
prev_med_prior_n <- nrow(DUS_discontinuer[DUS_discontinuer$prev_med_prior == 1, ])
meto_prop_prior_n <- nrow(DUS_discontinuer[DUS_discontinuer$meto_prop_prior == 1, ])
ami_prior_n <- nrow(DUS_discontinuer[DUS_discontinuer$ami_prior == 1, ])
c_l_v_prior_n <- nrow(DUS_discontinuer[DUS_discontinuer$c_l_v_prior == 1, ])
top_val_prior_n <- nrow(DUS_discontinuer[DUS_discontinuer$top_val_prior == 1, ])
bot_prior_n <- nrow(DUS_discontinuer[DUS_discontinuer$bot_prior == 1, ])
piz_clo_prior_n <- nrow(DUS_discontinuer[DUS_discontinuer$piz_clo_prior == 1, ])
cgrp_prior_n <- nrow(DUS_discontinuer[DUS_discontinuer$cgrp_prior == 1, ])
ere_prior_n <- nrow(DUS_discontinuer[DUS_discontinuer$ere_prior == 1, ])
gal_prior_n <- nrow(DUS_discontinuer[DUS_discontinuer$gal_prior == 1, ])
fre_prior_n <- nrow(DUS_discontinuer[DUS_discontinuer$fre_prior == 1, ])

acute_prior_prev <- round((acute_prior_n / denom_all)*100, 3)
PCM_prior_prev <- round((PCM_prior_n / denom_all)*100, 3)
COD_prior_prev <- round((COD_prior_n / denom_all)*100, 3)
NSAID_prior_prev <- round((NSAID_prior_n / denom_all)*100, 3)
antinausea_prior_prev <- round((antinausea_prior_n / denom_all)*100, 3)
prev_med_prior_prev <- round((prev_med_prior_n / denom_all)*100, 3)
meto_prop_prior_prev <- round((meto_prop_prior_n / denom_all)*100, 3)
ami_prior_prev <- round((ami_prior_n / denom_all)*100, 3)
c_l_v_prior_prev <- round((c_l_v_prior_n / denom_all)*100, 3)
top_val_prior_prev <- round((top_val_prior_n / denom_all)*100, 3)
bot_prior_prev <- round((bot_prior_n / denom_all)*100, 3)
piz_clo_prior_prev <- round((piz_clo_prior_n / denom_all)*100, 3)
cgrp_prior_prev <- round((cgrp_prior_n / denom_all)*100, 3)
ere_prior_prev <- round((ere_prior_n / denom_all)*100, 3)
gal_prior_prev <- round((gal_prior_n / denom_all)*100, 3)
fre_prior_prev <- round((fre_prior_n / denom_all)*100, 3)

acute_prior <- scoreci(acute_prior_n, denom_all)
PCM_prior <- scoreci(PCM_prior_n, denom_all)
COD_prior <- scoreci(COD_prior_n, denom_all)
NSAID_prior <- scoreci(NSAID_prior_n, denom_all)
antinausea_prior <- scoreci(antinausea_prior_n, denom_all)
prev_med_prior <- scoreci(prev_med_prior_n, denom_all)
meto_prop_prior <- scoreci(meto_prop_prior_n, denom_all)
ami_prior <- scoreci(ami_prior_n, denom_all)
c_l_v_prior <- scoreci(c_l_v_prior_n, denom_all)
top_val_prior <- scoreci(top_val_prior_n, denom_all)
bot_prior <- scoreci(bot_prior_n, denom_all)
piz_clo_prior <- scoreci(piz_clo_prior_n, denom_all)
cgrp_prior <- scoreci(cgrp_prior_n, denom_all)
ere_prior <- scoreci(ere_prior_n, denom_all)
gal_prior <- scoreci(gal_prior_n, denom_all)
fre_prior <- scoreci(fre_prior_n, denom_all)

acute_prior_low_CI <- round((acute_prior$conf.low)*100, 3)
acute_prior_up_CI <- round((acute_prior$conf.high)*100, 3)
PCM_prior_low_CI <- round((PCM_prior$conf.low)*100, 3)
PCM_prior_up_CI <- round((PCM_prior$conf.high)*100, 3)
COD_prior_low_CI <- round((COD_prior$conf.low)*100, 3)
COD_prior_up_CI <- round((COD_prior$conf.high)*100, 3)
NSAID_prior_low_CI <- round((NSAID_prior$conf.low)*100, 3)
NSAID_prior_up_CI <- round((NSAID_prior$conf.high)*100, 3)
antinausea_prior_low_CI <- round((antinausea_prior$conf.low)*100, 3)
antinausea_prior_up_CI <- round((antinausea_prior$conf.high)*100, 3)
prev_med_prior_low_CI <- round((prev_med_prior$conf.low)*100, 3)
prev_med_prior_up_CI <- round((prev_med_prior$conf.high)*100, 3)
meto_prop_prior_low_CI <- round((meto_prop_prior$conf.low)*100, 3)
meto_prop_prior_up_CI <- round((meto_prop_prior$conf.high)*100, 3)
ami_prior_low_CI <- round((ami_prior$conf.low)*100, 3)
ami_prior_up_CI <- round((ami_prior$conf.high)*100, 3)
c_l_v_prior_low_CI <- round((c_l_v_prior$conf.low)*100, 3)
c_l_v_prior_up_CI <- round((c_l_v_prior$conf.high)*100, 3)
top_val_prior_low_CI <- round((top_val_prior$conf.low)*100, 3)
top_val_prior_up_CI <- round((top_val_prior$conf.high)*100, 3)
bot_prior_low_CI <- round((bot_prior$conf.low)*100, 3)
bot_prior_up_CI <- round((bot_prior$conf.high)*100, 3)
piz_clo_prior_low_CI <- round((piz_clo_prior$conf.low)*100, 3)
piz_clo_prior_up_CI <- round((piz_clo_prior$conf.high)*100, 3)
cgrp_prior_low_CI <- round((cgrp_prior$conf.low)*100, 3)
cgrp_prior_up_CI <- round((cgrp_prior$conf.high)*100, 3)
ere_prior_low_CI <- round((ere_prior$conf.low)*100, 3)
ere_prior_up_CI <- round((ere_prior$conf.high)*100, 3)
gal_prior_low_CI <- round((gal_prior$conf.low)*100, 3)
gal_prior_up_CI <- round((gal_prior$conf.high)*100, 3)
fre_prior_low_CI <- round((fre_prior$conf.low)*100, 3)
fre_prior_up_CI <- round((fre_prior$conf.high)*100, 3)

acute_preg_n <- nrow(DUS_discontinuer[DUS_discontinuer$acute_preg == 1, ])
PCM_preg_n <- nrow(DUS_discontinuer[DUS_discontinuer$PCM_preg == 1, ])
COD_preg_n <- nrow(DUS_discontinuer[DUS_discontinuer$COD_preg == 1, ])
NSAID_preg_n <- nrow(DUS_discontinuer[DUS_discontinuer$NSAID_preg == 1, ])
antinausea_preg_n <- nrow(DUS_discontinuer[DUS_discontinuer$antinausea_preg == 1, ])
prev_med_preg_n <- nrow(DUS_discontinuer[DUS_discontinuer$prev_med_preg == 1, ])
meto_prop_preg_n <- nrow(DUS_discontinuer[DUS_discontinuer$meto_prop_preg == 1, ])
ami_preg_n <- nrow(DUS_discontinuer[DUS_discontinuer$ami_preg == 1, ])
c_l_v_preg_n <- nrow(DUS_discontinuer[DUS_discontinuer$c_l_v_preg == 1, ])
top_val_preg_n <- nrow(DUS_discontinuer[DUS_discontinuer$top_val_preg == 1, ])
bot_preg_n <- nrow(DUS_discontinuer[DUS_discontinuer$bot_preg == 1, ])
piz_clo_preg_n <- nrow(DUS_discontinuer[DUS_discontinuer$piz_clo_preg == 1, ])
cgrp_preg_n <- nrow(DUS_discontinuer[DUS_discontinuer$cgrp_preg == 1, ])
ere_preg_n <- nrow(DUS_discontinuer[DUS_discontinuer$ere_preg == 1, ])
gal_preg_n <- nrow(DUS_discontinuer[DUS_discontinuer$gal_preg == 1, ])
fre_preg_n <- nrow(DUS_discontinuer[DUS_discontinuer$fre_preg == 1, ])

acute_preg_prev <- round((acute_preg_n / denom_all)*100, 3)
PCM_preg_prev <- round((PCM_preg_n / denom_all)*100, 3)
COD_preg_prev <- round((COD_preg_n / denom_all)*100, 3)
NSAID_preg_prev <- round((NSAID_preg_n / denom_all)*100, 3)
antinausea_preg_prev <- round((antinausea_preg_n / denom_all)*100, 3)
prev_med_preg_prev <- round((prev_med_preg_n / denom_all)*100, 3)
meto_prop_preg_prev <- round((meto_prop_preg_n / denom_all)*100, 3)
ami_preg_prev <- round((ami_preg_n / denom_all)*100, 3)
c_l_v_preg_prev <- round((c_l_v_preg_n / denom_all)*100, 3)
top_val_preg_prev <- round((top_val_preg_n / denom_all)*100, 3)
bot_preg_prev <- round((bot_preg_n / denom_all)*100, 3)
piz_clo_preg_prev <- round((piz_clo_preg_n / denom_all)*100, 3)
cgrp_preg_prev <- round((cgrp_preg_n / denom_all)*100, 3)
ere_preg_prev <- round((ere_preg_n / denom_all)*100, 3)
gal_preg_prev <- round((gal_preg_n / denom_all)*100, 3)
fre_preg_prev <- round((fre_preg_n / denom_all)*100, 3)

acute_preg <- scoreci(acute_preg_n, denom_all)
PCM_preg <- scoreci(PCM_preg_n, denom_all)
COD_preg <- scoreci(COD_preg_n, denom_all)
NSAID_preg <- scoreci(NSAID_preg_n, denom_all)
antinausea_preg <- scoreci(antinausea_preg_n, denom_all)
prev_med_preg <- scoreci(prev_med_preg_n, denom_all)
meto_prop_preg <- scoreci(meto_prop_preg_n, denom_all)
ami_preg <- scoreci(ami_preg_n, denom_all)
c_l_v_preg <- scoreci(c_l_v_preg_n, denom_all)
top_val_preg <- scoreci(top_val_preg_n, denom_all)
bot_preg <- scoreci(bot_preg_n, denom_all)
piz_clo_preg <- scoreci(piz_clo_preg_n, denom_all)
cgrp_preg <- scoreci(cgrp_preg_n, denom_all)
ere_preg <- scoreci(ere_preg_n, denom_all)
gal_preg <- scoreci(gal_preg_n, denom_all)
fre_preg <- scoreci(fre_preg_n, denom_all)

acute_preg_low_CI <- round((acute_preg$conf.low)*100, 3)
acute_preg_up_CI <- round((acute_preg$conf.high)*100, 3)
PCM_preg_low_CI <- round((PCM_preg$conf.low)*100, 3)
PCM_preg_up_CI <- round((PCM_preg$conf.high)*100, 3)
COD_preg_low_CI <- round((COD_preg$conf.low)*100, 3)
COD_preg_up_CI <- round((COD_preg$conf.high)*100, 3)
NSAID_preg_low_CI <- round((NSAID_preg$conf.low)*100, 3)
NSAID_preg_up_CI <- round((NSAID_preg$conf.high)*100, 3)
antinausea_preg_low_CI <- round((antinausea_preg$conf.low)*100, 3)
antinausea_preg_up_CI <- round((antinausea_preg$conf.high)*100, 3)
prev_med_preg_low_CI <- round((prev_med_preg$conf.low)*100, 3)
prev_med_preg_up_CI <- round((prev_med_preg$conf.high)*100, 3)
meto_prop_preg_low_CI <- round((meto_prop_preg$conf.low)*100, 3)
meto_prop_preg_up_CI <- round((meto_prop_preg$conf.high)*100, 3)
ami_preg_low_CI <- round((ami_preg$conf.low)*100, 3)
ami_preg_up_CI <- round((ami_preg$conf.high)*100, 3)
c_l_v_preg_low_CI <- round((c_l_v_preg$conf.low)*100, 3)
c_l_v_preg_up_CI <- round((c_l_v_preg$conf.high)*100, 3)
top_val_preg_low_CI <- round((top_val_preg$conf.low)*100, 3)
top_val_preg_up_CI <- round((top_val_preg$conf.high)*100, 3)
bot_preg_low_CI <- round((bot_preg$conf.low)*100, 3)
bot_preg_up_CI <- round((bot_preg$conf.high)*100, 3)
piz_clo_preg_low_CI <- round((piz_clo_preg$conf.low)*100, 3)
piz_clo_preg_up_CI <- round((piz_clo_preg$conf.high)*100, 3)
cgrp_preg_low_CI <- round((cgrp_preg$conf.low)*100, 3)
cgrp_preg_up_CI <- round((cgrp_preg$conf.high)*100, 3)
ere_preg_low_CI <- round((ere_preg$conf.low)*100, 3)
ere_preg_up_CI <- round((ere_preg$conf.high)*100, 3)
gal_preg_low_CI <- round((gal_preg$conf.low)*100, 3)
gal_preg_up_CI <- round((gal_preg$conf.high)*100, 3)
fre_preg_low_CI <- round((fre_preg$conf.low)*100, 3)
fre_preg_up_CI <- round((fre_preg$conf.high)*100, 3)

acute_T1_n <- nrow(DUS_discontinuer[DUS_discontinuer$acute_T1 == 1, ])
PCM_T1_n <- nrow(DUS_discontinuer[DUS_discontinuer$PCM_T1 == 1, ])
COD_T1_n <- nrow(DUS_discontinuer[DUS_discontinuer$COD_T1 == 1, ])
NSAID_T1_n <- nrow(DUS_discontinuer[DUS_discontinuer$NSAID_T1 == 1, ])
antinausea_T1_n <- nrow(DUS_discontinuer[DUS_discontinuer$antinausea_T1 == 1, ])
prev_med_T1_n <- nrow(DUS_discontinuer[DUS_discontinuer$prev_med_T1 == 1, ])
meto_prop_T1_n <- nrow(DUS_discontinuer[DUS_discontinuer$meto_prop_T1 == 1, ])
ami_T1_n <- nrow(DUS_discontinuer[DUS_discontinuer$ami_T1 == 1, ])
c_l_v_T1_n <- nrow(DUS_discontinuer[DUS_discontinuer$c_l_v_T1 == 1, ])
top_val_T1_n <- nrow(DUS_discontinuer[DUS_discontinuer$top_val_T1 == 1, ])
bot_T1_n <- nrow(DUS_discontinuer[DUS_discontinuer$bot_T1 == 1, ])
piz_clo_T1_n <- nrow(DUS_discontinuer[DUS_discontinuer$piz_clo_T1 == 1, ])
cgrp_T1_n <- nrow(DUS_discontinuer[DUS_discontinuer$cgrp_T1 == 1, ])
ere_T1_n <- nrow(DUS_discontinuer[DUS_discontinuer$ere_T1 == 1, ])
gal_T1_n <- nrow(DUS_discontinuer[DUS_discontinuer$gal_T1 == 1, ])
fre_T1_n <- nrow(DUS_discontinuer[DUS_discontinuer$fre_T1 == 1, ])

acute_T1_prev <- round((acute_T1_n / denom_T1)*100, 3)
PCM_T1_prev <- round((PCM_T1_n / denom_T1)*100, 3)
COD_T1_prev <- round((COD_T1_n / denom_T1)*100, 3)
NSAID_T1_prev <- round((NSAID_T1_n / denom_T1)*100, 3)
antinausea_T1_prev <- round((antinausea_T1_n / denom_T1)*100, 3)
prev_med_T1_prev <- round((prev_med_T1_n / denom_T1)*100, 3)
meto_prop_T1_prev <- round((meto_prop_T1_n / denom_T1)*100, 3)
ami_T1_prev <- round((ami_T1_n / denom_T1)*100, 3)
c_l_v_T1_prev <- round((c_l_v_T1_n / denom_T1)*100, 3)
top_val_T1_prev <- round((top_val_T1_n / denom_T1)*100, 3)
bot_T1_prev <- round((bot_T1_n / denom_T1)*100, 3)
piz_clo_T1_prev <- round((piz_clo_T1_n / denom_T1)*100, 3)
cgrp_T1_prev <- round((cgrp_T1_n / denom_T1)*100, 3)
ere_T1_prev <- round((ere_T1_n / denom_T1)*100, 3)
gal_T1_prev <- round((gal_T1_n / denom_T1)*100, 3)
fre_T1_prev <- round((fre_T1_n / denom_T1)*100, 3)

acute_T1 <- scoreci(acute_T1_n, denom_T1)
PCM_T1 <- scoreci(PCM_T1_n, denom_T1)
COD_T1 <- scoreci(COD_T1_n, denom_T1)
NSAID_T1 <- scoreci(NSAID_T1_n, denom_T1)
antinausea_T1 <- scoreci(antinausea_T1_n, denom_T1)
prev_med_T1 <- scoreci(prev_med_T1_n, denom_T1)
meto_prop_T1 <- scoreci(meto_prop_T1_n, denom_T1)
ami_T1 <- scoreci(ami_T1_n, denom_T1)
c_l_v_T1 <- scoreci(c_l_v_T1_n, denom_T1)
top_val_T1 <- scoreci(top_val_T1_n, denom_T1)
bot_T1 <- scoreci(bot_T1_n, denom_T1)
piz_clo_T1 <- scoreci(piz_clo_T1_n, denom_T1)
cgrp_T1 <- scoreci(cgrp_T1_n, denom_T1)
ere_T1 <- scoreci(ere_T1_n, denom_T1)
gal_T1 <- scoreci(gal_T1_n, denom_T1)
fre_T1 <- scoreci(fre_T1_n, denom_T1)

acute_T1_low_CI <- round((acute_T1$conf.low)*100, 3)
acute_T1_up_CI <- round((acute_T1$conf.high)*100, 3)
PCM_T1_low_CI <- round((PCM_T1$conf.low)*100, 3)
PCM_T1_up_CI <- round((PCM_T1$conf.high)*100, 3)
COD_T1_low_CI <- round((COD_T1$conf.low)*100, 3)
COD_T1_up_CI <- round((COD_T1$conf.high)*100, 3)
NSAID_T1_low_CI <- round((NSAID_T1$conf.low)*100, 3)
NSAID_T1_up_CI <- round((NSAID_T1$conf.high)*100, 3)
antinausea_T1_low_CI <- round((antinausea_T1$conf.low)*100, 3)
antinausea_T1_up_CI <- round((antinausea_T1$conf.high)*100, 3)
prev_med_T1_low_CI <- round((prev_med_T1$conf.low)*100, 3)
prev_med_T1_up_CI <- round((prev_med_T1$conf.high)*100, 3)
meto_prop_T1_low_CI <- round((meto_prop_T1$conf.low)*100, 3)
meto_prop_T1_up_CI <- round((meto_prop_T1$conf.high)*100, 3)
ami_T1_low_CI <- round((ami_T1$conf.low)*100, 3)
ami_T1_up_CI <- round((ami_T1$conf.high)*100, 3)
c_l_v_T1_low_CI <- round((c_l_v_T1$conf.low)*100, 3)
c_l_v_T1_up_CI <- round((c_l_v_T1$conf.high)*100, 3)
top_val_T1_low_CI <- round((top_val_T1$conf.low)*100, 3)
top_val_T1_up_CI <- round((top_val_T1$conf.high)*100, 3)
bot_T1_low_CI <- round((bot_T1$conf.low)*100, 3)
bot_T1_up_CI <- round((bot_T1$conf.high)*100, 3)
piz_clo_T1_low_CI <- round((piz_clo_T1$conf.low)*100, 3)
piz_clo_T1_up_CI <- round((piz_clo_T1$conf.high)*100, 3)
cgrp_T1_low_CI <- round((cgrp_T1$conf.low)*100, 3)
cgrp_T1_up_CI <- round((cgrp_T1$conf.high)*100, 3)
ere_T1_low_CI <- round((ere_T1$conf.low)*100, 3)
ere_T1_up_CI <- round((ere_T1$conf.high)*100, 3)
gal_T1_low_CI <- round((gal_T1$conf.low)*100, 3)
gal_T1_up_CI <- round((gal_T1$conf.high)*100, 3)
fre_T1_low_CI <- round((fre_T1$conf.low)*100, 3)
fre_T1_up_CI <- round((fre_T1$conf.high)*100, 3)

table4_discontinuer <- data.frame(
  Time_period = c("Acute migraine treatment", "0-12 months before LMP", "Any time during pregnancy", "Trimester 1", "Paracetamol", "0-12 months before LMP", "Any time during pregnancy", "Trimester 1", "Codeine and paracetamol", "0-12 months before LMP", "Any time during pregnancy", "Trimester 1", "NSAIDs", "0-12 months before LMP", "Any time during pregnancy", "Trimester 1", "Antinauseants", "0-12 months before LMP", "Any time during pregnancy", "Trimester 1", "Preventive treatment", "0-12 months before LMP", "Any time during pregnancy", "Trimester 1", "Metoprolol/propranolol", "0-12 months before LMP", "Any time during pregnancy", "Trimester 1", "Amitriptyline", "0-12 months before LMP", "Any time during pregnancy", "Trimester 1", "Candesartan/lisinopril/verapamil", "0-12 months before LMP", "Any time during pregnancy", "Trimester 1", "Topiramate/valproic acid", "0-12 months before LMP", "Any time during pregnancy", "Trimester 1", "Botulinum toxin", "0-12 months before LMP", "Any time during pregnancy", "Trimester 1", "Pizotifen/clonidine", "0-12 months before LMP", "Any time during pregnancy", "Trimester 1", "CGRP antagonists", "0-12 months before LMP", "Any time during pregnancy", "Trimester 1", "Erenumab", "0-12 months before LMP", "Any time during pregnancy", "Trimester 1", "Galkanezumab", "0-12 months before LMP", "Any time during pregnancy", "Trimester 1", "Fremanezumab", "0-12 months before LMP", "Any time during pregnancy", "Trimester 1"),
  N_pregnancies = c("", denom_all, denom_all, denom_T1, "", denom_all, denom_all, denom_T1, "", denom_all, denom_all, denom_T1, "", denom_all, denom_all, denom_T1, "", denom_all, denom_all, denom_T1, "", denom_all, denom_all, denom_T1, "", denom_all, denom_all, denom_T1, "", denom_all, denom_all, denom_T1, "", denom_all, denom_all, denom_T1, "", denom_all, denom_all, denom_T1, "", denom_all, denom_all, denom_T1, "", denom_all, denom_all, denom_T1, "", denom_all, denom_all, denom_T1, "", denom_all, denom_all, denom_T1, "", denom_all, denom_all, denom_T1, "", denom_all, denom_all, denom_T1),
  N_users = c("", acute_prior_n, acute_preg_n, acute_T1_n, "", PCM_prior_n, PCM_preg_n, PCM_T1_n, "", COD_prior_n, COD_preg_n, COD_T1_n, "", NSAID_prior_n, NSAID_preg_n, NSAID_T1_n, "", antinausea_prior_n, antinausea_preg_n, antinausea_T1_n, "", prev_med_prior_n, prev_med_preg_n, prev_med_T1_n, "", meto_prop_prior_n, meto_prop_preg_n, meto_prop_T1_n, "", ami_prior_n, ami_preg_n, ami_T1_n, "", c_l_v_prior_n, c_l_v_preg_n, c_l_v_T1_n, "", top_val_prior_n, top_val_preg_n, top_val_T1_n, "", bot_prior_n, bot_preg_n, bot_T1_n, "", piz_clo_prior_n, piz_clo_preg_n, piz_clo_T1_n, "", cgrp_prior_n, cgrp_preg_n, cgrp_T1_n, "", ere_prior_n, ere_preg_n, ere_T1_n, "", gal_prior_n, gal_preg_n, gal_T1_n, "", fre_prior_n, fre_preg_n, fre_T1_n),
  Prevalence = c("", acute_prior_prev, acute_preg_prev, acute_T1_prev, "", PCM_prior_prev, PCM_preg_prev, PCM_T1_prev, "", COD_prior_prev, COD_preg_prev, COD_T1_prev, "", NSAID_prior_prev, NSAID_preg_prev, NSAID_T1_prev, "", antinausea_prior_prev, antinausea_preg_prev, antinausea_T1_prev, "", prev_med_prior_prev, prev_med_preg_prev, prev_med_T1_prev, "", meto_prop_prior_prev, meto_prop_preg_prev, meto_prop_T1_prev, "", ami_prior_prev, ami_preg_prev, ami_T1_prev, "", c_l_v_prior_prev, c_l_v_preg_prev, c_l_v_T1_prev, "", top_val_prior_prev, top_val_preg_prev, top_val_T1_prev, "", bot_prior_prev, bot_preg_prev, bot_T1_prev, "", piz_clo_prior_prev, piz_clo_preg_prev, piz_clo_T1_prev, "", cgrp_prior_prev, cgrp_preg_prev, cgrp_T1_prev, "", ere_prior_prev, ere_preg_prev, ere_T1_prev, "", gal_prior_prev, gal_preg_prev, gal_T1_prev, "", fre_prior_prev, fre_preg_prev, fre_T1_prev),
  CI_lower = c("", acute_prior_low_CI, acute_preg_low_CI, acute_T1_low_CI, "", PCM_prior_low_CI, PCM_preg_low_CI, PCM_T1_low_CI, "", COD_prior_low_CI, COD_preg_low_CI, COD_T1_low_CI, "", NSAID_prior_low_CI, NSAID_preg_low_CI, NSAID_T1_low_CI, "", antinausea_prior_low_CI, antinausea_preg_low_CI, antinausea_T1_low_CI, "", prev_med_prior_low_CI, prev_med_preg_low_CI, prev_med_T1_low_CI, "", meto_prop_prior_low_CI, meto_prop_preg_low_CI, meto_prop_T1_low_CI, "", ami_prior_low_CI, ami_preg_low_CI, ami_T1_low_CI, "", c_l_v_prior_low_CI, c_l_v_preg_low_CI, c_l_v_T1_low_CI, "", top_val_prior_low_CI, top_val_preg_low_CI, top_val_T1_low_CI, "", bot_prior_low_CI, bot_preg_low_CI, bot_T1_low_CI, "", piz_clo_prior_low_CI, piz_clo_preg_low_CI, piz_clo_T1_low_CI, "", cgrp_prior_low_CI, cgrp_preg_low_CI, cgrp_T1_low_CI, "", ere_prior_low_CI, ere_preg_low_CI, ere_T1_low_CI, "", gal_prior_low_CI, gal_preg_low_CI, gal_T1_low_CI, "", fre_prior_low_CI, fre_preg_low_CI, fre_T1_low_CI),
  CI_upper = c("", acute_prior_up_CI, acute_preg_up_CI, acute_T1_up_CI, "", PCM_prior_up_CI, PCM_preg_up_CI, PCM_T1_up_CI, "", COD_prior_up_CI, COD_preg_up_CI, COD_T1_up_CI, "", NSAID_prior_up_CI, NSAID_preg_up_CI, NSAID_T1_up_CI, "", antinausea_prior_up_CI, antinausea_preg_up_CI, antinausea_T1_up_CI, "", prev_med_prior_up_CI, prev_med_preg_up_CI, prev_med_T1_up_CI, "", meto_prop_prior_up_CI, meto_prop_preg_up_CI, meto_prop_T1_up_CI, "", ami_prior_up_CI, ami_preg_up_CI, ami_T1_up_CI, "", c_l_v_prior_up_CI, c_l_v_preg_up_CI, c_l_v_T1_up_CI, "", top_val_prior_up_CI, top_val_preg_up_CI, top_val_T1_up_CI, "", bot_prior_up_CI, bot_preg_up_CI, bot_T1_up_CI, "", piz_clo_prior_up_CI, piz_clo_preg_up_CI, piz_clo_T1_up_CI, "", cgrp_prior_up_CI, cgrp_preg_up_CI, cgrp_T1_up_CI, "", ere_prior_up_CI, ere_preg_up_CI, ere_T1_up_CI, "", gal_prior_up_CI, gal_preg_up_CI, gal_T1_up_CI, "", fre_prior_up_CI, fre_preg_up_CI, fre_T1_up_CI)
)

write.csv(table4_discontinuer, file.path(csv_path, "Table4_discontinuer.csv"), row.names = FALSE)  ##Path to output folder

# rm(list = ls.str(mode = "numeric"))
# rm(list = ls()[! ls() %in% c("dep", "diab", "DUS_all", "HT", "obesity", "replace_na", "scoreci")])

#Calculate cell counts for table 4 - non-users only
DUS_nonuser <- DUS_all[DUS_all$trip_group == "Non-user", ]
denom_all <- length(unique(DUS_nonuser$pregnancy_id))
denom_T1 <- nrow(DUS_nonuser[DUS_nonuser$denom_T1 == 1, ])
acute_prior_n <- nrow(DUS_nonuser[DUS_nonuser$acute_prior == 1, ])
PCM_prior_n <- nrow(DUS_nonuser[DUS_nonuser$PCM_prior == 1, ])
COD_prior_n <- nrow(DUS_nonuser[DUS_nonuser$COD_prior == 1, ])
NSAID_prior_n <- nrow(DUS_nonuser[DUS_nonuser$NSAID_prior == 1, ])
antinausea_prior_n <- nrow(DUS_nonuser[DUS_nonuser$antinausea_prior == 1, ])
prev_med_prior_n <- nrow(DUS_nonuser[DUS_nonuser$prev_med_prior == 1, ])
meto_prop_prior_n <- nrow(DUS_nonuser[DUS_nonuser$meto_prop_prior == 1, ])
ami_prior_n <- nrow(DUS_nonuser[DUS_nonuser$ami_prior == 1, ])
c_l_v_prior_n <- nrow(DUS_nonuser[DUS_nonuser$c_l_v_prior == 1, ])
top_val_prior_n <- nrow(DUS_nonuser[DUS_nonuser$top_val_prior == 1, ])
bot_prior_n <- nrow(DUS_nonuser[DUS_nonuser$bot_prior == 1, ])
piz_clo_prior_n <- nrow(DUS_nonuser[DUS_nonuser$piz_clo_prior == 1, ])
cgrp_prior_n <- nrow(DUS_nonuser[DUS_nonuser$cgrp_prior == 1, ])
ere_prior_n <- nrow(DUS_nonuser[DUS_nonuser$ere_prior == 1, ])
gal_prior_n <- nrow(DUS_nonuser[DUS_nonuser$gal_prior == 1, ])
fre_prior_n <- nrow(DUS_nonuser[DUS_nonuser$fre_prior == 1, ])

acute_prior_prev <- round((acute_prior_n / denom_all)*100, 3)
PCM_prior_prev <- round((PCM_prior_n / denom_all)*100, 3)
COD_prior_prev <- round((COD_prior_n / denom_all)*100, 3)
NSAID_prior_prev <- round((NSAID_prior_n / denom_all)*100, 3)
antinausea_prior_prev <- round((antinausea_prior_n / denom_all)*100, 3)
prev_med_prior_prev <- round((prev_med_prior_n / denom_all)*100, 3)
meto_prop_prior_prev <- round((meto_prop_prior_n / denom_all)*100, 3)
ami_prior_prev <- round((ami_prior_n / denom_all)*100, 3)
c_l_v_prior_prev <- round((c_l_v_prior_n / denom_all)*100, 3)
top_val_prior_prev <- round((top_val_prior_n / denom_all)*100, 3)
bot_prior_prev <- round((bot_prior_n / denom_all)*100, 3)
piz_clo_prior_prev <- round((piz_clo_prior_n / denom_all)*100, 3)
cgrp_prior_prev <- round((cgrp_prior_n / denom_all)*100, 3)
ere_prior_prev <- round((ere_prior_n / denom_all)*100, 3)
gal_prior_prev <- round((gal_prior_n / denom_all)*100, 3)
fre_prior_prev <- round((fre_prior_n / denom_all)*100, 3)

acute_prior <- scoreci(acute_prior_n, denom_all)
PCM_prior <- scoreci(PCM_prior_n, denom_all)
COD_prior <- scoreci(COD_prior_n, denom_all)
NSAID_prior <- scoreci(NSAID_prior_n, denom_all)
antinausea_prior <- scoreci(antinausea_prior_n, denom_all)
prev_med_prior <- scoreci(prev_med_prior_n, denom_all)
meto_prop_prior <- scoreci(meto_prop_prior_n, denom_all)
ami_prior <- scoreci(ami_prior_n, denom_all)
c_l_v_prior <- scoreci(c_l_v_prior_n, denom_all)
top_val_prior <- scoreci(top_val_prior_n, denom_all)
bot_prior <- scoreci(bot_prior_n, denom_all)
piz_clo_prior <- scoreci(piz_clo_prior_n, denom_all)
cgrp_prior <- scoreci(cgrp_prior_n, denom_all)
ere_prior <- scoreci(ere_prior_n, denom_all)
gal_prior <- scoreci(gal_prior_n, denom_all)
fre_prior <- scoreci(fre_prior_n, denom_all)

acute_prior_low_CI <- round((acute_prior$conf.low)*100, 3)
acute_prior_up_CI <- round((acute_prior$conf.high)*100, 3)
PCM_prior_low_CI <- round((PCM_prior$conf.low)*100, 3)
PCM_prior_up_CI <- round((PCM_prior$conf.high)*100, 3)
COD_prior_low_CI <- round((COD_prior$conf.low)*100, 3)
COD_prior_up_CI <- round((COD_prior$conf.high)*100, 3)
NSAID_prior_low_CI <- round((NSAID_prior$conf.low)*100, 3)
NSAID_prior_up_CI <- round((NSAID_prior$conf.high)*100, 3)
antinausea_prior_low_CI <- round((antinausea_prior$conf.low)*100, 3)
antinausea_prior_up_CI <- round((antinausea_prior$conf.high)*100, 3)
prev_med_prior_low_CI <- round((prev_med_prior$conf.low)*100, 3)
prev_med_prior_up_CI <- round((prev_med_prior$conf.high)*100, 3)
meto_prop_prior_low_CI <- round((meto_prop_prior$conf.low)*100, 3)
meto_prop_prior_up_CI <- round((meto_prop_prior$conf.high)*100, 3)
ami_prior_low_CI <- round((ami_prior$conf.low)*100, 3)
ami_prior_up_CI <- round((ami_prior$conf.high)*100, 3)
c_l_v_prior_low_CI <- round((c_l_v_prior$conf.low)*100, 3)
c_l_v_prior_up_CI <- round((c_l_v_prior$conf.high)*100, 3)
top_val_prior_low_CI <- round((top_val_prior$conf.low)*100, 3)
top_val_prior_up_CI <- round((top_val_prior$conf.high)*100, 3)
bot_prior_low_CI <- round((bot_prior$conf.low)*100, 3)
bot_prior_up_CI <- round((bot_prior$conf.high)*100, 3)
piz_clo_prior_low_CI <- round((piz_clo_prior$conf.low)*100, 3)
piz_clo_prior_up_CI <- round((piz_clo_prior$conf.high)*100, 3)
cgrp_prior_low_CI <- round((cgrp_prior$conf.low)*100, 3)
cgrp_prior_up_CI <- round((cgrp_prior$conf.high)*100, 3)
ere_prior_low_CI <- round((ere_prior$conf.low)*100, 3)
ere_prior_up_CI <- round((ere_prior$conf.high)*100, 3)
gal_prior_low_CI <- round((gal_prior$conf.low)*100, 3)
gal_prior_up_CI <- round((gal_prior$conf.high)*100, 3)
fre_prior_low_CI <- round((fre_prior$conf.low)*100, 3)
fre_prior_up_CI <- round((fre_prior$conf.high)*100, 3)

acute_preg_n <- nrow(DUS_nonuser[DUS_nonuser$acute_preg == 1, ])
PCM_preg_n <- nrow(DUS_nonuser[DUS_nonuser$PCM_preg == 1, ])
COD_preg_n <- nrow(DUS_nonuser[DUS_nonuser$COD_preg == 1, ])
NSAID_preg_n <- nrow(DUS_nonuser[DUS_nonuser$NSAID_preg == 1, ])
antinausea_preg_n <- nrow(DUS_nonuser[DUS_nonuser$antinausea_preg == 1, ])
prev_med_preg_n <- nrow(DUS_nonuser[DUS_nonuser$prev_med_preg == 1, ])
meto_prop_preg_n <- nrow(DUS_nonuser[DUS_nonuser$meto_prop_preg == 1, ])
ami_preg_n <- nrow(DUS_nonuser[DUS_nonuser$ami_preg == 1, ])
c_l_v_preg_n <- nrow(DUS_nonuser[DUS_nonuser$c_l_v_preg == 1, ])
top_val_preg_n <- nrow(DUS_nonuser[DUS_nonuser$top_val_preg == 1, ])
bot_preg_n <- nrow(DUS_nonuser[DUS_nonuser$bot_preg == 1, ])
piz_clo_preg_n <- nrow(DUS_nonuser[DUS_nonuser$piz_clo_preg == 1, ])
cgrp_preg_n <- nrow(DUS_nonuser[DUS_nonuser$cgrp_preg == 1, ])
ere_preg_n <- nrow(DUS_nonuser[DUS_nonuser$ere_preg == 1, ])
gal_preg_n <- nrow(DUS_nonuser[DUS_nonuser$gal_preg == 1, ])
fre_preg_n <- nrow(DUS_nonuser[DUS_nonuser$fre_preg == 1, ])

acute_preg_prev <- round((acute_preg_n / denom_all)*100, 3)
PCM_preg_prev <- round((PCM_preg_n / denom_all)*100, 3)
COD_preg_prev <- round((COD_preg_n / denom_all)*100, 3)
NSAID_preg_prev <- round((NSAID_preg_n / denom_all)*100, 3)
antinausea_preg_prev <- round((antinausea_preg_n / denom_all)*100, 3)
prev_med_preg_prev <- round((prev_med_preg_n / denom_all)*100, 3)
meto_prop_preg_prev <- round((meto_prop_preg_n / denom_all)*100, 3)
ami_preg_prev <- round((ami_preg_n / denom_all)*100, 3)
c_l_v_preg_prev <- round((c_l_v_preg_n / denom_all)*100, 3)
top_val_preg_prev <- round((top_val_preg_n / denom_all)*100, 3)
bot_preg_prev <- round((bot_preg_n / denom_all)*100, 3)
piz_clo_preg_prev <- round((piz_clo_preg_n / denom_all)*100, 3)
cgrp_preg_prev <- round((cgrp_preg_n / denom_all)*100, 3)
ere_preg_prev <- round((ere_preg_n / denom_all)*100, 3)
gal_preg_prev <- round((gal_preg_n / denom_all)*100, 3)
fre_preg_prev <- round((fre_preg_n / denom_all)*100, 3)

acute_preg <- scoreci(acute_preg_n, denom_all)
PCM_preg <- scoreci(PCM_preg_n, denom_all)
COD_preg <- scoreci(COD_preg_n, denom_all)
NSAID_preg <- scoreci(NSAID_preg_n, denom_all)
antinausea_preg <- scoreci(antinausea_preg_n, denom_all)
prev_med_preg <- scoreci(prev_med_preg_n, denom_all)
meto_prop_preg <- scoreci(meto_prop_preg_n, denom_all)
ami_preg <- scoreci(ami_preg_n, denom_all)
c_l_v_preg <- scoreci(c_l_v_preg_n, denom_all)
top_val_preg <- scoreci(top_val_preg_n, denom_all)
bot_preg <- scoreci(bot_preg_n, denom_all)
piz_clo_preg <- scoreci(piz_clo_preg_n, denom_all)
cgrp_preg <- scoreci(cgrp_preg_n, denom_all)
ere_preg <- scoreci(ere_preg_n, denom_all)
gal_preg <- scoreci(gal_preg_n, denom_all)
fre_preg <- scoreci(fre_preg_n, denom_all)

acute_preg_low_CI <- round((acute_preg$conf.low)*100, 3)
acute_preg_up_CI <- round((acute_preg$conf.high)*100, 3)
PCM_preg_low_CI <- round((PCM_preg$conf.low)*100, 3)
PCM_preg_up_CI <- round((PCM_preg$conf.high)*100, 3)
COD_preg_low_CI <- round((COD_preg$conf.low)*100, 3)
COD_preg_up_CI <- round((COD_preg$conf.high)*100, 3)
NSAID_preg_low_CI <- round((NSAID_preg$conf.low)*100, 3)
NSAID_preg_up_CI <- round((NSAID_preg$conf.high)*100, 3)
antinausea_preg_low_CI <- round((antinausea_preg$conf.low)*100, 3)
antinausea_preg_up_CI <- round((antinausea_preg$conf.high)*100, 3)
prev_med_preg_low_CI <- round((prev_med_preg$conf.low)*100, 3)
prev_med_preg_up_CI <- round((prev_med_preg$conf.high)*100, 3)
meto_prop_preg_low_CI <- round((meto_prop_preg$conf.low)*100, 3)
meto_prop_preg_up_CI <- round((meto_prop_preg$conf.high)*100, 3)
ami_preg_low_CI <- round((ami_preg$conf.low)*100, 3)
ami_preg_up_CI <- round((ami_preg$conf.high)*100, 3)
c_l_v_preg_low_CI <- round((c_l_v_preg$conf.low)*100, 3)
c_l_v_preg_up_CI <- round((c_l_v_preg$conf.high)*100, 3)
top_val_preg_low_CI <- round((top_val_preg$conf.low)*100, 3)
top_val_preg_up_CI <- round((top_val_preg$conf.high)*100, 3)
bot_preg_low_CI <- round((bot_preg$conf.low)*100, 3)
bot_preg_up_CI <- round((bot_preg$conf.high)*100, 3)
piz_clo_preg_low_CI <- round((piz_clo_preg$conf.low)*100, 3)
piz_clo_preg_up_CI <- round((piz_clo_preg$conf.high)*100, 3)
cgrp_preg_low_CI <- round((cgrp_preg$conf.low)*100, 3)
cgrp_preg_up_CI <- round((cgrp_preg$conf.high)*100, 3)
ere_preg_low_CI <- round((ere_preg$conf.low)*100, 3)
ere_preg_up_CI <- round((ere_preg$conf.high)*100, 3)
gal_preg_low_CI <- round((gal_preg$conf.low)*100, 3)
gal_preg_up_CI <- round((gal_preg$conf.high)*100, 3)
fre_preg_low_CI <- round((fre_preg$conf.low)*100, 3)
fre_preg_up_CI <- round((fre_preg$conf.high)*100, 3)

acute_T1_n <- nrow(DUS_nonuser[DUS_nonuser$acute_T1 == 1, ])
PCM_T1_n <- nrow(DUS_nonuser[DUS_nonuser$PCM_T1 == 1, ])
COD_T1_n <- nrow(DUS_nonuser[DUS_nonuser$COD_T1 == 1, ])
NSAID_T1_n <- nrow(DUS_nonuser[DUS_nonuser$NSAID_T1 == 1, ])
antinausea_T1_n <- nrow(DUS_nonuser[DUS_nonuser$antinausea_T1 == 1, ])
prev_med_T1_n <- nrow(DUS_nonuser[DUS_nonuser$prev_med_T1 == 1, ])
meto_prop_T1_n <- nrow(DUS_nonuser[DUS_nonuser$meto_prop_T1 == 1, ])
ami_T1_n <- nrow(DUS_nonuser[DUS_nonuser$ami_T1 == 1, ])
c_l_v_T1_n <- nrow(DUS_nonuser[DUS_nonuser$c_l_v_T1 == 1, ])
top_val_T1_n <- nrow(DUS_nonuser[DUS_nonuser$top_val_T1 == 1, ])
bot_T1_n <- nrow(DUS_nonuser[DUS_nonuser$bot_T1 == 1, ])
piz_clo_T1_n <- nrow(DUS_nonuser[DUS_nonuser$piz_clo_T1 == 1, ])
cgrp_T1_n <- nrow(DUS_nonuser[DUS_nonuser$cgrp_T1 == 1, ])
ere_T1_n <- nrow(DUS_nonuser[DUS_nonuser$ere_T1 == 1, ])
gal_T1_n <- nrow(DUS_nonuser[DUS_nonuser$gal_T1 == 1, ])
fre_T1_n <- nrow(DUS_nonuser[DUS_nonuser$fre_T1 == 1, ])

acute_T1_prev <- round((acute_T1_n / denom_T1)*100, 3)
PCM_T1_prev <- round((PCM_T1_n / denom_T1)*100, 3)
COD_T1_prev <- round((COD_T1_n / denom_T1)*100, 3)
NSAID_T1_prev <- round((NSAID_T1_n / denom_T1)*100, 3)
antinausea_T1_prev <- round((antinausea_T1_n / denom_T1)*100, 3)
prev_med_T1_prev <- round((prev_med_T1_n / denom_T1)*100, 3)
meto_prop_T1_prev <- round((meto_prop_T1_n / denom_T1)*100, 3)
ami_T1_prev <- round((ami_T1_n / denom_T1)*100, 3)
c_l_v_T1_prev <- round((c_l_v_T1_n / denom_T1)*100, 3)
top_val_T1_prev <- round((top_val_T1_n / denom_T1)*100, 3)
bot_T1_prev <- round((bot_T1_n / denom_T1)*100, 3)
piz_clo_T1_prev <- round((piz_clo_T1_n / denom_T1)*100, 3)
cgrp_T1_prev <- round((cgrp_T1_n / denom_T1)*100, 3)
ere_T1_prev <- round((ere_T1_n / denom_T1)*100, 3)
gal_T1_prev <- round((gal_T1_n / denom_T1)*100, 3)
fre_T1_prev <- round((fre_T1_n / denom_T1)*100, 3)

acute_T1 <- scoreci(acute_T1_n, denom_T1)
PCM_T1 <- scoreci(PCM_T1_n, denom_T1)
COD_T1 <- scoreci(COD_T1_n, denom_T1)
NSAID_T1 <- scoreci(NSAID_T1_n, denom_T1)
antinausea_T1 <- scoreci(antinausea_T1_n, denom_T1)
prev_med_T1 <- scoreci(prev_med_T1_n, denom_T1)
meto_prop_T1 <- scoreci(meto_prop_T1_n, denom_T1)
ami_T1 <- scoreci(ami_T1_n, denom_T1)
c_l_v_T1 <- scoreci(c_l_v_T1_n, denom_T1)
top_val_T1 <- scoreci(top_val_T1_n, denom_T1)
bot_T1 <- scoreci(bot_T1_n, denom_T1)
piz_clo_T1 <- scoreci(piz_clo_T1_n, denom_T1)
cgrp_T1 <- scoreci(cgrp_T1_n, denom_T1)
ere_T1 <- scoreci(ere_T1_n, denom_T1)
gal_T1 <- scoreci(gal_T1_n, denom_T1)
fre_T1 <- scoreci(fre_T1_n, denom_T1)

acute_T1_low_CI <- round((acute_T1$conf.low)*100, 3)
acute_T1_up_CI <- round((acute_T1$conf.high)*100, 3)
PCM_T1_low_CI <- round((PCM_T1$conf.low)*100, 3)
PCM_T1_up_CI <- round((PCM_T1$conf.high)*100, 3)
COD_T1_low_CI <- round((COD_T1$conf.low)*100, 3)
COD_T1_up_CI <- round((COD_T1$conf.high)*100, 3)
NSAID_T1_low_CI <- round((NSAID_T1$conf.low)*100, 3)
NSAID_T1_up_CI <- round((NSAID_T1$conf.high)*100, 3)
antinausea_T1_low_CI <- round((antinausea_T1$conf.low)*100, 3)
antinausea_T1_up_CI <- round((antinausea_T1$conf.high)*100, 3)
prev_med_T1_low_CI <- round((prev_med_T1$conf.low)*100, 3)
prev_med_T1_up_CI <- round((prev_med_T1$conf.high)*100, 3)
meto_prop_T1_low_CI <- round((meto_prop_T1$conf.low)*100, 3)
meto_prop_T1_up_CI <- round((meto_prop_T1$conf.high)*100, 3)
ami_T1_low_CI <- round((ami_T1$conf.low)*100, 3)
ami_T1_up_CI <- round((ami_T1$conf.high)*100, 3)
c_l_v_T1_low_CI <- round((c_l_v_T1$conf.low)*100, 3)
c_l_v_T1_up_CI <- round((c_l_v_T1$conf.high)*100, 3)
top_val_T1_low_CI <- round((top_val_T1$conf.low)*100, 3)
top_val_T1_up_CI <- round((top_val_T1$conf.high)*100, 3)
bot_T1_low_CI <- round((bot_T1$conf.low)*100, 3)
bot_T1_up_CI <- round((bot_T1$conf.high)*100, 3)
piz_clo_T1_low_CI <- round((piz_clo_T1$conf.low)*100, 3)
piz_clo_T1_up_CI <- round((piz_clo_T1$conf.high)*100, 3)
cgrp_T1_low_CI <- round((cgrp_T1$conf.low)*100, 3)
cgrp_T1_up_CI <- round((cgrp_T1$conf.high)*100, 3)
ere_T1_low_CI <- round((ere_T1$conf.low)*100, 3)
ere_T1_up_CI <- round((ere_T1$conf.high)*100, 3)
gal_T1_low_CI <- round((gal_T1$conf.low)*100, 3)
gal_T1_up_CI <- round((gal_T1$conf.high)*100, 3)
fre_T1_low_CI <- round((fre_T1$conf.low)*100, 3)
fre_T1_up_CI <- round((fre_T1$conf.high)*100, 3)

table4_nonuser <- data.frame(
  Time_period = c("Acute migraine treatment", "0-12 months before LMP", "Any time during pregnancy", "Trimester 1", "Paracetamol", "0-12 months before LMP", "Any time during pregnancy", "Trimester 1", "Codeine and paracetamol", "0-12 months before LMP", "Any time during pregnancy", "Trimester 1", "NSAIDs", "0-12 months before LMP", "Any time during pregnancy", "Trimester 1", "Antinauseants", "0-12 months before LMP", "Any time during pregnancy", "Trimester 1", "Preventive treatment", "0-12 months before LMP", "Any time during pregnancy", "Trimester 1", "Metoprolol/propranolol", "0-12 months before LMP", "Any time during pregnancy", "Trimester 1", "Amitriptyline", "0-12 months before LMP", "Any time during pregnancy", "Trimester 1", "Candesartan/lisinopril/verapamil", "0-12 months before LMP", "Any time during pregnancy", "Trimester 1", "Topiramate/valproic acid", "0-12 months before LMP", "Any time during pregnancy", "Trimester 1", "Botulinum toxin", "0-12 months before LMP", "Any time during pregnancy", "Trimester 1", "Pizotifen/clonidine", "0-12 months before LMP", "Any time during pregnancy", "Trimester 1", "CGRP antagonists", "0-12 months before LMP", "Any time during pregnancy", "Trimester 1", "Erenumab", "0-12 months before LMP", "Any time during pregnancy", "Trimester 1", "Galkanezumab", "0-12 months before LMP", "Any time during pregnancy", "Trimester 1", "Fremanezumab", "0-12 months before LMP", "Any time during pregnancy", "Trimester 1"),
  N_pregnancies = c("", denom_all, denom_all, denom_T1, "", denom_all, denom_all, denom_T1, "", denom_all, denom_all, denom_T1, "", denom_all, denom_all, denom_T1, "", denom_all, denom_all, denom_T1, "", denom_all, denom_all, denom_T1, "", denom_all, denom_all, denom_T1, "", denom_all, denom_all, denom_T1, "", denom_all, denom_all, denom_T1, "", denom_all, denom_all, denom_T1, "", denom_all, denom_all, denom_T1, "", denom_all, denom_all, denom_T1, "", denom_all, denom_all, denom_T1, "", denom_all, denom_all, denom_T1, "", denom_all, denom_all, denom_T1, "", denom_all, denom_all, denom_T1),
  N_users = c("", acute_prior_n, acute_preg_n, acute_T1_n, "", PCM_prior_n, PCM_preg_n, PCM_T1_n, "", COD_prior_n, COD_preg_n, COD_T1_n, "", NSAID_prior_n, NSAID_preg_n, NSAID_T1_n, "", antinausea_prior_n, antinausea_preg_n, antinausea_T1_n, "", prev_med_prior_n, prev_med_preg_n, prev_med_T1_n, "", meto_prop_prior_n, meto_prop_preg_n, meto_prop_T1_n, "", ami_prior_n, ami_preg_n, ami_T1_n, "", c_l_v_prior_n, c_l_v_preg_n, c_l_v_T1_n, "", top_val_prior_n, top_val_preg_n, top_val_T1_n, "", bot_prior_n, bot_preg_n, bot_T1_n, "", piz_clo_prior_n, piz_clo_preg_n, piz_clo_T1_n, "", cgrp_prior_n, cgrp_preg_n, cgrp_T1_n, "", ere_prior_n, ere_preg_n, ere_T1_n, "", gal_prior_n, gal_preg_n, gal_T1_n, "", fre_prior_n, fre_preg_n, fre_T1_n),
  Prevalence = c("", acute_prior_prev, acute_preg_prev, acute_T1_prev, "", PCM_prior_prev, PCM_preg_prev, PCM_T1_prev, "", COD_prior_prev, COD_preg_prev, COD_T1_prev, "", NSAID_prior_prev, NSAID_preg_prev, NSAID_T1_prev, "", antinausea_prior_prev, antinausea_preg_prev, antinausea_T1_prev, "", prev_med_prior_prev, prev_med_preg_prev, prev_med_T1_prev, "", meto_prop_prior_prev, meto_prop_preg_prev, meto_prop_T1_prev, "", ami_prior_prev, ami_preg_prev, ami_T1_prev, "", c_l_v_prior_prev, c_l_v_preg_prev, c_l_v_T1_prev, "", top_val_prior_prev, top_val_preg_prev, top_val_T1_prev, "", bot_prior_prev, bot_preg_prev, bot_T1_prev, "", piz_clo_prior_prev, piz_clo_preg_prev, piz_clo_T1_prev, "", cgrp_prior_prev, cgrp_preg_prev, cgrp_T1_prev, "", ere_prior_prev, ere_preg_prev, ere_T1_prev, "", gal_prior_prev, gal_preg_prev, gal_T1_prev, "", fre_prior_prev, fre_preg_prev, fre_T1_prev),
  CI_lower = c("", acute_prior_low_CI, acute_preg_low_CI, acute_T1_low_CI, "", PCM_prior_low_CI, PCM_preg_low_CI, PCM_T1_low_CI, "", COD_prior_low_CI, COD_preg_low_CI, COD_T1_low_CI, "", NSAID_prior_low_CI, NSAID_preg_low_CI, NSAID_T1_low_CI, "", antinausea_prior_low_CI, antinausea_preg_low_CI, antinausea_T1_low_CI, "", prev_med_prior_low_CI, prev_med_preg_low_CI, prev_med_T1_low_CI, "", meto_prop_prior_low_CI, meto_prop_preg_low_CI, meto_prop_T1_low_CI, "", ami_prior_low_CI, ami_preg_low_CI, ami_T1_low_CI, "", c_l_v_prior_low_CI, c_l_v_preg_low_CI, c_l_v_T1_low_CI, "", top_val_prior_low_CI, top_val_preg_low_CI, top_val_T1_low_CI, "", bot_prior_low_CI, bot_preg_low_CI, bot_T1_low_CI, "", piz_clo_prior_low_CI, piz_clo_preg_low_CI, piz_clo_T1_low_CI, "", cgrp_prior_low_CI, cgrp_preg_low_CI, cgrp_T1_low_CI, "", ere_prior_low_CI, ere_preg_low_CI, ere_T1_low_CI, "", gal_prior_low_CI, gal_preg_low_CI, gal_T1_low_CI, "", fre_prior_low_CI, fre_preg_low_CI, fre_T1_low_CI),
  CI_upper = c("", acute_prior_up_CI, acute_preg_up_CI, acute_T1_up_CI, "", PCM_prior_up_CI, PCM_preg_up_CI, PCM_T1_up_CI, "", COD_prior_up_CI, COD_preg_up_CI, COD_T1_up_CI, "", NSAID_prior_up_CI, NSAID_preg_up_CI, NSAID_T1_up_CI, "", antinausea_prior_up_CI, antinausea_preg_up_CI, antinausea_T1_up_CI, "", prev_med_prior_up_CI, prev_med_preg_up_CI, prev_med_T1_up_CI, "", meto_prop_prior_up_CI, meto_prop_preg_up_CI, meto_prop_T1_up_CI, "", ami_prior_up_CI, ami_preg_up_CI, ami_T1_up_CI, "", c_l_v_prior_up_CI, c_l_v_preg_up_CI, c_l_v_T1_up_CI, "", top_val_prior_up_CI, top_val_preg_up_CI, top_val_T1_up_CI, "", bot_prior_up_CI, bot_preg_up_CI, bot_T1_up_CI, "", piz_clo_prior_up_CI, piz_clo_preg_up_CI, piz_clo_T1_up_CI, "", cgrp_prior_up_CI, cgrp_preg_up_CI, cgrp_T1_up_CI, "", ere_prior_up_CI, ere_preg_up_CI, ere_T1_up_CI, "", gal_prior_up_CI, gal_preg_up_CI, gal_T1_up_CI, "", fre_prior_up_CI, fre_preg_up_CI, fre_T1_up_CI)
)

write.csv(table4_nonuser, file.path(csv_path, "Table4_nonuser.csv"), row.names = FALSE)  ##Path to output folder

# rm(list = ls.str(mode = "numeric"))
# rm(list = ls()[! ls() %in% c("table4_nonuser", "dep", "diab", "DUS_all", "HT", "obesity", "scoreci")])

############# Table 5 #########
cat("\nCreating Table 5 ...\n")

# Does not exist in SAP


############# Table 6 #########
cat("\nCreating Table 6 ...\n")

DUS_T1 <- DUS_all[DUS_all$trip_T1 == 1 & DUS_all$trip_T2 != 1 & DUS_all$trip_T3 != 1, ]
DUS_T2 <- DUS_all[DUS_all$trip_T2 == 1 & DUS_all$trip_T3 != 1, ]
DUS_T3 <- DUS_all[DUS_all$trip_T3 == 1, ]

denom_T1 <- nrow(DUS_T1[DUS_T1$trip_T1 == 1, ])
denom_T2 <- nrow(DUS_T2[DUS_T2$trip_T2 == 1, ])
denom_T3 <- nrow(DUS_T3[DUS_T3$trip_T3 == 1, ])

acute_T1_n <- nrow(DUS_T1[DUS_T1$acute_T1 == 1, ])
acute_T2_n <- nrow(DUS_T2[DUS_T2$acute_T2 == 1, ])
acute_T3_n <- nrow(DUS_T3[DUS_T3$acute_T3 == 1, ])
acute_T1_prev <- round((acute_T1_n / denom_T1)*100, 3)
acute_T2_prev <- round((acute_T2_n / denom_T2)*100, 3)
acute_T3_prev <- round((acute_T3_n / denom_T3)*100, 3)
acute_T1 <- scoreci(acute_T1_n, denom_T1)
acute_T2 <- scoreci(acute_T2_n, denom_T2)
acute_T3 <- scoreci(acute_T3_n, denom_T3)
acute_T1_low_CI <- round((acute_T1$conf.low)*100, 3)
acute_T1_up_CI <- round((acute_T1$conf.high)*100, 3)
acute_T2_low_CI <- round((acute_T2$conf.low)*100, 3)
acute_T2_up_CI <- round((acute_T2$conf.high)*100, 3)
acute_T3_low_CI <- round((acute_T3$conf.low)*100, 3)
acute_T3_up_CI <- round((acute_T3$conf.high)*100, 3)

PCM_T1_n <- nrow(DUS_T1[DUS_T1$PCM_T1 == 1, ])
PCM_T2_n <- nrow(DUS_T2[DUS_T2$PCM_T2 == 1, ])
PCM_T3_n <- nrow(DUS_T3[DUS_T3$PCM_T3 == 1, ])
PCM_T1_prev <- round((PCM_T1_n / denom_T1)*100, 3)
PCM_T2_prev <- round((PCM_T2_n / denom_T2)*100, 3)
PCM_T3_prev <- round((PCM_T3_n / denom_T3)*100, 3)
PCM_T1 <- scoreci(PCM_T1_n, denom_T1)
PCM_T2 <- scoreci(PCM_T2_n, denom_T2)
PCM_T3 <- scoreci(PCM_T3_n, denom_T3)
PCM_T1_low_CI <- round((PCM_T1$conf.low)*100, 3)
PCM_T1_up_CI <- round((PCM_T1$conf.high)*100, 3)
PCM_T2_low_CI <- round((PCM_T2$conf.low)*100, 3)
PCM_T2_up_CI <- round((PCM_T2$conf.high)*100, 3)
PCM_T3_low_CI <- round((PCM_T3$conf.low)*100, 3)
PCM_T3_up_CI <- round((PCM_T3$conf.high)*100, 3)

COD_T1_n <- nrow(DUS_T1[DUS_T1$COD_T1 == 1, ])
COD_T2_n <- nrow(DUS_T2[DUS_T2$COD_T2 == 1, ])
COD_T3_n <- nrow(DUS_T3[DUS_T3$COD_T3 == 1, ])
COD_T1_prev <- round((COD_T1_n / denom_T1)*100, 3)
COD_T2_prev <- round((COD_T2_n / denom_T2)*100, 3)
COD_T3_prev <- round((COD_T3_n / denom_T3)*100, 3)
COD_T1 <- scoreci(COD_T1_n, denom_T1)
COD_T2 <- scoreci(COD_T2_n, denom_T2)
COD_T3 <- scoreci(COD_T3_n, denom_T3)
COD_T1_low_CI <- round((COD_T1$conf.low)*100, 3)
COD_T1_up_CI <- round((COD_T1$conf.high)*100, 3)
COD_T2_low_CI <- round((COD_T2$conf.low)*100, 3)
COD_T2_up_CI <- round((COD_T2$conf.high)*100, 3)
COD_T3_low_CI <- round((COD_T3$conf.low)*100, 3)
COD_T3_up_CI <- round((COD_T3$conf.high)*100, 3)

NSAID_T1_n <- nrow(DUS_T1[DUS_T1$NSAID_T1 == 1, ])
NSAID_T2_n <- nrow(DUS_T2[DUS_T2$NSAID_T2 == 1, ])
NSAID_T3_n <- nrow(DUS_T3[DUS_T3$NSAID_T3 == 1, ])
NSAID_T1_prev <- round((NSAID_T1_n / denom_T1)*100, 3)
NSAID_T2_prev <- round((NSAID_T2_n / denom_T2)*100, 3)
NSAID_T3_prev <- round((NSAID_T3_n / denom_T3)*100, 3)
NSAID_T1 <- scoreci(NSAID_T1_n, denom_T1)
NSAID_T2 <- scoreci(NSAID_T2_n, denom_T2)
NSAID_T3 <- scoreci(NSAID_T3_n, denom_T3)
NSAID_T1_low_CI <- round((NSAID_T1$conf.low)*100, 3)
NSAID_T1_up_CI <- round((NSAID_T1$conf.high)*100, 3)
NSAID_T2_low_CI <- round((NSAID_T2$conf.low)*100, 3)
NSAID_T2_up_CI <- round((NSAID_T2$conf.high)*100, 3)
NSAID_T3_low_CI <- round((NSAID_T3$conf.low)*100, 3)
NSAID_T3_up_CI <- round((NSAID_T3$conf.high)*100, 3)

antinausea_T1_n <- nrow(DUS_T1[DUS_T1$antinausea_T1 == 1, ])
antinausea_T2_n <- nrow(DUS_T2[DUS_T2$antinausea_T2 == 1, ])
antinausea_T3_n <- nrow(DUS_T3[DUS_T3$antinausea_T3 == 1, ])
antinausea_T1_prev <- round((antinausea_T1_n / denom_T1)*100, 3)
antinausea_T2_prev <- round((antinausea_T2_n / denom_T2)*100, 3)
antinausea_T3_prev <- round((antinausea_T3_n / denom_T3)*100, 3)
antinausea_T1 <- scoreci(antinausea_T1_n, denom_T1)
antinausea_T2 <- scoreci(antinausea_T2_n, denom_T2)
antinausea_T3 <- scoreci(antinausea_T3_n, denom_T3)
antinausea_T1_low_CI <- round((antinausea_T1$conf.low)*100, 3)
antinausea_T1_up_CI <- round((antinausea_T1$conf.high)*100, 3)
antinausea_T2_low_CI <- round((antinausea_T2$conf.low)*100, 3)
antinausea_T2_up_CI <- round((antinausea_T2$conf.high)*100, 3)
antinausea_T3_low_CI <- round((antinausea_T3$conf.low)*100, 3)
antinausea_T3_up_CI <- round((antinausea_T3$conf.high)*100, 3)

prev_med_T1_n <- nrow(DUS_T1[DUS_T1$prev_med_T1 == 1, ])
prev_med_T2_n <- nrow(DUS_T2[DUS_T2$prev_med_T2 == 1, ])
prev_med_T3_n <- nrow(DUS_T3[DUS_T3$prev_med_T3 == 1, ])
prev_med_T1_prev <- round((prev_med_T1_n / denom_T1)*100, 3)
prev_med_T2_prev <- round((prev_med_T2_n / denom_T2)*100, 3)
prev_med_T3_prev <- round((prev_med_T3_n / denom_T3)*100, 3)
prev_med_T1 <- scoreci(prev_med_T1_n, denom_T1)
prev_med_T2 <- scoreci(prev_med_T2_n, denom_T2)
prev_med_T3 <- scoreci(prev_med_T3_n, denom_T3)
prev_med_T1_low_CI <- round((prev_med_T1$conf.low)*100, 3)
prev_med_T1_up_CI <- round((prev_med_T1$conf.high)*100, 3)
prev_med_T2_low_CI <- round((prev_med_T2$conf.low)*100, 3)
prev_med_T2_up_CI <- round((prev_med_T2$conf.high)*100, 3)
prev_med_T3_low_CI <- round((prev_med_T3$conf.low)*100, 3)
prev_med_T3_up_CI <- round((prev_med_T3$conf.high)*100, 3)

meto_prop_T1_n <- nrow(DUS_T1[DUS_T1$meto_prop_T1 == 1, ])
meto_prop_T2_n <- nrow(DUS_T2[DUS_T2$meto_prop_T2 == 1, ])
meto_prop_T3_n <- nrow(DUS_T3[DUS_T3$meto_prop_T3 == 1, ])
meto_prop_T1_prev <- round((meto_prop_T1_n / denom_T1)*100, 3)
meto_prop_T2_prev <- round((meto_prop_T2_n / denom_T2)*100, 3)
meto_prop_T3_prev <- round((meto_prop_T3_n / denom_T3)*100, 3)
meto_prop_T1 <- scoreci(meto_prop_T1_n, denom_T1)
meto_prop_T2 <- scoreci(meto_prop_T2_n, denom_T2)
meto_prop_T3 <- scoreci(meto_prop_T3_n, denom_T3)
meto_prop_T1_low_CI <- round((meto_prop_T1$conf.low)*100, 3)
meto_prop_T1_up_CI <- round((meto_prop_T1$conf.high)*100, 3)
meto_prop_T2_low_CI <- round((meto_prop_T2$conf.low)*100, 3)
meto_prop_T2_up_CI <- round((meto_prop_T2$conf.high)*100, 3)
meto_prop_T3_low_CI <- round((meto_prop_T3$conf.low)*100, 3)
meto_prop_T3_up_CI <- round((meto_prop_T3$conf.high)*100, 3)

ami_T1_n <- nrow(DUS_T1[DUS_T1$ami_T1 == 1, ])
ami_T2_n <- nrow(DUS_T2[DUS_T2$ami_T2 == 1, ])
ami_T3_n <- nrow(DUS_T3[DUS_T3$ami_T3 == 1, ])
ami_T1_prev <- round((ami_T1_n / denom_T1)*100, 3)
ami_T2_prev <- round((ami_T2_n / denom_T2)*100, 3)
ami_T3_prev <- round((ami_T3_n / denom_T3)*100, 3)
ami_T1 <- scoreci(ami_T1_n, denom_T1)
ami_T2 <- scoreci(ami_T2_n, denom_T2)
ami_T3 <- scoreci(ami_T3_n, denom_T3)
ami_T1_low_CI <- round((ami_T1$conf.low)*100, 3)
ami_T1_up_CI <- round((ami_T1$conf.high)*100, 3)
ami_T2_low_CI <- round((ami_T2$conf.low)*100, 3)
ami_T2_up_CI <- round((ami_T2$conf.high)*100, 3)
ami_T3_low_CI <- round((ami_T3$conf.low)*100, 3)
ami_T3_up_CI <- round((ami_T3$conf.high)*100, 3)

c_l_v_T1_n <- nrow(DUS_T1[DUS_T1$c_l_v_T1 == 1, ])
c_l_v_T2_n <- nrow(DUS_T2[DUS_T2$c_l_v_T2 == 1, ])
c_l_v_T3_n <- nrow(DUS_T3[DUS_T3$c_l_v_T3 == 1, ])
c_l_v_T1_prev <- round((c_l_v_T1_n / denom_T1)*100, 3)
c_l_v_T2_prev <- round((c_l_v_T2_n / denom_T2)*100, 3)
c_l_v_T3_prev <- round((c_l_v_T3_n / denom_T3)*100, 3)
c_l_v_T1 <- scoreci(c_l_v_T1_n, denom_T1)
c_l_v_T2 <- scoreci(c_l_v_T2_n, denom_T2)
c_l_v_T3 <- scoreci(c_l_v_T3_n, denom_T3)
c_l_v_T1_low_CI <- round((c_l_v_T1$conf.low)*100, 3)
c_l_v_T1_up_CI <- round((c_l_v_T1$conf.high)*100, 3)
c_l_v_T2_low_CI <- round((c_l_v_T2$conf.low)*100, 3)
c_l_v_T2_up_CI <- round((c_l_v_T2$conf.high)*100, 3)
c_l_v_T3_low_CI <- round((c_l_v_T3$conf.low)*100, 3)
c_l_v_T3_up_CI <- round((c_l_v_T3$conf.high)*100, 3)

top_val_T1_n <- nrow(DUS_T1[DUS_T1$top_val_T1 == 1, ])
top_val_T2_n <- nrow(DUS_T2[DUS_T2$top_val_T2 == 1, ])
top_val_T3_n <- nrow(DUS_T3[DUS_T3$top_val_T3 == 1, ])
top_val_T1_prev <- round((top_val_T1_n / denom_T1)*100, 3)
top_val_T2_prev <- round((top_val_T2_n / denom_T2)*100, 3)
top_val_T3_prev <- round((top_val_T3_n / denom_T3)*100, 3)
top_val_T1 <- scoreci(top_val_T1_n, denom_T1)
top_val_T2 <- scoreci(top_val_T2_n, denom_T2)
top_val_T3 <- scoreci(top_val_T3_n, denom_T3)
top_val_T1_low_CI <- round((top_val_T1$conf.low)*100, 3)
top_val_T1_up_CI <- round((top_val_T1$conf.high)*100, 3)
top_val_T2_low_CI <- round((top_val_T2$conf.low)*100, 3)
top_val_T2_up_CI <- round((top_val_T2$conf.high)*100, 3)
top_val_T3_low_CI <- round((top_val_T3$conf.low)*100, 3)
top_val_T3_up_CI <- round((top_val_T3$conf.high)*100, 3)

bot_T1_n <- nrow(DUS_T1[DUS_T1$bot_T1 == 1, ])
bot_T2_n <- nrow(DUS_T2[DUS_T2$bot_T2 == 1, ])
bot_T3_n <- nrow(DUS_T3[DUS_T3$bot_T3 == 1, ])
bot_T1_prev <- round((bot_T1_n / denom_T1)*100, 3)
bot_T2_prev <- round((bot_T2_n / denom_T2)*100, 3)
bot_T3_prev <- round((bot_T3_n / denom_T3)*100, 3)
bot_T1 <- scoreci(bot_T1_n, denom_T1)
bot_T2 <- scoreci(bot_T2_n, denom_T2)
bot_T3 <- scoreci(bot_T3_n, denom_T3)
bot_T1_low_CI <- round((bot_T1$conf.low)*100, 3)
bot_T1_up_CI <- round((bot_T1$conf.high)*100, 3)
bot_T2_low_CI <- round((bot_T2$conf.low)*100, 3)
bot_T2_up_CI <- round((bot_T2$conf.high)*100, 3)
bot_T3_low_CI <- round((bot_T3$conf.low)*100, 3)
bot_T3_up_CI <- round((bot_T3$conf.high)*100, 3)

piz_clo_T1_n <- nrow(DUS_T1[DUS_T1$piz_clo_T1 == 1, ])
piz_clo_T2_n <- nrow(DUS_T2[DUS_T2$piz_clo_T2 == 1, ])
piz_clo_T3_n <- nrow(DUS_T3[DUS_T3$piz_clo_T3 == 1, ])
piz_clo_T1_prev <- round((piz_clo_T1_n / denom_T1)*100, 3)
piz_clo_T2_prev <- round((piz_clo_T2_n / denom_T2)*100, 3)
piz_clo_T3_prev <- round((piz_clo_T3_n / denom_T3)*100, 3)
piz_clo_T1 <- scoreci(piz_clo_T1_n, denom_T1)
piz_clo_T2 <- scoreci(piz_clo_T2_n, denom_T2)
piz_clo_T3 <- scoreci(piz_clo_T3_n, denom_T3)
piz_clo_T1_low_CI <- round((piz_clo_T1$conf.low)*100, 3)
piz_clo_T1_up_CI <- round((piz_clo_T1$conf.high)*100, 3)
piz_clo_T2_low_CI <- round((piz_clo_T2$conf.low)*100, 3)
piz_clo_T2_up_CI <- round((piz_clo_T2$conf.high)*100, 3)
piz_clo_T3_low_CI <- round((piz_clo_T3$conf.low)*100, 3)
piz_clo_T3_up_CI <- round((piz_clo_T3$conf.high)*100, 3)

cgrp_T1_n <- nrow(DUS_T1[DUS_T1$cgrp_T1 == 1, ])
cgrp_T2_n <- nrow(DUS_T2[DUS_T2$cgrp_T2 == 1, ])
cgrp_T3_n <- nrow(DUS_T3[DUS_T3$cgrp_T3 == 1, ])
cgrp_T1_prev <- round((cgrp_T1_n / denom_T1)*100, 3)
cgrp_T2_prev <- round((cgrp_T2_n / denom_T2)*100, 3)
cgrp_T3_prev <- round((cgrp_T3_n / denom_T3)*100, 3)
cgrp_T1 <- scoreci(cgrp_T1_n, denom_T1)
cgrp_T2 <- scoreci(cgrp_T2_n, denom_T2)
cgrp_T3 <- scoreci(cgrp_T3_n, denom_T3)
cgrp_T1_low_CI <- round((cgrp_T1$conf.low)*100, 3)
cgrp_T1_up_CI <- round((cgrp_T1$conf.high)*100, 3)
cgrp_T2_low_CI <- round((cgrp_T2$conf.low)*100, 3)
cgrp_T2_up_CI <- round((cgrp_T2$conf.high)*100, 3)
cgrp_T3_low_CI <- round((cgrp_T3$conf.low)*100, 3)
cgrp_T3_up_CI <- round((cgrp_T3$conf.high)*100, 3)

ere_T1_n <- nrow(DUS_T1[DUS_T1$ere_T1 == 1, ])
ere_T2_n <- nrow(DUS_T2[DUS_T2$ere_T2 == 1, ])
ere_T3_n <- nrow(DUS_T3[DUS_T3$ere_T3 == 1, ])
ere_T1_prev <- round((ere_T1_n / denom_T1)*100, 3)
ere_T2_prev <- round((ere_T2_n / denom_T2)*100, 3)
ere_T3_prev <- round((ere_T3_n / denom_T3)*100, 3)
ere_T1 <- scoreci(ere_T1_n, denom_T1)
ere_T2 <- scoreci(ere_T2_n, denom_T2)
ere_T3 <- scoreci(ere_T3_n, denom_T3)
ere_T1_low_CI <- round((ere_T1$conf.low)*100, 3)
ere_T1_up_CI <- round((ere_T1$conf.high)*100, 3)
ere_T2_low_CI <- round((ere_T2$conf.low)*100, 3)
ere_T2_up_CI <- round((ere_T2$conf.high)*100, 3)
ere_T3_low_CI <- round((ere_T3$conf.low)*100, 3)
ere_T3_up_CI <- round((ere_T3$conf.high)*100, 3)

gal_T1_n <- nrow(DUS_T1[DUS_T1$gal_T1 == 1, ])
gal_T2_n <- nrow(DUS_T2[DUS_T2$gal_T2 == 1, ])
gal_T3_n <- nrow(DUS_T3[DUS_T3$gal_T3 == 1, ])
gal_T1_prev <- round((gal_T1_n / denom_T1)*100, 3)
gal_T2_prev <- round((gal_T2_n / denom_T2)*100, 3)
gal_T3_prev <- round((gal_T3_n / denom_T3)*100, 3)
gal_T1 <- scoreci(gal_T1_n, denom_T1)
gal_T2 <- scoreci(gal_T2_n, denom_T2)
gal_T3 <- scoreci(gal_T3_n, denom_T3)
gal_T1_low_CI <- round((gal_T1$conf.low)*100, 3)
gal_T1_up_CI <- round((gal_T1$conf.high)*100, 3)
gal_T2_low_CI <- round((gal_T2$conf.low)*100, 3)
gal_T2_up_CI <- round((gal_T2$conf.high)*100, 3)
gal_T3_low_CI <- round((gal_T3$conf.low)*100, 3)
gal_T3_up_CI <- round((gal_T3$conf.high)*100, 3)

fre_T1_n <- nrow(DUS_T1[DUS_T1$fre_T1 == 1, ])
fre_T2_n <- nrow(DUS_T2[DUS_T2$fre_T2 == 1, ])
fre_T3_n <- nrow(DUS_T3[DUS_T3$fre_T3 == 1, ])
fre_T1_prev <- round((fre_T1_n / denom_T1)*100, 3)
fre_T2_prev <- round((fre_T2_n / denom_T2)*100, 3)
fre_T3_prev <- round((fre_T3_n / denom_T3)*100, 3)
fre_T1 <- scoreci(fre_T1_n, denom_T1)
fre_T2 <- scoreci(fre_T2_n, denom_T2)
fre_T3 <- scoreci(fre_T3_n, denom_T3)
fre_T1_low_CI <- round((fre_T1$conf.low)*100, 3)
fre_T1_up_CI <- round((fre_T1$conf.high)*100, 3)
fre_T2_low_CI <- round((fre_T2$conf.low)*100, 3)
fre_T2_up_CI <- round((fre_T2$conf.high)*100, 3)
fre_T3_low_CI <- round((fre_T3$conf.low)*100, 3)
fre_T3_up_CI <- round((fre_T3$conf.high)*100, 3)

# table6 <- data.frame(
#   Other_migraine_medication = c("TOTAL N", "Acute migraine treatment", "Trimester 1", "Trimester 2", "Trimester 3", "Paracetamol", "Trimester 1", "Trimester 2", "Trimester 3", "Codeine and paracetamol", "Trimester 1", "Trimester 2", "Trimester 3", "NSAIDs", "Trimester 1", "Trimester 2", "Trimester 3", "Antinauseants", "Trimester 1", "Trimester 2", "Trimester 3", "Preventive treatment", "Trimester 1", "Trimester 2", "Trimester 3", "Metoprolol/propranolol", "Trimester 1", "Trimester 2", "Trimester 3", "Amitriptyline", "Trimester 1", "Trimester 2", "Trimester 3", "Candesartan/lisinopril/verapamil", "Trimester 1", "Trimester 2", "Trimester 3", "Topiramate/valproic acid", "Trimester 1", "Trimester 2", "Trimester 3", "Botulinum toxin", "Trimester 1", "Trimester 2", "Trimester 3", "Pizotifen/clonidine", "Trimester 1", "Trimester 2", "Trimester 3", "CGRP antagonists", "Trimester 1", "Trimester 2", "Trimester 3", "Erenumab", "Trimester 1", "Trimester 2", "Trimester 3", "Galkanezumab", "Trimester 1", "Trimester 2", "Trimester 3", "Fremanezumab", "Trimester 1", "Trimester 2", "Trimester 3"),
#   T1_N_preg = c(denom_T1, "", acute_T1_n, "NA", "NA", "", PCM_T1_n, "NA", "NA", "", COD_T1_n, "NA", "NA", "", NSAID_T1_n, "NA", "NA", "", antinausea_T1_n, "NA", "NA", "", prev_med_T1_n, "NA", "NA", "", meto_prop_T1_n, "NA", "NA", "", ami_T1_n, "NA", "NA", "", c_l_v_T1_n, "NA", "NA", "", top_val_T1_n, "NA", "NA", "", bot_T1_n, "NA", "NA", "", piz_clo_T1_n, "NA", "NA", "", cgrp_T1_n, "NA", "NA", "", ere_T1_n, "NA", "NA", "", gal_T1_n, "NA", "NA", "", fre_T1_n, "NA", "NA"),
#   T1_prev = c("", "", acute_T1_prev, "NA", "NA", "", PCM_T1_prev, "NA", "NA", "", COD_T1_prev, "NA", "NA", "", NSAID_T1_prev, "NA", "NA", "", antinausea_T1_prev, "NA", "NA", "", prev_med_T1_prev, "NA", "NA", "", meto_prop_T1_prev, "NA", "NA", "", ami_T1_prev, "NA", "NA", "", c_l_v_T1_prev, "NA", "NA", "", top_val_T1_prev, "NA", "NA", "", bot_T1_prev, "NA", "NA", "", piz_clo_T1_prev, "NA", "NA", "", cgrp_T1_prev, "NA", "NA", "", ere_T1_prev, "NA", "NA", "", gal_T1_prev, "NA", "NA", "", fre_T1_prev, "NA", "NA"),
#   T1_CI_low = c("", "", acute_T1_low_CI, "NA", "NA", "", PCM_T1_low_CI, "NA", "NA", "", COD_T1_low_CI, "NA", "NA", "", NSAID_T1_low_CI, "NA", "NA", "", antinausea_T1_low_CI, "NA", "NA", "", prev_med_T1_low_CI, "NA", "NA", "", meto_prop_T1_low_CI, "NA", "NA", "", ami_T1_low_CI, "NA", "NA", "", c_l_v_T1_low_CI, "NA", "NA", "", top_val_T1_low_CI, "NA", "NA", "", bot_T1_low_CI, "NA", "NA", "", piz_clo_T1_low_CI, "NA", "NA", "", cgrp_T1_low_CI, "NA", "NA", "", ere_T1_low_CI, "NA", "NA", "", gal_T1_low_CI, "NA", "NA", "", fre_T1_low_CI, "NA", "NA"),
#   T1_CI_up = c("", "", acute_T1_up_CI, "NA", "NA", "", PCM_T1_up_CI, "NA", "NA", "", COD_T1_up_CI, "NA", "NA", "", NSAID_T1_up_CI, "NA", "NA", "", antinausea_T1_up_CI, "NA", "NA", "", prev_med_T1_up_CI, "NA", "NA", "", meto_prop_T1_up_CI, "NA", "NA", "", ami_T1_up_CI, "NA", "NA", "", c_l_v_T1_up_CI, "NA", "NA", "", top_val_T1_up_CI, "NA", "NA", "", bot_T1_up_CI, "NA", "NA", "", piz_clo_T1_up_CI, "NA", "NA", "", cgrp_T1_up_CI, "NA", "NA", "", ere_T1_up_CI, "NA", "NA", "", gal_T1_up_CI, "NA", "NA", "", fre_T1_up_CI, "NA", "NA"),
#   T2_N_preg = c(denom_T2, "", "NA", acute_T2_n, "NA", "", "NA", PCM_T2_n, "NA", "", "NA", COD_T2_n, "NA", "", "NA", NSAID_T2_n, "NA", "", "NA", antinausea_T2_n, "NA", "", "NA", prev_med_T2_n, "NA", "", "NA", meto_prop_T2_n, "NA", "", "NA", ami_T2_n, "NA", "", "NA", c_l_v_T2_n, "NA", "", "NA", top_val_T2_n, "NA", "", "NA", bot_T2_n, "NA", "", "NA", piz_clo_T2_n, "NA", "", "NA", cgrp_T2_n, "NA", "", "NA", ere_T2_n, "NA", "", "NA", gal_T2_n, "NA", "", "NA", fre_T2_n, "NA"),
#   T2_prev = c("", "", "NA", acute_T2_prev, "NA", "", "NA", PCM_T2_prev, "NA", "", "NA", COD_T2_prev, "NA", "", "NA", NSAID_T2_prev, "NA", "", "NA", antinausea_T2_prev, "NA", "", "NA", prev_med_T2_prev, "NA", "", "NA", meto_prop_T2_prev, "NA", "", "NA", ami_T2_prev, "NA", "", "NA", c_l_v_T2_prev, "NA", "", "NA", top_val_T2_prev, "NA", "", "NA", bot_T2_prev, "NA", "", "NA", piz_clo_T2_prev, "NA", "", "NA", cgrp_T2_prev, "NA", "", "NA", ere_T2_prev, "NA", "", "NA", gal_T2_prev, "NA", "", "NA", fre_T2_prev, "NA"),
#   T2_CI_low = c("", "", "NA", acute_T2_low_CI, "NA", "", "NA", PCM_T2_low_CI, "NA", "", "NA", COD_T2_low_CI, "NA", "", "NA", NSAID_T2_low_CI, "NA", "", "NA", antinausea_T2_low_CI, "NA", "", "NA", prev_med_T2_low_CI, "NA", "", "NA", meto_prop_T2_low_CI, "NA", "", "NA", ami_T2_low_CI, "NA", "", "NA", c_l_v_T2_low_CI, "NA", "", "NA", top_val_T2_low_CI, "NA", "", "NA", bot_T2_low_CI, "NA", "", "NA", piz_clo_T2_low_CI, "NA", "", "NA", cgrp_T2_low_CI, "NA", "", "NA", ere_T2_low_CI, "NA", "", "NA", gal_T2_low_CI, "NA", "", "NA", fre_T2_low_CI, "NA"),
#   T2_CI_up = c("", "", "NA", acute_T2_up_CI, "NA", "", "NA", PCM_T2_up_CI, "NA", "", "NA", COD_T2_up_CI, "NA", "", "NA", NSAID_T2_up_CI, "NA", "", "NA", antinausea_T2_up_CI, "NA", "", "NA", prev_med_T2_up_CI, "NA", "", "NA", meto_prop_T2_up_CI, "NA", "", "NA", ami_T2_up_CI, "NA", "", "NA", c_l_v_T2_up_CI, "NA", "", "NA", top_val_T2_up_CI, "NA", "", "NA", bot_T2_up_CI, "NA", "", "NA", piz_clo_T2_up_CI, "NA", "", "NA", cgrp_T2_up_CI, "NA", "", "NA", ere_T2_up_CI, "NA", "", "NA", gal_T2_up_CI, "NA", "", "NA", fre_T2_up_CI, "NA"),
#   T3_N_preg = c(denom_T3, "", "NA", "NA", acute_T3_n, "", "NA", "NA", PCM_T3_n, "", "NA", "NA", COD_T3_n, "", "NA", "NA", NSAID_T3_n, "", "NA", "NA", antinausea_T3_n, "", "NA", "NA", prev_med_T3_n, "", "NA", "NA", meto_prop_T3_n, "", "NA", "NA", ami_T3_n, "", "NA", "NA", c_l_v_T3_n, "", "NA", "NA", top_val_T3_n, "", "NA", "NA", bot_T3_n, "", "NA", "NA", piz_clo_T3_n, "", "NA", "NA", cgrp_T3_n, "", "NA", "NA", ere_T3_n, "", "NA", "NA", gal_T3_n, "", "NA", "NA", fre_T3_n),
#   T3_prev = c("", "", "NA", "NA", acute_T3_prev, "", "NA", "NA", PCM_T3_prev, "", "NA", "NA", COD_T3_prev, "", "NA", "NA", NSAID_T3_prev, "", "NA", "NA", antinausea_T3_prev, "", "NA", "NA", prev_med_T3_prev, "", "NA", "NA", meto_prop_T3_prev, "", "NA", "NA", ami_T3_prev, "", "NA", "NA", c_l_v_T3_prev, "", "NA", "NA", top_val_T3_prev, "", "NA", "NA", bot_T3_prev, "", "NA", "NA", piz_clo_T3_prev, "", "NA", "NA", cgrp_T3_prev, "", "NA", "NA", ere_T3_prev, "", "NA", "NA", gal_T3_prev, "", "NA", "NA", fre_T3_prev),
#   T3_CI_low = c("", "", "NA", "NA", acute_T3_low_CI, "", "NA", "NA", PCM_T3_low_CI, "", "NA", "NA", COD_T3_low_CI, "", "NA", "NA", NSAID_T3_low_CI, "", "NA", "NA", antinausea_T3_low_CI, "", "NA", "NA", prev_med_T3_low_CI, "", "NA", "NA", meto_prop_T3_low_CI, "", "NA", "NA", ami_T3_low_CI, "", "NA", "NA", c_l_v_T3_low_CI, "", "NA", "NA", top_val_T3_low_CI, "", "NA", "NA", bot_T3_low_CI, "", "NA", "NA", piz_clo_T3_low_CI, "", "NA", "NA", cgrp_T3_low_CI, "", "NA", "NA", ere_T3_low_CI, "", "NA", "NA", gal_T3_low_CI, "", "NA", "NA", fre_T3_low_CI),
#   T3_CI_up = c("", "", "NA", "NA", acute_T3_up_CI, "", "NA", "NA", PCM_T3_up_CI, "", "NA", "NA", COD_T3_up_CI, "", "NA", "NA", NSAID_T3_up_CI, "", "NA", "NA", antinausea_T3_up_CI, "", "NA", "NA", prev_med_T3_up_CI, "", "NA", "NA", meto_prop_T3_up_CI, "", "NA", "NA", ami_T3_up_CI, "", "NA", "NA", c_l_v_T3_up_CI, "", "NA", "NA", top_val_T3_up_CI, "", "NA", "NA", bot_T3_up_CI, "", "NA", "NA", piz_clo_T3_up_CI, "", "NA", "NA", cgrp_T3_up_CI, "", "NA", "NA", ere_T3_up_CI, "", "NA", "NA", gal_T3_up_CI, "", "NA", "NA", fre_T3_up_CI)
# )

table6 <- data.frame(
  Other_migraine_medication=c("TOTALN","Trimester1","Trimester2","Trimester3","Acutemigrainetreatment","Trimester1","Trimester2","Trimester3","Paracetamol","Trimester1","Trimester2","Trimester3","Codeineandparacetamol","Trimester1","Trimester2","Trimester3","NSAIDs","Trimester1","Trimester2","Trimester3","Antinauseants","Trimester1","Trimester2","Trimester3","Preventivetreatment","Trimester1","Trimester2","Trimester3","Metoprolol/propranolol","Trimester1","Trimester2","Trimester3","Amitriptyline","Trimester1","Trimester2","Trimester3","Candesartan/lisinopril/verapamil","Trimester1","Trimester2","Trimester3","Topiramate/valproicacid","Trimester1","Trimester2","Trimester3","Botulinumtoxin","Trimester1","Trimester2","Trimester3","Pizotifen/clonidine","Trimester1","Trimester2","Trimester3","CGRPantagonists","Trimester1","Trimester2","Trimester3","Erenumab","Trimester1","Trimester2","Trimester3","Galkanezumab","Trimester1","Trimester2","Trimester3","Fremanezumab","Trimester1","Trimester2","Trimester3"),
  N_preg=c("",denom_T1,denom_T2,denom_T3,"",acute_T1_n,acute_T2_n,acute_T3_n,"",PCM_T1_n,PCM_T2_n,PCM_T3_n,"",COD_T1_n,COD_T2_n,COD_T3_n,"",NSAID_T1_n,NSAID_T2_n,NSAID_T3_n,"",antinausea_T1_n,antinausea_T2_n,antinausea_T3_n,"",prev_med_T1_n,prev_med_T2_n,prev_med_T3_n,"",meto_prop_T1_n,meto_prop_T2_n,meto_prop_T3_n,"",ami_T1_n,ami_T2_n,ami_T3_n,"",c_l_v_T1_n,c_l_v_T2_n,c_l_v_T3_n,"",top_val_T1_n,top_val_T2_n,top_val_T3_n,"",bot_T1_n,bot_T2_n,bot_T3_n,"",piz_clo_T1_n,piz_clo_T2_n,piz_clo_T3_n,"",cgrp_T1_n,cgrp_T2_n,cgrp_T3_n,"",ere_T1_n,ere_T2_n,ere_T3_n,"",gal_T1_n,gal_T2_n,gal_T3_n,"",fre_T1_n,fre_T2_n,fre_T3_n),
  prev=c("","","","","",acute_T1_prev,acute_T2_prev,acute_T3_prev,"",PCM_T1_prev,PCM_T2_prev,PCM_T3_prev,"",COD_T1_prev,COD_T2_prev,COD_T3_prev,"",NSAID_T1_prev,NSAID_T2_prev,NSAID_T3_prev,"",antinausea_T1_prev,antinausea_T2_prev,antinausea_T3_prev,"",prev_med_T1_prev,prev_med_T2_prev,prev_med_T3_prev,"",meto_prop_T1_prev,meto_prop_T2_prev,meto_prop_T3_prev,"",ami_T1_prev,ami_T2_prev,ami_T3_prev,"",c_l_v_T1_prev,c_l_v_T2_prev,c_l_v_T3_prev,"",top_val_T1_prev,top_val_T2_prev,top_val_T3_prev,"",bot_T1_prev,bot_T2_prev,bot_T3_prev,"",piz_clo_T1_prev,piz_clo_T2_prev,piz_clo_T3_prev,"",cgrp_T1_prev,cgrp_T2_prev,cgrp_T3_prev,"",ere_T1_prev,ere_T2_prev,ere_T3_prev,"",gal_T1_prev,gal_T2_prev,gal_T3_prev,"",fre_T1_prev,fre_T2_prev,fre_T3_prev),
  CI_low=c("","","","","",acute_T1_low_CI,acute_T2_low_CI,acute_T3_low_CI,"",PCM_T1_low_CI,PCM_T2_low_CI,PCM_T3_low_CI,"",COD_T1_low_CI,COD_T2_low_CI,COD_T3_low_CI,"",NSAID_T1_low_CI,NSAID_T2_low_CI,NSAID_T3_low_CI,"",antinausea_T1_low_CI,antinausea_T2_low_CI,antinausea_T3_low_CI,"",prev_med_T1_low_CI,prev_med_T2_low_CI,prev_med_T3_low_CI,"",meto_prop_T1_low_CI,meto_prop_T2_low_CI,meto_prop_T3_low_CI,"",ami_T1_low_CI,ami_T2_low_CI,ami_T3_low_CI,"",c_l_v_T1_low_CI,c_l_v_T2_low_CI,c_l_v_T3_low_CI,"",top_val_T1_low_CI,top_val_T2_low_CI,top_val_T3_low_CI,"",bot_T1_low_CI,bot_T2_low_CI,bot_T3_low_CI,"",piz_clo_T1_low_CI,piz_clo_T2_low_CI,piz_clo_T3_low_CI,"",cgrp_T1_low_CI,cgrp_T2_low_CI,cgrp_T3_low_CI,"",ere_T1_low_CI,ere_T2_low_CI,ere_T3_low_CI,"",gal_T1_low_CI,gal_T2_low_CI,gal_T3_low_CI,"",fre_T1_low_CI,fre_T2_low_CI,fre_T3_low_CI),
  CI_up=c("","","","","",acute_T1_up_CI,acute_T2_up_CI,acute_T3_up_CI,"",PCM_T1_up_CI,PCM_T2_up_CI,PCM_T3_up_CI,"",COD_T1_up_CI,COD_T2_up_CI,COD_T3_up_CI,"",NSAID_T1_up_CI,NSAID_T2_up_CI,NSAID_T3_up_CI,"",antinausea_T1_up_CI,antinausea_T2_up_CI,antinausea_T3_up_CI,"",prev_med_T1_up_CI,prev_med_T2_up_CI,prev_med_T3_up_CI,"",meto_prop_T1_up_CI,meto_prop_T2_up_CI,meto_prop_T3_up_CI,"",ami_T1_up_CI,ami_T2_up_CI,ami_T3_up_CI,"",c_l_v_T1_up_CI,c_l_v_T2_up_CI,c_l_v_T3_up_CI,"",top_val_T1_up_CI,top_val_T2_up_CI,top_val_T3_up_CI,"",bot_T1_up_CI,bot_T2_up_CI,bot_T3_up_CI,"",piz_clo_T1_up_CI,piz_clo_T2_up_CI,piz_clo_T3_up_CI,"",cgrp_T1_up_CI,cgrp_T2_up_CI,cgrp_T3_up_CI,"",ere_T1_up_CI,ere_T2_up_CI,ere_T3_up_CI,"",gal_T1_up_CI,gal_T2_up_CI,gal_T3_up_CI,"",fre_T1_up_CI,fre_T2_up_CI,fre_T3_up_CI)
)

write.csv(table6, file.path(csv_path, "Table6.csv"), row.names = FALSE)  ##Path to output folder

# rm(list = ls.str(mode = "numeric"))
# rm(list = ls()[! ls() %in% c("table6", "dep", "diab", "DUS_all", "HT", "obesity", "replace_na", "scoreci")])


############# Table 7 #########
cat("\nCreating Table 7 ...\n")

##Adding variables to data frame
# parity <- subset(parity, select = -c(pregnancy_id))
DUS_parity <- merge(DUS_all, parity, by = c("person_id", "pregnancy_id"), all.x = TRUE, allow.cartesian = TRUE) #many-to-many relationship
# DUS_parity$gap_LMP <- difftime(DUS_parity$event_date, DUS_parity$pregnancy_start_date, units = "days")
DUS_parity1 = suppressWarnings(DUS_parity %>% #filter(gap_LMP >= -365 & gap_LMP <= 0) %>%
                                 group_by(pregnancy_id) %>%
                                 summarize(parity = max(fixed_value, na.rm = TRUE))) 
DUS_all <- merge(DUS_all, DUS_parity1, by = "pregnancy_id", all.x=TRUE)
DUS_all$parity[is.infinite(DUS_all$parity)] <- NA
DUS_all$parity[is.nan(DUS_all$parity)] <- NA
#DUS_all$parity <- replace_na(DUS_all$parity, 0)
rm("parity", "DUS_parity", "DUS_parity1")


# obesity <- subset(obesity, select = -c(pregnancy_id))
DUS_obesity <- merge(DUS_all, obesity, by =  c("person_id", "pregnancy_id"), all.x = TRUE, allow.cartesian = TRUE) #many-to-many relationship
# DUS_obesity$gap_LMP <- difftime(DUS_obesity$event_date, DUS_obesity$pregnancy_start_date, units = "days")
# DUS_obesity$obesity_a <- ifelse((DUS_obesity$gap_LMP >= -365 & DUS_obesity$gap_LMP <= -1), 1, 0)
DUS_obesity1 = suppressWarnings(DUS_obesity %>% #filter(gap_LMP >= -365 & gap_LMP <= 0) %>%
  group_by(pregnancy_id) %>%
  summarize(obesity = (max(fixed_value, na.rm = TRUE)>30)*1, obesity_chk= any(chk))
  )
DUS_obesity1 = as.data.table(DUS_obesity1)
DUS_obesity1[obesity_chk == TRUE, obesity:=1]
DUS_all <- merge(DUS_all, DUS_obesity1, by = "pregnancy_id", all.x =TRUE)
# DUS_all$obesity <- replace_na(DUS_all$obesity, 0)
DUS_all$obesity[is.infinite(DUS_all$obesity)] <- NA
DUS_all$obesity[is.nan(DUS_all$obesity)] <- NA
rm("obesity", "DUS_obesity", "DUS_obesity1")

# HT <- subset(HT, select = -c(pregnancy_id))
DUS_HT <- merge(DUS_all, HT, by = c("person_id", "pregnancy_id"), all.x = TRUE, allow.cartesian = TRUE) #many-to-many relationship
DUS_HT$gap_LMP <- difftime(DUS_HT$event_date, DUS_HT$pregnancy_start_date, units = "days")
DUS_HT$HT_a <- ifelse((DUS_HT$gap_LMP >= -365 & DUS_HT$gap_LMP <= -1), 1, 0)
DUS_HT1 = DUS_HT %>%
  group_by(pregnancy_id) %>%
  summarize(HT = max(HT_a), HT_chk = any(chk))
DUS_HT1 = as.data.table(DUS_HT1)
DUS_HT1[HT_chk == TRUE, HT:=1]
DUS_all <- merge(DUS_all, DUS_HT1, by = c("pregnancy_id"))
DUS_all$HT <- replace_na(DUS_all$HT, 0)
rm("HT", "DUS_HT", "DUS_HT1")

# diab <- subset(diab, select = -c(pregnancy_id))
DUS_diab <- merge(DUS_all, diab, by = c("person_id", "pregnancy_id"), all.x = TRUE, allow.cartesian = TRUE) #many-to-many relationship
DUS_diab$gap_LMP <- difftime(DUS_diab$event_date, DUS_diab$pregnancy_start_date, units = "days")
DUS_diab$diab_a <- ifelse((DUS_diab$gap_LMP >= -365 & DUS_diab$gap_LMP <= -1), 1, 0)
DUS_diab1 = DUS_diab %>%
  group_by(pregnancy_id) %>%
  summarize(diab = max(diab_a), diab_chk = any(chk))
DUS_diab1 = as.data.table(DUS_diab1)
DUS_diab1[diab_chk == TRUE, diab:=1]
DUS_all <- merge(DUS_all, DUS_diab1, by = c("pregnancy_id"))
DUS_all$diab <- replace_na(DUS_all$diab, 0)
rm("diab", "DUS_diab", "DUS_diab1")

# dep <- subset(dep, select = -c(pregnancy_id))
DUS_dep <- merge(DUS_all, dep, by = c("person_id", "pregnancy_id"), all.x = TRUE, allow.cartesian = TRUE) #many-to-many relationship
DUS_dep$gap_LMP <- difftime(DUS_dep$event_date, DUS_dep$pregnancy_start_date, units = "days")
DUS_dep$dep_a <- ifelse((DUS_dep$gap_LMP >= -365 & DUS_dep$gap_LMP <= -1), 1, 0)
DUS_dep1 = DUS_dep %>%
  group_by(pregnancy_id) %>%
  summarize(dep = max(dep_a), dep_chk = any(chk))
DUS_dep1 = as.data.table(DUS_dep1)
DUS_dep1[dep_chk == TRUE, dep:=1]
DUS_all <- merge(DUS_all, DUS_dep1, by = c("pregnancy_id"))
DUS_all$dep <- replace_na(DUS_all$dep, 0)
rm("dep", "DUS_dep", "DUS_dep1")

##Calculate numbers - total cohort
denom_all <- length(unique(DUS_all$pregnancy_id))
year_0912_n <- nrow(DUS_all[DUS_all$year_group == "2009-2012", ])
year_0912_p <- round((year_0912_n / denom_all)*100, 1)
year_1316_n <- nrow(DUS_all[DUS_all$year_group == "2013-2016", ])
year_1316_p <- round((year_1316_n / denom_all)*100, 1)
year_1720_n <- nrow(DUS_all[DUS_all$year_group == "2017-2020", ])
year_1720_p <- round((year_1720_n / denom_all)*100, 1)
age_1524_n <- nrow(DUS_all[DUS_all$maternal_age == "15-24", ])
age_1524_p <- round((age_1524_n / denom_all)*100, 1)
age_2534_n <- nrow(DUS_all[DUS_all$maternal_age == "25-34", ])
age_2534_p <- round((age_2534_n / denom_all)*100, 1)
age_3549_n <- nrow(DUS_all[DUS_all$maternal_age == "35+", ])
age_3549_p <- round((age_3549_n / denom_all)*100, 1)
age_mis_n <- nrow(DUS_all[DUS_all$maternal_age == "", ])
age_mis_p <- round((age_mis_n / denom_all)*100, 1)
parity_0_n <- nrow(DUS_all[DUS_all$parity == 0, ])
parity_0_p <- round((parity_0_n / denom_all)*100, 1)
parity_1_n <- nrow(DUS_all[DUS_all$parity > 0, ])
parity_1_p <- round((parity_1_n / denom_all)*100, 1)
parity_mis_n <- nrow(DUS_all[is.na(DUS_all$parity), ])
parity_mis_p <- round((parity_mis_n / denom_all)*100, 1)
obesity_no_n <- nrow(DUS_all[DUS_all$obesity == 0, ])
obesity_no_p <- round((obesity_no_n / denom_all)*100, 1)
obesity_yes_n <- nrow(DUS_all[DUS_all$obesity == 1, ])
obesity_yes_p <- round((obesity_yes_n / denom_all)*100, 1)
obesity_mis_n <- nrow(DUS_all[is.na(DUS_all$obesity), ])
obesity_mis_p <- round((obesity_mis_n / denom_all)*100, 1)
HT_no_n <- nrow(DUS_all[DUS_all$HT == 0, ])
HT_no_p <- round((HT_no_n / denom_all)*100, 1)
HT_yes_n <- nrow(DUS_all[DUS_all$HT == 1, ])
HT_yes_p <- round((HT_yes_n / denom_all)*100, 1)
diab_no_n <- nrow(DUS_all[DUS_all$diab == 0, ])
diab_no_p <- round((diab_no_n / denom_all)*100, 1)
diab_yes_n <- nrow(DUS_all[DUS_all$diab == 1, ])
diab_yes_p <- round((diab_yes_n / denom_all)*100, 1)
dep_no_n <- nrow(DUS_all[DUS_all$dep == 0, ])
dep_no_p <- round((dep_no_n / denom_all)*100, 1)
dep_yes_n <- nrow(DUS_all[DUS_all$dep == 1, ])
dep_yes_p <- round((dep_yes_n / denom_all)*100, 1)

##Calculate numbers - non-users
nonusers <- DUS_all[DUS_all$trip_group == "Non-user", ]
denom_non <- length(unique(nonusers$pregnancy_id))
non_year_0912_n <- nrow(nonusers[nonusers$year_group == "2009-2012", ])
non_year_0912_p <- round((non_year_0912_n / denom_non)*100, 1)
non_year_1316_n <- nrow(nonusers[nonusers$year_group == "2013-2016", ])
non_year_1316_p <- round((non_year_1316_n / denom_non)*100, 1)
non_year_1720_n <- nrow(nonusers[nonusers$year_group == "2017-2020", ])
non_year_1720_p <- round((non_year_1720_n / denom_non)*100, 1)
non_age_1524_n <- nrow(nonusers[nonusers$maternal_age == "15-24", ])
non_age_1524_p <- round((non_age_1524_n / denom_non)*100, 1)
non_age_2534_n <- nrow(nonusers[nonusers$maternal_age == "25-34", ])
non_age_2534_p <- round((non_age_2534_n / denom_non)*100, 1)
non_age_3549_n <- nrow(nonusers[nonusers$maternal_age == "35+", ])
non_age_3549_p <- round((non_age_3549_n / denom_non)*100, 1)
non_age_mis_n <- nrow(nonusers[nonusers$maternal_age == "", ])
non_age_mis_p <- round((non_age_mis_n / denom_non)*100, 1)
non_parity_0_n <- nrow(nonusers[nonusers$parity == 0, ])
non_parity_0_p <- round((non_parity_0_n / denom_non)*100, 1)
non_parity_1_n <- nrow(nonusers[nonusers$parity > 0, ])
non_parity_1_p <- round((non_parity_1_n / denom_non)*100, 1)
non_parity_mis_n <- nrow(nonusers[is.na(nonusers$parity), ])
non_parity_mis_p <- round((non_parity_mis_n / denom_non)*100, 1)
non_obesity_no_n <- nrow(nonusers[nonusers$obesity == 0, ])
non_obesity_no_p <- round((non_obesity_no_n / denom_non)*100, 1)
non_obesity_yes_n <- nrow(nonusers[nonusers$obesity == 1, ])
non_obesity_yes_p <- round((non_obesity_yes_n / denom_non)*100, 1)
non_obesity_mis_n <- nrow(nonusers[is.na(nonusers$obesity), ])
non_obesity_mis_p <- round((non_obesity_mis_n / denom_non)*100, 1)
non_HT_no_n <- nrow(nonusers[nonusers$HT == 0, ])
non_HT_no_p <- round((non_HT_no_n / denom_non)*100, 1)
non_HT_yes_n <- nrow(nonusers[nonusers$HT == 1, ])
non_HT_yes_p <- round((non_HT_yes_n / denom_non)*100, 1)
non_diab_no_n <- nrow(nonusers[nonusers$diab == 0, ])
non_diab_no_p <- round((non_diab_no_n / denom_non)*100, 1)
non_diab_yes_n <- nrow(nonusers[nonusers$diab == 1, ])
non_diab_yes_p <- round((non_diab_yes_n / denom_non)*100, 1)
non_dep_no_n <- nrow(nonusers[nonusers$dep == 0, ])
non_dep_no_p <- round((non_dep_no_n / denom_non)*100, 1)
non_dep_yes_n <- nrow(nonusers[nonusers$dep == 1, ])
non_dep_yes_p <- round((non_dep_yes_n / denom_non)*100, 1)

##Calculate numbers - discontinuers
discontinuers <- DUS_all[DUS_all$trip_group == "Discontinuer", ]
denom_dis <- length(unique(discontinuers$pregnancy_id))
dis_year_0912_n <- nrow(discontinuers[discontinuers$year_group == "2009-2012", ])
dis_year_0912_p <- round((dis_year_0912_n / denom_dis)*100, 1)
dis_year_1316_n <- nrow(discontinuers[discontinuers$year_group == "2013-2016", ])
dis_year_1316_p <- round((dis_year_1316_n / denom_dis)*100, 1)
dis_year_1720_n <- nrow(discontinuers[discontinuers$year_group == "2017-2020", ])
dis_year_1720_p <- round((dis_year_1720_n / denom_dis)*100, 1)
dis_age_1524_n <- nrow(discontinuers[discontinuers$maternal_age == "15-24", ])
dis_age_1524_p <- round((dis_age_1524_n / denom_dis)*100, 1)
dis_age_2534_n <- nrow(discontinuers[discontinuers$maternal_age == "25-34", ])
dis_age_2534_p <- round((dis_age_2534_n / denom_dis)*100, 1)
dis_age_3549_n <- nrow(discontinuers[discontinuers$maternal_age == "35+", ])
dis_age_3549_p <- round((dis_age_3549_n / denom_dis)*100, 1)
dis_age_mis_n <- nrow(discontinuers[discontinuers$maternal_age == "", ])
dis_age_mis_p <- round((dis_age_mis_n / denom_dis)*100, 1)
dis_parity_0_n <- nrow(discontinuers[discontinuers$parity == 0, ])
dis_parity_0_p <- round((dis_parity_0_n / denom_dis)*100, 1)
dis_parity_1_n <- nrow(discontinuers[discontinuers$parity > 0, ])
dis_parity_1_p <- round((dis_parity_1_n / denom_dis)*100, 1)
dis_parity_mis_n <- nrow(discontinuers[is.na(discontinuers$parity), ])
dis_parity_mis_p <- round((dis_parity_mis_n / denom_dis)*100, 1)
dis_obesity_no_n <- nrow(discontinuers[discontinuers$obesity == 0, ])
dis_obesity_no_p <- round((dis_obesity_no_n / denom_dis)*100, 1)
dis_obesity_yes_n <- nrow(discontinuers[discontinuers$obesity == 1, ])
dis_obesity_yes_p <- round((dis_obesity_yes_n / denom_dis)*100, 1)
dis_obesity_mis_n <- nrow(discontinuers[is.na(discontinuers$obesity), ])
dis_obesity_mis_p <- round((dis_obesity_mis_n / denom_dis)*100, 1)
dis_HT_no_n <- nrow(discontinuers[discontinuers$HT == 0, ])
dis_HT_no_p <- round((dis_HT_no_n / denom_dis)*100, 1)
dis_HT_yes_n <- nrow(discontinuers[discontinuers$HT == 1, ])
dis_HT_yes_p <- round((dis_HT_yes_n / denom_dis)*100, 1)
dis_diab_no_n <- nrow(discontinuers[discontinuers$diab == 0, ])
dis_diab_no_p <- round((dis_diab_no_n / denom_dis)*100, 1)
dis_diab_yes_n <- nrow(discontinuers[discontinuers$diab == 1, ])
dis_diab_yes_p <- round((dis_diab_yes_n / denom_dis)*100, 1)
dis_dep_no_n <- nrow(discontinuers[discontinuers$dep == 0, ])
dis_dep_no_p <- round((dis_dep_no_n / denom_dis)*100, 1)
dis_dep_yes_n <- nrow(discontinuers[discontinuers$dep == 1, ])
dis_dep_yes_p <- round((dis_dep_yes_n / denom_dis)*100, 1)

##Calculate numbers - continuers
continuers <- DUS_all[DUS_all$trip_group == "Continuer" | DUS_all$trip_group == "Initiator", ]
denom_con <- length(unique(continuers$pregnancy_id))
con_year_0912_n <- nrow(continuers[continuers$year_group == "2009-2012", ])
con_year_0912_p <- round((con_year_0912_n / denom_con)*100, 1)
con_year_1316_n <- nrow(continuers[continuers$year_group == "2013-2016", ])
con_year_1316_p <- round((con_year_1316_n / denom_con)*100, 1)
con_year_1720_n <- nrow(continuers[continuers$year_group == "2017-2020", ])
con_year_1720_p <- round((con_year_1720_n / denom_con)*100, 1)
con_age_1524_n <- nrow(continuers[continuers$maternal_age == "15-24", ])
con_age_1524_p <- round((con_age_1524_n / denom_con)*100, 1)
con_age_2534_n <- nrow(continuers[continuers$maternal_age == "25-34", ])
con_age_2534_p <- round((con_age_2534_n / denom_con)*100, 1)
con_age_3549_n <- nrow(continuers[continuers$maternal_age == "35+", ])
con_age_3549_p <- round((con_age_3549_n / denom_con)*100, 1)
con_age_mis_n <- nrow(continuers[continuers$maternal_age == "", ])
con_age_mis_p <- round((con_age_mis_n / denom_con)*100, 1)
con_parity_0_n <- nrow(continuers[continuers$parity == 0, ])
con_parity_0_p <- round((con_parity_0_n / denom_con)*100, 1)
con_parity_1_n <- nrow(continuers[continuers$parity > 0, ])
con_parity_1_p <- round((con_parity_1_n / denom_con)*100, 1)
con_parity_mis_n <- nrow(continuers[is.na(continuers$parity), ])
con_parity_mis_p <- round((con_parity_mis_n / denom_con)*100, 1)
con_obesity_no_n <- nrow(continuers[continuers$obesity == 0, ])
con_obesity_no_p <- round((con_obesity_no_n / denom_con)*100, 1)
con_obesity_yes_n <- nrow(continuers[continuers$obesity == 1, ])
con_obesity_yes_p <- round((con_obesity_yes_n / denom_con)*100, 1)
con_obesity_mis_n <- nrow(continuers[is.na(continuers$obesity), ])
con_obesity_mis_p <- round((con_obesity_mis_n / denom_con)*100, 1)
con_HT_no_n <- nrow(continuers[continuers$HT == 0, ])
con_HT_no_p <- round((con_HT_no_n / denom_con)*100, 1)
con_HT_yes_n <- nrow(continuers[continuers$HT == 1, ])
con_HT_yes_p <- round((con_HT_yes_n / denom_con)*100, 1)
con_diab_no_n <- nrow(continuers[continuers$diab == 0, ])
con_diab_no_p <- round((con_diab_no_n / denom_con)*100, 1)
con_diab_yes_n <- nrow(continuers[continuers$diab == 1, ])
con_diab_yes_p <- round((con_diab_yes_n / denom_con)*100, 1)
con_dep_no_n <- nrow(continuers[continuers$dep == 0, ])
con_dep_no_p <- round((con_dep_no_n / denom_con)*100, 1)
con_dep_yes_n <- nrow(continuers[continuers$dep == 1, ])
con_dep_yes_p <- round((con_dep_yes_n / denom_con)*100, 1)

##Make table 7
#With missing row for obesity
# table7 <- data.frame(
#   Baseline_maternal_characteristic = c("TOTAL N", "Calendar year of LMP", "2009-2012", "2013-2016", "2017-2020", "Maternal age at LMP", "15-24 years", "25-34 years", "35-49 years", "Missing", "Parity at LMP", "0", "1 or more", "Missing", "Obesity", "No", "Yes", "Missing", "Chronic hypertension", "No", "Yes", "Diabetes", "No", "Yes", "Depression", "No", "Yes"),
#   Total_n = c(denom_all, "", year_0912_n, year_1316_n, year_1720_n, "", age_1524_n, age_2534_n, age_3549_n, age_mis_n, "", parity_0_n, parity_1_n, parity_mis_n, "", obesity_no_n, obesity_yes_n,obesity_mis_n, "", HT_no_n, HT_yes_n, "", diab_no_n, diab_yes_n, "", dep_no_n, dep_yes_n),
#   Total_p = c("", "", year_0912_p, year_1316_p, year_1720_p, "", age_1524_p, age_2534_p, age_3549_p, age_mis_p, "", parity_0_p, parity_1_p, parity_mis_p, "", obesity_no_p, obesity_yes_p, obesity_mis_p, "", HT_no_p, HT_yes_p, "", diab_no_p, diab_yes_p, "", dep_no_p, dep_yes_p),
#   Nonusers_n = c(denom_non, "", non_year_0912_n, non_year_1316_n, non_year_1720_n, "", non_age_1524_n, non_age_2534_n, non_age_3549_n, non_age_mis_n, "", non_parity_0_n, non_parity_1_n, non_parity_mis_n, "", non_obesity_no_n, non_obesity_yes_n, non_obesity_mis_n, "", non_HT_no_n, non_HT_yes_n, "", non_diab_no_n, non_diab_yes_n, "", non_dep_no_n, non_dep_yes_n),
#   Nonusers_p = c("", "", non_year_0912_p, non_year_1316_p, non_year_1720_p, "", non_age_1524_p, non_age_2534_p, non_age_3549_p, non_age_mis_p, "", non_parity_0_p, non_parity_1_p, non_parity_mis_p, "", non_obesity_no_p, non_obesity_yes_p,non_obesity_mis_p, "", non_HT_no_p, non_HT_yes_p, "", non_diab_no_p, non_diab_yes_p, "", non_dep_no_p, non_dep_yes_p),
#   Discontinuers_n = c(denom_dis, "", dis_year_0912_n, dis_year_1316_n, dis_year_1720_n, "", dis_age_1524_n, dis_age_2534_n, dis_age_3549_n, dis_age_mis_n, "", dis_parity_0_n, dis_parity_1_n, dis_parity_mis_n, "", dis_obesity_no_n, dis_obesity_yes_n,dis_obesity_mis_n, "", dis_HT_no_n, dis_HT_yes_n, "", dis_diab_no_n, dis_diab_yes_n, "", dis_dep_no_n, dis_dep_yes_n),
#   Discontinuers_p = c("", "", dis_year_0912_p, dis_year_1316_p, dis_year_1720_p, "", dis_age_1524_p, dis_age_2534_p, dis_age_3549_p, dis_age_mis_p, "", dis_parity_0_p, dis_parity_1_p, dis_parity_mis_p, "", dis_obesity_no_p, dis_obesity_yes_p,dis_obesity_mis_p, "", dis_HT_no_p, dis_HT_yes_p, "", dis_diab_no_p, dis_diab_yes_p, "", dis_dep_no_p, dis_dep_yes_p),
#   Continuers_n = c(denom_con, "", con_year_0912_n, con_year_1316_n, con_year_1720_n, "", con_age_1524_n, con_age_2534_n, con_age_3549_n, con_age_mis_n, "", con_parity_0_n, con_parity_1_n, con_parity_mis_n, "", con_obesity_no_n, con_obesity_yes_n,con_obesity_mis_n, "", con_HT_no_n, con_HT_yes_n, "", con_diab_no_n, con_diab_yes_n, "", con_dep_no_n, con_dep_yes_n),
#   Continuers_p = c("", "", con_year_0912_p, con_year_1316_p, con_year_1720_p, "", con_age_1524_p, con_age_2534_p, con_age_3549_p, con_age_mis_p, "", con_parity_0_p, con_parity_1_p, con_parity_mis_p, "", con_obesity_no_p, con_obesity_yes_p,con_obesity_mis_p, "", con_HT_no_p, con_HT_yes_p, "", con_diab_no_p, con_diab_yes_p, "", con_dep_no_p, con_dep_yes_p)
# )
table7 <- data.frame(
  Baseline_maternal_characteristic = c("TOTAL N", "Calendar year of LMP", "2009-2012", "2013-2016", "2017-2020", "Maternal age at LMP", "15-24 years", "25-34 years", "35-49 years", "Missing", "Parity at LMP", "0", "1 or more", "Missing", "Obesity", "No", "Yes", "Chronic hypertension", "No", "Yes", "Diabetes", "No", "Yes", "Depression", "No", "Yes"),
  Total_n = c(denom_all, "", year_0912_n, year_1316_n, year_1720_n, "", age_1524_n, age_2534_n, age_3549_n, age_mis_n, "", parity_0_n, parity_1_n, parity_mis_n, "", obesity_no_n, obesity_yes_n, "", HT_no_n, HT_yes_n, "", diab_no_n, diab_yes_n, "", dep_no_n, dep_yes_n),
  Total_p = c("", "", year_0912_p, year_1316_p, year_1720_p, "", age_1524_p, age_2534_p, age_3549_p, age_mis_p, "", parity_0_p, parity_1_p, parity_mis_p, "", obesity_no_p, obesity_yes_p, "", HT_no_p, HT_yes_p, "", diab_no_p, diab_yes_p, "", dep_no_p, dep_yes_p),
  Nonusers_n = c(denom_non, "", non_year_0912_n, non_year_1316_n, non_year_1720_n, "", non_age_1524_n, non_age_2534_n, non_age_3549_n, non_age_mis_n, "", non_parity_0_n, non_parity_1_n, non_parity_mis_n, "", non_obesity_no_n, non_obesity_yes_n, "", non_HT_no_n, non_HT_yes_n, "", non_diab_no_n, non_diab_yes_n, "", non_dep_no_n, non_dep_yes_n),
  Nonusers_p = c("", "", non_year_0912_p, non_year_1316_p, non_year_1720_p, "", non_age_1524_p, non_age_2534_p, non_age_3549_p, non_age_mis_p, "", non_parity_0_p, non_parity_1_p, non_parity_mis_p, "", non_obesity_no_p, non_obesity_yes_p, "", non_HT_no_p, non_HT_yes_p, "", non_diab_no_p, non_diab_yes_p, "", non_dep_no_p, non_dep_yes_p),
  Discontinuers_n = c(denom_dis, "", dis_year_0912_n, dis_year_1316_n, dis_year_1720_n, "", dis_age_1524_n, dis_age_2534_n, dis_age_3549_n, dis_age_mis_n, "", dis_parity_0_n, dis_parity_1_n, dis_parity_mis_n, "", dis_obesity_no_n, dis_obesity_yes_n, "", dis_HT_no_n, dis_HT_yes_n, "", dis_diab_no_n, dis_diab_yes_n, "", dis_dep_no_n, dis_dep_yes_n),
  Discontinuers_p = c("", "", dis_year_0912_p, dis_year_1316_p, dis_year_1720_p, "", dis_age_1524_p, dis_age_2534_p, dis_age_3549_p, dis_age_mis_p, "", dis_parity_0_p, dis_parity_1_p, dis_parity_mis_p, "", dis_obesity_no_p, dis_obesity_yes_p, "", dis_HT_no_p, dis_HT_yes_p, "", dis_diab_no_p, dis_diab_yes_p, "", dis_dep_no_p, dis_dep_yes_p),
  Continuers_n = c(denom_con, "", con_year_0912_n, con_year_1316_n, con_year_1720_n, "", con_age_1524_n, con_age_2534_n, con_age_3549_n, con_age_mis_n, "", con_parity_0_n, con_parity_1_n, con_parity_mis_n, "", con_obesity_no_n, con_obesity_yes_n, "", con_HT_no_n, con_HT_yes_n, "", con_diab_no_n, con_diab_yes_n, "", con_dep_no_n, con_dep_yes_n),
  Continuers_p = c("", "", con_year_0912_p, con_year_1316_p, con_year_1720_p, "", con_age_1524_p, con_age_2534_p, con_age_3549_p, con_age_mis_p, "", con_parity_0_p, con_parity_1_p, con_parity_mis_p, "", con_obesity_no_p, con_obesity_yes_p, "", con_HT_no_p, con_HT_yes_p, "", con_diab_no_p, con_diab_yes_p, "", con_dep_no_p, con_dep_yes_p)
)

write.csv(table7, file.path(csv_path, "Table7.csv"), row.names = FALSE)  ##Path to output folder

# rm(list = ls.str(mode = "numeric"))
# rm(list = ls()[! ls() %in% c("table7", "DUS_all", "replace_na", "scoreci")])


############# Table 8 #########
cat("\nCreating Table 8 ...\n")

#Calculate numbers - total cohort
denom_all <- length(unique(DUS_all$pregnancy_id))
MIG_T1_n <- nrow(DUS_all[DUS_all$MIG_T1 == 1, ])
MIG_T1_p <- round((MIG_T1_n / denom_all)*100, 1)
MIG_T2_n <- nrow(DUS_all[DUS_all$MIG_T2 == 1, ])
MIG_T2_p <- round((MIG_T2_n / denom_all)*100, 1)
MIG_T3_n <- nrow(DUS_all[DUS_all$MIG_T3 == 1, ])
MIG_T3_p <- round((MIG_T3_n / denom_all)*100, 1)
MIG_T4_n <- nrow(DUS_all[DUS_all$MIG_T4 == 1, ])
MIG_T4_p <- round((MIG_T4_n / denom_all)*100, 1)
MIG_T5_n <- nrow(DUS_all[DUS_all$MIG_T5 == 1, ])
MIG_T5_p <- round((MIG_T5_n / denom_all)*100, 1)
MIG_T6_n <- nrow(DUS_all[DUS_all$MIG_T6 == 1, ])
MIG_T6_p <- round((MIG_T6_n / denom_all)*100, 1)
MIG_S1_n <- nrow(DUS_all[DUS_all$MIG_S1 == 1, ])
MIG_S1_p <- round((MIG_S1_n / denom_all)*100, 1)
MIG_S2_n <- nrow(DUS_all[DUS_all$MIG_S2 == 1, ])
MIG_S2_p <- round((MIG_S2_n / denom_all)*100, 1)
MIG_S3_n <- nrow(DUS_all[DUS_all$MIG_S3 == 1, ])
MIG_S3_p <- round((MIG_S3_n / denom_all)*100, 1)
MIG_S4_n <- nrow(DUS_all[DUS_all$MIG_S4 == 1, ])
MIG_S4_p <- round((MIG_S4_n / denom_all)*100, 1)

#Calculate numbers - non-users
nonusers <- DUS_all[DUS_all$trip_group == "Non-user", ]
denom_non <- length(unique(nonusers$pregnancy_id))
non_MIG_T1_n <- nrow(nonusers[nonusers$MIG_T1 == 1, ])
non_MIG_T1_p <- round((non_MIG_T1_n / denom_non)*100, 1)
non_MIG_T2_n <- nrow(nonusers[nonusers$MIG_T2 == 1, ])
non_MIG_T2_p <- round((non_MIG_T2_n / denom_non)*100, 1)
non_MIG_T3_n <- nrow(nonusers[nonusers$MIG_T3 == 1, ])
non_MIG_T3_p <- round((non_MIG_T3_n / denom_non)*100, 1)
non_MIG_T4_n <- nrow(nonusers[nonusers$MIG_T4 == 1, ])
non_MIG_T4_p <- round((non_MIG_T4_n / denom_non)*100, 1)
non_MIG_T5_n <- nrow(nonusers[nonusers$MIG_T5 == 1, ])
non_MIG_T5_p <- round((non_MIG_T5_n / denom_non)*100, 1)
non_MIG_T6_n <- nrow(nonusers[nonusers$MIG_T6 == 1, ])
non_MIG_T6_p <- round((non_MIG_T6_n / denom_non)*100, 1)
non_MIG_S1_n <- nrow(nonusers[nonusers$MIG_S1 == 1, ])
non_MIG_S1_p <- round((non_MIG_S1_n / denom_non)*100, 1)
non_MIG_S2_n <- nrow(nonusers[nonusers$MIG_S2 == 1, ])
non_MIG_S2_p <- round((non_MIG_S2_n / denom_non)*100, 1)
non_MIG_S3_n <- nrow(nonusers[nonusers$MIG_S3 == 1, ])
non_MIG_S3_p <- round((non_MIG_S3_n / denom_non)*100, 1)
non_MIG_S4_n <- nrow(nonusers[nonusers$MIG_S4 == 1, ])
non_MIG_S4_p <- round((non_MIG_S4_n / denom_non)*100, 1)

#Calculate numbers - discontinuers
discontinuers <- DUS_all[DUS_all$trip_group == "Discontinuer", ]
denom_dis <- length(unique(discontinuers$pregnancy_id))
dis_MIG_T1_n <- nrow(discontinuers[discontinuers$MIG_T1 == 1, ])
dis_MIG_T1_p <- round((dis_MIG_T1_n / denom_dis)*100, 1)
dis_MIG_T2_n <- nrow(discontinuers[discontinuers$MIG_T2 == 1, ])
dis_MIG_T2_p <- round((dis_MIG_T2_n / denom_dis)*100, 1)
dis_MIG_T3_n <- nrow(discontinuers[discontinuers$MIG_T3 == 1, ])
dis_MIG_T3_p <- round((dis_MIG_T3_n / denom_dis)*100, 1)
dis_MIG_T4_n <- nrow(discontinuers[discontinuers$MIG_T4 == 1, ])
dis_MIG_T4_p <- round((dis_MIG_T4_n / denom_dis)*100, 1)
dis_MIG_T5_n <- nrow(discontinuers[discontinuers$MIG_T5 == 1, ])
dis_MIG_T5_p <- round((dis_MIG_T5_n / denom_dis)*100, 1)
dis_MIG_T6_n <- nrow(discontinuers[discontinuers$MIG_T6 == 1, ])
dis_MIG_T6_p <- round((dis_MIG_T6_n / denom_dis)*100, 1)
dis_MIG_S1_n <- nrow(discontinuers[discontinuers$MIG_S1 == 1, ])
dis_MIG_S1_p <- round((dis_MIG_S1_n / denom_dis)*100, 1)
dis_MIG_S2_n <- nrow(discontinuers[discontinuers$MIG_S2 == 1, ])
dis_MIG_S2_p <- round((dis_MIG_S2_n / denom_dis)*100, 1)
dis_MIG_S3_n <- nrow(discontinuers[discontinuers$MIG_S3 == 1, ])
dis_MIG_S3_p <- round((dis_MIG_S3_n / denom_dis)*100, 1)
dis_MIG_S4_n <- nrow(discontinuers[discontinuers$MIG_S4 == 1, ])
dis_MIG_S4_p <- round((dis_MIG_S4_n / denom_dis)*100, 1)

#Calculate numbers - continuers
continuers <- DUS_all[DUS_all$trip_group == "Continuer" | DUS_all$trip_group == "Initiator", ]
denom_con <- length(unique(continuers$pregnancy_id))
con_MIG_T1_n <- nrow(continuers[continuers$MIG_T1 == 1, ])
con_MIG_T1_p <- round((con_MIG_T1_n / denom_con)*100, 1)
con_MIG_T2_n <- nrow(continuers[continuers$MIG_T2 == 1, ])
con_MIG_T2_p <- round((con_MIG_T2_n / denom_con)*100, 1)
con_MIG_T3_n <- nrow(continuers[continuers$MIG_T3 == 1, ])
con_MIG_T3_p <- round((con_MIG_T3_n / denom_con)*100, 1)
con_MIG_T4_n <- nrow(continuers[continuers$MIG_T4 == 1, ])
con_MIG_T4_p <- round((con_MIG_T4_n / denom_con)*100, 1)
con_MIG_T5_n <- nrow(continuers[continuers$MIG_T5 == 1, ])
con_MIG_T5_p <- round((con_MIG_T5_n / denom_con)*100, 1)
con_MIG_T6_n <- nrow(continuers[continuers$MIG_T6 == 1, ])
con_MIG_T6_p <- round((con_MIG_T6_n / denom_con)*100, 1)
con_MIG_S1_n <- nrow(continuers[continuers$MIG_S1 == 1, ])
con_MIG_S1_p <- round((con_MIG_S1_n / denom_con)*100, 1)
con_MIG_S2_n <- nrow(continuers[continuers$MIG_S2 == 1, ])
con_MIG_S2_p <- round((con_MIG_S2_n / denom_con)*100, 1)
con_MIG_S3_n <- nrow(continuers[continuers$MIG_S3 == 1, ])
con_MIG_S3_p <- round((con_MIG_S3_n / denom_con)*100, 1)
con_MIG_S4_n <- nrow(continuers[continuers$MIG_S4 == 1, ])
con_MIG_S4_p <- round((con_MIG_S4_n / denom_con)*100, 1)

#Make table 8
table8 <- data.frame(
  Migraine_type_severity = c("TOTAL N", "Migraine type prior to LMP", "Migraine with aura", "Migraine without aura", "Status migrainosus", "Complicated migraine", "Other migraine", "Unspecified migraine", "Migraine severity prior to LMP", "Mild", "Moderate", "Severe", "Very severe"),
  Total_n = c(denom_all, "", MIG_T1_n, MIG_T2_n, MIG_T3_n, MIG_T4_n, MIG_T5_n, MIG_T6_n, "", MIG_S1_n, MIG_S2_n, MIG_S3_n, MIG_S4_n),
  Total_p = c("", "", MIG_T1_p, MIG_T2_p, MIG_T3_p, MIG_T4_p, MIG_T5_p, MIG_T6_p, "", MIG_S1_p, MIG_S2_p, MIG_S3_p, MIG_S4_p),
  Nonusers_n = c(denom_non, "", non_MIG_T1_n, non_MIG_T2_n, non_MIG_T3_n, non_MIG_T4_n, non_MIG_T5_n, non_MIG_T6_n, "", non_MIG_S1_n, non_MIG_S2_n, non_MIG_S3_n, non_MIG_S4_n),
  Nonusers_p = c("", "", non_MIG_T1_p, non_MIG_T2_p, non_MIG_T3_p, non_MIG_T4_p, non_MIG_T5_p, non_MIG_T6_p, "", non_MIG_S1_p, non_MIG_S2_p, non_MIG_S3_p, non_MIG_S4_p),
  Discontinuers_n = c(denom_dis, "", dis_MIG_T1_n, dis_MIG_T2_n, dis_MIG_T3_n, dis_MIG_T4_n, dis_MIG_T5_n, dis_MIG_T6_n, "", dis_MIG_S1_n, dis_MIG_S2_n, dis_MIG_S3_n, dis_MIG_S4_n),
  Discontinuers_p = c("", "", dis_MIG_T1_p, dis_MIG_T2_p, dis_MIG_T3_p, dis_MIG_T4_p, dis_MIG_T5_p, dis_MIG_T6_p, "", dis_MIG_S1_p, dis_MIG_S2_p, dis_MIG_S3_p, dis_MIG_S4_p),
  Continuers_n = c(denom_con, "", con_MIG_T1_n, con_MIG_T2_n, con_MIG_T3_n, con_MIG_T4_n, con_MIG_T5_n, con_MIG_T6_n, "", con_MIG_S1_n, con_MIG_S2_n, con_MIG_S3_n, con_MIG_S4_n),
  Continuers_p = c("", "", con_MIG_T1_p, con_MIG_T2_p, con_MIG_T3_p, con_MIG_T4_p, con_MIG_T5_p, con_MIG_T6_p, "", con_MIG_S1_p, con_MIG_S2_p, con_MIG_S3_p, con_MIG_S4_p)
)

write.csv(table8, file.path(csv_path, "Table8.csv"), row.names = FALSE)  ##Path to output folder

# rm(list = ls.str(mode = "numeric"))
# rm(list = ls()[! ls() %in% c("table8", "DUS_all", "replace_na", "scoreci")])


############# Table 9 #########
cat("\nCreating Table 9 ...\n")

##Numbers for table - Discontinuers
discontinuers <- DUS_all[DUS_all$trip_group == "Discontinuer", ]
dis_trip_prior_0_12_median <- median(discontinuers$trip_prior_0_12_n)
dis_trip_prior_0_12_Q1 <- quantile(discontinuers$trip_prior_0_12_n, prob = c(0.25))
dis_trip_prior_0_12_Q3 <- quantile(discontinuers$trip_prior_0_12_n, prob = c(0.75))
dis_trip_prior_0_3_median <- median(discontinuers$trip_prior_0_3_n)
dis_trip_prior_0_3_Q1 <- quantile(discontinuers$trip_prior_0_3_n, prob = c(0.25))
dis_trip_prior_0_3_Q3 <- quantile(discontinuers$trip_prior_0_3_n, prob = c(0.75))

#Numbers for table - Continuers
continuers <- DUS_all[DUS_all$trip_group == "Continuer" | DUS_all$trip_group == "Initiator", ]
con_trip_prior_0_12_median <- median(continuers$trip_prior_0_12_n)
con_trip_prior_0_12_Q1 <- quantile(continuers$trip_prior_0_12_n, prob = c(0.25))
con_trip_prior_0_12_Q3 <- quantile(continuers$trip_prior_0_12_n, prob = c(0.75))
con_trip_prior_0_3_median <- median(continuers$trip_prior_0_3_n)
con_trip_prior_0_3_Q1 <- quantile(continuers$trip_prior_0_3_n, prob = c(0.25))
con_trip_prior_0_3_Q3 <- quantile(continuers$trip_prior_0_3_n, prob = c(0.75))
con_trip_preg_median <- median(continuers$trip_preg_n)
con_trip_preg_Q1 <- quantile(continuers$trip_preg_n, prob = c(0.25))
con_trip_preg_Q3 <- quantile(continuers$trip_preg_n, prob = c(0.75))
con_trip_T1_median <- median(continuers$trip_T1_n)
con_trip_T1_Q1 <- quantile(continuers$trip_T1_n, prob = c(0.25))
con_trip_T1_Q3 <- quantile(continuers$trip_T1_n, prob = c(0.75))
con_trip_T2_median <- median(continuers$trip_T2_n)
con_trip_T2_Q1 <- quantile(continuers$trip_T2_n, prob = c(0.25))
con_trip_T2_Q3 <- quantile(continuers$trip_T2_n, prob = c(0.75))
con_trip_T3_median <- median(continuers$trip_T3_n)
con_trip_T3_Q1 <- quantile(continuers$trip_T3_n, prob = c(0.25))
con_trip_T3_Q3 <- quantile(continuers$trip_T3_n, prob = c(0.75))

#Make table 9
table9 <- data.frame(
  Window_of_exposure = c("0-12 months prior to LMP", "0-3 months prior to LMP", "Any time in pregnancy", "Trimester 1", "Trimester 2", "Trimester 3"),
  Discontinuers_median = c(dis_trip_prior_0_12_median, dis_trip_prior_0_3_median, "NA", "NA", "NA", "NA"),
  Discontinuers_p25 = c(dis_trip_prior_0_12_Q1, dis_trip_prior_0_3_Q1, "NA", "NA", "NA", "NA"),
  Discontinuers_p75 = c(dis_trip_prior_0_12_Q3, dis_trip_prior_0_3_Q3, "NA", "NA", "NA", "NA"),
  Continuers_median = c(con_trip_prior_0_12_median, con_trip_prior_0_3_median, con_trip_preg_median, con_trip_T1_median, con_trip_T2_median, con_trip_T3_median),
  Continuers_p25 = c(con_trip_prior_0_12_Q1, con_trip_prior_0_3_Q1, con_trip_preg_Q1, con_trip_T1_Q1, con_trip_T2_Q1, con_trip_T3_Q1),
  Continuers_p75 = c(con_trip_prior_0_12_Q3, con_trip_prior_0_3_Q3, con_trip_preg_Q3, con_trip_T1_Q3, con_trip_T2_Q3, con_trip_T3_Q3)
)

write.csv(table9, file.path(csv_path, "Table9.csv"), row.names = FALSE)  ##Path to output folder

# rm(list = ls.str(mode = "numeric"))
# rm(list = ls()[! ls() %in% c("table9", "DUS_all", "replace_na", "scoreci")])


############# Table 10 #########
cat("\nCreating Table 10 ...\n")

conserv <- DUS_all[DUS_all$trip_preg_c == 1, ]
denom_cons <- length(unique(conserv$pregnancy_id))
MIG_T1_n <- nrow(conserv[conserv$MIG_T1 == 1, ])
MIG_T1_p <- round((MIG_T1_n / denom_cons)*100, 1)
MIG_T2_n <- nrow(conserv[conserv$MIG_T2 == 1, ])
MIG_T2_p <- round((MIG_T2_n / denom_cons)*100, 1)
MIG_T3_n <- nrow(conserv[conserv$MIG_T3 == 1, ])
MIG_T3_p <- round((MIG_T3_n / denom_cons)*100, 1)
MIG_T4_n <- nrow(conserv[conserv$MIG_T4 == 1, ])
MIG_T4_p <- round((MIG_T4_n / denom_cons)*100, 1)
MIG_T5_n <- nrow(conserv[conserv$MIG_T5 == 1, ])
MIG_T5_p <- round((MIG_T5_n / denom_cons)*100, 1)
MIG_T6_n <- nrow(conserv[conserv$MIG_T6 == 1, ])
MIG_T6_p <- round((MIG_T6_n / denom_cons)*100, 1)
MIG_S1_n <- nrow(conserv[conserv$MIG_S1 == 1, ])
MIG_S1_p <- round((MIG_S1_n / denom_cons)*100, 1)
MIG_S2_n <- nrow(conserv[conserv$MIG_S2 == 1, ])
MIG_S2_p <- round((MIG_S2_n / denom_cons)*100, 1)
MIG_S3_n <- nrow(conserv[conserv$MIG_S3 == 1, ])
MIG_S3_p <- round((MIG_S3_n / denom_cons)*100, 1)
MIG_S4_n <- nrow(conserv[conserv$MIG_S4 == 1, ])
MIG_S4_p <- round((MIG_S4_n / denom_cons)*100, 1)

table10 <- data.frame(
  Migraine_type_severity = c("TOTAL N", "Migraine type prior to LMP", "Migraine with aura", "Migraine without aura", "Status migrainosus", "Complicated migraine", "Other migraine", "Unspecified migraine", "Migraine severity prior to LMP", "Mild", "Moderate", "Severe", "Very severe"),
  Number = c(denom_cons, "", MIG_T1_n, MIG_T2_n, MIG_T3_n, MIG_T4_n, MIG_T5_n, MIG_T6_n, "", MIG_S1_n, MIG_S2_n, MIG_S3_n, MIG_S4_n),
  Percentage = c("", "", MIG_T1_p, MIG_T2_p, MIG_T3_p, MIG_T4_p, MIG_T5_p, MIG_T6_p, "", MIG_S1_p, MIG_S2_p, MIG_S3_p, MIG_S4_p)
)

write.csv(table10, file.path(csv_path, "Table10.csv"), row.names = FALSE)  ##Path to output folder

# rm(list = ls()[!ls() %in% c("table10")])





