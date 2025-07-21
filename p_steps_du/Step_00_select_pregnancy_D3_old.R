initial_time_00<-Sys.time()
date_running_start_00<-Sys.Date()
#Load study parameters
source(paste0(pre_dir,"info/DAP_info.R"))
source(paste0(pre_dir,"parameters/study_parameters.R"))
source(paste0(pre_dir,"functions/create_age_band.R"))

from_pregnancy_study_population<-function(x){
  du_preg_fl<-list.files(file.path(dir_DP1_algorithm, "g_intermediate","pregnancy_d3"),x)  
  as.data.table(readRDS(file.path(dir_DP1_algorithm, "g_intermediate","pregnancy_d3", du_preg_fl)))
}

from_DP4_algorithm<-function(path_dir,x){
  if(length(path_dir)>1) path_dir = paste0(path_dir, collapse = "/")
  mig_preg_fl<-list.files(file.path(dir_DP1_algorithm,"g_intermediate", path_dir),x)
  if(length(mig_preg_fl)>0){
    as.data.table(readRDS(file.path(dir_DP1_algorithm,"g_intermediate", path_dir, mig_preg_fl)))  
  }else{
    data.table() # empty (length zero) data.table
  }
  
}

if(after_delivery_du>0){
#Load DU pregnancy cohort
du_preg_cohort<-from_pregnancy_study_population("DU_Pregnancy_D3")

#remove unnecessary columns
if("gdm_pe_filter" %in% names(du_preg_cohort)){du_preg_cohort[,gdm_pe_filter:=NULL]}
if("mig_filter" %in% names(du_preg_cohort)){du_preg_cohort[,mig_filter:=NULL]}
if("du_filter" %in% names(du_preg_cohort)){du_preg_cohort[,du_filter:=NULL]}
if("saf_filter" %in% names(du_preg_cohort)){du_preg_cohort[,saf_filter:=NULL]}
if("op_end_date_gdm_pe" %in% names(du_preg_cohort)){du_preg_cohort[,op_end_date_gdm_pe:=NULL]}
if("op_end_date_mig" %in% names(du_preg_cohort)){du_preg_cohort[,op_end_date_mig:=NULL]}
if("op_end_date_saf" %in% names(du_preg_cohort)){du_preg_cohort[,op_end_date_saf:=NULL]}
if("op_start_date_gdm_pe" %in% names(du_preg_cohort)){du_preg_cohort[,op_start_date_gdm_pe:=NULL]}
if("op_start_date_mig" %in% names(du_preg_cohort)){du_preg_cohort[,op_start_date_mig:=NULL]}
if("op_start_date_saf" %in% names(du_preg_cohort)){du_preg_cohort[,op_start_date_saf:=NULL]}
orig_du<-du_preg_cohort[,.N]
}else{
  orig_du<-0  
}
#Load MIG pregnancy cohort (onset migraine already removed)
mig_preg_cohort<-from_DP4_algorithm("pregnancy_d3","MIG_Pregnancy_D3")

#remove unnecessary columns
if("op_start_date_mig" %in% names(mig_preg_cohort)){mig_preg_cohort[,op_start_date_mig:=NULL]}
if("op_end_date_mig" %in% names(mig_preg_cohort)){mig_preg_cohort[,op_end_date_mig:=NULL]}
orig_mig<-mig_preg_cohort[,.N]



if(after_delivery_du>0){
  cols<-intersect(names(du_preg_cohort), names(mig_preg_cohort))
  preg_D3<-merge.data.table(du_preg_cohort, mig_preg_cohort, by=cols)
  rm(du_preg_cohort, mig_preg_cohort)
}else{
  preg_D3<-mig_preg_cohort
  rm(mig_preg_cohort)
}
preg_d3_rows<-preg_D3[,.N]

#Create flowchart
Indicator<-c("1.0. Number of original records from the DU pregnancy cohort (if observation time after delivery is available)",
             "1.1. Number of original records from the Migraine pregnancy cohort (all onset MG have been excluded)",
             "1.2. Number of records after merging DU and MIG pregnancy cohorts (if observation time after delivery is available)")
placeholder<-c(orig_du,orig_mig,preg_d3_rows)
flowchart<-data.table(Indicator=Indicator, Count=placeholder)
rm(Indicator,placeholder)
rm(orig_du,orig_mig,preg_d3_rows)

fwrite(flowchart,paste0(output_dir, "Drug utilisation/flowchart/Step_00_study_population_flowchart.csv"), row.names = F)
rm(flowchart)

preg_D3_during = preg_D3_b = copy(preg_D3)

###################################################
############ 12 month lookback period ##############

####create covariates: Migraine type####
#Type 1
mig_type_1 = from_DP4_algorithm("migraine_algorithm/final_d3","MIG_T1_a_D3")
if(length(mig_type_1)>0){
  cols<-c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date", "include")
  mig_type_1<-mig_type_1[,cols,with=F]
  preg_D3<-merge.data.table(preg_D3, mig_type_1, by=c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date"), all.x = T)
  setnames(preg_D3, "include", "MIG_T1")
  preg_D3[is.na(MIG_T1), MIG_T1:=0]
  rm(mig_type_1)
}else{
  preg_D3[,MIG_T1:=0]
}

#Type 2
mig_type_2<-from_DP4_algorithm("migraine_algorithm/final_d3","MIG_T2_a_D3")
if(length(mig_type_2)>0){
  cols<-c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date", "include")
  mig_type_2<-mig_type_2[,cols,with=F]
  preg_D3<-merge.data.table(preg_D3, mig_type_2, by=c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date"), all.x = T)
  setnames(preg_D3, "include", "MIG_T2")
  preg_D3[is.na(MIG_T2), MIG_T2:=0]
  rm(mig_type_2)
}else{
  preg_D3[,MIG_T2:=0]
}

#Type 3
mig_type_3<-from_DP4_algorithm("migraine_algorithm/final_d3","MIG_T3_a_D3")
if(length(mig_type_3)>0){
  cols<-c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date", "include")
  mig_type_3<-mig_type_3[,cols,with=F]
  preg_D3<-merge.data.table(preg_D3, mig_type_3, by=c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date"), all.x = T)
  setnames(preg_D3, "include", "MIG_T3")
  preg_D3[is.na(MIG_T3), MIG_T3:=0]
  rm(mig_type_3)
}else{
  preg_D3[,MIG_T3:=0]
}

#Type 4
mig_type_4<-from_DP4_algorithm("migraine_algorithm/final_d3","MIG_T4_a_D3")
if(length(mig_type_4)>0){
  cols<-c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date", "include")
  mig_type_4<-mig_type_4[,cols,with=F]
  preg_D3<-merge.data.table(preg_D3, mig_type_4, by=c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date"), all.x = T)
  setnames(preg_D3, "include", "MIG_T4")
  preg_D3[is.na(MIG_T4), MIG_T4:=0]
  rm(mig_type_4)
}else{
  preg_D3[,MIG_T4:=0]
}

#Type 5
mig_type_5<-from_DP4_algorithm("migraine_algorithm/final_d3","MIG_T5_a_D3")
if(length(mig_type_5)>0){
  cols<-c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date", "include")
  mig_type_5<-mig_type_5[,cols,with=F]
  preg_D3<-merge.data.table(preg_D3, mig_type_5, by=c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date"), all.x = T)
  setnames(preg_D3, "include", "MIG_T5")
  preg_D3[is.na(MIG_T5), MIG_T5:=0]
  rm(mig_type_5)
}else{
  preg_D3[,MIG_T5:=0]
}

#Type 6
mig_type_6<-from_DP4_algorithm("migraine_algorithm/final_d3","MIG_T6_a_D3")
if(length(mig_type_6)>0){
  cols<-c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date", "include")
  mig_type_6<-mig_type_6[,cols,with=F]
  preg_D3<-merge.data.table(preg_D3, mig_type_6, by=c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date"), all.x = T)
  setnames(preg_D3, "include", "MIG_T6")
  preg_D3[is.na(MIG_T6), MIG_T6:=0]
  rm(mig_type_6)
}else{
  preg_D3[,MIG_T6:=0]
}

####create covariates: Migraine severity####
#Severity 1
mig_severity_1<-from_DP4_algorithm("migraine_algorithm/final_d3","MIG_S1_a_D3")
if(length(mig_severity_1)>0){
  cols<-c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date", "include")
  mig_severity_1<-mig_severity_1[,cols,with=F]
  preg_D3<-merge.data.table(preg_D3, mig_severity_1, by=c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date"), all.x = T)
  setnames(preg_D3, "include", "MIG_S1")
  preg_D3[is.na(MIG_S1), MIG_S1:=0]
  rm(mig_severity_1)
}else{
  preg_D3[,MIG_S1:=0]
}

#Severity 2
mig_severity_2<-from_DP4_algorithm("migraine_algorithm/final_d3","MIG_S2_a_D3")
if(length(mig_severity_2)>0){
  cols<-c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date", "include")
  mig_severity_2<-mig_severity_2[,cols,with=F]
  preg_D3<-merge.data.table(preg_D3, mig_severity_2, by=c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date"), all.x = T)
  setnames(preg_D3, "include", "MIG_S2")
  preg_D3[is.na(MIG_S2), MIG_S2:=0]
  rm(mig_severity_2)
}else{
  preg_D3[,MIG_S2:=0]
}

#Severity 3
mig_severity_3<-from_DP4_algorithm("migraine_algorithm/final_d3","MIG_S3_a_D3")
if(length(mig_severity_3)>0){
  cols<-c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date", "include")
  mig_severity_3<-mig_severity_3[,cols,with=F]
  preg_D3<-merge.data.table(preg_D3, mig_severity_3, by=c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date"), all.x = T)
  setnames(preg_D3, "include", "MIG_S3")
  preg_D3[is.na(MIG_S3), MIG_S3:=0]
  rm(mig_severity_3)
}else{
  preg_D3[,MIG_S3:=0]
}

#Severity 4
mig_severity_4<-from_DP4_algorithm("migraine_algorithm/final_d3","MIG_S4_during_D3")
if(length(mig_severity_4)>0){
  cols<-c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date", "include")
  mig_severity_4<-mig_severity_4[,cols,with=F]
  preg_D3<-merge.data.table(preg_D3, mig_severity_4, by=c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date"), all.x = T)
  setnames(preg_D3, "include", "MIG_S4")
  preg_D3[is.na(MIG_S4), MIG_S4:=0]
  rm(mig_severity_4)
}else{
  preg_D3[,MIG_S4:=0]
}


#Dx
mig_tmp = from_DP4_algorithm("migraine_algorithm/final_d3","MIG_Dx_a_D3")
if(length(mig_tmp)>0){
  cols<-c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date", "include")
  mig_tmp<-mig_tmp[,cols,with=F]
  preg_D3<-merge.data.table(preg_D3, mig_tmp, by=c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date"), all.x = T)
  setnames(preg_D3, "include", "MIG_Dx")
  preg_D3[is.na(MIG_Dx), MIG_Dx:=0]
  rm(mig_tmp)
}else{
  preg_D3[,MIG_Dx:=0]
}

#DxRx
mig_tmp = from_DP4_algorithm("migraine_algorithm/final_d3","MIG_DxRx_a_D3")
if(length(mig_tmp)>0){
  cols<-c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date", "alternative")
  mig_tmp<-mig_tmp[,cols,with=F]
  preg_D3<-merge.data.table(preg_D3, mig_tmp, by=c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date"), all.x = T)
  setnames(preg_D3, "alternative", "MIG_DxRx")
  preg_D3[is.na(MIG_DxRx), MIG_DxRx:=0]
  rm(mig_tmp)
}else{
  preg_D3[,MIG_DxRx:=0]
}

#Dx_S
mig_tmp = from_DP4_algorithm("migraine_algorithm_sensitivity","MIG_Dx_a_D3.rds")
if(length(mig_tmp)>0){
  cols<-c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date", "alternative")
  mig_tmp<-mig_tmp[,cols,with=F]
  preg_D3<-merge.data.table(preg_D3, mig_tmp, by=c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date"), all.x = T)
  setnames(preg_D3, "alternative", "MIG_Dx_S")
  preg_D3[is.na(MIG_Dx_S), MIG_Dx_S:=0]
  rm(mig_tmp)
}else{
  preg_D3[,MIG_Dx_S:=0]
}

#DxRx_S
mig_tmp = from_DP4_algorithm("migraine_algorithm_sensitivity","MIG_DxRx_a_D3.rds")
if(length(mig_tmp)>0){
  cols<-c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date", "alternative")
  mig_tmp<-mig_tmp[,cols,with=F]
  preg_D3<-merge.data.table(preg_D3, mig_tmp, by=c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date"), all.x = T)
  setnames(preg_D3, "alternative", "MIG_DxRx_S")
  preg_D3[is.na(MIG_DxRx_S), MIG_DxRx_S:=0]
  rm(mig_tmp)
}else{
  preg_D3[,MIG_DxRx_S:=0]
}


#Recalculate maternal age and year group
if("year_group" %in% colnames(preg_D3)){preg_D3[,year_group:=NULL]}
if("maternal_age" %in% colnames(preg_D3)){preg_D3[,maternal_age:=NULL]}

age_band<-c(15,25,34)
preg_D3[,maternal_age:=lapply(.SD, function(x) create_age_band(x, age_band)), .SDcols = "age"]
preg_D3[is.na(maternal_age), maternal_age:="35+"]

year_group<-c(2009,2013,2017,2020)
preg_D3[,year_group:=lapply(.SD, function(x) create_age_band(x, year_group)), .SDcols = "year"]
preg_D3[is.na(year_group), year_group:="2020+"]


#### Save preg cohort ####
#save pregnancy population to g_intermediate
saveRDS(preg_D3,paste0(g_intermediate,"pregnancy_D3/pregnancy_D3_DU.rds"))

#Create an observation period template for each person_id + pregnancy_id to be used when filtering events and medicines
#Clean up the file
opt_lookback<-opt_lookback_du
after_delivery<-after_delivery_du
#The max lookback for all SAP is 12 month or 3 months(depending on the DAP)
preg_D3[,obs_min:=pregnancy_start_date - opt_lookback]
preg_D3[,obs_max:=pregnancy_end_date + after_delivery]
preg_D3<-preg_D3[,c("person_id", "obs_min", "obs_max")]
preg_D3[, min_filter:= obs_min == min(obs_min), by=person_id]
preg_D3[, max_filter:= obs_max == max(obs_max), by=person_id]
min_obs<-preg_D3[min_filter==T, c("person_id","obs_min")]
max_obs<-preg_D3[max_filter==T, c("person_id","obs_max")]
rm(preg_D3)
min_obs<-merge.data.table(min_obs, max_obs, by="person_id", all=T)
rm(max_obs)

# if(file.exists(paste0(projectFolder, "/g_intermediate/pregnancy_d3/obs_period_hint.rds"))){
#   file.rename(paste0(projectFolder, "/g_intermediate/pregnancy_d3/obs_period_hint.rds"),
#               paste0(projectFolder, "/g_intermediate/pregnancy_d3/obs_period_hint_mig.rds"))
# }

# save and use another obs_period_hint rds file for DU study
saveRDS(min_obs, paste0(projectFolder, "/g_intermediate/pregnancy_d3/obs_period_hint_DU.rds"))


#################### [DONE] #######################




###################################################
############ 3 month lookback period ##############

preg_D3 = preg_D3_b

####create covariates: Migraine type####
#Type 1
mig_type_1 = from_DP4_algorithm("migraine_algorithm/final_d3","MIG_T1_b_D3")
if(length(mig_type_1)>0){
  cols<-c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date", "include")
  mig_type_1<-mig_type_1[,cols,with=F]
  preg_D3<-merge.data.table(preg_D3, mig_type_1, by=c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date"), all.x = T)
  setnames(preg_D3, "include", "MIG_T1")
  preg_D3[is.na(MIG_T1), MIG_T1:=0]
  rm(mig_type_1)
}else{
  preg_D3[,MIG_T1:=0]
}


#Type 2
mig_type_2<-from_DP4_algorithm("migraine_algorithm/final_d3","MIG_T2_b_D3")
if(length(mig_type_2)>0){
  cols<-c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date", "include")
  mig_type_2<-mig_type_2[,cols,with=F]
  preg_D3<-merge.data.table(preg_D3, mig_type_2, by=c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date"), all.x = T)
  setnames(preg_D3, "include", "MIG_T2")
  preg_D3[is.na(MIG_T2), MIG_T2:=0]
  rm(mig_type_2)
}else{
  preg_D3[,MIG_T2:=0]
}

#Type 3
mig_type_3<-from_DP4_algorithm("migraine_algorithm/final_d3","MIG_T3_b_D3")
if(length(mig_type_3)>0){
  cols<-c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date", "include")
  mig_type_3<-mig_type_3[,cols,with=F]
  preg_D3<-merge.data.table(preg_D3, mig_type_3, by=c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date"), all.x = T)
  setnames(preg_D3, "include", "MIG_T3")
  preg_D3[is.na(MIG_T3), MIG_T3:=0]
  rm(mig_type_3)
}else{
  preg_D3[,MIG_T3:=0]
}

#Type 4
mig_type_4<-from_DP4_algorithm("migraine_algorithm/final_d3","MIG_T4_b_D3")
if(length(mig_type_4)>0){
  cols<-c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date", "include")
  mig_type_4<-mig_type_4[,cols,with=F]
  preg_D3<-merge.data.table(preg_D3, mig_type_4, by=c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date"), all.x = T)
  setnames(preg_D3, "include", "MIG_T4")
  preg_D3[is.na(MIG_T4), MIG_T4:=0]
  rm(mig_type_4)
}else{
  preg_D3[,MIG_T4:=0]
}

#Type 5
mig_type_5<-from_DP4_algorithm("migraine_algorithm/final_d3","MIG_T5_b_D3")
if(length(mig_type_5)>0){
  cols<-c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date", "include")
  mig_type_5<-mig_type_5[,cols,with=F]
  preg_D3<-merge.data.table(preg_D3, mig_type_5, by=c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date"), all.x = T)
  setnames(preg_D3, "include", "MIG_T5")
  preg_D3[is.na(MIG_T5), MIG_T5:=0]
  rm(mig_type_5)
}else{
  preg_D3[,MIG_T5:=0]
}

#Type 6
mig_type_6<-from_DP4_algorithm("migraine_algorithm/final_d3","MIG_T6_b_D3")
if(length(mig_type_6)>0){
  cols<-c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date", "include")
  mig_type_6<-mig_type_6[,cols,with=F]
  preg_D3<-merge.data.table(preg_D3, mig_type_6, by=c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date"), all.x = T)
  setnames(preg_D3, "include", "MIG_T6")
  preg_D3[is.na(MIG_T6), MIG_T6:=0]
  rm(mig_type_6)
}else{
  preg_D3[,MIG_T6:=0]
}

####create covariates: Migraine severity####
#Severity 1
mig_severity_1<-from_DP4_algorithm("migraine_algorithm/final_d3","MIG_S1_b_D3")
if(length(mig_severity_1)>0){
  cols<-c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date", "include")
  mig_severity_1<-mig_severity_1[,cols,with=F]
  preg_D3<-merge.data.table(preg_D3, mig_severity_1, by=c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date"), all.x = T)
  setnames(preg_D3, "include", "MIG_S1")
  preg_D3[is.na(MIG_S1), MIG_S1:=0]
  rm(mig_severity_1)
}else{
  preg_D3[,MIG_S1:=0]
}

#Severity 2
mig_severity_2<-from_DP4_algorithm("migraine_algorithm/final_d3","MIG_S2_b_D3")
if(length(mig_severity_2)>0){
  cols<-c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date", "include")
  mig_severity_2<-mig_severity_2[,cols,with=F]
  preg_D3<-merge.data.table(preg_D3, mig_severity_2, by=c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date"), all.x = T)
  setnames(preg_D3, "include", "MIG_S2")
  preg_D3[is.na(MIG_S2), MIG_S2:=0]
  rm(mig_severity_2)
}else{
  preg_D3[,MIG_S2:=0]
}

#Severity 3
mig_severity_3<-from_DP4_algorithm("migraine_algorithm/final_d3","MIG_S3_b_D3")
if(length(mig_severity_3)>0){
  cols<-c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date", "include")
  mig_severity_3<-mig_severity_3[,cols,with=F]
  preg_D3<-merge.data.table(preg_D3, mig_severity_3, by=c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date"), all.x = T)
  setnames(preg_D3, "include", "MIG_S3")
  preg_D3[is.na(MIG_S3), MIG_S3:=0]
  rm(mig_severity_3)
}else{
  preg_D3[,MIG_S3:=0]
}

#Severity 4
mig_severity_4<-from_DP4_algorithm("migraine_algorithm/final_d3","MIG_S4_during_D3")
if(length(mig_severity_4)>0){
  cols<-c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date", "include")
  mig_severity_4<-mig_severity_4[,cols,with=F]
  preg_D3<-merge.data.table(preg_D3, mig_severity_4, by=c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date"), all.x = T)
  setnames(preg_D3, "include", "MIG_S4")
  preg_D3[is.na(MIG_S4), MIG_S4:=0]
  rm(mig_severity_4)
}else{
  preg_D3[,MIG_S4:=0]
}


#Dx
mig_tmp = from_DP4_algorithm("migraine_algorithm/final_d3","MIG_Dx_b_D3")
if(length(mig_tmp)>0){
  cols<-c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date", "include")
  mig_tmp<-mig_tmp[,cols,with=F]
  preg_D3<-merge.data.table(preg_D3, mig_tmp, by=c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date"), all.x = T)
  setnames(preg_D3, "include", "MIG_Dx")
  preg_D3[is.na(MIG_Dx), MIG_Dx:=0]
  rm(mig_tmp)
}else{
  preg_D3[,MIG_Dx:=0]
}

#DxRx
mig_tmp = from_DP4_algorithm("migraine_algorithm/final_d3","MIG_DxRx_b_D3")
if(length(mig_tmp)>0){
  cols<-c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date", "alternative")
  mig_tmp<-mig_tmp[,cols,with=F]
  preg_D3<-merge.data.table(preg_D3, mig_tmp, by=c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date"), all.x = T)
  setnames(preg_D3, "alternative", "MIG_DxRx")
  preg_D3[is.na(MIG_DxRx), MIG_DxRx:=0]
  rm(mig_tmp)
}else{
  preg_D3[,MIG_DxRx:=0]
}

#Dx_S
mig_tmp = from_DP4_algorithm("migraine_algorithm_sensitivity","MIG_Dx_b_D3.rds")
if(length(mig_tmp)>0){
  cols<-c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date", "alternative")
  mig_tmp<-mig_tmp[,cols,with=F]
  preg_D3<-merge.data.table(preg_D3, mig_tmp, by=c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date"), all.x = T)
  setnames(preg_D3, "alternative", "MIG_Dx_S")
  preg_D3[is.na(MIG_Dx_S), MIG_Dx_S:=0]
  rm(mig_tmp)
}else{
  preg_D3[,MIG_Dx_S:=0]
}

#DxRx_S
mig_tmp = from_DP4_algorithm("migraine_algorithm_sensitivity","MIG_DxRx_b_D3.rds")
if(length(mig_tmp)>0){
  cols<-c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date", "alternative")
  mig_tmp<-mig_tmp[,cols,with=F]
  preg_D3<-merge.data.table(preg_D3, mig_tmp, by=c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date"), all.x = T)
  setnames(preg_D3, "alternative", "MIG_DxRx_S")
  preg_D3[is.na(MIG_DxRx_S), MIG_DxRx_S:=0]
  rm(mig_tmp)
}else{
  preg_D3[,MIG_DxRx_S:=0]
}

#Recalculate maternal age and year group
if("year_group" %in% colnames(preg_D3)){preg_D3[,year_group:=NULL]}
if("maternal_age" %in% colnames(preg_D3)){preg_D3[,maternal_age:=NULL]}

age_band<-c(15,25,34)
preg_D3[,maternal_age:=lapply(.SD, function(x) create_age_band(x, age_band)), .SDcols = "age"]
preg_D3[is.na(maternal_age), maternal_age:="35+"]

year_group<-c(2009,2013,2017,2020)
preg_D3[,year_group:=lapply(.SD, function(x) create_age_band(x, year_group)), .SDcols = "year"]
preg_D3[is.na(year_group), year_group:="2020+"]


#### Save preg cohort ####
#save pregnancy population to g_intermediate
saveRDS(preg_D3,paste0(g_intermediate,"pregnancy_D3/pregnancy_D3_DU_b.rds"))

#Create an observation period template for each person_id + pregnancy_id to be used when filtering events and medicines
#Clean up the file
opt_lookback<-opt_lookback_du
after_delivery<-after_delivery_du
#The max lookback for all SAP is 12 month or 3 months(depending on the DAP)
preg_D3[,obs_min:=pregnancy_start_date - opt_lookback]
preg_D3[,obs_max:=pregnancy_end_date + after_delivery]
preg_D3<-preg_D3[,c("person_id", "obs_min", "obs_max")]
preg_D3[, min_filter:= obs_min == min(obs_min), by=person_id]
preg_D3[, max_filter:= obs_max == max(obs_max), by=person_id]
min_obs<-preg_D3[min_filter==T, c("person_id","obs_min")]
max_obs<-preg_D3[max_filter==T, c("person_id","obs_max")]
rm(preg_D3)
min_obs<-merge.data.table(min_obs, max_obs, by="person_id", all=T)
rm(max_obs)

# if(file.exists(paste0(projectFolder, "/g_intermediate/pregnancy_d3/obs_period_hint.rds"))){
#   file.rename(paste0(projectFolder, "/g_intermediate/pregnancy_d3/obs_period_hint.rds"),
#               paste0(projectFolder, "/g_intermediate/pregnancy_d3/obs_period_hint_mig.rds"))
# }

# save and use another obs_period_hint rds file for DU study
saveRDS(min_obs, paste0(projectFolder, "/g_intermediate/pregnancy_d3/obs_period_hint_DU_b.rds"))


#################### [DONE] ########################



###################################################
############ During pregnancy ##############

preg_D3 = preg_D3_during

####create covariates: Migraine type####
#Type 1
mig_type_1 = from_DP4_algorithm("migraine_algorithm/final_d3","MIG_T1_during_D3")
if(length(mig_type_1)>0){
  cols<-c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date", "include")
  mig_type_1<-mig_type_1[,cols,with=F]
  preg_D3<-merge.data.table(preg_D3, mig_type_1, by=c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date"), all.x = T)
  setnames(preg_D3, "include", "MIG_T1")
  preg_D3[is.na(MIG_T1), MIG_T1:=0]
  rm(mig_type_1)
}else{
  preg_D3[,MIG_T1:=0]
}

#Type 2
mig_type_2<-from_DP4_algorithm("migraine_algorithm/final_d3","MIG_T2_during_D3")
if(length(mig_type_2)>0){
  cols<-c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date", "include")
  mig_type_2<-mig_type_2[,cols,with=F]
  preg_D3<-merge.data.table(preg_D3, mig_type_2, by=c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date"), all.x = T)
  setnames(preg_D3, "include", "MIG_T2")
  preg_D3[is.na(MIG_T2), MIG_T2:=0]
  rm(mig_type_2)
}else{
  preg_D3[,MIG_T2:=0]
}

#Type 3
mig_type_3<-from_DP4_algorithm("migraine_algorithm/final_d3","MIG_T3_during_D3")
if(length(mig_type_3)>0){
  cols<-c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date", "include")
  mig_type_3<-mig_type_3[,cols,with=F]
  preg_D3<-merge.data.table(preg_D3, mig_type_3, by=c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date"), all.x = T)
  setnames(preg_D3, "include", "MIG_T3")
  preg_D3[is.na(MIG_T3), MIG_T3:=0]
  rm(mig_type_3)
}else{
  preg_D3[,MIG_T3:=0]
}

#Type 4
mig_type_4<-from_DP4_algorithm("migraine_algorithm/final_d3","MIG_T4_during_D3")
if(length(mig_type_4)>0){
  cols<-c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date", "include")
  mig_type_4<-mig_type_4[,cols,with=F]
  preg_D3<-merge.data.table(preg_D3, mig_type_4, by=c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date"), all.x = T)
  setnames(preg_D3, "include", "MIG_T4")
  preg_D3[is.na(MIG_T4), MIG_T4:=0]
  rm(mig_type_4)
}else{
  preg_D3[,MIG_T4:=0]
}

#Type 5
mig_type_5<-from_DP4_algorithm("migraine_algorithm/final_d3","MIG_T5_during_D3")
if(length(mig_type_5)>0){
  cols<-c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date", "include")
  mig_type_5<-mig_type_5[,cols,with=F]
  preg_D3<-merge.data.table(preg_D3, mig_type_5, by=c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date"), all.x = T)
  setnames(preg_D3, "include", "MIG_T5")
  preg_D3[is.na(MIG_T5), MIG_T5:=0]
  rm(mig_type_5)
}else{
  preg_D3[,MIG_T5:=0]
}

#Type 6
mig_type_6<-from_DP4_algorithm("migraine_algorithm/final_d3","MIG_T6_during_D3")
if(length(mig_type_6)>0){
  cols<-c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date", "include")
  mig_type_6<-mig_type_6[,cols,with=F]
  preg_D3<-merge.data.table(preg_D3, mig_type_6, by=c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date"), all.x = T)
  setnames(preg_D3, "include", "MIG_T6")
  preg_D3[is.na(MIG_T6), MIG_T6:=0]
  rm(mig_type_6)
}else{
  preg_D3[,MIG_T6:=0]
}

####create covariates: Migraine severity####
#Severity 1
mig_severity_1<-from_DP4_algorithm("migraine_algorithm/final_d3","MIG_S1_during_D3")
if(length(mig_severity_1)>0){
  cols<-c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date", "include")
  mig_severity_1<-mig_severity_1[,cols,with=F]
  preg_D3<-merge.data.table(preg_D3, mig_severity_1, by=c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date"), all.x = T)
  setnames(preg_D3, "include", "MIG_S1")
  preg_D3[is.na(MIG_S1), MIG_S1:=0]
  rm(mig_severity_1)
}else{
  preg_D3[,MIG_S1:=0]
}

#Severity 2
mig_severity_2<-from_DP4_algorithm("migraine_algorithm/final_d3","MIG_S2_during_D3")
if(length(mig_severity_2)>0){
  cols<-c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date", "include")
  mig_severity_2<-mig_severity_2[,cols,with=F]
  preg_D3<-merge.data.table(preg_D3, mig_severity_2, by=c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date"), all.x = T)
  setnames(preg_D3, "include", "MIG_S2")
  preg_D3[is.na(MIG_S2), MIG_S2:=0]
  rm(mig_severity_2)
}else{
  preg_D3[,MIG_S2:=0]
}

#Severity 3
mig_severity_3<-from_DP4_algorithm("migraine_algorithm/final_d3","MIG_S3_during_D3")
if(length(mig_severity_3)>0){
  cols<-c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date", "include")
  mig_severity_3<-mig_severity_3[,cols,with=F]
  preg_D3<-merge.data.table(preg_D3, mig_severity_3, by=c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date"), all.x = T)
  setnames(preg_D3, "include", "MIG_S3")
  preg_D3[is.na(MIG_S3), MIG_S3:=0]
  rm(mig_severity_3)
}else{
  preg_D3[,MIG_S3:=0]
}

#Severity 4
mig_severity_4<-from_DP4_algorithm("migraine_algorithm/final_d3","MIG_S4_during_D3")
if(length(mig_severity_4)>0){
  cols<-c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date", "include")
  mig_severity_4<-mig_severity_4[,cols,with=F]
  preg_D3<-merge.data.table(preg_D3, mig_severity_4, by=c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date"), all.x = T)
  setnames(preg_D3, "include", "MIG_S4")
  preg_D3[is.na(MIG_S4), MIG_S4:=0]
  rm(mig_severity_4)
}else{
  preg_D3[,MIG_S4:=0]
}


#Dx
mig_tmp = from_DP4_algorithm("migraine_algorithm/final_d3","MIG_Dx_during_D3")
if(length(mig_tmp)>0){
  cols<-c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date", "include")
  mig_tmp<-mig_tmp[,cols,with=F]
  preg_D3<-merge.data.table(preg_D3, mig_tmp, by=c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date"), all.x = T)
  setnames(preg_D3, "include", "MIG_Dx")
  preg_D3[is.na(MIG_Dx), MIG_Dx:=0]
  rm(mig_tmp)
}else{
  preg_D3[,MIG_Dx:=0]
}

#DxRx
mig_tmp = from_DP4_algorithm("migraine_algorithm/final_d3","MIG_DxRx_during_D3")
if(length(mig_tmp)>0){
  cols<-c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date", "alternative")
  mig_tmp<-mig_tmp[,cols,with=F]
  preg_D3<-merge.data.table(preg_D3, mig_tmp, by=c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date"), all.x = T)
  setnames(preg_D3, "alternative", "MIG_DxRx")
  preg_D3[is.na(MIG_DxRx), MIG_DxRx:=0]
  rm(mig_tmp)
}else{
  preg_D3[,MIG_DxRx:=0]
}

#Dx_S
mig_tmp = from_DP4_algorithm("migraine_algorithm_sensitivity","MIG_Dx_during_D3.rds")
if(length(mig_tmp)>0){
  cols<-c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date", "alternative")
  mig_tmp<-mig_tmp[,cols,with=F]
  preg_D3<-merge.data.table(preg_D3, mig_tmp, by=c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date"), all.x = T)
  setnames(preg_D3, "alternative", "MIG_Dx_S")
  preg_D3[is.na(MIG_Dx_S), MIG_Dx_S:=0]
  rm(mig_tmp)
}else{
  preg_D3[,MIG_Dx_S:=0]
}

#DxRx_S
mig_tmp = from_DP4_algorithm("migraine_algorithm_sensitivity","MIG_DxRx_during_D3.rds")
if(length(mig_tmp)>0){
  cols<-c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date", "alternative")
  mig_tmp<-mig_tmp[,cols,with=F]
  preg_D3<-merge.data.table(preg_D3, mig_tmp, by=c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date"), all.x = T)
  setnames(preg_D3, "alternative", "MIG_DxRx_S")
  preg_D3[is.na(MIG_DxRx_S), MIG_DxRx_S:=0]
  rm(mig_tmp)
}else{
  preg_D3[,MIG_DxRx_S:=0]
}


#Recalculate maternal age and year group
if("year_group" %in% colnames(preg_D3)){preg_D3[,year_group:=NULL]}
if("maternal_age" %in% colnames(preg_D3)){preg_D3[,maternal_age:=NULL]}

age_band<-c(15,25,34)
preg_D3[,maternal_age:=lapply(.SD, function(x) create_age_band(x, age_band)), .SDcols = "age"]
preg_D3[is.na(maternal_age), maternal_age:="35+"]

year_group<-c(2009,2013,2017,2020)
preg_D3[,year_group:=lapply(.SD, function(x) create_age_band(x, year_group)), .SDcols = "year"]
preg_D3[is.na(year_group), year_group:="2020+"]


#### Save preg cohort ####
#save pregnancy population to g_intermediate
saveRDS(preg_D3,paste0(g_intermediate,"pregnancy_D3/pregnancy_D3_DU_during.rds"))



#################### [DONE] #######################


date_running_end_00<-Sys.Date()
end_time_00<-Sys.time()

time_log<-data.table(DAP=data_access_provider_name,
                     Script="Step_00_select_pregnancy_D3.R", 
                     Start_date=date_running_start_00, 
                     End_date=date_running_end_00,
                     Time_elaspsed=format(end_time_00-initial_time_00, digits=2))

fwrite(time_log,paste0(output_dir,"/Time log/DU/Step_00_time_log.csv"),row.names = F)
rm(time_log)


