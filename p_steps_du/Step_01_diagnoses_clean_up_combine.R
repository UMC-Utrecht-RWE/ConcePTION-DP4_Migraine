initial_time_01_c<-Sys.time()
date_running_start_01_c<-Sys.Date()

####Combine results by event####
###Checkbox files####
checkbox_files<-list.files(paste0(projectFolder, "/g_intermediate/tmp/"), "Checkbox_")

#Hypertension
hypertension_fl<-checkbox_files[str_detect(checkbox_files, "_BLHT_")]
if(length(hypertension_fl)>0){
  diag_checkbox<-lapply(paste0(tmp, hypertension_fl), readRDS)
  diag_checkbox<-as.data.table(do.call(rbind,diag_checkbox))
  #remove duplicates(same person id, date and condition)
  diag_checkbox[,comb:=paste0(person_id, "_", event_date)]
  dup<-diag_checkbox[duplicated(comb),.N]
  if(dup>0){
    dup_blht_checkbox<-data.table(StudyVar="BLHT_checkbox", removed_rec=dup)
  }else{
    dup_blht_checkbox<-data.table(StudyVar="BLHT_checkbox", removed_rec=0)  
  }
  rm(dup)
  diag_checkbox<-diag_checkbox[!duplicated(comb)]
  diag_checkbox[,comb:=NULL]
  #Summary of included records by event_code, meaning, condition, year
  sum_blht_checkbox<-diag_checkbox[,.N, by=year(event_date)]
  setnames(sum_blht_checkbox,"N", "no_records")
  sum_blht_checkbox[,indicator:="BLHT_checkbox"]
  saveRDS(diag_checkbox, paste0(g_intermediate,"diagnoses_D3/raw_data/BLHT_checkbox.rds"))
  rm(diag_checkbox)
  #remove files
  #remove all gdm files from tmp
  for (i in 1:length(hypertension_fl)){
    file.remove(paste0(tmp,hypertension_fl[[i]]))
  }
  
}else{
  dup_blht_checkbox<-data.table(StudyVar="BLHT_checkbox", removed_rec="N/A")
  sum_blht_checkbox<-NULL
}
rm(hypertension_fl)


#Diabetes
#Hypertension
diabetes_fl<-checkbox_files[str_detect(checkbox_files, "_BLDM_")]
if(length(diabetes_fl)>0){
  diag_checkbox<-lapply(paste0(tmp, diabetes_fl), readRDS)
  diag_checkbox<-as.data.table(do.call(rbind,diag_checkbox))
  #remove duplicates(same person id, date and condition)
  diag_checkbox[,comb:=paste0(person_id, "_", event_date)]
  dup<-diag_checkbox[duplicated(comb),.N]
  if(dup>0){
    dup_bldm_checkbox<-data.table(StudyVar="BLDM_checkbox", removed_rec=dup)
  }else{
    dup_bldm_checkbox<-data.table(StudyVar="BLDM_checkbox", removed_rec=0)  
  }
  rm(dup)
  diag_checkbox<-diag_checkbox[!duplicated(comb)]
  diag_checkbox[,comb:=NULL]
  #Summary of included records by event_code, meaning, condition, year
  sum_bldm_checkbox<-diag_checkbox[,.N, by=year(event_date)]
  setnames(sum_bldm_checkbox,"N", "no_records")
  sum_bldm_checkbox[,indicator:="BLDM_checkbox"]
  saveRDS(diag_checkbox, paste0(g_intermediate,"diagnoses_D3/raw_data/BLDM_checkbox.rds"))
  rm(diag_checkbox)
  #remove files
  #remove all gdm files from tmp
  for (i in 1:length(diabetes_fl)){
    file.remove(paste0(tmp,diabetes_fl[[i]]))
  }
  
}else{
  dup_bldm_checkbox<-data.table(StudyVar="BLDM_checkbox", removed_rec="N/A")
  sum_bldm_checkbox<-NULL
}
rm(diabetes_fl)

#Depression
#Hypertension
depression_fl<-checkbox_files[str_detect(checkbox_files, "_BLDEP_")]
if(length(depression_fl)>0){
  diag_checkbox<-lapply(paste0(tmp, depression_fl), readRDS)
  diag_checkbox<-as.data.table(do.call(rbind,diag_checkbox))
  #remove duplicates(same person id, date and condition)
  diag_checkbox[,comb:=paste0(person_id, "_", event_date)]
  dup<-diag_checkbox[duplicated(comb),.N]
  if(dup>0){
    dup_bldep_checkbox<-data.table(StudyVar="BLDEP_checkbox", removed_rec=dup)
  }else{
    dup_bldep_checkbox<-data.table(StudyVar="BLDEP_checkbox", removed_rec=0)  
  }
  rm(dup)
  diag_checkbox<-diag_checkbox[!duplicated(comb)]
  diag_checkbox[,comb:=NULL]
  #Summary of included records by event_code, meaning, condition, year
  sum_bldep_checkbox<-diag_checkbox[,.N, by=year(event_date)]
  setnames(sum_bldep_checkbox,"N", "no_records")
  sum_bldep_checkbox[,indicator:="BLDEP_checkbox"]
  saveRDS(diag_checkbox, paste0(g_intermediate,"diagnoses_D3/raw_data/BLDEP_checkbox.rds"))
  rm(diag_checkbox)
  #remove files
  #remove all gdm files from tmp
  for (i in 1:length(depression_fl)){
    file.remove(paste0(tmp,depression_fl[[i]]))
  }
  
}else{
  dup_bldep_checkbox<-data.table(StudyVar="BLDEP_checkbox", removed_rec="N/A")
  sum_bldep_checkbox<-NULL
}
rm(depression_fl)

#Parity
#Hypertension
parity_fl<-checkbox_files[str_detect(checkbox_files, "_PARITY_")]
if(length(parity_fl)>0){
  diag_checkbox<-lapply(paste0(tmp, parity_fl), readRDS)
  diag_checkbox<-as.data.table(do.call(rbind,diag_checkbox))
  #remove duplicates(same person id, date and condition)
  diag_checkbox[,comb:=paste0(person_id, "_", event_date)]
  dup<-diag_checkbox[duplicated(comb),.N]
  if(dup>0){
    dup_parity_checkbox<-data.table(StudyVar="PARITY_checkbox", removed_rec=dup)
  }else{
    dup_parity_checkbox<-data.table(StudyVar="PARITY_checkbox", removed_rec=0)  
  }
  rm(dup)
  diag_checkbox<-diag_checkbox[!duplicated(comb)]
  diag_checkbox[,comb:=NULL]
  #Summary of included records by event_code, meaning, condition, year
  sum_parity_checkbox<-diag_checkbox[,.N, by=year(event_date)]
  setnames(sum_parity_checkbox,"N", "no_records")
  sum_parity_checkbox[,indicator:="PARITY_checkbox"]
  saveRDS(diag_checkbox, paste0(g_intermediate,"diagnoses_D3/raw_data/PARITY_checkbox.rds"))
  rm(diag_checkbox)
  #remove files
  #remove all gdm files from tmp
  for (i in 1:length(parity_fl)){
    file.remove(paste0(tmp,parity_fl[[i]]))
  }
  
}else{
  dup_parity_checkbox<-data.table(StudyVar="PARITY_checkbox", removed_rec="N/A")
  sum_parity_checkbox<-NULL
}
rm(parity_fl)

#Obesity
#Hypertension
obesity_fl<-checkbox_files[str_detect(checkbox_files, "_OW_")]
if(length(obesity_fl)>0){
  diag_checkbox<-lapply(paste0(tmp, obesity_fl), readRDS)
  diag_checkbox<-as.data.table(do.call(rbind,diag_checkbox))
  #remove duplicates(same person id, date and condition)
  diag_checkbox[,comb:=paste0(person_id, "_", event_date)]
  dup<-diag_checkbox[duplicated(comb),.N]
  if(dup>0){
    dup_obesity_checkbox<-data.table(StudyVar="OW_checkbox", removed_rec=dup)
  }else{
    dup_obesity_checkbox<-data.table(StudyVar="OW_checkbox", removed_rec=0)  
  }
  rm(dup)
  diag_checkbox<-diag_checkbox[!duplicated(comb)]
  diag_checkbox[,comb:=NULL]
  #Summary of included records by event_code, meaning, condition, year
  sum_obesity_checkbox<-diag_checkbox[,.N, by=year(event_date)]
  setnames(sum_obesity_checkbox,"N", "no_records")
  sum_obesity_checkbox[,indicator:="OW_checkbox"]
  saveRDS(diag_checkbox, paste0(g_intermediate,"diagnoses_D3/raw_data/OW_checkbox.rds"))
  rm(diag_checkbox)
  #remove files
  #remove all gdm files from tmp
  for (i in 1:length(obesity_fl)){
    file.remove(paste0(tmp,obesity_fl[[i]]))
  }
  
}else{
  dup_obesity_checkbox<-data.table(StudyVar="OW_checkbox", removed_rec="N/A")
  sum_obesity_checkbox<-NULL
}
rm(obesity_fl)


### fixed column ####
fixed_files<-list.files(paste0(projectFolder, "/g_intermediate/tmp/"), "Fixed_")


#Hypertension
hypertension_fl<-fixed_files[str_detect(fixed_files, "_BLHT_")]
if(length(hypertension_fl)>0){
  diag_fixed<-lapply(paste0(tmp, hypertension_fl), readRDS)
  diag_fixed<-as.data.table(do.call(rbind,diag_fixed))
  #remove duplicates(same person id, date and condition)
  diag_fixed[,comb:=paste0(person_id, "_", event_date)]
  dup<-diag_fixed[duplicated(comb),.N]
  if(dup>0){
    dup_blht_fixed<-data.table(StudyVar="BLHT_fixed", removed_rec=dup)
  }else{
    dup_blht_fixed<-data.table(StudyVar="BLHT_fixed", removed_rec=0)  
  }
  rm(dup)
  diag_fixed<-diag_fixed[!duplicated(comb)]
  diag_fixed[,comb:=NULL]
  #Summary of included records by event_code, meaning, condition, year
  sum_blht_fixed<-diag_fixed[,.N, by=year(event_date)]
  setnames(sum_blht_fixed,"N", "no_records")
  sum_blht_fixed[,indicator:="BLHT_fixed"]
  saveRDS(diag_fixed, paste0(g_intermediate,"diagnoses_D3/raw_data/BLHT_fixed.rds"))
  rm(diag_fixed)
  #remove files
  #remove all gdm files from tmp
  for (i in 1:length(hypertension_fl)){
    file.remove(paste0(tmp,hypertension_fl[[i]]))
  }
  
}else{
  dup_blht_fixed<-data.table(StudyVar="BLHT_fixed", removed_rec="N/A")
  sum_blht_fixed<-NULL
}
rm(hypertension_fl)

#Diabetes
diabetes_fl<-fixed_files[str_detect(fixed_files, "_BLDM_")]
if(length(diabetes_fl)>0){
  diag_fixed<-lapply(paste0(tmp, diabetes_fl), readRDS)
  diag_fixed<-as.data.table(do.call(rbind,diag_fixed))
  #remove duplicates(same person id, date and condition)
  diag_fixed[,comb:=paste0(person_id, "_", event_date)]
  dup<-diag_fixed[duplicated(comb),.N]
  if(dup>0){
    dup_bldm_fixed<-data.table(StudyVar="BLDM_fixed", removed_rec=dup)
  }else{
    dup_bldm_fixed<-data.table(StudyVar="BLDM_fixed", removed_rec=0)  
  }
  rm(dup)
  diag_fixed<-diag_fixed[!duplicated(comb)]
  diag_fixed[,comb:=NULL]
  #Summary of included records by event_code, meaning, condition, year
  sum_bldm_fixed<-diag_fixed[,.N, by=year(event_date)]
  setnames(sum_bldm_fixed,"N", "no_records")
  sum_bldm_fixed[,indicator:="BLDM_fixed"]
  saveRDS(diag_fixed, paste0(g_intermediate,"diagnoses_D3/raw_data/BLDM_fixed.rds"))
  rm(diag_fixed)
  #remove files
  #remove all gdm files from tmp
  for (i in 1:length(diabetes_fl)){
    file.remove(paste0(tmp,diabetes_fl[[i]]))
  }
  
}else{
  dup_bldm_fixed<-data.table(StudyVar="BLDM_fixed", removed_rec="N/A")
  sum_bldm_fixed<-NULL
}
rm(diabetes_fl)

#Depression

depression_fl<-fixed_files[str_detect(fixed_files, "_BLDEP_")]
if(length(depression_fl)>0){
  diag_fixed<-lapply(paste0(tmp, depression_fl), readRDS)
  diag_fixed<-as.data.table(do.call(rbind,diag_fixed))
  #remove duplicates(same person id, date and condition)
  diag_fixed[,comb:=paste0(person_id, "_", event_date)]
  dup<-diag_fixed[duplicated(comb),.N]
  if(dup>0){
    dup_bldep_fixed<-data.table(StudyVar="BLDEP_fixed", removed_rec=dup)
  }else{
    dup_bldep_fixed<-data.table(StudyVar="BLDEP_fixed", removed_rec=0)  
  }
  rm(dup)
  diag_fixed<-diag_fixed[!duplicated(comb)]
  diag_fixed[,comb:=NULL]
  #Summary of included records by event_code, meaning, condition, year
  sum_bldep_fixed<-diag_fixed[,.N, by=year(event_date)]
  setnames(sum_bldep_fixed,"N", "no_records")
  sum_bldep_fixed[,indicator:="BLDEP_fixed"]
  saveRDS(diag_fixed, paste0(g_intermediate,"diagnoses_D3/raw_data/BLDEP_fixed.rds"))
  rm(diag_fixed)
  #remove files
  #remove all gdm files from tmp
  for (i in 1:length(depression_fl)){
    file.remove(paste0(tmp,depression_fl[[i]]))
  }
  
}else{
  dup_bldep_fixed<-data.table(StudyVar="BLDEP_fixed", removed_rec="N/A")
  sum_bldep_fixed<-NULL
}
rm(depression_fl)

#Parity

parity_fl<-fixed_files[str_detect(fixed_files, "_PARITY_")]
if(length(parity_fl)>0){
  diag_fixed<-lapply(paste0(tmp, parity_fl), readRDS)
  diag_fixed<-as.data.table(do.call(rbind,diag_fixed))
  #remove duplicates(same person id, date and condition)
  diag_fixed[,comb:=paste0(person_id, "_", event_date)]
  dup<-diag_fixed[duplicated(comb),.N]
  if(dup>0){
    dup_parity_fixed<-data.table(StudyVar="PARITY_fixed", removed_rec=dup)
  }else{
    dup_parity_fixed<-data.table(StudyVar="PARITY_fixed", removed_rec=0)  
  }
  rm(dup)
  diag_fixed<-diag_fixed[!duplicated(comb)]
  diag_fixed[,comb:=NULL]
  #Summary of included records by event_code, meaning, condition, year
  sum_parity_fixed<-diag_fixed[,.N, by=year(event_date)]
  setnames(sum_parity_fixed,"N", "no_records")
  sum_parity_fixed[,indicator:="PARITY_fixed"]
  saveRDS(diag_fixed, paste0(g_intermediate,"diagnoses_D3/raw_data/PARITY_fixed.rds"))
  rm(diag_fixed)
  #remove files
  #remove all gdm files from tmp
  for (i in 1:length(parity_fl)){
    file.remove(paste0(tmp,parity_fl[[i]]))
  }
  
}else{
  dup_parity_fixed<-data.table(StudyVar="PARITY_fixed", removed_rec="N/A")
  sum_parity_fixed<-NULL
}
rm(parity_fl)

#Obesity

obesity_fl<-fixed_files[str_detect(fixed_files, "_OW_")]
if(length(obesity_fl)>0){
  diag_fixed<-lapply(paste0(tmp, obesity_fl), readRDS)
  diag_fixed<-as.data.table(do.call(rbind,diag_fixed))
  #remove duplicates(same person id, date and condition)
  diag_fixed[,comb:=paste0(person_id, "_", event_date)]
  dup<-diag_fixed[duplicated(comb),.N]
  if(dup>0){
    dup_obesity_fixed<-data.table(StudyVar="OW_fixed", removed_rec=dup)
  }else{
    dup_obesity_fixed<-data.table(StudyVar="OW_fixed", removed_rec=0)  
  }
  rm(dup)
  diag_fixed<-diag_fixed[!duplicated(comb)]
  diag_fixed[,comb:=NULL]
  #Summary of included records by event_code, meaning, condition, year
  sum_obesity_fixed<-diag_fixed[,.N, by=year(event_date)]
  setnames(sum_obesity_fixed,"N", "no_records")
  sum_obesity_fixed[,indicator:="OW_fixed"]
  saveRDS(diag_fixed, paste0(g_intermediate,"diagnoses_D3/raw_data/OW_fixed.rds"))
  rm(diag_fixed)
  #remove files
  #remove all gdm files from tmp
  for (i in 1:length(obesity_fl)){
    file.remove(paste0(tmp,obesity_fl[[i]]))
  }
  
}else{
  dup_obesity_fixed<-data.table(StudyVar="OW_fixed", removed_rec="N/A")
  sum_obesity_fixed<-NULL
}
rm(obesity_fl)

#Height

height_fl<-fixed_files[str_detect(fixed_files, "_PBMI_h_")]
if(length(height_fl)>0){
  diag_fixed<-lapply(paste0(tmp, height_fl), readRDS)
  diag_fixed<-as.data.table(do.call(rbind,diag_fixed))
  #remove duplicates(same person id, date and condition)
  diag_fixed[,comb:=paste0(person_id, "_", event_date)]
  dup<-diag_fixed[duplicated(comb),.N]
  if(dup>0){
    dup_height_fixed<-data.table(StudyVar="PBMI_h_fixed", removed_rec=dup)
  }else{
    dup_height_fixed<-data.table(StudyVar="PBMI_h_fixed", removed_rec=0)  
  }
  rm(dup)
  diag_fixed<-diag_fixed[!duplicated(comb)]
  diag_fixed[,comb:=NULL]
  #Summary of included records by event_code, meaning, condition, year
  sum_height_fixed<-diag_fixed[,.N, by=year(event_date)]
  setnames(sum_height_fixed,"N", "no_records")
  sum_height_fixed[,indicator:="PBMI_h_fixed"]
  saveRDS(diag_fixed, paste0(g_intermediate,"diagnoses_D3/raw_data/PBMI_h_fixed.rds"))
  rm(diag_fixed)
  #remove files
  #remove all gdm files from tmp
  for (i in 1:length(height_fl)){
    file.remove(paste0(tmp,height_fl[[i]]))
  }
  
}else{
  dup_height_fixed<-data.table(StudyVar="PBMI_h_fixed", removed_rec="N/A")
  sum_height_fixed<-NULL
}
rm(height_fl)

#Weight

weight_fl<-fixed_files[str_detect(fixed_files, "_PBMI_w_")]
if(length(weight_fl)>0){
  diag_fixed<-lapply(paste0(tmp, weight_fl), readRDS)
  diag_fixed<-as.data.table(do.call(rbind,diag_fixed))
  #remove duplicates(same person id, date and condition)
  diag_fixed[,comb:=paste0(person_id, "_", event_date)]
  dup<-diag_fixed[duplicated(comb),.N]
  if(dup>0){
    dup_weight_fixed<-data.table(StudyVar="PBMI_w_fixed", removed_rec=dup)
  }else{
    dup_weight_fixed<-data.table(StudyVar="PBMI_w_fixed", removed_rec=0)  
  }
  rm(dup)
  diag_fixed<-diag_fixed[!duplicated(comb)]
  diag_fixed[,comb:=NULL]
  #Summary of included records by event_code, meaning, condition, year
  sum_weight_fixed<-diag_fixed[,.N, by=year(event_date)]
  setnames(sum_weight_fixed,"N", "no_records")
  sum_weight_fixed[,indicator:="PBMI_w_fixed"]
  saveRDS(diag_fixed, paste0(g_intermediate,"diagnoses_D3/raw_data/PBMI_w_fixed.rds"))
  rm(diag_fixed)
  #remove files
  #remove all gdm files from tmp
  for (i in 1:length(weight_fl)){
    file.remove(paste0(tmp,weight_fl[[i]]))
  }
  
}else{
  dup_weight_fixed<-data.table(StudyVar="PBMI_w_fixed", removed_rec="N/A")
  sum_weight_fixed<-NULL
}
rm(weight_fl)


#### diagnoses####

#V_HYPERTENSION_COV
hyp_fl<-list.files(paste0(projectFolder, "/g_intermediate/tmp/"), "V_HYPERTENSION_COV")
if(length(hyp_fl)>0){
  diag<-lapply(paste0(tmp, hyp_fl), readRDS)
  diag<-as.data.table(do.call(rbind,diag))
  #remove duplicates(same person id, date and condition)
  diag[,comb:=paste0(person_id, "_", event_date)]
  dup<-diag[duplicated(comb),.N]
  if(dup>0){
    dup_blht<-data.table(StudyVar="BLHT_diagnoses", removed_rec=dup)
  }else{
    dup_blht<-data.table(StudyVar="BLHT_diagnoses", removed_rec=0)  
  }
  rm(dup)
  diag<-diag[!duplicated(comb)]
  diag[,comb:=NULL]
  #Summary of included records by event_code, meaning, condition, year
  sum_blht<-diag[,.N, by=year(event_date)]
  setnames(sum_blht,"N", "no_records")
  sum_blht[,indicator:="BLHT_diagnoses"]
  setnames(diag, "variable_name", "event_name")
  diag[,variable_name:="BLHT_diagnoses"]
  saveRDS(diag, paste0(g_intermediate,"diagnoses_D3/raw_data/BLHT_diagnoses.rds"))
  rm(diag)
  #remove files
  #remove all gdm files from tmp
  for (i in 1:length(hyp_fl)){
    file.remove(paste0(tmp,hyp_fl[[i]]))
  }
  
}else{
  dup_blht<-data.table(StudyVar="BLHT_diagnoses", removed_rec="N/A")
  sum_blht<-NULL
}
rm(hyp_fl)

#E_DM12_COV

dm_fl<-list.files(paste0(projectFolder, "/g_intermediate/tmp/"), "E_DM12_COV")
if(length(dm_fl)>0){
  diag<-lapply(paste0(tmp, dm_fl), readRDS)
  diag<-as.data.table(do.call(rbind,diag))
  #remove duplicates(same person id, date and condition)
  diag[,comb:=paste0(person_id, "_", event_date)]
  dup<-diag[duplicated(comb),.N]
  if(dup>0){
    dup_bldm<-data.table(StudyVar="BLDM_diagnoses", removed_rec=dup)
  }else{
    dup_bldm<-data.table(StudyVar="BLDM_diagnoses", removed_rec=0)  
  }
  rm(dup)
  diag<-diag[!duplicated(comb)]
  diag[,comb:=NULL]
  #Summary of included records by event_code, meaning, condition, year
  sum_bldm<-diag[,.N, by=year(event_date)]
  setnames(sum_bldm,"N", "no_records")
  sum_bldm[,indicator:="BLDM_diagnoses"]
  setnames(diag, "variable_name", "event_name")
  diag[,variable_name:="BLDM_diagnoses"]
  saveRDS(diag, paste0(g_intermediate,"diagnoses_D3/raw_data/BLDM_diagnoses.rds"))
  rm(diag)
  #remove files
  #remove all gdm files from tmp
  for (i in 1:length(dm_fl)){
    file.remove(paste0(tmp,dm_fl[[i]]))
  }
  
}else{
  dup_bldm<-data.table(StudyVar="BLDM_diagnoses", removed_rec="N/A")
  sum_bldm<-NULL
}
rm(dm_fl)

#Ment_DEPRESSION_COV

dep_fl<-list.files(paste0(projectFolder, "/g_intermediate/tmp/"), "Ment_DEPRESSION_COV")
if(length(dep_fl)>0){
  diag<-lapply(paste0(tmp, dep_fl), readRDS)
  diag<-as.data.table(do.call(rbind,diag))
  #remove duplicates(same person id, date and condition)
  diag[,comb:=paste0(person_id, "_", event_date)]
  dup<-diag[duplicated(comb),.N]
  if(dup>0){
    dup_bldep<-data.table(StudyVar="BLDEP_diagnoses", removed_rec=dup)
  }else{
    dup_bldep<-data.table(StudyVar="BLDEP_diagnoses", removed_rec=0)  
  }
  rm(dup)
  diag<-diag[!duplicated(comb)]
  diag[,comb:=NULL]
  #Summary of included records by event_code, meaning, condition, year
  sum_bldep<-diag[,.N, by=year(event_date)]
  setnames(sum_bldep,"N", "no_records")
  sum_bldep[,indicator:="BLDEP_diagnoses"]
  setnames(diag, "variable_name", "event_name")
  diag[,variable_name:="BLDEP_diagnoses"]
  
  saveRDS(diag, paste0(g_intermediate,"diagnoses_D3/raw_data/BLDEP_diagnoses.rds"))
  rm(diag)
  #remove files
  #remove all gdm files from tmp
  for (i in 1:length(dep_fl)){
    file.remove(paste0(tmp,dep_fl[[i]]))
  }
  
}else{
  dup_bldep<-data.table(StudyVar="BLDEP_diagnoses", removed_rec="N/A")
  sum_bldep<-NULL
}
rm(dep_fl)

#L_OBESITY_COV

obesity_fl<-list.files(paste0(projectFolder, "/g_intermediate/tmp/"), "L_OBESITY_COV")
if(length(obesity_fl)>0){
  diag<-lapply(paste0(tmp, obesity_fl), readRDS)
  diag<-as.data.table(do.call(rbind,diag))
  #remove duplicates(same person id, date and condition)
  diag[,comb:=paste0(person_id, "_", event_date)]
  dup<-diag[duplicated(comb),.N]
  if(dup>0){
    dup_obesity<-data.table(StudyVar="OW_diagnoses", removed_rec=dup)
  }else{
    dup_obesity<-data.table(StudyVar="OW_diagnoses", removed_rec=0)  
  }
  rm(dup)
  diag<-diag[!duplicated(comb)]
  diag[,comb:=NULL]
  #Summary of included records by event_code, meaning, condition, year
  sum_obesity<-diag[,.N, by=year(event_date)]
  setnames(sum_obesity,"N", "no_records")
  sum_obesity[,indicator:="OW_diagnoses"]
  setnames(diag, "variable_name", "event_name")
  diag[,variable_name:="OW_diagnoses"]
  
  saveRDS(diag, paste0(g_intermediate,"diagnoses_D3/raw_data/OW_diagnoses.rds"))
  rm(diag)
  #remove files
  #remove all gdm files from tmp
  for (i in 1:length(obesity_fl)){
    file.remove(paste0(tmp,obesity_fl[[i]]))
  }
  
}else{
  dup_obesity<-data.table(StudyVar="OW_diagnoses", removed_rec="N/A")
  sum_obesity<-NULL
}
rm(obesity_fl)


#Combine dup results
if(sum(!is.null(dup_blht_checkbox),!is.null(dup_bldm_checkbox),!is.null(dup_bldep_checkbox),!is.null(dup_parity_checkbox),!is.null(dup_obesity_checkbox),
       !is.null(dup_blht_fixed),!is.null(dup_bldm_fixed),!is.null(dup_bldep_fixed), !is.null(dup_parity_fixed), !is.null(dup_obesity_fixed), !is.null(dup_height_fixed),!is.null(dup_weight_fixed),
       !is.null(dup_blht), !is.null(dup_bldm), !is.null(dup_bldep), !is.null(dup_obesity))){
  dup_du<-rbind(dup_blht_checkbox,dup_bldm_checkbox,dup_bldep_checkbox,dup_parity_checkbox,dup_obesity_checkbox,
                dup_blht_fixed,dup_bldm_fixed,dup_bldep_fixed,dup_parity_fixed,dup_obesity_fixed,dup_height_fixed,dup_weight_fixed,
                dup_blht,dup_bldm,dup_bldep,dup_obesity)
  rm(dup_blht_checkbox,dup_bldm_checkbox,dup_bldep_checkbox,dup_parity_checkbox,dup_obesity_checkbox,
     dup_blht_fixed,dup_bldm_fixed,dup_bldep_fixed,dup_parity_fixed,dup_obesity_fixed,dup_height_fixed,dup_weight_fixed,
     dup_blht,dup_bldm,dup_bldep,dup_obesity)
}else{
  dup_du<-NULL 
}

#Combine sum results
if(sum(!is.null(sum_blht_checkbox),!is.null(sum_bldm_checkbox),!is.null(sum_bldep_checkbox),!is.null(sum_parity_checkbox),!is.null(sum_obesity_checkbox),
       !is.null(sum_blht_fixed),!is.null(sum_bldm_fixed),!is.null(sum_bldep_fixed), !is.null(sum_parity_fixed), !is.null(sum_obesity_fixed), !is.null(sum_height_fixed),!is.null(sum_weight_fixed),
       !is.null(sum_blht), !is.null(sum_bldm), !is.null(sum_bldep), !is.null(sum_obesity))){
  sum_du<-rbind(sum_blht_checkbox,sum_bldm_checkbox,sum_bldep_checkbox,sum_parity_checkbox,sum_obesity_checkbox,
                sum_blht_fixed,sum_bldm_fixed,sum_bldep_fixed,sum_parity_fixed,sum_obesity_fixed,sum_height_fixed,sum_weight_fixed,
                sum_blht,sum_bldm,sum_bldep,sum_obesity)
  rm(sum_blht_checkbox,sum_bldm_checkbox,sum_bldep_checkbox,sum_parity_checkbox,sum_obesity_checkbox,
     sum_blht_fixed,sum_bldm_fixed,sum_bldep_fixed,sum_parity_fixed,sum_obesity_fixed,sum_height_fixed,sum_weight_fixed,
     sum_blht,sum_bldm,sum_bldep,sum_obesity)
}else{
  sum_du<-NULL 
}


date_running_end_01_c<-Sys.Date()
end_time_01_c<-Sys.time()

time_log_01_c<-data.table(DAP=data_access_provider_name,
                     Script="Step_01_diagnoses_clean_up_combine.R", 
                     Start_date=date_running_start_01_c, 
                     End_date=date_running_end_01_c,
                     Time_elaspsed=format(end_time_01_c-initial_time_01_c, digits=2))
fwrite(time_log_01_c,paste0(output_dir,"/Time log/DU/Step_01_combine_time_log.csv"),row.names = F)
rm(time_log_01_c)

rm_environment<-c("cat_ind","checkbox_date_col","code_var","codelist_gdm","codelist_migraine","codelist_pe","codes_ind", 
                  "codesheet_diagnoses_gdm","codesheet_diagnoses_gdm_cat","codesheet_diagnoses_migraine","codesheet_diagnoses_migraine_cat",
                  "codesheet_diagnoses_pe","codesheet_diagnoses_pe_cat","codesheet_diagnoses_pe","codesheet_diagnoses_pe_cat","codesheet_medicines_gdm",
                  "codesheet_medicines_migraine","codesheet_procedures_gdm","codesheet_procedures_migraine","col_1_name","col_names","cols",
                  "cols_to_keep","date_var","dates_flowchart","dates_persons","diag_cat_gdm",
                  "diag_cat_migraine","diag_cat_pe","diag_checkbox_gdm","diag_checkbox_gdm_mo","diag_checkbox_gdm_so","diag_checkbox_pe","diag_checkbox_pe_mo",
                  "diag_checkbox_pe_so","diag_ind","gdm_diag_cat","gdm_diag_checkbox_mo","gdm_diag_checkbox_so","i","j","not_equal_col1","not_equal_col2",
                  "not_equal_val1","not_equal_val2","not_fixed_gdm_checkbox","not_fixed_gdm_ogtt_all","not_fixed_gdm_ogtt_yes","not_fixed_pe_checkbox","pe_diag_cat",
                  "pe_diag_checkbox_mo","pe_diag_checkbox_so","val_1","values","values_df","voc_var","vocabularies_list_gdm","vocabularies_list_migraine","vocabularies_list_migraine_mg",
                  "vocabularies_list_pe","w","y","years_study_events","dup_pe","migraine_diag_cat","tables_search","events_gdm_diagnoses","events_pe_diagnoses","events_migraine_diagnoses","Indication",
                  "mo_gdm_diagnoses","mo_migraine_diagnoses","mo_pe_diagnoses","so_gdm_diagnoses","so_migraine_diagnoses","so_pe_diagnoses","same_names")

list_rm<-ls()[ls() %in% rm_environment]
rm(list = list_rm)
rm(list_rm,rm_environment)
