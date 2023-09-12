initial_time_02_c<-Sys.time()
date_running_start_02_c<-Sys.Date()

####Combine results by event####
###GDM checkbox####
#GDM checkbox files
print("Combine files about checkbox GDM")
gdm_checkbox_files<-list.files(tmp, "GDM_diagnoses_checkbox")
if(length(gdm_checkbox_files)>0){
  gdm_diag_checkbox<-lapply(paste0(tmp, gdm_checkbox_files), readRDS)
  gdm_diag_checkbox<-as.data.table(do.call(rbind,gdm_diag_checkbox))
  #remove duplicates(same person id, date and condition)
  gdm_diag_checkbox[,comb:=paste0(person_id, "_", event_date, "_", condition)]
  dup<-gdm_diag_checkbox[duplicated(comb),.N]
  if(dup>0){
    dup_dt_gdm<-data.table(StudyVar="GDM_checkbox", removed_rec=dup)
  }else{
    dup_dt_gdm<-data.table(StudyVar="GDM_checkbox", removed_rec=0)  
  }
  rm(dup)
  gdm_diag_checkbox<-gdm_diag_checkbox[!duplicated(comb)]
  gdm_diag_checkbox[,comb:=NULL]
  #Summary of included records by event_code, meaning, condition, year
  sum_gdm<-gdm_diag_checkbox[,.N, by=c("condition", "meaning", "year")]
  setnames(sum_gdm,"N", "no_records")
  #fwrite(sum,paste0(output_dir,"PE and GDM algorithm/summary_gdm_checkbox.csv"))
  gdm_diag_checkbox[,event_code:=NULL][,meaning:=NULL][,year:=NULL]
  #save data in g_intermediate/populations
  saveRDS(gdm_diag_checkbox, paste0(g_intermediate,"gdm_algorithm/GDM_checkbox.rds"))
  rm(gdm_diag_checkbox)
  #remove files
  #remove all gdm files from tmp
  for (i in 1:length(gdm_checkbox_files)){
    file.remove(paste0(tmp,gdm_checkbox_files[[i]]))
  }
  
}else{
  dup_dt_gdm<-data.table(StudyVar="GDM_checkbox", removed_rec="N/A")
  sum_gdm<-NULL
}
rm(gdm_checkbox_files)
#dup_dt_gdm<-data.table(StudyVar="GDM_checkbox", removed_rec=dup)

###PE checkbox####
#PE checkbox files
print("Combine files about checkbox PE")
pe_checkbox_files<-list.files(tmp, "PE_diagnoses_checkbox")
if(length(pe_checkbox_files)>0){
  pe_diag_checkbox<-lapply(paste0(tmp, pe_checkbox_files), readRDS)
  pe_diag_checkbox<-as.data.table(do.call(rbind,pe_diag_checkbox))
  #remove duplicates(same person id, date and condition)
  pe_diag_checkbox[,comb:=paste0(person_id, "_", event_date, "_", condition)]
  dup<-pe_diag_checkbox[duplicated(comb),.N]
  if(dup>0){
    dup_pe<-data.table(StudyVar="PE_checkbox", removed_rec=dup)
  }else{
    dup_pe<-data.table(StudyVar="PE_checkbox", removed_rec=0)  
  }
  rm(dup)
  pe_diag_checkbox<-pe_diag_checkbox[!duplicated(comb)]
  pe_diag_checkbox[,comb:=NULL]
  #Summary of included records by event_code, meaning, condition, year
  sum_pe<-pe_diag_checkbox[,.N, by=c("condition", "meaning", "year")]
  setnames(sum_pe,"N", "no_records")
  #fwrite(sum,paste0(output_dir,"PE and GDM algorithm/summary_pe_checkbox.csv"))
  pe_diag_checkbox[,event_code:=NULL][,meaning:=NULL][,year:=NULL]
  #save data in g_intermediate/populations
  saveRDS(pe_diag_checkbox, paste0(g_intermediate,"pe_algorithm/PE_checkbox.rds"))
  rm(pe_diag_checkbox)
  #remove files
  #remove all gdm files from tmp
  for (i in 1:length(pe_checkbox_files)){
    file.remove(paste0(tmp,pe_checkbox_files[[i]]))
  }
  
}else{
  dup_pe<-data.table(StudyVar="PE_checkbox", removed_rec="N/A")
  sum_pe<-NULL
}
rm(pe_checkbox_files)
# dup_dt_pe<-data.table(StudyVar="PE_checkbox", removed_rec=dup)
####GDM diagnoses####

#Diagnostic files
#DM_PREG
gdm_files<-list.files(tmp, "_DM_PREG_")
if(length(gdm_files)>0){
  gdm_dt<-lapply(paste0(tmp,gdm_files), readRDS)
  gdm_dt<-as.data.table(rbindlist(gdm_dt, fill=T))
  #remove duplicates
  gdm_dt[,comb:=paste0(person_id, "_", event_date, "_", condition)]
  dup<-gdm_dt[duplicated(comb),.N]
  if(dup>0){
    dup_dt_dm_preg<-data.table(StudyVar="GDM_diagnoses/DM_PREG", removed_rec=dup)
  }else{
    dup_dt_dm_preg<-data.table(StudyVar="GDM_diagnoses/DM_PREG", removed_rec=0)  
  }
  rm(dup)
  gdm_dt<-gdm_dt[!duplicated(comb)]
  gdm_dt[,comb:=NULL]
  #Summary of included records by event_code, meaning, condition, year
  sum_gdm_dm_preg<-gdm_dt[,.N, by=c("condition", "meaning", "year")]
  setnames(sum_gdm_dm_preg,"N", "no_records")
  #fwrite(sum,paste0(output_dir,"PE and GDM algorithm/summary_gdm_diagnoses_pre_gd.csv"))
  gdm_dt[,event_code:=NULL][,meaning:=NULL][,year:=NULL][,event_vocabulary:=NULL][,truncated_code:=NULL][,prior_gdm_pe:=NULL][,prior_mig:=NULL][,prior_du:=NULL][,prior_saf:=NULL][,after_gdm_pe:=NULL][,after_mig:=NULL][,after_du:=NULL][,after_saf:=NULL][,code_no_dot:=NULL][,filter:=NULL]
  #save data in g_intermediate/populations
  saveRDS(gdm_dt, paste0(g_intermediate,"gdm_algorithm/DM_PREG.rds"))
  rm(gdm_dt)
  #remove files
  #remove all gdm files from tmp
  for (i in 1:length(gdm_files)){
    file.remove(paste0(tmp,gdm_files[[i]]))
  }
  
}else{
  dup_dt_dm_preg<-data.table(StudyVar="GDM_diagnoses/DM_PREG", removed_rec="N/A")
  sum_gdm_dm_preg<-NULL
}
rm(gdm_files)

#PRE_GD
gdm_files<-list.files(tmp, "_PRE_GD_")
if(length(gdm_files)>0){
  gdm_dt<-lapply(paste0(tmp,gdm_files), readRDS)
  gdm_dt<-as.data.table(rbindlist(gdm_dt, fill=T))
  #remove duplicates
  gdm_dt[,comb:=paste0(person_id, "_", event_date, "_", condition)]
  dup<-gdm_dt[duplicated(comb),.N]
  if(dup>0){
    dup_dt_pre_gd<-data.table(StudyVar="GDM_diagnoses/PRE_GD", removed_rec=dup)
  }else{
    dup_dt_pre_gd<-data.table(StudyVar="GDM_diagnoses/PRE_GD", removed_rec=0)  
  }
  rm(dup)
  gdm_dt<-gdm_dt[!duplicated(comb)]
  gdm_dt[,comb:=NULL]
  #Summary of included records by event_code, meaning, condition, year
  sum_gdm_pre_gd<-gdm_dt[,.N, by=c("condition", "meaning", "year")]
  setnames(sum_gdm_pre_gd,"N", "no_records")
  #fwrite(sum,paste0(output_dir,"PE and GDM algorithm/summary_gdm_diagnoses_pre_gd.csv"))
  gdm_dt[,event_code:=NULL][,meaning:=NULL][,year:=NULL][,event_vocabulary:=NULL][,truncated_code:=NULL][,prior_gdm_pe:=NULL][,prior_mig:=NULL][,prior_du:=NULL][,prior_saf:=NULL][,after_gdm_pe:=NULL][,after_mig:=NULL][,after_du:=NULL][,after_saf:=NULL][,code_no_dot:=NULL][,filter:=NULL]
  #save data in g_intermediate/populations
  saveRDS(gdm_dt, paste0(g_intermediate,"gdm_algorithm/PRE_GD.rds"))
  rm(gdm_dt)
  #remove files
  #remove all gdm files from tmp
  for (i in 1:length(gdm_files)){
    file.remove(paste0(tmp,gdm_files[[i]]))
  }
  
}else{
  dup_dt_pre_gd<-data.table(StudyVar="GDM_diagnoses/PRE_GD", removed_rec="N/A")
  sum_gdm_pre_gd<-NULL
}
rm(gdm_files)

#UNK_GD
gdm_files<-list.files(tmp, "_UNK_GD_")
if(length(gdm_files)>0){
  gdm_dt<-lapply(paste0(tmp,gdm_files), readRDS)
  gdm_dt<-as.data.table(rbindlist(gdm_dt, fill=T))
  #remove duplicates
  gdm_dt[,comb:=paste0(person_id, "_", event_date, "_", condition)]
  dup<-gdm_dt[duplicated(comb),.N]
  if(dup>0){
    dup_dt_unk_gd<-data.table(StudyVar="GDM_diagnoses/UNK_GD", removed_rec=dup)
  }else{
    dup_dt_unk_gd<-data.table(StudyVar="GDM_diagnoses/UNK_GD", removed_rec=0)  
  }
  rm(dup)
  gdm_dt<-gdm_dt[!duplicated(comb)]
  gdm_dt[,comb:=NULL]
  #Summary of included records by event_code, meaning, condition, year
  sum_gdm_unk_gd<-gdm_dt[,.N, by=c("condition", "meaning", "year")]
  setnames(sum_gdm_unk_gd,"N", "no_records")
  #fwrite(sum,paste0(output_dir,"PE and GDM algorithm/summary_gdm_diagnoses_pre_gd.csv"))
  gdm_dt[,event_code:=NULL][,meaning:=NULL][,year:=NULL][,event_vocabulary:=NULL][,truncated_code:=NULL][,prior_gdm_pe:=NULL][,prior_mig:=NULL][,prior_du:=NULL][,prior_saf:=NULL][,after_gdm_pe:=NULL][,after_mig:=NULL][,after_du:=NULL][,after_saf:=NULL][,code_no_dot:=NULL][,filter:=NULL]
  #save data in g_intermediate/populations
  saveRDS(gdm_dt, paste0(g_intermediate,"gdm_algorithm/UNK_GD.rds"))
  rm(gdm_dt)
  #remove files
  #remove all gdm files from tmp
  for (i in 1:length(gdm_files)){
    file.remove(paste0(tmp,gdm_files[[i]]))
  }
  
}else{
  dup_dt_unk_gd<-data.table(StudyVar="GDM_diagnoses/UNK_GD", removed_rec="N/A")
  sum_gdm_unk_gd<-NULL
}
rm(gdm_files)

#GD
gdm_files<-list.files(tmp, "_GD_")
if(length(gdm_files)>0){
  gdm_dt<-lapply(paste0(tmp,gdm_files), readRDS)
  gdm_dt<-as.data.table(rbindlist(gdm_dt, fill=T))
  #remove duplicates
  gdm_dt[,comb:=paste0(person_id, "_", event_date, "_", condition)]
  dup<-gdm_dt[duplicated(comb),.N]
  if(dup>0){
    dup_dt_gd<-data.table(StudyVar="GDM_diagnoses/GD", removed_rec=dup)
  }else{
    dup_dt_gd<-data.table(StudyVar="GDM_diagnoses/GD", removed_rec=0)  
  }
  rm(dup)
  gdm_dt<-gdm_dt[!duplicated(comb)]
  gdm_dt[,comb:=NULL]
  #Summary of included records by event_code, meaning, condition, year
  sum_gdm_gd<-gdm_dt[,.N, by=c("condition", "meaning", "year")]
  setnames(sum_gdm_gd,"N", "no_records")
  #fwrite(sum,paste0(output_dir,"PE and GDM algorithm/summary_gdm_diagnoses_pre_gd.csv"))
  gdm_dt[,event_code:=NULL][,meaning:=NULL][,year:=NULL][,event_vocabulary:=NULL][,truncated_code:=NULL][,prior_gdm_pe:=NULL][,prior_mig:=NULL][,prior_du:=NULL][,prior_saf:=NULL][,after_gdm_pe:=NULL][,after_mig:=NULL][,after_du:=NULL][,after_saf:=NULL][,code_no_dot:=NULL][,filter:=NULL]
  #save data in g_intermediate/populations
  saveRDS(gdm_dt, paste0(g_intermediate,"gdm_algorithm/GD.rds"))
  rm(gdm_dt)
  #remove files
  #remove all gdm files from tmp
  for (i in 1:length(gdm_files)){
    file.remove(paste0(tmp,gdm_files[[i]]))
  }
  
}else{
  dup_dt_gd<-data.table(StudyVar="GDM_diagnoses/GD", removed_rec="N/A")
  sum_gdm_gd<-NULL
}
rm(gdm_files)

#DM
gdm_files<-list.files(tmp, "_DM_")
if(length(gdm_files)>0){
  gdm_dt<-lapply(paste0(tmp,gdm_files), readRDS)
  gdm_dt<-as.data.table(rbindlist(gdm_dt, fill=T))
  #remove duplicates
  gdm_dt[,comb:=paste0(person_id, "_", event_date, "_", condition)]
  dup<-gdm_dt[duplicated(comb),.N]
  if(dup>0){
    dup_dt_dm<-data.table(StudyVar="GDM_diagnoses/DM", removed_rec=dup)
  }else{
    dup_dt_dm<-data.table(StudyVar="GDM_diagnoses/DM", removed_rec=0)  
  }
  rm(dup)
  gdm_dt<-gdm_dt[!duplicated(comb)]
  gdm_dt[,comb:=NULL]
  #Summary of included records by event_code, meaning, condition, year
  sum_gdm_dm<-gdm_dt[,.N, by=c("condition", "meaning", "year")]
  setnames(sum_gdm_dm,"N", "no_records")
  #fwrite(sum,paste0(output_dir,"PE and GDM algorithm/summary_gdm_diagnoses_pre_gd.csv"))
  gdm_dt[,event_code:=NULL][,meaning:=NULL][,year:=NULL][,event_vocabulary:=NULL][,truncated_code:=NULL][,prior_gdm_pe:=NULL][,prior_mig:=NULL][,prior_du:=NULL][,prior_saf:=NULL][,after_gdm_pe:=NULL][,after_mig:=NULL][,after_du:=NULL][,after_saf:=NULL][,code_no_dot:=NULL][,filter:=NULL]
  #save data in g_intermediate/populations
  saveRDS(gdm_dt, paste0(g_intermediate,"gdm_algorithm/DM.rds"))
  rm(gdm_dt)
  #remove files
  #remove all gdm files from tmp
  for (i in 1:length(gdm_files)){
    file.remove(paste0(tmp,gdm_files[[i]]))
  }
  
}else{
  dup_dt_dm<-data.table(StudyVar="GDM_diagnoses/DM", removed_rec="N/A")
  sum_gdm_dm<-NULL
}
rm(gdm_files)


#Combine dup results
if(sum(!is.null(dup_dt_dm_preg),!is.null(dup_dt_unk_gd),!is.null(dup_dt_pre_gd),!is.null(dup_dt_gd),!is.null(dup_dt_dm),!is.null(dup_dt_gdm))){
  dup_gdm_diag<-rbind(dup_dt_dm_preg,dup_dt_unk_gd,dup_dt_pre_gd,dup_dt_gd,dup_dt_dm,dup_dt_gdm)
  rm(dup_dt_dm_preg,dup_dt_unk_gd,dup_dt_pre_gd,dup_dt_gd,dup_dt_dm,dup_dt_gdm)
}else{
  dup_gdm_diag<-NULL 
}

#Combine sum results
if(sum(!is.null(sum_gdm_dm_preg),!is.null(sum_gdm_unk_gd),!is.null(sum_gdm_pre_gd),!is.null(sum_gdm_gd),!is.null(sum_gdm_dm),!is.null(sum_gdm))){
  sum<-rbind(sum_gdm_dm_preg,sum_gdm_unk_gd,sum_gdm_pre_gd,sum_gdm_gd,sum_gdm_dm,sum_gdm)
  fwrite(sum,paste0(output_dir,"PE and GDM algorithm/Step_02_summary_gdm_icluded_record.csv"),row.names = F)
  rm(sum,sum_gdm_dm_preg,sum_gdm_unk_gd,sum_gdm_pre_gd,sum_gdm_gd,sum_gdm_dm,sum_gdm)
}

####PRE-ECLAMPSIA####
#HELLP
pe_files<-list.files(tmp, "_HELLP_")
if(length(pe_files)>0){
  pe_dt<-lapply(paste0(tmp,pe_files), readRDS)
  pe_dt<-as.data.table(rbindlist(pe_dt, fill=T))
  #remove duplicates
  pe_dt[,comb:=paste0(person_id, "_", event_date, "_", condition)]
  dup<-pe_dt[duplicated(comb),.N]
  if(dup>0){
    dup_dt_hellp<-data.table(StudyVar="PE_diagnoses/HELLP", removed_rec=dup)
  }else{
    dup_dt_hellp<-data.table(StudyVar="PE_diagnoses/HELLP", removed_rec=0)  
  }
  rm(dup)
  pe_dt<-pe_dt[!duplicated(comb)]
  pe_dt[,comb:=NULL]
  #Summary of included records by event_code, meaning, condition, year
  sum_hellp<-pe_dt[,.N, by=c("condition", "meaning", "year")]
  setnames(sum_hellp,"N", "no_records")
  #fwrite(sum,paste0(output_dir,"PE and GDM algorithm/summary_gdm_diagnoses_pre_gd.csv"))
  pe_dt[,event_code:=NULL][,meaning:=NULL][,year:=NULL][,event_vocabulary:=NULL][,truncated_code:=NULL][,prior_gdm_pe:=NULL][,prior_mig:=NULL][,prior_du:=NULL][,prior_saf:=NULL][,after_gdm_pe:=NULL][,after_mig:=NULL][,after_du:=NULL][,after_saf:=NULL][,code_no_dot:=NULL][,filter:=NULL]
  #save data in g_intermediate/populations
  saveRDS(pe_dt, paste0(g_intermediate,"pe_algorithm/HELLP.rds"))
  rm(pe_dt)
  #remove files
  #remove all gdm files from tmp
  for (i in 1:length(pe_files)){
    file.remove(paste0(tmp,pe_files[[i]]))
  }
  
}else{
  dup_dt_hellp<-data.table(StudyVar="PE_diagnoses/HELLP", removed_rec="N/A")
  sum_hellp<-NULL
}
rm(pe_files)

#ECL
pe_files<-list.files(tmp, "_ECL_")
if(length(pe_files)>0){
  pe_dt<-lapply(paste0(tmp,pe_files), readRDS)
  pe_dt<-as.data.table(rbindlist(pe_dt, fill=T))
  #remove duplicates
  pe_dt[,comb:=paste0(person_id, "_", event_date, "_", condition)]
  dup<-pe_dt[duplicated(comb),.N]
  if(dup>0){
    dup_dt_ecl<-data.table(StudyVar="PE_diagnoses/ECL", removed_rec=dup)
  }else{
    dup_dt_ecl<-data.table(StudyVar="PE_diagnoses/ECL", removed_rec=0)  
  }
  rm(dup)
  pe_dt<-pe_dt[!duplicated(comb)]
  pe_dt[,comb:=NULL]
  #Summary of included records by event_code, meaning, condition, year
  sum_ecl<-pe_dt[,.N, by=c("condition", "meaning", "year")]
  setnames(sum_ecl,"N", "no_records")
  #fwrite(sum,paste0(output_dir,"PE and GDM algorithm/summary_gdm_diagnoses_pre_gd.csv"))
  pe_dt[,event_code:=NULL][,meaning:=NULL][,year:=NULL][,event_vocabulary:=NULL][,truncated_code:=NULL][,prior_gdm_pe:=NULL][,prior_mig:=NULL][,prior_du:=NULL][,prior_saf:=NULL][,after_gdm_pe:=NULL][,after_mig:=NULL][,after_du:=NULL][,after_saf:=NULL][,code_no_dot:=NULL][,filter:=NULL]
  #save data in g_intermediate/populations
  saveRDS(pe_dt, paste0(g_intermediate,"pe_algorithm/ECL.rds"))
  rm(pe_dt)
  #remove files
  #remove all gdm files from tmp
  for (i in 1:length(pe_files)){
    file.remove(paste0(tmp,pe_files[[i]]))
  }
  
}else{
  dup_dt_ecl<-data.table(StudyVar="PE_diagnoses/ECL", removed_rec="N/A")
  sum_ecl<-NULL
}
rm(pe_files)


#PE
pe_files<-list.files(tmp, "_PE_")
if(length(pe_files)>0){
  pe_dt<-lapply(paste0(tmp,pe_files), readRDS)
  pe_dt<-as.data.table(rbindlist(pe_dt, fill=T))
  #remove duplicates
  pe_dt[,comb:=paste0(person_id, "_", event_date, "_", condition)]
  dup<-pe_dt[duplicated(comb),.N]
  if(dup>0){
    dup_dt_pe<-data.table(StudyVar="PE_diagnoses/PE", removed_rec=dup)
  }else{
    dup_dt_pe<-data.table(StudyVar="PE_diagnoses/PE", removed_rec=0)  
  }
  rm(dup)
  pe_dt<-pe_dt[!duplicated(comb)]
  pe_dt[,comb:=NULL]
  #Summary of included records by event_code, meaning, condition, year
  sum_dt_pe<-pe_dt[,.N, by=c("condition", "meaning", "year")]
  setnames(sum_dt_pe,"N", "no_records")
  #fwrite(sum,paste0(output_dir,"PE and GDM algorithm/summary_gdm_diagnoses_pre_gd.csv"))
  pe_dt[,event_code:=NULL][,meaning:=NULL][,year:=NULL][,event_vocabulary:=NULL][,truncated_code:=NULL][,prior_gdm_pe:=NULL][,prior_mig:=NULL][,prior_du:=NULL][,prior_saf:=NULL][,after_gdm_pe:=NULL][,after_mig:=NULL][,after_du:=NULL][,after_saf:=NULL][,code_no_dot:=NULL][,filter:=NULL]
  #save data in g_intermediate/populations
  saveRDS(pe_dt, paste0(g_intermediate,"pe_algorithm/PE.rds"))
  rm(pe_dt)
  #remove files
  #remove all gdm files from tmp
  for (i in 1:length(pe_files)){
    file.remove(paste0(tmp,pe_files[[i]]))
  }
  
}else{
  dup_dt_pe<-data.table(StudyVar="PE_diagnoses/PE", removed_rec="N/A")
  sum_dt_pe<-NULL
}
rm(pe_files)

#Combine dup results
if(sum(!is.null(dup_dt_hellp),!is.null(dup_dt_ecl),!is.null(dup_dt_pe),!is.null(dup_gdm_diag))){
  dup<-rbind(dup_gdm_diag,dup_dt_hellp,dup_dt_ecl,dup_dt_pe)
  rm(dup_gdm_diag,dup_dt_hellp,dup_dt_ecl,dup_dt_pe)
  fwrite(dup,paste0(output_dir,"PE and GDM algorithm/Step_02_gdm_pe_duplicated_diagnoses_removed.csv"),row.names = F)
  rm(dup)
}

#Combine sum results
if(sum(!is.null(sum_hellp),!is.null(sum_ecl),!is.null(sum_dt_pe),!is.null(sum_pe))){
  sum<-rbind(sum_hellp,sum_ecl,sum_dt_pe,sum_pe)
  fwrite(sum,paste0(output_dir,"PE and GDM algorithm/Step_02_summary_pe_icluded_record.csv"),row.names = F)
  rm(sum,sum_hellp,sum_ecl,sum_dt_pe,sum_pe)
}

####Migraine####
#MG_NO_AURA
mg_no_aura_files<-list.files(tmp, "_MG_NO_AURA_")
if(length(mg_no_aura_files)>0){
  mg_no_aura_dt<-lapply(paste0(tmp,mg_no_aura_files), readRDS)
  mg_no_aura_dt<-as.data.table(rbindlist(mg_no_aura_dt, fill=T))
  #remove duplicates
  mg_no_aura_dt[,comb:=paste0(person_id, "_", event_date, "_", condition)]
  dup<-mg_no_aura_dt[duplicated(comb),.N]
  if(dup>0){
    dup_dt_mg_no_aura<-data.table(StudyVar="Migraine_diagnoses/MG_NO_AURA", removed_rec=dup)
  }else{
    dup_dt_mg_no_aura<-data.table(StudyVar="Migraine_diagnoses/MG_NO_AURA", removed_rec=0)  
  }
  rm(dup)
  mg_no_aura_dt<-mg_no_aura_dt[!duplicated(comb)]
  mg_no_aura_dt[,comb:=NULL]
  #Summary of included records by event_code, meaning, condition, year
  sum_dt_mg_no_aura<-mg_no_aura_dt[,.N, by=c("condition", "meaning", "year")]
  setnames(sum_dt_mg_no_aura,"N", "no_records")
  #fwrite(sum,paste0(output_dir,"MG_NO_AURA and GDM algorithm/summary_gdm_diagnoses_pre_gd.csv"))
  mg_no_aura_dt[,event_code:=NULL][,meaning:=NULL][,year:=NULL][,event_vocabulary:=NULL][,prior_gdm_pe:=NULL][,prior_mig:=NULL][,prior_du:=NULL][,prior_saf:=NULL][,after_gdm_pe:=NULL][,after_mig:=NULL][,after_du:=NULL][,after_saf:=NULL][,code_no_dot:=NULL][,filter:=NULL]
  if("truncated_code" %in% names(mg_no_aura_dt)){mg_no_aura_dt[,truncated_code:=NULL]}
  #save data in g_intermediate/populations
  saveRDS(mg_no_aura_dt, paste0(g_intermediate,"migraine_algorithm/MG_NO_AURA.rds"))
  rm(mg_no_aura_dt)
  #remove files
  #remove all gdm files from tmp
  for (i in 1:length(mg_no_aura_files)){
    file.remove(paste0(tmp,mg_no_aura_files[[i]]))
  }
  
}else{
  dup_dt_mg_no_aura<-data.table(StudyVar="Migraine_diagnoses/MG_NO_AURA", removed_rec="N/A")
  sum_dt_mg_no_aura<-NULL
}
rm(mg_no_aura_files)

#MG_AURA
#MG_AURA
mg_aura_files<-list.files(tmp, "_MG_AURA_")
if(length(mg_aura_files)>0){
  mg_aura_dt<-lapply(paste0(tmp,mg_aura_files), readRDS)
  mg_aura_dt<-as.data.table(rbindlist(mg_aura_dt, fill=T))
  #remove duplicates
  mg_aura_dt[,comb:=paste0(person_id, "_", event_date, "_", condition)]
  dup<-mg_aura_dt[duplicated(comb),.N]
  if(dup>0){
    dup_dt_mg_aura<-data.table(StudyVar="Migraine_diagnoses/MG_AURA", removed_rec=dup)
  }else{
    dup_dt_mg_aura<-data.table(StudyVar="Migraine_diagnoses/MG_AURA", removed_rec=0)  
  }
  rm(dup)
  mg_aura_dt<-mg_aura_dt[!duplicated(comb)]
  mg_aura_dt[,comb:=NULL]
  #Summary of included records by event_code, meaning, condition, year
  sum_dt_mg_aura<-mg_aura_dt[,.N, by=c("condition", "meaning", "year")]
  setnames(sum_dt_mg_aura,"N", "no_records")
  #fwrite(sum,paste0(output_dir,"MG_AURA and GDM algorithm/summary_gdm_diagnoses_pre_gd.csv"))
  mg_aura_dt[,event_code:=NULL][,meaning:=NULL][,year:=NULL][,event_vocabulary:=NULL][,prior_gdm_pe:=NULL][,prior_mig:=NULL][,prior_du:=NULL][,prior_saf:=NULL][,after_gdm_pe:=NULL][,after_mig:=NULL][,after_du:=NULL][,after_saf:=NULL][,code_no_dot:=NULL][,filter:=NULL]
  if("truncated_code" %in% names(mg_aura_dt)){mg_aura_dt[,truncated_code:=NULL]}
  #save data in g_intermediate/populations
  saveRDS(mg_aura_dt, paste0(g_intermediate,"migraine_algorithm/MG_AURA.rds"))
  rm(mg_aura_dt)
  #remove files
  #remove all gdm files from tmp
  for (i in 1:length(mg_aura_files)){
    file.remove(paste0(tmp,mg_aura_files[[i]]))
  }
  
}else{
  dup_dt_mg_aura<-data.table(StudyVar="Migraine_diagnoses/MG_AURA", removed_rec="N/A")
  sum_dt_mg_aura<-NULL
}
rm(mg_aura_files)

#MG_STATUS
#MG_STATUS/MG_COMP MG_STACOMP
mg_status_files<-list.files(tmp, "_MG_STACOMP_")
if(length(mg_status_files)>0){
  mg_status_dt<-lapply(paste0(tmp,mg_status_files), readRDS)
  mg_status_dt<-as.data.table(rbindlist(mg_status_dt, fill=T))
  #remove duplicates
  mg_status_dt[,comb:=paste0(person_id, "_", event_date, "_", condition)]
  dup<-mg_status_dt[duplicated(comb),.N]
  if(dup>0){
    dup_dt_mg_status<-data.table(StudyVar="Migraine_diagnoses/MG_STACOMP", removed_rec=dup)
  }else{
    dup_dt_mg_status<-data.table(StudyVar="Migraine_diagnoses/MG_STACOMP", removed_rec=0)  
  }
  rm(dup)
  mg_status_dt<-mg_status_dt[!duplicated(comb)]
  mg_status_dt[,comb:=NULL]
  #Summary of included records by event_code, meaning, condition, year
  sum_dt_mg_status<-mg_status_dt[,.N, by=c("condition", "meaning", "year")]
  setnames(sum_dt_mg_status,"N", "no_records")
  #fwrite(sum,paste0(output_dir,"MG_STACOMP and GDM algorithm/summary_gdm_diagnoses_pre_gd.csv"))
  mg_status_dt[,event_code:=NULL][,meaning:=NULL][,year:=NULL][,event_vocabulary:=NULL][,prior_gdm_pe:=NULL][,prior_mig:=NULL][,prior_du:=NULL][,prior_saf:=NULL][,after_gdm_pe:=NULL][,after_mig:=NULL][,after_du:=NULL][,after_saf:=NULL][,code_no_dot:=NULL][,filter:=NULL]
  if("truncated_code" %in% names(mg_status_dt)){mg_status_dt[,truncated_code:=NULL]}
  
  #save data in g_intermediate/populations
  saveRDS(mg_status_dt, paste0(g_intermediate,"migraine_algorithm/MG_STACOMP.rds"))
  rm(mg_status_dt)
  #remove files
  #remove all gdm files from tmp
  for (i in 1:length(mg_status_files)){
    file.remove(paste0(tmp,mg_status_files[[i]]))
  }
  
}else{
  dup_dt_mg_status<-data.table(StudyVar="Migraine_diagnoses/MG_STACOMP", removed_rec="N/A")
  sum_dt_mg_status<-NULL
}
rm(mg_status_files)

#MG_OTHER
other_mg_files<-list.files(tmp, "_MG_OTHER_")
if(length(other_mg_files)>0){
  other_mg_dt<-lapply(paste0(tmp,other_mg_files), readRDS)
  other_mg_dt<-as.data.table(rbindlist(other_mg_dt, fill=T))
  #remove duplicates
  other_mg_dt[,comb:=paste0(person_id, "_", event_date, "_", condition)]
  dup<-other_mg_dt[duplicated(comb),.N]
  if(dup>0){
    dup_dt_other_mg<-data.table(StudyVar="Migraine_diagnoses/MG_OTHER", removed_rec=dup)
  }else{
    dup_dt_other_mg<-data.table(StudyVar="Migraine_diagnoses/MG_OTHER", removed_rec=0)  
  }
  rm(dup)
  other_mg_dt<-other_mg_dt[!duplicated(comb)]
  other_mg_dt[,comb:=NULL]
  #Summary of included records by event_code, meaning, condition, year
  sum_dt_other_mg<-other_mg_dt[,.N, by=c("condition", "meaning", "year")]
  setnames(sum_dt_other_mg,"N", "no_records")
  #fwrite(sum,paste0(output_dir,"MG_OTHER and GDM algorithm/summary_gdm_diagnoses_pre_gd.csv"))
  other_mg_dt[,event_code:=NULL][,meaning:=NULL][,year:=NULL][,event_vocabulary:=NULL][,prior_gdm_pe:=NULL][,prior_mig:=NULL][,prior_du:=NULL][,prior_saf:=NULL][,after_gdm_pe:=NULL][,after_mig:=NULL][,after_du:=NULL][,after_saf:=NULL][,code_no_dot:=NULL][,filter:=NULL]
  if("truncated_code" %in% names(other_mg_dt)){other_mg_dt[,truncated_code:=NULL]}
  #save data in g_intermediate/populations
  saveRDS(other_mg_dt, paste0(g_intermediate,"migraine_algorithm/MG_OTHER.rds"))
  rm(other_mg_dt)
  #remove files
  #remove all gdm files from tmp
  for (i in 1:length(other_mg_files)){
    file.remove(paste0(tmp,other_mg_files[[i]]))
  }
  
}else{
  dup_dt_other_mg<-data.table(StudyVar="Migraine_diagnoses/MG_OTHER", removed_rec="N/A")
  sum_dt_other_mg<-NULL
}
rm(other_mg_files)

#MG_UNSP
unsp_mg_files<-list.files(tmp, "_MG_UNSP_")
if(length(unsp_mg_files)>0){
  unsp_mg_dt<-lapply(paste0(tmp,unsp_mg_files), readRDS)
  unsp_mg_dt<-as.data.table(rbindlist(unsp_mg_dt, fill=T))
  #remove duplicates
  unsp_mg_dt[,comb:=paste0(person_id, "_", event_date, "_", condition)]
  dup<-unsp_mg_dt[duplicated(comb),.N]
  if(dup>0){
    dup_dt_unsp_mg<-data.table(StudyVar="Migraine_diagnoses/MG_UNSP", removed_rec=dup)
  }else{
    dup_dt_unsp_mg<-data.table(StudyVar="Migraine_diagnoses/MG_UNSP", removed_rec=0)  
  }
  rm(dup)
  unsp_mg_dt<-unsp_mg_dt[!duplicated(comb)]
  unsp_mg_dt[,comb:=NULL]
  #Summary of included records by event_code, meaning, condition, year
  sum_dt_unsp_mg<-unsp_mg_dt[,.N, by=c("condition", "meaning", "year")]
  setnames(sum_dt_unsp_mg,"N", "no_records")
  #fwrite(sum,paste0(output_dir,"UNSP_MG and GDM algorithm/summary_gdm_diagnoses_pre_gd.csv"))
  unsp_mg_dt[,event_code:=NULL][,meaning:=NULL][,year:=NULL][,event_vocabulary:=NULL][,prior_gdm_pe:=NULL][,prior_mig:=NULL][,prior_du:=NULL][,prior_saf:=NULL][,after_gdm_pe:=NULL][,after_mig:=NULL][,after_du:=NULL][,after_saf:=NULL][,code_no_dot:=NULL][,filter:=NULL]
  if("truncated_code" %in% names(unsp_mg_dt)){unsp_mg_dt[,truncated_code:=NULL]}
  
  #save data in g_intermediate/populations
  saveRDS(unsp_mg_dt, paste0(g_intermediate,"migraine_algorithm/MG_UNSP.rds"))
  rm(unsp_mg_dt)
  #remove files
  #remove all gdm files from tmp
  for (i in 1:length(unsp_mg_files)){
    file.remove(paste0(tmp,unsp_mg_files[[i]]))
  }
  
}else{
  dup_dt_unsp_mg<-data.table(StudyVar="Migraine_diagnoses/MG_UNSP", removed_rec="N/A")
  sum_dt_unsp_mg<-NULL
}
rm(unsp_mg_files)

#MG_UPC
comp_mg_files<-list.files(tmp, "_MG_UPC_")
if(length(comp_mg_files)>0){
  comp_mg_dt<-lapply(paste0(tmp,comp_mg_files), readRDS)
  comp_mg_dt<-as.data.table(rbindlist(comp_mg_dt, fill=T))
  #remove duplicates
  comp_mg_dt[,comb:=paste0(person_id, "_", event_date, "_", condition)]
  dup<-comp_mg_dt[duplicated(comb),.N]
  if(dup>0){
    dup_dt_comp_mg<-data.table(StudyVar="Migraine_diagnoses/MG_UPC", removed_rec=dup)
  }else{
    dup_dt_comp_mg<-data.table(StudyVar="Migraine_diagnoses/MG_UPC", removed_rec=0)  
  }
  rm(dup)
  comp_mg_dt<-comp_mg_dt[!duplicated(comb)]
  comp_mg_dt[,comb:=NULL]
  #Summary of included records by event_code, meaning, condition, year
  sum_dt_comp_mg<-comp_mg_dt[,.N, by=c("condition", "meaning", "year")]
  setnames(sum_dt_comp_mg,"N", "no_records")
  #fwrite(sum,paste0(output_dir,"COMP_MG and GDM algorithm/summary_gdm_diagnoses_pre_gd.csv"))
  comp_mg_dt[,event_code:=NULL][,meaning:=NULL][,year:=NULL][,event_vocabulary:=NULL][,prior_gdm_pe:=NULL][,prior_mig:=NULL][,prior_du:=NULL][,prior_saf:=NULL][,after_gdm_pe:=NULL][,after_mig:=NULL][,after_du:=NULL][,after_saf:=NULL][,code_no_dot:=NULL][,filter:=NULL]
  if("truncated_code" %in% names(comp_mg_dt)){comp_mg_dt[,truncated_code:=NULL]}
  
  #save data in g_intermediate/populations
  saveRDS(comp_mg_dt, paste0(g_intermediate,"migraine_algorithm/MG_UPC.rds"))
  rm(comp_mg_dt)
  #remove files
  #remove all gdm files from tmp
  for (i in 1:length(comp_mg_files)){
    file.remove(paste0(tmp,comp_mg_files[[i]]))
  }
  
}else{
  dup_dt_comp_mg<-data.table(StudyVar="Migraine_diagnoses/MG_UPC", removed_rec="N/A")
  sum_dt_comp_mg<-NULL
}
rm(comp_mg_files)

#MG
mg_files<-list.files(tmp, "_MG_")
if(length(mg_files)>0){
  mg_dt<-lapply(paste0(tmp,mg_files), readRDS)
  mg_dt<-as.data.table(rbindlist(mg_dt, fill=T))
  #remove duplicates
  mg_dt[,comb:=paste0(person_id, "_", event_date, "_", condition)]
  dup<-mg_dt[duplicated(comb),.N]
  if(dup>0){
    dup_dt_mg<-data.table(StudyVar="Migraine_diagnoses/MG", removed_rec=dup)
  }else{
    dup_dt_mg<-data.table(StudyVar="Migraine_diagnoses/MG", removed_rec=0)  
  }
  rm(dup)
  mg_dt<-mg_dt[!duplicated(comb)]
  mg_dt[,comb:=NULL]
  #Summary of included records by event_code, meaning, condition, year
  sum_dt_mg<-mg_dt[,.N, by=c("condition", "meaning", "year")]
  setnames(sum_dt_mg,"N", "no_records")
  #fwrite(sum,paste0(output_dir,"MG and GDM algorithm/summary_gdm_diagnoses_pre_gd.csv"))
  mg_dt[,event_code:=NULL][,meaning:=NULL][,year:=NULL][,event_vocabulary:=NULL][,prior_gdm_pe:=NULL][,prior_mig:=NULL][,prior_du:=NULL][,prior_saf:=NULL][,after_gdm_pe:=NULL][,after_mig:=NULL][,after_du:=NULL][,after_saf:=NULL][,code_no_dot:=NULL][,filter:=NULL]
  if("truncated_code" %in% names(mg_dt)){mg_dt[,truncated_code:=NULL]}
  
  #save data in g_intermediate/populations
  saveRDS(mg_dt, paste0(g_intermediate,"migraine_algorithm/MG.rds"))
  rm(mg_dt)
  #remove files
  #remove all gdm files from tmp
  for (i in 1:length(mg_files)){
    file.remove(paste0(tmp,mg_files[[i]]))
  }
  
}else{
  dup_dt_mg<-data.table(StudyVar="Migraine_diagnoses/MG", removed_rec="N/A")
  sum_dt_mg<-NULL
}
rm(mg_files)

#Combine dup results
if(sum(!is.null(dup_dt_mg_no_aura),!is.null(dup_dt_mg_aura),!is.null(dup_dt_mg_status),!is.null(dup_dt_other_mg),!is.null(dup_dt_unsp_mg),!is.null(dup_dt_comp_mg),!is.null(dup_dt_mg))){
  dup<-rbind(dup_dt_mg_no_aura,dup_dt_mg_aura,dup_dt_mg_status,dup_dt_other_mg,dup_dt_unsp_mg,dup_dt_comp_mg,dup_dt_mg)
  rm(dup_dt_mg_no_aura,dup_dt_mg_aura,dup_dt_mg_status,dup_dt_other_mg,dup_dt_unsp_mg,dup_dt_comp_mg,dup_dt_mg)
  fwrite(dup,paste0(output_dir,"Migraine algorithm/Step_02_mig_duplicated_diagnoses_removed.csv"),row.names = F)
  rm(dup)
}

#Combine sum results
if(sum(!is.null(sum_dt_mg_no_aura),!is.null(sum_dt_mg_aura),!is.null(sum_dt_mg_status),!is.null(sum_dt_other_mg),!is.null(sum_dt_unsp_mg),!is.null(sum_dt_comp_mg),!is.null(sum_dt_mg))){
  sum<-rbind(sum_dt_mg_no_aura,sum_dt_mg_aura,sum_dt_mg_status,sum_dt_other_mg,sum_dt_unsp_mg,sum_dt_comp_mg,sum_dt_mg)
  fwrite(sum,paste0(output_dir,"Migraine algorithm/Step_02_summary_mig_icluded_record.csv"),row.names = F)
  rm(sum,sum_dt_mg_no_aura,sum_dt_mg_aura,sum_dt_mg_status,sum_dt_other_mg,sum_dt_unsp_mg,sum_dt_comp_mg,sum_dt_mg)
}

date_running_end_02_c<-Sys.Date()
end_time_02_c<-Sys.time()

time_log_02_c<-data.table(DAP=data_access_provider_name,
                     Script="Step_02_diagnoses_clean_up_combine.R", 
                     Start_date=date_running_start_02_c, 
                     End_date=date_running_end_02_c,
                     Time_elaspsed=format(end_time_02_c-initial_time_02_c, digits=2))
fwrite(time_log_02_c,paste0(output_dir,"/Time log/Step_02_combine_time_log.csv"),row.names = F)
rm(time_log_02_c)

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
