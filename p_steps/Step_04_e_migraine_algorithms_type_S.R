initial_time_04_e<-Sys.time()
date_running_start_04_e<-Sys.Date()


#Identify all events needed to create the migraine type algorithms

#MIG_T1: MG_NO_AURA, baseline and during pregnancy
#MIG_T2: MG_AURA, baseline and during pregnancy
#MIG_T3: MG_STACOMP, baseline and during pregnancy
#MIG_T4: MG_OTHER, baseline and during pregnancy
#MIG_T5: MG_UPC, baseline and during pregnancy
#MIG_T6: MG_UNSP, baseline and during pregnancy


####APPLY Migraine ALGORITHM####
algorithm_template<-fread(paste0(projectFolder, "/p_steps/parameters/algorithms.csv"))
#### MIG_T1_a:Prevalence of MG_NO_AURA at baseline when lookback==12 months(3 months) ####
print("Create algorithm MIG_T1_a")
MIG_T1_a<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_T1_a"]
inc_col<-MIG_T1_a[TYPE=="AND",STUDY_VARIABLES]

if(length(inc_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col],include:=1]}else{pregnancy_d3_mig[,include:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm_sensitivity/final_d3/MIG_T1_a_D3.rds"))
pregnancy_d3_mig[,include:=NULL]

#### MIG_T1_b:Prevalence of MG_NO_AURA at baseline when lookback==3 months ####
print("Create algorithm MIG_T1_b")
MIG_T1_b<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_T1_b"]
inc_col<-MIG_T1_b[TYPE=="AND",STUDY_VARIABLES]

if(length(inc_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col],include:=1]}else{pregnancy_d3_mig[,include:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm_sensitivity/final_d3/MIG_T1_b_D3.rds"))
pregnancy_d3_mig[,include:=NULL]

#### MIG_T1_during:Prevalence of MG_NO_AURA during pregnancy when lookback==12 months(3 months) ####
print("Create algorithm MIG_T1_during")
MIG_T1_during<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_T1_during"]
inc_col<-MIG_T1_during[TYPE=="AND",STUDY_VARIABLES]

if(length(inc_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col],include:=1]}else{pregnancy_d3_mig[,include:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm_sensitivity/final_d3/MIG_T1_during_D3.rds"))
pregnancy_d3_mig[,include:=NULL]

#### MIG_T2_a:Prevalence of MG_AURA at baseline when lookback==12 months(3 months) ####
print("Create algorithm MIG_T2_a")
MIG_T2_a<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_T2_a"]
inc_col<-MIG_T2_a[TYPE=="AND",STUDY_VARIABLES]

if(length(inc_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col],include:=1]}else{pregnancy_d3_mig[,include:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm_sensitivity/final_d3/MIG_T2_a_D3.rds"))
pregnancy_d3_mig[,include:=NULL]

#### MIG_T2_b:Prevalence of MG_AURA at baseline when lookback==3 months ####
print("Create algorithm MIG_T2_b")
MIG_T2_b<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_T2_b"]
inc_col<-MIG_T2_b[TYPE=="AND",STUDY_VARIABLES]

if(length(inc_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col],include:=1]}else{pregnancy_d3_mig[,include:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm_sensitivity/final_d3/MIG_T2_b_D3.rds"))
pregnancy_d3_mig[,include:=NULL]

#### MIG_T2_during:Prevalence of MG_AURA during pregnancy when lookback==12 months(3 months) ####
print("Create algorithm MIG_T2_during")
MIG_T2_during<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_T2_during"]
inc_col<-MIG_T2_during[TYPE=="AND",STUDY_VARIABLES]

if(length(inc_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col],include:=1]}else{pregnancy_d3_mig[,include:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm_sensitivity/final_d3/MIG_T2_during_D3.rds"))
pregnancy_d3_mig[,include:=NULL]

#### MIG_T3_a:Prevalence of MG_STACOMP at baseline when lookback==12 months(3 months) ####
print("Create algorithm MIG_T3_a")
MIG_T3_a<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_T3_a"]
inc_col<-MIG_T3_a[TYPE=="AND",STUDY_VARIABLES]

if(length(inc_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col],include:=1]}else{pregnancy_d3_mig[,include:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm_sensitivity/final_d3/MIG_T3_a_D3.rds"))
pregnancy_d3_mig[,include:=NULL]

#### MIG_T3_b:Prevalence of MG_STACOMP at baseline when lookback==3 months ####
print("Create algorithm MIG_T3_b")
MIG_T3_b<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_T3_b"]
inc_col<-MIG_T3_b[TYPE=="AND",STUDY_VARIABLES]

if(length(inc_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col],include:=1]}else{pregnancy_d3_mig[,include:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm_sensitivity/final_d3/MIG_T3_b_D3.rds"))
pregnancy_d3_mig[,include:=NULL]

#### MIG_T3_during:Prevalence of MG_STACOMP during pregnancy when lookback==12 months(3 months) ####
print("Create algorithm MIG_T3_during")
MIG_T3_during<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_T3_during"]
inc_col<-MIG_T3_during[TYPE=="AND",STUDY_VARIABLES]

if(length(inc_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col],include:=1]}else{pregnancy_d3_mig[,include:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm_sensitivity/final_d3/MIG_T3_during_D3.rds"))
pregnancy_d3_mig[,include:=NULL]

#### MIG_T4_a:Prevalence of MG_OTHER at baseline when lookback==12 months(3 months) ####
print("Create algorithm MIG_T4_a")
MIG_T4_a<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_T4_a"]
inc_col<-MIG_T4_a[TYPE=="AND",STUDY_VARIABLES]

if(length(inc_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col],include:=1]}else{pregnancy_d3_mig[,include:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm_sensitivity/final_d3/MIG_T4_a_D3.rds"))
pregnancy_d3_mig[,include:=NULL]

#### MIG_T4_b:Prevalence of MG_OTHER at baseline when lookback==3 months ####
print("Create algorithm MIG_T4_b")
MIG_T4_b<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_T4_b"]
inc_col<-MIG_T4_b[TYPE=="AND",STUDY_VARIABLES]

if(length(inc_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col],include:=1]}else{pregnancy_d3_mig[,include:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm_sensitivity/final_d3/MIG_T4_b_D3.rds"))
pregnancy_d3_mig[,include:=NULL]

#### MIG_T4_during:Prevalence of MG_OTHER during pregnancy when lookback==12 months(3 months) ####
print("Create algorithm MIG_T4_during")
MIG_T4_during<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_T4_during"]
inc_col<-MIG_T4_during[TYPE=="AND",STUDY_VARIABLES]

if(length(inc_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col],include:=1]}else{pregnancy_d3_mig[,include:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm_sensitivity/final_d3/MIG_T4_during_D3.rds"))
pregnancy_d3_mig[,include:=NULL]

#### MIG_T5_a:Prevalence of MG_UPC at baseline when lookback==12 months(3 months) ####
print("Create algorithm MIG_T5_a")
MIG_T5_a<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_T5_a"]
inc_col<-MIG_T5_a[TYPE=="AND",STUDY_VARIABLES]

if(length(inc_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col],include:=1]}else{pregnancy_d3_mig[,include:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm_sensitivity/final_d3/MIG_T5_a_D3.rds"))
pregnancy_d3_mig[,include:=NULL]

#### MIG_T5_b:Prevalence of MG_UPC at baseline when lookback==3 months ####
print("Create algorithm MIG_T5_b")
MIG_T5_b<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_T5_b"]
inc_col<-MIG_T5_b[TYPE=="AND",STUDY_VARIABLES]

if(length(inc_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col],include:=1]}else{pregnancy_d3_mig[,include:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm_sensitivity/final_d3/MIG_T5_b_D3.rds"))
pregnancy_d3_mig[,include:=NULL]

#### MIG_T5_during:Prevalence of MG_UPC during pregnancy when lookback==12 months(3 months) ####
print("Create algorithm MIG_T5_during")
MIG_T5_during<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_T5_during"]
inc_col<-MIG_T5_during[TYPE=="AND",STUDY_VARIABLES]

if(length(inc_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col],include:=1]}else{pregnancy_d3_mig[,include:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm_sensitivity/final_d3/MIG_T5_during_D3.rds"))
pregnancy_d3_mig[,include:=NULL]

#### MIG_T6_a:Prevalence of MG_UNSP at baseline when lookback==12 months(3 months) ####
print("Create algorithm MIG_T6_a")
MIG_T6_a<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_T6_a"]
inc_col<-MIG_T6_a[TYPE=="AND",STUDY_VARIABLES]

if(length(inc_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col],include:=1]}else{pregnancy_d3_mig[,include:=NA]}

#Add remaining Migraines diagnosed with Rx to T6
MG_Rx = readRDS("g_intermediate/migraine_algorithm_sensitivity/final_d3/MIG_Rx_a_D3.rds")
MG_Rx = MG_Rx[, .(person_id, pregnancy_id, pregnancy_start_date, pregnancy_end_date, include)]

MG_T1 = readRDS("g_intermediate/migraine_algorithm_sensitivity/final_d3/MIG_T1_a_D3.rds")
MG_T2 = readRDS("g_intermediate/migraine_algorithm_sensitivity/final_d3/MIG_T2_a_D3.rds")
MG_T3 = readRDS("g_intermediate/migraine_algorithm_sensitivity/final_d3/MIG_T3_a_D3.rds")
MG_T4 = readRDS("g_intermediate/migraine_algorithm_sensitivity/final_d3/MIG_T4_a_D3.rds")
MG_T5 = readRDS("g_intermediate/migraine_algorithm_sensitivity/final_d3/MIG_T5_a_D3.rds")
MG_T1 = MG_T1[, .(person_id, pregnancy_id, pregnancy_start_date, pregnancy_end_date, include)]
MG_T2 = MG_T2[, .(person_id, pregnancy_id, pregnancy_start_date, pregnancy_end_date, include)]
MG_T3 = MG_T3[, .(person_id, pregnancy_id, pregnancy_start_date, pregnancy_end_date, include)]
MG_T4 = MG_T4[, .(person_id, pregnancy_id, pregnancy_start_date, pregnancy_end_date, include)]
MG_T5 = MG_T5[, .(person_id, pregnancy_id, pregnancy_start_date, pregnancy_end_date, include)]
setnames(MG_T1, "include", "include.T1")
setnames(MG_T2, "include", "include.T2")
setnames(MG_T3, "include", "include.T3")
setnames(MG_T4, "include", "include.T4")
setnames(MG_T5, "include", "include.T5")
setnames(MG_Rx, "include", "include.Rx")

MG_merged = merge(MG_Rx, MG_T1, by = c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date"), all.x = TRUE)
MG_merged = merge(MG_merged, MG_T2, by = c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date"), all.x = TRUE)
MG_merged = merge(MG_merged, MG_T3, by = c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date"), all.x = TRUE)
MG_merged = merge(MG_merged, MG_T4, by = c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date"), all.x = TRUE)
MG_merged = merge(MG_merged, MG_T5, by = c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date"), all.x = TRUE)

MG_merged[!is.na(include.Rx) & (!is.na(include.T1) | !is.na(include.T2) | !is.na(include.T3) | !is.na(include.T4) | !is.na(include.T5)), include.Rx := NA]

if(any(!is.na(MG_merged[, include.Rx]))){
  MG_merged[, include.T1:=NULL][, include.T2:=NULL][, include.T3:=NULL][, include.T4:=NULL][, include.T5:=NULL]
  pregnancy_d3_mig = merge(pregnancy_d3_mig, MG_merged, by = c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date"),
                           all.x = TRUE)
  pregnancy_d3_mig[!is.na(include.Rx), include:=1]
  pregnancy_d3_mig[, include.Rx:=NULL] 
}
saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm_sensitivity/final_d3/MIG_T6_a_D3.rds"))
pregnancy_d3_mig[,include:=NULL]

#### MIG_T6_b:Prevalence of MG_UNSP at baseline when lookback==3 months ####
print("Create algorithm MIG_T6_b")
MIG_T6_b<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_T6_b"]
inc_col<-MIG_T6_b[TYPE=="AND",STUDY_VARIABLES]

if(length(inc_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col],include:=1]}else{pregnancy_d3_mig[,include:=NA]}
#Add remaining Migraines diagnosed with Rx to T6
MG_Rx = readRDS("g_intermediate/migraine_algorithm_sensitivity/final_d3/MIG_Rx_b_D3.rds")
MG_Rx = MG_Rx[, .(person_id, pregnancy_id, pregnancy_start_date, pregnancy_end_date, include)]

MG_T1 = readRDS("g_intermediate/migraine_algorithm_sensitivity/final_d3/MIG_T1_b_D3.rds")
MG_T2 = readRDS("g_intermediate/migraine_algorithm_sensitivity/final_d3/MIG_T2_b_D3.rds")
MG_T3 = readRDS("g_intermediate/migraine_algorithm_sensitivity/final_d3/MIG_T3_b_D3.rds")
MG_T4 = readRDS("g_intermediate/migraine_algorithm_sensitivity/final_d3/MIG_T4_b_D3.rds")
MG_T5 = readRDS("g_intermediate/migraine_algorithm_sensitivity/final_d3/MIG_T5_b_D3.rds")
MG_T1 = MG_T1[, .(person_id, pregnancy_id, pregnancy_start_date, pregnancy_end_date, include)]
MG_T2 = MG_T2[, .(person_id, pregnancy_id, pregnancy_start_date, pregnancy_end_date, include)]
MG_T3 = MG_T3[, .(person_id, pregnancy_id, pregnancy_start_date, pregnancy_end_date, include)]
MG_T4 = MG_T4[, .(person_id, pregnancy_id, pregnancy_start_date, pregnancy_end_date, include)]
MG_T5 = MG_T5[, .(person_id, pregnancy_id, pregnancy_start_date, pregnancy_end_date, include)]
setnames(MG_T1, "include", "include.T1")
setnames(MG_T2, "include", "include.T2")
setnames(MG_T3, "include", "include.T3")
setnames(MG_T4, "include", "include.T4")
setnames(MG_T5, "include", "include.T5")
setnames(MG_Rx, "include", "include.Rx")

MG_merged = merge(MG_Rx, MG_T1, by = c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date"), all.x = TRUE)
MG_merged = merge(MG_merged, MG_T2, by = c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date"), all.x = TRUE)
MG_merged = merge(MG_merged, MG_T3, by = c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date"), all.x = TRUE)
MG_merged = merge(MG_merged, MG_T4, by = c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date"), all.x = TRUE)
MG_merged = merge(MG_merged, MG_T5, by = c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date"), all.x = TRUE)

MG_merged[!is.na(include.Rx) & (!is.na(include.T1) | !is.na(include.T2) | !is.na(include.T3) | !is.na(include.T4) | !is.na(include.T5)), include.Rx := NA]

if(any(!is.na(MG_merged[, include.Rx]))){
  MG_merged[, include.T1:=NULL][, include.T2:=NULL][, include.T3:=NULL][, include.T4:=NULL][, include.T5:=NULL]
  pregnancy_d3_mig = merge(pregnancy_d3_mig, MG_merged, by = c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date"),
                           all.x = TRUE)
  pregnancy_d3_mig[!is.na(include.Rx), include:=1]
  pregnancy_d3_mig[, include.Rx:=NULL] 
}
saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm_sensitivity/final_d3/MIG_T6_b_D3.rds"))
pregnancy_d3_mig[,include:=NULL]

#### MIG_T6_during:Prevalence of MG_UNSP during pregnancy when lookback==12 months(3 months) ####
print("Create algorithm MIG_T6_during")
MIG_T6_during<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_T6_during"]
inc_col<-MIG_T6_during[TYPE=="AND",STUDY_VARIABLES]

if(length(inc_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col],include:=1]}else{pregnancy_d3_mig[,include:=NA]}
#Add remaining Migraines diagnosed with Rx to T6
MG_Rx = readRDS("g_intermediate/migraine_algorithm_sensitivity/final_d3/MIG_Rx_during_D3.rds")
MG_Rx = MG_Rx[, .(person_id, pregnancy_id, pregnancy_start_date, pregnancy_end_date, include)]

MG_T1 = readRDS("g_intermediate/migraine_algorithm_sensitivity/final_d3/MIG_T1_during_D3.rds")
MG_T2 = readRDS("g_intermediate/migraine_algorithm_sensitivity/final_d3/MIG_T2_during_D3.rds")
MG_T3 = readRDS("g_intermediate/migraine_algorithm_sensitivity/final_d3/MIG_T3_during_D3.rds")
MG_T4 = readRDS("g_intermediate/migraine_algorithm_sensitivity/final_d3/MIG_T4_during_D3.rds")
MG_T5 = readRDS("g_intermediate/migraine_algorithm_sensitivity/final_d3/MIG_T5_during_D3.rds")
MG_T1 = MG_T1[, .(person_id, pregnancy_id, pregnancy_start_date, pregnancy_end_date, include)]
MG_T2 = MG_T2[, .(person_id, pregnancy_id, pregnancy_start_date, pregnancy_end_date, include)]
MG_T3 = MG_T3[, .(person_id, pregnancy_id, pregnancy_start_date, pregnancy_end_date, include)]
MG_T4 = MG_T4[, .(person_id, pregnancy_id, pregnancy_start_date, pregnancy_end_date, include)]
MG_T5 = MG_T5[, .(person_id, pregnancy_id, pregnancy_start_date, pregnancy_end_date, include)]
setnames(MG_T1, "include", "include.T1")
setnames(MG_T2, "include", "include.T2")
setnames(MG_T3, "include", "include.T3")
setnames(MG_T4, "include", "include.T4")
setnames(MG_T5, "include", "include.T5")
setnames(MG_Rx, "include", "include.Rx")

MG_merged = merge(MG_Rx, MG_T1, by = c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date"), all.x = TRUE)
MG_merged = merge(MG_merged, MG_T2, by = c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date"), all.x = TRUE)
MG_merged = merge(MG_merged, MG_T3, by = c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date"), all.x = TRUE)
MG_merged = merge(MG_merged, MG_T4, by = c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date"), all.x = TRUE)
MG_merged = merge(MG_merged, MG_T5, by = c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date"), all.x = TRUE)

MG_merged[!is.na(include.Rx) & (!is.na(include.T1) | !is.na(include.T2) | !is.na(include.T3) | !is.na(include.T4) | !is.na(include.T5)), include.Rx := NA]

if(any(!is.na(MG_merged[, include.Rx]))){
  MG_merged[, include.T1:=NULL][, include.T2:=NULL][, include.T3:=NULL][, include.T4:=NULL][, include.T5:=NULL]
  pregnancy_d3_mig = merge(pregnancy_d3_mig, MG_merged, by = c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date"),
                           all.x = TRUE)
  pregnancy_d3_mig[!is.na(include.Rx), include:=1]
  pregnancy_d3_mig[, include.Rx:=NULL] 
}
saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm_sensitivity/final_d3/MIG_T6_during_D3.rds"))
pregnancy_d3_mig[,include:=NULL]

#### Save time info ####
date_running_end_04_e<-Sys.Date()
end_time_04_e<-Sys.time()

time_log_04_e<-data.table(DAP=data_access_provider_name,
                          Script="Step_04_e_migraine_algorithms_type_S.R", 
                          Start_date=date_running_start_04_e, 
                          End_date=date_running_end_04_e,
                          Time_elaspsed=format(end_time_04_e-initial_time_04_e, digits=2))
fwrite(time_log_04_e,paste0(output_dir,"/Time log/Step_04_e_S_time_log.csv"),row.names = F)
rm(time_log_04_e)





