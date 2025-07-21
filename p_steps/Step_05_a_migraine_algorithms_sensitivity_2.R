initial_time_05_a<-Sys.time()
date_running_start_05_a<-Sys.Date()

####APPLY Migraine ALGORITHM####
algorithm_template<-fread(paste0(projectFolder, "/p_steps/parameters/algorithms.csv"))



#### MIG_DxRx_a:Prevalence of migraine at baseline when lookback==12 months(3 months) ####
print("Create algorithm MIG_DxRx_a_S")
MIG_DxRx_a<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_DxRx_a"]
alt_col<-MIG_DxRx_a[TYPE=="OR",STUDY_VARIABLES]

if(length(alt_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("|" , lapply(.SD,`>=`, 1)),.SDcols=alt_col],alternative:=1]}else{pregnancy_d3_mig[,alternative:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm_sensitivity/MIG_DxRx_a_D3.rds"))


###### Types
MIG_tmp<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_T1_a"]
inc_col<-MIG_tmp[TYPE=="AND",STUDY_VARIABLES]
if(length(inc_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col],MIG_T1:=1]}else{pregnancy_d3_mig[,MIG_T1:=NA]}

MIG_tmp<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_T2_a"]
inc_col<-MIG_tmp[TYPE=="AND",STUDY_VARIABLES]
if(length(inc_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col],MIG_T2:=1]}else{pregnancy_d3_mig[,MIG_T2:=NA]}

MIG_tmp<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_T3_a"]
inc_col<-MIG_tmp[TYPE=="AND",STUDY_VARIABLES]
if(length(inc_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col],MIG_T3:=1]}else{pregnancy_d3_mig[,MIG_T3:=NA]}

MIG_tmp<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_T4_a"]
inc_col<-MIG_tmp[TYPE=="AND",STUDY_VARIABLES]
if(length(inc_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col],MIG_T4:=1]}else{pregnancy_d3_mig[,MIG_T4:=NA]}




pregnancy_d3_mig[, MIG_S4:=0][, MIG_S3:=0][, MIG_S2:=0][, MIG_S1:=0]#[, MIG_DxRx := 0]
#pregnancy_d3_mig[, MIG_DxRx:= fifelse(Migraine_medicines>0 | MG>0, 1, 0)]

pregnancy_d3_mig[, MIG_S4:=fifelse(Migraine_med_profilactic>0 & MG>0, 1, 0)]

pregnancy_d3_mig[, MIG_S3:=fifelse(MIG_S4 == 0 & 
                                            (Migraine_med_profilactic_baseline>0 | Migraine_injections>0) & MG > 0, 
                                          1, 0)]

pregnancy_d3_mig[, MIG_S2:=fifelse(MIG_S4 == 0 & MIG_S3 == 0 &
                                            Migraine_medicines>0 ,
                                          1, 0)]

pregnancy_d3_mig[, MIG_S1:=fifelse(MIG_S4 == 0 & MIG_S3 == 0  & MIG_S2 == 0 &
                                            MG>0,
                                          1, 0)]


#### Save time info ####
date_running_end_05_a<-Sys.Date()
end_time_05_a<-Sys.time()

time_log_05_a<-data.table(DAP=data_access_provider_name,
                          Script="Step_05_a_migraine_algorithms_sensitivity.R", 
                          Start_date=date_running_start_05_a, 
                          End_date=date_running_end_05_a,
                          Time_elaspsed=format(end_time_05_a-initial_time_05_a, digits=2))
fwrite(time_log_05_a,paste0(output_dir,"/Time log/Step_05_a_time_log.csv"),row.names = F)
rm(time_log_05_a)

