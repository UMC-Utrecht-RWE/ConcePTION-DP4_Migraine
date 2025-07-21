initial_time_04_f<-Sys.time()
date_running_start_04_f<-Sys.Date()

saveRDS(pregnancy_d3_mig, "pregnancy_d3_mig_step5_sensitivity.rds", compress = FALSE)

#Identify all events needed to create the migraine type algorithms

#MIG_S1: MG diagnosis and no Migraine_medicines(N02CC) or Migraine_med_profilactic before and during pregnancy, baseline and during pregnancy
#MIG_S2: Migraine_medicines and no Migraine_medicines_injection, baseline and during pregnancy
#MIG_S3: Migraine_medicines_injection and no Migraine_med_profilactic during, baseline and during pregnancy
#MIG_S4: MG or Migraine_medicines and Migraine_med_profilactic,during pregnancy

####APPLY Migraine ALGORITHM####

################################
### MIG_S[1-3]_a ###############
################################

pregnancy_d3_mig[, MIG_S3_a:=0][, MIG_S2_a:=0][, MIG_S1_a:=0]#[, MIG_DxRx_a := 0]
#pregnancy_d3_mig[, MIG_DxRx_a:= fifelse(Migraine_medicines_baseline>0 | MG_baseline>0, 1, 0)]

pregnancy_d3_mig[, MIG_S3_a:=fifelse((Migraine_med_profilactic_baseline>0 | Migraine_injections_baseline>0) &  MG_baseline > 0, 1, 0)]

pregnancy_d3_mig[, MIG_S2_a:=fifelse(MIG_S3_a == 0 & Migraine_medicines_baseline>0, 1, 0)]

pregnancy_d3_mig[, MIG_S1_a:=fifelse(MIG_S3_a == 0  & MIG_S2_a == 0 & MG_baseline>0, 1, 0)]

################################
### MIG_S[1-3]_b ###############
################################

pregnancy_d3_mig[, MIG_S3_b:=0][, MIG_S2_b:=0][, MIG_S1_b:=0]#[, MIG_DxRx_a := 0]
#pregnancy_d3_mig[, MIG_DxRx_b:= fifelse(Migraine_medicines_baseline>0 | MG_baseline>0, 1, 0)]

pregnancy_d3_mig[, MIG_S3_b:=fifelse((Migraine_med_profilactic_baseline>0 | Migraine_injections_baseline>0) &  MG_baseline > 0, 1, 0)]

pregnancy_d3_mig[, MIG_S2_b:=fifelse(MIG_S3_b == 0 & Migraine_medicines_baseline>0, 1, 0)]

pregnancy_d3_mig[, MIG_S1_b:=fifelse(MIG_S3_b == 0  & MIG_S2_b == 0 & MG_baseline>0, 1, 0)]

################################
### MIG_S[1-4]_during ##########
################################

pregnancy_d3_mig[, MIG_S4_during:=0][, MIG_S3_during:=0][, MIG_S2_during:=0][, MIG_S1_during:=0]#[, MIG_DxRx_during := 0]
#pregnancy_d3_mig[, MIG_DxRx_during:= fifelse(Migraine_medicines_during>0 | MG_during>0, 1, 0)]

pregnancy_d3_mig[, MIG_S4_during:=fifelse(Migraine_med_profilactic_during>0 & MG_during>0, 1, 0)]

pregnancy_d3_mig[, MIG_S3_during:=fifelse(MIG_S4_during == 0 & 
                                        (Migraine_med_profilactic_baseline>0 | Migraine_injections_during>0) & MG_during > 0, 
                                      1, 0)]

pregnancy_d3_mig[, MIG_S2_during:=fifelse(MIG_S4_during == 0 & MIG_S3_during == 0 &
                              Migraine_medicines_during>0 ,
                            1, 0)]

pregnancy_d3_mig[, MIG_S1_during:=fifelse(MIG_S4_during == 0 & MIG_S3_during == 0  & MIG_S2_during == 0 &
                              MG_during>0,
                            1, 0)]

################################
### MIG_S[1-4]_during2##########
################################

pregnancy_d3_mig[, MIG_S4_during2:=0][, MIG_S3_during2:=0][, MIG_S2_during2:=0][, MIG_S1_during2:=0]#[, MIG_DxRx_during := 0]
#pregnancy_d3_mig[, MIG_DxRx_during:= fifelse(Migraine_medicines_during>0 | MG_during>0, 1, 0)]

pregnancy_d3_mig[, MIG_S4_during2:=fifelse(Migraine_med_profilactic_during>0 & (MG_during>0 | MG_baseline>0), 1, 0)]

pregnancy_d3_mig[, MIG_S3_during2:=fifelse(MIG_S4_during2 == 0 & 
                                            (Migraine_med_profilactic_baseline>0 | (Migraine_injections_during>0 | Migraine_injections_baseline>0) & (MG_during>0 | MG_baseline>0)), 
                                          1, 0)]

pregnancy_d3_mig[, MIG_S2_during2:=fifelse(MIG_S4_during2 == 0 & MIG_S3_during2 == 0 &
                                            (Migraine_medicines_during>0 | Migraine_medicines_baseline>0),
                                          1, 0)]

pregnancy_d3_mig[, MIG_S1_during2:=fifelse(MIG_S4_during2 == 0 & MIG_S3_during2 == 0  & MIG_S2_during2 == 0 &
                                            (MG_during>0 | MG_baseline>0),
                                          1, 0)]

################################
#### Add severity hierarchy ####
cols_to_update <- c("MIG_S1_a", "MIG_S1_b", "MIG_S1_during", 
                    "MIG_S2_a", "MIG_S2_b", "MIG_S2_during", 
                    "MIG_S3_a", "MIG_S3_b", "MIG_S3_during", "MIG_S4_during",
                    "MIG_S1_during2", "MIG_S2_during2", "MIG_S3_during2", "MIG_S4_during2")  

# Replace NA with 0 in the specified columns
pregnancy_d3_mig[, (cols_to_update) := lapply(.SD, function(x) { ifelse(is.na(x), 0, x) }), .SDcols = cols_to_update]

#export 

cols<-c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date", "year", "birth_date", "death_date", "op_end_date_mig",
        "op_start_date_mig", "age", "maternal_age", "year_group", "include")

setnames(pregnancy_d3_mig, "MIG_S1_a", "include")
saveRDS(pregnancy_d3_mig[,cols, with=F],paste0(projectFolder,"/g_intermediate/migraine_algorithm_sensitivity/final_d3/MIG_S1_a_D3.rds"))
pregnancy_d3_mig[,include:=NULL]

setnames(pregnancy_d3_mig, "MIG_S1_b", "include")
saveRDS(pregnancy_d3_mig[,cols, with=F],paste0(projectFolder,"/g_intermediate/migraine_algorithm_sensitivity/final_d3/MIG_S1_b_D3.rds"))
pregnancy_d3_mig[,include:=NULL]

setnames(pregnancy_d3_mig, "MIG_S1_during", "include")
saveRDS(pregnancy_d3_mig[,cols, with=F],paste0(projectFolder,"/g_intermediate/migraine_algorithm_sensitivity/final_d3/MIG_S1_during_D3.rds"))
pregnancy_d3_mig[,include:=NULL]

setnames(pregnancy_d3_mig, "MIG_S2_a", "include")
saveRDS(pregnancy_d3_mig[,cols, with=F],paste0(projectFolder,"/g_intermediate/migraine_algorithm_sensitivity/final_d3/MIG_S2_a_D3.rds"))
pregnancy_d3_mig[,include:=NULL]

setnames(pregnancy_d3_mig, "MIG_S2_b", "include")
saveRDS(pregnancy_d3_mig[,cols, with=F],paste0(projectFolder,"/g_intermediate/migraine_algorithm_sensitivity/final_d3/MIG_S2_b_D3.rds"))
pregnancy_d3_mig[,include:=NULL]

setnames(pregnancy_d3_mig, "MIG_S2_during", "include")
saveRDS(pregnancy_d3_mig[,cols, with=F],paste0(projectFolder,"/g_intermediate/migraine_algorithm_sensitivity/final_d3/MIG_S2_during_D3.rds"))
pregnancy_d3_mig[,include:=NULL]

setnames(pregnancy_d3_mig, "MIG_S3_a", "include")
saveRDS(pregnancy_d3_mig[,cols, with=F],paste0(projectFolder,"/g_intermediate/migraine_algorithm_sensitivity/final_d3/MIG_S3_a_D3.rds"))
pregnancy_d3_mig[,include:=NULL]

setnames(pregnancy_d3_mig, "MIG_S3_b", "include")
saveRDS(pregnancy_d3_mig[,cols, with=F],paste0(projectFolder,"/g_intermediate/migraine_algorithm_sensitivity/final_d3/MIG_S3_b_D3.rds"))
pregnancy_d3_mig[,include:=NULL]

setnames(pregnancy_d3_mig, "MIG_S3_during", "include")
saveRDS(pregnancy_d3_mig[,cols, with=F],paste0(projectFolder,"/g_intermediate/migraine_algorithm_sensitivity/final_d3/MIG_S3_during_D3.rds"))
pregnancy_d3_mig[,include:=NULL]

setnames(pregnancy_d3_mig, "MIG_S4_during", "include")
saveRDS(pregnancy_d3_mig[,cols, with=F],paste0(projectFolder,"/g_intermediate/migraine_algorithm_sensitivity/final_d3/MIG_S4_during_D3.rds"))
pregnancy_d3_mig[,include:=NULL]



setnames(pregnancy_d3_mig, "MIG_S1_during2", "include")
saveRDS(pregnancy_d3_mig[,cols, with=F],paste0(projectFolder,"/g_intermediate/migraine_algorithm_sensitivity/final_d3/MIG_S1_during2_D3.rds"))
pregnancy_d3_mig[,include:=NULL]

setnames(pregnancy_d3_mig, "MIG_S2_during2", "include")
saveRDS(pregnancy_d3_mig[,cols, with=F],paste0(projectFolder,"/g_intermediate/migraine_algorithm_sensitivity/final_d3/MIG_S2_during2_D3.rds"))
pregnancy_d3_mig[,include:=NULL]

setnames(pregnancy_d3_mig, "MIG_S3_during2", "include")
saveRDS(pregnancy_d3_mig[,cols, with=F],paste0(projectFolder,"/g_intermediate/migraine_algorithm_sensitivity/final_d3/MIG_S3_during2_D3.rds"))
pregnancy_d3_mig[,include:=NULL]

setnames(pregnancy_d3_mig, "MIG_S4_during2", "include")
saveRDS(pregnancy_d3_mig[,cols, with=F],paste0(projectFolder,"/g_intermediate/migraine_algorithm_sensitivity/final_d3/MIG_S4_during2_D3.rds"))
pregnancy_d3_mig[,include:=NULL]

#### Save time info ####
date_running_end_04_f<-Sys.Date()
end_time_04_f<-Sys.time()

time_log_04_f<-data.table(DAP=data_access_provider_name,
                          Script="Step_04_f_migraine_algorithms_severity_S.R", 
                          Start_date=date_running_start_04_f, 
                          End_date=date_running_end_04_f,
                          Time_elaspsed=format(end_time_04_f-initial_time_04_f, digits=2))
fwrite(time_log_04_f,paste0(output_dir,"/Time log/Step_04_f_S_time_log.csv"),row.names = F)
rm(time_log_04_f)




