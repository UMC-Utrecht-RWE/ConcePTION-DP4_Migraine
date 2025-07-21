initial_time_04_d<-Sys.time()
date_running_start_04_d<-Sys.Date()

####APPLY Migraine ALGORITHM####
algorithm_template<-fread(paste0(projectFolder, "/p_steps/parameters/algorithms.csv"))
#### MIG_Dx_a:Prevalence of migraine at baseline when lookback==12 months(3 months) ####
print("Create algorithm MIG_Dx_a")
MIG_Dx_a<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_Dx_a"]
inc_col<-MIG_Dx_a[TYPE=="AND",STUDY_VARIABLES]

if(length(inc_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col],include:=1]}else{pregnancy_d3_mig[,include:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm_sensitivity/final_d3/MIG_Dx_a_D3.rds"))
pregnancy_d3_mig[,include:=NULL]

#### MIG_Dx_b:Prevalence of migraine at baseline when lookback==3 months ####
print("Create algorithm MIG_Dx_b")
MIG_Dx_b<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_Dx_b"]
inc_col<-MIG_Dx_b[TYPE=="AND",STUDY_VARIABLES]

if(length(inc_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col],include:=1]}else{pregnancy_d3_mig[,include:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm_sensitivity/final_d3/MIG_Dx_b_D3.rds"))
pregnancy_d3_mig[,include:=NULL]

#### MIG_Rx_a:Prevalence of migraine at baseline when lookback==12 months(3 months) ####
print("Create algorithm MIG_Rx_a")
MIG_Rx_a<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_Rx_a"]
inc_col<-MIG_Rx_a[TYPE=="AND",STUDY_VARIABLES]

if(length(inc_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col],include:=1]}else{pregnancy_d3_mig[,include:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm_sensitivity/final_d3/MIG_Rx_a_D3.rds"))
pregnancy_d3_mig[,include:=NULL]

#### MIG_Rx_b:Prevalence of migraine at baseline when lookback==12 months(3 months) ####
print("Create algorithm MIG_Rx_b")
MIG_Rx_a<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_Rx_b"]
inc_col<-MIG_Rx_a[TYPE=="AND",STUDY_VARIABLES]

if(length(inc_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col],include:=1]}else{pregnancy_d3_mig[,include:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm_sensitivity/final_d3/MIG_Rx_b_D3.rds"))
pregnancy_d3_mig[,include:=NULL]

#### MIG_DxRx_a:Prevalence of migraine at baseline when lookback==12 months(3 months) ####
print("Create algorithm MIG_DxRx_a")
MIG_DxRx_a<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_DxRx_a"]
alt_col<-MIG_DxRx_a[TYPE=="OR",STUDY_VARIABLES]

if(length(alt_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("|" , lapply(.SD,`>=`, 1)),.SDcols=alt_col],alternative:=1]}else{pregnancy_d3_mig[,alternative:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm_sensitivity/final_d3/MIG_DxRx_a_D3.rds"))
pregnancy_d3_mig[,alternative:=NULL]

#### MIG_DxRx_b:Prevalence of migraine at baseline when lookback==3 months ####
print("Create algorithm MIG_DxRx_b")
MIG_DxRx_b<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_DxRx_b"]
alt_col<-MIG_DxRx_b[TYPE=="OR",STUDY_VARIABLES]

if(length(alt_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("|" , lapply(.SD,`>=`, 1)),.SDcols=alt_col],alternative:=1]}else{pregnancy_d3_mig[,alternative:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm_sensitivity/final_d3/MIG_DxRx_b_D3.rds"))
pregnancy_d3_mig[,alternative:=NULL]

#### MIG_Dx_during:Prevalence of migraine during pregnancy when lookback==12 months(3 months) ####
print("Create algorithm MIG_Dx_during")
MIG_Dx_during<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_Dx_during"]
inc_col<-MIG_Dx_during[TYPE=="AND",STUDY_VARIABLES]

if(length(inc_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col],include:=1]}else{pregnancy_d3_mig[,include:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm_sensitivity/final_d3/MIG_Dx_during_D3.rds"))
pregnancy_d3_mig[,include:=NULL]

#### MIG_Rx_during:Prevalence of migraine during pregnancy when lookback==12 months(3 months) ####
print("Create algorithm MIG_Rx_during")
MIG_Rx_during<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_Rx_during"]
inc_col<-MIG_Rx_during[TYPE=="AND",STUDY_VARIABLES]

if(length(inc_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col],include:=1]}else{pregnancy_d3_mig[,include:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm_sensitivity/final_d3/MIG_Rx_during_D3.rds"))
pregnancy_d3_mig[,include:=NULL]

#### MIG_checkbox_during:Prevalence of migraine during pregnancy when lookback==12 months(3 months) ####
if(!is.null(preg_d3_checkbox)){
print("Create algorithm MIG_checkbox_during")
MIG_checkbox_during<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_checkbox_during"]
inc_col<-MIG_checkbox_during[TYPE=="AND",STUDY_VARIABLES]
if(!"Migraine_checkbox_during" %in% names(preg_d3_checkbox)){preg_d3_checkbox[,Migraine_checkbox_during:=0]}

if(length(inc_col)>0){preg_d3_checkbox[preg_d3_checkbox[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col],include:=1]}else{preg_d3_checkbox[,include:=NA]}

saveRDS(preg_d3_checkbox,paste0(projectFolder,"/g_intermediate/migraine_algorithm_sensitivity/final_d3/MIG_checkbox_during_D3.rds"))
pregnancy_d3_mig[,include:=NULL]
}

#### MIG_Dx_first:Prevalence of migraine during pregnancy when lookback==12 months(3 months) ####
print("Create algorithm MIG_Dx_first")
MIG_Dx_first<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_Dx_first"]
inc_col<-MIG_Dx_first[TYPE=="AND",STUDY_VARIABLES]

#set the pregnancy trimester for each pregnancy
pregnancy_d3_mig[,GA:=pregnancy_end_date-pregnancy_start_date]
if(length(inc_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col] & GA>=97,include:=1]}else{pregnancy_d3_mig[,include:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm_sensitivity/final_d3/MIG_Dx_first_D3.rds"))
pregnancy_d3_mig[,include:=NULL]

#### MIG_Rx_first:Prevalence of migraine during pregnancy when lookback==12 months(3 months) ####
print("Create algorithm MIG_Rx_first")
MIG_Rx_first<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_Rx_first"]
inc_col<-MIG_Rx_first[TYPE=="AND",STUDY_VARIABLES]

if(length(inc_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col] & GA>=97,include:=1]}else{pregnancy_d3_mig[,include:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm_sensitivity/final_d3/MIG_Rx_first_D3.rds"))
pregnancy_d3_mig[,include:=NULL]

#### MIG_DxRx_first:Prevalence of migraine at baseline when lookback==12 months(3 months) ####
print("Create algorithm MIG_DxRx_first")
MIG_DxRx_first<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_DxRx_first"]
alt_col<-MIG_DxRx_first[TYPE=="OR",STUDY_VARIABLES]

if(length(alt_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("|" , lapply(.SD,`>=`, 1)),.SDcols=alt_col] & GA>=97,alternative:=1]}else{pregnancy_d3_mig[,alternative:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm_sensitivity/final_d3/MIG_DxRx_first_D3.rds"))
pregnancy_d3_mig[,alternative:=NULL]

#### MIG_DxRx_first:Prevalence of migraine at baseline when lookback==12 months(3 months) ####
print("Create algorithm MIG_DxRx_second")
MIG_DxRx_first<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_DxRx_second"]
alt_col<-MIG_DxRx_first[TYPE=="OR",STUDY_VARIABLES]

if(length(alt_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("|" , lapply(.SD,`>=`, 1)),.SDcols=alt_col] & GA>=97,alternative:=1]}else{pregnancy_d3_mig[,alternative:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm_sensitivity/final_d3/MIG_DxRx_second_D3.rds"))
pregnancy_d3_mig[,alternative:=NULL]


#### MIG_Dx_second:Prevalence of migraine during pregnancy when lookback==12 months(3 months) ####
print("Create algorithm MIG_Dx_second")
MIG_Dx_second<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_Dx_second"]
inc_col<-MIG_Dx_second[TYPE=="AND",STUDY_VARIABLES]

#set the pregnancy trimester for each pregnancy
if(length(inc_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col] & GA>=195,include:=1]}else{pregnancy_d3_mig[,include:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm_sensitivity/final_d3/MIG_Dx_second_D3.rds"))
pregnancy_d3_mig[,include:=NULL]

#### MIG_Rx_second:Prevalence of migraine during pregnancy when lookback==12 months(3 months) ####
print("Create algorithm MIG_Rx_second")
MIG_Rx_second<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_Rx_second"]
inc_col<-MIG_Rx_second[TYPE=="AND",STUDY_VARIABLES]

if(length(inc_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col] & GA>=195,include:=1]}else{pregnancy_d3_mig[,include:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm_sensitivity/final_d3/MIG_Rx_second_D3.rds"))
pregnancy_d3_mig[,include:=NULL]

#### MIG_Dx_third:Prevalence of migraine during pregnancy when lookback==12 months(3 months) ####
print("Create algorithm MIG_Dx_third")
MIG_Dx_third<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_Dx_third"]
inc_col<-MIG_Dx_third[TYPE=="AND",STUDY_VARIABLES]

#set the pregnancy trimester for each pregnancy
if(length(inc_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col] & GA>=196,include:=1]}else{pregnancy_d3_mig[,include:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm_sensitivity/final_d3/MIG_Dx_third_D3.rds"))
pregnancy_d3_mig[,include:=NULL]


#### MIG_Rx_third:Prevalence of migraine during pregnancy when lookback==12 months(3 months) ####
print("Create algorithm MIG_Rx_third")
MIG_Rx_third<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_Rx_third"]
inc_col<-MIG_Rx_third[TYPE=="AND",STUDY_VARIABLES]

if(length(inc_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col] & GA>=196,include:=1]}else{pregnancy_d3_mig[,include:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm_sensitivity/final_d3/MIG_Rx_third_D3.rds"))
pregnancy_d3_mig[,include:=NULL]

#### MIG_DxRx_third:Prevalence of migraine at baseline when lookback==12 months(3 months) ####
print("Create algorithm MIG_DxRx_third")
MIG_DxRx_third<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_DxRx_third"]
alt_col<-MIG_DxRx_third[TYPE=="OR",STUDY_VARIABLES]

if(length(alt_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("|" , lapply(.SD,`>=`, 1)),.SDcols=alt_col] & GA>=196,alternative:=1]}else{pregnancy_d3_mig[,alternative:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm_sensitivity/final_d3/MIG_DxRx_third_D3.rds"))
pregnancy_d3_mig[,alternative:=NULL]

#### MIG_DxRx_during:Prevalence of migraine at baseline when lookback==12 months(3 months) ####
print("Create algorithm MIG_DxRx_during")
MIG_DxRx_during<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_DxRx_during"]
alt_col<-MIG_DxRx_during[TYPE=="OR",STUDY_VARIABLES]

if(length(alt_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("|" , lapply(.SD,`>=`, 1)),.SDcols=alt_col],alternative:=1]}else{pregnancy_d3_mig[,alternative:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm_sensitivity/final_d3/MIG_DxRx_during_D3.rds"))
pregnancy_d3_mig[,alternative:=NULL]

#### Save time info ####
date_running_end_04_d<-Sys.Date()
end_time_04_d<-Sys.time()

time_log_04_d<-data.table(DAP=data_access_provider_name,
                          Script="Step_04_d_migraine_algorithms_S.R", 
                          Start_date=date_running_start_04_d, 
                          End_date=date_running_end_04_d,
                          Time_elaspsed=format(end_time_04_d-initial_time_04_d, digits=2))
fwrite(time_log_04_d,paste0(output_dir,"/Time log/Step_04_d_S_time_log.csv"),row.names = F)
rm(time_log_04_d)
