initial_time<-Sys.time()
date_running_start<-Sys.Date()


setnames(start_med,"truncated_atc_code","atc_code")
complete_med<-rbind(exact_med[,c("medicinal_product_group","atc_code")],start_med[,c("medicinal_product_group","atc_code")])

#Combine all data about GDM_medicines
print("Combine all files about GDM_medicines")
gdm_med_files_fl<-list.files(tmp, "GDM_medicines")
if(length(gdm_med_files_fl)>0){
  gdm_med<-lapply(paste0(tmp, gdm_med_files_fl), readRDS)
  gdm_med<-as.data.table(do.call(rbind,gdm_med))

  #Summary of included records by medicinal group, meaning, condition, year
  sum_gdm<-gdm_med[,.N, by=c("condition", "medicinal_product_group", "meaning", "year")]
  sum_gdm<-merge.data.table(sum_gdm,complete_med,by="medicinal_product_group",all.x=T)
  setnames(sum_gdm,"N", "no_records")
  setcolorder(sum_gdm,c("condition","meaning","medicinal_product_group","atc_code","year"))
  fwrite(sum_gdm,paste0(output_dir,"PE and GDM algorithm/Step_03_summary_gdm_medicines.csv"))
  rm(sum_gdm)
  gdm_med[,meaning:=NULL][,year:=NULL]
  #save data in g_intermediate/populations
  saveRDS(gdm_med, paste0(g_intermediate,"gdm_algorithm/GDM_medicines.rds"))
  rm(gdm_med)
  #remove files
  #remove all gdm files from tmp
  for (i in 1:length(gdm_med_files_fl)){
    file.remove(paste0(tmp,gdm_med_files_fl[[i]]))
  }
  
}
rm(gdm_med_files_fl)

#Combine all data about Migraine_medicines
print("Combine all files about Migraine_medicines")
mig_med_files_fl<-list.files(tmp, "Migraine_medicines")
if(length(mig_med_files_fl)>0){
  mig_med<-lapply(paste0(tmp, mig_med_files_fl), readRDS)
  mig_med<-as.data.table(do.call(rbind,mig_med))
  
  #Summary of included records by medicinal group, meaning, condition, year
  sum_mig<-mig_med[,.N, by=c("condition", "medicinal_product_group", "meaning", "year")]
  sum_mig<-merge.data.table(sum_mig,complete_med,by="medicinal_product_group",all.x=T)
  setnames(sum_mig,"N", "no_records")
  setcolorder(sum_mig,c("condition","meaning","medicinal_product_group","atc_code","year"))
  fwrite(sum_mig,paste0(output_dir,"Migraine algorithm/Step_03_summary_migraine_medicines.csv"))
  rm(sum_mig)
  mig_med[,meaning:=NULL][,year:=NULL]
  #save data in g_intermediate/populations
  saveRDS(mig_med, paste0(g_intermediate,"migraine_algorithm/Migraine_medicines.rds"))
  rm(mig_med)
  #remove files
  #remove all gdm files from tmp
  for (i in 1:length(mig_med_files_fl)){
    file.remove(paste0(tmp,mig_med_files_fl[[i]]))
  }
  
}
rm(mig_med_files_fl)

date_running_end<-Sys.Date()
end_time<-Sys.time()

time_log<-data.table(DAP=data_access_provider_name,
                     Script="Step_03_medicines_clean_up_combine.R", 
                     Start_date=date_running_start, 
                     End_date=date_running_end,
                     Time_elaspsed=format(end_time-initial_time, digits=2))
fwrite(time_log,paste0(output_dir,"/Time log/Step_03_combine_time_log.csv"),row.names = F)
rm(time_log)

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
                  "mo_gdm_diagnoses","mo_migraine_diagnoses","mo_pe_diagnoses","so_gdm_diagnoses","so_migraine_diagnoses","so_pe_diagnoses","same_names","complete_med","exact_med","gdm_med_codelist","migraine_med_codelist",
                  "start_med")

list_rm<-ls()[ls() %in% rm_environment]
rm(list = list_rm)
rm(list_rm,rm_environment)
