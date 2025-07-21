initial_time_02_c<-Sys.time()
date_running_start_02_c<-Sys.Date()


#Combine all data about DU_medicines
print("Combine all files about DU_medicines")
du_med_files_fl<-list.files(tmp, "DU_medicines")
if(length(du_med_files_fl)>0){
  du_med<-lapply(paste0(tmp, du_med_files_fl), readRDS)
  du_med<-as.data.table(do.call(rbind,du_med))
  #remove uneccessary columns
  cols<-c("condition","medicinal_product_id")
  du_med<-du_med[,-cols,with=F]
  
  #match first all drugs that are an exact match
  setkey(du_med, atc_code)
  setkey(du_med_codelist, atc_code)
  du_med<-merge.data.table(du_med, du_med_codelist[mechanism=="exact"], by="atc_code", allow.cartesian = T, all.x=T)
  du_med_exact<-du_med[!is.na(label)]
  du_med<-du_med[is.na(label)]
  #remove uneccessary cols
  cols<-c("medicinal_product_group", "mechanism", "comment", "label")
  du_med<-du_med[,-cols, with=F]
  setnames(du_med, "atc_code","atc_code_original")
  if("nchar" %in% names(du_med)){du_med[,nchar:=NULL]}
  
  du_med_codelist_st<-du_med_codelist[mechanism=="start"]
  setkey(du_med_codelist_st, atc_code)
  du_med_codelist_st[,nchar:=nchar(atc_code)]
  for(med_ind in 1:length(sort(unique(du_med_codelist_st[,nchar]),decreasing = T))){
    if(du_med[,.N]>0){
    du_med[,atc_code:=substr(atc_code_original, 1,sort(unique(du_med_codelist_st[,nchar]),decreasing = T)[med_ind])]
    du_med<-merge.data.table(du_med, du_med_codelist_st, by="atc_code", all.x = T, allow.cartesian = T)
    if(du_med[!is.na(medicinal_product_group),.N]>0){
      du_med_df<-du_med[!is.na(medicinal_product_group)]
      du_med_df[,atc_code:=NULL][,nchar:=NULL]
      setnames(du_med_df, "atc_code_original", "atc_code")
      du_med_exact<-rbind(du_med_exact,du_med_df)
      #remove medicines already matched
      du_med<-du_med[is.na(medicinal_product_group)]
      rm(du_med_df)
    }
    #remove unecessary cols
    cols<-c("medicinal_product_group", "mechanism", "comment", "label","atc_code", "nchar")
    du_med<-du_med[,-cols, with=F]
    }
  }
  
  rm(du_med, du_med_codelist, du_med_codelist_st, du_exact, du_start)
  
  #Add the information from medicinal_group and label from the codelist, use allow.cartesian=T because the same ATC code can be part of different groups
  #Summary of included records by medicinal group, meaning, condition, year
  sum_du<-du_med_exact[,.N, by=c("label", "medicinal_product_group", "year")]
  setnames(sum_du,"N", "no_records")
  fwrite(sum_du,paste0(output_dir,"Drug utilisation/Step_02_summary_du_medicines.csv"))
  rm(sum_du)
  du_med_exact[,meaning:=NULL][,mechanism:=NULL][,comment:=NULL]
  
  #save data in g_intermediate/medicines_d3
  for(i in 1:length(du_med_exact[!duplicated(label),label])){
  saveRDS(du_med_exact[label== du_med_exact[!duplicated(label),label][i]], paste0(g_intermediate,"medicines_d3/", du_med_exact[!duplicated(label),label][i], ".rds"))
  }
  rm(du_med_exact)
  #copy all du files into raw_data in medicines_d3
  #remove all du files from tmp
  for (i in 1:length(du_med_files_fl)){
    file.copy(paste0(tmp,du_med_files_fl[[i]]), paste0(g_intermediate,"medicines_d3/raw_data/",du_med_files_fl[[i]]))
  }
  #remove files
  #remove all du files from tmp
  for (i in 1:length(du_med_files_fl)){
    file.remove(paste0(tmp,du_med_files_fl[[i]]))
  }
  
}
rm(du_med_files_fl)

date_running_end_02_c<-Sys.Date()
end_time_02_c<-Sys.time()

time_log_02_c<-data.table(DAP=data_access_provider_name,
                     Script="Step_02_medicines_clean_up_combine.R", 
                     Start_date=date_running_start_02_c, 
                     End_date=date_running_end_02_c,
                     Time_elaspsed=format(end_time_02_c-initial_time_02_c, digits=2))
fwrite(time_log_02_c,paste0(output_dir,"/Time log/DU/Step_02_combine_time_log.csv"),row.names = F)
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
                  "mo_gdm_diagnoses","mo_migraine_diagnoses","mo_pe_diagnoses","so_gdm_diagnoses","so_migraine_diagnoses","so_pe_diagnoses","same_names","complete_med","exact_med","gdm_med_codelist","migraine_med_codelist",
                  "start_med", "du_med_exact", "exact_med", "min_obs", "start_med")

list_rm<-ls()[ls() %in% rm_environment]
rm(list = list_rm)
rm(list_rm,rm_environment)
