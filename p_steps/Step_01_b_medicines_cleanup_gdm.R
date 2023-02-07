#Clean up the medicines table to create the antidiabetic_medicines_D3

#Load the MEDICINES tables one by one and keep only antidiabetic medications as explained in antidiabetic_medications_codelist

#Load the codelist
med_codelist<-fread(paste0(pre_dir, list.files(pre_dir,"antidiabetic_medications_codelist")))

#Check for CDM table availability
source(paste0(pre_dir, "info.R"))

if(length(actual_tables$MEDICINES)>0){
  
  removed_med_rec<-list()
  removed_med_date<-list()
  med_summary<-list()
  index<-1
  
  for (med_tab_ind in 1:length(actual_tables$MEDICINES)){
    df<-fread(paste(path_dir, actual_tables$MEDICINES[med_tab_ind], sep=""), stringsAsFactors = FALSE, colClasses = "character")
    df<-df[,c("person_id", "medicinal_product_atc_code", "date_dispensing", "date_prescription", "meaning_of_drug_record")]
    df<-df[, lapply(.SD, FUN=function(x) gsub("^$|^ $", NA, x))] #make sure missing data is read appropriately
    #create truncated atc code that will be used for filtering
    df[,atc_code:=substr(medicinal_product_atc_code,1,4)]
    #merge with the medicines codelist to identify only antidiabetic drugs
    df<-merge.data.table(df, med_codelist, by="atc_code", all.x=T, allow.cartesian = T)
    #remove all records that are not antidiabetic
    no_antidiab_med<-df[is.na(medicinal_product_group),.N]
    removed_med_rec[[index]]<-data.table(file_name=actual_tables$MEDICINES[med_tab_ind], indicator="Other medication than antidiabetic", no_records=no_antidiab_med, total_records=df[,.N])
    rm(no_antidiab_med)
    df<-df[!is.na(medicinal_product_group)]
    #records with both date prescription and date dispensing empty
    empty_dates<-df[is.na(date_dispensing) & is.na(date_prescription),.N]
    removed_med_date[[index]]<-data.table(file_name=actual_tables$MEDICINES[med_tab_ind], indicator="Both date dispensing and date prescription are empty", no_records=empty_dates, total_records=df[,.N])
    rm(empty_dates)
    df<-df[!is.na(date_dispensing) | !is.na(date_prescription)]
    #If both dates are available date dispensing will be used
    #transform into date variables
    df[,date_dispensing:=as.Date(date_dispensing,"%Y%m%d")][,date_prescription:=as.Date(date_prescription,"%Y%m%d")] #transform to date variables
    df[,medicines_date:=date_prescription][!is.na(date_dispensing),medicines_date:=date_dispensing]#date that will be used for person-years
    #remove date_dispensing and date_prescription 
    df[,date_dispensing:=NULL][,date_prescription:=NULL]
    #create year variable
    df[,year:=year(medicines_date)]
    #create a short summary of the included records
    summary_med<-df[,.N, by=list(meaning_of_drug_record,medicinal_product_group, atc_code,year)]
    setnames(summary_med, "N", "no_records")
    med_summary[[index]]<-data.table(file_name=actual_tables$MEDICINES[med_tab_ind], summary_med)
    rm(summary_med)
    
    #export the medicines file to the g_intermediate/gdm_algorithm folder
    saveRDS(df, paste0(g_intermediate,"gdm_algorithm/", "antidiabetic_medications_", med_tab_ind))
    index<-index+1
  }
  
  #Combine all data and export to g_output
  removed_med_rec<-rbindlist(removed_med_rec)
  removed_med_date<-rbindlist(removed_med_date)
  removed_med_rec<-rbind(removed_med_rec,removed_med_date)
  #export
  fwrite(removed_med_rec, paste0(output_dir,"PE and GDM algorithm/", "removed_medicines_gdm.csv"), row.names = F)
  rm(removed_med_rec)
  
  med_summary<-rbindlist(med_summary)
  #export
  fwrite(med_summary, paste0(output_dir,"PE and GDM algorithm/", "medicines_description_gdm.csv"), row.names = F)
  rm(med_summary)
  
}
