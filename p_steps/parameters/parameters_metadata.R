####Load parameters####
tables_file<-list.files(paste0(projectFolder,"/p_parameters/"), "table_search")
tables_search<-read_excel(paste0(projectFolder,"/p_parameters/",tables_file),col_types = "text")
tables_search<-as.data.table(tables_search)

source(paste0(pre_dir,"info/DAP_info.R"))

#####Load additional concepts template####
additional_concepts_file<-list.files(paste0(projectFolder,"/p_parameters/"), "additional_concepts")
additional_concepts<-read_excel(paste0(projectFolder,"/p_parameters/",additional_concepts_file),col_types = "text")
additional_concepts<-as.data.table(additional_concepts)
#Keep only needed information based on the DAP
additional_concepts<-additional_concepts[DAP_NAME==data_access_provider_name]
if(additional_concepts[,.N]==0){
  stop("This is not a script issue. There is no data for your data source in additional_concepts. Fix the issue and then rerun the script.")
}
source(paste0(pre_dir,"parameters/template_error_check.R"))
#Split the information into medicines or diagnoses codesheet(where the codelists will be used) and not fixed

####GDM####
codesheet_medicines_gdm<-additional_concepts[StudyVar=="GDM_medicines" & type=="codesheet"]
codesheet_diagnoses_gdm<-additional_concepts[StudyVar=="GDM_diagnoses" & type=="codesheet"]
codesheet_diagnoses_gdm_cat<-additional_concepts[StudyVar=="GDM_diagnoses_cat" & type=="not_fixed"]
codesheet_procedures_gdm<-additional_concepts[StudyVar=="GDM_procedures" & type=="codesheet"]
not_fixed_gdm_ogtt_all<-additional_concepts[StudyVar=="GDM_OGTT_ALL" & type=="not_fixed"]
not_fixed_gdm_ogtt_yes<-additional_concepts[StudyVar=="GDM_OGTT_YES" & type=="not_fixed"]
not_fixed_gdm_checkbox<-additional_concepts[StudyVar=="GDM_checkbox" & type=="not_fixed"]

####PE####
codesheet_diagnoses_pe<-additional_concepts[StudyVar=="PE_diagnoses" & type=="codesheet"]
codesheet_diagnoses_pe_cat<-additional_concepts[StudyVar=="PE_diagnoses_cat" & type=="not_fixed"]
#codesheet_medicines_pe<-additional_concepts[StudyVar=="PE_medicines" & type=="codesheet"]
#codesheet_procedures_pe<-additional_concepts[StudyVar=="PE_procedures" & type=="codesheet"]
not_fixed_pe_checkbox<-additional_concepts[StudyVar=="PE_checkbox" & type=="not_fixed"]

####Migraine####
codesheet_medicines_migraine<-additional_concepts[StudyVar=="Migraine_medicines" & type=="codesheet"]
codesheet_diagnoses_migraine_cat<-additional_concepts[StudyVar=="Migraine_diagnoses_cat" & type=="not_fixed"]
codesheet_diagnoses_migraine<-additional_concepts[StudyVar=="Migraine_diagnoses" & type=="codesheet"]
codesheet_procedures_migraine<-additional_concepts[StudyVar=="Migraine_procedures" & type=="codesheet"]

####Diagnoses category####
gdm_diag_cat<-codesheet_diagnoses_gdm_cat[,c("table", "event_abbreviation","val_1","val_2","val_3","val_4","val_5","col_1","col_2","col_3","col_4","col_5","date_column","keep")]
gdm_diag_cat[,index:=1:gdm_diag_cat[,.N]]
if(gdm_diag_cat[,.N]>0){
diag_cat_gdm<-list()
ind<-1
for(diag_ind in 1:gdm_diag_cat[,.N]){
  if(gdm_diag_cat[diag_ind,][!is.na(val_1) & !is.na(val_2),.N]>0){
  col_1_name<-gdm_diag_cat[diag_ind,col_1]
  val_1<-gdm_diag_cat[diag_ind,val_1]
  col_names<-gdm_diag_cat[diag_ind,col_2]
  values<-c(gdm_diag_cat[diag_ind,val_2], gdm_diag_cat[diag_ind,val_3], gdm_diag_cat[diag_ind,val_4], gdm_diag_cat[diag_ind,val_5])
  values<-values[!is.na(values)]
  values_df<-data.table(col_names=values)
  setnames(values_df,"col_names",col_names)
  data<-data.table(table=gdm_diag_cat[diag_ind,table],event_abbreviation=gdm_diag_cat[diag_ind,event_abbreviation],col_1=val_1, values_df,keep=gdm_diag_cat[diag_ind,keep],index=gdm_diag_cat[diag_ind,index],date_column=gdm_diag_cat[diag_ind,date_column])
  setnames(data,"col_1",col_1_name)
  } else{
    col_1_name<-gdm_diag_cat[diag_ind,col_1]
    val_1<-gdm_diag_cat[diag_ind,val_1]
    data<-data.table(table=gdm_diag_cat[diag_ind,table],event_abbreviation=gdm_diag_cat[diag_ind,event_abbreviation],col_1=val_1,index=gdm_diag_cat[diag_ind,index],date_column=gdm_diag_cat[diag_ind,date_column])
    setnames(data,"col_1",col_1_name)
}
  diag_cat_gdm[[ind]]<-data
  rm(data)
  
  ind<-ind+1
}
diag_cat_gdm<-rbindlist(diag_cat_gdm,fill=T)
}else{diag_cat_gdm<-NULL}

pe_diag_cat<-codesheet_diagnoses_pe_cat[,c("table", "event_abbreviation","val_1","val_2","val_3","val_4","val_5","col_1","col_2","col_3","col_4","col_5","date_column","keep")]
pe_diag_cat[,index:=1:pe_diag_cat[,.N]]
if(pe_diag_cat[,.N]>0){
diag_cat_pe<-list()
ind<-1
for(diag_ind in 1:pe_diag_cat[,.N]){
  if(pe_diag_cat[diag_ind,][!is.na(val_1) & !is.na(val_2),.N]>0){
    col_1_name<-pe_diag_cat[diag_ind,col_1]
    val_1<-pe_diag_cat[diag_ind,val_1]
    col_names<-pe_diag_cat[diag_ind,col_2]
    values<-c(pe_diag_cat[diag_ind,val_2], pe_diag_cat[diag_ind,val_3], pe_diag_cat[diag_ind,val_4], pe_diag_cat[diag_ind,val_5])
    values<-values[!is.na(values)]
    values_df<-data.table(col_names=values)
    setnames(values_df,"col_names",col_names)
    data<-data.table(table=pe_diag_cat[diag_ind,table],event_abbreviation=pe_diag_cat[diag_ind,event_abbreviation],col_1=val_1, values_df,keep=pe_diag_cat[diag_ind,keep],index=pe_diag_cat[diag_ind,index],date_column=pe_diag_cat[diag_ind,date_column])
    setnames(data,"col_1",col_1_name)
  } else{
    col_1_name<-pe_diag_cat[diag_ind,col_1]
    val_1<-pe_diag_cat[diag_ind,val_1]
    data<-data.table(table=pe_diag_cat[diag_ind,table],event_abbreviation=pe_diag_cat[diag_ind,event_abbreviation],col_1=val_1,keep=pe_diag_cat[diag_ind,keep],index=pe_diag_cat[diag_ind,index],date_column=pe_diag_cat[diag_ind,date_column])
    setnames(data,"col_1",col_1_name)
  }
  diag_cat_pe[[ind]]<-data
  rm(data)
  
  ind<-ind+1
}
diag_cat_pe<-rbindlist(diag_cat_pe,fill=T)
}else{diag_cat_pe<-NULL}

migraine_diag_cat<-codesheet_diagnoses_migraine_cat[,c("table", "event_abbreviation","val_1","val_2","val_3","val_4","val_5","col_1","col_2","col_3","col_4","col_5","date_column","keep")]
migraine_diag_cat[,index:=1:migraine_diag_cat[,.N]]
if(migraine_diag_cat[,.N]>0){
  diag_cat_migraine<-list()
  ind<-1
  for(diag_ind in 1:migraine_diag_cat[,.N]){
    if(migraine_diag_cat[diag_ind,][!is.na(val_1) & !is.na(val_2),.N]>0){
      col_1_name<-migraine_diag_cat[diag_ind,col_1]
      val_1<-migraine_diag_cat[diag_ind,val_1]
      col_names<-migraine_diag_cat[diag_ind,col_2]
      values<-c(migraine_diag_cat[diag_ind,val_2], migraine_diag_cat[diag_ind,val_3], migraine_diag_cat[diag_ind,val_4], migraine_diag_cat[diag_ind,val_5])
      values<-values[!is.na(values)]
      values_df<-data.table(col_names=values)
      setnames(values_df,"col_names",col_names)
      data<-data.table(table=migraine_diag_cat[diag_ind,table],event_abbreviation=migraine_diag_cat[diag_ind,event_abbreviation],col_1=val_1, values_df,keep=migraine_diag_cat[diag_ind,keep],index=migraine_diag_cat[diag_ind,index],date_column=migraine_diag_cat[diag_ind,date_column])
      setnames(data,"col_1",col_1_name)
    } else{
      col_1_name<-migraine_diag_cat[diag_ind,col_1]
      val_1<-migraine_diag_cat[diag_ind,val_1]
      data<-data.table(table=migraine_diag_cat[diag_ind,table],event_abbreviation=migraine_diag_cat[diag_ind,event_abbreviation],col_1=val_1,keep=migraine_diag_cat[diag_ind,keep],index=migraine_diag_cat[diag_ind,index],date_column=migraine_diag_cat[diag_ind,date_column])
      setnames(data,"col_1",col_1_name)
    }
    diag_cat_migraine[[ind]]<-data
    rm(data)
    
    ind<-ind+1
  }
  diag_cat_migraine<-rbindlist(diag_cat_migraine,fill=T)
}else{diag_cat_migraine<-NULL}

#Gather information about checkbox
#GDM
if(not_fixed_gdm_checkbox[,.N]>0){
  not_fixed_gdm_checkbox[,index:=1:not_fixed_gdm_checkbox[,.N]]
  diag_checkbox_gdm<-list()
  ind<-1
  for(diag_ind in 1:not_fixed_gdm_checkbox[,.N]){
    if(not_fixed_gdm_checkbox[diag_ind,][!is.na(val_1) & !is.na(val_2),.N]>0){
      col_1_name<-not_fixed_gdm_checkbox[diag_ind,col_1]
      val_1<-not_fixed_gdm_checkbox[diag_ind,val_1]
      col_names<-not_fixed_gdm_checkbox[diag_ind,col_2]
      values<-c(not_fixed_gdm_checkbox[diag_ind,val_2], not_fixed_gdm_checkbox[diag_ind,val_3], not_fixed_gdm_checkbox[diag_ind,val_4], not_fixed_gdm_checkbox[diag_ind,val_5])
      values<-values[!is.na(values)]
      values_df<-data.table(col_names=values)
      setnames(values_df,"col_names",col_names)
      checkbox_date_col<-not_fixed_gdm_checkbox[diag_ind,date_column]
      data<-data.table(table=not_fixed_gdm_checkbox[diag_ind,table],event_abbreviation=not_fixed_gdm_checkbox[diag_ind,event_abbreviation],col_1=val_1, values_df,keep=not_fixed_gdm_checkbox[diag_ind,keep],index=not_fixed_gdm_checkbox[diag_ind,index], checkbox_date=not_fixed_gdm_checkbox[diag_ind,date_column])
      setnames(data,"col_1",col_1_name)
    } else{
      col_1_name<-not_fixed_gdm_checkbox[diag_ind,col_1]
      val_1<-not_fixed_gdm_checkbox[diag_ind,val_1]
      data<-data.table(table=not_fixed_gdm_checkbox[diag_ind,table],event_abbreviation=not_fixed_gdm_checkbox[diag_ind,event_abbreviation],col_1=val_1,keep=not_fixed_gdm_checkbox[diag_ind,keep],index=not_fixed_gdm_checkbox[diag_ind,index], checkbox_date=not_fixed_gdm_checkbox[diag_ind,date_column])
      setnames(data,"col_1",col_1_name)
    }
    diag_checkbox_gdm[[ind]]<-data
    rm(data)
    
    ind<-ind+1
  }
  diag_checkbox_gdm<-rbindlist(diag_checkbox_gdm,fill=T)
  diag_checkbox_gdm_mo<-diag_checkbox_gdm[table=="MEDICAL_OBSERVATIONS"]
  diag_checkbox_gdm_so<-diag_checkbox_gdm[table=="SURVEY_OBSERVATIONS"]
  
}else{
  diag_checkbox_gdm_mo<-NULL
  diag_checkbox_gdm_so<-NULL}

#PE
if(not_fixed_pe_checkbox[,.N]>0){
  not_fixed_pe_checkbox[,index:=1:not_fixed_pe_checkbox[,.N]]
  diag_checkbox_pe<-list()
  ind<-1
  for(diag_ind in 1:not_fixed_pe_checkbox[,.N]){
    if(not_fixed_pe_checkbox[diag_ind,][!is.na(val_1) & !is.na(val_2),.N]>0){
      col_1_name<-not_fixed_pe_checkbox[diag_ind,col_1]
      val_1<-not_fixed_pe_checkbox[diag_ind,val_1]
      col_names<-not_fixed_pe_checkbox[diag_ind,col_2]
      values<-c(not_fixed_pe_checkbox[diag_ind,val_2], not_fixed_pe_checkbox[diag_ind,val_3], not_fixed_pe_checkbox[diag_ind,val_4], not_fixed_pe_checkbox[diag_ind,val_5])
      values<-values[!is.na(values)]
      values_df<-data.table(col_names=values)
      setnames(values_df,"col_names",col_names)
      data<-data.table(table=not_fixed_pe_checkbox[diag_ind,table],event_abbreviation=not_fixed_pe_checkbox[diag_ind,event_abbreviation],col_1=val_1, values_df,keep=not_fixed_pe_checkbox[diag_ind,keep],index=not_fixed_pe_checkbox[diag_ind,index],checkbox_date=not_fixed_pe_checkbox[diag_ind,date_column])
      setnames(data,"col_1",col_1_name)
    } else{
      col_1_name<-not_fixed_pe_checkbox[diag_ind,col_1]
      val_1<-not_fixed_pe_checkbox[diag_ind,val_1]
      data<-data.table(table=not_fixed_pe_checkbox[diag_ind,table],event_abbreviation=not_fixed_pe_checkbox[diag_ind,event_abbreviation],col_1=val_1,keep=not_fixed_pe_checkbox[diag_ind,keep],index=not_fixed_pe_checkbox[diag_ind,index],checkbox_date=not_fixed_pe_checkbox[diag_ind,date_column])
      setnames(data,"col_1",col_1_name)
    }
    diag_checkbox_pe[[ind]]<-data
    rm(data)
    
    ind<-ind+1
  }
  diag_checkbox_pe<-rbindlist(diag_checkbox_pe,fill=T)
  diag_checkbox_pe_mo<-diag_checkbox_pe[table=="MEDICAL_OBSERVATIONS"]
  diag_checkbox_pe_so<-diag_checkbox_pe[table=="SURVEY_OBSERVATIONS"]
  
  
}else{
  diag_checkbox_pe_mo<-NULL
  diag_checkbox_pe_so<-NULL}
