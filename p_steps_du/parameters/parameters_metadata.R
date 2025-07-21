

#####Load additional concepts template####
additional_concepts_file<-list.files(paste0(projectFolder,"/p_parameters/"), "^additional_concepts")
if (tools::file_ext(additional_concepts_file) %in% c("xlsx", "xls")){
additional_concepts<-read_excel(paste0(projectFolder,"/p_parameters/",additional_concepts_file),col_types = "text")
}
if (tools::file_ext(additional_concepts_file) == "csv"){
  additional_concepts<-fread(paste0(projectFolder,"/p_parameters/",additional_concepts_file),colClasses = "character")
}
additional_concepts<-as.data.table(additional_concepts)
additional_concepts[, (names(additional_concepts)) := lapply(.SD, function(x) ifelse(x == " ", NA, x))]
additional_concepts[, (names(additional_concepts)) := lapply(.SD, function(x) ifelse(x == "", NA, x))]

#Keep only needed information based on the DAP
additional_concepts<-additional_concepts[DAP_NAME==data_access_provider_name]
if(additional_concepts[,.N]==0){
  stop("This is not a script issue. There is no data for your data source in additional_concepts. Fix the issue and then rerun the script.")
}
source(paste0(pre_dir,"parameters/template_error_check.R"))
#Split the information into medicines or diagnoses codesheet(where the codelists will be used) and not fixed


#Tables to be checked
tables_diagnoses<-unique(additional_concepts[type=="codesheet", table], na.rm=T) 
tables_diagnoses<-tables_diagnoses[!tables_diagnoses %in% "MEDICINES"]
tables_medicines<-unique(additional_concepts[type=="codesheet", table], na.rm=T)
tables_medicines<-tables_medicines[tables_medicines %in% "MEDICINES"]
tables_checkbox<-unique(additional_concepts[type=="not_fixed", table], na.rm=T)
tables_columns<-unique(additional_concepts[type=="fixed", table], na.rm=T) #when no actual value can be selected, but the whole column

#### Chronic hypertension ####
codesheet_diagnoses_hyp<-additional_concepts[StudyVar=="blht_event" & type=="codesheet"]
not_fixed_hyp_checkbox<-additional_concepts[StudyVar=="blht_checkbox" & type=="not_fixed"]
fixed_hyp_checkbox<-additional_concepts[StudyVar=="blht_checkbox" & type=="fixed"]

#### Diabetes #####
codesheet_diagnoses_dm<-additional_concepts[StudyVar=="bldm_event" & type=="codesheet"]
not_fixed_dm_checkbox<-additional_concepts[StudyVar=="bldm_checkbox" & type=="not_fixed"]
fixed_dm_checkbox<-additional_concepts[StudyVar=="bldm_checkbox" & type=="fixed"]

#### Depression ####
codesheet_diagnoses_dep<-additional_concepts[StudyVar=="bldep_event" & type=="codesheet"]
not_fixed_dep_checkbox<-additional_concepts[StudyVar=="bldep_checkbox" & type=="not_fixed"]
fixed_dep_checkbox<-additional_concepts[StudyVar=="bldep_checkbox" & type=="fixed"]

#### Parity ####
codesheet_diagnoses_parity<-additional_concepts[StudyVar=="PARITY_event" & type=="codesheet"]
not_fixed_parity_checkbox<-additional_concepts[StudyVar=="PARITY_checkbox" & type=="not_fixed"]
fixed_parity_checkbox<-additional_concepts[StudyVar=="PARITY_checkbox" & type=="fixed"]

#### Obesity ####
codesheet_diagnoses_bmi<-additional_concepts[StudyVar=="obesity_event" & type=="codesheet"]
not_fixed_bmi_checkbox<-additional_concepts[StudyVar=="obesity_checkbox" & type=="not_fixed"]
fixed_bmi_checkbox<-additional_concepts[StudyVar=="obesity_checkbox" & type=="fixed"]
#PBMI_checkbox not necessary

#### Height ####
codesheet_diagnoses_h<-additional_concepts[StudyVar=="PBMI_h" & type=="codesheet"]
not_fixed_h_checkbox<-additional_concepts[StudyVar=="PBMI_h" & type=="not_fixed"]
fixed_h_checkbox<-additional_concepts[StudyVar=="PBMI_h" & type=="fixed"]

#### Weight ####
codesheet_diagnoses_w<-additional_concepts[StudyVar=="PBMI_w" & type=="codesheet"]
not_fixed_w_checkbox<-additional_concepts[StudyVar=="PBMI_w" & type=="not_fixed"]
fixed_w_checkbox<-additional_concepts[StudyVar=="PBMI_w" & type=="fixed"]

####Gather information about checkbox####
#Chronic hypertension
if(not_fixed_hyp_checkbox[,.N]>0){
  not_fixed_hyp_checkbox[,index:=1:not_fixed_hyp_checkbox[,.N]]
  diag_checkbox_hyp<-list()
  ind<-1
  for(diag_ind in 1:not_fixed_hyp_checkbox[,.N]){
    if(not_fixed_hyp_checkbox[diag_ind,][!is.na(val_1) & !is.na(val_2),.N]>0){
      col_1_name<-not_fixed_hyp_checkbox[diag_ind,col_1]
      val_1<-not_fixed_hyp_checkbox[diag_ind,val_1]
      col_names<-not_fixed_hyp_checkbox[diag_ind,col_2]
      values<-c(not_fixed_hyp_checkbox[diag_ind,val_2], not_fixed_hyp_checkbox[diag_ind,val_3], not_fixed_hyp_checkbox[diag_ind,val_4], not_fixed_hyp_checkbox[diag_ind,val_5])
      values<-values[!is.na(values)]
      values_df<-data.table(col_names=values)
      setnames(values_df,"col_names",col_names)
      checkbox_date_col<-not_fixed_hyp_checkbox[diag_ind,date_column]
      data<-data.table(table=not_fixed_hyp_checkbox[diag_ind,table],event_abbreviation=not_fixed_hyp_checkbox[diag_ind,event_abbreviation],col_1=val_1, values_df,keep=not_fixed_hyp_checkbox[diag_ind,keep],index=not_fixed_hyp_checkbox[diag_ind,index], checkbox_date=not_fixed_hyp_checkbox[diag_ind,date_column])
      setnames(data,"col_1",col_1_name)
    } else{
      col_1_name<-not_fixed_hyp_checkbox[diag_ind,col_1]
      val_1<-not_fixed_hyp_checkbox[diag_ind,val_1]
      data<-data.table(table=not_fixed_hyp_checkbox[diag_ind,table],event_abbreviation=not_fixed_hyp_checkbox[diag_ind,event_abbreviation],col_1=val_1,keep=not_fixed_hyp_checkbox[diag_ind,keep],index=not_fixed_hyp_checkbox[diag_ind,index], checkbox_date=not_fixed_hyp_checkbox[diag_ind,date_column])
      setnames(data,"col_1",col_1_name)
    }
    diag_checkbox_hyp[[ind]]<-data
    rm(data)

    ind<-ind+1
  }
  diag_checkbox_hyp<-rbindlist(diag_checkbox_hyp,fill=T)
  diag_checkbox_hyp_mo<-diag_checkbox_hyp[table=="MEDICAL_OBSERVATIONS"]
  if(diag_checkbox_hyp_mo[,.N]==0){diag_checkbox_hyp_mo<-NULL}
  diag_checkbox_hyp_so<-diag_checkbox_hyp[table=="SURVEY_OBSERVATIONS"]
  if(diag_checkbox_hyp_so[,.N]==0){diag_checkbox_hyp_so<-NULL}

}else{
  diag_checkbox_hyp_mo<-NULL
  diag_checkbox_hyp_so<-NULL}

#Diabetes
if(not_fixed_dm_checkbox[,.N]>0){
  not_fixed_dm_checkbox[,index:=1:not_fixed_dm_checkbox[,.N]]
  diag_checkbox_dm<-list()
  ind<-1
  for(diag_ind in 1:not_fixed_dm_checkbox[,.N]){
    if(not_fixed_dm_checkbox[diag_ind,][!is.na(val_1) & !is.na(val_2),.N]>0){
      col_1_name<-not_fixed_dm_checkbox[diag_ind,col_1]
      val_1<-not_fixed_dm_checkbox[diag_ind,val_1]
      col_names<-not_fixed_dm_checkbox[diag_ind,col_2]
      values<-c(not_fixed_dm_checkbox[diag_ind,val_2], not_fixed_dm_checkbox[diag_ind,val_3], not_fixed_dm_checkbox[diag_ind,val_4], not_fixed_dm_checkbox[diag_ind,val_5])
      values<-values[!is.na(values)]
      values_df<-data.table(col_names=values)
      setnames(values_df,"col_names",col_names)
      checkbox_date_col<-not_fixed_dm_checkbox[diag_ind,date_column]
      data<-data.table(table=not_fixed_dm_checkbox[diag_ind,table],event_abbreviation=not_fixed_dm_checkbox[diag_ind,event_abbreviation],col_1=val_1, values_df,keep=not_fixed_dm_checkbox[diag_ind,keep],index=not_fixed_dm_checkbox[diag_ind,index], checkbox_date=not_fixed_dm_checkbox[diag_ind,date_column])
      setnames(data,"col_1",col_1_name)
    } else{
      col_1_name<-not_fixed_dm_checkbox[diag_ind,col_1]
      val_1<-not_fixed_dm_checkbox[diag_ind,val_1]
      data<-data.table(table=not_fixed_dm_checkbox[diag_ind,table],event_abbreviation=not_fixed_dm_checkbox[diag_ind,event_abbreviation],col_1=val_1,keep=not_fixed_dm_checkbox[diag_ind,keep],index=not_fixed_dm_checkbox[diag_ind,index], checkbox_date=not_fixed_dm_checkbox[diag_ind,date_column])
      setnames(data,"col_1",col_1_name)
    }
    diag_checkbox_dm[[ind]]<-data
    rm(data)
    
    ind<-ind+1
  }
  diag_checkbox_dm<-rbindlist(diag_checkbox_dm,fill=T)
  diag_checkbox_dm_mo<-diag_checkbox_dm[table=="MEDICAL_OBSERVATIONS"]
  if(diag_checkbox_dm_mo[,.N]==0){diag_checkbox_dm_mo<-NULL}
  diag_checkbox_dm_so<-diag_checkbox_dm[table=="SURVEY_OBSERVATIONS"]
  if(diag_checkbox_dm_so[,.N]==0){diag_checkbox_dm_so<-NULL}
  
}else{
  diag_checkbox_dm_mo<-NULL
  diag_checkbox_dm_so<-NULL}

#Depresion
if(not_fixed_dep_checkbox[,.N]>0){
  not_fixed_dep_checkbox[,index:=1:not_fixed_dep_checkbox[,.N]]
  diag_checkbox_dep<-list()
  ind<-1
  for(diag_ind in 1:not_fixed_dep_checkbox[,.N]){
    if(not_fixed_dep_checkbox[diag_ind,][!is.na(val_1) & !is.na(val_2),.N]>0){
      col_1_name<-not_fixed_dep_checkbox[diag_ind,col_1]
      val_1<-not_fixed_dep_checkbox[diag_ind,val_1]
      col_names<-not_fixed_dep_checkbox[diag_ind,col_2]
      values<-c(not_fixed_dep_checkbox[diag_ind,val_2], not_fixed_dep_checkbox[diag_ind,val_3], not_fixed_dep_checkbox[diag_ind,val_4], not_fixed_dep_checkbox[diag_ind,val_5])
      values<-values[!is.na(values)]
      values_df<-data.table(col_names=values)
      setnames(values_df,"col_names",col_names)
      checkbox_date_col<-not_fixed_dep_checkbox[diag_ind,date_column]
      data<-data.table(table=not_fixed_dep_checkbox[diag_ind,table],event_abbreviation=not_fixed_dep_checkbox[diag_ind,event_abbreviation],col_1=val_1, values_df,keep=not_fixed_dep_checkbox[diag_ind,keep],index=not_fixed_dep_checkbox[diag_ind,index], checkbox_date=not_fixed_dep_checkbox[diag_ind,date_column])
      setnames(data,"col_1",col_1_name)
    } else{
      col_1_name<-not_fixed_dep_checkbox[diag_ind,col_1]
      val_1<-not_fixed_dep_checkbox[diag_ind,val_1]
      data<-data.table(table=not_fixed_dep_checkbox[diag_ind,table],event_abbreviation=not_fixed_dep_checkbox[diag_ind,event_abbreviation],col_1=val_1,keep=not_fixed_dep_checkbox[diag_ind,keep],index=not_fixed_dep_checkbox[diag_ind,index], checkbox_date=not_fixed_dep_checkbox[diag_ind,date_column])
      setnames(data,"col_1",col_1_name)
    }
    diag_checkbox_dep[[ind]]<-data
    rm(data)
    
    ind<-ind+1
  }
  diag_checkbox_dep<-rbindlist(diag_checkbox_dep,fill=T)
  diag_checkbox_dep_mo<-diag_checkbox_dep[table=="MEDICAL_OBSERVATIONS"]
  if(diag_checkbox_dep_mo[,.N]==0){diag_checkbox_dep_mo<-NULL}
  diag_checkbox_dep_so<-diag_checkbox_dep[table=="SURVEY_OBSERVATIONS"]
  if(diag_checkbox_dep_so[,.N]==0){diag_checkbox_dep_so<-NULL}
  
}else{
  diag_checkbox_dep_mo<-NULL
  diag_checkbox_dep_so<-NULL}

#Parity
if(not_fixed_parity_checkbox[,.N]>0){
  not_fixed_parity_checkbox[,index:=1:not_fixed_parity_checkbox[,.N]]
  diag_checkbox_parity<-list()
  ind<-1
  for(diag_ind in 1:not_fixed_parity_checkbox[,.N]){
    if(not_fixed_parity_checkbox[diag_ind,][!is.na(val_1) & !is.na(val_2),.N]>0){
      col_1_name<-not_fixed_parity_checkbox[diag_ind,col_1]
      val_1<-not_fixed_parity_checkbox[diag_ind,val_1]
      col_names<-not_fixed_parity_checkbox[diag_ind,col_2]
      values<-c(not_fixed_parity_checkbox[diag_ind,val_2], not_fixed_parity_checkbox[diag_ind,val_3], not_fixed_parity_checkbox[diag_ind,val_4], not_fixed_parity_checkbox[diag_ind,val_5])
      values<-values[!is.na(values)]
      values_df<-data.table(col_names=values)
      setnames(values_df,"col_names",col_names)
      checkbox_date_col<-not_fixed_parity_checkbox[diag_ind,date_column]
      data<-data.table(table=not_fixed_parity_checkbox[diag_ind,table],event_abbreviation=not_fixed_parity_checkbox[diag_ind,event_abbreviation],col_1=val_1, values_df,keep=not_fixed_parity_checkbox[diag_ind,keep],index=not_fixed_parity_checkbox[diag_ind,index], checkbox_date=not_fixed_parity_checkbox[diag_ind,date_column])
      setnames(data,"col_1",col_1_name)
    } else{
      col_1_name<-not_fixed_parity_checkbox[diag_ind,col_1]
      val_1<-not_fixed_parity_checkbox[diag_ind,val_1]
      data<-data.table(table=not_fixed_parity_checkbox[diag_ind,table],event_abbreviation=not_fixed_parity_checkbox[diag_ind,event_abbreviation],col_1=val_1,keep=not_fixed_parity_checkbox[diag_ind,keep],index=not_fixed_parity_checkbox[diag_ind,index], checkbox_date=not_fixed_parity_checkbox[diag_ind,date_column])
      setnames(data,"col_1",col_1_name)
    }
    diag_checkbox_parity[[ind]]<-data
    rm(data)
    
    ind<-ind+1
  }
  diag_checkbox_parity<-rbindlist(diag_checkbox_parity,fill=T)
  diag_checkbox_parity_mo<-diag_checkbox_parity[table=="MEDICAL_OBSERVATIONS"]
  if(diag_checkbox_parity_mo[,.N]==0){diag_checkbox_parity_mo<-NULL}
  diag_checkbox_parity_so<-diag_checkbox_parity[table=="SURVEY_OBSERVATIONS"]
  if(diag_checkbox_parity_so[,.N]==0){diag_checkbox_parity_so<-NULL}
  
}else{
  diag_checkbox_parity_mo<-NULL
  diag_checkbox_parity_so<-NULL}

#Obesity
if(not_fixed_bmi_checkbox[,.N]>0){
  not_fixed_bmi_checkbox[,index:=1:not_fixed_bmi_checkbox[,.N]]
  diag_checkbox_obesity<-list()
  ind<-1
  for(diag_ind in 1:not_fixed_bmi_checkbox[,.N]){
    if(not_fixed_bmi_checkbox[diag_ind,][!is.na(val_1) & !is.na(val_2),.N]>0){
      col_1_name<-not_fixed_bmi_checkbox[diag_ind,col_1]
      val_1<-not_fixed_bmi_checkbox[diag_ind,val_1]
      col_names<-not_fixed_bmi_checkbox[diag_ind,col_2]
      values<-c(not_fixed_bmi_checkbox[diag_ind,val_2], not_fixed_bmi_checkbox[diag_ind,val_3], not_fixed_bmi_checkbox[diag_ind,val_4], not_fixed_bmi_checkbox[diag_ind,val_5])
      values<-values[!is.na(values)]
      values_df<-data.table(col_names=values)
      setnames(values_df,"col_names",col_names)
      checkbox_date_col<-not_fixed_bmi_checkbox[diag_ind,date_column]
      data<-data.table(table=not_fixed_bmi_checkbox[diag_ind,table],event_abbreviation=not_fixed_bmi_checkbox[diag_ind,event_abbreviation],col_1=val_1, values_df,keep=not_fixed_bmi_checkbox[diag_ind,keep],index=not_fixed_bmi_checkbox[diag_ind,index], checkbox_date=not_fixed_bmi_checkbox[diag_ind,date_column])
      setnames(data,"col_1",col_1_name)
    } else{
      col_1_name<-not_fixed_bmi_checkbox[diag_ind,col_1]
      val_1<-not_fixed_bmi_checkbox[diag_ind,val_1]
      data<-data.table(table=not_fixed_bmi_checkbox[diag_ind,table],event_abbreviation=not_fixed_bmi_checkbox[diag_ind,event_abbreviation],col_1=val_1,keep=not_fixed_bmi_checkbox[diag_ind,keep],index=not_fixed_bmi_checkbox[diag_ind,index], checkbox_date=not_fixed_bmi_checkbox[diag_ind,date_column])
      setnames(data,"col_1",col_1_name)
    }
    diag_checkbox_obesity[[ind]]<-data
    rm(data)
    
    ind<-ind+1
  }
  diag_checkbox_obesity<-rbindlist(diag_checkbox_obesity,fill=T)
  diag_checkbox_obesity_mo<-diag_checkbox_obesity[table=="MEDICAL_OBSERVATIONS"]
  if(diag_checkbox_obesity_mo[,.N]==0){diag_checkbox_obesity_mo<-NULL}
  diag_checkbox_obesity_so<-diag_checkbox_obesity[table=="SURVEY_OBSERVATIONS"]
  if(diag_checkbox_obesity_so[,.N]==0){diag_checkbox_obesity_so<-NULL}
  
}else{
  diag_checkbox_obesity_mo<-NULL
  diag_checkbox_obesity_so<-NULL}

#### Gather info for columns ####
#Hypertension
if(fixed_hyp_checkbox[,.N]>0){
  diag_fixed_hyp<-list()
  ind<-1
  for(diag_ind in 1:fixed_hyp_checkbox[,.N]){
    if(fixed_hyp_checkbox[diag_ind,][!is.na(val_1),.N]>0){
      col_1_name<-fixed_hyp_checkbox[diag_ind,col_1]
      val_1<-fixed_hyp_checkbox[diag_ind,val_1]
      keep_col<-fixed_hyp_checkbox[diag_ind,col_2]
      checkbox_date_col<-fixed_hyp_checkbox[diag_ind,date_column]
      data<-data.table(table=fixed_hyp_checkbox[diag_ind,table],event_abbreviation=fixed_hyp_checkbox[diag_ind,event_abbreviation],col_1=val_1,keep=keep_col,checkbox_date=checkbox_date_col)
      setnames(data,"col_1",col_1_name)
    }else{data<-NULL} 
    diag_fixed_hyp[[ind]]<-data
    rm(data)
    ind<-ind+1
  }
  
  diag_fixed_hyp<-rbindlist(diag_fixed_hyp,fill=T)
  diag_fixed_hyp_mo<-diag_fixed_hyp[table=="MEDICAL_OBSERVATIONS"]
  if(diag_fixed_hyp_mo[,.N]==0){diag_fixed_hyp_mo<-NULL}
  diag_fixed_hyp_so<-diag_fixed_hyp[table=="SURVEY_OBSERVATIONS"]
  if(diag_fixed_hyp_so[,.N]==0){diag_fixed_hyp_so<-NULL}
  
}else{
  diag_fixed_hyp_mo<-NULL
  diag_fixed_hyp_so<-NULL}

#Diabetes
if(fixed_dm_checkbox[,.N]>0){
  diag_fixed_dm<-list()
  ind<-1
  for(diag_ind in 1:fixed_dm_checkbox[,.N]){
    if(fixed_dm_checkbox[diag_ind,][!is.na(val_1),.N]>0){
      col_1_name<-fixed_dm_checkbox[diag_ind,col_1]
      val_1<-fixed_dm_checkbox[diag_ind,val_1]
      keep_col<-fixed_dm_checkbox[diag_ind,col_2]
      checkbox_date_col<-fixed_dm_checkbox[diag_ind,date_column]
      data<-data.table(table=fixed_dm_checkbox[diag_ind,table],event_abbreviation=fixed_dm_checkbox[diag_ind,event_abbreviation],col_1=val_1,keep=keep_col,checkbox_date=checkbox_date_col)
      setnames(data,"col_1",col_1_name)
    }else{data<-NULL} 
    diag_fixed_dm[[ind]]<-data
    rm(data)
    ind<-ind+1
  }
  
  diag_fixed_dm<-rbindlist(diag_fixed_dm,fill=T)
  diag_fixed_dm_mo<-diag_fixed_dm[table=="MEDICAL_OBSERVATIONS"]
  if(diag_fixed_dm_mo[,.N]==0){diag_fixed_dm_mo<-NULL}
  diag_fixed_dm_so<-diag_fixed_dm[table=="SURVEY_OBSERVATIONS"]
  if(diag_fixed_dm_so[,.N]==0){diag_fixed_dm_so<-NULL}
  
}else{
  diag_fixed_dm_mo<-NULL
  diag_fixed_dm_so<-NULL}

#Depresion
if(fixed_dep_checkbox[,.N]>0){
  diag_fixed_dep<-list()
  ind<-1
  for(diag_ind in 1:fixed_dep_checkbox[,.N]){
    if(fixed_dep_checkbox[diag_ind,][!is.na(val_1),.N]>0){
      col_1_name<-fixed_dep_checkbox[diag_ind,col_1]
      val_1<-fixed_dep_checkbox[diag_ind,val_1]
      keep_col<-fixed_dep_checkbox[diag_ind,col_2]
      checkbox_date_col<-fixed_dep_checkbox[diag_ind,date_column]
      data<-data.table(table=fixed_dep_checkbox[diag_ind,table],event_abbreviation=fixed_dep_checkbox[diag_ind,event_abbreviation],col_1=val_1,keep=keep_col,checkbox_date=checkbox_date_col)
      setnames(data,"col_1",col_1_name)
    }else{data<-NULL} 
    diag_fixed_dep[[ind]]<-data
    rm(data)
    ind<-ind+1
  }
  
  diag_fixed_dep<-rbindlist(diag_fixed_dep,fill=T)
  diag_fixed_dep_mo<-diag_fixed_dep[table=="MEDICAL_OBSERVATIONS"]
  if(diag_fixed_dep_mo[,.N]==0){diag_fixed_dep_mo<-NULL}
  diag_fixed_dep_so<-diag_fixed_dep[table=="SURVEY_OBSERVATIONS"]
  if(diag_fixed_dep_so[,.N]==0){diag_fixed_dep_so<-NULL}
  
}else{
  diag_fixed_dep_mo<-NULL
  diag_fixed_dep_so<-NULL}

#Parity
if(fixed_parity_checkbox[,.N]>0){
  diag_fixed_parity<-list()
  ind<-1
  for(diag_ind in 1:fixed_parity_checkbox[,.N]){
    if(fixed_parity_checkbox[diag_ind,][!is.na(val_1),.N]>0){
      col_1_name<-fixed_parity_checkbox[diag_ind,col_1]
      val_1<-fixed_parity_checkbox[diag_ind,val_1]
      keep_col<-fixed_parity_checkbox[diag_ind,col_2]
      checkbox_date_col<-fixed_parity_checkbox[diag_ind,date_column]
      data<-data.table(table=fixed_parity_checkbox[diag_ind,table],event_abbreviation=fixed_parity_checkbox[diag_ind,event_abbreviation],col_1=val_1,keep=keep_col,checkbox_date=checkbox_date_col)
      setnames(data,"col_1",col_1_name)
    }else{data<-NULL} 
    diag_fixed_parity[[ind]]<-data
    rm(data)
    ind<-ind+1
  }
  
  diag_fixed_parity<-rbindlist(diag_fixed_parity,fill=T)
  diag_fixed_parity_mo<-diag_fixed_parity[table=="MEDICAL_OBSERVATIONS"]
  if(diag_fixed_parity_mo[,.N]==0){diag_fixed_parity_mo<-NULL}
  diag_fixed_parity_so<-diag_fixed_parity[table=="SURVEY_OBSERVATIONS"]
  if(diag_fixed_parity_so[,.N]==0){diag_fixed_parity_so<-NULL}
  
}else{
  diag_fixed_parity_mo<-NULL
  diag_fixed_parity_so<-NULL}

#Obesity
if(fixed_bmi_checkbox[,.N]>0){
  diag_fixed_bmi<-list()
  ind<-1
  for(diag_ind in 1:fixed_bmi_checkbox[,.N]){
    if(fixed_bmi_checkbox[diag_ind,][!is.na(val_1),.N]>0){
      col_1_name<-fixed_bmi_checkbox[diag_ind,col_1]
      val_1<-fixed_bmi_checkbox[diag_ind,val_1]
      keep_col<-fixed_bmi_checkbox[diag_ind,col_2]
      checkbox_date_col<-fixed_bmi_checkbox[diag_ind,date_column]
      data<-data.table(table=fixed_bmi_checkbox[diag_ind,table],event_abbreviation=fixed_bmi_checkbox[diag_ind,event_abbreviation],col_1=val_1,keep=keep_col,checkbox_date=checkbox_date_col)
      setnames(data,"col_1",col_1_name)
    }else{data<-NULL} 
    diag_fixed_bmi[[ind]]<-data
    rm(data)
    ind<-ind+1
  }
  
  diag_fixed_bmi<-rbindlist(diag_fixed_bmi,fill=T)
  diag_fixed_bmi_mo<-diag_fixed_bmi[table=="MEDICAL_OBSERVATIONS"]
  if(diag_fixed_bmi_mo[,.N]==0){diag_fixed_bmi_mo<-NULL}
  diag_fixed_bmi_so<-diag_fixed_bmi[table=="SURVEY_OBSERVATIONS"]
  if(diag_fixed_bmi_so[,.N]==0){diag_fixed_bmi_so<-NULL}
  
}else{
  diag_fixed_bmi_mo<-NULL
  diag_fixed_bmi_so<-NULL}

#Height
if(fixed_h_checkbox[,.N]>0){
  diag_fixed_h<-list()
  ind<-1
  for(diag_ind in 1:fixed_h_checkbox[,.N]){
    if(fixed_h_checkbox[diag_ind,][!is.na(val_1),.N]>0){
      col_1_name<-fixed_h_checkbox[diag_ind,col_1]
      val_1<-fixed_h_checkbox[diag_ind,val_1]
      keep_col<-fixed_h_checkbox[diag_ind,col_2]
      checkbox_date_col<-fixed_h_checkbox[diag_ind,date_column]
      data<-data.table(table=fixed_h_checkbox[diag_ind,table],event_abbreviation=fixed_h_checkbox[diag_ind,event_abbreviation],col_1=val_1,keep=keep_col,checkbox_date=checkbox_date_col)
      setnames(data,"col_1",col_1_name)
    }else{data<-NULL} 
    diag_fixed_h[[ind]]<-data
    rm(data)
    ind<-ind+1
  }
  
  diag_fixed_h<-rbindlist(diag_fixed_h,fill=T)
  diag_fixed_h_mo<-diag_fixed_h[table=="MEDICAL_OBSERVATIONS"]
  if(diag_fixed_h_mo[,.N]==0){diag_fixed_h_mo<-NULL}
  diag_fixed_h_so<-diag_fixed_h[table=="SURVEY_OBSERVATIONS"]
  if(diag_fixed_h_so[,.N]==0){diag_fixed_h_so<-NULL}
  
}else{
  diag_fixed_h_mo<-NULL
  diag_fixed_h_so<-NULL}

#Weight
if(fixed_w_checkbox[,.N]>0){
  diag_fixed_w<-list()
  ind<-1
  for(diag_ind in 1:fixed_w_checkbox[,.N]){
    if(fixed_w_checkbox[diag_ind,][!is.na(val_1),.N]>0){
      col_1_name<-fixed_w_checkbox[diag_ind,col_1]
      val_1<-fixed_w_checkbox[diag_ind,val_1]
      keep_col<-fixed_w_checkbox[diag_ind,col_2]
      checkbox_date_col<-fixed_w_checkbox[diag_ind,date_column]
      data<-data.table(table=fixed_w_checkbox[diag_ind,table],event_abbreviation=fixed_w_checkbox[diag_ind,event_abbreviation],col_1=val_1,keep=keep_col,checkbox_date=checkbox_date_col)
      setnames(data,"col_1",col_1_name)
    }else{data<-NULL} 
    diag_fixed_w[[ind]]<-data
    rm(data)
    ind<-ind+1
  }
  
  diag_fixed_w<-rbindlist(diag_fixed_w,fill=T)
  diag_fixed_w_mo<-diag_fixed_w[table=="MEDICAL_OBSERVATIONS"]
  if(diag_fixed_w_mo[,.N]==0){diag_fixed_w_mo<-NULL}
  diag_fixed_w_so<-diag_fixed_w[table=="SURVEY_OBSERVATIONS"]
  if(diag_fixed_w_so[,.N]==0){diag_fixed_w_so<-NULL}
  
}else{
  diag_fixed_w_mo<-NULL
  diag_fixed_w_so<-NULL}




#### MEDICINES ####
codesheet_medicines_du<-additional_concepts[StudyVar=="du_medicines" & type=="codesheet"]
