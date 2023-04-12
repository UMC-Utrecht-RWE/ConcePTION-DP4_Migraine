#Load EVENTS table and apply filter to select PE diagoses/GDM diagnoses/Migraine diagnoses

####load info, parameters, conceptsets####
source(paste0(pre_dir,"info/directory_info.R"))
source(paste0(pre_dir,"info/DAP_info.R"))
source(paste0(pre_dir,"codelists/create_conceptsets.R"))
source(paste0(pre_dir,"parameters/parameters_metadata.R"))
source(paste0(pre_dir,"parameters/study_parameters.R"))


#####The tables that will be search for diagnostic codes: EVENTS, MEDICAL_OBSERVATIONS, SURVEY_OBSERVATIONS####
#The information about GDM diagnoses is saved in 
#codesheet_diagnoses_gdm
#The information about GDM diagnoses is saved in 
#codesheet_diagnoses_pe
#The information about GDM diagnoses is saved in 
#codesheet_diagnoses_migraine
events_gdm_diagnoses<-ifelse("EVENTS" %in% codesheet_diagnoses_gdm[,table],1,0)
mo_gdm_diagnoses<-ifelse("MEDICAL_OBSERVATIONS" %in% c(codesheet_diagnoses_gdm[,table],codesheet_diagnoses_gdm_cat[,table]),1,0)
so_gdm_diagnoses<-ifelse("SURVEY_OBSERVATIONS" %in% c(codesheet_diagnoses_gdm[,table],codesheet_diagnoses_gdm_cat[,table]),1,0)

events_pe_diagnoses<-ifelse("EVENTS" %in% codesheet_diagnoses_pe[,table],1,0)
mo_pe_diagnoses<-ifelse("MEDICAL_OBSERVATIONS" %in% c(codesheet_diagnoses_pe[,table],codesheet_diagnoses_pe_cat[,table]),1,0)
so_pe_diagnoses<-ifelse("SURVEY_OBSERVATIONS" %in% c(codesheet_diagnoses_pe[,table],codesheet_diagnoses_pe_cat[,table]),1,0)

events_migraine_diagnoses<-ifelse("EVENTS" %in% codesheet_diagnoses_migraine[,table],1,0)
mo_migraine_diagnoses<-ifelse("MEDICAL_OBSERVATIONS" %in% c(codesheet_diagnoses_migraine[,table],codesheet_diagnoses_migraine_cat[,table]),1,0)
so_migraine_diagnoses<-ifelse("SURVEY_OBSERVATIONS" %in% c(codesheet_diagnoses_migraine[,table],codesheet_diagnoses_migraine_cat[,table]),1,0)

####EVENTS####
source(paste0(pre_dir,"Step_02_a_diagnoses_clean_up_events.R"))
####MO####
source(paste0(pre_dir,"Step_02_b_diagnoses_clean_up_mo.R"))
####SO####
source(paste0(pre_dir,"Step_02_c_diagnoses_clean_up_so.R"))


####Create flowchart and export####
#gdm&pe
flowchart<-merge.data.table(flowchart_events_gdm_pe,flowchart_mo_gdm_pe, by="Indicator")
rm(flowchart_events_gdm_pe,flowchart_mo_gdm_pe)
flowchart<-merge.data.table(flowchart,flowchart_so_gdm_pe, by="Indicator")
rm(flowchart_so_gdm_pe)
fwrite(flowchart, paste0(projectFolder, "/g_output/PE and GDM algorithm/flowchart_diagnostic_tables.csv"))
rm(flowchart)

flowchart<-merge.data.table(flowchart_events_mig,flowchart_mo_mig, by="Indicator")
rm(flowchart_events_mig,flowchart_mo_mig)
flowchart<-merge.data.table(flowchart,flowchart_so_mig, by="Indicator")
rm(flowchart_so_mig)
fwrite(flowchart, paste0(projectFolder, "/g_output/Migraine algorithm/flowchart_diagnostic_tables.csv"))
rm(flowchart)

####Combine results by year and event####
###GDM files####
#GDM checkbox files
gdm_checkbox_files<-list.files(tmp, "GDM_diagnoses_checkbox")
if(length(gdm_checkbox_files)>0){
  #Combine files by type of event
  files<-list()
  for (i in 1: length(gdm_checkbox_files)){
    files<-append(files,substr(gdm_checkbox_files[i],1,4))
  }
  files<-do.call(c,files)
  #remove duplicates 
  files<-files[!duplicated(files)]
  #create list with names year_condition
  gdm_list<-vector(mode="list", length=length(files))
  names(gdm_list)<-files
  rm(files)
  #separate all files into the right category
  for (i in 1:length(gdm_list)){
    gdm_list[[i]]<-gdm_checkbox_files[startsWith(gdm_checkbox_files,names(gdm_list)[i])]
  }
  rm(gdm_checkbox_files)
  gdm_checkbox_files<-gdm_list
  rm(gdm_list)
  
  #Load all files, combine by year, clean up by removing duplicated diagnoses
  duplicates_gdm_checkbox<-list()
  index_gdm<-1
  for (gdm_fl in 1:length(gdm_checkbox_files)){
    event_df<-lapply(paste0(tmp, gdm_checkbox_files[[gdm_fl]]), readRDS)
    event_df<-rbindlist(event_df,fill = T)
    #create variable person_id_^_event date
    event_df[,comb:=paste0(person_id, "_", event_date, "_", condition)]
    dup<-event_df[duplicated(comb),.N]
    duplicates_gdm_checkbox[[index_gdm]]<-dup
    rm(dup)
    event_df<-event_df[!duplicated(comb)]
    event_df[,comb:=NULL]
    #write the new files to g_intermediate
    saveRDS(event_df,paste0(projectFolder, "/g_intermediate/gdm_algorithm/", names(gdm_checkbox_files)[gdm_fl], "_GDM_checkbox.rds"))
    rm(event_df)
    index_gdm<-index_gdm+1
  }
  rm(index_gdm)
  
  #remove all gdm files from tmp
  for (i in 1:length(gdm_checkbox_files)){
    file.remove(paste0(tmp,gdm_checkbox_files[[i]]))
  }
  
  #Export duplicates
  duplicates_gdm_checkbox<-sum(do.call(rbind,duplicates_gdm_checkbox))
  dup<-data.table(Indicator="GDM Algorithm, Duplicated diagnostic checkbox records removed:", no_records=duplicates_gdm_checkbox)
  fwrite(dup, paste0(projectFolder, "/g_output/PE and GDM algorithm/removed_diagostic_checkbox_gdm.csv"), row.names = F)
  rm(dup)
} else {rm(gdm_checkbox_files)}

#Diagnostic files
gdm_events<-paste0("_GDM_", names(conditions_gdm))
gdm_events_sentence<-paste(gdm_events, collapse = "|")
gdm_events_cat<-list.files(tmp, "GDM_diagnoses_cat")
gdm_files<-list.files(tmp, gdm_events_sentence)
gdm_files<-c(gdm_files,gdm_events_cat)
if(length(gdm_files)>0){
#Combine files by type of event
files<-list()
for (i in 1: length(gdm_files)){
  files<-append(files,substr(gdm_files[i],1,4))
}
files<-do.call(c,files)
#remove duplicates 
files<-files[!duplicated(files)]
#create list with names year_condition
gdm_list<-vector(mode="list", length=length(files))
names(gdm_list)<-files
rm(files)
#separate all files into the right category
for (i in 1:length(gdm_list)){
  gdm_list[[i]]<-gdm_files[startsWith(gdm_files,names(gdm_list)[i])]
}
rm(gdm_files)
gdm_files<-gdm_list
rm(gdm_list)

#Load all files, combine by year, clean up by removing duplicated diagnoses
duplicates_gdm<-list()
index_gdm<-1
for (gdm_fl in 1:length(gdm_files)){
  event_df<-lapply(paste0(tmp, gdm_files[[gdm_fl]]), readRDS)
  event_df<-rbindlist(event_df,fill = T)
  #create variable person_id_^_event date
  event_df[,comb:=paste0(person_id, "_", event_date, "_", condition)]
  dup<-event_df[duplicated(comb),.N]
  duplicates_gdm[[index_gdm]]<-dup
  rm(dup)
  event_df<-event_df[!duplicated(comb)]
  event_df[,comb:=NULL]
  #write the new files to g_intermediate
  saveRDS(event_df,paste0(projectFolder, "/g_intermediate/gdm_algorithm/", names(gdm_files)[gdm_fl], "_GDM_diagnoses.rds"))
rm(event_df)
index_gdm<-index_gdm+1
}

#remove all gdm files from tmp
for (i in 1:length(gdm_files)){
  file.remove(paste0(tmp,gdm_files[[i]]))
}

#Export duplicates
duplicates_gdm<-sum(do.call(rbind,duplicates_gdm))
dup<-data.table(Indicator="GDM Algorithm, Duplicated diagnostic records removed:", no_records=duplicates_gdm)
fwrite(dup, paste0(projectFolder, "/g_output/PE and GDM algorithm/removed_diagostic_duplicates_gdm.csv"), row.names = F)
rm(dup)
} else {rm(gdm_files)}


###PE files####
#PE checkbox files
pe_checkbox_files<-list.files(tmp, "PE_diagnoses_checkbox")
if(length(pe_checkbox_files)>0){
  #Combine files by type of event
  files<-list()
  for (i in 1: length(pe_checkbox_files)){
    files<-append(files,substr(pe_checkbox_files[i],1,4))
  }
  files<-do.call(c,files)
  #remove duplicates 
  files<-files[!duplicated(files)]
  #create list with names year_condition
  pe_list<-vector(mode="list", length=length(files))
  names(pe_list)<-files
  rm(files)
  #separate all files into the right category
  for (i in 1:length(pe_list)){
    pe_list[[i]]<-pe_checkbox_files[startsWith(pe_checkbox_files,names(pe_list)[i])]
  }
  rm(pe_checkbox_files)
  pe_checkbox_files<-pe_list
  rm(pe_list)
  
  #Load all files, combine by year, clean up by removing duplicated diagnoses
  duplicates_pe_checkbox<-list()
  index_pe<-1
  for (pe_fl in 1:length(pe_checkbox_files)){
    event_df<-lapply(paste0(tmp, pe_checkbox_files[[pe_fl]]), readRDS)
    event_df<-rbindlist(event_df,fill = T)
    #create variable person_id_^_event date
    event_df[,comb:=paste0(person_id, "_", event_date, "_", condition)]
    dup<-event_df[duplicated(comb),.N]
    duplicates_pe_checkbox[[index_pe]]<-dup
    rm(dup)
    event_df<-event_df[!duplicated(comb)]
    event_df[,comb:=NULL]
    #write the new files to g_intermediate
    saveRDS(event_df,paste0(projectFolder, "/g_intermediate/pe_algorithm/", names(pe_checkbox_files)[pe_fl], "_PE_checkbox.rds"))
    rm(event_df)
    index_pe<-index_pe+1
  }
  rm(index_pe)
  
  #remove all gdm files from tmp
  for (i in 1:length(pe_checkbox_files)){
    file.remove(paste0(tmp,pe_checkbox_files[[i]]))
  }
  
  #Export duplicates
  duplicates_pe_checkbox<-sum(do.call(rbind,duplicates_pe_checkbox))
  dup<-data.table(Indicator="PE Algorithm, Duplicated diagnostic checkbox records removed:", no_records=duplicates_pe_checkbox)
  fwrite(dup, paste0(projectFolder, "/g_output/PE and PE algorithm/removed_diagostic_checkbox_pe.csv"), row.names = F)
  rm(dup)
} else {rm(pe_checkbox_files)}

#Diagnostic files
pe_events<-paste0("_PE_", names(conditions_pe))
pe_events_sentence<-paste(pe_events, collapse = "|")
pe_events_cat<-list.files(tmp, "PE_diagnoses_cat")
pe_files<-list.files(tmp, pe_events_sentence)
pe_files<-c(pe_files,pe_events_cat)

if(length(pe_files)>0){
  #Combine files by type of event
  files<-list()
  for (i in 1: length(pe_files)){
    files<-append(files,substr(pe_files[i],1,4))
  }
  files<-do.call(c,files)
  #remove duplicates 
  files<-files[!duplicated(files)]
  #create list with names year_condition
  pe_list<-vector(mode="list", length=length(files))
  names(pe_list)<-files
  rm(files)
  #separate all files into the right category
  for (i in 1:length(pe_list)){
    pe_list[[i]]<-pe_files[startsWith(pe_files,names(pe_list)[i])]
  }
  rm(pe_files)
  pe_files<-pe_list
  rm(pe_list)
  
  #Load all files, combine by year, clean up by removing duplicated diagnoses
  duplicates_pe<-list()
  index_pe<-1
  for (pe_fl in 1:length(pe_files)){
    event_df<-lapply(paste0(tmp, pe_files[[pe_fl]]), readRDS)
    event_df<-rbindlist(event_df,fill=T)
    #create variable person_id_^_event date
    event_df[,comb:=paste0(person_id, "_", event_date, "_", condition)]
    dup<-event_df[duplicated(comb),.N]
    duplicates_pe[[index_pe]]<-dup
    rm(dup)
    event_df<-event_df[!duplicated(comb)]
    event_df[,comb:=NULL]
    #write the new files to g_intermediate
    saveRDS(event_df,paste0(projectFolder, "/g_intermediate/pe_algorithm/", names(pe_files)[pe_fl], "_PE_diagnoses.rds"))
    rm(event_df)
    index_pe<-index_pe+1
  }
  
  #remove all pe files from tmp
  for (i in 1:length(pe_files)){
    file.remove(paste0(tmp,pe_files[[i]]))
  }
  
  #Export duplicates
  duplicates_pe<-sum(do.call(rbind,duplicates_pe))
  dup<-data.table(Indicator="PE Algorithm, Duplicated diagnostic records removed:", no_records=duplicates_pe)
  fwrite(dup, paste0(projectFolder, "/g_output/PE and GDM algorithm/removed_diagostic_duplicates_pe.csv"), row.names = F)
  rm(dup)
} else {rm(pe_files)}
###Migraine files####
migraine_events<-paste0("_Migraine_", names(conditions_migraine))
migraine_events_sentence<-paste(migraine_events, collapse = "|")
migraine_events_cat<-list.files(tmp, "Migraine_diagnoses_cat")
migraine_files<-list.files(tmp, migraine_events_sentence)
migraine_files<-c(migraine_files,migraine_events_cat)


if(length(migraine_files)>0){
  #Combine files by type of event
  files<-list()
  for (i in 1: length(migraine_files)){
    files<-append(files,substr(migraine_files[i],1,4))
  }
  files<-do.call(c,files)
  #remove duplicates 
  files<-files[!duplicated(files)]
  #create list with names year_condition
  migraine_list<-vector(mode="list", length=length(files))
  names(migraine_list)<-files
  rm(files)
  #separate all files into the right category
  for (i in 1:length(migraine_list)){
    migraine_list[[i]]<-migraine_files[startsWith(migraine_files,names(migraine_list)[i])]
  }
  rm(migraine_files)
  migraine_files<-migraine_list
  rm(migraine_list)
  
  #Load all files, combine by year, clean up by removing duplicated diagnoses
  duplicates_migraine<-list()
  index_migraine<-1
  for (migraine_fl in 1:length(migraine_files)){
    event_df<-lapply(paste0(tmp, migraine_files[[migraine_fl]]), readRDS)
    event_df<-rbindlist(event_df, fill = T)
    #create variable person_id_^_event date
    event_df[,comb:=paste0(person_id, "_", event_date, "_", condition)]
    dup<-event_df[duplicated(comb),.N]
    duplicates_migraine[[index_migraine]]<-dup
    rm(dup)
    event_df<-event_df[!duplicated(comb)]
    event_df[,comb:=NULL]
    #write the new files to g_intermediate
    saveRDS(event_df,paste0(projectFolder, "/g_intermediate/migraine_algorithm/", names(migraine_files)[migraine_fl], "_Migraine_diagnoses.rds"))
    rm(event_df)
    index_migraine<-index_migraine+1
  }
  
  #remove all pe files from tmp
  for (i in 1:length(migraine_files)){
    file.remove(paste0(tmp,migraine_files[[i]]))
  }
  
  #Export duplicates
  duplicates_migraine<-sum(do.call(rbind,duplicates_migraine))
  dup<-data.table(Indicator="Migraine Algorithm, Duplicated diagnostic records removed:", no_records=duplicates_migraine)
  fwrite(dup, paste0(projectFolder, "/g_output/Migraine algorithm/removed_diagostic_duplicates_migraine.csv"), row.names = F)
  rm(dup)
} else {rm(migraine_files)}




