#Load parameters
tables_file<-list.files(paste0(projectFolder,"/p_parameters/"), "table_search")
tables_search<-read_excel(paste0(projectFolder,"/p_parameters/",tables_file),col_types = "text")
tables_search<-as.data.table(tables_search)

source(paste0(pre_dir,"info/DAP_info.R"))

#Load additional concepts template
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

#GDM
codesheet_medicines_gdm<-additional_concepts[StudyVar=="GDM_medicines" & type=="codesheet"]
codesheet_diagnoses_gdm<-additional_concepts[StudyVar=="GDM_diagnoses" & type=="codesheet"]
codesheet_procedures_gdm<-additional_concepts[StudyVar=="GDM_procedures" & type=="codesheet"]
not_fixed_gdm_ogtt_all<-additional_concepts[StudyVar=="GDM_OGTT_ALL" & type=="not_fixed"]
not_fixed_gdm_ogtt_yes<-additional_concepts[StudyVar=="GDM_OGTT_YES" & type=="not_fixed"]
not_fixed_gdm_checkbox<-additional_concepts[StudyVar=="GDM_checkbox" & type=="not_fixed"]

#PE
codesheet_diagnoses_pe<-additional_concepts[StudyVar=="PE_diagnoses" & type=="codesheet"]
#codesheet_medicines_pe<-additional_concepts[StudyVar=="PE_medicines" & type=="codesheet"]
#codesheet_procedures_pe<-additional_concepts[StudyVar=="PE_procedures" & type=="codesheet"]
not_fixed_pe_checkbox<-additional_concepts[StudyVar=="GDM_checkbox" & type=="not_fixed"]

#Migraine
codesheet_medicines_migraine<-additional_concepts[StudyVar=="Migraine_medicines" & type=="codesheet"]
codesheet_diagnoses_migraine<-additional_concepts[StudyVar=="Migraine_diagnoses" & type=="codesheet"]
codesheet_procedures_migraine<-additional_concepts[StudyVar=="Migraine_procedures" & type=="codesheet"]




