#Clean up the medicines table to create the diagnoses D3  

#Load the MEDICINES tables one by one
initial_time_01<-Sys.time()
date_running_start_01<-Sys.Date()

#Clean folders
unlink(paste0(projectFolder,"/g_intermediate/tmp"), recursive = T)#delete folder
dir.create(paste0(projectFolder, "/g_intermediate/tmp"))

if("raw_data" %in% list.files(paste0(projectFolder,"/g_intermediate/diagnoses_d3/"))){
  unlink(paste0(projectFolder,"/g_intermediate/diagnoses_d3/raw_data"), recursive = T)#delete folder 
  dir.create(paste0(projectFolder, "/g_intermediate/diagnoses_d3/raw_data"))
}else{
  dir.create(paste0(projectFolder, "/g_intermediate/diagnoses_d3/raw_data")) 
}
####load info, parameters, conceptsets####
source(paste0(pre_dir,"info/directory_info.R"))
source(paste0(pre_dir,"info/DAP_info.R"))
source(paste0(pre_dir,"parameters/parameters_metadata.R"))
source(paste0(pre_dir,"parameters/study_parameters.R"))

#### Create the programming codelist #####
source(paste0(pre_dir, "codelists/create_concepts_functions.R"))
source(paste0(pre_dir, "Codelists/create_programming_codelist.R"))


#create date flowcharts
Indication<-c("start_coverage", "end_coverage", "minimum_start_pregnancy_date", "maxiumum_start_pregnancy_date", "lookback_period", "after_delivery")
Drug_utilisation<-c(as.character(du_start_study_date),as.character(du_end_study_date),as.character(min_preg_date_du),as.character(max_preg_date_du), as.character(opt_lookback_du), as.character(after_delivery_du))

dates_flowchart<-data.table(Indication,Drug_utilisation)
if(!dir.exists(paste0(output_dir, "Drug utilisation/flowchart")))
  dir.create(paste0(output_dir, "Drug utilisation/flowchart"))
fwrite(dates_flowchart,paste0(output_dir, "Drug utilisation/flowchart/inclusion_dates_flowchart.csv"), row.names = F)
rm(dates_flowchart)
#####The tables that will be search for diagnostic codes: EVENTS, MEDICAL_OBSERVATIONS, SURVEY_OBSERVATIONS####

####EVENTS####
source(paste0(pre_dir,"Step_01_a_diagnoses_clean_up_events.R"))
####MO####
source(paste0(pre_dir,"Step_01_b_diagnoses_clean_up_mo.R"))
####SO####
source(paste0(pre_dir,"Step_01_c_diagnoses_clean_up_so.R"))


date_running_end_01<-Sys.Date()
end_time_01<-Sys.time()

time_log_01<-data.table(DAP=data_access_provider_name,
                     Script="Step_01_diagnoses_clean_up.R", 
                     Start_date=date_running_start_01, 
                     End_date=date_running_end_01,
                     Time_elaspsed=format(end_time_01-initial_time_01, digits=2))
fwrite(time_log_01,paste0(output_dir,"/Time log/DU/Step_01_time_log.csv"),row.names = F)

