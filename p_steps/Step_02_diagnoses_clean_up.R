
initial_time<-Sys.time()
date_running_start<-Sys.Date()

#Clean folders
unlink(paste0(projectFolder,"/g_intermediate/tmp"), recursive = T)#delete folder
dir.create(paste0(projectFolder, "/g_intermediate/tmp"))

unlink(paste0(projectFolder,"/g_intermediate/gdm_algorithm"), recursive = T)#delete folder
dir.create(paste0(projectFolder, "/g_intermediate/gdm_algorithm"))

unlink(paste0(projectFolder,"/g_intermediate/pe_algorithm"), recursive = T)#delete folder
dir.create(paste0(projectFolder, "/g_intermediate/pe_algorithm"))

unlink(paste0(projectFolder,"/g_intermediate/migraine_algorithm"), recursive = T)#delete folder
dir.create(paste0(projectFolder, "/g_intermediate/migraine_algorithm"))

unlink(paste0(projectFolder,"/g_output/PE and GDM algorithm"), recursive = T)#delete folder
dir.create(paste0(projectFolder, "/g_output/PE and GDM algorithm"))

unlink(paste0(projectFolder,"/g_output/Migraine algorithm"), recursive = T)#delete folder
dir.create(paste0(projectFolder, "/g_output/Migraine algorithm"))


#Load EVENTS table and apply filter to select PE diagoses/GDM diagnoses/Migraine diagnoses
####load info, parameters, conceptsets####
source(paste0(pre_dir,"info/directory_info.R"))
source(paste0(pre_dir,"info/DAP_info.R"))
source(paste0(pre_dir,"codelists/create_conceptsets.R"))
source(paste0(pre_dir,"parameters/parameters_metadata.R"))
source(paste0(pre_dir,"parameters/study_parameters.R"))

#create date flowcharts
Indication<-c("start_coverage", "end_coverage", "minimum_start_pregnancy_date", "maxiumum_start_pregnancy_date","minimum_start_pregnancy_date_5_years")
GDM_and_PE<-c(gdm_pe_start_study_date,gdm_pe_end_study_date,min_preg_date_gdm_pe,max_preg_date_gdm_pe, ifelse(!is.null(gdm_pe_start_preg_lookback),gdm_pe_start_preg_lookback,NA))
Migraine<-c(mig_start_study_date,mig_end_study_date,min_preg_date_mig,max_preg_date_mig, ifelse(!is.null(mig_start_preg_lookback),mig_start_preg_lookback,NA))
Drug_utilisation<-c(du_start_study_date,du_end_study_date,min_preg_date_du,max_preg_date_du, ifelse(!is.null(du_start_preg_lookback),du_start_preg_lookback,NA))
Safety<-c(saf_start_study_date,saf_end_study_date,min_preg_date_saf,max_preg_date_saf, ifelse(!is.null(saf_start_preg_lookback),saf_start_preg_lookback,NA))

dates_flowchart<-data.table(Indication,GDM_and_PE,Migraine,Drug_utilisation,Safety)
fwrite(dates_flowchart,paste0(output_dir, "PE and GDM algorithm/inclusion_dates_flowchart.csv"), row.names = F)
fwrite(dates_flowchart,paste0(output_dir, "Migraine algorithm/inclusion_dates_flowchart.csv"), row.names = F)

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
print("Create flowchart_gdm_pe.")
flowchart<-merge.data.table(flowchart_events_gdm_pe,flowchart_mo_gdm_pe, by="Indicator")
rm(flowchart_events_gdm_pe,flowchart_mo_gdm_pe)
flowchart<-merge.data.table(flowchart,flowchart_so_gdm_pe, by="Indicator")
rm(flowchart_so_gdm_pe)
fwrite(flowchart, paste0(projectFolder, "/g_output/PE and GDM algorithm/Step_02_flowchart_diagnostic_tables.csv"))
rm(flowchart)
print("Create flowchart_migraine.")
flowchart<-merge.data.table(flowchart_events_mig,flowchart_mo_mig, by="Indicator")
rm(flowchart_events_mig,flowchart_mo_mig)
flowchart<-merge.data.table(flowchart,flowchart_so_mig, by="Indicator")
rm(flowchart_so_mig)
fwrite(flowchart, paste0(projectFolder, "/g_output/Migraine algorithm/Step_02_flowchart_diagnostic_tables.csv"))
rm(flowchart)


date_running_end<-Sys.Date()
end_time<-Sys.time()

time_log<-data.table(DAP=data_access_provider_name,
                     Script="Step_02_diagnoses_clean_up.R", 
                     Start_date=date_running_start, 
                     End_date=date_running_end,
                     Time_elaspsed=format(end_time-initial_time, digits=2))
fwrite(time_log,paste0(output_dir,"/Time log/Step_02_time_log.csv"),row.names = F)

