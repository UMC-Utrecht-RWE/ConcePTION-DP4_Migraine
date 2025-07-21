
#####################################################################
### Set parameters ##################################################
#####################################################################

## PATH to the CDM direcory
CDM_dir<-"N:/durable/ConcePTION/CDMInstances/ConcePTION_2023/"

## PATH to the PregnancyAlgorithm outputs directory
PregnancyAlgorithm_g_output_dir <- "N:/durable/ConcePTION/Data characterisation/ConcePTIONAlgorithmPregnancies/v5.2.1_overCDM(ConcePTION2023)/g_output/"


######################################################################
#### DP4 Migraine algorithm and Drug Utilization study ###############
######################################################################

########### Migraine Algorithm ##############
source("clean_environment.R")
#Clean previous outputs
pre_dir<-paste0(projectFolder,"/p_steps/")
source(paste0(pre_dir,"clean_folders.R"))
source(paste0(pre_dir,"Step_00_create_clean_pregnancy_D3.R"))
render(paste0(pre_dir,"Report_0_pregnancy_algorithm_flowchart.Rmd"), output_dir = output_dir, output_file = paste0(format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", "Report_0_Pregnancy_algorithm_flowchart.html")) 
source(paste0(pre_dir,"save_environment.R"))

####Create pregnancy study population####
source("clean_environment.R")
#Create study population(persons/observation_periods/pregnancies)
load(paste0(g_intermediate,"environment.RData"))
pre_dir<-paste0(projectFolder,"/p_steps/")
source(paste0(pre_dir,"Step_01_create_study_population.R"))
render(paste0(pre_dir,"Report_1_pregnancy_study_population.Rmd"), output_dir = output_dir, output_file = paste0(format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", "Report_1_Pregnancy_study_population.html")) 
source(paste0(pre_dir,"save_environment.R"))

####Run diagnostic filtering script####
source("clean_environment.R")
#Create diagnoses D3(GDM/PE/Migraine)
load(paste0(g_intermediate,"environment.RData"))
pre_dir<-paste0(projectFolder,"/p_steps/")
#save wsp
source(paste0(pre_dir,"Step_02_diagnoses_clean_up.R"))
source(paste0(pre_dir,"Step_02_diagnoses_clean_up_combine.R"))
render(paste0(pre_dir,"Report_2_diagnoses_clean_up.Rmd"), output_dir = output_dir, output_file = paste0(format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", "Report_2_Diagnoses_clean_up.html")) 
source(paste0(pre_dir,"save_environment.R"))

####Run medicines filtering script####
source("clean_environment.R")
#Create medicines D3(GDM/PE/Migraine)
load(paste0(g_intermediate,"environment.RData"))
pre_dir<-paste0(projectFolder,"/p_steps/")
source(paste0(pre_dir,"Step_03_medicines_clean_up.R"))
source(paste0(pre_dir,"Step_03_medicines_clean_up_combine.R"))
render(paste0(pre_dir,"Report_3_medicines_clean_up.Rmd"), output_dir = output_dir, output_file = paste0(format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", "Report_3_Medicines_clean_up.html")) 
source(paste0(pre_dir,"save_environment.R"))

####Create algorithms for GDM/PE/Migraine####
source("clean_environment.R")
load(paste0(projectFolder,"/g_intermediate/environment.RData"))
pre_dir<-paste0(projectFolder,"/p_steps/")
source(paste0(pre_dir,"Step_04_algorithms.R"))
source(paste0(pre_dir,"Step_05_algorithms_sensitivity.R"))
render(paste0(pre_dir,"Report_4_gdm_pe_algorithms.Rmd"), output_dir = output_dir, output_file = paste0(format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", "Report_4_gdm_pe_algorithms.html")) 
render(paste0(pre_dir,"Report_4_migraine_algorithms.Rmd"), output_dir = output_dir, output_file = paste0(format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", "Report_4_migraine_algorithms.html")) 
render(paste0(pre_dir,"Report_4_migraine_algorithms_type.Rmd"), output_dir = output_dir, output_file = paste0(format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", "Report_4_migraine_algorithms_type.html")) 
render(paste0(pre_dir,"Report_4_migraine_algorithms_severity.Rmd"), output_dir = output_dir, output_file = paste0(format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", "Report_4_migraine_algorithms_severity.html")) 
source(paste0(pre_dir,"save_environment.R"))

#create masked output csv
source(file.path(projectFolder,"mask.R"))


########### DRUG UTILISATION ##############
source("clean_environment.R")
pre_dir<-paste0(projectFolder,"/p_steps_du/")
source(paste0(pre_dir,"Step_00_select_pregnancy_D3_primary.R"))
source(paste0(pre_dir,"Step_00_select_pregnancy_D3_sensitivity.R"))
source(paste0(pre_dir,"save_environment.R"))
####Run diagnostic filtering script####
source("clean_environment.R")
load(paste0(g_intermediate,"environment.RData"))
pre_dir<-paste0(projectFolder,"/p_steps_du/")
source(paste0(pre_dir,"Step_01_diagnoses_clean_up.R"))
source(paste0(pre_dir,"Step_01_diagnoses_clean_up_combine.R"))
source(paste0(pre_dir,"save_environment.R"))

####Run medicines filtering script####
source("clean_environment.R")
#Create medicines D3(GDM/PE/Migraine)
load(paste0(g_intermediate,"environment.RData"))
pre_dir<-paste0(projectFolder,"/p_steps_du/")
source(paste0(pre_dir,"Step_02_medicines_clean_up.R"))
source(paste0(pre_dir,"Step_02_medicines_clean_up_combine.R"))
source(paste0(pre_dir,"save_environment.R"))

####Apply time anchoring####
source("clean_environment.R")
load(paste0(projectFolder,"/g_intermediate/environment.RData"))
pre_dir<-paste0(projectFolder,"/p_steps_du/")
source(paste0(pre_dir,"Step_03_time_anchoring.R"))
source(paste0(pre_dir,"save_environment.R"))


#### Creating Shell tables ####
source("clean_environment.R")
load(paste0(projectFolder,"/g_intermediate/environment.RData"))
pre_dir<-paste0(projectFolder,"/p_steps_du/")
DU_sensitivity_analysis = FALSE
suppressWarnings(source(paste0(pre_dir,"Step_07_shell_tables.R")))
masking = FALSE
render(paste0(pre_dir,"Step_07_shell_tables.Rmd"), output_dir = "g_output/", output_file = paste0(format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", "Report_5_drug_utilisation.html"))


### Sensitivity Analysis ###
source("clean_environment.R")
load(paste0(projectFolder,"/g_intermediate/environment.RData"))
pre_dir<-paste0(projectFolder,"/p_steps_du/")
DU_sensitivity_analysis = TRUE
masking = FALSE
suppressWarnings(source(paste0(pre_dir,"Step_07_shell_tables.R")))
render(paste0(pre_dir,"Step_07_shell_tables_sensitivity.Rmd"), output_dir = "g_output", output_file = paste0(format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", "Report_5_drug_utilisation_sensitivity.html"))


sking = TRUE
source(paste0(pre_dir,"Step_09_masking_small_counts.R"))

cat("\nDone!\n")

