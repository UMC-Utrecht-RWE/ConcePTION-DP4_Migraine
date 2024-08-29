####Clean pregnancies####
rm(list=ls())
if(!require(rstudioapi)){install.packages("rstudioapi")}
library(rstudioapi)
projectFolder<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(projectFolder)
source("packages.R")
source(paste0(projectFolder,"/p_steps/clean_folders.R"))
source("99_path.R")
setwd(projectFolder)

#Run pregnancy algorithm and save results in g_intermediate/pregnancy_algorithm
source(paste0(pre_dir,"Step_00_create_clean_pregnancy_D3.R"))
render(paste0(pre_dir,"Report_0_pregnancy_algorithm_flowchart.Rmd"), output_dir = output_dir, output_file = paste0(format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", "Report_0_Pregnancy_algorithm_flowchart.html")) 
source(paste0(pre_dir,"save_environment.R"))

####Create pregnancy study population####
rm(list=ls())
if(!require(rstudioapi)){install.packages("rstudioapi")}
library(rstudioapi)
projectFolder<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(projectFolder)
source("packages.R")
source("99_path.R")
setwd(projectFolder)

#Create study population(persons/observation_periods/pregnancies)
load(paste0(g_intermediate,"environment.RData"))
source(paste0(pre_dir,"Step_01_create_study_population.R"))
render(paste0(pre_dir,"Report_1_pregnancy_study_population.Rmd"), output_dir = output_dir, output_file = paste0(format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", "Report_1_Pregnancy_study_population.html")) 
source(paste0(pre_dir,"save_environment.R"))

####Run diagnostic filtering script####
rm(list=ls())
if(!require(rstudioapi)){install.packages("rstudioapi")}
library(rstudioapi)
projectFolder<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(projectFolder)
source("packages.R")
source("99_path.R")
setwd(projectFolder)

#Create diagnoses D3(GDM/PE/Migraine)
load(paste0(g_intermediate,"environment.RData"))
source(paste0(pre_dir,"Step_02_diagnoses_clean_up.R"))
source(paste0(pre_dir,"Step_02_diagnoses_clean_up_combine.R"))
render(paste0(pre_dir,"Report_2_diagnoses_clean_up.Rmd"), output_dir = output_dir, output_file = paste0(format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", "Report_2_Diagnoses_clean_up.html")) 
source(paste0(pre_dir,"save_environment.R"))

####Run medicines filtering script####
rm(list=ls())
if(!require(rstudioapi)){install.packages("rstudioapi")}
library(rstudioapi)
projectFolder<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(projectFolder)
source("packages.R")
source("99_path.R")
setwd(projectFolder)

#Create medicines D3(GDM/PE/Migraine)
load(paste0(g_intermediate,"environment.RData"))
source(paste0(pre_dir,"Step_03_medicines_clean_up.R"))
source(paste0(pre_dir,"Step_03_medicines_clean_up_combine.R"))
render(paste0(pre_dir,"Report_3_medicines_clean_up.Rmd"), output_dir = output_dir, output_file = paste0(format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", "Report_3_Medicines_clean_up.html")) 
source(paste0(pre_dir,"save_environment.R"))

####Create algorithms for GDM/PE/Migraine####
rm(list=ls())
if(!require(rstudioapi)){install.packages("rstudioapi")}
library(rstudioapi)
projectFolder<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(projectFolder)
source("packages.R")
source("99_path.R")
setwd(projectFolder)

load(paste0(projectFolder,"/g_intermediate/environment.RData"))
source(paste0(pre_dir,"Step_04_algorithms.R"))
source(paste0(pre_dir,"Step_05_algorithms_sensitivity.R"))
render(paste0(pre_dir,"Report_4_gdm_pe_algorithms.Rmd"), output_dir = output_dir, output_file = paste0(format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", "Report_4_gdm_pe_algorithms.html")) 
render(paste0(pre_dir,"Report_4_migraine_algorithms.Rmd"), output_dir = output_dir, output_file = paste0(format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", "Report_4_migraine_algorithms.html")) 
render(paste0(pre_dir,"Report_4_migraine_algorithms_type.Rmd"), output_dir = output_dir, output_file = paste0(format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", "Report_4_migraine_algorithms_type.html")) 
render(paste0(pre_dir,"Report_4_migraine_algorithms_severity.Rmd"), output_dir = output_dir, output_file = paste0(format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", "Report_4_migraine_algorithms_severity.html")) 
source(paste0(pre_dir,"save_environment.R"))


#create masked output csv

source(paste0(projectFolder,"mask.R"))




