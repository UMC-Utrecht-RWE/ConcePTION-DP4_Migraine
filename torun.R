####Clean pregnancies####
rm(list=ls())
if(!require(rstudioapi)){install.packages("rstudioapi")}
library(rstudioapi)
projectFolder<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(projectFolder)
source("packages.R")
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

####Run diagnostic filtering script####
source(paste0(pre_dir,"Step_02_a_diagnoses_clean_up.R"))

#Run medicines filtering script
source(paste0(pre_dir,"Step_02_b_medicines_cleanup.R"))

#Run GDM algorithm




