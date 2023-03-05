rm(list=ls())
if(!require(rstudioapi)){install.packages("rstudioapi")}
library(rstudioapi)

projectFolder<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(projectFolder)

source("packages.R")
source("99_path.R")
setwd(projectFolder)

#Run pregnancy algorithm and save results in g_intermediate/pregnancy_algorithm
system.time(source(paste0(pre_dir,"Step_00_create_clean_pregnancy_D3.R")))
system.time(render(paste0(pre_dir,"/Report_1_pregnancy_algorithm_flowchart.Rmd"), output_dir = paste0(output_dir,"Pregnancy algorithm/"), output_file = paste0(format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", "Pregnancy_algorithm_flowchart.html"))) 

#Run diagnostic filtering script
source(paste0(pre_dir,"Step_01_a_diagnoses_clean_up.R"))

#Run medicines filtering script
source(paste0(pre_dir,"Step_01_b_medicines_cleanup.R"))

#Create study population(persons/observation_periods/pregnancies)
source(paste0(pre_dir,"Step_02_create_study_population.R"))
file.choose()

#Run GDM algorithm




