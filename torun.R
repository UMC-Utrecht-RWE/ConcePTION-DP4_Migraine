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

#Run diagnostic filtering script
source(paste0(pre_dir,"Step_01_a_diagnoses_clean_up.R"))

#Run medicines filtering script
source(paste0(pre_dir,"Step_01_b_medicines_cleanup.R"))

