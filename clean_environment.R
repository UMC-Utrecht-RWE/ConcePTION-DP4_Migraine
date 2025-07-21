rm(list=setdiff(ls(), c("CDM_dir","PregnancyAlgorithm_g_output_dir")))

if(!"INSTANCE.CSV" %in% toupper(list.files(CDM_dir))){
  stop("CDM_dir does not include an expected ConcePTION CDM!")
}


if(!"d3_pregnancy_final.rdata" %in% tolower(list.files(PregnancyAlgorithm_g_output_dir))){
  if("d3_pregnancy_final.rdata" %in% tolower(list.files(file.path(PregnancyAlgorithm_g_output_dir, "g_output")))){
    PregnancyAlgorithm_g_output_dir <- file.path(PregnancyAlgorithm_g_output_dir, "g_output")
  }else{
    stop(paste0("'",PregnancyAlgorithm_g_output_dir,"' is not a valid path to 'PregnancyAlgorithm' output directory"))
  }
}



CDM_dir <- paste0(CDM_dir, "/")
PregnancyAlgorithm_g_output_dir <- paste0(PregnancyAlgorithm_g_output_dir, "/")

CDM_dir <- gsub("/+","/",gsub("\\\\","/",CDM_dir))
PregnancyAlgorithm_g_output_dir <-  gsub("/+","/",gsub("\\\\","/",PregnancyAlgorithm_g_output_dir))


if(!require(rstudioapi)){install.packages("rstudioapi")}
library(rstudioapi)
projectFolder<-getwd()

suppressWarnings(suppressMessages(source("packages.R")))
source("99_path.R")
setwd(projectFolder)


rm <- function(...){suppressWarnings(base::rm(...))}