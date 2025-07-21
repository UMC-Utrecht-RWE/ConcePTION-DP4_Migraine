#Directory
setwd('..') #in Demonstration project
setwd('..') #in Migraine DP
dir_base<-getwd()
# set the name of the study

## PATH to the CDM (direcory that incldeus cdm_instance.csv table)
dirinput<-CDM_dir

## PATH to the PregnancyAlgorithm outputs (directory that includes D3_pregnancy_final.rds file)
dirpregnancy <- PregnancyAlgorithm_g_output_dir


path<-path_dir<-dirinput

#Set the path to where you want your report to be saved(make sure that the output folder already exists)
output_dir<-paste0(projectFolder,"/g_output/")
path_output<-output_dir

pre_dir<-paste0(projectFolder,"/p_steps/")

g_intermediate<-paste0(projectFolder,"/g_intermediate/")
tmp<-paste0(g_intermediate,"tmp/")
#populations_dir<-paste0(g_intermediate,"populations/")

#eurocat_dir<-paste0(output_dir,"EUROCAT/")

dirinput<-path
thisdir<-paste0(pre_dir,"Pregnancy algorithm")

dir_DP1_algorithm<-projectFolder
