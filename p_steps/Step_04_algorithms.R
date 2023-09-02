initial_time_04<-Sys.time()
date_running_start_04<-Sys.Date()


age_band_creation<-function(x){
  if(x>=15 & x<=24){y<-"15-24"}
  if(x>=25 & x<=29){y<-"25-29"}
  if(x>=30 & x<=34){y<-"30-34"}
  if(x>=35 & x<=39){y<-"35-39"}
  if(x>=40){y<-"40+"}
  return(y)
}

year_group_creation<-function(x){
  if(x>=2005 & x<=2009){y<-"2005-2009"}
  if(x>=2010 & x<=2014){y<-"2010-2014"}
  if(x>=2015 & x<=2019){y<-"2015-2019"}
  if(x>=2020){y<-"2020-present"}
  return(y)
}

#### GDM and PE ####
#Load all gdm_algorithm file and combine with the pregnancy D3
pregnancy_d3_gdm_pe<-readRDS(paste0(projectFolder,"/g_intermediate/pregnancy_d3/GDM_PE_Pregnancy_D3.rds"))
pregnancy_d3_gdm_pe[,pregnancy_start_date:=as.IDate(pregnancy_start_date)][,pregnancy_end_date:=as.IDate(pregnancy_end_date)][,birth_date:=as.IDate(birth_date)][,death_date:=as.IDate(death_date)][,op_start_date_gdm_pe:=as.IDate(op_start_date_gdm_pe)][,op_end_date_gdm_pe:=as.IDate(op_end_date_gdm_pe)]
pregnancy_d3_gdm_pe[,age:=floor((pregnancy_start_date-birth_date)/365.25)]
pregnancy_d3_gdm_pe[,maternal_age:=as.character(lapply(age, age_band_creation))]
pregnancy_d3_gdm_pe[,year:=year(pregnancy_start_date)]
pregnancy_d3_gdm_pe[,year_group:=as.character(lapply(year, year_group_creation))]

if("final_d3" %in% list.files(paste0(projectFolder,"/g_intermediate/gdm_algorithm/"))){
  unlink(paste0(projectFolder,"/g_intermediate/gdm_algorithm/final_d3"), recursive = T)
}
source(paste0(projectFolder,"/p_steps/Step_04_a_gdm_algorithms.R"))
if("final_d3" %in% list.files(paste0(projectFolder,"/g_intermediate/pe_algorithm/"))){
  unlink(paste0(projectFolder,"/g_intermediate/pe_algorithm/final_d3"), recursive = T)
}
source(paste0(projectFolder,"/p_steps/Step_04_b_pe_algorithms.R"))

#removed rec
removed_rec<-rbind(removed_rec_gdm,removed_rec_pe)
fwrite(removed_rec,paste0(projectFolder,"/g_output/PE and GDM algorithm/Step_04_excluded_records_gdm_pe.csv"),row.names = F)
rm(removed_rec,removed_rec_gdm,removed_rec_pe)

sum<-rbind(sum_gdm,sum_pe)
fwrite(sum,paste0(projectFolder,"/g_output/PE and GDM algorithm/Step_04_summary_included_records_gdm_pe.csv"),row.names = F)
rm(sum,sum_gdm,sum_pe)

rm(pregnancy_d3_gdm_pe)
gc()

#### Migraine ####
#Load all migraine_algorithm file and combine with the pregnancy D3
pregnancy_d3_mig<-readRDS(paste0(projectFolder,"/g_intermediate/pregnancy_d3/MIG_Pregnancy_D3.rds"))
pregnancy_d3_mig[,pregnancy_start_date:=as.IDate(pregnancy_start_date)][,pregnancy_end_date:=as.IDate(pregnancy_end_date)][,birth_date:=as.IDate(birth_date)][,death_date:=as.IDate(death_date)][,op_start_date_mig:=as.IDate(op_start_date_mig)][,op_end_date_mig:=as.IDate(op_end_date_mig)]
pregnancy_d3_mig[,age:=floor((pregnancy_start_date-birth_date)/365.25)]
pregnancy_d3_mig[,maternal_age:=as.character(lapply(age, age_band_creation))]
pregnancy_d3_mig[,year:=year(pregnancy_start_date)]
pregnancy_d3_mig[,year_group:=as.character(lapply(year, year_group_creation))]
#remove uneccesary variables
if("gdm_pe_filter" %in% names(pregnancy_d3_mig)){pregnancy_d3_mig[,gdm_pe_filter:=NULL]}
if("du_filter" %in% names(pregnancy_d3_mig)){pregnancy_d3_mig[,du_filter:=NULL]}
if("saf_filter" %in% names(pregnancy_d3_mig)){pregnancy_d3_mig[,saf_filter:=NULL]}
if("mig_filter" %in% names(pregnancy_d3_mig)){pregnancy_d3_mig[,mig_filter:=NULL]}
if("op_end_date_gdm_pe" %in% names(pregnancy_d3_mig)){pregnancy_d3_mig[,op_end_date_gdm_pe:=NULL]}
if("op_end_date_du" %in% names(pregnancy_d3_mig)){pregnancy_d3_mig[,op_end_date_du:=NULL]}
if("op_end_date_saf" %in% names(pregnancy_d3_mig)){pregnancy_d3_mig[,op_end_date_saf:=NULL]}
if("op_start_date_gdm_pe" %in% names(pregnancy_d3_mig)){pregnancy_d3_mig[,op_start_date_gdm_pe:=NULL]}
if("op_start_date_du" %in% names(pregnancy_d3_mig)){pregnancy_d3_mig[,op_start_date_du:=NULL]}
if("op_start_date_saf" %in% names(pregnancy_d3_mig)){pregnancy_d3_mig[,op_start_date_saf:=NULL]}
if("sex_at_instance_creation" %in% names(pregnancy_d3_mig)){pregnancy_d3_mig[,sex_at_instance_creation:=NULL]}

if("final_d3" %in% list.files(paste0(projectFolder,"/g_intermediate/migraine_algorithm/"))){
 unlink(paste0(projectFolder,"/g_intermediate/migraine_algorithm/final_d3"), recursive = T)
}
source(paste0(projectFolder,"/p_steps/Step_04_c_migraine_algorithms.R"))

date_running_end_04<-Sys.Date()
end_time_04<-Sys.time()

time_log_04<-data.table(DAP=data_access_provider_name,
                     Script="Step_04_algorithm.R", 
                     Start_date=date_running_start_04, 
                     End_date=date_running_end_04,
                     Time_elaspsed=format(end_time_04-initial_time_04, digits=2))
fwrite(time_log_04,paste0(output_dir,"/Time log/Step_04_time_log.csv"),row.names = F)
rm(time_log_04)

