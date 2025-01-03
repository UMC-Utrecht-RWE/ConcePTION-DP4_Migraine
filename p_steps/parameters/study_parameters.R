#study parameters

####Load CDM Source table####
if(length(list.files(path_dir,"^CDM_SOURCE"))>1){
  stop("More than one CDM SOURCE table is present in the working directory. Fix error and run the script again.")
}
CDM_SOURCE<-fread(paste0(path_dir, list.files(path_dir,"^CDM_SOURCE")))

#####Load study dates template####
study_dates_file<-list.files(paste0(projectFolder,"/p_steps/parameters/"), "study_dates")
study_dates<-read_excel(paste0(projectFolder,"/p_steps/parameters/",study_dates_file),col_types = "text")
study_dates<-as.data.table(study_dates)
#Keep only needed information based on the DAP
study_dates<-study_dates[DAP==data_access_provider_name]
if(study_dates[,.N]==0){
  stop("This is not a script issue. There is no data for your data source in study_dates. Fix the issue and then rerun the script.")
}
study_dates[,optional_lookback:=as.numeric(optional_lookback)][,after_delivery:=as.numeric(after_delivery)]
#source(paste0(pre_dir,"parameters/template_error_check.R"))
#Split the information into medicines or diagnoses codesheet(where the codelists will be used) and not fixed

####Start dates end End dates for all projects####
####GDM & PE####
if(study_dates[Study=="GDM_and_PE",.N]>0){
#start study date
gdm_pe_start_study_date<-as.IDate(study_dates[Study=="GDM_and_PE",start_coverage],"%Y%m%d")
#Will be used in observation periods
#end study date: the min date between date creation and recommended end date or end study date
gdm_pe_end_study_date<-min(as.IDate(as.character(CDM_SOURCE[,date_creation]),"%Y%m%d"),
                           as.IDate(as.character(CDM_SOURCE[,recommended_end_date]),"%Y%m%d"),
                           as.IDate(study_dates[Study=="GDM_and_PE",end_coverage],"%Y%m%d"), na.rm = T)

#min_preg_date_gdm_pe
min_preg_date_gdm_pe<-as.IDate(as.character(study_dates[Study=="GDM_and_PE",min_preg_date]),"%Y%m%d")
#max_preg_date_gdm_pe
max_preg_date_gdm_pe<-as.IDate(as.character(study_dates[Study=="GDM_and_PE",max_preg_date]),"%Y%m%d")

#optional_lookback: will be used for diagnoses, medication and other data related to the event
opt_lookback_gdm_pe<-study_dates[Study=="GDM_and_PE",optional_lookback]
after_delivery_gdm_pe<-study_dates[Study=="GDM_and_PE",after_delivery]
}

####Migraine####
if(study_dates[Study=="Migraine" & DAP==data_access_provider_name,.N]>0){
#start study date
mig_start_study_date<-unique(as.IDate(study_dates[Study=="Migraine",start_coverage],"%Y%m%d"))
#end study date: the min date between date creation and recommended end date or end study date
mig_end_study_date<-min(as.IDate(as.character(CDM_SOURCE[,date_creation]),"%Y%m%d"),
                           as.IDate(as.character(CDM_SOURCE[,recommended_end_date]),"%Y%m%d"),
                           as.IDate(study_dates[Study=="Migraine",end_coverage],"%Y%m%d"), na.rm = T)
#min_preg_date_mig
min_preg_date_mig<-unique(as.IDate(study_dates[Study=="Migraine",min_preg_date],"%Y%m%d"))
#max_preg_date_gdm_pe
max_preg_date_mig<-as.IDate(as.character(study_dates[Study=="Migraine",max_preg_date]),"%Y%m%d")

#optional_lookback: will be used for pregnancy start date
opt_lookback_migraine<-unique(study_dates[Study=="Migraine",optional_lookback])
after_delivery_migraine<-study_dates[Study=="Migraine",after_delivery]
}
####Drug utilisation####
if(study_dates[Study=="Drug_utilisation" & DAP==data_access_provider_name,.N]>0){
#start study date
du_start_study_date<-as.IDate(study_dates[Study=="Drug_utilisation",start_coverage],"%Y%m%d")
#Will be used in observation periods
#end study date: the min date between date creation and recommended end date or end study date
du_end_study_date<-min(as.IDate(as.character(CDM_SOURCE[,date_creation]),"%Y%m%d"),
                           as.IDate(as.character(CDM_SOURCE[,recommended_end_date]),"%Y%m%d"),
                           as.IDate(study_dates[Study=="Drug_utilisation",end_coverage],"%Y%m%d"), na.rm = T)

#min_preg_date_du
min_preg_date_du<-as.IDate(study_dates[Study=="Drug_utilisation",min_preg_date],"%Y%m%d")
#max_preg_date_du
max_preg_date_du<-as.IDate(study_dates[Study=="Drug_utilisation",max_preg_date],"%Y%m%d")

opt_lookback_du<-unique(study_dates[Study=="Drug_utilisation",optional_lookback])
after_delivery_du<-study_dates[Study=="Drug_utilisation",after_delivery]
}
####Safety####
if(study_dates[Study=="Safety" & DAP==data_access_provider_name,.N]>0){
#start study date
saf_start_study_date<-as.IDate(study_dates[Study=="Safety",start_coverage],"%Y%m%d")
#Will be used in observation periods
#end study date: the min date between date creation and recommended end date or end study date
saf_end_study_date<-min(as.IDate(as.character(CDM_SOURCE[,date_creation]),"%Y%m%d"),
                           as.IDate(as.character(CDM_SOURCE[,recommended_end_date]),"%Y%m%d"),
                           as.IDate(study_dates[Study=="Safety",end_coverage],"%Y%m%d"), na.rm = T)

#min_preg_date_saf
min_preg_date_saf<-as.IDate(study_dates[Study=="Safety",min_preg_date],"%Y%m%d")
#max_preg_date_saf
max_preg_date_saf<-as.IDate(study_dates[Study=="Safety",max_preg_date],"%Y%m%d")

#optional_lookback: will be used for diagnoses, medication and other data related to the event
opt_lookback_saf<-unique(study_dates[Study=="Safety",optional_lookback])
after_delivery_saf<-study_dates[Study=="Safety",after_delivery]
}
####Age pregnancy at start pregnancy####
#min age pregnancy
min_age_preg<-15
max_age_preg<-49
