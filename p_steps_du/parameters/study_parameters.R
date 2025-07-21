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
obs_after<-study_dates[Study=="Drug_utilisation",c("DAP", "after_delivery")]
names(obs_after)<-c("DAP_name", "after")
#Keep only needed information based on the DAP
study_dates<-study_dates[DAP==data_access_provider_name]
if(study_dates[,.N]==0){
  stop("This is not a script issue. There is no data for your data source in study_dates. Fix the issue and then rerun the script.")
}
study_dates[,optional_lookback:=as.numeric(optional_lookback)][,after_delivery:=as.numeric(after_delivery)]
####Select the obs periods for the DAP of interest
obs_after<-as.numeric(obs_after[DAP_name==data_access_provider_name, after])

#source(paste0(pre_dir,"parameters/template_error_check.R"))
#Split the information into medicines or diagnoses codesheet(where the codelists will be used) and not fixed

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
