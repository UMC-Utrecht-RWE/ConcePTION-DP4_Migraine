#study parameters

#Load CDM Source table
if(length(list.files(path_dir,"^CDM_SOURCE"))>1){
  print("More than one CDM SOURCE table is present in the working directory. Fix error and run the script again.")
}
CDM_SOURCE<-fread(paste0(path_dir, list.files(path_dir,"^CDM_SOURCE")))

#start study date
start_study_date<-"20050101"
start_study_date<-as.IDate(start_study_date,"%Y%m%d")
#end study date: the min date between date creation and recommended end date
end_study_date<-end_study_date <- min(as.IDate(as.character(CDM_SOURCE[,date_creation]),"%Y%m%d"),
                                      as.IDate(as.character(CDM_SOURCE[,recommended_end_date]),"%Y%m%d"),na.rm = T)
start_study_plus_lookback<-start_study_date-365.25

#min age pregnancy
min_age_preg<-15
max_age_preg<-55