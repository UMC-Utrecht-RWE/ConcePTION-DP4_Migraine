#Run pregnancy algorithm and save results to g_intermediate/pregnancy after clean up

#source(paste0(pre_dir,"Pregnancy algorithm/to_run.R"))

#Load the final pregnancy D3
rm(list=setdiff(ls(), "projectFolder"))
setwd(projectFolder)
source("99_path.R")
setwd(projectFolder)
initial_time_00<-Sys.time()
date_running_start_00<-Sys.Date()
#Load study parameters
source(paste0(projectFolder,"/p_steps/info/DAP_info.R"))
source(paste0(projectFolder,"/p_steps/parameters/study_parameters.R"))

####Load the final pregnancy D3####
preg_file<-list.files(paste0(thisdir,"/g_output/"), "pregnancy_final")

#decluter environment and set up the 99_path again

get(load(paste0(thisdir,"/g_output/", preg_file)))
pregnancy_D3<-D3_pregnancy_final
rm(D3_pregnancy_final)
pregnancy_D3[,pregnancy_start_date:=as.IDate(pregnancy_start_date)]
pregnancy_D3[,pregnancy_end_date:=as.IDate(pregnancy_end_date)]

original_rows<-pregnancy_D3[,.N]

###Get a cross tabulation between quality color, outcomes and type of data
crosstab_rec<-pregnancy_D3[,.N, by=c("highest_quality","type_of_pregnancy_end")]
setnames(crosstab_rec,"N","no_records")
fwrite(crosstab_rec,paste0(output_dir, "Pregnancy algorithm/Step_00_crosstabulation_quality_outcome.csv"), row.names = F)
all_colors_present<-pregnancy_D3[!duplicated(highest_quality),highest_quality]
all_color_present<-paste(all_colors_present,collapse=",")
all_outcomes_present<-pregnancy_D3[!duplicated(type_of_pregnancy_end),type_of_pregnancy_end]
all_outcome_present<-paste(all_outcomes_present,collapse=",")

#####Apply selection script####
source(paste0(projectFolder,"/p_steps/Step_00_a_D3_pregnancy_final_filter.R"))

####cross tab of included records####
#count number of records that are removed
crosstab_rec_inc<-pregnancy_D3[,.N, by=c("highest_quality","type_of_pregnancy_end")]
setnames(crosstab_rec_inc,"N","included_records")
#combine
crosstab<-merge.data.table(crosstab_rec,crosstab_rec_inc, by=c("highest_quality","type_of_pregnancy_end"), all=T)
crosstab[is.na(included_records), included_records:=0]
crosstab[,no_records:=as.numeric(no_records)][,included_records:=as.numeric(included_records)]
crosstab[,removed_records:=no_records-included_records]
crosstab[,removal_percentage:=round((removed_records/no_records)*100,1)]
not_incl_rec<-crosstab[,sum(removed_records)]

fwrite(crosstab,paste0(output_dir, "Pregnancy algorithm/Step_00_other_quality_records_removed.csv"), row.names = F)
rm(crosstab)

####check for issues####
issues_preg_alg_sex<-pregnancy_D3[sex_at_instance_creation!="F",.N]
pregnancy_D3<-pregnancy_D3[sex_at_instance_creation=="F"]
issues_preg_alg_date<-pregnancy_D3[is.na(pregnancy_start_date)|is.na(pregnancy_end_date),.N]
pregnancy_D3<-pregnancy_D3[!is.na(pregnancy_start_date) & !is.na(pregnancy_end_date)]
issues_preg_alg_pid<-pregnancy_D3[is.na(person_id),.N]
pregnancy_D3<-pregnancy_D3[!is.na(person_id)]
issues_preg_alg_prid<-pregnancy_D3[is.na(pregnancy_id),.N]
pregnancy_D3<-pregnancy_D3[!is.na(pregnancy_id)]

#calculate gestational age and exclude all records with gestational age>43 weeks(301 days)
pregnancy_D3[,diff:=pregnancy_end_date- pregnancy_start_date]
issues_preg_alg_ga<-pregnancy_D3[diff>=301,.N]
pregnancy_D3<-pregnancy_D3[diff<301]


#Check for presence of multiple pregnancies
#We eitehr check for the same combination person_id:pregnancy_id:start_date_pregnancy or person_id:start_date_pregnancy
pregnancy_D3[,id_st:=rowid(person_id, pregnancy_start_date)]
issue_1<-pregnancy_D3[id_st>1,.N]
if(issue_1>0){
  pregnancy_D3[,comb:=paste(person_id,pregnancy_start_date,sep="_")]
  pregnancy_D3[duplicated(comb) | duplicated(comb, fromLast = TRUE)][,Filter := diff == max(diff), by = comb]
  #keep only records where FILTER is equal to TRUE
  pregnancy_D3<-pregnancy_D3[Filter==T]
  pregnancy_D3[,Filter:=NULL]
  #Keep not duplicated id(remove twins, triplets etc)
  pregnancy_D3<-pregnancy_D3[!duplicated(comb)]
  pregnancy_D3[,comb:=NULL]
}

incl_rec<-pregnancy_D3[,.N]
Indicator<-c("1.0. Number of original records from the pregnancy algorithm",
             "1.1. Number of records with quality other than needed",
             "1.2. Records with sex other than female", 
             "1.3. Records with missing start or end date of pregnancy or both",
             "1.4. Records with missing person id",
             "1.5. Records with missing pregnancy id",
             "1.6. Records where gestational age is longer than 43 weeks(301 days)",
             "1.7. Records with the same person_id and start_date_pregnancy(keep the longest record, exclude others)",
             "1.8. Records left after exclusions")
placeholder<-c(original_rows,not_incl_rec,issues_preg_alg_sex,issues_preg_alg_date,issues_preg_alg_pid,issues_preg_alg_prid,issues_preg_alg_ga,issue_1, incl_rec)
issues<-data.table(Indicator=Indicator, Count=placeholder)
rm(Indicator,placeholder)
rm(issues_preg_alg_sex,not_incl_rec,issues_preg_alg_date,issues_preg_alg_pid,issues_preg_alg_prid,issues_preg_alg_ga,incl_rec, issue_1)

fwrite(issues,paste0(output_dir, "Pregnancy algorithm/Step_00_issues_flowchart_pregnancy_D3.csv"), row.names = F)
rm(issues)

####Remove all records with date before min preg date####
####GDM and PE####
pregnancy_D3[,filter_1:=as.character(NA)]
pregnancy_D3[,min_preg_date:=min_preg_date_gdm_pe]
pregnancy_D3[,dif:=min_preg_date - pregnancy_start_date]
before_min_date_gdm_pe<-pregnancy_D3[dif>0,.N]
pregnancy_D3[dif<=0,filter_1:=1]
pregnancy_D3[,min_preg_date:=NULL][,dif:=NULL]

pregnancy_D3[,filter_2:=as.character(NA)]
pregnancy_D3[,max_preg_date:=max_preg_date_gdm_pe]
pregnancy_D3[,dif:=max_preg_date - pregnancy_start_date]
after_max_date_gdm_pe<-pregnancy_D3[dif<0 & filter_1==1,.N]
pregnancy_D3[dif>=0 & filter_1==1,filter_2:=1]
pregnancy_D3[,max_preg_date:=NULL][,dif:=NULL]

pregnancy_D3[filter_1==1 & filter_2==1,gdm_pe_filter:=1]
pregnancy_D3[,filter_1:=NULL][,filter_2:=NULL]


#pregnancies with ga>20 weeks(140 days) only
pregnancy_D3[,GA:=pregnancy_end_date - pregnancy_start_date]
pregnancy_D3[GA>140, include_ga:=1]
not_incl_ga<-pregnancy_D3[GA<=140 & gdm_pe_filter==1,.N]
#update gdm_pe_filter
pregnancy_D3[GA<=140, gdm_pe_filter:=NA]
pregnancy_D3[,GA:=NULL]

included_gdm_pe<-pregnancy_D3[gdm_pe_filter==1,.N]

issues_gdm_pe<-data.table(Indicator=c(paste0("1.0. Records with start date before: ",min_preg_date_gdm_pe),
                          paste0("1.1. Records with end date after: ", max_preg_date_gdm_pe),
                          "1.2. Records with GA<20 weeks(140 days)",
#                          "Records with pregnancy outcome other than LB/SB",
                          "1.3. Records left after removing all issues above"),
                          GDM_and_PE=c(before_min_date_gdm_pe,
                                       after_max_date_gdm_pe,
                                       not_incl_ga,
#                                       "N/A",
                                       included_gdm_pe)
)

rm(before_min_date_gdm_pe,after_max_date_gdm_pe,not_incl_ga,included_gdm_pe)
fwrite(issues_gdm_pe,paste0(output_dir, "Pregnancy algorithm/Step_00_issues_GDM_PE_flowchart_pregnancy_D3.csv"), row.names = F)
rm(issues_gdm_pe)
####Migraine####
pregnancy_D3[,filter_1:=as.character(NA)]
pregnancy_D3[,min_preg_date:=min_preg_date_mig]
pregnancy_D3[,dif:=min_preg_date - pregnancy_start_date]
before_min_date_mig<-pregnancy_D3[dif>0,.N]
pregnancy_D3[dif<=0,filter_1:=1]
pregnancy_D3[,min_preg_date:=NULL][,dif:=NULL]

pregnancy_D3[,filter_2:=as.character(NA)]
pregnancy_D3[,max_preg_date:=max_preg_date_mig]
pregnancy_D3[,dif:=max_preg_date - pregnancy_start_date]
after_max_date_mig<-pregnancy_D3[dif<0 & filter_1==1,.N]
pregnancy_D3[dif>=0 & filter_1==1,filter_2:=1]
pregnancy_D3[,max_preg_date:=NULL][,dif:=NULL]

pregnancy_D3[filter_1==1 & filter_2==1,mig_filter:=1]
pregnancy_D3[,filter_1:=NULL][,filter_2:=NULL]

#Calculate number of pregnancy records with outcome LB/SB
if(data_access_provider_name=="NIHW"){
  pregnancy_D3[type_of_pregnancy_end  %in% c("LB", "SB"), keep_outcome:=1]  
}else{
pregnancy_D3[type_of_pregnancy_end %in% c("LB", "SB", "SA", "T"), keep_outcome:=1]
}
other_outcome_mig<-pregnancy_D3[is.na(keep_outcome) & mig_filter==1,.N]
if(other_outcome_mig>0){
type_other_outcome<-paste(unique(pregnancy_D3[is.na(keep_outcome) & mig_filter==1,type_of_pregnancy_end]), collapse = ",")
}else{
  type_other_outcome<-"N/A" 
}
#Update filter_mig based on the type of outcome
pregnancy_D3[is.na(keep_outcome), mig_filter:=NA]
pregnancy_D3[,keep_outcome:=NULL]
included_mig<-pregnancy_D3[mig_filter==1,.N]

issues_mig<-data.table(Indicator=c(paste0("1.0. Records with start date before: ", min_preg_date_mig),
                                      paste0("1.1. Records with end date after: ", max_preg_date_mig),
#                                   "Records with GA> 20 weeks(140 days)",
                                      paste0("1.2. Records with pregnancy outcome other than LB/SB/SA/T (",type_other_outcome,")"),
                                   "1.3. Records left after removing all issues above"),

                          Migraine=c(before_min_date_mig,
                                       after_max_date_mig,
#                                     "N/A",
                                       other_outcome_mig,
                                     included_mig)
)

rm(before_min_date_mig,after_max_date_mig,other_outcome_mig,included_mig,type_other_outcome)
fwrite(issues_mig,paste0(output_dir, "Pregnancy algorithm/Step_00_issues_Migraine_flowchart_pregnancy_D3.csv"), row.names = F)
rm(issues_mig)
####Drug utilisation####
pregnancy_D3[,filter_1:=as.character(NA)]
pregnancy_D3[,min_preg_date:=min_preg_date_du]
pregnancy_D3[,dif:=min_preg_date - pregnancy_start_date]
before_min_date_du<-pregnancy_D3[dif>0,.N]
pregnancy_D3[dif<=0,filter_1:=1]
pregnancy_D3[,min_preg_date:=NULL][,dif:=NULL]

pregnancy_D3[,filter_2:=as.character(NA)]
pregnancy_D3[,max_preg_date:=max_preg_date_du]
pregnancy_D3[,dif:=max_preg_date - pregnancy_start_date]
after_max_date_du<-pregnancy_D3[dif<0 & filter_1==1,.N]
pregnancy_D3[dif>=0 & filter_1==1,filter_2:=1]
pregnancy_D3[,max_preg_date:=NULL][,dif:=NULL]

pregnancy_D3[filter_1==1 & filter_2==1,du_filter:=1]
pregnancy_D3[,filter_1:=NULL][,filter_2:=NULL]

#Calculate number of pregnancy records with outcome LB/SB
if(data_access_provider_name=="NIHW"){
  pregnancy_D3[type_of_pregnancy_end  %in% c("LB", "SB"), keep_outcome:=1]  
}else{
pregnancy_D3[type_of_pregnancy_end  %in% c("LB", "SB", "SA", "T"), keep_outcome:=1]
}
other_outcome_du<-pregnancy_D3[is.na(keep_outcome) & du_filter==1,.N]
if(other_outcome_du>0){
  type_other_outcome<-paste(unique(pregnancy_D3[is.na(keep_outcome) & du_filter==1,type_of_pregnancy_end]), collapse = ",")
}else{
  type_other_outcome<-"N/A" 
}

#Update filter_mig based on the type of outcome
pregnancy_D3[is.na(keep_outcome), du_filter:=NA]
pregnancy_D3[,keep_outcome:=NULL]
included_du<-pregnancy_D3[du_filter==1,.N]


issues_du<-data.table(Indicator=c(paste0("1.0. Records with start date before: ",min_preg_date_du),
                                   paste0("1.1. Records with end date after:", max_preg_date_du),
#                                   "Records with GA> 20 weeks(140 days)",
paste0("1.2. Records with pregnancy outcome other than LB/SB/SA/T (",type_other_outcome,")"),
                                   "1.3. Records left after removing all issues above"),
                       Drug_utilisation=c(before_min_date_du,
                                  after_max_date_du,
#                                  "N/A",
                                  other_outcome_du,
                                  included_du)
)

rm(before_min_date_du,after_max_date_du,other_outcome_du,included_du,type_other_outcome)
fwrite(issues_du,paste0(output_dir, "Pregnancy algorithm/Step_00_issues_DU_flowchart_pregnancy_D3.csv"), row.names = F)
rm(issues_du)
####Safety####
pregnancy_D3[,filter_1:=as.character(NA)]
pregnancy_D3[,min_preg_date:=min_preg_date_saf]
pregnancy_D3[,dif:=min_preg_date - pregnancy_start_date]
before_min_date_saf<-pregnancy_D3[dif>0,.N]
pregnancy_D3[dif<=0,filter_1:=1]
pregnancy_D3[,min_preg_date:=NULL][,dif:=NULL]

pregnancy_D3[,filter_2:=as.character(NA)]
pregnancy_D3[,max_preg_date:=max_preg_date_saf]
pregnancy_D3[,dif:=max_preg_date - pregnancy_start_date]
after_max_date_saf<-pregnancy_D3[dif<0 & filter_1==1,.N]
pregnancy_D3[dif>=0 & filter_1==1,filter_2:=1]
pregnancy_D3[,max_preg_date:=NULL][,dif:=NULL]

pregnancy_D3[filter_1==1 & filter_2==1,saf_filter:=1]
pregnancy_D3[,filter_1:=NULL][,filter_2:=NULL]

#Calculate number of pregnancy records with outcome LB/SB
if(data_access_provider_name=="NIHW"){
  pregnancy_D3[type_of_pregnancy_end  %in% c("LB", "SB", "T"), keep_outcome:=1]  
}else{
pregnancy_D3[type_of_pregnancy_end  %in% c("LB", "SB", "SA", "T"), keep_outcome:=1]
}
other_outcome_saf<-pregnancy_D3[is.na(keep_outcome) & saf_filter==1,.N]
if(other_outcome_saf>0){
  type_other_outcome<-paste(unique(pregnancy_D3[is.na(keep_outcome) & saf_filter==1,type_of_pregnancy_end]), collapse = ",")
}else{
  type_other_outcome<-"N/A" 
}

#Update filter_mig based on the type of outcome
pregnancy_D3[is.na(keep_outcome), saf_filter:=NA]
pregnancy_D3[,keep_outcome:=NULL]
included_saf<-pregnancy_D3[saf_filter==1,.N]


issues_saf<-data.table(Indicator=c(paste0("1.0. Records with start date before: ",min_preg_date_saf),
                                  paste0("1.1. Records with end date after: ", max_preg_date_saf),
#                                  "Records with GA> 20 weeks(140 days)",
paste0("1.2. Records with pregnancy outcome other than LB/SB/SA/T (",type_other_outcome,")"),
                                  "1.3. Records left after removing all issues above"),
                      Safety=c(before_min_date_saf,
                                 after_max_date_saf,
#                                 "N/A",
                                 other_outcome_saf,
                                 included_saf)
)

rm(before_min_date_saf,after_max_date_saf,other_outcome_saf,included_saf,type_other_outcome)
fwrite(issues_saf,paste0(output_dir, "Pregnancy algorithm/Step_00_issues_Safety_flowchart_pregnancy_D3.csv"), row.names = F)
rm(issues_saf)

####remove uneccessary columns####
pregnancy_D3<-pregnancy_D3[,c("person_id","pregnancy_id","pregnancy_start_date","pregnancy_end_date","gdm_pe_filter","mig_filter","du_filter","saf_filter")]

####Create summary of included records by year of start pregnancy####
pregnancy_D3[,year:=year(pregnancy_start_date)]
summary_gdm<-as.data.table(pregnancy_D3[gdm_pe_filter==1,.N,by="year"])
setnames(summary_gdm,"N","GDM_and_PE")
summary_mig<-as.data.table(pregnancy_D3[mig_filter==1,.N,by="year"])
setnames(summary_mig,"N","Migraine")
summary_du<-as.data.table(pregnancy_D3[du_filter==1,.N,by="year"])
setnames(summary_du,"N","Drug_utilisation")
summary_saf<-as.data.table(pregnancy_D3[saf_filter==1,.N,by="year"])
setnames(summary_saf,"N","Safety")
summary<-merge.data.table(summary_gdm,summary_mig,by="year",all=T)
rm(summary_gdm,summary_mig)
summary<-merge.data.table(summary,summary_du,by="year",all=T)
rm(summary_du)
summary<-merge.data.table(summary,summary_saf,by="year",all=T)
rm(summary_saf)
summary[is.na(GDM_and_PE),GDM_and_PE:=0]
summary[is.na(Migraine),Migraine:=0]
summary[is.na(Drug_utilisation),Drug_utilisation:=0]
summary[is.na(Safety),Safety:=0]

fwrite(summary,paste0(output_dir, "Pregnancy algorithm/Step_00_included_records_pregnancy_D3.csv"), row.names = F)
rm(summary)

#EMA Update 9/7/2024 and 15/7/2024 step 1 to follow Hedvig requests

# 6 MONTHS FOLLOW UP DATE (180 days? )

pregnancy_D3$follow_up_180_days<- (pregnancy_D3$pregnancy_end_date+180)

# age categories (15-24, 25-34, 35-49)
# Update calendar year
#categories (2009-2012, 2013-2016, 2017-2020)

pregnancy_D3$age_category<-NA

pregnancy_D3$age_category[pregnancy_D3$age_at_start_of_pregnancy>=15 & pregnancy_D3$age_at_start_of_pregnancy<25]<-1
pregnancy_D3$age_category[pregnancy_D3$age_at_start_of_pregnancy>=25 & pregnancy_D3$age_at_start_of_pregnancy<35]<-2
pregnancy_D3$age_category[pregnancy_D3$age_at_start_of_pregnancy>=35 & pregnancy_D3$age_at_start_of_pregnancy<50]<-3

pregnancy_D3$year<- as.numeric(format(pregnancy_D3$pregnancy_start_date,'%Y'))
pregnancy_D3$year_category<-NA

pregnancy_D3$year_category[pregnancy_D3$year>=2009 & pregnancy_D3$year<2013]<-1
pregnancy_D3$year_category[pregnancy_D3$year>=2013 & pregnancy_D3$year<2017]<-2
pregnancy_D3$year_category[pregnancy_D3$year>=2017 & pregnancy_D3$year<2021]<-3

#ADD TRIMESTERS

days_trim<-97

pregnancy_D3$trim_1_start<- pregnancy_D3$pregnancy_start_date
pregnancy_D3$trim_1_end<- pregnancy_D3$pregnancy_start_date+97

pregnancy_D3$trim_2_start<-(pregnancy_D3$trim_1_end)+1
pregnancy_D3$trim_2_end<- pregnancy_D3$trim_2_start+97

pregnancy_D3$trim_3_start<-pregnancy_D3$trim_2_end+1
pregnancy_D3$trim_3_end<- pregnancy_D3$pregnancy_end_date

pregnancy_D3$trim_1_end[pregnancy_D3$trim_1_end>=pregnancy_D3$pregnancy_end_date]<-pregnancy_D3$pregnancy_end_date[pregnancy_D3$trim_1_end>=pregnancy_D3$pregnancy_end_date]

pregnancy_D3$trim_2_start[pregnancy_D3$trim_1_end==pregnancy_D3$pregnancy_end_date]<-0
pregnancy_D3$trim_2_end[pregnancy_D3$trim_2_start==0]<-0
pregnancy_D3$trim_2_end[pregnancy_D3$trim_2_end>=pregnancy_D3$pregnancy_end_date]<-pregnancy_D3$pregnancy_end_date[pregnancy_D3$trim_2_end>=pregnancy_D3$pregnancy_end_date]

pregnancy_D3$trim_3_start[pregnancy_D3$trim_2_end==pregnancy_D3$pregnancy_end_date]<-0
pregnancy_D3$trim_3_start[pregnancy_D3$trim_2_end==0]<-0
pregnancy_D3$trim_3_end[pregnancy_D3$trim_3_start==0]<-0

pregnancy_D3$trim_2_start[pregnancy_D3$trim_2_start==0]<-NA
pregnancy_D3$trim_2_end[pregnancy_D3$trim_2_end==0]<-NA
pregnancy_D3$trim_3_start[pregnancy_D3$trim_3_start==0]<-NA
pregnancy_D3$trim_3_end[pregnancy_D3$trim_3_end==0]<-NA

#save pregnancy population to g_intermediate
saveRDS(pregnancy_D3,paste0(g_intermediate,"pregnancy_algorithm/pregnancy_D3.rds"))
rm(pregnancy_D3)

date_running_end_00<-Sys.Date()
end_time_00<-Sys.time()

time_log<-data.table(DAP=data_access_provider_name,
                     Script="Step_00_create_clean_pregnancy_D3.R", 
                     Start_date=date_running_start_00, 
                     End_date=date_running_end_00,
                     Time_elaspsed=format(end_time_00-initial_time_00, digits=2))
fwrite(time_log,paste0(output_dir,"/Time log/Step_00_time_log.csv"),row.names = F)
rm(time_log)


