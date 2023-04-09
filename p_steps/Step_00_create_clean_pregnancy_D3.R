#Run pregnancy algorithm and save results to g_intermediate/pregnancy after clean up

#source(paste0(pre_dir,"Pregnancy algorithm/to_run.R"))

#Load the final pregnancy D3
rm(list=setdiff(ls(), "projectFolder"))
setwd(projectFolder)
source("99_path.R")
setwd(projectFolder)
initial_time<-Sys.time()
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

####keep only green records####
#count number of records that are removed
yellow_rec<-pregnancy_D3[highest_quality=="2_yellow",.N]
blue_rec<-pregnancy_D3[highest_quality=="3_blue",.N]
red_rec<-pregnancy_D3[highest_quality=="4_red",.N]
not_green_rec<-pregnancy_D3[highest_quality!="1_green",.N]

removed_rec<-data.table(Indicator="Pregnancy records", 
                        Quality=c("yellow","blue","red"), 
                        removed_records=c(yellow_rec,blue_rec,red_rec), 
                        percentage=c(round((yellow_rec/pregnancy_D3[,.N])*100,1),
                                     round((blue_rec/pregnancy_D3[,.N])*100,1),
                                     round((red_rec/pregnancy_D3[,.N])*100,1)))
rm(yellow_rec,blue_rec,red_rec)
fwrite(removed_rec,paste0(output_dir, "Pregnancy algorithm/other_quality_records_removed.csv"), row.names = F)

pregnancy_D3<-pregnancy_D3[highest_quality=="1_green"]

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
pregnancy_D3[,diff:=NULL]
incl_rec<-pregnancy_D3[,.N]
Indicator<-c("1.0. Number of original records from the pregnancy algorithm",
             "1.1. Number of records with quality other than green",
             "1.2. Records with sex other than female", 
             "1.3. Records with missing start or end date of pregnancy or both",
             "1.4. Records with missing person id",
             "1.5. Records with missing pregnancy id",
             "1.6. Records where gestational age is longer than 43 weeks(301 days)",
             "1.7. Records left after exclusions")
placeholder<-c(original_rows,not_green_rec,issues_preg_alg_sex,issues_preg_alg_date,issues_preg_alg_pid,issues_preg_alg_prid,issues_preg_alg_ga,incl_rec)
issues<-data.table(Indicator=Indicator, Count=placeholder)
rm(Indicator,placeholder)
rm(issues_preg_alg_sex,not_green_rec,issues_preg_alg_date,issues_preg_alg_pid,issues_preg_alg_prid,issues_preg_alg_ga,incl_rec)

fwrite(issues,paste0(output_dir, "Pregnancy algorithm/issues_flowchart_pregnancy_D3.csv"), row.names = F)
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
fwrite(issues_gdm_pe,paste0(output_dir, "Pregnancy algorithm/issues_GDM_PE_flowchart_pregnancy_D3.csv"), row.names = F)
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
pregnancy_D3[type_of_pregnancy_end=="LB" | type_of_pregnancy_end=="SB", keep_outcome:=1]
other_outcome_mig<-pregnancy_D3[is.na(keep_outcome) & mig_filter==1,.N]
#Update filter_mig based on the type of outcome
pregnancy_D3[is.na(keep_outcome), mig_filter:=NA]
pregnancy_D3[,keep_outcome:=NULL]
included_mig<-pregnancy_D3[mig_filter==1,.N]

issues_mig<-data.table(Indicator=c(paste0("1.0. Records with start date before: ", min_preg_date_mig),
                                      paste0("1.1. Records with end date after: ", max_preg_date_mig),
#                                   "Records with GA> 20 weeks(140 days)",
                                      "1.2. Records with pregnancy outcome other than LB/SB",
                                   "1.3. Records left after removing all issues above"),

                          Migraine=c(before_min_date_mig,
                                       after_max_date_mig,
#                                     "N/A",
                                       other_outcome_mig,
                                     included_mig)
)

rm(before_min_date_mig,after_max_date_mig,other_outcome_mig,included_mig)
fwrite(issues_mig,paste0(output_dir, "Pregnancy algorithm/issues_Migraine_flowchart_pregnancy_D3.csv"), row.names = F)
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
pregnancy_D3[type_of_pregnancy_end=="LB" | type_of_pregnancy_end=="SB", keep_outcome:=1]
other_outcome_du<-pregnancy_D3[is.na(keep_outcome) & du_filter==1,.N]
#Update filter_mig based on the type of outcome
pregnancy_D3[is.na(keep_outcome), du_filter:=NA]
pregnancy_D3[,keep_outcome:=NULL]
included_du<-pregnancy_D3[du_filter==1,.N]


issues_du<-data.table(Indicator=c(paste0("1.0. Records with start date before: ",min_preg_date_du),
                                   paste0("1.1. Records with end date after:", max_preg_date_du),
#                                   "Records with GA> 20 weeks(140 days)",
                                   "1.2. Records with pregnancy outcome other than LB/SB",
                                   "1.3. Records left after removing all issues above"),
                       Drug_utilisation=c(before_min_date_du,
                                  after_max_date_du,
#                                  "N/A",
                                  other_outcome_du,
                                  included_du)
)

rm(before_min_date_du,after_max_date_du,other_outcome_du,included_du)
fwrite(issues_du,paste0(output_dir, "Pregnancy algorithm/issues_DU_flowchart_pregnancy_D3.csv"), row.names = F)
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
pregnancy_D3[type_of_pregnancy_end=="LB" | type_of_pregnancy_end=="SB", keep_outcome:=1]
other_outcome_saf<-pregnancy_D3[is.na(keep_outcome) & saf_filter==1,.N]
#Update filter_mig based on the type of outcome
pregnancy_D3[is.na(keep_outcome), saf_filter:=NA]
pregnancy_D3[,keep_outcome:=NULL]
included_saf<-pregnancy_D3[saf_filter==1,.N]


issues_saf<-data.table(Indicator=c(paste0("1.0. Records with start date before: ",min_preg_date_saf),
                                  paste0("1.1. Records with end date after: ", max_preg_date_saf),
#                                  "Records with GA> 20 weeks(140 days)",
                                  "1.2. Records with pregnancy outcome other than LB/SB",
                                  "1.3. Records left after removing all issues above"),
                      Safety=c(before_min_date_saf,
                                 after_max_date_saf,
#                                 "N/A",
                                 other_outcome_saf,
                                 included_saf)
)

rm(before_min_date_saf,after_max_date_saf,other_outcome_saf,included_saf)
fwrite(issues_saf,paste0(output_dir, "Pregnancy algorithm/issues_Safety_flowchart_pregnancy_D3.csv"), row.names = F)
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

fwrite(summary,paste0(output_dir, "Pregnancy algorithm/included_records_pregnancy_D3.csv"), row.names = F)
rm(summary)

#save pregnancy population to g_intermediate
saveRDS(pregnancy_D3,paste0(g_intermediate,"pregnancy_algorithm/pregnancy_D3.rds"))
rm(pregnancy_D3)

end_time<-Sys.time()
time_log<-data.table(DAP=data_access_provider_name, Script="Step_00_create_clean_pregnancy_D3.R", Date=Sys.Date(), Time_elapsed=format(end_time-initial_time),digits=2)
fwrite(time_log, paste0(output_dir,"Time log/Step_00_time_log.csv"),row.names = F)
