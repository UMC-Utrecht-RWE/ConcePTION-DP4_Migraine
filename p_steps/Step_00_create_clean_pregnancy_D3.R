#Run pregnancy algorithm and save results to g_intermediate/pregnancy after clean up

source(paste0(pre_dir,"Pregnancy algorithm/to_run.R"))

#Load the final pregnancy D3
rm(list=setdiff(ls(), "projectFolder"))
setwd(projectFolder)
source("99_path.R")
setwd(projectFolder)

preg_file<-list.files(paste0(thisdir,"/g_output/"), "pregnancy_final")

#decluter environment and set up the 99_path again

pregnancy_D3<-get(load(paste0(thisdir,"/g_output/", preg_file)))
pregnancy_D3[,pregnancy_start_date:=as.IDate(pregnancy_start_date)]
pregnancy_D3[,pregnancy_end_date:=as.IDate(pregnancy_end_date)]

original_rows<-pregnancy_D3[,.N]
#check for issues
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

#Remove all records with date before min preg date
#Load CDM Source table
if(length(list.files(path_dir,"^CDM_SOURCE"))>1){
  stop("More than one CDM SOURCE table is present in the working directory. Fix error and run the script again.")
}
CDM_SOURCE<-fread(paste0(path_dir, list.files(path_dir,"^CDM_SOURCE")))


if(CDM_SOURCE[,data_access_provider_name] %in% c("FISABIO")){
  min_preg_date<-as.IDate("20100401","%Y%m%d")
}else{
  min_preg_date<-as.IDate("20050401","%Y%m%d")
}
pregnancy_D3[,min_preg_date:=min_preg_date]
pregnancy_D3[,dif:=min_preg_date - pregnancy_start_date]
before_min_date<-pregnancy_D3[dif>0,.N]
pregnancy_D3<-pregnancy_D3[dif<=0]
pregnancy_D3[,min_preg_date:=NULL][,dif:=NULL]

if(CDM_SOURCE[,data_access_provider_name] %in% c("THL", "GePaRD")){
  max_preg_date<-as.IDate("20180301","%Y%m%d")
}else{
  max_preg_date<-as.IDate("20190301","%Y%m%d")
}

pregnancy_D3[,max_preg_date:=max_preg_date]
pregnancy_D3[,dif:=max_preg_date - pregnancy_start_date]
after_max_date<-pregnancy_D3[dif<0,.N]
pregnancy_D3<-pregnancy_D3[dif>=0]
pregnancy_D3[,max_preg_date:=NULL][,dif:=NULL]

issues<-data.table(Indicator=c("Number of original records from the pregnancy algorithm",
                               "Records with sex other than female", 
                               "Records with missing start or end date of pregnancy or both",
                               "Records with missing person id",
                               "Records with missing pregnancy id",
                               "Records where gestational age loger than 43 weeks(301 days)",
                               paste0("Records with start date before: ", min_preg_date),
                               paste0("Records with end date after: ", max_preg_date),
                               "Records left after removing all issues above"), 
                               no_records=c(original_rows,
                                            issues_preg_alg_sex,
                                            issues_preg_alg_date,
                                            issues_preg_alg_pid,
                                            issues_preg_alg_prid,issues_preg_alg_ga,
                                            before_min_date,
                                            after_max_date,
                                            pregnancy_D3[,.N]))
rm(issues_preg_alg_sex,issues_preg_alg_date,issues_preg_alg_pid,issues_preg_alg_prid,issues_preg_alg_ga)

if(issues[no_records>0,.N]>0){
fwrite(issues,paste0(output_dir, "Pregnancy algorithm/issues_pregnancies_D3.csv"), row.names = F)
}

#keep only green records
#count number of records that are removed
yellow_rec<-pregnancy_D3[highest_quality=="2_yellow",.N]
blue_rec<-pregnancy_D3[highest_quality=="3_blue",.N]
red_rec<-pregnancy_D3[highest_quality=="4_red",.N]

removed_rec<-data.table(Indicator="Pregnancy records", 
                        Quality=c("yellow","blue","red"), 
                        removed_records=c(yellow_rec,blue_rec,red_rec), 
                        percentage=c(round((yellow_rec/pregnancy_D3[,.N])*100,1),
                                     round((blue_rec/pregnancy_D3[,.N])*100,1),
                                     round((red_rec/pregnancy_D3[,.N])*100,1)))
rm(yellow_rec,blue_rec,red_rec)
fwrite(removed_rec,paste0(output_dir, "Pregnancy algorithm/other_quality_records_removed.csv"), row.names = F)

pregnancy_D3<-pregnancy_D3[highest_quality=="1_green"]

#remove uneccessary columns
pregnancy_D3<-pregnancy_D3[,c("person_id","pregnancy_id","age_at_start_of_pregnancy","pregnancy_start_date","pregnancy_end_date","gestage_at_first_record","gestage_greater_44")]
#save pregnancy population to g_intermediate
saveRDS(pregnancy_D3,paste0(g_intermediate,"pregnancy_algorithm/pregnancy_D3.rds"))
