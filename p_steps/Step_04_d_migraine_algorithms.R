initial_time_04_d<-Sys.time()
date_running_start_04_d<-Sys.Date()

####APPLY Migraine ALGORITHM####
algorithm_template<-fread(paste0(projectFolder, "/p_steps/parameters/algorithms.csv"))
#### MIG_Dx_a:Prevalence of migraine at baseline when lookback==12 months(3 months) ####
print("Create algorithm MIG_Dx_a")
MIG_Dx_a<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_Dx_a"]
inc_col<-MIG_Dx_a[TYPE=="AND",STUDY_VARIABLES]

if(length(inc_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col],include:=1]}else{pregnancy_d3_mig[,include:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm/final_d3/MIG_Dx_a_D3.rds"))

#export MIG_Dx_a
MIG_Dx_a_dt<-data.table(algorithm="MIG_Dx_a", no_diagnosed_pregnancies=pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id),.N])
MIG_Dx_a_dt[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_Dx_a_dt[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
MIG_Dx_a_dt[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_a_dt[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_a_dt[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_Dx_a_dt,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Dx_a.csv"),row.names = F)

#EMA 23/7 add age bands and replace maternal age

age_band_creation<-function(x){
  y<-vector()
  if(x>=15 & x<=24){y<-"15-24"}
  if(x>=25 & x<=34){y<-"25-34"}
  if(x>=35){y<-"35+"}
  return(y)
}

pregnancy_d3_mig$age_band<-as.character(lapply(pregnancy_d3_mig$maternal_age, age_band_creation))

records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id), .N, by=.(age_band)]
names(records)<-c("age_band","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id), by="age_band",.N]
names(total)<-c("age_band","no_pregnancies")
records<-merge.data.table(records,total,by="age_band",all=T)
MIG_Dx_a_1<-data.table(algorithm="MIG_Dx_a", records)
MIG_Dx_a_1[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_Dx_a_1<-MIG_Dx_a_1[order(age_band)]
rm(records,total)
MIG_Dx_a_1[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_Dx_a_1[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_Dx_a_1[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_Dx_a_1[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_a_1[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_a_1[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_Dx_a_1,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Dx_a_age.csv"),row.names = F)
rm(MIG_Dx_a_1)

#by year to year group
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by="year_group", .N]
names(records)<-c("year_group","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by="year_group",.N]
names(total)<-c("year_group","no_pregnancies")
records<-merge.data.table(records,total,by="year_group",all=T)
MIG_Dx_a_2<-data.table(algorithm="MIG_Dx_a", records)
MIG_Dx_a_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_Dx_a_2<-MIG_Dx_a_2[order(year_group)]
rm(records,total)
MIG_Dx_a_2[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_Dx_a_2[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_Dx_a_2[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_Dx_a_2[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_a_2[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_a_2[lower_95_CI<0,lower_95_CI:=0]
fwrite(MIG_Dx_a_2,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Dx_a_year.csv"),row.names = F)
rm(MIG_Dx_a_2)

#by year group and maternal age_band
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by=c("year_group","age_band"), .N]
names(records)<-c("year_group","age_band", "no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by=c("year_group","age_band"),.N]
names(total)<-c("year_group","age_band","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","age_band"), all=T)
MIG_Dx_a_3<-data.table(algorithm="MIG_Dx_a", records)
MIG_Dx_a_3[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_Dx_a_3<-MIG_Dx_a_3[order(year_group,age_band)]
rm(records,total)
MIG_Dx_a_3[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_Dx_a_3[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_Dx_a_3[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_Dx_a_3[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_a_3[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_a_3[lower_95_CI<0,lower_95_CI:=0]
pregnancy_d3_mig[,include:=NULL]
rm(MIG_Dx_a)
fwrite(MIG_Dx_a_3,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Dx_a_year_age.csv"),row.names = F)
rm(MIG_Dx_a_3)

#### MIG_Dx_b:Prevalence of migraine at baseline when lookback==3 months ####
print("Create algorithm MIG_Dx_b")
MIG_Dx_b<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_Dx_b"]
inc_col<-MIG_Dx_b[TYPE=="AND",STUDY_VARIABLES]

if(length(inc_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col],include:=1]}else{pregnancy_d3_mig[,include:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm/final_d3/MIG_Dx_b_D3.rds"))


#export MIG_Dx_b
MIG_Dx_b_dt<-data.table(algorithm="MIG_Dx_b", no_diagnosed_pregnancies=pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id),.N])
MIG_Dx_b_dt[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_Dx_b_dt[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
MIG_Dx_b_dt[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_b_dt[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_b_dt[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_Dx_b_dt,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Dx_b.csv"),row.names = F)

#by maternal age
pregnancy_d3_mig[,age_band:=as.character(age_band)]
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id), .N, by=.(age_band)]
names(records)<-c("age_band","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id), by="age_band",.N]
names(total)<-c("age_band","no_pregnancies")
records<-merge.data.table(records,total,by="age_band",all=T)
MIG_Dx_b_1<-data.table(algorithm="MIG_Dx_b", records)
MIG_Dx_b_1[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_Dx_b_1<-MIG_Dx_b_1[order(age_band)]
rm(records,total)
MIG_Dx_b_1[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_Dx_b_1[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_Dx_b_1[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_Dx_b_1[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_b_1[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_b_1[lower_95_CI<0,lower_95_CI:=0]
fwrite(MIG_Dx_b_1,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Dx_b_age.csv"),row.names = F)
rm(MIG_Dx_b_1)


#by year to year group
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by="year_group", .N]
names(records)<-c("year_group","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by="year_group",.N]
names(total)<-c("year_group","no_pregnancies")
records<-merge.data.table(records,total,by="year_group",all=T)
MIG_Dx_b_2<-data.table(algorithm="MIG_Dx_b", records)
MIG_Dx_b_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_Dx_b_2<-MIG_Dx_b_2[order(year_group)]
rm(records,total)
MIG_Dx_b_2[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_Dx_b_2[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_Dx_b_2[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_Dx_b_2[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_b_2[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_b_2[lower_95_CI<0,lower_95_CI:=0]
fwrite(MIG_Dx_b_2,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Dx_b_year.csv"),row.names = F)
rm(MIG_Dx_b_2)


#by year group and maternal age
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by=c("year_group","age_band"), .N]
names(records)<-c("year_group","age_band", "no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by=c("year_group","age_band"),.N]
names(total)<-c("year_group","age_band","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","age_band"), all=T)
MIG_Dx_b_3<-data.table(algorithm="MIG_Dx_b", records)
MIG_Dx_b_3[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_Dx_b_3<-MIG_Dx_b_3[order(year_group,age_band)]
rm(records,total)
MIG_Dx_b_3[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_Dx_b_3[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_Dx_b_3[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_Dx_b_3[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_b_3[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_b_3[lower_95_CI<0,lower_95_CI:=0]
pregnancy_d3_mig[,include:=NULL]
fwrite(MIG_Dx_b_3,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Dx_b_year_age.csv"),row.names = F)
rm(MIG_Dx_b_3)


#### MIG_Rx_a:Prevalence of migraine at baseline when lookback==12 months(3 months) ####
print("Create algorithm MIG_Rx_a")
MIG_Rx_a<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_Rx_a"]
inc_col<-MIG_Rx_a[TYPE=="AND",STUDY_VARIABLES]

if(length(inc_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col],include:=1]}else{pregnancy_d3_mig[,include:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm/final_d3/MIG_Rx_a_D3.rds"))

#export MIG_Rx_a
MIG_Rx_a_dt<-data.table(algorithm="MIG_Rx_a", no_diagnosed_pregnancies=pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id),.N])
MIG_Rx_a_dt[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_Rx_a_dt[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
MIG_Rx_a_dt[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_a_dt[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_a_dt[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_Rx_a_dt,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Rx_a.csv"),row.names = F)

#by maternal age_band
pregnancy_d3_mig[,age_band:=as.character(age_band)]
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id), .N, by=.(age_band)]
names(records)<-c("age_band","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id), by="age_band",.N]
names(total)<-c("age_band","no_pregnancies")
records<-merge.data.table(records,total,by="age_band",all=T)
MIG_Rx_a_1<-data.table(algorithm="MIG_Rx_a", records)
MIG_Rx_a_1[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_Rx_a_1<-MIG_Rx_a_1[order(age_band)]
rm(records,total)
MIG_Rx_a_1[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_Rx_a_1[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_Rx_a_1[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_Rx_a_1[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_a_1[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_a_1[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_Rx_a_1,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Rx_a_age.csv"),row.names = F)
rm(MIG_Rx_a_1)


#by year to year_group
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by="year_group", .N]
names(records)<-c("year_group","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by="year_group",.N]
names(total)<-c("year_group","no_pregnancies")
records<-merge.data.table(records,total,by="year_group",all=T)
MIG_Rx_a_2<-data.table(algorithm="MIG_Rx_a", records)
MIG_Rx_a_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_Rx_a_2<-MIG_Rx_a_2[order(year_group)]
rm(records,total)
MIG_Rx_a_2[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_Rx_a_2[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_Rx_a_2[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_Rx_a_2[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_a_2[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_a_2[lower_95_CI<0,lower_95_CI:=0]
fwrite(MIG_Rx_a_2,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Rx_a_year.csv"),row.names = F)
rm(MIG_Rx_a_2)


#by year group and maternal age_band
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by=c("year_group","age_band"), .N]
names(records)<-c("year_group","age_band", "no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by=c("year_group","age_band"),.N]
names(total)<-c("year_group","age_band","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","age_band"), all=T)
MIG_Rx_a_3<-data.table(algorithm="MIG_Rx_a", records)
MIG_Rx_a_3[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_Rx_a_3<-MIG_Rx_a_3[order(year_group,age_band)]
rm(records,total)
MIG_Rx_a_3[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_Rx_a_3[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_Rx_a_3[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_Rx_a_3[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_a_3[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_a_3[lower_95_CI<0,lower_95_CI:=0]

pregnancy_d3_mig[,include:=NULL]
fwrite(MIG_Rx_a_3,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Rx_a_year_age.csv"),row.names = F)
rm(MIG_Rx_a_3)

#### MIG_Rx_b:Prevalence of migraine at baseline when lookback==3 months ####
print("Create algorithm MIG_Rx_b")
MIG_Rx_b<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_Rx_b"]
inc_col<-MIG_Rx_b[TYPE=="AND",STUDY_VARIABLES]

if(length(inc_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col],include:=1]}else{pregnancy_d3_mig[,include:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm/final_d3/MIG_Rx_b_D3.rds"))

#export MIG_Dx_a
MIG_Rx_b_dt<-data.table(algorithm="MIG_Rx_b", no_diagnosed_pregnancies=pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id),.N])
MIG_Rx_b_dt[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_Rx_b_dt[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
MIG_Rx_b_dt[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_b_dt[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_b_dt[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_Rx_b_dt,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Rx_b.csv"),row.names = F)

#by maternal age
pregnancy_d3_mig[,age_band:=as.character(age_band)]
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id), .N, by=.(age_band)]
names(records)<-c("age_band","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id), by="age_band",.N]
names(total)<-c("age_band","no_pregnancies")
records<-merge.data.table(records,total,by="age_band",all=T)
MIG_Rx_b_1<-data.table(algorithm="MIG_Rx_b", records)
MIG_Rx_b_1[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_Rx_b_1<-MIG_Rx_b_1[order(age_band)]
rm(records,total)
MIG_Rx_b_1[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_Rx_b_1[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_Rx_b_1[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_Rx_b_1[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_b_1[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_b_1[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_Rx_b_1,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Rx_b_age.csv"),row.names = F)
rm(MIG_Rx_b_1)


#by year to year_group
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by="year_group", .N]
names(records)<-c("year_group","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by="year_group",.N]
names(total)<-c("year_group","no_pregnancies")
records<-merge.data.table(records,total,by="year_group",all=T)
MIG_Rx_b_2<-data.table(algorithm="MIG_Rx_b", records)
MIG_Rx_b_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_Rx_b_2<-MIG_Rx_b_2[order(year_group)]
rm(records,total)
MIG_Rx_b_2[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_Rx_b_2[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_Rx_b_2[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_Rx_b_2[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_b_2[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_b_2[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_Rx_b_2,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Rx_b_year.csv"),row.names = F)
rm(MIG_Rx_b_2)


#by year group and maternal age
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by=c("year_group","age_band"), .N]
names(records)<-c("year_group","age_band", "no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by=c("year_group","age_band"),.N]
names(total)<-c("year_group","age_band","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","age_band"), all=T)
MIG_Rx_b_3<-data.table(algorithm="MIG_Rx_b", records)
MIG_Rx_b_3[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_Rx_b_3<-MIG_Rx_b_3[order(year_group,age_band)]
rm(records,total)
MIG_Rx_b_3[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_Rx_b_3[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_Rx_b_3[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_Rx_b_3[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_b_3[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_b_3[lower_95_CI<0,lower_95_CI:=0]

pregnancy_d3_mig[,include:=NULL]
fwrite(MIG_Rx_b_3,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Rx_b_year_age.csv"),row.names = F)
rm(MIG_Rx_b_3)


#### MIG_DxRx_a:Prevalence of migraine at baseline when lookback==12 months(3 months) ####
print("Create algorithm MIG_DxRx_a")
MIG_DxRx_a<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_DxRx_a"]
alt_col<-MIG_DxRx_a[TYPE=="OR",STUDY_VARIABLES]

if(length(alt_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("|" , lapply(.SD,`>=`, 1)),.SDcols=alt_col],alternative:=1]}else{pregnancy_d3_mig[,alternative:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm/final_d3/MIG_DxRx_a_D3.rds"))

#export MIG_Dx_a
MIG_DxRx_a_dt<-data.table(algorithm="MIG_DxRx_a", no_diagnosed_pregnancies=pregnancy_d3_mig[alternative==1 & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id),.N])
MIG_DxRx_a_dt[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_DxRx_a_dt[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
MIG_DxRx_a_dt[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_a_dt[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_a_dt[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_DxRx_a_dt,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_DxRx_a.csv"),row.names = F)

#by maternal age_band
pregnancy_d3_mig[,age_band:=as.character(age_band)]
records<-pregnancy_d3_mig[alternative==1 & !duplicated(pregnancy_id), .N, by=.(age_band)]
names(records)<-c("age_band","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id), by="age_band",.N]
names(total)<-c("age_band","no_pregnancies")
records<-merge.data.table(records,total,by="age_band",all=T)
MIG_DxRx_a_1<-data.table(algorithm="MIG_DxRx_a", records)
MIG_DxRx_a_1[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_DxRx_a_1<-MIG_DxRx_a_1[order(age_band)]
rm(records,total)
MIG_DxRx_a_1[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_DxRx_a_1[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_DxRx_a_1[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_DxRx_a_1[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_a_1[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_a_1[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_DxRx_a_1,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_DxRx_a_age.csv"),row.names = F)
rm(MIG_DxRx_a_1)

#by year to year_group
records<-pregnancy_d3_mig[alternative==1 & !duplicated(pregnancy_id),by="year_group", .N]
names(records)<-c("year_group","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by="year_group",.N]
names(total)<-c("year_group","no_pregnancies")
records<-merge.data.table(records,total,by="year_group",all=T)
MIG_DxRx_a_2<-data.table(algorithm="MIG_DxRx_a", records)
MIG_DxRx_a_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_DxRx_a_2<-MIG_DxRx_a_2[order(year_group)]
rm(records,total)
MIG_DxRx_a_2[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_DxRx_a_2[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_DxRx_a_2[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_DxRx_a_2[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_a_2[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_a_2[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_DxRx_a_2,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_DxRx_a_year.csv"),row.names = F)
rm(MIG_DxRx_a_2)


#by year group and maternal age
records<-pregnancy_d3_mig[alternative==1 & !duplicated(pregnancy_id),by=c("year_group","age_band"), .N]
names(records)<-c("year_group","age_band", "no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by=c("year_group","age_band"),.N]
names(total)<-c("year_group","age_band","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","age_band"), all=T)
MIG_DxRx_a_3<-data.table(algorithm="MIG_DxRx_a", records)
MIG_DxRx_a_3[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_DxRx_a_3<-MIG_DxRx_a_3[order(year_group,age_band)]
rm(records,total)
MIG_DxRx_a_3[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_DxRx_a_3[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_DxRx_a_3[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_DxRx_a_3[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_a_3[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_a_3[lower_95_CI<0,lower_95_CI:=0]

pregnancy_d3_mig[,alternative:=NULL]
fwrite(MIG_DxRx_a_3,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_DxRx_a_year_age.csv"),row.names = F)
rm(MIG_DxRx_a_3)


#### MIG_DxRx_b:Prevalence of migraine at baseline when lookback==3 months ####
print("Create algorithm MIG_DxRx_b")
MIG_DxRx_b<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_DxRx_b"]
alt_col<-MIG_DxRx_b[TYPE=="OR",STUDY_VARIABLES]

if(length(alt_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("|" , lapply(.SD,`>=`, 1)),.SDcols=alt_col],alternative:=1]}else{pregnancy_d3_mig[,alternative:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm/final_d3/MIG_DxRx_b_D3.rds"))

#export MIG_Dx_a
MIG_DxRx_b_dt<-data.table(algorithm="MIG_DxRx_b", no_diagnosed_pregnancies=pregnancy_d3_mig[alternative==1 & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id),.N])
MIG_DxRx_b_dt[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_DxRx_b_dt[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
MIG_DxRx_b_dt[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_b_dt[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_b_dt[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_DxRx_b_dt,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_DxRx_b.csv"),row.names = F)

#by maternal age_band
pregnancy_d3_mig[,age_band:=as.character(age_band)]
records<-pregnancy_d3_mig[alternative==1 & !duplicated(pregnancy_id), .N, by=.(age_band)]
names(records)<-c("age_band","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id), by="age_band",.N]
names(total)<-c("age_band","no_pregnancies")
records<-merge.data.table(records,total,by="age_band",all=T)
MIG_DxRx_b_1<-data.table(algorithm="MIG_DxRx_b", records)
MIG_DxRx_b_1[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_DxRx_b_1<-MIG_DxRx_b_1[order(age_band)]
rm(records,total)
MIG_DxRx_b_1[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_DxRx_b_1[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_DxRx_b_1[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_DxRx_b_1[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_b_1[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_b_1[lower_95_CI<0,lower_95_CI:=0]
fwrite(MIG_DxRx_b_1,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_DxRx_b_age.csv"),row.names = F)
rm(MIG_DxRx_b_1)


#by year to year_group
records<-pregnancy_d3_mig[alternative==1 & !duplicated(pregnancy_id),by="year_group", .N]
names(records)<-c("year_group","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by="year_group",.N]
names(total)<-c("year_group","no_pregnancies")
records<-merge.data.table(records,total,by="year_group",all=T)
MIG_DxRx_b_2<-data.table(algorithm="MIG_DxRx_b", records)
MIG_DxRx_b_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_DxRx_b_2<-MIG_DxRx_b_2[order(year_group)]
rm(records,total)
MIG_DxRx_b_2[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_DxRx_b_2[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_DxRx_b_2[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_DxRx_b_2[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_b_2[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_b_2[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_DxRx_b_2,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_DxRx_b_year.csv"),row.names = F)
rm(MIG_DxRx_b_2)


#by year group and maternal age_band
records<-pregnancy_d3_mig[alternative==1 & !duplicated(pregnancy_id),by=c("year_group","age_band"), .N]
names(records)<-c("year_group","age_band", "no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by=c("year_group","age_band"),.N]
names(total)<-c("year_group","age_band","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","age_band"), all=T)
MIG_DxRx_b_3<-data.table(algorithm="MIG_DxRx_b", records)
MIG_DxRx_b_3[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_DxRx_b_3<-MIG_DxRx_b_3[order(year_group,age_band)]
rm(records,total)
MIG_DxRx_b_3[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_DxRx_b_3[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_DxRx_b_3[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_DxRx_b_3[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_b_3[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_b_3[lower_95_CI<0,lower_95_CI:=0]

pregnancy_d3_mig[,alternative:=NULL]
fwrite(MIG_DxRx_b_3,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_DxRx_b_year_age.csv"),row.names = F)
rm(MIG_DxRx_b_3)

#### MIG_Dx_during:Prevalence of migraine during pregnancy when lookback==12 months(3 months) ####
print("Create algorithm MIG_Dx_during")
MIG_Dx_during<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_Dx_during"]
inc_col<-MIG_Dx_during[TYPE=="AND",STUDY_VARIABLES]

if(length(inc_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col],include:=1]}else{pregnancy_d3_mig[,include:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm/final_d3/MIG_Dx_during_D3.rds"))

#export MIG_Dx_during
MIG_Dx_during_dt<-data.table(algorithm="MIG_Dx_during", no_diagnosed_pregnancies=pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id),.N])
MIG_Dx_during_dt[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_Dx_during_dt[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
MIG_Dx_during_dt[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_during_dt[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_during_dt[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_Dx_during_dt,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Dx_during.csv"),row.names = F)

#by maternal age_band
pregnancy_d3_mig[,age_band:=as.character(age_band)]
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id), .N, by=.(age_band)]
names(records)<-c("age_band","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id), by="age_band",.N]
names(total)<-c("age_band","no_pregnancies")
records<-merge.data.table(records,total,by="age_band",all=T)
MIG_Dx_during_1<-data.table(algorithm="MIG_Dx_during", records)
MIG_Dx_during_1[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_Dx_during_1<-MIG_Dx_during_1[order(age_band)]
rm(records,total)
MIG_Dx_during_1[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_Dx_during_1[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_Dx_during_1[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_Dx_during_1[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_during_1[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_during_1[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_Dx_during_1,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Dx_during_age.csv"),row.names = F)
rm(MIG_Dx_during_1)

#by year to year_group
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by="year_group", .N]
names(records)<-c("year_group","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by="year_group",.N]
names(total)<-c("year_group","no_pregnancies")
records<-merge.data.table(records,total,by="year_group",all=T)
MIG_Dx_during_2<-data.table(algorithm="MIG_Dx_during", records)
MIG_Dx_during_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_Dx_during_2<-MIG_Dx_during_2[order(year_group)]
rm(records,total)
MIG_Dx_during_2[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_Dx_during_2[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_Dx_during_2[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_Dx_during_2[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_during_2[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_during_2[lower_95_CI<0,lower_95_CI:=0]
fwrite(MIG_Dx_during_2,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Dx_during_year.csv"),row.names = F)
rm(MIG_Dx_during_2)

#by year group and maternal age_band
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by=c("year_group","age_band"), .N]
names(records)<-c("year_group","age_band", "no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by=c("year_group","age_band"),.N]
names(total)<-c("year_group","age_band","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","age_band"), all=T)
MIG_Dx_during_3<-data.table(algorithm="MIG_Dx_during", records)
MIG_Dx_during_3[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_Dx_during_3<-MIG_Dx_during_3[order(year_group,age_band)]
rm(records,total)
MIG_Dx_during_3[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_Dx_during_3[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_Dx_during_3[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_Dx_during_3[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_during_3[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_during_3[lower_95_CI<0,lower_95_CI:=0]
pregnancy_d3_mig[,include:=NULL]
rm(MIG_Dx_during)
fwrite(MIG_Dx_during_3,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Dx_during_year_age.csv"),row.names = F)
rm(MIG_Dx_during_3)

#### MIG_Rx_during:Prevalence of migraine during pregnancy when lookback==12 months(3 months) ####
print("Create algorithm MIG_Rx_during")
MIG_Rx_during<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_Rx_during"]
inc_col<-MIG_Rx_during[TYPE=="AND",STUDY_VARIABLES]

if(length(inc_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col],include:=1]}else{pregnancy_d3_mig[,include:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm/final_d3/MIG_Rx_during_D3.rds"))

#export MIG_Rx_during
MIG_Rx_during_dt<-data.table(algorithm="MIG_Rx_during", no_diagnosed_pregnancies=pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id),.N])
MIG_Rx_during_dt[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_Rx_during_dt[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
MIG_Rx_during_dt[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_during_dt[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_during_dt[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_Rx_during_dt,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Rx_during.csv"),row.names = F)

#by maternal age
pregnancy_d3_mig[,age_band:=as.character(age_band)]
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id), .N, by=.(age_band)]
names(records)<-c("age_band","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id), by="age_band",.N]
names(total)<-c("age_band","no_pregnancies")
records<-merge.data.table(records,total,by="age_band",all=T)
MIG_Rx_during_1<-data.table(algorithm="MIG_Rx_during", records)
MIG_Rx_during_1[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_Rx_during_1<-MIG_Rx_during_1[order(age_band)]
rm(records,total)
MIG_Rx_during_1[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_Rx_during_1[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_Rx_during_1[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_Rx_during_1[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_during_1[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_during_1[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_Rx_during_1,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Rx_during_age.csv"),row.names = F)
rm(MIG_Rx_during_1)


#by year to year_group
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by="year_group", .N]
names(records)<-c("year_group","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by="year_group",.N]
names(total)<-c("year_group","no_pregnancies")
records<-merge.data.table(records,total,by="year_group",all=T)
MIG_Rx_during_2<-data.table(algorithm="MIG_Rx_during", records)
MIG_Rx_during_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_Rx_during_2<-MIG_Rx_during_2[order(year_group)]
rm(records,total)
MIG_Rx_during_2[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_Rx_during_2[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_Rx_during_2[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_Rx_during_2[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_during_2[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_during_2[lower_95_CI<0,lower_95_CI:=0]
fwrite(MIG_Rx_during_2,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Rx_during_year.csv"),row.names = F)
rm(MIG_Rx_during_2)


#by year group and maternal age_band
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by=c("year_group","age_band"), .N]
names(records)<-c("year_group","age_band", "no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by=c("year_group","age_band"),.N]
names(total)<-c("year_group","age_band","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","age_band"), all=T)
MIG_Rx_during_3<-data.table(algorithm="MIG_Rx_during", records)
MIG_Rx_during_3[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_Rx_during_3<-MIG_Rx_during_3[order(year_group,age_band)]
rm(records,total)
MIG_Rx_during_3[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_Rx_during_3[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_Rx_during_3[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_Rx_during_3[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_during_3[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_during_3[lower_95_CI<0,lower_95_CI:=0]

pregnancy_d3_mig[,include:=NULL]
fwrite(MIG_Rx_during_3,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Rx_during_year_age.csv"),row.names = F)
rm(MIG_Rx_during_3)

#### MIG_DxRx_during:Prevalence of migraine at baseline when lookback==12 months(3 months) ####
print("Create algorithm MIG_DxRx_during")
MIG_DxRx_during<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_DxRx_during"]
alt_col<-MIG_DxRx_during[TYPE=="OR",STUDY_VARIABLES]

if(length(alt_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("|" , lapply(.SD,`>=`, 1)),.SDcols=alt_col],alternative:=1]}else{pregnancy_d3_mig[,alternative:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm/final_d3/MIG_DxRx_during_D3.rds"))

#export MIG_Dx_a
MIG_DxRx_during_dt<-data.table(algorithm="MIG_DxRx_during", no_diagnosed_pregnancies=pregnancy_d3_mig[alternative==1 & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id),.N])
MIG_DxRx_during_dt[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_DxRx_during_dt[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
MIG_DxRx_during_dt[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_during_dt[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_during_dt[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_DxRx_during_dt,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_DxRx_during.csv"),row.names = F)

#by maternal age_band
pregnancy_d3_mig[,age_band:=as.character(age_band)]
records<-pregnancy_d3_mig[alternative==1 & !duplicated(pregnancy_id), .N, by=.(age_band)]
names(records)<-c("age_band","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id), by="age_band",.N]
names(total)<-c("age_band","no_pregnancies")
records<-merge.data.table(records,total,by="age_band",all=T)
MIG_DxRx_during_1<-data.table(algorithm="MIG_DxRx_during", records)
MIG_DxRx_during_1[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_DxRx_during_1<-MIG_DxRx_during_1[order(age_band)]
rm(records,total)
MIG_DxRx_during_1[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_DxRx_during_1[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_DxRx_during_1[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_DxRx_during_1[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_during_1[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_during_1[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_DxRx_during_1,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_DxRx_during_age.csv"),row.names = F)
rm(MIG_DxRx_during_1)

#by year to year_group
records<-pregnancy_d3_mig[alternative==1 & !duplicated(pregnancy_id),by="year_group", .N]
names(records)<-c("year_group","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by="year_group",.N]
names(total)<-c("year_group","no_pregnancies")
records<-merge.data.table(records,total,by="year_group",all=T)
MIG_DxRx_during_2<-data.table(algorithm="MIG_DxRx_during", records)
MIG_DxRx_during_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_DxRx_during_2<-MIG_DxRx_during_2[order(year_group)]
rm(records,total)
MIG_DxRx_during_2[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_DxRx_during_2[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_DxRx_during_2[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_DxRx_during_2[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_during_2[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_during_2[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_DxRx_during_2,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_DxRx_during_year.csv"),row.names = F)
rm(MIG_DxRx_during_2)


#by year group and maternal age
records<-pregnancy_d3_mig[alternative==1 & !duplicated(pregnancy_id),by=c("year_group","age_band"), .N]
names(records)<-c("year_group","age_band", "no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by=c("year_group","age_band"),.N]
names(total)<-c("year_group","age_band","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","age_band"), all=T)
MIG_DxRx_during_3<-data.table(algorithm="MIG_DxRx_during", records)
MIG_DxRx_during_3[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_DxRx_during_3<-MIG_DxRx_during_3[order(year_group,age_band)]
rm(records,total)
MIG_DxRx_during_3[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_DxRx_during_3[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_DxRx_during_3[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_DxRx_during_3[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_during_3[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_during_3[lower_95_CI<0,lower_95_CI:=0]

pregnancy_d3_mig[,alternative:=NULL]
fwrite(MIG_DxRx_during_3,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_DxRx_during_year_age.csv"),row.names = F)
rm(MIG_DxRx_during_3)


#### MIG_checkbox_during:Prevalence of migraine during pregnancy when lookback==12 months(3 months) ####
if(!is.null(preg_d3_checkbox)){
print("Create algorithm MIG_checkbox_during")
MIG_checkbox_during<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_checkbox_during"]
inc_col<-MIG_checkbox_during[TYPE=="AND",STUDY_VARIABLES]
if(!"Migraine_checkbox_during" %in% names(preg_d3_checkbox)){preg_d3_checkbox[,Migraine_checkbox_during:=0]}

if(length(inc_col)>0){preg_d3_checkbox[preg_d3_checkbox[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col],include:=1]}else{preg_d3_checkbox[,include:=NA]}

saveRDS(preg_d3_checkbox,paste0(projectFolder,"/g_intermediate/migraine_algorithm/final_d3/MIG_checkbox_during_D3.rds"))

#export MIG_checkbox_during
MIG_checkbox_during_dt<-data.table(algorithm="MIG_checkbox_during", no_diagnosed_pregnancies=preg_d3_checkbox[include==1 & !duplicated(pregnancy_id),.N], no_pregnancies=preg_d3_checkbox[!duplicated(pregnancy_id),.N])
MIG_checkbox_during_dt[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_checkbox_during_dt[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
MIG_checkbox_during_dt[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_checkbox_during_dt[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_checkbox_during_dt[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_checkbox_during_dt,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_checkbox_during.csv"),row.names = F)

MIG_checkbox_baseline_dt<-MIG_checkbox_during_dt[,-c("algorithm")]
MIG_checkbox_baseline_dt[,algorithm:="MIG_checkbox_baseline_a"]
setcolorder(MIG_checkbox_baseline_dt, c("algorithm","no_diagnosed_pregnancies","no_pregnancies","prevalence_100_pregnancies","lower_95_CI","upper_95_CI"))
fwrite(MIG_checkbox_baseline_dt,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_checkbox_baseline_a.csv"),row.names = F)
rm(MIG_checkbox_baseline_dt)

#by maternal age_band
preg_d3_checkbox[,age_band:=as.character(age_band)]
records<-preg_d3_checkbox[include==1 & !duplicated(pregnancy_id), .N, by=.(age_band)]
names(records)<-c("age_band","no_diagnosed_pregnancies")
total<-preg_d3_checkbox[!duplicated(pregnancy_id), by="age_band",.N]
names(total)<-c("age_band","no_pregnancies")
records<-merge.data.table(records,total,by="age_band",all=T)
MIG_Dx_during_1<-data.table(algorithm="MIG_checkbox_during", records)
MIG_Dx_during_1[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_Dx_during_1<-MIG_Dx_during_1[order(age_band)]
rm(records,total)
MIG_Dx_during_1[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_Dx_during_1[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_Dx_during_1[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_Dx_during_1[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_during_1[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_during_1[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_Dx_during_1,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_checkbox_during_age.csv"),row.names = F)
MIG_Dx_baseline_1<-MIG_Dx_during_1[,-c("algorithm")]
MIG_Dx_baseline_1[,algorithm:="MIG_checkbox_baseline_a"]
setcolorder(MIG_Dx_baseline_1, c("algorithm","age_band", "no_diagnosed_pregnancies","no_pregnancies","prevalence_100_pregnancies","lower_95_CI","upper_95_CI"))
fwrite(MIG_Dx_baseline_1,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_checkbox_baseline_a_age.csv"),row.names = F)
rm(MIG_Dx_during_1,MIG_Dx_baseline_1)

#by year to year_group
records<-preg_d3_checkbox[include==1 & !duplicated(pregnancy_id),by="year_group", .N]
names(records)<-c("year_group","no_diagnosed_pregnancies")
total<-preg_d3_checkbox[!duplicated(pregnancy_id),by="year_group",.N]
names(total)<-c("year_group","no_pregnancies")
records<-merge.data.table(records,total,by="year_group",all=T)
MIG_Dx_during_2<-data.table(algorithm="MIG_checkbox_during", records)
MIG_Dx_during_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_Dx_during_2<-MIG_Dx_during_2[order(year_group)]
rm(records,total)
MIG_Dx_during_2[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_Dx_during_2[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_Dx_during_2[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_Dx_during_2[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_during_2[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_during_2[lower_95_CI<0,lower_95_CI:=0]
fwrite(MIG_Dx_during_2,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_checkbox_during_year.csv"),row.names = F)

MIG_Dx_baseline_2<-MIG_Dx_during_2[,-c("algorithm")]
MIG_Dx_baseline_2[,algorithm:="MIG_checkbox_baseline_a"]
setcolorder(MIG_Dx_baseline_2, c("algorithm","year_group", "no_diagnosed_pregnancies","no_pregnancies","prevalence_100_pregnancies","lower_95_CI","upper_95_CI"))
fwrite(MIG_Dx_baseline_2,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_checkbox_baseline_a_year.csv"),row.names = F)

rm(MIG_Dx_during_2,MIG_Dx_baseline_2)

#by year group and maternal age_band
records<-preg_d3_checkbox[include==1 & !duplicated(pregnancy_id),by=c("year_group","age_band"), .N]
names(records)<-c("year_group","age_band", "no_diagnosed_pregnancies")
total<-preg_d3_checkbox[!duplicated(pregnancy_id),by=c("year_group","age_band"),.N]
names(total)<-c("year_group","age_band","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","age_band"), all=T)
MIG_Dx_during_3<-data.table(algorithm="MIG_checkbox_during", records)
MIG_Dx_during_3[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_Dx_during_3<-MIG_Dx_during_3[order(year_group,age_band)]
rm(records,total)
MIG_Dx_during_3[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_Dx_during_3[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_Dx_during_3[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_Dx_during_3[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_during_3[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_during_3[lower_95_CI<0,lower_95_CI:=0]
preg_d3_checkbox[,include:=NULL]

fwrite(MIG_Dx_during_3,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_checkbox_during_year_age.csv"),row.names = F)

MIG_Dx_baseline_3<-MIG_Dx_during_3[,-c("algorithm")]
MIG_Dx_baseline_3[,algorithm:="MIG_checkbox_baseline_a"]
setcolorder(MIG_Dx_baseline_3, c("algorithm", "year_group","age_band", "no_diagnosed_pregnancies","no_pregnancies","prevalence_100_pregnancies","lower_95_CI","upper_95_CI"))
fwrite(MIG_Dx_baseline_3,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_checkbox_baseline_a_year_age.csv"),row.names = F)

rm(MIG_Dx_during_3,MIG_Dx_baseline_3)
}else{
  MIG_checkbox_during_dt<-data.table(algorithm="MIG_checkbox_during",no_diagnosed_pregnancies=0, no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id),.N], prevalence_100_pregnancies=0, lower_95_CI=0, upper_95_CI=0)  
  MIG_checkbox_baseline_dt<-data.table(algorithm="MIG_checkbox_baseline_a",no_diagnosed_pregnancies=0, no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id),.N], prevalence_100_pregnancies=0, lower_95_CI=0, upper_95_CI=0)  
  MIG_Dx_during_1<-data.table(algorithm="MIG_checkbox_during",no_diagnosed_pregnancies=0, no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id),.N], prevalence_100_pregnancies=0, lower_95_CI=0, upper_95_CI=0)  
  MIG_Dx_baseline_1<-data.table(algorithm="MIG_checkbox_baseline_a",no_diagnosed_pregnancies=0, no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id),.N], prevalence_100_pregnancies=0, lower_95_CI=0, upper_95_CI=0)  
  MIG_Dx_during_2<-data.table(algorithm="MIG_checkbox_during",no_diagnosed_pregnancies=0, no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id),.N], prevalence_100_pregnancies=0, lower_95_CI=0, upper_95_CI=0)  
  MIG_Dx_baseline_2<-data.table(algorithm="MIG_checkbox_baseline_a",no_diagnosed_pregnancies=0, no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id),.N], prevalence_100_pregnancies=0, lower_95_CI=0, upper_95_CI=0)  
  MIG_Dx_during_3<-data.table(algorithm="MIG_checkbox_during",no_diagnosed_pregnancies=0, no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id),.N], prevalence_100_pregnancies=0, lower_95_CI=0, upper_95_CI=0)  
  MIG_Dx_baseline_3<-data.table(algorithm="MIG_checkbox_baseline_a",no_diagnosed_pregnancies=0, no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id),.N], prevalence_100_pregnancies=0, lower_95_CI=0, upper_95_CI=0)  
  
  fwrite(MIG_checkbox_during_dt,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_checkbox_during.csv"),row.names = F)
  fwrite(MIG_checkbox_baseline_dt,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_checkbox_baseline_a.csv"),row.names = F)
  fwrite(MIG_Dx_during_1,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_checkbox_during_age.csv"),row.names = F)
  fwrite(MIG_Dx_baseline_1,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_checkbox_baseline_a_age.csv"),row.names = F)
  fwrite(MIG_Dx_during_2,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_checkbox_during_year.csv"),row.names = F)
  fwrite(MIG_Dx_baseline_2,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_checkbox_baseline_a_year.csv"),row.names = F)
  fwrite(MIG_Dx_during_3,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_checkbox_during_year_age.csv"),row.names = F)
  fwrite(MIG_Dx_baseline_3,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_checkbox_baseline_a_year_age.csv"),row.names = F)
  
  
}
#### Select the trimester timepoint ####
pregnancy_d3_mig[,dif:=pregnancy_end_date-pregnancy_start_date]
pregnancy_d3_mig[dif<97, trimester:=0]
pregnancy_d3_mig[dif>=97, trimester:=1]
pregnancy_d3_mig[dif>=195, trimester:=2]
pregnancy_d3_mig[dif>=280, trimester:=3]
pregnancy_d3_mig[,dif:=NULL]
#### MIG_Dx_first:Prevalence of migraine during pregnancy when lookback==12 months(3 months) ####
print("Create algorithm MIG_Dx_first")
MIG_Dx_first<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_Dx_first"]
inc_col<-MIG_Dx_first[TYPE=="AND",STUDY_VARIABLES]

#set the pregnancy trimester for each pregnancy
if(length(inc_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col] & trimester %in% c(1,2,3),include:=1]}else{pregnancy_d3_mig[,include:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm/final_d3/MIG_Dx_first_D3.rds"))

#export MIG_Dx_first
MIG_Dx_first_dt<-data.table(algorithm="MIG_Dx_first", no_diagnosed_pregnancies=pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id) & trimester %in% c(1,2,3),.N])
MIG_Dx_first_dt[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_Dx_first_dt[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
MIG_Dx_first_dt[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_first_dt[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_first_dt[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_Dx_first_dt,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Dx_first.csv"),row.names = F)

#by maternal age_band
pregnancy_d3_mig[,age_band:=as.character(age_band)]
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id), .N, by=.(age_band)]
names(records)<-c("age_band","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id) & trimester %in% c(1,2,3), by="age_band",.N]
names(total)<-c("age_band","no_pregnancies")
records<-merge.data.table(records,total,by="age_band",all=T)
MIG_Dx_first_1<-data.table(algorithm="MIG_Dx_first", records)
MIG_Dx_first_1[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_Dx_first_1<-MIG_Dx_first_1[order(age_band)]
rm(records,total)
MIG_Dx_first_1[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_Dx_first_1[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_Dx_first_1[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_Dx_first_1[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_first_1[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_first_1[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_Dx_first_1,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Dx_first_age.csv"),row.names = F)
rm(MIG_Dx_first_1)

#by year to year_group
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by="year_group", .N]
names(records)<-c("year_group","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id) & trimester%in% c(1,2,3),by="year_group",.N]
names(total)<-c("year_group","no_pregnancies")
records<-merge.data.table(records,total,by="year_group",all=T)
MIG_Dx_first_2<-data.table(algorithm="MIG_Dx_first", records)
MIG_Dx_first_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_Dx_first_2<-MIG_Dx_first_2[order(year_group)]
rm(records,total)
MIG_Dx_first_2[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_Dx_first_2[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_Dx_first_2[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_Dx_first_2[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_first_2[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_first_2[lower_95_CI<0,lower_95_CI:=0]
fwrite(MIG_Dx_first_2,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Dx_first_year.csv"),row.names = F)
rm(MIG_Dx_first_2)

#by year group and maternal age_band
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by=c("year_group","age_band"), .N]
names(records)<-c("year_group","age_band", "no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id) & trimester%in% c(1,2,3),by=c("year_group","age_band"),.N]
names(total)<-c("year_group","age_band","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","age_band"), all=T)
MIG_Dx_first_3<-data.table(algorithm="MIG_Dx_first", records)
MIG_Dx_first_3[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_Dx_first_3<-MIG_Dx_first_3[order(year_group,age_band)]
rm(records,total)
MIG_Dx_first_3[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_Dx_first_3[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_Dx_first_3[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_Dx_first_3[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_first_3[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_first_3[lower_95_CI<0,lower_95_CI:=0]
pregnancy_d3_mig[,include:=NULL]
rm(MIG_Dx_first)
fwrite(MIG_Dx_first_3,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Dx_first_year_age.csv"),row.names = F)
rm(MIG_Dx_first_3)

#### MIG_Rx_first:Prevalence of migraine during pregnancy when lookback==12 months(3 months) ####
print("Create algorithm MIG_Rx_first")
MIG_Rx_first<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_Rx_first"]
inc_col<-MIG_Rx_first[TYPE=="AND",STUDY_VARIABLES]

if(length(inc_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col] & trimester %in% c(1,2,3),include:=1]}else{pregnancy_d3_mig[,include:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm/final_d3/MIG_Rx_first_D3.rds"))

#export MIG_Rx_first
MIG_Rx_first_dt<-data.table(algorithm="MIG_Rx_first", no_diagnosed_pregnancies=pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id) & trimester %in% c(1,2,3),.N])
MIG_Rx_first_dt[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_Rx_first_dt[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
MIG_Rx_first_dt[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_first_dt[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_first_dt[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_Rx_first_dt,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Rx_first.csv"),row.names = F)

#by maternal age_band
pregnancy_d3_mig[,age_band:=as.character(age_band)]
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id), .N, by=.(age_band)]
names(records)<-c("age_band","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id) & trimester %in% c(1,2,3), by="age_band",.N]
names(total)<-c("age_band","no_pregnancies")
records<-merge.data.table(records,total,by="age_band",all=T)
MIG_Rx_first_1<-data.table(algorithm="MIG_Rx_first", records)
MIG_Rx_first_1[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_Rx_first_1<-MIG_Rx_first_1[order(age_band)]
rm(records,total)
MIG_Rx_first_1[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_Rx_first_1[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_Rx_first_1[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_Rx_first_1[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_first_1[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_first_1[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_Rx_first_1,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Rx_first_age.csv"),row.names = F)
rm(MIG_Rx_first_1)


#by year
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by="year_group", .N]
names(records)<-c("year_group","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id) & trimester %in% c(1,2,3),by="year_group",.N]
names(total)<-c("year_group","no_pregnancies")
records<-merge.data.table(records,total,by="year_group",all=T)
MIG_Rx_first_2<-data.table(algorithm="MIG_Rx_first", records)
MIG_Rx_first_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_Rx_first_2<-MIG_Rx_first_2[order(year_group)]
rm(records,total)
MIG_Rx_first_2[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_Rx_first_2[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_Rx_first_2[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_Rx_first_2[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_first_2[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_first_2[lower_95_CI<0,lower_95_CI:=0]
fwrite(MIG_Rx_first_2,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Rx_first_year.csv"),row.names = F)
rm(MIG_Rx_first_2)


#by year group and maternal age_band
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by=c("year_group","age_band"), .N]
names(records)<-c("year_group","age_band", "no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id) & trimester %in% c(1,2,3),by=c("year_group","age_band"),.N]
names(total)<-c("year_group","age_band","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","age_band"), all=T)
MIG_Rx_first_3<-data.table(algorithm="MIG_Rx_first", records)
MIG_Rx_first_3[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_Rx_first_3<-MIG_Rx_first_3[order(year_group,age_band)]
rm(records,total)
MIG_Rx_first_3[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_Rx_first_3[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_Rx_first_3[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_Rx_first_3[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_first_3[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_first_3[lower_95_CI<0,lower_95_CI:=0]

pregnancy_d3_mig[,include:=NULL]
fwrite(MIG_Rx_first_3,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Rx_first_year_age.csv"),row.names = F)
rm(MIG_Rx_first_3)

#### MIG_DxRx_first:Prevalence of migraine at baseline when lookback==12 months(3 months) ####
print("Create algorithm MIG_DxRx_first")
MIG_DxRx_first<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_DxRx_first"]
alt_col<-MIG_DxRx_first[TYPE=="OR",STUDY_VARIABLES]

if(length(alt_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("|" , lapply(.SD,`>=`, 1)),.SDcols=alt_col] & trimester%in% c(1,2,3),alternative:=1]}else{pregnancy_d3_mig[,alternative:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm/final_d3/MIG_DxRx_first_D3.rds"))

#export MIG_Dx_a
MIG_DxRx_first_dt<-data.table(algorithm="MIG_DxRx_first", no_diagnosed_pregnancies=pregnancy_d3_mig[alternative==1 & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id) & trimester%in% c(1,2,3),.N])
MIG_DxRx_first_dt[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_DxRx_first_dt[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
MIG_DxRx_first_dt[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_first_dt[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_first_dt[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_DxRx_first_dt,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_DxRx_first.csv"),row.names = F)

#by maternal age_band
pregnancy_d3_mig[,age_band:=as.character(age_band)]
records<-pregnancy_d3_mig[alternative==1 & !duplicated(pregnancy_id), .N, by=.(age_band)]
names(records)<-c("age_band","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id)& trimester%in% c(1,2,3), by="age_band",.N]
names(total)<-c("age_band","no_pregnancies")
records<-merge.data.table(records,total,by="age_band",all=T)
MIG_DxRx_first_1<-data.table(algorithm="MIG_DxRx_first", records)
MIG_DxRx_first_1[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_DxRx_first_1<-MIG_DxRx_first_1[order(age_band)]
rm(records,total)
MIG_DxRx_first_1[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_DxRx_first_1[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_DxRx_first_1[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_DxRx_first_1[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_first_1[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_first_1[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_DxRx_first_1,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_DxRx_first_age.csv"),row.names = F)
rm(MIG_DxRx_first_1)

#by year to year_group
records<-pregnancy_d3_mig[alternative==1 & !duplicated(pregnancy_id),by="year_group", .N]
names(records)<-c("year_group","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id)& trimester %in% c(1,2,3),by="year_group",.N]
names(total)<-c("year_group","no_pregnancies")
records<-merge.data.table(records,total,by="year_group",all=T)
MIG_DxRx_first_2<-data.table(algorithm="MIG_DxRx_first", records)
MIG_DxRx_first_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_DxRx_first_2<-MIG_DxRx_first_2[order(year_group)]
rm(records,total)
MIG_DxRx_first_2[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_DxRx_first_2[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_DxRx_first_2[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_DxRx_first_2[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_first_2[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_first_2[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_DxRx_first_2,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_DxRx_first_year.csv"),row.names = F)
rm(MIG_DxRx_first_2)


#by year group and maternal age_band
records<-pregnancy_d3_mig[alternative==1 & !duplicated(pregnancy_id),by=c("year_group","age_band"), .N]
names(records)<-c("year_group","age_band", "no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id)& trimester %in% c(1,2,3),by=c("year_group","age_band"),.N]
names(total)<-c("year_group","age_band","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","age_band"), all=T)
MIG_DxRx_first_3<-data.table(algorithm="MIG_DxRx_first", records)
MIG_DxRx_first_3[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_DxRx_first_3<-MIG_DxRx_first_3[order(year_group,age_band)]
rm(records,total)
MIG_DxRx_first_3[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_DxRx_first_3[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_DxRx_first_3[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_DxRx_first_3[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_first_3[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_first_3[lower_95_CI<0,lower_95_CI:=0]

pregnancy_d3_mig[,alternative:=NULL]
fwrite(MIG_DxRx_first_3,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_DxRx_first_year_age.csv"),row.names = F)
rm(MIG_DxRx_first_3)



#### MIG_Dx_second:Prevalence of migraine during pregnancy when lookback==12 months(3 months) ####
print("Create algorithm MIG_Dx_second")
MIG_Dx_second<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_Dx_second"]
inc_col<-MIG_Dx_second[TYPE=="AND",STUDY_VARIABLES]

#set the pregnancy trimester for each pregnancy
if(length(inc_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col] & trimester %in% c(2,3),include:=1]}else{pregnancy_d3_mig[,include:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm/final_d3/MIG_Dx_second_D3.rds"))

#export MIG_Dx_second
MIG_Dx_second_dt<-data.table(algorithm="MIG_Dx_second", no_diagnosed_pregnancies=pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id) & trimester %in% c(2,3),.N])
MIG_Dx_second_dt[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_Dx_second_dt[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
MIG_Dx_second_dt[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_second_dt[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_second_dt[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_Dx_second_dt,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Dx_second.csv"),row.names = F)

#by maternal age_band
pregnancy_d3_mig[,age_band:=as.character(age_band)]
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id), .N, by=.(age_band)]
names(records)<-c("age_band","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id) & trimester %in% c(2,3), by="age_band",.N]
names(total)<-c("age_band","no_pregnancies")
records<-merge.data.table(records,total,by="age_band",all=T)
MIG_Dx_second_1<-data.table(algorithm="MIG_Dx_second", records)
MIG_Dx_second_1[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_Dx_second_1<-MIG_Dx_second_1[order(age_band)]
rm(records,total)
MIG_Dx_second_1[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_Dx_second_1[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_Dx_second_1[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_Dx_second_1[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_second_1[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_second_1[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_Dx_second_1,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Dx_second_age.csv"),row.names = F)
rm(MIG_Dx_second_1)

#by year to year_group
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by="year_group", .N]
names(records)<-c("year_group","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id) & trimester %in% c(2,3),by="year_group",.N]
names(total)<-c("year_group","no_pregnancies")
records<-merge.data.table(records,total,by="year_group",all=T)
MIG_Dx_second_2<-data.table(algorithm="MIG_Dx_second", records)
MIG_Dx_second_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_Dx_second_2<-MIG_Dx_second_2[order(year_group)]
rm(records,total)
MIG_Dx_second_2[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_Dx_second_2[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_Dx_second_2[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_Dx_second_2[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_second_2[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_second_2[lower_95_CI<0,lower_95_CI:=0]
fwrite(MIG_Dx_second_2,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Dx_second_year.csv"),row.names = F)
rm(MIG_Dx_second_2)

#by year group and maternal age_band
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by=c("year_group","age_band"), .N]
names(records)<-c("year_group","age_band", "no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id) & trimester %in% c(2,3),by=c("year_group","age_band"),.N]
names(total)<-c("year_group","age_band","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","age_band"), all=T)
MIG_Dx_second_3<-data.table(algorithm="MIG_Dx_second", records)
MIG_Dx_second_3[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_Dx_second_3<-MIG_Dx_second_3[order(year_group,age_band)]
rm(records,total)
MIG_Dx_second_3[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_Dx_second_3[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_Dx_second_3[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_Dx_second_3[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_second_3[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_second_3[lower_95_CI<0,lower_95_CI:=0]
pregnancy_d3_mig[,include:=NULL]
rm(MIG_Dx_second)
fwrite(MIG_Dx_second_3,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Dx_second_year_age.csv"),row.names = F)
rm(MIG_Dx_second_3)

#### MIG_Rx_second:Prevalence of migraine during pregnancy when lookback==12 months(3 months) ####
print("Create algorithm MIG_Rx_second")
MIG_Rx_second<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_Rx_second"]
inc_col<-MIG_Rx_second[TYPE=="AND",STUDY_VARIABLES]

if(length(inc_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col] & trimester %in% c(2,3),include:=1]}else{pregnancy_d3_mig[,include:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm/final_d3/MIG_Rx_second_D3.rds"))

#export MIG_Rx_second
MIG_Rx_second_dt<-data.table(algorithm="MIG_Rx_second", no_diagnosed_pregnancies=pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id) & trimester%in% c(2,3),.N])
MIG_Rx_second_dt[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_Rx_second_dt[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
MIG_Rx_second_dt[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_second_dt[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_second_dt[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_Rx_second_dt,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Rx_second.csv"),row.names = F)

#by maternal age_band
pregnancy_d3_mig[,age_band:=as.character(age_band)]
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id), .N, by=.(age_band)]
names(records)<-c("age_band","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id) & trimester%in% c(2,3), by="age_band",.N]
names(total)<-c("age_band","no_pregnancies")
records<-merge.data.table(records,total,by="age_band",all=T)
MIG_Rx_second_1<-data.table(algorithm="MIG_Rx_second", records)
MIG_Rx_second_1[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_Rx_second_1<-MIG_Rx_second_1[order(age_band)]
rm(records,total)
MIG_Rx_second_1[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_Rx_second_1[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_Rx_second_1[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_Rx_second_1[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_second_1[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_second_1[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_Rx_second_1,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Rx_second_age.csv"),row.names = F)
rm(MIG_Rx_second_1)


#by year to year_group
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by="year_group", .N]
names(records)<-c("year_group","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id) & trimester%in% c(2,3),by="year_group",.N]
names(total)<-c("year_group","no_pregnancies")
records<-merge.data.table(records,total,by="year_group",all=T)
MIG_Rx_second_2<-data.table(algorithm="MIG_Rx_second", records)
MIG_Rx_second_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_Rx_second_2<-MIG_Rx_second_2[order(year_group)]
rm(records,total)
MIG_Rx_second_2[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_Rx_second_2[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_Rx_second_2[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_Rx_second_2[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_second_2[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_second_2[lower_95_CI<0,lower_95_CI:=0]
fwrite(MIG_Rx_second_2,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Rx_second_year.csv"),row.names = F)
rm(MIG_Rx_second_2)


#by year group and maternal age
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by=c("year_group","age_band"), .N]
names(records)<-c("year_group","age_band", "no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id) & trimester%in% c(2,3),by=c("year_group","age_band"),.N]
names(total)<-c("year_group","age_band","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","age_band"), all=T)
MIG_Rx_second_3<-data.table(algorithm="MIG_Rx_second", records)
MIG_Rx_second_3[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_Rx_second_3<-MIG_Rx_second_3[order(year_group,age_band)]
rm(records,total)
MIG_Rx_second_3[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_Rx_second_3[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_Rx_second_3[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_Rx_second_3[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_second_3[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_second_3[lower_95_CI<0,lower_95_CI:=0]

pregnancy_d3_mig[,include:=NULL]
fwrite(MIG_Rx_second_3,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Rx_second_year_age.csv"),row.names = F)
rm(MIG_Rx_second_3)

#### MIG_DxRx_second:Prevalence of migraine at baseline when lookback==12 months(3 months) ####
print("Create algorithm MIG_DxRx_second")
MIG_DxRx_second<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_DxRx_second"]
alt_col<-MIG_DxRx_second[TYPE=="OR",STUDY_VARIABLES]

if(length(alt_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("|" , lapply(.SD,`>=`, 1)),.SDcols=alt_col] & trimester%in% c(2,3),alternative:=1]}else{pregnancy_d3_mig[,alternative:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm/final_d3/MIG_DxRx_second_D3.rds"))

#export MIG_Dx_a
MIG_DxRx_second_dt<-data.table(algorithm="MIG_DxRx_second", no_diagnosed_pregnancies=pregnancy_d3_mig[alternative==1 & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id) & trimester%in% c(2,3),.N])
MIG_DxRx_second_dt[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_DxRx_second_dt[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
MIG_DxRx_second_dt[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_second_dt[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_second_dt[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_DxRx_second_dt,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_DxRx_second.csv"),row.names = F)

#by maternal age_band
pregnancy_d3_mig[,age_band:=as.character(age_band)]
records<-pregnancy_d3_mig[alternative==1 & !duplicated(pregnancy_id), .N, by=.(age_band)]
names(records)<-c("age_band","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id)& trimester%in% c(2,3), by="age_band",.N]
names(total)<-c("age_band","no_pregnancies")
records<-merge.data.table(records,total,by="age_band",all=T)
MIG_DxRx_second_1<-data.table(algorithm="MIG_DxRx_second", records)
MIG_DxRx_second_1[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_DxRx_second_1<-MIG_DxRx_second_1[order(age_band)]
rm(records,total)
MIG_DxRx_second_1[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_DxRx_second_1[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_DxRx_second_1[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_DxRx_second_1[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_second_1[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_second_1[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_DxRx_second_1,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_DxRx_second_age.csv"),row.names = F)
rm(MIG_DxRx_second_1)

#by year to year_group
records<-pregnancy_d3_mig[alternative==1 & !duplicated(pregnancy_id),by="year_group", .N]
names(records)<-c("year_group","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id)& trimester%in% c(2,3),by="year_group",.N]
names(total)<-c("year_group","no_pregnancies")
records<-merge.data.table(records,total,by="year_group",all=T)
MIG_DxRx_second_2<-data.table(algorithm="MIG_DxRx_second", records)
MIG_DxRx_second_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_DxRx_second_2<-MIG_DxRx_second_2[order(year_group)]
rm(records,total)
MIG_DxRx_second_2[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_DxRx_second_2[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_DxRx_second_2[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_DxRx_second_2[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_second_2[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_second_2[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_DxRx_second_2,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_DxRx_second_year.csv"),row.names = F)
rm(MIG_DxRx_second_2)


#by year group and maternal age_band
records<-pregnancy_d3_mig[alternative==1 & !duplicated(pregnancy_id),by=c("year_group","age_band"), .N]
names(records)<-c("year_group","age_band", "no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id)& trimester%in% c(2,3),by=c("year_group","age_band"),.N]
names(total)<-c("year_group","age_band","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","age_band"), all=T)
MIG_DxRx_second_3<-data.table(algorithm="MIG_DxRx_second", records)
MIG_DxRx_second_3[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_DxRx_second_3<-MIG_DxRx_second_3[order(year_group,age_band)]
rm(records,total)
MIG_DxRx_second_3[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_DxRx_second_3[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_DxRx_second_3[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_DxRx_second_3[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_second_3[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_second_3[lower_95_CI<0,lower_95_CI:=0]

pregnancy_d3_mig[,alternative:=NULL]
fwrite(MIG_DxRx_second_3,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_DxRx_second_year_age.csv"),row.names = F)
rm(MIG_DxRx_second_3)



#### MIG_Dx_third:Prevalence of migraine during pregnancy when lookback==12 months(3 months) ####
print("Create algorithm MIG_Dx_third")
MIG_Dx_third<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_Dx_third"]
inc_col<-MIG_Dx_third[TYPE=="AND",STUDY_VARIABLES]

#set the pregnancy trimester for each pregnancy
if(length(inc_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col] & trimester == 3,include:=1]}else{pregnancy_d3_mig[,include:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm/final_d3/MIG_Dx_third_D3.rds"))

#export MIG_Dx_third
MIG_Dx_third_dt<-data.table(algorithm="MIG_Dx_third", no_diagnosed_pregnancies=pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id) & trimester == 3,.N])
MIG_Dx_third_dt[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_Dx_third_dt[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
MIG_Dx_third_dt[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_third_dt[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_third_dt[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_Dx_third_dt,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Dx_third.csv"),row.names = F)

#by maternal age_band
pregnancy_d3_mig[,age_band:=as.character(age_band)]
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id), .N, by=.(age_band)]
names(records)<-c("age_band","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id) & trimester == 3, by="age_band",.N]
names(total)<-c("age_band","no_pregnancies")
records<-merge.data.table(records,total,by="age_band",all=T)
MIG_Dx_third_1<-data.table(algorithm="MIG_Dx_third", records)
MIG_Dx_third_1[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_Dx_third_1<-MIG_Dx_third_1[order(age_band)]
rm(records,total)
MIG_Dx_third_1[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_Dx_third_1[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_Dx_third_1[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_Dx_third_1[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_third_1[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_third_1[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_Dx_third_1,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Dx_third_age.csv"),row.names = F)
rm(MIG_Dx_third_1)

#by year to year_group
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by="year_group", .N]
names(records)<-c("year_group","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id) & trimester == 3,by="year_group",.N]
names(total)<-c("year_group","no_pregnancies")
records<-merge.data.table(records,total,by="year_group",all=T)
MIG_Dx_third_2<-data.table(algorithm="MIG_Dx_third", records)
MIG_Dx_third_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_Dx_third_2<-MIG_Dx_third_2[order(year_group)]
rm(records,total)
MIG_Dx_third_2[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_Dx_third_2[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_Dx_third_2[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_Dx_third_2[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_third_2[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_third_2[lower_95_CI<0,lower_95_CI:=0]
fwrite(MIG_Dx_third_2,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Dx_third_year.csv"),row.names = F)
rm(MIG_Dx_third_2)

#by year group and maternal age_band
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by=c("year_group","age_band"), .N]
names(records)<-c("year_group","age_band", "no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id) & trimester == 3,by=c("year_group","age_band"),.N]
names(total)<-c("year_group","age_band","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","age_band"), all=T)
MIG_Dx_third_3<-data.table(algorithm="MIG_Dx_third", records)
MIG_Dx_third_3[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_Dx_third_3<-MIG_Dx_third_3[order(year_group,age_band)]
rm(records,total)
MIG_Dx_third_3[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_Dx_third_3[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_Dx_third_3[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_Dx_third_3[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_third_3[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_third_3[lower_95_CI<0,lower_95_CI:=0]
pregnancy_d3_mig[,include:=NULL]
rm(MIG_Dx_third)
fwrite(MIG_Dx_third_3,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Dx_third_year_age.csv"),row.names = F)
rm(MIG_Dx_third_3)

#### MIG_Rx_third:Prevalence of migraine during pregnancy when lookback==12 months(3 months) ####
print("Create algorithm MIG_Rx_third")
MIG_Rx_third<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_Rx_third"]
inc_col<-MIG_Rx_third[TYPE=="AND",STUDY_VARIABLES]

if(length(inc_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col] & trimester == 3,include:=1]}else{pregnancy_d3_mig[,include:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm/final_d3/MIG_Rx_third_D3.rds"))

#export MIG_Rx_third
MIG_Rx_third_dt<-data.table(algorithm="MIG_Rx_third", no_diagnosed_pregnancies=pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id) & trimester== 3,.N])
MIG_Rx_third_dt[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_Rx_third_dt[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
MIG_Rx_third_dt[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_third_dt[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_third_dt[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_Rx_third_dt,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Rx_third.csv"),row.names = F)

#by maternal age_band
pregnancy_d3_mig[,age_band:=as.character(age_band)]
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id), .N, by=.(age_band)]
names(records)<-c("age_band","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id) & trimester== 3, by="age_band",.N]
names(total)<-c("age_band","no_pregnancies")
records<-merge.data.table(records,total,by="age_band",all=T)
MIG_Rx_third_1<-data.table(algorithm="MIG_Rx_third", records)
MIG_Rx_third_1[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_Rx_third_1<-MIG_Rx_third_1[order(age_band)]
rm(records,total)
MIG_Rx_third_1[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_Rx_third_1[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_Rx_third_1[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_Rx_third_1[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_third_1[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_third_1[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_Rx_third_1,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Rx_third_age.csv"),row.names = F)
rm(MIG_Rx_third_1)


#by year to year_group
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by="year_group", .N]
names(records)<-c("year_group","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id) & trimester== 3,by="year_group",.N]
names(total)<-c("year_group","no_pregnancies")
records<-merge.data.table(records,total,by="year_group",all=T)
MIG_Rx_third_2<-data.table(algorithm="MIG_Rx_third", records)
MIG_Rx_third_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_Rx_third_2<-MIG_Rx_third_2[order(year_group)]
rm(records,total)
MIG_Rx_third_2[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_Rx_third_2[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_Rx_third_2[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_Rx_third_2[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_third_2[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_third_2[lower_95_CI<0,lower_95_CI:=0]
fwrite(MIG_Rx_third_2,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Rx_third_year.csv"),row.names = F)
rm(MIG_Rx_third_2)


#by year group and maternal age_band
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by=c("year_group","age_band"), .N]
names(records)<-c("year_group","age_band", "no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id) & trimester== 3,by=c("year_group","age_band"),.N]
names(total)<-c("year_group","age_band","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","age_band"), all=T)
MIG_Rx_third_3<-data.table(algorithm="MIG_Rx_third", records)
MIG_Rx_third_3[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_Rx_third_3<-MIG_Rx_third_3[order(year_group,age_band)]
rm(records,total)
MIG_Rx_third_3[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_Rx_third_3[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_Rx_third_3[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_Rx_third_3[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_third_3[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_third_3[lower_95_CI<0,lower_95_CI:=0]

pregnancy_d3_mig[,include:=NULL]
fwrite(MIG_Rx_third_3,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Rx_third_year_age.csv"),row.names = F)
rm(MIG_Rx_third_3)

#### MIG_DxRx_third:Prevalence of migraine at baseline when lookback==12 months(3 months) ####
print("Create algorithm MIG_DxRx_third")
MIG_DxRx_third<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_DxRx_third"]
alt_col<-MIG_DxRx_third[TYPE=="OR",STUDY_VARIABLES]

if(length(alt_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("|" , lapply(.SD,`>=`, 1)),.SDcols=alt_col] & trimester==3,alternative:=1]}else{pregnancy_d3_mig[,alternative:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm/final_d3/MIG_DxRx_third_D3.rds"))

#export MIG_Dx_a
MIG_DxRx_third_dt<-data.table(algorithm="MIG_DxRx_third", no_diagnosed_pregnancies=pregnancy_d3_mig[alternative==1 & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id) & trimester==3,.N])
MIG_DxRx_third_dt[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_DxRx_third_dt[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
MIG_DxRx_third_dt[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_third_dt[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_third_dt[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_DxRx_third_dt,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_DxRx_third.csv"),row.names = F)

#by maternal age_band
pregnancy_d3_mig[,age_band:=as.character(age_band)]
records<-pregnancy_d3_mig[alternative==1 & !duplicated(pregnancy_id), .N, by=.(age_band)]
names(records)<-c("age_band","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id)& trimester==3, by="age_band",.N]
names(total)<-c("age_band","no_pregnancies")
records<-merge.data.table(records,total,by="age_band",all=T)
MIG_DxRx_third_1<-data.table(algorithm="MIG_DxRx_third", records)
MIG_DxRx_third_1[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_DxRx_third_1<-MIG_DxRx_third_1[order(age_band)]
rm(records,total)
MIG_DxRx_third_1[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_DxRx_third_1[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_DxRx_third_1[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_DxRx_third_1[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_third_1[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_third_1[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_DxRx_third_1,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_DxRx_third_age.csv"),row.names = F)
rm(MIG_DxRx_third_1)

#by year to year_group
records<-pregnancy_d3_mig[alternative==1 & !duplicated(pregnancy_id),by="year_group", .N]
names(records)<-c("year_group","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id)& trimester==3,by="year_group",.N]
names(total)<-c("year_group","no_pregnancies")
records<-merge.data.table(records,total,by="year_group",all=T)
MIG_DxRx_third_2<-data.table(algorithm="MIG_DxRx_third", records)
MIG_DxRx_third_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_DxRx_third_2<-MIG_DxRx_third_2[order(year_group)]
rm(records,total)
MIG_DxRx_third_2[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_DxRx_third_2[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_DxRx_third_2[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_DxRx_third_2[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_third_2[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_third_2[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_DxRx_third_2,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_DxRx_third_year.csv"),row.names = F)
rm(MIG_DxRx_third_2)


#by year group and maternal age_band
records<-pregnancy_d3_mig[alternative==1 & !duplicated(pregnancy_id),by=c("year_group","age_band"), .N]
names(records)<-c("year_group","age_band", "no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id)& trimester==3,by=c("year_group","age_band"),.N]
names(total)<-c("year_group","age_band","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","age_band"), all=T)
MIG_DxRx_third_3<-data.table(algorithm="MIG_DxRx_third", records)
MIG_DxRx_third_3[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_DxRx_third_3<-MIG_DxRx_third_3[order(year_group,age_band)]
rm(records,total)
MIG_DxRx_third_3[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_DxRx_third_3[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_DxRx_third_3[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_DxRx_third_3[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_third_3[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_third_3[lower_95_CI<0,lower_95_CI:=0]

pregnancy_d3_mig[,alternative:=NULL]
fwrite(MIG_DxRx_third_3,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_DxRx_third_year_age.csv"),row.names = F)
rm(MIG_DxRx_third_3)


#### Save time info ####
date_running_end_04_d<-Sys.Date()
end_time_04_d<-Sys.time()

time_log_04_d<-data.table(DAP=data_access_provider_name,
                          Script="Step_04_d_migraine_algorithms.R", 
                          Start_date=date_running_start_04_d, 
                          End_date=date_running_end_04_d,
                          Time_elaspsed=format(end_time_04_d-initial_time_04_d, digits=2))
fwrite(time_log_04_d,paste0(output_dir,"/Time log/Step_04_d_time_log.csv"),row.names = F)
rm(time_log_04_d)
