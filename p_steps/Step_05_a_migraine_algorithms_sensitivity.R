initial_time_05_a<-Sys.time()
date_running_start_05_a<-Sys.Date()

####APPLY Migraine ALGORITHM####
algorithm_template<-fread(paste0(projectFolder, "/p_steps/parameters/algorithms.csv"))



#### MIG_DxRx_a:Prevalence of migraine at baseline when lookback==12 months(3 months) ####
print("Create algorithm MIG_DxRx_a_S")
MIG_DxRx_a<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_DxRx_a"]
alt_col<-MIG_DxRx_a[TYPE=="OR",STUDY_VARIABLES]

if(length(alt_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("|" , lapply(.SD,`>=`, 1)),.SDcols=alt_col],alternative:=1]}else{pregnancy_d3_mig[,alternative:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm_sensitivity/MIG_DxRx_a_D3.rds"))

#export MIG_Dx_a
MIG_DxRx_a_dt<-data.table(algorithm="MIG_DxRx_a_S", no_diagnosed_pregnancies=pregnancy_d3_mig[alternative==1 & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id),.N])
MIG_DxRx_a_dt[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_DxRx_a_dt[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
MIG_DxRx_a_dt[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_a_dt[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_a_dt[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_DxRx_a_dt,paste0(projectFolder,"/g_output/Migraine algorithm/Step_05_MIG_DxRx_a_S.csv"),row.names = F)

#by maternal age
pregnancy_d3_mig[,maternal_age:=as.character(maternal_age)]
records<-pregnancy_d3_mig[alternative==1 & !duplicated(pregnancy_id), .N, by=.(maternal_age)]
names(records)<-c("maternal_age","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id), by="maternal_age",.N]
names(total)<-c("maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by="maternal_age",all=T)
MIG_DxRx_a_1<-data.table(algorithm="MIG_DxRx_a_S", records)
MIG_DxRx_a_1[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_DxRx_a_1<-MIG_DxRx_a_1[order(maternal_age)]
rm(records,total)
MIG_DxRx_a_1[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_DxRx_a_1[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_DxRx_a_1[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_DxRx_a_1[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_a_1[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_a_1[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_DxRx_a_1,paste0(projectFolder,"/g_output/Migraine algorithm/Step_05_MIG_DxRx_a_age_S.csv"),row.names = F)
rm(MIG_DxRx_a_1)

#by year to year_group
records<-pregnancy_d3_mig[alternative==1 & !duplicated(pregnancy_id),by="year_group", .N]
names(records)<-c("year_group","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by="year_group",.N]
names(total)<-c("year_group","no_pregnancies")
records<-merge.data.table(records,total,by="year_group",all=T)
MIG_DxRx_a_2<-data.table(algorithm="MIG_DxRx_a_S", records)
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

fwrite(MIG_DxRx_a_2,paste0(projectFolder,"/g_output/Migraine algorithm/Step_05_MIG_DxRx_a_year_S.csv"),row.names = F)
rm(MIG_DxRx_a_2)


#by year group and maternal age
records<-pregnancy_d3_mig[alternative==1 & !duplicated(pregnancy_id),by=c("year_group","maternal_age"), .N]
names(records)<-c("year_group","maternal_age", "no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by=c("year_group","maternal_age"),.N]
names(total)<-c("year_group","maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","maternal_age"), all=T)
MIG_DxRx_a_3<-data.table(algorithm="MIG_DxRx_a_S", records)
MIG_DxRx_a_3[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_DxRx_a_3<-MIG_DxRx_a_3[order(year_group,maternal_age)]
rm(records,total)
MIG_DxRx_a_3[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_DxRx_a_3[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_DxRx_a_3[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_DxRx_a_3[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_a_3[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_a_3[lower_95_CI<0,lower_95_CI:=0]

pregnancy_d3_mig[,alternative:=NULL]
fwrite(MIG_DxRx_a_3,paste0(projectFolder,"/g_output/Migraine algorithm/Step_05_MIG_DxRx_a_year_age_S.csv"),row.names = F)
rm(MIG_DxRx_a_3)


#### MIG_DxRx_b:Prevalence of migraine at baseline when lookback==3 months ####
print("Create algorithm MIG_DxRx_b_S")
MIG_DxRx_b<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_DxRx_b"]
alt_col<-MIG_DxRx_b[TYPE=="OR",STUDY_VARIABLES]

if(length(alt_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("|" , lapply(.SD,`>=`, 1)),.SDcols=alt_col],alternative:=1]}else{pregnancy_d3_mig[,alternative:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm_sensitivity/MIG_DxRx_b_D3.rds"))

#export MIG_Dx_a
MIG_DxRx_b_dt<-data.table(algorithm="MIG_DxRx_b_S", no_diagnosed_pregnancies=pregnancy_d3_mig[alternative==1 & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id),.N])
MIG_DxRx_b_dt[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_DxRx_b_dt[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
MIG_DxRx_b_dt[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_b_dt[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_b_dt[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_DxRx_b_dt,paste0(projectFolder,"/g_output/Migraine algorithm/Step_05_MIG_DxRx_b_S.csv"),row.names = F)

#by maternal age
pregnancy_d3_mig[,maternal_age:=as.character(maternal_age)]
records<-pregnancy_d3_mig[alternative==1 & !duplicated(pregnancy_id), .N, by=.(maternal_age)]
names(records)<-c("maternal_age","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id), by="maternal_age",.N]
names(total)<-c("maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by="maternal_age",all=T)
MIG_DxRx_b_1<-data.table(algorithm="MIG_DxRx_b_S", records)
MIG_DxRx_b_1[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_DxRx_b_1<-MIG_DxRx_b_1[order(maternal_age)]
rm(records,total)
MIG_DxRx_b_1[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_DxRx_b_1[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_DxRx_b_1[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_DxRx_b_1[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_b_1[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_b_1[lower_95_CI<0,lower_95_CI:=0]
fwrite(MIG_DxRx_b_1,paste0(projectFolder,"/g_output/Migraine algorithm/Step_05_MIG_DxRx_b_age_S.csv"),row.names = F)
rm(MIG_DxRx_b_1)


#by year to year_group
records<-pregnancy_d3_mig[alternative==1 & !duplicated(pregnancy_id),by="year_group", .N]
names(records)<-c("year_group","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by="year_group",.N]
names(total)<-c("year_group","no_pregnancies")
records<-merge.data.table(records,total,by="year_group",all=T)
MIG_DxRx_b_2<-data.table(algorithm="MIG_DxRx_b_S", records)
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

fwrite(MIG_DxRx_b_2,paste0(projectFolder,"/g_output/Migraine algorithm/Step_05_MIG_DxRx_b_year_S.csv"),row.names = F)
rm(MIG_DxRx_b_2)


#by year group and maternal age
records<-pregnancy_d3_mig[alternative==1 & !duplicated(pregnancy_id),by=c("year_group","maternal_age"), .N]
names(records)<-c("year_group","maternal_age", "no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by=c("year_group","maternal_age"),.N]
names(total)<-c("year_group","maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","maternal_age"), all=T)
MIG_DxRx_b_3<-data.table(algorithm="MIG_DxRx_b_S", records)
MIG_DxRx_b_3[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_DxRx_b_3<-MIG_DxRx_b_3[order(year_group,maternal_age)]
rm(records,total)
MIG_DxRx_b_3[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_DxRx_b_3[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_DxRx_b_3[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_DxRx_b_3[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_b_3[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_b_3[lower_95_CI<0,lower_95_CI:=0]

pregnancy_d3_mig[,alternative:=NULL]
fwrite(MIG_DxRx_b_3,paste0(projectFolder,"/g_output/Migraine algorithm/Step_05_MIG_DxRx_b_year_age_S.csv"),row.names = F)
rm(MIG_DxRx_b_3)

#### MIG_DxRx_during:Prevalence of migraine at baseline when lookback==12 months(3 months) ####
print("Create algorithm MIG_DxRx_during_S")
MIG_DxRx_during<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_DxRx_during"]
alt_col<-MIG_DxRx_during[TYPE=="OR",STUDY_VARIABLES]

if(length(alt_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("|" , lapply(.SD,`>=`, 1)),.SDcols=alt_col],alternative:=1]}else{pregnancy_d3_mig[,alternative:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm_sensitivity/MIG_DxRx_during_D3.rds"))

#export MIG_Dx_a
MIG_DxRx_during_dt<-data.table(algorithm="MIG_DxRx_during_S", no_diagnosed_pregnancies=pregnancy_d3_mig[alternative==1 & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id),.N])
MIG_DxRx_during_dt[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_DxRx_during_dt[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
MIG_DxRx_during_dt[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_during_dt[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_during_dt[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_DxRx_during_dt,paste0(projectFolder,"/g_output/Migraine algorithm/Step_05_MIG_DxRx_during_S.csv"),row.names = F)

#by maternal age
pregnancy_d3_mig[,maternal_age:=as.character(maternal_age)]
records<-pregnancy_d3_mig[alternative==1 & !duplicated(pregnancy_id), .N, by=.(maternal_age)]
names(records)<-c("maternal_age","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id), by="maternal_age",.N]
names(total)<-c("maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by="maternal_age",all=T)
MIG_DxRx_during_1<-data.table(algorithm="MIG_DxRx_during_S", records)
MIG_DxRx_during_1[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_DxRx_during_1<-MIG_DxRx_during_1[order(maternal_age)]
rm(records,total)
MIG_DxRx_during_1[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_DxRx_during_1[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_DxRx_during_1[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_DxRx_during_1[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_during_1[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_during_1[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_DxRx_during_1,paste0(projectFolder,"/g_output/Migraine algorithm/Step_05_MIG_DxRx_during_age_S.csv"),row.names = F)
rm(MIG_DxRx_during_1)

#by year to year_group
records<-pregnancy_d3_mig[alternative==1 & !duplicated(pregnancy_id),by="year_group", .N]
names(records)<-c("year_group","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by="year_group",.N]
names(total)<-c("year_group","no_pregnancies")
records<-merge.data.table(records,total,by="year_group",all=T)
MIG_DxRx_during_2<-data.table(algorithm="MIG_DxRx_during_S", records)
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

fwrite(MIG_DxRx_during_2,paste0(projectFolder,"/g_output/Migraine algorithm/Step_05_MIG_DxRx_during_year_S.csv"),row.names = F)
rm(MIG_DxRx_during_2)


#by year group and maternal age
records<-pregnancy_d3_mig[alternative==1 & !duplicated(pregnancy_id),by=c("year_group","maternal_age"), .N]
names(records)<-c("year_group","maternal_age", "no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by=c("year_group","maternal_age"),.N]
names(total)<-c("year_group","maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","maternal_age"), all=T)
MIG_DxRx_during_3<-data.table(algorithm="MIG_DxRx_during_S", records)
MIG_DxRx_during_3[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_DxRx_during_3<-MIG_DxRx_during_3[order(year_group,maternal_age)]
rm(records,total)
MIG_DxRx_during_3[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_DxRx_during_3[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_DxRx_during_3[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_DxRx_during_3[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_during_3[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_during_3[lower_95_CI<0,lower_95_CI:=0]

pregnancy_d3_mig[,alternative:=NULL]
fwrite(MIG_DxRx_during_3,paste0(projectFolder,"/g_output/Migraine algorithm/Step_05_MIG_DxRx_during_year_age_S.csv"),row.names = F)
rm(MIG_DxRx_during_3)

#### Select the trimester timepoint ####
pregnancy_d3_mig[,dif:=pregnancy_end_date-pregnancy_start_date]
pregnancy_d3_mig[dif<97, trimester:=0]
pregnancy_d3_mig[dif>=97, trimester:=1]
pregnancy_d3_mig[dif>=195, trimester:=2]
pregnancy_d3_mig[dif>=280, trimester:=3]
pregnancy_d3_mig[,dif:=NULL]
#### MIG_DxRx_first:Prevalence of migraine at baseline when lookback==12 months(3 months) ####
print("Create algorithm MIG_DxRx_first")
MIG_DxRx_first<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_DxRx_first"]
alt_col<-MIG_DxRx_first[TYPE=="OR",STUDY_VARIABLES]

if(length(alt_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("|" , lapply(.SD,`>=`, 1)),.SDcols=alt_col] & trimester%in% c(1,2,3),alternative:=1]}else{pregnancy_d3_mig[,alternative:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm_sensitivity/MIG_DxRx_first_D3.rds"))

#export MIG_Dx_a
MIG_DxRx_first_dt<-data.table(algorithm="MIG_DxRx_first_S", no_diagnosed_pregnancies=pregnancy_d3_mig[alternative==1 & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id) & trimester%in% c(1,2,3),.N])
MIG_DxRx_first_dt[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_DxRx_first_dt[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
MIG_DxRx_first_dt[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_first_dt[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_first_dt[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_DxRx_first_dt,paste0(projectFolder,"/g_output/Migraine algorithm/Step_05_MIG_DxRx_first_S.csv"),row.names = F)

#by maternal age
pregnancy_d3_mig[,maternal_age:=as.character(maternal_age)]
records<-pregnancy_d3_mig[alternative==1 & !duplicated(pregnancy_id), .N, by=.(maternal_age)]
names(records)<-c("maternal_age","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id)& trimester%in% c(1,2,3), by="maternal_age",.N]
names(total)<-c("maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by="maternal_age",all=T)
MIG_DxRx_first_1<-data.table(algorithm="MIG_DxRx_first_S", records)
MIG_DxRx_first_1[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_DxRx_first_1<-MIG_DxRx_first_1[order(maternal_age)]
rm(records,total)
MIG_DxRx_first_1[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_DxRx_first_1[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_DxRx_first_1[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_DxRx_first_1[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_first_1[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_first_1[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_DxRx_first_1,paste0(projectFolder,"/g_output/Migraine algorithm/Step_05_MIG_DxRx_first_age.csv"),row.names = F)
rm(MIG_DxRx_first_1)

#by year to year_group
records<-pregnancy_d3_mig[alternative==1 & !duplicated(pregnancy_id),by="year_group", .N]
names(records)<-c("year_group","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id)& trimester %in% c(1,2,3),by="year_group",.N]
names(total)<-c("year_group","no_pregnancies")
records<-merge.data.table(records,total,by="year_group",all=T)
MIG_DxRx_first_2<-data.table(algorithm="MIG_DxRx_first_S", records)
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

fwrite(MIG_DxRx_first_2,paste0(projectFolder,"/g_output/Migraine algorithm/Step_05_MIG_DxRx_first_year_S.csv"),row.names = F)
rm(MIG_DxRx_first_2)


#by year group and maternal age
records<-pregnancy_d3_mig[alternative==1 & !duplicated(pregnancy_id),by=c("year_group","maternal_age"), .N]
names(records)<-c("year_group","maternal_age", "no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id)& trimester %in% c(1,2,3),by=c("year_group","maternal_age"),.N]
names(total)<-c("year_group","maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","maternal_age"), all=T)
MIG_DxRx_first_3<-data.table(algorithm="MIG_DxRx_first_S", records)
MIG_DxRx_first_3[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_DxRx_first_3<-MIG_DxRx_first_3[order(year_group,maternal_age)]
rm(records,total)
MIG_DxRx_first_3[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_DxRx_first_3[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_DxRx_first_3[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_DxRx_first_3[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_first_3[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_first_3[lower_95_CI<0,lower_95_CI:=0]

pregnancy_d3_mig[,alternative:=NULL]
fwrite(MIG_DxRx_first_3,paste0(projectFolder,"/g_output/Migraine algorithm/Step_05_MIG_DxRx_first_year_age_S.csv"),row.names = F)
rm(MIG_DxRx_first_3)

#### MIG_DxRx_second:Prevalence of migraine at baseline when lookback==12 months(3 months) ####
print("Create algorithm MIG_DxRx_second")
MIG_DxRx_second<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_DxRx_second"]
alt_col<-MIG_DxRx_second[TYPE=="OR",STUDY_VARIABLES]

if(length(alt_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("|" , lapply(.SD,`>=`, 1)),.SDcols=alt_col] & trimester%in% c(2,3),alternative:=1]}else{pregnancy_d3_mig[,alternative:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm_sensitivity/MIG_DxRx_second_D3.rds"))

#export MIG_Dx_a
MIG_DxRx_second_dt<-data.table(algorithm="MIG_DxRx_second_S", no_diagnosed_pregnancies=pregnancy_d3_mig[alternative==1 & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id) & trimester%in% c(2,3),.N])
MIG_DxRx_second_dt[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_DxRx_second_dt[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
MIG_DxRx_second_dt[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_second_dt[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_second_dt[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_DxRx_second_dt,paste0(projectFolder,"/g_output/Migraine algorithm/Step_05_MIG_DxRx_second_S.csv"),row.names = F)

#by maternal age
pregnancy_d3_mig[,maternal_age:=as.character(maternal_age)]
records<-pregnancy_d3_mig[alternative==1 & !duplicated(pregnancy_id), .N, by=.(maternal_age)]
names(records)<-c("maternal_age","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id)& trimester%in% c(2,3), by="maternal_age",.N]
names(total)<-c("maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by="maternal_age",all=T)
MIG_DxRx_second_1<-data.table(algorithm="MIG_DxRx_second_S", records)
MIG_DxRx_second_1[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_DxRx_second_1<-MIG_DxRx_second_1[order(maternal_age)]
rm(records,total)
MIG_DxRx_second_1[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_DxRx_second_1[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_DxRx_second_1[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_DxRx_second_1[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_second_1[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_second_1[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_DxRx_second_1,paste0(projectFolder,"/g_output/Migraine algorithm/Step_05_MIG_DxRx_second_age_S.csv"),row.names = F)
rm(MIG_DxRx_second_1)

#by year to year_group
records<-pregnancy_d3_mig[alternative==1 & !duplicated(pregnancy_id),by="year_group", .N]
names(records)<-c("year_group","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id)& trimester%in% c(2,3),by="year_group",.N]
names(total)<-c("year_group","no_pregnancies")
records<-merge.data.table(records,total,by="year_group",all=T)
MIG_DxRx_second_2<-data.table(algorithm="MIG_DxRx_second_S", records)
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

fwrite(MIG_DxRx_second_2,paste0(projectFolder,"/g_output/Migraine algorithm/Step_05_MIG_DxRx_second_year_S.csv"),row.names = F)
rm(MIG_DxRx_second_2)


#by year group and maternal age
records<-pregnancy_d3_mig[alternative==1 & !duplicated(pregnancy_id),by=c("year_group","maternal_age"), .N]
names(records)<-c("year_group","maternal_age", "no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id)& trimester%in% c(2,3),by=c("year_group","maternal_age"),.N]
names(total)<-c("year_group","maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","maternal_age"), all=T)
MIG_DxRx_second_3<-data.table(algorithm="MIG_DxRx_second_S", records)
MIG_DxRx_second_3[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_DxRx_second_3<-MIG_DxRx_second_3[order(year_group,maternal_age)]
rm(records,total)
MIG_DxRx_second_3[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_DxRx_second_3[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_DxRx_second_3[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_DxRx_second_3[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_second_3[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_second_3[lower_95_CI<0,lower_95_CI:=0]

pregnancy_d3_mig[,alternative:=NULL]
fwrite(MIG_DxRx_second_3,paste0(projectFolder,"/g_output/Migraine algorithm/Step_05_MIG_DxRx_second_year_age_S.csv"),row.names = F)
rm(MIG_DxRx_second_3)

#### MIG_DxRx_third:Prevalence of migraine at baseline when lookback==12 months(3 months) ####
print("Create algorithm MIG_DxRx_third")
MIG_DxRx_third<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_DxRx_third"]
alt_col<-MIG_DxRx_third[TYPE=="OR",STUDY_VARIABLES]

if(length(alt_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("|" , lapply(.SD,`>=`, 1)),.SDcols=alt_col] & trimester==3,alternative:=1]}else{pregnancy_d3_mig[,alternative:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm_sensitivity/MIG_DxRx_third_D3.rds"))

#export MIG_Dx_a
MIG_DxRx_third_dt<-data.table(algorithm="MIG_DxRx_third_S", no_diagnosed_pregnancies=pregnancy_d3_mig[alternative==1 & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id) & trimester==3,.N])
MIG_DxRx_third_dt[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_DxRx_third_dt[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
MIG_DxRx_third_dt[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_third_dt[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_third_dt[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_DxRx_third_dt,paste0(projectFolder,"/g_output/Migraine algorithm/Step_05_MIG_DxRx_third_S.csv"),row.names = F)

#by maternal age
pregnancy_d3_mig[,maternal_age:=as.character(maternal_age)]
records<-pregnancy_d3_mig[alternative==1 & !duplicated(pregnancy_id), .N, by=.(maternal_age)]
names(records)<-c("maternal_age","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id)& trimester==3, by="maternal_age",.N]
names(total)<-c("maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by="maternal_age",all=T)
MIG_DxRx_third_1<-data.table(algorithm="MIG_DxRx_third_S", records)
MIG_DxRx_third_1[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_DxRx_third_1<-MIG_DxRx_third_1[order(maternal_age)]
rm(records,total)
MIG_DxRx_third_1[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_DxRx_third_1[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_DxRx_third_1[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_DxRx_third_1[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_third_1[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_third_1[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_DxRx_third_1,paste0(projectFolder,"/g_output/Migraine algorithm/Step_05_MIG_DxRx_third_age_S.csv"),row.names = F)
rm(MIG_DxRx_third_1)

#by year to year_group
records<-pregnancy_d3_mig[alternative==1 & !duplicated(pregnancy_id),by="year_group", .N]
names(records)<-c("year_group","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id)& trimester==3,by="year_group",.N]
names(total)<-c("year_group","no_pregnancies")
records<-merge.data.table(records,total,by="year_group",all=T)
MIG_DxRx_third_2<-data.table(algorithm="MIG_DxRx_third_S", records)
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

fwrite(MIG_DxRx_third_2,paste0(projectFolder,"/g_output/Migraine algorithm/Step_05_MIG_DxRx_third_year_S.csv"),row.names = F)
rm(MIG_DxRx_third_2)


#by year group and maternal age
records<-pregnancy_d3_mig[alternative==1 & !duplicated(pregnancy_id),by=c("year_group","maternal_age"), .N]
names(records)<-c("year_group","maternal_age", "no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id)& trimester==3,by=c("year_group","maternal_age"),.N]
names(total)<-c("year_group","maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","maternal_age"), all=T)
MIG_DxRx_third_3<-data.table(algorithm="MIG_DxRx_third_S", records)
MIG_DxRx_third_3[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_DxRx_third_3<-MIG_DxRx_third_3[order(year_group,maternal_age)]
rm(records,total)
MIG_DxRx_third_3[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,3)]
MIG_DxRx_third_3[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_DxRx_third_3[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_DxRx_third_3[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_third_3[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_third_3[lower_95_CI<0,lower_95_CI:=0]

pregnancy_d3_mig[,alternative:=NULL]
fwrite(MIG_DxRx_third_3,paste0(projectFolder,"/g_output/Migraine algorithm/Step_05_MIG_DxRx_third_year_age_S.csv"),row.names = F)
rm(MIG_DxRx_third_3)


#### Save time info ####
date_running_end_05_a<-Sys.Date()
end_time_05_a<-Sys.time()

time_log_05_a<-data.table(DAP=data_access_provider_name,
                          Script="Step_05_a_migraine_algorithms_sensitivity.R", 
                          Start_date=date_running_start_05_a, 
                          End_date=date_running_end_05_a,
                          Time_elaspsed=format(end_time_05_a-initial_time_05_a, digits=2))
fwrite(time_log_05_a,paste0(output_dir,"/Time log/Step_05_a_time_log.csv"),row.names = F)
rm(time_log_05_a)

