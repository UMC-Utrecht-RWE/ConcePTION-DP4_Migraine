initial_time_04_e<-Sys.time()
date_running_start_04_e<-Sys.Date()


#Identify all events needed to create the migraine type algorithms

#MIG_T1: MG_NO_AURA, baseline and during pregnancy
#MIG_T2: MG_AURA, baseline and during pregnancy
#MIG_T3: MG_STACOMP, baseline and during pregnancy
#MIG_T4: MG_OTHER, baseline and during pregnancy
#MIG_T5: MG_UPC, baseline and during pregnancy
#MIG_T6: MG_UNSP, baseline and during pregnancy


####APPLY Migraine ALGORITHM####
algorithm_template<-fread(paste0(projectFolder, "/p_steps/parameters/algorithms.csv"))
#### MIG_T1_a:Prevalence of MG_NO_AURA at baseline when lookback==12 months(3 months) ####
print("Create algorithm MIG_T1_a")
MIG_T1_a<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_T1_a"]
inc_col<-MIG_T1_a[TYPE=="AND",STUDY_VARIABLES]

if(length(inc_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col],include:=1]}else{pregnancy_d3_mig[,include:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm/final_d3/MIG_T1_a_D3.rds"))

#export MIG_T1_a
MIG_T1_a_dt<-data.table(algorithm="MIG_T1_a", no_diagnosed_pregnancies=pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id),.N])
MIG_T1_a_dt[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T1_a_dt[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
MIG_T1_a_dt[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T1_a_dt[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T1_a_dt[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_T1_a_dt,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T1_a.csv"),row.names = F)


#number of pregnancies with baseline migraine
pregnancy_d3_mig[MG_baseline>=1 | Migraine_medicines_baseline>=1,preg_baseline:=1]
MIG_T1_a_coh<-data.table(algorithm="MIG_T1_a", no_diagnosed_pregnancies=pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id) & preg_baseline==1,.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id) & preg_baseline==1,.N])
MIG_T1_a_coh[,percentage:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T1_a_coh[,denominator:="Number of pregnancies with baseline migraine(MG diagnoses or Triptan prescription), lookback= 365 days(90 days/75 days)"]

#by maternal age
pregnancy_d3_mig[,maternal_age:=as.character(maternal_age)]
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id), .N, by=.(maternal_age)]
names(records)<-c("maternal_age","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id), by="maternal_age",.N]
names(total)<-c("maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by="maternal_age",all=T)
MIG_T1_a_1<-data.table(algorithm="MIG_T1_a", records)
MIG_T1_a_1[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_T1_a_1<-MIG_T1_a_1[order(maternal_age)]
rm(records,total)
MIG_T1_a_1[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T1_a_1[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_T1_a_1[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_T1_a_1[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T1_a_1[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T1_a_1[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_T1_a_1,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T1_a_age.csv"),row.names = F)
rm(MIG_T1_a_1)

#by year
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by="year", .N]
names(records)<-c("year","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by="year",.N]
names(total)<-c("year","no_pregnancies")
records<-merge.data.table(records,total,by="year",all=T)
MIG_T1_a_2<-data.table(algorithm="MIG_T1_a", records)
MIG_T1_a_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_T1_a_2<-MIG_T1_a_2[order(year)]
rm(records,total)
MIG_T1_a_2[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T1_a_2[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_T1_a_2[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_T1_a_2[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T1_a_2[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T1_a_2[lower_95_CI<0,lower_95_CI:=0]
fwrite(MIG_T1_a_2,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T1_a_year.csv"),row.names = F)
rm(MIG_T1_a_2)

#number of pregnnacies with baseline migraine
records_coh<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id) & preg_baseline==1,by="year",.N]
names(records_coh)<-c("year","no_diagnosed_pregnancies")
total_coh<-pregnancy_d3_mig[!duplicated(pregnancy_id) & preg_baseline==1,by="year",.N]
names(total_coh)<-c("year","no_pregnancies")
records_coh<-merge.data.table(records_coh,total_coh,by="year",all=T)
MIG_T1_a_coh_2<-data.table(algorithm="MIG_T1_a", records_coh)
MIG_T1_a_coh_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_T1_a_coh_2[,percentage:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T1_a_coh_2[,denominator:="Number of pregnancies with baseline migraine(MG diagnoses or Triptan prescription), lookback= 365 days(90 days/75 days) by year"]

#by year group and maternal age
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by=c("year_group","maternal_age"), .N]
names(records)<-c("year_group","maternal_age", "no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by=c("year_group","maternal_age"),.N]
names(total)<-c("year_group","maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","maternal_age"), all=T)
MIG_T1_a_3<-data.table(algorithm="MIG_T1_a", records)
MIG_T1_a_3[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_T1_a_3<-MIG_T1_a_3[order(year_group,maternal_age)]
rm(records,total)
MIG_T1_a_3[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T1_a_3[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_T1_a_3[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_T1_a_3[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T1_a_3[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T1_a_3[lower_95_CI<0,lower_95_CI:=0]
pregnancy_d3_mig[,include:=NULL]
rm(MIG_T1_a)
fwrite(MIG_T1_a_3,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T1_a_year_age.csv"),row.names = F)
rm(MIG_T1_a_3)

pregnancy_d3_mig[,preg_baseline:=NULL]
#### MIG_T1_b:Prevalence of MG_NO_AURA at baseline when lookback==3 months ####
print("Create algorithm MIG_T1_b")
MIG_T1_b<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_T1_b"]
inc_col<-MIG_T1_b[TYPE=="AND",STUDY_VARIABLES]

if(length(inc_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col],include:=1]}else{pregnancy_d3_mig[,include:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm/final_d3/MIG_T1_b_D3.rds"))


#export MIG_T1_b
MIG_T1_b_dt<-data.table(algorithm="MIG_T1_b", no_diagnosed_pregnancies=pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id),.N])
MIG_T1_b_dt[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T1_b_dt[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
MIG_T1_b_dt[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T1_b_dt[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T1_b_dt[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_T1_b_dt,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T1_b.csv"),row.names = F)

#number of pregnancies with baseline migraine
pregnancy_d3_mig[MG_baseline_2>=1 | Migraine_medicines_baseline_2>=1,preg_baseline_2:=1]
MIG_T1_b_coh<-data.table(algorithm="MIG_T1_b", no_diagnosed_pregnancies=pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id) & preg_baseline_2==1,.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id) & preg_baseline_2==1,.N])
MIG_T1_b_coh[,percentage:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T1_b_coh[,denominator:="Number of pregnancies with baseline migraine(MG diagnoses or Triptan prescription), lookback= 90 days/75 days"]

#by maternal age
pregnancy_d3_mig[,maternal_age:=as.character(maternal_age)]
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id), .N, by=.(maternal_age)]
names(records)<-c("maternal_age","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id), by="maternal_age",.N]
names(total)<-c("maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by="maternal_age",all=T)
MIG_T1_b_1<-data.table(algorithm="MIG_T1_b", records)
MIG_T1_b_1[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_T1_b_1<-MIG_T1_b_1[order(maternal_age)]
rm(records,total)
MIG_T1_b_1[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T1_b_1[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_T1_b_1[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_T1_b_1[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T1_b_1[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T1_b_1[lower_95_CI<0,lower_95_CI:=0]
fwrite(MIG_T1_b_1,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T1_b_age.csv"),row.names = F)
rm(MIG_T1_b_1)


#by year
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by="year", .N]
names(records)<-c("year","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by="year",.N]
names(total)<-c("year","no_pregnancies")
records<-merge.data.table(records,total,by="year",all=T)
MIG_T1_b_2<-data.table(algorithm="MIG_T1_b", records)
MIG_T1_b_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_T1_b_2<-MIG_T1_b_2[order(year)]
rm(records,total)
MIG_T1_b_2[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T1_b_2[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_T1_b_2[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_T1_b_2[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T1_b_2[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T1_b_2[lower_95_CI<0,lower_95_CI:=0]
fwrite(MIG_T1_b_2,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T1_b_year.csv"),row.names = F)
rm(MIG_T1_b_2)

#number of pregnnacies with baseline migraine
records_coh<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id) & preg_baseline_2==1,by="year",.N]
names(records_coh)<-c("year","no_diagnosed_pregnancies")
total_coh<-pregnancy_d3_mig[!duplicated(pregnancy_id) & preg_baseline_2==1,by="year",.N]
names(total_coh)<-c("year","no_pregnancies")
records_coh<-merge.data.table(records_coh,total_coh,by="year",all=T)
MIG_T1_b_coh_2<-data.table(algorithm="MIG_T1_b", records_coh)
MIG_T1_b_coh_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_T1_b_coh_2[,percentage:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T1_b_coh_2[,denominator:="Number of pregnancies with baseline migraine(MG diagnoses or Triptan prescription), lookback= 90 days/75 days by year"]

#by year group and maternal age
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by=c("year_group","maternal_age"), .N]
names(records)<-c("year_group","maternal_age", "no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by=c("year_group","maternal_age"),.N]
names(total)<-c("year_group","maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","maternal_age"), all=T)
MIG_T1_b_3<-data.table(algorithm="MIG_T1_b", records)
MIG_T1_b_3[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_T1_b_3<-MIG_T1_b_3[order(year_group,maternal_age)]
rm(records,total)
MIG_T1_b_3[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T1_b_3[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_T1_b_3[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_T1_b_3[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T1_b_3[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T1_b_3[lower_95_CI<0,lower_95_CI:=0]
pregnancy_d3_mig[,include:=NULL]
fwrite(MIG_T1_b_3,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T1_b_year_age.csv"),row.names = F)
rm(MIG_T1_b_3)

pregnancy_d3_mig[,preg_baseline_2:=NULL]
#### MIG_T1_during:Prevalence of MG_NO_AURA during pregnancy when lookback==12 months(3 months) ####
print("Create algorithm MIG_T1_during")
MIG_T1_during<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_T1_during"]
inc_col<-MIG_T1_during[TYPE=="AND",STUDY_VARIABLES]

if(length(inc_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col],include:=1]}else{pregnancy_d3_mig[,include:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm/final_d3/MIG_T1_during_D3.rds"))

#export MIG_T1_during
MIG_T1_during_dt<-data.table(algorithm="MIG_T1_during", no_diagnosed_pregnancies=pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id),.N])
MIG_T1_during_dt[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T1_during_dt[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
MIG_T1_during_dt[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T1_during_dt[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T1_during_dt[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_T1_during_dt,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T1_during.csv"),row.names = F)

#number of pregnancies with baseline migraine
pregnancy_d3_mig[MG_during>=1 | Migraine_medicines_during>=1,preg_during:=1]
MIG_T1_during_coh<-data.table(algorithm="MIG_T1_during", no_diagnosed_pregnancies=pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id) & preg_during==1,.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id) & preg_during==1,.N])
MIG_T1_during_coh[,percentage:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T1_during_coh[,denominator:="Number of pregnancies with migraine(MG diagnoses or Triptan prescription) during pregnancy"]

#by maternal age
pregnancy_d3_mig[,maternal_age:=as.character(maternal_age)]
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id), .N, by=.(maternal_age)]
names(records)<-c("maternal_age","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id), by="maternal_age",.N]
names(total)<-c("maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by="maternal_age",all=T)
MIG_T1_during_1<-data.table(algorithm="MIG_T1_during", records)
MIG_T1_during_1[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_T1_during_1<-MIG_T1_during_1[order(maternal_age)]
rm(records,total)
MIG_T1_during_1[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T1_during_1[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_T1_during_1[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_T1_during_1[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T1_during_1[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T1_during_1[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_T1_during_1,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T1_during_age.csv"),row.names = F)
rm(MIG_T1_during_1)

#by year
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by="year", .N]
names(records)<-c("year","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by="year",.N]
names(total)<-c("year","no_pregnancies")
records<-merge.data.table(records,total,by="year",all=T)
MIG_T1_during_2<-data.table(algorithm="MIG_T1_during", records)
MIG_T1_during_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_T1_during_2<-MIG_T1_during_2[order(year)]
rm(records,total)
MIG_T1_during_2[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T1_during_2[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_T1_during_2[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_T1_during_2[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T1_during_2[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T1_during_2[lower_95_CI<0,lower_95_CI:=0]
fwrite(MIG_T1_during_2,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T1_during_year.csv"),row.names = F)
rm(MIG_T1_during_2)

#number of pregnnacies with migraine
records_coh<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id) & preg_during==1,by="year",.N]
names(records_coh)<-c("year","no_diagnosed_pregnancies")
total_coh<-pregnancy_d3_mig[!duplicated(pregnancy_id) & preg_during==1,by="year",.N]
names(total_coh)<-c("year","no_pregnancies")
records_coh<-merge.data.table(records_coh,total_coh,by="year",all=T)
MIG_T1_during_coh_2<-data.table(algorithm="MIG_T1_during", records_coh)
MIG_T1_during_coh_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_T1_during_coh_2[,percentage:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T1_during_coh_2[,denominator:="Number of pregnancies with migraine(MG diagnoses or Triptan prescription) during pregnancy by year"]

#by year group and maternal age
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by=c("year_group","maternal_age"), .N]
names(records)<-c("year_group","maternal_age", "no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by=c("year_group","maternal_age"),.N]
names(total)<-c("year_group","maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","maternal_age"), all=T)
MIG_T1_during_3<-data.table(algorithm="MIG_T1_during", records)
MIG_T1_during_3[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_T1_during_3<-MIG_T1_during_3[order(year_group,maternal_age)]
rm(records,total)
MIG_T1_during_3[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T1_during_3[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_T1_during_3[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_T1_during_3[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T1_during_3[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T1_during_3[lower_95_CI<0,lower_95_CI:=0]
pregnancy_d3_mig[,include:=NULL]
rm(MIG_T1_during)
fwrite(MIG_T1_during_3,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T1_during_year_age.csv"),row.names = F)
rm(MIG_T1_during_3)

pregnancy_d3_mig[,preg_during:=NULL]
#### MIG_T1_both:Proportion of pregnancies having the same type MG_NO_AURA during pregnancy and before when lookback==12 months(3 months) ####
print("Create algorithm MIG_T1_both")

pregnancy_d3_mig[MG_NO_AURA_baseline>=1 & MG_AURA_during>=1, both:=1]
pregnancy_d3_mig[MG_NO_AURA_baseline>=1 | MG_AURA_during>=1, total:=1]

#number of pregnancies having the same type in baseline and during
no_preg_both<-pregnancy_d3_mig[both==1 & !duplicated(pregnancy_id), .N]
#number of pregnancies having the type of interest in baseline or during
no_preg_tot<-pregnancy_d3_mig[total==1 & !duplicated(pregnancy_id), .N]
percentage_preg<-round((no_preg_both/no_preg_tot)*100,1)

MIG_T1_both<-data.table(algorithm="MIG_T1_both", event_definition="MG_NO_AURA", no_pregnancies_same_type=no_preg_both, no_pregnancies=no_preg_tot, percentage=percentage_preg)

pregnancy_d3_mig[,both:=NULL][,total:=NULL][,MG_NO_AURA_baseline:=NULL][,MG_NO_AURA_baseline_2:=NULL][,MG_NO_AURA_during:=NULL]
rm(no_preg_both,no_preg_tot,percentage_preg)

fwrite(MIG_T1_both,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T1_both.csv"),row.names = F)
rm(MIG_T1_both)



#### MIG_T2_a:Prevalence of MG_AURA at baseline when lookback==12 months(3 months) ####
print("Create algorithm MIG_T2_a")
MIG_T2_a<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_T2_a"]
inc_col<-MIG_T2_a[TYPE=="AND",STUDY_VARIABLES]

if(length(inc_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col],include:=1]}else{pregnancy_d3_mig[,include:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm/final_d3/MIG_T2_a_D3.rds"))

#export MIG_T2_a
MIG_T2_a_dt<-data.table(algorithm="MIG_T2_a", no_diagnosed_pregnancies=pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id),.N])
MIG_T2_a_dt[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T2_a_dt[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
MIG_T2_a_dt[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T2_a_dt[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T2_a_dt[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_T2_a_dt,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T2_a.csv"),row.names = F)

#number of pregnancies with baseline migraine
pregnancy_d3_mig[MG_baseline>=1 | Migraine_medicines_baseline>=1,preg_baseline:=1]
MIG_T2_a_coh<-data.table(algorithm="MIG_T2_a", no_diagnosed_pregnancies=pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id) & preg_baseline==1,.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id) & preg_baseline==1,.N])
MIG_T2_a_coh[,percentage:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T2_a_coh[,denominator:="Number of pregnancies with baseline migraine(MG diagnoses or Triptan prescription), lookback= 365 days(90 days/75 days)"]

#by maternal age
pregnancy_d3_mig[,maternal_age:=as.character(maternal_age)]
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id), .N, by=.(maternal_age)]
names(records)<-c("maternal_age","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id), by="maternal_age",.N]
names(total)<-c("maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by="maternal_age",all=T)
MIG_T2_a_1<-data.table(algorithm="MIG_T2_a", records)
MIG_T2_a_1[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_T2_a_1<-MIG_T2_a_1[order(maternal_age)]
rm(records,total)
MIG_T2_a_1[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T2_a_1[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_T2_a_1[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_T2_a_1[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T2_a_1[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T2_a_1[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_T2_a_1,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T2_a_age.csv"),row.names = F)
rm(MIG_T2_a_1)

#by year
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by="year", .N]
names(records)<-c("year","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by="year",.N]
names(total)<-c("year","no_pregnancies")
records<-merge.data.table(records,total,by="year",all=T)
MIG_T2_a_2<-data.table(algorithm="MIG_T2_a", records)
MIG_T2_a_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_T2_a_2<-MIG_T2_a_2[order(year)]
rm(records,total)
MIG_T2_a_2[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T2_a_2[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_T2_a_2[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_T2_a_2[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T2_a_2[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T2_a_2[lower_95_CI<0,lower_95_CI:=0]
fwrite(MIG_T2_a_2,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T2_a_year.csv"),row.names = F)
rm(MIG_T2_a_2)

#number of pregnnacies with baseline migraine
records_coh<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id) & preg_baseline==1,by="year",.N]
names(records_coh)<-c("year","no_diagnosed_pregnancies")
total_coh<-pregnancy_d3_mig[!duplicated(pregnancy_id) & preg_baseline==1,by="year",.N]
names(total_coh)<-c("year","no_pregnancies")
records_coh<-merge.data.table(records_coh,total_coh,by="year",all=T)
MIG_T2_a_coh_2<-data.table(algorithm="MIG_T2_a", records_coh)
MIG_T2_a_coh_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_T2_a_coh_2[,percentage:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T2_a_coh_2[,denominator:="Number of pregnancies with baseline migraine(MG diagnoses or Triptan prescription), lookback= 365 days(90 days/75 days) by year"]

#by year group and maternal age
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by=c("year_group","maternal_age"), .N]
names(records)<-c("year_group","maternal_age", "no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by=c("year_group","maternal_age"),.N]
names(total)<-c("year_group","maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","maternal_age"), all=T)
MIG_T2_a_3<-data.table(algorithm="MIG_T2_a", records)
MIG_T2_a_3[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_T2_a_3<-MIG_T2_a_3[order(year_group,maternal_age)]
rm(records,total)
MIG_T2_a_3[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T2_a_3[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_T2_a_3[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_T2_a_3[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T2_a_3[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T2_a_3[lower_95_CI<0,lower_95_CI:=0]
pregnancy_d3_mig[,include:=NULL]
rm(MIG_T2_a)
fwrite(MIG_T2_a_3,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T2_a_year_age.csv"),row.names = F)
rm(MIG_T2_a_3)

pregnancy_d3_mig[,preg_baseline:=NULL]
#### MIG_T2_b:Prevalence of MG_AURA at baseline when lookback==3 months ####
print("Create algorithm MIG_T2_b")
MIG_T2_b<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_T2_b"]
inc_col<-MIG_T2_b[TYPE=="AND",STUDY_VARIABLES]

if(length(inc_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col],include:=1]}else{pregnancy_d3_mig[,include:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm/final_d3/MIG_T2_b_D3.rds"))


#export MIG_T2_b
MIG_T2_b_dt<-data.table(algorithm="MIG_T2_b", no_diagnosed_pregnancies=pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id),.N])
MIG_T2_b_dt[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T2_b_dt[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
MIG_T2_b_dt[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T2_b_dt[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T2_b_dt[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_T2_b_dt,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T2_b.csv"),row.names = F)

#number of pregnancies with baseline migraine
pregnancy_d3_mig[MG_baseline_2>=1 | Migraine_medicines_baseline_2>=1,preg_baseline_2:=1]
MIG_T2_b_coh<-data.table(algorithm="MIG_T2_b", no_diagnosed_pregnancies=pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id) & preg_baseline_2==1,.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id) & preg_baseline_2==1,.N])
MIG_T2_b_coh[,percentage:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T2_b_coh[,denominator:="Number of pregnancies with baseline migraine(MG diagnoses or Triptan prescription), lookback= 90 days/75 days"]

#by maternal age
pregnancy_d3_mig[,maternal_age:=as.character(maternal_age)]
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id), .N, by=.(maternal_age)]
names(records)<-c("maternal_age","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id), by="maternal_age",.N]
names(total)<-c("maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by="maternal_age",all=T)
MIG_T2_b_1<-data.table(algorithm="MIG_T2_b", records)
MIG_T2_b_1[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_T2_b_1<-MIG_T2_b_1[order(maternal_age)]
rm(records,total)
MIG_T2_b_1[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T2_b_1[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_T2_b_1[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_T2_b_1[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T2_b_1[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T2_b_1[lower_95_CI<0,lower_95_CI:=0]
fwrite(MIG_T2_b_1,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T2_b_age.csv"),row.names = F)
rm(MIG_T2_b_1)


#by year
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by="year", .N]
names(records)<-c("year","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by="year",.N]
names(total)<-c("year","no_pregnancies")
records<-merge.data.table(records,total,by="year",all=T)
MIG_T2_b_2<-data.table(algorithm="MIG_T2_b", records)
MIG_T2_b_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_T2_b_2<-MIG_T2_b_2[order(year)]
rm(records,total)
MIG_T2_b_2[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T2_b_2[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_T2_b_2[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_T2_b_2[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T2_b_2[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T2_b_2[lower_95_CI<0,lower_95_CI:=0]
fwrite(MIG_T2_b_2,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T2_b_year.csv"),row.names = F)
rm(MIG_T2_b_2)

#number of pregnnacies with baseline migraine
records_coh<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id) & preg_baseline_2==1,by="year",.N]
names(records_coh)<-c("year","no_diagnosed_pregnancies")
total_coh<-pregnancy_d3_mig[!duplicated(pregnancy_id) & preg_baseline_2==1,by="year",.N]
names(total_coh)<-c("year","no_pregnancies")
records_coh<-merge.data.table(records_coh,total_coh,by="year",all=T)
MIG_T2_b_coh_2<-data.table(algorithm="MIG_T2_b", records_coh)
MIG_T2_b_coh_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_T2_b_coh_2[,percentage:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T2_b_coh_2[,denominator:="Number of pregnancies with baseline migraine(MG diagnoses or Triptan prescription), lookback= 90 days/75 days by year"]

#by year group and maternal age
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by=c("year_group","maternal_age"), .N]
names(records)<-c("year_group","maternal_age", "no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by=c("year_group","maternal_age"),.N]
names(total)<-c("year_group","maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","maternal_age"), all=T)
MIG_T2_b_3<-data.table(algorithm="MIG_T2_b", records)
MIG_T2_b_3[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_T2_b_3<-MIG_T2_b_3[order(year_group,maternal_age)]
rm(records,total)
MIG_T2_b_3[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T2_b_3[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_T2_b_3[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_T2_b_3[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T2_b_3[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T2_b_3[lower_95_CI<0,lower_95_CI:=0]
pregnancy_d3_mig[,include:=NULL]
fwrite(MIG_T2_b_3,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T2_b_year_age.csv"),row.names = F)
rm(MIG_T2_b_3)
pregnancy_d3_mig[,preg_baseline_2:=NULL]
#### MIG_T2_during:Prevalence of MG_AURA during pregnancy when lookback==12 months(3 months) ####
print("Create algorithm MIG_T2_during")
MIG_T2_during<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_T2_during"]
inc_col<-MIG_T2_during[TYPE=="AND",STUDY_VARIABLES]

if(length(inc_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col],include:=1]}else{pregnancy_d3_mig[,include:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm/final_d3/MIG_T2_during_D3.rds"))

#export MIG_T2_during
MIG_T2_during_dt<-data.table(algorithm="MIG_T2_during", no_diagnosed_pregnancies=pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id),.N])
MIG_T2_during_dt[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T2_during_dt[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
MIG_T2_during_dt[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T2_during_dt[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T2_during_dt[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_T2_during_dt,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T2_during.csv"),row.names = F)

#number of pregnancies with baseline migraine
pregnancy_d3_mig[MG_during>=1 | Migraine_medicines_during>=1,preg_during:=1]
MIG_T2_during_coh<-data.table(algorithm="MIG_T2_during", no_diagnosed_pregnancies=pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id) & preg_during==1,.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id) & preg_during==1,.N])
MIG_T2_during_coh[,percentage:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T2_during_coh[,denominator:="Number of pregnancies with migraine(MG diagnoses or Triptan prescription) during pregnancy"]

#by maternal age
pregnancy_d3_mig[,maternal_age:=as.character(maternal_age)]
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id), .N, by=.(maternal_age)]
names(records)<-c("maternal_age","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id), by="maternal_age",.N]
names(total)<-c("maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by="maternal_age",all=T)
MIG_T2_during_1<-data.table(algorithm="MIG_T2_during", records)
MIG_T2_during_1[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_T2_during_1<-MIG_T2_during_1[order(maternal_age)]
rm(records,total)
MIG_T2_during_1[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T2_during_1[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_T2_during_1[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_T2_during_1[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T2_during_1[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T2_during_1[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_T2_during_1,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T2_during_age.csv"),row.names = F)
rm(MIG_T2_during_1)

#by year
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by="year", .N]
names(records)<-c("year","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by="year",.N]
names(total)<-c("year","no_pregnancies")
records<-merge.data.table(records,total,by="year",all=T)
MIG_T2_during_2<-data.table(algorithm="MIG_T2_during", records)
MIG_T2_during_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_T2_during_2<-MIG_T2_during_2[order(year)]
rm(records,total)
MIG_T2_during_2[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T2_during_2[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_T2_during_2[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_T2_during_2[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T2_during_2[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T2_during_2[lower_95_CI<0,lower_95_CI:=0]
fwrite(MIG_T2_during_2,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T2_during_year.csv"),row.names = F)
rm(MIG_T2_during_2)

#number of pregnnacies with migraine
records_coh<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id) & preg_during==1,by="year",.N]
names(records_coh)<-c("year","no_diagnosed_pregnancies")
total_coh<-pregnancy_d3_mig[!duplicated(pregnancy_id) & preg_during==1,by="year",.N]
names(total_coh)<-c("year","no_pregnancies")
records_coh<-merge.data.table(records_coh,total_coh,by="year",all=T)
MIG_T2_during_coh_2<-data.table(algorithm="MIG_T2_during", records_coh)
MIG_T2_during_coh_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_T2_during_coh_2[,percentage:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T2_during_coh_2[,denominator:="Number of pregnancies with migraine(MG diagnoses or Triptan prescription) during pregnancy by year"]

#by year group and maternal age
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by=c("year_group","maternal_age"), .N]
names(records)<-c("year_group","maternal_age", "no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by=c("year_group","maternal_age"),.N]
names(total)<-c("year_group","maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","maternal_age"), all=T)
MIG_T2_during_3<-data.table(algorithm="MIG_T2_during", records)
MIG_T2_during_3[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_T2_during_3<-MIG_T2_during_3[order(year_group,maternal_age)]
rm(records,total)
MIG_T2_during_3[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T2_during_3[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_T2_during_3[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_T2_during_3[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T2_during_3[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T2_during_3[lower_95_CI<0,lower_95_CI:=0]
pregnancy_d3_mig[,include:=NULL]
rm(MIG_T2_during)
fwrite(MIG_T2_during_3,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T2_during_year_age.csv"),row.names = F)
rm(MIG_T2_during_3)
pregnancy_d3_mig[,preg_during:=NULL]
#### MIG_T2_both:Proportion of pregnancies having the same type MG_AURA during pregnancy and before when lookback==12 months(3 months) ####
print("Create algorithm MIG_T2_both")

pregnancy_d3_mig[MG_AURA_baseline>=1 & MG_AURA_during>=1, both:=1]
pregnancy_d3_mig[MG_AURA_baseline>=1 | MG_AURA_during>=1, total:=1]

#number of pregnancies having the same type in baseline and during
no_preg_both<-pregnancy_d3_mig[both==1 & !duplicated(pregnancy_id), .N]
#number of pregnancies having the type of interest in baseline or during
no_preg_tot<-pregnancy_d3_mig[total==1 & !duplicated(pregnancy_id), .N]
percentage_preg<-round((no_preg_both/no_preg_tot)*100,1)

MIG_T2_both<-data.table(algorithm="MIG_T2_both", event_definition="MG_AURA", no_pregnancies_same_type=no_preg_both, no_pregnancies=no_preg_tot, percentage=percentage_preg)

pregnancy_d3_mig[,both:=NULL][,total:=NULL][,MG_AURA_baseline:=NULL][,MG_AURA_baseline_2:=NULL][,MG_AURA_during:=NULL]
rm(no_preg_both,no_preg_tot,percentage_preg)

fwrite(MIG_T2_both,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T2_both.csv"),row.names = F)
rm(MIG_T2_both)



#### MIG_T3_a:Prevalence of MG_STACOMP at baseline when lookback==12 months(3 months) ####
print("Create algorithm MIG_T3_a")
MIG_T3_a<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_T3_a"]
inc_col<-MIG_T3_a[TYPE=="AND",STUDY_VARIABLES]

if(length(inc_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col],include:=1]}else{pregnancy_d3_mig[,include:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm/final_d3/MIG_T3_a_D3.rds"))

#export MIG_T3_a
MIG_T3_a_dt<-data.table(algorithm="MIG_T3_a", no_diagnosed_pregnancies=pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id),.N])
MIG_T3_a_dt[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T3_a_dt[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
MIG_T3_a_dt[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T3_a_dt[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T3_a_dt[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_T3_a_dt,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T3_a.csv"),row.names = F)

#number of pregnancies with baseline migraine
pregnancy_d3_mig[MG_baseline>=1 | Migraine_medicines_baseline>=1,preg_baseline:=1]
MIG_T3_a_coh<-data.table(algorithm="MIG_T3_a", no_diagnosed_pregnancies=pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id) & preg_baseline==1,.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id) & preg_baseline==1,.N])
MIG_T3_a_coh[,percentage:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T3_a_coh[,denominator:="Number of pregnancies with baseline migraine(MG diagnoses or Triptan prescription), lookback= 365 days(90 days/75 days)"]

#by maternal age
pregnancy_d3_mig[,maternal_age:=as.character(maternal_age)]
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id), .N, by=.(maternal_age)]
names(records)<-c("maternal_age","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id), by="maternal_age",.N]
names(total)<-c("maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by="maternal_age",all=T)
MIG_T3_a_1<-data.table(algorithm="MIG_T3_a", records)
MIG_T3_a_1[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_T3_a_1<-MIG_T3_a_1[order(maternal_age)]
rm(records,total)
MIG_T3_a_1[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T3_a_1[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_T3_a_1[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_T3_a_1[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T3_a_1[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T3_a_1[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_T3_a_1,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T3_a_age.csv"),row.names = F)
rm(MIG_T3_a_1)

#by year
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by="year", .N]
names(records)<-c("year","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by="year",.N]
names(total)<-c("year","no_pregnancies")
records<-merge.data.table(records,total,by="year",all=T)
MIG_T3_a_2<-data.table(algorithm="MIG_T3_a", records)
MIG_T3_a_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_T3_a_2<-MIG_T3_a_2[order(year)]
rm(records,total)
MIG_T3_a_2[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T3_a_2[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_T3_a_2[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_T3_a_2[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T3_a_2[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T3_a_2[lower_95_CI<0,lower_95_CI:=0]
fwrite(MIG_T3_a_2,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T3_a_year.csv"),row.names = F)
rm(MIG_T3_a_2)

#number of pregnnacies with baseline migraine
records_coh<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id) & preg_baseline==1,by="year",.N]
names(records_coh)<-c("year","no_diagnosed_pregnancies")
total_coh<-pregnancy_d3_mig[!duplicated(pregnancy_id) & preg_baseline==1,by="year",.N]
names(total_coh)<-c("year","no_pregnancies")
records_coh<-merge.data.table(records_coh,total_coh,by="year",all=T)
MIG_T3_a_coh_2<-data.table(algorithm="MIG_T3_a", records_coh)
MIG_T3_a_coh_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_T3_a_coh_2[,percentage:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T3_a_coh_2[,denominator:="Number of pregnancies with baseline migraine(MG diagnoses or Triptan prescription), lookback= 365 days(90 days/75 days) by year"]

#by year group and maternal age
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by=c("year_group","maternal_age"), .N]
names(records)<-c("year_group","maternal_age", "no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by=c("year_group","maternal_age"),.N]
names(total)<-c("year_group","maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","maternal_age"), all=T)
MIG_T3_a_3<-data.table(algorithm="MIG_T3_a", records)
MIG_T3_a_3[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_T3_a_3<-MIG_T3_a_3[order(year_group,maternal_age)]
rm(records,total)
MIG_T3_a_3[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T3_a_3[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_T3_a_3[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_T3_a_3[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T3_a_3[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T3_a_3[lower_95_CI<0,lower_95_CI:=0]
pregnancy_d3_mig[,include:=NULL]
rm(MIG_T3_a)
fwrite(MIG_T3_a_3,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T3_a_year_age.csv"),row.names = F)
rm(MIG_T3_a_3)
pregnancy_d3_mig[,preg_baseline:=NULL]
#### MIG_T3_b:Prevalence of MG_STACOMP at baseline when lookback==3 months ####
print("Create algorithm MIG_T3_b")
MIG_T3_b<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_T3_b"]
inc_col<-MIG_T3_b[TYPE=="AND",STUDY_VARIABLES]

if(length(inc_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col],include:=1]}else{pregnancy_d3_mig[,include:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm/final_d3/MIG_T3_b_D3.rds"))


#export MIG_T3_b
MIG_T3_b_dt<-data.table(algorithm="MIG_T3_b", no_diagnosed_pregnancies=pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id),.N])
MIG_T3_b_dt[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T3_b_dt[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
MIG_T3_b_dt[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T3_b_dt[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T3_b_dt[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_T3_b_dt,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T3_b.csv"),row.names = F)

#number of pregnancies with baseline migraine
pregnancy_d3_mig[MG_baseline_2>=1 | Migraine_medicines_baseline_2>=1,preg_baseline_2:=1]
MIG_T3_b_coh<-data.table(algorithm="MIG_T3_b", no_diagnosed_pregnancies=pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id) & preg_baseline_2==1,.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id) & preg_baseline_2==1,.N])
MIG_T3_b_coh[,percentage:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T3_b_coh[,denominator:="Number of pregnancies with baseline migraine(MG diagnoses or Triptan prescription), lookback= 90 days/75 days"]

#by maternal age
pregnancy_d3_mig[,maternal_age:=as.character(maternal_age)]
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id), .N, by=.(maternal_age)]
names(records)<-c("maternal_age","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id), by="maternal_age",.N]
names(total)<-c("maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by="maternal_age",all=T)
MIG_T3_b_1<-data.table(algorithm="MIG_T3_b", records)
MIG_T3_b_1[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_T3_b_1<-MIG_T3_b_1[order(maternal_age)]
rm(records,total)
MIG_T3_b_1[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T3_b_1[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_T3_b_1[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_T3_b_1[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T3_b_1[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T3_b_1[lower_95_CI<0,lower_95_CI:=0]
fwrite(MIG_T3_b_1,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T3_b_age.csv"),row.names = F)
rm(MIG_T3_b_1)


#by year
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by="year", .N]
names(records)<-c("year","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by="year",.N]
names(total)<-c("year","no_pregnancies")
records<-merge.data.table(records,total,by="year",all=T)
MIG_T3_b_2<-data.table(algorithm="MIG_T3_b", records)
MIG_T3_b_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_T3_b_2<-MIG_T3_b_2[order(year)]
rm(records,total)
MIG_T3_b_2[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T3_b_2[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_T3_b_2[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_T3_b_2[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T3_b_2[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T3_b_2[lower_95_CI<0,lower_95_CI:=0]
fwrite(MIG_T3_b_2,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T3_b_year.csv"),row.names = F)
rm(MIG_T3_b_2)

#number of pregnnacies with baseline migraine
records_coh<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id) & preg_baseline_2==1,by="year",.N]
names(records_coh)<-c("year","no_diagnosed_pregnancies")
total_coh<-pregnancy_d3_mig[!duplicated(pregnancy_id) & preg_baseline_2==1,by="year",.N]
names(total_coh)<-c("year","no_pregnancies")
records_coh<-merge.data.table(records_coh,total_coh,by="year",all=T)
MIG_T3_b_coh_2<-data.table(algorithm="MIG_T3_b", records_coh)
MIG_T3_b_coh_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_T3_b_coh_2[,percentage:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T3_b_coh_2[,denominator:="Number of pregnancies with baseline migraine(MG diagnoses or Triptan prescription), lookback= 90 days/75 days by year"]

#by year group and maternal age
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by=c("year_group","maternal_age"), .N]
names(records)<-c("year_group","maternal_age", "no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by=c("year_group","maternal_age"),.N]
names(total)<-c("year_group","maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","maternal_age"), all=T)
MIG_T3_b_3<-data.table(algorithm="MIG_T3_b", records)
MIG_T3_b_3[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_T3_b_3<-MIG_T3_b_3[order(year_group,maternal_age)]
rm(records,total)
MIG_T3_b_3[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T3_b_3[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_T3_b_3[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_T3_b_3[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T3_b_3[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T3_b_3[lower_95_CI<0,lower_95_CI:=0]
pregnancy_d3_mig[,include:=NULL]
fwrite(MIG_T3_b_3,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T3_b_year_age.csv"),row.names = F)
rm(MIG_T3_b_3)
pregnancy_d3_mig[,preg_baseline_2:=NULL]
#### MIG_T3_during:Prevalence of MG_STACOMP during pregnancy when lookback==12 months(3 months) ####
print("Create algorithm MIG_T3_during")
MIG_T3_during<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_T3_during"]
inc_col<-MIG_T3_during[TYPE=="AND",STUDY_VARIABLES]

if(length(inc_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col],include:=1]}else{pregnancy_d3_mig[,include:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm/final_d3/MIG_T3_during_D3.rds"))

#export MIG_T3_during
MIG_T3_during_dt<-data.table(algorithm="MIG_T3_during", no_diagnosed_pregnancies=pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id),.N])
MIG_T3_during_dt[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T3_during_dt[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
MIG_T3_during_dt[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T3_during_dt[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T3_during_dt[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_T3_during_dt,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T3_during.csv"),row.names = F)

#number of pregnancies with baseline migraine
pregnancy_d3_mig[MG_during>=1 | Migraine_medicines_during>=1,preg_during:=1]
MIG_T3_during_coh<-data.table(algorithm="MIG_T3_during", no_diagnosed_pregnancies=pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id) & preg_during==1,.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id) & preg_during==1,.N])
MIG_T3_during_coh[,percentage:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T3_during_coh[,denominator:="Number of pregnancies with migraine(MG diagnoses or Triptan prescription) during pregnancy"]

#by maternal age
pregnancy_d3_mig[,maternal_age:=as.character(maternal_age)]
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id), .N, by=.(maternal_age)]
names(records)<-c("maternal_age","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id), by="maternal_age",.N]
names(total)<-c("maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by="maternal_age",all=T)
MIG_T3_during_1<-data.table(algorithm="MIG_T3_during", records)
MIG_T3_during_1[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_T3_during_1<-MIG_T3_during_1[order(maternal_age)]
rm(records,total)
MIG_T3_during_1[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T3_during_1[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_T3_during_1[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_T3_during_1[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T3_during_1[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T3_during_1[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_T3_during_1,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T3_during_age.csv"),row.names = F)
rm(MIG_T3_during_1)

#by year
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by="year", .N]
names(records)<-c("year","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by="year",.N]
names(total)<-c("year","no_pregnancies")
records<-merge.data.table(records,total,by="year",all=T)
MIG_T3_during_2<-data.table(algorithm="MIG_T3_during", records)
MIG_T3_during_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_T3_during_2<-MIG_T3_during_2[order(year)]
rm(records,total)
MIG_T3_during_2[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T3_during_2[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_T3_during_2[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_T3_during_2[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T3_during_2[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T3_during_2[lower_95_CI<0,lower_95_CI:=0]
fwrite(MIG_T3_during_2,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T3_during_year.csv"),row.names = F)
rm(MIG_T3_during_2)

#number of pregnnacies with migraine
records_coh<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id) & preg_during==1,by="year",.N]
names(records_coh)<-c("year","no_diagnosed_pregnancies")
total_coh<-pregnancy_d3_mig[!duplicated(pregnancy_id) & preg_during==1,by="year",.N]
names(total_coh)<-c("year","no_pregnancies")
records_coh<-merge.data.table(records_coh,total_coh,by="year",all=T)
MIG_T3_during_coh_2<-data.table(algorithm="MIG_T3_during", records_coh)
MIG_T3_during_coh_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_T3_during_coh_2[,percentage:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T3_during_coh_2[,denominator:="Number of pregnancies with migraine(MG diagnoses or Triptan prescription) during pregnancy by year"]

#by year group and maternal age
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by=c("year_group","maternal_age"), .N]
names(records)<-c("year_group","maternal_age", "no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by=c("year_group","maternal_age"),.N]
names(total)<-c("year_group","maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","maternal_age"), all=T)
MIG_T3_during_3<-data.table(algorithm="MIG_T3_during", records)
MIG_T3_during_3[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_T3_during_3<-MIG_T3_during_3[order(year_group,maternal_age)]
rm(records,total)
MIG_T3_during_3[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T3_during_3[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_T3_during_3[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_T3_during_3[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T3_during_3[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T3_during_3[lower_95_CI<0,lower_95_CI:=0]
pregnancy_d3_mig[,include:=NULL]
rm(MIG_T3_during)
fwrite(MIG_T3_during_3,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T3_during_year_age.csv"),row.names = F)
rm(MIG_T3_during_3)
pregnancy_d3_mig[,preg_during:=NULL]
#### MIG_T3_both:Proportion of pregnancies having the same type MG_STACOMP during pregnancy and before when lookback==12 months(3 months) ####
print("Create algorithm MIG_T3_both")

pregnancy_d3_mig[MG_STACOMP_baseline>=1 & MG_STACOMP_during>=1, both:=1]
pregnancy_d3_mig[MG_STACOMP_baseline>=1 | MG_STACOMP_during>=1, total:=1]

#number of pregnancies having the same type in baseline and during
no_preg_both<-pregnancy_d3_mig[both==1 & !duplicated(pregnancy_id), .N]
#number of pregnancies having the type of interest in baseline or during
no_preg_tot<-pregnancy_d3_mig[total==1 & !duplicated(pregnancy_id), .N]
percentage_preg<-round((no_preg_both/no_preg_tot)*100,1)

MIG_T3_both<-data.table(algorithm="MIG_T3_both", event_definition="MG_STACOMP", no_pregnancies_same_type=no_preg_both, no_pregnancies=no_preg_tot, percentage=percentage_preg)

pregnancy_d3_mig[,both:=NULL][,total:=NULL][,MG_STACOMP_baseline:=NULL][,MG_STACOMP_baseline_2:=NULL][,MG_STACOMP_during:=NULL]
rm(no_preg_both,no_preg_tot,percentage_preg)

fwrite(MIG_T3_both,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T3_both.csv"),row.names = F)
rm(MIG_T3_both)

#### MIG_T4_a:Prevalence of MG_OTHER at baseline when lookback==12 months(3 months) ####
print("Create algorithm MIG_T4_a")
MIG_T4_a<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_T4_a"]
inc_col<-MIG_T4_a[TYPE=="AND",STUDY_VARIABLES]

if(length(inc_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col],include:=1]}else{pregnancy_d3_mig[,include:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm/final_d3/MIG_T4_a_D3.rds"))

#export MIG_T4_a
MIG_T4_a_dt<-data.table(algorithm="MIG_T4_a", no_diagnosed_pregnancies=pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id),.N])
MIG_T4_a_dt[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T4_a_dt[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
MIG_T4_a_dt[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T4_a_dt[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T4_a_dt[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_T4_a_dt,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T4_a.csv"),row.names = F)

#number of pregnancies with baseline migraine
pregnancy_d3_mig[MG_baseline>=1 | Migraine_medicines_baseline>=1,preg_baseline:=1]
MIG_T4_a_coh<-data.table(algorithm="MIG_T4_a", no_diagnosed_pregnancies=pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id) & preg_baseline==1,.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id) & preg_baseline==1,.N])
MIG_T4_a_coh[,percentage:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T4_a_coh[,denominator:="Number of pregnancies with baseline migraine(MG diagnoses or Triptan prescription), lookback= 365 days(90 days/75 days)"]

#by maternal age
pregnancy_d3_mig[,maternal_age:=as.character(maternal_age)]
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id), .N, by=.(maternal_age)]
names(records)<-c("maternal_age","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id), by="maternal_age",.N]
names(total)<-c("maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by="maternal_age",all=T)
MIG_T4_a_1<-data.table(algorithm="MIG_T4_a", records)
MIG_T4_a_1[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_T4_a_1<-MIG_T4_a_1[order(maternal_age)]
rm(records,total)
MIG_T4_a_1[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T4_a_1[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_T4_a_1[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_T4_a_1[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T4_a_1[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T4_a_1[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_T4_a_1,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T4_a_age.csv"),row.names = F)
rm(MIG_T4_a_1)

#by year
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by="year", .N]
names(records)<-c("year","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by="year",.N]
names(total)<-c("year","no_pregnancies")
records<-merge.data.table(records,total,by="year",all=T)
MIG_T4_a_2<-data.table(algorithm="MIG_T4_a", records)
MIG_T4_a_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_T4_a_2<-MIG_T4_a_2[order(year)]
rm(records,total)
MIG_T4_a_2[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T4_a_2[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_T4_a_2[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_T4_a_2[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T4_a_2[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T4_a_2[lower_95_CI<0,lower_95_CI:=0]
fwrite(MIG_T4_a_2,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T4_a_year.csv"),row.names = F)
rm(MIG_T4_a_2)

#number of pregnnacies with baseline migraine
records_coh<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id) & preg_baseline==1,by="year",.N]
names(records_coh)<-c("year","no_diagnosed_pregnancies")
total_coh<-pregnancy_d3_mig[!duplicated(pregnancy_id) & preg_baseline==1,by="year",.N]
names(total_coh)<-c("year","no_pregnancies")
records_coh<-merge.data.table(records_coh,total_coh,by="year",all=T)
MIG_T4_a_coh_2<-data.table(algorithm="MIG_T4_a", records_coh)
MIG_T4_a_coh_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_T4_a_coh_2[,percentage:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T4_a_coh_2[,denominator:="Number of pregnancies with baseline migraine(MG diagnoses or Triptan prescription), lookback= 365 days(90 days/75 days) by year"]

#by year group and maternal age
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by=c("year_group","maternal_age"), .N]
names(records)<-c("year_group","maternal_age", "no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by=c("year_group","maternal_age"),.N]
names(total)<-c("year_group","maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","maternal_age"), all=T)
MIG_T4_a_3<-data.table(algorithm="MIG_T4_a", records)
MIG_T4_a_3[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_T4_a_3<-MIG_T4_a_3[order(year_group,maternal_age)]
rm(records,total)
MIG_T4_a_3[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T4_a_3[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_T4_a_3[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_T4_a_3[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T4_a_3[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T4_a_3[lower_95_CI<0,lower_95_CI:=0]
pregnancy_d3_mig[,include:=NULL]
rm(MIG_T4_a)
fwrite(MIG_T4_a_3,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T4_a_year_age.csv"),row.names = F)
rm(MIG_T4_a_3)
pregnancy_d3_mig[,preg_baseline:=NULL]
#### MIG_T4_b:Prevalence of MG_OTHER at baseline when lookback==3 months ####
print("Create algorithm MIG_T4_b")
MIG_T4_b<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_T4_b"]
inc_col<-MIG_T4_b[TYPE=="AND",STUDY_VARIABLES]

if(length(inc_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col],include:=1]}else{pregnancy_d3_mig[,include:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm/final_d3/MIG_T4_b_D3.rds"))


#export MIG_T4_b
MIG_T4_b_dt<-data.table(algorithm="MIG_T4_b", no_diagnosed_pregnancies=pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id),.N])
MIG_T4_b_dt[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T4_b_dt[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
MIG_T4_b_dt[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T4_b_dt[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T4_b_dt[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_T4_b_dt,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T4_b.csv"),row.names = F)

#number of pregnancies with baseline migraine
pregnancy_d3_mig[MG_baseline_2>=1 | Migraine_medicines_baseline_2>=1,preg_baseline_2:=1]
MIG_T4_b_coh<-data.table(algorithm="MIG_T4_b", no_diagnosed_pregnancies=pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id) & preg_baseline_2==1,.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id) & preg_baseline_2==1,.N])
MIG_T4_b_coh[,percentage:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T4_b_coh[,denominator:="Number of pregnancies with baseline migraine(MG diagnoses or Triptan prescription), lookback= 90 days/75 days"]

#by maternal age
pregnancy_d3_mig[,maternal_age:=as.character(maternal_age)]
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id), .N, by=.(maternal_age)]
names(records)<-c("maternal_age","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id), by="maternal_age",.N]
names(total)<-c("maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by="maternal_age",all=T)
MIG_T4_b_1<-data.table(algorithm="MIG_T4_b", records)
MIG_T4_b_1[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_T4_b_1<-MIG_T4_b_1[order(maternal_age)]
rm(records,total)
MIG_T4_b_1[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T4_b_1[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_T4_b_1[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_T4_b_1[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T4_b_1[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T4_b_1[lower_95_CI<0,lower_95_CI:=0]
fwrite(MIG_T4_b_1,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T4_b_age.csv"),row.names = F)
rm(MIG_T4_b_1)


#by year
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by="year", .N]
names(records)<-c("year","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by="year",.N]
names(total)<-c("year","no_pregnancies")
records<-merge.data.table(records,total,by="year",all=T)
MIG_T4_b_2<-data.table(algorithm="MIG_T4_b", records)
MIG_T4_b_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_T4_b_2<-MIG_T4_b_2[order(year)]
rm(records,total)
MIG_T4_b_2[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T4_b_2[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_T4_b_2[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_T4_b_2[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T4_b_2[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T4_b_2[lower_95_CI<0,lower_95_CI:=0]
fwrite(MIG_T4_b_2,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T4_b_year.csv"),row.names = F)
rm(MIG_T4_b_2)

#number of pregnnacies with baseline migraine
records_coh<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id) & preg_baseline_2==1,by="year",.N]
names(records_coh)<-c("year","no_diagnosed_pregnancies")
total_coh<-pregnancy_d3_mig[!duplicated(pregnancy_id) & preg_baseline_2==1,by="year",.N]
names(total_coh)<-c("year","no_pregnancies")
records_coh<-merge.data.table(records_coh,total_coh,by="year",all=T)
MIG_T4_b_coh_2<-data.table(algorithm="MIG_T4_b", records_coh)
MIG_T4_b_coh_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_T4_b_coh_2[,percentage:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T4_b_coh_2[,denominator:="Number of pregnancies with baseline migraine(MG diagnoses or Triptan prescription), lookback= 90 days/75 days by year"]

#by year group and maternal age
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by=c("year_group","maternal_age"), .N]
names(records)<-c("year_group","maternal_age", "no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by=c("year_group","maternal_age"),.N]
names(total)<-c("year_group","maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","maternal_age"), all=T)
MIG_T4_b_3<-data.table(algorithm="MIG_T4_b", records)
MIG_T4_b_3[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_T4_b_3<-MIG_T4_b_3[order(year_group,maternal_age)]
rm(records,total)
MIG_T4_b_3[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T4_b_3[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_T4_b_3[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_T4_b_3[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T4_b_3[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T4_b_3[lower_95_CI<0,lower_95_CI:=0]
pregnancy_d3_mig[,include:=NULL]
fwrite(MIG_T4_b_3,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T4_b_year_age.csv"),row.names = F)
rm(MIG_T4_b_3)
pregnancy_d3_mig[,preg_baseline_2:=NULL]
#### MIG_T4_during:Prevalence of MG_OTHER during pregnancy when lookback==12 months(3 months) ####
print("Create algorithm MIG_T4_during")
MIG_T4_during<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_T4_during"]
inc_col<-MIG_T4_during[TYPE=="AND",STUDY_VARIABLES]

if(length(inc_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col],include:=1]}else{pregnancy_d3_mig[,include:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm/final_d3/MIG_T4_during_D3.rds"))

#export MIG_T4_during
MIG_T4_during_dt<-data.table(algorithm="MIG_T4_during", no_diagnosed_pregnancies=pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id),.N])
MIG_T4_during_dt[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T4_during_dt[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
MIG_T4_during_dt[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T4_during_dt[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T4_during_dt[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_T4_during_dt,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T4_during.csv"),row.names = F)

#number of pregnancies with baseline migraine
pregnancy_d3_mig[MG_during>=1 | Migraine_medicines_during>=1,preg_during:=1]
MIG_T4_during_coh<-data.table(algorithm="MIG_T4_during", no_diagnosed_pregnancies=pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id) & preg_during==1,.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id) & preg_during==1,.N])
MIG_T4_during_coh[,percentage:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T4_during_coh[,denominator:="Number of pregnancies with migraine(MG diagnoses or Triptan prescription) during pregnancy"]

#by maternal age
pregnancy_d3_mig[,maternal_age:=as.character(maternal_age)]
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id), .N, by=.(maternal_age)]
names(records)<-c("maternal_age","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id), by="maternal_age",.N]
names(total)<-c("maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by="maternal_age",all=T)
MIG_T4_during_1<-data.table(algorithm="MIG_T4_during", records)
MIG_T4_during_1[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_T4_during_1<-MIG_T4_during_1[order(maternal_age)]
rm(records,total)
MIG_T4_during_1[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T4_during_1[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_T4_during_1[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_T4_during_1[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T4_during_1[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T4_during_1[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_T4_during_1,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T4_during_age.csv"),row.names = F)
rm(MIG_T4_during_1)

#by year
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by="year", .N]
names(records)<-c("year","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by="year",.N]
names(total)<-c("year","no_pregnancies")
records<-merge.data.table(records,total,by="year",all=T)
MIG_T4_during_2<-data.table(algorithm="MIG_T4_during", records)
MIG_T4_during_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_T4_during_2<-MIG_T4_during_2[order(year)]
rm(records,total)
MIG_T4_during_2[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T4_during_2[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_T4_during_2[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_T4_during_2[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T4_during_2[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T4_during_2[lower_95_CI<0,lower_95_CI:=0]
fwrite(MIG_T4_during_2,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T4_during_year.csv"),row.names = F)
rm(MIG_T4_during_2)

#number of pregnnacies with migraine
records_coh<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id) & preg_during==1,by="year",.N]
names(records_coh)<-c("year","no_diagnosed_pregnancies")
total_coh<-pregnancy_d3_mig[!duplicated(pregnancy_id) & preg_during==1,by="year",.N]
names(total_coh)<-c("year","no_pregnancies")
records_coh<-merge.data.table(records_coh,total_coh,by="year",all=T)
MIG_T4_during_coh_2<-data.table(algorithm="MIG_T4_during", records_coh)
MIG_T4_during_coh_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_T4_during_coh_2[,percentage:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T4_during_coh_2[,denominator:="Number of pregnancies with migraine(MG diagnoses or Triptan prescription) during pregnancy by year"]

#by year group and maternal age
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by=c("year_group","maternal_age"), .N]
names(records)<-c("year_group","maternal_age", "no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by=c("year_group","maternal_age"),.N]
names(total)<-c("year_group","maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","maternal_age"), all=T)
MIG_T4_during_3<-data.table(algorithm="MIG_T4_during", records)
MIG_T4_during_3[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_T4_during_3<-MIG_T4_during_3[order(year_group,maternal_age)]
rm(records,total)
MIG_T4_during_3[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T4_during_3[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_T4_during_3[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_T4_during_3[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T4_during_3[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T4_during_3[lower_95_CI<0,lower_95_CI:=0]
pregnancy_d3_mig[,include:=NULL]
rm(MIG_T4_during)
fwrite(MIG_T4_during_3,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T4_during_year_age.csv"),row.names = F)
rm(MIG_T4_during_3)
pregnancy_d3_mig[,preg_during:=NULL]
#### MIG_T4_both:Proportion of pregnancies having the same type MG_OTHER during pregnancy and before when lookback==12 months(3 months) ####
print("Create algorithm MIG_T4_both")

pregnancy_d3_mig[MG_OTHER_baseline>=1 & MG_OTHER_during>=1, both:=1]
pregnancy_d3_mig[MG_OTHER_baseline>=1 | MG_OTHER_during>=1, total:=1]

#number of pregnancies having the same type in baseline and during
no_preg_both<-pregnancy_d3_mig[both==1 & !duplicated(pregnancy_id), .N]
#number of pregnancies having the type of interest in baseline or during
no_preg_tot<-pregnancy_d3_mig[total==1 & !duplicated(pregnancy_id), .N]
percentage_preg<-round((no_preg_both/no_preg_tot)*100,1)

MIG_T4_both<-data.table(algorithm="MIG_T4_both", event_definition="MG_OTHER", no_pregnancies_same_type=no_preg_both, no_pregnancies=no_preg_tot, percentage=percentage_preg)

pregnancy_d3_mig[,both:=NULL][,total:=NULL][,MG_OTHER_baseline:=NULL][,MG_OTHER_baseline_2:=NULL][,MG_OTHER_during:=NULL]
rm(no_preg_both,no_preg_tot,percentage_preg)

fwrite(MIG_T4_both,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T4_both.csv"),row.names = F)
rm(MIG_T4_both)

#### MIG_T5_a:Prevalence of MG_UPC at baseline when lookback==12 months(3 months) ####
print("Create algorithm MIG_T5_a")
MIG_T5_a<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_T5_a"]
inc_col<-MIG_T5_a[TYPE=="AND",STUDY_VARIABLES]

if(length(inc_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col],include:=1]}else{pregnancy_d3_mig[,include:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm/final_d3/MIG_T5_a_D3.rds"))

#export MIG_T5_a
MIG_T5_a_dt<-data.table(algorithm="MIG_T5_a", no_diagnosed_pregnancies=pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id),.N])
MIG_T5_a_dt[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T5_a_dt[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
MIG_T5_a_dt[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T5_a_dt[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T5_a_dt[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_T5_a_dt,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T5_a.csv"),row.names = F)

#number of pregnancies with baseline migraine
pregnancy_d3_mig[MG_baseline>=1 | Migraine_medicines_baseline>=1,preg_baseline:=1]
MIG_T5_a_coh<-data.table(algorithm="MIG_T5_a", no_diagnosed_pregnancies=pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id) & preg_baseline==1,.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id) & preg_baseline==1,.N])
MIG_T5_a_coh[,percentage:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T5_a_coh[,denominator:="Number of pregnancies with baseline migraine(MG diagnoses or Triptan prescription), lookback= 365 days(90 days/75 days)"]

#by maternal age
pregnancy_d3_mig[,maternal_age:=as.character(maternal_age)]
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id), .N, by=.(maternal_age)]
names(records)<-c("maternal_age","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id), by="maternal_age",.N]
names(total)<-c("maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by="maternal_age",all=T)
MIG_T5_a_1<-data.table(algorithm="MIG_T5_a", records)
MIG_T5_a_1[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_T5_a_1<-MIG_T5_a_1[order(maternal_age)]
rm(records,total)
MIG_T5_a_1[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T5_a_1[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_T5_a_1[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_T5_a_1[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T5_a_1[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T5_a_1[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_T5_a_1,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T5_a_age.csv"),row.names = F)
rm(MIG_T5_a_1)

#by year
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by="year", .N]
names(records)<-c("year","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by="year",.N]
names(total)<-c("year","no_pregnancies")
records<-merge.data.table(records,total,by="year",all=T)
MIG_T5_a_2<-data.table(algorithm="MIG_T5_a", records)
MIG_T5_a_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_T5_a_2<-MIG_T5_a_2[order(year)]
rm(records,total)
MIG_T5_a_2[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T5_a_2[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_T5_a_2[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_T5_a_2[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T5_a_2[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T5_a_2[lower_95_CI<0,lower_95_CI:=0]
fwrite(MIG_T5_a_2,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T5_a_year.csv"),row.names = F)
rm(MIG_T5_a_2)

#number of pregnnacies with baseline migraine
records_coh<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id) & preg_baseline==1,by="year",.N]
names(records_coh)<-c("year","no_diagnosed_pregnancies")
total_coh<-pregnancy_d3_mig[!duplicated(pregnancy_id) & preg_baseline==1,by="year",.N]
names(total_coh)<-c("year","no_pregnancies")
records_coh<-merge.data.table(records_coh,total_coh,by="year",all=T)
MIG_T5_a_coh_2<-data.table(algorithm="MIG_T5_a", records_coh)
MIG_T5_a_coh_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_T5_a_coh_2[,percentage:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T5_a_coh_2[,denominator:="Number of pregnancies with baseline migraine(MG diagnoses or Triptan prescription), lookback= 365 days(90 days/75 days) by year"]

#by year group and maternal age
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by=c("year_group","maternal_age"), .N]
names(records)<-c("year_group","maternal_age", "no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by=c("year_group","maternal_age"),.N]
names(total)<-c("year_group","maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","maternal_age"), all=T)
MIG_T5_a_3<-data.table(algorithm="MIG_T5_a", records)
MIG_T5_a_3[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_T5_a_3<-MIG_T5_a_3[order(year_group,maternal_age)]
rm(records,total)
MIG_T5_a_3[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T5_a_3[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_T5_a_3[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_T5_a_3[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T5_a_3[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T5_a_3[lower_95_CI<0,lower_95_CI:=0]
pregnancy_d3_mig[,include:=NULL]
rm(MIG_T5_a)
fwrite(MIG_T5_a_3,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T5_a_year_age.csv"),row.names = F)
rm(MIG_T5_a_3)
pregnancy_d3_mig[,preg_baseline:=NULL]
#### MIG_T5_b:Prevalence of MG_UPC at baseline when lookback==3 months ####
print("Create algorithm MIG_T5_b")
MIG_T5_b<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_T5_b"]
inc_col<-MIG_T5_b[TYPE=="AND",STUDY_VARIABLES]

if(length(inc_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col],include:=1]}else{pregnancy_d3_mig[,include:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm/final_d3/MIG_T5_b_D3.rds"))


#export MIG_T5_b
MIG_T5_b_dt<-data.table(algorithm="MIG_T5_b", no_diagnosed_pregnancies=pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id),.N])
MIG_T5_b_dt[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T5_b_dt[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
MIG_T5_b_dt[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T5_b_dt[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T5_b_dt[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_T5_b_dt,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T5_b.csv"),row.names = F)

#number of pregnancies with baseline migraine
pregnancy_d3_mig[MG_baseline_2>=1 | Migraine_medicines_baseline_2>=1,preg_baseline_2:=1]
MIG_T5_b_coh<-data.table(algorithm="MIG_T5_b", no_diagnosed_pregnancies=pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id) & preg_baseline_2==1,.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id) & preg_baseline_2==1,.N])
MIG_T5_b_coh[,percentage:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T5_b_coh[,denominator:="Number of pregnancies with baseline migraine(MG diagnoses or Triptan prescription), lookback= 90 days/75 days"]

#by maternal age
pregnancy_d3_mig[,maternal_age:=as.character(maternal_age)]
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id), .N, by=.(maternal_age)]
names(records)<-c("maternal_age","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id), by="maternal_age",.N]
names(total)<-c("maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by="maternal_age",all=T)
MIG_T5_b_1<-data.table(algorithm="MIG_T5_b", records)
MIG_T5_b_1[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_T5_b_1<-MIG_T5_b_1[order(maternal_age)]
rm(records,total)
MIG_T5_b_1[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T5_b_1[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_T5_b_1[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_T5_b_1[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T5_b_1[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T5_b_1[lower_95_CI<0,lower_95_CI:=0]
fwrite(MIG_T5_b_1,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T5_b_age.csv"),row.names = F)
rm(MIG_T5_b_1)


#by year
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by="year", .N]
names(records)<-c("year","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by="year",.N]
names(total)<-c("year","no_pregnancies")
records<-merge.data.table(records,total,by="year",all=T)
MIG_T5_b_2<-data.table(algorithm="MIG_T5_b", records)
MIG_T5_b_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_T5_b_2<-MIG_T5_b_2[order(year)]
rm(records,total)
MIG_T5_b_2[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T5_b_2[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_T5_b_2[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_T5_b_2[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T5_b_2[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T5_b_2[lower_95_CI<0,lower_95_CI:=0]
fwrite(MIG_T5_b_2,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T5_b_year.csv"),row.names = F)
rm(MIG_T5_b_2)

#number of pregnnacies with baseline migraine
records_coh<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id) & preg_baseline_2==1,by="year",.N]
names(records_coh)<-c("year","no_diagnosed_pregnancies")
total_coh<-pregnancy_d3_mig[!duplicated(pregnancy_id) & preg_baseline_2==1,by="year",.N]
names(total_coh)<-c("year","no_pregnancies")
records_coh<-merge.data.table(records_coh,total_coh,by="year",all=T)
MIG_T5_b_coh_2<-data.table(algorithm="MIG_T5_b", records_coh)
MIG_T5_b_coh_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_T5_b_coh_2[,percentage:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T5_b_coh_2[,denominator:="Number of pregnancies with baseline migraine(MG diagnoses or Triptan prescription), lookback= 90 days/75 days by year"]

#by year group and maternal age
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by=c("year_group","maternal_age"), .N]
names(records)<-c("year_group","maternal_age", "no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by=c("year_group","maternal_age"),.N]
names(total)<-c("year_group","maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","maternal_age"), all=T)
MIG_T5_b_3<-data.table(algorithm="MIG_T5_b", records)
MIG_T5_b_3[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_T5_b_3<-MIG_T5_b_3[order(year_group,maternal_age)]
rm(records,total)
MIG_T5_b_3[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T5_b_3[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_T5_b_3[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_T5_b_3[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T5_b_3[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T5_b_3[lower_95_CI<0,lower_95_CI:=0]
pregnancy_d3_mig[,include:=NULL]
fwrite(MIG_T5_b_3,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T5_b_year_age.csv"),row.names = F)
rm(MIG_T5_b_3)
pregnancy_d3_mig[,preg_baseline_2:=NULL]
#### MIG_T5_during:Prevalence of MG_UPC during pregnancy when lookback==12 months(3 months) ####
print("Create algorithm MIG_T5_during")
MIG_T5_during<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_T5_during"]
inc_col<-MIG_T5_during[TYPE=="AND",STUDY_VARIABLES]

if(length(inc_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col],include:=1]}else{pregnancy_d3_mig[,include:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm/final_d3/MIG_T5_during_D3.rds"))

#export MIG_T5_during
MIG_T5_during_dt<-data.table(algorithm="MIG_T5_during", no_diagnosed_pregnancies=pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id),.N])
MIG_T5_during_dt[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T5_during_dt[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
MIG_T5_during_dt[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T5_during_dt[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T5_during_dt[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_T5_during_dt,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T5_during.csv"),row.names = F)

#number of pregnancies with baseline migraine
pregnancy_d3_mig[MG_during>=1 | Migraine_medicines_during>=1,preg_during:=1]
MIG_T5_during_coh<-data.table(algorithm="MIG_T5_during", no_diagnosed_pregnancies=pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id) & preg_during==1,.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id) & preg_during==1,.N])
MIG_T5_during_coh[,percentage:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T5_during_coh[,denominator:="Number of pregnancies with migraine(MG diagnoses or Triptan prescription) during pregnancy"]

#by maternal age
pregnancy_d3_mig[,maternal_age:=as.character(maternal_age)]
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id), .N, by=.(maternal_age)]
names(records)<-c("maternal_age","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id), by="maternal_age",.N]
names(total)<-c("maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by="maternal_age",all=T)
MIG_T5_during_1<-data.table(algorithm="MIG_T5_during", records)
MIG_T5_during_1[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_T5_during_1<-MIG_T5_during_1[order(maternal_age)]
rm(records,total)
MIG_T5_during_1[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T5_during_1[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_T5_during_1[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_T5_during_1[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T5_during_1[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T5_during_1[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_T5_during_1,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T5_during_age.csv"),row.names = F)
rm(MIG_T5_during_1)

#by year
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by="year", .N]
names(records)<-c("year","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by="year",.N]
names(total)<-c("year","no_pregnancies")
records<-merge.data.table(records,total,by="year",all=T)
MIG_T5_during_2<-data.table(algorithm="MIG_T5_during", records)
MIG_T5_during_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_T5_during_2<-MIG_T5_during_2[order(year)]
rm(records,total)
MIG_T5_during_2[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T5_during_2[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_T5_during_2[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_T5_during_2[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T5_during_2[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T5_during_2[lower_95_CI<0,lower_95_CI:=0]
fwrite(MIG_T5_during_2,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T5_during_year.csv"),row.names = F)
rm(MIG_T5_during_2)

#number of pregnnacies with migraine
records_coh<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id) & preg_during==1,by="year",.N]
names(records_coh)<-c("year","no_diagnosed_pregnancies")
total_coh<-pregnancy_d3_mig[!duplicated(pregnancy_id) & preg_during==1,by="year",.N]
names(total_coh)<-c("year","no_pregnancies")
records_coh<-merge.data.table(records_coh,total_coh,by="year",all=T)
MIG_T5_during_coh_2<-data.table(algorithm="MIG_T5_during", records_coh)
MIG_T5_during_coh_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_T5_during_coh_2[,percentage:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T5_during_coh_2[,denominator:="Number of pregnancies with migraine(MG diagnoses or Triptan prescription) during pregnancy by year"]

#by year group and maternal age
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by=c("year_group","maternal_age"), .N]
names(records)<-c("year_group","maternal_age", "no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by=c("year_group","maternal_age"),.N]
names(total)<-c("year_group","maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","maternal_age"), all=T)
MIG_T5_during_3<-data.table(algorithm="MIG_T5_during", records)
MIG_T5_during_3[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_T5_during_3<-MIG_T5_during_3[order(year_group,maternal_age)]
rm(records,total)
MIG_T5_during_3[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T5_during_3[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_T5_during_3[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_T5_during_3[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T5_during_3[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T5_during_3[lower_95_CI<0,lower_95_CI:=0]
pregnancy_d3_mig[,include:=NULL]
rm(MIG_T5_during)
fwrite(MIG_T5_during_3,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T5_during_year_age.csv"),row.names = F)
rm(MIG_T5_during_3)
pregnancy_d3_mig[,preg_during:=NULL]
#### MIG_T5_both:Proportion of pregnancies having the same type MG_UPC during pregnancy and before when lookback==12 months(3 months) ####
print("Create algorithm MIG_T5_both")

pregnancy_d3_mig[MG_UPC_baseline>=1 & MG_UPC_during>=1, both:=1]
pregnancy_d3_mig[MG_UPC_baseline>=1 | MG_UPC_during>=1, total:=1]

#number of pregnancies having the same type in baseline and during
no_preg_both<-pregnancy_d3_mig[both==1 & !duplicated(pregnancy_id), .N]
#number of pregnancies having the type of interest in baseline or during
no_preg_tot<-pregnancy_d3_mig[total==1 & !duplicated(pregnancy_id), .N]
percentage_preg<-round((no_preg_both/no_preg_tot)*100,1)

MIG_T5_both<-data.table(algorithm="MIG_T5_both", event_definition="MG_UPC", no_pregnancies_same_type=no_preg_both, no_pregnancies=no_preg_tot, percentage=percentage_preg)

pregnancy_d3_mig[,both:=NULL][,total:=NULL][,MG_UPC_baseline:=NULL][,MG_UPC_baseline_2:=NULL][,MG_UPC_during:=NULL]
rm(no_preg_both,no_preg_tot,percentage_preg)

fwrite(MIG_T5_both,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T5_both.csv"),row.names = F)
rm(MIG_T5_both)

#### MIG_T6_a:Prevalence of MG_UNSP at baseline when lookback==12 months(3 months) ####
print("Create algorithm MIG_T6_a")
MIG_T6_a<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_T6_a"]
inc_col<-MIG_T6_a[TYPE=="AND",STUDY_VARIABLES]

if(length(inc_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col],include:=1]}else{pregnancy_d3_mig[,include:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm/final_d3/MIG_T6_a_D3.rds"))

#export MIG_T6_a
MIG_T6_a_dt<-data.table(algorithm="MIG_T6_a", no_diagnosed_pregnancies=pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id),.N])
MIG_T6_a_dt[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T6_a_dt[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
MIG_T6_a_dt[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T6_a_dt[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T6_a_dt[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_T6_a_dt,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T6_a.csv"),row.names = F)


#number of pregnancies with baseline migraine
pregnancy_d3_mig[MG_baseline>=1 | Migraine_medicines_baseline>=1,preg_baseline:=1]
MIG_T6_a_coh<-data.table(algorithm="MIG_T6_a", no_diagnosed_pregnancies=pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id) & preg_baseline==1,.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id) & preg_baseline==1,.N])
MIG_T6_a_coh[,percentage:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T6_a_coh[,denominator:="Number of pregnancies with baseline migraine(MG diagnoses or Triptan prescription), lookback= 365 days(90 days/75 days)"]

#by maternal age
pregnancy_d3_mig[,maternal_age:=as.character(maternal_age)]
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id), .N, by=.(maternal_age)]
names(records)<-c("maternal_age","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id), by="maternal_age",.N]
names(total)<-c("maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by="maternal_age",all=T)
MIG_T6_a_1<-data.table(algorithm="MIG_T6_a", records)
MIG_T6_a_1[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_T6_a_1<-MIG_T6_a_1[order(maternal_age)]
rm(records,total)
MIG_T6_a_1[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T6_a_1[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_T6_a_1[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_T6_a_1[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T6_a_1[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T6_a_1[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_T6_a_1,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T6_a_age.csv"),row.names = F)
rm(MIG_T6_a_1)

#by year
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by="year", .N]
names(records)<-c("year","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by="year",.N]
names(total)<-c("year","no_pregnancies")
records<-merge.data.table(records,total,by="year",all=T)
MIG_T6_a_2<-data.table(algorithm="MIG_T6_a", records)
MIG_T6_a_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_T6_a_2<-MIG_T6_a_2[order(year)]
rm(records,total)
MIG_T6_a_2[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T6_a_2[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_T6_a_2[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_T6_a_2[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T6_a_2[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T6_a_2[lower_95_CI<0,lower_95_CI:=0]
fwrite(MIG_T6_a_2,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T6_a_year.csv"),row.names = F)
rm(MIG_T6_a_2)

#number of pregnnacies with baseline migraine
records_coh<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id) & preg_baseline==1,by="year",.N]
names(records_coh)<-c("year","no_diagnosed_pregnancies")
total_coh<-pregnancy_d3_mig[!duplicated(pregnancy_id) & preg_baseline==1,by="year",.N]
names(total_coh)<-c("year","no_pregnancies")
records_coh<-merge.data.table(records_coh,total_coh,by="year",all=T)
MIG_T6_a_coh_2<-data.table(algorithm="MIG_T6_a", records_coh)
MIG_T6_a_coh_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_T6_a_coh_2[,percentage:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T6_a_coh_2[,denominator:="Number of pregnancies with baseline migraine(MG diagnoses or Triptan prescription), lookback= 365 days(90 days/75 days) by year"]

#by year group and maternal age
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by=c("year_group","maternal_age"), .N]
names(records)<-c("year_group","maternal_age", "no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by=c("year_group","maternal_age"),.N]
names(total)<-c("year_group","maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","maternal_age"), all=T)
MIG_T6_a_3<-data.table(algorithm="MIG_T6_a", records)
MIG_T6_a_3[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_T6_a_3<-MIG_T6_a_3[order(year_group,maternal_age)]
rm(records,total)
MIG_T6_a_3[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T6_a_3[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_T6_a_3[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_T6_a_3[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T6_a_3[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T6_a_3[lower_95_CI<0,lower_95_CI:=0]
pregnancy_d3_mig[,include:=NULL]
rm(MIG_T6_a)
fwrite(MIG_T6_a_3,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T6_a_year_age.csv"),row.names = F)
rm(MIG_T6_a_3)
pregnancy_d3_mig[,preg_baseline:=NULL]
#### MIG_T6_b:Prevalence of MG_UNSP at baseline when lookback==3 months ####
print("Create algorithm MIG_T6_b")
MIG_T6_b<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_T6_b"]
inc_col<-MIG_T6_b[TYPE=="AND",STUDY_VARIABLES]

if(length(inc_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col],include:=1]}else{pregnancy_d3_mig[,include:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm/final_d3/MIG_T6_b_D3.rds"))


#export MIG_T6_b
MIG_T6_b_dt<-data.table(algorithm="MIG_T6_b", no_diagnosed_pregnancies=pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id),.N])
MIG_T6_b_dt[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T6_b_dt[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
MIG_T6_b_dt[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T6_b_dt[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T6_b_dt[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_T6_b_dt,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T6_b.csv"),row.names = F)

#number of pregnancies with baseline migraine
pregnancy_d3_mig[MG_baseline_2>=1 | Migraine_medicines_baseline_2>=1,preg_baseline_2:=1]
MIG_T6_b_coh<-data.table(algorithm="MIG_T6_b", no_diagnosed_pregnancies=pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id) & preg_baseline_2==1,.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id) & preg_baseline_2==1,.N])
MIG_T6_b_coh[,percentage:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T6_b_coh[,denominator:="Number of pregnancies with baseline migraine(MG diagnoses or Triptan prescription), lookback= 90 days/75 days"]

#by maternal age
pregnancy_d3_mig[,maternal_age:=as.character(maternal_age)]
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id), .N, by=.(maternal_age)]
names(records)<-c("maternal_age","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id), by="maternal_age",.N]
names(total)<-c("maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by="maternal_age",all=T)
MIG_T6_b_1<-data.table(algorithm="MIG_T6_b", records)
MIG_T6_b_1[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_T6_b_1<-MIG_T6_b_1[order(maternal_age)]
rm(records,total)
MIG_T6_b_1[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T6_b_1[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_T6_b_1[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_T6_b_1[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T6_b_1[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T6_b_1[lower_95_CI<0,lower_95_CI:=0]
fwrite(MIG_T6_b_1,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T6_b_age.csv"),row.names = F)
rm(MIG_T6_b_1)


#by year
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by="year", .N]
names(records)<-c("year","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by="year",.N]
names(total)<-c("year","no_pregnancies")
records<-merge.data.table(records,total,by="year",all=T)
MIG_T6_b_2<-data.table(algorithm="MIG_T6_b", records)
MIG_T6_b_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_T6_b_2<-MIG_T6_b_2[order(year)]
rm(records,total)
MIG_T6_b_2[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T6_b_2[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_T6_b_2[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_T6_b_2[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T6_b_2[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T6_b_2[lower_95_CI<0,lower_95_CI:=0]
fwrite(MIG_T6_b_2,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T6_b_year.csv"),row.names = F)
rm(MIG_T6_b_2)

#number of pregnnacies with baseline migraine
records_coh<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id) & preg_baseline_2==1,by="year",.N]
names(records_coh)<-c("year","no_diagnosed_pregnancies")
total_coh<-pregnancy_d3_mig[!duplicated(pregnancy_id) & preg_baseline_2==1,by="year",.N]
names(total_coh)<-c("year","no_pregnancies")
records_coh<-merge.data.table(records_coh,total_coh,by="year",all=T)
MIG_T6_b_coh_2<-data.table(algorithm="MIG_T6_b", records_coh)
MIG_T6_b_coh_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_T6_b_coh_2[,percentage:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T6_b_coh_2[,denominator:="Number of pregnancies with baseline migraine(MG diagnoses or Triptan prescription), lookback= 90 days/75 days by year"]

#by year group and maternal age
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by=c("year_group","maternal_age"), .N]
names(records)<-c("year_group","maternal_age", "no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by=c("year_group","maternal_age"),.N]
names(total)<-c("year_group","maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","maternal_age"), all=T)
MIG_T6_b_3<-data.table(algorithm="MIG_T6_b", records)
MIG_T6_b_3[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_T6_b_3<-MIG_T6_b_3[order(year_group,maternal_age)]
rm(records,total)
MIG_T6_b_3[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T6_b_3[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_T6_b_3[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_T6_b_3[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T6_b_3[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T6_b_3[lower_95_CI<0,lower_95_CI:=0]
pregnancy_d3_mig[,include:=NULL]
fwrite(MIG_T6_b_3,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T6_b_year_age.csv"),row.names = F)
rm(MIG_T6_b_3)
pregnancy_d3_mig[,preg_baseline_2:=NULL]
#### MIG_T6_during:Prevalence of MG_UNSP during pregnancy when lookback==12 months(3 months) ####
print("Create algorithm MIG_T6_during")
MIG_T6_during<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_T6_during"]
inc_col<-MIG_T6_during[TYPE=="AND",STUDY_VARIABLES]

if(length(inc_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col],include:=1]}else{pregnancy_d3_mig[,include:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm/final_d3/MIG_T6_during_D3.rds"))

#export MIG_T6_during
MIG_T6_during_dt<-data.table(algorithm="MIG_T6_during", no_diagnosed_pregnancies=pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id),.N])
MIG_T6_during_dt[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T6_during_dt[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
MIG_T6_during_dt[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T6_during_dt[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T6_during_dt[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_T6_during_dt,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T6_during.csv"),row.names = F)

#number of pregnancies with baseline migraine
pregnancy_d3_mig[MG_during>=1 | Migraine_medicines_during>=1,preg_during:=1]
MIG_T6_during_coh<-data.table(algorithm="MIG_T6_during", no_diagnosed_pregnancies=pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id) & preg_during==1,.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id) & preg_during==1,.N])
MIG_T6_during_coh[,percentage:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T6_during_coh[,denominator:="Number of pregnancies with migraine(MG diagnoses or Triptan prescription) during pregnancy"]

#by maternal age
pregnancy_d3_mig[,maternal_age:=as.character(maternal_age)]
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id), .N, by=.(maternal_age)]
names(records)<-c("maternal_age","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id), by="maternal_age",.N]
names(total)<-c("maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by="maternal_age",all=T)
MIG_T6_during_1<-data.table(algorithm="MIG_T6_during", records)
MIG_T6_during_1[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_T6_during_1<-MIG_T6_during_1[order(maternal_age)]
rm(records,total)
MIG_T6_during_1[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T6_during_1[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_T6_during_1[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_T6_during_1[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T6_during_1[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T6_during_1[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_T6_during_1,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T6_during_age.csv"),row.names = F)
rm(MIG_T6_during_1)

#by year
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by="year", .N]
names(records)<-c("year","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by="year",.N]
names(total)<-c("year","no_pregnancies")
records<-merge.data.table(records,total,by="year",all=T)
MIG_T6_during_2<-data.table(algorithm="MIG_T6_during", records)
MIG_T6_during_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_T6_during_2<-MIG_T6_during_2[order(year)]
rm(records,total)
MIG_T6_during_2[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T6_during_2[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_T6_during_2[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_T6_during_2[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T6_during_2[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T6_during_2[lower_95_CI<0,lower_95_CI:=0]
fwrite(MIG_T6_during_2,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T6_during_year.csv"),row.names = F)
rm(MIG_T6_during_2)

#number of pregnnacies with migraine
records_coh<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id) & preg_during==1,by="year",.N]
names(records_coh)<-c("year","no_diagnosed_pregnancies")
total_coh<-pregnancy_d3_mig[!duplicated(pregnancy_id) & preg_during==1,by="year",.N]
names(total_coh)<-c("year","no_pregnancies")
records_coh<-merge.data.table(records_coh,total_coh,by="year",all=T)
MIG_T6_during_coh_2<-data.table(algorithm="MIG_T6_during", records_coh)
MIG_T6_during_coh_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_T6_during_coh_2[,percentage:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T6_during_coh_2[,denominator:="Number of pregnancies with migraine(MG diagnoses or Triptan prescription) during pregnancy by year"]

#by year group and maternal age
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by=c("year_group","maternal_age"), .N]
names(records)<-c("year_group","maternal_age", "no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by=c("year_group","maternal_age"),.N]
names(total)<-c("year_group","maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","maternal_age"), all=T)
MIG_T6_during_3<-data.table(algorithm="MIG_T6_during", records)
MIG_T6_during_3[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_T6_during_3<-MIG_T6_during_3[order(year_group,maternal_age)]
rm(records,total)
MIG_T6_during_3[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_T6_during_3[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_T6_during_3[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_T6_during_3[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T6_during_3[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_T6_during_3[lower_95_CI<0,lower_95_CI:=0]
pregnancy_d3_mig[,include:=NULL]
rm(MIG_T6_during)
fwrite(MIG_T6_during_3,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T6_during_year_age.csv"),row.names = F)
rm(MIG_T6_during_3)
pregnancy_d3_mig[,preg_during:=NULL]
#### MIG_T6_both:Proportion of pregnancies having the same type MG_UNSP during pregnancy and before when lookback==12 months(3 months) ####
print("Create algorithm MIG_T6_both")

pregnancy_d3_mig[MG_UNSP_baseline>=1 & MG_UNSP_during>=1, both:=1]
pregnancy_d3_mig[MG_UNSP_baseline>=1 | MG_UNSP_during>=1, total:=1]

#number of pregnancies having the same type in baseline and during
no_preg_both<-pregnancy_d3_mig[both==1 & !duplicated(pregnancy_id), .N]
#number of pregnancies having the type of interest in baseline or during
no_preg_tot<-pregnancy_d3_mig[total==1 & !duplicated(pregnancy_id), .N]
percentage_preg<-round((no_preg_both/no_preg_tot)*100,1)

MIG_T6_both<-data.table(algorithm="MIG_T6_both", event_definition="MG_UNSP", no_pregnancies_same_type=no_preg_both, no_pregnancies=no_preg_tot, percentage=percentage_preg)

pregnancy_d3_mig[,both:=NULL][,total:=NULL][,MG_UNSP_baseline:=NULL][,MG_UNSP_baseline_2:=NULL][,MG_UNSP_during:=NULL]
rm(no_preg_both,no_preg_tot,percentage_preg)

fwrite(MIG_T6_both,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T6_both.csv"),row.names = F)
rm(MIG_T6_both)

#### Migraine type proportions ####
#Type a baseline
#MIG_T1_a_coh,MIG_T2_a_coh,MIG_T3_a_coh,MIG_T4_a_coh,MIG_T5_a_coh,MIG_T6_a_coh
MIG_T_a_coh<-rbind(MIG_T1_a_coh,MIG_T2_a_coh,MIG_T3_a_coh,MIG_T4_a_coh,MIG_T5_a_coh,MIG_T6_a_coh)
rm(MIG_T1_a_coh,MIG_T2_a_coh,MIG_T3_a_coh,MIG_T4_a_coh,MIG_T5_a_coh,MIG_T6_a_coh)

fwrite(MIG_T_a_coh,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T_a.csv"),row.names = F)
rm(MIG_T_a_coh)
#Type a baseline year
#MIG_T1_a_coh_2,MIG_T2_a_coh_2,MIG_T3_a_coh_2,MIG_T4_a_coh_2,MIG_T5_a_coh_2,MIG_T6_a_coh_2
MIG_T_a_coh_2<-rbind(MIG_T1_a_coh_2,MIG_T2_a_coh_2,MIG_T3_a_coh_2,MIG_T4_a_coh_2,MIG_T5_a_coh_2,MIG_T6_a_coh_2)
rm(MIG_T1_a_coh_2,MIG_T2_a_coh_2,MIG_T3_a_coh_2,MIG_T4_a_coh_2,MIG_T5_a_coh_2,MIG_T6_a_coh_2)

fwrite(MIG_T_a_coh_2,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T_a_year.csv"),row.names = F)
rm(MIG_T_a_coh_2)
#Type b baseline
#MIG_T1_b_coh,MIG_T2_b_coh,MIG_T3_b_coh,MIG_T4_b_coh,MIG_T5_b_coh,MIG_T6_b_coh
MIG_T_b_coh<-rbind(MIG_T1_b_coh,MIG_T2_b_coh,MIG_T3_b_coh,MIG_T4_b_coh,MIG_T5_b_coh,MIG_T6_b_coh)
rm(MIG_T1_b_coh,MIG_T2_b_coh,MIG_T3_b_coh,MIG_T4_b_coh,MIG_T5_b_coh,MIG_T6_b_coh)
fwrite(MIG_T_b_coh,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T_b.csv"),row.names = F)
rm(MIG_T_b_coh)

#Type b baseline year
#MIG_T1_b_coh_2,MIG_T2_b_coh_2,MIG_T3_b_coh_2,MIG_T4_b_coh_2,MIG_T5_b_coh_2,MIG_T6_b_coh_2
MIG_T_b_coh_2<-rbind(MIG_T1_b_coh_2,MIG_T2_b_coh_2,MIG_T3_b_coh_2,MIG_T4_b_coh_2,MIG_T5_b_coh_2,MIG_T6_b_coh_2)
rm(MIG_T1_b_coh_2,MIG_T2_b_coh_2,MIG_T3_b_coh_2,MIG_T4_b_coh_2,MIG_T5_b_coh_2,MIG_T6_b_coh_2)
fwrite(MIG_T_b_coh_2,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T_b_year.csv"),row.names = F)
rm(MIG_T_b_coh_2)

#Type during
#MIG_T1_during_coh,MIG_T2_during_coh,MIG_T3_during_coh,MIG_T4_during_coh,MIG_T5_during_coh,MIG_T6_during_coh
MIG_T_during_coh<-rbind(MIG_T1_during_coh,MIG_T2_during_coh,MIG_T3_during_coh,MIG_T4_during_coh,MIG_T5_during_coh,MIG_T6_during_coh)
rm(MIG_T1_during_coh,MIG_T2_during_coh,MIG_T3_during_coh,MIG_T4_during_coh,MIG_T5_during_coh,MIG_T6_during_coh)
fwrite(MIG_T_during_coh,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T_during.csv"),row.names = F)
rm(MIG_T_during_coh)

#Type during year
#MIG_T1_during_coh_2,MIG_T2_during_coh,MIG_T3_during_coh,MIG_T4_during_coh,MIG_T5_during_coh,MIG_T6_during_coh
MIG_T_during_coh_2<-rbind(MIG_T1_during_coh_2,MIG_T2_during_coh_2,MIG_T3_during_coh_2,MIG_T4_during_coh_2,MIG_T5_during_coh_2,MIG_T6_during_coh_2)
rm(MIG_T1_during_coh_2,MIG_T2_during_coh_2,MIG_T3_during_coh_2,MIG_T4_during_coh_2,MIG_T5_during_coh_2,MIG_T6_during_coh_2)
fwrite(MIG_T_during_coh_2,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_T_during_year.csv"),row.names = F)
rm(MIG_T_during_coh_2)

#### Save time info ####
date_running_end_04_e<-Sys.Date()
end_time_04_e<-Sys.time()

time_log_04_e<-data.table(DAP=data_access_provider_name,
                          Script="Step_04_e_migraine_algorithms_type.R", 
                          Start_date=date_running_start_04_e, 
                          End_date=date_running_end_04_e,
                          Time_elaspsed=format(end_time_04_e-initial_time_04_e, digits=2))
fwrite(time_log_04_e,paste0(output_dir,"/Time log/Step_04_e_time_log.csv"),row.names = F)
rm(time_log_04_e)





