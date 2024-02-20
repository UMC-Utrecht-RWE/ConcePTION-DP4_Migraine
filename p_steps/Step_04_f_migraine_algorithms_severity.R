initial_time_04_f<-Sys.time()
date_running_start_04_f<-Sys.Date()


#Identify all events needed to create the migraine type algorithms

#MIG_S1: MG diagnosis and no Migraine_medicines(N02CC) or Migraine_med_profilactic before and during pregnancy, baseline and during pregnancy
#MIG_S2: Migraine_medicines and no Migraine_medicines_injection, baseline and during pregnancy
#MIG_S3: Migraine_medicines_injection and no Migraine_med_profilactic during, baseline and during pregnancy
#MIG_S4: MG or Migraine_medicines and Migraine_med_profilactic,during pregnancy

####APPLY Migraine ALGORITHM####
algorithm_template<-fread(paste0(projectFolder, "/p_steps/parameters/algorithms.csv"))
#### MIG_S1_a:Prevalence of Mild migraine at baseline when lookback==12 months(3 months) ####
print("Create algorithm MIG_S1_a")
MIG_S1_a<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_S1_a"]
inc_col<-MIG_S1_a[TYPE=="AND",STUDY_VARIABLES]
excl_col<-MIG_S1_a[TYPE=="AND_NOT",STUDY_VARIABLES]

if(length(inc_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col],include:=1]}else{pregnancy_d3_mig[,include:=NA]}
if(length(excl_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("|" , lapply(.SD,`>=`, 1)),.SDcols=excl_col],exclude:=1]}else{pregnancy_d3_mig[,exclude:=NA]}

#Update include comun based on exclude
pregnancy_d3_mig[exclude==1, include:=NA]
pregnancy_d3_mig[,exclude:=NULL]

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm/final_d3/MIG_S1_a_D3.rds"))

#export MIG_S1_a
MIG_S1_a_dt<-data.table(algorithm="MIG_S1_a", no_diagnosed_pregnancies=pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id),.N])
MIG_S1_a_dt[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_S1_a_dt[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
MIG_S1_a_dt[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_S1_a_dt[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_S1_a_dt[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_S1_a_dt,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_S1_a.csv"),row.names = F)

#number of pregnancies with baseline migraine
pregnancy_d3_mig[MG_baseline>=1 | Migraine_medicines_baseline>=1,preg_baseline:=1]
MIG_S1_a_coh<-data.table(algorithm="MIG_S1_a", no_diagnosed_pregnancies=pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id) & preg_baseline==1,.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id) & preg_baseline==1,.N])
MIG_S1_a_coh[,percentage:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_S1_a_coh[,denominator:="Number of pregnancies with baseline migraine(MG diagnoses or Triptan prescription), lookback= 365 days(90 days/75 days)"]

#by maternal age
pregnancy_d3_mig[,maternal_age:=as.character(maternal_age)]
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id), .N, by=.(maternal_age)]
names(records)<-c("maternal_age","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id), by="maternal_age",.N]
names(total)<-c("maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by="maternal_age",all=T)
MIG_S1_a_1<-data.table(algorithm="MIG_S1_a", records)
MIG_S1_a_1[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_S1_a_1<-MIG_S1_a_1[order(maternal_age)]
rm(records,total)
MIG_S1_a_1[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_S1_a_1[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_S1_a_1[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_S1_a_1[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_S1_a_1[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_S1_a_1[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_S1_a_1,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_S1_a_age.csv"),row.names = F)
rm(MIG_S1_a_1)

#by year
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by="year", .N]
names(records)<-c("year","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by="year",.N]
names(total)<-c("year","no_pregnancies")
records<-merge.data.table(records,total,by="year",all=T)
MIG_S1_a_2<-data.table(algorithm="MIG_S1_a", records)
MIG_S1_a_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_S1_a_2<-MIG_S1_a_2[order(year)]
rm(records,total)
MIG_S1_a_2[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_S1_a_2[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_S1_a_2[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_S1_a_2[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_S1_a_2[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_S1_a_2[lower_95_CI<0,lower_95_CI:=0]
fwrite(MIG_S1_a_2,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_S1_a_year.csv"),row.names = F)
rm(MIG_S1_a_2)

#number of pregnnacies with baseline migraine
records_coh<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id) & preg_baseline==1,by="year",.N]
names(records_coh)<-c("year","no_diagnosed_pregnancies")
total_coh<-pregnancy_d3_mig[!duplicated(pregnancy_id) & preg_baseline==1,by="year",.N]
names(total_coh)<-c("year","no_pregnancies")
records_coh<-merge.data.table(records_coh,total_coh,by="year",all=T)
MIG_S1_a_coh_2<-data.table(algorithm="MIG_S1_a", records_coh)
MIG_S1_a_coh_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_S1_a_coh_2[,percentage:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_S1_a_coh_2[,denominator:="Number of pregnancies with baseline migraine(MG diagnoses or Triptan prescription), lookback= 365 days(90 days/75 days) by year"]

#by year group and maternal age
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by=c("year_group","maternal_age"), .N]
names(records)<-c("year_group","maternal_age", "no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by=c("year_group","maternal_age"),.N]
names(total)<-c("year_group","maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","maternal_age"), all=T)
MIG_S1_a_3<-data.table(algorithm="MIG_S1_a", records)
MIG_S1_a_3[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_S1_a_3<-MIG_S1_a_3[order(year_group,maternal_age)]
rm(records,total)
MIG_S1_a_3[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_S1_a_3[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_S1_a_3[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_S1_a_3[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_S1_a_3[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_S1_a_3[lower_95_CI<0,lower_95_CI:=0]
pregnancy_d3_mig[,include:=NULL]
rm(MIG_S1_a)
fwrite(MIG_S1_a_3,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_S1_a_year_age.csv"),row.names = F)
rm(MIG_S1_a_3)

#### MIG_S1_b:Prevalence of Mild migraine at baseline when lookback==3 months####
print("Create algorithm MIG_S1_b")
MIG_S1_b<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_S1_b"]
inc_col<-MIG_S1_b[TYPE=="AND",STUDY_VARIABLES]
excl_col<-MIG_S1_b[TYPE=="AND_NOT",STUDY_VARIABLES]

if(length(inc_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col],include:=1]}else{pregnancy_d3_mig[,include:=NA]}
if(length(excl_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("|" , lapply(.SD,`>=`, 1)),.SDcols=excl_col],exclude:=1]}else{pregnancy_d3_mig[,exclude:=NA]}

#Update include comun based on exclude
pregnancy_d3_mig[exclude==1, include:=NA]
pregnancy_d3_mig[,exclude:=NULL]

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm/final_d3/MIG_S1_b_D3.rds"))

#export MIG_S1_b
MIG_S1_b_dt<-data.table(algorithm="MIG_S1_b", no_diagnosed_pregnancies=pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id),.N])
MIG_S1_b_dt[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_S1_b_dt[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
MIG_S1_b_dt[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_S1_b_dt[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_S1_b_dt[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_S1_b_dt,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_S1_b.csv"),row.names = F)

#number of pregnancies with baseline migraine
pregnancy_d3_mig[MG_baseline_2>=1 | Migraine_medicines_baseline_2>=1,preg_baseline_2:=1]
MIG_S1_b_coh<-data.table(algorithm="MIG_S1_b", no_diagnosed_pregnancies=pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id) & preg_baseline_2==1,.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id) & preg_baseline_2==1,.N])
MIG_S1_b_coh[,percentage:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_S1_b_coh[,denominator:="Number of pregnancies with baseline migraine(MG diagnoses or Triptan prescription), lookback= 90 days/75 days"]

#by maternal age
pregnancy_d3_mig[,maternal_age:=as.character(maternal_age)]
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id), .N, by=.(maternal_age)]
names(records)<-c("maternal_age","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id), by="maternal_age",.N]
names(total)<-c("maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by="maternal_age",all=T)
MIG_S1_b_1<-data.table(algorithm="MIG_S1_b", records)
MIG_S1_b_1[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_S1_b_1<-MIG_S1_b_1[order(maternal_age)]
rm(records,total)
MIG_S1_b_1[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_S1_b_1[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_S1_b_1[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_S1_b_1[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_S1_b_1[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_S1_b_1[lower_95_CI<0,lower_95_CI:=0]
fwrite(MIG_S1_b_1,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_S1_b_age.csv"),row.names = F)
rm(MIG_S1_b_1)


#by year
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by="year", .N]
names(records)<-c("year","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by="year",.N]
names(total)<-c("year","no_pregnancies")
records<-merge.data.table(records,total,by="year",all=T)
MIG_S1_b_2<-data.table(algorithm="MIG_S1_b", records)
MIG_S1_b_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_S1_b_2<-MIG_S1_b_2[order(year)]
rm(records,total)
MIG_S1_b_2[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_S1_b_2[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_S1_b_2[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_S1_b_2[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_S1_b_2[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_S1_b_2[lower_95_CI<0,lower_95_CI:=0]
fwrite(MIG_S1_b_2,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_S1_b_year.csv"),row.names = F)
rm(MIG_S1_b_2)

#number of pregnnacies with baseline migraine
records_coh<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id) & preg_baseline_2==1,by="year",.N]
names(records_coh)<-c("year","no_diagnosed_pregnancies")
total_coh<-pregnancy_d3_mig[!duplicated(pregnancy_id) & preg_baseline_2==1,by="year",.N]
names(total_coh)<-c("year","no_pregnancies")
records_coh<-merge.data.table(records_coh,total_coh,by="year",all=T)
MIG_S1_b_coh_2<-data.table(algorithm="MIG_S1_b", records_coh)
MIG_S1_b_coh_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_S1_b_coh_2[,percentage:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_S1_b_coh_2[,denominator:="Number of pregnancies with baseline migraine(MG diagnoses or Triptan prescription), lookback= 90 days/75 days by year"]


#by year group and maternal age
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by=c("year_group","maternal_age"), .N]
names(records)<-c("year_group","maternal_age", "no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by=c("year_group","maternal_age"),.N]
names(total)<-c("year_group","maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","maternal_age"), all=T)
MIG_S1_b_3<-data.table(algorithm="MIG_S1_b", records)
MIG_S1_b_3[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_S1_b_3<-MIG_S1_b_3[order(year_group,maternal_age)]
rm(records,total)
MIG_S1_b_3[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_S1_b_3[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_S1_b_3[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_S1_b_3[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_S1_b_3[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_S1_b_3[lower_95_CI<0,lower_95_CI:=0]
pregnancy_d3_mig[,include:=NULL]
fwrite(MIG_S1_b_3,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_S1_b_year_age.csv"),row.names = F)
rm(MIG_S1_b_3)

#### MIG_S1_during:Prevalence of Mild migraine during pregnancy when lookback==12 months(3 months) ####
print("Create algorithm MIG_S1_during")
MIG_S1_during<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_S1_during"]
inc_col<-MIG_S1_during[TYPE=="AND",STUDY_VARIABLES]
excl_col<-MIG_S1_during[TYPE=="AND_NOT",STUDY_VARIABLES]

if(length(inc_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col],include:=1]}else{pregnancy_d3_mig[,include:=NA]}
if(length(excl_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("|" , lapply(.SD,`>=`, 1)),.SDcols=excl_col],exclude:=1]}else{pregnancy_d3_mig[,exclude:=NA]}

#Update include comun based on exclude
pregnancy_d3_mig[exclude==1, include:=NA]
pregnancy_d3_mig[,exclude:=NULL]

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm/final_d3/MIG_S1_during_D3.rds"))

#export MIG_S1_during
MIG_S1_during_dt<-data.table(algorithm="MIG_S1_during", no_diagnosed_pregnancies=pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id),.N])
MIG_S1_during_dt[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_S1_during_dt[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
MIG_S1_during_dt[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_S1_during_dt[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_S1_during_dt[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_S1_during_dt,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_S1_during.csv"),row.names = F)

#number of pregnancies with baseline migraine
pregnancy_d3_mig[MG_during>=1 | Migraine_medicines_during>=1,preg_during:=1]
MIG_S1_during_coh<-data.table(algorithm="MIG_S1_during", no_diagnosed_pregnancies=pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id) & preg_during==1,.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id) & preg_during==1,.N])
MIG_S1_during_coh[,percentage:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_S1_during_coh[,denominator:="Number of pregnancies with migraine(MG diagnoses or Triptan prescription) during pregnancy"]

#by maternal age
pregnancy_d3_mig[,maternal_age:=as.character(maternal_age)]
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id), .N, by=.(maternal_age)]
names(records)<-c("maternal_age","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id), by="maternal_age",.N]
names(total)<-c("maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by="maternal_age",all=T)
MIG_S1_during_1<-data.table(algorithm="MIG_S1_during", records)
MIG_S1_during_1[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_S1_during_1<-MIG_S1_during_1[order(maternal_age)]
rm(records,total)
MIG_S1_during_1[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_S1_during_1[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_S1_during_1[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_S1_during_1[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_S1_during_1[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_S1_during_1[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_S1_during_1,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_S1_during_age.csv"),row.names = F)
rm(MIG_S1_during_1)

#by year
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by="year", .N]
names(records)<-c("year","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by="year",.N]
names(total)<-c("year","no_pregnancies")
records<-merge.data.table(records,total,by="year",all=T)
MIG_S1_during_2<-data.table(algorithm="MIG_S1_during", records)
MIG_S1_during_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_S1_during_2<-MIG_S1_during_2[order(year)]
rm(records,total)
MIG_S1_during_2[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_S1_during_2[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_S1_during_2[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_S1_during_2[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_S1_during_2[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_S1_during_2[lower_95_CI<0,lower_95_CI:=0]
fwrite(MIG_S1_during_2,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_S1_during_year.csv"),row.names = F)
rm(MIG_S1_during_2)

#number of pregnnacies with migraine
records_coh<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id) & preg_during==1,by="year",.N]
names(records_coh)<-c("year","no_diagnosed_pregnancies")
total_coh<-pregnancy_d3_mig[!duplicated(pregnancy_id) & preg_during==1,by="year",.N]
names(total_coh)<-c("year","no_pregnancies")
records_coh<-merge.data.table(records_coh,total_coh,by="year",all=T)
MIG_S1_during_coh_2<-data.table(algorithm="MIG_S1_during", records_coh)
MIG_S1_during_coh_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_S1_during_coh_2[,percentage:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_S1_during_coh_2[,denominator:="Number of pregnancies with migraine(MG diagnoses or Triptan prescription) during pregnancy by year"]

#by year group and maternal age
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by=c("year_group","maternal_age"), .N]
names(records)<-c("year_group","maternal_age", "no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by=c("year_group","maternal_age"),.N]
names(total)<-c("year_group","maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","maternal_age"), all=T)
MIG_S1_during_3<-data.table(algorithm="MIG_S1_during", records)
MIG_S1_during_3[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_S1_during_3<-MIG_S1_during_3[order(year_group,maternal_age)]
rm(records,total)
MIG_S1_during_3[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_S1_during_3[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_S1_during_3[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_S1_during_3[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_S1_during_3[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_S1_during_3[lower_95_CI<0,lower_95_CI:=0]
pregnancy_d3_mig[,include:=NULL]
rm(MIG_S1_during)
fwrite(MIG_S1_during_3,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_S1_during_year_age.csv"),row.names = F)
rm(MIG_S1_during_3)

#### MIG_S1_both:Proportion of pregnancies having the same type S1 during pregnancy and before when lookback==12 months(3 months) ####
print("Create algorithm MIG_S1_both")

pregnancy_d3_mig[MG_baseline>=1 & Migraine_medicines_baseline==0 & Migraine_medicines_during==0 & Migraine_med_profilactic_baseline==0 & Migraine_med_profilactic_during==0, S_1:=1]
pregnancy_d3_mig[MG_during>=1 & Migraine_medicines_baseline==0 & Migraine_medicines_during==0 & Migraine_med_profilactic_baseline==0 & Migraine_med_profilactic_during==0, S_2:=1]
pregnancy_d3_mig[S_1==1 & S_2==1, both:=1]
pregnancy_d3_mig[S_1==1 | S_2==1, total:=1]

#number of pregnancies having the same type in baseline and during
no_preg_both<-pregnancy_d3_mig[both==1 & !duplicated(pregnancy_id), .N]
#number of pregnancies having the type of interest in baseline or during
no_preg_tot<-pregnancy_d3_mig[total==1 & !duplicated(pregnancy_id), .N]
percentage_preg<-round((no_preg_both/no_preg_tot)*100,1)

MIG_S1_both<-data.table(algorithm="MIG_S1_both", event_definition="Mild", no_pregnancies_same_type=no_preg_both, no_pregnancies=no_preg_tot, percentage=percentage_preg)

pregnancy_d3_mig[,both:=NULL][,total:=NULL][,S_1:=NULL][,S_2:=NULL]
rm(no_preg_both,no_preg_tot,percentage_preg)

fwrite(MIG_S1_both,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_S1_both.csv"),row.names = F)
rm(MIG_S1_both)


#### MIG_S2_a:Prevalence of Moderate migraine at baseline when lookback==12 months(3 months) ####
print("Create algorithm MIG_S2_a")
MIG_S2_a<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_S2_a"]
inc_col<-MIG_S2_a[TYPE=="AND",STUDY_VARIABLES]
excl_col<-MIG_S2_a[TYPE=="AND_NOT",STUDY_VARIABLES]

if(length(inc_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col],include:=1]}else{pregnancy_d3_mig[,include:=NA]}
if(length(excl_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("|" , lapply(.SD,`>=`, 1)),.SDcols=excl_col],exclude:=1]}else{pregnancy_d3_mig[,exclude:=NA]}

#Update include comun based on exclude
pregnancy_d3_mig[exclude==1, include:=NA]
pregnancy_d3_mig[,exclude:=NULL]

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm/final_d3/MIG_S2_a_D3.rds"))

#export MIG_S2_a
MIG_S2_a_dt<-data.table(algorithm="MIG_S2_a", no_diagnosed_pregnancies=pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id),.N])
MIG_S2_a_dt[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_S2_a_dt[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
MIG_S2_a_dt[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_S2_a_dt[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_S2_a_dt[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_S2_a_dt,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_S2_a.csv"),row.names = F)

#number of pregnancies with baseline migraine
pregnancy_d3_mig[MG_baseline>=1 | Migraine_medicines_baseline>=1,preg_baseline:=1]
MIG_S2_a_coh<-data.table(algorithm="MIG_S2_a", no_diagnosed_pregnancies=pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id) & preg_baseline==1,.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id) & preg_baseline==1,.N])
MIG_S2_a_coh[,percentage:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_S2_a_coh[,denominator:="Number of pregnancies with baseline migraine(MG diagnoses or Triptan prescription), lookback= 365 days(90 days/75 days)"]

#by maternal age
pregnancy_d3_mig[,maternal_age:=as.character(maternal_age)]
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id), .N, by=.(maternal_age)]
names(records)<-c("maternal_age","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id), by="maternal_age",.N]
names(total)<-c("maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by="maternal_age",all=T)
MIG_S2_a_1<-data.table(algorithm="MIG_S2_a", records)
MIG_S2_a_1[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_S2_a_1<-MIG_S2_a_1[order(maternal_age)]
rm(records,total)
MIG_S2_a_1[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_S2_a_1[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_S2_a_1[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_S2_a_1[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_S2_a_1[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_S2_a_1[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_S2_a_1,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_S2_a_age.csv"),row.names = F)
rm(MIG_S2_a_1)

#by year
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by="year", .N]
names(records)<-c("year","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by="year",.N]
names(total)<-c("year","no_pregnancies")
records<-merge.data.table(records,total,by="year",all=T)
MIG_S2_a_2<-data.table(algorithm="MIG_S2_a", records)
MIG_S2_a_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_S2_a_2<-MIG_S2_a_2[order(year)]
rm(records,total)
MIG_S2_a_2[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_S2_a_2[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_S2_a_2[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_S2_a_2[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_S2_a_2[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_S2_a_2[lower_95_CI<0,lower_95_CI:=0]
fwrite(MIG_S2_a_2,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_S2_a_year.csv"),row.names = F)
rm(MIG_S2_a_2)

#number of pregnnacies with baseline migraine
records_coh<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id) & preg_baseline==1,by="year",.N]
names(records_coh)<-c("year","no_diagnosed_pregnancies")
total_coh<-pregnancy_d3_mig[!duplicated(pregnancy_id) & preg_baseline==1,by="year",.N]
names(total_coh)<-c("year","no_pregnancies")
records_coh<-merge.data.table(records_coh,total_coh,by="year",all=T)
MIG_S2_a_coh_2<-data.table(algorithm="MIG_S2_a", records_coh)
MIG_S2_a_coh_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_S2_a_coh_2[,percentage:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_S2_a_coh_2[,denominator:="Number of pregnancies with baseline migraine(MG diagnoses or Triptan prescription), lookback= 365 days(90 days/75 days) by year"]

#by year group and maternal age
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by=c("year_group","maternal_age"), .N]
names(records)<-c("year_group","maternal_age", "no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by=c("year_group","maternal_age"),.N]
names(total)<-c("year_group","maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","maternal_age"), all=T)
MIG_S2_a_3<-data.table(algorithm="MIG_S2_a", records)
MIG_S2_a_3[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_S2_a_3<-MIG_S2_a_3[order(year_group,maternal_age)]
rm(records,total)
MIG_S2_a_3[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_S2_a_3[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_S2_a_3[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_S2_a_3[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_S2_a_3[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_S2_a_3[lower_95_CI<0,lower_95_CI:=0]
pregnancy_d3_mig[,include:=NULL]
rm(MIG_S2_a)
fwrite(MIG_S2_a_3,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_S2_a_year_age.csv"),row.names = F)
rm(MIG_S2_a_3)

#### MIG_S2_b:Prevalence of Moderate migraine at baseline when lookback==3 months ####
print("Create algorithm MIG_S2_b")
MIG_S2_b<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_S2_b"]
inc_col<-MIG_S2_b[TYPE=="AND",STUDY_VARIABLES]
excl_col<-MIG_S2_b[TYPE=="AND_NOT",STUDY_VARIABLES]

if(length(inc_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col],include:=1]}else{pregnancy_d3_mig[,include:=NA]}
if(length(excl_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("|" , lapply(.SD,`>=`, 1)),.SDcols=excl_col],exclude:=1]}else{pregnancy_d3_mig[,exclude:=NA]}

#Update include comun based on exclude
pregnancy_d3_mig[exclude==1, include:=NA]
pregnancy_d3_mig[,exclude:=NULL]

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm/final_d3/MIG_S2_b_D3.rds"))

#export MIG_S2_b
MIG_S2_b_dt<-data.table(algorithm="MIG_S2_b", no_diagnosed_pregnancies=pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id),.N])
MIG_S2_b_dt[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_S2_b_dt[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
MIG_S2_b_dt[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_S2_b_dt[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_S2_b_dt[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_S2_b_dt,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_S2_b.csv"),row.names = F)

#number of pregnancies with baseline migraine
pregnancy_d3_mig[MG_baseline_2>=1 | Migraine_medicines_baseline_2>=1,preg_baseline_2:=1]
MIG_S2_b_coh<-data.table(algorithm="MIG_S2_b", no_diagnosed_pregnancies=pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id) & preg_baseline_2==1,.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id) & preg_baseline_2==1,.N])
MIG_S2_b_coh[,percentage:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_S2_b_coh[,denominator:="Number of pregnancies with baseline migraine(MG diagnoses or Triptan prescription), lookback= 90 days/75 days"]

#by maternal age
pregnancy_d3_mig[,maternal_age:=as.character(maternal_age)]
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id), .N, by=.(maternal_age)]
names(records)<-c("maternal_age","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id), by="maternal_age",.N]
names(total)<-c("maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by="maternal_age",all=T)
MIG_S2_b_1<-data.table(algorithm="MIG_S2_b", records)
MIG_S2_b_1[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_S2_b_1<-MIG_S2_b_1[order(maternal_age)]
rm(records,total)
MIG_S2_b_1[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_S2_b_1[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_S2_b_1[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_S2_b_1[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_S2_b_1[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_S2_b_1[lower_95_CI<0,lower_95_CI:=0]
fwrite(MIG_S2_b_1,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_S2_b_age.csv"),row.names = F)
rm(MIG_S2_b_1)


#by year
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by="year", .N]
names(records)<-c("year","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by="year",.N]
names(total)<-c("year","no_pregnancies")
records<-merge.data.table(records,total,by="year",all=T)
MIG_S2_b_2<-data.table(algorithm="MIG_S2_b", records)
MIG_S2_b_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_S2_b_2<-MIG_S2_b_2[order(year)]
rm(records,total)
MIG_S2_b_2[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_S2_b_2[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_S2_b_2[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_S2_b_2[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_S2_b_2[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_S2_b_2[lower_95_CI<0,lower_95_CI:=0]
fwrite(MIG_S2_b_2,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_S2_b_year.csv"),row.names = F)
rm(MIG_S2_b_2)

#number of pregnnacies with baseline migraine
records_coh<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id) & preg_baseline_2==1,by="year",.N]
names(records_coh)<-c("year","no_diagnosed_pregnancies")
total_coh<-pregnancy_d3_mig[!duplicated(pregnancy_id) & preg_baseline_2==1,by="year",.N]
names(total_coh)<-c("year","no_pregnancies")
records_coh<-merge.data.table(records_coh,total_coh,by="year",all=T)
MIG_S2_b_coh_2<-data.table(algorithm="MIG_S2_b", records_coh)
MIG_S2_b_coh_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_S2_b_coh_2[,percentage:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_S2_b_coh_2[,denominator:="Number of pregnancies with baseline migraine(MG diagnoses or Triptan prescription), lookback= 90 days/75 days by year"]

#by year group and maternal age
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by=c("year_group","maternal_age"), .N]
names(records)<-c("year_group","maternal_age", "no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by=c("year_group","maternal_age"),.N]
names(total)<-c("year_group","maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","maternal_age"), all=T)
MIG_S2_b_3<-data.table(algorithm="MIG_S2_b", records)
MIG_S2_b_3[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_S2_b_3<-MIG_S2_b_3[order(year_group,maternal_age)]
rm(records,total)
MIG_S2_b_3[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_S2_b_3[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_S2_b_3[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_S2_b_3[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_S2_b_3[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_S2_b_3[lower_95_CI<0,lower_95_CI:=0]
pregnancy_d3_mig[,include:=NULL]
fwrite(MIG_S2_b_3,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_S2_b_year_age.csv"),row.names = F)
rm(MIG_S2_b_3)

#### MIG_S2_during:Prevalence of Moderate migrain during pregnancy when lookback==12 months(3 months) ####
print("Create algorithm MIG_S2_during")
MIG_S2_during<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_S2_during"]
inc_col<-MIG_S2_during[TYPE=="AND",STUDY_VARIABLES]
excl_col<-MIG_S2_during[TYPE=="AND_NOT",STUDY_VARIABLES]

if(length(inc_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col],include:=1]}else{pregnancy_d3_mig[,include:=NA]}
if(length(excl_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("|" , lapply(.SD,`>=`, 1)),.SDcols=excl_col],exclude:=1]}else{pregnancy_d3_mig[,exclude:=NA]}

#Update include comun based on exclude
pregnancy_d3_mig[exclude==1, include:=NA]
pregnancy_d3_mig[,exclude:=NULL]

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm/final_d3/MIG_S2_during_D3.rds"))

#export MIG_S2_during
MIG_S2_during_dt<-data.table(algorithm="MIG_S2_during", no_diagnosed_pregnancies=pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id),.N])
MIG_S2_during_dt[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_S2_during_dt[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
MIG_S2_during_dt[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_S2_during_dt[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_S2_during_dt[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_S2_during_dt,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_S2_during.csv"),row.names = F)

#number of pregnancies with baseline migraine
pregnancy_d3_mig[MG_during>=1 | Migraine_medicines_during>=1,preg_during:=1]
MIG_S2_during_coh<-data.table(algorithm="MIG_S2_during", no_diagnosed_pregnancies=pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id) & preg_during==1,.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id) & preg_during==1,.N])
MIG_S2_during_coh[,percentage:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_S2_during_coh[,denominator:="Number of pregnancies with migraine(MG diagnoses or Triptan prescription) during pregnancy"]

#by maternal age
pregnancy_d3_mig[,maternal_age:=as.character(maternal_age)]
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id), .N, by=.(maternal_age)]
names(records)<-c("maternal_age","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id), by="maternal_age",.N]
names(total)<-c("maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by="maternal_age",all=T)
MIG_S2_during_1<-data.table(algorithm="MIG_S2_during", records)
MIG_S2_during_1[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_S2_during_1<-MIG_S2_during_1[order(maternal_age)]
rm(records,total)
MIG_S2_during_1[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_S2_during_1[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_S2_during_1[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_S2_during_1[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_S2_during_1[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_S2_during_1[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_S2_during_1,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_S2_during_age.csv"),row.names = F)
rm(MIG_S2_during_1)

#by year
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by="year", .N]
names(records)<-c("year","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by="year",.N]
names(total)<-c("year","no_pregnancies")
records<-merge.data.table(records,total,by="year",all=T)
MIG_S2_during_2<-data.table(algorithm="MIG_S2_during", records)
MIG_S2_during_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_S2_during_2<-MIG_S2_during_2[order(year)]
rm(records,total)
MIG_S2_during_2[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_S2_during_2[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_S2_during_2[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_S2_during_2[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_S2_during_2[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_S2_during_2[lower_95_CI<0,lower_95_CI:=0]
fwrite(MIG_S2_during_2,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_S2_during_year.csv"),row.names = F)
rm(MIG_S2_during_2)

#number of pregnnacies with migraine
records_coh<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id) & preg_during==1,by="year",.N]
names(records_coh)<-c("year","no_diagnosed_pregnancies")
total_coh<-pregnancy_d3_mig[!duplicated(pregnancy_id) & preg_during==1,by="year",.N]
names(total_coh)<-c("year","no_pregnancies")
records_coh<-merge.data.table(records_coh,total_coh,by="year",all=T)
MIG_S2_during_coh_2<-data.table(algorithm="MIG_S2_during", records_coh)
MIG_S2_during_coh_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_S2_during_coh_2[,percentage:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_S2_during_coh_2[,denominator:="Number of pregnancies with migraine(MG diagnoses or Triptan prescription) during pregnancy by year"]

#by year group and maternal age
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by=c("year_group","maternal_age"), .N]
names(records)<-c("year_group","maternal_age", "no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by=c("year_group","maternal_age"),.N]
names(total)<-c("year_group","maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","maternal_age"), all=T)
MIG_S2_during_3<-data.table(algorithm="MIG_S2_during", records)
MIG_S2_during_3[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_S2_during_3<-MIG_S2_during_3[order(year_group,maternal_age)]
rm(records,total)
MIG_S2_during_3[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_S2_during_3[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_S2_during_3[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_S2_during_3[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_S2_during_3[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_S2_during_3[lower_95_CI<0,lower_95_CI:=0]
pregnancy_d3_mig[,include:=NULL]
rm(MIG_S2_during)
fwrite(MIG_S2_during_3,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_S2_during_year_age.csv"),row.names = F)
rm(MIG_S2_during_3)

#### MIG_S2_both:Proportion of pregnancies having the same type S2 during pregnancy and before when lookback==12 months(3 months) ####
print("Create algorithm MIG_S2_both")

pregnancy_d3_mig[Migraine_medicines_baseline>=1 & Migraine_injections_baseline==0 & Migraine_injections_during==0, S_1:=1]
pregnancy_d3_mig[Migraine_medicines_during>=1 & Migraine_injections_baseline==0 & Migraine_injections_during==0, S_2:=1]
pregnancy_d3_mig[S_1==1 & S_2==1, both:=1]
pregnancy_d3_mig[S_1==1 | S_2==1, total:=1]

#number of pregnancies having the same type in baseline and during
no_preg_both<-pregnancy_d3_mig[both==1 & !duplicated(pregnancy_id), .N]
#number of pregnancies having the type of interest in baseline or during
no_preg_tot<-pregnancy_d3_mig[total==1 & !duplicated(pregnancy_id), .N]
percentage_preg<-round((no_preg_both/no_preg_tot)*100,1)

MIG_S2_both<-data.table(algorithm="MIG_S2_both", event_definition="Moderate", no_pregnancies_same_type=no_preg_both, no_pregnancies=no_preg_tot, percentage=percentage_preg)

pregnancy_d3_mig[,both:=NULL][,total:=NULL][,S_1:=NULL][,S_2:=NULL]
rm(no_preg_both,no_preg_tot,percentage_preg)

fwrite(MIG_S2_both,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_S2_both.csv"),row.names = F)
rm(MIG_S2_both)


#### MIG_S3_a:Prevalence of Severe migraine at baseline when lookback==12 months(3 months) ####
print("Create algorithm MIG_S3_a")
MIG_S3_a<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_S3_a"]
inc_col<-MIG_S3_a[TYPE=="AND",STUDY_VARIABLES]
excl_col<-MIG_S3_a[TYPE=="AND_NOT",STUDY_VARIABLES]

if(length(inc_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col],include:=1]}else{pregnancy_d3_mig[,include:=NA]}
if(length(excl_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("|" , lapply(.SD,`>=`, 1)),.SDcols=excl_col],exclude:=1]}else{pregnancy_d3_mig[,exclude:=NA]}

#Update include comun based on exclude
pregnancy_d3_mig[exclude==1, include:=NA]
pregnancy_d3_mig[,exclude:=NULL]

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm/final_d3/MIG_S3_a_D3.rds"))

#export MIG_S3_a
MIG_S3_a_dt<-data.table(algorithm="MIG_S3_a", no_diagnosed_pregnancies=pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id),.N])
MIG_S3_a_dt[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_S3_a_dt[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
MIG_S3_a_dt[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_S3_a_dt[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_S3_a_dt[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_S3_a_dt,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_S3_a.csv"),row.names = F)

#number of pregnancies with baseline migraine
pregnancy_d3_mig[MG_baseline>=1 | Migraine_medicines_baseline>=1,preg_baseline:=1]
MIG_S3_a_coh<-data.table(algorithm="MIG_S3_a", no_diagnosed_pregnancies=pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id) & preg_baseline==1,.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id) & preg_baseline==1,.N])
MIG_S3_a_coh[,percentage:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_S3_a_coh[,denominator:="Number of pregnancies with baseline migraine(MG diagnoses or Triptan prescription), lookback= 365 days(90 days/75 days)"]

#by maternal age
pregnancy_d3_mig[,maternal_age:=as.character(maternal_age)]
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id), .N, by=.(maternal_age)]
names(records)<-c("maternal_age","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id), by="maternal_age",.N]
names(total)<-c("maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by="maternal_age",all=T)
MIG_S3_a_1<-data.table(algorithm="MIG_S3_a", records)
MIG_S3_a_1[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_S3_a_1<-MIG_S3_a_1[order(maternal_age)]
rm(records,total)
MIG_S3_a_1[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_S3_a_1[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_S3_a_1[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_S3_a_1[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_S3_a_1[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_S3_a_1[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_S3_a_1,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_S3_a_age.csv"),row.names = F)
rm(MIG_S3_a_1)

#by year
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by="year", .N]
names(records)<-c("year","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by="year",.N]
names(total)<-c("year","no_pregnancies")
records<-merge.data.table(records,total,by="year",all=T)
MIG_S3_a_2<-data.table(algorithm="MIG_S3_a", records)
MIG_S3_a_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_S3_a_2<-MIG_S3_a_2[order(year)]
rm(records,total)
MIG_S3_a_2[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_S3_a_2[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_S3_a_2[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_S3_a_2[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_S3_a_2[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_S3_a_2[lower_95_CI<0,lower_95_CI:=0]
fwrite(MIG_S3_a_2,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_S3_a_year.csv"),row.names = F)
rm(MIG_S3_a_2)

#number of pregnnacies with baseline migraine
records_coh<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id) & preg_baseline==1,by="year",.N]
names(records_coh)<-c("year","no_diagnosed_pregnancies")
total_coh<-pregnancy_d3_mig[!duplicated(pregnancy_id) & preg_baseline==1,by="year",.N]
names(total_coh)<-c("year","no_pregnancies")
records_coh<-merge.data.table(records_coh,total_coh,by="year",all=T)
MIG_S3_a_coh_2<-data.table(algorithm="MIG_S3_a", records_coh)
MIG_S3_a_coh_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_S3_a_coh_2[,percentage:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_S3_a_coh_2[,denominator:="Number of pregnancies with baseline migraine(MG diagnoses or Triptan prescription), lookback= 365 days(90 days/75 days) by year"]

#by year group and maternal age
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by=c("year_group","maternal_age"), .N]
names(records)<-c("year_group","maternal_age", "no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by=c("year_group","maternal_age"),.N]
names(total)<-c("year_group","maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","maternal_age"), all=T)
MIG_S3_a_3<-data.table(algorithm="MIG_S3_a", records)
MIG_S3_a_3[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_S3_a_3<-MIG_S3_a_3[order(year_group,maternal_age)]
rm(records,total)
MIG_S3_a_3[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_S3_a_3[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_S3_a_3[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_S3_a_3[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_S3_a_3[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_S3_a_3[lower_95_CI<0,lower_95_CI:=0]
pregnancy_d3_mig[,include:=NULL]
rm(MIG_S3_a)
fwrite(MIG_S3_a_3,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_S3_a_year_age.csv"),row.names = F)
rm(MIG_S3_a_3)

#### MIG_S3_b:Prevalence of Severe migraine at baseline when lookback==3 months ####
print("Create algorithm MIG_S3_b")
MIG_S3_b<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_S3_b"]
inc_col<-MIG_S3_b[TYPE=="AND",STUDY_VARIABLES]
excl_col<-MIG_S3_b[TYPE=="AND_NOT",STUDY_VARIABLES]

if(length(inc_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col],include:=1]}else{pregnancy_d3_mig[,include:=NA]}
if(length(excl_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("|" , lapply(.SD,`>=`, 1)),.SDcols=excl_col],exclude:=1]}else{pregnancy_d3_mig[,exclude:=NA]}

#Update include comun based on exclude
pregnancy_d3_mig[exclude==1, include:=NA]
pregnancy_d3_mig[,exclude:=NULL]

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm/final_d3/MIG_S3_b_D3.rds"))

#export MIG_S3_b
MIG_S3_b_dt<-data.table(algorithm="MIG_S3_b", no_diagnosed_pregnancies=pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id),.N])
MIG_S3_b_dt[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_S3_b_dt[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
MIG_S3_b_dt[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_S3_b_dt[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_S3_b_dt[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_S3_b_dt,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_S3_b.csv"),row.names = F)

#number of pregnancies with baseline migraine
pregnancy_d3_mig[MG_baseline_2>=1 | Migraine_medicines_baseline_2>=1,preg_baseline_2:=1]
MIG_S3_b_coh<-data.table(algorithm="MIG_S3_b", no_diagnosed_pregnancies=pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id) & preg_baseline_2==1,.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id) & preg_baseline_2==1,.N])
MIG_S3_b_coh[,percentage:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_S3_b_coh[,denominator:="Number of pregnancies with baseline migraine(MG diagnoses or Triptan prescription), lookback= 90 days/75 days"]

#by maternal age
pregnancy_d3_mig[,maternal_age:=as.character(maternal_age)]
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id), .N, by=.(maternal_age)]
names(records)<-c("maternal_age","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id), by="maternal_age",.N]
names(total)<-c("maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by="maternal_age",all=T)
MIG_S3_b_1<-data.table(algorithm="MIG_S3_b", records)
MIG_S3_b_1[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_S3_b_1<-MIG_S3_b_1[order(maternal_age)]
rm(records,total)
MIG_S3_b_1[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_S3_b_1[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_S3_b_1[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_S3_b_1[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_S3_b_1[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_S3_b_1[lower_95_CI<0,lower_95_CI:=0]
fwrite(MIG_S3_b_1,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_S3_b_age.csv"),row.names = F)
rm(MIG_S3_b_1)


#by year
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by="year", .N]
names(records)<-c("year","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by="year",.N]
names(total)<-c("year","no_pregnancies")
records<-merge.data.table(records,total,by="year",all=T)
MIG_S3_b_2<-data.table(algorithm="MIG_S3_b", records)
MIG_S3_b_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_S3_b_2<-MIG_S3_b_2[order(year)]
rm(records,total)
MIG_S3_b_2[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_S3_b_2[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_S3_b_2[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_S3_b_2[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_S3_b_2[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_S3_b_2[lower_95_CI<0,lower_95_CI:=0]
fwrite(MIG_S3_b_2,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_S3_b_year.csv"),row.names = F)
rm(MIG_S3_b_2)

#number of pregnnacies with baseline migraine
records_coh<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id) & preg_baseline_2==1,by="year",.N]
names(records_coh)<-c("year","no_diagnosed_pregnancies")
total_coh<-pregnancy_d3_mig[!duplicated(pregnancy_id) & preg_baseline_2==1,by="year",.N]
names(total_coh)<-c("year","no_pregnancies")
records_coh<-merge.data.table(records_coh,total_coh,by="year",all=T)
MIG_S3_b_coh_2<-data.table(algorithm="MIG_S3_b", records_coh)
MIG_S3_b_coh_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_S3_b_coh_2[,percentage:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_S3_b_coh_2[,denominator:="Number of pregnancies with baseline migraine(MG diagnoses or Triptan prescription), lookback= 90 days/75 days by year"]

#by year group and maternal age
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by=c("year_group","maternal_age"), .N]
names(records)<-c("year_group","maternal_age", "no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by=c("year_group","maternal_age"),.N]
names(total)<-c("year_group","maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","maternal_age"), all=T)
MIG_S3_b_3<-data.table(algorithm="MIG_S3_b", records)
MIG_S3_b_3[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_S3_b_3<-MIG_S3_b_3[order(year_group,maternal_age)]
rm(records,total)
MIG_S3_b_3[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_S3_b_3[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_S3_b_3[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_S3_b_3[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_S3_b_3[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_S3_b_3[lower_95_CI<0,lower_95_CI:=0]
pregnancy_d3_mig[,include:=NULL]
fwrite(MIG_S3_b_3,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_S3_b_year_age.csv"),row.names = F)
rm(MIG_S3_b_3)

#### MIG_S3_during:Prevalence of Severe migraine during pregnancy when lookback==12 months(3 months) ####
print("Create algorithm MIG_S3_during")
MIG_S3_during<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_S3_during"]
inc_col<-MIG_S3_during[TYPE=="AND",STUDY_VARIABLES]
excl_col<-MIG_S3_during[TYPE=="AND_NOT",STUDY_VARIABLES]

if(length(inc_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col],include:=1]}else{pregnancy_d3_mig[,include:=NA]}
if(length(excl_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("|" , lapply(.SD,`>=`, 1)),.SDcols=excl_col],exclude:=1]}else{pregnancy_d3_mig[,exclude:=NA]}

#Update include comun based on exclude
pregnancy_d3_mig[exclude==1, include:=NA]
pregnancy_d3_mig[,exclude:=NULL]

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm/final_d3/MIG_S3_during_D3.rds"))

#export MIG_S3_during
MIG_S3_during_dt<-data.table(algorithm="MIG_S3_during", no_diagnosed_pregnancies=pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id),.N])
MIG_S3_during_dt[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_S3_during_dt[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
MIG_S3_during_dt[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_S3_during_dt[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_S3_during_dt[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_S3_during_dt,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_S3_during.csv"),row.names = F)

#number of pregnancies with baseline migraine
pregnancy_d3_mig[MG_during>=1 | Migraine_medicines_during>=1,preg_during:=1]
MIG_S3_during_coh<-data.table(algorithm="MIG_S3_during", no_diagnosed_pregnancies=pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id) & preg_during==1,.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id) & preg_during==1,.N])
MIG_S3_during_coh[,percentage:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_S3_during_coh[,denominator:="Number of pregnancies with migraine(MG diagnoses or Triptan prescription) during pregnancy"]

#by maternal age
pregnancy_d3_mig[,maternal_age:=as.character(maternal_age)]
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id), .N, by=.(maternal_age)]
names(records)<-c("maternal_age","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id), by="maternal_age",.N]
names(total)<-c("maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by="maternal_age",all=T)
MIG_S3_during_1<-data.table(algorithm="MIG_S3_during", records)
MIG_S3_during_1[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_S3_during_1<-MIG_S3_during_1[order(maternal_age)]
rm(records,total)
MIG_S3_during_1[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_S3_during_1[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_S3_during_1[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_S3_during_1[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_S3_during_1[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_S3_during_1[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_S3_during_1,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_S3_during_age.csv"),row.names = F)
rm(MIG_S3_during_1)

#by year
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by="year", .N]
names(records)<-c("year","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by="year",.N]
names(total)<-c("year","no_pregnancies")
records<-merge.data.table(records,total,by="year",all=T)
MIG_S3_during_2<-data.table(algorithm="MIG_S3_during", records)
MIG_S3_during_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_S3_during_2<-MIG_S3_during_2[order(year)]
rm(records,total)
MIG_S3_during_2[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_S3_during_2[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_S3_during_2[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_S3_during_2[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_S3_during_2[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_S3_during_2[lower_95_CI<0,lower_95_CI:=0]
fwrite(MIG_S3_during_2,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_S3_during_year.csv"),row.names = F)
rm(MIG_S3_during_2)

#number of pregnnacies with migraine
records_coh<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id) & preg_during==1,by="year",.N]
names(records_coh)<-c("year","no_diagnosed_pregnancies")
total_coh<-pregnancy_d3_mig[!duplicated(pregnancy_id) & preg_during==1,by="year",.N]
names(total_coh)<-c("year","no_pregnancies")
records_coh<-merge.data.table(records_coh,total_coh,by="year",all=T)
MIG_S3_during_coh_2<-data.table(algorithm="MIG_S3_during", records_coh)
MIG_S3_during_coh_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_S3_during_coh_2[,percentage:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_S3_during_coh_2[,denominator:="Number of pregnancies with migraine(MG diagnoses or Triptan prescription) during pregnancy by year"]

#by year group and maternal age
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by=c("year_group","maternal_age"), .N]
names(records)<-c("year_group","maternal_age", "no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by=c("year_group","maternal_age"),.N]
names(total)<-c("year_group","maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","maternal_age"), all=T)
MIG_S3_during_3<-data.table(algorithm="MIG_S3_during", records)
MIG_S3_during_3[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_S3_during_3<-MIG_S3_during_3[order(year_group,maternal_age)]
rm(records,total)
MIG_S3_during_3[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_S3_during_3[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_S3_during_3[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_S3_during_3[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_S3_during_3[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_S3_during_3[lower_95_CI<0,lower_95_CI:=0]
pregnancy_d3_mig[,include:=NULL]
rm(MIG_S3_during)
fwrite(MIG_S3_during_3,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_S3_during_year_age.csv"),row.names = F)
rm(MIG_S3_during_3)

#### MIG_S3_both:Proportion of pregnancies having the same type S3 during pregnancy and before when lookback==12 months(3 months) ####
print("Create algorithm MIG_S3_both")

pregnancy_d3_mig[Migraine_injections_baseline>=1 & Migraine_med_profilactic_during==0, S_1:=1]
pregnancy_d3_mig[Migraine_injections_during>=1 & Migraine_med_profilactic_during==0, S_2:=1]
pregnancy_d3_mig[S_1==1 & S_2==1, both:=1]
pregnancy_d3_mig[S_1==1 | S_2==1, total:=1]

#number of pregnancies having the same type in baseline and during
no_preg_both<-pregnancy_d3_mig[both==1 & !duplicated(pregnancy_id), .N]
#number of pregnancies having the type of interest in baseline or during
no_preg_tot<-pregnancy_d3_mig[total==1 & !duplicated(pregnancy_id), .N]
percentage_preg<-round((no_preg_both/no_preg_tot)*100,1)

MIG_S3_both<-data.table(algorithm="MIG_S3_both", event_definition="Severe", no_pregnancies_same_type=no_preg_both, no_pregnancies=no_preg_tot, percentage=percentage_preg)

pregnancy_d3_mig[,both:=NULL][,total:=NULL][,S_1:=NULL][,S_2:=NULL]
rm(no_preg_both,no_preg_tot,percentage_preg)

fwrite(MIG_S3_both,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_S3_both.csv"),row.names = F)
rm(MIG_S3_both)

#### MIG_S4_during:Prevalence of Very severe migraine during pregnancy when lookback==12 months(3 months) ####
print("Create algorithm MIG_S4_during")
MIG_S4_during<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_S4_during"]
inc_col<-MIG_S4_during[TYPE=="AND",STUDY_VARIABLES]
alt_col<-MIG_S4_during[TYPE=="OR",STUDY_VARIABLES]

if(length(alt_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("|" , lapply(.SD,`>=`, 1)),.SDcols=alt_col],alternative:=1]}else{pregnancy_d3_mig[,alternative:=NA]}
if(length(inc_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col],include:=1]}else{pregnancy_d3_mig[,include:=NA]}

#Update include comun based on exclude
pregnancy_d3_mig[is.na(alternative), include:=NA]
pregnancy_d3_mig[,alternative:=NULL]

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm/final_d3/MIG_S4_during_D3.rds"))

#export MIG_S4_during
MIG_S4_during_dt<-data.table(algorithm="MIG_S4_during", no_diagnosed_pregnancies=pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id),.N])
MIG_S4_during_dt[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_S4_during_dt[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
MIG_S4_during_dt[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_S4_during_dt[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_S4_during_dt[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_S4_during_dt,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_S4_during.csv"),row.names = F)

#number of pregnancies with baseline migraine
pregnancy_d3_mig[MG_during>=1 | Migraine_medicines_during>=1,preg_during:=1]
MIG_S4_during_coh<-data.table(algorithm="MIG_S4_during", no_diagnosed_pregnancies=pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id) & preg_during==1,.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id) & preg_during==1,.N])
MIG_S4_during_coh[,percentage:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_S4_during_coh[,denominator:="Number of pregnancies with migraine(MG diagnoses or Triptan prescription) during pregnancy"]

#by maternal age
pregnancy_d3_mig[,maternal_age:=as.character(maternal_age)]
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id), .N, by=.(maternal_age)]
names(records)<-c("maternal_age","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id), by="maternal_age",.N]
names(total)<-c("maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by="maternal_age",all=T)
MIG_S4_during_1<-data.table(algorithm="MIG_S4_during", records)
MIG_S4_during_1[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_S4_during_1<-MIG_S4_during_1[order(maternal_age)]
rm(records,total)
MIG_S4_during_1[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_S4_during_1[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_S4_during_1[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_S4_during_1[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_S4_during_1[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_S4_during_1[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_S4_during_1,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_S4_during_age.csv"),row.names = F)
rm(MIG_S4_during_1)

#by year
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by="year", .N]
names(records)<-c("year","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by="year",.N]
names(total)<-c("year","no_pregnancies")
records<-merge.data.table(records,total,by="year",all=T)
MIG_S4_during_2<-data.table(algorithm="MIG_S4_during", records)
MIG_S4_during_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_S4_during_2<-MIG_S4_during_2[order(year)]
rm(records,total)
MIG_S4_during_2[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_S4_during_2[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_S4_during_2[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_S4_during_2[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_S4_during_2[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_S4_during_2[lower_95_CI<0,lower_95_CI:=0]
fwrite(MIG_S4_during_2,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_S4_during_year.csv"),row.names = F)
rm(MIG_S4_during_2)

#number of pregnnacies with migraine
records_coh<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id) & preg_during==1,by="year",.N]
names(records_coh)<-c("year","no_diagnosed_pregnancies")
total_coh<-pregnancy_d3_mig[!duplicated(pregnancy_id) & preg_during==1,by="year",.N]
names(total_coh)<-c("year","no_pregnancies")
records_coh<-merge.data.table(records_coh,total_coh,by="year",all=T)
MIG_S4_during_coh_2<-data.table(algorithm="MIG_S4_during", records_coh)
MIG_S4_during_coh_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_S4_during_coh_2[,percentage:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_S4_during_coh_2[,denominator:="Number of pregnancies with migraine(MG diagnoses or Triptan prescription) during pregnancy by year"]

#by year group and maternal age
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by=c("year_group","maternal_age"), .N]
names(records)<-c("year_group","maternal_age", "no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by=c("year_group","maternal_age"),.N]
names(total)<-c("year_group","maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","maternal_age"), all=T)
MIG_S4_during_3<-data.table(algorithm="MIG_S4_during", records)
MIG_S4_during_3[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_S4_during_3<-MIG_S4_during_3[order(year_group,maternal_age)]
rm(records,total)
MIG_S4_during_3[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_S4_during_3[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_S4_during_3[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_S4_during_3[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_S4_during_3[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_S4_during_3[lower_95_CI<0,lower_95_CI:=0]
pregnancy_d3_mig[,include:=NULL]
rm(MIG_S4_during)
fwrite(MIG_S4_during_3,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_S4_during_year_age.csv"),row.names = F)
rm(MIG_S4_during_3)

#### Migraine severity proportions ####
#Type a baseline
#MIG_S1_a_coh,MIG_S2_a_coh,MIG_S3_a_coh
MIG_S_a_coh<-rbind(MIG_S1_a_coh,MIG_S2_a_coh,MIG_S3_a_coh)
rm(MIG_S1_a_coh,MIG_S2_a_coh,MIG_S3_a_coh)

fwrite(MIG_S_a_coh,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_S_a.csv"),row.names = F)
rm(MIG_S_a_coh)
#Type a baseline year
#MIG_S1_a_coh_2,MIG_S2_a_coh_2,MIG_S3_a_coh_2
MIG_S_a_coh_2<-rbind(MIG_S1_a_coh_2,MIG_S2_a_coh_2,MIG_S3_a_coh_2)
rm(MIG_S1_a_coh_2,MIG_S2_a_coh_2,MIG_S3_a_coh_2)

fwrite(MIG_S_a_coh_2,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_S_a_year.csv"),row.names = F)
rm(MIG_S_a_coh_2)
#Type b baseline
#MIG_S1_b_coh,MIG_S2_b_coh,MIG_S3_b_coh
MIG_S_b_coh<-rbind(MIG_S1_b_coh,MIG_S2_b_coh,MIG_S3_b_coh)
rm(MIG_S1_b_coh,MIG_S2_b_coh,MIG_S3_b_coh)
fwrite(MIG_S_b_coh,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_S_b.csv"),row.names = F)
rm(MIG_S_b_coh)

#Type b baseline year
#MIG_S1_b_coh_2,MIG_S2_b_coh_2,MIG_S3_b_coh_2
MIG_S_b_coh_2<-rbind(MIG_S1_b_coh_2,MIG_S2_b_coh_2,MIG_S3_b_coh_2)
rm(MIG_S1_b_coh_2,MIG_S2_b_coh_2,MIG_S3_b_coh_2)
fwrite(MIG_S_b_coh_2,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_S_b_year.csv"),row.names = F)
rm(MIG_S_b_coh_2)

#Type during
#MIG_S1_during_coh,MIG_S2_during_coh,MIG_S3_during_coh,MIG_S4_during_coh
MIG_S_during_coh<-rbind(MIG_S1_during_coh,MIG_S2_during_coh,MIG_S3_during_coh,MIG_S4_during_coh)
rm(MIG_S1_during_coh,MIG_S2_during_coh,MIG_S3_during_coh,MIG_S4_during_coh)
fwrite(MIG_S_during_coh,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_S_during.csv"),row.names = F)
rm(MIG_S_during_coh)

#Type during year
#MIG_S1_during_coh_2,MIG_S2_during_coh,MIG_S3_during_coh,MIG_S4_during_coh
MIG_S_during_coh_2<-rbind(MIG_S1_during_coh_2,MIG_S2_during_coh_2,MIG_S3_during_coh_2,MIG_S4_during_coh_2)
rm(MIG_S1_during_coh_2,MIG_S2_during_coh_2,MIG_S3_during_coh_2,MIG_S4_during_coh_2)
fwrite(MIG_S_during_coh_2,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_S_during_year.csv"),row.names = F)
rm(MIG_S_during_coh_2)

#### Save time info ####
date_running_end_04_f<-Sys.Date()
end_time_04_f<-Sys.time()

time_log_04_f<-data.table(DAP=data_access_provider_name,
                          Script="Step_04_f_migraine_algorithms_severity.R", 
                          Start_date=date_running_start_04_f, 
                          End_date=date_running_end_04_f,
                          Time_elaspsed=format(end_time_04_f-initial_time_04_f, digits=2))
fwrite(time_log_04_f,paste0(output_dir,"/Time log/Step_04_f_time_log.csv"),row.names = F)
rm(time_log_04_f)





