initial_time_04_b<-Sys.time()
date_running_start_04_b<-Sys.Date()


####PE DIAGNOSES####
print("Loading all PE diagnoses D3 and merge with the pregnancy D3.")
obs_period_diag<-data.table(StudyVar=c("PE","ECL","HELLP","PE_checkbox"),
                       lookback=c(0,0,0,0),
                       start_date=c(20*7,20*7,20*7,20*7),
                       end_date=c(7*40,7*40,7*40,7*40),
                       after=c(7,7,7,7))
fwrite(obs_period_diag, paste0(projectFolder,"/g_output/PE and GDM algorithm/Step_04_observation_periods_pe.csv"),row.names = F)

#pe files
pe_files<-list.files(paste0(projectFolder,"/g_intermediate/pe_algorithm/"))
names_events<-list()
for(i in 1:length(pe_files)){
  names_events[[i]]<-unlist(str_split(pe_files[i],"[.]"))[1] 
}
names_events<-as.vector(do.call(rbind,names_events))
original<-list()
before<-list()
after<-list()
sum<-list()

dir.create(paste0(projectFolder,"/g_intermediate/pe_algorithm/final_d3"))


w<-1
for(pe_fl in 1:length(pe_files)){
  pe_dt<-readRDS(paste0(projectFolder,"/g_intermediate/pe_algorithm/", pe_files[pe_fl]))
  #merge with the pregnancy d3
  pe_dt<-merge.data.table(pregnancy_d3_gdm_pe, pe_dt, by="person_id", all.x=T, allow.cartesian = T)
  pe_dt<-pe_dt[!is.na(event_date)]
  if(pe_dt[,.N]>0){
    original[[w]]<-data.table(StudyVar=names_events[pe_fl], event_records=pe_dt[,.N])
    pe_dt[,event_date:=as.IDate(event_date)]
    
    if(obs_period_diag[StudyVar==names_events[pe_fl],lookback]>0){
      pe_dt[,lookback:=obs_period_diag[StudyVar==names_events[pe_fl],lookback]]
      pe_dt[,start_preg:=as.IDate(pregnancy_start_date-lookback)]
      #exclude all pregnancies that are outside observation period of interest
      pe_dt[,diff:=event_date-start_preg]
      #remove all records with date before start date pregnancy+lookback
      before[[w]]<-data.table(StudyVar=names_events[pe_fl], before_start=pe_dt[diff<0,.N])
      pe_dt<-pe_dt[diff>=0]
      pe_dt[,diff:=NULL][,lookback:=NULL][,start_preg:=NULL]
    }else{
      #remove all records before start obs
      pe_dt[,start:=obs_period_diag[StudyVar==names_events[pe_fl],start_date]]
      pe_dt[,start_preg:=as.IDate(pregnancy_start_date+start)]
      pe_dt[,diff:=event_date-start_preg]
      before[[w]]<-data.table(StudyVar=names_events[pe_fl], before_start=pe_dt[diff<0,.N])
      pe_dt<-pe_dt[diff>=0]
      pe_dt[,diff:=NULL][,start:=NULL][,start_preg:=NULL]
    }
    
    if(pe_dt[,.N]>0){
      if(obs_period_diag[StudyVar==names_events[pe_fl],after]>0){
        pe_dt[,after:=obs_period_diag[StudyVar==names_events[pe_fl],after]]
        pe_dt[,end_preg:=as.IDate(pregnancy_end_date+after)]
        pe_dt[,diff:=event_date-end_preg]
        after[[w]]<-data.table(StudyVar=names_events[pe_fl], after_end=pe_dt[diff>0,.N])
        pe_dt<-pe_dt[diff<=0]
        pe_dt[,diff:=NULL][,after:=NULL][,end_preg:=NULL]
      }else{
        pe_dt[,end:=obs_period_diag[StudyVar==names_events[pe_fl],end_date]]
        pe_dt[,end_preg:=as.IDate(pregnancy_start_date+end)]
        pe_dt[,diff:=event_date-end_preg]
        after[[w]]<-data.table(StudyVar=names_events[pe_fl], after_end=pe_dt[diff>0,.N])
        pe_dt<-pe_dt[diff<=0]
        pe_dt[,diff:=NULL][,end:=NULL][,end_preg:=NULL]
      }
    }else{
      after[[w]]<-data.table(StudyVar=names_events[pe_fl], after_end=0)
    }
    
    if(pe_dt[,.N]>0){
      #create a summary of included records
      sum[[w]]<-data.table(StudyVar=names_events[pe_fl], no_records=pe_dt[!is.na(event_date),.N], no_pregnancies=pe_dt[!duplicated(pregnancy_id),.N])
      saveRDS(pe_dt, paste0(projectFolder,"/g_intermediate/pe_algorithm/final_d3/", names_events[pe_fl],"_pregnancy_D3.rds"))
      cols<-c("person_id","pregnancy_id","pregnancy_start_date","pregnancy_end_date")
      pe_dt<-pe_dt[,cols,with=F]
      pe_dt[,names_events[pe_fl]:=1]
      pe_dt<-pe_dt[,lapply(.SD, sum),.SDcols = names_events[pe_fl], by=c("person_id","pregnancy_id","pregnancy_start_date","pregnancy_end_date")]
      pregnancy_d3_gdm_pe<-merge.data.table(pregnancy_d3_gdm_pe,pe_dt,all.x=T, by=cols)
      pregnancy_d3_gdm_pe[is.na(get(names_events[pe_fl])),names_events[pe_fl]:=0]
    }else{
      pregnancy_d3_gdm_pe[,names_events[pe_fl]:=0]
    }
  }else{
    pregnancy_d3_gdm_pe[,names_events[pe_fl]:=0]
  }
  
  rm(pe_dt) 
  w<-w+1
}

#Combine files
original<-as.data.table(do.call(rbind,original))
before<-as.data.table(do.call(rbind,before))
after<-as.data.table(do.call(rbind,after))
removed_rec_pe<-merge.data.table(original,before,by="StudyVar",all=T)
removed_rec_pe<-merge.data.table(removed_rec_pe,after,by="StudyVar",all=T)
removed_rec_pe[is.na(before_start),before_start:="N/A"]
removed_rec_pe[is.na(after_end),after_end:="N/A"]
setnames(removed_rec_pe,"event_records", "original_records")
#fwrite(removed_rec_pe,paste0(projectFolder,"/g_output/PE and GDM algorithm/removed_rec_gdm.csv"),row.names = F)


#Pregnancies that had the event of interest not in the timeframe of study were excluded, this is why the number of pregnancies is different between different events
sum_pe<-as.data.table(do.call(rbind,sum))
#fwrite(sum_pe, paste0(projectFolder,"/g_output/PE and GDM algorithm/Step_04_summary_records_pe.csv"),row.names = F)
rm(original,before,after,sum)
rm(pe_files)

#identify events that are not present from conditions_pe
not_present<-setdiff(obs_period_diag[,StudyVar], names_events)
pregnancy_d3_gdm_pe[,eval(not_present):=list(0)]
rm(obs_period_diag)

print("Export PE pregnancy D3")
fwrite(pregnancy_d3_gdm_pe,paste0(projectFolder,"/g_intermediate/pe_algorithm/final_d3/pregnancy_D3_pe_algorithm.csv"),row.names = F)
####APPLY PE ALGORITHM####
algorithm_template<-fread(paste0(projectFolder,"/p_steps/parameters/algorithms.csv"))
####PE_1####
print("Create algorithm PE_1.")
PE_1<-algorithm_template[NEW_STUDY_VARIABLES=="PE_1"]
#excl_col<-PE_1[TYPE=="AND_NOT",STUDY_VARIABLES]
inc_col<-PE_1[TYPE=="AND",STUDY_VARIABLES]
#alt_col<-PE_1[TYPE=="OR",STUDY_VARIABLES]

#if(length(excl_col)>0){pregnancy_d3_gdm_pe[pregnancy_d3_gdm_pe[,Reduce("|" , lapply(.SD,`>=`, 1)),.SDcols=excl_col],exclude:=1]}else{pregnancy_d3_gdm_pe[,exclude:=NA]}
if(length(inc_col)>0){pregnancy_d3_gdm_pe[pregnancy_d3_gdm_pe[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col],include:=1]}else{pregnancy_d3_gdm_pe[,include:=NA]}
#if(length(alt_col)>0){pregnancy_d3_gdm_pe[pregnancy_d3_gdm_pe[,Reduce("|" , lapply(.SD,`>=`, 1)),.SDcols=alt_col],alternative:=1]}else{pregnancy_d3_gdm_pe[,alternative:=1]}

saveRDS(pregnancy_d3_gdm_pe,paste0(projectFolder,"/g_intermediate/pe_algorithm/final_d3/PE_1_D3.rds"))
#export PE_1
PE_1_a<-data.table(algorithm="PE_1", no_diagnosed_pregnancies=pregnancy_d3_gdm_pe[include==1 & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_gdm_pe[!duplicated(pregnancy_id),.N])
PE_1_a[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
PE_1_a[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
PE_1_a[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
PE_1_a[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
PE_1_a[lower_95_CI<0,lower_95_CI:=0]
fwrite(PE_1_a,paste0(projectFolder,"/g_output/PE and GDM algorithm/Step_04_PE_1.csv"),row.names = F)

pregnancy_d3_gdm_pe[,maternal_age:=as.character(maternal_age)]
records<-pregnancy_d3_gdm_pe[include==1 & !duplicated(pregnancy_id), .N, by=.(maternal_age)]
names(records)<-c("maternal_age","no_diagnosed_pregnancies")
total<-pregnancy_d3_gdm_pe[!duplicated(pregnancy_id), by="maternal_age",.N]
names(total)<-c("maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by="maternal_age",all=T)
PE_1_b<-data.table(algorithm="PE_1", records)
PE_1_b[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
PE_1_b<-PE_1_b[order(maternal_age)]
rm(records,total)
PE_1_b[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
PE_1_b[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
PE_1_b[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
PE_1_b[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
PE_1_b[lower_95_CI<0,lower_95_CI:=0]
fwrite(PE_1_b,paste0(projectFolder,"/g_output/PE and GDM algorithm/Step_04_PE_1_age.csv"),row.names = F)

records<-pregnancy_d3_gdm_pe[include==1 & !duplicated(pregnancy_id),by="year", .N]
names(records)<-c("year","no_diagnosed_pregnancies")
total<-pregnancy_d3_gdm_pe[!duplicated(pregnancy_id),by="year",.N]
names(total)<-c("year","no_pregnancies")
records<-merge.data.table(records,total,by="year",all=T)
PE_1_c<-data.table(algorithm="PE_1", records)
PE_1_c[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
PE_1_c<-PE_1_c[order(year)]
rm(records,total)
PE_1_c[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
PE_1_c[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
PE_1_c[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
PE_1_c[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
PE_1_c[lower_95_CI<0,lower_95_CI:=0]
fwrite(PE_1_c,paste0(projectFolder,"/g_output/PE and GDM algorithm/Step_04_PE_1_year.csv"),row.names = F)



records<-pregnancy_d3_gdm_pe[include==1 & !duplicated(pregnancy_id),by=c("year_group","maternal_age"), .N]
names(records)<-c("year_group","maternal_age", "no_diagnosed_pregnancies")
total<-pregnancy_d3_gdm_pe[!duplicated(pregnancy_id),by=c("year_group","maternal_age"),.N]
names(total)<-c("year_group","maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","maternal_age"), all=T)
PE_1_d<-data.table(algorithm="PE_1", records)
PE_1_d[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
PE_1_d<-PE_1_d[order(year_group,maternal_age)]
rm(records,total)
pregnancy_d3_gdm_pe[,include:=NULL]
PE_1_d[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
PE_1_d[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
PE_1_d[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
PE_1_d[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
PE_1_d[lower_95_CI<0,lower_95_CI:=0]
fwrite(PE_1_d,paste0(projectFolder,"/g_output/PE and GDM algorithm/Step_04_PE_1_year_age.csv"),row.names = F)
rm(PE_1,PE_1_a,PE_1_b,PE_1_c,PE_1_d)
rm(inc_col)


####PE_2####
print("Create algorithm PE_2.")
PE_2<-algorithm_template[NEW_STUDY_VARIABLES=="PE_2"]
#excl_col<-PE_2[TYPE=="AND_NOT",STUDY_VARIABLES]
inc_col<-PE_2[TYPE=="AND",STUDY_VARIABLES]
#alt_col<-PE_2[TYPE=="OR",STUDY_VARIABLES]
if(algorithm_template[NEW_STUDY_VARIABLES=="PE_2" & COMPLEXITY=="complex" & !is.na(RULE),.N]>0){
  rule<-algorithm_template[NEW_STUDY_VARIABLES=="PE_2" & COMPLEXITY=="complex",RULE]
  value<-algorithm_template[NEW_STUDY_VARIABLES=="PE_2" & COMPLEXITY=="complex",VALUE]
  
}else{
  script<-algorithm_template[NEW_STUDY_VARIABLES=="PE_2" & COMPLEXITY=="complex",SCRIPT]
}

#if(length(excl_col)>0){pregnancy_d3_gdm_pe[pregnancy_d3_gdm_pe[,Reduce("|" , lapply(.SD,`>=`, 1)),.SDcols=excl_col],exclude:=1]}else{pregnancy_d3_gdm_pe[,exclude:=NA]}
if(length(inc_col)>0){pregnancy_d3_gdm_pe[pregnancy_d3_gdm_pe[,Reduce("&" , lapply(.SD, rule, value)),.SDcols=inc_col],include:=1]}else{pregnancy_d3_gdm_pe[,include:=NA]}
#if(length(alt_col)>0){pregnancy_d3_gdm_pe[pregnancy_d3_gdm_pe[,Reduce("|" , lapply(.SD,`>=`, 1)),.SDcols=alt_col],alternative:=1]}else{pregnancy_d3_gdm_pe[,alternative:=1]}

saveRDS(pregnancy_d3_gdm_pe,paste0(projectFolder,"/g_intermediate/pe_algorithm/final_d3/PE_2_D3.rds"))

#export PE_2
PE_2_dt<-data.table(algorithm="PE_2", no_diagnosed_pregnancies=pregnancy_d3_gdm_pe[include==1 & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_gdm_pe[!duplicated(pregnancy_id),.N])
PE_2_dt[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
PE_2_dt[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
PE_2_dt[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
PE_2_dt[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
PE_2_dt[lower_95_CI<0,lower_95_CI:=0]
fwrite(PE_2_dt,paste0(projectFolder,"/g_output/PE and GDM algorithm/Step_04_PE_2.csv"),row.names = F)

pregnancy_d3_gdm_pe[,maternal_age:=as.character(maternal_age)]
records<-pregnancy_d3_gdm_pe[include==1 & !duplicated(pregnancy_id), .N, by=.(maternal_age)]
names(records)<-c("maternal_age","no_diagnosed_pregnancies")
total<-pregnancy_d3_gdm_pe[!duplicated(pregnancy_id), by="maternal_age",.N]
names(total)<-c("maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by="maternal_age",all=T)
PE_2_b<-data.table(algorithm="PE_2", records)
PE_2_b[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
PE_2_b<-PE_2_b[order(maternal_age)]
rm(records,total)
PE_2_b[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
PE_2_b[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
PE_2_b[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
PE_2_b[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
PE_2_b[lower_95_CI<0,lower_95_CI:=0]
fwrite(PE_2_b,paste0(projectFolder,"/g_output/PE and GDM algorithm/Step_04_PE_2_age.csv"),row.names = F)


records<-pregnancy_d3_gdm_pe[include==1 & !duplicated(pregnancy_id),by="year", .N]
names(records)<-c("year","no_diagnosed_pregnancies")
total<-pregnancy_d3_gdm_pe[!duplicated(pregnancy_id),by="year",.N]
names(total)<-c("year","no_pregnancies")
records<-merge.data.table(records,total,by="year",all=T)
PE_2_c<-data.table(algorithm="PE_2", records)
PE_2_c[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
PE_2_c<-PE_2_c[order(year)]
rm(records,total)
PE_2_c[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
PE_2_c[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
PE_2_c[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
PE_2_c[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
PE_2_c[lower_95_CI<0,lower_95_CI:=0]
fwrite(PE_2_c,paste0(projectFolder,"/g_output/PE and GDM algorithm/Step_04_PE_2_year.csv"),row.names = F)


records<-pregnancy_d3_gdm_pe[include==1 & !duplicated(pregnancy_id),by=c("year_group","maternal_age"), .N]
names(records)<-c("year_group","maternal_age", "no_diagnosed_pregnancies")
total<-pregnancy_d3_gdm_pe[!duplicated(pregnancy_id),by=c("year_group","maternal_age"),.N]
names(total)<-c("year_group","maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","maternal_age"), all=T)
PE_2_d<-data.table(algorithm="PE_2", records)
PE_2_d[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
PE_2_d<-PE_2_d[order(year_group,maternal_age)]
rm(records,total)
pregnancy_d3_gdm_pe[,include:=NULL]
PE_2_d[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
PE_2_d[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
PE_2_d[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
PE_2_d[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
PE_2_d[lower_95_CI<0,lower_95_CI:=0]
fwrite(PE_2_d,paste0(projectFolder,"/g_output/PE and GDM algorithm/Step_04_PE_2_year_age.csv"),row.names = F)
rm(PE_2,PE_2_dt,PE_2_b,PE_2_c,PE_2_d)
rm(inc_col)
if("rule" %in% ls()){rm(rule)}
if("value" %in% ls()){rm(value)}
if("script" %in% ls()){rm(script)}

####PE_3####
print("Create algorithm PE_3.")
PE_3<-algorithm_template[NEW_STUDY_VARIABLES=="PE_3"]
#excl_col<-PE_3[TYPE=="AND_NOT",STUDY_VARIABLES]
#inc_col<-PE_3[TYPE=="AND",STUDY_VARIABLES]
alt_col<-PE_3[TYPE=="OR",STUDY_VARIABLES]

#if(length(excl_col)>0){pregnancy_d3_gdm_pe[pregnancy_d3_gdm_pe[,Reduce("|" , lapply(.SD,`>=`, 1)),.SDcols=excl_col],exclude:=1]}else{pregnancy_d3_gdm_pe[,exclude:=NA]}
# if(length(inc_col)>0){pregnancy_d3_gdm_pe[pregnancy_d3_gdm_pe[,Reduce("|" , lapply(.SD,`>=`, 1)),.SDcols=inc_col],include:=1]}else{pregnancy_d3_gdm_pe[,include:=NA]}
if(length(alt_col)>0){pregnancy_d3_gdm_pe[pregnancy_d3_gdm_pe[,Reduce("|" , lapply(.SD,`>=`, 1)),.SDcols=alt_col],alternative:=1]}else{pregnancy_d3_gdm_pe[,alternative:=NA]}

saveRDS(pregnancy_d3_gdm_pe,paste0(projectFolder,"/g_intermediate/pe_algorithm/final_d3/PE_3_D3.rds"))

#export PE_3
PE_3_dt<-data.table(algorithm="PE_3", no_diagnosed_pregnancies=pregnancy_d3_gdm_pe[alternative==1 & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_gdm_pe[!duplicated(pregnancy_id),.N])
PE_3_dt[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
PE_3_dt[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
PE_3_dt[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
PE_3_dt[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
PE_3_dt[lower_95_CI<0,lower_95_CI:=0]
fwrite(PE_3_dt,paste0(projectFolder,"/g_output/PE and GDM algorithm/Step_04_PE_3.csv"),row.names = F)

pregnancy_d3_gdm_pe[,maternal_age:=as.character(maternal_age)]
records<-pregnancy_d3_gdm_pe[alternative==1 & !duplicated(pregnancy_id), .N, by=.(maternal_age)]
names(records)<-c("maternal_age","no_diagnosed_pregnancies")
total<-pregnancy_d3_gdm_pe[!duplicated(pregnancy_id), by="maternal_age",.N]
names(total)<-c("maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by="maternal_age",all=T)
PE_3_b<-data.table(algorithm="PE_3", records)
PE_3_b[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
PE_3_b<-PE_3_b[order(maternal_age)]
rm(records,total)
PE_3_b[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
PE_3_b[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
PE_3_b[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
PE_3_b[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
PE_3_b[lower_95_CI<0,lower_95_CI:=0]
fwrite(PE_3_b,paste0(projectFolder,"/g_output/PE and GDM algorithm/Step_04_PE_3_age.csv"),row.names = F)

records<-pregnancy_d3_gdm_pe[alternative==1 & !duplicated(pregnancy_id),by="year", .N]
names(records)<-c("year","no_diagnosed_pregnancies")
total<-pregnancy_d3_gdm_pe[!duplicated(pregnancy_id),by="year",.N]
names(total)<-c("year","no_pregnancies")
records<-merge.data.table(records,total,by="year",all=T)
PE_3_c<-data.table(algorithm="PE_3", records)
PE_3_c[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
PE_3_c<-PE_3_c[order(year)]
rm(records,total)
PE_3_c[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
PE_3_c[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
PE_3_c[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
PE_3_c[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
PE_3_c[lower_95_CI<0,lower_95_CI:=0]
fwrite(PE_3_c,paste0(projectFolder,"/g_output/PE and GDM algorithm/Step_04_PE_3_year.csv"),row.names = F)


records<-pregnancy_d3_gdm_pe[alternative==1 & !duplicated(pregnancy_id),by=c("year_group","maternal_age"), .N]
names(records)<-c("year_group","maternal_age", "no_diagnosed_pregnancies")
total<-pregnancy_d3_gdm_pe[!duplicated(pregnancy_id),by=c("year_group","maternal_age"),.N]
names(total)<-c("year_group","maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","maternal_age"), all=T)
PE_3_d<-data.table(algorithm="PE_3", records)
PE_3_d[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
PE_3_d<-PE_3_d[order(year_group,maternal_age)]
rm(records,total)
pregnancy_d3_gdm_pe[,alternative:=NULL]
PE_3_d[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
PE_3_d[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
PE_3_d[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
PE_3_d[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
PE_3_d[lower_95_CI<0,lower_95_CI:=0]
fwrite(PE_3_d,paste0(projectFolder,"/g_output/PE and GDM algorithm/Step_04_PE_3_year_age.csv"),row.names = F)
rm(PE_3,PE_3_dt,PE_3_b,PE_3_c,PE_3_d)
rm(alt_col)


####PE_4####
print("Create algorithm PE_4.")
PE_4_a<-algorithm_template[NEW_STUDY_VARIABLES=="PE_4_a"]
inc_col<-PE_4_a[TYPE=="AND",STUDY_VARIABLES]
#alt_col<-PE_2[TYPE=="OR",STUDY_VARIABLES]
if(algorithm_template[NEW_STUDY_VARIABLES=="PE_4_a" & COMPLEXITY=="complex" & !is.na(RULE),.N]>0){
  rule<-algorithm_template[NEW_STUDY_VARIABLES=="PE_4_a" & COMPLEXITY=="complex",RULE]
  value<-algorithm_template[NEW_STUDY_VARIABLES=="PE_4_a" & COMPLEXITY=="complex",VALUE]
}else{
  script<-algorithm_template[NEW_STUDY_VARIABLES=="PE_4_a" & COMPLEXITY=="complex",SCRIPT]
}
if(length(inc_col)>0){pregnancy_d3_gdm_pe[pregnancy_d3_gdm_pe[,Reduce("&" , lapply(.SD,rule, value)),.SDcols=inc_col],include_a:=1]}else{pregnancy_d3_gdm_pe[,include_a:=NA]}
rm(inc_col)
if("rule" %in% ls()){rm(rule)}
if("value" %in% ls()){rm(value)}
if("script" %in% ls()){rm(script)}


PE_4_d<-algorithm_template[NEW_STUDY_VARIABLES=="PE_4_d"]
inc_col<-PE_4_d[TYPE=="AND",STUDY_VARIABLES]
#alt_col<-PE_2[TYPE=="OR",STUDY_VARIABLES]
if(algorithm_template[NEW_STUDY_VARIABLES=="PE_4_d" & COMPLEXITY=="complex" & !is.na(RULE),.N]>0){
  rule<-algorithm_template[NEW_STUDY_VARIABLES=="PE_4_d" & COMPLEXITY=="complex",RULE]
  value<-algorithm_template[NEW_STUDY_VARIABLES=="PE_4_d" & COMPLEXITY=="complex",VALUE]
}else{
  script<-algorithm_template[NEW_STUDY_VARIABLES=="PE_4_d" & COMPLEXITY=="complex",SCRIPT]
}
if(length(inc_col)>0){pregnancy_d3_gdm_pe[pregnancy_d3_gdm_pe[,Reduce("&" , lapply(.SD,rule, value)),.SDcols=inc_col],include_d:=1]}else{pregnancy_d3_gdm_pe[,include_d:=NA]}
rm(inc_col)
if("rule" %in% ls()){rm(rule)}
if("value" %in% ls()){rm(value)}
if("script" %in% ls()){rm(script)}


PE_4_f<-algorithm_template[NEW_STUDY_VARIABLES=="PE_4_f"]
inc_col<-PE_4_f[TYPE=="AND",STUDY_VARIABLES]
#alt_col<-PE_2[TYPE=="OR",STUDY_VARIABLES]
if(algorithm_template[NEW_STUDY_VARIABLES=="PE_4_f" & COMPLEXITY=="complex" & !is.na(RULE),.N]>0){
  rule<-algorithm_template[NEW_STUDY_VARIABLES=="PE_4_f" & COMPLEXITY=="complex",RULE]
  value<-algorithm_template[NEW_STUDY_VARIABLES=="PE_4_f" & COMPLEXITY=="complex",VALUE]
}else{
  script<-algorithm_template[NEW_STUDY_VARIABLES=="PE_4_f" & COMPLEXITY=="complex",SCRIPT]
}
if(length(inc_col)>0){pregnancy_d3_gdm_pe[pregnancy_d3_gdm_pe[,Reduce("&" , lapply(.SD,rule, value)),.SDcols=inc_col],include_f:=1]}else{pregnancy_d3_gdm_pe[,include_f:=NA]}
rm(inc_col)
if("rule" %in% ls()){rm(rule)}
if("value" %in% ls()){rm(value)}
if("script" %in% ls()){rm(script)}


PE_4_b<-algorithm_template[NEW_STUDY_VARIABLES=="PE_4_b"]
inc_col<-PE_4_b[TYPE=="AND",STUDY_VARIABLES]
if(length(inc_col)>0){pregnancy_d3_gdm_pe[pregnancy_d3_gdm_pe[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col],include_b:=1]}else{pregnancy_d3_gdm_pe[,include_b:=NA]}
rm(inc_col)

PE_4_c<-algorithm_template[NEW_STUDY_VARIABLES=="PE_4_c"]
inc_col<-PE_4_c[TYPE=="AND",STUDY_VARIABLES]
if(length(inc_col)>0){pregnancy_d3_gdm_pe[pregnancy_d3_gdm_pe[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col],include_c:=1]}else{pregnancy_d3_gdm_pe[,include_c:=NA]}
rm(inc_col)

PE_4_e<-algorithm_template[NEW_STUDY_VARIABLES=="PE_4_e"]
inc_col<-PE_4_e[TYPE=="AND",STUDY_VARIABLES]
if(length(inc_col)>0){pregnancy_d3_gdm_pe[pregnancy_d3_gdm_pe[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col],include_e:=1]}else{pregnancy_d3_gdm_pe[,include_e:=NA]}
rm(inc_col)

pregnancy_d3_gdm_pe[,include:=ifelse(include_a==1|include_b==1|include_c==1|include_d==1|include_e==1|include_f==1,1,NA)]
pregnancy_d3_gdm_pe[,include_a:=NULL][,include_b:=NULL][,include_c:=NULL][,include_d:=NULL][,include_e:=NULL][,include_f:=NULL]

saveRDS(pregnancy_d3_gdm_pe,paste0(projectFolder,"/g_intermediate/pe_algorithm/final_d3/PE_4_D3.rds"))
rm(PE_4_a,PE_4_b,PE_4_c,PE_4_d,PE_4_e,PE_4_f) 

PE_4_dt<-data.table(algorithm="PE_4", no_diagnosed_pregnancies=pregnancy_d3_gdm_pe[include==1 & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_gdm_pe[!duplicated(pregnancy_id),.N])
PE_4_dt[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
PE_4_dt[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
PE_4_dt[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
PE_4_dt[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
PE_4_dt[lower_95_CI<0,lower_95_CI:=0]
fwrite(PE_4_dt,paste0(projectFolder,"/g_output/PE and GDM algorithm/Step_04_PE_4.csv"),row.names = F)

pregnancy_d3_gdm_pe[,maternal_age:=as.character(maternal_age)]
records<-pregnancy_d3_gdm_pe[include==1 & !duplicated(pregnancy_id), .N, by=.(maternal_age)]
names(records)<-c("maternal_age","no_diagnosed_pregnancies")
total<-pregnancy_d3_gdm_pe[!duplicated(pregnancy_id), by="maternal_age",.N]
names(total)<-c("maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by="maternal_age",all=T)
PE_4_b<-data.table(algorithm="PE_4", records)
PE_4_b[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
PE_4_b<-PE_4_b[order(maternal_age)]
rm(records,total)
PE_4_b[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
PE_4_b[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
PE_4_b[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
PE_4_b[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
PE_4_b[lower_95_CI<0,lower_95_CI:=0]
fwrite(PE_4_b,paste0(projectFolder,"/g_output/PE and GDM algorithm/Step_04_PE_4_age.csv"),row.names = F)


records<-pregnancy_d3_gdm_pe[include==1 & !duplicated(pregnancy_id),by="year", .N]
names(records)<-c("year","no_diagnosed_pregnancies")
total<-pregnancy_d3_gdm_pe[!duplicated(pregnancy_id),by="year",.N]
names(total)<-c("year","no_pregnancies")
records<-merge.data.table(records,total,by="year",all=T)
PE_4_c<-data.table(algorithm="PE_4", records)
PE_4_c[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
PE_4_c<-PE_4_c[order(year)]
rm(records,total)
PE_4_c[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
PE_4_c[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
PE_4_c[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
PE_4_c[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
PE_4_c[lower_95_CI<0,lower_95_CI:=0]
fwrite(PE_4_c,paste0(projectFolder,"/g_output/PE and GDM algorithm/Step_04_PE_4_year.csv"),row.names = F)


records<-pregnancy_d3_gdm_pe[include==1 & !duplicated(pregnancy_id),by=c("year_group","maternal_age"), .N]
names(records)<-c("year_group","maternal_age", "no_diagnosed_pregnancies")
total<-pregnancy_d3_gdm_pe[!duplicated(pregnancy_id),by=c("year_group","maternal_age"),.N]
names(total)<-c("year_group","maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","maternal_age"), all=T)
PE_4_d<-data.table(algorithm="PE_4", records)
PE_4_d[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
PE_4_d<-PE_4_d[order(year_group,maternal_age)]
rm(records,total)
pregnancy_d3_gdm_pe[,include:=NULL]
PE_4_d[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
PE_4_d[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
PE_4_d[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
PE_4_d[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
PE_4_d[lower_95_CI<0,lower_95_CI:=0]
fwrite(PE_4_d,paste0(projectFolder,"/g_output/PE and GDM algorithm/Step_04_PE_4_year_age.csv"),row.names = F)
rm(PE_4_dt,PE_4_b,PE_4_c,PE_4_d)

####PE_5####
print("Create algorithm PE_5.")
PE_5<-algorithm_template[NEW_STUDY_VARIABLES=="PE_5"]
#excl_col<-PE_5[TYPE=="AND_NOT",STUDY_VARIABLES]
inc_col<-PE_5[TYPE=="AND",STUDY_VARIABLES]
#alt_col<-PE_5[TYPE=="OR",STUDY_VARIABLES]

#if(length(excl_col)>0){pregnancy_d3_gdm_pe[pregnancy_d3_gdm_pe[,Reduce("|" , lapply(.SD,`>=`, 1)),.SDcols=excl_col],exclude:=1]}else{pregnancy_d3_gdm_pe[,exclude:=NA]}
if(length(inc_col)>0){pregnancy_d3_gdm_pe[pregnancy_d3_gdm_pe[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col],include:=1]}else{pregnancy_d3_gdm_pe[,include:=NA]}
#if(length(alt_col)>0){pregnancy_d3_gdm_pe[pregnancy_d3_gdm_pe[,Reduce("|" , lapply(.SD,`>=`, 1)),.SDcols=alt_col],alternative:=1]}else{pregnancy_d3_gdm_pe[,alternative:=1]}

saveRDS(pregnancy_d3_gdm_pe,paste0(projectFolder,"/g_intermediate/pe_algorithm/final_d3/PE_5_D3.rds"))

#export PE_5
PE_5_dt<-data.table(algorithm="PE_5", no_diagnosed_pregnancies=pregnancy_d3_gdm_pe[include==1 & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_gdm_pe[!duplicated(pregnancy_id),.N])
PE_5_dt[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
PE_5_dt[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
PE_5_dt[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
PE_5_dt[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
PE_5_dt[lower_95_CI<0,lower_95_CI:=0]
fwrite(PE_5_dt,paste0(projectFolder,"/g_output/PE and GDM algorithm/Step_04_PE_5.csv"),row.names = F)

pregnancy_d3_gdm_pe[,maternal_age:=as.character(maternal_age)]
records<-pregnancy_d3_gdm_pe[include==1 & !duplicated(pregnancy_id), .N, by=.(maternal_age)]
names(records)<-c("maternal_age","no_diagnosed_pregnancies")
total<-pregnancy_d3_gdm_pe[!duplicated(pregnancy_id), by="maternal_age",.N]
names(total)<-c("maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by="maternal_age",all=T)
PE_5_b<-data.table(algorithm="PE_5", records)
PE_5_b[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
PE_5_b<-PE_5_b[order(maternal_age)]
rm(records,total)
PE_5_b[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
PE_5_b[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
PE_5_b[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
PE_5_b[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
PE_5_b[lower_95_CI<0,lower_95_CI:=0]
fwrite(PE_5_b,paste0(projectFolder,"/g_output/PE and GDM algorithm/Step_04_PE_5_age.csv"),row.names = F)


records<-pregnancy_d3_gdm_pe[include==1 & !duplicated(pregnancy_id),by="year", .N]
names(records)<-c("year","no_diagnosed_pregnancies")
total<-pregnancy_d3_gdm_pe[!duplicated(pregnancy_id),by="year",.N]
names(total)<-c("year","no_pregnancies")
records<-merge.data.table(records,total,by="year",all=T)
PE_5_c<-data.table(algorithm="PE_5", records)
PE_5_c[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
PE_5_c<-PE_5_c[order(year)]
rm(records,total)
PE_5_c[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
PE_5_c[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
PE_5_c[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
PE_5_c[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
PE_5_c[lower_95_CI<0,lower_95_CI:=0]
fwrite(PE_5_c,paste0(projectFolder,"/g_output/PE and GDM algorithm/Step_04_PE_5_year.csv"),row.names = F)


records<-pregnancy_d3_gdm_pe[include==1 & !duplicated(pregnancy_id),by=c("year_group","maternal_age"), .N]
names(records)<-c("year_group","maternal_age", "no_diagnosed_pregnancies")
total<-pregnancy_d3_gdm_pe[!duplicated(pregnancy_id),by=c("year_group","maternal_age"),.N]
names(total)<-c("year_group","maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","maternal_age"), all=T)
PE_5_d<-data.table(algorithm="PE_5", records)
PE_5_d[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
PE_5_d<-PE_5_d[order(year_group,maternal_age)]
rm(records,total)
pregnancy_d3_gdm_pe[,include:=NULL]
PE_5_d[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
PE_5_d[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
PE_5_d[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
PE_5_d[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
PE_5_d[lower_95_CI<0,lower_95_CI:=0]
fwrite(PE_5_d,paste0(projectFolder,"/g_output/PE and GDM algorithm/Step_04_PE_5_year_age.csv"),row.names = F)
rm(PE_5,PE_5_dt,PE_5_b,PE_5_c,PE_5_d)
rm(inc_col)

if("PE" %in% names(pregnancy_d3_gdm_pe)){pregnancy_d3_gdm_pe[,PE:=NULL]}
if("ECL" %in% names(pregnancy_d3_gdm_pe)){pregnancy_d3_gdm_pe[,ECL:=NULL]}
if("HELLP" %in% names(pregnancy_d3_gdm_pe)){pregnancy_d3_gdm_pe[,HELLP:=NULL]}

date_running_end_04_b<-Sys.Date()
end_time_04_b<-Sys.time()

time_log_04_b<-data.table(DAP=data_access_provider_name,
                     Script="Step_04_b_pe_algorithms.R", 
                     Start_date=date_running_start_04_b, 
                     End_date=date_running_end_04_b,
                     Time_elaspsed=format(end_time_04_b-initial_time_04_b, digits=2))
fwrite(time_log_04_b,paste0(output_dir,"/Time log/Step_04_b_time_log.csv"),row.names = F)
rm(time_log_04_b)