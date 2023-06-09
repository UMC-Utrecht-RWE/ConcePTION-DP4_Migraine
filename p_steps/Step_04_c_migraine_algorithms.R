initial_time<-Sys.time()
date_running_start<-Sys.Date()


####Migraine DIAGNOSES####
print("Loading all Migraine diagnoses D3 and merge with the pregnancy D3.")
obs_period<-data.table(StudyVar=c("MG","MG_NO_AURA","MG_AURA","MG_STATUS","OTHER_MG","UNSP_MG","COMP_MG"),
                       lookback=c(0,0,0,0,0,0,0),
                       start_date=c(0,0,0,0,0,0,0),
                       end_date=c(7*40,7*40,7*40,7*40,7*40,7*40,7*40),
                       after=c(0,0,0,0,0,0,0))
#mig files
mig_files<-list.files(paste0(projectFolder,"/g_intermediate/migraine_algorithm/"))
mig_files<-mig_files[!mig_files %in% "Migraine_medicines.rds"]
names_events<-list()
for(i in 1:length(mig_files)){
  names_events[[i]]<-unlist(str_split(mig_files[i],"[.]"))[1] 
}
names_events<-as.vector(do.call(rbind,names_events))
original<-list()
before<-list()
after<-list()
sum<-list()

  dir.create(paste0(projectFolder,"/g_intermediate/migraine_algorithm/final_d3"))

#Create an exclude variable that will be used to exclude pregnancies with onset migraine diagnoses or triptan prescription
  #pregnancy_d3_mig
  
  
w<-1
for(gdm_fl in 1:length(mig_files)){
  gdm_dt<-readRDS(paste0(projectFolder,"/g_intermediate/gdm_algorithm/", gdm_files[gdm_fl]))
  #merge with the pregnancy d3
  gdm_dt<-merge.data.table(pregnancy_d3_mig, gdm_dt, by="person_id", all.x=T, allow.cartesian = T)
  gdm_dt<-gdm_dt[!is.na(event_date)]
  if(gdm_dt[,.N]>0){
    original[[w]]<-data.table(StudyVar=names_events[gdm_fl], event_records=gdm_dt[,.N])
    gdm_dt[,event_date:=as.IDate(event_date)]
    
    if(obs_period[StudyVar==names_events[gdm_fl],lookback]>0){
      gdm_dt[,lookback:=obs_period[StudyVar==names_events[gdm_fl],lookback]]
      gdm_dt[,start_preg:=as.IDate(pregnancy_start_date-lookback)]
      #exclude all pregnancies that are outside observation period of interest
      gdm_dt[,diff:=event_date-start_preg]
      #remove all records with date before start date pregnancy+lookback
      before[[w]]<-data.table(StudyVar=names_events[gdm_fl], before_start=gdm_dt[diff<0,.N])
      gdm_dt<-gdm_dt[diff>=0]
      gdm_dt[,diff:=NULL][,lookback:=NULL][,start_preg:=NULL]
    }else{
      #remove all records before start obs
      gdm_dt[,start:=obs_period[StudyVar==names_events[gdm_fl],start_date]]
      gdm_dt[,start_preg:=as.IDate(pregnancy_start_date+start)]
      gdm_dt[,diff:=event_date-start_preg]
      before[[w]]<-data.table(StudyVar=names_events[gdm_fl], before_start=gdm_dt[diff<0,.N])
      gdm_dt<-gdm_dt[diff>=0]
      gdm_dt[,diff:=NULL][,start:=NULL][,start_preg:=NULL]
    }
    
    if(gdm_dt[,.N]>0){
      if(obs_period[StudyVar==names_events[gdm_fl],after]>0){
        gdm_dt[,after:=obs_period[StudyVar==names_events[gdm_fl],after]]
        gdm_dt[,end_preg:=as.IDate(pregnancy_end_date+after)]
        gdm_dt[,diff:=event_date-end_preg]
        after[[w]]<-data.table(StudyVar=names_events[gdm_fl], after_end=gdm_dt[diff>0,.N])
        gdm_dt<-gdm_dt[diff<=0]
        gdm_dt[,diff:=NULL][,after:=NULL][,end_preg:=NULL]
      }else{
        gdm_dt[,end:=obs_period[StudyVar==names_events[gdm_fl],end_date]]
        gdm_dt[,end_preg:=as.IDate(pregnancy_start_date+end)]
        gdm_dt[,diff:=event_date-end_preg]
        after[[w]]<-data.table(StudyVar=names_events[gdm_fl], after_end=gdm_dt[diff>0,.N])
        gdm_dt<-gdm_dt[diff<=0]
        gdm_dt[,diff:=NULL][,end:=NULL][,end_preg:=NULL]
      }
    }else{
      after[[w]]<-data.table(StudyVar=names_events[gdm_fl], after_end=0)
    }
    
    if(gdm_dt[,.N]>0){
      #create a summary of included records
      sum[[w]]<-data.table(StudyVar=names_events[gdm_fl], no_records=gdm_dt[!is.na(event_date),.N], no_pregnancies=gdm_dt[!duplicated(pregnancy_id),.N])
      fwrite(gdm_dt, paste0(projectFolder,"/g_intermediate/gdm_algorithm/final_d3/", names_events[gdm_fl],"_pregnancy_D3.rds"),row.names = F)
      cols<-c("person_id","pregnancy_id","pregnancy_start_date","pregnancy_end_date")
      gdm_dt<-gdm_dt[,cols,with=F]
      gdm_dt[,names_events[gdm_fl]:=1]
      gdm_dt<-gdm_dt[,lapply(.SD, sum),.SDcols = names_events[gdm_fl], by=c("person_id","pregnancy_id","pregnancy_start_date","pregnancy_end_date")]
      pregnancy_d3_gdm_pe<-merge.data.table(pregnancy_d3_gdm_pe,gdm_dt,all.x=T, by=cols)
      pregnancy_d3_gdm_pe[is.na(get(names_events[gdm_fl])),names_events[gdm_fl]:=0]
    }else{
      pregnancy_d3_gdm_pe[,names_events[gdm_fl]:=0]
    }
  }else{
    pregnancy_d3_gdm_pe[,names_events[gdm_fl]:=0]
  }
  
  rm(gdm_dt) 
  w<-w+1
}

#Combine files
original<-as.data.table(do.call(rbind,original))
before<-as.data.table(do.call(rbind,before))
after<-as.data.table(do.call(rbind,after))
removed_rec<-merge.data.table(original,before,by="StudyVar",all=T)
removed_rec<-merge.data.table(removed_rec,after,by="StudyVar",all=T)
removed_rec[is.na(before_start),before_start:="N/A"]
removed_rec[is.na(after_end),after_end:="N/A"]
setnames(removed_rec,"event_records","original_records")
#Pregnancies that had the event of interest not in the timeframe of study were excluded, this is why the number of pregnancies is different between different events
sum_gdm<-as.data.table(do.call(rbind,sum))
rm(original,before,after,sum)
rm(obs_period,gdm_files)

#identify events that are not present from conditions_gdm
not_present<-setdiff(names(conditions_gdm), names_events)
pregnancy_d3_gdm_pe[,eval(not_present):=list(0)]
####GDM MEDICINES####
print("Loading all GDM medicines D3 and merge with the pregnancy D3.")

obs_period<-data.table(StudyVar=c("GD_med","PRE_GD_med"),
                       lookback=c(0,6*30.25),
                       start_date=c(98,0),
                       end_date=c(7*40,97),
                       after=c(7,0))


gdm_med_fl<-list.files(paste0(projectFolder,"/g_intermediate/gdm_algorithm/"),"GDM_medicines")
gdm_med<-readRDS(paste0(projectFolder,"/g_intermediate/gdm_algorithm/",gdm_med_fl))

gdm_med<-merge.data.table(pregnancy_d3_gdm_pe, gdm_med, by="person_id", all.x=T, allow.cartesian = T)
gdm_med<-gdm_med[!is.na(medicine_date)]
gdm_med[,pregnancy_start_date:=as.IDate(pregnancy_start_date)][,pregnancy_end_date:=as.IDate(pregnancy_end_date)][,birth_date:=as.IDate(birth_date)][,death_date:=as.IDate(death_date)][,op_start_date_gdm_pe:=as.IDate(op_start_date_gdm_pe)][,op_end_date_gdm_pe:=as.IDate(op_end_date_gdm_pe)][,medicine_date:=as.IDate(medicine_date)]

original<-data.table(StudyVar=c("GD_med","PRE_GD_med"), original_records=as.character(gdm_med[,.N]))
before<-list()
after<-list()
sum<-list()
w<-1
for(med_fl in 1:length(obs_period[,.N])){
if(gdm_med[,.N]>0){
  if(obs_period[med_fl,lookback]>0){
    gdm_med[,lookback:=obs_period[med_fl,lookback]]
    gdm_med[,start_preg:=as.IDate(pregnancy_start_date-lookback)]
    #exclude all pregnancies that are outside observation period of interest
    gdm_med[,diff:=medicine_date-start_preg]
    #remove all records with date before start date pregnancy+lookback
    before[[w]]<-data.table(StudyVar=obs_period[med_fl,StudyVar], before_start=as.character(gdm_med[diff<0,.N]))
    gdm_med<-gdm_med[diff>=0]
    gdm_med[,diff:=NULL][,lookback:=NULL][,start_preg:=NULL]
  }else{
    #remove all records before start obs
    gdm_med[,start:=obs_period[med_fl,start_date]]
    gdm_med[,start_preg:=as.IDate(pregnancy_start_date+start)]
    gdm_med[,diff:=medicine_date-start_preg]
    before[[w]]<-data.table(StudyVar=obs_period[med_fl,StudyVar], before_start=as.character(gdm_med[diff<0,.N]))
    gdm_med<-gdm_med[diff>=0]
    gdm_med[,diff:=NULL][,start:=NULL][,start_preg:=NULL]
  }
  
  if(gdm_med[,.N]>0){
    if(obs_period[med_fl,after]>0){
      gdm_med[,after:=obs_period[med_fl,after]]
      gdm_med[,end_preg:=as.IDate(pregnancy_end_date+after)]
      gdm_med[,diff:=medicine_date-end_preg]
      after[[w]]<-data.table(StudyVar=obs_period[med_fl,StudyVar], after_end=as.character(gdm_med[diff>0,.N]))
      gdm_med<-gdm_med[diff<=0]
      gdm_med[,diff:=NULL][,after:=NULL][,end_preg:=NULL]
    }else{
      gdm_med[,end:=obs_period[med_fl,end_date]]
      gdm_med[,end_preg:=as.IDate(pregnancy_start_date+end)]
      gdm_med[,diff:=medicine_date-end_preg]
      after[[w]]<-data.table(StudyVar=obs_period[med_fl,StudyVar], after_end=as.character(gdm_med[diff>0,.N]))
      gdm_med<-gdm_med[diff<=0]
      gdm_med[,diff:=NULL][,end:=NULL][,end_preg:=NULL]
    }
  }else{
    after[[w]]<-data.table(StudyVar=names_events[gdm_fl], after_end=as.character(0))
  }
  
  if(gdm_med[,.N]>0){
    #create a summary of included records
    sum[[w]]<-data.table(StudyVar=obs_period[med_fl,StudyVar], no_records=gdm_med[!is.na(medicine_date) & !duplicated(pregnancy_id),.N], no_pregnancies=gdm_med[!duplicated(pregnancy_id),.N])
    saveRDS(gdm_med, paste0(projectFolder,"/g_intermediate/gdm_algorithm/final_d3/", obs_period[med_fl,StudyVar],"_pregnancy_D3.rds"))
    cols<-c("person_id","pregnancy_id","pregnancy_start_date","pregnancy_end_date")
    gdm_med<-gdm_med[,cols,with=F]
    gdm_med[,obs_period[med_fl, StudyVar]:=1]
    gdm_med<-gdm_med[,lapply(.SD,sum),by=cols,.SDcols=obs_period[med_fl, StudyVar]]
    pregnancy_d3_gdm_pe<-merge.data.table(pregnancy_d3_gdm_pe,gdm_med,all.x=T, by=cols)
    pregnancy_d3_gdm_pe[is.na(get(obs_period[med_fl,StudyVar])),obs_period[med_fl,StudyVar]:=0]
  }else{
    pregnancy_d3_gdm_pe[,obs_period[med_fl,StudyVar]:=0]
  }
}else{
  pregnancy_d3_gdm_pe[,obs_period[med_fl,StudyVar]:=0]
}
}
rm(gdm_med) 

#Combine files
before<-as.data.table(do.call(rbind,before))
after<-as.data.table(do.call(rbind,after))
removed_rec_med<-merge.data.table(original,before,by="StudyVar",all=T)
removed_rec_med<-merge.data.table(removed_rec_med,after,by="StudyVar",all=T)
removed_rec_med[is.na(before_start),before_start:="N/A"]
removed_rec_med[is.na(after_end),after_end:="N/A"]

removed_rec<-rbind(removed_rec,removed_rec_med)
fwrite(removed_rec,paste0(projectFolder,"/g_output/PE and GDM algorithm/removed_rec_gdm.csv"),row.names = F)
rm(removed_rec)

#Pregnancies that had the event of interest not in the timeframe of study were excluded, this is why the number of pregnancies is different between different events
sum_med<-as.data.table(do.call(rbind,sum))
rm(original,before,after,sum)
rm(gdm_med_fl)

#identify events that are not present from conditions_gdm
not_present<-setdiff(obs_period[,StudyVar], names(pregnancy_d3_gdm_pe))
pregnancy_d3_gdm_pe[,eval(not_present):=list(0)]
rm(obs_period)

print("Export GDM pregnancy D3")
fwrite(pregnancy_d3_gdm_pe,paste0(projectFolder,"/g_intermediate/gdm_algorithm/final_d3/pregnancy_d3_gdm_algorithm.csv"),row.names = F)

####APPLY GDM ALGORITHM####
algorithm_template<-fread(paste0(projectFolder, "/p_steps/parameters/algorithms.csv"))
####GDM_1####
print("Create algorithm GDM_1.")
GDM_1<-algorithm_template[NEW_STUDY_VARIABLES=="GDM_1"]
excl_col<-GDM_1[TYPE=="AND_NOT",STUDY_VARIABLES]
inc_col<-GDM_1[TYPE=="AND",STUDY_VARIABLES]

pregnancy_d3_gdm_pe[pregnancy_d3_gdm_pe[,Reduce("|" , lapply(.SD,`>=`, 1)),.SDcols=excl_col],exclude:=1]
pregnancy_d3_gdm_pe[pregnancy_d3_gdm_pe[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col],include:=1]

fwrite(pregnancy_d3_gdm_pe,paste0(projectFolder,"/g_intermediate/gdm_algorithm/final_d3/GDM_1_D3.rds"),row.names = F)

#export GDM_1
GDM_1_dt<-data.table(algorithm="GDM_1", no_diagnosed_pregnancies=pregnancy_d3_gdm_pe[include==1 & is.na(exclude) & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_gdm_pe[!duplicated(pregnancy_id),.N])

pregnancy_d3_gdm_pe[,maternal_age:=as.character(maternal_age)]
records<-pregnancy_d3_gdm_pe[include==1 & is.na(exclude) & !duplicated(pregnancy_id), .N, by=.(maternal_age)]
names(records)<-c("maternal_age","no_diagnosed_pregnancies")
total<-pregnancy_d3_gdm_pe[!duplicated(pregnancy_id), by="maternal_age",.N]
names(total)<-c("maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by="maternal_age",all=T)
GDM_1_b<-data.table(algorithm="GDM_1", records)
GDM_1_b[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
GDM_1_b<-GDM_1_b[order(maternal_age)]
rm(records,total)

records<-pregnancy_d3_gdm_pe[include==1 & is.na(exclude) & !duplicated(pregnancy_id),by="year", .N]
names(records)<-c("year","no_diagnosed_pregnancies")
total<-pregnancy_d3_gdm_pe[!duplicated(pregnancy_id),by="year",.N]
names(total)<-c("year","no_pregnancies")
records<-merge.data.table(records,total,by="year",all=T)
GDM_1_c<-data.table(algorithm="GDM_1", records)
GDM_1_c[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
GDM_1_c<-GDM_1_c[order(year)]
rm(records,total)

records<-pregnancy_d3_gdm_pe[include==1 & is.na(exclude) & !duplicated(pregnancy_id),by=c("year_group","maternal_age"), .N]
names(records)<-c("year_group","maternal_age", "no_diagnosed_pregnancies")
total<-pregnancy_d3_gdm_pe[!duplicated(pregnancy_id),by=c("year_group","maternal_age"),.N]
names(total)<-c("year_group","maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","maternal_age"), all=T)
GDM_1_d<-data.table(algorithm="GDM_1", records)
GDM_1_d[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
GDM_1_d<-GDM_1_d[order(year_group,maternal_age)]
rm(records,total)
pregnancy_d3_gdm_pe[,include:=NULL][,exclude:=NULL]

####GDM_2####
print("Create algorithm GDM_2.")
GDM_2<-algorithm_template[NEW_STUDY_VARIABLES=="GDM_2"]
excl_col<-GDM_2[TYPE=="AND_NOT",STUDY_VARIABLES]
#inc_col<-GDM_2[TYPE=="AND",STUDY_VARIABLES]
alt_col<-GDM_2[TYPE=="OR",STUDY_VARIABLES]

if(length(alt_col)>0){pregnancy_d3_gdm_pe[pregnancy_d3_gdm_pe[,Reduce("|" , lapply(.SD,`>=`, 1)),.SDcols=alt_col],alternative:=1]}else{pregnancy_d3_gdm_pe[,alternative:=NA]}
if(length(excl_col)>0){pregnancy_d3_gdm_pe[pregnancy_d3_gdm_pe[,Reduce("|" , lapply(.SD,`>=`, 1)),.SDcols=excl_col],exclude:=1]}else{pregnancy_d3_gdm_pe[,exclude:=NA]}

fwrite(pregnancy_d3_gdm_pe,paste0(projectFolder,"/g_intermediate/gdm_algorithm/final_d3/GDM_2_D3.rds"),row.names = F)

#export GDM_2
GDM_2_dt<-data.table(algorithm="GDM_2", no_diagnosed_pregnancies=pregnancy_d3_gdm_pe[alternative==1 & is.na(exclude) & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_gdm_pe[!duplicated(pregnancy_id),.N])

pregnancy_d3_gdm_pe[,maternal_age:=as.character(maternal_age)]
records<-pregnancy_d3_gdm_pe[alternative==1 & is.na(exclude) & !duplicated(pregnancy_id), .N, by=.(maternal_age)]
names(records)<-c("maternal_age","no_diagnosed_pregnancies")
total<-pregnancy_d3_gdm_pe[!duplicated(pregnancy_id), by="maternal_age",.N]
names(total)<-c("maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by="maternal_age",all=T)
GDM_2_b<-data.table(algorithm="GDM_2", records)
GDM_2_b[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
GDM_2_b<-GDM_2_b[order(maternal_age)]
rm(records,total)

records<-pregnancy_d3_gdm_pe[alternative==1 & is.na(exclude) & !duplicated(pregnancy_id),by="year", .N]
names(records)<-c("year","no_diagnosed_pregnancies")
total<-pregnancy_d3_gdm_pe[!duplicated(pregnancy_id),by="year",.N]
names(total)<-c("year","no_pregnancies")
records<-merge.data.table(records,total,by="year",all=T)
GDM_2_c<-data.table(algorithm="GDM_2", records)
GDM_2_c[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
GDM_2_c<-GDM_2_c[order(year)]
rm(records,total)

records<-pregnancy_d3_gdm_pe[alternative==1 & is.na(exclude) & !duplicated(pregnancy_id),by=c("year_group","maternal_age"), .N]
names(records)<-c("year_group","maternal_age", "no_diagnosed_pregnancies")
total<-pregnancy_d3_gdm_pe[!duplicated(pregnancy_id),by=c("year_group","maternal_age"),.N]
names(total)<-c("year_group","maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","maternal_age"), all=T)
GDM_2_d<-data.table(algorithm="GDM_2", records)
GDM_2_d[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
GDM_2_d<-GDM_2_d[order(year_group,maternal_age)]
rm(records,total)
pregnancy_d3_gdm_pe[,alternative:=NULL][,exclude:=NULL]

####GDM_3####
print("Create algorithm GDM_3.")
GDM_3<-algorithm_template[NEW_STUDY_VARIABLES=="GDM_3"]
excl_col<-GDM_3[TYPE=="AND_NOT",STUDY_VARIABLES]
#inc_col<-GDM_2[TYPE=="AND",STUDY_VARIABLES]
alt_col<-GDM_3[TYPE=="OR",STUDY_VARIABLES]
if(algorithm_template[NEW_STUDY_VARIABLES=="GDM_3" & COMPLEXITY=="complex" & !is.na(RULE),.N]>0){
  rule<-unique(algorithm_template[NEW_STUDY_VARIABLES=="GDM_3" & COMPLEXITY=="complex",RULE])
  value<-unique(algorithm_template[NEW_STUDY_VARIABLES=="GDM_3" & COMPLEXITY=="complex",VALUE])
}else{
  script<-algorithm_template[NEW_STUDY_VARIABLES=="GDM_3" & COMPLEXITY=="complex",SCRIPT]
}

if(length(alt_col)>0){pregnancy_d3_gdm_pe[pregnancy_d3_gdm_pe[,Reduce("|" , lapply(.SD,rule, value)),.SDcols=alt_col],alternative:=1]}else{pregnancy_d3_gdm_pe[,alternative:=NA]}
if(length(excl_col)>0){pregnancy_d3_gdm_pe[pregnancy_d3_gdm_pe[,Reduce("|" , lapply(.SD,`>=`, 1)),.SDcols=excl_col],exclude:=1]}else{pregnancy_d3_gdm_pe[,exclude:=NA]}

fwrite(pregnancy_d3_gdm_pe,paste0(projectFolder,"/g_intermediate/gdm_algorithm/final_d3/GDM_3_D3.rds"),row.names = F)

#export GDM_3
GDM_3_dt<-data.table(algorithm="GDM_3", no_diagnosed_pregnancies=pregnancy_d3_gdm_pe[alternative==1 & is.na(exclude) & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_gdm_pe[!duplicated(pregnancy_id),.N])

pregnancy_d3_gdm_pe[,maternal_age:=as.character(maternal_age)]
records<-pregnancy_d3_gdm_pe[alternative==1 & is.na(exclude) & !duplicated(pregnancy_id), .N, by=.(maternal_age)]
names(records)<-c("maternal_age","no_diagnosed_pregnancies")
total<-pregnancy_d3_gdm_pe[!duplicated(pregnancy_id), by="maternal_age",.N]
names(total)<-c("maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by="maternal_age",all=T)
GDM_3_b<-data.table(algorithm="GDM_3", records)
GDM_3_b[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
GDM_3_b<-GDM_3_b[order(maternal_age)]
rm(records,total)

records<-pregnancy_d3_gdm_pe[alternative==1 & is.na(exclude) & !duplicated(pregnancy_id),by="year", .N]
names(records)<-c("year","no_diagnosed_pregnancies")
total<-pregnancy_d3_gdm_pe[!duplicated(pregnancy_id),by="year",.N]
names(total)<-c("year","no_pregnancies")
records<-merge.data.table(records,total,by="year",all=T)
GDM_3_c<-data.table(algorithm="GDM_3", records)
GDM_3_c[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
GDM_3_c<-GDM_3_c[order(year)]
rm(records,total)

records<-pregnancy_d3_gdm_pe[alternative==1 & is.na(exclude) & !duplicated(pregnancy_id),by=c("year_group","maternal_age"), .N]
names(records)<-c("year_group","maternal_age", "no_diagnosed_pregnancies")
total<-pregnancy_d3_gdm_pe[!duplicated(pregnancy_id),by=c("year_group","maternal_age"),.N]
names(total)<-c("year_group","maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","maternal_age"), all=T)
GDM_3_d<-data.table(algorithm="GDM_3", records)
GDM_3_d[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
GDM_3_d<-GDM_3_d[order(year_group,maternal_age)]
rm(records,total)
pregnancy_d3_gdm_pe[,alternative:=NULL][,exclude:=NULL]

####GDM_4####
print("Create algorithm GDM_4.")
GDM_4<-algorithm_template[NEW_STUDY_VARIABLES=="GDM_4"]
excl_col<-GDM_4[TYPE=="AND_NOT",STUDY_VARIABLES]
#inc_col<-GDM_2[TYPE=="AND",STUDY_VARIABLES]
alt_col<-GDM_4[TYPE=="OR",STUDY_VARIABLES]

if(length(alt_col)>0){pregnancy_d3_gdm_pe[pregnancy_d3_gdm_pe[,Reduce("|" , lapply(.SD,`>=`, 1)),.SDcols=alt_col],alternative:=1]}else{pregnancy_d3_gdm_pe[,alternative:=NA]}
if(length(excl_col)>0){pregnancy_d3_gdm_pe[pregnancy_d3_gdm_pe[,Reduce("|" , lapply(.SD,`>=`, 1)),.SDcols=excl_col],exclude:=1]}else{pregnancy_d3_gdm_pe[,exclude:=NA]}

fwrite(pregnancy_d3_gdm_pe,paste0(projectFolder,"/g_intermediate/gdm_algorithm/final_d3/GDM_4_D3.rds"),row.names = F)

#export GDM_3
GDM_4_dt<-data.table(algorithm="GDM_4", no_diagnosed_pregnancies=pregnancy_d3_gdm_pe[alternative==1 & is.na(exclude) & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_gdm_pe[!duplicated(pregnancy_id),.N])

pregnancy_d3_gdm_pe[,maternal_age:=as.character(maternal_age)]
records<-pregnancy_d3_gdm_pe[alternative==1 & is.na(exclude) & !duplicated(pregnancy_id), .N, by=.(maternal_age)]
names(records)<-c("maternal_age","no_diagnosed_pregnancies")
total<-pregnancy_d3_gdm_pe[!duplicated(pregnancy_id), by="maternal_age",.N]
names(total)<-c("maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by="maternal_age",all=T)
GDM_4_b<-data.table(algorithm="GDM_4", records)
GDM_4_b[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
GDM_4_b<-GDM_4_b[order(maternal_age)]
rm(records,total)

records<-pregnancy_d3_gdm_pe[alternative==1 & is.na(exclude) & !duplicated(pregnancy_id),by="year", .N]
names(records)<-c("year","no_diagnosed_pregnancies")
total<-pregnancy_d3_gdm_pe[!duplicated(pregnancy_id),by="year",.N]
names(total)<-c("year","no_pregnancies")
records<-merge.data.table(records,total,by="year",all=T)
GDM_4_c<-data.table(algorithm="GDM_4", records)
GDM_4_c[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
GDM_4_c<-GDM_4_c[order(year)]
rm(records,total)

records<-pregnancy_d3_gdm_pe[alternative==1 & is.na(exclude) & !duplicated(pregnancy_id),by=c("year_group","maternal_age"), .N]
names(records)<-c("year_group","maternal_age", "no_diagnosed_pregnancies")
total<-pregnancy_d3_gdm_pe[!duplicated(pregnancy_id),by=c("year_group","maternal_age"),.N]
names(total)<-c("year_group","maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","maternal_age"), all=T)
GDM_4_d<-data.table(algorithm="GDM_4", records)
GDM_4_d[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
GDM_4_d<-GDM_4_d[order(year_group,maternal_age)]
rm(records,total)
pregnancy_d3_gdm_pe[,alternative:=NULL][,exclude:=NULL]

####GDM_5####
print("Create algorithm GDM_5.")
GDM_5<-algorithm_template[NEW_STUDY_VARIABLES=="GDM_5"]
excl_col<-GDM_5[TYPE=="AND_NOT",STUDY_VARIABLES]
#inc_col<-GDM_2[TYPE=="AND",STUDY_VARIABLES]
alt_col<-GDM_5[TYPE=="OR",STUDY_VARIABLES]

if(length(alt_col)>0){pregnancy_d3_gdm_pe[pregnancy_d3_gdm_pe[,Reduce("|" , lapply(.SD,`>=`, 1)),.SDcols=alt_col],alternative:=1]}else{pregnancy_d3_gdm_pe[,alternative:=NA]}
if(length(excl_col)>0){pregnancy_d3_gdm_pe[pregnancy_d3_gdm_pe[,Reduce("|" , lapply(.SD,`>=`, 1)),.SDcols=excl_col],exclude:=1]}else{pregnancy_d3_gdm_pe[,exclude:=NA]}

fwrite(pregnancy_d3_gdm_pe,paste0(projectFolder,"/g_intermediate/gdm_algorithm/final_d3/GDM_5_D3.rds"),row.names = F)

#export GDM_3
GDM_5_dt<-data.table(algorithm="GDM_5", no_diagnosed_pregnancies=pregnancy_d3_gdm_pe[alternative==1 & is.na(exclude) & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_gdm_pe[!duplicated(pregnancy_id),.N])

pregnancy_d3_gdm_pe[,maternal_age:=as.character(maternal_age)]
records<-pregnancy_d3_gdm_pe[alternative==1 & is.na(exclude) & !duplicated(pregnancy_id), .N, by=.(maternal_age)]
names(records)<-c("maternal_age","no_diagnosed_pregnancies")
total<-pregnancy_d3_gdm_pe[!duplicated(pregnancy_id), by="maternal_age",.N]
names(total)<-c("maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by="maternal_age",all=T)
GDM_5_b<-data.table(algorithm="GDM_5", records)
GDM_5_b[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
GDM_5_b<-GDM_5_b[order(maternal_age)]
rm(records,total)

records<-pregnancy_d3_gdm_pe[alternative==1 & is.na(exclude) & !duplicated(pregnancy_id),by="year", .N]
names(records)<-c("year","no_diagnosed_pregnancies")
total<-pregnancy_d3_gdm_pe[!duplicated(pregnancy_id),by="year",.N]
names(total)<-c("year","no_pregnancies")
records<-merge.data.table(records,total,by="year",all=T)
GDM_5_c<-data.table(algorithm="GDM_5", records)
GDM_5_c[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
GDM_5_c<-GDM_5_c[order(year)]
rm(records,total)

records<-pregnancy_d3_gdm_pe[alternative==1 & is.na(exclude) & !duplicated(pregnancy_id),by=c("year_group","maternal_age"), .N]
names(records)<-c("year_group","maternal_age", "no_diagnosed_pregnancies")
total<-pregnancy_d3_gdm_pe[!duplicated(pregnancy_id),by=c("year_group","maternal_age"),.N]
names(total)<-c("year_group","maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","maternal_age"), all=T)
GDM_5_d<-data.table(algorithm="GDM_5", records)
GDM_5_d[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
GDM_5_d<-GDM_5_d[order(year_group,maternal_age)]
rm(records,total)
pregnancy_d3_gdm_pe[,alternative:=NULL][,exclude:=NULL]

###GDM_8###
GDM_8<-algorithm_template[NEW_STUDY_VARIABLES=="GDM_8"]
#excl_col<-GDM_8[TYPE=="AND_NOT",STUDY_VARIABLES]
inc_col<-GDM_8[TYPE=="AND",STUDY_VARIABLES]

#pregnancy_d3_gdm_pe[pregnancy_d3_gdm_pe[,Reduce("|" , lapply(.SD,`==`, 1)),.SDcols=excl_col],exclude:=1]
pregnancy_d3_gdm_pe[pregnancy_d3_gdm_pe[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col],include:=1]
#export GDM_8
GDM_8_dt<-data.table(algorithm="GDM_8", no_diagnosed_pregnancies=pregnancy_d3_gdm_pe[include==1 & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_gdm_pe[!duplicated(pregnancy_id),.N])

fwrite(pregnancy_d3_gdm_pe,paste0(projectFolder,"/g_intermediate/gdm_algorithm/final_d3/GDM_8_D3.rds"),row.names = F)

#export GDM_8
GDM_8_dt<-data.table(algorithm="GDM_8", no_diagnosed_pregnancies=pregnancy_d3_gdm_pe[include==1 & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_gdm_pe[!duplicated(pregnancy_id),.N])

pregnancy_d3_gdm_pe[,maternal_age:=as.character(maternal_age)]
records<-pregnancy_d3_gdm_pe[include==1 & !duplicated(pregnancy_id), .N, by=.(maternal_age)]
names(records)<-c("maternal_age","no_diagnosed_pregnancies")
total<-pregnancy_d3_gdm_pe[!duplicated(pregnancy_id), by="maternal_age",.N]
names(total)<-c("maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by="maternal_age",all=T)
GDM_8_b<-data.table(algorithm="GDM_8", records)
GDM_8_b[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
GDM_8_b<-GDM_8_b[order(maternal_age)]
rm(records,total)

records<-pregnancy_d3_gdm_pe[include==1 & !duplicated(pregnancy_id),by="year", .N]
names(records)<-c("year","no_diagnosed_pregnancies")
total<-pregnancy_d3_gdm_pe[!duplicated(pregnancy_id),by="year",.N]
names(total)<-c("year","no_pregnancies")
records<-merge.data.table(records,total,by="year",all=T)
GDM_8_c<-data.table(algorithm="GDM_8", records)
GDM_8_c[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
GDM_8_c<-GDM_8_c[order(year)]
rm(records,total)

records<-pregnancy_d3_gdm_pe[include==1 & !duplicated(pregnancy_id),by=c("year_group","maternal_age"), .N]
names(records)<-c("year_group","maternal_age", "no_diagnosed_pregnancies")
total<-pregnancy_d3_gdm_pe[!duplicated(pregnancy_id),by=c("year_group","maternal_age"),.N]
names(total)<-c("year_group","maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","maternal_age"), all=T)
GDM_8_d<-data.table(algorithm="GDM_8", records)
GDM_8_d[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
GDM_8_d<-GDM_8_d[order(year_group,maternal_age)]
rm(records,total)
pregnancy_d3_gdm_pe[,include:=NULL]

if("DM" %in% names(pregnancy_d3_gdm_pe)){pregnancy_d3_gdm_pe[,DM:=NULL]}
if("GD" %in% names(pregnancy_d3_gdm_pe)){pregnancy_d3_gdm_pe[,GD:=NULL]}
if("GDM_checkbox" %in% names(pregnancy_d3_gdm_pe)){pregnancy_d3_gdm_pe[,GDM_checkbox:=NULL]}
if("PRE_GD" %in% names(pregnancy_d3_gdm_pe)){pregnancy_d3_gdm_pe[,PRE_GD:=NULL]}
if("UNK_GD" %in% names(pregnancy_d3_gdm_pe)){pregnancy_d3_gdm_pe[,UNK_GD:=NULL]}
if("DM_PREG" %in% names(pregnancy_d3_gdm_pe)){pregnancy_d3_gdm_pe[,DM_PREG:=NULL]}
if("GD_med" %in% names(pregnancy_d3_gdm_pe)){pregnancy_d3_gdm_pe[,GD_med:=NULL]}
if("PRE_GD_med" %in% names(pregnancy_d3_gdm_pe)){pregnancy_d3_gdm_pe[,PRE_GD_med:=NULL]}

date_running_end<-Sys.Date()
end_time<-Sys.time()

time_log<-data.table(DAP=data_access_provider_name,
                     Script="Step_04_c_migraine_algorithms.R", 
                     Start_date=date_running_start, 
                     End_date=date_running_end,
                     Time_elaspsed=format(end_time-initial_time, digits=2))
fwrite(time_log,paste0(output_dir,"/Time log/Step_04_c_time_log.csv"),row.names = F)


