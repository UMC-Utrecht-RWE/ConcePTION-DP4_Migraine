initial_time<-Sys.time()
date_running_start<-Sys.Date()


age_band_creation<-function(x){
  if(x>=15 & x<=24){y<-"15-24"}
  if(x>=25 & x<=29){y<-"25-29"}
  if(x>=30 & x<=34){y<-"30-34"}
  if(x>=35 & x<=39){y<-"35-39"}
  if(x>=40){y<-"40+"}
  return(y)
}

year_group_creation<-function(x){
  if(x>=2005 & x<=2009){y<-"2005-2009"}
  if(x>=2010 & x<=2014){y<-"2010-2014"}
  if(x>=2015 & x<=2019){y<-"2015-2019"}
  return(y)
}


#Load all gdm_algorithm file and combine with the pregnancy D3
pregnancy_d3_gdm_pe<-readRDS(paste0(projectFolder,"/g_intermediate/pregnancy_d3/GDM_PE_Pregnancy_D3.rds"))
pregnancy_d3_gdm_pe[,pregnancy_start_date:=as.IDate(pregnancy_start_date)][,pregnancy_end_date:=as.IDate(pregnancy_end_date)][,birth_date:=as.IDate(birth_date)][,death_date:=as.IDate(death_date)][,op_start_date_gdm_pe:=as.IDate(op_start_date_gdm_pe)][,op_end_date_gdm_pe:=as.IDate(op_end_date_gdm_pe)]
pregnancy_d3_gdm_pe[,age:=floor((pregnancy_start_date-birth_date)/365.25)]
pregnancy_d3_gdm_pe[,maternal_age:=as.character(lapply(age, age_band_creation))]
pregnancy_d3_gdm_pe[,year:=year(pregnancy_start_date)]
pregnancy_d3_gdm_pe[,year_group:=as.character(lapply(year, year_group_creation))]

####GDM DIAGNOSES####
obs_period<-data.table(StudyVar=c("GD","UNK_GD","PRE_GD","DM","DM_PREG","GDM_checkbox"),
                       lookback=c(0,0,0,6*30.25,0,0),
                       start_date=c(98,98,0,0,0,0),
                       end_date=c(7*40,7*40,97,97,7*40,7*40),
                       after=c(7,7,0,0,7,7))
#gdm files
gdm_files<-list.files(paste0(projectFolder,"/g_intermediate/gdm_algorithm/"))
names_events<-list()
for(i in 1:length(gdm_files)){
  names_events[[i]]<-unlist(str_split(gdm_files[i],"[.]"))[1] 
}
names_events<-as.vector(do.call(rbind,names_events))
original<-list()
before<-list()
after<-list()
sum<-list()
dir.create(paste0(projectFolder,"/g_intermediate/gdm_algorithm/final_d3"))

w<-1
for(gdm_fl in 1:length(gdm_files)){
 gdm_dt<-readRDS(paste0(projectFolder,"/g_intermediate/gdm_algorithm/", gdm_files[gdm_fl]))
 #merge with the pregnancy d3
 gdm_dt<-merge.data.table(pregnancy_d3_gdm_pe, gdm_dt, by="person_id", all.x=T, allow.cartesian = T)
 gdm_dt<-gdm_dt[!is.na(event_date)]
 gdm_dt[,pregnancy_start_date:=as.IDate(pregnancy_start_date)][,pregnancy_end_date:=as.IDate(pregnancy_end_date)][,birth_date:=as.IDate(birth_date)][,death_date:=as.IDate(death_date)][,op_start_date_gdm_pe:=as.IDate(op_start_date_gdm_pe)][,op_end_date_gdm_pe:=as.IDate(op_end_date_gdm_pe)][,event_date:=as.IDate(event_date)]
 original[[w]]<-data.table(StudyVar=names_events[gdm_fl], event_records=as.character(gdm_dt[,.N]))
  if(gdm_dt[,.N]>0){
  if(obs_period[StudyVar==names_events[gdm_fl],lookback]>0){
 gdm_dt[,lookback:=obs_period[StudyVar==names_events[gdm_fl],lookback]]
 gdm_dt[,start_preg:=as.IDate(pregnancy_start_date-lookback)]
#exclude all pregnancies that are outside observation period of interest
 gdm_dt[,diff:=event_date-start_preg]
#remove all records with date before start date pregnancy+lookback
 before[[w]]<-data.table(StudyVar=names_events[gdm_fl], before_start=as.character(gdm_dt[diff<0,.N]))
 gdm_dt<-gdm_dt[diff>=0]
 gdm_dt[,diff:=NULL][,lookback:=NULL][,start_preg:=NULL]
 }else{
   #remove all records before start obs
   gdm_dt[,start:=obs_period[StudyVar==names_events[gdm_fl],start_date]]
   gdm_dt[,start_preg:=as.IDate(pregnancy_start_date+start)]
   gdm_dt[,diff:=event_date-start_preg]
   before[[w]]<-data.table(StudyVar=names_events[gdm_fl], before_start=as.character(gdm_dt[diff<0,.N]))
   gdm_dt<-gdm_dt[diff>=0]
   gdm_dt[,diff:=NULL][,start:=NULL][,start_preg:=NULL]
 }
 
 if(gdm_dt[,.N]>0){
   if(obs_period[StudyVar==names_events[gdm_fl],after]>0){
     gdm_dt[,after:=obs_period[StudyVar==names_events[gdm_fl],after]]
     gdm_dt[,end_preg:=as.IDate(pregnancy_end_date+after)]
     gdm_dt[,diff:=event_date-end_preg]
     after[[w]]<-data.table(StudyVar=names_events[gdm_fl], after_end=as.character(gdm_dt[diff>0,.N]))
     gdm_dt<-gdm_dt[diff<=0]
     gdm_dt[,diff:=NULL][,after:=NULL][,end_preg:=NULL]
   }else{
   gdm_dt[,end:=obs_period[StudyVar==names_events[gdm_fl],end_date]]
   gdm_dt[,end_preg:=as.IDate(pregnancy_start_date+end)]
   gdm_dt[,diff:=event_date-end_preg]
   after[[w]]<-data.table(StudyVar=names_events[gdm_fl], after_end=as.character(gdm_dt[diff>0,.N]))
   gdm_dt<-gdm_dt[diff<=0]
   gdm_dt[,diff:=NULL][,end:=NULL][,end_preg:=NULL]
   }
 }else{
   after[[w]]<-data.table(StudyVar=names_events[gdm_fl], after_end=as.character(0))
 }
 
 if(gdm_dt[,.N]>0){
   #create a summary of included records
   sum[[w]]<-data.table(StudyVar=names_events[gdm_fl], no_records=gdm_dt[!is.na(event_date) & !duplicated(pregnancy_id),.N], no_pregnancies=gdm_dt[!duplicated(pregnancy_id),.N])
   saveRDS(gdm_dt, paste0(projectFolder,"/g_intermediate/gdm_algorithm/final_d3/", names_events[gdm_fl],"_pregnancy_D3.rds"))
   cols<-c("person_id","pregnancy_id","pregnancy_start_date","pregnancy_end_date")
   gdm_dt<-gdm_dt[,cols,with=F]
   gdm_dt[,names_events[gdm_fl]:=1]
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

#Pregnancies that had the event of interest not in the timeframe of study were excluded, this is why the number of pregnancies is different between different events
sum_gdm<-as.data.table(do.call(rbind,sum))
rm(original,before,after,sum)
rm(obs_period,gdm_files)

#identify events that are not present from conditions_gdm
not_present<-setdiff(names(conditions_gdm), names_events)
pregnancy_d3_gdm_pe[,eval(not_present):=list(0,0)]
####GDM MEDICINES####

####APPLY GDM ALGORITHM####
algorithm_template<-fread("/Users/vhoxhaj/Desktop/DP4_Migraine_GitHub/Demonstration project/DP4_Migraine/p_steps/parameters/algorithms.csv")
#GDM_1
GDM_1<-algorithm_template[NEW_STUDY_VARIABLES=="GDM_1"]
excl_col<-GDM_1[TYPE=="AND_NOT",STUDY_VARIABLES]
inc_col<-GDM_1[TYPE=="AND",STUDY_VARIABLES]

pregnancy_d3_gdm_pe[pregnancy_d3_gdm_pe[,Reduce("|" , lapply(.SD,`==`, 1)),.SDcols=excl_col],exclude:=1]
pregnancy_d3_gdm_pe[pregnancy_d3_gdm_pe[,Reduce("|" , lapply(.SD,`==`, 1)),.SDcols=inc_col],include:=1]
#export GDM_1
GDM_1_dt<-data.table(algorithm="GDM_1", no_diagnosed_pregnancies=pregnancy_d3_gdm_pe[include==1 & is.na(exclude) & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_gdm_pe[!duplicated(pregnancy_id),.N])

pregnancy_d3_gdm_pe[,GDM_1:=NULL][,include:=NULL][,exclude:=NULL]
#GDM_8
GDM_8<-algorithm_template[NEW_STUDY_VARIABLES=="GDM_8"]
excl_col<-GDM_8[TYPE=="AND_NOT",STUDY_VARIABLES]
inc_col<-GDM_8[TYPE=="AND",STUDY_VARIABLES]

pregnancy_d3_gdm_pe[pregnancy_d3_gdm_pe[,Reduce("|" , lapply(.SD,`==`, 1)),.SDcols=excl_col],exclude:=1]
pregnancy_d3_gdm_pe[pregnancy_d3_gdm_pe[,Reduce("|" , lapply(.SD,`==`, 1)),.SDcols=inc_col],include:=1]
#export GDM_8
GDM_8_dt<-data.table(algorithm="GDM_8", no_diagnosed_pregnancies=pregnancy_d3_gdm_pe[include==1 & is.na(exclude) & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_gdm_pe[!duplicated(pregnancy_id),.N])


pregnancy_d3_gdm_pe[,DM:=NULL][,GD:=NULL][,GDM_checkbox:=NULL][,PRE_GD:=NULL][,UNK_GD:=NULL][,DM_PREG:=NULL]
####PE DIAGNOSES####
obs_period<-data.table(StudyVar=c("PE","ECL","HELLP","PE_checkbox"),
                       lookback=c(0,0,0,0),
                       start_date=c(20*7,20*7,20*7,20*7),
                       end_date=c(7*40,7*40,7*40,7*40),
                       after=c(7,7,7,7))
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
  pe_dt[,event_date:=as.IDate(event_date,"%Y%m%d")]
  
  if(obs_period[StudyVar==names_events[pe_fl],lookback]>0){
    pe_dt[,lookback:=obs_period[StudyVar==names_events[pe_fl],lookback]]
    pe_dt[,start_preg:=as.IDate(pregnancy_start_date-lookback)]
    #exclude all pregnancies that are outside observation period of interest
    pe_dt[,diff:=event_date-start_preg]
    #remove all records with date before start date pregnancy+lookback
    before[[w]]<-data.table(StudyVar=names_events[pe_fl], before_start=pe_dt[diff<0,.N])
    pe_dt<-pe_dt[diff>=0]
    pe_dt[,diff:=NULL][,lookback:=NULL][,start_preg:=NULL]
  }else{
    #remove all records before start obs
    pe_dt[,start:=obs_period[StudyVar==names_events[pe_fl],start_date]]
    pe_dt[,start_preg:=as.IDate(pregnancy_start_date+start)]
    pe_dt[,diff:=event_date-start_preg]
    before[[w]]<-data.table(StudyVar=names_events[pe_fl], before_start=pe_dt[diff<0,.N])
    pe_dt<-pe_dt[diff>=0]
    pe_dt[,diff:=NULL][,start:=NULL][,start_preg:=NULL]
  }
  
  if(pe_dt[,.N]>0){
    if(obs_period[StudyVar==names_events[pe_fl],after]>0){
      pe_dt[,after:=obs_period[StudyVar==names_events[pe_fl],after]]
      pe_dt[,end_preg:=as.IDate(pregnancy_end_date+after)]
      pe_dt[,diff:=event_date-end_preg]
      after[[w]]<-data.table(StudyVar=names_events[pe_fl], after_end=pe_dt[diff>0,.N])
      pe_dt<-pe_dt[diff<=0]
      pe_dt[,diff:=NULL][,after:=NULL][,end_preg:=NULL]
    }else{
      pe_dt[,end:=obs_period[StudyVar==names_events[pe_fl],end_date]]
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
removed_rec<-merge.data.table(original,before,by="StudyVar",all=T)
removed_rec<-merge.data.table(removed_rec,after,by="StudyVar",all=T)
removed_rec[is.na(before_start),before_start:="N/A"]
removed_rec[is.na(after_end),after_end:="N/A"]

#Pregnancies that had the event of interest not in the timeframe of study were excluded, this is why the number of pregnancies is different between different events
sum_pe<-as.data.table(do.call(rbind,sum))
rm(original,before,after,sum)
rm(obs_period,pe_files)

#identify events that are not present from conditions_gdm
not_present<-setdiff(names(conditions_pe), names_events)
pregnancy_d3_gdm_pe[,eval(not_present):=list(0,0)]

####APPLY PE ALGORITHM####
algorithm_template<-fread("/Users/vhoxhaj/Desktop/DP4_Migraine_GitHub/Demonstration project/DP4_Migraine/p_steps/parameters/algorithms.csv")
#PE_1
PE_1<-algorithm_template[NEW_STUDY_VARIABLES=="PE_1"]
#excl_col<-PE_1[TYPE=="AND_NOT",STUDY_VARIABLES]
inc_col<-PE_1[TYPE=="AND",STUDY_VARIABLES]
#alt_col<-PE_1[TYPE=="OR",STUDY_VARIABLES]

#if(length(excl_col)>0){pregnancy_d3_gdm_pe[pregnancy_d3_gdm_pe[,Reduce("|" , lapply(.SD,`>=`, 1)),.SDcols=excl_col],exclude:=1]}else{pregnancy_d3_gdm_pe[,exclude:=NA]}
  if(length(inc_col)>0){pregnancy_d3_gdm_pe[pregnancy_d3_gdm_pe[,Reduce("|" , lapply(.SD,`>=`, 1)),.SDcols=inc_col],include:=1]}else{pregnancy_d3_gdm_pe[,include:=NA]}
    #if(length(alt_col)>0){pregnancy_d3_gdm_pe[pregnancy_d3_gdm_pe[,Reduce("|" , lapply(.SD,`>=`, 1)),.SDcols=alt_col],alternative:=1]}else{pregnancy_d3_gdm_pe[,alternative:=1]}

#export PE_1
PE_1_a<-data.table(algorithm="PE_1", no_diagnosed_pregnancies=pregnancy_d3_gdm_pe[include>=1 & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_gdm_pe[!duplicated(pregnancy_id),.N])

pregnancy_d3_gdm_pe[,maternal_age:=as.character(maternal_age)]
records<-pregnancy_d3_gdm_pe[include>=1 & !duplicated(pregnancy_id), .N, by=.(maternal_age)]
names(records)<-c("maternal_age","no_diagnosed_pregnancies")
total<-pregnancy_d3_gdm_pe[!duplicated(pregnancy_id), by="maternal_age",.N]
names(total)<-c("maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by="maternal_age",all=T)
PE_1_b<-data.table(algorithm="PE_1", records)
PE_1_b[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
PE_1_b<-PE_1_b[order(maternal_age)]
rm(records,total)

ggplot(PE_1_b) +
  geom_col(aes(x = factor(maternal_age,levels =c('15-24','25-29','30-34','35-39','40+')),  
               y = no_diagnosed_pregnancies), fill = "blue", width = 0.3) +
  geom_col(aes(x = maternal_age, y = no_pregnancies), alpha = 0.3, fill = "red", width = 0.6) +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(size = 8,angle = 45))

records<-pregnancy_d3_gdm_pe[include>=1 & !duplicated(pregnancy_id),by="year", .N]
names(records)<-c("year","no_diagnosed_pregnancies")
total<-pregnancy_d3_gdm_pe[!duplicated(pregnancy_id),by="year",.N]
names(total)<-c("year","no_pregnancies")
records<-merge.data.table(records,total,by="year",all=T)
PE_1_c<-data.table(algorithm="PE_1", records)
PE_1_c[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
PE_1_c<-PE_1_c[order(year)]
rm(records,total)

years<-PE_1_c[!duplicated(year),year]
ggplot(PE_1_c) +
  geom_col(aes(x = year, y = no_diagnosed_pregnancies), fill = "blue", width = 0.3) +
  geom_col(aes(x = year, y = no_pregnancies), alpha = 0.3, fill = "red", width = 0.6) +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(size = 8,angle = 45))

records<-pregnancy_d3_gdm_pe[include>=1 & !duplicated(pregnancy_id),by=c("year_group","maternal_age"), .N]
names(records)<-c("year_group","maternal_age", "no_diagnosed_pregnancies")
total<-pregnancy_d3_gdm_pe[!duplicated(pregnancy_id),by=c("year_group","maternal_age"),.N]
names(total)<-c("year_group","maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","maternal_age"), all=T)
PE_1_d<-data.table(algorithm="PE_1", records)
PE_1_d[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
PE_1_d<-PE_1_d[order(year_group,maternal_age)]
rm(records,total)

pregnancy_d3_gdm_pe[,include:=NULL]

#PE_2
PE_2<-algorithm_template[NEW_STUDY_VARIABLES=="PE_2"]
#excl_col<-PE_2[TYPE=="AND_NOT",STUDY_VARIABLES]
inc_col<-PE_2[TYPE=="AND",STUDY_VARIABLES]
#alt_col<-PE_2[TYPE=="OR",STUDY_VARIABLES]

#if(length(excl_col)>0){pregnancy_d3_gdm_pe[pregnancy_d3_gdm_pe[,Reduce("|" , lapply(.SD,`>=`, 1)),.SDcols=excl_col],exclude:=1]}else{pregnancy_d3_gdm_pe[,exclude:=NA]}
if(length(inc_col)>0){pregnancy_d3_gdm_pe[pregnancy_d3_gdm_pe[,Reduce("|" , lapply(.SD,`>=`, 2)),.SDcols=inc_col],include:=1]}else{pregnancy_d3_gdm_pe[,include:=NA]}
#if(length(alt_col)>0){pregnancy_d3_gdm_pe[pregnancy_d3_gdm_pe[,Reduce("|" , lapply(.SD,`>=`, 1)),.SDcols=alt_col],alternative:=1]}else{pregnancy_d3_gdm_pe[,alternative:=1]}

#export PE_3
PE_2_dt<-data.table(algorithm="PE_2", no_diagnosed_pregnancies=pregnancy_d3_gdm_pe[include>=2 & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_gdm_pe[!duplicated(pregnancy_id),.N])

pregnancy_d3_gdm_pe[,include:=NULL]

#PE_3
PE_3<-algorithm_template[NEW_STUDY_VARIABLES=="PE_3"]
excl_col<-PE_3[TYPE=="AND_NOT",STUDY_VARIABLES]
inc_col<-PE_3[TYPE=="AND",STUDY_VARIABLES]
alt_col<-PE_3[TYPE=="OR",STUDY_VARIABLES]

if(length(excl_col)>0){pregnancy_d3_gdm_pe[pregnancy_d3_gdm_pe[,Reduce("|" , lapply(.SD,`>=`, 1)),.SDcols=excl_col],exclude:=1]}else{pregnancy_d3_gdm_pe[,exclude:=NA]}
  if(length(inc_col)>0){pregnancy_d3_gdm_pe[pregnancy_d3_gdm_pe[,Reduce("|" , lapply(.SD,`>=`, 1)),.SDcols=inc_col],include:=1]}else{pregnancy_d3_gdm_pe[,include:=NA]}
    if(length(alt_col)>0){pregnancy_d3_gdm_pe[pregnancy_d3_gdm_pe[,Reduce("|" , lapply(.SD,`>=`, 1)),.SDcols=alt_col],alternative:=1]}else{pregnancy_d3_gdm_pe[,alternative:=1]}

#export PE_3
PE_3_dt<-data.table(algorithm="PE_3", no_diagnosed_pregnancies=pregnancy_d3_gdm_pe[include>=1 & alternative>=1 & is.na(exclude) & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_gdm_pe[!duplicated(pregnancy_id),.N])

pregnancy_d3_gdm_pe[,PE_3:=NULL][,include:=NULL][,exclude:=NULL][,alternative:=NULL]


#PE_5
PE_5<-algorithm_template[NEW_STUDY_VARIABLES=="PE_5"]
#excl_col<-PE_5[TYPE=="AND_NOT",STUDY_VARIABLES]
inc_col<-PE_5[TYPE=="AND",STUDY_VARIABLES]
#alt_col<-PE_5[TYPE=="OR",STUDY_VARIABLES]

#if(length(excl_col)>0){pregnancy_d3_gdm_pe[pregnancy_d3_gdm_pe[,Reduce("|" , lapply(.SD,`>=`, 1)),.SDcols=excl_col],exclude:=1]}else{pregnancy_d3_gdm_pe[,exclude:=NA]}
if(length(inc_col)>0){pregnancy_d3_gdm_pe[pregnancy_d3_gdm_pe[,Reduce("|" , lapply(.SD,`>=`, 1)),.SDcols=inc_col],include:=1]}else{pregnancy_d3_gdm_pe[,include:=NA]}
#if(length(alt_col)>0){pregnancy_d3_gdm_pe[pregnancy_d3_gdm_pe[,Reduce("|" , lapply(.SD,`>=`, 1)),.SDcols=alt_col],alternative:=1]}else{pregnancy_d3_gdm_pe[,alternative:=1]}

#export PE_3
PE_5_dt<-data.table(algorithm="PE_5", no_diagnosed_pregnancies=pregnancy_d3_gdm_pe[include>=1 & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_gdm_pe[!duplicated(pregnancy_id),.N])

pregnancy_d3_gdm_pe[,include:=NULL]
