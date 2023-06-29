initial_time<-Sys.time()
date_running_start<-Sys.Date()

orig_mig<-pregnancy_d3_mig[,.N]
####Migraine DIAGNOSES FILES####
print("Loading all Migraine diagnoses D3 and merge with the pregnancy D3.")
obs_period_diag<-data.table(StudyVar=c("MG","MG_NO_AURA","MG_AURA","MG_STATUS","OTHER_MG","UNSP_MG","COMP_MG"),
                       lookback=c(365.25,365.25,365.25,365.25,365.25,365.25,365.25),
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

#Add onset diagnoses and mendication variable to the pregnancy d3
pregnancy_d3_mig[,onset_diag:=0][,onset_med:=0]
####Exclude all onset diagnoses of migraine####
#Create an exclude variable that will be used to exclude pregnancies with onset migraine diagnoses or triptan prescription in pregnancy
#Check only MG diagnoses files as it inlcuded all children codes
files_mg<-mig_files[str_detect(mig_files,"MG.rds")]
names_mg<-names_events[names_events %in% "MG"]
    mig_dt<-readRDS(paste0(projectFolder,"/g_intermediate/migraine_algorithm/", files_mg))
    #merge with the pregnancy d3
    mig_dt<-merge.data.table(pregnancy_d3_mig, mig_dt, by="person_id", all.x=T, allow.cartesian = T)
    mig_dt<-mig_dt[!is.na(event_date)]
    if(mig_dt[,.N]>0){
      mig_dt[,event_date:=as.IDate(event_date)]
      #remove all records 1 year before start of pregnancy
      if(obs_period_diag[StudyVar==names_mg,lookback]>0){
        mig_dt[,lookback:=obs_period_diag[StudyVar==names_mg,lookback]]
        mig_dt[,start_preg:=as.IDate(pregnancy_start_date-lookback)]
        #exclude all events that are outside observation period of interest
        mig_dt[,diff:=event_date-start_preg]
        #remove all records with date before start date pregnancy+lookback
        mig_dt<-mig_dt[diff>=0]
        mig_dt[,diff:=NULL][,lookback:=NULL][,start_preg:=NULL]
      }else{
        #remove all records before start obs
        mig_dt[,start:=obs_period_diag[StudyVar==names_mg,start_date]]
        mig_dt[,start_preg:=as.IDate(pregnancy_start_date+start)]
        mig_dt[,diff:=event_date-start_preg]
        mig_dt<-mig_dt[diff>=0]
        mig_dt[,diff:=NULL][,start:=NULL][,start_preg:=NULL]
      }
      if(mig_dt[,.N]>0){
        if(obs_period_diag[StudyVar==names_mg,after]>0){
          mig_dt[,after:=obs_period_diag[StudyVar==names_mg,after]]
          mig_dt[,end_preg:=as.IDate(pregnancy_end_date+after)]
          mig_dt[,diff:=event_date-end_preg]
          mig_dt<-mig_dt[diff<=0]
          mig_dt[,diff:=NULL][,after:=NULL][,end_preg:=NULL]
        }else{
          mig_dt[,end:=obs_period_diag[StudyVar==names_mg,end_date]]
          mig_dt[,end_preg:=as.IDate(pregnancy_start_date+end)]
          mig_dt[,diff:=event_date-end_preg]
          mig_dt<-mig_dt[diff<=0]
          mig_dt[,diff:=NULL][,end:=NULL][,end_preg:=NULL]
        }
      }
      
      if(mig_dt[,.N]>0){
        #Check for diagnoses before start date of pregnancy(12 months before start date)
        mig_dt[,min_date_diag:=pregnancy_start_date-365.25]
        mig_dt[event_date>=min_date_diag & event_date<pregnancy_start_date,prior_diag:=1]
        #Check for diagnoses in pregnancy
        mig_dt[event_date>=pregnancy_start_date & event_date<=pregnancy_end_date,preg_diag:=1]
        #add preg id
        mig_dt[,preg_diag:=as.numeric(preg_diag)][,prior_diag:=as.numeric(prior_diag)]
        prior_diag<-mig_diag[prior_diag==1,.N, by=c("person_id","pregnancy_id")]
        setnames(prior_diag,"N","prior_diag")
        preg_diag<-mig_diag[preg_diag==1,.N, by=c("person_id","pregnancy_id")]
        setnames(preg_diag,"N","preg_diag")
        rm(mig_diag)
        prior_diag<-merge.data.table(prior_diag,preg_diag, all=T, by=c("person_id","pregnancy_id"))
        prior_diag<-prior_diag[is.na(prior_diag) & preg_diag==1]
        #Count the number of cases to exclude
        excluded_prior_diag<-data.table(event_definition=names_mg, excluded_records=prior_diag[,.N])
        prior_diag[,prior_diag:=NULL]
        #Add the identifier pregnancy id to the main pregnancy D3
        pregnancy_d3_mig<-merge.data.table(pregnancy_d3_mig,prior_diag, by=c("person_id","pregnancy_id"), all.x=T)
        #update the exclude variable
        pregnancy_d3_mig[preg_diag==1, onset_diag:=1]
        pregnancy_d3_mig[,preg_diag:=NULL]
        pregnancy_d3_mig[is.na(onset_diag), onset_diag:=0]
      }
    }
    rm(mig_dt) 


####Migraine MEDICINES files####
  print("Loading all Migraine medicines D3 and merge with the pregnancy D3.")
  
  obs_period_med<-data.table(StudyVar=c("Migraine_medicines"),
                         lookback=c(365.25),
                         start_date=c(0),
                         end_date=c(40*7),
                         after=c(0))
  
####Exclude all onset prescriptions of migraine#### 
  mig_med_fl<-list.files(paste0(projectFolder,"/g_intermediate/migraine_algorithm/"),"Migraine_medicines")
  if(length(mig_med_fl)>0){
  mig_med<-readRDS(paste0(projectFolder,"/g_intermediate/migraine_algorithm/",mig_med_fl))
 #Keep only records related to triptans(N02CC)
  mig_med[,truncated_atc:=substr(atc_code,1,5)]
  mig_med<-mig_med[truncated_atc == "N02CC"]
  mig_med[,truncated_atc:=NULL]
  
  mig_med<-merge.data.table(pregnancy_d3_mig, mig_med, by="person_id", all.x=T, allow.cartesian = T)
  mig_med<-mig_med[!is.na(medicine_date)]
  mig_med[,pregnancy_start_date:=as.IDate(pregnancy_start_date)][,pregnancy_end_date:=as.IDate(pregnancy_end_date)][,birth_date:=as.IDate(birth_date)][,death_date:=as.IDate(death_date)][,op_start_date_gdm_pe:=as.IDate(op_start_date_gdm_pe)][,op_end_date_gdm_pe:=as.IDate(op_end_date_gdm_pe)][,medicine_date:=as.IDate(medicine_date)]
  if(mig_med[,.N]>0){
    #remove all records before lookback
  if(obs_period_med[med_fl,lookback]>0){
    mig_med[,lookback:=obs_period_med[med_fl,lookback]]
    mig_med[,start_preg:=as.IDate(pregnancy_start_date-lookback)]
    #exclude all records that are outside observation period of interest
    mig_med[,diff:=medicine_date-start_preg]
    #remove all records with date before start date pregnancy+lookback
    mig_med<-mig_med[diff>=0]
    mig_med[,diff:=NULL][,lookback:=NULL][,start_preg:=NULL]
  }else{
    #remove all records before start obs
    mig_med[,start:=obs_period_med[med_fl,start_date]]
    mig_med[,start_preg:=as.IDate(pregnancy_start_date+start)]
    mig_med[,diff:=medicine_date-start_preg]
    mig_med<-mig_med[diff>=0]
    mig_med[,diff:=NULL][,start:=NULL][,start_preg:=NULL]
  }
  
  if(mig_med[,.N]>0){
    #remove all records after end of pregnancy
    if(obs_period_med[med_fl,after]>0){
      mig_med[,after:=obs_period_med[med_fl,after]]
      mig_med[,end_preg:=as.IDate(pregnancy_end_date+after)]
      mig_med[,diff:=medicine_date-end_preg]
      mig_med<-mig_med[diff<=0]
      mig_med[,diff:=NULL][,after:=NULL][,end_preg:=NULL]
    }else{
      mig_med[,end:=obs_period_med[med_fl,end_date]]
      mig_med[,end_preg:=as.IDate(pregnancy_start_date+end)]
      mig_med[,diff:=medicine_date-end_preg]
      mig_med<-mig_med[diff<=0]
      mig_med[,diff:=NULL][,end:=NULL][,end_preg:=NULL]
    }
  }
  
  if(mig_med[,.N]>0){
    #Check for prescriptions before start date of pregnancy(12 months before start date)
    mig_med[,min_date_med:=pregnancy_start_date-365.25]
    mig_med[medicine_date>=min_date_med & medicine_date<pregnancy_start_date,prior_med:=1]
    #Check for prescriptions in pregnancy
    mig_med[medicine_date>=pregnancy_start_date & medicine_date<=pregnancy_end_date,preg_med:=1]
    #add preg id
  mig_med[,preg_med:=as.numeric(preg_med)]
  prior_med<-mig_med[prior_med==1,.N, by=c("person_id","pregnancy_id")]
  setnames(prior_med,"N","prior_med")
  preg_med<-mig_med[preg_med==1,.N, by=c("person_id","pregnancy_id")]
  setnames(preg_med,"N","preg_med")
  rm(mig_med)
  prior_med<-merge.data.table(prior_med,preg_med, all=T, by=c("person_id","pregnancy_id"))
  prior_med<-prior_med[is.na(prior_med) & preg_med==1]
  
    #Count the number of cases to exclude
    excluded_prior_med<-data.table(event_definition="Triptan_prescription(N02CC)", excluded_records=prior_med[,.N])
    prior_med[,prior_med:=NULL]
    #Add the identifier pregnancy id to the main pregnancy D3
    pregnancy_d3_mig<-merge.data.table(pregnancy_d3_mig,prior_med, by=c("person_id","pregnancy_id"), all.x=T)
    #update the exclude variable
    pregnancy_d3_mig[preg_med==1, onset_med:=1]
    pregnancy_d3_mig[,preg_med:=NULL]
    pregnancy_d3_mig[is.na(onset_med), onset_med:=0]
    
  }
  }
  }else{print("There are no migraine medicines records.")}
  
####Export excluded number of pregnancies####
#number of pregnancies with onset diagnoses
onset_diag<-pregnancy_d3_mig[onset_diag==1 & is.na(onset_med),.N]
#number of pregnancies with onset prescription
onset_med<-pregnancy_d3_mig[is.na(onset_diag) & onset_med==1,.N]
#number of pregnancies with onset of diag and pres
onset_both<-pregnancy_d3_mig[onset_diag==1 & onset_med==1,.N]
#excluded preg
excluded_preg<-pregnancy_d3_mig[onset_diag==1 | onset_med==1,.N]

pregnancy_d3_mig<-pregnancy_d3_mig[onset_diag==0 & onset_med==0]
included_preg<-pregnancy_d3_mig[,.N]
pregnancy_d3_mig[,onset_diag:=NULL][,onset_med:=NULL]

excluded_preg_flowchart<-data.table(Indicator=c("1.0. Number of pregnancies before exclusions",
                                                "1.1. Number of pregnancies with onset diagnoses of migraine only",
                                                "1.2. Number of pregnancies with onset prescription of triptans only",
                                                "1.3. Number of pregnancies with onset diagnoses of migraine and prescription of triptans",
                                                "1.4. Number of excluded pregnancies",
                                                "1.5. Number of included pregnancies"),
                                    Count=c(orig_mig,
                                            onset_diag,
                                            onset_med,
                                            onset_both,
                                            excluded_preg,
                                            included_preg))
rm(orig_mig,onset_diag,onset_med,onset_both,excluded_preg,included_preg)
fwrite(excluded_preg_flowchart, paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_excluded_pregnancies_onset_diag_med.csv"), row.names = F)
rm(excluded_preg_flowchart)
#delete the first version of the pregnancy D3 and save this one
file.remove(paste0(projectFolder,"/g_intermediate/pregnancy_d3/MIG_Pregnancy_D3.rds"))
saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/pregnancy_d3/MIG_Pregnancy_D3.rds"))
####Migraine DIAGNOSES####
  original<-list()
  before<-list()
  after<-list()
  sum<-list()
  
  dir.create(paste0(projectFolder,"/g_intermediate/migraine_algorithm/final_d3"))
  
w<-1
for(mig_fl in 1:length(mig_files)){
  mig_dt<-readRDS(paste0(projectFolder,"/g_intermediate/migraine_algorithm/", mig_files[mig_fl]))
  #merge with the pregnancy d3
  mig_dt<-merge.data.table(pregnancy_d3_mig, mig_dt, by="person_id", all.x=T, allow.cartesian = T)
  mig_dt<-mig_dt[!is.na(event_date)]
  if(mig_dt[,.N]>0){
    original[[w]]<-data.table(StudyVar=names_events[mig_fl], event_records=mig_dt[,.N])
    mig_dt[,event_date:=as.IDate(event_date)]
    
    if(obs_period_med[StudyVar==names_events[mig_fl],lookback]>0){
      mig_dt[,lookback:=obs_period_med[StudyVar==names_events[mig_fl],lookback]]
      mig_dt[,start_preg:=as.IDate(pregnancy_start_date-lookback)]
      #exclude all events that are outside observation period of interest
      mig_dt[,diff:=event_date-start_preg]
      #remove all records with date before start date pregnancy+lookback
      before[[w]]<-data.table(StudyVar=names_events[mig_fl], before_start=mig_dt[diff<0,.N])
      mig_dt<-mig_dt[diff>=0]
      mig_dt[,diff:=NULL][,lookback:=NULL][,start_preg:=NULL]
    }else{
      #remove all records before start obs
      mig_dt[,start:=obs_period_med[StudyVar==names_events[mig_fl],start_date]]
      mig_dt[,start_preg:=as.IDate(pregnancy_start_date+start)]
      mig_dt[,diff:=event_date-start_preg]
      before[[w]]<-data.table(StudyVar=names_events[mig_fl], before_start=mig_dt[diff<0,.N])
      mig_dt<-mig_dt[diff>=0]
      mig_dt[,diff:=NULL][,start:=NULL][,start_preg:=NULL]
    }
    
    if(mig_dt[,.N]>0){
      if(obs_period_med[StudyVar==names_events[mig_fl],after]>0){
        mig_dt[,after:=obs_period_med[StudyVar==names_events[mig_fl],after]]
        mig_dt[,end_preg:=as.IDate(pregnancy_end_date+after)]
        mig_dt[,diff:=event_date-end_preg]
        after[[w]]<-data.table(StudyVar=names_events[mig_fl], after_end=mig_dt[diff>0,.N])
        mig_dt<-mig_dt[diff<=0]
        mig_dt[,diff:=NULL][,after:=NULL][,end_preg:=NULL]
      }else{
        mig_dt[,end:=obs_period_med[StudyVar==names_events[mig_fl],end_date]]
        mig_dt[,end_preg:=as.IDate(pregnancy_start_date+end)]
        mig_dt[,diff:=event_date-end_preg]
        after[[w]]<-data.table(StudyVar=names_events[mig_fl], after_end=mig_dt[diff>0,.N])
        mig_dt<-mig_dt[diff<=0]
        mig_dt[,diff:=NULL][,end:=NULL][,end_preg:=NULL]
      }
    }else{
      after[[w]]<-data.table(StudyVar=names_events[mig_fl], after_end=0)
    }
    
    if(mig_dt[,.N]>0){
      #create a summary of included records
      sum[[w]]<-data.table(StudyVar=names_events[mig_fl], no_records=mig_dt[!is.na(event_date),.N], no_pregnancies=mig_dt[!duplicated(pregnancy_id),.N])
      fwrite(mig_dt, paste0(projectFolder,"/g_intermediate/migraine_algorithm/final_d3/", names_events[mig_fl],"_pregnancy_D3.rds"),row.names = F)
      cols<-c("person_id","pregnancy_id","pregnancy_start_date","pregnancy_end_date")
      mig_dt<-mig_dt[,cols,with=F]
      mig_dt[,names_events[mig_fl]:=1]
      mig_dt<-mig_dt[,lapply(.SD, sum),.SDcols = names_events[mig_fl], by=c("person_id","pregnancy_id","pregnancy_start_date","pregnancy_end_date")]
      pregnancy_d3_mig<-merge.data.table(pregnancy_d3_mig,mig_dt,all.x=T, by=cols)
      pregnancy_d3_mig[is.na(get(names_events[mig_fl])),names_events[mig_fl]:=0]
    }else{
      pregnancy_d3_mig[,names_events[mig_fl]:=0]
    }
  }else{
    pregnancy_d3_mig[,names_events[mig_fl]:=0]
  }
  
  rm(mig_dt) 
  w<-w+1
}

#Combine files
original<-as.data.table(do.call(rbind,original))
before<-as.data.table(do.call(rbind,before))
after<-as.data.table(do.call(rbind,after))
if(sum(length(original),length(before))>0){
removed_rec<-merge.data.table(original,before,by="StudyVar",all=T)
}else{
  removed_rec<-NULL  
}
if(sum(length(removed_rec),length(after))>0){
removed_rec<-merge.data.table(removed_rec,after,by="StudyVar",all=T)
removed_rec[is.na(before_start),before_start:="N/A"]
removed_rec[is.na(after_end),after_end:="N/A"]
setnames(removed_rec,"event_records","original_records")
}else{
  removed_rec<-NULL    
}
#Pregnancies that had the event of interest not in the timeframe of study were excluded, this is why the number of pregnancies is different between different events
sum_mig<-as.data.table(do.call(rbind,sum))
rm(original,before,after,sum)
rm(mig_files)

#identify events that are not present from conditions_migraine
not_present<-setdiff(names(conditions_migraine), names_events)
pregnancy_d3_mig[,eval(not_present):=list(0)]
####Migraine MEDICINES####
print("Loading all Migraine medicines D3 and merge with the pregnancy D3.")

# #obs_period<-data.table(StudyVar=c("GD_med","PRE_GD_med"),
#                        lookback=c(0,6*30.25),
#                        start_date=c(98,0),
#                        end_date=c(7*40,97),
#                        after=c(7,0))


mig_med_fl<-list.files(paste0(projectFolder,"/g_intermediate/migraine_algorithm/"),"Migraine_medicines")
mig_med<-readRDS(paste0(projectFolder,"/g_intermediate/migraine_algorithm/",mig_med_fl))

mig_med<-merge.data.table(pregnancy_d3_mig, mig_med, by="person_id", all.x=T, allow.cartesian = T)
mig_med<-mig_med[!is.na(medicine_date)]
mig_med[,pregnancy_start_date:=as.IDate(pregnancy_start_date)][,pregnancy_end_date:=as.IDate(pregnancy_end_date)][,birth_date:=as.IDate(birth_date)][,death_date:=as.IDate(death_date)][,op_start_date_gdm_pe:=as.IDate(op_start_date_gdm_pe)][,op_end_date_gdm_pe:=as.IDate(op_end_date_gdm_pe)][,medicine_date:=as.IDate(medicine_date)]

original<-data.table(StudyVar=c("Migraine_medicines"), original_records=as.character(mig_med[,.N]))
before<-list()
after<-list()
sum<-list()
w<-1
for(med_fl in 1:length(obs_period_med[,.N])){
if(mig_med[,.N]>0){
  if(obs_period[med_fl,lookback]>0){
    mig_med[,lookback:=obs_period[med_fl,lookback]]
    mig_med[,start_preg:=as.IDate(pregnancy_start_date-lookback)]
    #exclude all pregnancies that are outside observation period of interest
    mig_med[,diff:=medicine_date-start_preg]
    #remove all records with date before start date pregnancy+lookback
    before[[w]]<-data.table(StudyVar=obs_period[med_fl,StudyVar], before_start=as.character(mig_med[diff<0,.N]))
    mig_med<-mig_med[diff>=0]
    mig_med[,diff:=NULL][,lookback:=NULL][,start_preg:=NULL]
  }else{
    #remove all records before start obs
    mig_med[,start:=obs_period[med_fl,start_date]]
    mig_med[,start_preg:=as.IDate(pregnancy_start_date+start)]
    mig_med[,diff:=medicine_date-start_preg]
    before[[w]]<-data.table(StudyVar=obs_period[med_fl,StudyVar], before_start=as.character(mig_med[diff<0,.N]))
    mig_med<-mig_med[diff>=0]
    mig_med[,diff:=NULL][,start:=NULL][,start_preg:=NULL]
  }
  
  if(mig_med[,.N]>0){
    if(obs_period[med_fl,after]>0){
      mig_med[,after:=obs_period[med_fl,after]]
      mig_med[,end_preg:=as.IDate(pregnancy_end_date+after)]
      mig_med[,diff:=medicine_date-end_preg]
      after[[w]]<-data.table(StudyVar=obs_period[med_fl,StudyVar], after_end=as.character(mig_med[diff>0,.N]))
      mig_med<-mig_med[diff<=0]
      mig_med[,diff:=NULL][,after:=NULL][,end_preg:=NULL]
    }else{
      mig_med[,end:=obs_period[med_fl,end_date]]
      mig_med[,end_preg:=as.IDate(pregnancy_start_date+end)]
      mig_med[,diff:=medicine_date-end_preg]
      after[[w]]<-data.table(StudyVar=obs_period[med_fl,StudyVar], after_end=as.character(mig_med[diff>0,.N]))
      mig_med<-mig_med[diff<=0]
      mig_med[,diff:=NULL][,end:=NULL][,end_preg:=NULL]
    }
  }else{
    after[[w]]<-data.table(StudyVar=names_events[mig_fl], after_end=as.character(0))
  }
  
  if(mig_med[,.N]>0){
    #create a summary of included records
    sum[[w]]<-data.table(StudyVar=obs_period[med_fl,StudyVar], no_records=mig_med[!is.na(medicine_date) & !duplicated(pregnancy_id),.N], no_pregnancies=mig_med[!duplicated(pregnancy_id),.N])
    saveRDS(mig_med, paste0(projectFolder,"/g_intermediate/migraine_algorithm/final_d3/", obs_period[med_fl,StudyVar],"_pregnancy_D3.rds"))
    cols<-c("person_id","pregnancy_id","pregnancy_start_date","pregnancy_end_date")
    mig_med<-mig_med[,cols,with=F]
    mig_med[,obs_period[med_fl, StudyVar]:=1]
    mig_med<-mig_med[,lapply(.SD,sum),by=cols,.SDcols=obs_period[med_fl, StudyVar]]
    pregnancy_d3_mig<-merge.data.table(pregnancy_d3_mig,mig_med,all.x=T, by=cols)
    pregnancy_d3_mig[is.na(get(obs_period[med_fl,StudyVar])),obs_period[med_fl,StudyVar]:=0]
  }else{
    pregnancy_d3_mig[,obs_period[med_fl,StudyVar]:=0]
  }
}else{
  pregnancy_d3_mig[,obs_period[med_fl,StudyVar]:=0]
}
}
rm(mig_med) 

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
rm(mig_med_fl)

#identify events that are not present from conditions_migraine
not_present<-setdiff(obs_period[,StudyVar], names(pregnancy_d3_mig))
pregnancy_d3_mig[,eval(not_present):=list(0)]
rm(obs_period)

print("Export GDM pregnancy D3")
fwrite(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm/final_d3/pregnancy_d3_migraine_algorithm.csv"),row.names = F)

####APPLY GDM ALGORITHM####
algorithm_template<-fread(paste0(projectFolder, "/p_steps/parameters/algorithms.csv"))
####GDM_1####
print("Create algorithm GDM_1.")
GDM_1<-algorithm_template[NEW_STUDY_VARIABLES=="GDM_1"]
excl_col<-GDM_1[TYPE=="AND_NOT",STUDY_VARIABLES]
inc_col<-GDM_1[TYPE=="AND",STUDY_VARIABLES]

pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("|" , lapply(.SD,`>=`, 1)),.SDcols=excl_col],exclude:=1]
pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col],include:=1]

fwrite(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm/final_d3/GDM_1_D3.rds"),row.names = F)

#export GDM_1
GDM_1_dt<-data.table(algorithm="GDM_1", no_diagnosed_pregnancies=pregnancy_d3_mig[include==1 & is.na(exclude) & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id),.N])

pregnancy_d3_mig[,maternal_age:=as.character(maternal_age)]
records<-pregnancy_d3_mig[include==1 & is.na(exclude) & !duplicated(pregnancy_id), .N, by=.(maternal_age)]
names(records)<-c("maternal_age","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id), by="maternal_age",.N]
names(total)<-c("maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by="maternal_age",all=T)
GDM_1_b<-data.table(algorithm="GDM_1", records)
GDM_1_b[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
GDM_1_b<-GDM_1_b[order(maternal_age)]
rm(records,total)

records<-pregnancy_d3_mig[include==1 & is.na(exclude) & !duplicated(pregnancy_id),by="year", .N]
names(records)<-c("year","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by="year",.N]
names(total)<-c("year","no_pregnancies")
records<-merge.data.table(records,total,by="year",all=T)
GDM_1_c<-data.table(algorithm="GDM_1", records)
GDM_1_c[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
GDM_1_c<-GDM_1_c[order(year)]
rm(records,total)

records<-pregnancy_d3_mig[include==1 & is.na(exclude) & !duplicated(pregnancy_id),by=c("year_group","maternal_age"), .N]
names(records)<-c("year_group","maternal_age", "no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by=c("year_group","maternal_age"),.N]
names(total)<-c("year_group","maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","maternal_age"), all=T)
GDM_1_d<-data.table(algorithm="GDM_1", records)
GDM_1_d[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
GDM_1_d<-GDM_1_d[order(year_group,maternal_age)]
rm(records,total)
pregnancy_d3_mig[,include:=NULL][,exclude:=NULL]

####GDM_2####
print("Create algorithm GDM_2.")
GDM_2<-algorithm_template[NEW_STUDY_VARIABLES=="GDM_2"]
excl_col<-GDM_2[TYPE=="AND_NOT",STUDY_VARIABLES]
#inc_col<-GDM_2[TYPE=="AND",STUDY_VARIABLES]
alt_col<-GDM_2[TYPE=="OR",STUDY_VARIABLES]

if(length(alt_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("|" , lapply(.SD,`>=`, 1)),.SDcols=alt_col],alternative:=1]}else{pregnancy_d3_mig[,alternative:=NA]}
if(length(excl_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("|" , lapply(.SD,`>=`, 1)),.SDcols=excl_col],exclude:=1]}else{pregnancy_d3_mig[,exclude:=NA]}

fwrite(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm/final_d3/GDM_2_D3.rds"),row.names = F)

#export GDM_2
GDM_2_dt<-data.table(algorithm="GDM_2", no_diagnosed_pregnancies=pregnancy_d3_mig[alternative==1 & is.na(exclude) & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id),.N])

pregnancy_d3_mig[,maternal_age:=as.character(maternal_age)]
records<-pregnancy_d3_mig[alternative==1 & is.na(exclude) & !duplicated(pregnancy_id), .N, by=.(maternal_age)]
names(records)<-c("maternal_age","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id), by="maternal_age",.N]
names(total)<-c("maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by="maternal_age",all=T)
GDM_2_b<-data.table(algorithm="GDM_2", records)
GDM_2_b[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
GDM_2_b<-GDM_2_b[order(maternal_age)]
rm(records,total)

records<-pregnancy_d3_mig[alternative==1 & is.na(exclude) & !duplicated(pregnancy_id),by="year", .N]
names(records)<-c("year","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by="year",.N]
names(total)<-c("year","no_pregnancies")
records<-merge.data.table(records,total,by="year",all=T)
GDM_2_c<-data.table(algorithm="GDM_2", records)
GDM_2_c[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
GDM_2_c<-GDM_2_c[order(year)]
rm(records,total)

records<-pregnancy_d3_mig[alternative==1 & is.na(exclude) & !duplicated(pregnancy_id),by=c("year_group","maternal_age"), .N]
names(records)<-c("year_group","maternal_age", "no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by=c("year_group","maternal_age"),.N]
names(total)<-c("year_group","maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","maternal_age"), all=T)
GDM_2_d<-data.table(algorithm="GDM_2", records)
GDM_2_d[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
GDM_2_d<-GDM_2_d[order(year_group,maternal_age)]
rm(records,total)
pregnancy_d3_mig[,alternative:=NULL][,exclude:=NULL]

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

if(length(alt_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("|" , lapply(.SD,rule, value)),.SDcols=alt_col],alternative:=1]}else{pregnancy_d3_mig[,alternative:=NA]}
if(length(excl_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("|" , lapply(.SD,`>=`, 1)),.SDcols=excl_col],exclude:=1]}else{pregnancy_d3_mig[,exclude:=NA]}

fwrite(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm/final_d3/GDM_3_D3.rds"),row.names = F)

#export GDM_3
GDM_3_dt<-data.table(algorithm="GDM_3", no_diagnosed_pregnancies=pregnancy_d3_mig[alternative==1 & is.na(exclude) & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id),.N])

pregnancy_d3_mig[,maternal_age:=as.character(maternal_age)]
records<-pregnancy_d3_mig[alternative==1 & is.na(exclude) & !duplicated(pregnancy_id), .N, by=.(maternal_age)]
names(records)<-c("maternal_age","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id), by="maternal_age",.N]
names(total)<-c("maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by="maternal_age",all=T)
GDM_3_b<-data.table(algorithm="GDM_3", records)
GDM_3_b[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
GDM_3_b<-GDM_3_b[order(maternal_age)]
rm(records,total)

records<-pregnancy_d3_mig[alternative==1 & is.na(exclude) & !duplicated(pregnancy_id),by="year", .N]
names(records)<-c("year","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by="year",.N]
names(total)<-c("year","no_pregnancies")
records<-merge.data.table(records,total,by="year",all=T)
GDM_3_c<-data.table(algorithm="GDM_3", records)
GDM_3_c[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
GDM_3_c<-GDM_3_c[order(year)]
rm(records,total)

records<-pregnancy_d3_mig[alternative==1 & is.na(exclude) & !duplicated(pregnancy_id),by=c("year_group","maternal_age"), .N]
names(records)<-c("year_group","maternal_age", "no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by=c("year_group","maternal_age"),.N]
names(total)<-c("year_group","maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","maternal_age"), all=T)
GDM_3_d<-data.table(algorithm="GDM_3", records)
GDM_3_d[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
GDM_3_d<-GDM_3_d[order(year_group,maternal_age)]
rm(records,total)
pregnancy_d3_mig[,alternative:=NULL][,exclude:=NULL]

####GDM_4####
print("Create algorithm GDM_4.")
GDM_4<-algorithm_template[NEW_STUDY_VARIABLES=="GDM_4"]
excl_col<-GDM_4[TYPE=="AND_NOT",STUDY_VARIABLES]
#inc_col<-GDM_2[TYPE=="AND",STUDY_VARIABLES]
alt_col<-GDM_4[TYPE=="OR",STUDY_VARIABLES]

if(length(alt_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("|" , lapply(.SD,`>=`, 1)),.SDcols=alt_col],alternative:=1]}else{pregnancy_d3_mig[,alternative:=NA]}
if(length(excl_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("|" , lapply(.SD,`>=`, 1)),.SDcols=excl_col],exclude:=1]}else{pregnancy_d3_mig[,exclude:=NA]}

fwrite(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm/final_d3/GDM_4_D3.rds"),row.names = F)

#export GDM_3
GDM_4_dt<-data.table(algorithm="GDM_4", no_diagnosed_pregnancies=pregnancy_d3_mig[alternative==1 & is.na(exclude) & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id),.N])

pregnancy_d3_mig[,maternal_age:=as.character(maternal_age)]
records<-pregnancy_d3_mig[alternative==1 & is.na(exclude) & !duplicated(pregnancy_id), .N, by=.(maternal_age)]
names(records)<-c("maternal_age","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id), by="maternal_age",.N]
names(total)<-c("maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by="maternal_age",all=T)
GDM_4_b<-data.table(algorithm="GDM_4", records)
GDM_4_b[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
GDM_4_b<-GDM_4_b[order(maternal_age)]
rm(records,total)

records<-pregnancy_d3_mig[alternative==1 & is.na(exclude) & !duplicated(pregnancy_id),by="year", .N]
names(records)<-c("year","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by="year",.N]
names(total)<-c("year","no_pregnancies")
records<-merge.data.table(records,total,by="year",all=T)
GDM_4_c<-data.table(algorithm="GDM_4", records)
GDM_4_c[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
GDM_4_c<-GDM_4_c[order(year)]
rm(records,total)

records<-pregnancy_d3_mig[alternative==1 & is.na(exclude) & !duplicated(pregnancy_id),by=c("year_group","maternal_age"), .N]
names(records)<-c("year_group","maternal_age", "no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by=c("year_group","maternal_age"),.N]
names(total)<-c("year_group","maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","maternal_age"), all=T)
GDM_4_d<-data.table(algorithm="GDM_4", records)
GDM_4_d[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
GDM_4_d<-GDM_4_d[order(year_group,maternal_age)]
rm(records,total)
pregnancy_d3_mig[,alternative:=NULL][,exclude:=NULL]

####GDM_5####
print("Create algorithm GDM_5.")
GDM_5<-algorithm_template[NEW_STUDY_VARIABLES=="GDM_5"]
excl_col<-GDM_5[TYPE=="AND_NOT",STUDY_VARIABLES]
#inc_col<-GDM_2[TYPE=="AND",STUDY_VARIABLES]
alt_col<-GDM_5[TYPE=="OR",STUDY_VARIABLES]

if(length(alt_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("|" , lapply(.SD,`>=`, 1)),.SDcols=alt_col],alternative:=1]}else{pregnancy_d3_mig[,alternative:=NA]}
if(length(excl_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("|" , lapply(.SD,`>=`, 1)),.SDcols=excl_col],exclude:=1]}else{pregnancy_d3_mig[,exclude:=NA]}

fwrite(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm/final_d3/GDM_5_D3.rds"),row.names = F)

#export GDM_3
GDM_5_dt<-data.table(algorithm="GDM_5", no_diagnosed_pregnancies=pregnancy_d3_mig[alternative==1 & is.na(exclude) & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id),.N])

pregnancy_d3_mig[,maternal_age:=as.character(maternal_age)]
records<-pregnancy_d3_mig[alternative==1 & is.na(exclude) & !duplicated(pregnancy_id), .N, by=.(maternal_age)]
names(records)<-c("maternal_age","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id), by="maternal_age",.N]
names(total)<-c("maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by="maternal_age",all=T)
GDM_5_b<-data.table(algorithm="GDM_5", records)
GDM_5_b[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
GDM_5_b<-GDM_5_b[order(maternal_age)]
rm(records,total)

records<-pregnancy_d3_mig[alternative==1 & is.na(exclude) & !duplicated(pregnancy_id),by="year", .N]
names(records)<-c("year","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by="year",.N]
names(total)<-c("year","no_pregnancies")
records<-merge.data.table(records,total,by="year",all=T)
GDM_5_c<-data.table(algorithm="GDM_5", records)
GDM_5_c[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
GDM_5_c<-GDM_5_c[order(year)]
rm(records,total)

records<-pregnancy_d3_mig[alternative==1 & is.na(exclude) & !duplicated(pregnancy_id),by=c("year_group","maternal_age"), .N]
names(records)<-c("year_group","maternal_age", "no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by=c("year_group","maternal_age"),.N]
names(total)<-c("year_group","maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","maternal_age"), all=T)
GDM_5_d<-data.table(algorithm="GDM_5", records)
GDM_5_d[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
GDM_5_d<-GDM_5_d[order(year_group,maternal_age)]
rm(records,total)
pregnancy_d3_mig[,alternative:=NULL][,exclude:=NULL]

###GDM_8###
GDM_8<-algorithm_template[NEW_STUDY_VARIABLES=="GDM_8"]
#excl_col<-GDM_8[TYPE=="AND_NOT",STUDY_VARIABLES]
inc_col<-GDM_8[TYPE=="AND",STUDY_VARIABLES]

#pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("|" , lapply(.SD,`==`, 1)),.SDcols=excl_col],exclude:=1]
pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col],include:=1]
#export GDM_8
GDM_8_dt<-data.table(algorithm="GDM_8", no_diagnosed_pregnancies=pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id),.N])

fwrite(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm/final_d3/GDM_8_D3.rds"),row.names = F)

#export GDM_8
GDM_8_dt<-data.table(algorithm="GDM_8", no_diagnosed_pregnancies=pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id),.N])

pregnancy_d3_mig[,maternal_age:=as.character(maternal_age)]
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id), .N, by=.(maternal_age)]
names(records)<-c("maternal_age","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id), by="maternal_age",.N]
names(total)<-c("maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by="maternal_age",all=T)
GDM_8_b<-data.table(algorithm="GDM_8", records)
GDM_8_b[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
GDM_8_b<-GDM_8_b[order(maternal_age)]
rm(records,total)

records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by="year", .N]
names(records)<-c("year","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by="year",.N]
names(total)<-c("year","no_pregnancies")
records<-merge.data.table(records,total,by="year",all=T)
GDM_8_c<-data.table(algorithm="GDM_8", records)
GDM_8_c[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
GDM_8_c<-GDM_8_c[order(year)]
rm(records,total)

records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by=c("year_group","maternal_age"), .N]
names(records)<-c("year_group","maternal_age", "no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by=c("year_group","maternal_age"),.N]
names(total)<-c("year_group","maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","maternal_age"), all=T)
GDM_8_d<-data.table(algorithm="GDM_8", records)
GDM_8_d[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
GDM_8_d<-GDM_8_d[order(year_group,maternal_age)]
rm(records,total)
pregnancy_d3_mig[,include:=NULL]

if("DM" %in% names(pregnancy_d3_mig)){pregnancy_d3_mig[,DM:=NULL]}
if("GD" %in% names(pregnancy_d3_mig)){pregnancy_d3_mig[,GD:=NULL]}
if("GDM_checkbox" %in% names(pregnancy_d3_mig)){pregnancy_d3_mig[,GDM_checkbox:=NULL]}
if("PRE_GD" %in% names(pregnancy_d3_mig)){pregnancy_d3_mig[,PRE_GD:=NULL]}
if("UNK_GD" %in% names(pregnancy_d3_mig)){pregnancy_d3_mig[,UNK_GD:=NULL]}
if("DM_PREG" %in% names(pregnancy_d3_mig)){pregnancy_d3_mig[,DM_PREG:=NULL]}
if("GD_med" %in% names(pregnancy_d3_mig)){pregnancy_d3_mig[,GD_med:=NULL]}
if("PRE_GD_med" %in% names(pregnancy_d3_mig)){pregnancy_d3_mig[,PRE_GD_med:=NULL]}

date_running_end<-Sys.Date()
end_time<-Sys.time()

time_log<-data.table(DAP=data_access_provider_name,
                     Script="Step_04_c_migraine_algorithms.R", 
                     Start_date=date_running_start, 
                     End_date=date_running_end,
                     Time_elaspsed=format(end_time-initial_time, digits=2))
fwrite(time_log,paste0(output_dir,"/Time log/Step_04_c_time_log.csv"),row.names = F)


