initial_time_04_c<-Sys.time()
date_running_start_04_c<-Sys.Date()

orig_mig<-pregnancy_d3_mig[,.N]
####Migraine DIAGNOSES FILES####
print("Loading all Migraine diagnoses D3 and merge with the pregnancy D3.")
if(DAP_name %in% c("CHUT")){
  obs_period_diag<-data.table(StudyVar=c("MG","MG_NO_AURA","MG_AURA","MG_STACOMP","MG_OTHER","MG_UNSP","MG_UPC"),
                              lookback=c(3*30.25,3*30.25,3*30.25,3*30.25,3*30.25,3*30.25,3*30.25),
                              start_date=c(0,0,0,0,0,0,0),
                              end_date=c(7*40,7*40,7*40,7*40,7*40,7*40,7*40),
                              after=c(0,0,0,0,0,0,0))  
}else{
obs_period_diag<-data.table(StudyVar=c("MG","MG_NO_AURA","MG_AURA","MG_OTHER","MG_UNSP","MG_STACOMP","MG_UPC"),
                       lookback=c(365.25,365.25,365.25,365.25,365.25,365.25,365.25),
                       start_date=c(0,0,0,0,0,0,0),
                       end_date=c(7*40,7*40,7*40,7*40,7*40,7*40,7*40),
                       after=c(0,0,0,0,0,0,0))
}
#trimester dates
trimester_timepoint<-data.table(Indicator=c("first","second","third"),
                                start=c(0,98,196),
                                end=c(97,195,7*40))
#start: pregnancy start date + start -1 will give the next window
#end: pregnancy start date + start will give the previous window
#example second trimester: start=pregnancy start date + 98 -1 end=pregnancy start date + end
#mig files
mig_files<-list.files(paste0(projectFolder,"/g_intermediate/migraine_algorithm/"))
mig_files<-mig_files[!mig_files %in% "Migraine_medicines.rds"]
names_events<-list()
for(i in 1:length(mig_files)){
  names_events[[i]]<-unlist(str_split(mig_files[i],"[.]"))[1] 
}
names_events<-as.vector(do.call(rbind,names_events))
names_events_extend<-c(paste0(names_events,"_baseline"),paste0(names_events,"_baseline_2"),paste0(names_events,"_during"),paste0(names_events,"_first"),paste0(names_events,"_second"),paste0(names_events,"_third"))
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
    if("keep" %in% names(mig_dt)){mig_dt[,keep:=NULL]}
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
        mig_dt[,min_date_diag:=pregnancy_start_date-obs_period_diag[StudyVar==names_mg,lookback]]
        mig_dt[,prior_diag:=ifelse(event_date>=min_date_diag & event_date<pregnancy_start_date,1,0)]
        #Check for diagnoses in pregnancy
        mig_dt[,preg_diag:=ifelse(event_date>=pregnancy_start_date & event_date<=pregnancy_end_date,1,0)]
        #add preg id
        mig_dt[,preg_diag:=as.numeric(preg_diag)][,prior_diag:=as.numeric(prior_diag)]
        prior_diag<-mig_dt[prior_diag==1,.N, by=c("person_id","pregnancy_id")]
        setnames(prior_diag,"N","prior_diag")
        preg_diag<-mig_dt[preg_diag==1,.N, by=c("person_id","pregnancy_id")]
        setnames(preg_diag,"N","preg_diag")
        prior_diag<-merge.data.table(prior_diag,preg_diag, all=T, by=c("person_id","pregnancy_id"))
        prior_diag[is.na(prior_diag),prior_diag:=0]
        prior_diag[is.na(preg_diag),preg_diag:=0]
        prior_diag<-prior_diag[prior_diag==0 & preg_diag>0]
        #Count the number of cases to exclude
        excluded_prior_diag<-data.table(event_definition=names_mg, excluded_records=prior_diag[,.N])
        prior_diag[,prior_diag:=NULL]
        #Add the identifier pregnancy id to the main pregnancy D3
        pregnancy_d3_mig<-merge.data.table(pregnancy_d3_mig,prior_diag, by=c("person_id","pregnancy_id"), all.x=T)
        #update the exclude variable
        pregnancy_d3_mig[preg_diag>0, onset_diag:=1]
        pregnancy_d3_mig[,preg_diag:=NULL]
        pregnancy_d3_mig[is.na(onset_diag), onset_diag:=0]
      }
    }
    rm(mig_dt) 


####Migraine MEDICINES files####
  print("Loading all Migraine medicines D3 and merge with the pregnancy D3.")
 
    if (DAP_name %in% c("NIHW", "CHUT")){
      obs_period_med<-data.table(StudyVar=c("Migraine_medicines"),
                                 lookback=c(3*30.25),
                                 start_date=c(0),
                                 end_date=c(40*7),
                                 after=c(0))
    }else{
  obs_period_med<-data.table(StudyVar=c("Migraine_medicines"),
                         lookback=c(365.25),
                         start_date=c(0),
                         end_date=c(40*7),
                         after=c(0))
    }
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
  mig_med[,pregnancy_start_date:=as.IDate(pregnancy_start_date)][,pregnancy_end_date:=as.IDate(pregnancy_end_date)][,birth_date:=as.IDate(birth_date)][,death_date:=as.IDate(death_date)][,op_start_date_mig:=as.IDate(op_start_date_mig)][,op_end_date_mig:=as.IDate(op_end_date_mig)][,medicine_date:=as.IDate(medicine_date)]
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
    mig_med[,min_date_med:=pregnancy_start_date-obs_period_med[med_fl,lookback]]
    mig_med[,prior_med:=ifelse(medicine_date>=min_date_med & medicine_date<pregnancy_start_date,1,0)]
    #Check for prescriptions in pregnancy
    mig_med[,preg_med:=ifelse(medicine_date>=pregnancy_start_date & medicine_date<=pregnancy_end_date,1,0)]
    #add preg id
  mig_med[,preg_med:=as.numeric(preg_med)]
  prior_med<-mig_med[prior_med==1,.N, by=c("person_id","pregnancy_id")]
  setnames(prior_med,"N","prior_med")
  preg_med<-mig_med[preg_med==1,.N, by=c("person_id","pregnancy_id")]
  setnames(preg_med,"N","preg_med")
  rm(mig_med)
  prior_med<-merge.data.table(prior_med,preg_med, all=T, by=c("person_id","pregnancy_id"))
  prior_med[is.na(prior_med),prior_med:=0]
  prior_med[is.na(preg_med),preg_med:=0]
  prior_med<-prior_med[prior_med==0 & preg_med>0]
  
    #Count the number of cases to exclude
    excluded_prior_med<-data.table(event_definition="Triptan_prescription(N02CC)", excluded_records=prior_med[,.N])
    prior_med[,prior_med:=NULL]
    #Add the identifier pregnancy id to the main pregnancy D3
    pregnancy_d3_mig<-merge.data.table(pregnancy_d3_mig,prior_med, by=c("person_id","pregnancy_id"), all.x=T)
    #update the exclude variable
    pregnancy_d3_mig[!is.na(preg_med), onset_med:=1]
    pregnancy_d3_mig[,preg_med:=NULL]
    pregnancy_d3_mig[is.na(onset_med),onset_med:=0]
  }
  }
  }else{print("There are no migraine medicines records.")}
  
####Export excluded number of pregnancies####
#number of pregnancies with onset diagnoses in pregnancy
onset_diag<-pregnancy_d3_mig[onset_diag==1 & onset_med==0,.N]
#number of pregnancies with onset prescription
onset_med<-pregnancy_d3_mig[onset_diag==0 & onset_med==1,.N]
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
    
    if(obs_period_diag[StudyVar==names_events[mig_fl],lookback]>0){
      mig_dt[,lookback:=obs_period_diag[StudyVar==names_events[mig_fl],lookback]]
      mig_dt[,start_preg:=as.IDate(pregnancy_start_date-lookback)]
      #exclude all events that are outside observation period of interest
      mig_dt[,diff:=event_date-start_preg]
      #remove all records with date before start date pregnancy+lookback
      before[[w]]<-data.table(StudyVar=names_events[mig_fl], before_start=mig_dt[diff<0,.N])
      mig_dt<-mig_dt[diff>=0]
      mig_dt[,diff:=NULL][,lookback:=NULL][,start_preg:=NULL]
    }else{
      #remove all records before start obs
      mig_dt[,start:=obs_period_diag[StudyVar==names_events[mig_fl],start_date]]
      mig_dt[,start_preg:=as.IDate(pregnancy_start_date+start)]
      mig_dt[,diff:=event_date-start_preg]
      before[[w]]<-data.table(StudyVar=names_events[mig_fl], before_start=mig_dt[diff<0,.N])
      mig_dt<-mig_dt[diff>=0]
      mig_dt[,diff:=NULL][,start:=NULL][,start_preg:=NULL]
    }
    
    if(mig_dt[,.N]>0){
      if(obs_period_diag[StudyVar==names_events[mig_fl],after]>0){
        mig_dt[,after:=obs_period_diag[StudyVar==names_events[mig_fl],after]]
        mig_dt[,end_preg:=as.IDate(pregnancy_end_date+after)]
        mig_dt[,diff:=event_date-end_preg]
        after[[w]]<-data.table(StudyVar=names_events[mig_fl], after_end=mig_dt[diff>0,.N])
        mig_dt<-mig_dt[diff<=0]
        mig_dt[,diff:=NULL][,after:=NULL][,end_preg:=NULL]
      }else{
        mig_dt[,end:=obs_period_diag[StudyVar==names_events[mig_fl],end_date]]
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
      saveRDS(mig_dt, paste0(projectFolder,"/g_intermediate/migraine_algorithm/final_d3/", names_events[mig_fl],"_pregnancy_D3.rds"))
      cols<-c("person_id","pregnancy_id","pregnancy_start_date","pregnancy_end_date","event_date")
      mig_dt<-mig_dt[,cols,with=F]
      #Identify all baseline events
      mig_dt[,dif:=event_date-pregnancy_start_date]
      mig_dt[dif<0,paste0(names_events[mig_fl],"_baseline"):=1]
      mig_baseline<-mig_dt[get(paste0(names_events[mig_fl],"_baseline"))==1,lapply(.SD, sum),.SDcols = paste0(names_events[mig_fl],"_baseline"), by=c("person_id","pregnancy_id","pregnancy_start_date","pregnancy_end_date")]
      mig_dt[,dif:=NULL][,eval(paste0(names_events[mig_fl],"_baseline")):=NULL]
      pregnancy_d3_mig<-merge.data.table(pregnancy_d3_mig,mig_baseline,all.x=T, by=cols[!cols %in% "event_date"])
      pregnancy_d3_mig[is.na(get(paste0(names_events[mig_fl],"_baseline"))),eval(paste0(names_events[mig_fl],"_baseline")):=0]
      #depending on the DAP
      if(!DAP_name %in% c("CHUT")){
        mig_dt[,start:=pregnancy_start_date - 3*30.25] 
        mig_dt[event_date>=start & event_date<pregnancy_start_date,paste0(names_events[mig_fl],"_baseline_2"):=1]
        mig_baseline_2<-mig_dt[get(paste0(names_events[mig_fl],"_baseline_2"))==1,lapply(.SD, sum),.SDcols = paste0(names_events[mig_fl],"_baseline_2"), by=c("person_id","pregnancy_id","pregnancy_start_date","pregnancy_end_date")]
        mig_dt[,start:=NULL][,eval(paste0(names_events[mig_fl],"_baseline_2")):=NULL]
        pregnancy_d3_mig<-merge.data.table(pregnancy_d3_mig,mig_baseline_2,all.x=T, by=cols[!cols %in% "event_date"])
        pregnancy_d3_mig[is.na(get(paste0(names_events[mig_fl],"_baseline_2"))),eval(paste0(names_events[mig_fl],"_baseline_2")):=0]
      }else{
        pregnancy_d3_mig[,eval(paste0(names_events[mig_fl],"_baseline_2")):=get(paste0(names_events[mig_fl],"_baseline"))]
      }
      #during pregnancy
      mig_dt[event_date>=pregnancy_start_date & event_date<pregnancy_end_date,paste0(names_events[mig_fl],"_during"):=1]
      mig_during<-mig_dt[get(paste0(names_events[mig_fl],"_during"))==1,lapply(.SD, sum),.SDcols = paste0(names_events[mig_fl],"_during"), by=c("person_id","pregnancy_id","pregnancy_start_date","pregnancy_end_date")]
      mig_dt[,eval(paste0(names_events[mig_fl],"_during")):=NULL]
      pregnancy_d3_mig<-merge.data.table(pregnancy_d3_mig,mig_during,all.x=T, by=cols[!cols %in% "event_date"])
      pregnancy_d3_mig[is.na(get(paste0(names_events[mig_fl],"_during"))),eval(paste0(names_events[mig_fl],"_during")):=0]
      #first
      mig_dt[,start:=pregnancy_start_date + trimester_timepoint[Indicator=="first",as.numeric(start)]][,end:=pregnancy_start_date + trimester_timepoint[Indicator=="first",as.numeric(end)]]
      mig_dt[event_date>=start & event_date<=end,paste0(names_events[mig_fl],"_first"):=1]
      mig_first<-mig_dt[get(paste0(names_events[mig_fl],"_first"))==1,lapply(.SD, sum),.SDcols = paste0(names_events[mig_fl],"_first"), by=c("person_id","pregnancy_id","pregnancy_start_date","pregnancy_end_date")]
      mig_dt[,eval(paste0(names_events[mig_fl],"_first")):=NULL]
      pregnancy_d3_mig<-merge.data.table(pregnancy_d3_mig,mig_first,all.x=T, by=cols[!cols %in% "event_date"])
      pregnancy_d3_mig[is.na(get(paste0(names_events[mig_fl],"_first"))),eval(paste0(names_events[mig_fl],"_first")):=0]
      #second
      mig_dt[,start:=pregnancy_start_date + trimester_timepoint[Indicator=="second",as.numeric(start)]][,end:=pregnancy_start_date + trimester_timepoint[Indicator=="second",as.numeric(end)]]
      mig_dt[event_date>=start & event_date<=end,paste0(names_events[mig_fl],"_second"):=1]
      mig_second<-mig_dt[get(paste0(names_events[mig_fl],"_second"))==1,lapply(.SD, sum),.SDcols = paste0(names_events[mig_fl],"_second"), by=c("person_id","pregnancy_id","pregnancy_start_date","pregnancy_end_date")]
      mig_dt[,eval(paste0(names_events[mig_fl],"_second")):=NULL]
      pregnancy_d3_mig<-merge.data.table(pregnancy_d3_mig,mig_second,all.x=T, by=cols[!cols %in% "event_date"])
      pregnancy_d3_mig[is.na(get(paste0(names_events[mig_fl],"_second"))),eval(paste0(names_events[mig_fl],"_second")):=0]
      #third
      mig_dt[,start:=pregnancy_start_date + trimester_timepoint[Indicator=="third",as.numeric(start)]][,end:=pregnancy_start_date + trimester_timepoint[Indicator=="third",as.numeric(end)]]
      mig_dt[event_date>=start & event_date<=end,paste0(names_events[mig_fl],"_third"):=1]
      mig_third<-mig_dt[get(paste0(names_events[mig_fl],"_third"))==1,lapply(.SD, sum),.SDcols = paste0(names_events[mig_fl],"_third"), by=c("person_id","pregnancy_id","pregnancy_start_date","pregnancy_end_date")]
      mig_dt[,eval(paste0(names_events[mig_fl],"_third")):=NULL]
      pregnancy_d3_mig<-merge.data.table(pregnancy_d3_mig,mig_third,all.x=T, by=cols[!cols %in% "event_date"])
      pregnancy_d3_mig[is.na(get(paste0(names_events[mig_fl],"_third"))),eval(paste0(names_events[mig_fl],"_third")):=0]
      
      }else{
      pregnancy_d3_mig[,eval(paste0(names_events[mig_fl],"_baseline")):=0]
      pregnancy_d3_mig[,eval(paste0(names_events[mig_fl],"_baseline_2")):=0]
      pregnancy_d3_mig[,eval(paste0(names_events[mig_fl],"_during")):=0]
      pregnancy_d3_mig[,eval(paste0(names_events[mig_fl],"_first")):=0]
      pregnancy_d3_mig[,eval(paste0(names_events[mig_fl],"_second")):=0]
      pregnancy_d3_mig[,eval(paste0(names_events[mig_fl],"_third")):=0]
    }
  }else{
    pregnancy_d3_mig[,eval(paste0(names_events[mig_fl],"_baseline")):=0]
    pregnancy_d3_mig[,eval(paste0(names_events[mig_fl],"_baseline_2")):=0]
    pregnancy_d3_mig[,eval(paste0(names_events[mig_fl],"_during")):=0]
    pregnancy_d3_mig[,eval(paste0(names_events[mig_fl],"_first")):=0]
    pregnancy_d3_mig[,eval(paste0(names_events[mig_fl],"_second")):=0]
    pregnancy_d3_mig[,eval(paste0(names_events[mig_fl],"_third")):=0]
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
fwrite(sum_mig,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_summary_included_records_migraine.csv"),row.names = F)
rm(sum_mig)

names_migraine<-c(paste0(names(conditions_migraine),"_baseline"),paste0(names(conditions_migraine),"_baseline_2"),paste0(names(conditions_migraine),"_during"),paste0(names(conditions_migraine),"_first"),paste0(names(conditions_migraine),"_second"),paste0(names(conditions_migraine),"_third"))
#identify events that are not present from conditions_migraine
not_present<-setdiff(names_migraine, names_events_extend)
pregnancy_d3_mig[,eval(not_present):=list(0)]
####Migraine MEDICINES####
print("Loading all Migraine medicines D3 and merge with the pregnancy D3.")

mig_med_fl<-list.files(paste0(projectFolder,"/g_intermediate/migraine_algorithm/"),"Migraine_medicines")
mig_med<-readRDS(paste0(projectFolder,"/g_intermediate/migraine_algorithm/",mig_med_fl))

mig_med[,truncated_atc:=substr(atc_code,1,5)]
mig_med<-mig_med[truncated_atc == "N02CC"]
mig_med[,truncated_atc:=NULL]

mig_med<-merge.data.table(pregnancy_d3_mig, mig_med, by="person_id", all.x=T, allow.cartesian = T)
mig_med<-mig_med[!is.na(medicine_date)]
mig_med[,pregnancy_start_date:=as.IDate(pregnancy_start_date)][,pregnancy_end_date:=as.IDate(pregnancy_end_date)][,birth_date:=as.IDate(birth_date)][,death_date:=as.IDate(death_date)][,op_start_date_mig:=as.IDate(op_start_date_mig)][,op_end_date_mig:=as.IDate(op_end_date_mig)][,medicine_date:=as.IDate(medicine_date)]


original<-data.table(StudyVar=c("Migraine_medicines"), original_records=as.character(mig_med[,.N]))
before<-list()
after<-list()
sum<-list()
w<-1
for(med_fl in 1:length(obs_period_med[,.N])){
if(mig_med[,.N]>0){
  if(obs_period_med[med_fl,lookback]>0){
    mig_med[,lookback:=obs_period_med[med_fl,lookback]]
    mig_med[,start_preg:=as.IDate(pregnancy_start_date-lookback)]
    #exclude all pregnancies that are outside observation period of interest
    mig_med[,diff:=medicine_date-start_preg]
    #remove all records with date before start date pregnancy+lookback
    before[[w]]<-data.table(StudyVar=obs_period_med[med_fl,StudyVar], before_start=as.character(mig_med[diff<0,.N]))
    mig_med<-mig_med[diff>=0]
    mig_med[,diff:=NULL][,lookback:=NULL][,start_preg:=NULL]
  }else{
    #remove all records before start obs
    mig_med[,start:=obs_period_med[med_fl,start_date]]
    mig_med[,start_preg:=as.IDate(pregnancy_start_date+start)]
    mig_med[,diff:=medicine_date-start_preg]
    before[[w]]<-data.table(StudyVar=obs_period_med[med_fl,StudyVar], before_start=as.character(mig_med[diff<0,.N]))
    mig_med<-mig_med[diff>=0]
    mig_med[,diff:=NULL][,start:=NULL][,start_preg:=NULL]
  }
  
  if(mig_med[,.N]>0){
    if(obs_period_med[med_fl,after]>0){
      mig_med[,after:=obs_period_med[med_fl,after]]
      mig_med[,end_preg:=as.IDate(pregnancy_end_date+after)]
      mig_med[,diff:=medicine_date-end_preg]
      after[[w]]<-data.table(StudyVar=obs_period_med[med_fl,StudyVar], after_end=as.character(mig_med[diff>0,.N]))
      mig_med<-mig_med[diff<=0]
      mig_med[,diff:=NULL][,after:=NULL][,end_preg:=NULL]
    }else{
      mig_med[,end:=obs_period_med[med_fl,end_date]]
      mig_med[,end_preg:=as.IDate(pregnancy_start_date+end)]
      mig_med[,diff:=medicine_date-end_preg]
      after[[w]]<-data.table(StudyVar=obs_period_med[med_fl,StudyVar], after_end=as.character(mig_med[diff>0,.N]))
      mig_med<-mig_med[diff<=0]
      mig_med[,diff:=NULL][,end:=NULL][,end_preg:=NULL]
    }
  }else{
    after[[w]]<-data.table(StudyVar=names_events[mig_fl], after_end=as.character(0))
  }
  
  if(mig_med[,.N]>0){
    #create a summary of included records
    sum[[w]]<-data.table(StudyVar=obs_period_med[med_fl,StudyVar], no_records=mig_med[!is.na(medicine_date) & !duplicated(pregnancy_id),.N], no_pregnancies=mig_med[!duplicated(pregnancy_id),.N])
    cols_to_exp<-c("person_id","pregnancy_id","pregnancy_start_date","pregnancy_end_date","year","birth_date","death_date","op_end_date_mig","op_start_date_mig","age","maternal_age","year_group","medicine_date","atc_code","condition","medicinal_product_group")
    saveRDS(mig_med[,cols_to_exp,with=F], paste0(projectFolder,"/g_intermediate/migraine_algorithm/final_d3/", obs_period_med[med_fl,StudyVar],"_pregnancy_D3.rds"))
    rm(cols_to_exp)
    cols<-c("person_id","pregnancy_id","pregnancy_start_date","pregnancy_end_date","medicine_date")
    mig_med<-mig_med[,cols,with=F]
    #Identify all baseline prescriptions
    mig_med[,dif:=medicine_date-pregnancy_start_date]
    mig_med[dif<0,paste0(obs_period_med[med_fl, StudyVar],"_baseline"):=1]
    mig_med_baseline<-mig_med[get(paste0(obs_period_med[med_fl, StudyVar],"_baseline"))==1,lapply(.SD, sum),.SDcols = paste0(obs_period_med[med_fl, StudyVar],"_baseline"), by=c("person_id","pregnancy_id","pregnancy_start_date","pregnancy_end_date")]
    mig_med[,dif:=NULL][,eval(paste0(obs_period_med[med_fl, StudyVar],"_baseline")):=NULL]
    pregnancy_d3_mig<-merge.data.table(pregnancy_d3_mig,mig_med_baseline,all.x=T, by=cols[!cols %in% "medicine_date"])
    pregnancy_d3_mig[is.na(get(paste0(obs_period_med[med_fl, StudyVar],"_baseline"))),eval(paste0(obs_period_med[med_fl, StudyVar],"_baseline")):=0]
    #depending on the DAP
    if(!DAP_name %in% c("NIHW", "CHUT")){
      mig_med[,start:=pregnancy_start_date - 3*30.25] 
      mig_med[medicine_date>=start & medicine_date<pregnancy_start_date,paste0(obs_period_med[med_fl, StudyVar],"_baseline_2"):=1]
      mig_baseline_2<-mig_med[get(paste0(obs_period_med[med_fl, StudyVar],"_baseline_2"))==1,lapply(.SD, sum),.SDcols = paste0(obs_period_med[med_fl, StudyVar],"_baseline_2"), by=c("person_id","pregnancy_id","pregnancy_start_date","pregnancy_end_date")]
      mig_med[,start:=NULL][,eval(paste0(obs_period_med[med_fl, StudyVar],"_baseline_2")):=NULL]
      pregnancy_d3_mig<-merge.data.table(pregnancy_d3_mig,mig_baseline_2,all.x=T, by=cols[!cols %in% "medicine_date"])
      pregnancy_d3_mig[is.na(get(paste0(obs_period_med[med_fl, StudyVar],"_baseline_2"))),eval(paste0(obs_period_med[med_fl, StudyVar],"_baseline_2")):=0]
    }else{
      pregnancy_d3_mig[,eval(paste0(obs_period_med[med_fl, StudyVar],"_baseline_2")):=get(paste0(obs_period_med[med_fl, StudyVar],"_baseline"))]
    }
    #during pregnancy
    mig_med[medicine_date>=pregnancy_start_date & medicine_date<pregnancy_end_date,paste0(obs_period_med[med_fl, StudyVar],"_during"):=1]
    mig_med_during<-mig_med[get(paste0(obs_period_med[med_fl, StudyVar],"_during"))==1,lapply(.SD, sum),.SDcols = paste0(obs_period_med[med_fl, StudyVar],"_during"), by=c("person_id","pregnancy_id","pregnancy_start_date","pregnancy_end_date")]
    mig_med[,eval(paste0(obs_period_med[med_fl, StudyVar],"_during")):=NULL]
    pregnancy_d3_mig<-merge.data.table(pregnancy_d3_mig,mig_med_during,all.x=T, by=cols[!cols %in% "medicine_date"])
    pregnancy_d3_mig[is.na(get(paste0(obs_period_med[med_fl, StudyVar],"_during"))),eval(paste0(obs_period_med[med_fl, StudyVar],"_during")):=0]
    #first
    mig_med[,start:=pregnancy_start_date + trimester_timepoint[Indicator=="first",as.numeric(start)]][,end:=pregnancy_start_date + trimester_timepoint[Indicator=="first",as.numeric(end)]]
    mig_med[medicine_date>=start & medicine_date<=end,paste0(obs_period_med[med_fl, StudyVar],"_first"):=1]
    mig_med_first<-mig_med[get(paste0(obs_period_med[med_fl, StudyVar],"_first"))==1,lapply(.SD, sum),.SDcols = paste0(obs_period_med[med_fl, StudyVar],"_first"), by=c("person_id","pregnancy_id","pregnancy_start_date","pregnancy_end_date")]
    mig_med[,eval(paste0(obs_period_med[med_fl, StudyVar],"_first")):=NULL]
    pregnancy_d3_mig<-merge.data.table(pregnancy_d3_mig,mig_med_first,all.x=T, by=cols[!cols %in% "medicine_date"])
    pregnancy_d3_mig[is.na(get(paste0(obs_period_med[med_fl, StudyVar],"_first"))),eval(paste0(obs_period_med[med_fl, StudyVar],"_first")):=0]
    #second
    mig_med[,start:=pregnancy_start_date + trimester_timepoint[Indicator=="second",as.numeric(start)]][,end:=pregnancy_start_date + trimester_timepoint[Indicator=="second",as.numeric(end)]]
    mig_med[medicine_date>=start & medicine_date<=end,paste0(obs_period_med[med_fl, StudyVar],"_second"):=1]
    mig_med_second<-mig_med[get(paste0(obs_period_med[med_fl, StudyVar],"_second"))==1,lapply(.SD, sum),.SDcols = paste0(obs_period_med[med_fl, StudyVar],"_second"), by=c("person_id","pregnancy_id","pregnancy_start_date","pregnancy_end_date")]
    mig_med[,eval(paste0(obs_period_med[med_fl, StudyVar],"_second")):=NULL]
    pregnancy_d3_mig<-merge.data.table(pregnancy_d3_mig,mig_med_second,all.x=T, by=cols[!cols %in% "medicine_date"])
    pregnancy_d3_mig[is.na(get(paste0(obs_period_med[med_fl, StudyVar],"_second"))),eval(paste0(obs_period_med[med_fl, StudyVar],"_second")):=0]
    #third
    mig_med[,start:=pregnancy_start_date + trimester_timepoint[Indicator=="third",as.numeric(start)]][,end:=pregnancy_start_date + trimester_timepoint[Indicator=="third",as.numeric(end)]]
    mig_med[medicine_date>=start & medicine_date<=end,paste0(obs_period_med[med_fl, StudyVar],"_third"):=1]
    mig_med_third<-mig_med[get(paste0(obs_period_med[med_fl, StudyVar],"_third"))==1,lapply(.SD, sum),.SDcols = paste0(obs_period_med[med_fl, StudyVar],"_third"), by=c("person_id","pregnancy_id","pregnancy_start_date","pregnancy_end_date")]
    mig_med[,eval(paste0(obs_period_med[med_fl, StudyVar],"_third")):=NULL]
    pregnancy_d3_mig<-merge.data.table(pregnancy_d3_mig,mig_med_third,all.x=T, by=cols[!cols %in% "medicine_date"])
    pregnancy_d3_mig[is.na(get(paste0(obs_period_med[med_fl, StudyVar],"_third"))),eval(paste0(obs_period_med[med_fl, StudyVar],"_third")):=0]
    
  }else{
    pregnancy_d3_mig[,eval(paste0(obs_period_med[med_fl,StudyVar],"_baseline")):=0]
    pregnancy_d3_mig[,eval(paste0(obs_period_med[med_fl,StudyVar],"_baseline_2")):=0]
    pregnancy_d3_mig[,eval(paste0(obs_period_med[med_fl,StudyVar],"_during")):=0]
    pregnancy_d3_mig[,eval(paste0(obs_period_med[med_fl,StudyVar],"_first")):=0]
    pregnancy_d3_mig[,eval(paste0(obs_period_med[med_fl,StudyVar],"_second")):=0]
    pregnancy_d3_mig[,eval(paste0(obs_period_med[med_fl,StudyVar],"_third")):=0]
  }
}else{
  pregnancy_d3_mig[,eval(paste0(obs_period_med[med_fl,StudyVar],"_baseline")):=0]
  pregnancy_d3_mig[,eval(paste0(obs_period_med[med_fl,StudyVar],"_baseline_2")):=0]
  pregnancy_d3_mig[,eval(paste0(obs_period_med[med_fl,StudyVar],"_during")):=0]
  pregnancy_d3_mig[,eval(paste0(obs_period_med[med_fl,StudyVar],"_first")):=0]
  pregnancy_d3_mig[,eval(paste0(obs_period_med[med_fl,StudyVar],"_second")):=0]
  pregnancy_d3_mig[,eval(paste0(obs_period_med[med_fl,StudyVar],"_third")):=0]
  
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
fwrite(removed_rec,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_excluded_records_migraine.csv"),row.names = F)
rm(removed_rec)

#Pregnancies that had the event of interest not in the timeframe of study were excluded, this is why the number of pregnancies is different between different events
sum_med<-as.data.table(do.call(rbind,sum))
rm(original,before,after,sum)
rm(mig_med_fl)

names_medicines<-c(paste0(obs_period_med[,StudyVar],"_baseline"),paste0(obs_period_med[med_fl,StudyVar],"_baseline_2"),paste0(obs_period_med[med_fl,StudyVar],"_during"),paste0(obs_period_med[med_fl,StudyVar],"_first"),paste0(obs_period_med[med_fl,StudyVar],"_second"),paste0(obs_period_med[med_fl,StudyVar],"_third"))
#identify events that are not present from conditions_migraine
not_present<-setdiff(names_medicines, names(pregnancy_d3_mig))
if(length(not_present)>0){
pregnancy_d3_mig[,eval(not_present):=list(0)]
}
#rm(obs_period_med)

obs_period<-rbind(obs_period_diag,obs_period_med)
fwrite(obs_period, paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_observation_periods_migraine.csv"),row.names = F)
rm(obs_period,obs_period_diag,obs_period_med)

print("Export Migraine pregnancy D3")
fwrite(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm/final_d3/pregnancy_d3_migraine_algorithm.csv"),row.names = F)

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
MIG_Dx_a_dt[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_Dx_a_dt[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
MIG_Dx_a_dt[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_a_dt[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_a_dt[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_Dx_a_dt,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Dx_a.csv"),row.names = F)

#by maternal age
pregnancy_d3_mig[,maternal_age:=as.character(maternal_age)]
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id), .N, by=.(maternal_age)]
names(records)<-c("maternal_age","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id), by="maternal_age",.N]
names(total)<-c("maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by="maternal_age",all=T)
MIG_Dx_a_1<-data.table(algorithm="MIG_Dx_a", records)
MIG_Dx_a_1[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_Dx_a_1<-MIG_Dx_a_1[order(maternal_age)]
rm(records,total)
MIG_Dx_a_1[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_Dx_a_1[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_Dx_a_1[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_Dx_a_1[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_a_1[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_a_1[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_Dx_a_1,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Dx_a_age.csv"),row.names = F)
rm(MIG_Dx_a_1)

#by year
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by="year", .N]
names(records)<-c("year","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by="year",.N]
names(total)<-c("year","no_pregnancies")
records<-merge.data.table(records,total,by="year",all=T)
MIG_Dx_a_2<-data.table(algorithm="MIG_Dx_a", records)
MIG_Dx_a_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_Dx_a_2<-MIG_Dx_a_2[order(year)]
rm(records,total)
MIG_Dx_a_2[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_Dx_a_2[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_Dx_a_2[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_Dx_a_2[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_a_2[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_a_2[lower_95_CI<0,lower_95_CI:=0]
fwrite(MIG_Dx_a_2,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Dx_a_year.csv"),row.names = F)
rm(MIG_Dx_a_2)

#by year group and maternal age
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by=c("year_group","maternal_age"), .N]
names(records)<-c("year_group","maternal_age", "no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by=c("year_group","maternal_age"),.N]
names(total)<-c("year_group","maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","maternal_age"), all=T)
MIG_Dx_a_3<-data.table(algorithm="MIG_Dx_a", records)
MIG_Dx_a_3[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_Dx_a_3<-MIG_Dx_a_3[order(year_group,maternal_age)]
rm(records,total)
MIG_Dx_a_3[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
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
MIG_Dx_b_dt[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_Dx_b_dt[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
MIG_Dx_b_dt[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_b_dt[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_b_dt[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_Dx_b_dt,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Dx_b.csv"),row.names = F)

#by maternal age
pregnancy_d3_mig[,maternal_age:=as.character(maternal_age)]
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id), .N, by=.(maternal_age)]
names(records)<-c("maternal_age","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id), by="maternal_age",.N]
names(total)<-c("maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by="maternal_age",all=T)
MIG_Dx_b_1<-data.table(algorithm="MIG_Dx_b", records)
MIG_Dx_b_1[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_Dx_b_1<-MIG_Dx_b_1[order(maternal_age)]
rm(records,total)
MIG_Dx_b_1[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_Dx_b_1[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_Dx_b_1[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_Dx_b_1[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_b_1[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_b_1[lower_95_CI<0,lower_95_CI:=0]
fwrite(MIG_Dx_b_1,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Dx_b_age.csv"),row.names = F)
rm(MIG_Dx_b_1)


#by year
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by="year", .N]
names(records)<-c("year","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by="year",.N]
names(total)<-c("year","no_pregnancies")
records<-merge.data.table(records,total,by="year",all=T)
MIG_Dx_b_2<-data.table(algorithm="MIG_Dx_b", records)
MIG_Dx_b_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_Dx_b_2<-MIG_Dx_b_2[order(year)]
rm(records,total)
MIG_Dx_b_2[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_Dx_b_2[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_Dx_b_2[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_Dx_b_2[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_b_2[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_b_2[lower_95_CI<0,lower_95_CI:=0]
fwrite(MIG_Dx_b_2,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Dx_b_year.csv"),row.names = F)
rm(MIG_Dx_b_2)


#by year group and maternal age
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by=c("year_group","maternal_age"), .N]
names(records)<-c("year_group","maternal_age", "no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by=c("year_group","maternal_age"),.N]
names(total)<-c("year_group","maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","maternal_age"), all=T)
MIG_Dx_b_3<-data.table(algorithm="MIG_Dx_b", records)
MIG_Dx_b_3[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_Dx_b_3<-MIG_Dx_b_3[order(year_group,maternal_age)]
rm(records,total)
MIG_Dx_b_3[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
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
MIG_Rx_a_dt[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_Rx_a_dt[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
MIG_Rx_a_dt[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_a_dt[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_a_dt[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_Rx_a_dt,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Rx_a.csv"),row.names = F)

#by maternal age
pregnancy_d3_mig[,maternal_age:=as.character(maternal_age)]
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id), .N, by=.(maternal_age)]
names(records)<-c("maternal_age","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id), by="maternal_age",.N]
names(total)<-c("maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by="maternal_age",all=T)
MIG_Rx_a_1<-data.table(algorithm="MIG_Rx_a", records)
MIG_Rx_a_1[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_Rx_a_1<-MIG_Rx_a_1[order(maternal_age)]
rm(records,total)
MIG_Rx_a_1[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_Rx_a_1[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_Rx_a_1[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_Rx_a_1[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_a_1[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_a_1[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_Rx_a_1,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Rx_a_age.csv"),row.names = F)
rm(MIG_Rx_a_1)


#by year
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by="year", .N]
names(records)<-c("year","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by="year",.N]
names(total)<-c("year","no_pregnancies")
records<-merge.data.table(records,total,by="year",all=T)
MIG_Rx_a_2<-data.table(algorithm="MIG_Rx_a", records)
MIG_Rx_a_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_Rx_a_2<-MIG_Rx_a_2[order(year)]
rm(records,total)
MIG_Rx_a_2[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_Rx_a_2[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_Rx_a_2[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_Rx_a_2[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_a_2[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_a_2[lower_95_CI<0,lower_95_CI:=0]
fwrite(MIG_Rx_a_2,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Rx_a_year.csv"),row.names = F)
rm(MIG_Rx_a_2)


#by year group and maternal age
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by=c("year_group","maternal_age"), .N]
names(records)<-c("year_group","maternal_age", "no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by=c("year_group","maternal_age"),.N]
names(total)<-c("year_group","maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","maternal_age"), all=T)
MIG_Rx_a_3<-data.table(algorithm="MIG_Rx_a", records)
MIG_Rx_a_3[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_Rx_a_3<-MIG_Rx_a_3[order(year_group,maternal_age)]
rm(records,total)
MIG_Rx_a_3[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
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
MIG_Rx_b_dt[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_Rx_b_dt[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
MIG_Rx_b_dt[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_b_dt[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_b_dt[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_Rx_b_dt,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Rx_b.csv"),row.names = F)

#by maternal age
pregnancy_d3_mig[,maternal_age:=as.character(maternal_age)]
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id), .N, by=.(maternal_age)]
names(records)<-c("maternal_age","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id), by="maternal_age",.N]
names(total)<-c("maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by="maternal_age",all=T)
MIG_Rx_b_1<-data.table(algorithm="MIG_Rx_b", records)
MIG_Rx_b_1[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_Rx_b_1<-MIG_Rx_b_1[order(maternal_age)]
rm(records,total)
MIG_Rx_b_1[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_Rx_b_1[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_Rx_b_1[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_Rx_b_1[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_b_1[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_b_1[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_Rx_b_1,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Rx_b_age.csv"),row.names = F)
rm(MIG_Rx_b_1)


#by year
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by="year", .N]
names(records)<-c("year","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by="year",.N]
names(total)<-c("year","no_pregnancies")
records<-merge.data.table(records,total,by="year",all=T)
MIG_Rx_b_2<-data.table(algorithm="MIG_Rx_b", records)
MIG_Rx_b_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_Rx_b_2<-MIG_Rx_b_2[order(year)]
rm(records,total)
MIG_Rx_b_2[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_Rx_b_2[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_Rx_b_2[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_Rx_b_2[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_b_2[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_b_2[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_Rx_b_2,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Rx_b_year.csv"),row.names = F)
rm(MIG_Rx_b_2)


#by year group and maternal age
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by=c("year_group","maternal_age"), .N]
names(records)<-c("year_group","maternal_age", "no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by=c("year_group","maternal_age"),.N]
names(total)<-c("year_group","maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","maternal_age"), all=T)
MIG_Rx_b_3<-data.table(algorithm="MIG_Rx_b", records)
MIG_Rx_b_3[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_Rx_b_3<-MIG_Rx_b_3[order(year_group,maternal_age)]
rm(records,total)
MIG_Rx_b_3[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
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
MIG_DxRx_a_dt[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_DxRx_a_dt[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
MIG_DxRx_a_dt[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_a_dt[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_a_dt[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_DxRx_a_dt,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_DxRx_a.csv"),row.names = F)

#by maternal age
pregnancy_d3_mig[,maternal_age:=as.character(maternal_age)]
records<-pregnancy_d3_mig[alternative==1 & !duplicated(pregnancy_id), .N, by=.(maternal_age)]
names(records)<-c("maternal_age","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id), by="maternal_age",.N]
names(total)<-c("maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by="maternal_age",all=T)
MIG_DxRx_a_1<-data.table(algorithm="MIG_DxRx_a", records)
MIG_DxRx_a_1[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_DxRx_a_1<-MIG_DxRx_a_1[order(maternal_age)]
rm(records,total)
MIG_DxRx_a_1[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_DxRx_a_1[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_DxRx_a_1[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_DxRx_a_1[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_a_1[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_a_1[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_DxRx_a_1,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_DxRx_a_age.csv"),row.names = F)
rm(MIG_DxRx_a_1)

#by year
records<-pregnancy_d3_mig[alternative==1 & !duplicated(pregnancy_id),by="year", .N]
names(records)<-c("year","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by="year",.N]
names(total)<-c("year","no_pregnancies")
records<-merge.data.table(records,total,by="year",all=T)
MIG_DxRx_a_2<-data.table(algorithm="MIG_DxRx_a", records)
MIG_DxRx_a_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_DxRx_a_2<-MIG_DxRx_a_2[order(year)]
rm(records,total)
MIG_DxRx_a_2[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_DxRx_a_2[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_DxRx_a_2[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_DxRx_a_2[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_a_2[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_a_2[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_DxRx_a_2,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_DxRx_a_year.csv"),row.names = F)
rm(MIG_DxRx_a_2)


#by year group and maternal age
records<-pregnancy_d3_mig[alternative==1 & !duplicated(pregnancy_id),by=c("year_group","maternal_age"), .N]
names(records)<-c("year_group","maternal_age", "no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by=c("year_group","maternal_age"),.N]
names(total)<-c("year_group","maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","maternal_age"), all=T)
MIG_DxRx_a_3<-data.table(algorithm="MIG_DxRx_a", records)
MIG_DxRx_a_3[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_DxRx_a_3<-MIG_DxRx_a_3[order(year_group,maternal_age)]
rm(records,total)
MIG_DxRx_a_3[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
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
MIG_DxRx_b_dt[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_DxRx_b_dt[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
MIG_DxRx_b_dt[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_b_dt[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_b_dt[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_DxRx_b_dt,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_DxRx_b.csv"),row.names = F)

#by maternal age
pregnancy_d3_mig[,maternal_age:=as.character(maternal_age)]
records<-pregnancy_d3_mig[alternative==1 & !duplicated(pregnancy_id), .N, by=.(maternal_age)]
names(records)<-c("maternal_age","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id), by="maternal_age",.N]
names(total)<-c("maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by="maternal_age",all=T)
MIG_DxRx_b_1<-data.table(algorithm="MIG_DxRx_b", records)
MIG_DxRx_b_1[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_DxRx_b_1<-MIG_DxRx_b_1[order(maternal_age)]
rm(records,total)
MIG_DxRx_b_1[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_DxRx_b_1[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_DxRx_b_1[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_DxRx_b_1[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_b_1[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_b_1[lower_95_CI<0,lower_95_CI:=0]
fwrite(MIG_DxRx_b_1,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_DxRx_b_age.csv"),row.names = F)
rm(MIG_DxRx_b_1)


#by year
records<-pregnancy_d3_mig[alternative==1 & !duplicated(pregnancy_id),by="year", .N]
names(records)<-c("year","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by="year",.N]
names(total)<-c("year","no_pregnancies")
records<-merge.data.table(records,total,by="year",all=T)
MIG_DxRx_b_2<-data.table(algorithm="MIG_DxRx_b", records)
MIG_DxRx_b_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_DxRx_b_2<-MIG_DxRx_b_2[order(year)]
rm(records,total)
MIG_DxRx_b_2[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_DxRx_b_2[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_DxRx_b_2[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_DxRx_b_2[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_b_2[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_b_2[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_DxRx_b_2,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_DxRx_b_year.csv"),row.names = F)
rm(MIG_DxRx_b_2)


#by year group and maternal age
records<-pregnancy_d3_mig[alternative==1 & !duplicated(pregnancy_id),by=c("year_group","maternal_age"), .N]
names(records)<-c("year_group","maternal_age", "no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by=c("year_group","maternal_age"),.N]
names(total)<-c("year_group","maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","maternal_age"), all=T)
MIG_DxRx_b_3<-data.table(algorithm="MIG_DxRx_b", records)
MIG_DxRx_b_3[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_DxRx_b_3<-MIG_DxRx_b_3[order(year_group,maternal_age)]
rm(records,total)
MIG_DxRx_b_3[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
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
MIG_Dx_during_dt[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_Dx_during_dt[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
MIG_Dx_during_dt[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_during_dt[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_during_dt[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_Dx_during_dt,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Dx_during.csv"),row.names = F)

#by maternal age
pregnancy_d3_mig[,maternal_age:=as.character(maternal_age)]
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id), .N, by=.(maternal_age)]
names(records)<-c("maternal_age","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id), by="maternal_age",.N]
names(total)<-c("maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by="maternal_age",all=T)
MIG_Dx_during_1<-data.table(algorithm="MIG_Dx_during", records)
MIG_Dx_during_1[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_Dx_during_1<-MIG_Dx_during_1[order(maternal_age)]
rm(records,total)
MIG_Dx_during_1[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_Dx_during_1[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_Dx_during_1[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_Dx_during_1[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_during_1[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_during_1[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_Dx_during_1,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Dx_during_age.csv"),row.names = F)
rm(MIG_Dx_during_1)

#by year
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by="year", .N]
names(records)<-c("year","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by="year",.N]
names(total)<-c("year","no_pregnancies")
records<-merge.data.table(records,total,by="year",all=T)
MIG_Dx_during_2<-data.table(algorithm="MIG_Dx_during", records)
MIG_Dx_during_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_Dx_during_2<-MIG_Dx_during_2[order(year)]
rm(records,total)
MIG_Dx_during_2[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_Dx_during_2[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_Dx_during_2[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_Dx_during_2[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_during_2[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_during_2[lower_95_CI<0,lower_95_CI:=0]
fwrite(MIG_Dx_during_2,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Dx_during_year.csv"),row.names = F)
rm(MIG_Dx_during_2)

#by year group and maternal age
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by=c("year_group","maternal_age"), .N]
names(records)<-c("year_group","maternal_age", "no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by=c("year_group","maternal_age"),.N]
names(total)<-c("year_group","maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","maternal_age"), all=T)
MIG_Dx_during_3<-data.table(algorithm="MIG_Dx_during", records)
MIG_Dx_during_3[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_Dx_during_3<-MIG_Dx_during_3[order(year_group,maternal_age)]
rm(records,total)
MIG_Dx_during_3[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
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
MIG_Rx_during_dt[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_Rx_during_dt[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
MIG_Rx_during_dt[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_during_dt[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_during_dt[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_Rx_during_dt,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Rx_during.csv"),row.names = F)

#by maternal age
pregnancy_d3_mig[,maternal_age:=as.character(maternal_age)]
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id), .N, by=.(maternal_age)]
names(records)<-c("maternal_age","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id), by="maternal_age",.N]
names(total)<-c("maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by="maternal_age",all=T)
MIG_Rx_during_1<-data.table(algorithm="MIG_Rx_during", records)
MIG_Rx_during_1[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_Rx_during_1<-MIG_Rx_during_1[order(maternal_age)]
rm(records,total)
MIG_Rx_during_1[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_Rx_during_1[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_Rx_during_1[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_Rx_during_1[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_during_1[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_during_1[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_Rx_during_1,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Rx_during_age.csv"),row.names = F)
rm(MIG_Rx_during_1)


#by year
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by="year", .N]
names(records)<-c("year","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by="year",.N]
names(total)<-c("year","no_pregnancies")
records<-merge.data.table(records,total,by="year",all=T)
MIG_Rx_during_2<-data.table(algorithm="MIG_Rx_during", records)
MIG_Rx_during_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_Rx_during_2<-MIG_Rx_during_2[order(year)]
rm(records,total)
MIG_Rx_during_2[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_Rx_during_2[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_Rx_during_2[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_Rx_during_2[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_during_2[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_during_2[lower_95_CI<0,lower_95_CI:=0]
fwrite(MIG_Rx_during_2,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Rx_during_year.csv"),row.names = F)
rm(MIG_Rx_during_2)


#by year group and maternal age
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by=c("year_group","maternal_age"), .N]
names(records)<-c("year_group","maternal_age", "no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by=c("year_group","maternal_age"),.N]
names(total)<-c("year_group","maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","maternal_age"), all=T)
MIG_Rx_during_3<-data.table(algorithm="MIG_Rx_during", records)
MIG_Rx_during_3[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_Rx_during_3<-MIG_Rx_during_3[order(year_group,maternal_age)]
rm(records,total)
MIG_Rx_during_3[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
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
MIG_DxRx_during_dt[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_DxRx_during_dt[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
MIG_DxRx_during_dt[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_during_dt[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_during_dt[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_DxRx_during_dt,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_DxRx_during.csv"),row.names = F)

#by maternal age
pregnancy_d3_mig[,maternal_age:=as.character(maternal_age)]
records<-pregnancy_d3_mig[alternative==1 & !duplicated(pregnancy_id), .N, by=.(maternal_age)]
names(records)<-c("maternal_age","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id), by="maternal_age",.N]
names(total)<-c("maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by="maternal_age",all=T)
MIG_DxRx_during_1<-data.table(algorithm="MIG_DxRx_during", records)
MIG_DxRx_during_1[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_DxRx_during_1<-MIG_DxRx_during_1[order(maternal_age)]
rm(records,total)
MIG_DxRx_during_1[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_DxRx_during_1[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_DxRx_during_1[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_DxRx_during_1[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_during_1[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_during_1[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_DxRx_during_1,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_DxRx_during_age.csv"),row.names = F)
rm(MIG_DxRx_during_1)

#by year
records<-pregnancy_d3_mig[alternative==1 & !duplicated(pregnancy_id),by="year", .N]
names(records)<-c("year","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by="year",.N]
names(total)<-c("year","no_pregnancies")
records<-merge.data.table(records,total,by="year",all=T)
MIG_DxRx_during_2<-data.table(algorithm="MIG_DxRx_during", records)
MIG_DxRx_during_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_DxRx_during_2<-MIG_DxRx_during_2[order(year)]
rm(records,total)
MIG_DxRx_during_2[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_DxRx_during_2[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_DxRx_during_2[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_DxRx_during_2[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_during_2[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_during_2[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_DxRx_during_2,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_DxRx_during_year.csv"),row.names = F)
rm(MIG_DxRx_during_2)


#by year group and maternal age
records<-pregnancy_d3_mig[alternative==1 & !duplicated(pregnancy_id),by=c("year_group","maternal_age"), .N]
names(records)<-c("year_group","maternal_age", "no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id),by=c("year_group","maternal_age"),.N]
names(total)<-c("year_group","maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","maternal_age"), all=T)
MIG_DxRx_during_3<-data.table(algorithm="MIG_DxRx_during", records)
MIG_DxRx_during_3[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_DxRx_during_3<-MIG_DxRx_during_3[order(year_group,maternal_age)]
rm(records,total)
MIG_DxRx_during_3[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_DxRx_during_3[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_DxRx_during_3[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_DxRx_during_3[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_during_3[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_during_3[lower_95_CI<0,lower_95_CI:=0]

pregnancy_d3_mig[,alternative:=NULL]
fwrite(MIG_DxRx_during_3,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_DxRx_during_year_age.csv"),row.names = F)
rm(MIG_DxRx_during_3)


#### Select the trimester timepoint ####
pregnancy_d3_mig[,dif:=pregnancy_end_date-pregnancy_start_date]
pregnancy_d3_mig[dif>=97, trimester:=1]
pregnancy_d3_mig[dif>=195, trimester:=2]
pregnancy_d3_mig[dif>=280, trimester:=3]
pregnancy_d3_mig[,dif:=NULL]
#### MIG_Dx_first:Prevalence of migraine during pregnancy when lookback==12 months(3 months) ####
print("Create algorithm MIG_Dx_first")
MIG_Dx_first<-algorithm_template[NEW_STUDY_VARIABLES=="MIG_Dx_first"]
inc_col<-MIG_Dx_first[TYPE=="AND",STUDY_VARIABLES]

#set the pregnancy trimester for each pregnancy
if(length(inc_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col] & trimester==1,include:=1]}else{pregnancy_d3_mig[,include:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm/final_d3/MIG_Dx_first_D3.rds"))

#export MIG_Dx_first
MIG_Dx_first_dt<-data.table(algorithm="MIG_Dx_first", no_diagnosed_pregnancies=pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id) & trimester==1,.N])
MIG_Dx_first_dt[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_Dx_first_dt[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
MIG_Dx_first_dt[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_first_dt[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_first_dt[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_Dx_first_dt,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Dx_first.csv"),row.names = F)

#by maternal age
pregnancy_d3_mig[,maternal_age:=as.character(maternal_age)]
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id), .N, by=.(maternal_age)]
names(records)<-c("maternal_age","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id) & trimester==1, by="maternal_age",.N]
names(total)<-c("maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by="maternal_age",all=T)
MIG_Dx_first_1<-data.table(algorithm="MIG_Dx_first", records)
MIG_Dx_first_1[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_Dx_first_1<-MIG_Dx_first_1[order(maternal_age)]
rm(records,total)
MIG_Dx_first_1[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_Dx_first_1[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_Dx_first_1[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_Dx_first_1[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_first_1[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_first_1[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_Dx_first_1,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Dx_first_age.csv"),row.names = F)
rm(MIG_Dx_first_1)

#by year
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by="year", .N]
names(records)<-c("year","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id) & trimester==1,by="year",.N]
names(total)<-c("year","no_pregnancies")
records<-merge.data.table(records,total,by="year",all=T)
MIG_Dx_first_2<-data.table(algorithm="MIG_Dx_first", records)
MIG_Dx_first_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_Dx_first_2<-MIG_Dx_first_2[order(year)]
rm(records,total)
MIG_Dx_first_2[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_Dx_first_2[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_Dx_first_2[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_Dx_first_2[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_first_2[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_first_2[lower_95_CI<0,lower_95_CI:=0]
fwrite(MIG_Dx_first_2,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Dx_first_year.csv"),row.names = F)
rm(MIG_Dx_first_2)

#by year group and maternal age
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by=c("year_group","maternal_age"), .N]
names(records)<-c("year_group","maternal_age", "no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id) & trimester==1,by=c("year_group","maternal_age"),.N]
names(total)<-c("year_group","maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","maternal_age"), all=T)
MIG_Dx_first_3<-data.table(algorithm="MIG_Dx_first", records)
MIG_Dx_first_3[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_Dx_first_3<-MIG_Dx_first_3[order(year_group,maternal_age)]
rm(records,total)
MIG_Dx_first_3[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
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

if(length(inc_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col] & trimester==1,include:=1]}else{pregnancy_d3_mig[,include:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm/final_d3/MIG_Rx_first_D3.rds"))

#export MIG_Rx_first
MIG_Rx_first_dt<-data.table(algorithm="MIG_Rx_first", no_diagnosed_pregnancies=pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id) & trimester==1,.N])
MIG_Rx_first_dt[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_Rx_first_dt[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
MIG_Rx_first_dt[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_first_dt[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_first_dt[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_Rx_first_dt,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Rx_first.csv"),row.names = F)

#by maternal age
pregnancy_d3_mig[,maternal_age:=as.character(maternal_age)]
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id), .N, by=.(maternal_age)]
names(records)<-c("maternal_age","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id) & trimester==1, by="maternal_age",.N]
names(total)<-c("maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by="maternal_age",all=T)
MIG_Rx_first_1<-data.table(algorithm="MIG_Rx_first", records)
MIG_Rx_first_1[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_Rx_first_1<-MIG_Rx_first_1[order(maternal_age)]
rm(records,total)
MIG_Rx_first_1[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_Rx_first_1[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_Rx_first_1[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_Rx_first_1[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_first_1[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_first_1[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_Rx_first_1,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Rx_first_age.csv"),row.names = F)
rm(MIG_Rx_first_1)


#by year
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by="year", .N]
names(records)<-c("year","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id) & trimester==1,by="year",.N]
names(total)<-c("year","no_pregnancies")
records<-merge.data.table(records,total,by="year",all=T)
MIG_Rx_first_2<-data.table(algorithm="MIG_Rx_first", records)
MIG_Rx_first_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_Rx_first_2<-MIG_Rx_first_2[order(year)]
rm(records,total)
MIG_Rx_first_2[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_Rx_first_2[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_Rx_first_2[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_Rx_first_2[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_first_2[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_first_2[lower_95_CI<0,lower_95_CI:=0]
fwrite(MIG_Rx_first_2,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Rx_first_year.csv"),row.names = F)
rm(MIG_Rx_first_2)


#by year group and maternal age
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by=c("year_group","maternal_age"), .N]
names(records)<-c("year_group","maternal_age", "no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id) & trimester==1,by=c("year_group","maternal_age"),.N]
names(total)<-c("year_group","maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","maternal_age"), all=T)
MIG_Rx_first_3<-data.table(algorithm="MIG_Rx_first", records)
MIG_Rx_first_3[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_Rx_first_3<-MIG_Rx_first_3[order(year_group,maternal_age)]
rm(records,total)
MIG_Rx_first_3[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
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

if(length(alt_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("|" , lapply(.SD,`>=`, 1)),.SDcols=alt_col] & trimester==1,alternative:=1]}else{pregnancy_d3_mig[,alternative:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm/final_d3/MIG_DxRx_first_D3.rds"))

#export MIG_Dx_a
MIG_DxRx_first_dt<-data.table(algorithm="MIG_DxRx_first", no_diagnosed_pregnancies=pregnancy_d3_mig[alternative==1 & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id) & trimester==1,.N])
MIG_DxRx_first_dt[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_DxRx_first_dt[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
MIG_DxRx_first_dt[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_first_dt[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_first_dt[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_DxRx_first_dt,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_DxRx_first.csv"),row.names = F)

#by maternal age
pregnancy_d3_mig[,maternal_age:=as.character(maternal_age)]
records<-pregnancy_d3_mig[alternative==1 & !duplicated(pregnancy_id), .N, by=.(maternal_age)]
names(records)<-c("maternal_age","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id)& trimester==1, by="maternal_age",.N]
names(total)<-c("maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by="maternal_age",all=T)
MIG_DxRx_first_1<-data.table(algorithm="MIG_DxRx_first", records)
MIG_DxRx_first_1[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_DxRx_first_1<-MIG_DxRx_first_1[order(maternal_age)]
rm(records,total)
MIG_DxRx_first_1[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_DxRx_first_1[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_DxRx_first_1[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_DxRx_first_1[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_first_1[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_first_1[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_DxRx_first_1,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_DxRx_first_age.csv"),row.names = F)
rm(MIG_DxRx_first_1)

#by year
records<-pregnancy_d3_mig[alternative==1 & !duplicated(pregnancy_id),by="year", .N]
names(records)<-c("year","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id)& trimester==1,by="year",.N]
names(total)<-c("year","no_pregnancies")
records<-merge.data.table(records,total,by="year",all=T)
MIG_DxRx_first_2<-data.table(algorithm="MIG_DxRx_first", records)
MIG_DxRx_first_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_DxRx_first_2<-MIG_DxRx_first_2[order(year)]
rm(records,total)
MIG_DxRx_first_2[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_DxRx_first_2[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_DxRx_first_2[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_DxRx_first_2[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_first_2[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_first_2[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_DxRx_first_2,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_DxRx_first_year.csv"),row.names = F)
rm(MIG_DxRx_first_2)


#by year group and maternal age
records<-pregnancy_d3_mig[alternative==1 & !duplicated(pregnancy_id),by=c("year_group","maternal_age"), .N]
names(records)<-c("year_group","maternal_age", "no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id)& trimester==1,by=c("year_group","maternal_age"),.N]
names(total)<-c("year_group","maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","maternal_age"), all=T)
MIG_DxRx_first_3<-data.table(algorithm="MIG_DxRx_first", records)
MIG_DxRx_first_3[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_DxRx_first_3<-MIG_DxRx_first_3[order(year_group,maternal_age)]
rm(records,total)
MIG_DxRx_first_3[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
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
if(length(inc_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col] & trimester <= 2,include:=1]}else{pregnancy_d3_mig[,include:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm/final_d3/MIG_Dx_second_D3.rds"))

#export MIG_Dx_second
MIG_Dx_second_dt<-data.table(algorithm="MIG_Dx_second", no_diagnosed_pregnancies=pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id) & trimester <= 2,.N])
MIG_Dx_second_dt[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_Dx_second_dt[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
MIG_Dx_second_dt[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_second_dt[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_second_dt[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_Dx_second_dt,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Dx_second.csv"),row.names = F)

#by maternal age
pregnancy_d3_mig[,maternal_age:=as.character(maternal_age)]
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id), .N, by=.(maternal_age)]
names(records)<-c("maternal_age","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id) & trimester <= 2, by="maternal_age",.N]
names(total)<-c("maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by="maternal_age",all=T)
MIG_Dx_second_1<-data.table(algorithm="MIG_Dx_second", records)
MIG_Dx_second_1[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_Dx_second_1<-MIG_Dx_second_1[order(maternal_age)]
rm(records,total)
MIG_Dx_second_1[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_Dx_second_1[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_Dx_second_1[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_Dx_second_1[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_second_1[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_second_1[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_Dx_second_1,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Dx_second_age.csv"),row.names = F)
rm(MIG_Dx_second_1)

#by year
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by="year", .N]
names(records)<-c("year","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id) & trimester <= 2,by="year",.N]
names(total)<-c("year","no_pregnancies")
records<-merge.data.table(records,total,by="year",all=T)
MIG_Dx_second_2<-data.table(algorithm="MIG_Dx_second", records)
MIG_Dx_second_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_Dx_second_2<-MIG_Dx_second_2[order(year)]
rm(records,total)
MIG_Dx_second_2[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_Dx_second_2[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_Dx_second_2[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_Dx_second_2[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_second_2[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_second_2[lower_95_CI<0,lower_95_CI:=0]
fwrite(MIG_Dx_second_2,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Dx_second_year.csv"),row.names = F)
rm(MIG_Dx_second_2)

#by year group and maternal age
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by=c("year_group","maternal_age"), .N]
names(records)<-c("year_group","maternal_age", "no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id) & trimester <= 2,by=c("year_group","maternal_age"),.N]
names(total)<-c("year_group","maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","maternal_age"), all=T)
MIG_Dx_second_3<-data.table(algorithm="MIG_Dx_second", records)
MIG_Dx_second_3[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_Dx_second_3<-MIG_Dx_second_3[order(year_group,maternal_age)]
rm(records,total)
MIG_Dx_second_3[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
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

if(length(inc_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col] & trimester <= 2,include:=1]}else{pregnancy_d3_mig[,include:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm/final_d3/MIG_Rx_second_D3.rds"))

#export MIG_Rx_second
MIG_Rx_second_dt<-data.table(algorithm="MIG_Rx_second", no_diagnosed_pregnancies=pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id) & trimester<= 2,.N])
MIG_Rx_second_dt[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_Rx_second_dt[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
MIG_Rx_second_dt[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_second_dt[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_second_dt[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_Rx_second_dt,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Rx_second.csv"),row.names = F)

#by maternal age
pregnancy_d3_mig[,maternal_age:=as.character(maternal_age)]
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id), .N, by=.(maternal_age)]
names(records)<-c("maternal_age","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id) & trimester<= 2, by="maternal_age",.N]
names(total)<-c("maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by="maternal_age",all=T)
MIG_Rx_second_1<-data.table(algorithm="MIG_Rx_second", records)
MIG_Rx_second_1[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_Rx_second_1<-MIG_Rx_second_1[order(maternal_age)]
rm(records,total)
MIG_Rx_second_1[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_Rx_second_1[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_Rx_second_1[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_Rx_second_1[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_second_1[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_second_1[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_Rx_second_1,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Rx_second_age.csv"),row.names = F)
rm(MIG_Rx_second_1)


#by year
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by="year", .N]
names(records)<-c("year","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id) & trimester<= 2,by="year",.N]
names(total)<-c("year","no_pregnancies")
records<-merge.data.table(records,total,by="year",all=T)
MIG_Rx_second_2<-data.table(algorithm="MIG_Rx_second", records)
MIG_Rx_second_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_Rx_second_2<-MIG_Rx_second_2[order(year)]
rm(records,total)
MIG_Rx_second_2[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_Rx_second_2[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_Rx_second_2[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_Rx_second_2[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_second_2[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_second_2[lower_95_CI<0,lower_95_CI:=0]
fwrite(MIG_Rx_second_2,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Rx_second_year.csv"),row.names = F)
rm(MIG_Rx_second_2)


#by year group and maternal age
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by=c("year_group","maternal_age"), .N]
names(records)<-c("year_group","maternal_age", "no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id) & trimester<= 2,by=c("year_group","maternal_age"),.N]
names(total)<-c("year_group","maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","maternal_age"), all=T)
MIG_Rx_second_3<-data.table(algorithm="MIG_Rx_second", records)
MIG_Rx_second_3[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_Rx_second_3<-MIG_Rx_second_3[order(year_group,maternal_age)]
rm(records,total)
MIG_Rx_second_3[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
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

if(length(alt_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("|" , lapply(.SD,`>=`, 1)),.SDcols=alt_col] & trimester<=2,alternative:=1]}else{pregnancy_d3_mig[,alternative:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm/final_d3/MIG_DxRx_second_D3.rds"))

#export MIG_Dx_a
MIG_DxRx_second_dt<-data.table(algorithm="MIG_DxRx_second", no_diagnosed_pregnancies=pregnancy_d3_mig[alternative==1 & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id) & trimester<=2,.N])
MIG_DxRx_second_dt[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_DxRx_second_dt[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
MIG_DxRx_second_dt[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_second_dt[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_second_dt[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_DxRx_second_dt,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_DxRx_second.csv"),row.names = F)

#by maternal age
pregnancy_d3_mig[,maternal_age:=as.character(maternal_age)]
records<-pregnancy_d3_mig[alternative==1 & !duplicated(pregnancy_id), .N, by=.(maternal_age)]
names(records)<-c("maternal_age","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id)& trimester<=2, by="maternal_age",.N]
names(total)<-c("maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by="maternal_age",all=T)
MIG_DxRx_second_1<-data.table(algorithm="MIG_DxRx_second", records)
MIG_DxRx_second_1[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_DxRx_second_1<-MIG_DxRx_second_1[order(maternal_age)]
rm(records,total)
MIG_DxRx_second_1[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_DxRx_second_1[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_DxRx_second_1[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_DxRx_second_1[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_second_1[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_second_1[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_DxRx_second_1,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_DxRx_second_age.csv"),row.names = F)
rm(MIG_DxRx_second_1)

#by year
records<-pregnancy_d3_mig[alternative==1 & !duplicated(pregnancy_id),by="year", .N]
names(records)<-c("year","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id)& trimester<=2,by="year",.N]
names(total)<-c("year","no_pregnancies")
records<-merge.data.table(records,total,by="year",all=T)
MIG_DxRx_second_2<-data.table(algorithm="MIG_DxRx_second", records)
MIG_DxRx_second_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_DxRx_second_2<-MIG_DxRx_second_2[order(year)]
rm(records,total)
MIG_DxRx_second_2[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_DxRx_second_2[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_DxRx_second_2[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_DxRx_second_2[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_second_2[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_second_2[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_DxRx_second_2,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_DxRx_second_year.csv"),row.names = F)
rm(MIG_DxRx_second_2)


#by year group and maternal age
records<-pregnancy_d3_mig[alternative==1 & !duplicated(pregnancy_id),by=c("year_group","maternal_age"), .N]
names(records)<-c("year_group","maternal_age", "no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id)& trimester<=2,by=c("year_group","maternal_age"),.N]
names(total)<-c("year_group","maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","maternal_age"), all=T)
MIG_DxRx_second_3<-data.table(algorithm="MIG_DxRx_second", records)
MIG_DxRx_second_3[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_DxRx_second_3<-MIG_DxRx_second_3[order(year_group,maternal_age)]
rm(records,total)
MIG_DxRx_second_3[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
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
if(length(inc_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col] & trimester <= 3,include:=1]}else{pregnancy_d3_mig[,include:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm/final_d3/MIG_Dx_third_D3.rds"))

#export MIG_Dx_third
MIG_Dx_third_dt<-data.table(algorithm="MIG_Dx_third", no_diagnosed_pregnancies=pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id) & trimester <= 3,.N])
MIG_Dx_third_dt[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_Dx_third_dt[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
MIG_Dx_third_dt[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_third_dt[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_third_dt[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_Dx_third_dt,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Dx_third.csv"),row.names = F)

#by maternal age
pregnancy_d3_mig[,maternal_age:=as.character(maternal_age)]
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id), .N, by=.(maternal_age)]
names(records)<-c("maternal_age","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id) & trimester <= 3, by="maternal_age",.N]
names(total)<-c("maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by="maternal_age",all=T)
MIG_Dx_third_1<-data.table(algorithm="MIG_Dx_third", records)
MIG_Dx_third_1[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_Dx_third_1<-MIG_Dx_third_1[order(maternal_age)]
rm(records,total)
MIG_Dx_third_1[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_Dx_third_1[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_Dx_third_1[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_Dx_third_1[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_third_1[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_third_1[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_Dx_third_1,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Dx_third_age.csv"),row.names = F)
rm(MIG_Dx_third_1)

#by year
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by="year", .N]
names(records)<-c("year","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id) & trimester <= 3,by="year",.N]
names(total)<-c("year","no_pregnancies")
records<-merge.data.table(records,total,by="year",all=T)
MIG_Dx_third_2<-data.table(algorithm="MIG_Dx_third", records)
MIG_Dx_third_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_Dx_third_2<-MIG_Dx_third_2[order(year)]
rm(records,total)
MIG_Dx_third_2[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_Dx_third_2[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_Dx_third_2[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_Dx_third_2[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_third_2[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Dx_third_2[lower_95_CI<0,lower_95_CI:=0]
fwrite(MIG_Dx_third_2,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Dx_third_year.csv"),row.names = F)
rm(MIG_Dx_third_2)

#by year group and maternal age
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by=c("year_group","maternal_age"), .N]
names(records)<-c("year_group","maternal_age", "no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id) & trimester <= 3,by=c("year_group","maternal_age"),.N]
names(total)<-c("year_group","maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","maternal_age"), all=T)
MIG_Dx_third_3<-data.table(algorithm="MIG_Dx_third", records)
MIG_Dx_third_3[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_Dx_third_3<-MIG_Dx_third_3[order(year_group,maternal_age)]
rm(records,total)
MIG_Dx_third_3[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
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

if(length(inc_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("&" , lapply(.SD,`>=`, 1)),.SDcols=inc_col] & trimester <= 3,include:=1]}else{pregnancy_d3_mig[,include:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm/final_d3/MIG_Rx_third_D3.rds"))

#export MIG_Rx_third
MIG_Rx_third_dt<-data.table(algorithm="MIG_Rx_third", no_diagnosed_pregnancies=pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id) & trimester<= 3,.N])
MIG_Rx_third_dt[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_Rx_third_dt[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
MIG_Rx_third_dt[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_third_dt[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_third_dt[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_Rx_third_dt,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Rx_third.csv"),row.names = F)

#by maternal age
pregnancy_d3_mig[,maternal_age:=as.character(maternal_age)]
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id), .N, by=.(maternal_age)]
names(records)<-c("maternal_age","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id) & trimester<= 3, by="maternal_age",.N]
names(total)<-c("maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by="maternal_age",all=T)
MIG_Rx_third_1<-data.table(algorithm="MIG_Rx_third", records)
MIG_Rx_third_1[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_Rx_third_1<-MIG_Rx_third_1[order(maternal_age)]
rm(records,total)
MIG_Rx_third_1[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_Rx_third_1[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_Rx_third_1[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_Rx_third_1[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_third_1[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_third_1[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_Rx_third_1,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Rx_third_age.csv"),row.names = F)
rm(MIG_Rx_third_1)


#by year
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by="year", .N]
names(records)<-c("year","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id) & trimester<= 3,by="year",.N]
names(total)<-c("year","no_pregnancies")
records<-merge.data.table(records,total,by="year",all=T)
MIG_Rx_third_2<-data.table(algorithm="MIG_Rx_third", records)
MIG_Rx_third_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_Rx_third_2<-MIG_Rx_third_2[order(year)]
rm(records,total)
MIG_Rx_third_2[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_Rx_third_2[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_Rx_third_2[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_Rx_third_2[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_third_2[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_Rx_third_2[lower_95_CI<0,lower_95_CI:=0]
fwrite(MIG_Rx_third_2,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_Rx_third_year.csv"),row.names = F)
rm(MIG_Rx_third_2)


#by year group and maternal age
records<-pregnancy_d3_mig[include==1 & !duplicated(pregnancy_id),by=c("year_group","maternal_age"), .N]
names(records)<-c("year_group","maternal_age", "no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id) & trimester<= 3,by=c("year_group","maternal_age"),.N]
names(total)<-c("year_group","maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","maternal_age"), all=T)
MIG_Rx_third_3<-data.table(algorithm="MIG_Rx_third", records)
MIG_Rx_third_3[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_Rx_third_3<-MIG_Rx_third_3[order(year_group,maternal_age)]
rm(records,total)
MIG_Rx_third_3[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
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

if(length(alt_col)>0){pregnancy_d3_mig[pregnancy_d3_mig[,Reduce("|" , lapply(.SD,`>=`, 1)),.SDcols=alt_col] & trimester<=3,alternative:=1]}else{pregnancy_d3_mig[,alternative:=NA]}

saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm/final_d3/MIG_DxRx_third_D3.rds"))

#export MIG_Dx_a
MIG_DxRx_third_dt<-data.table(algorithm="MIG_DxRx_third", no_diagnosed_pregnancies=pregnancy_d3_mig[alternative==1 & !duplicated(pregnancy_id),.N], no_pregnancies=pregnancy_d3_mig[!duplicated(pregnancy_id) & trimester<=3,.N])
MIG_DxRx_third_dt[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_DxRx_third_dt[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
#lower CI
MIG_DxRx_third_dt[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_third_dt[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_third_dt[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_DxRx_third_dt,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_DxRx_third.csv"),row.names = F)

#by maternal age
pregnancy_d3_mig[,maternal_age:=as.character(maternal_age)]
records<-pregnancy_d3_mig[alternative==1 & !duplicated(pregnancy_id), .N, by=.(maternal_age)]
names(records)<-c("maternal_age","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id)& trimester<=3, by="maternal_age",.N]
names(total)<-c("maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by="maternal_age",all=T)
MIG_DxRx_third_1<-data.table(algorithm="MIG_DxRx_third", records)
MIG_DxRx_third_1[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_DxRx_third_1<-MIG_DxRx_third_1[order(maternal_age)]
rm(records,total)
MIG_DxRx_third_1[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_DxRx_third_1[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_DxRx_third_1[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_DxRx_third_1[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_third_1[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_third_1[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_DxRx_third_1,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_DxRx_third_age.csv"),row.names = F)
rm(MIG_DxRx_third_1)

#by year
records<-pregnancy_d3_mig[alternative==1 & !duplicated(pregnancy_id),by="year", .N]
names(records)<-c("year","no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id)& trimester<=3,by="year",.N]
names(total)<-c("year","no_pregnancies")
records<-merge.data.table(records,total,by="year",all=T)
MIG_DxRx_third_2<-data.table(algorithm="MIG_DxRx_third", records)
MIG_DxRx_third_2[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_DxRx_third_2<-MIG_DxRx_third_2[order(year)]
rm(records,total)
MIG_DxRx_third_2[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_DxRx_third_2[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_DxRx_third_2[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_DxRx_third_2[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_third_2[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_third_2[lower_95_CI<0,lower_95_CI:=0]

fwrite(MIG_DxRx_third_2,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_DxRx_third_year.csv"),row.names = F)
rm(MIG_DxRx_third_2)


#by year group and maternal age
records<-pregnancy_d3_mig[alternative==1 & !duplicated(pregnancy_id),by=c("year_group","maternal_age"), .N]
names(records)<-c("year_group","maternal_age", "no_diagnosed_pregnancies")
total<-pregnancy_d3_mig[!duplicated(pregnancy_id)& trimester<=3,by=c("year_group","maternal_age"),.N]
names(total)<-c("year_group","maternal_age","no_pregnancies")
records<-merge.data.table(records,total,by=c("year_group","maternal_age"), all=T)
MIG_DxRx_third_3<-data.table(algorithm="MIG_DxRx_third", records)
MIG_DxRx_third_3[is.na(no_diagnosed_pregnancies),no_diagnosed_pregnancies:=0]
MIG_DxRx_third_3<-MIG_DxRx_third_3[order(year_group,maternal_age)]
rm(records,total)
MIG_DxRx_third_3[,prevalence_100_pregnancies:=round((no_diagnosed_pregnancies/no_pregnancies)*100,1)]
MIG_DxRx_third_3[,no_diagnosed_pregnancies:=as.numeric(no_diagnosed_pregnancies)][,no_pregnancies:=as.numeric(no_pregnancies)]
MIG_DxRx_third_3[no_diagnosed_pregnancies != 0 & prevalence_100_pregnancies==0, prevalence_100_pregnancies:=0.0001]
#lower CI
MIG_DxRx_third_3[,lower_95_CI:=lower_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_third_3[,upper_95_CI:=upper_ci(no_diagnosed_pregnancies,no_pregnancies,prevalence_100_pregnancies)]
MIG_DxRx_third_3[lower_95_CI<0,lower_95_CI:=0]

pregnancy_d3_mig[,alternative:=NULL]
fwrite(MIG_DxRx_third_3,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_MIG_DxRx_third_year_age.csv"),row.names = F)
rm(MIG_DxRx_third_3)




date_running_end_04_c<-Sys.Date()
end_time_04_c<-Sys.time()

time_log_04_c<-data.table(DAP=data_access_provider_name,
                          Script="Step_04_c_migraine_algorithms.R", 
                          Start_date=date_running_start_04_c, 
                          End_date=date_running_end_04_c,
                          Time_elaspsed=format(end_time_04_c-initial_time_04_c, digits=2))
fwrite(time_log_04_c,paste0(output_dir,"/Time log/Step_04_c_time_log.csv"),row.names = F)
rm(time_log_04_c)
