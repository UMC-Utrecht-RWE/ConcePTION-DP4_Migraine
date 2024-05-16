initial_time_04_c<-Sys.time()
date_running_start_04_c<-Sys.Date()

orig_mig<-pregnancy_d3_mig[,.N]

#### Load data from additional conceptsets for triptan injection identification ####
additional_concepts_file<-list.files(paste0(projectFolder,"/p_parameters/"), "additional_concepts")
additional_concepts<-read_excel(paste0(projectFolder,"/p_parameters/",additional_concepts_file),col_types = "text")
additional_concepts<-as.data.table(additional_concepts)
#Keep only needed information based on the DAP
additional_concepts<-additional_concepts[DAP_NAME==data_access_provider_name]
if(additional_concepts[,.N]==0){
  stop("This is not a script issue. There is no data for your data source in additional_concepts. Fix the issue and then rerun the script.")
}
#Keep data only about sumatriptan injections
additional_concepts<-additional_concepts[StudyVar=="Sumatriptan_inj"]
#keep only value_2 as it contains data about the identification code
additional_concepts<-additional_concepts[,.(val_1)]
setnames(additional_concepts,"val_1","medicinal_product_id")
additional_concepts[,injection:=1]
additional_concepts[,atc_code:="N02CC01"]

#### Migraine check box clean up: Combine data from Migraine_checkbox and Migraine_checkbox_cat ####
migraine_checkbox_files<-list.files(paste0(projectFolder,"/g_intermediate/migraine_algorithm/"),"Migraine_checkbox")
if(length(migraine_checkbox_files)>0){
  migraine_checkbox<-lapply(paste0(projectFolder,"/g_intermediate/migraine_algorithm/", migraine_checkbox_files), readRDS)
  migraine_checkbox<-as.data.table(do.call(rbind,migraine_checkbox))
  #Change conditio to Migraine for all
  migraine_checkbox[,condition:="Migraine_checkbox"]
  #remove duplicates
  migraine_checkbox[,comb:=paste0(person_id,"_",event_date)]
  migraine_checkbox<-migraine_checkbox[!duplicated(comb)]
  migraine_checkbox[,comb:=NULL]
  
  #Save old files inside a new folder
  if("raw_data" %in% list.files(paste0(projectFolder,"/g_intermediate/migraine_algorithm/"))){
    unlink(paste0(projectFolder,"/g_intermediate/migraine_algorithm/raw_data"), recursive = T)
  }
  dir.create(paste0(projectFolder, "/g_intermediate/migraine_algorithm/raw_data"))
  #remove all pe files from tmp
  for (i in 1:length(migraine_checkbox_files)){
    file.copy(paste0(projectFolder,"/g_intermediate/migraine_algorithm/", migraine_checkbox_files[[i]]),paste0(projectFolder,"/g_intermediate/migraine_algorithm/raw_data/", migraine_checkbox_files[[i]]),overwrite = T)
    file.remove(paste0(projectFolder,"/g_intermediate/migraine_algorithm/", migraine_checkbox_files[[i]]))
  }
  
  saveRDS(migraine_checkbox,paste0(projectFolder,"/g_intermediate/migraine_algorithm/Migraine_checkbox.rds"))
  rm(migraine_checkbox)
}

####Migraine DIAGNOSES FILES####
print("Loading all Migraine diagnoses D3 and merge with the pregnancy D3.")
if(DAP_name == "CHUT"){
  obs_period_diag<-data.table(StudyVar=c("MG","MG_NO_AURA","MG_AURA","MG_STACOMP","MG_OTHER","MG_UNSP","MG_UPC", "Migraine_checkbox"),
                              lookback=c(2.5*30.25,2.5*30.25,2.5*30.25,2.5*30.25,2.5*30.25,2.5*30.25,2.5*30.25,2.5*30.25),
                              start_date=c(0,0,0,0,0,0,0,0),
                              end_date=c(7*40,7*40,7*40,7*40,7*40,7*40,7*40,7*40),
                              after=c(0,0,0,0,0,0,0,0))  
}else{
  obs_period_diag<-data.table(StudyVar=c("MG","MG_NO_AURA","MG_AURA","MG_OTHER","MG_UNSP","MG_STACOMP","MG_UPC","Migraine_checkbox"),
                              lookback=c(365.25,365.25,365.25,365.25,365.25,365.25,365.25,5),
                              start_date=c(0,0,0,0,0,0,0,0),
                              end_date=c(7*40,7*40,7*40,7*40,7*40,7*40,7*40,7*40),
                              after=c(0,0,0,0,0,0,0,0))
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
names_events<-names_events[!names_events %in% "raw_data"]
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
      #mig_dt[,end:=obs_period_diag[StudyVar==names_mg,after]]
      mig_dt[,end_preg:=as.IDate(pregnancy_end_date)]
      mig_dt[,diff:=event_date-end_preg]
      mig_dt<-mig_dt[diff<=0]
      mig_dt[,diff:=NULL][,end_preg:=NULL]
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
    rm(preg_diag)
    prior_diag[is.na(prior_diag),prior_diag:=0]
    prior_diag[is.na(preg_diag),preg_diag:=0]
    prior_diag<-prior_diag[prior_diag==0 & preg_diag>0]
    #Count the number of cases to exclude
    excluded_prior_diag<-data.table(event_definition=names_mg, excluded_records=prior_diag[,.N])
    prior_diag[,prior_diag:=NULL]
    #Add the identifier pregnancy id to the main pregnancy D3
    pregnancy_d3_mig<-merge.data.table(pregnancy_d3_mig,prior_diag, by=c("person_id","pregnancy_id"), all.x=T)
    rm(prior_diag)
    #update the exclude variable
    pregnancy_d3_mig[preg_diag>0, onset_diag:=1]
    pregnancy_d3_mig[,preg_diag:=NULL]
    pregnancy_d3_mig[is.na(onset_diag), onset_diag:=0]
  }
}
rm(mig_dt) 


####Migraine MEDICINES files####
print("Loading all Migraine medicines D3 and merge with the pregnancy D3.")

if (DAP_name == "NIHW"){
  obs_period_med<-data.table(StudyVar=c("Migraine_medicines"),
                             lookback=c(3*30.25),
                             start_date=c(0),
                             end_date=c(40*7),
                             after=c(0))}
if (DAP_name == "CHUT"){
  obs_period_med<-data.table(StudyVar=c("Migraine_medicines"),
                             lookback=c(2.5*30.25),
                             start_date=c(0),
                             end_date=c(40*7),
                             after=c(0))}

if (!DAP_name %in% c("NIHW", "CHUT")){
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
        #mig_med[,end:=obs_period_med[med_fl,end_date]]
        mig_med[,end_preg:=as.IDate(pregnancy_end_date)]
        mig_med[,diff:=medicine_date-end_preg]
        mig_med<-mig_med[diff<=0]
        mig_med[,diff:=NULL][,end_preg:=NULL]
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
      rm(preg_med)
      prior_med[is.na(prior_med),prior_med:=0]
      prior_med[is.na(preg_med),preg_med:=0]
      prior_med<-prior_med[prior_med==0 & preg_med>0]
      
      #Count the number of cases to exclude
      excluded_prior_med<-data.table(event_definition="Triptan_prescription(N02CC)", excluded_records=prior_med[,.N])
      prior_med[,prior_med:=NULL]
      #Add the identifier pregnancy id to the main pregnancy D3
      pregnancy_d3_mig<-merge.data.table(pregnancy_d3_mig,prior_med, by=c("person_id","pregnancy_id"), all.x=T)
      rm(prior_med)
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
#Create another copy of the pregnancy D3 that will be used for Migraine_checkbox
file.copy(paste0(projectFolder,"/g_intermediate/pregnancy_d3/MIG_Pregnancy_D3.rds"),paste0(projectFolder,"/g_intermediate/pregnancy_d3/MIGcheckbox_Pregnancy_D3.rds"))
file.remove(paste0(projectFolder,"/g_intermediate/pregnancy_d3/MIG_Pregnancy_D3.rds"))
saveRDS(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/pregnancy_d3/MIG_Pregnancy_D3.rds"))
####Migraine DIAGNOSES####
original<-list()
before<-list()
after<-list()
sum<-list()

dir.create(paste0(projectFolder,"/g_intermediate/migraine_algorithm/final_d3"))

mig_files_checkbox<-mig_files[grepl("Migraine_checkbox", mig_files)]
if(length(mig_files_checkbox)>0){mig_files<-mig_files[!mig_files %in% mig_files_checkbox]}
mig_files<-mig_files[!mig_files %in% "raw_data"]

names_events<-names_events[!names_events %in% "Migraine_checkbox"]
names_events<-names_events[!names_events %in% "raw_data"]
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
        #mig_dt[,end:=obs_period_diag[StudyVar==names_events[mig_fl],end_date]]
        mig_dt[,end_preg:=as.IDate(pregnancy_end_date)]
        mig_dt[,diff:=event_date-end_preg]
        after[[w]]<-data.table(StudyVar=names_events[mig_fl], after_end=mig_dt[diff>0,.N])
        mig_dt<-mig_dt[diff<=0]
        mig_dt[,diff:=NULL][,end_preg:=NULL]
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

        #Identify all baseline events except for Migraine checkbox as the during calculation will be used
        mig_dt[,dif:=event_date-pregnancy_start_date]
        mig_dt[dif<0,paste0(names_events[mig_fl],"_baseline"):=1]
        mig_baseline<-mig_dt[get(paste0(names_events[mig_fl],"_baseline"))==1,lapply(.SD, sum),.SDcols = paste0(names_events[mig_fl],"_baseline"), by=c("person_id","pregnancy_id","pregnancy_start_date","pregnancy_end_date")]
        mig_dt[,dif:=NULL][,eval(paste0(names_events[mig_fl],"_baseline")):=NULL]
        pregnancy_d3_mig<-merge.data.table(pregnancy_d3_mig,mig_baseline,all.x=T, by=cols[!cols %in% "event_date"])
        rm(mig_baseline)
        pregnancy_d3_mig[is.na(get(paste0(names_events[mig_fl],"_baseline"))),eval(paste0(names_events[mig_fl],"_baseline")):=0]

      if(names_events[mig_fl] != "Migraine_checkbox"){ 
        #depending on the DAP
        if(!DAP_name %in% c("CHUT")){
          mig_dt[,start:=pregnancy_start_date - 3*30.25] 
          mig_dt[event_date>=start & event_date<pregnancy_start_date,paste0(names_events[mig_fl],"_baseline_2"):=1]
          mig_baseline_2<-mig_dt[get(paste0(names_events[mig_fl],"_baseline_2"))==1,lapply(.SD, sum),.SDcols = paste0(names_events[mig_fl],"_baseline_2"), by=c("person_id","pregnancy_id","pregnancy_start_date","pregnancy_end_date")]
          mig_dt[,start:=NULL][,eval(paste0(names_events[mig_fl],"_baseline_2")):=NULL]
          pregnancy_d3_mig<-merge.data.table(pregnancy_d3_mig,mig_baseline_2,all.x=T, by=cols[!cols %in% "event_date"])
          rm(mig_baseline_2)
          pregnancy_d3_mig[is.na(get(paste0(names_events[mig_fl],"_baseline_2"))),eval(paste0(names_events[mig_fl],"_baseline_2")):=0]
        }else{
          pregnancy_d3_mig[,eval(paste0(names_events[mig_fl],"_baseline_2")):=get(paste0(names_events[mig_fl],"_baseline"))]
        }
      }
      #during pregnancy
      mig_dt[event_date>=pregnancy_start_date & event_date<=pregnancy_end_date,paste0(names_events[mig_fl],"_during"):=1]
      mig_during<-mig_dt[get(paste0(names_events[mig_fl],"_during"))==1,lapply(.SD, sum),.SDcols = paste0(names_events[mig_fl],"_during"), by=c("person_id","pregnancy_id","pregnancy_start_date","pregnancy_end_date")]
      mig_dt[,eval(paste0(names_events[mig_fl],"_during")):=NULL]
      pregnancy_d3_mig<-merge.data.table(pregnancy_d3_mig,mig_during,all.x=T, by=cols[!cols %in% "event_date"])
      rm(mig_during)
      pregnancy_d3_mig[is.na(get(paste0(names_events[mig_fl],"_during"))),eval(paste0(names_events[mig_fl],"_during")):=0]
      #first
      mig_dt[,start:=pregnancy_start_date + trimester_timepoint[Indicator=="first",as.numeric(start)]][,end:=pregnancy_start_date + trimester_timepoint[Indicator=="first",as.numeric(end)]]
      mig_dt[event_date>=start & event_date<end,paste0(names_events[mig_fl],"_first"):=1]
      mig_first<-mig_dt[get(paste0(names_events[mig_fl],"_first"))==1,lapply(.SD, sum),.SDcols = paste0(names_events[mig_fl],"_first"), by=c("person_id","pregnancy_id","pregnancy_start_date","pregnancy_end_date")]
      mig_dt[,eval(paste0(names_events[mig_fl],"_first")):=NULL]
      pregnancy_d3_mig<-merge.data.table(pregnancy_d3_mig,mig_first,all.x=T, by=cols[!cols %in% "event_date"])
      rm(mig_first)
      pregnancy_d3_mig[is.na(get(paste0(names_events[mig_fl],"_first"))),eval(paste0(names_events[mig_fl],"_first")):=0]
      #second
      mig_dt[,start:=pregnancy_start_date + trimester_timepoint[Indicator=="second",as.numeric(start)]][,end:=pregnancy_start_date + trimester_timepoint[Indicator=="second",as.numeric(end)]]
      mig_dt[event_date>=start & event_date<end,paste0(names_events[mig_fl],"_second"):=1]
      mig_second<-mig_dt[get(paste0(names_events[mig_fl],"_second"))==1,lapply(.SD, sum),.SDcols = paste0(names_events[mig_fl],"_second"), by=c("person_id","pregnancy_id","pregnancy_start_date","pregnancy_end_date")]
      mig_dt[,eval(paste0(names_events[mig_fl],"_second")):=NULL]
      pregnancy_d3_mig<-merge.data.table(pregnancy_d3_mig,mig_second,all.x=T, by=cols[!cols %in% "event_date"])
      rm(mig_second)
      pregnancy_d3_mig[is.na(get(paste0(names_events[mig_fl],"_second"))),eval(paste0(names_events[mig_fl],"_second")):=0]
      #third
      mig_dt[,start:=pregnancy_start_date + trimester_timepoint[Indicator=="third",as.numeric(start)]][,end:=pregnancy_start_date + trimester_timepoint[Indicator=="third",as.numeric(end)]]
      mig_dt[event_date>=start & event_date<end,paste0(names_events[mig_fl],"_third"):=1]
      mig_third<-mig_dt[get(paste0(names_events[mig_fl],"_third"))==1,lapply(.SD, sum),.SDcols = paste0(names_events[mig_fl],"_third"), by=c("person_id","pregnancy_id","pregnancy_start_date","pregnancy_end_date")]
      mig_dt[,eval(paste0(names_events[mig_fl],"_third")):=NULL]
      pregnancy_d3_mig<-merge.data.table(pregnancy_d3_mig,mig_third,all.x=T, by=cols[!cols %in% "event_date"])
      rm(mig_third)
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


if(length(mig_files_checkbox)>0){
w<-w+1
#load the pregnancy d3 for migraine checkbox
preg_d3_checkbox<-as.data.table(readRDS(paste0(projectFolder,"/g_intermediate/pregnancy_d3/MIGcheckbox_Pregnancy_D3.rds")))
#remove uneccesary variables
if("gdm_pe_filter" %in% names(preg_d3_checkbox)){preg_d3_checkbox[,gdm_pe_filter:=NULL]}
if("du_filter" %in% names(preg_d3_checkbox)){preg_d3_checkbox[,du_filter:=NULL]}
if("saf_filter" %in% names(preg_d3_checkbox)){preg_d3_checkbox[,saf_filter:=NULL]}
if("mig_filter" %in% names(preg_d3_checkbox)){preg_d3_checkbox[,mig_filter:=NULL]}
if("op_end_date_gdm_pe" %in% names(preg_d3_checkbox)){preg_d3_checkbox[,op_end_date_gdm_pe:=NULL]}
if("op_end_date_du" %in% names(preg_d3_checkbox)){preg_d3_checkbox[,op_end_date_du:=NULL]}
if("op_end_date_saf" %in% names(preg_d3_checkbox)){preg_d3_checkbox[,op_end_date_saf:=NULL]}
if("op_start_date_gdm_pe" %in% names(preg_d3_checkbox)){preg_d3_checkbox[,op_start_date_gdm_pe:=NULL]}
if("op_start_date_du" %in% names(preg_d3_checkbox)){preg_d3_checkbox[,op_start_date_du:=NULL]}
if("op_start_date_saf" %in% names(preg_d3_checkbox)){preg_d3_checkbox[,op_start_date_saf:=NULL]}
if("sex_at_instance_creation" %in% names(preg_d3_checkbox)){preg_d3_checkbox[,sex_at_instance_creation:=NULL]}
preg_d3_checkbox[,age:=floor((pregnancy_start_date-birth_date)/365.25)]
preg_d3_checkbox[,maternal_age:=as.character(lapply(age, age_band_creation))]
preg_d3_checkbox[,year:=year(pregnancy_start_date)]
preg_d3_checkbox[,year_group:=as.character(lapply(year, year_group_creation))]

for(mig_fl in 1:length(mig_files_checkbox)){
  mig_dt<-readRDS(paste0(projectFolder,"/g_intermediate/migraine_algorithm/", mig_files_checkbox[mig_fl]))
  #merge with the pregnancy d3
  mig_dt<-merge.data.table(preg_d3_checkbox, mig_dt, by="person_id", all.x=T, allow.cartesian = T)
  mig_dt<-mig_dt[!is.na(event_date)]
  if(mig_dt[,.N]>0){
    original[[w]]<-data.table(StudyVar="Migraine_checkbox", event_records=mig_dt[,.N])
    mig_dt[,event_date:=as.IDate(event_date)]
    
    if(obs_period_diag[StudyVar=="Migraine_checkbox",lookback]>0){
      mig_dt[,lookback:=obs_period_diag[StudyVar=="Migraine_checkbox",lookback]]
      mig_dt[,start_preg:=as.IDate(pregnancy_start_date-lookback)]
      #exclude all events that are outside observation period of interest
      mig_dt[,diff:=event_date-start_preg]
      #remove all records with date before start date pregnancy+lookback
      before[[w]]<-data.table(StudyVar="Migraine_checkbox", before_start=mig_dt[diff<0,.N])
      mig_dt<-mig_dt[diff>=0]
      mig_dt[,diff:=NULL][,lookback:=NULL][,start_preg:=NULL]
    }else{
      #remove all records before start obs
      mig_dt[,start:=obs_period_diag[StudyVar=="Migraine_checkbox",start_date]]
      mig_dt[,start_preg:=as.IDate(pregnancy_start_date+start)]
      mig_dt[,diff:=event_date-start_preg]
      before[[w]]<-data.table(StudyVar="Migraine_checkbox", before_start=mig_dt[diff<0,.N])
      mig_dt<-mig_dt[diff>=0]
      mig_dt[,diff:=NULL][,start:=NULL][,start_preg:=NULL]
    }
    
    if(mig_dt[,.N]>0){
      if(obs_period_diag[StudyVar=="Migraine_checkbox",after]>0){
        mig_dt[,after:=obs_period_diag[StudyVar=="Migraine_checkbox",after] + 5 ]
        mig_dt[,end_preg:=as.IDate(pregnancy_end_date+after)]
        mig_dt[,diff:=event_date-end_preg]
        after[[w]]<-data.table(StudyVar="Migraine_checkbox", after_end=mig_dt[diff>0,.N])
        mig_dt<-mig_dt[diff<=0]
        mig_dt[,diff:=NULL][,after:=NULL][,end_preg:=NULL]
      }else{
        #mig_dt[,end:=obs_period_diag[StudyVar==names_events[mig_fl],end_date]]
        mig_dt[,end_preg:=as.IDate(pregnancy_end_date) + 5]
        mig_dt[,diff:=event_date-end_preg]
        after[[w]]<-data.table(StudyVar="Migraine_checkbox", after_end=mig_dt[diff>0,.N])
        mig_dt<-mig_dt[diff<=0]
        mig_dt[,diff:=NULL][,end_preg:=NULL]
      }
    }else{
      after[[w]]<-data.table(StudyVar="Migraine_checkbox", after_end=0)
    }
    
    if(mig_dt[,.N]>0){
      #create a summary of included records
      sum[[w]]<-data.table(StudyVar="Migraine_checkbox", no_records=mig_dt[!is.na(event_date),.N], no_pregnancies=mig_dt[!duplicated(pregnancy_id),.N])
      saveRDS(mig_dt, paste0(projectFolder,"/g_intermediate/migraine_algorithm/final_d3/", "Migraine_checkbox_pregnancy_D3.rds"))
      cols<-c("person_id","pregnancy_id","pregnancy_start_date","pregnancy_end_date","event_date")
      mig_dt<-mig_dt[,cols,with=F]

        mig_dt[event_date>=pregnancy_start_date + 5 & event_date<=pregnancy_end_date+5,"Migraine_checkbox_baseline":=1]
        mig_baseline<-mig_dt[get("Migraine_checkbox_baseline")==1,lapply(.SD, sum),.SDcols = "Migraine_checkbox_baseline", by=c("person_id","pregnancy_id","pregnancy_start_date","pregnancy_end_date")]
        mig_dt[,Migraine_checkbox_baseline:=NULL]
        preg_d3_checkbox<-merge.data.table(preg_d3_checkbox,mig_baseline,all.x=T, by=cols[!cols %in% "event_date"])
        rm(mig_baseline)
        preg_d3_checkbox[is.na(Migraine_checkbox_baseline),Migraine_checkbox_baseline:=0]
      
        preg_d3_checkbox[,Migraine_checkbox_during:=Migraine_checkbox_baseline]
      
    }else{
      preg_d3_checkbox[,Migraine_checkbox_baseline:=0]
      preg_d3_checkbox[,Migraine_checkbox_during:=0]
    }
  }else{
    preg_d3_checkbox[,Migraine_checkbox_baseline:=0]
    preg_d3_checkbox[,Migraine_checkbox_during:=0]
  }
  
  rm(mig_dt) 
  w<-w+1
}
}else{preg_d3_checkbox<-NULL}

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
if(length(not_present)>0){pregnancy_d3_mig[,eval(not_present):=list(0)]}
####Migraine MEDICINES: Triptans####
print("Loading all Migraine medicines D3 and merge with the pregnancy D3.")

mig_med_fl<-list.files(paste0(projectFolder,"/g_intermediate/migraine_algorithm/"),"Migraine_medicines")
mig_med<-readRDS(paste0(projectFolder,"/g_intermediate/migraine_algorithm/",mig_med_fl))

#Migraine_medicines: triptans separately
mig_med[,truncated_atc:=substr(atc_code,1,5)]
mig_med_other<-mig_med[truncated_atc != "N02CC"]
mig_med<-mig_med[truncated_atc == "N02CC"]
mig_med[,truncated_atc:=NULL]

#Add info about medicinal product id
mig_med<-merge.data.table(mig_med, additional_concepts, by=c("atc_code", "medicinal_product_id"), all.x=T)

#Find all triptans
cols_to_merge<-c("person_id","pregnancy_id","pregnancy_start_date","pregnancy_end_date","year","birth_date","death_date","op_end_date_mig","op_start_date_mig","age","maternal_age","year_group")
mig_med<-merge.data.table(pregnancy_d3_mig[,cols_to_merge,with=F], mig_med, by="person_id", all.x=T, allow.cartesian = T)
mig_med<-mig_med[!is.na(medicine_date)]
mig_med[,pregnancy_start_date:=as.IDate(pregnancy_start_date)][,pregnancy_end_date:=as.IDate(pregnancy_end_date)][,birth_date:=as.IDate(birth_date)][,death_date:=as.IDate(death_date)][,op_start_date_mig:=as.IDate(op_start_date_mig)][,op_end_date_mig:=as.IDate(op_end_date_mig)][,medicine_date:=as.IDate(medicine_date)]

original<-data.table(StudyVar=c("Migraine_medicines","Migraine_medicines_injections"), original_records=c(as.character(mig_med[,.N]),as.character(mig_med[injection==1,.N])))
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
      before[[w]]<-data.table(StudyVar=c(obs_period_med[med_fl,StudyVar], "Migraine_medicines_injections"), 
                              before_start=c(as.character(mig_med[diff<0,.N]),as.character(mig_med[diff<0 & injection==1,.N])))
      mig_med<-mig_med[diff>=0]
      mig_med[,diff:=NULL][,lookback:=NULL][,start_preg:=NULL]
    }else{
      #remove all records before start obs
      mig_med[,start:=obs_period_med[med_fl,start_date]]
      mig_med[,start_preg:=as.IDate(pregnancy_start_date+start)]
      mig_med[,diff:=medicine_date-start_preg]
      before[[w]]<-data.table(StudyVar=c(obs_period_med[med_fl,StudyVar], "Migraine_medicines_injections"), 
                              before_start=c(as.character(mig_med[diff<0,.N]),as.character(mig_med[diff<0 & injection==1,.N])))
      mig_med<-mig_med[diff>=0]
      mig_med[,diff:=NULL][,start:=NULL][,start_preg:=NULL]
    }
    
    if(mig_med[,.N]>0){
      if(obs_period_med[med_fl,after]>0){
        mig_med[,after:=obs_period_med[med_fl,after]]
        mig_med[,end_preg:=as.IDate(pregnancy_end_date+after)]
        mig_med[,diff:=medicine_date-end_preg]
        after[[w]]<-data.table(StudyVar=c(obs_period_med[med_fl,StudyVar], "Migraine_medicines_injections"),  
                               after_end=c(as.character(mig_med[diff>0,.N]),as.character(mig_med[diff>0 & injection==1,.N])))
        mig_med<-mig_med[diff<=0]
        mig_med[,diff:=NULL][,after:=NULL][,end_preg:=NULL]
      }else{
        #mig_med[,end:=after]
        mig_med[,end_preg:=as.IDate(pregnancy_end_date)]
        mig_med[,diff:=medicine_date-end_preg]
        after[[w]]<-data.table(StudyVar=c(obs_period_med[med_fl,StudyVar], "Migraine_medicines_injections"),  
                               after_end=c(as.character(mig_med[diff>0,.N]),as.character(mig_med[diff>0 & injection==1,.N])))
        mig_med<-mig_med[diff<=0]
        mig_med[,diff:=NULL][,end_preg:=NULL]
      }
    }else{
      after[[w]]<-data.table(StudyVar=c(obs_period_med[med_fl,StudyVar], "Migraine_medicines_injections"), after_end=c(as.character(0),as.character(0)))
    }
    
    if(mig_med[,.N]>0){
      #create a summary of included records
      sum[[w]]<-data.table(StudyVar=c(obs_period_med[med_fl,StudyVar], "Migraine_medicines_injections"), 
                           no_records=c(mig_med[!is.na(medicine_date),.N], mig_med[!is.na(medicine_date) & injection==1,.N]), 
                           no_pregnancies=c(mig_med[!duplicated(pregnancy_id),.N], mig_med[!duplicated(pregnancy_id),.N]))
      cols_to_exp<-c("person_id","pregnancy_id","pregnancy_start_date","pregnancy_end_date","year","birth_date","death_date","op_end_date_mig","op_start_date_mig","age","maternal_age","year_group","medicine_date","atc_code","condition","medicinal_product_group")
      saveRDS(mig_med[,cols_to_exp,with=F], paste0(projectFolder,"/g_intermediate/migraine_algorithm/final_d3/", obs_period_med[med_fl,StudyVar],"_pregnancy_D3.rds"))
      saveRDS(mig_med[injection==1,cols_to_exp,with=F], paste0(projectFolder,"/g_intermediate/migraine_algorithm/final_d3/", "Migraine_medicines_injections_pregnancy_D3.rds"))
      rm(cols_to_exp)
      cols<-c("person_id","pregnancy_id","pregnancy_start_date","pregnancy_end_date","medicine_date","medicinal_product_id","injection")
      mig_med<-mig_med[,cols,with=F]
      #Identify all baseline prescriptions
      mig_med[,dif:=medicine_date-pregnancy_start_date]
      mig_med[dif<0,paste0(obs_period_med[med_fl, StudyVar],"_baseline"):=1]
      mig_med[dif<0 & injection==1,Migraine_injections_baseline:=1]
      mig_med_baseline<-mig_med[get(paste0(obs_period_med[med_fl, StudyVar],"_baseline"))==1,lapply(.SD, sum),.SDcols = paste0(obs_period_med[med_fl, StudyVar],"_baseline"), by=c("person_id","pregnancy_id","pregnancy_start_date","pregnancy_end_date")]
      mig_inj_baseline<-mig_med[Migraine_injections_baseline==1,lapply(.SD, sum),.SDcols = "Migraine_injections_baseline", by=c("person_id","pregnancy_id","pregnancy_start_date","pregnancy_end_date")]
      
      mig_med[,dif:=NULL][,eval(paste0(obs_period_med[med_fl, StudyVar],"_baseline")):=NULL][,Migraine_injections_baseline:=NULL]
      pregnancy_d3_mig<-merge.data.table(pregnancy_d3_mig,mig_med_baseline,all.x=T, by=cols[!cols %in% c("medicine_date","injection","medicinal_product_id")])
      rm(mig_med_baseline)
      pregnancy_d3_mig[is.na(get(paste0(obs_period_med[med_fl, StudyVar],"_baseline"))),eval(paste0(obs_period_med[med_fl, StudyVar],"_baseline")):=0]
      pregnancy_d3_mig<-merge.data.table(pregnancy_d3_mig,mig_inj_baseline,all.x=T, by=cols[!cols %in% c("medicine_date","injection","medicinal_product_id")])
      rm(mig_inj_baseline)
      pregnancy_d3_mig[is.na(Migraine_injections_baseline),Migraine_injections_baseline:=0]
      
      #depending on the DAP: Different lookback
      if(!DAP_name %in% c("NIHW", "CHUT")){
        mig_med[,start:=pregnancy_start_date - 3*30.25] 
        mig_med[medicine_date>=start & medicine_date<pregnancy_start_date,paste0(obs_period_med[med_fl, StudyVar],"_baseline_2"):=1]
        mig_med[medicine_date>=start & medicine_date<pregnancy_start_date & injection==1, Migraine_injections_baseline_2:=1]
        mig_baseline_2<-mig_med[get(paste0(obs_period_med[med_fl, StudyVar],"_baseline_2"))==1,lapply(.SD, sum),.SDcols = paste0(obs_period_med[med_fl, StudyVar],"_baseline_2"), by=c("person_id","pregnancy_id","pregnancy_start_date","pregnancy_end_date")]
        mig_baseline_inj_2<-mig_med[Migraine_injections_baseline_2==1,lapply(.SD, sum),.SDcols = "Migraine_injections_baseline_2", by=c("person_id","pregnancy_id","pregnancy_start_date","pregnancy_end_date")]
        mig_med[,start:=NULL][,eval(paste0(obs_period_med[med_fl, StudyVar],"_baseline_2")):=NULL][,Migraine_injections_baseline_2:=NULL]
        pregnancy_d3_mig<-merge.data.table(pregnancy_d3_mig,mig_baseline_2,all.x=T, by=cols[!cols %in% c("medicine_date","medicinal_product_id","injection")])
        rm(mig_baseline_2)
        pregnancy_d3_mig[is.na(get(paste0(obs_period_med[med_fl, StudyVar],"_baseline_2"))),eval(paste0(obs_period_med[med_fl, StudyVar],"_baseline_2")):=0]
        pregnancy_d3_mig<-merge.data.table(pregnancy_d3_mig,mig_baseline_inj_2,all.x=T, by=cols[!cols %in% c("medicine_date","medicinal_product_id","injection")])
        rm(mig_baseline_inj_2)
        pregnancy_d3_mig[is.na(Migraine_injections_baseline_2),Migraine_injections_baseline_2:=0]
      
        }else{
        pregnancy_d3_mig[,eval(paste0(obs_period_med[med_fl, StudyVar],"_baseline_2")):=get(paste0(obs_period_med[med_fl, StudyVar],"_baseline"))]
          pregnancy_d3_mig[,Migraine_injections_baseline_2:=Migraine_injections_baseline]
      }
      
      #during pregnancy
      mig_med[medicine_date>=pregnancy_start_date & medicine_date<=pregnancy_end_date,paste0(obs_period_med[med_fl, StudyVar],"_during"):=1]
      mig_med[medicine_date>=pregnancy_start_date & medicine_date<=pregnancy_end_date & injection==1,Migraine_injections_during:=1]
      mig_med_during<-mig_med[get(paste0(obs_period_med[med_fl, StudyVar],"_during"))==1,lapply(.SD, sum),.SDcols = paste0(obs_period_med[med_fl, StudyVar],"_during"), by=c("person_id","pregnancy_id","pregnancy_start_date","pregnancy_end_date")]
      mig_inj_during<-mig_med[Migraine_injections_during==1,lapply(.SD, sum),.SDcols = "Migraine_injections_during", by=c("person_id","pregnancy_id","pregnancy_start_date","pregnancy_end_date")]
      mig_med[,eval(paste0(obs_period_med[med_fl, StudyVar],"_during")):=NULL][,Migraine_injections_during:=NULL]
      pregnancy_d3_mig<-merge.data.table(pregnancy_d3_mig,mig_med_during,all.x=T, by=cols[!cols %in% c("medicine_date","medicinal_product_id","injection")])
      rm(mig_med_during)
      pregnancy_d3_mig[is.na(get(paste0(obs_period_med[med_fl, StudyVar],"_during"))),eval(paste0(obs_period_med[med_fl, StudyVar],"_during")):=0]
      pregnancy_d3_mig<-merge.data.table(pregnancy_d3_mig,mig_inj_during,all.x=T, by=cols[!cols %in% c("medicine_date","medicinal_product_id","injection")])
      rm(mig_inj_during)
      pregnancy_d3_mig[is.na(Migraine_injections_during),Migraine_injections_during:=0]
      cols<-cols[!cols %in% c("medicinal_product_id","injection")]
      #first
      mig_med[,start:=pregnancy_start_date + trimester_timepoint[Indicator=="first",as.numeric(start)]][,end:=pregnancy_start_date + trimester_timepoint[Indicator=="first",as.numeric(end)]]
      mig_med[medicine_date>=start & medicine_date<end,paste0(obs_period_med[med_fl, StudyVar],"_first"):=1]
      mig_med_first<-mig_med[get(paste0(obs_period_med[med_fl, StudyVar],"_first"))==1,lapply(.SD, sum),.SDcols = paste0(obs_period_med[med_fl, StudyVar],"_first"), by=c("person_id","pregnancy_id","pregnancy_start_date","pregnancy_end_date")]
      mig_med[,eval(paste0(obs_period_med[med_fl, StudyVar],"_first")):=NULL]
      pregnancy_d3_mig<-merge.data.table(pregnancy_d3_mig,mig_med_first,all.x=T, by=cols[!cols %in% "medicine_date"])
      rm(mig_med_first)
      pregnancy_d3_mig[is.na(get(paste0(obs_period_med[med_fl, StudyVar],"_first"))),eval(paste0(obs_period_med[med_fl, StudyVar],"_first")):=0]
      #second
      mig_med[,start:=pregnancy_start_date + trimester_timepoint[Indicator=="second",as.numeric(start)]][,end:=pregnancy_start_date + trimester_timepoint[Indicator=="second",as.numeric(end)]]
      mig_med[medicine_date>=start & medicine_date<end,paste0(obs_period_med[med_fl, StudyVar],"_second"):=1]
      mig_med_second<-mig_med[get(paste0(obs_period_med[med_fl, StudyVar],"_second"))==1,lapply(.SD, sum),.SDcols = paste0(obs_period_med[med_fl, StudyVar],"_second"), by=c("person_id","pregnancy_id","pregnancy_start_date","pregnancy_end_date")]
      mig_med[,eval(paste0(obs_period_med[med_fl, StudyVar],"_second")):=NULL]
      pregnancy_d3_mig<-merge.data.table(pregnancy_d3_mig,mig_med_second,all.x=T, by=cols[!cols %in% "medicine_date"])
      rm(mig_med_second)
      pregnancy_d3_mig[is.na(get(paste0(obs_period_med[med_fl, StudyVar],"_second"))),eval(paste0(obs_period_med[med_fl, StudyVar],"_second")):=0]
      #third
      mig_med[,start:=pregnancy_start_date + trimester_timepoint[Indicator=="third",as.numeric(start)]][,end:=pregnancy_start_date + trimester_timepoint[Indicator=="third",as.numeric(end)]]
      mig_med[medicine_date>=start & medicine_date<end,paste0(obs_period_med[med_fl, StudyVar],"_third"):=1]
      mig_med_third<-mig_med[get(paste0(obs_period_med[med_fl, StudyVar],"_third"))==1,lapply(.SD, sum),.SDcols = paste0(obs_period_med[med_fl, StudyVar],"_third"), by=c("person_id","pregnancy_id","pregnancy_start_date","pregnancy_end_date")]
      mig_med[,eval(paste0(obs_period_med[med_fl, StudyVar],"_third")):=NULL]
      pregnancy_d3_mig<-merge.data.table(pregnancy_d3_mig,mig_med_third,all.x=T, by=cols[!cols %in% "medicine_date"])
      rm(mig_med_third)
      pregnancy_d3_mig[is.na(get(paste0(obs_period_med[med_fl, StudyVar],"_third"))),eval(paste0(obs_period_med[med_fl, StudyVar],"_third")):=0]
    }else{
      pregnancy_d3_mig[,eval(paste0(obs_period_med[med_fl,StudyVar],"_baseline")):=0]
      pregnancy_d3_mig[,eval(paste0(obs_period_med[med_fl,StudyVar],"_baseline_2")):=0]
      pregnancy_d3_mig[,eval(paste0(obs_period_med[med_fl,StudyVar],"_during")):=0]
      pregnancy_d3_mig[,eval(paste0(obs_period_med[med_fl,StudyVar],"_first")):=0]
      pregnancy_d3_mig[,eval(paste0(obs_period_med[med_fl,StudyVar],"_second")):=0]
      pregnancy_d3_mig[,eval(paste0(obs_period_med[med_fl,StudyVar],"_third")):=0]
      pregnancy_d3_mig[,Migraine_injections_baseline:=0]
      pregnancy_d3_mig[,Migraine_injections_baseline_2:=0]
      pregnancy_d3_mig[,Migraine_injections_during:=0]
    }
  }else{
    pregnancy_d3_mig[,eval(paste0(obs_period_med[med_fl,StudyVar],"_baseline")):=0]
    pregnancy_d3_mig[,eval(paste0(obs_period_med[med_fl,StudyVar],"_baseline_2")):=0]
    pregnancy_d3_mig[,eval(paste0(obs_period_med[med_fl,StudyVar],"_during")):=0]
    pregnancy_d3_mig[,eval(paste0(obs_period_med[med_fl,StudyVar],"_first")):=0]
    pregnancy_d3_mig[,eval(paste0(obs_period_med[med_fl,StudyVar],"_second")):=0]
    pregnancy_d3_mig[,eval(paste0(obs_period_med[med_fl,StudyVar],"_third")):=0]
    regnancy_d3_mig[,Migraine_injections_baseline:=0]
    pregnancy_d3_mig[,Migraine_injections_baseline_2:=0]
    pregnancy_d3_mig[,Migraine_injections_during:=0]
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
removed_rec_med[,StudyVar:=c("Migraine_medicines(N02CC)", "Migraine_medicines(N02CC01, injection)")]

####Migraine MEDICINES: Other####
#remove uneccessary columns
if("medicinal_product_id" %in% names(mig_med_other)){mig_med_other[,medicinal_product_id:=NULL]}
if("truncated_atc" %in% names(mig_med_other)){mig_med_other[,truncated_atc:=NULL]}

#Find all profilactic medication
cols_to_merge<-c("person_id","pregnancy_id","pregnancy_start_date","pregnancy_end_date","year","birth_date","death_date","op_end_date_mig","op_start_date_mig","age","maternal_age","year_group")
mig_med_other<-merge.data.table(pregnancy_d3_mig[,cols_to_merge,with=F], mig_med_other, by="person_id", all.x=T, allow.cartesian = T)
mig_med_other<-mig_med_other[!is.na(medicine_date)]
mig_med_other[,pregnancy_start_date:=as.IDate(pregnancy_start_date)][,pregnancy_end_date:=as.IDate(pregnancy_end_date)][,birth_date:=as.IDate(birth_date)][,death_date:=as.IDate(death_date)][,op_start_date_mig:=as.IDate(op_start_date_mig)][,op_end_date_mig:=as.IDate(op_end_date_mig)][,medicine_date:=as.IDate(medicine_date)]
if(mig_med_other[,.N]>0){
mig_med_other[,condition:="Migraine_med_profilactic"]
original<-data.table(StudyVar=c("Migraine_medicines_profilactic"), original_records=as.character(mig_med_other[,.N]))
lookback_profilactic<-0
start_date<-0
after<-0

if(lookback_profilactic>0){
  mig_med_other[,lookback:=lookback_profilactic]
  mig_med_other[,start_preg:=as.IDate(pregnancy_start_date-lookback)]
  #exclude all pregnancies that are outside observation period of interest
  mig_med_other[,diff:=medicine_date-start_preg]
  #remove all records with date before start date pregnancy+lookback
  before<-data.table(StudyVar="Migraine_medicines_profilactic", 
                     before_start=c(as.character(mig_med_other[diff<0,.N])))
  mig_med_other<-mig_med_other[diff>=0]
  mig_med_other[,diff:=NULL][,lookback:=NULL]
}else{
  #remove all records before start obs
  #mig_med_other[,start:=start_date]
  mig_med_other[,start_preg:=as.IDate(pregnancy_start_date)]
  mig_med_other[,diff:=medicine_date-start_preg]
  before<-data.table(StudyVar="Migraine_medicines_profilactic", 
                          before_start=as.character(mig_med_other[diff<0,.N]))
  mig_med_other<-mig_med_other[diff>=0]
  mig_med_other[,diff:=NULL]
}

if(mig_med_other[,.N]>0){
  
  if(after>0){
    mig_med_other[,after:=after]
    mig_med_other[,end_preg:=as.IDate(pregnancy_end_date+after)]
    mig_med_other[,diff:=medicine_date-end_preg]
    after<-data.table(StudyVar="Migraine_medicines_profilactic",  
                           after_end=as.character(mig_med_other[diff>0,.N]))
    mig_med_other<-mig_med_other[diff<=0]
    mig_med_other[,diff:=NULL][,after:=NULL][,end_preg:=NULL]
  }else{
    #mig_med_other[,end:=after]
    mig_med_other[,end_preg:=as.IDate(pregnancy_end_date)]
    mig_med_other[,diff:=medicine_date-end_preg]
    after<-data.table(StudyVar="Migraine_medicines_profilactic",  
                           after_end=as.character(mig_med_other[diff>0,.N]))
    mig_med_other<-mig_med_other[diff<=0]
    mig_med_other[,diff:=NULL][,end_preg:=NULL]
  }  
}else{
  after<-data.table(StudyVar="Migraine_medicines_profilactic", after_end=as.character(0))
}

if(mig_med_other[,.N]>0){
  #create a summary of included records
  sum_prof<-data.table(StudyVar="Migraine_medicines_profilactic", 
                       no_records=mig_med_other[!is.na(medicine_date),.N], 
                       no_pregnancies=mig_med_other[!duplicated(pregnancy_id),.N])
  cols_to_exp<-c("person_id","pregnancy_id","pregnancy_start_date","pregnancy_end_date","year","birth_date","death_date","op_end_date_mig","op_start_date_mig","age","maternal_age","year_group","medicine_date","atc_code","condition","medicinal_product_group")
  saveRDS(mig_med_other[,cols_to_exp,with=F], paste0(projectFolder,"/g_intermediate/migraine_algorithm/final_d3/Migraine_medicines_profilactic_pregnancy_D3.rds"))
  rm(cols_to_exp)
  cols<-c("person_id","pregnancy_id","pregnancy_start_date","pregnancy_end_date","medicine_date")
  mig_med_other<-mig_med_other[,cols,with=F]
  #Identify all baseline prescriptions
  mig_med_other[,dif:=medicine_date-pregnancy_start_date]
  mig_med_other[dif<0,Migraine_med_profilactic_baseline:=1]
  mig_med_baseline<-mig_med_other[Migraine_med_profilactic_baseline==1,lapply(.SD, sum),.SDcols = "Migraine_med_profilactic_baseline", by=c("person_id","pregnancy_id","pregnancy_start_date","pregnancy_end_date")]
  mig_med_other[,dif:=NULL][,Migraine_med_profilactic_baseline:=NULL]
  pregnancy_d3_mig<-merge.data.table(pregnancy_d3_mig,mig_med_baseline,all.x=T, by=cols[!cols %in% c("medicine_date")])
  rm(mig_med_baseline)
  pregnancy_d3_mig[is.na(Migraine_med_profilactic_baseline),Migraine_med_profilactic_baseline:=0]

  #depending on the DAP: Different lookback
  if(!DAP_name %in% c("NIHW", "CHUT")){
    mig_med_other[,start:=pregnancy_start_date - 3*30.25] 
    mig_med_other[medicine_date>=start & medicine_date<pregnancy_start_date,Migraine_med_profilactic_baseline_2:=1]
    mig_baseline_2<-mig_med_other[Migraine_med_profilactic_baseline_2==1,lapply(.SD, sum),.SDcols = "Migraine_med_profilactic_baseline_2", by=c("person_id","pregnancy_id","pregnancy_start_date","pregnancy_end_date")]
    mig_med_other[,start:=NULL][,Migraine_med_profilactic_baseline_2:=NULL]
    pregnancy_d3_mig<-merge.data.table(pregnancy_d3_mig,mig_baseline_2,all.x=T, by=cols[!cols %in% c("medicine_date","medicinal_product_id","injection")])
    rm(mig_baseline_2)
    pregnancy_d3_mig[is.na(Migraine_med_profilactic_baseline_2),Migraine_med_profilactic_baseline_2:=0]

  }else{
    pregnancy_d3_mig[,Migraine_med_profilactic_baseline_2:=Migraine_med_profilactic_baseline]
  }
  
  #during pregnancy
  mig_med_other[medicine_date>=pregnancy_start_date & medicine_date<=pregnancy_end_date, Migraine_med_profilactic_during:=1]
  mig_med_during<-mig_med_other[Migraine_med_profilactic_during==1,lapply(.SD, sum),.SDcols = "Migraine_med_profilactic_during", by=c("person_id","pregnancy_id","pregnancy_start_date","pregnancy_end_date")]
  mig_med_other[,Migraine_med_profilactic_during:=NULL]
  pregnancy_d3_mig<-merge.data.table(pregnancy_d3_mig,mig_med_during,all.x=T, by=cols[!cols %in% c("medicine_date")])
  rm(mig_med_during)
  pregnancy_d3_mig[is.na(Migraine_med_profilactic_during),Migraine_med_profilactic_during:=0]
}else{
  pregnancy_d3_mig[,Migraine_med_profilactic_during:=0]
}
}else{
  original<-data.table(StudyVar=c("Migraine_medicines_profilactic"), original_records=as.character(0))
  before<-data.table(StudyVar="Migraine_medicines_profilactic", before_start=as.character(0))
  after<-data.table(StudyVar="Migraine_medicines_profilactic", after_end=as.character(0))
  pregnancy_d3_mig[,Migraine_med_profilactic:=0]
  sum_prof<-data.table(StudyVar="Migraine_medicines_profilactic", 
                       no_records=as.character(0), 
                       no_pregnancies=as.character(0))
  
}
removed_rec_prof<-merge.data.table(original,before,by="StudyVar",all=T)
removed_rec_prof<-merge.data.table(removed_rec_prof,after,by="StudyVar",all=T)
removed_rec_prof[is.na(before_start),before_start:="N/A"]
removed_rec_prof[is.na(after_end),after_end:="N/A"]
removed_rec_med<-rbind(removed_rec_med,removed_rec_prof)
rm(before,after,original)

removed_rec<-rbind(removed_rec,removed_rec_med)
fwrite(removed_rec,paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_excluded_records_migraine.csv"),row.names = F)
rm(removed_rec)


#Pregnancies that had the event of interest not in the timeframe of study were excluded, this is why the number of pregnancies is different between different events
sum_med<-as.data.table(do.call(rbind,sum))
sum_med<-rbind(sum_med,sum_prof)
#rm(original,before,after,sum)
rm(mig_med_fl)

names_medicines<-c(paste0(obs_period_med[,StudyVar],"_baseline"),paste0(obs_period_med[med_fl,StudyVar],"_baseline_2"),paste0(obs_period_med[med_fl,StudyVar],"_during"),paste0(obs_period_med[med_fl,StudyVar],"_first"),paste0(obs_period_med[med_fl,StudyVar],"_second"),paste0(obs_period_med[med_fl,StudyVar],"_third"), "Migraine_med_profilactic_baseline", "Migraine_med_profilactic_baseline_2", "Migraine_med_profilactic_during", "Migraine_injections_baseline", "Migraine_injections_baseline_2", "Migraine_injections_during")
#identify events that are not present from conditions_migraine
not_present<-setdiff(names_medicines, names(pregnancy_d3_mig))
if(length(not_present)>0){pregnancy_d3_mig[,eval(not_present):=list(0)]}
#rm(obs_period_med)

obs_period<-rbind(obs_period_diag,obs_period_med)
fwrite(obs_period, paste0(projectFolder,"/g_output/Migraine algorithm/Step_04_observation_periods_migraine.csv"),row.names = F)
rm(obs_period,obs_period_diag,obs_period_med)

#Remove uneccessary columns for checkbx
if("Migraine_checkbox_first" %in% names(pregnancy_d3_mig)){pregnancy_d3_mig[,Migraine_checkbox_first:=NULL]}
if("Migraine_checkbox_second" %in% names(pregnancy_d3_mig)){pregnancy_d3_mig[,Migraine_checkbox_second:=NULL]}
if("Migraine_checkbox_third" %in% names(pregnancy_d3_mig)){pregnancy_d3_mig[,Migraine_checkbox_third:=NULL]}
if("Migraine_checkbox_baseline_2" %in% names(pregnancy_d3_mig)){pregnancy_d3_mig[,Migraine_checkbox_baseline_2:=NULL]}
if("MG_AURA_first" %in% names(pregnancy_d3_mig)){pregnancy_d3_mig[,MG_AURA_first:=NULL]}
if("MG_AURA_second" %in% names(pregnancy_d3_mig)){pregnancy_d3_mig[,MG_AURA_second:=NULL]}
if("MG_AURA_third" %in% names(pregnancy_d3_mig)){pregnancy_d3_mig[,MG_AURA_third:=NULL]}
if("MG_NO_AURA_first" %in% names(pregnancy_d3_mig)){pregnancy_d3_mig[,MG_NO_AURA_first:=NULL]}
if("MG_NO_AURA_second" %in% names(pregnancy_d3_mig)){pregnancy_d3_mig[,MG_NO_AURA_second:=NULL]}
if("MG_NO_AURA_third" %in% names(pregnancy_d3_mig)){pregnancy_d3_mig[,MG_NO_AURA_third:=NULL]}
if("MG_OTHER_first" %in% names(pregnancy_d3_mig)){pregnancy_d3_mig[,MG_OTHER_first:=NULL]}
if("MG_OTHER_second" %in% names(pregnancy_d3_mig)){pregnancy_d3_mig[,MG_OTHER_second:=NULL]}
if("MG_OTHER_third" %in% names(pregnancy_d3_mig)){pregnancy_d3_mig[,MG_OTHER_third:=NULL]}
if("MG_STACOMP_first" %in% names(pregnancy_d3_mig)){pregnancy_d3_mig[,MG_STACOMP_first:=NULL]}
if("MG_STACOMP_second" %in% names(pregnancy_d3_mig)){pregnancy_d3_mig[,MG_STACOMP_second:=NULL]}
if("MG_STACOMP_third" %in% names(pregnancy_d3_mig)){pregnancy_d3_mig[,MG_STACOMP_third:=NULL]}
if("MG_UNSP_first" %in% names(pregnancy_d3_mig)){pregnancy_d3_mig[,MG_UNSP_first:=NULL]}
if("MG_UNSP_second" %in% names(pregnancy_d3_mig)){pregnancy_d3_mig[,MG_UNSP_second:=NULL]}
if("MG_UNSP_third" %in% names(pregnancy_d3_mig)){pregnancy_d3_mig[,MG_UNSP_third:=NULL]}
if("MG_UPC_first" %in% names(pregnancy_d3_mig)){pregnancy_d3_mig[,MG_UPC_first:=NULL]}
if("MG_UPC_second" %in% names(pregnancy_d3_mig)){pregnancy_d3_mig[,MG_UPC_second:=NULL]}
if("MG_UPC_third" %in% names(pregnancy_d3_mig)){pregnancy_d3_mig[,MG_UPC_third:=NULL]}


print("Export Migraine pregnancy D3")
fwrite(pregnancy_d3_mig,paste0(projectFolder,"/g_intermediate/migraine_algorithm/final_d3/pregnancy_d3_migraine_algorithm.csv"),row.names = F)

date_running_end_04_c<-Sys.Date()
end_time_04_c<-Sys.time()

time_log_04_c<-data.table(DAP=data_access_provider_name,
                          Script="Step_04_c_migraine_algorithms_prepare.R", 
                          Start_date=date_running_start_04_c, 
                          End_date=date_running_end_04_c,
                          Time_elaspsed=format(end_time_04_c-initial_time_04_c, digits=2))
fwrite(time_log_04_c,paste0(output_dir,"/Time log/Step_04_c_time_log.csv"),row.names = F)
rm(time_log_04_c)

source(paste0(pre_dir,"save_environment.R"))
