initial_time_05<-Sys.time()
date_running_start_05<-Sys.Date()

#### Load pregnancy ####
pregnancy_d3_mig<-as.data.table(readRDS(paste0(projectFolder,"/g_intermediate/migraine_algorithm_sensitivity/pregnancy_D3_sensitivity/MIG_Pregnancy_D3_S.rds")))
#### Parameters ####
orig_mig<-pregnancy_d3_mig[,.N]

if(DAP_name == "CHUT"){
  obs_period_diag<-data.table(StudyVar=c("MG","MG_NO_AURA","MG_AURA","MG_STACOMP","MG_OTHER","MG_UNSP","MG_UPC", "Migraine_checkbox"),
                              lookback=c(rep(3*30.4375-14,7),5),
                              start_date="pregnancy_start_date",
                              add_start=rep(0,8),
                              end_date="pregnancy_end_date",
                              after=rep(0,8))  
}else{
  obs_period_diag<-data.table(StudyVar=c("MG","MG_NO_AURA","MG_AURA","MG_OTHER","MG_UNSP","MG_STACOMP","MG_UPC","Migraine_checkbox"),
                              lookback=c(365.25,365.25,365.25,365.25,365.25,365.25,365.25,5),
                              start_date="pregnancy_start_date",
                              add_start=rep(0,8),
                              end_date="pregnancy_end_date",
                              after=rep(0,8)) 
}

if (DAP_name == "NIHW"){
  obs_period_med<-data.table(StudyVar="Migraine_medicines",
                             lookback=c(3*30.25),
                             start_date="pregnancy_start_date",
                             add_start=0,
                             end_date="pregnancy_end_date",
                             after=0)}
if (DAP_name == "CHUT"){
  obs_period_med<-data.table(StudyVar=c("Migraine_medicines"),
                             lookback=3*30.4375-14,
                             start_date="pregnancy_start_date",
                             add_start=0,
                             end_date="pregnancy_end_date",
                             after=0)}

if (!DAP_name %in% c("NIHW", "CHUT")){
  obs_period_med<-data.table(StudyVar=c("Migraine_medicines"),
                             lookback=365.25,
                             start_date="pregnancy_start_date",
                             add_start=0,
                             end_date="pregnancy_end_date",
                             after=0)
}

if("final_d3" %in% list.files(paste0(projectFolder,"/g_intermediate/migraine_algorithm_sensitivity/migraine_diagnoses/"))){
  unlink(paste0(projectFolder,"/g_intermediate/migraine_algorithm_sensitivity/migraine_diagnoses/final_d3"), recursive = T)
}
dir.create(paste0(projectFolder,"/g_intermediate/migraine_algorithm_sensitivity/migraine_diagnoses/final_d3"))

if("final_d3" %in% list.files(paste0(projectFolder,"/g_intermediate/migraine_algorithm_sensitivity/pregnancy_D3_sensitivity/"))){
  unlink(paste0(projectFolder,"/g_intermediate/migraine_algorithm_sensitivity/pregnancy_D3_sensitivity/final_d3"), recursive = T)
}
dir.create(paste0(projectFolder,"/g_intermediate/migraine_algorithm_sensitivity/pregnancy_D3_sensitivity/final_d3"))

####Migraine DIAGNOSES sensitivity####
mig_files<-list.files(paste0(projectFolder,"/g_intermediate/migraine_algorithm_sensitivity/migraine_diagnoses/"), "MG_S")
names_events<-"MG"
w<-1
for(mig_fl in 1:length(mig_files)){
  mig_dt<-readRDS(paste0(projectFolder,"/g_intermediate/migraine_algorithm_sensitivity/migraine_diagnoses/", mig_files[mig_fl]))
  #merge with the pregnancy d3
  mig_dt<-merge.data.table(pregnancy_d3_mig, mig_dt, by="person_id", all.x=T, allow.cartesian = T)
  mig_dt<-mig_dt[!is.na(event_date)]
  if(mig_dt[,.N]>0){
    mig_dt[,event_date:=as.IDate(event_date)]
    
    mig_dt[,lookback:=as.numeric(obs_period_diag[StudyVar==names_events[mig_fl],lookback])]
    mig_dt[,start_preg:=as.IDate(get(obs_period_diag[StudyVar==names_events[mig_fl],start_date])-lookback)]
    mig_dt[,start_preg:=as.IDate(start_preg+as.numeric(obs_period_diag[StudyVar==names_events[mig_fl],add_start]))]
    #exclude all events that are outside observation period of interest
    mig_dt[,diff:=event_date-start_preg]
    #remove all records with date before start date pregnancy+lookback
    mig_dt<-mig_dt[diff>=0]
    mig_dt[,diff:=NULL][,lookback:=NULL][,start_preg:=NULL]
    
    if(mig_dt[,.N]>0){
      mig_dt[,after:=as.numeric(obs_period_diag[StudyVar==names_events[mig_fl],after])]
      mig_dt[,end_preg:=as.IDate(get(obs_period_diag[StudyVar==names_events[mig_fl],end_date])+ after)]
      mig_dt[,diff:=event_date-end_preg]
      mig_dt<-mig_dt[diff<=0]
      mig_dt[,diff:=NULL][,after:=NULL][,end_preg:=NULL]
      }
    
    if(mig_dt[,.N]>0){

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
      mig_dt[,start:=get(trimester_timepoint[Indicator=="first", start]) + as.numeric(trimester_timepoint[Indicator=="first",add_start])][,end:=get(trimester_timepoint[Indicator=="first", start]) + as.numeric(trimester_timepoint[Indicator=="first",end])]
      #Replace end date with end of pregnancy if this comes before
      mig_dt[end>pregnancy_end_date, end:=pregnancy_end_date]
      mig_dt[event_date>=start & event_date<end,paste0(names_events[mig_fl],"_first"):=1]
      mig_first<-mig_dt[get(paste0(names_events[mig_fl],"_first"))==1,lapply(.SD, sum),.SDcols = paste0(names_events[mig_fl],"_first"), by=c("person_id","pregnancy_id","pregnancy_start_date","pregnancy_end_date")]
      mig_dt[,eval(paste0(names_events[mig_fl],"_first")):=NULL]
      pregnancy_d3_mig<-merge.data.table(pregnancy_d3_mig,mig_first,all.x=T, by=cols[!cols %in% "event_date"])
      rm(mig_first)
      pregnancy_d3_mig[is.na(get(paste0(names_events[mig_fl],"_first"))),eval(paste0(names_events[mig_fl],"_first")):=0]
      #second
      mig_dt[,start:=get(trimester_timepoint[Indicator=="second", start]) + as.numeric(trimester_timepoint[Indicator=="second",add_start])][,end:=get(trimester_timepoint[Indicator=="second", start]) + as.numeric(trimester_timepoint[Indicator=="second",end])]
      mig_dt[end>pregnancy_end_date, end:=pregnancy_end_date]
      mig_dt[event_date>=start & event_date<end,paste0(names_events[mig_fl],"_second"):=1]
      mig_second<-mig_dt[get(paste0(names_events[mig_fl],"_second"))==1,lapply(.SD, sum),.SDcols = paste0(names_events[mig_fl],"_second"), by=c("person_id","pregnancy_id","pregnancy_start_date","pregnancy_end_date")]
      mig_dt[,eval(paste0(names_events[mig_fl],"_second")):=NULL]
      pregnancy_d3_mig<-merge.data.table(pregnancy_d3_mig,mig_second,all.x=T, by=cols[!cols %in% "event_date"])
      rm(mig_second)
      pregnancy_d3_mig[is.na(get(paste0(names_events[mig_fl],"_second"))),eval(paste0(names_events[mig_fl],"_second")):=0]
      #third
      mig_dt[,start:=get(trimester_timepoint[Indicator=="third", start]) + as.numeric(trimester_timepoint[Indicator=="third",add_start])][,end:=get(trimester_timepoint[Indicator=="third", start]) + as.numeric(trimester_timepoint[Indicator=="third",end])]
      mig_dt[end>pregnancy_end_date, end:=pregnancy_end_date]
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

names_migraine<-c("MG_baseline","MG_baseline_2","MG_during","MG_first","MG_second","MG_third")
#identify events that are not present from conditions_migraine
not_present<-setdiff(names_migraine, names(pregnancy_d3_mig))
if(length(not_present)>0){pregnancy_d3_mig[,eval(not_present):=list(0)]}

####Migraine MEDICINES: Triptans####
print("Loading all Migraine medicines D3 and merge with the pregnancy D3.")

mig_med_fl<-list.files(paste0(projectFolder,"/g_intermediate/migraine_algorithm_sensitivity/migraine_medicines/"),"Migraine_medicines_S")
mig_med<-readRDS(paste0(projectFolder,"/g_intermediate/migraine_algorithm_sensitivity/migraine_medicines/",mig_med_fl))

#Migraine_medicines: triptans separately
mig_med[,truncated_atc:=substr(atc_code,1,5)]
mig_med<-mig_med[truncated_atc == "N02CC"]
mig_med[,truncated_atc:=NULL]

#Find all triptans
cols_to_merge<-c("person_id","pregnancy_id","pregnancy_start_date","pregnancy_end_date","year","birth_date","death_date","op_end_date_mig","op_start_date_mig","age","maternal_age","year_group")
mig_med<-merge.data.table(pregnancy_d3_mig[,cols_to_merge,with=F], mig_med, by="person_id", all.x=T, allow.cartesian = T)
mig_med<-mig_med[!is.na(medicine_date)]
mig_med[,pregnancy_start_date:=as.IDate(pregnancy_start_date)][,pregnancy_end_date:=as.IDate(pregnancy_end_date)][,birth_date:=as.IDate(birth_date)][,death_date:=as.IDate(death_date)][,op_start_date_mig:=as.IDate(op_start_date_mig)][,op_end_date_mig:=as.IDate(op_end_date_mig)][,medicine_date:=as.IDate(medicine_date)]

w<-1
for(med_fl in 1:length(obs_period_med[,.N])){
  if(mig_med[,.N]>0){
    mig_med[,lookback:=as.numeric(obs_period_med[med_fl,lookback])]
    mig_med[,start_preg:=as.IDate(get(obs_period_med[med_fl,start_date])-lookback)]
    mig_med[,start_preg:=as.IDate(start_preg + as.numeric(obs_period_med[med_fl,add_start]))]
    mig_med[,diff:=medicine_date-start_preg]
    #remove all records with date before start date pregnancy+lookback
    mig_med<-mig_med[diff>=0]
    mig_med[,diff:=NULL][,lookback:=NULL][,start_preg:=NULL]
    
    if(mig_med[,.N]>0){
      mig_med[,after:=as.numeric(obs_period_med[med_fl,after])]
      mig_med[,end_preg:=as.IDate(get(obs_period_med[med_fl,end_date])+after)]
      mig_med[,diff:=medicine_date-end_preg]
      mig_med<-mig_med[diff<=0]
      mig_med[,diff:=NULL][,after:=NULL][,end_preg:=NULL]
    }
    
    if(mig_med[,.N]>0){
      cols<-c("person_id","pregnancy_id","pregnancy_start_date","pregnancy_end_date","medicine_date")
      mig_med<-mig_med[,cols,with=F]
      #Identify all baseline prescriptions
      mig_med[,dif:=medicine_date-pregnancy_start_date]
      mig_med[dif<0,paste0(obs_period_med[med_fl, StudyVar],"_baseline"):=1]
      mig_med_baseline<-mig_med[get(paste0(obs_period_med[med_fl, StudyVar],"_baseline"))==1,lapply(.SD, sum),.SDcols = paste0(obs_period_med[med_fl, StudyVar],"_baseline"), by=c("person_id","pregnancy_id","pregnancy_start_date","pregnancy_end_date")]

      mig_med[,dif:=NULL][,eval(paste0(obs_period_med[med_fl, StudyVar],"_baseline")):=NULL]
      pregnancy_d3_mig<-merge.data.table(pregnancy_d3_mig,mig_med_baseline,all.x=T, by=cols[!cols %in% c("medicine_date")])
      rm(mig_med_baseline)
      pregnancy_d3_mig[is.na(get(paste0(obs_period_med[med_fl, StudyVar],"_baseline"))),eval(paste0(obs_period_med[med_fl, StudyVar],"_baseline")):=0]

      #depending on the DAP: Different lookback
      if(!DAP_name %in% c("NIHW", "CHUT")){
        mig_med[,start:=pregnancy_start_date - 3*30.25] 
        mig_med[medicine_date>=start & medicine_date<pregnancy_start_date,paste0(obs_period_med[med_fl, StudyVar],"_baseline_2"):=1]
        mig_baseline_2<-mig_med[get(paste0(obs_period_med[med_fl, StudyVar],"_baseline_2"))==1,lapply(.SD, sum),.SDcols = paste0(obs_period_med[med_fl, StudyVar],"_baseline_2"), by=c("person_id","pregnancy_id","pregnancy_start_date","pregnancy_end_date")]
        mig_med[,start:=NULL][,eval(paste0(obs_period_med[med_fl, StudyVar],"_baseline_2")):=NULL]
        pregnancy_d3_mig<-merge.data.table(pregnancy_d3_mig,mig_baseline_2,all.x=T, by=cols[!cols %in% c("medicine_date")])
        rm(mig_baseline_2)
        pregnancy_d3_mig[is.na(get(paste0(obs_period_med[med_fl, StudyVar],"_baseline_2"))),eval(paste0(obs_period_med[med_fl, StudyVar],"_baseline_2")):=0]

      }else{
        pregnancy_d3_mig[,eval(paste0(obs_period_med[med_fl, StudyVar],"_baseline_2")):=get(paste0(obs_period_med[med_fl, StudyVar],"_baseline"))]
      }
      
      #during pregnancy
      mig_med[medicine_date>=pregnancy_start_date & medicine_date<=pregnancy_end_date,paste0(obs_period_med[med_fl, StudyVar],"_during"):=1]
      mig_med_during<-mig_med[get(paste0(obs_period_med[med_fl, StudyVar],"_during"))==1,lapply(.SD, sum),.SDcols = paste0(obs_period_med[med_fl, StudyVar],"_during"), by=c("person_id","pregnancy_id","pregnancy_start_date","pregnancy_end_date")]
      mig_med[,eval(paste0(obs_period_med[med_fl, StudyVar],"_during")):=NULL]
      pregnancy_d3_mig<-merge.data.table(pregnancy_d3_mig,mig_med_during,all.x=T, by=cols[!cols %in% c("medicine_date")])
      rm(mig_med_during)
      pregnancy_d3_mig[is.na(get(paste0(obs_period_med[med_fl, StudyVar],"_during"))),eval(paste0(obs_period_med[med_fl, StudyVar],"_during")):=0]
      cols<-cols[!cols %in% c("medicinal_product_id","injection")]
      #first
      mig_med[,start:=get(trimester_timepoint[Indicator=="first", start]) + as.numeric(trimester_timepoint[Indicator=="first",add_start])][,end:=get(trimester_timepoint[Indicator=="first", start]) + as.numeric(trimester_timepoint[Indicator=="first",end])]
      #Replace end date with end of pregnancy if this comes before
      mig_med[end>pregnancy_end_date, end:=pregnancy_end_date]
      mig_med[medicine_date>=start & medicine_date<end,paste0(obs_period_med[med_fl, StudyVar],"_first"):=1]
      mig_med_first<-mig_med[get(paste0(obs_period_med[med_fl, StudyVar],"_first"))==1,lapply(.SD, sum),.SDcols = paste0(obs_period_med[med_fl, StudyVar],"_first"), by=c("person_id","pregnancy_id","pregnancy_start_date","pregnancy_end_date")]
      mig_med[,eval(paste0(obs_period_med[med_fl, StudyVar],"_first")):=NULL]
      pregnancy_d3_mig<-merge.data.table(pregnancy_d3_mig,mig_med_first,all.x=T, by=cols[!cols %in% "medicine_date"])
      rm(mig_med_first)
      pregnancy_d3_mig[is.na(get(paste0(obs_period_med[med_fl, StudyVar],"_first"))),eval(paste0(obs_period_med[med_fl, StudyVar],"_first")):=0]
      #second
      mig_med[,start:=get(trimester_timepoint[Indicator=="second", start]) + as.numeric(trimester_timepoint[Indicator=="second",add_start])][,end:=get(trimester_timepoint[Indicator=="second", start]) + as.numeric(trimester_timepoint[Indicator=="second",end])]
      #Replace end date with end of pregnancy if this comes before
      mig_med[end>pregnancy_end_date, end:=pregnancy_end_date]
      mig_med[medicine_date>=start & medicine_date<end,paste0(obs_period_med[med_fl, StudyVar],"_second"):=1]
      mig_med_second<-mig_med[get(paste0(obs_period_med[med_fl, StudyVar],"_second"))==1,lapply(.SD, sum),.SDcols = paste0(obs_period_med[med_fl, StudyVar],"_second"), by=c("person_id","pregnancy_id","pregnancy_start_date","pregnancy_end_date")]
      mig_med[,eval(paste0(obs_period_med[med_fl, StudyVar],"_second")):=NULL]
      pregnancy_d3_mig<-merge.data.table(pregnancy_d3_mig,mig_med_second,all.x=T, by=cols[!cols %in% "medicine_date"])
      rm(mig_med_second)
      pregnancy_d3_mig[is.na(get(paste0(obs_period_med[med_fl, StudyVar],"_second"))),eval(paste0(obs_period_med[med_fl, StudyVar],"_second")):=0]
      #third
      mig_med[,start:=get(trimester_timepoint[Indicator=="third", start]) + as.numeric(trimester_timepoint[Indicator=="third",add_start])][,end:=get(trimester_timepoint[Indicator=="third", start]) + as.numeric(trimester_timepoint[Indicator=="third",end])]
      #Replace end date with end of pregnancy if this comes before
      mig_med[end>pregnancy_end_date, end:=pregnancy_end_date]
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
#### Save preganncy cohort ####
saveRDS(pregnancy_d3_mig, paste0(projectFolder,"/g_intermediate/migraine_algorithm_sensitivity/pregnancy_D3_sensitivity/final_d3/MIG_Pregnancy_D3_S.rds"))
#### Apply the algorithm ####
source(paste0(projectFolder,"/p_steps/Step_05_a_migraine_algorithms_sensitivity.R"))
date_running_end_05<-Sys.Date()
end_time_05<-Sys.time()

time_log_05<-data.table(DAP=data_access_provider_name,
                        Script="Step_05_algorithms_sensitivity.R", 
                        Start_date=date_running_start_05, 
                        End_date=date_running_end_05,
                        Time_elaspsed=format(end_time_05-initial_time_05, digits=2))
fwrite(time_log_05,paste0(output_dir,"/Time log/Step_05_time_log.csv"),row.names = F)
rm(time_log_05)

