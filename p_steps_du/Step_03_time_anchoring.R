initial_time_03<-Sys.time()
date_running_start_03<-Sys.Date()

#Load pregnancy D3
pregnancy_d3<-readRDS(paste0(projectFolder,"/g_intermediate/pregnancy_d3/pregnancy_D3_DU.rds"))

time_anchoring<-fread(paste0(projectFolder, "/p_parameters/covariates_time_anchoring.csv"))
time_anchoring<-time_anchoring[DAP_NAME == data_access_provider_name]

#### Checkbox ####
checkbox_files<-list.files(paste0(projectFolder,"/g_intermediate/diagnoses_d3/raw_data/"),"_checkbox")
covariate<-checkbox_files
original<-list()
included<-list()
w<-1
if(length(checkbox_files)>0){
  for(check_ind in checkbox_files){
    checkbox<-lapply(paste0(projectFolder,"/g_intermediate/diagnoses_d3/raw_data/", check_ind), readRDS)
    checkbox<-as.data.table(do.call(rbind,checkbox))
    original[[w]]<-checkbox[,.N]
    cov_name<-paste0(unique(checkbox[,variable_name]), "_checkbox")
    time_zero<-time_anchoring[variable_name==cov_name, time_zero]
    before_start<-time_anchoring[variable_name==cov_name, before_start]
    time_end<-time_anchoring[variable_name==cov_name, time_end]
    after_start<-time_anchoring[variable_name==cov_name, after_start]
    #merge with the pregnancy d3
    checkbox<-merge.data.table(pregnancy_d3, checkbox, by="person_id", all.x=T, allow.cartesian = T)
    checkbox<-checkbox[!is.na(event_date)]
    checkbox[,start:=get(time_zero) - before_start]
    checkbox[,end:=get(time_end) + after_start]
    checkbox[,start:=as.IDate(start)][,end:=as.IDate(end)][,event_date:=as.IDate(event_date)]
    checkbox[,diff_start:=event_date-start]
    checkbox[,diff_end:=event_date-end]
    #keep records within window of interest
    checkbox[diff_start>=0 & diff_end <= 0, keep:=1]
    checkbox[is.na(keep), keep:=0]
    checkbox<-checkbox[keep == 1]
    included[[w]]<-checkbox[,.N]
    if(checkbox[,.N]>0){
      cols_export<-c("person_id", "pregnancy_id", "event_date", "variable_name")
      saveRDS(checkbox[,cols_export, with=F],paste0(projectFolder,"/g_intermediate/diagnoses_d3/", cov_name, ".rds"))
      
    }
    rm(checkbox)
    w<-w+1
    
  }
}
original<-c(do.call(rbind,original))
if(is.null(original)){original<-0}
included<-c(do.call(rbind,included))
if(is.null(included)){included<-0}
flowchart_included_rec_checkbox<-data.table(variable_name=covariate, original_rec=original, included_rec=included)

#### Fixed ####
fixed_files<-list.files(paste0(projectFolder,"/g_intermediate/diagnoses_d3/raw_data/"),"_fixed")
covariate<-fixed_files
original<-list()
included<-list()
w<-1
if(length(fixed_files)>0){
  for(fixed_ind in fixed_files){
    fixed<-lapply(paste0(projectFolder,"/g_intermediate/diagnoses_d3/raw_data/", fixed_ind), readRDS)
    fixed<-as.data.table(do.call(rbind,fixed))
    original[[w]]<-fixed[,.N]
    cov_name<-paste0(unique(fixed[,variable_name]), "_fixed")
    time_zero<-time_anchoring[variable_name==cov_name, time_zero]
    before_start<-time_anchoring[variable_name==cov_name, before_start]
    time_end<-time_anchoring[variable_name==cov_name, time_end]
    after_start<-time_anchoring[variable_name==cov_name, after_start]
    #merge with the pregnancy d3
    fixed<-merge.data.table(pregnancy_d3, fixed, by="person_id", all.x=T, allow.cartesian = T)
    fixed<-fixed[!is.na(event_date)]
    fixed[,start:=get(time_zero) - before_start]
    fixed[,end:=get(time_end) + after_start]
    fixed[,start:=as.IDate(start)][,end:=as.IDate(end)][,event_date:=as.IDate(event_date)]
    fixed[,diff_start:=event_date-start]
    fixed[,diff_end:=event_date-end]
    #keep records within window of interest
    # fixed[diff_start>0 & diff_end < 0, keep:=1]
    fixed[diff_start>=0 & diff_end <= 0, keep:=1]
    fixed[is.na(keep), keep:=0]
    fixed<-fixed[keep == 1]
    included[[w]]<-fixed[,.N]
    if(fixed[,.N]>0){
      setnames(fixed, "event_code", "value")
      cols_export<-c("person_id", "pregnancy_id", "event_date", "variable_name", "value")
      saveRDS(fixed[,cols_export, with=F],paste0(projectFolder,"/g_intermediate/diagnoses_d3/", cov_name, ".rds"))
      
    }
    rm(fixed)
    w<-w+1
    
  }
}
original<-c(do.call(rbind,original))
if(is.null(original)){original<-0}
included<-c(do.call(rbind,included))
if(is.null(included)){included<-0}
flowchart_included_rec_fixed<-data.table(variable_name=covariate, original_rec=original, included_rec=included)

#### Diagnoses ####
diag_files<-list.files(paste0(projectFolder,"/g_intermediate/diagnoses_d3/raw_data/"),"_diagnoses")
covariate<-diag_files
original<-list()
included<-list()
w<-1
if(length(diag_files)>0){
  for(diagnoses_ind in diag_files){
    diagnoses<-lapply(paste0(projectFolder,"/g_intermediate/diagnoses_d3/raw_data/", diagnoses_ind), readRDS)
    diagnoses<-as.data.table(do.call(rbind,diagnoses))
    original[[w]]<-diagnoses[,.N]
    cov_name<-unique(diagnoses[,variable_name])
    time_zero<-time_anchoring[variable_name==cov_name, time_zero]
    before_start<-time_anchoring[variable_name==cov_name, before_start]
    time_end<-time_anchoring[variable_name==cov_name, time_end]
    after_start<-time_anchoring[variable_name==cov_name, after_start]
    #merge with the pregnancy d3
    diagnoses<-merge.data.table(pregnancy_d3, diagnoses, by="person_id", all.x=T, allow.cartesian = T)
    diagnoses<-diagnoses[!is.na(event_date)]
    diagnoses[,start:=get(time_zero) - before_start]
    diagnoses[,end:=get(time_end) + after_start]
    diagnoses[,start:=as.IDate(start)][,end:=as.IDate(end)][,event_date:=as.IDate(event_date)]
    diagnoses[,diff_start:=event_date-start]
    diagnoses[,diff_end:=event_date-end]
    #keep records within window of interest
    diagnoses[diff_start>=0 & diff_end <= 0, keep:=1]
    diagnoses[is.na(keep), keep:=0]
    diagnoses<-diagnoses[keep == 1]
    included[[w]]<-diagnoses[,.N]
    if(diagnoses[,.N]>0){
      cols_export<-c("person_id", "pregnancy_id", "event_date", "variable_name")
      saveRDS(diagnoses[,cols_export, with=F],paste0(projectFolder,"/g_intermediate/diagnoses_d3/", cov_name, ".rds"))
      
    }
    rm(diagnoses)
    w<-w+1
    
  }
}
original<-c(do.call(rbind,original))
if(is.null(original)){original<-0}
included<-c(do.call(rbind,included))
if(is.null(included)){included<-0}
flowchart_included_rec_diagnoses<-data.table(variable_name=covariate, original_rec=original, included_rec=included)

flowchart<-rbind(flowchart_included_rec_checkbox, flowchart_included_rec_fixed, flowchart_included_rec_diagnoses)
fwrite(flowchart, paste0(projectFolder, "/g_output/Drug utilisation/flowchart/included_records_cov_anchored.csv"), row.names = F)
rm(flowchart)



date_running_end_03<-Sys.Date()
end_time_03<-Sys.time()

time_log_03<-data.table(DAP=data_access_provider_name,
                     Script="Step_03_time_anchoring.R", 
                     Start_date=date_running_start_03, 
                     End_date=date_running_end_03,
                     Time_elaspsed=format(end_time_03-initial_time_03, digits=2))
fwrite(time_log_03,paste0(output_dir,"/Time log/DU/Step_03_time_log.csv"),row.names = F)
rm(time_log_03)

