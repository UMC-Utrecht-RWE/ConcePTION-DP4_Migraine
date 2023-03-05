#Load EVENTS table and apply filter to select PE diagoses/GDM diagnoses/Migraine diagnoses

####load info, parameters, conceptsets####
source(paste0(pre_dir,"info/directory_info.R"))
source(paste0(pre_dir,"info/DAP_info.R"))
source(paste0(pre_dir,"codelists/create_conceptsets.R"))
source(paste0(pre_dir,"parameters/parameters_metadata.R"))
source(paste0(pre_dir,"parameters/study_parameters.R"))


#####The tables that will be search for diagnostic codes: EVENTS, MEDICAL_OBSERVATIONS, SURVEY_OBSERVATIONS####
#The information about GDM diagnoses is saved in 
#codesheet_diagnoses_gdm
#The information about GDM diagnoses is saved in 
#codesheet_diagnoses_pe
#The information about GDM diagnoses is saved in 
#codesheet_diagnoses_migraine
events_gdm_diagnoses<-ifelse("EVENTS" %in% codesheet_diagnoses_gdm[,table],1,0)
mo_gdm_diagnoses<-ifelse("MEDICAL_OBSERVATIONS" %in% c(codesheet_diagnoses_gdm[,table],codesheet_diagnoses_gdm_cat[,table]),1,0)
so_gdm_diagnoses<-ifelse("SURVEY_OBSERVATIONS" %in% c(codesheet_diagnoses_gdm[,table],codesheet_diagnoses_gdm_cat[,table]),1,0)

events_pe_diagnoses<-ifelse("EVENTS" %in% codesheet_diagnoses_pe[,table],1,0)
mo_pe_diagnoses<-ifelse("MEDICAL_OBSERVATIONS" %in% c(codesheet_diagnoses_pe[,table],codesheet_diagnoses_pe_cat[,table]),1,0)
so_pe_diagnoses<-ifelse("SURVEY_OBSERVATIONS" %in% c(codesheet_diagnoses_pe[,table],codesheet_diagnoses_pe_cat[,table]),1,0)

events_migraine_diagnoses<-ifelse("EVENTS" %in% codesheet_diagnoses_migraine[,table],1,0)
mo_migraine_diagnoses<-ifelse("MEDICAL_OBSERVATIONS" %in% c(codesheet_diagnoses_migraine[,table],codesheet_diagnoses_migraine_cat[,table]),1,0)
so_migraine_diagnoses<-ifelse("SURVEY_OBSERVATIONS" %in% c(codesheet_diagnoses_migraine[,table],codesheet_diagnoses_migraine_cat[,table]),1,0)

####EVENTS####
#check if we need to search the events table for GDM
if(sum(events_gdm_diagnoses,events_pe_diagnoses,events_migraine_diagnoses)>0){
  if("code" %in% codesheet_diagnoses_gdm[table=="EVENTS",val_1]){
    code_var<-codesheet_diagnoses_gdm[table=="EVENTS",col_1]
    voc_var<-codesheet_diagnoses_gdm[table=="EVENTS",col_2]
    date_var<-codesheet_diagnoses_gdm[table=="EVENTS",date_column]
  }else{
    code_var<-codesheet_diagnoses_gdm[table=="EVENTS",col_2]
    voc_var<-codesheet_diagnoses_gdm[table=="EVENTS",col_1]
    date_var<-codesheet_diagnoses_gdm[table=="EVENTS",date_column]
  }
  if(length(actual_tables$EVENTS)>0){
  print("Analyse EVENTS table.")
  ####List for saving info####
  print("Creating lists to save the information.")
  original_rows_gdm_pe<-list() #number of table original rows
  original_rows_mig<-list() #number of table original rows
  empty_event_date_gdm_pe<-list() #number of records with missing event date
  empty_event_date_mig<-list() #number of records with missing event date
  empty_event_code_gdm_pe<-list() #numer of records with missing event code
  empty_event_code_mig<-list() #numer of records with missing event code
  empty_event_vocabulary_gdm_pe<-list() #numer of records with missing event vocabulary
  empty_event_vocabulary_mig<-list() #numer of records with missing event vocabulary
  empty_event_meaning_gdm_pe<-list() #numer of records with missing event meaning
  empty_event_meaning_mig<-list() #numer of records with missing event meaning
  prior_diagnoses_rec_gdm_pe<-list() #number of records with date prior to start study date - 365 days
  prior_diagnoses_rec_mig<-list() #number of records with date prior to start study date - 365 days
  after_diagnoses_rec_gdm_pe<-list() #number of records with date after end study date
  after_diagnoses_rec_mig<-list() #number of records with date after end study date
  included_records_filtering_gdm_pe<-list()
  included_records_filtering_mig<-list()
  w<-1
  #####Run the loop section####
  for (y in 1:length(actual_tables$EVENTS)){
    print(paste0("Analyzing table ",actual_tables$EVENTS[y], "."))
    #Load the table
    df<-fread(paste(path_dir, actual_tables$EVENTS[y], sep=""), stringsAsFactors = FALSE, colClasses = "character")
    cols<-c("person_id", "meaning_of_event", code_var, voc_var, date_var)
    df<-df[,cols, with=F]
    df<-df[, lapply(.SD, FUN=function(x) gsub("^$|^ $", NA, x))] #make sure missing data is read appropriately
    setnames(df,"meaning_of_event","meaning")
    setnames(df, date_var,"event_date")
    setnames(df, code_var,"event_code")
    setnames(df, voc_var,"event_vocabulary")
    original_rows_gdm_pe[[w]]<-df[,.N]
    original_rows_mig[[w]]<-df[,.N]
    #remove empty dates
    empty_event_date_gdm_pe[[w]]<-df[is.na(event_date),.N]
    empty_event_date_mig[[w]]<-df[is.na(event_date),.N]
    df<-df[!is.na(event_date)]
    #remove empty codes
    empty_event_code_gdm_pe[[w]]<-df[is.na(event_code),.N]
    empty_event_code_mig[[w]]<-df[is.na(event_code),.N]
    df<-df[!is.na(event_code)]
    #remove empty voacbularies
    empty_event_vocabulary_gdm_pe[[w]]<-df[is.na(event_vocabulary),.N]
    empty_event_vocabulary_mig[[w]]<-df[is.na(event_vocabulary),.N]
    df<-df[!is.na(event_vocabulary)]
    #empty meaning
    empty_event_meaning_gdm_pe[[w]]<-df[is.na(meaning),.N]
    empty_event_meaning_mig[[w]]<-df[is.na(meaning),.N]
    df<-df[!is.na(meaning)]
    #transform into date variables
    df[,event_date:=as.IDate(event_date,"%Y%m%d")] #transform to date variables
    #create year variable
    df[,year:=year(event_date)]
    #remove all records with dates prior to start study date or start study date plus lookback if any
    #gdm&pe
    if(!is.null(gmd_pe_start_coverage_lookback)){
      df[,prior_gdm_pe:=ifelse(gmd_pe_start_coverage_lookback>event_date,1,0)]
    }else{
      df[,prior_gdm_pe:=ifelse(gdm_pe_start_study_date>event_date,1,0)]
    }
    prior_diagnoses_rec_gdm_pe[[w]]<-df[prior_gdm_pe==1,.N]
    #migraine
    df[,prior_mig:=ifelse(mig_start_study_date>event_date,1,0)]
    prior_diagnoses_rec_mig[[w]]<-df[prior_mig==1,.N]
    #Check if the data needed for the drug utilization and safety study has a longer follow up
    df[,prior_du:=ifelse(du_start_study_date>event_date,1,0)]
    df[,prior_saf:=ifelse(saf_start_study_date>event_date,1,0)]
    #clean up dataset:keep records when at least one is zero
    df<-df[prior_gdm_pe==0|prior_mig==0|prior_du==0|prior_saf==0]
    #remove all records with dates after end study date
    #gdm
    df[,after_gdm_pe:=ifelse(gdm_pe_end_study_date<event_date,1,0)]
    after_diagnoses_rec_gdm_pe[[w]]<-df[after_gdm_pe==1 & prior_gdm_pe==0,.N]
    #migraine
    df[,after_mig:=ifelse(mig_end_study_date<event_date,1,0)]
    after_diagnoses_rec_mig[[w]]<-df[after_mig==1 & prior_mig==0,.N]
    #Check if the data needed for the drug utilization and safety study has a longer follow up
    df[,after_du:=ifelse(du_end_study_date<event_date,1,0)]
    df[,after_saf:=ifelse(saf_end_study_date<event_date,1,0)]
    #clean up dataset:keep records when at least one is zero
    df<-df[after_gdm_pe==0|after_mig==0|after_du==0|after_saf==0]
    #create code with nodot
    df[,code_no_dot:=gsub("\\.","",df[,event_code])]
    included_records_filtering_gdm_pe[[w]]<-df[prior_gdm_pe==0 & after_gdm_pe==0,.N]
    included_records_filtering_mig[[w]]<-df[prior_mig==0 & after_mig==0,.N]
    
    #Create combination inclusion records
    df[prior_gdm_pe==0 | prior_du==0 | prior_saf==0, comb_prior_gdm_pe:=0]
    df[after_gdm_pe==0 | after_du==0 | after_saf==0, comb_after_gdm_pe:=0]
    
    df[prior_mig==0 | prior_du==0 | prior_saf==0, comb_prior_mig:=0]
    df[after_mig==0 | after_du==0 | after_saf==0, comb_after_mig:=0]
    
    
    
    if(df[,.N]>0){
      
      if(df[comb_prior_gdm_pe==0 & comb_after_gdm_pe==0,.N]>0){
      if(events_gdm_diagnoses>0){
        print(paste0("Filtering data for GDM diagnoses:", actual_tables$EVENTS[y]))
      years_study_events<-sort(df[comb_prior_gdm_pe==0 & comb_after_gdm_pe==0][!duplicated(year), year])#years present in this table

      if(sum(df[comb_prior_gdm_pe==0 & comb_after_gdm_pe==0][!duplicated(event_vocabulary), event_vocabulary] %in% vocabularies_list_gdm)>0){
        for (i in 1:length(conditions_gdm)){
          for(j in 1:length(conditions_gdm[[i]])){
            
            codes<-data.table(event_vocabulary=names(conditions_gdm[[i]])[j], truncated_code=conditions_gdm[[i]][[j]], filter=1)
            for(codes_ind in 1:codes[,.N]){
              length<-nchar(codes[codes_ind,truncated_code])
              #create truncated code
              df[comb_prior_gdm_pe==0 & comb_after_gdm_pe==0, truncated_code:=substr(code_no_dot,1,length)]
            
            df<-merge.data.table(df,codes[codes_ind,],by=c("event_vocabulary","truncated_code"),all.x = T,allow.cartesian = T)
            
            if(df[comb_prior_gdm_pe==0 & comb_after_gdm_pe==0][filter==1,.N]>0){
              years_this_event<-sort(df[comb_prior_gdm_pe==0 & comb_after_gdm_pe==0][!duplicated(year),year])
              for(year_ind in 1:length(years_this_event)){
                saveRDS(data.table(df[comb_prior_gdm_pe==0 & comb_after_gdm_pe==0][filter==1 & year==years_this_event[year_ind]], condition=names(conditions_gdm[i])), paste0(tmp,years_this_event[year_ind],"_","GDM_", names(conditions_gdm[i]), "_",actual_tables$EVENTS[y],"_", codes_ind, ".rds"))
              }
            } else {
              years_this_event<-NULL}# new 01.06.2022
            
            rm(years_this_event)
            rm(length)
            if("filter" %in% names(df)){df[,filter:=NULL]}
            if("truncated_code" %in% names(df)){df[,truncated_code:=NULL]}
            }
            rm(codes)
        }
      }
      
      }
      }
      
      if(events_pe_diagnoses>0){
        print(paste0("Filtering data for PE diagnoses:", actual_tables$EVENTS[y]))
        years_study_events<-sort(df[comb_prior_gdm_pe==0 & comb_after_gdm_pe==0][!duplicated(year), year])#years present in this table
      if(sum(df[comb_prior_gdm_pe==0 & comb_after_gdm_pe==0][!duplicated(event_vocabulary), event_vocabulary] %in% vocabularies_list_pe)>0){
        for (i in 1:length(conditions_pe)){
          for(j in 1:length(conditions_pe[[i]])){
            
            codes<-data.table(event_vocabulary=names(conditions_pe[[i]])[j], truncated_code=conditions_pe[[i]][[j]], filter=1)
            for(codes_ind in 1:codes[,.N]){
              length<-nchar(codes[codes_ind,truncated_code])
              #create truncated code
              df[comb_prior_gdm_pe==0 & comb_after_gdm_pe==0,truncated_code:=substr(code_no_dot,1,length)]
              
              df<-merge.data.table(df,codes[codes_ind,],by=c("event_vocabulary","truncated_code"),all.x = T,allow.cartesian = T)
              
              if(df[comb_prior_gdm_pe==0 & comb_after_gdm_pe==0][filter==1,.N]>0){
                years_this_event<-sort(df[comb_prior_gdm_pe==0 & comb_after_gdm_pe==0][filter==1][!duplicated(year),year])
                for(year_ind in 1:length(years_this_event)){
                  saveRDS(data.table(df[comb_prior_gdm_pe==0 & comb_after_gdm_pe==0][filter==1 & year==years_this_event[year_ind]], condition=names(conditions_pe[i])), paste0(tmp,years_this_event[year_ind],"_","PE_", names(conditions_pe[i]), "_",actual_tables$EVENTS[y],"_", codes_ind, ".rds"))
                }
              } else {
                years_this_event<-NULL}# new 01.06.2022
              
              rm(years_this_event)
              rm(length)
              if("filter" %in% names(df)){df[,filter:=NULL]}
              if("truncated_code" %in% names(df)){df[,truncated_code:=NULL]}
            }
            rm(codes)
          }
        }
        
      }
      }
      }
      
    if(df[prior_mig==0 & after_mig==0,.N]>0){
      if(events_migraine_diagnoses>0){
        print(paste0("Filtering data for Migraine diagnoses:", actual_tables$EVENTS[y]))
        years_study_events<-sort(df[comb_prior_mig==0 & comb_after_mig==0][!duplicated(year), year])#years present in this table
        
        #startWith for the event Migraine
        if(sum(df[comb_prior_mig==0 & comb_after_mig==0][!duplicated(event_vocabulary), event_vocabulary] %in% vocabularies_list_migraine_mg)>0){
          for (i in 1:length(conditions_migraine[names(conditions_migraine)=="MG"])){
            for(j in 1:length(conditions_migraine[names(conditions_migraine)=="MG"][[i]])){
              
              codes<-data.table(event_vocabulary=names(conditions_migraine[names(conditions_migraine)=="MG"][[i]])[j], truncated_code=conditions_migraine[names(conditions_migraine)=="MG"][[i]][[j]], filter=1)
              for(codes_ind in 1:codes[,.N]){
                length<-nchar(codes[codes_ind,truncated_code])
                #create truncated code
                df[comb_prior_mig==0 & comb_after_mig==0,truncated_code:=substr(code_no_dot,1,length)]
                
                df<-merge.data.table(df,codes[codes_ind,],by=c("event_vocabulary","truncated_code"),all.x = T,allow.cartesian = T)
                
                if(df[comb_prior_mig==0 & comb_after_mig==0][filter==1,.N]>0){
                  years_this_event<-sort(df[comb_prior_mig==0 & comb_after_mig==0][filter==1][!duplicated(year),year])
                  for(year_ind in 1:length(years_this_event)){
                    saveRDS(data.table(df[comb_prior_mig==0 & comb_after_mig==0][filter==1 & year==years_this_event[year_ind]], condition=names(conditions_migraine[names(conditions_migraine)=="MG"][i])), paste0(tmp,years_this_event[year_ind],"_","Migraine_", names(conditions_migraine[names(conditions_migraine)=="MG"][i]), "_",actual_tables$EVENTS[y],"_", codes_ind, ".rds"))
                  }
                } else {
                  years_this_event<-NULL}# new 01.06.2022
                
                rm(years_this_event)
                rm(length)
                if("filter" %in% names(df)){df[,filter:=NULL]}
                if("truncated_code" %in% names(df)){df[,truncated_code:=NULL]}
              }
              rm(codes)
            }
          }
          
        }
        
        #exact match for the other events related to migraine
        if(sum(df[comb_prior_mig==0 & comb_after_mig==0][!duplicated(event_vocabulary), event_vocabulary] %in% vocabularies_list_migraine)>0){
          for (i in 1:length(conditions_migraine[names(conditions_migraine)!="MG"])){
            for(j in 1:length(conditions_migraine[names(conditions_migraine)!="MG"][[i]])){
              
              codes<-data.table(event_vocabulary=names(conditions_migraine[names(conditions_migraine)!="MG"][[i]])[j], code_no_dot=conditions_migraine[names(conditions_migraine)!="MG"][[i]][[j]], filter=1)
              for(codes_ind in 1:codes[,.N]){
                #create truncated code
                df<-merge.data.table(df,codes[codes_ind,],by=c("event_vocabulary","code_no_dot"),all.x = T,allow.cartesian = T)
                
                if(df[comb_prior_mig==0 & comb_after_mig==0][filter==1,.N]>0){
                  years_this_event<-sort(df[comb_prior_mig==0 & comb_after_mig==0][filter==1][!duplicated(year),year])
                  for(year_ind in 1:length(years_this_event)){
                    saveRDS(data.table(df[comb_prior_mig==0 & comb_after_mig==0][filter==1 & year==years_this_event[year_ind]], condition=names(conditions_migraine[names(conditions_migraine)!="MG"][i])), paste0(tmp,years_this_event[year_ind],"_","Migraine_", names(conditions_migraine[names(conditions_migraine)!="MG"][i]), "_",actual_tables$EVENTS[y],"_", codes_ind, ".rds"))
                  }
                } else {
                  years_this_event<-NULL}# new 01.06.2022
                
                rm(years_this_event)
                if("filter" %in% names(df)){df[,filter:=NULL]}
              }
              rm(codes)
            }
          }
          
        }
      }
    }
      }
    
    w<-w+1
    rm(df)
  }
  ####Combine results####
  #combine flowchart results
  original_rows_gdm_pe<-sum(do.call(rbind,original_rows_gdm_pe))
  original_rows_mig<-sum(do.call(rbind,original_rows_mig))
  empty_event_date_gdm_pe<-sum(do.call(rbind,empty_event_date_gdm_pe))
  empty_event_date_mig<-sum(do.call(rbind,empty_event_date_mig))
  empty_event_code_gdm_pe<-sum(do.call(rbind,empty_event_code_gdm_pe))
  empty_event_code_mig<-sum(do.call(rbind,empty_event_code_mig))
  empty_event_vocabulary_gdm_pe<-sum(do.call(rbind,empty_event_vocabulary_gdm_pe))
  empty_event_vocabulary_mig<-sum(do.call(rbind,empty_event_vocabulary_mig))
  empty_event_meaning_gdm_pe<-sum(do.call(rbind,empty_event_meaning_gdm_pe))
  empty_event_meaning_mig<-sum(do.call(rbind,empty_event_meaning_mig))
  prior_diagnoses_rec_gdm_pe<-sum(do.call(rbind,prior_diagnoses_rec_gdm_pe))
  prior_diagnoses_rec_mig<-sum(do.call(rbind,prior_diagnoses_rec_mig))
  after_diagnoses_rec_gdm_pe<-sum(do.call(rbind,after_diagnoses_rec_gdm_pe))
  after_diagnoses_rec_mig<-sum(do.call(rbind,after_diagnoses_rec_mig))
  included_records_filtering_gdm_pe<-sum(do.call(rbind,included_records_filtering_gdm_pe))
  included_records_filtering_mig<-sum(do.call(rbind,included_records_filtering_mig))
  
  #create flowchart and export to g_output
  flowchart_events_gdm_pe<-data.table(Indicator=c("Number of original rows",
                                           "Number of records with missing event date",
                                           "Number of records with missing event code",
                                           "Number of records with missing event vocabulary",
                                           "Number of records with missing event meaning",
                                           "Number of records with event date before start of study date",
                                           "Number of records with event date after end of study date",
                                           "Number of records included, before data filtering"),
                               EVENTS=c(original_rows_gdm_pe,
                                       empty_event_date_gdm_pe,
                                       empty_event_code_gdm_pe,
                                       empty_event_vocabulary_gdm_pe,
                                       empty_event_meaning_gdm_pe,
                                       prior_diagnoses_rec_gdm_pe,
                                       after_diagnoses_rec_gdm_pe,
                                       included_records_filtering_gdm_pe))
  rm(original_rows_gdm_pe,empty_event_date_gdm_pe,empty_event_code_gdm_pe,empty_event_vocabulary_gdm_pe,empty_event_meaning_gdm_pe,prior_diagnoses_rec_gdm_pe,after_diagnoses_rec_gdm_pe,included_records_filtering_gdm_pe)
  
  flowchart_events_mig<-data.table(Indicator=c("Number of original rows",
                                                  "Number of records with missing event date",
                                                  "Number of records with missing event code",
                                                  "Number of records with missing event vocabulary",
                                                  "Number of records with missing event meaning",
                                                  "Number of records with event date before start of study date",
                                                  "Number of records with event date after end of study date",
                                                  "Number of records included, before data filtering"),
                                      EVENTS=c(original_rows_mig,
                                               empty_event_date_mig,
                                               empty_event_code_mig,
                                               empty_event_vocabulary_mig,
                                               empty_event_meaning_mig,
                                               prior_diagnoses_rec_mig,
                                               after_diagnoses_rec_mig,
                                               included_records_filtering_mig))
  
  #fwrite(flowchart_events, paste0(output_dir, "PE and GDM algorithm/flowchart_events.csv"), row.names = F)
  rm(original_rows_mig,empty_event_date_mig,empty_event_code_mig,empty_event_vocabulary_mig,empty_event_meaning_mig,prior_diagnoses_rec_mig,after_diagnoses_rec_mig,included_records_filtering_mig)
  
} else {
  flowchart_events_gdm_pe<-data.table(Indicator=c("Number of original rows",
                                           "Number of records with missing event date",
                                           "Number of records with missing event code",
                                           "Number of records with missing event vocabulary",
                                           "Number of records with missing event meaning",
                                           "Number of records with event date before start of study date",
                                           "Number of records with event date after end of study date",
                                           "Number of records included, before data filtering"),
                               EVENTS=c(0,
                                        0,
                                        0,
                                        0,
                                        0,
                                        0,
                                        0,
                                        0))
  flowchart_events_mig<-data.table(Indicator=c("Number of original rows",
                                                  "Number of records with missing event date",
                                                  "Number of records with missing event code",
                                                  "Number of records with missing event vocabulary",
                                                  "Number of records with missing event meaning",
                                                  "Number of records with event date before start of study date",
                                                  "Number of records with event date after end of study date",
                                                  "Number of records included, before data filtering"),
                                      EVENTS=c(0,
                                               0,
                                               0,
                                               0,
                                               0,
                                               0,
                                               0,
                                               0))
  
}
}else{
  flowchart_events_gdm_pe<-data.table(Indicator=c("Number of original rows",
                                           "Number of records with missing event date",
                                           "Number of records with missing event code",
                                           "Number of records with missing event vocabulary",
                                           "Number of records with missing event meaning",
                                           "Number of records with event date before start of study date",
                                           "Number of records with event date after end of study date",
                                           "Number of records included, before data filtering"),
                               EVENTS=c(0,
                                        0,
                                        0,
                                        0,
                                        0,
                                        0,
                                        0,
                                        0))
  flowchart_events_mig<-data.table(Indicator=c("Number of original rows",
                                                  "Number of records with missing event date",
                                                  "Number of records with missing event code",
                                                  "Number of records with missing event vocabulary",
                                                  "Number of records with missing event meaning",
                                                  "Number of records with event date before start of study date",
                                                  "Number of records with event date after end of study date",
                                                  "Number of records included, before data filtering"),
                                      EVENTS=c(0,
                                               0,
                                               0,
                                               0,
                                               0,
                                               0,
                                               0,
                                               0))
  
  
  }
####MEDICAL_OBSERVATIONS####
#Load MEDICAL_OBERVATIONS table and apply filter to select PE diagoses/GDM diagnoses/Migraine diagnoses
if(sum(sum(mo_gdm_diagnoses,mo_pe_diagnoses,mo_migraine_diagnoses)>0,
   sum(!is.null(diag_cat_gdm),!is.null(diag_cat_pe),!is.null(diag_cat_migraine),!is.null(diag_checkbox_gdm),!is.null(diag_checkbox_pe_mo)))>0){
  if(codesheet_diagnoses_gdm[table=="MEDICAL_OBSERVATIONS",.N]>0){
  if("code" %in% codesheet_diagnoses_gdm[table=="MEDICAL_OBSERVATIONS",val_1]){
    code_var<-codesheet_diagnoses_gdm[table=="MEDICAL_OBSERVATIONS",col_1]
    voc_var<-codesheet_diagnoses_gdm[table=="MEDICAL_OBSERVATIONS",col_2]
    date_var<-codesheet_diagnoses_gdm[table=="MEDICAL_OBSERVATIONS",date_column]
  }else{
    code_var<-codesheet_diagnoses_gdm[table=="MEDICAL_OBSERVATIONS",col_2]
    voc_var<-codesheet_diagnoses_gdm[table=="MEDICAL_OBSERVATIONS",col_1]
    date_var<-codesheet_diagnoses_gdm[table=="MEDICAL_OBSERVATIONS",date_column]
  }
  }
  if(codesheet_diagnoses_pe[table=="MEDICAL_OBSERVATIONS",.N]>0){
    if("code" %in% codesheet_diagnoses_pe[table=="MEDICAL_OBSERVATIONS",val_1]){
      code_var<-codesheet_diagnoses_pe[table=="MEDICAL_OBSERVATIONS",col_1]
      voc_var<-codesheet_diagnoses_pe[table=="MEDICAL_OBSERVATIONS",col_2]
      date_var<-codesheet_diagnoses_pe[table=="MEDICAL_OBSERVATIONS",date_column]
    }else{
      code_var<-codesheet_diagnoses_pe[table=="MEDICAL_OBSERVATIONS",col_2]
      voc_var<-codesheet_diagnoses_pe[table=="MEDICAL_OBSERVATIONS",col_1]
      date_var<-codesheet_diagnoses_pe[table=="MEDICAL_OBSERVATIONS",date_column]
    }
  }
  if(codesheet_diagnoses_migraine[table=="MEDICAL_OBSERVATIONS",.N]>0){
    if("code" %in% codesheet_diagnoses_migraine[table=="MEDICAL_OBSERVATIONS",val_1]){
      code_var<-codesheet_diagnoses_migraine[table=="MEDICAL_OBSERVATIONS",col_1]
      voc_var<-codesheet_diagnoses_migraine[table=="MEDICAL_OBSERVATIONS",col_2]
      date_var<-codesheet_diagnoses_migraine[table=="MEDICAL_OBSERVATIONS",date_column]
    }else{
      code_var<-codesheet_diagnoses_migraine[table=="MEDICAL_OBSERVATIONS",col_2]
      voc_var<-codesheet_diagnoses_migraine[table=="MEDICAL_OBSERVATIONS",col_1]
      date_var<-codesheet_diagnoses_migraine[table=="MEDICAL_OBSERVATIONS",date_column]
    }
  }
  if(length(actual_tables$MEDICAL_OBSERVATIONS)>0){
    print("Analyse MEDICAL_OBSERVATIONS table.")
    ####List for saving info####
    print("Creating lists to save the information.")
    original_rows<-list() #number of table original rows
    empty_event_date<-list() #number of records with missing event date
    empty_event_code<-list() #numer of records with missing event code
    empty_event_vocabulary<-list() #numer of records with missing event vocabulary
    empty_event_meaning<-list() #numer of records with missing event meaning
    prior_diagnoses_rec<-list() #number of records with date prior to start study date - 365 days
    after_diagnoses_rec<-list() #number of records with date after end study date
    included_records_filtering<-list()
    w<-1
    #####Run the loop section####
    for (y in 1:length(actual_tables$MEDICAL_OBSERVATIONS)){
      print(paste0("Analyzing table ",actual_tables$MEDICAL_OBSERVATIONS[y], "."))
      #Load the table
      df<-fread(paste(path_dir, actual_tables$MEDICAL_OBSERVATIONS[y], sep=""), stringsAsFactors = FALSE, colClasses = "character")
      df<-df[, lapply(.SD, FUN=function(x) gsub("^$|^ $", NA, x))] 
      if(sum(codesheet_diagnoses_gdm[table=="MEDICAL_OBSERVATIONS",.N],
             codesheet_diagnoses_pe[table=="MEDICAL_OBSERVATIONS",.N],
             codesheet_diagnoses_migraine[table=="MEDICAL_OBSERVATIONS",.N])>0){
      cols<-c("person_id", "mo_meaning", code_var, voc_var, date_var)
      df<-df[,cols, with=F]
     #make sure missing data is read appropriately
      setnames(df, date_var,"event_date")
      setnames(df, code_var,"event_code")
      setnames(df, voc_var,"event_vocabulary")
      setnames(df,"mo_meaning","meaning")
      original_rows[[w]]<-df[,.N]
      #remove empty dates
      empty_event_date[[w]]<-df[is.na(event_date),.N]
      df<-df[!is.na(event_date)]
      #remove empty codes
      empty_event_code[[w]]<-df[is.na(event_code),.N]
      df<-df[!is.na(event_code)]
      #remove empty voacbularies
      empty_event_vocabulary[[w]]<-df[is.na(event_vocabulary),.N]
      df<-df[!is.na(event_vocabulary)]
      #empty meaning
      empty_event_meaning[[w]]<-df[is.na(meaning),.N]
      df<-df[!is.na(meaning)]
      #transform into date variables
      df[,event_date:=as.IDate(event_date,"%Y%m%d")] #transform to date variables
      #create year variable
      df[,year:=year(event_date)]
      #remove all records with dates prior to start study date - 365 days
      prior_date<-start_study_date-365.25
      df[,prior:=ifelse(prior_date>event_date,1,0)]
      prior_diagnoses_rec[[w]]<-df[prior==1,.N]
      df<-df[prior==0]
      df[,prior:=NULL]
      #remove all records with dates after end study date
      df[,after:=ifelse(end_study_date<event_date,1,0)]
      after_diagnoses_rec[[w]]<-df[after==1,.N]
      df<-df[after==0]
      df[,after:=NULL]
      #create code with nodot
      df[,code_no_dot:=gsub("\\.","",df[,event_code])]
      included_records_filtering[[w]] <-df[,.N]
      if(df[,.N]>0){
        #data filtering based on codelists
        if(events_gdm_diagnoses>0){
          print(paste0("Filtering data for GDM diagnoses:", actual_tables$MEDICAL_OBSERVATIONS[y]))
          years_study_events<-sort(df[!duplicated(year), year])#years present in this table
          
          if(sum(df[!duplicated(event_vocabulary), event_vocabulary] %in% vocabularies_list_gdm)>0){
            for (i in 1:length(conditions_gdm)){
              for(j in 1:length(conditions_gdm[[i]])){
                
                codes<-data.table(event_vocabulary=names(conditions_gdm[[i]])[j], truncated_code=conditions_gdm[[i]][[j]], filter=1)
                for(codes_ind in 1:codes[,.N]){
                  length<-nchar(codes[codes_ind,truncated_code])
                  #create truncated code
                  df[,truncated_code:=substr(code_no_dot,1,length)]
                  
                  df<-merge.data.table(df,codes[codes_ind,],by=c("event_vocabulary","truncated_code"),all.x = T,allow.cartesian = T)
                  
                  if(df[filter==1,.N]>0){
                    years_this_event<-sort(df[filter==1][!duplicated(year),year])
                    for(year_ind in 1:length(years_this_event)){
                      saveRDS(data.table(df[filter==1 & year==years_this_event[year_ind]], condition=names(conditions_gdm[i])), paste0(tmp,years_this_event[year_ind],"_","GDM_", names(conditions_gdm[i]), "_",actual_tables$MEDICAL_OBSERVATIONS[y],"_", codes_ind, ".rds"))
                    }
                  } else {
                    years_this_event<-NULL}# new 01.06.2022
                  
                  rm(years_this_event)
                  rm(length)
                  if("filter" %in% names(df)){df[,filter:=NULL]}
                  if("truncated_code" %in% names(df)){df[,truncated_code:=NULL]}
                }
                rm(codes)
              }
            }
            
          }
        }
        
        if(mo_pe_diagnoses>0){
          print(paste0("Filtering data for PE diagnoses:", actual_tables$MEDICAL_OBSERVATIONS[y]))
          years_study_events<-sort(df[!duplicated(year), year])#years present in this table
          if(sum(df[!duplicated(event_vocabulary), event_vocabulary] %in% vocabularies_list_pe)>0){
            for (i in 1:length(conditions_pe)){
              for(j in 1:length(conditions_pe[[i]])){
                
                codes<-data.table(event_vocabulary=names(conditions_pe[[i]])[j], truncated_code=conditions_pe[[i]][[j]], filter=1)
                for(codes_ind in 1:codes[,.N]){
                  length<-nchar(codes[codes_ind,truncated_code])
                  #create truncated code
                  df[,truncated_code:=substr(code_no_dot,1,length)]
                  
                  df<-merge.data.table(df,codes[codes_ind,],by=c("event_vocabulary","truncated_code"),all.x = T,allow.cartesian = T)
                  
                  if(df[filter==1,.N]>0){
                    years_this_event<-sort(df[filter==1][!duplicated(year),year])
                    for(year_ind in 1:length(years_this_event)){
                      saveRDS(data.table(df[filter==1 & year==years_this_event[year_ind]], condition=names(conditions_pe[i])), paste0(tmp,years_this_event[year_ind],"_","PE_", names(conditions_pe[i]), "_",actual_tables$MEDICAL_OBSERVATIONS[y],"_", codes_ind, ".rds"))
                    }
                  } else {
                    years_this_event<-NULL}# new 01.06.2022
                  
                  rm(years_this_event)
                  rm(length)
                  if("filter" %in% names(df)){df[,filter:=NULL]}
                  if("truncated_code" %in% names(df)){df[,truncated_code:=NULL]}
                }
                rm(codes)
              }
            }
            
          }
        }
        
        if(mo_migraine_diagnoses>0){
          print(paste0("Filtering data for Migraine diagnoses:", actual_tables$MEDICAL_OBSERVATIONS[y]))
          years_study_events<-sort(df[!duplicated(year), year])#years present in this table
          
          #startWith for the event Migraine
          if(sum(df[!duplicated(event_vocabulary), event_vocabulary] %in% vocabularies_list_migraine_mg)>0){
            for (i in 1:length(conditions_migraine[names(conditions_migraine)=="MG"])){
              for(j in 1:length(conditions_migraine[names(conditions_migraine)=="MG"][[i]])){
                
                codes<-data.table(event_vocabulary=names(conditions_migraine[names(conditions_migraine)=="MG"][[i]])[j], truncated_code=conditions_migraine[names(conditions_migraine)=="MG"][[i]][[j]], filter=1)
                for(codes_ind in 1:codes[,.N]){
                  length<-nchar(codes[codes_ind,truncated_code])
                  #create truncated code
                  df[,truncated_code:=substr(code_no_dot,1,length)]
                  
                  df<-merge.data.table(df,codes[codes_ind,],by=c("event_vocabulary","truncated_code"),all.x = T,allow.cartesian = T)
                  
                  if(df[filter==1,.N]>0){
                    years_this_event<-sort(df[filter==1][!duplicated(year),year])
                    for(year_ind in 1:length(years_this_event)){
                      saveRDS(data.table(df[filter==1 & year==years_this_event[year_ind]], condition=names(conditions_migraine[names(conditions_migraine)=="MG"][i])), paste0(tmp,years_this_event[year_ind],"_","Migraine_", names(conditions_migraine[names(conditions_migraine)=="MG"][i]), "_",actual_tables$MEDICAL_OBSERVATIONS[y],"_", codes_ind, ".rds"))
                    }
                  } else {
                    years_this_event<-NULL}# new 01.06.2022
                  
                  rm(years_this_event)
                  rm(length)
                  if("filter" %in% names(df)){df[,filter:=NULL]}
                  if("truncated_code" %in% names(df)){df[,truncated_code:=NULL]}
                }
                rm(codes)
              }
            }
            
          }
          
          #exact match for the other events related to migraine
          if(sum(df[!duplicated(event_vocabulary), event_vocabulary] %in% vocabularies_list_migraine)>0){
            for (i in 1:length(conditions_migraine[names(conditions_migraine)!="MG"])){
              for(j in 1:length(conditions_migraine[names(conditions_migraine)!="MG"][[i]])){
                
                codes<-data.table(event_vocabulary=names(conditions_migraine[names(conditions_migraine)!="MG"][[i]])[j], code_no_dot=conditions_migraine[names(conditions_migraine)!="MG"][[i]][[j]], filter=1)
                for(codes_ind in 1:codes[,.N]){
                  #create truncated code
                  df<-merge.data.table(df,codes[codes_ind,],by=c("event_vocabulary","code_no_dot"),all.x = T,allow.cartesian = T)
                  
                  if(df[filter==1,.N]>0){
                    years_this_event<-sort(df[filter==1][!duplicated(year),year])
                    for(year_ind in 1:length(years_this_event)){
                      saveRDS(data.table(df[filter==1 & year==years_this_event[year_ind]], condition=names(conditions_migraine[names(conditions_migraine)!="MG"][i])), paste0(tmp,years_this_event[year_ind],"_","Migraine_", names(conditions_migraine[names(conditions_migraine)!="MG"][i]), "_",actual_tables$MEDICAL_OBSERVATIONS[y],"_", codes_ind, ".rds"))
                    }
                  } else {
                    years_this_event<-NULL}# new 01.06.2022
                  
                  rm(years_this_event)
                  if("filter" %in% names(df)){df[,filter:=NULL]}
                }
                rm(codes)
              }
            }
            
          }
        }
      }
      }
      
      #Change name back
      if(sum(codesheet_diagnoses_gdm[table=="MEDICAL_OBSERVATIONS",.N],
             codesheet_diagnoses_pe[table=="MEDICAL_OBSERVATIONS",.N],
             codesheet_diagnoses_migraine[table=="MEDICAL_OBSERVATIONS",.N])>0){
        setnames(df,"event_date",date_var)
        setnames(df, "event_code",code_var)
        setnames(df, "event_vocabulary",voc_var)
        setnames(df,"meaning", "mo_meaning")
      }
      
      if(df[,.N]>0){
      #Gather information about not fixed values
      #Diagnoses GDM
      if(!is.null(diag_cat_gdm)){
        gdm_diag_cat_mo<-gdm_diag_cat[table=="MEDICAL_OBSERVATIONS"]
        if(gdm_diag_cat_mo[,.N]>0){
          names_df<-names(df)
          same_names<-intersect(names_df,names(gdm_diag_cat_mo))
          df<-merge.data.table(df,diag_cat_gdm,by=same_names,all.x=T)
          if(df[!is.na(event_abbreviation),.N]>0){
          for(cat_ind in 1:gdm_diag_cat_mo[,.N]){
            cols_to_keep<-c(gdm_diag_cat_mo[cat_ind==index,date_column],gdm_diag_cat_mo[cat_ind==index,keep])
            #rename columns
            setnames(df,gdm_diag_cat_mo[cat_ind==index,date_column], "event_date")
            setnames(df,gdm_diag_cat_mo[cat_ind==index,keep], "event_code")
            setnames(df, "so_meaning", "meaning")
            setnames(df, "event_abbreviation", "condition")
            
            #date variable
            df[,event_date:=as.IDate(event_date, "%Y%m%d")]
            if(!"year" %in% names(df)){df[,year:=year(event_date)]}
            
            cols_to_keep<-c("event_date","event_code", "person_id","meaning","condition","year")
            years_cat<-sort(df[!is.na(condition) & index==cat_ind][!duplicated(year),year])
            for(year_ind in 1:length(years_cat)){
              saveRDS(df[!is.na(condition)][year==years_cat[year_ind] & index==cat_ind, cols_to_keep, with=F], paste0(projectFolder,"/g_intermediate/tmp/", years_cat[year_ind],"_", "GDM_diagnoses_cat_mo",cat_ind,".rds"))
               }# new 01.06.2022
          
          rm(years_cat,year_ind)
          
            #rename columns
            setnames(df, "event_date", gdm_diag_cat_mo[cat_ind==index,date_column])
            setnames(df,"event_code", gdm_diag_cat_mo[cat_ind==index,keep])
            setnames(df, "meaning", "so_meaning")
            setnames(df, "condition", "event_abbreviation")
            
          }
          df[,table:=NULL][,event_abbreviation:=NULL][,keep:=NULL][,index:=NULL]
        }
        }
      }
      
      #PE
      if(!is.null(diag_cat_pe)){
        pe_diag_cat_mo<-pe_diag_cat[table=="MEDICAL_OBSERVATIONS"]
        if(pe_diag_cat_mo[,.N]>0){
          names_df<-names(df)
          same_names<-intersect(names_df,names(pe_diag_cat_mo))
          df<-merge.data.table(df,diag_cat_pe,by=same_names,all.x=T)
          if(df[!is.na(event_abbreviation),.N]>0){
          for(cat_ind in 1:pe_diag_cat_mo[,.N]){
            cols_to_keep<-c(pe_diag_cat_mo[cat_ind==index,date_column],pe_diag_cat_mo[cat_ind==index,keep])
            #rename columns
            setnames(df,pe_diag_cat_mo[cat_ind==index,date_column], "event_date")
            setnames(df,pe_diag_cat_mo[cat_ind==index,keep], "event_code")
            setnames(df, "so_meaning", "meaning")
            setnames(df, "event_abbreviation", "condition")
            
            #date variable
            df[,event_date:=as.IDate(event_date, "%Y%m%d")]
            if(!"year" %in% names(df)){df[,year:=year(event_date)]}
            
            cols_to_keep<-c("event_date","event_code", "person_id","meaning","condition","year")
            
            years_cat<-sort(df[!is.na(condition) & index==cat_ind][!duplicated(year),year])
            for(year_ind in 1:length(years_cat)){
              saveRDS(df[!is.na(condition)][year==years_cat[year_ind] & index==cat_ind, cols_to_keep, with=F], paste0(projectFolder,"/g_intermediate/tmp/", years_cat[year_ind],"_", "PE_diagnoses_cat_mo",cat_ind,".rds"))
            }# new 01.06.2022
          
          rm(years_cat,year_ind)

            #rename columns
            setnames(df, "event_date", pe_diag_cat_mo[cat_ind==index,date_column])
            setnames(df,"event_code", pe_diag_cat_mo[cat_ind==index,keep])
            setnames(df, "meaning", "so_meaning")
            setnames(df, "condition", "event_abbreviation")
            
          }
          df[,table:=NULL][,event_abbreviation:=NULL][,keep:=NULL][,index:=NULL]
        }
      }
      }
      
      #Migraine
      if(!is.null(diag_cat_migraine)){
        migraine_diag_cat_mo<-migraine_diag_cat[table=="MEDICAL_OBSERVATIONS"]
        if(migraine_diag_cat_mo[,.N]>0){
          names_df<-names(df)
          same_names<-intersect(names_df,names(migraine_diag_cat_mo))
          df<-merge.data.table(df,diag_cat_migraine,by=same_names,all.x=T)
          if(df[!is.na(event_abbreviation),.N]>0){
          for(cat_ind in 1:migraine_diag_cat_mo[,.N]){
            cols_to_keep<-c(migraine_diag_cat_mo[cat_ind==index,date_column],migraine_diag_cat_mo[cat_ind==index,keep])
            #rename columns
            setnames(df,migraine_diag_cat_mo[cat_ind==index,date_column], "event_date")
            setnames(df,migraine_diag_cat_mo[cat_ind==index,keep], "event_code")
            setnames(df, "so_meaning", "meaning")
            setnames(df, "event_abbreviation", "condition")
            
            #date variable
            df[,event_date:=as.IDate(event_date, "%Y%m%d")]
            if(!"year" %in% names(df)){df[,year:=year(event_date)]}
            
            cols_to_keep<-c("event_date","event_code", "person_id","meaning","condition","year")
            years_cat<-sort(df[!is.na(condition) & index==cat_ind][!duplicated(year),year])
            for(year_ind in 1:length(years_cat)){
              saveRDS(df[!is.na(condition)][year==years_cat[year_ind] & index==cat_ind, cols_to_keep, with=F], paste0(projectFolder,"/g_intermediate/tmp/", years_cat[year_ind],"_", "Migraine_diagnoses_cat_mo",cat_ind,".rds"))
            }# new 01.06.2022
          
          rm(years_cat,year_ind)
          
            #rename columns
            setnames(df, "event_date", migraine_diag_cat_mo[cat_ind==index,date_column])
            setnames(df,"event_code", migraine_diag_cat_mo[cat_ind==index,keep])
            setnames(df, "meaning", "so_meaning")
            setnames(df, "condition", "event_abbreviation")
            
          }
          df[,table:=NULL][,event_abbreviation:=NULL][,keep:=NULL][,index:=NULL]
        }
      }
      }
        
      #Checkbox information
        #GDM
        if(!is.null(diag_checkbox_gdm)){
          gdm_diag_checkbox_mo<-diag_checkbox_gdm[table=="MEDICAL_OBSERVATIONS"]
          if(gdm_diag_checkbox_mo[,.N]>0){
            names_df<-names(df)
            same_names<-intersect(names_df,names(gdm_diag_checkbox_mo))
            df<-merge.data.table(df,gdm_diag_checkbox_mo,by=same_names,all.x=T)
            if(df[!is.na(event_abbreviation),.N]>0){
              for(cat_ind in 1:gdm_diag_checkbox_mo[,.N]){
                cols_to_keep<-c(gdm_diag_checkbox_mo[cat_ind==index,checkbox_date],gdm_diag_checkbox_mo[cat_ind==index,keep])
                #rename columns
                setnames(df,gdm_diag_checkbox_mo[cat_ind==index,checkbox_date], "event_date")
                setnames(df,gdm_diag_checkbox_mo[cat_ind==index,keep], "event_code")
                setnames(df, "so_meaning", "meaning")
                setnames(df, "event_abbreviation", "condition")
                
                #date variable
                df[,event_date:=as.IDate(event_date, "%Y%m%d")]
                if(!"year" %in% names(df)){df[,year:=year(event_date)]}
                
                cols_to_keep<-c("event_date","event_code", "person_id","meaning","condition","year")
                years_cat<-sort(df[!is.na(condition) & index==cat_ind][!duplicated(year),year])
                for(year_ind in 1:length(years_cat)){
                  saveRDS(df[!is.na(condition)][year==years_cat[year_ind] & index==cat_ind, cols_to_keep, with=F], paste0(projectFolder,"/g_intermediate/tmp/", years_cat[year_ind],"_", "GDM_diagnoses_checkbox_mo",cat_ind,".rds"))
                }# new 01.06.2022
                
                rm(years_cat,year_ind)
                
                #rename columns
                setnames(df, "event_date", gdm_diag_checkbox_mo[cat_ind==index,checkbox_date])
                setnames(df,"event_code", gdm_diag_checkbox_mo[cat_ind==index,keep])
                setnames(df, "meaning", "so_meaning")
                setnames(df, "condition", "event_abbreviation")
                
              }
              df[,table:=NULL][,event_abbreviation:=NULL][,keep:=NULL][,index:=NULL]
            }
          }
        }
        
        #PE
        if(!is.null(diag_checkbox_pe_mo)){
          pe_diag_checkbox_mo<-diag_checkbox_pe_mo[table=="MEDICAL_OBSERVATIONS"]
          if(pe_diag_checkbox_mo[,.N]>0){
            names_df<-names(df)
            same_names<-intersect(names_df,names(pe_diag_checkbox_mo))
            df<-merge.data.table(df,pe_diag_checkbox_mo,by=same_names,all.x=T)
            if(df[!is.na(event_abbreviation),.N]>0){
              for(cat_ind in 1:pe_diag_checkbox_mo[,.N]){
                cols_to_keep<-c(pe_diag_checkbox_mo[cat_ind==index,checkbox_date],pe_diag_checkbox_mo[cat_ind==index,keep])
                #rename columns
                setnames(df,pe_diag_checkbox_mo[cat_ind==index,checkbox_date], "event_date")
                setnames(df,pe_diag_checkbox_mo[cat_ind==index,keep], "event_code")
                setnames(df, "so_meaning", "meaning")
                setnames(df, "event_abbreviation", "condition")
                
                #date variable
                df[,event_date:=as.IDate(event_date, "%Y%m%d")]
                if(!"year" %in% names(df)){df[,year:=year(event_date)]}
                
                cols_to_keep<-c("event_date","event_code", "person_id","meaning","condition","year")
                years_cat<-sort(df[!is.na(condition) & index==cat_ind][!duplicated(year),year])
                for(year_ind in 1:length(years_cat)){
                  saveRDS(df[!is.na(condition)][year==years_cat[year_ind] & index==cat_ind, cols_to_keep, with=F], paste0(projectFolder,"/g_intermediate/tmp/", years_cat[year_ind],"_", "PE_diagnoses_checkbox_mo",cat_ind,".rds"))
                }# new 01.06.2022
                
                rm(years_cat,year_ind)
                
                #rename columns
                setnames(df, "event_date", pe_diag_checkbox_mo[cat_ind==index,checkbox_date])
                setnames(df,"event_code", pe_diag_checkbox_mo[cat_ind==index,keep])
                setnames(df, "meaning", "so_meaning")
                setnames(df, "condition", "event_abbreviation")
                
              }
              df[,table:=NULL][,event_abbreviation:=NULL][,keep:=NULL][,index:=NULL]
            }
          }
        }
    }
      w<-w+1
      rm(df)
    }
    ####Combine results####
    #combine flowchart results
    original_rows<-sum(do.call(rbind,original_rows))
    empty_event_date<-sum(do.call(rbind,empty_event_date))
    empty_event_code<-sum(do.call(rbind,empty_event_code))
    empty_event_vocabulary<-sum(do.call(rbind,empty_event_vocabulary))
    empty_event_meaning<-sum(do.call(rbind,empty_event_meaning))
    prior_diagnoses_rec<-sum(do.call(rbind,prior_diagnoses_rec))
    after_diagnoses_rec<-sum(do.call(rbind,after_diagnoses_rec))
    included_records_filtering<-sum(do.call(rbind,included_records_filtering))
    
    #create flowchart and export to g_output
    flowchart_mo<-data.table(Indicator=c("Number of original rows",
                                         "Number of records with missing event date",
                                         "Number of records with missing event code",
                                         "Number of records with missing event vocabulary",
                                         "Number of records with missing event meaning",
                                         "Number of records with event date before start of study date",
                                         "Number of records with event date after end of study date",
                                         "Number of records included, before data filtering"),
                             MEDICAL_OBSERVATIONS=c(original_rows,
                                                    empty_event_date,
                                                    empty_event_code,
                                                    empty_event_vocabulary,
                                                    empty_event_meaning,
                                                    prior_diagnoses_rec,
                                                    after_diagnoses_rec,
                                                    included_records_filtering))
    #fwrite(flowchart_events, paste0(output_dir, "PE and GDM algorithm/flowchart_events.csv"), row.names = F)
    rm(original_rows,empty_event_date,empty_event_code,empty_event_vocabulary,empty_event_meaning,prior_diagnoses_rec,after_diagnoses_rec,included_records_filtering)
    
  }else{
    flowchart_mo<-data.table(Indicator=c("Number of original rows",
                                         "Number of records with missing event date",
                                         "Number of records with missing event code",
                                         "Number of records with missing event vocabulary",
                                         "Number of records with missing event meaning",
                                         "Number of records with event date before start of study date",
                                         "Number of records with event date after end of study date",
                                         "Number of records included, before data filtering"),
                             MEDICAL_OBSERVATIONS=c(0,
                                                    0,
                                                    0,
                                                    0,
                                                    0,
                                                    0,
                                                    0,
                                                    0))
  }
  }else{
  flowchart_mo<-data.table(Indicator=c("Number of original rows",
                                       "Number of records with missing event date",
                                       "Number of records with missing event code",
                                       "Number of records with missing event vocabulary",
                                       "Number of records with missing event meaning",
                                       "Number of records with event date before start of study date",
                                       "Number of records with event date after end of study date",
                                       "Number of records included, before data filtering"),
                           MEDICAL_OBSERVATIONS=c(0,
                                                  0,
                                                  0,
                                                  0,
                                                  0,
                                                  0,
                                                  0,
                                                  0))
  
}
####SURVEY_OBSERVATIONS####
#Load SURVEY_OBERVATIONS table and apply filter to select PE diagoses/GDM diagnoses/Migraine diagnoses
if(sum(sum(so_gdm_diagnoses,so_pe_diagnoses,so_migraine_diagnoses)>0,
       sum(!is.null(diag_cat_gdm),!is.null(diag_cat_pe),!is.null(diag_cat_migraine),!is.null(diag_checkbox_gdm),!is.null(diag_checkbox_pe_so)))>0){
  if(codesheet_diagnoses_gdm[table=="SURVEY_OBSERVATIONS",.N]>0){
  if("code" %in% codesheet_diagnoses_gdm[table=="SURVEY_OBSERVATIONS",val_1]){
    code_var<-codesheet_diagnoses_gdm[table=="SURVEY_OBSERVATIONS",col_1]
    voc_var<-codesheet_diagnoses_gdm[table=="SURVEY_OBSERVATIONS",col_2]
    date_var<-codesheet_diagnoses_gdm[table=="SURVEY_OBSERVATIONS",date_column]
  }else{
    code_var<-codesheet_diagnoses_gdm[table=="SURVEY_OBSERVATIONS",col_2]
    voc_var<-codesheet_diagnoses_gdm[table=="SURVEY_OBSERVATIONS",col_1]
    date_var<-codesheet_diagnoses_gdm[table=="SURVEY_OBSERVATIONS",date_column]
  }
  }
  if(codesheet_diagnoses_pe[table=="SURVEY_OBSERVATIONS",.N]>0){
    if("code" %in% codesheet_diagnoses_pe[table=="SURVEY_OBSERVATIONS",val_1]){
      code_var<-codesheet_diagnoses_pe[table=="SURVEY_OBSERVATIONS",col_1]
      voc_var<-codesheet_diagnoses_pe[table=="SURVEY_OBSERVATIONS",col_2]
      date_var<-codesheet_diagnoses_pe[table=="SURVEY_OBSERVATIONS",date_column]
    }else{
      code_var<-codesheet_diagnoses_pe[table=="SURVEY_OBSERVATIONS",col_2]
      voc_var<-codesheet_diagnoses_pe[table=="SURVEY_OBSERVATIONS",col_1]
      date_var<-codesheet_diagnoses_pe[table=="SURVEY_OBSERVATIONS",date_column]
    }
  }
  if(codesheet_diagnoses_migraine[table=="SURVEY_OBSERVATIONS",.N]>0){
    if("code" %in% codesheet_diagnoses_migraine[table=="SURVEY_OBSERVATIONS",val_1]){
      code_var<-codesheet_diagnoses_migraine[table=="SURVEY_OBSERVATIONS",col_1]
      voc_var<-codesheet_diagnoses_migraine[table=="SURVEY_OBSERVATIONS",col_2]
      date_var<-codesheet_diagnoses_migraine[table=="SURVEY_OBSERVATIONS",date_column]
    }else{
      code_var<-codesheet_diagnoses_migraine[table=="SURVEY_OBSERVATIONS",col_2]
      voc_var<-codesheet_diagnoses_migraine[table=="SURVEY_OBSERVATIONS",col_1]
      date_var<-codesheet_diagnoses_migraine[table=="SURVEY_OBSERVATIONS",date_column]
    }
  }
  if(length(actual_tables$SURVEY_OBSERVATIONS)>0){
    print("Analyse SURVEY_OBSERVATIONS table.")
    ####List for saving info####
    print("Creating lists to save the information.")
    original_rows<-list() #number of table original rows
    empty_event_date<-list() #number of records with missing event date
    empty_event_code<-list() #numer of records with missing event code
    empty_event_vocabulary<-list() #numer of records with missing event vocabulary
    empty_event_meaning<-list() #numer of records with missing event meaning
    prior_diagnoses_rec<-list() #number of records with date prior to start study date - 365 days
    after_diagnoses_rec<-list() #number of records with date after end study date
    included_records_filtering<-list()
    w<-1
    #####Run the loop section####
    for (y in 1:length(actual_tables$SURVEY_OBSERVATIONS)){
      print(paste0("Analyzing table ",actual_tables$SURVEY_OBSERVATIONS[y], "."))
      #Load the table
      df<-fread(paste(path_dir, actual_tables$SURVEY_OBSERVATIONS[y], sep=""), stringsAsFactors = FALSE, colClasses = "character")
      df<-df[, lapply(.SD, FUN=function(x) gsub("^$|^ $", NA, x))] #make sure missing data is read appropriately
      if(sum(codesheet_diagnoses_gdm[table=="SURVEY_OBSERVATIONS",.N],
             codesheet_diagnoses_pe[table=="SURVEY_OBSERVATIONS",.N],
             codesheet_diagnoses_migraine[table=="SURVEY_OBSERVATIONS",.N])>0){
      cols<-c("person_id", "so_meaning", code_var, voc_var, date_var)
      df<-df[,cols, with=F]
      setnames(df, date_var,"event_date")
      setnames(df, code_var,"event_code")
      setnames(df, voc_var,"event_vocabulary")
      setnames(df,"so_meaning","meaning")
      original_rows[[w]]<-df[,.N]
      #remove empty dates
      empty_event_date[[w]]<-df[is.na(event_date),.N]
      df<-df[!is.na(event_date)]
      #remove empty codes
      empty_event_code[[w]]<-df[is.na(event_code),.N]
      df<-df[!is.na(event_code)]
      #remove empty voacbularies
      empty_event_vocabulary[[w]]<-df[is.na(event_vocabulary),.N]
      df<-df[!is.na(event_vocabulary)]
      #empty meaning
      empty_event_meaning[[w]]<-df[is.na(meaning),.N]
      df<-df[!is.na(meaning)]
      #transform into date variables
      df[,event_date:=as.IDate(event_date,"%Y%m%d")] #transform to date variables
      #create year variable
      df[,year:=year(event_date)]
      #remove all records with dates prior to start study date - 365 days
      prior_date<-start_study_date-365.25
      df[,prior:=ifelse(prior_date>event_date,1,0)]
      prior_diagnoses_rec[[w]]<-df[prior==1,.N]
      df<-df[prior==0]
      df[,prior:=NULL]
      #remove all records with dates after end study date
      df[,after:=ifelse(end_study_date<event_date,1,0)]
      after_diagnoses_rec[[w]]<-df[after==1,.N]
      df<-df[after==0]
      df[,after:=NULL]
      #create code with nodot
      df[,code_no_dot:=gsub("\\.","",df[,event_code])]
      included_records_filtering[[w]] <-df[,.N]
      if(df[,.N]>0){
        
        if(events_gdm_diagnoses>0){
          print(paste0("Filtering data for GDM diagnoses:", actual_tables$SURVEY_OBSERVATIONS[y]))
          years_study_events<-sort(df[!duplicated(year), year])#years present in this table
          
          if(sum(df[!duplicated(event_vocabulary), event_vocabulary] %in% vocabularies_list_gdm)>0){
            for (i in 1:length(conditions_gdm)){
              for(j in 1:length(conditions_gdm[[i]])){
                
                codes<-data.table(event_vocabulary=names(conditions_gdm[[i]])[j], truncated_code=conditions_gdm[[i]][[j]], filter=1)
                for(codes_ind in 1:codes[,.N]){
                  length<-nchar(codes[codes_ind,truncated_code])
                  #create truncated code
                  df[,truncated_code:=substr(code_no_dot,1,length)]
                  
                  df<-merge.data.table(df,codes[codes_ind,],by=c("event_vocabulary","truncated_code"),all.x = T,allow.cartesian = T)
                  
                  if(df[filter==1,.N]>0){
                    years_this_event<-sort(df[filter==1][!duplicated(year),year])
                    for(year_ind in 1:length(years_this_event)){
                      saveRDS(data.table(df[filter==1 & year==years_this_event[year_ind]], condition=names(conditions_gdm[i])), paste0(tmp,years_this_event[year_ind],"_","GDM_", names(conditions_gdm[i]), "_",actual_tables$SURVEY_OBSERVATIONS[y],"_", codes_ind, ".rds"))
                    }
                  } else {
                    years_this_event<-NULL}# new 01.06.2022
                  
                  rm(years_this_event)
                  rm(length)
                  if("filter" %in% names(df)){df[,filter:=NULL]}
                  if("truncated_code" %in% names(df)){df[,truncated_code:=NULL]}
                }
                rm(codes)
              }
            }
            
          }
        }
        
        if(mo_pe_diagnoses>0){
          print(paste0("Filtering data for PE diagnoses:", actual_tables$SURVEY_OBSERVATIONS[y]))
          years_study_events<-sort(df[!duplicated(year), year])#years present in this table
          if(sum(df[!duplicated(event_vocabulary), event_vocabulary] %in% vocabularies_list_pe)>0){
            for (i in 1:length(conditions_pe)){
              for(j in 1:length(conditions_pe[[i]])){
                
                codes<-data.table(event_vocabulary=names(conditions_pe[[i]])[j], truncated_code=conditions_pe[[i]][[j]], filter=1)
                for(codes_ind in 1:codes[,.N]){
                  length<-nchar(codes[codes_ind,truncated_code])
                  #create truncated code
                  df[,truncated_code:=substr(code_no_dot,1,length)]
                  
                  df<-merge.data.table(df,codes[codes_ind,],by=c("event_vocabulary","truncated_code"),all.x = T,allow.cartesian = T)
                  
                  if(df[filter==1,.N]>0){
                    years_this_event<-sort(df[filter==1][!duplicated(year),year])
                    for(year_ind in 1:length(years_this_event)){
                      saveRDS(data.table(df[filter==1 & year==years_this_event[year_ind]], condition=names(conditions_pe[i])), paste0(tmp,years_this_event[year_ind],"_","PE_", names(conditions_pe[i]), "_",actual_tables$SURVEY_OBSERVATIONS[y],"_", codes_ind, ".rds"))
                    }
                  } else {
                    years_this_event<-NULL}# new 01.06.2022
                  
                  rm(years_this_event)
                  rm(length)
                  if("filter" %in% names(df)){df[,filter:=NULL]}
                  if("truncated_code" %in% names(df)){df[,truncated_code:=NULL]}
                }
                rm(codes)
              }
            }
            
          }
        }
        
        if(so_migraine_diagnoses>0){
          print(paste0("Filtering data for Migraine diagnoses:", actual_tables$SURVEY_OBSERVATIONS[y]))
          years_study_events<-sort(df[!duplicated(year), year])#years present in this table
          
          #startWith for the event Migraine
          if(sum(df[!duplicated(event_vocabulary), event_vocabulary] %in% vocabularies_list_migraine_mg)>0){
            for (i in 1:length(conditions_migraine[names(conditions_migraine)=="MG"])){
              for(j in 1:length(conditions_migraine[names(conditions_migraine)=="MG"][[i]])){
                
                codes<-data.table(event_vocabulary=names(conditions_migraine[names(conditions_migraine)=="MG"][[i]])[j], truncated_code=conditions_migraine[names(conditions_migraine)=="MG"][[i]][[j]], filter=1)
                for(codes_ind in 1:codes[,.N]){
                  length<-nchar(codes[codes_ind,truncated_code])
                  #create truncated code
                  df[,truncated_code:=substr(code_no_dot,1,length)]
                  
                  df<-merge.data.table(df,codes[codes_ind,],by=c("event_vocabulary","truncated_code"),all.x = T,allow.cartesian = T)
                  
                  if(df[filter==1,.N]>0){
                    years_this_event<-sort(df[filter==1][!duplicated(year),year])
                    for(year_ind in 1:length(years_this_event)){
                      saveRDS(data.table(df[filter==1 & year==years_this_event[year_ind]], condition=names(conditions_migraine[names(conditions_migraine)=="MG"][i])), paste0(tmp,years_this_event[year_ind],"_","Migraine_", names(conditions_migraine[names(conditions_migraine)=="MG"][i]), "_",actual_tables$SURVEY_OBSERVATIONS[y],"_", codes_ind, ".rds"))
                    }
                  } else {
                    years_this_event<-NULL}# new 01.06.2022
                  
                  rm(years_this_event)
                  rm(length)
                  if("filter" %in% names(df)){df[,filter:=NULL]}
                  if("truncated_code" %in% names(df)){df[,truncated_code:=NULL]}
                }
                rm(codes)
              }
            }
            
          }
          
          #exact match for the other events related to migraine
          if(sum(df[!duplicated(event_vocabulary), event_vocabulary] %in% vocabularies_list_migraine)>0){
            for (i in 1:length(conditions_migraine[names(conditions_migraine)!="MG"])){
              for(j in 1:length(conditions_migraine[names(conditions_migraine)!="MG"][[i]])){
                
                codes<-data.table(event_vocabulary=names(conditions_migraine[names(conditions_migraine)!="MG"][[i]])[j], code_no_dot=conditions_migraine[names(conditions_migraine)!="MG"][[i]][[j]], filter=1)
                for(codes_ind in 1:codes[,.N]){
                  #create truncated code
                  df<-merge.data.table(df,codes[codes_ind,],by=c("event_vocabulary","code_no_dot"),all.x = T,allow.cartesian = T)
                  
                  if(df[filter==1,.N]>0){
                    years_this_event<-sort(df[filter==1][!duplicated(year),year])
                    for(year_ind in 1:length(years_this_event)){
                      saveRDS(data.table(df[filter==1 & year==years_this_event[year_ind]], condition=names(conditions_migraine[names(conditions_migraine)!="MG"][i])), paste0(tmp,years_this_event[year_ind],"_","Migraine_", names(conditions_migraine[names(conditions_migraine)!="MG"][i]), "_",actual_tables$SURVEY_OBSERVATIONS[y],"_", codes_ind, ".rds"))
                    }
                  } else {
                    years_this_event<-NULL}# new 01.06.2022
                  
                  rm(years_this_event)
                  if("filter" %in% names(df)){df[,filter:=NULL]}
                }
                rm(codes)
              }
            }
            
          }
        }
      }
      }
      
      #Change name back
      if(sum(codesheet_diagnoses_gdm[table=="SURVEY_OBSERVATIONS",.N],
             codesheet_diagnoses_pe[table=="SURVEY_OBSERVATIONS",.N],
             codesheet_diagnoses_migraine[table=="SURVEY_OBSERVATIONS",.N])>0){
      setnames(df,"event_date",date_var)
      setnames(df, "event_code",code_var)
      setnames(df, "event_vocabulary",voc_var)
      setnames(df,"meaning", "so_meaning")
      }
      
      if(df[,.N]>0){
      #check for not fixed info
      #GDM
      if(!is.null(diag_cat_gdm)){
      gdm_diag_cat_so<-gdm_diag_cat[table=="SURVEY_OBSERVATIONS"]
      if(gdm_diag_cat_so[,.N]>0){
      names_df<-names(df)
      same_names<-intersect(names_df,names(gdm_diag_cat_so))
      df<-merge.data.table(df,diag_cat_gdm,by=same_names,all.x=T)
      if(df[!is.na(event_abbreviation),.N]>0){
      for(cat_ind in 1:gdm_diag_cat_so[,.N]){
        #cols_to_keep<-c(gdm_diag_cat_so[cat_ind==index,date_column],gdm_diag_cat_so[cat_ind==index,keep])
        #rename columns
        setnames(df,gdm_diag_cat_so[cat_ind==index,date_column], "event_date")
        setnames(df,gdm_diag_cat_so[cat_ind==index,keep], "event_code")
        setnames(df, "so_meaning", "meaning")
        setnames(df, "event_abbreviation", "condition")
        
        #date variable
        df[,event_date:=as.IDate(event_date, "%Y%m%d")]
        if(!"year" %in% names(df)){df[,year:=year(event_date)]}
        
        cols_to_keep<-c("event_date","event_code", "person_id","meaning","condition","year")
        years_cat<-sort(df[!is.na(condition) & index==cat_ind][!duplicated(year),year])
        
        for(year_ind in 1:length(years_cat)){
          saveRDS(df[!is.na(condition)][year==years_cat[year_ind] & index==cat_ind, cols_to_keep, with=F], paste0(projectFolder,"/g_intermediate/tmp/", years_cat[year_ind],"_", "GDM_diagnoses_cat_so",cat_ind,".rds"))
        }# new 01.06.2022
      
      rm(years_cat,year_ind)
      
        #rename columns
        setnames(df, "event_date", gdm_diag_cat_so[cat_ind==index,date_column])
        setnames(df,"event_code", gdm_diag_cat_so[cat_ind==index,keep])
        setnames(df, "meaning", "so_meaning")
        setnames(df, "condition", "event_abbreviation")
        
      }
      df[,table:=NULL][,event_abbreviation:=NULL][,keep:=NULL][,index:=NULL]
      }
      }
      }
      
      #PE
      if(!is.null(diag_cat_pe)){
      pe_diag_cat_so<-pe_diag_cat[table=="SURVEY_OBSERVATIONS"]
      if(pe_diag_cat_so[,.N]>0){
      names_df<-names(df)
      same_names<-intersect(names_df,names(pe_diag_cat_so))
      df<-merge.data.table(df,diag_cat_pe,by=same_names,all.x=T)
      if(df[!is.na(event_abbreviation),.N]>0){
      for(cat_ind in 1:pe_diag_cat_so[,.N]){
        cols_to_keep<-c(pe_diag_cat_so[cat_ind==index,date_column],pe_diag_cat_so[cat_ind==index,keep])
        #rename columns
        setnames(df,pe_diag_cat_so[cat_ind==index,date_column], "event_date")
        setnames(df,pe_diag_cat_so[cat_ind==index,keep], "event_code")
        setnames(df, "so_meaning", "meaning")
        setnames(df, "event_abbreviation", "condition")
        
        #date variable
        df[,event_date:=as.IDate(event_date, "%Y%m%d")]
        if(!"year" %in% names(df)){df[,year:=year(event_date)]}
        
        cols_to_keep<-c("event_date","event_code", "person_id","meaning","condition","year")
        
        years_cat<-sort(df[!is.na(condition) & index==cat_ind][!duplicated(year),year])
        for(year_ind in 1:length(years_cat)){
          saveRDS(df[!is.na(condition)][year==years_cat[year_ind] & index==cat_ind, cols_to_keep, with=F], paste0(projectFolder,"/g_intermediate/tmp/", years_cat[year_ind],"_", "PE_diagnoses_cat_so",cat_ind,".rds"))
        }# new 01.06.2022
        
        rm(years_cat,year_ind)
        
        #rename columns
        setnames(df, "event_date", pe_diag_cat_so[cat_ind==index,date_column])
        setnames(df,"event_code", pe_diag_cat_so[cat_ind==index,keep])
        setnames(df, "meaning", "so_meaning")
        setnames(df, "condition", "event_abbreviation")
        
      }
      df[,table:=NULL][,event_abbreviation:=NULL][,keep:=NULL][,index:=NULL]
      }
      }
      }
        
      #Migraine
      if(!is.null(diag_cat_migraine)){
      migraine_diag_cat_so<-migraine_diag_cat[table=="SURVEY_OBSERVATIONS"]
      if(migraine_diag_cat_so[,.N]>0){
      names_df<-names(df)
      same_names<-intersect(names_df,names(migraine_diag_cat_so))
      df<-merge.data.table(df,diag_cat_migraine,by=same_names,all.x=T)
      if(df[!is.na(event_abbreviation),.N]>0){
      for(cat_ind in 1:migraine_diag_cat_so[,.N]){
        cols_to_keep<-c(migraine_diag_cat_so[cat_ind==index,date_column],migraine_diag_cat_so[cat_ind==index,keep])
        #rename columns
        setnames(df,migraine_diag_cat_so[cat_ind==index,date_column], "event_date")
        setnames(df,migraine_diag_cat_so[cat_ind==index,keep], "event_code")
        setnames(df, "so_meaning", "meaning")
        setnames(df, "event_abbreviation", "condition")
        
        #date variable
        df[,event_date:=as.IDate(event_date, "%Y%m%d")]
        if(!"year" %in% names(df)){df[,year:=year(event_date)]}
        
        cols_to_keep<-c("event_date","event_code", "person_id","meaning","condition","year")
        years_cat<-sort(df[!is.na(condition) & index==cat_ind][!duplicated(year),year])
        for(year_ind in 1:length(years_cat)){
          saveRDS(df[!is.na(condition)][year==years_cat[year_ind] & index==cat_ind, cols_to_keep, with=F], paste0(projectFolder,"/g_intermediate/tmp/", years_cat[year_ind],"_", "Migraine_diagnoses_cat_so",cat_ind,".rds"))
        }# new 01.06.2022
        
        rm(years_cat,year_ind)
        
        #rename columns
        setnames(df, "event_date", migraine_diag_cat_so[cat_ind==index,date_column])
        setnames(df,"event_code", migraine_diag_cat_so[cat_ind==index,keep])
        setnames(df, "meaning", "so_meaning")
        setnames(df, "condition", "event_abbreviation")
        
      }
      df[,table:=NULL][,event_abbreviation:=NULL][,keep:=NULL][,index:=NULL]
      }
      }
      }
        
      #Check box info
        #GDM
        if(!is.null(diag_checkbox_gdm)){
          gdm_diag_checkbox_so<-diag_checkbox_gdm[table=="SURVEY_OBSERVATIONS"]
          if(gdm_diag_checkbox_so[,.N]>0){
            names_df<-names(df)
            same_names<-intersect(names_df,names(gdm_diag_checkbox_so))
            df<-merge.data.table(df,gdm_diag_checkbox_so,by=same_names,all.x=T)
            if(df[!is.na(event_abbreviation),.N]>0){
              for(cat_ind in 1:gdm_diag_checkbox_so[,.N]){
                cols_to_keep<-c(gdm_diag_checkbox_so[cat_ind==index,checkbox_date],gdm_diag_checkbox_so[cat_ind==index,keep])
                #rename columns
                setnames(df,gdm_diag_checkbox_so[cat_ind==index,checkbox_date], "event_date")
                setnames(df,gdm_diag_checkbox_so[cat_ind==index,keep], "event_code")
                setnames(df, "so_meaning", "meaning")
                setnames(df, "event_abbreviation", "condition")
                
                #date variable
                df[,event_date:=as.IDate(event_date, "%Y%m%d")]
                if(!"year" %in% names(df)){df[,year:=year(event_date)]}
                
                cols_to_keep<-c("event_date","event_code", "person_id","meaning","condition","year")
                years_cat<-sort(df[!is.na(condition) & index==cat_ind][!duplicated(year),year])
                for(year_ind in 1:length(years_cat)){
                  saveRDS(df[!is.na(condition)][year==years_cat[year_ind] & index==cat_ind, cols_to_keep, with=F], paste0(projectFolder,"/g_intermediate/tmp/", years_cat[year_ind],"_", "GDM_diagnoses_checkbox_so",cat_ind,".rds"))
                }# new 01.06.2022
                
                rm(years_cat,year_ind)
                
                #rename columns
                setnames(df, "event_date", gdm_diag_checkbox_so[cat_ind==index,checkbox_date])
                setnames(df,"event_code", gdm_diag_checkbox_so[cat_ind==index,keep])
                setnames(df, "meaning", "so_meaning")
                setnames(df, "condition", "event_abbreviation")
                
              }
              df[,table:=NULL][,event_abbreviation:=NULL][,keep:=NULL][,index:=NULL]
            }
          }
        }
        
        #PE
        if(!is.null(diag_checkbox_pe_so)){
          pe_diag_checkbox_so<-diag_checkbox_pe_so[table=="SURVEY_OBSERVATIONS"]
          if(pe_diag_checkbox_so[,.N]>0){
            names_df<-names(df)
            same_names<-intersect(names_df,names(pe_diag_checkbox_so))
            df<-merge.data.table(df,pe_diag_checkbox_so,by=same_names,all.x=T)
            if(df[!is.na(event_abbreviation),.N]>0){
              for(cat_ind in 1:pe_diag_checkbox_so[,.N]){
                cols_to_keep<-c(pe_diag_checkbox_so[cat_ind==index,checkbox_date],pe_diag_checkbox_so[cat_ind==index,keep])
                #rename columns
                setnames(df,pe_diag_checkbox_so[cat_ind==index,checkbox_date], "event_date")
                setnames(df,pe_diag_checkbox_so[cat_ind==index,keep], "event_code")
                setnames(df, "so_meaning", "meaning")
                setnames(df, "event_abbreviation", "condition")
                
                #date variable
                df[,event_date:=as.IDate(event_date, "%Y%m%d")]
                if(!"year" %in% names(df)){df[,year:=year(event_date)]}
                
                cols_to_keep<-c("event_date","event_code", "person_id","meaning","condition","year")
                years_cat<-sort(df[!is.na(condition) & index==cat_ind][!duplicated(year),year])
                for(year_ind in 1:length(years_cat)){
                  saveRDS(df[!is.na(condition)][year==years_cat[year_ind] & index==cat_ind, cols_to_keep, with=F], paste0(projectFolder,"/g_intermediate/tmp/", years_cat[year_ind],"_", "PE_diagnoses_checkbox_so",cat_ind,".rds"))
                }# new 01.06.2022
                
                rm(years_cat,year_ind)
                
                #rename columns
                setnames(df, "event_date", pe_diag_checkbox_so[cat_ind==index,checkbox_date])
                setnames(df,"event_code", pe_diag_checkbox_so[cat_ind==index,keep])
                setnames(df, "meaning", "so_meaning")
                setnames(df, "condition", "event_abbreviation")
                
              }
              df[,table:=NULL][,event_abbreviation:=NULL][,keep:=NULL][,index:=NULL]
            }
          }
        }
      }
      w<-w+1
      rm(df)
    }
    ####Combine results####
    #combine flowchart results
    original_rows<-sum(do.call(rbind,original_rows))
    empty_event_date<-sum(do.call(rbind,empty_event_date))
    empty_event_code<-sum(do.call(rbind,empty_event_code))
    empty_event_vocabulary<-sum(do.call(rbind,empty_event_vocabulary))
    empty_event_meaning<-sum(do.call(rbind,empty_event_meaning))
    prior_diagnoses_rec<-sum(do.call(rbind,prior_diagnoses_rec))
    after_diagnoses_rec<-sum(do.call(rbind,after_diagnoses_rec))
    included_records_filtering<-sum(do.call(rbind,included_records_filtering))
    
    #create flowchart and export to g_output
    flowchart_so<-data.table(Indicator=c("Number of original rows",
                                         "Number of records with missing event date",
                                         "Number of records with missing event code",
                                         "Number of records with missing event vocabulary",
                                         "Number of records with missing event meaning",
                                         "Number of records with event date before start of study date",
                                         "Number of records with event date after end of study date",
                                         "Number of records included, before data filtering"),
                             SURVEY_OBSERVATIONS=c(original_rows,
                                                   empty_event_date,
                                                   empty_event_code,
                                                   empty_event_vocabulary,
                                                   empty_event_meaning,
                                                   prior_diagnoses_rec,
                                                   after_diagnoses_rec,
                                                   included_records_filtering))
    #fwrite(flowchart_events, paste0(output_dir, "PE and GDM algorithm/flowchart_events.csv"), row.names = F)
    rm(original_rows,empty_event_date,empty_event_code,empty_event_vocabulary,empty_event_meaning,prior_diagnoses_rec,after_diagnoses_rec,included_records_filtering)
    
    }else{
    flowchart_so<-data.table(Indicator=c("Number of original rows",
                                         "Number of records with missing event date",
                                         "Number of records with missing event code",
                                         "Number of records with missing event vocabulary",
                                         "Number of records with missing event meaning",
                                         "Number of records with event date before start of study date",
                                         "Number of records with event date after end of study date",
                                         "Number of records included, before data filtering"),
                             SURVEY_OBSERVATIONS=c(0,
                                                   0,
                                                   0,
                                                   0,
                                                   0,
                                                   0,
                                                   0,
                                                   0))
  }
  }else{
  flowchart_so<-data.table(Indicator=c("Number of original rows",
                                       "Number of records with missing event date",
                                       "Number of records with missing event code",
                                       "Number of records with missing event vocabulary",
                                       "Number of records with missing event meaning",
                                       "Number of records with event date before start of study date",
                                       "Number of records with event date after end of study date",
                                       "Number of records included, before data filtering"),
                           SURVEY_OBSERVATIONS=c(0,
                                                 0,
                                                 0,
                                                 0,
                                                 0,
                                                 0,
                                                 0,
                                                 0))
  
}

####Create flowchart and export####
flowchart<-merge.data.table(flowchart_events,flowchart_mo, by="Indicator")
rm(flowchart_events,flowchart_mo)
flowchart<-merge.data.table(flowchart,flowchart_so, by="Indicator")
rm(flowchart_so)
fwrite(flowchart, paste0(projectFolder, "/g_output/PE and GDM algorithm/flowchart_diagnostic_tables.csv"))
fwrite(flowchart, paste0(projectFolder, "/g_output/Migraine algorithm/flowchart_diagnostic_tables.csv"))

####Combine results by year and event####
###GDM files####
#GDM checkbox files
gdm_checkbox_files<-list.files(tmp, "GDM_diagnoses_checkbox")
if(length(gdm_checkbox_files)>0){
  #Combine files by type of event
  files<-list()
  for (i in 1: length(gdm_checkbox_files)){
    files<-append(files,substr(gdm_checkbox_files[i],1,4))
  }
  files<-do.call(c,files)
  #remove duplicates 
  files<-files[!duplicated(files)]
  #create list with names year_condition
  gdm_list<-vector(mode="list", length=length(files))
  names(gdm_list)<-files
  rm(files)
  #separate all files into the right category
  for (i in 1:length(gdm_list)){
    gdm_list[[i]]<-gdm_checkbox_files[startsWith(gdm_checkbox_files,names(gdm_list)[i])]
  }
  rm(gdm_checkbox_files)
  gdm_checkbox_files<-gdm_list
  rm(gdm_list)
  
  #Load all files, combine by year, clean up by removing duplicated diagnoses
  duplicates_gdm_checkbox<-list()
  index_gdm<-1
  for (gdm_fl in 1:length(gdm_checkbox_files)){
    event_df<-lapply(paste0(tmp, gdm_checkbox_files[[gdm_fl]]), readRDS)
    event_df<-rbindlist(event_df,fill = T)
    #create variable person_id_^_event date
    event_df[,comb:=paste0(person_id, "_", event_date, "_", condition)]
    dup<-event_df[duplicated(comb),.N]
    duplicates_gdm_checkbox[[index_gdm]]<-dup
    rm(dup)
    event_df<-event_df[!duplicated(comb)]
    event_df[,comb:=NULL]
    #write the new files to g_intermediate
    saveRDS(event_df,paste0(projectFolder, "/g_intermediate/gdm_algorithm/", names(gdm_checkbox_files)[gdm_fl], "_GDM_checkbox.rds"))
    rm(event_df)
    index_gdm<-index_gdm+1
  }
  rm(index_gdm)
  
  #remove all gdm files from tmp
  for (i in 1:length(gdm_checkbox_files)){
    file.remove(paste0(tmp,gdm_checkbox_files[[i]]))
  }
  
  #Export duplicates
  duplicates_gdm_checkbox<-sum(do.call(rbind,duplicates_gdm_checkbox))
  dup<-data.table(Indicator="GDM Algorithm, Duplicated diagnostic checkbox records removed:", no_records=duplicates_gdm_checkbox)
  fwrite(dup, paste0(projectFolder, "/g_output/PE and GDM algorithm/removed_diagostic_checkbox_gdm.csv"), row.names = F)
  rm(dup)
} else {rm(gdm_checkbox_files)}

#Diagnostic files
gdm_events<-paste0("_GDM_", names(conditions_gdm))
gdm_events_sentence<-paste(gdm_events, collapse = "|")
gdm_events_cat<-list.files(tmp, "GDM_diagnoses_cat")
gdm_files<-list.files(tmp, gdm_events_sentence)
gdm_files<-c(gdm_files,gdm_events_cat)
if(length(gdm_files)>0){
#Combine files by type of event
files<-list()
for (i in 1: length(gdm_files)){
  files<-append(files,substr(gdm_files[i],1,4))
}
files<-do.call(c,files)
#remove duplicates 
files<-files[!duplicated(files)]
#create list with names year_condition
gdm_list<-vector(mode="list", length=length(files))
names(gdm_list)<-files
rm(files)
#separate all files into the right category
for (i in 1:length(gdm_list)){
  gdm_list[[i]]<-gdm_files[startsWith(gdm_files,names(gdm_list)[i])]
}
rm(gdm_files)
gdm_files<-gdm_list
rm(gdm_list)

#Load all files, combine by year, clean up by removing duplicated diagnoses
duplicates_gdm<-list()
index_gdm<-1
for (gdm_fl in 1:length(gdm_files)){
  event_df<-lapply(paste0(tmp, gdm_files[[gdm_fl]]), readRDS)
  event_df<-rbindlist(event_df,fill = T)
  #create variable person_id_^_event date
  event_df[,comb:=paste0(person_id, "_", event_date, "_", condition)]
  dup<-event_df[duplicated(comb),.N]
  duplicates_gdm[[index_gdm]]<-dup
  rm(dup)
  event_df<-event_df[!duplicated(comb)]
  event_df[,comb:=NULL]
  #write the new files to g_intermediate
  saveRDS(event_df,paste0(projectFolder, "/g_intermediate/gdm_algorithm/", names(gdm_files)[gdm_fl], "_GDM_diagnoses.rds"))
rm(event_df)
index_gdm<-index_gdm+1
}

#remove all gdm files from tmp
for (i in 1:length(gdm_files)){
  file.remove(paste0(tmp,gdm_files[[i]]))
}

#Export duplicates
duplicates_gdm<-sum(do.call(rbind,duplicates_gdm))
dup<-data.table(Indicator="GDM Algorithm, Duplicated diagnostic records removed:", no_records=duplicates_gdm)
fwrite(dup, paste0(projectFolder, "/g_output/PE and GDM algorithm/removed_diagostic_duplicates_gdm.csv"), row.names = F)
rm(dup)
} else {rm(gdm_files)}


###PE files####
#PE checkbox files
pe_checkbox_files<-list.files(tmp, "PE_diagnoses_checkbox")
if(length(pe_checkbox_files)>0){
  #Combine files by type of event
  files<-list()
  for (i in 1: length(pe_checkbox_files)){
    files<-append(files,substr(pe_checkbox_files[i],1,4))
  }
  files<-do.call(c,files)
  #remove duplicates 
  files<-files[!duplicated(files)]
  #create list with names year_condition
  pe_list<-vector(mode="list", length=length(files))
  names(pe_list)<-files
  rm(files)
  #separate all files into the right category
  for (i in 1:length(pe_list)){
    pe_list[[i]]<-pe_checkbox_files[startsWith(pe_checkbox_files,names(pe_list)[i])]
  }
  rm(pe_checkbox_files)
  pe_checkbox_files<-pe_list
  rm(pe_list)
  
  #Load all files, combine by year, clean up by removing duplicated diagnoses
  duplicates_pe_checkbox<-list()
  index_pe<-1
  for (pe_fl in 1:length(pe_checkbox_files)){
    event_df<-lapply(paste0(tmp, pe_checkbox_files[[pe_fl]]), readRDS)
    event_df<-rbindlist(event_df,fill = T)
    #create variable person_id_^_event date
    event_df[,comb:=paste0(person_id, "_", event_date, "_", condition)]
    dup<-event_df[duplicated(comb),.N]
    duplicates_pe_checkbox[[index_pe]]<-dup
    rm(dup)
    event_df<-event_df[!duplicated(comb)]
    event_df[,comb:=NULL]
    #write the new files to g_intermediate
    saveRDS(event_df,paste0(projectFolder, "/g_intermediate/pe_algorithm/", names(pe_checkbox_files)[pe_fl], "_PE_checkbox.rds"))
    rm(event_df)
    index_pe<-index_pe+1
  }
  rm(index_pe)
  
  #remove all gdm files from tmp
  for (i in 1:length(pe_checkbox_files)){
    file.remove(paste0(tmp,pe_checkbox_files[[i]]))
  }
  
  #Export duplicates
  duplicates_pe_checkbox<-sum(do.call(rbind,duplicates_pe_checkbox))
  dup<-data.table(Indicator="PE Algorithm, Duplicated diagnostic checkbox records removed:", no_records=duplicates_pe_checkbox)
  fwrite(dup, paste0(projectFolder, "/g_output/PE and PE algorithm/removed_diagostic_checkbox_pe.csv"), row.names = F)
  rm(dup)
} else {rm(pe_checkbox_files)}

#Diagnostic files
pe_events<-paste0("_PE_", names(conditions_pe))
pe_events_sentence<-paste(pe_events, collapse = "|")
pe_events_cat<-list.files(tmp, "PE_diagnoses_cat")
pe_files<-list.files(tmp, pe_events_sentence)
pe_files<-c(pe_files,pe_events_cat)

if(length(pe_files)>0){
  #Combine files by type of event
  files<-list()
  for (i in 1: length(pe_files)){
    files<-append(files,substr(pe_files[i],1,4))
  }
  files<-do.call(c,files)
  #remove duplicates 
  files<-files[!duplicated(files)]
  #create list with names year_condition
  pe_list<-vector(mode="list", length=length(files))
  names(pe_list)<-files
  rm(files)
  #separate all files into the right category
  for (i in 1:length(pe_list)){
    pe_list[[i]]<-pe_files[startsWith(pe_files,names(pe_list)[i])]
  }
  rm(pe_files)
  pe_files<-pe_list
  rm(pe_list)
  
  #Load all files, combine by year, clean up by removing duplicated diagnoses
  duplicates_pe<-list()
  index_pe<-1
  for (pe_fl in 1:length(pe_files)){
    event_df<-lapply(paste0(tmp, pe_files[[pe_fl]]), readRDS)
    event_df<-rbindlist(event_df,fill=T)
    #create variable person_id_^_event date
    event_df[,comb:=paste0(person_id, "_", event_date, "_", condition)]
    dup<-event_df[duplicated(comb),.N]
    duplicates_pe[[index_pe]]<-dup
    rm(dup)
    event_df<-event_df[!duplicated(comb)]
    event_df[,comb:=NULL]
    #write the new files to g_intermediate
    saveRDS(event_df,paste0(projectFolder, "/g_intermediate/pe_algorithm/", names(pe_files)[pe_fl], "_PE_diagnoses.rds"))
    rm(event_df)
    index_pe<-index_pe+1
  }
  
  #remove all pe files from tmp
  for (i in 1:length(pe_files)){
    file.remove(paste0(tmp,pe_files[[i]]))
  }
  
  #Export duplicates
  duplicates_pe<-sum(do.call(rbind,duplicates_pe))
  dup<-data.table(Indicator="PE Algorithm, Duplicated diagnostic records removed:", no_records=duplicates_pe)
  fwrite(dup, paste0(projectFolder, "/g_output/PE and GDM algorithm/removed_diagostic_duplicates_pe.csv"), row.names = F)
  rm(dup)
} else {rm(pe_files)}
###Migraine files####
migraine_events<-paste0("_Migraine_", names(conditions_migraine))
migraine_events_sentence<-paste(migraine_events, collapse = "|")
migraine_events_cat<-list.files(tmp, "Migraine_diagnoses_cat")
migraine_files<-list.files(tmp, migraine_events_sentence)
migraine_files<-c(migraine_files,migraine_events_cat)


if(length(migraine_files)>0){
  #Combine files by type of event
  files<-list()
  for (i in 1: length(migraine_files)){
    files<-append(files,substr(migraine_files[i],1,4))
  }
  files<-do.call(c,files)
  #remove duplicates 
  files<-files[!duplicated(files)]
  #create list with names year_condition
  migraine_list<-vector(mode="list", length=length(files))
  names(migraine_list)<-files
  rm(files)
  #separate all files into the right category
  for (i in 1:length(migraine_list)){
    migraine_list[[i]]<-migraine_files[startsWith(migraine_files,names(migraine_list)[i])]
  }
  rm(migraine_files)
  migraine_files<-migraine_list
  rm(migraine_list)
  
  #Load all files, combine by year, clean up by removing duplicated diagnoses
  duplicates_migraine<-list()
  index_migraine<-1
  for (migraine_fl in 1:length(migraine_files)){
    event_df<-lapply(paste0(tmp, migraine_files[[migraine_fl]]), readRDS)
    event_df<-rbindlist(event_df, fill = T)
    #create variable person_id_^_event date
    event_df[,comb:=paste0(person_id, "_", event_date, "_", condition)]
    dup<-event_df[duplicated(comb),.N]
    duplicates_migraine[[index_migraine]]<-dup
    rm(dup)
    event_df<-event_df[!duplicated(comb)]
    event_df[,comb:=NULL]
    #write the new files to g_intermediate
    saveRDS(event_df,paste0(projectFolder, "/g_intermediate/migraine_algorithm/", names(migraine_files)[migraine_fl], "_Migraine_diagnoses.rds"))
    rm(event_df)
    index_migraine<-index_migraine+1
  }
  
  #remove all pe files from tmp
  for (i in 1:length(migraine_files)){
    file.remove(paste0(tmp,migraine_files[[i]]))
  }
  
  #Export duplicates
  duplicates_migraine<-sum(do.call(rbind,duplicates_migraine))
  dup<-data.table(Indicator="Migraine Algorithm, Duplicated diagnostic records removed:", no_records=duplicates_migraine)
  fwrite(dup, paste0(projectFolder, "/g_output/Migraine algorithm/removed_diagostic_duplicates_migraine.csv"), row.names = F)
  rm(dup)
} else {rm(migraine_files)}




