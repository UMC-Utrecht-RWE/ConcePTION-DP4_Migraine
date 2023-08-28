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
  if("code" %in% codesheet_diagnoses_pe[table=="EVENTS",val_1]){
    code_var<-codesheet_diagnoses_pe[table=="EVENTS",col_1]
    voc_var<-codesheet_diagnoses_pe[table=="EVENTS",col_2]
    date_var<-codesheet_diagnoses_pe[table=="EVENTS",date_column]
  }else{
    code_var<-codesheet_diagnoses_pe[table=="EVENTS",col_2]
    voc_var<-codesheet_diagnoses_pe[table=="EVENTS",col_1]
    date_var<-codesheet_diagnoses_pe[table=="EVENTS",date_column]
  }
  if("code" %in% codesheet_diagnoses_migraine[table=="EVENTS",val_1]){
    code_var<-codesheet_diagnoses_migraine[table=="EVENTS",col_1]
    voc_var<-codesheet_diagnoses_migraine[table=="EVENTS",col_2]
    date_var<-codesheet_diagnoses_migraine[table=="EVENTS",date_column]
  }else{
    code_var<-codesheet_diagnoses_migraine[table=="EVENTS",col_2]
    voc_var<-codesheet_diagnoses_migraine[table=="EVENTS",col_1]
    date_var<-codesheet_diagnoses_migraine[table=="EVENTS",date_column]
  }
  if(length(actual_tables$EVENTS)>0){
    #Load the observation periods hint table(use to make smaller the table)
    hint_fl<-list.files(paste0(projectFolder, "/g_intermediate/pregnancy_d3/"), "obs_period_hint")
    #use this table first to remove all uneccessary subjects(not needed for any of the studies)
    obs_hint_table<-readRDS(paste0(projectFolder, "/g_intermediate/pregnancy_d3/", hint_fl))
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
    any_study_no<-list() #number of records for subjects not needed in any of the SAPs
    outside_obs<-list() #number of records outside obs min and obs max between SAPs for each subject
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
      #merge with the obs_hint table and remove subjects not present in the pregnancy d3 for any of teh studies
      df<-merge.data.table(df, obs_hint_table, by="person_id", all.x = T)
      any_study_no[[w]]<-df[is.na(obs_min), .N]
      df<-df[!is.na(obs_min)]
      if(df[,.N]>0){
        #Remove all records outside the window for observation for each subject
        df[event_date>=obs_min & event_date<=obs_max, keep:=1]
        outside_obs[[w]]<-df[is.na(keep),.N]
        df<-df[keep==1]
        if(df[,.N]>0){
          #remove all records with dates prior to start study date or start study date plus lookback if any
          #gdm&pe
          df[,prior_gdm_pe:=ifelse(gdm_pe_start_study_date>event_date,1,0)]
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
          df[,code_no_dot:=as.character(gsub("\\.","",df[,event_code]))]
          included_records_filtering_gdm_pe[[w]]<-df[prior_gdm_pe==0 & after_gdm_pe==0,.N]
          included_records_filtering_mig[[w]]<-df[prior_mig==0 & after_mig==0,.N]
          
          #maybe not needed
          # #Create combination inclusion records
          # df[prior_gdm_pe==0 | prior_du==0 | prior_saf==0, comb_prior_gdm_pe:=0]
          # df[after_gdm_pe==0 | after_du==0 | after_saf==0, comb_after_gdm_pe:=0]
          # 
          # df[prior_mig==0 | prior_du==0 | prior_saf==0, comb_prior_mig:=0]
          # df[after_mig==0 | after_du==0 | after_saf==0, comb_after_mig:=0]
          
          
          
          if(df[,.N]>0){
            
            if(df[prior_gdm_pe==0 & after_gdm_pe==0,.N]>0){
              if(events_gdm_diagnoses>0){
                print(paste0("Filtering data for GDM diagnoses:", actual_tables$EVENTS[y]))
                years_study_events<-sort(df[prior_gdm_pe==0 & after_gdm_pe==0][!duplicated(year), year])#years present in this table
                
                if(sum(df[prior_gdm_pe==0 & after_gdm_pe==0][!duplicated(event_vocabulary), event_vocabulary] %in% vocabularies_list_gdm)>0){
                  for (i in 1:length(conditions_gdm)){
                    for(j in 1:length(conditions_gdm[[i]])){
                      
                      codes<-data.table(event_vocabulary=names(conditions_gdm[[i]])[j], truncated_code=conditions_gdm[[i]][[j]], filter=1)
                      for(codes_ind in 1:codes[,.N]){
                        length<-nchar(codes[codes_ind,truncated_code])
                        #create truncated code
                        df[prior_gdm_pe==0 & after_gdm_pe==0 & event_vocabulary==names(conditions_gdm[[i]])[j], truncated_code:=substr(code_no_dot,1,length)]
                        
                        df<-merge.data.table(df,codes[codes_ind,],by=c("event_vocabulary","truncated_code"),all.x = T,allow.cartesian = T)
                        
                        if(df[prior_gdm_pe==0 & after_gdm_pe==0 & filter==1,.N]>0){
                          years_this_event<-sort(df[prior_gdm_pe==0 & after_gdm_pe==0 & filter==1][!duplicated(year),year])
                          for(year_ind in 1:length(years_this_event)){
                            saveRDS(data.table(df[prior_gdm_pe==0 & after_gdm_pe==0 & filter==1 & year==years_this_event[year_ind]], condition=names(conditions_gdm[i])), paste0(tmp,years_this_event[year_ind],"_","GDM_", names(conditions_gdm[i]), "_",actual_tables$EVENTS[y],"_", codes_ind, ".rds"))
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
                years_study_events<-sort(df[prior_gdm_pe==0 & after_gdm_pe==0][!duplicated(year), year])#years present in this table
                if(sum(df[prior_gdm_pe==0 & after_gdm_pe==0][!duplicated(event_vocabulary), event_vocabulary] %in% vocabularies_list_pe)>0){
                  for (i in 1:length(conditions_pe)){
                    for(j in 1:length(conditions_pe[[i]])){
                      
                      codes<-data.table(event_vocabulary=names(conditions_pe[[i]])[j], truncated_code=conditions_pe[[i]][[j]], filter=1)
                      for(codes_ind in 1:codes[,.N]){
                        length<-nchar(codes[codes_ind,truncated_code])
                        #create truncated code
                        df[prior_gdm_pe==0 & after_gdm_pe==0 & event_vocabulary==names(conditions_pe[[i]])[j],truncated_code:=substr(code_no_dot,1,length)]
                        
                        df<-merge.data.table(df,codes[codes_ind,],by=c("event_vocabulary","truncated_code"),all.x = T,allow.cartesian = T)
                        
                        if(df[prior_gdm_pe==0 & after_gdm_pe==0 & filter==1,.N]>0){
                          years_this_event<-sort(df[prior_gdm_pe==0 & after_gdm_pe==0 & filter==1][!duplicated(year),year])
                          for(year_ind in 1:length(years_this_event)){
                            saveRDS(data.table(df[prior_gdm_pe==0 & after_gdm_pe==0 & filter==1 & year==years_this_event[year_ind]], condition=names(conditions_pe[i])), paste0(tmp,years_this_event[year_ind],"_","PE_", names(conditions_pe[i]), "_",actual_tables$EVENTS[y],"_", codes_ind, ".rds"))
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
                vocabularies_to_start_with<-c("ICD", "ICPC", "RCD")
                print(paste0("Filtering data for Migraine diagnoses:", actual_tables$EVENTS[y]))
                years_study_events<-sort(df[prior_mig==0 & after_mig==0][!duplicated(year), year])#years present in this table
                
                #startWith for the event Migraine
                if(sum(sapply(vocabularies_to_start_with, function(start) any(startsWith(df[prior_mig==0 & after_mig==0][!duplicated(event_vocabulary), event_vocabulary], start))))>=1){
                  if(sum(df[prior_mig==0 & after_mig==0][!duplicated(event_vocabulary), event_vocabulary] %in% vocabularies_list_migraine)>0){
                    for (i in 1:length(conditions_migraine)){
                      for(j in 1:length(conditions_migraine[[i]])){
                        
                        codes<-data.table(event_vocabulary=names(conditions_migraine[[i]])[j], truncated_code=conditions_migraine[[i]][[j]], filter=1)
                        for(codes_ind in 1:codes[,.N]){
                          length<-nchar(codes[codes_ind,truncated_code])
                          #create truncated code
                          df[prior_mig==0 & after_mig==0 & event_vocabulary==names(conditions_migraine[[i]])[j],truncated_code:=substr(code_no_dot,1,length)]
                          
                          df<-merge.data.table(df,codes[codes_ind,],by=c("event_vocabulary","truncated_code"),all.x = T,allow.cartesian = T)
                          
                          if(df[prior_mig==0 & after_mig==0 & filter==1,.N]>0){
                            years_this_event<-sort(df[prior_mig==0 & after_mig==0 & filter==1][!duplicated(year),year])
                            for(year_ind in 1:length(years_this_event)){
                              saveRDS(data.table(df[prior_mig==0 & after_mig==0 & filter==1 & year==years_this_event[year_ind]], condition=names(conditions_migraine[i])), paste0(tmp,years_this_event[year_ind],"_","Migraine_", names(conditions_migraine[i]), "_",actual_tables$EVENTS[y],"_", codes_ind, ".rds"))
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
                
                if(sum(sapply(vocabularies_to_start_with, function(start) any(startsWith(df[prior_mig==0 & after_mig==0][!duplicated(event_vocabulary), event_vocabulary], start))))==0){
                  #exact match for the other events related to migraine
                  if(sum(df[prior_mig==0 & after_mig==0][!duplicated(event_vocabulary), event_vocabulary] %in% vocabularies_list_migraine)>0){
                    for (i in 1:length(conditions_migraine)){
                      for(j in 1:length(conditions_migraine[[i]])){
                        
                        codes<-data.table(event_vocabulary=names(conditions_migraine[[i]])[j], code_no_dot=conditions_migraine[[i]][[j]], filter=1)
                        for(codes_ind in 1:codes[,.N]){
                          #create truncated code
                          df<-merge.data.table(df,codes[codes_ind,],by=c("event_vocabulary","code_no_dot"),all.x = T,allow.cartesian = T)
                          
                          if(df[prior_mig==0 & after_mig==0 & filter==1,.N]>0){
                            years_this_event<-sort(df[prior_mig==0 & after_mig==0 & filter==1][!duplicated(year),year])
                            for(year_ind in 1:length(years_this_event)){
                              saveRDS(data.table(df[prior_mig==0 & after_mig==0 & filter==1 & year==years_this_event[year_ind]], condition=names(conditions_migraine[i])), paste0(tmp,years_this_event[year_ind],"_","Migraine_", names(conditions_migraine[i]), "_",actual_tables$EVENTS[y],"_", codes_ind, ".rds"))
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
          }
        }else{
          prior_diagnoses_rec_gdm_pe[[w]]<-0
          prior_diagnoses_rec_mig[[w]]<-0
          after_diagnoses_rec_gdm_pe[[w]]<-0
          after_diagnoses_rec_mig[[w]]<-0
          included_records_filtering_gdm_pe[[w]]<-0
          included_records_filtering_mig[[w]]<-0
        }
      }else{
        outside_obs[[w]]<-0  
        prior_diagnoses_rec_gdm_pe[[w]]<-0
        prior_diagnoses_rec_mig[[w]]<-0
        after_diagnoses_rec_gdm_pe[[w]]<-0
        after_diagnoses_rec_mig[[w]]<-0
        included_records_filtering_gdm_pe[[w]]<-0
        included_records_filtering_mig[[w]]<-0
        
      }
      w<-w+1
      rm(df)
    }
    rm(hint_fl,obs_hint_table)
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
    any_study_no<-sum(do.call(rbind,any_study_no))
    outside_obs<-sum(do.call(rbind,outside_obs))
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
                                                    "Number of records not present in the pregnancy D3 for any of the studies",
                                                    "Number of records outside min and max observation period for any of the studies",
                                                    "Number of records with event date before start of study date",
                                                    "Number of records with event date after end of study date",
                                                    "Number of records included, before data filtering"),
                                        EVENTS=c(original_rows_gdm_pe,
                                                 empty_event_date_gdm_pe,
                                                 empty_event_code_gdm_pe,
                                                 empty_event_vocabulary_gdm_pe,
                                                 empty_event_meaning_gdm_pe,
                                                 any_study_no,
                                                 outside_obs,
                                                 prior_diagnoses_rec_gdm_pe,
                                                 after_diagnoses_rec_gdm_pe,
                                                 included_records_filtering_gdm_pe))
    rm(original_rows_gdm_pe,empty_event_date_gdm_pe,empty_event_code_gdm_pe,empty_event_vocabulary_gdm_pe,empty_event_meaning_gdm_pe,prior_diagnoses_rec_gdm_pe,after_diagnoses_rec_gdm_pe,included_records_filtering_gdm_pe)
    
    flowchart_events_mig<-data.table(Indicator=c("Number of original rows",
                                                 "Number of records with missing event date",
                                                 "Number of records with missing event code",
                                                 "Number of records with missing event vocabulary",
                                                 "Number of records with missing event meaning",
                                                 "Number of records not present in the pregnancy D3 for any of the studies",
                                                 "Number of records outside min and max observation period for any of the studies",
                                                 "Number of records with event date before start of study date",
                                                 "Number of records with event date after end of study date",
                                                 "Number of records included, before data filtering"),
                                     EVENTS=c(original_rows_mig,
                                              empty_event_date_mig,
                                              empty_event_code_mig,
                                              empty_event_vocabulary_mig,
                                              empty_event_meaning_mig,
                                              any_study_no,
                                              outside_obs,
                                              prior_diagnoses_rec_mig,
                                              after_diagnoses_rec_mig,
                                              included_records_filtering_mig))
    
    #fwrite(flowchart_events, paste0(output_dir, "PE and GDM algorithm/flowchart_events.csv"), row.names = F)
    rm(original_rows_mig,empty_event_date_mig,empty_event_code_mig,empty_event_vocabulary_mig,empty_event_meaning_mig,any_study_no,outside_obs,prior_diagnoses_rec_mig,after_diagnoses_rec_mig,included_records_filtering_mig)
    
  } else {
    flowchart_events_gdm_pe<-data.table(Indicator=c("Number of original rows",
                                                    "Number of records with missing event date",
                                                    "Number of records with missing event code",
                                                    "Number of records with missing event vocabulary",
                                                    "Number of records with missing event meaning",
                                                    "Number of records not present in the pregnancy D3 for any of the studies",
                                                    "Number of records outside min and max observation period for any of the studies",
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
                                                 0,
                                                 0,
                                                 0))
    flowchart_events_mig<-data.table(Indicator=c("Number of original rows",
                                                 "Number of records with missing event date",
                                                 "Number of records with missing event code",
                                                 "Number of records with missing event vocabulary",
                                                 "Number of records with missing event meaning",
                                                 "Number of records not present in the pregnancy D3 for any of the studies",
                                                 "Number of records outside min and max observation period for any of the studies",
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
                                                  "Number of records not present in the pregnancy D3 for any of the studies",
                                                  "Number of records outside min and max observation period for any of the studies",
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
                                               0,
                                               0,
                                               0))
  flowchart_events_mig<-data.table(Indicator=c("Number of original rows",
                                               "Number of records with missing event date",
                                               "Number of records with missing event code",
                                               "Number of records with missing event vocabulary",
                                               "Number of records with missing event meaning",
                                               "Number of records not present in the pregnancy D3 for any of the studies",
                                               "Number of records outside min and max observation period for any of the studies",
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
                                            0,
                                            0,
                                            0))
  
  
}