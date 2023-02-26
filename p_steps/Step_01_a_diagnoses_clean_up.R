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
mo_gdm_diagnoses<-ifelse("MEDICAL_OBSERVATIONS" %in% codesheet_diagnoses_gdm[,table],1,0)
so_gdm_diagnoses<-ifelse("SURVEY_OBSERVATIONS" %in% codesheet_diagnoses_gdm[,table],1,0)

events_pe_diagnoses<-ifelse("EVENTS" %in% codesheet_diagnoses_pe[,table],1,0)
mo_pe_diagnoses<-ifelse("MEDICAL_OBSERVATIONS" %in% codesheet_diagnoses_pe[,table],1,0)
so_pe_diagnoses<-ifelse("SURVEY_OBSERVATIONS" %in% codesheet_diagnoses_pe[,table],1,0)

####EVENTS####
#check if we need to search the events table for GDM
if(sum(events_gdm_diagnoses,events_pe_diagnoses)>0){
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
    included_records_filtering[[w]]<-df[,.N]
    
    if(df[,.N]>0){
      
      if(events_gdm_diagnoses>0){
        print(paste0("Filtering data for GDM diagnoses:", actual_tables$EVENTS[y]))
      years_study_events<-sort(df[!duplicated(year), year])#years present in this table

      if(sum(df[!duplicated(event_vocabulary), event_vocabulary] %in% vocabularies_list_gdm)>0){
        for (i in 1:length(conditions_gdm)){
          for(j in 1:length(conditions_gdm[[i]])){
            
            codes<-data.table(event_vocabulary=names(conditions_gdm[[i]])[j], truncated_code=conditions_gdm[[i]][[j]], filter=1)
            for(codes_ind in 1:codes[,.N]){
              length<-nchar(codes[codes_ind,truncated_code])
              #create truncated code
              df[,truncated_code:=substr(code_no_dot,1,length)]
            
            df<-merge.data.table(df,codes,by=c("event_vocabulary","truncated_code"),all.x = T,allow.cartesian = T)
            
            if(df[filter==1,.N]>0){
              years_this_event<-sort(df[filter==1][!duplicated(year),year])
              for(year_ind in 1:length(years_this_event)){
                saveRDS(data.table(df[filter==1 & year==years_this_event[year_ind]], condition=names(conditions_gdm[i])), paste0(tmp,years_this_event[year_ind],"_","GDM_", names(conditions_gdm[i]), "_",actual_tables$EVENTS[y],"_", codes_ind, ".rds"))
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
        years_study_events<-sort(df[!duplicated(year), year])#years present in this table
      if(sum(df[!duplicated(event_vocabulary), event_vocabulary] %in% vocabularies_list_pe)>0){
        for (i in 1:length(conditions_pe)){
          for(j in 1:length(conditions_pe[[i]])){
            
            codes<-data.table(event_vocabulary=names(conditions_pe[[i]])[j], truncated_code=conditions_pe[[i]][[j]], filter=1)
            for(codes_ind in 1:codes[,.N]){
              length<-nchar(codes[codes_ind,truncated_code])
              #create truncated code
              df[,truncated_code:=substr(code_no_dot,1,length)]
              
              df<-merge.data.table(df,codes,by=c("event_vocabulary","truncated_code"),all.x = T,allow.cartesian = T)
              
              if(df[filter==1,.N]>0){
                years_this_event<-sort(df[filter==1][!duplicated(year),year])
                for(year_ind in 1:length(years_this_event)){
                  saveRDS(data.table(df[filter==1 & year==years_this_event[year_ind]], condition=names(conditions_pe[i])), paste0(tmp,years_this_event[year_ind],"_","PE_", names(conditions_pe[i]), "_",actual_tables$EVENTS[y],"_", codes_ind, ".rds"))
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
    
    w<-w+1
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
  flowchart_events<-data.table(Indicator=c("Number of original rows",
                                           "Number of records with missing event date",
                                           "Number of records with missing event code",
                                           "Number of records with missing event vocabulary",
                                           "Number of records with missing event meaning",
                                           "Number of records with event date before start of study date",
                                           "Number of records with event date after end of study date",
                                           "Number of records included, before data filtering"),
                               EVENTS=c(original_rows,
                                       empty_event_date,
                                       empty_event_code,
                                       empty_event_vocabulary,
                                       empty_event_meaning,
                                       prior_diagnoses_rec,
                                       after_diagnoses_rec,
                                       included_records_filtering))
  #fwrite(flowchart_events, paste0(output_dir, "PE and GDM algorithm/flowchart_events.csv"), row.names = F)
  rm(original_rows,empty_event_date,empty_event_code,empty_event_vocabulary,empty_event_meaning,prior_diagnoses_rec,after_diagnoses_rec,included_records_filtering)
  
} else {
  flowchart_events<-data.table(Indicator=c("Number of original rows",
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
  }
####MEDICAL_OBSERVATIONS####
#Load MEDICAL_OBERVATIONS table and apply filter to select PE diagoses/GDM diagnoses/Migraine diagnoses
if(sum(mo_gdm_diagnoses,mo_pe_diagnoses)>0){
  if("code" %in% codesheet_diagnoses_gdm[table=="MEDICAL_OBSERVATIONS",val_1]){
    code_var<-codesheet_diagnoses_gdm[table=="MEDICAL_OBSERVATIONS",col_1]
    voc_var<-codesheet_diagnoses_gdm[table=="MEDICAL_OBSERVATIONS",col_2]
    date_var<-codesheet_diagnoses_gdm[table=="MEDICAL_OBSERVATIONS",date_column]
  }else{
    code_var<-codesheet_diagnoses_gdm[table=="MEDICAL_OBSERVATIONS",col_2]
    voc_var<-codesheet_diagnoses_gdm[table=="MEDICAL_OBSERVATIONS",col_1]
    date_var<-codesheet_diagnoses_gdm[table=="MEDICAL_OBSERVATIONS",date_column]
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
      cols<-c("person_id", "mo_meaning", code_var, voc_var, date_var)
      df<-df[,cols, with=F]
      df<-df[, lapply(.SD, FUN=function(x) gsub("^$|^ $", NA, x))] #make sure missing data is read appropriately
      setnames(df,"mo_meaning","meaning")
      setnames(df, date_var,"event_date")
      setnames(df, code_var,"event_code")
      setnames(df, voc_var,"event_vocabulary")
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
                  
                  df<-merge.data.table(df,codes,by=c("event_vocabulary","truncated_code"),all.x = T,allow.cartesian = T)
                  
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
                  
                  df<-merge.data.table(df,codes,by=c("event_vocabulary","truncated_code"),all.x = T,allow.cartesian = T)
                  
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
      }
      
      w<-w+1
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
    
  } else {
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
}
####SURVEY_OBSERVATIONS####
#Load SURVEY_OBERVATIONS table and apply filter to select PE diagoses/GDM diagnoses/Migraine diagnoses
if(sum(so_gdm_diagnoses,so_pe_diagnoses)>0){
  if("code" %in% codesheet_diagnoses_gdm[table=="SURVEY_OBSERVATIONS",val_1]){
    code_var<-codesheet_diagnoses_gdm[table=="SURVEY_OBSERVATIONS",col_1]
    voc_var<-codesheet_diagnoses_gdm[table=="SURVEY_OBSERVATIONS",col_2]
    date_var<-codesheet_diagnoses_gdm[table=="SURVEY_OBSERVATIONS",date_column]
  }else{
    code_var<-codesheet_diagnoses_gdm[table=="SURVEY_OBSERVATIONS",col_2]
    voc_var<-codesheet_diagnoses_gdm[table=="SURVEY_OBSERVATIONS",col_1]
    date_var<-codesheet_diagnoses_gdm[table=="SURVEY_OBSERVATIONS",date_column]
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
      cols<-c("person_id", "so_meaning", code_var, voc_var, date_var)
      df<-df[,cols, with=F]
      df<-df[, lapply(.SD, FUN=function(x) gsub("^$|^ $", NA, x))] #make sure missing data is read appropriately
      setnames(df,"so_meaning","meaning")
      setnames(df, date_var,"event_date")
      setnames(df, code_var,"event_code")
      setnames(df, voc_var,"event_vocabulary")
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
                  
                  df<-merge.data.table(df,codes,by=c("event_vocabulary","truncated_code"),all.x = T,allow.cartesian = T)
                  
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
                  
                  df<-merge.data.table(df,codes,by=c("event_vocabulary","truncated_code"),all.x = T,allow.cartesian = T)
                  
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
      }
      
      w<-w+1
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
    
  } else {
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
gdm_events<-paste0("_GDM_", names(conditions_gdm))
gdm_events_sentence<-paste(gdm_events, collapse = "|")
gdm_files<-list.files(tmp, gdm_events_sentence)
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
  event_df<-rbindlist(event_df)
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
pe_events<-paste0("_PE_", names(conditions_pe))
pe_events_sentence<-paste(pe_events, collapse = "|")
pe_files<-list.files(tmp, pe_events_sentence)
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
    event_df<-rbindlist(event_df)
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
migraine_files<-list.files(tmp, migraine_events_sentence)
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
    event_df<-rbindlist(event_df)
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




