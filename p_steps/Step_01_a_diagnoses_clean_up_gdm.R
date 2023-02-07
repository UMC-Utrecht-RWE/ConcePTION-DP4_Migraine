#Load EVENTS table and apply filter to select PE diagoses/GDM diagnoses/Migraine diagnoses

#load info, parameters, conceptsets
source(paste0(pre_dir,"info.R"))
source(paste0(pre_dir,"DAP_info.R"))
source(paste0(pre_dir,"create_conceptsets.R"))
source(paste0(pre_dir,"parameters_metadata.R"))
source(paste0(pre_dir,"study_parameters.R"))


#check if we need to search the events table for GDM
if(tables_search[GDM_diagnosis=="yes" | PE_diagnosis=="yes",.N]>0){
  if(tables_search[Table=="EVENTS",.N]>0){
print("Analyse EVENTS table.")
if(length(actual_tables$EVENTS)>0){
  ####List for saving info####
  print("Creating lists to save the information.")
  original_rows<-list() #number of table original rows
  empty_event_date<-list() #number of records with missing event date
  empty_event_code<-list() #numer of records with missing event code
  empty_event_vocabulary<-list() #numer of records with missing event vocabulary
  empty_event_meaning<-list() #numer of records with missing event meaning
  prior_diagnoses_rec<-list() #number of records with date prior to start study date - 365 days
  after_diagnoses_rec<-list() #number of records with date after end study date
  w<-1
  #####Run the loop section####
  for (y in 1:length(actual_tables$EVENTS)){
    print(paste0("Analyzing table ",actual_tables$EVENTS[y], "."))
    #Load the table
    df<-fread(paste(path_dir, actual_tables$EVENTS[y], sep=""), stringsAsFactors = FALSE, colClasses = "character")
    df<-df[,c("person_id", "start_date_record", "event_code", "event_record_vocabulary", "meaning_of_event")]
    df<-df[, lapply(.SD, FUN=function(x) gsub("^$|^ $", NA, x))] #make sure missing data is read appropriately
    setnames(df,"meaning_of_event","meaning")
    setnames(df,"start_date_record","event_date")
    setnames(df,"event_record_vocabulary","event_vocabulary")
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
    
    if(df[,.N]>0){
      
      if(tables_search[GDM_diagnosis=="yes",.N]>0){
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
      
      if(tables_search[PE_diagnosis=="yes",.N]>0){
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
  
  #create flowchart and export to g_output
  flowchart_events<-data.table(Indicator=c("Number of original rows",
                                           "Number of records with missing event date",
                                           "Number of records with missing event code",
                                           "Number of records with missing event vocabulary",
                                           "Number of records with missing event meaning",
                                           "Number of records with event date before start of study date",
                                           "Number of records with event date after end of study date"),
                               EVENTS=c(original_rows,
                                       empty_event_date,
                                       empty_event_code,
                                       empty_event_vocabulary,
                                       empty_event_meaning,
                                       prior_diagnoses_rec,
                                       after_diagnoses_rec))
  fwrite(flowchart_events, paste0(output_dir, "PE and GDM algorithm/flowchart_events.csv"), row.names = F)
  
  
}
  
    #Combine all results from the events table and save in GDM/PE folders in g_intermediate
    #GDM
    gdm_events<-paste0("_GDM_", names(conditions_gdm))
    gdm_events_sentence<-paste(gdm_events, collapse = "|")
    gdm_files<-list.files(tmp, gdm_events_sentence)
    #Combine files by type of event
    files<-list()
    for (i in 1: length(gdm_files)){
      files<-append(files,unique(list(unlist(str_split(gdm_files[i],"_"))[3])))
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
      gdm_list[[i]]<-gdm_files[str_detect(gdm_files,paste0("_", names(gdm_list)[i], "_"))]
    }
    rm(gdm_files)
    gdm_files<-gdm_list
    rm(gdm_list)
    
  }
}

#Load MEDICAL_OBERVATIONS table and apply filter to select PE diagoses/GDM diagnoses/Migraine diagnoses
#First check where the diagnostic code and date variables are saved
