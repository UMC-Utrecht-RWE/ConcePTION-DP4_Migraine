####SURVEY_OBSERVATIONS####
#Load SURVEY_OBSERVATIONS table and apply filter to select PE diagoses/GDM diagnoses/Migraine diagnoses
print("Check if the SURVEY OBSERVATIONS table is needed.")
if(sum(sum(so_gdm_diagnoses,so_pe_diagnoses,so_migraine_diagnoses,so_gdm_diagnoses_cat,so_pe_diagnoses_cat,so_migraine_diagnoses_cat)>0,
       sum(!is.null(diag_checkbox_gdm_so),!is.null(diag_checkbox_pe_so)))>0){
  if(sum(codesheet_diagnoses_gdm[table=="SURVEY_OBSERVATIONS",.N],
         codesheet_diagnoses_pe[table=="SURVEY_OBSERVATIONS",.N],
         codesheet_diagnoses_migraine[table=="SURVEY_OBSERVATIONS",.N])>0){
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
  }}else{
    code_var<-"so_source_value"
    voc_var<-"so_unit"
    date_var<-"so_date"
  }
  
  if(sum(codesheet_diagnoses_gdm_cat[table=="SURVEY_OBSERVATIONS",.N],
         codesheet_diagnoses_pe_cat[table=="SURVEY_OBSERVATIONS",.N],
         codesheet_diagnoses_migraine_cat[table=="SURVEY_OBSERVATIONS",.N])>0){
    code_var_2<-codesheet_diagnoses_gdm_cat[table=="SURVEY_OBSERVATIONS",col_1]
    voc_var_2<-codesheet_diagnoses_gdm_cat[table=="SURVEY_OBSERVATIONS",col_2]
    date_var_2<-codesheet_diagnoses_gdm_cat[table=="SURVEY_OBSERVATIONS",date_column]
  }else{
    code_var_2<-code_var
    voc_var_2<-voc_var
    date_var_2<-date_var
  }
  cols_needed<-unique(c(code_var,code_var_2, voc_var, voc_var_2, date_var, date_var_2))
  
  if(length(actual_tables$SURVEY_OBSERVATIONS)>0){
    #Load the observation periods hint table(use to make smaller the table)
    hint_fl<-list.files(paste0(projectFolder, "/g_intermediate/pregnancy_d3/"), "obs_period_hint")
    #use this table first to remove all uneccessary subjects(not needed for any of the studies)
    obs_hint_table<-readRDS(paste0(projectFolder, "/g_intermediate/pregnancy_d3/", hint_fl))
    
    print("Analyse SURVEY_OBSERVATIONS table.")
    ####List for saving info####
    print("Creating lists to save the information.")
    original_rows_gdm_pe<-list() #number of table original rows
    #original_rows_gdm_pe_cat<-list() #number of table original rows
    
    original_rows_mig<-list() #number of table original rows
    #original_rows_mig_cat<-list() #number of table original rows

    empty_event_date_gdm_pe<-list() #number of records with missing event date
    #empty_event_date_gdm_pe_cat<-list() #number of records with missing event date

    empty_event_date_mig<-list() #number of records with missing event date
    #empty_event_date_mig_cat<-list() #number of records with missing event date

    empty_event_code_gdm_pe<-list() #numer of records with missing event code
    #empty_event_code_gdm_pe_cat<-list() #numer of records with missing event code

    empty_event_code_mig<-list() #numer of records with missing event code
    #empty_event_code_mig_cat<-list() #numer of records with missing event code

    empty_event_vocabulary_gdm_pe<-list() #numer of records with missing event vocabulary
    #empty_event_vocabulary_gdm_pe_cat<-list() #numer of records with missing event vocabulary

    empty_event_vocabulary_mig<-list() #numer of records with missing event vocabulary
    #empty_event_vocabulary_mig_cat<-list() #numer of records with missing event vocabulary

    empty_event_meaning_gdm_pe<-list() #numer of records with missing event meaning
    #empty_event_meaning_gdm_pe_cat<-list() #numer of records with missing event meaning

    empty_event_meaning_mig<-list() #numer of records with missing event meaning
    #empty_event_meaning_mig_cat<-list() #numer of records with missing event meaning

    any_study_no<-list() #number of records for subjects not needed in any of the SAPs
    outside_obs<-list() #number of records outside obs min and obs max between SAPs for each subject
    
    prior_diagnoses_rec_gdm_pe<-list() #number of records with date prior to start study date
    #prior_diagnoses_rec_gdm_pe_cat<-list() #number of records with date prior to start study date

    prior_diagnoses_rec_mig<-list() #number of records with date prior to start study date
    #prior_diagnoses_rec_mig_cat<-list() #number of records with date prior to start study date
    
    after_diagnoses_rec_gdm_pe<-list() #number of records with date after end study date
    #after_diagnoses_rec_gdm_pe_cat<-list() #number of records with date after end study date

    after_diagnoses_rec_mig<-list() #number of records with date after end study date
    #after_diagnoses_rec_mig_cat<-list() #number of records with date after end study date
    
    included_records_filtering_gdm_pe<-list()
    #included_records_filtering_gdm_pe_cat<-list()

    included_records_filtering_mig<-list()
    #included_records_filtering_mig_cat<-list()
    
    w<-1
    #####Run the loop section####
    for (y in 1:length(actual_tables$SURVEY_OBSERVATIONS)){
      print(paste0("Analyzing table ",actual_tables$SURVEY_OBSERVATIONS[y], "."))
      #Load the table
      df<-fread(paste(path_dir, actual_tables$SURVEY_OBSERVATIONS[y], sep=""), stringsAsFactors = FALSE, colClasses = "character")
      df<-df[, lapply(.SD, FUN=function(x) gsub("^$|^ $", NA, x))] 
      df[,so_origin:=NULL][,survey_id:=NULL][,so_source_table:=NULL]
      #####Diagnoses####
      if(sum(codesheet_diagnoses_gdm[table=="SURVEY_OBSERVATIONS",.N],
             codesheet_diagnoses_pe[table=="SURVEY_OBSERVATIONS",.N],
             codesheet_diagnoses_migraine[table=="SURVEY_OBSERVATIONS",.N],
             !is.null(diag_checkbox_gdm_so),!is.null(diag_checkbox_pe_so),
             so_gdm_diagnoses_cat, so_pe_diagnoses_cat, so_migraine_diagnoses_cat)>0){
        #cols<-c("person_id", "so_meaning", code_var, voc_var, date_var)
        #df<-df[,cols, with=F]
        #remove unecessary columns
        remove_cols<-setdiff(names(df),cols_needed)
        remove_cols<-remove_cols[!remove_cols %in% c("person_id","so_meaning")]
        if(!is.null(remove_cols)){df[,eval(remove_cols):=NULL]}
        #make sure missing data is read appropriately
        setnames(df, date_var,"event_date")
        setnames(df, code_var,"event_code")
        setnames(df, voc_var,"event_vocabulary")
        setnames(df,"so_meaning","meaning")
        original_rows_gdm_pe[[w]]<-df[,.N]
        original_rows_mig[[w]]<-df[,.N]
        #remove empty dates
        empty_event_date_gdm_pe[[w]]<-df[is.na(event_date),.N]
        empty_event_date_mig[[w]]<-df[is.na(event_date),.N]
        df[is.na(event_date),remove:=1]
        #remove empty codes
        empty_event_code_gdm_pe[[w]]<-df[is.na(event_code) & is.na(remove),.N]
        empty_event_code_mig[[w]]<-df[is.na(event_code)& is.na(remove),.N]
        df[is.na(event_code) & is.na(remove),remove:=1]
        #remove empty voacbularies
        empty_event_vocabulary_gdm_pe[[w]]<-df[is.na(event_vocabulary) & is.na(remove),.N]
        empty_event_vocabulary_mig[[w]]<-df[is.na(event_vocabulary) & is.na(remove),.N]
        df[is.na(event_vocabulary) & is.na(remove), remove:=1]
        #empty meaning
        empty_event_meaning_gdm_pe[[w]]<-df[is.na(meaning) & is.na(remove),.N]
        empty_event_meaning_mig[[w]]<-df[is.na(meaning) & is.na(remove),.N]
        df[is.na(meaning) & is.na(remove), remove:=1]
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
          df[,obs_min:=NULL][,obs_max:=NULL][,keep:=NULL]
          if(df[,.N]>0){
        #gdm&pe
        df[is.na(remove),prior_gdm_pe:=ifelse(gdm_pe_start_study_date>event_date,1,0)]
        prior_diagnoses_rec_gdm_pe[[w]]<-df[prior_gdm_pe==1 & is.na(remove),.N]
        #migraine
        df[is.na(remove),prior_mig:=ifelse(mig_start_study_date>event_date,1,0)]
        prior_diagnoses_rec_mig[[w]]<-df[prior_mig==1 & is.na(remove),.N]
        #Check if the data needed for the drug utilization and safety study has a longer follow up
        df[is.na(remove),prior_du:=ifelse(du_start_study_date>event_date,1,0)]
        df[is.na(remove),prior_saf:=ifelse(saf_start_study_date>event_date,1,0)]
        #clean up dataset:keep records when at least one is zero
        #df<-df[is.na(remove)][prior_gdm_pe==0|prior_mig==0|prior_du==0|prior_saf==0]
        #remove all records with dates after end study date
        #gdm
        df[is.na(remove),after_gdm_pe:=ifelse(gdm_pe_end_study_date<event_date,1,0)]
        after_diagnoses_rec_gdm_pe[[w]]<-df[after_gdm_pe==1 & prior_gdm_pe==0 & is.na(remove),.N]
        #migraine
        df[is.na(remove),after_mig:=ifelse(mig_end_study_date<event_date,1,0)]
        after_diagnoses_rec_mig[[w]]<-df[after_mig==1 & prior_mig==0 & is.na(remove),.N]
        #Check if the data needed for the drug utilization and safety study has a longer follow up
        df[is.na(remove),after_du:=ifelse(du_end_study_date<event_date,1,0)]
        df[is.na(remove),after_saf:=ifelse(saf_end_study_date<event_date,1,0)]
        #clean up dataset:keep records when at least one is zero
        #df<-df[after_gdm_pe==0|after_mig==0|after_du==0|after_saf==0]
        #create code with nodot
        df[,code_no_dot:=as.character(gsub("\\.","",df[,event_code]))]
        included_records_filtering_gdm_pe[[w]]<-df[prior_gdm_pe==0 & after_gdm_pe==0 & is.na(remove),.N]
        included_records_filtering_mig[[w]]<-df[prior_mig==0 & after_mig==0 & is.na(remove),.N]
        
        # #Create combination inclusion records
        # df[prior_gdm_pe==0 | prior_du==0 | prior_saf==0, comb_prior_gdm_pe:=0]
        # df[after_gdm_pe==0 | after_du==0 | after_saf==0, comb_after_gdm_pe:=0]
        # 
        # df[prior_mig==0 | prior_du==0 | prior_saf==0, comb_prior_mig:=0]
        # df[after_mig==0 | after_du==0 | after_saf==0, comb_after_mig:=0]
        
        if(df[is.na(remove),.N]>0){
          if(df[prior_gdm_pe==0 & after_gdm_pe==0 & is.na(remove),.N]>0){
            #data filtering based on codelists
            if(so_gdm_diagnoses>0){
              print(paste0("Filtering data for GDM diagnoses:", actual_tables$SURVEY_OBSERVATIONS[y]))
              years_study_events<-sort(df[prior_gdm_pe==0 & after_gdm_pe==0 & is.na(remove)][!duplicated(year), year])#years present in this table
              
              if(sum(df[prior_gdm_pe==0 & after_gdm_pe==0 & is.na(remove)][!duplicated(event_vocabulary), event_vocabulary] %in% vocabularies_list_gdm)>0){
                for (i in 1:length(conditions_gdm)){
                  for(j in 1:length(conditions_gdm[[i]])){
                    
                    codes<-data.table(event_vocabulary=names(conditions_gdm[[i]])[j], truncated_code=conditions_gdm[[i]][[j]], filter=1)
                    for(codes_ind in 1:codes[,.N]){
                      length<-nchar(codes[codes_ind,truncated_code])
                      #create truncated code
                      df[,truncated_code:=substr(code_no_dot,1,length)]
                      
                      df<-merge.data.table(df,codes[codes_ind,],by=c("event_vocabulary","truncated_code"),all.x = T,allow.cartesian = T)
                      
                      if(df[prior_gdm_pe==0 & after_gdm_pe==0 & filter==1 & is.na(remove),.N]>0){
                        years_this_event<-sort(df[prior_gdm_pe==0 & after_gdm_pe==0 & filter==1 & is.na(remove)][!duplicated(year),year])
                        for(year_ind in 1:length(years_this_event)){
                          saveRDS(data.table(df[prior_gdm_pe==0 & after_gdm_pe==0 & is.na(remove) & filter==1 & year==years_this_event[year_ind]], condition=names(conditions_gdm[i])), paste0(tmp,years_this_event[year_ind],"_","GDM_", names(conditions_gdm[i]), "_",actual_tables$SURVEY_OBSERVATIONS[y],"_", codes_ind, ".rds"))
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
            
            if(so_pe_diagnoses>0){
              print(paste0("Filtering data for PE diagnoses:", actual_tables$SURVEY_OBSERVATIONS[y]))
              years_study_events<-sort(df[prior_gdm_pe==0 & after_gdm_pe==0 & is.na(remove)][!duplicated(year), year])#years present in this table
              if(sum(df[prior_gdm_pe==0 & after_gdm_pe==0 & is.na(remove)][!duplicated(event_vocabulary), event_vocabulary] %in% vocabularies_list_pe)>0){
                for (i in 1:length(conditions_pe)){
                  for(j in 1:length(conditions_pe[[i]])){
                    
                    codes<-data.table(event_vocabulary=names(conditions_pe[[i]])[j], truncated_code=conditions_pe[[i]][[j]], filter=1)
                    for(codes_ind in 1:codes[,.N]){
                      length<-nchar(codes[codes_ind,truncated_code])
                      #create truncated code
                      df[,truncated_code:=substr(code_no_dot,1,length)]
                      
                      df<-merge.data.table(df,codes[codes_ind,],by=c("event_vocabulary","truncated_code"),all.x = T,allow.cartesian = T)
                      
                      if(df[prior_gdm_pe==0 & after_gdm_pe==0 & filter==1 & is.na(remove),.N]>0){
                        years_this_event<-sort(df[prior_gdm_pe==0 & after_gdm_pe==0 & filter==1 & is.na(remove)][!duplicated(year),year])
                        for(year_ind in 1:length(years_this_event)){
                          saveRDS(data.table(df[prior_gdm_pe==0 & after_gdm_pe==0 & is.na(remove) & filter==1 & year==years_this_event[year_ind]], condition=names(conditions_pe[i])), paste0(tmp,years_this_event[year_ind],"_","PE_", names(conditions_pe[i]), "_",actual_tables$SURVEY_OBSERVATIONS[y],"_", codes_ind, ".rds"))
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
          
          if(df[prior_mig==0 & after_mig==0 & is.na(remove),.N]>0){
            if(so_migraine_diagnoses>0){
              vocabularies_to_start_with<-c("ICD", "ICPC", "RCD")
              print(paste0("Filtering data for Migraine diagnoses:", actual_tables$SURVEY_OBSERVATIONS[y]))
              years_study_events<-sort(df[prior_mig==0 & after_mig==0 & is.na(remove)][!duplicated(year), year])#years present in this table
              
              #startWith for the event Migraine
              if(sum(sapply(vocabularies_to_start_with, function(start) any(startsWith(df[prior_mig==0 & after_mig==0][!duplicated(event_vocabulary), event_vocabulary], start))))>=1){
              if(sum(df[prior_mig==0 & after_mig==0 & is.na(remove)][!duplicated(event_vocabulary), event_vocabulary] %in% vocabularies_list_migraine)>0){
                for (i in 1:length(conditions_migraine)){
                  for(j in 1:length(conditions_migraine[[i]])){
                    
                    codes<-data.table(event_vocabulary=names(conditions_migraine[[i]])[j], truncated_code=conditions_migraine[[i]][[j]], filter=1)
                    for(codes_ind in 1:codes[,.N]){
                      length<-nchar(codes[codes_ind,truncated_code])
                      #create truncated code
                      df[,truncated_code:=substr(code_no_dot,1,length)]
                      
                      df<-merge.data.table(df,codes[codes_ind,],by=c("event_vocabulary","truncated_code"),all.x = T,allow.cartesian = T)
                      
                      if(df[prior_mig==0 & after_mig==0 & filter==1 & is.na(remove),.N]>0){
                        years_this_event<-sort(df[prior_mig==0 & after_mig==0 & filter==1 & is.na(remove)][!duplicated(year),year])
                        for(year_ind in 1:length(years_this_event)){
                          saveRDS(data.table(df[prior_mig==0 & after_mig==0 & filter==1 & is.na(remove) & year==years_this_event[year_ind]], condition=names(conditions_migraine[i])), paste0(tmp,years_this_event[year_ind],"_","Migraine_", names(conditions_migraine[i]), "_",actual_tables$SURVEY_OBSERVATIONS[y],"_", codes_ind, ".rds"))
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
              #exact match for the other events related to migraine
              if(sum(sapply(vocabularies_to_start_with, function(start) any(startsWith(df[prior_mig==0 & after_mig==0][!duplicated(event_vocabulary), event_vocabulary], start))))==0){
              if(sum(df[prior_mig==0 & after_mig==0 & is.na(remove)][!duplicated(event_vocabulary), event_vocabulary] %in% vocabularies_list_migraine)>0){
                for (i in 1:length(conditions_migraine)){
                  for(j in 1:length(conditions_migraine[[i]])){
                    
                    codes<-data.table(event_vocabulary=names(conditions_migraine[[i]])[j], code_no_dot=conditions_migraine[[i]][[j]], filter=1)
                    for(codes_ind in 1:codes[,.N]){
                      #create truncated code
                      df<-merge.data.table(df,codes[codes_ind,],by=c("event_vocabulary","code_no_dot"),all.x = T,allow.cartesian = T)
                      
                      if(df[prior_mig==0 & after_mig==0 & filter==1 & is.na(remove),.N]>0){
                        years_this_event<-sort(df[prior_mig==0 & after_mig==0 & filter==1 & is.na(remove)][!duplicated(year),year])
                        for(year_ind in 1:length(years_this_event)){
                          saveRDS(data.table(df[prior_mig==0 & after_mig==0 & filter==1 & is.na(remove) & year==years_this_event[year_ind]], condition=names(conditions_migraine[i])), paste0(tmp,years_this_event[year_ind],"_","Migraine_", names(conditions_migraine[i]), "_",actual_tables$SURVEY_OBSERVATIONS[y],"_", codes_ind, ".rds"))
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
      
      #Change name back
        setnames(df,"event_date",date_var)
        setnames(df, "event_code",code_var)
        setnames(df, "event_vocabulary",voc_var)
        setnames(df,"meaning", "so_meaning")
        #df[,remove:=NULL][,year:=NULL][,prior_gdm_pe:=NULL][,prior_mig:=NULL][,prior_du:=NULL][,prior_saf:=NULL][,after_gdm_pe:=NULL][,after_mig:=NULL][,after_du:=NULL][,after_saf:=NULL][,code_no_dot:=NULL]
      
      #New diagnoses category
        setnames(df, date_var_2,"event_date")
        setnames(df, code_var_2,"event_code")
        setnames(df, voc_var_2,"event_vocabulary")
        setnames(df,"so_meaning","meaning")
        
        if(df[is.na(remove),.N]>0){
          if(df[prior_gdm_pe==0 & after_gdm_pe==0 & is.na(remove),.N]>0){
            #data filtering based on codelists
            if(so_gdm_diagnoses_cat>0){
              print(paste0("Filtering data for GDM diagnoses category:", actual_tables$SURVEY_OBSERVATIONS[y]))
              years_study_events<-sort(df[prior_gdm_pe==0 & after_gdm_pe==0 & is.na(remove)][!duplicated(year), year])#years present in this table
              
              if(sum(df[prior_gdm_pe==0 & after_gdm_pe==0 & is.na(remove)][!duplicated(event_vocabulary), event_vocabulary] %in% vocabularies_list_gdm)>0){
                for (i in 1:length(conditions_gdm)){
                  for(j in 1:length(conditions_gdm[[i]])){
                    
                    codes<-data.table(event_vocabulary=names(conditions_gdm[[i]])[j], truncated_code=conditions_gdm[[i]][[j]], filter=1)
                    for(codes_ind in 1:codes[,.N]){
                      length<-nchar(codes[codes_ind,truncated_code])
                      #create truncated code
                      df[,truncated_code:=substr(code_no_dot,1,length)]
                      
                      df<-merge.data.table(df,codes[codes_ind,],by=c("event_vocabulary","truncated_code"),all.x = T,allow.cartesian = T)
                      
                      if(df[prior_gdm_pe==0 & after_gdm_pe==0 & filter==1 & is.na(remove),.N]>0){
                        years_this_event<-sort(df[prior_gdm_pe==0 & after_gdm_pe==0 & filter==1 & is.na(remove)][!duplicated(year),year])
                        for(year_ind in 1:length(years_this_event)){
                          saveRDS(data.table(df[prior_gdm_pe==0 & after_gdm_pe==0 & is.na(remove) & filter==1 & year==years_this_event[year_ind]], condition=names(conditions_gdm[i])), paste0(tmp,years_this_event[year_ind],"_","GDMcat_", names(conditions_gdm[i]), "_",actual_tables$SURVEY_OBSERVATIONS[y],"_", codes_ind, ".rds"))
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
            
            if(so_pe_diagnoses_cat>0){
              print(paste0("Filtering data for PE diagnoses category:", actual_tables$SURVEY_OBSERVATIONS[y]))
              years_study_events<-sort(df[prior_gdm_pe==0 & after_gdm_pe==0 & is.na(remove)][!duplicated(year), year])#years present in this table
              if(sum(df[prior_gdm_pe==0 & after_gdm_pe==0 & is.na(remove)][!duplicated(event_vocabulary), event_vocabulary] %in% vocabularies_list_pe)>0){
                for (i in 1:length(conditions_pe)){
                  for(j in 1:length(conditions_pe[[i]])){
                    
                    codes<-data.table(event_vocabulary=names(conditions_pe[[i]])[j], truncated_code=conditions_pe[[i]][[j]], filter=1)
                    for(codes_ind in 1:codes[,.N]){
                      length<-nchar(codes[codes_ind,truncated_code])
                      #create truncated code
                      df[,truncated_code:=substr(code_no_dot,1,length)]
                      
                      df<-merge.data.table(df,codes[codes_ind,],by=c("event_vocabulary","truncated_code"),all.x = T,allow.cartesian = T)
                      
                      if(df[prior_gdm_pe==0 & after_gdm_pe==0 & filter==1 & is.na(remove),.N]>0){
                        years_this_event<-sort(df[prior_gdm_pe==0 & after_gdm_pe==0 & filter==1 & is.na(remove)][!duplicated(year),year])
                        for(year_ind in 1:length(years_this_event)){
                          saveRDS(data.table(df[prior_gdm_pe==0 & after_gdm_pe==0 & is.na(remove) & filter==1 & year==years_this_event[year_ind]], condition=names(conditions_pe[i])), paste0(tmp,years_this_event[year_ind],"_","PEcat_", names(conditions_pe[i]), "_",actual_tables$SURVEY_OBSERVATIONS[y],"_", codes_ind, ".rds"))
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
          
          
          if(df[prior_mig==0 & after_mig==0 & is.na(remove),.N]>0){
            if(so_migraine_diagnoses_cat>0){ 
              vocabularies_to_start_with<-c("ICD", "ICPC", "RCD")
              print(paste0("Filtering data for Migraine diagnoses category:", actual_tables$SURVEY_OBSERVATIONS[y]))
              years_study_events<-sort(df[prior_mig==0 & after_mig==0 & is.na(remove)][!duplicated(year), year])#years present in this table
              
              #startWith for the event Migraine
              if(sum(sapply(vocabularies_to_start_with, function(start) any(startsWith(df[prior_mig==0 & after_mig==0][!duplicated(event_vocabulary), event_vocabulary], start))))>=1){
                if(sum(df[prior_mig==0 & after_mig==0 & is.na(remove)][!duplicated(event_vocabulary), event_vocabulary] %in% vocabularies_list_migraine)>0){
                  for (i in 1:length(conditions_migraine)){
                    for(j in 1:length(conditions_migraine[[i]])){
                      
                      codes<-data.table(event_vocabulary=names(conditions_migraine[[i]])[j], truncated_code=conditions_migraine[[i]][[j]], filter=1)
                      for(codes_ind in 1:codes[,.N]){
                        length<-nchar(codes[codes_ind,truncated_code])
                        #create truncated code
                        df[,truncated_code:=substr(code_no_dot,1,length)]
                        
                        df<-merge.data.table(df,codes[codes_ind,],by=c("event_vocabulary","truncated_code"),all.x = T,allow.cartesian = T)
                        
                        if(df[prior_mig==0 & after_mig==0 & filter==1 & is.na(remove),.N]>0){
                          years_this_event<-sort(df[prior_mig==0 & after_mig==0 & filter==1 & is.na(remove)][!duplicated(year),year])
                          for(year_ind in 1:length(years_this_event)){
                            saveRDS(data.table(df[prior_mig==0 & after_mig==0 & filter==1 & is.na(remove) & year==years_this_event[year_ind]], condition=names(conditions_migraine[i])), paste0(tmp,years_this_event[year_ind],"_","Migrainecat_", names(conditions_migraine[i]), "_",actual_tables$SURVEY_OBSERVATIONS[y],"_", codes_ind, ".rds"))
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
              #exact match for the other events related to migraine
              if(sum(sapply(vocabularies_to_start_with, function(start) any(startsWith(df[prior_mig==0 & after_mig==0][!duplicated(event_vocabulary), event_vocabulary], start))))==0){
                if(sum(df[prior_mig==0 & after_mig==0 & is.na(remove)][!duplicated(event_vocabulary), event_vocabulary] %in% vocabularies_list_migraine)>0){
                  for (i in 1:length(conditions_migraine)){
                    for(j in 1:length(conditions_migraine[[i]])){
                      
                      codes<-data.table(event_vocabulary=names(conditions_migraine[[i]])[j], code_no_dot=conditions_migraine[[i]][[j]], filter=1)
                      for(codes_ind in 1:codes[,.N]){
                        #create truncated code
                        df<-merge.data.table(df,codes[codes_ind,],by=c("event_vocabulary","code_no_dot"),all.x = T,allow.cartesian = T)
                        
                        if(df[prior_mig==0 & after_mig==0 & filter==1 & is.na(remove),.N]>0){
                          years_this_event<-sort(df[prior_mig==0 & after_mig==0 & filter==1 & is.na(remove)][!duplicated(year),year])
                          for(year_ind in 1:length(years_this_event)){
                            saveRDS(data.table(df[prior_mig==0 & after_mig==0 & filter==1 & is.na(remove) & year==years_this_event[year_ind]], condition=names(conditions_migraine[i])), paste0(tmp,years_this_event[year_ind],"_","Migrainecat_", names(conditions_migraine[i]), "_",actual_tables$SURVEY_OBSERVATIONS[y],"_", codes_ind, ".rds"))
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
      ####Diagnoses category####
      #Gather information about not fixed values/Not needed at the moment
      # if(sum(!is.null(diag_cat_gdm),!is.null(diag_cat_pe), !is.null(diag_cat_migraine))>0){
      #   #Diagnoses category GDM&PE
      #   if(!is.null(diag_cat_gdm)){
      #     if(df[prior_gdm_pe==0 & after_gdm_pe==0,.N]>0){
      #       print(paste0("Filtering data for GDM diagnoses category:", actual_tables$SURVEY_OBSERVATIONS[y]))
      #       gdm_diag_cat_so<-gdm_diag_cat[table=="SURVEY_OBSERVATIONS"]
      #       if(gdm_diag_cat_so[,.N]>0){
      #         names_df<-names(df)
      #         same_names<-intersect(names_df,names(gdm_diag_cat_so))
      #         df<-merge.data.table(df,diag_cat_gdm,by=same_names,all.x=T)
      #         if(df[prior_gdm_pe==0 & after_gdm_pe==0 & !is.na(event_abbreviation),.N]>0){
      #           for(cat_ind in 1:gdm_diag_cat_so[,.N]){
      #             cols_to_keep<-c(gdm_diag_cat_so[cat_ind==index,date_column],gdm_diag_cat_so[cat_ind==index,keep])
      #             #rename columns
      #             setnames(df,gdm_diag_cat_so[cat_ind==index,date_column], "event_date")
      #             setnames(df,gdm_diag_cat_so[cat_ind==index,keep], "event_code")
      #             setnames(df, "so_meaning", "meaning")
      #             setnames(df, "event_abbreviation", "condition")
      #             
      #             #date variable
      #             df[,event_date:=as.IDate(event_date, "%Y%m%d")]
      #             if(!"year" %in% names(df)){df[,year:=year(event_date)]}
      #             
      #             cols_to_keep<-c("event_date","event_code", "person_id","meaning","condition","year")
      #             years_cat<-sort(df[prior_gdm_pe==0 & after_gdm_pe==0 & !is.na(condition) & index==cat_ind][!duplicated(year),year])
      #             for(year_ind in 1:length(years_cat)){
      #               saveRDS(df[prior_gdm_pe==0 & after_gdm_pe==0 &!is.na(condition)][year==years_cat[year_ind] & index==cat_ind, cols_to_keep, with=F], paste0(projectFolder,"/g_intermediate/tmp/", years_cat[year_ind],"_", "GDM_diagnoses_cat_so",cat_ind,".rds"))
      #             }# new 01.06.2022
      #             
      #             rm(years_cat,year_ind)
      #             
      #             #rename columns
      #             setnames(df, "event_date", gdm_diag_cat_so[cat_ind==index,date_column])
      #             setnames(df,"event_code", gdm_diag_cat_so[cat_ind==index,keep])
      #             setnames(df, "meaning", "so_meaning")
      #             setnames(df, "condition", "event_abbreviation")
      #             
      #           }
      #           df[,table:=NULL][,event_abbreviation:=NULL][,keep:=NULL][,index:=NULL]
      #         }
      #       }
      #     }
      #   }
      #   #Diagnoses category PE
      #   if(!is.null(diag_cat_pe)){
      #     print(paste0("Filtering data for PE diagnoses category:", actual_tables$SURVEY_OBSERVATIONS[y]))
      #     pe_diag_cat_so<-pe_diag_cat[table=="SURVEY_OBSERVATIONS"]
      #     if(pe_diag_cat_so[,.N]>0){
      #       names_df<-names(df)
      #       same_names<-intersect(names_df,names(pe_diag_cat_so))
      #       df<-merge.data.table(df,diag_cat_pe,by=same_names,all.x=T)
      #       if(df[prior_gdm_pe==0 & after_gdm_pe==0 & !is.na(event_abbreviation),.N]>0){
      #         for(cat_ind in 1:pe_diag_cat_so[,.N]){
      #           cols_to_keep<-c(pe_diag_cat_so[cat_ind==index,date_column],pe_diag_cat_so[cat_ind==index,keep])
      #           #rename columns
      #           setnames(df,pe_diag_cat_so[cat_ind==index,date_column], "event_date")
      #           setnames(df,pe_diag_cat_so[cat_ind==index,keep], "event_code")
      #           setnames(df, "so_meaning", "meaning")
      #           setnames(df, "event_abbreviation", "condition")
      #           
      #           #date variable
      #           df[,event_date:=as.IDate(event_date, "%Y%m%d")]
      #           if(!"year" %in% names(df)){df[,year:=year(event_date)]}
      #           
      #           cols_to_keep<-c("event_date","event_code", "person_id","meaning","condition","year")
      #           
      #           years_cat<-sort(df[prior_gdm_pe==0 & after_gdm_pe==0 & !is.na(condition) & index==cat_ind][!duplicated(year),year])
      #           for(year_ind in 1:length(years_cat)){
      #             saveRDS(df[prior_gdm_pe==0 & after_gdm_pe==0 &!is.na(condition)][year==years_cat[year_ind] & index==cat_ind, cols_to_keep, with=F], paste0(projectFolder,"/g_intermediate/tmp/", years_cat[year_ind],"_", "PE_diagnoses_cat_so",cat_ind,".rds"))
      #           }# new 01.06.2022
      #           
      #           rm(years_cat,year_ind)
      #           
      #           #rename columns
      #           setnames(df, "event_date", pe_diag_cat_so[cat_ind==index,date_column])
      #           setnames(df,"event_code", pe_diag_cat_so[cat_ind==index,keep])
      #           setnames(df, "meaning", "so_meaning")
      #           setnames(df, "condition", "event_abbreviation")
      #           
      #         }
      #         df[,table:=NULL][,event_abbreviation:=NULL][,keep:=NULL][,index:=NULL]
      #       }
      #     }
      #   }
      #   #Migraine
      #   if(!is.null(diag_cat_migraine)){
      #     if(df[prior_mig==0 & after_mig==0,.N]>0){
      #       print(paste0("Filtering data for Migraine diagnoses category:", actual_tables$SURVEY_OBSERVATIONS[y]))
      #       migraine_diag_cat_so<-migraine_diag_cat[table=="SURVEY_OBSERVATIONS"]
      #       if(migraine_diag_cat_so[,.N]>0){
      #         names_df<-names(df)
      #         same_names<-intersect(names_df,names(migraine_diag_cat_so))
      #         df<-merge.data.table(df,diag_cat_migraine,by=same_names,all.x=T)
      #         if(df[prior_mig==0 & after_mig==0 & !is.na(event_abbreviation),.N]>0){
      #           for(cat_ind in 1:migraine_diag_cat_so[,.N]){
      #             cols_to_keep<-c(migraine_diag_cat_so[cat_ind==index,date_column],migraine_diag_cat_so[cat_ind==index,keep])
      #             #rename columns
      #             setnames(df,migraine_diag_cat_so[cat_ind==index,date_column], "event_date")
      #             setnames(df,migraine_diag_cat_so[cat_ind==index,keep], "event_code")
      #             setnames(df, "so_meaning", "meaning")
      #             setnames(df, "event_abbreviation", "condition")
      #             
      #             #date variable
      #             df[,event_date:=as.IDate(event_date, "%Y%m%d")]
      #             if(!"year" %in% names(df)){df[,year:=year(event_date)]}
      #             
      #             cols_to_keep<-c("event_date","event_code", "person_id","meaning","condition","year")
      #             years_cat<-sort(df[prior_mig==0 & after_mig==0 & !is.na(condition) & index==cat_ind][!duplicated(year),year])
      #             for(year_ind in 1:length(years_cat)){
      #               saveRDS(df[prior_mig==0 & after_mig==0 & !is.na(condition)][year==years_cat[year_ind] & index==cat_ind, cols_to_keep, with=F], paste0(projectFolder,"/g_intermediate/tmp/", years_cat[year_ind],"_", "Migraine_diagnoses_cat_so",cat_ind,".rds"))
      #             }# new 01.06.2022
      #             
      #             rm(years_cat,year_ind)
      #             
      #             #rename columns
      #             setnames(df, "event_date", migraine_diag_cat_so[cat_ind==index,date_column])
      #             setnames(df,"event_code", migraine_diag_cat_so[cat_ind==index,keep])
      #             setnames(df, "meaning", "so_meaning")
      #             setnames(df, "condition", "event_abbreviation")
      #             
      #           }
      #           df[,table:=NULL][,event_abbreviation:=NULL][,keep:=NULL][,index:=NULL]
      #         }
      #       }
      #     }
      #   }
      # }
      
        
        setnames(df,"event_date",date_var_2)
        setnames(df, "event_code",code_var_2)
        setnames(df, "event_vocabulary",voc_var_2)
        setnames(df,"meaning", "so_meaning")
        df[,remove:=NULL][,year:=NULL][,prior_gdm_pe:=NULL][,prior_mig:=NULL][,prior_du:=NULL][,prior_saf:=NULL][,after_gdm_pe:=NULL][,after_mig:=NULL][,after_du:=NULL][,after_saf:=NULL][,code_no_dot:=NULL]
        
      ####Checkbox information####
      
      #GDM
      if(!is.null(diag_checkbox_gdm_so)){
  
        if(df[,.N]>0){
          print(paste0("Filtering data for GDM checkbox:", actual_tables$SURVEY_OBSERVATIONS[y]))
          gdm_diag_checkbox_so<-diag_checkbox_gdm_so[table=="SURVEY_OBSERVATIONS"]
          if(gdm_diag_checkbox_so[,.N]>0){
            names_df<-names(df)
            same_names<-as.character(intersect(names_df,names(gdm_diag_checkbox_so)))
            df<-merge.data.table(df,gdm_diag_checkbox_so,by=same_names,all.x=T)
            if(df[!is.na(event_abbreviation),.N]>0){
              for(cat_ind in 1:gdm_diag_checkbox_so[!duplicated(index),.N]){
                cols_to_keep<-c(unique(gdm_diag_checkbox_so[cat_ind==index,checkbox_date]),unique(gdm_diag_checkbox_so[cat_ind==index,keep]))
                #rename columns
                setnames(df,unique(gdm_diag_checkbox_so[cat_ind==index,checkbox_date]), "event_date")
                setnames(df,unique(gdm_diag_checkbox_so[cat_ind==index,keep]), "event_code")
                setnames(df, "so_meaning", "meaning")
                setnames(df, "event_abbreviation", "condition")
                
                #date variable
                df[,event_date:=as.IDate(event_date, "%Y%m%d")]
                df[,prior_gdm_pe:=ifelse(gdm_pe_start_study_date>event_date,1,0)]
                df[,after_gdm_pe:=ifelse(gdm_pe_end_study_date<event_date,1,0)]
                if(!"year" %in% names(df)){df[,year:=year(event_date)]}
                df[prior_gdm_pe==0 & after_gdm_pe==0 & !is.na(condition),include:=1]
                df[,prior_gdm_pe:=NULL][,after_gdm_pe:=NULL]
                
                if(df[include==1 & index==cat_ind,.N]>0){
                cols_to_keep<-c("event_date","event_code", "person_id","meaning","condition","year")
                years_cat<-sort(df[include==1 & index==cat_ind][!duplicated(year),year])
                for(year_ind in 1:length(years_cat)){
                  saveRDS(df[include==1 & !is.na(condition)][year==years_cat[year_ind] & index==cat_ind, cols_to_keep, with=F], paste0(projectFolder,"/g_intermediate/tmp/", years_cat[year_ind],"_", "GDM_diagnoses_checkbox_so",cat_ind,".rds"))
                }# new 01.06.2022
                }
                if("years_cat" %in% ls()){rm(years_cat)}
                if("year_ind" %in% ls()){rm(year_ind)}
                
                #rename columns
                setnames(df, "event_date", unique(gdm_diag_checkbox_so[cat_ind==index,checkbox_date]))
                setnames(df,"event_code", unique(gdm_diag_checkbox_so[cat_ind==index,keep]))
                setnames(df, "meaning", "so_meaning")
                setnames(df, "condition", "event_abbreviation")
                
              }
              df[,table:=NULL][,event_abbreviation:=NULL][,keep:=NULL][,index:=NULL]
            }
          }
        }
      }
     
       #remove uneccessary variables
      if("checkbox_date" %in% names(df)){df[,checkbox_date:=NULL]}
      if("include" %in% names(df)){df[,include:=NULL]}
      if("year" %in% names(df)){df[,year:=NULL]}
      
      #PE
      if(!is.null(diag_checkbox_pe_so)){
        if(df[,.N]>0){
          print(paste0("Filtering data for PE checkbox:", actual_tables$SURVEY_OBSERVATIONS[y]))
          pe_diag_checkbox_so<-diag_checkbox_pe_so[table=="SURVEY_OBSERVATIONS"]
          if(pe_diag_checkbox_so[,.N]>0){
            names_df<-names(df)
            same_names<-intersect(names_df,names(pe_diag_checkbox_so))
            df<-merge.data.table(df,pe_diag_checkbox_so,by=same_names,all.x=T)
            if(df[!is.na(event_abbreviation),.N]>0){
              for(cat_ind in 1:pe_diag_checkbox_so[!duplicated(index),.N]){
                cols_to_keep<-c(unique(pe_diag_checkbox_so[cat_ind==index,checkbox_date]),unique(pe_diag_checkbox_so[cat_ind==index,keep]))
                #rename columns
                setnames(df,unique(pe_diag_checkbox_so[cat_ind==index,checkbox_date]), "event_date")
                setnames(df,unique(pe_diag_checkbox_so[cat_ind==index,keep]), "event_code")
                setnames(df, "so_meaning", "meaning")
                setnames(df, "event_abbreviation", "condition")
                
                #date variable
                df[,event_date:=as.IDate(event_date, "%Y%m%d")]
                df[,prior_gdm_pe:=ifelse(gdm_pe_start_study_date>event_date,1,0)]
                df[,after_gdm_pe:=ifelse(gdm_pe_end_study_date<event_date,1,0)]
                if(!"year" %in% names(df)){df[,year:=year(event_date)]}
                df[prior_gdm_pe==0 & after_gdm_pe==0 & !is.na(condition),include:=1]
                df[,prior_gdm_pe:=NULL][,after_gdm_pe:=NULL]
                
                if(df[include==1 & index==cat_ind,.N]>0){
                cols_to_keep<-c("event_date","event_code", "person_id","meaning","condition","year")
                years_cat<-sort(df[include==1 & !is.na(condition) & index==cat_ind][!duplicated(year),year])
                for(year_ind in 1:length(years_cat)){
                  saveRDS(df[include==1 & !is.na(condition)][year==years_cat[year_ind] & index==cat_ind, cols_to_keep, with=F], paste0(projectFolder,"/g_intermediate/tmp/", years_cat[year_ind],"_", "PE_diagnoses_checkbox_so",cat_ind,".rds"))
                }# new 01.06.2022
                }
                if("years_cat" %in% ls()){rm(years_cat)}
                if("year_ind" %in% ls()){rm(year_ind)}
                
                
                #rename columns
                setnames(df, "event_date", unique(pe_diag_checkbox_so[cat_ind==index,checkbox_date]))
                setnames(df,"event_code", unique(pe_diag_checkbox_so[cat_ind==index,keep]))
                setnames(df, "meaning", "so_meaning")
                setnames(df, "condition", "event_abbreviation")
                
                #remove columns
                if("checkbox_date" %in% names(df)){df[,checkbox_date:=NULL]}
                
              }
              df[,table:=NULL][,event_abbreviation:=NULL][,keep:=NULL][,index:=NULL]
            }
          }
        }
      }
        #close the loop after removing not present subjects and uneccessary dates
      }
      #close the loop use for filtering
    }
    #close the loop
  }
  w<-w+1
  rm(df)
  #continue with the next table
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
  any_study_no<-sum(do.call(rbind,any_study_no))
  outside_obs<-sum(do.call(rbind,outside_obs))
  prior_diagnoses_rec_gdm_pe<-sum(do.call(rbind,prior_diagnoses_rec_gdm_pe))
  prior_diagnoses_rec_mig<-sum(do.call(rbind,prior_diagnoses_rec_mig))
  after_diagnoses_rec_gdm_pe<-sum(do.call(rbind,after_diagnoses_rec_gdm_pe))
  after_diagnoses_rec_mig<-sum(do.call(rbind,after_diagnoses_rec_mig))
  included_records_filtering_gdm_pe<-sum(do.call(rbind,included_records_filtering_gdm_pe))
  included_records_filtering_mig<-sum(do.call(rbind,included_records_filtering_mig))
  
  #create flowchart and export to g_output
  flowchart_so_gdm_pe<-data.table(Indicator=c("Number of original rows",
                                              "Number of records with missing event date",
                                              "Number of records with missing event code",
                                              "Number of records with missing event vocabulary",
                                              "Number of records with missing event meaning",
                                              "Number of records not present in the pregnancy D3 for any of the studies",
                                              "Number of records outside min and max observation period for any of the studies",
                                              "Number of records with event date before start of study date",
                                              "Number of records with event date after end of study date",
                                              "Number of records included, before data filtering"),
                                  SURVEY_OBSERVATIONS=c(original_rows_gdm_pe,
                                                         empty_event_date_gdm_pe,
                                                         empty_event_code_gdm_pe,
                                                         empty_event_vocabulary_gdm_pe,
                                                         empty_event_meaning_gdm_pe,
                                                        any_study_no,
                                                        outside_obs,
                                                         prior_diagnoses_rec_gdm_pe,
                                                         after_diagnoses_rec_gdm_pe,
                                                         included_records_filtering_gdm_pe))
  #fwrite(flowchart_events, paste0(output_dir, "PE and GDM algorithm/flowchart_events.csv"), row.names = F)
  rm(original_rows_gdm_pe,empty_event_date_gdm_pe,empty_event_code_gdm_pe,empty_event_vocabulary_gdm_pe,empty_event_meaning_gdm_pe,prior_diagnoses_rec_gdm_pe,after_diagnoses_rec_gdm_pe,included_records_filtering_gdm_pe)
  
  flowchart_so_mig<-data.table(Indicator=c("Number of original rows",
                                           "Number of records with missing event date",
                                           "Number of records with missing event code",
                                           "Number of records with missing event vocabulary",
                                           "Number of records with missing event meaning",
                                           "Number of records not present in the pregnancy D3 for any of the studies",
                                           "Number of records outside min and max observation period for any of the studies",
                                           "Number of records with event date before start of study date",
                                           "Number of records with event date after end of study date",
                                           "Number of records included, before data filtering"),
                               SURVEY_OBSERVATIONS=c(original_rows_mig,
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
  
  
}else{
  flowchart_so_gdm_pe<-data.table(Indicator=c("Number of original rows",
                                              "Number of records with missing event date",
                                              "Number of records with missing event code",
                                              "Number of records with missing event vocabulary",
                                              "Number of records with missing event meaning",
                                              "Number of records not present in the pregnancy D3 for any of the studies",
                                              "Number of records outside min and max observation period for any of the studies",
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
                                                         0,
                                                         0,
                                                         0))
  
  flowchart_so_mig<-data.table(Indicator=c("Number of original rows",
                                           "Number of records with missing event date",
                                           "Number of records with missing event code",
                                           "Number of records with missing event vocabulary",
                                           "Number of records with missing event meaning",
                                           "Number of records not present in the pregnancy D3 for any of the studies",
                                           "Number of records outside min and max observation period for any of the studies",
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
                                                      0,
                                                      0,
                                                      0))
}
}else{
  flowchart_so_gdm_pe<-data.table(Indicator=c("Number of original rows",
                                              "Number of records with missing event date",
                                              "Number of records with missing event code",
                                              "Number of records with missing event vocabulary",
                                              "Number of records with missing event meaning",
                                              "Number of records not present in the pregnancy D3 for any of the studies",
                                              "Number of records outside min and max observation period for any of the studies",
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
                                                         0,
                                                         0,
                                                         0))
  
  flowchart_so_mig<-data.table(Indicator=c("Number of original rows",
                                           "Number of records with missing event date",
                                           "Number of records with missing event code",
                                           "Number of records with missing event vocabulary",
                                           "Number of records with missing event meaning",
                                           "Number of records not present in the pregnancy D3 for any of the studies",
                                           "Number of records outside min and max observation period for any of the studies",
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
                                                      0,
                                                      0,
                                                      0))
  
  
}