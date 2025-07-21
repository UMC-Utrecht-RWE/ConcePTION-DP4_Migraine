####EVENTS####
#check if we need to search the events table for GDM
if("EVENTS" %in% tables_diagnoses){
  if(codesheet_diagnoses_hyp[,.N]>0){
    code_var<-codesheet_diagnoses_hyp[table=="EVENTS",col_1]
    voc_var<-codesheet_diagnoses_hyp[table=="EVENTS",col_2]
    date_var<-codesheet_diagnoses_hyp[table=="EVENTS",date_column]
  }
  if(codesheet_diagnoses_dm[,.N]>0){
    code_var<-codesheet_diagnoses_dm[table=="EVENTS",col_1]
    voc_var<-codesheet_diagnoses_dm[table=="EVENTS",col_2]
    date_var<-codesheet_diagnoses_dm[table=="EVENTS",date_column]
  }
  if(codesheet_diagnoses_dep[,.N]>0){
    code_var<-codesheet_diagnoses_dep[table=="EVENTS",col_1]
    voc_var<-codesheet_diagnoses_dep[table=="EVENTS",col_2]
    date_var<-codesheet_diagnoses_dep[table=="EVENTS",date_column]
  }
  if(codesheet_diagnoses_parity[,.N]>0){
    code_var<-codesheet_diagnoses_parity[table=="EVENTS",col_1]
    voc_var<-codesheet_diagnoses_parity[table=="EVENTS",col_2]
    date_var<-codesheet_diagnoses_parity[table=="EVENTS",date_column]
  }
  if(codesheet_diagnoses_bmi[,.N]>0){
    code_var<-codesheet_diagnoses_bmi[table=="EVENTS",col_1]
    voc_var<-codesheet_diagnoses_bmi[table=="EVENTS",col_2]
    date_var<-codesheet_diagnoses_bmi[table=="EVENTS",date_column]
  }
  
  
  if(length(actual_tables$EVENTS)>0){
    #Load the observation periods hint table(use to make smaller the table)
    hint_fl<-list.files(paste0(projectFolder, "/g_intermediate/pregnancy_d3/"), "obs_period_hint_DU.rds")
    #use this table first to remove all uneccessary subjects(not needed for any of the studies)
    obs_hint_table<-readRDS(paste0(projectFolder, "/g_intermediate/pregnancy_d3/", hint_fl))
    print("Analyse EVENTS table.")
    ####List for saving info####
    print("Creating lists to save the information.")
    original_rows_du<-list() #number of table original rows
    empty_ev_date_du<-list() #number of records with missing medicine date
    empty_code_du<-list() #numer of records with missing atc code
    empty_voc_du<-list()
    empty_meaning_du<-list() #numer of records with missing med meaning
    any_study_no<-list() #number of records for subjects not needed in any of the SAPs
    outside_obs<-list() #number of records outside obs min and obs max between SAPs for each subject
    prior_ev_rec_du<-list() #number of records with date prior to start study date
    after_ev_rec_du<-list() #number of records with date after end study date
    pre_dt_du<-list()
 
    w<-1
    #####Run the loop section####
    for (y in 1:length(actual_tables$EVENTS)){
      print(paste0("Analyzing table ",actual_tables$EVENTS[y], "."))
      #Load the table
      cols<-c("person_id", "meaning_of_event", code_var, voc_var, date_var)
      
      df<-fread(paste(path_dir, actual_tables$EVENTS[y], sep=""), stringsAsFactors = FALSE, colClasses = "character", select = cols)
      df<-df[,cols, with=F]
      df<-df[, lapply(.SD, FUN=function(x) gsub("^$|^ $", NA, x))] #make sure missing data is read appropriately
      setnames(df,"meaning_of_event","meaning")
      setnames(df, date_var,"event_date")
      setnames(df, code_var,"event_code")
      setnames(df, voc_var,"coding_system")
      
      #number of original rows
      original_rows_du[[w]]<-df[,.N]
      #records with empty event date
      empty_ev_date_du[[w]]<-df[is.na(event_date),.N]
      df<-df[!is.na(event_date)]
      #remove records with empty codes
      empty_code_du[[w]]<-df[is.na(event_code),.N]
      df<-df[!is.na(event_code)]
      #remove records with empty voc
      empty_voc_du[[w]]<-df[is.na(coding_system),.N]
      df<-df[!is.na(coding_system)]
      #empty meaning
      empty_meaning_du[[w]]<-df[is.na(meaning),.N]
      df<-df[!is.na(meaning)]
      #transform into date variables
      df[,event_date:=ymd(event_date)] #transform to date variables
      #create year variable
      df[,year:=year(event_date)]
      #merge with the obs_hint table and remove subjects not present in the pregnancy d3 for any of teh studies
      setkey(df, person_id)
      setkey(obs_hint_table, person_id)
      df<-merge.data.table(df, obs_hint_table, by="person_id", all.x = T)
      any_study_no[[w]]<-df[is.na(obs_min), .N]
      df<-df[!is.na(obs_min)]
      if(df[,.N]>0){
        #Remove all records outside the window for observation for each subject
        setorder(df, event_date)
        df[event_date>=obs_min & event_date<=obs_max, keep:=1]
        outside_obs[[w]]<-df[is.na(keep),.N]
        df<-df[keep==1]
        if(df[,.N]>0){
          #remove all records with dates prior to start study date or start study date plus lookback if any
          #migraine
          df[,prior_du:=ifelse(du_start_study_date>event_date,1,0)]
          prior_ev_rec_du[[w]]<-df[prior_du==1,.N]
          #clean up dataset:keep records when at least one is zero
          df<-df[prior_du==0]
          #remove all records with dates after end study date
          #migraine
          df[,after_du:=ifelse(du_end_study_date<event_date,1,0)]
          after_ev_rec_du[[w]]<-df[after_du==1 & prior_du==0,.N]
          #Check if the data needed for the drug utilization and safety study has a longer follow up
          df[,after_du:=ifelse(du_end_study_date<event_date,1,0)]
          #clean up dataset:keep records when at least one is zero
          df<-df[after_du==0]
          #incl rec
          pre_dt_du[[w]]<-df[prior_du==0 & after_du==0,.N]
          if(df[,.N]>0){
            table_name<-actual_tables$EVENTS[y]
            #### ----------- Step 2: DAP specific codelist ---------- ####
            df[, code_no_dot := gsub("\\.", "", event_code)]

            # DAP_specific_codelist<-df[, .(code_no_dot, coding_system)][!duplicated(code_no_dot,coding_system)]
            DAP_specific_codelist<-collapse::funique(df[, .(code_no_dot, coding_system)])
            
            #### ---------- Step 3: Update the programming codelist by keeping only the vocabularies of interest ---------- ####
            short_codelist<-codelist_final[coding_system %in% DAP_specific_codelist[!duplicated(coding_system), coding_system]]
            
            #### ----------- Step 4: Create the event aggregated nested list ---------- ####
            if(short_codelist[,.N]>0){
              aggregated_list <- short_codelist[, .(code_no_dot = list(code_no_dot)), by = .(variable_name, coding_system)]
              # Convert the aggregated data table into a double-level list
              nested_list <- split(aggregated_list, aggregated_list$variable_name)
              nested_list <- lapply(nested_list, function(x) {
                setNames(as.list(x$code_no_dot), x$coding_system)
              })
              
              events_present<-short_codelist[,.(variable_name, coding_system)]
              events_present<-unique(events_present)
            }
            #### ----------- Step 5: Filter data ---------- ####
            if(short_codelist[,.N]>0){
              #Wrap function: needs to be updated to be separate
              conception_concept_creation<-function(filter_mechanism, events_presence){
                
                for (event_ind in events_presence[,variable_name]){
                  event_placeholder<-event_ind
                  for (voc_ind in events_presence[variable_name == event_ind, coding_system]){
                    voc_placeholder<-voc_ind
                    mechanism<-filter_mechanism[coding_system == voc_ind, mechanism]
                    
                    if(mechanism == "start"){
                      inside_filter_function_start(dataset = df,
                                                   specific_codelist = DAP_specific_codelist,
                                                   nested_codelist = nested_list, 
                                                   full_codelist = short_codelist, 
                                                   code_var = "code_no_dot",
                                                   vocabulary_var<-"coding_system",
                                                   event_var<-"variable_name",
                                                   tags_var = "tags",
                                                   event_name = event_placeholder,
                                                   voc_name = voc_placeholder,
                                                   file_name = table_name, 
                                                   output_folder = paste0(projectFolder, "/g_intermediate/tmp/"),
                                                   var_export = c("person_id", "event_date", "variable_name","tags"))
                    }
                    
                    if(mechanism == "exact"){
                      inside_filter_function_exact(dataset = df,
                                                   specific_codelist = DAP_specific_codelist,
                                                   nested_codelist = nested_list, 
                                                   full_codelist = short_codelist,
                                                   code_var = "code_no_dot",
                                                   vocabulary_var<-"coding_system",
                                                   event_var<-"variable_name",
                                                   tags_var = "tags",
                                                   event_name = event_placeholder,
                                                   voc_name = voc_placeholder,
                                                   file_name = table_name, 
                                                   output_folder = paste0(projectFolder, "/g_intermediate/tmp/"),
                                                   var_export = c("person_id", "event_date", "variable_name","tags"))
                    }
                    
                    rm(voc_placeholder)
                  }
                  rm(event_placeholder)
                }
                
              }
              conception_concept_creation(filter_mechanism = mechanism,
                                          events_presence = events_present)
              
            }

          }
        }
      }
      w<-w+1
      rm(df)
    }
    rm(hint_fl,obs_hint_table)
    ####Combine results####
    #combine flowchart results
    original_rows_du<-list() #number of table original rows
    empty_ev_date_du<-list() #number of records with missing medicine date
    empty_code_du<-list() #numer of records with missing atc code
    empty_voc_du<-list()
    empty_meaning_du<-list() #numer of records with missing med meaning
    any_study_no<-list() #number of records for subjects not needed in any of the SAPs
    outside_obs<-list() #number of records outside obs min and obs max between SAPs for each subject
    prior_ev_rec_du<-list() #number of records with date prior to start study date
    after_ev_rec_du<-list() #number of records with date after end study date
    pre_dt_du<-list()
    
    
    original_rows_du<-sum(do.call(rbind,original_rows_du))
    empty_ev_date_du<-sum(do.call(rbind,empty_ev_date_du))
    empty_code_du<-sum(do.call(rbind,empty_code_du))
    empty_voc_du<-sum(do.call(rbind,empty_voc_du))
    empty_meaning_du<-sum(do.call(rbind,empty_meaning_du))
    any_study_no<-sum(do.call(rbind,any_study_no))
    outside_obs<-sum(do.call(rbind,outside_obs))
    prior_ev_rec_du<-sum(do.call(rbind,prior_ev_rec_du))
    after_ev_rec_du<-sum(do.call(rbind,after_ev_rec_du))
    pre_dt_du<-sum(do.call(rbind,pre_dt_du))

    #create flowchart and export to g_output
    flowchart_events_du<-data.table(Indicator=c("Number of original rows",
                                                    "Number of records with missing event date",
                                                    "Number of records with missing event code",
                                                    "Number of records with missing event vocabulary",
                                                    "Number of records with missing event meaning",
                                                    "Number of records not present in the pregnancy D3 for the DU study",
                                                    "Number of records outside min and max observation period for the DU study",
                                                    "Number of records with event date before start of study date",
                                                    "Number of records with event date after end of study date",
                                                    "Number of records included, before data filtering"),
                                        EVENTS=c(original_rows_du,
                                                 empty_ev_date_du,
                                                 empty_code_du,
                                                 empty_voc_du,
                                                 empty_meaning_du,
                                                 any_study_no,
                                                 outside_obs,
                                                 prior_ev_rec_du,
                                                 after_ev_rec_du,
                                                 pre_dt_du))
    rm(original_rows_du,empty_ev_date_du,empty_code_du,empty_voc_du,empty_meaning_du,any_study_no,outside_obs,prior_ev_rec_du,after_ev_rec_du,pre_dt_du)
    fwrite(flowchart_events_du, paste0(projectFolder, "/g_output/Drug utilisation/flowchart/events_table_flowchart.csv"), row.names = F)
  } else {
    flowchart_events_du<-data.table(Indicator=c("Number of original rows",
                                                "Number of records with missing event date",
                                                "Number of records with missing event code",
                                                "Number of records with missing event vocabulary",
                                                "Number of records with missing event meaning",
                                                "Number of records not present in the pregnancy D3 for the DU study",
                                                "Number of records outside min and max observation period for the DU study",
                                                "Number of records with event date before start of study date",
                                                "Number of records with event date after end of study date",
                                                "Number of records included, before data filtering"),
                                    EVENTS=rep(0,10))
    fwrite(flowchart_events_du, paste0(projectFolder, "/g_output/Drug utilisation/flowchart/events_table_flowchart.csv"), row.names = F)
  }
}else{
  flowchart_events_du<-data.table(Indicator=c("Number of original rows",
                                              "Number of records with missing event date",
                                              "Number of records with missing event code",
                                              "Number of records with missing event vocabulary",
                                              "Number of records with missing event meaning",
                                              "Number of records not present in the pregnancy D3 for the DU study",
                                              "Number of records outside min and max observation period for the DU study",
                                              "Number of records with event date before start of study date",
                                              "Number of records with event date after end of study date",
                                              "Number of records included, before data filtering"),
                                  EVENTS=rep("N/A",10))

  fwrite(flowchart_events_du, paste0(projectFolder, "/g_output/Drug utilisation/flowchart/events_table_flowchart.csv"), row.names = F)
}
# Define the vector of variable names to potentially remove
vars_to_remove <- c("code_var", "voc_var", "date_var", "code_var_check", "voc_var_check", "date_var_check", "code_var_fix", "voc_var_fix", "date_var_fix")

# Check if variables exist and remove only those that do
rm(list = vars_to_remove[vars_to_remove %in% ls()])
