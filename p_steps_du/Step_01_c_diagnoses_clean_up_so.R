####SURVEY_OBSERVATIONS####
#check if we need to search the events table for GDM
if("SURVEY_OBSERVATIONS" %in% unique(c(tables_diagnoses, tables_checkbox,tables_columns))){
  source(paste0(pre_dir, "Step_01_c_pre_diagnoses_clean_up_so.R"))
  
  if(length(actual_tables$SURVEY_OBSERVATIONS)>0){
    #Load the observation periods hint table(use to make smaller the table)
    hint_fl<-list.files(paste0(projectFolder, "/g_intermediate/pregnancy_d3/"), "obs_period_hint_DU.rds")
    #use this table first to remove all uneccessary subjects(not needed for any of the studies)
    obs_hint_table<-readRDS(paste0(projectFolder, "/g_intermediate/pregnancy_d3/", hint_fl))
    print("Analyse SURVEY_OBSERVATIONS table.")
    ####List for saving info####
    print("Creating lists to save the information.")
    original_rows_du<-list() #number of table original rows
    empty_ev_date_du<-list() #number of records with missing medicine date
    empty_meaning_du<-list() #numer of records with missing med meaning
    any_study_no<-list() #number of records for subjects not needed in any of the SAPs
    outside_obs<-list() #number of records outside obs min and obs max between SAPs for each subject
    prior_ev_rec_du<-list() #number of records with date prior to start study date
    after_ev_rec_du<-list() #number of records with date after end study date
    
    empty_code_du_diag<-list() #numer of records with missing atc code
    empty_voc_du_diag<-list()
    empty_code_du_check<-list() #numer of records with missing atc code
    empty_voc_du_check<-list()
    empty_code_du_fix<-list() #numer of records with missing atc code
    empty_voc_du_fix<-list()

    pre_dt_du_diag<-list()
    pre_dt_du_check<-list()
    pre_dt_du_fix<-list()
    
    
    
    vars <- c("code_var", "code_var_check", "code_var_fix", "voc_var", "voc_var_check","voc_var_fix","date_var","date_var_check","date_var_fix")
    
    # Check if each variable exists, if not, assign NULL
    for (v in vars) {
      if (!exists(v)) assign(v, NULL)
    }
    
    cols_needed<-unique(c(code_var, code_var_check, code_var_fix, voc_var, voc_var_check, voc_var_fix, date_var, date_var_check, date_var_fix, "so_meaning", "person_id"))
    
    w<-1
    #####Run the loop section####
    for (y in 1:length(actual_tables$SURVEY_OBSERVATIONS)){
      print(paste0("Analyzing table ",actual_tables$SURVEY_OBSERVATIONS[y], "."))
      #Load the table
      df<-fread(paste(path_dir, actual_tables$SURVEY_OBSERVATIONS[y], sep=""), stringsAsFactors = FALSE, colClasses = "character", select = cols_needed)
      df<-df[,cols_needed, with=F]
      df<-df[, lapply(.SD, FUN=function(x) gsub("^$|^ $", NA, x))] #make sure missing data is read appropriately
      setnames(df,"so_meaning","meaning")
      df[,keep_diag:=1][,keep_check:=1][,keep_fix:=1]
      #number of original rows
      original_rows_du[[w]]<-df[,.N]
      #records with empty event date
      empty_ev_date_du[[w]]<-df[is.na(so_date),.N]
      df<-df[!is.na(so_date)]
      #transform into date variables
      # df[,so_date:=as.Date(so_date,"%Y%m%d")] #transform to date variables
      df[,so_date:=ymd(so_date)] #transform to date variables
      #create year variable
      df[,year:=year(so_date)]
      #empty meaning
      empty_meaning_du[[w]]<-df[is.na(meaning),.N]
      df<-df[!is.na(meaning)]
      #merge with the obs_hint table and remove subjects not present in the pregnancy d3 for any of teh studies
      setkey(df, person_id)
      setkey(obs_hint_table, person_id)
      df<-merge.data.table(df, obs_hint_table, by="person_id", all.x = T)
      any_study_no[[w]]<-df[is.na(obs_min), .N]
      df<-df[!is.na(obs_min)]
      if(df[,.N]>0){
        #Remove all records outside the window for observation for each subject
        df[so_date>=obs_min & so_date<=obs_max, keep:=1]
        df[is.na(keep), keep:=0]
        outside_obs[[w]]<-df[keep==0,.N]
        df<-df[keep==1]
        df[,keep:=NULL]
        if(df[,.N]>0){
          #remove all records with dates prior to start study date or start study date plus lookback if any
          #du
          df[,prior_du:=ifelse(du_start_study_date>so_date,1,0)]
          prior_ev_rec_du[[w]]<-df[prior_du==1,.N]
          df<-df[prior_du==0]
          df[,prior_du:=NULL]
          if(df[,.N]>0){
          df[,after_du:=ifelse(du_end_study_date<so_date,1,0)]
          after_ev_rec_du[[w]]<-df[after_du==1,.N]
          #clean up dataset:keep records when at least one is zero
          df<-df[after_du==0]
          df[,after_du:=NULL]
          if(df[,.N]>0){
            #Set up analysis for diagnoses
            if("SURVEY_OBSERVATIONS" %in% tables_diagnoses){
              setnames(df, date_var,"event_date")
              setnames(df, code_var,"event_code")
              setnames(df, voc_var,"coding_system")
              
              #remove records with empty codes
              empty_code_du_diag[[w]]<-df[is.na(event_code) & keep_diag==1,.N]
              df[is.na(event_code) & keep_diag==1, keep_diag:=0]
              #remove records with empty voc
              empty_voc_du_diag[[w]]<-df[is.na(coding_system)& keep_diag==1,.N]
              df[is.na(coding_system) & keep_diag==1, keep_diag:=0]
              #incl rec
              pre_dt_du[[w]]<-df[keep_diag==1,.N]
              if(df[keep_diag==1,.N]>0){
                    table_name<-actual_tables$SURVEY_OBSERVATIONS[y]
                    #### ----------- Step 2: DAP specific codelist ---------- ####
                    df[keep_diag==1, code_no_dot := gsub("\\.", "", event_code)]
                    # DAP_specific_codelist<-df[keep_diag==1, .(code_no_dot, coding_system)][!duplicated(code_no_dot,coding_system)]
                    DAP_specific_codelist<-collapse::funique(df[keep_diag==1, .(code_no_dot, coding_system)])
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
                              inside_filter_function_start(dataset = df[keep_diag==1],
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
                                                           var_export = c("person_id", "event_date", "variable_name", "tags"))
                            }
                            
                            if(mechanism == "exact"){
                              inside_filter_function_exact(dataset = df[keep_diag==1],
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
                                                           var_export = c("person_id", "event_date", "variable_name", "tags"))
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
            df[,keep_diag:=NULL]
            
            #set df names back to original
            if("event_date" %in% names(df)){setnames(df,"event_date",date_var)}
            if("event_code" %in% names(df)){setnames(df, "event_code",code_var)}
            if("coding_system" %in% names(df)){setnames(df, "coding_system",voc_var)}
            
            
            #Set up analysis for checkbox
            if("SURVEY_OBSERVATIONS" %in% tables_checkbox){

              #remove records with empty codes
              empty_code_du_check[[w]]<-df[is.na(get(code_var_check)) & keep_check==1,.N]
              df[is.na(get(code_var_check)) & keep_check==1, keep_check:=0]
              #remove records with empty voc
              empty_voc_du_check[[w]]<-df[is.na(get(voc_var_check)) & keep_check==1,.N]
              df[is.na(get(voc_var_check)) & keep_check==1, keep_check:=0]
              #incl rec
              pre_dt_du_check[[w]]<-df[keep_check==1,.N]
              if(df[keep_check==1,.N]>0){
                table_name<-actual_tables$SURVEY_OBSERVATIONS[y]
                print(paste0("Filtering data for Hypertension checkbox:", actual_tables$SURVEY_OBSERVATIONS[y]))
                names_df<-names(df)
                if(!is.null(diag_checkbox_hyp_so)){
                same_names<-intersect(names_df,names(diag_checkbox_hyp_so))
                df<-merge.data.table(df,diag_checkbox_hyp_so,by=same_names,all.x=T)
                if(df[!is.na(event_abbreviation),.N]>0){
                  setnames(df, date_var_check, "event_date")
                  setnames(df, "event_abbreviation", "variable_name")
                  
                  saveRDS(df[!is.na(variable_name), c("person_id", "event_date", "variable_name")], paste0(projectFolder, "/g_intermediate/tmp/Checkbox_", unique(diag_checkbox_hyp_so[,event_abbreviation]), "_", actual_tables$SURVEY_OBSERVATIONS[y], ".rds"))
               
                             if("event_date" %in% names(df)){setnames(df,"event_date",date_var_check)}
                             # Remove columns
                             df[, c("table", "variable_name", "keep", "index", "checkbox_date") := NULL]
                             
                             
                             
                }else{
                  df[, c("table", "event_abbreviation", "keep", "index", "checkbox_date") := NULL]
                }
                }
                
                print(paste0("Filtering data for Diabetes checkbox:", actual_tables$SURVEY_OBSERVATIONS[y]))
                if(!is.null(diag_checkbox_dm_so)){
                  same_names<-intersect(names_df,names(diag_checkbox_dm_so))
                  df<-merge.data.table(df,diag_checkbox_dm_so,by=same_names,all.x=T)
                  if(df[!is.na(event_abbreviation),.N]>0){
                    setnames(df, date_var_check, "event_date")
                    setnames(df, "event_abbreviation", "variable_name")
                    
                    saveRDS(df[!is.na(variable_name), c("person_id", "event_date", "variable_name")], paste0(projectFolder, "/g_intermediate/tmp/Checkbox_", unique(diag_checkbox_dm_so[,event_abbreviation]), "_", actual_tables$SURVEY_OBSERVATIONS[y], ".rds"))
                               
                               if("event_date" %in% names(df)){setnames(df,"event_date",date_var_check)}
                               # Remove columns
                               df[, c("table", "variable_name", "keep", "index", "checkbox_date") := NULL]
                               
                               
                               
                  }else{
                    df[, c("table", "event_abbreviation", "keep", "index", "checkbox_date") := NULL]
                  }
                }
                
                print(paste0("Filtering data for Depression checkbox:", actual_tables$SURVEY_OBSERVATIONS[y]))
                if(!is.null(diag_checkbox_dep_so)){
                  same_names<-intersect(names_df,names(diag_checkbox_dep_so))
                  df<-merge.data.table(df,diag_checkbox_dep_so,by=same_names,all.x=T)
                  if(df[!is.na(event_abbreviation),.N]>0){
                    setnames(df, date_var_check, "event_date")
                    setnames(df, "event_abbreviation", "variable_name")
                    
                    saveRDS(df[!is.na(variable_name), c("person_id", "event_date", "variable_name")], paste0(projectFolder, "/g_intermediate/tmp/Checkbox_", unique(diag_checkbox_dep_so[,event_abbreviation]), "_", actual_tables$SURVEY_OBSERVATIONS[y], ".rds"))
                               
                               if("event_date" %in% names(df)){setnames(df,"event_date",date_var_check)}
                               # Remove columns
                               df[, c("table", "variable_name", "keep", "index", "checkbox_date") := NULL]
                               
                               
                               
                  }else{
                    df[, c("table", "event_abbreviation", "keep", "index", "checkbox_date") := NULL]
                  }
                }
                
                print(paste0("Filtering data for Parity checkbox:", actual_tables$SURVEY_OBSERVATIONS[y]))
                if(!is.null(diag_checkbox_parity_so)){
                  same_names<-intersect(names_df,names(diag_checkbox_parity_so))
                  df<-merge.data.table(df,diag_checkbox_parity_so,by=same_names,all.x=T)
                  if(df[!is.na(event_abbreviation),.N]>0){
                    setnames(df, date_var_check, "event_date")
                    setnames(df, "event_abbreviation", "variable_name")
                    
                    saveRDS(df[!is.na(variable_name), c("person_id", "event_date", "variable_name")], paste0(projectFolder, "/g_intermediate/tmp/Checkbox_", unique(diag_checkbox_parity_so[,event_abbreviation]), "_", actual_tables$SURVEY_OBSERVATIONS[y], ".rds"))
                               
                               if("event_date" %in% names(df)){setnames(df,"event_date",date_var_check)}
                               # Remove columns
                               df[, c("table", "variable_name", "keep", "index", "checkbox_date") := NULL]
                               
                               
                               
                  }else{
                    df[, c("table", "event_abbreviation", "keep", "index", "checkbox_date") := NULL]
                  }
                }
                
                print(paste0("Filtering data for Obesity checkbox:", actual_tables$SURVEY_OBSERVATIONS[y]))
                if(!is.null(diag_checkbox_obesity_so)){
                  same_names<-intersect(names_df,names(diag_checkbox_obesity_so))
                  df<-merge.data.table(df,diag_checkbox_obesity_so,by=same_names,all.x=T)
                  if(df[!is.na(event_abbreviation),.N]>0){
                    setnames(df, date_var_check, "event_date")
                    setnames(df, "event_abbreviation", "variable_name")
                    
                    saveRDS(df[!is.na(variable_name), c("person_id", "event_date", "variable_name")], paste0(projectFolder, "/g_intermediate/tmp/Checkbox_", unique(diag_checkbox_obesity_so[,event_abbreviation]), "_", actual_tables$SURVEY_OBSERVATIONS[y], ".rds"))
                               
                               if("event_date" %in% names(df)){setnames(df,"event_date",date_var_check)}
                               # Remove columns
                               df[, c("table", "variable_name", "keep", "index", "checkbox_date") := NULL]
                               
                               
                               
                  }else{
                    df[, c("table", "event_abbreviation", "keep", "index", "checkbox_date") := NULL]
                  }
                }
                
              }
              
 
              }
            df[,keep_check:=NULL]
            
            #Set up analysis for fixed
            if("SURVEY_OBSERVATIONS" %in% tables_columns){
              
              #remove records with empty codes
              empty_code_du_fix[[w]]<-df[is.na(get(code_var_fix)) & keep_fix==1,.N]
              df[is.na(get(code_var_fix)) & keep_fix==1, keep_fix:=0]
              #remove records with empty voc
              empty_voc_du_fix[[w]]<-df[is.na(get(voc_var_fix)) & keep_fix==1,.N]
              df[is.na(get(voc_var_fix)) & keep_fix==1, keep_fix:=0]
              #incl rec
              pre_dt_du_fix[[w]]<-df[keep_fix==1,.N]
              if(df[keep_fix==1,.N]>0){
                table_name<-actual_tables$SURVEY_OBSERVATIONS[y]
                print(paste0("Filtering data for Hypertension fixed columns:", actual_tables$SURVEY_OBSERVATIONS[y]))
                names_df<-names(df)
                if(!is.null(diag_fixed_hyp_so)){
                  same_names<-intersect(names_df,names(diag_fixed_hyp_so))
                  df<-merge.data.table(df,diag_fixed_hyp_so,by=same_names,all.x=T)
                  if(df[!is.na(event_abbreviation),.N]>0){
                    setnames(df, date_var_fix, "event_date")
                    setnames(df, "event_abbreviation", "variable_name")
                    col_keep<-unique(diag_fixed_hyp_so[,keep])
                    setnames(df, col_keep, "event_code")
                    
                    saveRDS(df[!is.na(variable_name), c("person_id", "event_date", "variable_name", "event_code")], paste0(projectFolder, "/g_intermediate/tmp/Fixed_", unique(diag_fixed_hyp_so[,event_abbreviation]), "_", actual_tables$SURVEY_OBSERVATIONS[y], ".rds"))
                    
                    if("event_date" %in% names(df)){setnames(df,"event_date",date_var_fix)}
                    if("event_code" %in% names(df)){setnames(df,"event_code",col_keep)}
                    # Remove columns
                    df[, c("table", "variable_name", "keep", "checkbox_date") := NULL]
                    
                    
                    
                  }else{
                    df[, c("table", "event_abbreviation", "keep", "checkbox_date") := NULL]
                  }
                }
                
                print(paste0("Filtering data for Diabetes fixed columns:", actual_tables$SURVEY_OBSERVATIONS[y]))
                if(!is.null(diag_fixed_dm_so)){
                  same_names<-intersect(names_df,names(diag_fixed_dm_so))
                  df<-merge.data.table(df,diag_fixed_dm_so,by=same_names,all.x=T)
                  if(df[!is.na(event_abbreviation),.N]>0){
                    setnames(df, date_var_fix, "event_date")
                    setnames(df, "event_abbreviation", "variable_name")
                    col_keep<-unique(diag_fixed_dm_so[,keep])
                    setnames(df, col_keep, "event_code")
                    
                    saveRDS(df[!is.na(variable_name), c("person_id", "event_date", "variable_name", "event_code")], paste0(projectFolder, "/g_intermediate/tmp/Fixed_", unique(diag_fixed_dm_so[,event_abbreviation]), "_", actual_tables$SURVEY_OBSERVATIONS[y], ".rds"))
                    
                    if("event_date" %in% names(df)){setnames(df,"event_date",date_var_fix)}
                    if("event_code" %in% names(df)){setnames(df,"event_code",col_keep)}
                    # Remove columns
                    df[, c("table", "variable_name", "keep", "checkbox_date") := NULL]
                    
                    
                    
                  }else{
                    df[, c("table", "event_abbreviation", "keep", "checkbox_date") := NULL]
                  }
                }
                
                print(paste0("Filtering data for Depression fixed columns:", actual_tables$SURVEY_OBSERVATIONS[y]))
                if(!is.null(diag_fixed_dep_so)){
                  same_names<-intersect(names_df,names(diag_fixed_dep_so))
                  df<-merge.data.table(df,diag_fixed_dep_so,by=same_names,all.x=T)
                  if(df[!is.na(event_abbreviation),.N]>0){
                    setnames(df, date_var_fix, "event_date")
                    setnames(df, "event_abbreviation", "variable_name")
                    col_keep<-unique(diag_fixed_dep_so[,keep])
                    setnames(df, col_keep, "event_code")
                    
                    saveRDS(df[!is.na(variable_name), c("person_id", "event_date", "variable_name", "event_code")], paste0(projectFolder, "/g_intermediate/tmp/Fixed_", unique(diag_fixed_dep_so[,event_abbreviation]), "_", actual_tables$SURVEY_OBSERVATIONS[y], ".rds"))
                    
                    if("event_date" %in% names(df)){setnames(df,"event_date",date_var_fix)}
                    if("event_code" %in% names(df)){setnames(df,"event_code",col_keep)}
                    # Remove columns
                    df[, c("table", "variable_name", "keep", "checkbox_date") := NULL]
                    
                    
                    
                  }else{
                    df[, c("table", "event_abbreviation", "keep", "checkbox_date") := NULL]
                  }
                }
                
                print(paste0("Filtering data for Parity fixed columns:", actual_tables$SURVEY_OBSERVATIONS[y]))
                if(!is.null(diag_fixed_parity_so)){
                  same_names<-intersect(names_df,names(diag_fixed_parity_so))
                  df<-merge.data.table(df,diag_fixed_parity_so,by=same_names,all.x=T)
                  if(df[!is.na(event_abbreviation),.N]>0){
                    setnames(df, date_var_fix, "event_date")
                    setnames(df, "event_abbreviation", "variable_name")
                    col_keep<-unique(diag_fixed_parity_so[,keep])
                    setnames(df, col_keep, "event_code")
                    
                    saveRDS(df[!is.na(variable_name), c("person_id", "event_date", "variable_name", "event_code")], paste0(projectFolder, "/g_intermediate/tmp/Fixed_", unique(diag_fixed_parity_so[,event_abbreviation]), "_", actual_tables$SURVEY_OBSERVATIONS[y], ".rds"))
                    
                    if("event_date" %in% names(df)){setnames(df,"event_date",date_var_fix)}
                    if("event_code" %in% names(df)){setnames(df,"event_code",col_keep)}
                    # Remove columns
                    df[, c("table", "variable_name", "keep", "checkbox_date") := NULL]
                    
                    
                    
                  }else{
                    df[, c("table", "event_abbreviation", "keep", "checkbox_date") := NULL]
                  }
                }
                
                print(paste0("Filtering data for Obesity fixed columns:", actual_tables$SURVEY_OBSERVATIONS[y]))
                names_df<-names(df)
                if(!is.null(diag_fixed_bmi_so)){
                  same_names<-intersect(names_df,names(diag_fixed_bmi_so))
                  df<-merge.data.table(df,diag_fixed_bmi_so,by=same_names,all.x=T)
                  if(df[!is.na(event_abbreviation),.N]>0){
                    setnames(df, date_var_fix, "event_date")
                    setnames(df, "event_abbreviation", "variable_name")
                    col_keep<-unique(diag_fixed_bmi_so[,keep])
                    setnames(df, col_keep, "event_code")
                    
                    saveRDS(df[!is.na(variable_name), c("person_id", "event_date", "variable_name", "event_code")], paste0(projectFolder, "/g_intermediate/tmp/Fixed_", unique(diag_fixed_bmi_so[,event_abbreviation]), "_", actual_tables$SURVEY_OBSERVATIONS[y], ".rds"))
                    
                    if("event_date" %in% names(df)){setnames(df,"event_date",date_var_fix)}
                    if("event_code" %in% names(df)){setnames(df,"event_code",col_keep)}
                    # Remove columns
                    df[, c("table", "variable_name", "keep", "checkbox_date") := NULL]
                    
                    
                    
                  }else{
                    df[, c("table", "event_abbreviation", "keep", "checkbox_date") := NULL]
                  }
                }
                
                print(paste0("Filtering data for Height fixed columns:", actual_tables$SURVEY_OBSERVATIONS[y]))
                names_df<-names(df)
                if(!is.null(diag_fixed_h_so)){
                  same_names<-intersect(names_df,names(diag_fixed_h_so))
                  df<-merge.data.table(df,diag_fixed_h_so,by=same_names,all.x=T)
                  if(df[!is.na(event_abbreviation),.N]>0){
                    setnames(df, date_var_fix, "event_date")
                    setnames(df, "event_abbreviation", "variable_name")
                    col_keep<-unique(diag_fixed_h_so[,keep])
                    setnames(df, col_keep, "event_code")
                    
                    saveRDS(df[!is.na(variable_name), c("person_id", "event_date", "variable_name", "event_code")], paste0(projectFolder, "/g_intermediate/tmp/Fixed_", unique(diag_fixed_h_so[,event_abbreviation]), "_", actual_tables$SURVEY_OBSERVATIONS[y], ".rds"))
                    
                    if("event_date" %in% names(df)){setnames(df,"event_date",date_var_fix)}
                    if("event_code" %in% names(df)){setnames(df,"event_code",col_keep)}
                    # Remove columns
                    df[, c("table", "variable_name", "keep", "checkbox_date") := NULL]
                    
                    
                    
                  }else{
                    df[, c("table", "event_abbreviation", "keep", "checkbox_date") := NULL]
                  }
                }
                
                print(paste0("Filtering data for Weight fixed columns:", actual_tables$SURVEY_OBSERVATIONS[y]))
                names_df<-names(df)
                if(!is.null(diag_fixed_w_so)){
                  same_names<-intersect(names_df,names(diag_fixed_w_so))
                  df<-merge.data.table(df,diag_fixed_w_so,by=same_names,all.x=T)
                  if(df[!is.na(event_abbreviation),.N]>0){
                    setnames(df, date_var_fix, "event_date")
                    setnames(df, "event_abbreviation", "variable_name")
                    col_keep<-unique(diag_fixed_w_so[,keep])
                    setnames(df, col_keep, "event_code")
                    
                    saveRDS(df[!is.na(variable_name), c("person_id", "event_date", "variable_name", "event_code")], paste0(projectFolder, "/g_intermediate/tmp/Fixed_", unique(diag_fixed_w_so[,event_abbreviation]), "_", actual_tables$SURVEY_OBSERVATIONS[y], ".rds"))
                    
                    if("event_date" %in% names(df)){setnames(df,"event_date",date_var_fix)}
                    if("event_code" %in% names(df)){setnames(df,"event_code",col_keep)}
                    # Remove columns
                    df[, c("table", "variable_name", "keep", "checkbox_date") := NULL]
                    
                    
                    
                  }else{
                    df[, c("table", "event_abbreviation", "keep", "checkbox_date") := NULL]
                  }
                }
                
              }
              
              
            }
            df[,keep_fix:=NULL]
            
              }
            }
          
            
            
          }
          
      
      w<-w+1
      rm(df)
      }
    }
    rm(hint_fl,obs_hint_table,cols_needed)
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
    empty_meaning_du<-sum(do.call(rbind,empty_meaning_du))
    any_study_no<-sum(do.call(rbind,any_study_no))
    outside_obs<-sum(do.call(rbind,outside_obs))
    prior_ev_rec_du<-sum(do.call(rbind,prior_ev_rec_du))
    after_ev_rec_du<-sum(do.call(rbind,after_ev_rec_du))
    
    empty_code_du_diag<-sum(do.call(rbind,empty_code_du_diag))
    empty_code_du_check<-sum(do.call(rbind,empty_code_du_check))
    empty_code_du_fix<-sum(do.call(rbind,empty_code_du_fix))
    
    empty_voc_du_diag<-sum(do.call(rbind,empty_voc_du_diag))
    empty_voc_du_check<-sum(do.call(rbind,empty_voc_du_check))
    empty_voc_du_fix<-sum(do.call(rbind,empty_voc_du_fix))
    
    
    pre_dt_du_diag<-sum(do.call(rbind,pre_dt_du_diag))
    pre_dt_du_check<-sum(do.call(rbind,pre_dt_du_check))
    pre_dt_du_fix<-sum(do.call(rbind,pre_dt_du_fix))
    
    #create flowchart and export to g_output
    flowchart_events_du<-data.table(Indicator=c("Number of original rows",
                                                "Number of records with missing event date",
                                                "Number of records with missing event meaning",
                                                "Number of records not present in the pregnancy D3 for the DU study",
                                                "Number of records outside min and max observation period for the DU study",
                                                "Number of records with event date before start of study date",
                                                "Number of records with event date after end of study date",
                                                "Number of records with missing event code for diagnostic information",
                                                "Number of records with missing event vocabulary for diagnostic information",
                                                "Number of records with missing event code for checkbox information",
                                                "Number of records with missing event vocabulary for checkbox information",
                                                "Number of records with missing event code for column information",
                                                "Number of records with missing event vocabulary for column information",
                                                "Number of records included, before data filtering for diagnostic information",
                                                "Number of records included, before data filtering for checkbox information",
                                                "Number of records included, before data filtering for column information"),
                                    SURVEY_OBSERVATIONS=c(original_rows_du,
                                                           empty_ev_date_du,
                                                          empty_meaning_du,
                                                          any_study_no,
                                                          outside_obs,
                                                          prior_ev_rec_du,
                                                          after_ev_rec_du,
                                                           empty_code_du_diag,
                                                           empty_voc_du_diag,
                                                          empty_code_du_check,
                                                          empty_voc_du_check,
                                                          empty_code_du_fix,
                                                          empty_voc_du_fix,
                                                           pre_dt_du_diag,
                                                          pre_dt_du_check,
                                                          pre_dt_du_fix))
    rm(original_rows_du,empty_ev_date_du,empty_meaning_du,any_study_no,outside_obs,prior_ev_rec_du,after_ev_rec_du,empty_code_du_diag,
       empty_voc_du_diag,empty_code_du_check,empty_voc_du_check,empty_code_du_fix,empty_voc_du_fix,pre_dt_du_diag,pre_dt_du_check,pre_dt_du_fix)
    fwrite(flowchart_events_du, paste0(projectFolder, "/g_output/Drug utilisation/flowchart/so_table_flowchart.csv"), row.names = F)
    
  } else {
    flowchart_events_du<-data.table(Indicator=c("Number of original rows",
                                                "Number of records with missing event date",
                                                "Number of records with missing event meaning",
                                                "Number of records not present in the pregnancy D3 for the DU study",
                                                "Number of records outside min and max observation period for the DU study",
                                                "Number of records with event date before start of study date",
                                                "Number of records with event date after end of study date",
                                                "Number of records with missing event code for diagnostic information",
                                                "Number of records with missing event vocabulary for diagnostic information",
                                                "Number of records with missing event code for checkbox information",
                                                "Number of records with missing event vocabulary for checkbox information",
                                                "Number of records with missing event code for column information",
                                                "Number of records with missing event vocabulary for column information",
                                                "Number of records included, before data filtering for diagnostic information",
                                                "Number of records included, before data filtering for checkbox information",
                                                "Number of records included, before data filtering for column information"),
                                    SURVEY_OBSERVATIONS=rep(0,16))
    fwrite(flowchart_events_du, paste0(projectFolder, "/g_output/Drug utilisation/flowchart/so_table_flowchart.csv"), row.names = F)
    
  }
}else{
  flowchart_events_du<-data.table(Indicator=c("Number of original rows",
                                              "Number of records with missing event date",
                                              "Number of records with missing event meaning",
                                              "Number of records not present in the pregnancy D3 for the DU study",
                                              "Number of records outside min and max observation period for the DU study",
                                              "Number of records with event date before start of study date",
                                              "Number of records with event date after end of study date",
                                              "Number of records with missing event code for diagnostic information",
                                              "Number of records with missing event vocabulary for diagnostic information",
                                              "Number of records with missing event code for checkbox information",
                                              "Number of records with missing event vocabulary for checkbox information",
                                              "Number of records with missing event code for column information",
                                              "Number of records with missing event vocabulary for column information",
                                              "Number of records included, before data filtering for diagnostic information",
                                              "Number of records included, before data filtering for checkbox information",
                                              "Number of records included, before data filtering for column information"),
                                  SURVEY_OBSERVATIONS=rep("N/A",16))
  fwrite(flowchart_events_du, paste0(projectFolder, "/g_output/Drug utilisation/flowchart/so_table_flowchart.csv"), row.names = F)
  
  
  
}
# Define the vector of variable names to potentially remove
vars_to_remove <- c("code_var", "voc_var", "date_var", "code_var_check", "voc_var_check", "date_var_check", "code_var_fix", "voc_var_fix", "date_var_fix")

# Check if variables exist and remove only those that do
rm(list = vars_to_remove[vars_to_remove %in% ls()])