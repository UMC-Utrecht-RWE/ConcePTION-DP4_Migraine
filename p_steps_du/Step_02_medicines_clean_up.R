#Clean up the medicines table to create the antidiabetic_medicines_D3

#Load the MEDICINES tables one by one
initial_time_02<-Sys.time()
date_running_start_02<-Sys.Date()

#Clean folders
unlink(paste0(projectFolder,"/g_intermediate/tmp"), recursive = T)#delete folder
dir.create(paste0(projectFolder, "/g_intermediate/tmp"))

if("raw_data" %in% list.files(paste0(projectFolder,"/g_intermediate/medicines_d3/"))){
  unlink(paste0(projectFolder,"/g_intermediate/medicines_d3/raw_data"), recursive = T)#delete folder 
  dir.create(paste0(projectFolder, "/g_intermediate/medicines_d3/raw_data"))
}else{
  dir.create(paste0(projectFolder, "/g_intermediate/medicines_d3/raw_data")) 
}
####load info, parameters, conceptsets####
source(paste0(pre_dir,"info/directory_info.R"))
source(paste0(pre_dir,"info/DAP_info.R"))
source(paste0(pre_dir,"parameters/parameters_metadata.R"))
source(paste0(pre_dir,"parameters/study_parameters.R"))

#create date flowcharts
Indication<-c("start_coverage", "end_coverage", "minimum_start_pregnancy_date", "maxiumum_start_pregnancy_date", "lookback_period", "after_delivery")
#Migraine<-c(as.character(mig_start_study_date),as.character(mig_end_study_date),as.character(min_preg_date_du),as.character(max_preg_date_du), as.character(opt_lookback_duraine), as.character(after_delivery_duraine))
Drug_utilisation<-c(as.character(du_start_study_date),as.character(du_end_study_date),as.character(min_preg_date_du),as.character(max_preg_date_du), as.character(opt_lookback_du), as.character(after_delivery_du))
#Safety<-c(as.character(saf_start_study_date),as.character(saf_end_study_date),as.character(min_preg_date_saf),as.character(max_preg_date_saf), as.character(opt_lookback_saf), as.character(after_delivery_saf))

dates_flowchart<-data.table(Indication,Drug_utilisation)
fwrite(dates_flowchart,paste0(output_dir, "Drug utilisation/flowchart/inclusion_dates_flowchart.csv"), row.names = F)
rm(dates_flowchart)

####Load the codelist####
du_med_codelist<-fread(paste0(pre_dir,"codelists/", list.files(paste0(pre_dir,"codelists/"),"du_medications_codelist")))

du_start<-du_med_codelist[mechanism=="start"]
du_start[,condition:="DU_medicines"]
du_exact<-du_med_codelist[mechanism=="exact"]
du_exact[,condition:="DU_medicines"]

#Clean up data: If exact with is part of start with, remove it from the list

#Migraine
du_start[,remove:=1]
du_start[,check:=atc_code]
for (du_start_ind in 1:length(du_start)){
  length_var<-nchar(du_start[du_start_ind,atc_code])
  du_exact[,check:=substr(atc_code,1,length_var)]
  du_exact<-merge.data.table(du_exact,du_start[,c("check","remove")], all.x = T, by="check")
  du_exact<-du_exact[is.na(remove)]
  du_exact[,remove:=NULL][,check:=NULL]
}
du_start[,remove:=NULL][,check:=NULL]

start_med<-du_start[,c("atc_code","mechanism","condition")]
start_med<-start_med[!duplicated(atc_code)]
exact_med<-du_exact[,c("atc_code","mechanism","condition")]
exact_med<-exact_med[!duplicated(atc_code)]


#group start_med into different number of characters
start_med[,nchar:=nchar(atc_code)]
#Available groups:A for 3 characters, B for 4, C for 5, D for 6
start_med[,group:=as.character()]
start_med[nchar==3, group:="A"]
start_med[nchar==4, group:="B"]
start_med[nchar==5, group:="C"]
start_med[nchar==6, group:="D"]
setnames(start_med,"atc_code","truncated_atc_code")

#remove exact codes if they are already present in the start with codes
dif_levels<-start_med[!duplicated(nchar),nchar]

list<-list()
ind_list<-1
for(i in 1:length(dif_levels)){
  level_1_exact<-substr(exact_med[!duplicated(atc_code),atc_code],1,dif_levels[i])
  level_1_exact<-data.table(atc_code=exact_med[!duplicated(atc_code),atc_code],truncated_atc_code=level_1_exact, remove=1)
  start_med_2<-merge.data.table(start_med,level_1_exact,by="truncated_atc_code",all.x=T)
  list[[ind_list]]<-unique(start_med_2[!is.na(atc_code) & remove==1,atc_code])
  rm(level_1_exact,start_med_2)
  ind_list<-ind_list+1
}
list<-do.call(c,list)
#update start med
exact_med<-exact_med[!atc_code %in% list]
rm(list,ind_list)

#####The tables that will be search for medicines: MEDICINES####
#The information about Migraine diagnoses is saved in 
#codesheet_medicines_du
med_du_medicines<-ifelse("MEDICINES" %in% names(actual_tables),1,0)


if(length(med_du_medicines)>0){
  
    if("code" %in% codesheet_medicines_du[table=="MEDICINES",val_1]){
  code_var<-codesheet_medicines_du[table=="MEDICINES",col_1]
  date_var1<-codesheet_medicines_du[table=="MEDICINES",date_column]
  date_var2<-codesheet_medicines_du[table=="MEDICINES",date_column2]
}
    if(!is.na(date_var1) & is.na(date_var2)){date_var<-date_var1}
    if(is.na(date_var1) & !is.na(date_var2)){date_var<-date_var2}
    if(!is.na(date_var1) & !is.na(date_var2)){date_var<-"both"}

    ####Main script####
  if(length(actual_tables$MEDICINES)>0){
    #Load the observation periods hint table(use to make smaller the table)
    hint_fl<-list.files(paste0(projectFolder, "/g_intermediate/pregnancy_d3/"), "obs_period_hint_DU.rds")
    #use this table first to remove all uneccessary subjects(not needed for any of the studies)
    obs_hint_table<-readRDS(paste0(projectFolder, "/g_intermediate/pregnancy_d3/", hint_fl))
    print("Analyse MEDICINES table.")
    ####List for saving info####
    print("Creating lists to save the information.")
  original_rows_du<-list() #number of table original rows
  empty_med_date_du<-list() #number of records with missing medicine date
  empty_atc_code_du<-list() #numer of records with missing atc code
  lower_atc_du<-list() #numer of records with atc code < 3 digits
  empty_med_meaning_du<-list() #numer of records with missing med meaning
  any_study_no<-list() #number of records for subjects not needed in any of the SAPs
  outside_obs<-list() #number of records outside obs min and obs max between SAPs for each subject
  prior_med_rec_du<-list() #number of records with date prior to start study date
  after_med_rec_du<-list() #number of records with date after end study date
  pre_dt_du<-list()
  w<-1
  ####Loop####
  for (med_tab_ind in 1:length(actual_tables$MEDICINES)){
    print(paste0("Analyzing table ",actual_tables$MEDICINES[med_tab_ind], "."))
    
    if(date_var=="both"){
      cols<-c("person_id", code_var,"date_prescription","date_dispensing", "meaning_of_drug_record", "medicinal_product_id")
    }else{
      cols<-c("person_id", code_var, date_var, "meaning_of_drug_record","medicinal_product_id")  
    }
    
    df<-fread(paste(path_dir, actual_tables$MEDICINES[med_tab_ind], sep=""), stringsAsFactors = FALSE, colClasses = "character", select = cols)
    
    df<-df[,cols, with=F]
    df<-df[, lapply(.SD, FUN=function(x) gsub("^$|^ $", NA, x))] #make sure missing data is read appropriately
    #create date variable
    if(date_var=="both"){
      df[,medicine_date:=date_prescription]
      df[!is.na(date_dispensing), medicine_date:=date_dispensing]
      df[,date_dispensing:=NULL][,date_prescription:=NULL]
    }else{
      setnames(df, date_var,"medicine_date") 
    }
    setnames(df,"meaning_of_drug_record","meaning")
    setnames(df, code_var,"atc_code")
   
    #number of original rows
    original_rows_du[[w]]<-df[,.N]
    #records with empty medicines date
    empty_med_date_du[[w]]<-df[is.na(medicine_date),.N]
    df<-df[!is.na(medicine_date)]
    #remove records with empty atc codes
    empty_atc_code_du[[w]]<-df[is.na(atc_code),.N]
    df<-df[!is.na(atc_code)]
    #remove records with atc<3 digits
    df[,nchar:=nchar(atc_code)]
    lower_atc_du[[w]]<-df[nchar<3,.N]
    df<-df[nchar>=3]
    df[,nchar:=NULL]
    #empty meaning
    empty_med_meaning_du[[w]]<-df[is.na(meaning),.N]
    df<-df[!is.na(meaning)]
    
    #transform into date variables
    # df[,medicine_date:=as.IDate(medicine_date,"%Y%m%d")] #transform to date variables
    df[,medicine_date:=ymd(medicine_date)] #transform to date variables
    #create year variable
    df[,year:=year(medicine_date)]
    #merge with the obs_hint table and remove subjects not present in the pregnancy d3 for any of teh studies
    setkey(df, person_id)
    setkey(obs_hint_table, person_id)
    df<-merge.data.table(df, obs_hint_table, by="person_id", all.x = T)
    any_study_no[[w]]<-df[is.na(obs_min), .N]
    df<-df[!is.na(obs_min)]
    if(df[,.N]>0){
      #Remove all records outside the window for observation for each subject
      df[medicine_date>=obs_min & medicine_date<=obs_max, keep:=1]
      outside_obs[[w]]<-df[is.na(keep),.N]
      df<-df[keep==1]
      if(df[,.N]>0){
    #remove all records with dates prior to start study date or start study date plus lookback if any
    #migraine
    df[,prior_du:=ifelse(du_start_study_date>medicine_date,1,0)]
    prior_med_rec_du[[w]]<-df[prior_du==1,.N]
    #clean up dataset:keep records when at least one is zero
    df<-df[prior_du==0]
    #remove all records with dates after end study date
    #migraine
    df[,after_du:=ifelse(du_end_study_date<medicine_date,1,0)]
    after_med_rec_du[[w]]<-df[after_du==1 & prior_du==0,.N]
    #Check if the data needed for the drug utilization and safety study has a longer follow up
    df[,after_du:=ifelse(du_end_study_date<medicine_date,1,0)]
    #clean up dataset:keep records when at least one is zero
    df<-df[after_du==0]
    #incl rec
    pre_dt_du[[w]]<-df[prior_du==0 & after_du==0,.N]

    if(df[,.N]>0){
    #Apply the exact match codes
    #merge with the medicines codelist
     setkey(exact_med, atc_code)
     setkey(df, atc_code)
    df<-merge.data.table(df, exact_med, by="atc_code", all.x=T, allow.cartesian = T)
    df[condition=="DU_medicines" & prior_du==0 & after_du==0,include_du:=1]
    # setnames(df,"medicinal_product_group","medicinal_product_group_start")
    # setnames(df,"condition","condition_start")
    cols_to_incl<-c("person_id","medicine_date","year","atc_code", "meaning", "condition","medicinal_product_id")
    
    if(df[include_du==1,.N]>0){
      saveRDS(df[condition=="DU_medicines",cols_to_incl,with=F], paste0(tmp, "DU_medicines_exact_", med_tab_ind,".rds"))
    }
    df[,include_du:=NULL]
    
    # print(paste0("Analyze: ", actual_tables$MEDICINES[med_tab_ind], " for GDM exact match filtering."))
    # #export all gdm codes by year
    # year_gdm<-df[condition=="GDM"][!duplicated(year), year]
    # if(length(year_gdm)>0){
    #   for (year_ind in 1:length(year_gdm)){
    #   #export the medicines file to the g_intermediate/gdm_algorithm folder
    #   saveRDS(df[condition=="GDM" & year==year_gdm[year_ind]], paste0(g_intermediate,"gdm_algorithm/", year_gdm[year_ind], "_GDM_exact_med_", med_tab_ind,".rds"))
    #   }
    # }
    # rm(year_gdm)
    
    # print(paste0("Analyze: ", actual_tables$MEDICINES[med_tab_ind], " for Migraine exact match filtering."))
    # 
    # year_du<-df[condition=="Migraine"][!duplicated(year), year]
    # if(length(year_du)>0){
    #   for (year_ind in 1:length(year_du)){
    #     #export the medicines file to the g_intermediate/gdm_algorithm folder
    #     saveRDS(df[condition=="Migraine" & year==year_du[year_ind]], paste0(g_intermediate,"migraine_algorithm/",year_du[year_ind], "_duraine_exact_med_", med_tab_ind,".rds"))
    #   }
    # }
    
    #create truncated atc code that will be used for filtering
    #df[,atc_code:=substr(get(code_var),1,4)]
    
    #remove uneccessary columns
    df[,mechanism:=NULL][,condition:=NULL]
    
    #Apply start with codes
    
    start_med_group<-start_med[!duplicated(group), group]
    for (group_ind in 1:length(start_med_group)){
      length<-start_med[group==start_med_group[group_ind]][!duplicated(nchar),nchar]
      df[,truncated_atc_code:=substr(atc_code,1,length)]
      setkey(df, truncated_atc_code)
      df<-merge.data.table(df, start_med[group==start_med_group[group_ind]], by="truncated_atc_code", all.x=T, allow.cartesian = T)
      #df[condition=="GDM_medicines" & prior_gdm_pe==0 & after_gdm_pe==0 & !is.na(include_gdm), include_gdm:="3_gdm"]#when the record is retrieved both from start and exact match

      
      #df[condition=="Migraine_medicines" & prior_du==0 & after_du==0 & !is.na(include_du),include_du:="3_du"]#when the record is retrieved both from start and exact match
      if("include_du" %in% names(df)){df[is.na(condition) & include_du==1,include_du:=NA]}else{df[,include_du:=NA]}
      df[condition=="DU_medicines" & prior_du==0 & after_du==0 & is.na(include_du),include_du:=1]#when the record is retrieved only from start match
      #1_du means it was only retrieved by exact match
      
      #Add missing info
      df[!is.na(include_du) & is.na(condition),condition:="DU_medicines"]

      #df[,condition_start:=NULL][,medicinal_product_group_start:=NULL][,group:=NULL][,mechanism:=NULL][,nchar:=NULL]
      cols_to_incl<-c("person_id","medicine_date","year","atc_code", "meaning", "condition","medicinal_product_id")


      if(df[include_du==1,.N]>0){
        saveRDS(df[condition=="DU_medicines",cols_to_incl,with=F], paste0(tmp, "DU_medicines_", med_tab_ind, start_med_group[group_ind],".rds"))
       }
      
      #print(paste0("Analyze: ", actual_tables$MEDICINES[med_tab_ind], " for Migraine group ", start_med_group[group_ind], " match filtering."))
      
      # year_du<-df[condition=="Migraine"][!duplicated(year), year]
      # if(length(year_du)>0){
      #   for (year_ind in 1:length(year_du)){
      #     #export the medicines file to the g_intermediate/gdm_algorithm folder
      #     saveRDS(df[,cols_to_incl,with=F][condition=="Migraine" & year==year_du[year_ind]], paste0(g_intermediate,"migraine_algorithm/",year_du[year_ind], "_duraine_med_", med_tab_ind,".rds"))
      #   }
      # }
      # 
      
      # if(df[condition=="GDM",.N]>0){
      #   #create a short summary of the included records
      #   med_summary_gdm_fl<-df[condition=="GDM",.N, by=list(meaning,medicinal_product_group, atc_code,year)]
      #   setnames(med_summary_gdm_fl, "N", "no_records")
      #   med_summary_gdm[[index]]<-med_summary_gdm_fl
      #   rm(med_summary_gdm_fl)
      # }
      # 
      # if(df[condition=="Migraine",.N]>0){
      #   #create a short summary of the included records
      #   med_summary_duraine_fl<-df[condition=="Migraine",.N, by=list(meaning,medicinal_product_group, atc_code,year)]
      #   setnames(med_summary_duraine_fl, "N", "no_records")
      #   med_summary_duraine[[index]]<-med_summary_duraine_fl
      #   rm(med_summary_duraine_fl)
      # }
      # if(df[condition=="GDM",.N]>0){
      # #create a short summary of the included records
      # med_summary_gdm_start_fl<-df[condition=="GDM",.N, by=list(meaning,medicinal_product_group, truncated_atc_code,year)]
      # setnames(med_summary_gdm_start_fl, "N", "no_records")
      # med_summary_gdm_start[[index]]<-med_summary_gdm_start_fl
      # rm(med_summary_gdm_start_fl)
      # }
      # 
      # if(df[condition=="Migraine",.N]>0){
      # #create a short summary of the included records
      # med_summary_duraine_start_fl<-df[condition=="Migraine",.N, by=list(meaning,medicinal_product_group, truncated_atc_code,year)]
      # setnames(med_summary_duraine_start_fl, "N", "no_records")
      # med_summary_duraine_start[[index]]<-med_summary_duraine_start_fl
      # rm(med_summary_duraine_start_fl)
      # }
      df[,truncated_atc_code:=NULL][,mechanism:=NULL][,condition:=NULL][,nchar:=NULL][,group:=NULL]
       rm(length)
    }
    }
      }
    }
    w<-w+1
    rm(df)
  }
  ####Flowchart####
  #flowchart
  #Combine all data and export to g_output
  original_rows_du<-sum(do.call(rbind,original_rows_du))
  empty_med_date_du<-sum(do.call(rbind,empty_med_date_du))
  empty_atc_code_du<-sum(do.call(rbind,empty_atc_code_du))
  lower_atc_du<-sum(do.call(rbind,lower_atc_du))
  empty_med_meaning_du<-sum(do.call(rbind,empty_med_meaning_du))
  any_study_no<-sum(do.call(rbind,any_study_no))
  outside_obs<-sum(do.call(rbind,outside_obs))
  prior_med_rec_du<-sum(do.call(rbind,prior_med_rec_du))
  after_med_rec_du<-sum(do.call(rbind,after_med_rec_du))
  pre_dt_du<-sum(do.call(rbind,pre_dt_du))
  

  flowchart_medicines_du<-data.table(Indicator=c("Number of original records",
                                                     "Number of records with empty medicine date",
                                                     "Number of records with empty ATC codes",
                                                     "Number of records with ATC code shorter than 3 characters",
                                                     "Number of records with missing meaning(source of record)",
                                                  "Number of records not present in the pregnancy D3 for any of the studies",
                                                  "Number of records outside min and max observation period for any of the studies",
                                                     "Number of records with medicine date before start of study date",
                                                     "Number of records with medicine date after end of study date",
                                                     "Number of records included, before data filtering"),
                                         Drug_utilisation=c(original_rows_du,
                                                    empty_med_date_du,
                                                    empty_atc_code_du,
                                                    lower_atc_du,
                                                    empty_med_meaning_du,
                                                    any_study_no,
                                                    outside_obs,
                                                    prior_med_rec_du,
                                                    after_med_rec_du,
                                                    pre_dt_du))
  
  
  rm(original_rows_du,empty_med_date_du,lower_atc_du,empty_med_meaning_du,prior_med_rec_du,after_med_rec_du,pre_dt_du)
  rm(any_study_no, outside_obs)
  
  
  }else{
    flowchart_medicines_du<-data.table(Indicator=c("Number of original records",
                                                    "Number of records with empty medicine date",
                                                    "Number of records with empty ATC codes",
                                                    "Number of records with ATC code shorter than 3 characters",
                                                    "Number of records with missing meaning(source of record)",
                                                    "Number of records not present in the pregnancy D3 for any of the studies",
                                                    "Number of records outside min and max observation period for any of the studies",
                                                    "Number of records with medicine date before start of study date",
                                                    "Number of records with medicine date after end of study date",
                                                    "Number of records included, before data filtering"),
                                       Drug_utilisation=c(0,
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

  flowchart_medicines_du<-data.table(Indicator=c("Number of original records",
                                                  "Number of records with empty medicine date",
                                                  "Number of records with empty ATC codes",
                                                  "Number of records with ATC code shorter than 3 characters",
                                                  "Number of records with missing meaning(source of record)",
                                                  "Number of records not present in the pregnancy D3 for any of the studies",
                                                  "Number of records outside min and max observation period for any of the studies",
                                                  "Number of records with medicine date before start of study date",
                                                  "Number of records with medicine date after end of study date",
                                                  "Number of records included, before data filtering"),
                                      Drug_utilisation=c(0,
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

fwrite(flowchart_medicines_du, paste0(projectFolder,"/g_output/Drug utilisation/flowchart/Step_02_flowchart_medicines_du.csv"),row.names = F)
rm(flowchart_medicines_du)

date_running_end_02<-Sys.Date()
end_time_02<-Sys.time()

time_log_02<-data.table(DAP=data_access_provider_name,
                     Script="Step_02_medicines_clean_up.R", 
                     Start_date=date_running_start_02, 
                     End_date=date_running_end_02,
                     Time_elaspsed=format(end_time_02-initial_time_02, digits=2))
fwrite(time_log_02,paste0(output_dir,"/Time log/DU/Step_02_time_log.csv"),row.names = F)

