#Clean up the medicines table to create the antidiabetic_medicines_D3

#Load the MEDICINES tables one by one and keep only antidiabetic medications as explained in antidiabetic_medications_codelist
initial_time_03<-Sys.time()
date_running_start_03<-Sys.Date()

#Clean folders
unlink(paste0(projectFolder,"/g_intermediate/tmp"), recursive = T)#delete folder
dir.create(paste0(projectFolder, "/g_intermediate/tmp"))


####load info, parameters, conceptsets####
source(paste0(pre_dir,"info/directory_info.R"))
source(paste0(pre_dir,"info/DAP_info.R"))
source(paste0(pre_dir,"parameters/parameters_metadata.R"))
source(paste0(pre_dir,"parameters/study_parameters.R"))

#create date flowcharts
Indication<-c("start_coverage", "end_coverage", "minimum_start_pregnancy_date", "maxiumum_start_pregnancy_date", "lookback_period", "after_delivery")
GDM_and_PE<-c(as.character(gdm_pe_start_study_date),as.character(gdm_pe_end_study_date),as.character(min_preg_date_gdm_pe),as.character(max_preg_date_gdm_pe), as.character(opt_lookback_gdm_pe), as.character(after_delivery_gdm_pe))
Migraine<-c(as.character(mig_start_study_date),as.character(mig_end_study_date),as.character(min_preg_date_mig),as.character(max_preg_date_mig), as.character(opt_lookback_migraine), as.character(after_delivery_migraine))
Drug_utilisation<-c(as.character(du_start_study_date),as.character(du_end_study_date),as.character(min_preg_date_du),as.character(max_preg_date_du), as.character(opt_lookback_du), as.character(after_delivery_du))
Safety<-c(as.character(saf_start_study_date),as.character(saf_end_study_date),as.character(min_preg_date_saf),as.character(max_preg_date_saf), as.character(opt_lookback_saf), as.character(after_delivery_saf))

dates_flowchart<-data.table(Indication,GDM_and_PE,Migraine,Drug_utilisation,Safety)
fwrite(dates_flowchart,paste0(output_dir, "PE and GDM algorithm/inclusion_dates_flowchart.csv"), row.names = F)
fwrite(dates_flowchart,paste0(output_dir, "Migraine algorithm/inclusion_dates_flowchart.csv"), row.names = F)
rm(dates_flowchart)

####Load the codelist####
gdm_med_codelist<-fread(paste0(pre_dir,"codelists/", list.files(paste0(pre_dir,"codelists/"),"antidiabetic_medications_codelist")))
migraine_med_codelist<-fread(paste0(pre_dir,"codelists/", list.files(paste0(pre_dir,"codelists/"),"migraine_medications_codelist")))

#Separate codelist into start and exact match
gdm_start<-gdm_med_codelist[mechanism=="start"]
gdm_start[,condition:="GDM_medicines"]
gdm_exact<-gdm_med_codelist[mechanism=="exact"]
gdm_exact[,condition:="GDM_medicines"]
migraine_start<-migraine_med_codelist[mechanism=="start"]
migraine_start[,condition:="Migraine_medicines"]
migraine_exact<-migraine_med_codelist[mechanism=="exact"]
migraine_exact[,condition:="Migraine_medicines"]

#Clean up data: If exact with is part of start with, remove it from the list
#GDM
gdm_start[,remove:=1]
gdm_start[,check:=atc_code]
for (gdm_start_ind in 1:length(gdm_start)){
  length_var<-nchar(gdm_start[gdm_start_ind,atc_code])
  gdm_exact[,check:=substr(atc_code,1,length_var)]
  gdm_exact<-merge.data.table(gdm_exact,gdm_start[,c("check","remove")], all.x = T, by="check")
  gdm_exact<-gdm_exact[is.na(remove)]
  gdm_exact[,remove:=NULL][,check:=NULL]
}
gdm_start[,remove:=NULL][,check:=NULL]

#Migraine
migraine_start[,remove:=1]
migraine_start[,check:=atc_code]
for (migraine_start_ind in 1:length(migraine_start)){
  length_var<-nchar(migraine_start[migraine_start_ind,atc_code])
  migraine_exact[,check:=substr(atc_code,1,length_var)]
  migraine_exact<-merge.data.table(migraine_exact,migraine_start[,c("check","remove")], all.x = T, by="check")
  migraine_exact<-migraine_exact[is.na(remove)]
  migraine_exact[,remove:=NULL][,check:=NULL]
}
migraine_start[,remove:=NULL][,check:=NULL]

gdm_start<-gdm_start[,c("medicinal_product_group","atc_code","mechanism","condition")]
migraine_start<-migraine_start[,c("medicinal_product_group","atc_code","mechanism","condition")]
start_med<-rbind(gdm_start,migraine_start)
rm(gdm_start,migraine_start)
gdm_exact<-gdm_exact[,c("medicinal_product_group","atc_code","mechanism","condition")]
migraine_exact<-migraine_exact[,c("medicinal_product_group","atc_code","mechanism","condition")]
exact_med<-rbind(gdm_exact,migraine_exact)
rm(gdm_exact,migraine_exact)
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
####Check for CDM table availability####
source(paste0(pre_dir, "info/directory_info.R"))

#####The tables that will be search for medicines: MEDICINES####
#The information about GDM medicines is saved in 
#codesheet_medicines_gdm
#The information about Migraine diagnoses is saved in 
#codesheet_medicines_migraine
med_gdm_medicines<-ifelse("MEDICINES" %in% codesheet_medicines_gdm[,table],1,0)

med_migraine_medicines<-ifelse("MEDICINES" %in% codesheet_medicines_migraine[,table],1,0)

if(sum(length(med_gdm_medicines),length(med_migraine_medicines))>0){
    if("code" %in% codesheet_medicines_gdm[table=="MEDICINES",val_1]){
  code_var<-codesheet_medicines_gdm[table=="MEDICINES",col_1]
  date_var1<-codesheet_medicines_gdm[table=="MEDICINES",date_column]
  date_var2<-codesheet_medicines_gdm[table=="MEDICINES",date_column2]
}
    if("code" %in% codesheet_medicines_migraine[table=="MEDICINES",val_1]){
  code_var<-codesheet_medicines_migraine[table=="MEDICINES",col_1]
  date_var1<-codesheet_medicines_migraine[table=="MEDICINES",date_column]
  date_var2<-codesheet_medicines_migraine[table=="MEDICINES",date_column2]
}
    if(!is.na(date_var1) & is.na(date_var2)){date_var<-date_var1}
    if(is.na(date_var1) & !is.na(date_var2)){date_var<-date_var2}
    if(!is.na(date_var1) & !is.na(date_var2)){date_var<-"both"}

    ####Main script####
  if(length(actual_tables$MEDICINES)>0){
    #Load the observation periods hint table(use to make smaller the table)
    hint_fl<-list.files(paste0(projectFolder, "/g_intermediate/pregnancy_d3/"), "obs_period_hint")
    #use this table first to remove all uneccessary subjects(not needed for any of the studies)
    obs_hint_table<-readRDS(paste0(projectFolder, "/g_intermediate/pregnancy_d3/", hint_fl))
    print("Analyse MEDICINES table.")
    ####List for saving info####
    print("Creating lists to save the information.")
  original_rows_gdm_pe<-list() #number of table original rows
  original_rows_mig<-list() #number of table original rows
  empty_med_date_gdm_pe<-list() #number of records with missing medicine date
  empty_med_date_mig<-list() #number of records with missing medicine date
  empty_atc_code_gdm_pe<-list() #numer of records with missing atc code
  empty_atc_code_mig<-list() #numer of records with missing atc code
  lower_atc_gdm_pe<-list() #numer of records with atc code < 3 digits
  lower_atc_mig<-list() #numer of records with atc code < 3 digits
  empty_med_meaning_gdm_pe<-list() #numer of records with missing med meaning
  empty_med_meaning_mig<-list() #numer of records with missing med meaning
  any_study_no<-list() #number of records for subjects not needed in any of the SAPs
  outside_obs<-list() #number of records outside obs min and obs max between SAPs for each subject
  prior_med_rec_gdm_pe<-list() #number of records with date prior to start study date
  prior_med_rec_mig<-list() #number of records with date prior to start study date
  after_med_rec_gdm_pe<-list() #number of records with date after end study date
  after_med_rec_mig<-list() #number of records with date after end study date
  pre_dt_gdm_pe<-list()
  pre_dt_mig<-list()
  w<-1
  ####Loop####
  for (med_tab_ind in 1:length(actual_tables$MEDICINES)){
    print(paste0("Analyzing table ",actual_tables$MEDICINES[med_tab_ind], "."))
    df<-fread(paste(path_dir, actual_tables$MEDICINES[med_tab_ind], sep=""), stringsAsFactors = FALSE, colClasses = "character")
    if(date_var=="both"){
    cols<-c("person_id", code_var,"date_prescription","date_dispensing", "meaning_of_drug_record", "medicinal_product_id")
    }else{
      cols<-c("person_id", code_var, date_var, "meaning_of_drug_record","medicinal_product_id")  
    }
    df<-df[,cols, with=F]
    df<-df[, lapply(.SD, FUN=function(x) gsub("^$|^ $", NA, x))] #make sure missing data is read appropriately
    #create date variable
    if(date_var=="both"){
      df[,medicine_date:=date_prescription]
      df[!is.na(date_dispensing), medicine_date:=date_dispensing]
      df[,date_prescription:=NULL][,date_prescription:=NULL]
    }else{
      setnames(df, date_var,"medicine_date") 
    }
    setnames(df,"meaning_of_drug_record","meaning")
    setnames(df, code_var,"atc_code")
   
    #number of original rows
    original_rows_gdm_pe[[w]]<-df[,.N]
    original_rows_mig[[w]]<-df[,.N]
    #records with empty medicines date
    empty_med_date_gdm_pe[[w]]<-df[is.na(medicine_date),.N]
    empty_med_date_mig[[w]]<-df[is.na(medicine_date),.N]
    df<-df[!is.na(medicine_date)]
    #remove records with empty atc codes
    empty_atc_code_gdm_pe[[w]]<-df[is.na(atc_code),.N]
    empty_atc_code_mig[[w]]<-df[is.na(atc_code),.N]
    df<-df[!is.na(atc_code)]
    #remove records with atc<3 digits
    df[,nchar:=nchar(atc_code)]
    lower_atc_gdm_pe[[w]]<-df[nchar<3,.N]
    lower_atc_mig[[w]]<-df[nchar<3,.N]
    df<-df[nchar>=3]
    df[,nchar:=NULL]
    #empty meaning
    empty_med_meaning_gdm_pe[[w]]<-df[is.na(meaning),.N]
    empty_med_meaning_mig[[w]]<-df[is.na(meaning),.N]
    df<-df[!is.na(meaning)]
    
    #transform into date variables
    df[,medicine_date:=as.IDate(medicine_date,"%Y%m%d")] #transform to date variables
    #create year variable
    df[,year:=year(medicine_date)]
    #merge with the obs_hint table and remove subjects not present in the pregnancy d3 for any of teh studies
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
    #gdm&pe
    df[,prior_gdm_pe:=ifelse(gdm_pe_start_study_date>medicine_date,1,0)]
    prior_med_rec_gdm_pe[[w]]<-df[prior_gdm_pe==1,.N]
    #migraine
    df[,prior_mig:=ifelse(mig_start_study_date>medicine_date,1,0)]
    prior_med_rec_mig[[w]]<-df[prior_mig==1,.N]
    #Check if the data needed for the drug utilization and safety study has a longer follow up
    df[,prior_du:=ifelse(du_start_study_date>medicine_date,1,0)]
    df[,prior_saf:=ifelse(saf_start_study_date>medicine_date,1,0)]
    #clean up dataset:keep records when at least one is zero
    df<-df[prior_gdm_pe==0|prior_mig==0|prior_du==0|prior_saf==0]
    #remove all records with dates after end study date
    #gdm
    df[,after_gdm_pe:=ifelse(gdm_pe_end_study_date<medicine_date,1,0)]
    after_med_rec_gdm_pe[[w]]<-df[after_gdm_pe==1 & prior_gdm_pe==0,.N]
    #migraine
    df[,after_mig:=ifelse(mig_end_study_date<medicine_date,1,0)]
    after_med_rec_mig[[w]]<-df[after_mig==1 & prior_mig==0,.N]
    #Check if the data needed for the drug utilization and safety study has a longer follow up
    df[,after_du:=ifelse(du_end_study_date<medicine_date,1,0)]
    df[,after_saf:=ifelse(saf_end_study_date<medicine_date,1,0)]
    #clean up dataset:keep records when at least one is zero
    df<-df[after_gdm_pe==0|after_mig==0|after_du==0|after_saf==0]
    #incl rec
    pre_dt_gdm_pe[[w]]<-df[prior_gdm_pe==0 & after_gdm_pe==0,.N]
    pre_dt_mig[[w]]<-df[prior_mig==0 & after_mig==0,.N]

    if(df[,.N]>0){
    #Apply the exact match codes
    #merge with the medicines codelist
    df<-merge.data.table(df, exact_med, by="atc_code", all.x=T, allow.cartesian = T)
    df[condition=="GDM_medicines" & prior_gdm_pe==0 & after_gdm_pe==0,include_gdm:=1]
    df[condition=="Migraine_medicines" & prior_mig==0 & after_mig==0,include_mig:=1]
    # setnames(df,"medicinal_product_group","medicinal_product_group_start")
    # setnames(df,"condition","condition_start")
    cols_to_incl<-c("person_id","medicine_date","year","atc_code", "meaning", "condition","medicinal_product_group","medicinal_product_id")
    
    if(df[include_gdm==1,.N]>0){
      saveRDS(df[condition=="GDM_medicines",cols_to_incl,with=F], paste0(tmp, "GDM_medicines_exact_", med_tab_ind,".rds"))
    }
    df[,include_gdm:=NULL]
    
    if(df[include_mig==1,.N]>0){
      saveRDS(df[condition=="Migraine_medicines",cols_to_incl,with=F], paste0(tmp, "Migraine_medicines_exact_", med_tab_ind,".rds"))
    }
    df[,include_mig:=NULL]
    
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
    # year_mig<-df[condition=="Migraine"][!duplicated(year), year]
    # if(length(year_mig)>0){
    #   for (year_ind in 1:length(year_mig)){
    #     #export the medicines file to the g_intermediate/gdm_algorithm folder
    #     saveRDS(df[condition=="Migraine" & year==year_mig[year_ind]], paste0(g_intermediate,"migraine_algorithm/",year_mig[year_ind], "_Migraine_exact_med_", med_tab_ind,".rds"))
    #   }
    # }
    
    #create truncated atc code that will be used for filtering
    #df[,atc_code:=substr(get(code_var),1,4)]
    
    #remove uneccessary columns
    df[,mechanism:=NULL][,medicinal_product_group:=NULL][,condition:=NULL]
    
    #Apply start with codes
    
    start_med_group<-start_med[!duplicated(group), group]
    for (group_ind in 1:length(start_med_group)){
      length<-start_med[group==start_med_group[group_ind]][!duplicated(nchar),nchar]
      df[,truncated_atc_code:=substr(atc_code,1,length)]
      df<-merge.data.table(df, start_med[group==start_med_group[group_ind]], by="truncated_atc_code", all.x=T, allow.cartesian = T)
      #df[condition=="GDM_medicines" & prior_gdm_pe==0 & after_gdm_pe==0 & !is.na(include_gdm), include_gdm:="3_gdm"]#when the record is retrieved both from start and exact match
      if("include_gdm" %in% names(df)){df[is.na(condition) & include_gdm==1,include_gdm:=NA]}else{df[,include_gdm:=NA]}
      df[condition=="GDM_medicines" & prior_gdm_pe==0 & after_gdm_pe==0 & is.na(include_gdm), include_gdm:=1]#when the record is retrieved only from start match
      #1_gdm means it was only retrieved by exact match
      
      #df[condition=="Migraine_medicines" & prior_mig==0 & after_mig==0 & !is.na(include_mig),include_mig:="3_mig"]#when the record is retrieved both from start and exact match
      if("include_mig" %in% names(df)){df[is.na(condition) & include_mig==1,include_mig:=NA]}else{df[,include_mig:=NA]}
      df[condition=="Migraine_medicines" & prior_mig==0 & after_mig==0 & is.na(include_mig),include_mig:=1]#when the record is retrieved only from start match
      #1_mig means it was only retrieved by exact match
      
      #Add missing info
      df[!is.na(include_gdm) & is.na(condition),condition:=condition_start]
      df[!is.na(include_mig) & is.na(condition),condition:=condition_start]
      df[!is.na(include_gdm) & is.na(medicinal_product_group),condition:=medicinal_product_group_start]
      df[!is.na(include_mig) & is.na(medicinal_product_group),condition:=medicinal_product_group_start]
      #df[,condition_start:=NULL][,medicinal_product_group_start:=NULL][,group:=NULL][,mechanism:=NULL][,nchar:=NULL]
      cols_to_incl<-c("person_id","medicine_date","year","atc_code", "meaning", "condition","medicinal_product_group","medicinal_product_id")
      
      if(df[include_gdm==1,.N]>0){
        saveRDS(df[condition=="GDM_medicines",cols_to_incl,with=F], paste0(tmp, "GDM_medicines_", med_tab_ind, start_med_group[group_ind], ".rds"))
         }

      if(df[include_mig==1,.N]>0){
        saveRDS(df[condition=="Migraine_medicines",cols_to_incl,with=F], paste0(tmp, "Migraine_medicines_", med_tab_ind, start_med_group[group_ind],".rds"))
       }
      
      #print(paste0("Analyze: ", actual_tables$MEDICINES[med_tab_ind], " for Migraine group ", start_med_group[group_ind], " match filtering."))
      
      # year_mig<-df[condition=="Migraine"][!duplicated(year), year]
      # if(length(year_mig)>0){
      #   for (year_ind in 1:length(year_mig)){
      #     #export the medicines file to the g_intermediate/gdm_algorithm folder
      #     saveRDS(df[,cols_to_incl,with=F][condition=="Migraine" & year==year_mig[year_ind]], paste0(g_intermediate,"migraine_algorithm/",year_mig[year_ind], "_Migraine_med_", med_tab_ind,".rds"))
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
      #   med_summary_migraine_fl<-df[condition=="Migraine",.N, by=list(meaning,medicinal_product_group, atc_code,year)]
      #   setnames(med_summary_migraine_fl, "N", "no_records")
      #   med_summary_migraine[[index]]<-med_summary_migraine_fl
      #   rm(med_summary_migraine_fl)
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
      # med_summary_migraine_start_fl<-df[condition=="Migraine",.N, by=list(meaning,medicinal_product_group, truncated_atc_code,year)]
      # setnames(med_summary_migraine_start_fl, "N", "no_records")
      # med_summary_migraine_start[[index]]<-med_summary_migraine_start_fl
      # rm(med_summary_migraine_start_fl)
      # }
      df[,truncated_atc_code:=NULL][,medicinal_product_group:=NULL][,mechanism:=NULL][,condition:=NULL][,nchar:=NULL][,group:=NULL]
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
  original_rows_gdm_pe<-sum(do.call(rbind,original_rows_gdm_pe))
  original_rows_mig<-sum(do.call(rbind,original_rows_mig))
  empty_med_date_gdm_pe<-sum(do.call(rbind,empty_med_date_gdm_pe))
  empty_med_date_mig<-sum(do.call(rbind,empty_med_date_mig))
  empty_atc_code_gdm_pe<-sum(do.call(rbind,empty_atc_code_gdm_pe))
  empty_atc_code_mig<-sum(do.call(rbind,empty_atc_code_mig))
  lower_atc_gdm_pe<-sum(do.call(rbind,lower_atc_gdm_pe))
  lower_atc_mig<-sum(do.call(rbind,lower_atc_mig))
  empty_med_meaning_gdm_pe<-sum(do.call(rbind,empty_med_meaning_gdm_pe))
  empty_med_meaning_mig<-sum(do.call(rbind,empty_med_meaning_mig))
  any_study_no<-sum(do.call(rbind,any_study_no))
  outside_obs<-sum(do.call(rbind,outside_obs))
  prior_med_rec_gdm_pe<-sum(do.call(rbind,prior_med_rec_gdm_pe))
  prior_med_rec_mig<-sum(do.call(rbind,prior_med_rec_mig))
  after_med_rec_gdm_pe<-sum(do.call(rbind,after_med_rec_gdm_pe))
  after_med_rec_mig<-sum(do.call(rbind,after_med_rec_mig))
  pre_dt_gdm_pe<-sum(do.call(rbind,pre_dt_gdm_pe))
  pre_dt_mig<-sum(do.call(rbind,pre_dt_mig))
  
  flowchart_medicines_gdm_pe<-data.table(Indicator=c("Number of original records",
                                              "Number of records with empty medicine date",
                                              "Number of records with empty ATC codes",
                                              "Number of records with ATC code shorter than 3 characters",
                                              "Number of records with missing meaning(source of record)",
                                              "Number of records not present in the pregnancy D3 for any of the studies",
                                              "Number of records outside min and max observation period for any of the studies",
                                              "Number of records with medicine date before start of study date",
                                              "Number of records with medicine date after end of study date",
                                              "Number of records included, before data filtering"),
                                  GDM_and_PE=c(original_rows_gdm_pe,
                                               empty_med_date_gdm_pe,
                                               empty_atc_code_gdm_pe,
                                               lower_atc_gdm_pe,
                                               empty_med_meaning_gdm_pe,
                                               any_study_no,
                                               outside_obs,
                                               prior_med_rec_gdm_pe,
                                               after_med_rec_gdm_pe,
                                               pre_dt_gdm_pe))
  flowchart_medicines_mig<-data.table(Indicator=c("Number of original records",
                                                     "Number of records with empty medicine date",
                                                     "Number of records with empty ATC codes",
                                                     "Number of records with ATC code shorter than 3 characters",
                                                     "Number of records with missing meaning(source of record)",
                                                  "Number of records not present in the pregnancy D3 for any of the studies",
                                                  "Number of records outside min and max observation period for any of the studies",
                                                     "Number of records with medicine date before start of study date",
                                                     "Number of records with medicine date after end of study date",
                                                     "Number of records included, before data filtering"),
                                         Migraine=c(original_rows_mig,
                                                    empty_med_date_mig,
                                                    empty_atc_code_mig,
                                                    lower_atc_mig,
                                                    empty_med_meaning_mig,
                                                    any_study_no,
                                                    outside_obs,
                                                    prior_med_rec_mig,
                                                    after_med_rec_mig,
                                                    pre_dt_mig))
  
  
  rm(original_rows_gdm_pe,empty_med_date_gdm_pe,lower_atc_gdm_pe,empty_med_meaning_gdm_pe,prior_med_rec_gdm_pe,after_med_rec_gdm_pe,pre_dt_gdm_pe)
  rm(original_rows_mig,empty_med_date_mig,lower_atc_mig,empty_med_meaning_mig,prior_med_rec_mig,after_med_rec_mig,pre_dt_mig)
  rm(any_study_no, outside_obs)
  
  
  
  }else{
    flowchart_medicines_gdm_pe<-data.table(Indicator=c("Number of original records",
                                                       "Number of records with empty medicine date",
                                                       "Number of records with empty ATC codes",
                                                       "Number of records with ATC code shorter than 3 characters",
                                                       "Number of records with missing meaning(source of record)",
                                                       "Number of records not present in the pregnancy D3 for any of the studies",
                                                       "Number of records outside min and max observation period for any of the studies",
                                                       "Number of records with medicine date before start of study date",
                                                       "Number of records with medicine date after end of study date",
                                                       "Number of records included, before data filtering"),
                                           GDM_and_PE=c(0,
                                                        0,
                                                        0,
                                                        0,
                                                        0,
                                                        0,
                                                        0,
                                                        0,
                                                        0,
                                                        0))
    flowchart_medicines_mig<-data.table(Indicator=c("Number of original records",
                                                    "Number of records with empty medicine date",
                                                    "Number of records with empty ATC codes",
                                                    "Number of records with ATC code shorter than 3 characters",
                                                    "Number of records with missing meaning(source of record)",
                                                    "Number of records not present in the pregnancy D3 for any of the studies",
                                                    "Number of records outside min and max observation period for any of the studies",
                                                    "Number of records with medicine date before start of study date",
                                                    "Number of records with medicine date after end of study date",
                                                    "Number of records included, before data filtering"),
                                        Migraine=c(0,
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
  flowchart_medicines_gdm_pe<-data.table(Indicator=c("Number of original records",
                                                     "Number of records with empty medicine date",
                                                     "Number of records with empty ATC codes",
                                                     "Number of records with ATC code shorter than 3 characters",
                                                     "Number of records with missing meaning(source of record)",
                                                     "Number of records not present in the pregnancy D3 for any of the studies",
                                                     "Number of records outside min and max observation period for any of the studies",
                                                     "Number of records with medicine date before start of study date",
                                                     "Number of records with medicine date after end of study date",
                                                     "Number of records included, before data filtering"),
                                         GDM_and_PE=c(0,
                                                      0,
                                                      0,
                                                      0,
                                                      0,
                                                      0,
                                                      0,
                                                      0,
                                                      0,
                                                      0))
  flowchart_medicines_mig<-data.table(Indicator=c("Number of original records",
                                                  "Number of records with empty medicine date",
                                                  "Number of records with empty ATC codes",
                                                  "Number of records with ATC code shorter than 3 characters",
                                                  "Number of records with missing meaning(source of record)",
                                                  "Number of records not present in the pregnancy D3 for any of the studies",
                                                  "Number of records outside min and max observation period for any of the studies",
                                                  "Number of records with medicine date before start of study date",
                                                  "Number of records with medicine date after end of study date",
                                                  "Number of records included, before data filtering"),
                                      Migraine=c(0,
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

fwrite(flowchart_medicines_gdm_pe, paste0(projectFolder,"/g_output/PE and GDM algorithm/Step_03_flowchart_medicines_gdm_pe.csv"),row.names = F)
fwrite(flowchart_medicines_mig, paste0(projectFolder,"/g_output/Migraine algorithm/Step_03_flowchart_medicines_mig.csv"),row.names = F)
rm(flowchart_medicines_gdm_pe,flowchart_medicines_mig)

date_running_end_03<-Sys.Date()
end_time_03<-Sys.time()

time_log_03<-data.table(DAP=data_access_provider_name,
                     Script="Step_03_medicines_clean_up.R", 
                     Start_date=date_running_start_03, 
                     End_date=date_running_end_03,
                     Time_elaspsed=format(end_time_03-initial_time_03, digits=2))
fwrite(time_log_03,paste0(output_dir,"/Time log/Step_03_time_log.csv"),row.names = F)

