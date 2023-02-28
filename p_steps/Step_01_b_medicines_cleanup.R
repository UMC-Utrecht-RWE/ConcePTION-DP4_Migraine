#Clean up the medicines table to create the antidiabetic_medicines_D3

#Load the MEDICINES tables one by one and keep only antidiabetic medications as explained in antidiabetic_medications_codelist


####load info, parameters, conceptsets####
source(paste0(pre_dir,"info/directory_info.R"))
source(paste0(pre_dir,"info/DAP_info.R"))
source(paste0(pre_dir,"parameters/parameters_metadata.R"))
source(paste0(pre_dir,"parameters/study_parameters.R"))

####Load the codelist####
gdm_med_codelist<-fread(paste0(pre_dir,"codelists/", list.files(paste0(pre_dir,"codelists/"),"antidiabetic_medications_codelist")))
migraine_med_codelist<-fread(paste0(pre_dir,"codelists/", list.files(paste0(pre_dir,"codelists/"),"migraine_medications_codelist")))

#Separate codelist into start and exact match
gdm_start<-gdm_med_codelist[mechanism=="start"]
gdm_start[,condition:="GDM"]
gdm_exact<-gdm_med_codelist[mechanism=="exact"]
gdm_exact[,condition:="GDM"]
migraine_start<-migraine_med_codelist[mechanism=="start"]
migraine_start[,condition:="Migraine"]
migraine_exact<-migraine_med_codelist[mechanism=="exact"]
migraine_exact[,condition:="Migraine"]

start_med<-rbind(gdm_start,migraine_start)
rm(gdm_start,migraine_start)
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

####Check for CDM table availability####
source(paste0(pre_dir, "info/directory_info.R"))

#####The tables that will be search for medicines: MEDICINES####
#The information about GDM medicines is saved in 
#codesheet_medicines_gdm
#The information about Migraine diagnoses is saved in 
#codesheet_medicines_migraine
med_gdm_medicines<-ifelse("MEDICINES" %in% codesheet_medicines_gdm[,table],1,0)

med_migraine_medicines<-ifelse("MEDICINES" %in% codesheet_medicines_migraine[,table],1,0)

if("code" %in% codesheet_medicines_gdm[table=="MEDICINES",val_1]){
  code_var<-codesheet_medicines_gdm[table=="MEDICINES",col_1]
  date_var1<-codesheet_medicines_gdm[table=="MEDICINES",date_column]
  date_var2<-codesheet_medicines_gdm[table=="MEDICINES",date_column2]
}
if(!is.na(date_var1) & is.na(date_var2)){date_var<-date_var1}
if(is.na(date_var1) & !is.na(date_var2)){date_var<-date_var2}
if(!is.na(date_var1) & !is.na(date_var2)){date_var<-"both"}

####Main script####
if(length(actual_tables$MEDICINES)>0){
  
  original_rows<-list()
  removed_med_date<-list()
  empty_atc<-list()
  lower_atc<-list()
  med_summary_gdm<-list()
  med_summary_migraine<-list()
  index<-1
  
  for (med_tab_ind in 1:length(actual_tables$MEDICINES)){
    df<-fread(paste(path_dir, actual_tables$MEDICINES[med_tab_ind], sep=""), stringsAsFactors = FALSE, colClasses = "character")
    if(date_var=="both"){
    cols<-c("person_id", code_var,"date_prescription","date_dispensing", "meaning_of_drug_record")
    }else{
      cols<-c("person_id", code_var, date_var, "meaning_of_drug_record")  
    }
    df<-df[,cols, with=F]
    df<-df[, lapply(.SD, FUN=function(x) gsub("^$|^ $", NA, x))] #make sure missing data is read appropriately
    #create date variable
    if(date_var=="both"){
      df[,medicine_date:=date_prescription]
      df[!is.na(date_dispensing), medicine_date:=date_dispensing]
      df[,date_prescription:=NULL][,date_prescription:=NULL]
    }
    setnames(df,"meaning_of_drug_record","meaning")
    setnames(df, code_var,"atc_code")
    setnames(df, date_var,"medicine_date")
    #number of original rows
    original_rows[[index]]<-df[,.N]
    #records with empty medicines date
    empty_dates<-df[is.na(medicine_date),.N]
    removed_med_date[[index]]<-empty_dates
    rm(empty_dates)
    df<-df[!is.na(medicine_date)]
    #transform into date variables
    df[,medicine_date:=as.Date(medicine_date,"%Y%m%d")] #transform to date variables
    #create year variable
    df[,year:=year(medicine_date)]
    #remove records with empty atc codes
    empty_atc[[index]]<-df[is.na(atc_code),.N]
    df<-df[!is.na(atc_code)]
    #remove records with atc<3 digits
    df[,nchar:=nchar(atc_code)]
    lower_atc[[index]]<-df[nchar<3,.N]
    df<-df[nchar>=3]
    df[,nchar:=NULL]
    
    #Apply the exact match codes
    #merge with the medicines codelist
    df<-merge.data.table(df, exact_med, by="atc_code", all.x=T, allow.cartesian = T)
    df[condition=="GDM",include:="1_gdm"]
    df[condition=="Migraine",include:="1_mig"]
    setnames(df,"medicinal_product_group","medicinal_product_group_start")
    setnames(df,"condition","condition_start")
    
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
    df[,mechanism:=NULL]
    
    #Apply start with codes
    
    start_med_group<-start_med[!duplicated(group), group]
    for (group_ind in 1:length(start_med_group)){
      length<-start_med[group==start_med_group[group_ind]][!duplicated(nchar),nchar]
      df[,truncated_atc_code:=substr(atc_code,1,length)]
      df<-merge.data.table(df, start_med[group==start_med_group[group_ind]], by="truncated_atc_code", all.x=T, allow.cartesian = T)
      df[condition=="GDM" & !is.na(include), include:="3_gdm"]#when the record is retrieved both from start and exact match
      df[condition=="GDM" & is.na(include), include:="2_gdm"]#when the record is retrieved only from start match
      #1_gdm means it was only retrieved by exact match
      
      df[condition=="Migraine" & !is.na(include),include:="3_mig"]#when the record is retrieved both from start and exact match
      df[condition=="Migraine" & is.na(include),include:="2_mig"]#when the record is retrieved only from start match
      #1_mig means it was only retrieved by exact match
      
      #Add missing info
      df[!is.na(include) & is.na(condition),condition:=condition_start]
      df[!is.na(include) & is.na(medicinal_product_group),condition:=medicinal_product_group_start]
      #df[,condition_start:=NULL][,medicinal_product_group_start:=NULL][,group:=NULL][,mechanism:=NULL][,nchar:=NULL]
      cols_to_incl<-c("person_id","medicine_date","year","atc_code", "meaning", "condition")
      
      print(paste0("Analyze: ", actual_tables$MEDICINES[med_tab_ind], " for GDM group ", start_med_group[group_ind], " match filtering."))
      year_gdm<-df[condition=="GDM"][!duplicated(year), year]
      if(length(year_gdm)>0){
        for (year_ind in 1:length(year_gdm)){
          #export the medicines file to the g_intermediate/gdm_algorithm folder
          saveRDS(df[,cols_to_incl,with=F][condition=="GDM" & year==year_gdm[year_ind]], paste0(g_intermediate,"gdm_algorithm/", year_gdm[year_ind], "_GDM_med_", med_tab_ind,".rds"))
        }
      }
      rm(year_gdm)
      
      print(paste0("Analyze: ", actual_tables$MEDICINES[med_tab_ind], " for Migraine group ", start_med_group[group_ind], " match filtering."))
      
      year_mig<-df[condition=="Migraine"][!duplicated(year), year]
      if(length(year_mig)>0){
        for (year_ind in 1:length(year_mig)){
          #export the medicines file to the g_intermediate/gdm_algorithm folder
          saveRDS(df[,cols_to_incl,with=F][condition=="Migraine" & year==year_mig[year_ind]], paste0(g_intermediate,"migraine_algorithm/",year_mig[year_ind], "_Migraine_med_", med_tab_ind,".rds"))
        }
      }
      
      if(df[condition=="GDM",.N]>0){
        #create a short summary of the included records
        med_summary_gdm_fl<-df[condition=="GDM",.N, by=list(meaning,medicinal_product_group, atc_code,year)]
        setnames(med_summary_gdm_fl, "N", "no_records")
        med_summary_gdm[[index]]<-med_summary_gdm_fl
        rm(med_summary_gdm_fl)
      }
      
      if(df[condition=="Migraine",.N]>0){
        #create a short summary of the included records
        med_summary_migraine_fl<-df[condition=="Migraine",.N, by=list(meaning,medicinal_product_group, atc_code,year)]
        setnames(med_summary_migraine_fl, "N", "no_records")
        med_summary_migraine[[index]]<-med_summary_migraine_fl
        rm(med_summary_migraine_fl)
      }
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
      df[,medicinal_product_group:=NULL][,mechanism:=NULL][,condition:=NULL][,nchar:=NULL][,group:=NULL]
       rm(length)
    }
    
    index<-index+1
    rm(df)
  }
  
  #Combine all data and export to g_output
  original_rows<-sum(do.call(rbind,original_rows))
  removed_med_date<-sum(do.call(rbind,removed_med_date))
  empty_atc<-sum(do.call(rbind,empty_atc))
  lower_atc<-sum(do.call(rbind,lower_atc))
  
  flowchart_medicines<-data.table(Indicator=c("Number of original records",
                                     "Number of records with empty medicine date",
                                     "Number of records with empty ATC codes",
                                     "Number of records with ATC code shorter than 3 characters"),
                         MEDICINES=c(original_rows,
                                     removed_med_date,
                                     empty_atc,
                                     lower_atc))
  
  rm(original_rows,removed_med_date,empty_atc,lower_atc)
  fwrite(flowchart_medicines, paste0(projectFolder,"/g_output/PE and GDM algorithm/flowchart_medicines.csv"),row.names = F)
  fwrite(flowchart_medicines, paste0(projectFolder,"/g_output/Migraine algorithm/flowchart_medicines.csv"),row.names = F)
  
  med_summary_gdm<-as.data.table(do.call(rbind,med_summary_gdm))
  med_summary_gdm_<-med_summary_gdm[,lapply(.SD,sum), by=c("meaning", "medicinal_product_group","atc_code","year"), .SDcols="no_records"]
  med_summary_migraine<-as.data.table(do.call(rbind,med_summary_migraine))
  med_summary_migraine<-med_summary_migraine[,lapply(.SD,sum), by=c("meaning", "medicinal_product_group","atc_code","year"), .SDcols="no_records"]

  #export
  if(!is.null(med_summary_gdm)){
  fwrite(med_summary_gdm, paste0(output_dir,"PE and GDM algorithm/", "medicines_description_gdm.csv"), row.names = F)
  }
  if(!is.null(med_summary_migraine)){
    fwrite(med_summary_migraine, paste0(output_dir,"Migraine algorithm/", "medicines_description_migraine.csv"), row.names = F)
  }
  
  rm(med_summary_gdm,med_summary_migraine)
  
}else{
  flowchart_medicines<-data.table(Indicator=c("Number of original records",
                                     "Number of records with empty medicine date",
                                     "Number of records with empty ATC codes",
                                     "Number of records with ATC code shorter than 3 characters"),
                         MEDICINES=c(0,
                                     0,
                                     0,
                                     0))
  
  
}
####Files clean up####
#GDM
gdm_med_files_fl<-list.files(paste0(projectFolder,"/g_intermediate/gdm_algorithm/"), "_GDM_med_")
#combine files by year
if(length(gdm_med_files_fl)>0){
  files<-list()
  for (i in 1: length(gdm_med_files_fl)){
    files<-append(files,substr(gdm_med_files_fl[i],1,4))
  }
  files<-do.call(c,files)
  #remove duplicates 
  files<-files[!duplicated(files)]
  #create list with names year_condition
  gdm_med_list<-vector(mode="list", length=length(files))
  names(gdm_med_list)<-files
  rm(files)
  #separate all files into the right category
  for (i in 1:length(gdm_med_list)){
    gdm_med_list[[i]]<-gdm_med_files_fl[startsWith(gdm_med_files_fl,names(gdm_med_list)[i])]
  }
  gdm_med_files<-gdm_med_list
  rm(gdm_med_list)
  
  
  #Combine files
  for (med_fl_ind in 1:length(gdm_med_files)){
    df_med<-lapply(paste0(projectFolder,"/g_intermediate/gdm_algorithm/", gdm_med_files[[med_fl_ind]]), readRDS)
    df_med<-as.data.table(do.call(rbind,df_med))
    saveRDS(df_med, paste0(projectFolder,"/g_intermediate/gdm_algorithm/", names(gdm_med_files)[med_fl_ind],"_GDM_med.rds"))
  }
  rm(df_med)
  
  #delete old files
  for(i in 1:length(gdm_med_files_fl)){
    file.remove(paste0(projectFolder,"/g_intermediate/gdm_algorithm/", gdm_med_files_fl[[i]]))
  }
}

#Migraine
mig_med_files_fl<-list.files(paste0(projectFolder,"/g_intermediate/migraine_algorithm/"), "_Migraine_med_")
#combine files by year
if(length(mig_med_files_fl)>0){
  files<-list()
  for (i in 1: length(mig_med_files_fl)){
    files<-append(files,substr(mig_med_files_fl[i],1,4))
  }
  files<-do.call(c,files)
  #remove duplicates 
  files<-files[!duplicated(files)]
  #create list with names year_condition
  mig_med_list<-vector(mode="list", length=length(files))
  names(mig_med_list)<-files
  rm(files)
  #separate all files into the right category
  for (i in 1:length(mig_med_list)){
    mig_med_list[[i]]<-mig_med_files_fl[startsWith(mig_med_files_fl,names(mig_med_list)[i])]
  }
  mig_med_files<-mig_med_list
  rm(mig_med_list)
  
  
  #Combine files
  for (med_fl_ind in 1:length(mig_med_files)){
    df_med<-lapply(paste0(projectFolder,"/g_intermediate/migraine_algorithm/", mig_med_files[[med_fl_ind]]), readRDS)
    df_med<-as.data.table(do.call(rbind,df_med))
    saveRDS(df_med, paste0(projectFolder,"/g_intermediate/migraine_algorithm/", names(mig_med_files)[med_fl_ind],"_Migraine_med.rds"))
  }
  rm(df_med)
  
  #delete old files
  for(i in 1:length(mig_med_files_fl)){
    file.remove(paste0(projectFolder,"/g_intermediate/migraine_algorithm/", mig_med_files_fl[[i]]))
  }
}



