#Functions to create concept sets
inside_filter_function_start<-function(dataset, specific_codelist, nested_codelist, full_codelist, code_var, vocabulary_var, event_var, tags_var, event_name, voc_name, file_name, output_folder, var_export=NULL){
  #get all codes from the nested codelist that match the criteria
  codes<-data.table(event_var = event_name, vocabulary_var = voc_name, code_var = nested_codelist[[event_name]][[voc_name]], filter = 1)
  names(codes)<-c(event_var, vocabulary_var, code_var, "filter")
  #Identify all tags
  codes<-merge.data.table(codes, full_codelist, by = c(event_var, vocabulary_var, code_var), all.x=T)
  codes<-codes[,c(event_var, vocabulary_var, code_var, tags_var, "filter"), with = F]
  #Create a check filter variable to check against the vocabulary of interest only
  specific_codelist[,filter_check := ifelse(eval(get(vocabulary_var)) == voc_name, 1, 0)]
  specific_codelist[,checked:=0]
  #Create code levels
  codes[,nchar:=nchar(get(code_var))]
  levels<-sort(codes[!duplicated(nchar),nchar],decreasing = T)
  
  df_checked<-list()
  level<-1
  for(codes_ind in levels){
    #Identify all codes present at the last level
    codes[nchar==codes_ind,truncated_code:=get(code_var)]
    #create truncated code in the dataset
    specific_codelist[checked == 0,truncated_code:=substr(get(code_var),1,codes_ind)]
    
    specific_codelist_refined<-merge.data.table(specific_codelist[filter_check == 1 & checked == 0], codes[,-c(code_var, "nchar"), with=F], by = c(vocabulary_var,"truncated_code"), all.x = T, allow.cartesian = T)
    specific_codelist_refined[filter==1,checked:=1]
    specific_codelist_refined<-specific_codelist_refined[filter == 1]
    specific_codelist_refined[,checked:=NULL][,truncated_code:=NULL]
    if(specific_codelist_refined[,.N]>0){
      df_checked[[level]] <- specific_codelist_refined
      level <- level + 1 
    }
    
    
    #Update the specific_codelist so codes already checke don't get checked again
    specific_codelist<-merge.data.table(specific_codelist, specific_codelist_refined[,c(vocabulary_var, code_var, "filter_check", "filter"), with=F], by = c(vocabulary_var, code_var, "filter_check"), all.x = T, allow.cartesian = T)
    specific_codelist[filter == 1, checked := 1]
    #remove col
    codes[,truncated_code:=NULL]
    specific_codelist[,truncated_code:=NULL][,filter:=NULL]
    
    rm(specific_codelist_refined)
  }
  df_checked<-as.data.table(do.call(rbind,df_checked))
  if(df_checked[,.N]>0){
    df_checked<-df_checked[,c(vocabulary_var, code_var, event_var, tags_var), with = F]
    
    #Merge with main datatset: add it separately
    dataset<-merge.data.table(dataset, df_checked, by = c(code_var, vocabulary_var), all.x = T, allow.cartesian = T)
    rm(df_checked)
    if(dataset[!is.na(tags), .N]>0){
      #Keep only narrow tags in the final dataset
      if(dataset[tags == "narrow", .N]>0){
        #Export file
        export_name<-paste0(event_name, "_", voc_name, "_", file_name)
        if(!is.null(var_export)){
          dataset<-dataset[,var_export, with=F]
        }
        saveRDS(dataset[tags == "narrow"], paste0(output_folder, export_name, ".rds"))
      }
    }
    dataset[,variable_name:=NULL][,tags:=NULL]
  }
  specific_codelist[,filter_check:=NULL][,checked:=NULL]
  
}

inside_filter_function_exact<-function(dataset, specific_codelist, nested_codelist, full_codelist, code_var, vocabulary_var, event_var, tags_var, event_name, voc_name, file_name, output_folder, var_export=NULL){
  #get all codes from the nested codelist that match the criteria
  codes<-data.table(event_var = event_name, vocabulary_var = voc_name, code_var = nested_codelist[[event_name]][[voc_name]], filter = 1)
  names(codes)<-c(event_var, vocabulary_var, code_var, "filter")
  #Identify all tags
  codes<-merge.data.table(codes, full_codelist, by = c(event_var, vocabulary_var, code_var), all.x=T)
  codes<-codes[,c(event_var, vocabulary_var, code_var, tags_var, "filter"), with = F]
  
  specific_codelist_refined<-merge.data.table(specific_codelist, codes, , by = c(vocabulary_var, code_var), all.x = T, allow.cartesian = T)
  
  specific_codelist_refined<-specific_codelist_refined[,c(vocabulary_var, code_var, event_var, tags_var), with = F]
  
  #Merge with main datatset: add it separately
  dataset<-merge.data.table(dataset, specific_codelist_refined, by = c(code_var, vocabulary_var), all.x = T, allow.cartesian = T)
  rm(specific_codelist_refined)
  if(dataset[!is.na(tags),.N]>0){
    #Keep only narrow tags in the final dataset
    if(dataset[tags == "narrow", .N]>0){
      #Export file
      export_name<-paste0(event_name, "_", voc_name, "_", file_name)
      if(!is.null(var_export)){
        dataset<-dataset[,var_export, with=F]
      }
      saveRDS(dataset[tags == "narrow"], paste0(output_folder, export_name, ".rds"))
    }
  }
  dataset[,variable_name:=NULL][,tags:=NULL]
  
}
