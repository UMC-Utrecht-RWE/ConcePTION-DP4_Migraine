#### Necessary steps to have a cleaned programming codelist ####
#### ---------- Necessary functions ---------- ####
identify_parent_clusters<-function(codelist_name, parent_code_fl){
  codelist<-copy(codelist_name)
  parent_code<-parent_code_fl
  level_parent<-nchar(parent_code)
  parent_code<-data.table("truncated_code"=parent_code, "present"=1)
  codelist[,include:=0]
  #identify all codes that start with this parent code
  codelist[,truncated_code:=substr(code_no_dot,1,level_parent)]
  codelist<-merge.data.table(codelist,parent_code, by="truncated_code", all.x=T)
  codelist[present==1, include:=1]
  codelist[,present:=NULL]
  #keep only codes of interest
  codelist<-codelist[include==1]
  codelist[,include:=NULL]
  return(codelist)
}

clean_parent_clusters<-function(parent_cluster){
  parent_cluster[,nchar:=nchar(code_no_dot)]
  #identify parent
  parent_code<-parent_cluster[nchar==min(parent_cluster[!duplicated(nchar),nchar]),code_no_dot]
  level_parent<-nchar(parent_code)
  parent_cluster[,cluster:=0]
  #Identify all levels present, sort in descending order
  levels<-sort(parent_cluster[!duplicated(nchar),nchar], decreasing = T)
  #levels<-levels[!levels %in% level_parent]
  parent_cluster[,keep:=0]
  parent_cluster[nchar==levels[1], keep:=1]
  for (level_ln in 2:length(levels)){
    parent_cluster[,check:=0]
    parent_cluster[nchar==levels[level_ln], check:=1]
    codes_to_check<-parent_cluster[check==1, code_no_dot]
    for(code_ln in 1:length(codes_to_check)){
      if("truncated_code" %in% names(parent_cluster)){parent_cluster[,truncated_code:=NULL]}
      code_of_int<-data.table("truncated_code"=codes_to_check[code_ln], present=1)
      parent_cluster[nchar==nchar(codes_to_check[code_ln])+1,truncated_code:=substr(code_no_dot,1,nchar(codes_to_check[code_ln]))]
      parent_cluster<-merge.data.table(parent_cluster, code_of_int, by="truncated_code", all.x=T)
      parent_cluster[,remove:=0]
      #check if the code is select as keep==1, present==1, compare if tags are the same remove the present==1 if not keep
      tags_check<-parent_cluster[check==1 & code_no_dot==codes_to_check[code_ln], tags]
      parent_cluster[present==1 & keep==1 & tags==tags_check, remove:=1]
      parent_cluster<-parent_cluster[remove==0]
      parent_cluster[,remove:=NULL][,present:=NULL]
      parent_cluster[check==1 & code_no_dot==codes_to_check[code_ln], keep:=1]
      parent_cluster[check==1 & code_no_dot==codes_to_check[code_ln], check:=0]
      rm(code_of_int)
    }
    parent_cluster[,check:=NULL]
  }
  
  return(parent_cluster)
}

tags_cleanup<-function(codelist_name, tag_var_name, possible_rule = "automatic", dt_possible_rule = NULL){
  
  #Copy codelist
  codelist<-copy(codelist_name)
  if(!"variable_name" %in% names(codelist)){
    codelist[,variable_name:=paste(system, event_abbreviation, type, sep="_")]
  }
  
  if(sum(!codelist[,unique(tags)] %in% c("exclude", "narrow", "possible", "ignore"))!=0){stop("The codelist contains not allowed tags. Check original file and confirm the only values in the column tags are: narrow, possible, exclude, ignore.")}
  
  if(!possible_rule %in% c("automatic", "manual")){stop("The parameter possible_rule contains not allowable values. Set to automatic or manual.")}
  
  if(possible_rule == "automatic"){
    #Identify all events where only possible/exclude/ignore tags are available
    codelist[,possible_var:=ifelse("possible" %in% get(tag_var_name), 1, 0), by="variable_name"]
    codelist[,narrow_var:=ifelse("narrow" %in% get(tag_var_name), 1, 0), by="variable_name"]
    #if both possible and narrow are present possible_replace==1(will be replaced with exclude)
    #if only possible is present possible_replace==0(will be replaced with narrow)
    codelist[, possible_replace:=ifelse(possible_var == 1 & narrow_var == 1, 1, 0)]
    codelist[possible_replace == 1 & get(tag_var_name) == "possible", eval(tag_var_name):="exclude"]
    codelist[possible_replace == 0 & get(tag_var_name) == "possible", eval(tag_var_name):="narrow"]
    codelist[,c("possible_var", "narrow_var", "possible_replace"):=NULL]
  }
  
  if(possible_rule == "manual"){
    if(is.null(dt_possible_rule)){stop("The possible_rule has been set to manual but no input information has been provided. Set up the data.table dt_possible_rule with the columns variable_name and possible_rule.The data.table should contain all events present in the codelist.")}
    #Check if the input dataset has the right variables
    if(length(setdiff(sort(c("variable_name", "possible_rule")), sort(names(dt_possible_rule))))!=0){stop("The data.table dt_possible_rule contains not allowed columns. The column names should be variable_name and possible_rule.")}
    if(sum(!dt_possible_rule[,unique(possible_rule)] %in% c("exclude", "narrow"))!=0){stop("The possible_rule column in the data.table dt_possible_rule contains not allowed values The column should be set to exclude or narrow.")}
    codelist<-data.table::merge.data.table(codelist, dt_possible_rule, by="variable_name", all.x = T)
    codelist[get(tag_var_name) == "possible", eval(tag_var_name):=possible_rule]
    codelist[,possible_rule:=NULL]
  }
  
  #remove ignore tags
  codelist<-codelist[!get(tag_var_name) %in% "ignore"]
  
  if(sum(!codelist[,unique(tags)] %in% c("exclude", "narrow"))!=0){stop("The replace action of the possible tags has not been applied correctly. If possible_rule == 'manual' make sure dt_possible_rule contains all events names and the respective exchange rule.")}
  
  return(codelist)
}


#### ---------- Step 1: Load the codelist ---------- ####
codelist_dt_fl<-list.files(paste0(pre_dir, "/codelists/"), "full_codelist")
codelist_dt<-fread(paste0(pre_dir, "/codelists/", codelist_dt_fl), colClasses = "character")
codelist_dt<-codelist_dt[tags %in% c("narrow", "possible", "exclude", "ignore")]
codelist_dt[,variable_name:=paste(system, event_abbreviation, type, sep="_")]
#remove all PrA events
codelist_dt<-codelist_dt[!type %in% "PrA"]
codelist_dt<-codelist_dt[variable_name %in% c("V_HYPERTENSION_COV", "E_DM12_COV", "Ment_DEPRESSION_COV", "L_OBESITY_COV")]


if(codelist_dt[,.N]>0){
#### ---------- Step 2: Create codelist with cleaned tags (only exclude and narrow present in the output codelist) ---------- ####
cleaned_codelist<-tags_cleanup(codelist_name = codelist_dt, tag_var_name = "tags", possible_rule = "automatic")

#### ---------- Step 3: Keep only necessary columns in the codelist and rename them accordingly, create code_no_dot, remove duplicates ---------- ####
cleaned_codelist<-cleaned_codelist[,.(event_definition, coding_system, tags, code, variable_name)]
names(cleaned_codelist)<-c("event_definition", "coding_system", "tags", "event_code", "variable_name")
cleaned_codelist[, code_no_dot := gsub("\\.", "", event_code)]
cleaned_codelist<-unique(cleaned_codelist, by = c("variable_name", "coding_system", "code_no_dot"))

#### ---------- Step 4: Load the mechanism parameter file ---------- ####
# mechanism<-data.table(coding_system = cleaned_codelist[!duplicated(coding_system), coding_system],
#                       mechanism = c("start", "start", "start", "start", "start", "start", "exact", "exact", "exact",
#                                     "start", "start", "start", "start", "exact", "exact", "start", "start", "start",
#                                     "exact", "exact", "exact", "exact", "start", "exact", "exact", "exact", "exact",
#                                     "exact", "exact", "start", "exact", "exact", "exact", "exact", "exact", "start",
#                                     "exact", "exact", "start"))
#fwrite(mechanism, "/Users/vhoxhaj/Desktop/coding_system_mechanism.csv", row.names = F)

mechanism<-fread(paste0(pre_dir, "codelists/coding_system_mechanism.csv"), colClasses = "character")
#### ---------- Step 5: Identify parent clussters, clean up the codelist for the start with procedure ---------- ####

cleaned_codelist_start<-cleaned_codelist[coding_system %in% mechanism[mechanism == "start", coding_system]]
if(cleaned_codelist_start[,.N]>0){
#Identify all events present in the codelist start
conditions_present_start<-cleaned_codelist_start[!duplicated(variable_name),variable_name]

#identify parent clusters and remove uneccessary codes
list_cond<-list()
for (cond_ind in 1:length(conditions_present_start)){
  #Create a short version of the codelist where only the event of interest is present
  codl_short<-cleaned_codelist_start[variable_name == conditions_present_start[cond_ind]]
  #Check the presence of all vocabularies for the event of interest
  voc_present<-codl_short[!duplicated(coding_system),coding_system]
  #Start a loop over all vocabularies present, look at a vocabulary at a time
  list_voc<-list()
  for (voc_ind in 1:length(voc_present)){
    #Create a short version of the codelist where only the vocabulary of interest is present
    codl_short_voc<-codl_short[coding_system == voc_present[voc_ind]]
    #Calculate the number of characters for each code present
    codl_short_voc[,nchar:=nchar(code_no_dot)]
    #start from the shortest to create clusters
    code_length<-sort(as.numeric(codl_short_voc[!duplicated(nchar),nchar]))
    list_final<-list()
    #create parent clusters
    if(codl_short_voc[,.N]>0){
      for (cod_l in 1:length(code_length)){
        if(codl_short_voc[,.N]>0){
          codes<-codl_short_voc[nchar==code_length[cod_l]][!duplicated(code_no_dot),code_no_dot]
          if(length(codes)>0){
            list<-list()
            #identify all clusters and save them in a list
            for(code_ind in 1:length(codes)){
              list[[code_ind]]<-identify_parent_clusters(codelist = codl_short_voc, parent_code_fl = codes[code_ind])
            }
            #remove from the codelist all codes that are now present in clusters
            for (list_ln in 1:length(list)){
              list_element<-list[[list_ln]]
              list_element<-list_element[,c("event_definition","variable_name", "coding_system","event_code","tags","code_no_dot")]
              cols<-c("event_definition","variable_name", "coding_system","event_code","tags","code_no_dot")
              list_element[,remove:=1]
              codl_short_voc<-merge.data.table(codl_short_voc,list_element, by=cols, all.x = T)
              codl_short_voc<-codl_short_voc[is.na(remove)]
              codl_short_voc[,remove:=NULL]
            }
            list_final<-append(list_final,list)
            rm(list)
          }
        }
      }
    }  
    #remove empty list elements if any
    filtered_list <- Filter(function(x) !is.null(x) && length(x) > 0, list_final)
    names(filtered_list)<-LETTERS[seq_along(filtered_list)]
    rm(list_final)
    #identify all one row datatables
    one_row_datasets <- names(filtered_list)[sapply(filtered_list, function(x) nrow(x) == 1)]
    one_row_list<-filtered_list[names(filtered_list) %in% one_row_datasets]
    one_row_list<-as.data.table(do.call(rbind, one_row_list))
    #work only with clusters with more than one row
    filtered_list<-filtered_list[!names(filtered_list) %in% one_row_datasets]
    if(length(filtered_list)>0){
      list_cleaned<-list()
      for(list_ln in 1:length(filtered_list)){
        list_cleaned[[list_ln]]<-clean_parent_clusters(parent_cluster = filtered_list[[list_ln]])
      }
      #remove empty list elements if any
      list_cleaned <- Filter(function(x) !is.null(x) && length(x) > 0, list_cleaned)
      #combine
      list_cleaned<-as.data.table(do.call(rbind, list_cleaned))
    }else{
      list_cleaned<-data.table() 
    }
    
    cols<-c("event_definition","variable_name", "coding_system","event_code","tags","code_no_dot")
    if(list_cleaned[,.N]>0){
      list_cleaned<-list_cleaned[,cols,with=F]
    }
    if(one_row_list[,.N]>0){
      one_row_list<-one_row_list[,cols,with=F]
    }
    list_cleaned<-rbind(list_cleaned,one_row_list)
    rm(one_row_list)
    list_voc[[voc_ind]]<-list_cleaned
    rm(list_cleaned)
  }
  list_voc<-as.data.table(do.call(rbind, list_voc))
  list_cond[[cond_ind]]<-list_voc
  rm(list_voc)
}

list_cond<-as.data.table(do.call(rbind, list_cond))    
rm(cleaned_codelist_start, conditions_present_start)
}else{list_cond<-NULL}
#### ---------- Step 6: Create the exact match codelist ---------- ####

cleaned_codelist_exact<-cleaned_codelist[coding_system %in% mechanism[mechanism == "exact", coding_system]]
#keep only narrow codes
cleaned_codelist_exact<-cleaned_codelist_exact[tags %in% "narrow"]
if(cleaned_codelist_exact[,.N]==0){cleaned_codelist_exact<-NULL}
#### ---------- Step 7: Create the full programming codelist ---------- ####
codelist_final<-rbind(list_cond, cleaned_codelist_exact)
rm(list_cond, cleaned_codelist_exact)

fwrite(codelist_final, paste0(pre_dir, "codelists/programming_codelist.csv"), row.names = F)



}else{
  stop("No events are present in the codelist. Fix the codelist or list of restricted events and run the script again.")
}