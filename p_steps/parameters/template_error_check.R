#Template error check

#Check if any study var GDM_diagnoses,PE_diagnoses,Migraine_diagnoses haven't been set to codesheet
####Medicines####
if("GDM_medicines" %in% additional_concepts[,StudyVar]){
codesheet_not_medicines_gdm<-additional_concepts[StudyVar=="GDM_medicines" & type!="codesheet"]
if(codesheet_not_medicines_gdm[,.N]>0){
 stop("This is not a script error. The additional_concepts template has been filled out wrong. For the StudyVar GDM_medicines, there are vales not equal to 'codesheet' in type.") 
}
rm(codesheet_not_medicines_gdm)
}
if("PE_medicines" %in% additional_concepts[,StudyVar]){
codesheet_not_medicines_pe<-additional_concepts[StudyVar=="PE_medicines"& type!="codesheet"]
if(codesheet_not_medicines_pe[,.N]>0){
  stop("This is not a script error. The additional_concepts template has been filled out wrong. For the StudyVar PE_medicines, there are vales not equal to 'codesheet' in type.") 
}
rm(codesheet_not_medicines_pe)
}
if("Migraine_medicines" %in% additional_concepts[,StudyVar]){
codesheet_not_medicines_migraine<-additional_concepts[StudyVar=="Migraine_medicines"& type!="codesheet"]
if(codesheet_not_medicines_migraine[,.N]>0){
  stop("This is not a script error. The additional_concepts template has been filled out wrong. For the StudyVar Migraine_medicines, there are vales not equal to 'codesheet' in type.") 
}
rm(codesheet_not_medicines_migraine)
}
####Diagnoses####
if("GDM_diagnoses" %in% additional_concepts[,StudyVar]){
codesheet_not_diagnoses_gdm<-additional_concepts[StudyVar=="GDM_diagnoses"& type!="codesheet"]
if(codesheet_not_diagnoses_gdm[,.N]>0){
  stop("This is not a script error. The additional_concepts template has been filled out wrong. For the StudyVar GDM_diagnoses, there are vales not equal to 'codesheet' in type.") 
}
rm(codesheet_not_diagnoses_gdm)
}
if("PE_diagnoses" %in% additional_concepts[,StudyVar]){
codesheet_not_diagnoses_pe<-additional_concepts[StudyVar=="PE_diagnoses"& type!="codesheet"]
if(codesheet_not_diagnoses_pe[,.N]>0){
  stop("This is not a script error. The additional_concepts template has been filled out wrong. For the StudyVar PE_diagnoses, there are vales not equal to 'codesheet' in type.") 
}
rm(codesheet_not_diagnoses_pe)
}
if("Migraine_diagnoses" %in% additional_concepts[,StudyVar]){
codesheet_not_diagnoses_migraine<-additional_concepts[StudyVar=="Migraine_diagnoses"& type!="codesheet"]
if(codesheet_not_diagnoses_migraine[,.N]>0){
  stop("This is not a script error. The additional_concepts template has been filled out wrong. For the StudyVar Migraine_diagnoses, there are vales not equal to 'codesheet' in type.") 
}
rm(codesheet_not_diagnoses_migraine)
}
####Procedures####
if("GDM_procedures" %in% additional_concepts[,StudyVar]){
  codesheet_not_procedures_gdm<-additional_concepts[StudyVar=="GDM_procedures"& type!="codesheet"]
  if(codesheet_not_procedures_gdm[,.N]>0){
    stop("This is not a script error. The additional_concepts template has been filled out wrong. For the StudyVar GDM_procedures, there are vales not equal to 'codesheet' in type.") 
  }
  rm(codesheet_not_procedures_gdm)
}
if("PE_procedures" %in% additional_concepts[,StudyVar]){
  codesheet_not_procedures_pe<-additional_concepts[StudyVar=="PE_diagnoses"& type!="codesheet"]
  if(codesheet_not_procedures_pe[,.N]>0){
    stop("This is not a script error. The additional_concepts template has been filled out wrong. For the StudyVar PE_procedures, there are vales not equal to 'codesheet' in type.") 
  }
  rm(codesheet_not_procedures_pe)
}
if("Migraine_procedures" %in% additional_concepts[,StudyVar]){
  codesheet_not_procedures_migraine<-additional_concepts[StudyVar=="Migraine_procedures"& type!="codesheet"]
  if(codesheet_not_procedures_migraine[,.N]>0){
    stop("This is not a script error. The additional_concepts template has been filled out wrong. For the StudyVar Migraine_diagnoses, there are vales not equal to 'codesheet' in type.") 
  }
  rm(codesheet_not_procedures_migraine)
}
####EVENTS columns####
if(sum(c("GDM_diagnoses","PE_diagnoses","Migraine_diagnoses") %in% additional_concepts[,StudyVar])>0){
  codesheet_events<-additional_concepts[StudyVar %in% c("GDM_diagnoses","PE_diagnoses","Migraine_diagnoses") & table=="EVENTS", c("val_1","col_1", "val_2","col_2")]
  #Check for different values in val_1
  not_equal_val1<-codesheet_events[!duplicated(val_1),.N]
  if(not_equal_val1>1){
    stop("This is not a script error. The additional_concepts template has been filled out wrong. The columnn val_1 has different values between GDM_diagnoses, PE_diagnoses and Migraine_diagnoses for table set to EVENTS.") 
  }
  #Check for different values in col_1
  not_equal_col1<-codesheet_events[!duplicated(col_1),.N]
  if(not_equal_col1>1){
    stop("This is not a script error. The additional_concepts template has been filled out wrong. The columnn col_1 has different values between GDM_diagnoses, PE_diagnoses and Migraine_diagnoses for table set to EVENTS.") 
  }
  
  #Check for different values in val_2
  not_equal_val2<-codesheet_events[!duplicated(val_2),.N]
  if(not_equal_val2>1){
    stop("This is not a script error. The additional_concepts template has been filled out wrong. The columnn val_2 has different values between GDM_diagnoses, PE_diagnoses and Migraine_diagnoses for table set to EVENTS.") 
  }
  
  #Check for different values in col_2
  not_equal_col2<-codesheet_events[!duplicated(col_2),.N]
  if(not_equal_col2>1){
    stop("This is not a script error. The additional_concepts template has been filled out wrong. The columnn col_2 has different values between GDM_diagnoses, PE_diagnoses and Migraine_diagnoses for table set to EVENTS.") 
  }
  
  
  rm(codesheet_events)
}
####MO columns####
if(sum(c("GDM_diagnoses","PE_diagnoses","Migraine_diagnoses") %in% additional_concepts[,StudyVar])>0){
  codesheet_mo<-additional_concepts[StudyVar %in% c("GDM_diagnoses","PE_diagnoses","Migraine_diagnoses") & table=="MEDICAL_OBSERVATIONS", c("val_1","col_1", "val_2","col_2")]
  #Check for different values in val_1
  not_equal_val1<-codesheet_mo[!duplicated(val_1),.N]
  if(not_equal_val1>1){
    stop("This is not a script error. The additional_concepts template has been filled out wrong. The columnn val_1 has different values between GDM_diagnoses, PE_diagnoses and Migraine_diagnoses for table set to MEDICAL_OBSERVATIONS.") 
  }
  #Check for different values in col_1
  not_equal_col1<-codesheet_mo[!duplicated(col_1),.N]
  if(not_equal_col1>1){
    stop("This is not a script error. The additional_concepts template has been filled out wrong. The columnn col_1 has different values between GDM_diagnoses, PE_diagnoses and Migraine_diagnoses for table set to MEDICAL_OBSERVATIONS.") 
  }
  
  #Check for different values in val_2
  not_equal_val2<-codesheet_mo[!duplicated(val_2),.N]
  if(not_equal_val2>1){
    stop("This is not a script error. The additional_concepts template has been filled out wrong. The columnn val_2 has different values between GDM_diagnoses, PE_diagnoses and Migraine_diagnoses for table set to MEDICAL_OBSERVATIONS.") 
  }
  
  #Check for different values in col_2
  not_equal_col2<-codesheet_mo[!duplicated(col_2),.N]
  if(not_equal_col2>1){
    stop("This is not a script error. The additional_concepts template has been filled out wrong. The columnn col_2 has different values between GDM_diagnoses, PE_diagnoses and Migraine_diagnoses for table set to MEDICAL_OBSERVATIONS.") 
  }
  
  
  rm(codesheet_mo)
}
####SO columns####
if(sum(c("GDM_diagnoses","PE_diagnoses","Migraine_diagnoses") %in% additional_concepts[,StudyVar])>0){
  codesheet_so<-additional_concepts[StudyVar %in% c("GDM_diagnoses","PE_diagnoses","Migraine_diagnoses") & table=="SUREVY_OBSERVATIONS", c("val_1","col_1", "val_2","col_2")]
  #Check for different values in val_1
  not_equal_val1<-codesheet_so[!duplicated(val_1),.N]
  if(not_equal_val1>1){
    stop("This is not a script error. The additional_concepts template has been filled out wrong. The columnn val_1 has different values between GDM_diagnoses, PE_diagnoses and Migraine_diagnoses for table set to SUREVY_OBSERVATIONS.") 
  }
  #Check for different values in col_1
  not_equal_col1<-codesheet_so[!duplicated(col_1),.N]
  if(not_equal_col1>1){
    stop("This is not a script error. The additional_concepts template has been filled out wrong. The columnn col_1 has different values between GDM_diagnoses, PE_diagnoses and Migraine_diagnoses for table set to SUREVY_OBSERVATIONS.") 
  }
  
  #Check for different values in val_2
  not_equal_val2<-codesheet_so[!duplicated(val_2),.N]
  if(not_equal_val2>1){
    stop("This is not a script error. The additional_concepts template has been filled out wrong. The columnn val_2 has different values between GDM_diagnoses, PE_diagnoses and Migraine_diagnoses for table set to SUREVY_OBSERVATIONS.") 
  }
  
  #Check for different values in col_2
  not_equal_col2<-codesheet_so[!duplicated(col_2),.N]
  if(not_equal_col2>1){
    stop("This is not a script error. The additional_concepts template has been filled out wrong. The columnn col_2 has different values between GDM_diagnoses, PE_diagnoses and Migraine_diagnoses for table set to SUREVY_OBSERVATIONS.") 
  }
  
  
  rm(codesheet_so)
}

####Medicines columns####
if(sum(c("GDM_medicines","Migraine_medicines") %in% additional_concepts[,StudyVar])>0){
  codesheet_med<-additional_concepts[StudyVar %in% c("GDM_medicines","Migraine_medicines") & table=="MEDICINES", c("val_1","col_1", "val_2","col_2")]
  #Check for different values in val_1
  not_equal_val1<-codesheet_med[!duplicated(val_1),.N]
  if(not_equal_val1>1){
    stop("This is not a script error. The additional_concepts template has been filled out wrong. The columnn val_1 has different values between GDM_medicines and Migraine_medicines for table set to MEDICINES.") 
  }
  #Check for different values in col_1
  not_equal_col1<-codesheet_med[!duplicated(col_1),.N]
  if(not_equal_col1>1){
    stop("This is not a script error. The additional_concepts template has been filled out wrong. The columnn col_1 has different values between GDM_medicines and Migraine_medicines for table set to MEDICINES.") 
  }
  
  #Check for different values in val_2
  not_equal_val2<-codesheet_med[!duplicated(val_2),.N]
  if(not_equal_val2>1){
    stop("This is not a script error. The additional_concepts template has been filled out wrong. The columnn val_2 has different values between GDM_medicines and Migraine_medicines for table set to MEDICINES.") 
  }
  
  #Check for different values in col_2
  not_equal_col2<-codesheet_med[!duplicated(col_2),.N]
  if(not_equal_col2>1){
    stop("This is not a script error. The additional_concepts template has been filled out wrong. The columnn col_2 has different values between GDM_medicines and Migraine_medicines for table set to MEDICINES.") 
  }
  
  
  rm(codesheet_med)
}
