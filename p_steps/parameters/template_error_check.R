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