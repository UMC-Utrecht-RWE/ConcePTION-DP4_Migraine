#Create concept sets

#####GDM####
#GDM diagnoses
codelist_gdm<-fread(paste0(pre_dir,"codelists/gestational_diabetes_codelist.csv"), colClasses = "character")
#select only necessary columns
codelist_gdm<-codelist_gdm[,c("event_abbreviation", "coding_system", "code","tags")]
codelist_gdm<-codelist_gdm[,coding_system:=gsub("/","",coding_system)]
#Create variable code_no_dot by removing dot from all codes
codelist_gdm[,code_no_dot:=gsub("\\.","",codelist_gdm[,code])]
vocabularies_list_gdm<-codelist_gdm[!duplicated(coding_system), coding_system]
conditions_gdm<-vector(mode="list", length=length(unique(na.omit(codelist_gdm[,event_abbreviation]))))
names(conditions_gdm)<-unique(na.omit(codelist_gdm[,event_abbreviation]))
for (i in 1:length(conditions_gdm)){
  vocabularies<-vector(mode="list", length=length(unique(na.omit(codelist_gdm[,coding_system]))))
  names(vocabularies)<-unique(na.omit(codelist_gdm[,coding_system]))
  for (j in 1:length(vocabularies)){
    vocabularies[[j]]<-codelist_gdm[event_abbreviation==names(conditions_gdm)[i] & coding_system==names(vocabularies)[j], code_no_dot]
  }
  conditions_gdm[[i]]<-list.append(conditions_gdm[[i]],vocabularies)
  rm(vocabularies)
}

#remove empty vocabularies
conditions_gdm<-lapply(conditions_gdm, function(x) Filter(length, x))

####PE####
#PE diagnoses
codelist_pe<-fread(paste0(pre_dir,"codelists/pre_eclampsia_codelist.csv"), colClasses = "character")
#select only necessary columns
codelist_pe<-codelist_pe[,c("event_abbreviation", "coding_system", "code","tags")]
codelist_pe<-codelist_pe[,coding_system:=gsub("/","",coding_system)]
#Create variable code_no_dot by removing dot from all codes
codelist_pe[,code_no_dot:=gsub("\\.","",codelist_pe[,code])]
vocabularies_list_pe<-codelist_pe[!duplicated(coding_system), coding_system]
conditions_pe<-vector(mode="list", length=length(unique(na.omit(codelist_pe[,event_abbreviation]))))
names(conditions_pe)<-unique(na.omit(codelist_pe[,event_abbreviation]))
for (i in 1:length(conditions_pe)){
  vocabularies<-vector(mode="list", length=length(unique(na.omit(codelist_pe[,coding_system]))))
  names(vocabularies)<-unique(na.omit(codelist_pe[,coding_system]))
  for (j in 1:length(vocabularies)){
    vocabularies[[j]]<-codelist_pe[event_abbreviation==names(conditions_pe)[i] & coding_system==names(vocabularies)[j], code_no_dot]
  }
  conditions_pe[[i]]<-list.append(conditions_pe[[i]],vocabularies)
  rm(vocabularies)
}

#remove empty vocabularies
conditions_pe<-lapply(conditions_pe, function(x) Filter(length, x))

####Migraine####
#Migraine diagnoses
codelist_migraine<-fread(paste0(pre_dir,"codelists/migraine_codelist.csv"), colClasses = "character")
#select only necessary columns
codelist_migraine<-codelist_migraine[,c("event_abbreviation", "coding_system", "code","tags")]
codelist_migraine<-codelist_migraine[,coding_system:=gsub("/","",coding_system)]
#Create variable code_no_dot by removing dot from all codes
codelist_migraine[,code_no_dot:=gsub("\\.","",codelist_migraine[,code])]
vocabularies_list_migraine<-codelist_migraine[!duplicated(coding_system), coding_system]
conditions_migraine<-vector(mode="list", length=length(unique(na.omit(codelist_migraine[,event_abbreviation]))))
names(conditions_migraine)<-unique(na.omit(codelist_migraine[,event_abbreviation]))
for (i in 1:length(conditions_migraine)){
  vocabularies<-vector(mode="list", length=length(unique(na.omit(codelist_migraine[,coding_system]))))
  names(vocabularies)<-unique(na.omit(codelist_migraine[,coding_system]))
  for (j in 1:length(vocabularies)){
    vocabularies[[j]]<-codelist_migraine[event_abbreviation==names(conditions_migraine)[i] & coding_system==names(vocabularies)[j], code_no_dot]
  }
  conditions_migraine[[i]]<-list.append(conditions_migraine[[i]],vocabularies)
  rm(vocabularies)
}

#remove empty vocabularies
conditions_migraine<-lapply(conditions_migraine, function(x) Filter(length, x))
