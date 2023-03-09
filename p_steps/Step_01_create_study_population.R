#Load the Observation periods table

####load info, parameters, conceptsets####
source(paste0(pre_dir,"info/directory_info.R"))
source(paste0(pre_dir,"functions/CreateSpells_v15.R"))

print('Import and append observation periods files')
obs_table_files<-actual_tables$OBSERVATION_PERIODS

OBS_PER<-lapply(paste0(path_dir,obs_table_files),function(x) fread(x, colClasses = "character"))
OBS_PER<-as.data.table(rbindlist(OBS_PER))
OBS_PER[,op_origin:=NULL]
#create an op_end_date_max with the maximum between gdm_pe_end_study_date, mig_end_study_date, du_end_study_date, saf_end_study_date
#create an op_end_date_min with the minimum between gdm_pe_start_study_date, mig_start_study_date, du_start_study_date, saf_start_study_date
#Allows to run the CreateSpells once
op_end_date_max<-max(gdm_pe_end_study_date,mig_end_study_date,du_end_study_date,saf_end_study_date)
OBS_PER[, op_end_date_max:= ifelse(is.na(op_end_date),op_end_date_max,op_end_date)]
print('Set start and end date to date format')
lapply(c("op_start_date","op_end_date","op_end_date_max"), function (x) OBS_PER <- OBS_PER[,eval(x) := as.IDate(as.character(get(x)),"%Y%m%d")])

####Run create spells####
OBS_PER1 <- CreateSpells(
  dataset=OBS_PER,
  id="person_id" ,
  start_date = "op_start_date",
  end_date = "op_end_date_max",
  overlap = FALSE,
  only_overlaps = F
)
original_rows<-OBS_PER[,.N]
rm(OBS_PER)
after_run_create_spell<-OBS_PER1[,.N]
#Keep only one record per person(latest)
OBS_PER1 <- OBS_PER1[,temp := lapply(.SD, max), by = c("person_id"), .SDcols = "num_spell"][temp == num_spell,][,temp := NULL]
setnames(OBS_PER1, "entry_spell_category", "op_start_date")
setnames(OBS_PER1, "exit_spell_category", "op_end_date")
included_spells<-OBS_PER1[,.N]

#Fix dates of observation for each project
origin_date<-as.Date("2016-12-06") - as.numeric(as.Date("2016-12-06"))
#GDM_and_PE
OBS_PER1[, op_end_date_gdm_pe:= ifelse(op_end_date>=gdm_pe_end_study_date,gdm_pe_end_study_date,op_end_date)]
OBS_PER1[,op_end_date_gdm_pe:=as.Date.numeric(op_end_date_gdm_pe,origin_date)]
#Migraine
OBS_PER1[, op_end_date_mig:= ifelse(op_end_date>=mig_end_study_date,mig_end_study_date,op_end_date)]
OBS_PER1[,op_end_date_mig:=as.Date.numeric(op_end_date_mig,origin_date)]
#Drug_utilisation
OBS_PER1[, op_end_date_du:= ifelse(op_end_date>=du_end_study_date,du_end_study_date,op_end_date)]
OBS_PER1[,op_end_date_du:=as.Date.numeric(op_end_date_du,origin_date)]
#Safety
OBS_PER1[, op_end_date_saf:= ifelse(op_end_date>=saf_end_study_date,saf_end_study_date,op_end_date)]
OBS_PER1[,op_end_date_saf:=as.Date.numeric(op_end_date_saf,origin_date)]
#Remove op_end_date variable and op_origin
OBS_PER1[,op_end_date:=NULL]

#Do the same for op_start_date: If op_start_date before start study date replace with start study date
#GDM_and_PE
OBS_PER1[,op_start_date_gdm_pe:=ifelse(op_start_date<gdm_pe_start_study_date,gdm_pe_start_study_date,op_start_date)]
OBS_PER1[,op_start_date_gdm_pe:=as.Date.numeric(op_start_date_gdm_pe,origin_date)]
#Migraine
OBS_PER1[,op_start_date_mig:=ifelse(op_start_date<mig_start_study_date,mig_start_study_date,op_start_date)]
OBS_PER1[,op_start_date_mig:=as.Date.numeric(op_start_date_mig,origin_date)]
#Drug utilisation
OBS_PER1[,op_start_date_du:=ifelse(op_start_date<du_start_study_date,du_start_study_date,op_start_date)]
OBS_PER1[,op_start_date_du:=as.Date.numeric(op_start_date_du,origin_date)]
#Safety
OBS_PER1[,op_start_date_saf:=ifelse(op_start_date<saf_start_study_date,saf_start_study_date,op_start_date)]
OBS_PER1[,op_start_date_saf:=as.Date.numeric(op_start_date_saf,origin_date)]
#remove op_start_date
OBS_PER1[,op_start_date:=NULL]

#create obs_per flowchart
Indicator=c("Number of original records in the OBSERVATION_PERIODS table",
            "Number of records in the OBSERVATION_PERIODS table after cleaning up the spells",
            "Number of records in the OBSERVATION_PERIODS table after keeping one record per person(latest)")
placeholder<-c(original_rows, after_run_create_spell, included_spells)
flowchart_obs_per<-data.table(Indicator,no_records=placeholder)
rm(original_rows,after_run_create_spell,included_spells)
rm(Indicator,placeholder)

####Load PERSONS table####
persons_table_files<-actual_tables$PERSONS

PERSONS<-lapply(paste0(path_dir,persons_table_files),function(x) fread(x, colClasses = "character"))
PERSONS<-as.data.table(rbindlist(PERSONS))
lapply(c("race","country_of_birth","quality"), function (x) PERSONS <- PERSONS[,eval(x) := NULL])
original_rows_persons<-PERSONS[,.N]
#Number of records other than female
no_females<-PERSONS[sex_at_instance_creation!="F",.N]
PERSONS<-PERSONS[sex_at_instance_creation=="F"]
#empty year of birth
no_birth_year<-PERSONS[is.na(year_of_birth),.N]
PERSONS<-PERSONS[!is.na(year_of_birth)]
#empty year of death when date or month is present
no_death_year<-PERSONS[is.na(year_of_death) & (!is.na(day_of_death)|!is.na(month_of_death)),.N]
PERSONS<-PERSONS[!is.na(year_of_death) & (!is.na(day_of_death)|!is.na(month_of_death))]

dates_persons <- c("year_of_birth", "month_of_birth","day_of_birth","year_of_death", "month_of_death","day_of_death")
invisible(lapply(dates_persons, function (x) if (class(PERSONS[[x]]) != "integer") PERSONS[, eval(x) := as.integer(get(x)) ]))
#impute day only:birth
impt_day_birth<-PERSONS[is.na(day_of_birth) & (!is.na(month_of_birth)|!is.na(year_of_birth)),.N]
PERSONS<-PERSONS[is.na(day_of_birth) & (!is.na(month_of_birth)|!is.na(year_of_birth)), day_of_birth:=1]
#impute day only:death
impt_day_death<-PERSONS[is.na(day_of_death) & (!is.na(month_of_death)|!is.na(year_of_death)),.N]
PERSONS<-PERSONS[is.na(day_of_death) & (!is.na(month_of_death)|!is.na(year_of_death)), day_of_death:=1]
#impute day and month:birth
impt_dm_birth<-PERSONS[is.na(day_of_birth) & is.na(month_of_birth) & !is.na(year_of_birth),.N]
PERSONS<-PERSONS[is.na(day_of_birth) & is.na(month_of_birth) & !is.na(year_of_birth), c("day_of_birth","month_of_birth"):=c(1,7)]
#impute day and month:death
impt_dm_death<-PERSONS[is.na(day_of_death) & is.na(month_of_death) & !is.na(year_of_death),.N]
PERSONS<-PERSONS[is.na(day_of_death) & is.na(month_of_death) & !is.na(year_of_death), c("day_of_death","month_of_death"):=c(1,7)]

#Create birth date and death date
PERSONS[!is.na(day_of_birth) & !is.na(month_of_birth) & !is.na(year_of_birth),birth_date := as.IDate(paste0(year_of_birth, sprintf("%02d",month_of_birth),sprintf("%02d",day_of_birth)),"%Y%m%d")]
PERSONS[!is.na(day_of_death) & !is.na(month_of_death) & !is.na(year_of_death),death_date := as.IDate(paste0(year_of_death, sprintf("%02d",month_of_death),sprintf("%02d",day_of_death)),"%Y%m%d")]
incl_pers<-PERSONS[,.N]
lapply(c("day_of_birth", "month_of_birth","year_of_birth", "day_of_death","month_of_death","year_of_death"), function (x) PERSONS <- PERSONS[,eval(x) := NULL])
#Create flowchart persons
Indicator=c("Number of original records in the PERSONS table",
            "Number of records in the PERSONS table with sex other than female",
            "Number of records in the PERSONS table with empty year of birth",
            "Number of records in the PERSONS table with empty year of death, when date or/and month of death are present",
            "Number of records in the PERSONS table where only day of birth was imputed",
            "Number of records in the PERSONS table where only day of death was imputed",
            "Number of records in the PERSONS table where both day and month of birth were imputed",
            "Number of records in the PERSONS table where both day and month of death were imputed",
            "Number of records left in the PERSONS table")
placeholder<-c(original_rows_persons, no_females, no_birth_year, no_death_year,impt_day_birth,impt_day_death,impt_dm_birth,impt_dm_death,incl_pers)
flowchart<-data.table(Indicator,no_records=placeholder)
rm(original_rows_persons,no_females,no_birth_year,no_death_year,impt_day_birth,impt_day_death,impt_dm_birth,impt_dm_death,incl_pers)
rm(Indicator,placeholder)

#Combine flowchart
flowchart<-rbind(flowchart_obs_per,flowchart)
rm(flowchart_obs_per)

fwrite(flowchart, paste0(output_dir,"Pregnancy study population/flowchart_same_criteria_study_population.csv"), row.names = F)
rm(flowchart)
####Merge Observation periods with PERSONS####
PERSONS<-merge.data.table(PERSONS,OBS_PER1, by="person_id", all.x=T)
rm(OBS_PER1)
PERSONS[,num_spell:=NULL]
#Check number of persons present in PERSONS table but that are missing in OBS_PER
#GDM_and_PE
not_present_in_obs_gdm_pe<-PERSONS[!is.na(sex_at_instance_creation) & is.na(op_start_date_gdm_pe),.N]
#Migraine
not_present_in_obs_mig<-PERSONS[!is.na(sex_at_instance_creation) & is.na(op_start_date_mig),.N]
#Drug utilisation
not_present_in_obs_du<-PERSONS[!is.na(sex_at_instance_creation) & is.na(op_start_date_du),.N]
#Safety
not_present_in_obs_saf<-PERSONS[!is.na(sex_at_instance_creation) & is.na(op_start_date_saf),.N]

Indicator=c("Number of records present in PERSONS but not in OBSERVATION_PERIODS")
flowchart_obs_pers<-data.table(Indicator,GDM_and_PE=not_present_in_obs_gdm_pe,Migraine=not_present_in_obs_mig,Drug_utilisation=not_present_in_obs_du,Safety=not_present_in_obs_saf)
rm(not_present_in_obs_gdm_pe,not_present_in_obs_mig,not_present_in_obs_du,not_present_in_obs_saf)
rm(Indicator)

#Update end date for each study if the death date is before
lapply(c("birth_date","death_date","op_end_date_gdm_pe","op_end_date_mig","op_end_date_du","op_end_date_saf","op_start_date_gdm_pe","op_start_date_mig","op_start_date_du","op_start_date_saf"), function (x) PERSONS <- PERSONS[,eval(x) := as.IDate(get(x))])

PERSONS[!is.na(sex_at_instance_creation) & !is.na(op_start_date_gdm_pe), gdm_pe_filter:=1]
PERSONS[!is.na(sex_at_instance_creation) & !is.na(op_start_date_mig), mig_filter:=1]
PERSONS[!is.na(sex_at_instance_creation) & !is.na(op_start_date_du), du_filter:=1]
PERSONS[!is.na(sex_at_instance_creation) & !is.na(op_start_date_saf), saf_filter:=1]

#Fix end dates
#GDM_and_PE
PERSONS[,dif:=death_date-op_end_date_gdm_pe]
PERSONS[dif<0,op_end_date_gdm_pe:=death_date]
PERSONS[,dif:=NULL]
#Migraine
PERSONS[,dif:=death_date-op_end_date_mig]
PERSONS[dif<0,op_end_date_mig:=death_date]
PERSONS[,dif:=NULL]
#Drug utilisation
PERSONS[,dif:=death_date-op_end_date_du]
PERSONS[dif<0,op_end_date_du:=death_date]
PERSONS[,dif:=NULL]
#Safety
PERSONS[,dif:=death_date-op_end_date_saf]
PERSONS[dif<0,op_end_date_saf:=death_date]
PERSONS[,dif:=NULL]

#Check for at least one day of follow up
#GDM_and_PE
no_fup_gdm_pe<-PERSONS[gdm_pe_filter==1 & op_end_date_gdm_pe<=op_start_date_gdm_pe,.N]
PERSONS[gdm_pe_filter==1 & op_end_date_gdm_pe<=op_start_date_gdm_pe,gdm_pe_filter:=NA]
#Migraine
no_fup_mig<-PERSONS[mig_filter==1 & op_end_date_mig<=op_start_date_mig,.N]
PERSONS[mig_filter==1 & op_end_date_mig<=op_start_date_mig,mig_filter:=NA]
#Drug utilisation
no_fup_du<-PERSONS[du_filter==1 & op_end_date_du<=op_start_date_du,.N]
PERSONS[du_filter==1 & op_end_date_du<=op_start_date_du,du_filter:=NA]
#Safety
no_fup_saf<-PERSONS[saf_filter==1 & op_end_date_saf<=op_start_date_saf,.N]
PERSONS[saf_filter==1 & op_end_date_saf<=op_start_date_saf,saf_filter:=NA]


Indicator<-c("Number of records with follow up <1 day")
flowchart_separate<-data.table(Indicator,GDM_and_PE=no_fup_gdm_pe,Migraine=no_fup_mig,Drug_utilisation=no_fup_du,Safety=no_fup_saf)
rm(no_fup_gdm_pe,no_fup_mig,no_fup_du,no_fup_saf)
rm(Indicator)


flowchart_separate<-rbind(flowchart_obs_pers,flowchart_separate)
rm(flowchart_obs_pers)

#Update the persons table by removing all uneccessary rows
PERSONS[gdm_pe_filter==1 | mig_filter==1 | du_filter==1 | saf_filter==1,all_filter:=1]
PERSONS<-PERSONS[!is.na(sex_at_instance_creation) & all_filter==1]
PERSONS[,all_filter:=NULL]

#impossible birthdates
#GDM_and_PE
PERSONS[gdm_pe_filter==1 & !is.na(op_end_date_gdm_pe),age:=floor((op_end_date_gdm_pe-birth_date)/365.25)]
impossible_birth_gdm_pe<-PERSONS[age>120, .N]
PERSONS[age>120, gdm_pe_filter:=NA]
PERSONS[,age:=NULL]
#Migraine
PERSONS[mig_filter==1 & !is.na(op_end_date_mig),age:=floor((op_end_date_mig-birth_date)/365.25)]
impossible_birth_mig<-PERSONS[age>120, .N]
PERSONS[age>120, mig_filter:=NA]
PERSONS[,age:=NULL]
#Drug utilisation
PERSONS[du_filter==1 & !is.na(op_end_date_du),age:=floor((op_end_date_du-birth_date)/365.25)]
impossible_birth_du<-PERSONS[age>120, .N]
PERSONS[age>120, du_filter:=NA]
PERSONS[,age:=NULL]
#Safety
PERSONS[saf_filter==1 & !is.na(op_end_date_saf),age:=floor((op_end_date_saf-birth_date)/365.25)]
impossible_birth_saf<-PERSONS[age>120, .N]
PERSONS[age>120, saf_filter:=NA]
PERSONS[,age:=NULL]

Indicator<-c("Number of subjects with age>120 years old at end follow up")
flowchart_birth_imp<-data.table(Indicator,GDM_and_PE=impossible_birth_gdm_pe,Migraine=impossible_birth_mig,Drug_utilisation=impossible_birth_du,Safety=impossible_birth_saf)
rm(impossible_birth_gdm_pe,impossible_birth_mig,impossible_birth_du,impossible_birth_saf)
rm(Indicator)

flowchart_separate<-rbind(flowchart_separate,flowchart_birth_imp)
rm(flowchart_birth_imp)

#rename filter variable
setnames(PERSONS,"gdm_pe_filter","gdm_pe_filter_persons")
setnames(PERSONS,"mig_filter","mig_filter_persons")
setnames(PERSONS,"du_filter","du_filter_persons")
setnames(PERSONS,"saf_filter","saf_filter_persons")

####Load pregnancies df####
pregnancy_D3<-readRDS(paste0(projectFolder,"/g_intermediate/pregnancy_algorithm/pregnancy_D3.rds"))

####Merge pregnancy with persons table####
pregnancy_D3<-merge.data.table(pregnancy_D3,PERSONS, by="person_id", allow.cartesian = T, all.x = T)
rm(PERSONS)

#Check for subjects present in pregnancy D3 but not in PERSONS
#GDM and PE
preg_not_in_persons_gdm_pe<-pregnancy_D3[gdm_pe_filter==1 & is.na(sex_at_instance_creation),.N]
pregnancy_D3[gdm_pe_filter==1 & is.na(sex_at_instance_creation), gdm_pe_filter:=NA]
#Migraine
preg_not_in_persons_mig<-pregnancy_D3[mig_filter==1  & is.na(sex_at_instance_creation),.N]
pregnancy_D3[mig_filter==1 & is.na(sex_at_instance_creation), mig_filter:=NA]
#Drug utilisation
preg_not_in_persons_du<-pregnancy_D3[du_filter==1  & is.na(sex_at_instance_creation),.N]
pregnancy_D3[du_filter==1 & is.na(sex_at_instance_creation), du_filter:=NA]
#Safety
preg_not_in_persons_saf<-pregnancy_D3[saf_filter==1  & is.na(sex_at_instance_creation),.N]
pregnancy_D3[saf_filter==1 & is.na(sex_at_instance_creation), saf_filter:=NA]

Indicator<-c("Number of subjects present in the pregnancy D3 but missing in the PERSONS table")
flowchart_pers_mis<-data.table(Indicator,GDM_and_PE=preg_not_in_persons_gdm_pe,Migraine=preg_not_in_persons_mig,Drug_utilisation=preg_not_in_persons_du,Safety=preg_not_in_persons_saf)
rm(preg_not_in_persons_gdm_pe,preg_not_in_persons_mig,preg_not_in_persons_du,preg_not_in_persons_saf)
rm(Indicator)

flowchart_separate<-rbind(flowchart_separate,flowchart_pers_mis)
rm(flowchart_pers_mis)

#Number of pregnancies after merging with the PERSONS table
#GDM and PE
preg_incl_gdm_pe<-pregnancy_D3[gdm_pe_filter==1 & gdm_pe_filter_persons==1,.N]
#Migraine
preg_incl_mig<-pregnancy_D3[mig_filter==1 & mig_filter_persons==1,.N]
#Drug utilisation
preg_incl_du<-pregnancy_D3[du_filter==1 & du_filter_persons==1,.N]
#Safety
preg_incl_saf<-pregnancy_D3[saf_filter==1 & saf_filter_persons==1,.N]

Indicator<-c("Number of pregnancies included after merging with the PERSONS table")
flowchart_incl_preg<-data.table(Indicator,GDM_and_PE=preg_incl_gdm_pe,Migraine=preg_incl_mig,Drug_utilisation=preg_incl_du,Safety=preg_incl_saf)
rm(preg_incl_gdm_pe,preg_incl_mig,preg_incl_du,preg_incl_saf)
rm(Indicator)

flowchart_separate<-rbind(flowchart_separate,flowchart_incl_preg)
rm(flowchart_incl_preg)

#Update the event filter by combining with persons filter
pregnancy_D3[gdm_pe_filter==1 & is.na(gdm_pe_filter_persons), gdm_pe_filter:=NA]
pregnancy_D3[mig_filter==1 & is.na(mig_filter_persons), mig_filter:=NA]
pregnancy_D3[du_filter==1 & is.na(du_filter_persons), du_filter:=NA]
pregnancy_D3[saf_filter==1 & is.na(saf_filter_persons), saf_filter:=NA]
pregnancy_D3[,gdm_pe_filter_persons:=NULL][,mig_filter_persons:=NULL][,du_filter_persons:=NULL][,saf_filter_persons:=NULL]

#Check if we have pregnancy records before min pregnancy start date
pregnancy_D3[,pregnancy_start_date:=as.IDate(pregnancy_start_date)][,pregnancy_end_date:=as.IDate(pregnancy_end_date)]
#GDM and PE
pregnancy_D3[gdm_pe_filter==1,min_preg_date_gdm_pe:=as.IDate(min_preg_date_gdm_pe)]
pregnancy_D3[gdm_pe_filter==1,dif:=min_preg_date_gdm_pe-pregnancy_start_date]
preg_before_start_gdm_pe<-pregnancy_D3[dif>0,.N]
pregnancy_D3[dif>0, gdm_pe_filter:=NA]
pregnancy_D3[,dif:=NULL][,min_preg_date_gdm_pe:=NULL]
#Migraine
pregnancy_D3[mig_filter==1,min_preg_date_mig:=as.IDate(min_preg_date_mig)]
pregnancy_D3[mig_filter==1,dif:=min_preg_date_mig-pregnancy_start_date]
preg_before_start_mig<-pregnancy_D3[dif>0,.N]
pregnancy_D3[dif>0, mig_filter:=NA]
pregnancy_D3[,dif:=NULL][,min_preg_date_mig:=NULL]
#Drug utilisation
pregnancy_D3[du_filter==1,min_preg_date_du:=as.IDate(min_preg_date_du)]
pregnancy_D3[du_filter==1,dif:=min_preg_date_du-pregnancy_start_date]
preg_before_start_du<-pregnancy_D3[dif>0,.N]
pregnancy_D3[dif>0, du_filter:=NA]
pregnancy_D3[,dif:=NULL][,min_preg_date_du:=NULL]
#Safety
pregnancy_D3[saf_filter==1,min_preg_date_saf:=as.IDate(min_preg_date_gdm_pe)]
pregnancy_D3[saf_filter==1,dif:=min_preg_date_saf-pregnancy_start_date]
preg_before_start_saf<-pregnancy_D3[dif>0,.N]
pregnancy_D3[dif>0, saf_filter:=NA]
pregnancy_D3[,dif:=NULL][,min_preg_date_saf:=NULL]

Indicator<-c("Number of pregnancies with pregnancy start date before the minimum pregnancy date")
flowchart_min_preg<-data.table(Indicator,GDM_and_PE=preg_before_start_gdm_pe,Migraine=preg_before_start_mig,Drug_utilisation=preg_before_start_du,Safety=preg_before_start_saf)
rm(preg_before_start_gdm_pe,preg_before_start_mig,preg_before_start_du,preg_before_start_saf)
rm(Indicator)

flowchart_separate<-rbind(flowchart_separate,flowchart_min_preg)
rm(flowchart_min_preg)

#Number of pregnancies with start date after max pregnancy start date
#GDM and PE
pregnancy_D3[gdm_pe_filter==1,max_preg_date_gdm_pe:=as.IDate(max_preg_date_gdm_pe)]
pregnancy_D3[gdm_pe_filter==1,dif:=max_preg_date_gdm_pe-pregnancy_start_date]
preg_after_max_gdm_pe<-pregnancy_D3[dif<=0,.N]
pregnancy_D3[dif<=0, gdm_pe_filter:=NA]
pregnancy_D3[,dif:=NULL][,max_preg_date_gdm_pe:=NULL]
#Migraine
pregnancy_D3[mig_filter==1,max_preg_date_mig:=as.IDate(max_preg_date_mig)]
pregnancy_D3[mig_filter==1,dif:=max_preg_date_mig-pregnancy_start_date]
preg_after_max_mig<-pregnancy_D3[dif<=0,.N]
pregnancy_D3[dif<=0, mig_filter:=NA]
pregnancy_D3[,dif:=NULL][,max_preg_date_mig:=NULL]
#Drug utilisation
pregnancy_D3[du_filter==1,max_preg_date_du:=as.IDate(max_preg_date_du)]
pregnancy_D3[du_filter==1,dif:=max_preg_date_du-pregnancy_start_date]
preg_after_max_du<-pregnancy_D3[dif<=0,.N]
pregnancy_D3[dif<=0, du_filter:=NA]
pregnancy_D3[,dif:=NULL][,max_preg_date_du:=NULL]
#Safety
pregnancy_D3[saf_filter==1,max_preg_date_saf:=as.IDate(max_preg_date_saf)]
pregnancy_D3[saf_filter==1,dif:=max_preg_date_saf-pregnancy_start_date]
preg_after_max_saf<-pregnancy_D3[dif<=0,.N]
pregnancy_D3[dif<=0, saf_filter:=NA]
pregnancy_D3[,dif:=NULL][,max_preg_date_saf:=NULL]

Indicator<-c("Number of pregnancies with pregnancy start date after the maximum pregnancy date")
flowchart_max_preg<-data.table(Indicator,GDM_and_PE=preg_after_max_gdm_pe,Migraine=preg_after_max_mig,Drug_utilisation=preg_after_max_du,Safety=preg_after_max_saf)
rm(preg_after_max_gdm_pe,preg_after_max_mig,preg_after_max_du,preg_after_max_saf)
rm(Indicator)

flowchart_separate<-rbind(flowchart_separate,flowchart_max_preg)
rm(flowchart_max_preg)

#Age at start pregnancy 15:49
pregnancy_D3[,age:=floor((pregnancy_start_date-birth_date)/365.25)]

#GDM and PE
age_before_15_gdm_pe<-pregnancy_D3[gdm_pe_filter==1 & age< min_age_preg,.N]
pregnancy_D3[gdm_pe_filter==1 & age< min_age_preg, gdm_pe_filter:=NA]
#Migraine
age_before_15_mig<-pregnancy_D3[mig_filter==1 & age< min_age_preg,.N]
pregnancy_D3[mig_filter==1 & age< min_age_preg, mig_filter:=NA]
#Drug utilisation
age_before_15_du<-pregnancy_D3[du_filter==1 & age< min_age_preg,.N]
pregnancy_D3[du_filter==1 & age< min_age_preg, du_filter:=NA]
#Safety
age_before_15_saf<-pregnancy_D3[saf_filter==1 & age< min_age_preg,.N]
pregnancy_D3[saf_filter==1 & age< min_age_preg, saf_filter:=NA]

Indicator<-c(paste0("Number of pregnancies with age at pregnancy start date < ", min_age_preg, "years old"))
flowchart_min_age<-data.table(Indicator,GDM_and_PE=age_before_15_gdm_pe,Migraine=age_before_15_mig,Drug_utilisation=age_before_15_du,Safety=age_before_15_saf)
rm(age_before_15_gdm_pe,age_before_15_mig,age_before_15_du,age_before_15_saf)
rm(Indicator)

flowchart_separate<-rbind(flowchart_separate,flowchart_min_age)
rm(flowchart_min_age)

#GDM and PE
age_after_49_gdm_pe<-pregnancy_D3[gdm_pe_filter==1 & age> max_age_preg,.N]
pregnancy_D3[gdm_pe_filter==1 & age> max_age_preg, gdm_pe_filter:=NA]
#Migraine
age_after_49_mig<-pregnancy_D3[mig_filter==1 & age>max_age_preg,.N]
pregnancy_D3[mig_filter==1 & age> max_age_preg, mig_filter:=NA]
#Drug utilisation
age_after_49_du<-pregnancy_D3[du_filter==1 & age> max_age_preg,.N]
pregnancy_D3[du_filter==1 & age> max_age_preg, du_filter:=NA]
#Safety
age_after_49_saf<-pregnancy_D3[saf_filter==1 & age> max_age_preg,.N]
pregnancy_D3[saf_filter==1 & age> max_age_preg, saf_filter:=NA]

Indicator<-c(paste0("Number of pregnancies with age at pregnancy start date > ", max_age_preg, "years old"))
flowchart_max_age<-data.table(Indicator,GDM_and_PE=age_after_49_gdm_pe,Migraine=age_after_49_mig,Drug_utilisation=age_after_49_du,Safety=age_after_49_saf)
rm(age_after_49_gdm_pe,age_after_49_mig,age_after_49_du,age_after_49_saf)
rm(Indicator)

flowchart_separate<-rbind(flowchart_separate,flowchart_max_age)
rm(flowchart_max_age)

####Apply specific criteria to each project####



fwrite(flowchart_separate, paste0(output_dir,"Pregnancy study population/flowchart_specific_criteria_study_population"), row.names = F)
rm(flowchart_separate)