
initial_time<-Sys.time()
date_running_start<-Sys.Date()

#Clean folders
unlink(paste0(projectFolder,"/g_intermediate/tmp"), recursive = T)#delete folder
dir.create(paste0(projectFolder, "/g_intermediate/tmp"))

unlink(paste0(projectFolder,"/g_intermediate/pregnancy_d3"), recursive = T)#delete folder
dir.create(paste0(projectFolder, "/g_intermediate/pregnancy_d3"))

unlink(paste0(projectFolder,"/g_output/Pregnancy study population"), recursive = T)#delete folder
dir.create(paste0(projectFolder, "/g_output/Pregnancy study population"))


#Load the Observation periods table
####load info, parameters, conceptsets####
source(paste0(pre_dir,"info/directory_info.R"))
source(paste0(pre_dir,"functions/CreateSpells_v15.R"))

print('Import and append observation periods files')
obs_table_files<-actual_tables$OBSERVATION_PERIODS

OBS_PER<-lapply(paste0(path_dir,obs_table_files),function(x) fread(x, colClasses = "character"))
OBS_PER<-as.data.table(rbindlist(OBS_PER))
OBS_PER[,op_origin:=NULL]
OBS_PER<-OBS_PER[, lapply(.SD, FUN=function(x) gsub("^$|^ $", NA, x))] #make sure missing data is read appropriately

#create an op_end_date_max with the maximum between gdm_pe_end_study_date, mig_end_study_date, du_end_study_date, saf_end_study_date
#create an op_end_date_min with the minimum between gdm_pe_start_study_date, mig_start_study_date, du_start_study_date, saf_start_study_date
#Allows to run the CreateSpells once
OBS_PER[,op_end_date_max:=as.character()]
op_end_date_max<-max(gdm_pe_end_study_date,mig_end_study_date,du_end_study_date,saf_end_study_date)
end_date_max <- paste0(year(op_end_date_max),sprintf("%02d",month(op_end_date_max)),sprintf("%02d",day(op_end_date_max)))
OBS_PER[, op_end_date_max:= as.character(op_end_date)]
OBS_PER[is.na(op_end_date), op_end_date_max:= as.character(end_date_max)]

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
#identif duplicated person id
OBS_PER[duplicated(person_id), dup:=1]
OBS_PER<-merge.data.table(OBS_PER, OBS_PER[dup==1,c("person_id","dup")], by="person_id", all.x=T, allow.cartesian = T)
OBS_PER[,dup.x:=NULL]

#no_of_subjects with 1 spell
one_spell_org<-OBS_PER[is.na(dup.y),.N]
more_than_one_org<-OBS_PER[!is.na(dup.y) & !duplicated(person_id),.N]
rm(OBS_PER)
after_run_create_spell<-OBS_PER1[,.N]
OBS_PER1[duplicated(person_id), dup:=1]
OBS_PER1<-merge.data.table(OBS_PER1, OBS_PER1[dup==1,c("person_id","dup")], by="person_id", all.x=T, allow.cartesian = T)
OBS_PER1[,dup.x:=NULL]
one_spell_1<-OBS_PER1[is.na(dup.y),.N]
more_than_one_1<-OBS_PER1[!is.na(dup.y) & !duplicated(person_id),.N]
OBS_PER1[,dup.y:=NULL]
#Keep only one record per person(latest)
OBS_PER1 <- OBS_PER1[,temp := lapply(.SD, max), by = c("person_id"), .SDcols = "num_spell"][temp == num_spell,][,temp := NULL]
setnames(OBS_PER1, "entry_spell_category", "op_start_date")
setnames(OBS_PER1, "exit_spell_category", "op_end_date")
included_spells<-OBS_PER1[,.N]
OBS_PER1[duplicated(person_id), dup:=1]
OBS_PER1<-merge.data.table(OBS_PER1, OBS_PER1[dup==1,c("person_id","dup")], by="person_id", all.x=T, allow.cartesian = T)
OBS_PER1[,dup.x:=NULL]
one_spell_1a<-OBS_PER1[is.na(dup.y),.N]
more_than_one_1a<-OBS_PER1[!is.na(dup.y) & !duplicated(person_id),.N]
OBS_PER1[,dup.y:=NULL]

#Create the overview
spells_overview<-data.table(Indicator=c("1.0. Number of subjects with one observation period",
                                        "1.1. Number of subjects with more than one observation period"),
                            Original_table=c(one_spell_org,more_than_one_org),
                            After_CreateSpells=c(one_spell_1,more_than_one_1),
                            After_Selection=c(one_spell_1a,more_than_one_1a))
rm(one_spell_org,more_than_one_org,one_spell_1,more_than_one_1,one_spell_1a,more_than_one_1a)
fwrite(spells_overview, paste0(output_dir,"Pregnancy study population/Step_01_spells_overview.csv"), row.names = F)
rm(spells_overview)

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
Indicator=c("2.0. Number of original records in the OBSERVATION_PERIODS table",
            "2.1. Number of records in the OBSERVATION_PERIODS table after cleaning up the spells",
            "2.2. Number of records in the OBSERVATION_PERIODS table after keeping one record per person(latest)")
placeholder<-c(original_rows, after_run_create_spell, included_spells)
flowchart_obs_per<-data.table(Indicator,no_records=placeholder)
rm(original_rows,after_run_create_spell,included_spells)
rm(Indicator,placeholder)

####Load PERSONS table####
persons_table_files<-actual_tables$PERSONS

PERSONS<-lapply(paste0(path_dir,persons_table_files),function(x) fread(x, colClasses = "character"))
PERSONS<-as.data.table(rbindlist(PERSONS))
lapply(c("race","country_of_birth","quality"), function (x) PERSONS <- PERSONS[,eval(x) := NULL])
PERSONS<-PERSONS[, lapply(.SD, FUN=function(x) gsub("^$|^ $", NA, x))] #make sure missing data is read appropriately
original_rows_persons<-PERSONS[,.N]
#Number of records other than female
no_females<-PERSONS[sex_at_instance_creation!="F",.N]
PERSONS<-PERSONS[sex_at_instance_creation=="F"]
#empty year of birth
no_birth_year<-PERSONS[is.na(year_of_birth),.N]
PERSONS<-PERSONS[!is.na(year_of_birth)]
#empty year of death when date or month is present
PERSONS[!is.na(day_of_death) | !is.na(month_of_death), dm:=1]
PERSONS[is.na(year_of_death) & dm==1, remove:=1]
no_death_year<-PERSONS[remove==1,.N]
PERSONS<-PERSONS[is.na(remove)]
PERSONS[,dm:=NULL][,remove:=NULL]

dates_persons <- c("year_of_birth", "month_of_birth","day_of_birth","year_of_death", "month_of_death","day_of_death")
invisible(lapply(dates_persons, function (x) if (class(PERSONS[[x]]) != "integer") PERSONS[, eval(x) := as.integer(get(x)) ]))
#impute day only:birth
impt_day_birth<-PERSONS[is.na(day_of_birth) & !is.na(month_of_birth) & !is.na(year_of_birth),.N]
PERSONS<-PERSONS[is.na(day_of_birth) & !is.na(month_of_birth) & !is.na(year_of_birth), day_of_birth:=1]
#impute day only:death
impt_day_death<-PERSONS[is.na(day_of_death) & !is.na(month_of_death) & !is.na(year_of_death),.N]
PERSONS<-PERSONS[is.na(day_of_death) & !is.na(month_of_death) & !is.na(year_of_death), day_of_death:=1]
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
Indicator=c("3.0. Number of original records in the PERSONS table",
            "3.1. Number of records in the PERSONS table with sex other than female",
            "3.2. Number of records in the PERSONS table with empty year of birth",
            "3.3. Number of records in the PERSONS table with empty year of death, when date or/and month of death are present",
            "3.4. Number of records in the PERSONS table where only day of birth was imputed",
            "3.5. Number of records in the PERSONS table where only day of death was imputed",
            "3.6. Number of records in the PERSONS table where both day and month of birth were imputed",
            "3.7. Number of records in the PERSONS table where both day and month of death were imputed",
            "3.8. Number of records left in the PERSONS table")
placeholder<-c(original_rows_persons, no_females, no_birth_year, no_death_year,impt_day_birth,impt_day_death,impt_dm_birth,impt_dm_death,incl_pers)
flowchart<-data.table(Indicator,no_records=placeholder)
rm(original_rows_persons,no_females,no_birth_year,no_death_year,impt_day_birth,impt_day_death,impt_dm_birth,impt_dm_death,incl_pers)
rm(Indicator,placeholder)

#Combine flowchart
flowchart<-rbind(flowchart_obs_per,flowchart)
rm(flowchart_obs_per)

fwrite(flowchart, paste0(output_dir,"Pregnancy study population/Step_01_flowchart_same_criteria_study_population.csv"), row.names = F)
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

Indicator=c("4.0. Number of records present in PERSONS but not in OBSERVATION_PERIODS")
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


Indicator<-c("4.1. Number of records with follow up <1 day")
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

Indicator<-c("4.2. Number of subjects with age>120 years old at end follow up")
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

Indicator<-c("5.0. Number of subjects present in the pregnancy D3 but missing in the PERSONS table")
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

Indicator<-c("5.1. Number of pregnancies included after merging with the PERSONS table")
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

Indicator<-c("5.2. Number of pregnancies with pregnancy start date before the minimum pregnancy date")
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

Indicator<-c("5.3. Number of pregnancies with pregnancy start date after the maximum pregnancy date")
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

Indicator<-c(paste0("5.4. Number of pregnancies with age at pregnancy start date < ", min_age_preg, "years old"))
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

Indicator<-c(paste0("5.5. Number of pregnancies with age at pregnancy start date > ", max_age_preg, "years old"))
flowchart_max_age<-data.table(Indicator,GDM_and_PE=age_after_49_gdm_pe,Migraine=age_after_49_mig,Drug_utilisation=age_after_49_du,Safety=age_after_49_saf)
rm(age_after_49_gdm_pe,age_after_49_mig,age_after_49_du,age_after_49_saf)
rm(Indicator)

flowchart_separate<-rbind(flowchart_separate,flowchart_max_age)
rm(flowchart_max_age)

####Apply specific criteria to each project####
#Check if the records have at least 3 months of lookback
lookback_3_months<-3*30
lookback_12_months<-12*30
#GDM_and_PE
pregnancy_D3[gdm_pe_filter==1,dif:=pregnancy_start_date-op_start_date_gdm_pe]
less_than_3_month_gdm_pe<-pregnancy_D3[gdm_pe_filter==1 & dif<lookback_3_months,.N]
pregnancy_D3[gdm_pe_filter==1 & dif<lookback_3_months,gdm_pe_filter:=NA]
pregnancy_D3[,dif:=NULL]
included_rec_gdm_pe<-pregnancy_D3[gdm_pe_filter==1,.N]
pregnancy_D3[gdm_pe_filter==1,dif:=pregnancy_start_date-op_start_date_gdm_pe]
lookback_12_month_gdm_pe<-pregnancy_D3[gdm_pe_filter==1 & dif>=lookback_12_months,.N]
pregnancy_D3[,dif:=NULL]
#Migraine
pregnancy_D3[mig_filter==1,dif:=pregnancy_start_date-op_start_date_mig]
less_than_3_month_mig<-pregnancy_D3[mig_filter==1 & dif<lookback_3_months,.N]
pregnancy_D3[mig_filter==1 & dif<lookback_3_months,mig_filter:=NA]
pregnancy_D3[,dif:=NULL]
included_rec_mig<-pregnancy_D3[mig_filter==1,.N]
pregnancy_D3[mig_filter==1,dif:=pregnancy_start_date-op_start_date_mig]
lookback_12_month_mig<-pregnancy_D3[mig_filter==1 & dif>=lookback_12_months,.N]
pregnancy_D3[,dif:=NULL]
#Check number of records to be included when the lookback is 5 years
#mig_start_preg_lookback
#calculate all pregnancy records that start after mig_start_preg_lookback
mig_start_preg_lookback<-as.IDate(mig_start_preg_lookback)
incl_5_years<-pregnancy_D3[mig_filter==1 & pregnancy_start_date>=mig_start_preg_lookback,.N]
#Drug utilisation
pregnancy_D3[du_filter==1,dif:=pregnancy_start_date-op_start_date_du]
less_than_3_month_du<-pregnancy_D3[du_filter==1 & dif<lookback_3_months,.N]
pregnancy_D3[du_filter==1 & dif<lookback_3_months,du_filter:=NA]
pregnancy_D3[,dif:=NULL]
included_rec_du<-pregnancy_D3[du_filter==1,.N]
pregnancy_D3[du_filter==1,dif:=pregnancy_start_date-op_start_date_du]
lookback_12_month_du<-pregnancy_D3[du_filter==1 & dif>=lookback_12_months,.N]
pregnancy_D3[,dif:=NULL]

#Safety
pregnancy_D3[saf_filter==1,dif:=pregnancy_start_date-op_start_date_saf]
less_than_3_month_saf<-pregnancy_D3[saf_filter==1 & dif<lookback_3_months,.N]
pregnancy_D3[saf_filter==1 & dif<lookback_3_months,saf_filter:=NA]
pregnancy_D3[,dif:=NULL]
included_rec_saf<-pregnancy_D3[saf_filter==1,.N]
pregnancy_D3[saf_filter==1,dif:=pregnancy_start_date-op_start_date_saf]
lookback_12_month_saf<-pregnancy_D3[saf_filter==1 & dif>=lookback_12_months,.N]
pregnancy_D3[,dif:=NULL]


#follow up 42 weeks
#GDM and PE
pregnancy_D3[gdm_pe_filter==1,new_date:=pregnancy_start_date+42*7]
no_fup_42_weeks<-pregnancy_D3[gdm_pe_filter==1 & new_date>op_end_date_gdm_pe,.N]
pregnancy_D3[gdm_pe_filter==1 & new_date>op_end_date_gdm_pe, gdm_pe_filter:=NA]
pregnancy_D3[,new_date:=NULL]
#7 days after delivery
no_7_days<-pregnancy_D3[gdm_pe_filter==1 & op_end_date_gdm_pe<pregnancy_end_date+7,.N]
pregnancy_D3[gdm_pe_filter==1 & op_end_date_gdm_pe<pregnancy_end_date+7,gdm_pe_filter:=NA]
#Safety
pregnancy_D3[saf_filter==1,new_date:=pregnancy_start_date+42*7]
no_fup_42_weeks_saf<-pregnancy_D3[saf_filter==1 & new_date>op_end_date_saf,.N]
pregnancy_D3[saf_filter==1 & new_date>op_end_date_saf, saf_filter:=NA]
pregnancy_D3[,new_date:=NULL]

#Check if its multiple or single pregnancy record
pregnancy_D3[saf_filter==1, rowid:=rowid(person_id,pregnancy_start_date)]
multiple_preg_records<-pregnancy_D3[!duplicated(person_id) & rowid>=2,.N]
pregnancy_D3<-pregnancy_D3[rowid==1] 
pregnancy_D3[,rowid:=NULL]
#Create flowchart
Indicator<-c("5.6. Number of records with lookback period less than 3 months")
flowchart_3_months<-data.table(Indicator,GDM_and_PE=less_than_3_month_gdm_pe,Migraine=less_than_3_month_mig,Drug_utilisation=less_than_3_month_du,Safety=less_than_3_month_saf)
rm(less_than_3_month_gdm_pe,less_than_3_month_mig,less_than_3_month_du,less_than_3_month_saf)
rm(Indicator)

flowchart_separate<-rbind(flowchart_separate,flowchart_3_months)
rm(flowchart_3_months)

Indicator<-c("5.7. Number of included records up to now")
flowchart_incl<-data.table(Indicator,GDM_and_PE=included_rec_gdm_pe,Migraine=included_rec_mig,Drug_utilisation=included_rec_du,Safety=included_rec_saf)
rm(included_rec_gdm_pe,included_rec_mig,included_rec_du,included_rec_saf)
rm(Indicator)

flowchart_separate<-rbind(flowchart_separate,flowchart_incl)
rm(flowchart_incl)

Indicator<-c("5.8. Number of records with 12 months lookback")
flowchart_12<-data.table(Indicator,GDM_and_PE=lookback_12_month_gdm_pe,Migraine=lookback_12_month_mig,Drug_utilisation=lookback_12_month_du,Safety=lookback_12_month_saf)
rm(lookback_12_month_gdm_pe,lookback_12_month_mig,lookback_12_month_du,lookback_12_month_saf)
rm(Indicator)

flowchart_separate<-rbind(flowchart_separate,flowchart_12)
rm(flowchart_12)

Indicator<-c("5.9. Number of records with less than 42 weeks of follow up")
flowchart_42_fup<-data.table(Indicator,GDM_and_PE=no_fup_42_weeks,Migraine="N/A",Drug_utilisation="N/A",Safety=no_fup_42_weeks_saf)
rm(no_fup_42_weeks,no_fup_42_weeks_saf)
rm(Indicator)

flowchart_separate<-rbind(flowchart_separate,flowchart_42_fup)
rm(flowchart_42_fup)

Indicator<-c("5.10. Number of records with less than 7 days after pregnancy end date")
flowchart_7_fup<-data.table(Indicator,GDM_and_PE=no_7_days,Migraine="N/A",Drug_utilisation="N/A",Safety="N/A")
rm(no_7_days)
rm(Indicator)

flowchart_separate<-rbind(flowchart_separate,flowchart_7_fup)
rm(flowchart_7_fup)

Indicator<-c("5.11. Number of records with 5 years lookback")
flowchart_5<-data.table(Indicator,GDM_and_PE="N/A",Migraine=incl_5_years,Drug_utilisation="N/A",Safety="N/A")
rm(incl_5_years)
rm(Indicator)

flowchart_separate<-rbind(flowchart_separate,flowchart_5)
rm(flowchart_5)

Indicator<-c("5.12. Number of multiple pregnancies")
flowchart_mult<-data.table(Indicator,GDM_and_PE="N/A",Migraine="N/A",Drug_utilisation="N/A",Safety=multiple_preg_records)
rm(multiple_preg_records)
rm(Indicator)

flowchart_separate<-rbind(flowchart_separate,flowchart_mult)
rm(flowchart_mult)

#Included records
incl_gdm_pe<-pregnancy_D3[gdm_pe_filter==1,.N]
incl_mig<-pregnancy_D3[mig_filter==1,.N]
incl_du<-pregnancy_D3[du_filter==1,.N]
incl_saf<-pregnancy_D3[saf_filter==1,.N]

Indicator<-c("5.13. Number of included records")
flowchart_final<-data.table(Indicator,GDM_and_PE=incl_gdm_pe,Migraine=incl_mig,Drug_utilisation=incl_du,Safety=incl_saf)
rm(incl_gdm_pe,incl_mig,incl_du,incl_saf)
rm(Indicator)

flowchart_separate<-rbind(flowchart_separate,flowchart_final)
rm(flowchart_final)

fwrite(flowchart_separate, paste0(output_dir,"Pregnancy study population/Step_01_flowchart_specific_criteria_study_population.csv"), row.names = F)
rm(flowchart_separate)

#Export the D3_PREGNANCY by specific project
cols<-c("person_id","pregnancy_id","pregnancy_start_date","pregnancy_end_date","year","sex_at_instance_creation","birth_date","death_date","op_start_date_gdm_pe","op_end_date_gdm_pe")
saveRDS(pregnancy_D3[gdm_pe_filter==1][,cols,with=F], paste0(projectFolder,"/g_intermediate/pregnancy_d3/GDM_PE_Pregnancy_D3.rds"))
rm(cols)

cols<-c("person_id","pregnancy_id","pregnancy_start_date","pregnancy_end_date","year","sex_at_instance_creation","birth_date","death_date","op_start_date_mig","op_end_date_mig")
saveRDS(pregnancy_D3[mig_filter==1], paste0(projectFolder,"/g_intermediate/pregnancy_d3/MIG_Pregnancy_D3.rds"))
rm(cols)

cols<-c("person_id","pregnancy_id","pregnancy_start_date","pregnancy_end_date","year","sex_at_instance_creation","birth_date","death_date","op_start_date_du","op_end_date_du")
saveRDS(pregnancy_D3[du_filter==1], paste0(projectFolder,"/g_intermediate/pregnancy_d3/DU_Pregnancy_D3.rds"))
rm(cols)

cols<-c("person_id","pregnancy_id","pregnancy_start_date","pregnancy_end_date","year","sex_at_instance_creation","birth_date","death_date","op_start_date_saf","op_end_date_saf")
saveRDS(pregnancy_D3[saf_filter==1], paste0(projectFolder,"/g_intermediate/pregnancy_d3/SAF_Pregnancy_D3.rds"))
rm(cols)

summary_gdm<-as.data.table(pregnancy_D3[gdm_pe_filter==1,.N,by="year"])
setnames(summary_gdm,"N","GDM_and_PE")
summary_mig<-as.data.table(pregnancy_D3[mig_filter==1,.N,by="year"])
setnames(summary_mig,"N","Migraine")
summary_du<-as.data.table(pregnancy_D3[du_filter==1,.N,by="year"])
setnames(summary_du,"N","Drug_utilisation")
summary_saf<-as.data.table(pregnancy_D3[saf_filter==1,.N,by="year"])
setnames(summary_saf,"N","Safety")
summary<-merge.data.table(summary_gdm,summary_mig,by="year",all=T)
rm(summary_gdm,summary_mig)
summary<-merge.data.table(summary,summary_du,by="year",all=T)
rm(summary_du)
summary<-merge.data.table(summary,summary_saf,by="year",all=T)
rm(summary_saf)
summary[is.na(GDM_and_PE),GDM_and_PE:=0]
summary[is.na(Migraine),Migraine:=0]
summary[is.na(Drug_utilisation),Drug_utilisation:=0]
summary[is.na(Safety),Safety:=0]

fwrite(summary,paste0(output_dir, "Pregnancy study population/Step_01_included_records_study_pop_pregnancy_D3.csv"), row.names = F)
rm(summary)

rm(pregnancy_D3,persons_table_files,obs_table_files,lookback_3_months,lookback_12_months)

#create date flowcharts
Indication<-c("start_coverage", "end_coverage", "minimum_start_pregnancy_date", "maxiumum_start_pregnancy_date","minimum_start_pregnancy_date_5_years")
GDM_and_PE<-c(gdm_pe_start_study_date,gdm_pe_end_study_date,min_preg_date_gdm_pe,max_preg_date_gdm_pe, ifelse(!is.null(gdm_pe_start_preg_lookback),gdm_pe_start_preg_lookback,NA))
Migraine<-c(mig_start_study_date,mig_end_study_date,min_preg_date_mig,max_preg_date_mig, ifelse(!is.null(mig_start_preg_lookback),mig_start_preg_lookback,NA))
Drug_utilisation<-c(du_start_study_date,du_end_study_date,min_preg_date_du,max_preg_date_du, ifelse(!is.null(du_start_preg_lookback),du_start_preg_lookback,NA))
Safety<-c(saf_start_study_date,saf_end_study_date,min_preg_date_saf,max_preg_date_saf, ifelse(!is.null(saf_start_preg_lookback),saf_start_preg_lookback,NA))

dates_flowchart<-data.table(Indication,GDM_and_PE,Migraine,Drug_utilisation,Safety)
fwrite(dates_flowchart,paste0(output_dir, "Pregnancy study population/inclusion_dates_flowchart.csv"), row.names = F)
rm(dates_flowchart)

date_running_end<-Sys.Date()
end_time<-Sys.time()

time_log<-data.table(DAP=data_access_provider_name,
                     Script="Step_01_create_study_population.R", 
                     Start_date=date_running_start, 
                     End_date=date_running_end,
                     Time_elaspsed=format(end_time-initial_time, digits=2))
fwrite(time_log,paste0(output_dir,"/Time log/Step_01_time_log.csv"),row.names = F)
rm(time_log)


