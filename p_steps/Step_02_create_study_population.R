#Load the Observation periods table

####load info, parameters, conceptsets####
source(paste0(pre_dir,"info/directory_info.R"))
source(paste0(pre_dir,"functions/CreateSpells_v15.R"))

print('Import and append observation periods files')
obs_table_files<-actual_tables$OBSERVATION_PERIODS

OBS_PER<-lapply(paste0(path_dir,obs_table_files),function(x) fread(x, colClasses = "character"))
OBS_PER<-as.data.table(rbindlist(OBS_PER))

print('Set start and end date to date format and if end date is empty fill with end study date')
lapply(c("op_start_date","op_end_date"), function (x) OBS_PER <- OBS_PER[,eval(x) := as.IDate(as.character(get(x)),"%Y%m%d")])
OBS_PER[is.na(op_end_date), op_end_date := end_study_date]

####Run create spells####
OBS_PER1 <- CreateSpells(
  dataset=OBS_PER,
  id="person_id" ,
  start_date = "op_start_date",
  end_date = "op_end_date",
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

lapply(c("day_of_birth", "month_of_birth","year_of_birth", "day_of_death","month_of_death","year_of_death"), function (x) PERSONS <- PERSONS[,eval(x) := NULL])

####Merge Observation periods with PERSONS####
PERSONS<-merge.data.table(PERSONS,OBS_PER1, by="person_id", all.x=T)
rm(OBS_PER1)
#Keep only records where all person id have a num_spell
not_present_in_obs<-PERSONS[!is.na(person_id) & is.na(num_spell),.N]
PERSONS<-PERSONS[!is.na(person_id) & !is.na(num_spell)]

#fix op_start_date and op_end_date
#op_start_date the earliest between start study date + lookback and op_start_date
PERSONS[,start_study_plus_lookback:=as.IDate(start_study_plus_lookback)][,op_start_date:=as.IDate(op_start_date)]
PERSONS[,dif:=start_study_plus_lookback-op_start_date]
PERSONS[dif>=0,start_follow_up:=start_study_plus_lookback]
PERSONS[dif<0,start_follow_up:=op_start_date]
PERSONS[,start_study_plus_lookback:=NULL][,dif:=NULL][,op_start_date:=NULL]

PERSONS[,end_study_date:=as.IDate(end_study_date)][,op_end_date:=as.IDate(op_end_date)]
PERSONS[,dif:=end_study_date-op_end_date]
PERSONS[dif>=0,end_follow_up:=op_end_date]
PERSONS[dif<0,end_follow_up:=end_study_date]
PERSONS[,end_study_date:=NULL][,dif:=NULL][,op_end_date:=NULL]

PERSONS[,death_date:=as.IDate(death_date)]
PERSONS[,dif:=death_date-end_follow_up]
PERSONS[dif<0,end_follow_up:=death_date]
PERSONS[,death_date:=NULL][,dif:=NULL]


#impossible birthdates
PERSONS[,start_follow_up:=as.IDate(start_follow_up)][,end_follow_up:=as.IDate(end_follow_up)]
PERSONS[!is.na(end_follow_up),age:=floor((end_follow_up-birth_date)/365.25)]
impossible_birth<-PERSONS[age>120, .N]
PERSONS<-PERSONS[age<=120]
PERSONS[,age:=NULL]

####Load pregnancies df####
pregnancy_D3<-readRDS(paste0(projectFolder,"/g_intermediate/pregnancy_algorithm/pregnancy_D3.rds"))

####Merge pregnancy with persons table####
pregnancy_D3<-merge.data.table(pregnancy_D3,PERSONS, by="person_id", allow.cartesian = T, all.x = T)
preg_not_in_persons<-pregnancy_D3[!is.na(person_id) & is.na(start_follow_up),.N]
pregnancy_D3<-pregnancy_D3[!is.na(start_follow_up)]
rm(PERSONS)
#Check if we have pregnancy records before op start date
pregnancy_D3[,pregnancy_start_date:=as.IDate(pregnancy_start_date)][,pregnancy_end_date:=pregnancy_end_date]
pregnancy_D3[,dif:=start_follow_up-pregnancy_start_date]
preg_before_start<-pregnancy_D3[dif>0,.N]
pregnancy_D3<-pregnancy_D3[dif<=0]
pregnancy_D3[,dif:=NULL]

pregnancy_D3[,dif:=end_follow_up-pregnancy_end_date]
preg_after_end<-pregnancy_D3[dif<0,.N]
pregnancy_D3<-pregnancy_D3[dif>=0]
pregnancy_D3[,dif:=NULL]

pregnancy_D3[,dif:=pregnancy_start_date-end_follow_up]
preg_after_start<-pregnancy_D3[dif>=0,.N]
pregnancy_D3<-pregnancy_D3[dif<0]
pregnancy_D3[,dif:=NULL]

saveRDS(pregnancy_D3,paste0(projectFolder,"/g_intermediate/pregnancy_algorithm/pregnancy_study_population.rds"))

flowchart_study_population<-data.table("Indicator"=c("Number of original records in the OBSERVATION_PERIODS table",
                                            "Number of records in the OBSERVATION_PERIODS table after cleaning up the spells",
                                            "Nmber of records in the OBSERVATION_PERIODS table after keeping one record per person(latest)",
                                            "Number of original records in the PERSONS table",
                                            "Number of records in the PERSONS table with sex other than female",
                                            "Number of records in the PERSONS table with empty year of birth",
                                            "Number of records in the PERSONS table with empty year of death, when date or/and month of death are present",
                                            "Number of rcords in the PERSONS table where only day of birth was imputed",
                                            "Number of rcords in the PERSONS table where only day of death was imputed",
                                            "Number of rcords in the PERSONS table where both day and month of birth were imputed",
                                            "Number of rcords in the PERSONS table where both day and month of death were imputed",
                                            "Number of records present in PERSONS but not in OBSERVATION_PERIODS",
                                            "Number of records with impossible birthdates(age more than 120 years old at end follow up date)",
                                            "Number of records present in pregnancy_D3(from pregnancy algorithm) but not in PERSONS",
                                            "Number of pregnancies with start date before start of observation plus lookback time(365.25 days)",
                                            "Number of pregnancies with end date after end of observation",
                                            "Number of pregnancies with start date after end of observation"),
                              "no_records"=c(original_rows,
                                             after_run_create_spell,
                                             included_spells,
                                             original_rows_persons,
                                             no_females,
                                             no_birth_year,
                                             no_death_year,
                                             impt_day_birth,
                                             impt_day_death,
                                             impt_dm_birth,
                                             impt_dm_death,
                                             not_present_in_obs,
                                             impossible_birth,
                                             preg_not_in_persons,
                                             preg_before_start,
                                             preg_after_end,
                                             preg_after_start))

fwrite(flowchart_study_population, paste0(output_dir,"Pregnancy algorithm/flowchart_study_population.csv"), row.names = F)
rm(flowchart_study_population)