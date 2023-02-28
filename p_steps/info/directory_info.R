`%!in%` = Negate(`%in%`)

#Get EVENTS, MO, SO, MEDICINES, VACCINES tables
actual_tables<-list()
actual_tables$EVENTS<-list.files(path_dir, pattern="^EVENTS")
actual_tables$MEDICAL_OBSERVATIONS<-list.files(path_dir, pattern="^MEDICAL_OBSERVATIONS")
actual_tables$SURVEY_OBSERVATIONS<-list.files(path_dir, pattern="^SURVEY_OBSERVATIONS")
actual_tables$MEDICINES<-list.files(path_dir, pattern="^MEDICINES")
actual_tables$SURVEY_ID<-list.files(path_dir, pattern="^SURVEY_ID")
actual_tables$EUROCAT<-list.files(path_dir, pattern="^EUROCAT")
actual_tables$PERSONS<-list.files(path_dir, pattern="^PERSONS")
actual_tables$OBSERVATION_PERIODS<-list.files(path_dir, pattern="^OBSERVATION_PERIODS")
