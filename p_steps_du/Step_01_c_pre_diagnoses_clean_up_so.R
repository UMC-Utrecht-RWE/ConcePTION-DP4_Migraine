if("SURVEY_OBSERVATIONS" %in% tables_diagnoses){
if(codesheet_diagnoses_hyp[,.N]>0){
  code_var<-codesheet_diagnoses_hyp[table=="SURVEY_OBSERVATIONS",col_1]
  voc_var<-codesheet_diagnoses_hyp[table=="SURVEY_OBSERVATIONS",col_2]
  date_var<-codesheet_diagnoses_hyp[table=="SURVEY_OBSERVATIONS",date_column]
}
if(codesheet_diagnoses_dm[,.N]>0){
  code_var<-codesheet_diagnoses_dm[table=="SURVEY_OBSERVATIONS",col_1]
  voc_var<-codesheet_diagnoses_dm[table=="SURVEY_OBSERVATIONS",col_2]
  date_var<-codesheet_diagnoses_dm[table=="SURVEY_OBSERVATIONS",date_column]
}
if(codesheet_diagnoses_dep[,.N]>0){
  code_var<-codesheet_diagnoses_dep[table=="SURVEY_OBSERVATIONS",col_1]
  voc_var<-codesheet_diagnoses_dep[table=="SURVEY_OBSERVATIONS",col_2]
  date_var<-codesheet_diagnoses_dep[table=="SURVEY_OBSERVATIONS",date_column]
}
if(codesheet_diagnoses_parity[,.N]>0){
  code_var<-codesheet_diagnoses_parity[table=="SURVEY_OBSERVATIONS",col_1]
  voc_var<-codesheet_diagnoses_parity[table=="SURVEY_OBSERVATIONS",col_2]
  date_var<-codesheet_diagnoses_parity[table=="SURVEY_OBSERVATIONS",date_column]
}
if(codesheet_diagnoses_bmi[,.N]>0){
  code_var<-codesheet_diagnoses_bmi[table=="SURVEY_OBSERVATIONS",col_1]
  voc_var<-codesheet_diagnoses_bmi[table=="SURVEY_OBSERVATIONS",col_2]
  date_var<-codesheet_diagnoses_bmi[table=="SURVEY_OBSERVATIONS",date_column]
}
}else{
  code_var <- NULL
  voc_var <- NULL
  date_var <- NULL
}
#### Checkbox ####
if("SURVEY_OBSERVATIONS" %in% tables_checkbox){
if(!is.null(diag_checkbox_hyp_so)){
  code_var_check<-not_fixed_hyp_checkbox[table=="SURVEY_OBSERVATIONS",col_1]
  voc_var_check<-not_fixed_hyp_checkbox[table=="SURVEY_OBSERVATIONS",col_2]
  date_var_check<-not_fixed_hyp_checkbox[table=="SURVEY_OBSERVATIONS",date_column]
}

if(!is.null(diag_checkbox_dm_so)){
  code_var_check<-not_fixed_dm_checkbox[table=="SURVEY_OBSERVATIONS",col_1]
  voc_var_check<-not_fixed_dm_checkbox[table=="SURVEY_OBSERVATIONS",col_2]
  date_var_check<-not_fixed_dm_checkbox[table=="SURVEY_OBSERVATIONS",date_column]
}

if(!is.null(diag_checkbox_dep_so)){
  code_var_check<-not_fixed_dep_checkbox[table=="SURVEY_OBSERVATIONS",col_1]
  voc_var_check<-not_fixed_dep_checkbox[table=="SURVEY_OBSERVATIONS",col_2]
  date_var_check<-not_fixed_dep_checkbox[table=="SURVEY_OBSERVATIONS",date_column]
}

if(!is.null(diag_checkbox_parity_so)){
  code_var_check<-not_fixed_parity_checkbox[table=="SURVEY_OBSERVATIONS",col_1]
  voc_var_check<-not_fixed_parity_checkbox[table=="SURVEY_OBSERVATIONS",col_2]
  date_var_check<-not_fixed_parity_checkbox[table=="SURVEY_OBSERVATIONS",date_column]
}

if(!is.null(diag_checkbox_obesity_so)){
  code_var_check<-not_fixed_bmi_checkbox[table=="SURVEY_OBSERVATIONS",col_1]
  voc_var_check<-not_fixed_bmi_checkbox[table=="SURVEY_OBSERVATIONS",col_2]
  date_var_check<-not_fixed_bmi_checkbox[table=="SURVEY_OBSERVATIONS",date_column]
}

}else{
  code_var_check <- NULL
  voc_var_check <- NULL
  date_var_check <- NULL
}
#### Fixed ####
if("SURVEY_OBSERVATIONS" %in% tables_columns){
if(fixed_hyp_checkbox[,.N]>0){
  code_var_fix<-fixed_hyp_checkbox[table=="SURVEY_OBSERVATIONS",col_1]
  voc_var_fix<-fixed_hyp_checkbox[table=="SURVEY_OBSERVATIONS",col_2]
  date_var_fix<-fixed_hyp_checkbox[table=="SURVEY_OBSERVATIONS",date_column]
}

if(fixed_dm_checkbox[,.N]>0){
  code_var_fix<-fixed_dm_checkbox[table=="SURVEY_OBSERVATIONS",col_1]
  voc_var_fix<-fixed_dm_checkbox[table=="SURVEY_OBSERVATIONS",col_2]
  date_var_fix<-fixed_dm_checkbox[table=="SURVEY_OBSERVATIONS",date_column]
}

if(fixed_dep_checkbox[,.N]>0){
  code_var_fix<-fixed_dep_checkbox[table=="SURVEY_OBSERVATIONS",col_1]
  voc_var_fix<-fixed_dep_checkbox[table=="SURVEY_OBSERVATIONS",col_2]
  date_var_fix<-fixed_dep_checkbox[table=="SURVEY_OBSERVATIONS",date_column]
}

if(fixed_parity_checkbox[,.N]>0){
  code_var_fix<-fixed_parity_checkbox[table=="SURVEY_OBSERVATIONS",col_1]
  voc_var_fix<-fixed_parity_checkbox[table=="SURVEY_OBSERVATIONS",col_2]
  date_var_fix<-fixed_parity_checkbox[table=="SURVEY_OBSERVATIONS",date_column]
}

if(fixed_bmi_checkbox[,.N]>0){
  code_var_fix<-fixed_bmi_checkbox[table=="SURVEY_OBSERVATIONS",col_1]
  voc_var_fix<-fixed_bmi_checkbox[table=="SURVEY_OBSERVATIONS",col_2]
  date_var_fix<-fixed_bmi_checkbox[table=="SURVEY_OBSERVATIONS",date_column]
}

if(fixed_h_checkbox[,.N]>0){
  code_var_fix<-fixed_h_checkbox[table=="SURVEY_OBSERVATIONS",col_1]
  voc_var_fix<-fixed_h_checkbox[table=="SURVEY_OBSERVATIONS",col_2]
  date_var_fix<-fixed_h_checkbox[table=="SURVEY_OBSERVATIONS",date_column]
}

if(fixed_w_checkbox[,.N]>0){
  code_var_fix<-fixed_w_checkbox[table=="SURVEY_OBSERVATIONS",col_1]
  voc_var_fix<-fixed_w_checkbox[table=="SURVEY_OBSERVATIONS",col_2]
  date_var_fix<-fixed_w_checkbox[table=="SURVEY_OBSERVATIONS",date_column]
}
}else{
  code_var_fix <- NULL
  voc_var_fix <- NULL
  date_var_fix <- NULL
}
