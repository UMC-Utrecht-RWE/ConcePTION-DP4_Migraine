if("MEDICAL_OBSERVATIONS" %in% tables_diagnoses){
if(codesheet_diagnoses_hyp[,.N]>0){
  code_var<-codesheet_diagnoses_hyp[table=="MEDICAL_OBSERVATIONS",col_1]
  voc_var<-codesheet_diagnoses_hyp[table=="MEDICAL_OBSERVATIONS",col_2]
  date_var<-codesheet_diagnoses_hyp[table=="MEDICAL_OBSERVATIONS",date_column]
}
if(codesheet_diagnoses_dm[,.N]>0){
  code_var<-codesheet_diagnoses_dm[table=="MEDICAL_OBSERVATIONS",col_1]
  voc_var<-codesheet_diagnoses_dm[table=="MEDICAL_OBSERVATIONS",col_2]
  date_var<-codesheet_diagnoses_dm[table=="MEDICAL_OBSERVATIONS",date_column]
}
if(codesheet_diagnoses_dep[,.N]>0){
  code_var<-codesheet_diagnoses_dep[table=="MEDICAL_OBSERVATIONS",col_1]
  voc_var<-codesheet_diagnoses_dep[table=="MEDICAL_OBSERVATIONS",col_2]
  date_var<-codesheet_diagnoses_dep[table=="MEDICAL_OBSERVATIONS",date_column]
}
if(codesheet_diagnoses_parity[,.N]>0){
  code_var<-codesheet_diagnoses_parity[table=="MEDICAL_OBSERVATIONS",col_1]
  voc_var<-codesheet_diagnoses_parity[table=="MEDICAL_OBSERVATIONS",col_2]
  date_var<-codesheet_diagnoses_parity[table=="MEDICAL_OBSERVATIONS",date_column]
}
if(codesheet_diagnoses_bmi[,.N]>0){
  code_var<-codesheet_diagnoses_bmi[table=="MEDICAL_OBSERVATIONS",col_1]
  voc_var<-codesheet_diagnoses_bmi[table=="MEDICAL_OBSERVATIONS",col_2]
  date_var<-codesheet_diagnoses_bmi[table=="MEDICAL_OBSERVATIONS",date_column]
}
}
#### Checkbox ####
if("MEDICAL_OBSERVATIONS" %in% tables_checkbox){
if(!is.null(diag_checkbox_hyp_mo)){
  code_var_check<-not_fixed_hyp_checkbox[table=="MEDICAL_OBSERVATIONS",col_1]
  voc_var_check<-not_fixed_hyp_checkbox[table=="MEDICAL_OBSERVATIONS",col_2]
  date_var_check<-not_fixed_hyp_checkbox[table=="MEDICAL_OBSERVATIONS",date_column]
}

if(!is.null(diag_checkbox_dm_mo)){
  code_var_check<-not_fixed_dm_checkbox[table=="MEDICAL_OBSERVATIONS",col_1]
  voc_var_check<-not_fixed_dm_checkbox[table=="MEDICAL_OBSERVATIONS",col_2]
  date_var_check<-not_fixed_dm_checkbox[table=="MEDICAL_OBSERVATIONS",date_column]
}

if(!is.null(diag_checkbox_dep_mo)){
  code_var_check<-not_fixed_dep_checkbox[table=="MEDICAL_OBSERVATIONS",col_1]
  voc_var_check<-not_fixed_dep_checkbox[table=="MEDICAL_OBSERVATIONS",col_2]
  date_var_check<-not_fixed_dep_checkbox[table=="MEDICAL_OBSERVATIONS",date_column]
}

if(!is.null(diag_checkbox_parity_mo)){
  code_var_check<-not_fixed_parity_checkbox[table=="MEDICAL_OBSERVATIONS",col_1]
  voc_var_check<-not_fixed_parity_checkbox[table=="MEDICAL_OBSERVATIONS",col_2]
  date_var_check<-not_fixed_parity_checkbox[table=="MEDICAL_OBSERVATIONS",date_column]
}

if(!is.null(diag_checkbox_obesity_mo)){
  code_var_check<-not_fixed_bmi_checkbox[table=="MEDICAL_OBSERVATIONS",col_1]
  voc_var_check<-not_fixed_bmi_checkbox[table=="MEDICAL_OBSERVATIONS",col_2]
  date_var_check<-not_fixed_bmi_checkbox[table=="MEDICAL_OBSERVATIONS",date_column]
}

}
#### Fixed ####
if("MEDICAL_OBSERVATIONS" %in% tables_columns){
if(fixed_hyp_checkbox[,.N]>0){
  code_var_fix<-fixed_hyp_checkbox[table=="MEDICAL_OBSERVATIONS",col_1]
  voc_var_fix<-fixed_hyp_checkbox[table=="MEDICAL_OBSERVATIONS",col_2]
  date_var_fix<-fixed_hyp_checkbox[table=="MEDICAL_OBSERVATIONS",date_column]
}

if(fixed_dm_checkbox[,.N]>0){
  code_var_fix<-fixed_dm_checkbox[table=="MEDICAL_OBSERVATIONS",col_1]
  voc_var_fix<-fixed_dm_checkbox[table=="MEDICAL_OBSERVATIONS",col_2]
  date_var_fix<-fixed_dm_checkbox[table=="MEDICAL_OBSERVATIONS",date_column]
}

if(fixed_dep_checkbox[,.N]>0){
  code_var_fix<-fixed_dep_checkbox[table=="MEDICAL_OBSERVATIONS",col_1]
  voc_var_fix<-fixed_dep_checkbox[table=="MEDICAL_OBSERVATIONS",col_2]
  date_var_fix<-fixed_dep_checkbox[table=="MEDICAL_OBSERVATIONS",date_column]
}

if(fixed_parity_checkbox[,.N]>0){
  code_var_fix<-fixed_parity_checkbox[table=="MEDICAL_OBSERVATIONS",col_1]
  voc_var_fix<-fixed_parity_checkbox[table=="MEDICAL_OBSERVATIONS",col_2]
  date_var_fix<-fixed_parity_checkbox[table=="MEDICAL_OBSERVATIONS",date_column]
}

if(fixed_bmi_checkbox[,.N]>0){
  code_var_fix<-fixed_bmi_checkbox[table=="MEDICAL_OBSERVATIONS",col_1]
  voc_var_fix<-fixed_bmi_checkbox[table=="MEDICAL_OBSERVATIONS",col_2]
  date_var_fix<-fixed_bmi_checkbox[table=="MEDICAL_OBSERVATIONS",date_column]
}

if(fixed_h_checkbox[,.N]>0){
  code_var_fix<-fixed_h_checkbox[table=="MEDICAL_OBSERVATIONS",col_1]
  voc_var_fix<-fixed_h_checkbox[table=="MEDICAL_OBSERVATIONS",col_2]
  date_var_fix<-fixed_h_checkbox[table=="MEDICAL_OBSERVATIONS",date_column]
}

if(fixed_w_checkbox[,.N]>0){
  code_var_fix<-fixed_w_checkbox[table=="MEDICAL_OBSERVATIONS",col_1]
  voc_var_fix<-fixed_w_checkbox[table=="MEDICAL_OBSERVATIONS",col_2]
  date_var_fix<-fixed_w_checkbox[table=="MEDICAL_OBSERVATIONS",date_column]
}
}