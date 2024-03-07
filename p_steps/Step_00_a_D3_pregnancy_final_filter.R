library(data.table)

# UOSL: green and yellow from PROMPT and green yellow and blue from CONCEPTSET
if(data_access_provider_name=="UOSL"){
D3_filtered <- rbind(pregnancy_D3[meaning_of_principal_record == "birth_registry_mother" & imputed_end_of_pregnancy == 0], #prompts
                     pregnancy_D3[!meaning_of_principal_record %in% c("birth_registry_mother", "PERSON_RELATIONSHIP")][highest_quality=="2_yellow"][type_of_pregnancy_end == "SA" | type_of_pregnancy_end == "T"]) #conceptsets
rm(pregnancy_D3)
pregnancy_D3<-D3_filtered
rm(D3_filtered)
}


# SAIL: PROMPT & EUROCAT only quality green and yellow
#FIX MEANINGS FROM THE PROMPTS, EUROCAT
if(data_access_provider_name=="USWAN"){
D3_filtered <- rbind(pregnancy_D3[meaning_of_principal_record %in% c("birth_registry_mother")][imputed_end_of_pregnancy == 0][type_of_end_pregnancy %in% c("LB","SB")], #prompts
                     pregnancy_D3[!meaning_of_principal_record %in% c("birth_registry_mother", "PERSON_RELATIONSHIP")][imputed_end_of_pregnancy == 0][type_of_pregnancy_end %in% c("SA", "T", "LB", "SB")]) #eurocat

rm(pregnancy_D3)
pregnancy_D3<-D3_filtered
rm(D3_filtered)
}
# FISABIO: PROMPT only quality green and yellow
if(data_access_provider_name=="FISABIO"){
  
  # FISABIO: only birth registry
  D3_filtered <-pregnancy_D3[meaning_of_principal_record %in% c("birth_registry_mother", "perinatal_death_registry_mother", "anomalies_mother_registry","perinatal_death_registry_child", "birth_registry_child")][imputed_end_of_pregnancy == 0][type_of_end_pregnancy=="LB" | type_of_end_pregnancy=="SB"] #prompts

  rm(pregnancy_D3)
pregnancy_D3<-D3_filtered
rm(D3_filtered)
}

# THL: only pregnancies from registry 
if(data_access_provider_name=="NIHW"){

D3_filtered <- rbind(pregnancy_D3[meaning_of_principal_record %in% c("birth_registry","birth_registry_child") & imputed_end_of_pregnancy == 0][type_of_pregnancy_end == "LB" | type_of_pregnancy_end == "SB"],
                     pregnancy_D3[meaning_of_principal_record %in% c("induced_termination_registry") & imputed_end_of_pregnancy == 0][type_of_pregnancy_end == "T"])

rm(pregnancy_D3)
pregnancy_D3<-D3_filtered
rm(D3_filtered)
}


# FERRARA: only pregnancies from registry
#FIX THE MEANINGS FOR CONCEPTSETS
if(data_access_provider_name=="FERR"){
  
  D3_filtered <- rbind(pregnancy_D3[meaning_of_principal_record %in% c("birth_registry_mother","birth_registry_child")][imputed_start_of_pregnancy==0 & imputed_end_of_pregnancy == 0][type_of_pregnancy_end == "LB"], #prompts
                       pregnancy_D3[!meaning_of_principal_record %in% c("birth_registry_mother","birth_registry_child")][highest_quality=="2_yellow"][type_of_pregnancy_end %in% c("LB", "SB", "SA", "T", "UNF")], #conceptsets
                       pregnancy_D3[!meaning_of_principal_record %in% c("birth_registry_mother","birth_registry_child")][highest_quality=="4_red"][type_of_pregnancy_end %in% c("LOSTFU", "ONGOING", "UNK")]) #conceptsets

  
  rm(pregnancy_D3)
  pregnancy_D3<-D3_filtered
  rm(D3_filtered)
}

# EFEMERIS: 
if(data_access_provider_name=="CHUT"){
  
  D3_filtered <- pregnancy_D3[imputed_start_of_pregnancy==0 & imputed_end_of_pregnancy == 0][type_of_pregnancy_end %in% c("LB", "SB", "SA", "T", "UNK")] #prompts
  
  rm(pregnancy_D3)
  pregnancy_D3<-D3_filtered
  rm(D3_filtered)
}
