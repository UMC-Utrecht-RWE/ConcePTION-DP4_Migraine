library(data.table)

# UOSL: green and yellow from PROMPT and green yellow and blue from CONCEPTSET
if(data_access_provider_name=="UOSL"){
D3_filtered <- rbind(pregnancy_D3[(meaning_of_principal_record == "birth_registry_mother" & imputed_end_of_pregnancy == 0)],
                     pregnancy_D3[!meaning_of_principal_record %in% c("birth_registry_mother", "PERSON_RELATIONSHIP")][highest_quality=="2_yellow"][type_of_pregnancy_end == "SA" | type_of_pregnancy_end == "T"])
rm(pregnancy_D3)
pregnancy_D3<-D3_filtered
rm(D3_filtered)
}


# SAIL: PROMPT & EUROCAT only quality green and yellow
if(data_access_provider_name=="USWAN"){
D3_filtered <- rbind(pregnancy_D3[imputed_end_of_pregnancy == 0][PROMPT == "yes"][!type_of_pregnancy_end %in% c("T", "SA")],
                     pregnancy_D3[imputed_end_of_pregnancy == 0][EUROCAT == "yes"])

rm(pregnancy_D3)
pregnancy_D3<-D3_filtered
rm(D3_filtered)
}
# FISABIO: PROMPT only quality green and yellow
if(data_access_provider_name=="FISABIO"){
  
  # FISABIO: only birth registry
  list_of_meanings_FISABIO <- c("perinatal_death_registry_mother",
                                "birth_registry_mother",
                                "anomalies_mother_registry",   
                                "perinatal_death_registry_child",
                                "birth_registry_child",
                                "anomalies_baby_registry")
  
D3_filtered <- pregnancy_D3[PROMPT == "yes" & imputed_end_of_pregnancy == 0][meaning_of_principal_record %in% list_of_meanings]

rm(pregnancy_D3)
pregnancy_D3<-D3_filtered
rm(D3_filtered)
}

# THL DP5: only pregnancies from registry 
if(data_access_provider_name=="NIHW"){
list_of_meanings_THL = c("birth_registry",
                         "induced_termination_registry",
                         "birth_registry_child")

D3_filtered <- pregnancy_D3[meaning_of_principal_record %in% list_of_meanings & imputed_end_of_pregnancy == 0]

rm(pregnancy_D3)
pregnancy_D3<-D3_filtered
rm(D3_filtered)
}


# FERRARA DP5: only pregnancies from registry #needs clarifications

