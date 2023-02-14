#Load parameters
tables_file<-list.files(paste0(projectFolder,"/p_parameters/"), "table_search")
tables_search<-read_excel(paste0(projectFolder,"/p_parameters/",tables_file),col_types = "text")
tables_search<-as.data.table(tables_search)

additional_concepts_file<-list.files(paste0(projectFolder,"/p_parameters/"), "additional_concepts")
additional_concepts<-read_excel(paste0(projectFolder,"/p_parameters/",additional_concepts_file),col_types = "text")
additional_concepts<-as.data.table(additional_concepts)



