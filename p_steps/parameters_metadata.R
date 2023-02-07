#Load parameters
tables_file<-list.files(paste0(projectFolder,"/p_parameters/"), "table_search")
tables_search<-read_excel(paste0(projectFolder,"/p_parameters/",tables_file),col_types = "text")
tables_search<-as.data.table(tables_search)
