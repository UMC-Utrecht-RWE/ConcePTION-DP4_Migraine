#Clean folders
unlink(paste0(projectFolder,"/g_output"), recursive = T)#delete folder
dir.create(paste0(projectFolder, "/g_output"))
output_dir<-paste0(projectFolder,"/g_output/")
dir.create(paste(output_dir, "DU study population", sep=""))
dir.create(paste(output_dir, "Drug utilisation", sep=""))
dir.create(paste(output_dir, "Time log", sep=""))

unlink(paste0(projectFolder,"/g_intermediate"), recursive = T)#delete folder
dir.create(paste0(projectFolder, "/g_intermediate"))
g_intermediate<-paste0(projectFolder,"/g_intermediate/")
dir.create(paste(g_intermediate, "pregnancy_d3", sep=""))
dir.create(paste(g_intermediate, "medicines_d3", sep=""))
dir.create(paste(g_intermediate, "diagnoses_d3", sep=""))
dir.create(paste(g_intermediate, "tmp", sep=""))
