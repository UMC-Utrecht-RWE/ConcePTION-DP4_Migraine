#Clean folders
unlink(paste0(projectFolder,"/g_output"), recursive = T)#delete folder
dir.create(paste0(projectFolder, "/g_output"))
output_dir<-paste0(projectFolder,"/g_output/")
# dir.create(paste(output_dir, "Drug utilization", sep=""))
dir.create(paste(output_dir, "Migraine algorithm", sep=""))
dir.create(paste(output_dir, "PE and GDM algorithm", sep=""))
dir.create(paste(output_dir, "Pregnancy algorithm", sep=""))
dir.create(paste(output_dir, "Pregnancy study population", sep=""))
dir.create(paste(output_dir, "Safety", sep=""))
dir.create(paste(output_dir, "Time log", sep=""))
dir.create(paste(output_dir, "Time log/DU", sep=""))
#Drug Utilisation
dir.create(paste(output_dir, "Drug utilisation", sep=""))
dir.create(paste(output_dir, "Drug utilisation/flowchart", sep=""))


unlink(paste0(projectFolder,"/g_intermediate"), recursive = T)#delete folder
dir.create(paste0(projectFolder, "/g_intermediate"))
g_intermediate<-paste0(projectFolder,"/g_intermediate/")
dir.create(paste(g_intermediate, "gdm_algorithm", sep=""))
dir.create(paste(g_intermediate, "migraine_algorithm", sep=""))
dir.create(paste(g_intermediate, "migraine_algorithm_sensitivity", sep=""))
dir.create(paste(g_intermediate, "pe_algorithm", sep=""))
dir.create(paste(g_intermediate, "pregnancy_algorithm", sep=""))
dir.create(paste(g_intermediate, "pregnancy_d3", sep=""))
dir.create(paste(g_intermediate, "tmp", sep=""))
#Drug Utilisation
dir.create(paste(g_intermediate, "medicines_d3", sep=""))
dir.create(paste(g_intermediate, "diagnoses_d3", sep=""))
