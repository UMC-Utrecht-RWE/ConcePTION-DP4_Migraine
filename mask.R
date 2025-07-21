#masking script for DP4

# (1) For masking, I checked the tables and I think it is enough if you loop over all the tables and mask the columns if their names belong to one of the below:
#   c("removed_rec", "included_records", "no_records", "no_diagnosed_pregnancies", "no_pregnancies", "no_pregnancies_same_type", "Count", "original_records", "before_start", "after_end") 
# 
# --> assume tables all in g_output
# 

#set packages and paths so file can be run independently

rm(list=setdiff(ls(), c("CDM_dir","PregnancyAlgorithm_g_output_dir")))
if(!require(rstudioapi)){install.packages("rstudioapi")}
library(rstudioapi)
projectFolder<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(projectFolder)
source("packages.R")
source("99_path.R")
setwd(projectFolder)

masked_folder<-file.path(dirname(output_dir), paste0(basename(output_dir),'_masked/'))
dir.create(masked_folder, showWarnings = FALSE)

#list all files in g_output

raw_file<-list.files(output_dir, pattern=".csv", recursive = TRUE)
raw_file<-raw_file[!grepl('^Time$|log|issues', raw_file)]

#loop read and add column "x___masked" and make all values 5 or less ==5 for each of the target columns

my_cols<-c("removed_rec", "included_records", "no_records", "no_diagnosed_pregnancies", 
           "no_pregnancies", "no_pregnancies_same_type", "Count", "original_records",
           "before_start", "after_end") 

mask= function(x){
  x[x>=1 & x<=4]<-"<5" 
  return (x)}


for (i in 1:length(raw_file)){
  #print(i)
  print(raw_file[i])
  my_file<-fread(paste0(output_dir,raw_file[i]))
  mask_cols<-my_cols[my_cols%in%colnames(my_file)]
  #print(mask_cols)
  if(length(mask_cols>0)){
  for (j in 1:length(mask_cols)){
    target_col<-mask_cols[j]
    #my_file[,..target_col]<-
     newcol<- mask(my_file[,..target_col])
     colnames(newcol)<-(paste0("mask_", target_col))
     #print(newcol)
     my_file<-cbind(my_file, newcol)
     my_file[,(target_col):=NULL] 
     #print(my_file)
  }
    filename<-str_split(raw_file[i],"/")
    file_name<-file.path(masked_folder, raw_file[i])
    dir.create(dirname(file_name), showWarnings = FALSE)
    fwrite(my_file,file_name)
  
}else{
  file_name<-file.path(masked_folder, raw_file[i])
  dir.create(dirname(file_name), showWarnings = FALSE)
  file.copy(paste0(output_dir, raw_file[i]),file_name,overwrite=TRUE)}
}
  

