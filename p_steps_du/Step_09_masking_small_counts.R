
output_dir = "g_output/Drug utilisation/csv/"
masked_folder = "g_output_masked/Drug utilisation"

if(!dir.exists(masked_folder)){
  dir.create(masked_folder)
}

should_masked_cols <- list(
  list(main_col = "N_users",
       dep_col = c("Prevalence", "CI_lower", "CI_upper")),
  
  list(main_col = "T1_N_preg",
       dep_col = c("T1_prev","T1_CI_low","T1_CI_up")),
  
  list(main_col = "T2_N_preg",
       dep_col = c("T2_prev","T2_CI_low","T2_CI_up")),
  
  list(main_col = "T3_N_preg",
       dep_col = c("T3_prev","T3_CI_low","T3_CI_up")),
  
  list(main_col = "Total_n", dep_col = "Total_p"),
  list(main_col = "Nonusers_n", dep_col = "Nonusers_p"),
  list(main_col = "Discontinuers_n", dep_col = "Discontinuers_p"),
  list(main_col = "Continuers_n", dep_col = "Continuers_p"),
  
  list(main_col = "Number", dep_col = "Percentage")
)


mask_dt <-function(my_file){
  if(!is.data.table(my_file)) my_file = as.data.table(my_file)
  for(j in seq_along(should_masked_cols)){
    target_col<-should_masked_cols[[j]]$main_col
    
    if(target_col %in% colnames(my_file)){
      res = as.numeric(my_file[,target_col, with=FALSE][[1]])
      if(any(class(res) %in% c("numeric", "integer")) && any(res<5, na.rm = TRUE)){
        for(k in seq_along(should_masked_cols[[j]]$dep_col)){
          target_col2 = should_masked_cols[[j]]$dep_col[k]
          if(target_col2 %in% colnames(my_file)){
            res2 = my_file[,target_col2, with=FALSE][[1]]
            res2[res<5] = "<5"
            my_file[, (target_col2):=res2]
          }
        }
        
        res[res<5] = "<5"
        my_file[, (target_col):=res]
      }
    }
  }
  my_file
}

dir.create(masked_folder, showWarnings = FALSE)
raw_file<-list.files(output_dir, pattern=".csv", recursive = TRUE)
raw_file<-raw_file[!grepl('^Time$|log|issues', raw_file)]
for (i in 1:length(raw_file)){
  my_file<-fread(file.path(output_dir,raw_file[i]))
  my_file = mask_dt(my_file)
  dd = dirname(file.path(masked_folder,raw_file[i]))
  if(dd!="" && !dir.exists(dd)){
    dir.create(dd, recursive = TRUE)
  }
  fwrite(my_file,file.path(masked_folder,raw_file[i]))
}


