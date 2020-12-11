# Download recoveries function #### 
# A function to download, read, bind and write RMIS recovery data into one data frame (parallelized)
#
# ARGS: 
#   startYear and endYear of desired data from RMIS, to be passed to the make_file_list function inside
#   by_brood: Logical to determine if recoveries should downloaded for brood years(by_brood=TRUE, default), or calendar years(by_brood=FALSE)
#
# RETURNS: NULL. Downloads recovery .zip files to temp_csvs folder in GatherData
source("functions/using.R")

# Required pkgs
using("doParallel", "foreach", "parallel" , "tidyverse", "lubridate")

download_recoveries <- function(start_yr, end_yr, by_brood=TRUE) {
  #create GatherData directory
  subDir <- file.path(getwd(),"GatherData/temp_csvs")
  if (file.exists(subDir)){
    print("GatherData/temp_csvs directory already exists and will not be created")
  } else {
    print("GatherData/temp_csvs directory will be created")
    dir.create(file.path(subDir))
  }
  if(by_brood){
    
  startYear <- start_yr 
  endYear <- end_yr 
  
  }else{
    startYear <- start_yr
    endYear <- end_yr
  }
  
  curYear <- year(Sys.Date())
  
  if (startYear > (curYear-1) || (endYear > (curYear-1))){
    warning(paste0("Chosen brood year(s) may not be fully reported to RMIS"))
    }
  
  # BUILD TEST FOR COMPLETED BROODs- maybe year(sys.Date()-1)?
  #if(endYear>)
   # Create list with the file  paths
      file_list <- make_file_list(startYear, endYear)

    # Set up clusters for parallel downloads
      cl <- makeCluster(detectCores())
        
      registerDoParallel(cl=cl)

  # Do download, read and combine the csvs with parallel for loop
     foreach(i=seq_along(file_list[[1]]), .inorder=FALSE) %dopar% {

      # Download the files from download urls, save them to the local paths
       download.file(url=file_list$download_urls[i], destfile=file_list$local_paths[i], quiet=TRUE)
       
    }
    
stopCluster(cl)

}


