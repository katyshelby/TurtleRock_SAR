download_luts <- function(RMIS_url="ftp://ftp.rmpc.org/pub/data/"){
  source("functions/using.R")
  using("doParallel", "foreach", "parallel" , "tidyverse")
  
  lut_dir <- "RMIS_LUTs/"
  
  lut_filenames <- c("LC041_ALL_FULLSET.zip",
                    "run.zip",
                    "species.zip",
                    "study_type.zip",
                    "marks.zip",
                    "location_type.zip",
                    "gear.zip", 
                    "fishery.zip",
                    "period.zip",
                    "adclip_selective_fishery.csv"
                    )

    ftp_paths <- paste0(RMIS_url, lut_filenames)
    dest_paths <- paste0(lut_dir, lut_filenames)                 
    
       cl <- makeCluster(detectCores())
        
      registerDoParallel(cl=cl)

    foreach(i=seq_along(ftp_paths)) %dopar% {
    download.file(url=ftp_paths[i], destfile=dest_paths[i], quiet=TRUE)
    } %>% 
      invisible()
    stopCluster(cl)
} 

#download_luts()
