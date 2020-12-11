# make_file_list ####
# A function to create a list of urls and destination paths for RMIS cwt recovery data files
#
# ARGS: 
#   startYear, endYear = years (inclusive) of desired cwt recovery data from RMIS
#   RMIS_url = the ftp url for RMIS public data backups
#   temp_dir = a file in the project folder to store downloaded csvs temporarily
#
# RETURNS:  a list with two vectors: 1) the url paths to desired files 
#                                    2) the local paths to read/write downloaded files
source("functions/using.R")

make_file_list <- function(startYear, 
                              endYear,
                              RMIS_url="ftp://ftp.rmpc.org/pub/data/", 
                              temp_dir="GatherData/temp_csvs") {
# # Error message
#   if(is.null(startYear) | is.null(endYear)) {
# 
#     return(message("ERROR: Please supply start and/or end year"))
#   }
  
  # load pkgs required for this function
    using("RCurl", "tidyverse")
  
# Get filenames in the RMIS ftp directory - Takes a minute
ftp_names <- RCurl::getURL(RMIS_url, dirlistonly = TRUE)

# Make a vector of filenames, separating by line breaks ("\n")
  filenames <- strsplit(ftp_names, "\n") [[1]]

# Only want the csv files, use the regex below to grab only files beginning with RC041_ and ending with .csv
    # Changed to .zip to help with download speed. readr can read from local zips but not url .zips
  csvs <- filenames[grep("^RC041_.*\\.zip", filenames)]

  #  Only grab ODFW/WDFW files, replace line 22 with:
  #   csvs <-  filenames[grep("^RC041_ODFW.*\\.csv | ^RC041_WDFW.*\\.csv", filenames,fixed=F)]

# Extract the years for each recovery filename,convert to number. used to filter out years we don't need
  yrs <- substr(csvs, start=nchar(csvs)-8, nchar(csvs)-5) %>% as.integer

# Subset the csv filenames for only the years we want, drop the "\r" from filenames (for when we go to download) 
  files <- csvs[which(yrs >= startYear & yrs<=endYear)] %>% substr(1, nchar(.)-1)

# Return the list with a vector of file urls and the local path to save the downloaded files
  list(download_urls=paste0(RMIS_url, files), local_paths=paste0(temp_dir,"/", files))
}
