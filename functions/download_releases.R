download_releases <- function(url="ftp://ftp.rmpc.org/pub/data/RL041_ALL_FULLSET.zip"){
  #create Gather Data
  subDir <- file.path(getwd(),"GatherData")
  if (file.exists(subDir)){
    print("GatherData directory already exists and will not be created")
  } else {
    print("GatherData directory will be created")
    dir.create(file.path(subDir))
  }
  download.file(url=url, 
                destfile="GatherData/RL041_ALL_FULLSET.zip", 
                quiet=TRUE)
}


