
read_releases <- function(first_by=NULL, last_by=NULL){
  
  if(!file.exists("GatherData/RL041_ALL_FULLSET.zip")){stop("Release data not found")}
  
 if (!is.null(first_by) & !is.null(last_by)) {  
  read_csv("GatherData/RL041_ALL_FULLSET.zip", 
           col_types=cols(.default="c",
                          species="i",
                          run="i",
                          avg_weight="d",
                          cwt_1st_mark_count="d",
                          cwt_2nd_mark_count="d",
                          non_cwt_1st_mark_count="d",
                          non_cwt_2nd_mark_count="d",
                          brood_year="i",
                          tag_loss_rate="d",
                          avg_length="d",
                          tag_loss_sample_size="i",
                          tag_loss_days="d"), 
           progress=FALSE)%>% 
    # Filter for Chinook (1) and Coho (2)
    filter( brood_year >= first_by, 
            brood_year <= last_by)
 } else {
   read_csv("GatherData/RL041_ALL_FULLSET.zip", 
           col_types=cols(.default="c",
                          species="i",
                          run="i",
                          avg_weight="d",
                          cwt_1st_mark_count="d",
                          cwt_2nd_mark_count="d",
                          non_cwt_1st_mark_count="d",
                          non_cwt_2nd_mark_count="d",
                          brood_year="i",
                          tag_loss_rate="d",
                          avg_length="d",
                          tag_loss_sample_size="i",
                          tag_loss_days="d"), 
           progress=FALSE)
  
 }
}  
