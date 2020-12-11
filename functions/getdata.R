getdata<-function(first_brood_year,
                  last_brood_year,
                  min_total_age,
                  max_total_age,
                  hatcheryname,
                  SP,
                  CWT,
                  runtiming,
                  marktype,
                  filename,
                  download_releases,
                  download_recoveries
){
  first_by = first_brood_year
  last_by = last_brood_year
  # Release data ####
  # Download releases if needed
  # UNCOMMENT TO RUN FOR FIRST TIME
  if(download_releases=="Yes"){
    download_releases()
  }
  if(download_recoveries=="Yes"){
    download_recoveries(start_yr=first_by+min_total_age, end_yr=last_by+max_total_age, by_brood=TRUE) 
  }
  # Pull in tidied release data in long format
  rels <- tidy_rels(first_by=first_by, last_by=last_by, format="l",
                    # Filter conditions passed in as ...
                    str_detect(hatchery,paste(hatcheryname, collapse = "|")),
                    species_name==SP,
                    #run_name==runtiming,
                    record_code=="T") %>% 
    select(tag_code=tag_code_or_release_id, 
           species=species_name, 
           run=run_name,
           hatchery,
           stock,
           release_site,
           brood_year,
           first_release_date,
           last_release_date,
           avg_length,
           avg_weight,
           CWT_status,
           mark,
           N_released)
  

  if(!missing(runtiming)){
    rels<-rels%>%filter(run%in%runtiming)
  }

  # Recovery data ####
  # Parallel read-filter-combine for all the recovery files, recs is line-by-line recovery data
  # Common fields between releases and recovery data (e.g., species_name, have the suffix "_recovery" or "_release" to indicate which table the field is from )
  # To filter for records reported as Chinook at release use species_name_release=="Chinook"
  recs <- filter_and_combine_recoveries(start_yr=first_by+min_total_age, 
                                        end_yr=last_by+max_total_age,
                                        str_detect(hatchery,paste(hatcheryname, collapse = "|")),
                                        #run_name==runtiming,
                                        species_name_release==SP)

  if(!missing(runtiming)){
    recs<-recs%>%filter(run_name%in%runtiming)
  }  
    # Looks up fishery names, gears and management fishery groupings, calculates total age as run_year-brood_year
    recs<-recs%>%tidy_recoveries()  %>% 
    # Filter out tag codes NOT in the release data
    filter(tag_code %in% rels$tag_code) %>% 
    # Keep necessary columns
    select(tag_code,
           species_name_release,
           species_name_recovery,
           recovery_date,
           run_year,
           recovery_agency=reporting_agency_recovery,
           recovery_location,
           estimated_number,
           fishery,
           mgt_fishery,
           fishery_short_name,
           recovery_region=psc_region.y)



  # Lookup table with arbitrary fishery groupings at a higher level than RMIS mgt fisheries. Based on Missing Production Report groupings.
  fishery_grp_lut <- read_xlsx("RMIS_LUTs/rmis_rar_fishery_map.xlsx","rar_original_fishery_map") %>% select(mgt_fishery_name,MPR_Groupings) %>% distinct() 
  
  # There are missing managment fishery designations from some CDFO recoveries, assign them to Canadian Fisheries
  rec_detail <- recs %>%  
    left_join(fishery_grp_lut, by=c("mgt_fishery"="mgt_fishery_name")) %>% 
    mutate(MPR_Groupings=case_when(is.na(MPR_Groupings) & recovery_agency == "CDFO" ~ "Canadian Fisheries",
                                   TRUE ~ MPR_Groupings)) 
  # Check for still missing fishery group
  # %>% filter(is.na(MPR_Groupings))
  
  
  # Summarize release and recovery data ####
  
  # Summarize Ad clipped CWT releases 
  cwt_release_summary <- rels %>% 
    # UNCLIPPED CWTS ARE NOT SAMPLED IN COL R FISHERIES, could be similar elsewhere; So filter for mark and CWT status desired
    filter(CWT_status%in%CWT, mark%in%marktype) %>% 
    group_by(tag_code,
             species,
             run, 
             hatchery,
             stock,
             release_site,
             CWT_status,
             mark,
             brood_year,
             first_release_date,
             last_release_date,
             avg_length,
             avg_weight) %>% 
    summarize(CWT_released=sum(N_released)) %>% 
    arrange(brood_year,first_release_date)
  
  # Summarize recovered and expanded recoveries (by tag code)
  rec_summary <- rec_detail %>% 
    group_by_at(vars(-estimated_number)) %>% 
    summarize(estimated_number=sum(estimated_number),
              raw_number=n()) %>% 
    ungroup() 
  
  #create results directory
  subDir <- file.path(getwd(),"results")
  if (file.exists(subDir)){
    print("results directory already exists and will not be created")
  } else {
    print("results directory will be created")
    dir.create(file.path(subDir))
  }
  # Write data to excel ####
  # Ad-CWT release info and detailed recovery data
  write.xlsx(list(ReleaseData=cwt_release_summary, RecoveryData=rec_summary), file=paste(getwd(),"/results/",filename,".xlsx",sep=""))
}