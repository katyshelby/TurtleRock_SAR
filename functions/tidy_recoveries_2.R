
# Tidy up and decode recovery data ####
# Args: recoveries: a dataframe read from recoveries.csv, 
#       release_data: tidied release data in wide format
#       ...         : Domain to filter relases ("CR" is Columbia River)

# Returns: Recovery data with decoded fishery and tag info lookups

tidy_recoveries_2 <- function(recoveries, ...) {
  using("readxl") #%>% invisible()
  
  # Filter only in-sample tags with known catch and tag_stats == Ok 
    # sample_type 5 leads to double-counting (see RMIS manual), 
    # tag_status==1 so no unresolved discrepancies 

  RMIS_recoveries <- recoveries #%>% filter(sample_type != 5, tag_status == 1)

  # Years to filter for expected age classes in recoveries
  first_by <- min(RMIS_recoveries$run_year) - 6 
  last_by <- max(RMIS_recoveries$run_year) - 2
  
  releases <- tidy_rels(first_by, last_by, format="w", domain="CR")#...)


  tag_lu <- releases %>%
    select(tag_code = tag_code_or_release_id,
           release_agency,
           reporting_agency,
           species_name,
           run_name,
           brood_year,
           hatchery,
           stock,
           release_site,
           first_release_date,
           last_release_date,
           avg_weight,
           avg_length,
           Ad_CWT,
           Ad_NoCWT,
           Unclipped_CWT,
           Unclipped_NoCWT)

  
  # Tags to summarize recoveries
  RecoveryTags <- tag_lu$tag_code
  
  # Lookup tables to de-code RMIS data
  fishery_lu <-read_xlsx("RMIS_LUTs/rmis_rar_fishery_map.xlsx", sheet = "rar_original_fishery_map", trim_ws = TRUE, progress = FALSE)

  gear_lu <- read_csv("RMIS_LUTs/gear.zip", col_types = cols(fishery = "d"))
  
  fishery_lu2 <- read_csv("RMIS_LUTs/fishery.zip", col_types = cols(fishery = "d"))
  
  RMIS_locations <- read_csv("RMIS_LUTs/LC041_ALL_FULLSET.zip", col_types = cols(.default = "c"), progress=FALSE)

  RMIS_recoveries <-  RMIS_recoveries %>%
    filter(tag_code %in% RecoveryTags) %>%
    left_join(tag_lu, by = "tag_code",suffix=c("_recovery","_release")) #%>%
    #mutate(temp_prefix = NA)

  # Get unique recovery location/fishery rows from recoveries, used to create a mgt fishery lut for the recoveries
    unique_rec_locs <- RMIS_recoveries %>% 
      select(recovery_location_code,fishery) %>% 
      distinct() %>% 
      mutate(temp_prefix=NA,fishery=as.numeric(fishery))

  # Matches the fishery prefix to corresponding row(s) in the fishery_lu
        mgt_fishery_lu <-
    sapply(1:nrow(unique_rec_locs), function(x)
  which(startsWith(unique_rec_locs$recovery_location_code[x], prefix = fishery_lu$location_code_prefix) &
          unique_rec_locs$fishery[x] >= fishery_lu$starting_psc_fishery & unique_rec_locs$fishery[x] <= fishery_lu$ending_psc_fishery)
      )

    RMIS_recoveries %>% #select(tag_code,recovery_location_code,fishery,estimated_number,run_year,recovery_date) %>% 
          mutate(fishery=as.numeric(fishery)) %>% 
          left_join(RMIS_locations %>% 
                      filter(location_type==1),by=c("recovery_location_code"="location_code")) %>% 
          select(tag_code,recovery_location_code,recovery_date,brood_year_release,hatchery_release,run,species_name_release,fishery,estimated_number,run_year,name,psc_basin,psc_region.x,description) %>% 
          left_join(fishery_lu2,by="fishery")
        
lu_mgt_fishery <- function(recovery_location_code, fishery){
 # recovery_location_code="1M1NW116 11"
 # fishery=64
 # Find largest matching strings
  match_counts <- nchar(str_extract(recovery_location_code, fishery_lu$location_code_prefix))
  
  # Pick largest matching location prefix and fishery codes between
    matches <- which(match_counts==max(match_counts,na.rm=TRUE) & 
                       fishery >= fishery_lu$starting_psc_fishery &
                       fishery <= 
                       fishery_lu$ending_psc_fishery)

  fishery_lu[matches,]

  }

    RMIS_recoveries %>% select(tag_code,recovery_location_code,fishery,estimated_number,run_year) %>% 
          mutate(fishery=as.numeric(fishery)) %>% 
          left_join(RMIS_locations %>% 
                      filter(location_type==1),by=c("recovery_location_code"="location_code")) %>% 
          select(tag_code,recovery_location_code,fishery,estimated_number,run_year,name,psc_basin,psc_region,description) %>% 
          left_join(fishery_lu2,by="fishery") 

unique_rec_locs %>% # pluck("recovery_location_code",1) %>% 
  mutate(dat=map2(.x=recovery_location_code,.y=fishery, ~lu_mgt_fishery(.x,.y))) %>% 
  mutate(more_than_one=map_lgl(dat,~nrow(.x)>1),
         no_matches=map_lgl(dat,~nrow(.x)<1)) %>% 
  filter(more_than_one) %>% 
  unnest(dat)


fishery_lu$location_code_prefix %>% startsWith("5M")

RMIS_locations %>% filter(location_type==1) %>% select(location_code)


unique_rec_locs %>%
  mutate(dat=map2(.x=recovery_location_code,.y=fishery, ~lu_mgt_fishery(.x,.y))) %>% 
  mutate(multiple_matches=map_lgl(dat,~nrow(.x)>1),
         no_matches=map_lgl(dat,~nrow(.x)<1)) %>%
  filter(multiple_matches) %>% 
  unnest(dat) %>%
  select(recovery_location_code, location_code_prefix) %>% 
  mutate(count=nchar(str_extract(recovery_location_code,location_code_prefix))) %>% 
  group_by(recovery_location_code) %>% 
  filter(count==max(count))
  
      mutate(Matching_prfx=map(.x=recovery_location_code,.y=location_code_prefix,~str_count(.x,.y))) %>% 
  select(recovery_location_code,location_code_prefix,Matching_prfx)

    RMIS_recoveries %>% filter(startsWith(recovery_location_code, "5M"),fishery>=40,fishery<=49) %>% select(fishery,recovery_location_code) %>% 
      select(recovery_location_code) %>% distinct()
    
no_codes <-
    sapply(1:length(mgt_fishery_lu), function(x)
      length(mgt_fishery_lu[[x]])) == 0
  
  
  unique_rec_locs$temp_prefix[no_codes] <- "X"
  
  mgt_fishery_lu[which(unique_rec_locs$temp_prefix == "X")] <-
    sapply(which(unique_rec_locs$temp_prefix == "X"), function(x)
      which(
        startsWith(unique_rec_locs$temp_prefix[x], prefix = fishery_lu$location_code_prefix) &
          unique_rec_locs$fishery[x] >= fishery_lu$starting_psc_fishery &
          unique_rec_locs$fishery[x] <= fishery_lu$ending_psc_fishery
      ))
  
  no_codes <-
    sapply(1:length(mgt_fishery_lu), function(x)
      length(mgt_fishery_lu[[x]])) == 0
  
  mgt_fishery_lu[no_codes] <- NA
  
  # Matches with >1 corresponding fishery codes
  to_fix <-sapply(1:length(mgt_fishery_lu), function(x) length(mgt_fishery_lu[[x]])) > 1
  
  if(any(to_fix)){
  # Replace with the longest match
    for (i in 1:length(mgt_fishery_lu[to_fix])) {
      mgt_fishery_lu[to_fix][[i]] <-
      mgt_fishery_lu[to_fix][[i]][which.max(nchar(fishery_lu$location_code_prefix[mgt_fishery_lu[to_fix][[i]]]))]
    }
  }
  
  mgt_fishery_lu <- unlist(mgt_fishery_lu)
  
  rec_fishery_lu <- bind_cols(unique_rec_locs, mgt_fishery=fishery_lu$mgt_fishery_name[mgt_fishery_lu]) %>% 
                    select(- temp_prefix)

  RMIS_recoveries %>% 
    left_join(rec_fishery_lu, by=c("recovery_location_code","fishery")) %>% 
    #select(tag_code,recovery_date,mgt_fishery) %>% 
    left_join(select(filter(RMIS_locations,location_type==1), location_code, recovery_location=name, description), by=c("recovery_location_code"="location_code")) %>% 
    left_join(fishery_lu2 %>% mutate(fishery=as.character(fishery)),by="fishery") %>% 
    left_join(gear_lu %>% mutate(fishery=as.character(fishery)), by = c("reporting_agency", "fishery", "gear")) %>% 
    mutate(age=run_year - brood_year_release)
      
}

