# Function gathers releases into long format with mark (Ad or NoClip), CWT status column (CWT or no CWT), and N_released columns
# Filters for any RMIS_fields, passed in as ...

# first_by =first brood year data needed
# last_by= last brood year data needed

# ... =  conditions passed in for dplyr::filter()
# e.g.,
# rmis_domain=="CR" # Releases for Columbia River 
# species_name=="Chinook"
# str_detect(hatchery, "COWLITZ") # any releases from COWLITZ hatchery

tidy_rels <- function(first_by, last_by, format="l", ...){
  
  # Make sure the format is either "w" for wide or "l" for long
  if(!format %in% c("w","l")){stop("Choose a format: 'w'=wide format, 'l'=long format")}
  
  # Create quosure for filter conditions
     filter_conditions <- rlang::quos(...)
  # Read-in release data, do lookups and tidy, filter by domain "CR" = Columbia River
  rels <- read_releases(first_by, last_by) %>% 
    releases_for_tr() %>% 
    decode_release_data() %>% 
    # filter on unquoted filter conditions (...)
    filter(!!!filter_conditions) 
  
  # If long format
  if(format == "l"){ 
    rels %>%  
    gather(key="Clip_CWT", value="N_released", contains("CWT")) %>% 
    separate(col=Clip_CWT, into=c("mark", "CWT_status")) %>% 
    filter(!is.na(N_released) | N_released > 0) 
  } 
  else{rels}
}
