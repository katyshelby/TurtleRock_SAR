# # Function to read in RMIS release data, decode locations/species/runs/study types ####
#
# 
#  # Read release data assign release id
#   RMIS_releases <- read_releases(startYear=startYear, endYear=endYear) %>% assign_rel_id()

decode_release_data <- function(RMIS_releases){
  using("lubridate")
  #RMIS_releases <- read_releases(first_by=1974,last_by=1974)
 # Read lookup tables
  RMIS_locations <- read_csv("RMIS_LUTs/LC041_ALL_FULLSET.zip", col_types=cols(.default="c"),progress=FALSE)
  RMIS_runs <- read_csv("RMIS_LUTs/run.zip", col_types=cols(.default="c",run="i"),progress=FALSE)
  RMIS_species <- read_csv("RMIS_LUTs/species.zip", col_types=cols(.default="c",species="i"),progress=FALSE)
  RMIS_studytype <- read_csv("RMIS_LUTs/study_type.zip", col_types=cols(.default="c"),progress=FALSE)
  
  # THIS IS NOT ON THE FTP SERVER, HAVE TO KEEP IN PROJECT FILE
  RMIS_domain <- read_tsv("RMIS_LUTs/rmis_region_LUT.txt", col_types=cols(.default="c"),progress=FALSE)


  missing_release_locs <- which(is.na(RMIS_releases$release_location_code) | 
                                  RMIS_releases$release_location_code=="" | 
                                  grepl("TEMP",RMIS_releases$release_location_code))

  missing_domains <- RMIS_releases %>% 
    filter(grepl("TEMP", release_location_code) | is.na(release_location_code) | release_location_code == "") %>% 
  left_join(filter(RMIS_locations, location_type == 3), by=c("hatchery_location_code"="location_code")) %>% 
  select(tag_code_or_release_id, psc_region) %>% 
  left_join(RMIS_domain, by=c("psc_region"="rmis_region")) %>% select(rmis_domain)

CR_rel_data <- 

  # Filter the releases for brood years, grab all years for both release summary and tag recoveries  
  RMIS_releases %>% 
  # Then join to the location table (type 4 = release locations) to lookup the release site name and psc region
    left_join(
    
    #Select just the location code and name from the locations table to join to the releases
      select(filter(RMIS_locations,location_type==4), location_code, release_site=name, psc_region),
  
    #Join the release data to the filtered location table (line above) by release location code and RelCode (named in line above: "RelCode=location_code")
      by=c("release_location_code"="location_code")) %>% 
  
    # Join the psc region looked up above to the rmis_domain table to lookup the RMIS domain,
      left_join(RMIS_domain, by=c("psc_region"="rmis_region")) %>%
  
    # Join to the location table again, this time lookup hatchery name
      left_join(
      select(filter(RMIS_locations, location_type==3), location_code, hatchery=name),
      by=c("hatchery_location_code"="location_code")) %>%
    
    # Join to location table one last time to lookup stock name
      left_join(
      select(filter(RMIS_locations, location_type==5), location_code, stock=name),
      by=c("stock_location_code"="location_code")) %>%   
  
    left_join(select(RMIS_species, species, species_name), by=c("species")) %>% 
    left_join(RMIS_runs, by="run") %>% 
    left_join(RMIS_studytype, by="study_type") %>% 
  
    mutate(release_year=substr(last_release_date, 1, 4)) #%>%
  
    # Finally, select only the columns needed.
    #select(record_code,tag_code_or_release_id,reporting_agency, species=species_name, run=run_name, tag_code_or_release_id, Hatchery, Stock, RelSite,brood_year,first_release_date, last_release_date,release_year,rmis_domain,
    #       cwt_1st_mark, cwt_1st_mark_count,non_cwt_1st_mark,non_cwt_1st_mark_count,cwt_2nd_mark,
    #       cwt_2nd_mark_count,non_cwt_2nd_mark,non_cwt_2nd_mark_count,release_location_code,study_type=study_type_name,avg_weight,rel_id,comp_key)

# Lookup the rmis domain for the missing domains, replace in CR_rel_data
CR_rel_data[which(is.na(CR_rel_data$rmis_domain)), "rmis_domain"] <- 
  
  CR_rel_data[which(is.na(CR_rel_data$rmis_domain)),] %>% 
  left_join(filter(RMIS_locations,location_type==3), by=c("hatchery_location_code"="location_code")) %>%
  left_join(RMIS_domain, by=c("psc_region.y"="rmis_region")) %>% 
  select(rmis_domain.y)

# Check for agencies with releases having undefined domains
agencies_no_domain <- CR_rel_data %>% 
  filter(is.na(rmis_domain)) %>% 
  select(release_agency) %>% 
  distinct()

# # Print warning for missing rmis_domains. Col R are usually reported, have seen WDFW coastal data with no domain
# if(nrow(agencies_no_domain)>0){
#   warning(paste(nrow(agencies_no_domain),"agencies have undefined release domains."), 
#           paste(c(" Agencies include:", pull(agencies_no_domain, release_agency)), collapse=" "))
# }

# Change release date to date format
 CR_rel_data %>% 
mutate_at(vars(contains("release_date")),~suppressWarnings(ymd(.x)))

 }

# Test
  # tst <- read_releases(first_by=2017, last_by=2018) %>% assign_rel_id() %>% releases_for_tr() %>% decode_release_data() %>% filter(tag_code_or_release_id=="201703") %>%
  #   gather(key="Clip_CWT",value="N_released",contains("CWT")) %>% separate(col=Clip_CWT,into=c("Mark","CWT_status"))
  # 
  # 
  # 
  
  
# Make this function to use with calc_SAR
  

#   
# tst %>% 
#   unite(mark,CWT_status,col="Mark_CWT") %>% 
#   rowid_to_column() %>% 
#   spread(key=Mark_CWT, value=N_released,fill=0) %>% 
#   select(-rowid)  %>% 
#   group_by_at(vars(-c("Ad_CWT","Ad_noCWT","NoClip_CWT", "NoClip_noCWT"))) %>% 
#   summarise(Ad_CWT=sum(Ad_CWT),Ad_noCWT=sum(Ad_noCWT),NoClip_CWT=sum(NoClip_CWT),NoClip_noCWT=sum(NoClip_noCWT)) %>% 
#   ungroup %>%   #filter(tag_code_or_release_id=="044377") %>% 
#   select(tag_code_or_release_id,comp_key,rel_id,Ad_CWT,Ad_noCWT,NoClip_CWT,NoClip_noCWT) %>% group_by(comp_key) %>% 
#   mutate(tr=)
# 
# colnames(o)
# o %>% mutate(year=o %$% str_sub(comp_key,start=-4) %>% as.numeric()) %>% filter(year>=2012) %>% select(tag_code_or_release_id,ADcwt,ADnocwt,UCcwt,UCnocwt) %>% filter(tag_code_or_release_id=="044377")

# # write to file function....maybe
# if(write_to_file == TRUE){
# # Add the data we just made tidy to the excel spreadsheet
#   og_dir <- getwd()
#   message("><((*> Please choose where to save output. ><((*>")
#   dir_choice <- rstudioapi::selectDirectory()
#   setwd(dir_choice)
# 
#   if(file.exists(paste(getwd(),"/releases.xlsx",sep=""))){
#     wb <- loadWorkbook(paste(getwd(),"/releases.xlsx",sep=""))
#     removeSheet(wb,"ReleaseData")
#     ReleaseData <- createSheet(wb,sheetName="ReleaseData")
#     addDataFrame(output_for_excel, sheet=ReleaseData, row.names=FALSE)
#     saveWorkbook(wb,"releases.xlsx")
#     }else{
#       wb <- createWorkbook()
#       releaseData <- createSheet(wb,sheetName="ReleaseData")
#       addDataFrame(output_for_excel, sheet=releaseData, row.names=FALSE)
#       saveWorkbook(wb,"releases.xlsx")
#       setwd(og_dir)}
#   
# }else{
#   output_for_excel
#   }