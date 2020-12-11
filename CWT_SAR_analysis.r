library(purrr)
#==========================================
# Source function files in functions folder
#==========================================
path <- "functions/"
files <- paste0(path, list.files(path))
purrr::walk(files, source)
memory.size(max=T)
#===============================================
# Load packages, install and load if not already
#===============================================
using("lubridate", "openxlsx","tidyverse","readxl")
detach(package:MASS, unload=TRUE)
#======================================================================================================
# download raw RMIS data, if necessary, to folder full of get cowlitz data from from raw RMIS downloads. 
# filter species, run, and mark type, print as excel file with release and recovery tabs
#====================================================================================================== 
getdata(download_releases = "No",
        download_recoveries = "No",
        first_brood_year = 1995,
        last_brood_year = 2014,
        min_total_age = 3,
        max_total_age = 5,
        hatcheryname =c("CHELAN","WELLS","TURTLE ROCK"), #can use a comma separated list here
        SP = "Chinook",
        CWT = "CWT",
        marktype = "Ad",
        filename ="UCRSuChk_CWT_data",
        #optional argument (leave out if you don't want to limit)
        runtiming =c("Summer")
)

#=============================================================
#look at where estimated (expanded) tags are missing from RMIS
#specify release grouping variables 
#specify recovery grouping variables
#
#query will write file that shows expanded and unexpanded CWT 
#by release group and recovery group and note the % of raw tags 
#unexpanded by release group
#
#can use this to help guide your final query!
#=============================================================
missing_expansions(
  datafile = "UCRSuChk_CWT_data.xlsx",
  MPRfilters = c("High Seas","Juvenile Sampling"),
  CWT_status = "CWT",
  mark = "Ad",
  min_ocean_age = 2,
  min_total_age = 3,
  max_ocean_age = 5,
  max_total_age = 6,
  rel_groups = c("hatchery","brood_year","release_site"),
  rec_groups = c("MPR_Groupings")
)
#==============================================================================================================
# #prepare data for analysis (two ways, by pooled tag groups-->"group", or by individual tag group-->"tagcode")
#==============================================================================================================
# dat<-preparedata(
#   datafile ="UCRSuChk_CWT_data.xlsx",
#   MPRfilters = c("Terminal Sport","High Seas","Juvenile Sampling"),
#   CWT_status = "CWT",
#   mark = "Ad",
#   min_ocean_age = 2,
#   min_total_age = 3,
#   max_ocean_age = 5,
#   max_total_age = 6,
#   adjust_expanded_tags_for_terminal_harvest=T,
#   terminal_harvest_file="CowlitzSpCkHarvestRate_byage_10.20.2020.csv",
#   set_expanded_to_raw_when_missing=T, #must be T if expanding escapement for terminal harvest!!!
#   create_age_season_group = T, #organize output for analysis by "group" or "tagcode" or by release group (combined tag codes)
#   release_group_DOYs = c(0,75, 115, 210,365), #day of year breaks for release groups
#   release_group_cats= c("early spring","spring","summer","fall"), #names of release group time periods
#   rel_groups<-c("hatchery","stock","release_site","age_season_group","brood_year"),
#   rec_groups<-c(NULL),
#   writefile = "Yes",
#   filename = "CowlitzSpChk_SAR_Analysis_bygroup_"
# )

dat<-preparedata(
  datafile ="UCRSuChk_CWT_data.xlsx",
  MPRfilters = c("High Seas","Juvenile Sampling"),
  CWT_status = "CWT",
  mark = "Ad",
  min_ocean_age = 2,
  min_total_age = 3,
  max_ocean_age = 5,
  max_total_age = 6,
  adjust_expanded_tags_for_terminal_harvest = F,
  terminal_harvest_file=NULL,
  set_expanded_to_raw_when_missing=T, #must be T if expanding escapement for terminal harvest!!!
  create_age_season_group = F, #must be T if expanding escapement for terminal harvest!!!
  release_group_DOYs = NULL,
  release_group_cats= NULL,
  rel_groups<-c("hatchery","stock","release_site","brood_year","tag_code","first_release_month","first_release_doy","last_release_doy","avg_weight","release_age","avg_length"),
  rec_groups<-c(NULL),
  writefile = "Yes",
  filename = "UCRSuChk_CWT_Analysis_bytag_"
)

#===============================================
# Load packages, install and load if not already
#===============================================
using("MASS","R2jags","RColorBrewer","mgcv")

#=========================================================================
#run analysis (two ways, by individual tag code or by groups of tag codes)
#=========================================================================
run_analysis(dat = "UCRSuChk_CWT_Analysis_bytag_Data_for_Analysis.csv",
             bygrouportagcode = "tagcode",
             grouplist=NA,#list groups for group analysis
             options=NA #this is if you wanted to compare total returns as a function of different mixtures of smolt release strategies; right now only functional for grouped analysis
)
# run_analysis(dat = "CowlitzSpChk_SAR_Analysis_bygroup_Data_for_Analysis.csv",
#              bygrouportagcode = "group",
#              grouplist=c("early spring_2","spring_2","summer_1","fall_1"),
#              options=NA #this is if you wanted to compare total returns as a function of different mixtures of smolt release strategies; right now only functional for grouped analysis
# )


