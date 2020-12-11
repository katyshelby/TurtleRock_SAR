preparedata<-function(
  datafile,
  MPRfilters,
  CWT_status = CWT_status,
  mark = mark,
  min_ocean_age,
  min_total_age,
  max_ocean_age,
  max_total_age,
  adjust_expanded_tags_for_terminal_harvest,
  terminal_harvest_file,
  set_expanded_to_raw_when_missing,
  create_age_season_group,
  release_group_DOYs,
  release_group_cats,
  rel_groups,
  rec_groups,
  writefile,
  filename
){
  #read in release data and create new date and age columns
  ReleaseData<-data.frame(read_xlsx(path=file.path(getwd(),"results",datafile),sheet="ReleaseData"))
  ReleaseData$release_year<-as.numeric(format(ReleaseData$first_release_date,"%Y"))
  ReleaseData$first_release_month<-as.numeric(format(ReleaseData$first_release_date,"%m"))
  ReleaseData$release_age<-ReleaseData$release_year-ReleaseData$brood_year
  ReleaseData$first_release_doy<-as.numeric(format(ReleaseData$first_release_date,"%j"))
  ReleaseData$last_release_doy<-as.numeric(format(ReleaseData$first_release_date,"%j"))
  ReleaseData<-data.frame(ReleaseData%>%filter(CWT_status%in%CWT_status, mark%in%mark))
  
  #read in release data and create new date and age columns
  RecoveryData<-data.frame(read_xlsx(path=file.path(getwd(),"results",datafile),sheet="RecoveryData"))
  RecoveryData<-merge(ReleaseData[,colnames(ReleaseData)%in%c("brood_year","release_year","tag_code")],RecoveryData,by="tag_code")
  RecoveryData$ocean_age<-RecoveryData$run_year-RecoveryData$release_year
  RecoveryData$total_age<-RecoveryData$run_year-RecoveryData$brood_year
  
  #filter out data not being used
  RecoveryData<-data.frame(RecoveryData%>%filter(ocean_age>=min_ocean_age,
                                                 ocean_age<=max_ocean_age,
                                                 total_age>=min_total_age,
                                                 total_age<=max_total_age,
                                                 !MPR_Groupings%in%MPRfilters)
  )

  #set expanded recoveries = to raw if no expanded exist
  if(set_expanded_to_raw_when_missing==T){
    RecoveryData$estimated_number[is.na(RecoveryData$estimated_number)]<-RecoveryData$raw_number[is.na(RecoveryData$estimated_number)]
  }
  
  #expand escapement recoveries for terminal harvest (if removing terminal sport harvest recoveries due to incomplete data)
  if(adjust_expanded_tags_for_terminal_harvest==T){
    terminalharvest<-data.frame(read.csv(file.path(getwd(),"supplementaldata",terminal_harvest_file)))
    RecoveryData<-merge(RecoveryData,terminalharvest,by=c("run_year","total_age"),all.x=T)
    RecoveryData$CowlitzR_hr[is.na(RecoveryData$CowlitzR_hr)]<-0
    RecoveryData$CowlitzR_hr<-pmin(RecoveryData$CowlitzR_hr,0.9)
    RecoveryData<-data.frame(RecoveryData%>%mutate(estimated_number=ifelse(mgt_fishery=="Escapement",estimated_number/(1-CowlitzR_hr),estimated_number)))
  }
  
  #summarize recovery data by tag group
  RecoveryData<-data.frame(RecoveryData
                           %>%group_by(tag_code)
                           %>%summarise(Raw_Recoveries=sum(raw_number),Exp_Recoveries=sum(estimated_number,na.rm = T))
  )
  
  #merge tag and recovery data by tag code
  dat<-data.frame(left_join(ReleaseData,RecoveryData,by="tag_code"))
  
  #create season/age group
  if(create_age_season_group==T){
    breaks=release_group_DOYs
    dat$release_date_category <- cut(dat$first_release_doy, 
                                             breaks=breaks, 
                                             labels=release_group_cats)
    dat$age_season_group<-paste(dat$release_date_category,dat$release_age,sep="_")
  }
  
  print(c(rel_groups=rel_groups,rec_groups))
  #summarize merged data by specified groupings
  dat<-data.frame(dat%>%group_by(across(c(rel_groups,rec_groups)))
                  %>%summarise(Releases=sum(CWT_released),Raw_Recoveries=sum(Raw_Recoveries),Exp_Recoveries=sum(Exp_Recoveries),SampleRate_obs=Raw_Recoveries/Exp_Recoveries,Raw_SAR=round(Exp_Recoveries/Releases,6))
  )
  #add column noting if expansion was performed
  dat$Expanded_Tags_Type<-"RMIS"
  if(adjust_expanded_tags_for_terminal_harvest==T){
      dat$Expanded_Tags_Type<-"RMIS_adjusted_for_terminal_harvest" 
  }

  #write data out
  write.csv(dat,paste(getwd(),"/results/",filename,"Data_for_Analysis.csv",sep=""),row.names = F)
  return(dat)
}