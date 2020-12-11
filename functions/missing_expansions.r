missing_expansions<-function(
  datafile,
  CWT_status,
  mark,
  MPRfilters,
  min_ocean_age,
  min_total_age,
  max_ocean_age,
  max_total_age,
  rel_groups,
  rec_groups,
  ...
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
  RecoveryData<-data.frame(RecoveryData%>%filter(ocean_age>=min_ocean_age,
                                                 ocean_age<=max_ocean_age,
                                                 total_age>=min_total_age,
                                                 total_age<=max_total_age,
                                                 !MPR_Groupings%in%MPRfilters)
                           %>%mutate(ExpansionStatus=ifelse(is.na(estimated_number),"missing","notmissing"))
                           %>%group_by(across(c("tag_code",rec_groups,ExpansionStatus)))
                           %>%summarise(Raw_Recoveries=sum(raw_number),Exp_Recoveries=sum(estimated_number,na.rm = F))
  )

  #merge release and recovery data
  dat<-merge(ReleaseData,RecoveryData,by="tag_code",all.x=TRUE)
  dat<-data.frame(dat%>%group_by(across(c(rel_groups,rec_groups,ExpansionStatus)))
                  %>%summarise(Raw_Recoveries=sum(Raw_Recoveries),Exp_Recoveries=sum(Exp_Recoveries,na.rm = F),SampleRate_obs=Raw_Recoveries/Exp_Recoveries)
                  %>%ungroup()
                  %>%group_by(across(rel_groups))
                  %>%mutate(Proportion_of_group_raw_recoveries=Raw_Recoveries/sum(Raw_Recoveries))
                  %>%ungroup()
                  %>%unite_(col='release_group', rel_groups, sep="", remove=T)
  )
  write.csv(dat,paste(getwd(),"/results/","missing_CWT_expansions.csv",sep=""),row.names = F)
}