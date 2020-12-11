run_analysis<-function(dat,bygrouportagcode,grouplist,options){
  #load data
  dat<-data.frame(read.csv(paste(getwd(),"/results/",dat,sep="")))
  logit<-function(x){log(x/(1-x))}
  ilogit<-function(x){exp(x)/(1+exp(x))}
  if(bygrouportagcode=="group"){
    #final filters
    dat<-dat[dat$age_season_group%in%grouplist,]
    if(!is.na(options)){
      O=nrow(options)
    }
    if(is.na(options)){
      O = 1
      options = matrix(1,ncol=length(unique(dat$release_group)),nrow=O)
    }
    #jags data
    jagsdat<-list(
      n=nrow(dat),
      Y=max(dat$brood_year)-min(dat$brood_year)+1,
      S=length(unique(dat$release_group)),
      Strategy = as.numeric(as.factor(dat$release_group)),
      Year = dat$brood_year-min(dat$brood_year)+1,
      Raw_Recoveries = dat$Raw_Recoveries,
      Releases=dat$Releases,
      O=O,
      options=options,
      sample_rate_obs=dat$SampleRate_obs
    )
    
    #paramters to monitor
    par<-c(
      "Kappa",
      "corr",
      "sigma",
      "phi",
      "p",
      "p_mu",
      "p_obs",
      "mu",
      "b1",
      "b2",
      "sigma_st",
      "sigma_yr",
      "resid",
      "R",
      "R_sum",
      "R_sum_med",
      "R_sum_mean"
    )
    #run model
    mod<-jags.parallel(data=jagsdat
                       ,model.file =  file.path("models\\Cowlitz_SAR_release_groups.jags")
                       ,parameters.to.save = par
                       ,inits=NULL
                       ,n.chains = 4
                       ,n.iter = 20000
                       ,n.burnin = 10000
                       ,n.thin = 1
                       ,jags.module = "mix" # if using the betabinom likelihood
                       
    )
    write.csv(mod$BUGSoutput$summary,paste(getwd(),"results/SAR_analysis_release_group_summary.csv",sep=""))
    col_list<-brewer.pal(n=c(jagsdat$S+2),"RdBu")[2:(2+jagsdat$S)]
    colors<-data.frame(col_list[1:length(grouplist)],unique(dat$release_group))
    colnames(colors)<-c("col_list","release_group")
    dat<-merge(dat,colors,by="release_group")
    pdf(paste(getwd(),"results/","SAR_Analysis_release_group_plots.pdf",sep=""))
    matplot(mod$BUGSoutput$median$p,type="l",col=col_list,lwd=2,lty=1,ylab="SAR",las=2,xaxt="n")
    axis(side=1,at=c(1:jagsdat$Y),labels=c(min(dat$brood_year):max(dat$brood_year)),las=2)
    points(y=dat$Raw_SAR,x=dat$brood_year-min(dat$brood_year)+1,col=as.character(dat$col_list),pch=20,cex=2)
    legend("topright",legend=unique(dat$release_group),col=col_list,lty=1,lwd=4,bty="n")
    dev.off()
  }
  if(bygrouportagcode=="tagcode"){
    mod3<-gam(cbind(round(Exp_Recoveries), Releases-round(Exp_Recoveries)) ~ 
              + s(brood_year,bs="ps",m=1,k=c(max(dat$brood_year)-min(dat$brood_year)+1))
              + s(first_release_doy,bs="cc",m=2,k=24)
              + s(avg_weight,bs="ts",m=2,k=4)
              + s(as.factor(dat$hatchery),bs="re",m=1)
              + s(as.factor(dat$release_site),bs="re",m=1)
              + s(as.factor(dat$hatchery),as.factor(dat$release_site),bs="re",m=1)
              ,knots=list( first_release_doy=seq(0,365, len=24) )
              ,family=quasibinomial(link="logit"), weights=SampleRate_obs ,data=dat,method="REML")
    sink("results/model_results.txt")
    print(summary(mod3))
    print(mod3)
    print(gam.check(mod3))
    sink()
    #plot preds by date and size
    pdf(paste(getwd(),"/results/","SAR_Analysis_tag_code_plots.pdf",sep=""))
    plot(mod3, select=1,shade=T,ylim=c(-5,5))
    plot(mod3, select=2,shade=T,ylim=c(-5,5))
    plot(mod3, select=3,shade=T,ylim=c(-5,5))
    dev.off()
    # newdat<-expand.grid("brood_year"=c(2000),"first_release_doy"=seq(0,365,2),"avg_weight"=seq(30,100,length.out = 4))
    # 
    # beta <- coef(mod3)
    # V <- vcov(mod3)
    # min(eigen(V)$values)
    # num_beta_vecs <- 10000
    # Cv <- chol(V)
    # nus <- rnorm(num_beta_vecs * length(beta))
    # beta_sims <- as.vector(beta) + t(Cv) %*% matrix(nus, nrow = length(beta), ncol = num_beta_vecs)
    # covar_sim <- predict(mod3, newdata = newdat, type = "lpmatrix")
    # linpred_sim <- covar_sim %*% beta_sims
    # invlink <- function(x) ilogit(x)
    # #invlink <- function(x) x
    # y_sim <- invlink(linpred_sim)
    # quants<-t(apply(y_sim,1,quantile,prob=c(0.025,0.25,0.5,0.75,0.975)))
    # newdat<-data.frame(newdat,quants)
    # 
    # newdat$preds<-predict(mod3,newdata = newdat,type="response")
    # 
    # col_list=brewer.pal(n=length(unique(newdat$avg_weight)),"RdBu")
    # colors<-data.frame(col_list,unique(newdat$avg_weight))
    # colnames(colors)<-c("col_list","avg_weight")
    # newdat<-merge(newdat,colors,by="avg_weight")
    # p<-ggplot(newdat,aes(y=X50.,x=first_release_doy,group=factor(avg_weight)))+
    #   facet_wrap(~avg_weight)+
    #   geom_ribbon(aes(ymin=X2.5.,ymax=X97.5.,),alpha=0.2,fill=newdat$col_list,color=NA)+
    #   geom_ribbon(aes(ymin=X25.,ymax=X75.),alpha=0.2,fill=newdat$col_list,color=NA)+
    #   theme(legend.justification = "right")+
    #   scale_colour_manual(values=col_list)+
    #   geom_line(aes(color=factor(avg_weight)),size=2)+
    #   ylab(label="SAR")+
    #   theme_bw()
    # print(p)
    # p<-ggplot(newdat,aes(y=X50.,x=first_release_doy,group=factor(avg_weight)))+
    #   theme(legend.justification = "right")+
    #   scale_colour_manual(values=col_list)+
    #   geom_line(aes(color=factor(avg_weight)),size=2)+
    #   ylab(label="SAR")+
    #   theme_bw()
    # print(p)
    #dev.off()
  }
}
