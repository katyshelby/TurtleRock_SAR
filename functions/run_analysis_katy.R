run_analysis_katy<-function(datafile,bygrouportagcode,grouplist,options){
  #load data
  dat<-data.frame(read.csv(paste(getwd(),"/results/",datafile,sep="")))%>%
    mutate(hatchery=factor(hatchery),release_site=factor(release_site))
  logit<-function(x){log(x/(1-x))}
  ilogit<-function(x){exp(x)/(1+exp(x))}
    if(bygrouportagcode=="tagcode"){
    mod3<-gam(cbind(round(Exp_Recoveries), Releases-round(Exp_Recoveries)) ~ 
              + s(brood_year,bs="ps",m=1,k=c(max(dat$brood_year)-min(dat$brood_year)+1))
              + s(first_release_doy,bs="cc",m=2,k=12)
              + s(avg_weight,bs="ts",m=2,k=4)
              + s(hatchery,bs="re",m=1)
              + s(release_site,bs="re",m=1)
              + s(hatchery,release_site,bs="re",m=1)
              ,knots=list( first_release_doy=seq(0,365, len=12) )
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
    newdat<-expand.grid("brood_year"=c(2000),
                        "first_release_doy"=seq(0,365,2),
                        "avg_weight"=c(20,25.5,30),
                        "hatchery"=c("CHELAN FALLS HATCHERY"),
                        "release_site"=c("CHELAN R     47.0052")
                        )
 

    beta <- coef(mod3)
    V <- vcov(mod3)
    min(eigen(V)$values)
    num_beta_vecs <- 10000
    Cv <- chol(V)
    nus <- rnorm(num_beta_vecs * length(beta))
    beta_sims <- as.vector(beta) + t(Cv) %*% matrix(nus, nrow = length(beta), ncol = num_beta_vecs)
    covar_sim <- predict(mod3, newdata = newdat, type = "lpmatrix")
    linpred_sim <- covar_sim %*% beta_sims
    invlink <- function(x) ilogit(x)
    #invlink <- function(x) x
    y_sim <- invlink(linpred_sim)
    quants<-t(apply(y_sim,1,quantile,prob=c(0.025,0.25,0.5,0.75,0.975)))
    newdat<-data.frame(newdat,quants)
    newdat$preds<-predict(mod3,newdata = newdat,type="response")
    
    col_list=brewer.pal(n=length(unique(newdat$avg_weight)),"Spectral")
    colors<-data.frame(col_list,unique(newdat$avg_weight))
    colnames(colors)<-c("col_list","avg_weight")
    newdat<-merge(newdat,colors,by="avg_weight")
    p<-ggplot(newdat,aes(y=X50.,x=first_release_doy,group=factor(avg_weight)))+
      facet_wrap(~avg_weight)+
      geom_ribbon(aes(ymin=X2.5.,ymax=X97.5.,),alpha=0.2,fill=newdat$col_list,color=NA)+
      geom_ribbon(aes(ymin=X25.,ymax=X75.),alpha=0.2,fill=newdat$col_list,color=NA)+
      theme(legend.justification = "right")+
      scale_colour_manual(values=col_list)+
      geom_line(aes(color=factor(avg_weight)),size=2)+
      ylab(label="SAR")+
      theme_bw()
    print(p)
    }
  return(newdat)
}
