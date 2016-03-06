#### Randomization Within Samples (randomizing Actor ID within observation sample period)

# Observed STTC Values of Alpha vs Others - whole sample  dt=120
startt<-list()
endt<-list()
observedX<-list()
for(i in 1:length(alphas)){
  
  dfy<-cohortdata[[i]]
  startt[[i]] <- dfy$cumetime[1]
  endt[[i]] <- dfy$cumetime[nrow(dfy)]
  dfz <- dfy %>% filter(Actor!="End") %>% filter(Actor!="Start")
  
  valtimes <- dfz$cumetime
  aa <- valtimes[dfz$Actor==alphas[[i]]]
  bb <- valtimes[dfz$Actor!=alphas[[i]]]
  observedX[[i]] <- tiling.corr(aa,bb,dt=2, rec.time=c(startt[[i]], endt[[i]]))
}

names(observedX)<-names(alphas)



####Randomization within samples  #takes a while.   DT=2min/120s
permedX <- matrix(nrow=length(alphas), ncol=1000)

for(i in 1:length(alphas)){
  for(j in 1:1000){
    
    dfy<-cohortdata[[i]]
    startt[[i]] <- dfy$cumetime[1]
    endt[[i]] <- dfy$cumetime[nrow(dfy)]
    dfz <- dfy %>% filter(Actor!="End") %>% filter(Actor!="Start")
    valtimes <- dfz$cumetime
    
    dfz$Actor <- do.call("c", split(dfz, dfz$uniqueobs) %>% lapply(., function(x) sample(x$Actor)  %>% as.character ) ) #Actors Randomized Within sample
    
    aa <- valtimes[dfz$Actor==alphas[[i]]]
    bb <- valtimes[dfz$Actor!=alphas[[i]]]
    permedX[i,j] <- tiling.corr(aa,bb,dt=2, rec.time=c(startt[[i]], endt[[i]]))
  }
}

#get pvalues
pvalsX<-list()
for(i in 1:length(alphas)){ pvalsX[[i]]<- (sum(permedX[i,]) <= observedX[[i]]) / 1000 }
pvalsX %>% unlist #

permedX.df <- reshape2::melt(permedX) %>% as.data.frame %>% mutate(cohort=rep(names(alphas),1000))
observedX.df <- data.frame(cohort=names(alphas), obs=unlist(observedX))

ggplot(permedX.df, aes(value)) + geom_histogram() + 
  geom_vline(data=observedX.df, mapping=aes(xintercept=obs), color="red",lty=2) +
  facet_wrap(~cohort, ncol=5)


#### DT=1.5

startt<-list()
endt<-list()
observedY<-list()
for(i in 1:length(alphas)){
  
  dfy<-cohortdata[[i]]
  startt[[i]] <- dfy$cumetime[1]
  endt[[i]] <- dfy$cumetime[nrow(dfy)]
  dfz <- dfy %>% filter(Actor!="End") %>% filter(Actor!="Start")
  
  valtimes <- dfz$cumetime
  aa <- valtimes[dfz$Actor==alphas[[i]]]
  bb <- valtimes[dfz$Actor!=alphas[[i]]]
  observedY[[i]] <- tiling.corr(aa,bb,dt=1.5, rec.time=c(startt[[i]], endt[[i]]))
}

names(observedY)<-names(alphas)


permedY <- matrix(nrow=length(alphas), ncol=1000)

for(i in 1:length(alphas)){
  for(j in 1:1000){
    
    dfy<-cohortdata[[i]]
    startt[[i]] <- dfy$cumetime[1]
    endt[[i]] <- dfy$cumetime[nrow(dfy)]
    dfz <- dfy %>% filter(Actor!="End") %>% filter(Actor!="Start")
    valtimes <- dfz$cumetime
    
    dfz$Actor <- do.call("c", split(dfz, dfz$uniqueobs) %>% lapply(., function(x) sample(x$Actor)  %>% as.character ) ) #Actors Randomized Within sample
    
    aa <- valtimes[dfz$Actor==alphas[[i]]]
    bb <- valtimes[dfz$Actor!=alphas[[i]]]
    permedY[i,j] <- tiling.corr(aa,bb,dt=1.5, rec.time=c(startt[[i]], endt[[i]]))
  }
}


#get pvalues
pvalsY<-list()
for(i in 1:length(alphas)){ pvalsY[[i]]<- (sum(permedY[i,]) <= observedY[[i]]) / 1000 }
pvalsY %>% unlist #

permedY.df <- reshape2::melt(permedY) %>% as.data.frame %>% mutate(cohort=rep(names(alphas),1000))
observedY.df <- data.frame(cohort=names(alphas), obs=unlist(observedY))

ggplot(permedY.df, aes(value)) + geom_histogram() + 
  geom_vline(data=observedY.df, mapping=aes(xintercept=obs), color="red",lty=2) +
  facet_wrap(~cohort, ncol=5)



#### DT= 1
startt<-list()
endt<-list()
observedZ<-list()
for(i in 1:length(alphas)){
  
  dfy<-cohortdata[[i]]
  startt[[i]] <- dfy$cumetime[1]
  endt[[i]] <- dfy$cumetime[nrow(dfy)]
  dfz <- dfy %>% filter(Actor!="End") %>% filter(Actor!="Start")
  
  valtimes <- dfz$cumetime
  aa <- valtimes[dfz$Actor==alphas[[i]]]
  bb <- valtimes[dfz$Actor!=alphas[[i]]]
  observedZ[[i]] <- tiling.corr(aa,bb,dt=1, rec.time=c(startt[[i]], endt[[i]]))
}

names(observedZ)<-names(alphas)


permedZ <- matrix(nrow=length(alphas), ncol=1000)

for(i in 1:length(alphas)){
  for(j in 1:1000){
    
    dfy<-cohortdata[[i]]
    startt[[i]] <- dfy$cumetime[1]
    endt[[i]] <- dfy$cumetime[nrow(dfy)]
    dfz <- dfy %>% filter(Actor!="End") %>% filter(Actor!="Start")
    valtimes <- dfz$cumetime
    
    dfz$Actor <- do.call("c", split(dfz, dfz$uniqueobs) %>% lapply(., function(x) sample(x$Actor)  %>% as.character ) ) #Actors Randomized Within sample
    
    aa <- valtimes[dfz$Actor==alphas[[i]]]
    bb <- valtimes[dfz$Actor!=alphas[[i]]]
    permedZ[i,j] <- tiling.corr(aa,bb,dt=1, rec.time=c(startt[[i]], endt[[i]]))
  }
}

observedZ
permedZ[1:13,1:11]
pvalsZ<-list()
for(i in 1:length(alphas)){ pvalsZ[[i]]<- (sum(permedZ[i,]) <= observedZ[[i]]) / 1000 }
pvalsZ %>% unlist #


permedZ.df <- reshape2::melt(permedZ) %>% as.data.frame %>% mutate(cohort=rep(names(alphas),1000))
observedZ.df <- data.frame(cohort=names(alphas), obs=unlist(observedZ))
head(permedZ.df)
head(observedZ.df)

ggplot(permedZ.df, aes(value)) + geom_histogram() + 
  geom_vline(data=observedZ.df, mapping=aes(xintercept=obs), color="red",lty=2) +
  facet_wrap(~cohort, ncol=5)




#### Save Results
#saveRDS(list(permedX.df, permedY.df, permedZ.df, observedX.df, observedY.df, observedZ.df), file="sttc_sample_randomization_final.RData")

pvalsX %>% unlist
pvalsY %>% unlist
pvalsZ %>% unlist

### READ Data
rand.data <- readRDS("sttc_sample_randomization_final.RData")

#e.g. for dt=1.5 / 90s
randomplot<-ggplot(rand.data[[2]], aes(value)) + geom_histogram() + 
  geom_vline(data=rand.data[[5]], mapping=aes(xintercept=obs), color="red",lty=2) +
  facet_wrap(~cohort, ncol=5,scales="free_x") + theme_minimal() + xlab("Permuted STTC value") +
  ylab("Count") + theme(panel.margin = unit(2, "lines"))
randomplot
