## Code for Attention Hierarchy Paper

#Libraries
library(dplyr)
library(ggplot2)
library(magrittr)
library(sjemea)
library(purrr)
library(compete) #devtools::install_github('jalapic/compete')
library(PlayerRatings)
library(lme4)
library(lmerTest)
library(tidyr)


cohortdata <- readRDS('sttc_cohorts_all.RData')
lapply(cohortdata,head)
#[1] "cohortAA" "cohortAB" "cohortAC" "cohortAD" "cohortAE" "cohortAF" "cohortAG" "cohortAH" "cohortAI" "cohortAJ" "cohortAK" "cohortAL" "cohortAM"

alldata <- do.call('rbind', Map(cbind, cohortdata, cohort=names(cohortdata)))
alphas <- lapply(cohortdata, function(x) which.max(table(x$Actor)) %>% names(.)) #store ID of alpha in list





## Methods:----

# number of days of observation
alldata %>%
  group_by(cohort) %>%
  summarize(maxday = max(day)) %>%
  ungroup() %>%
  .$maxday %>% as.numeric() %>%
  summary() #median=22.0

# number of unique observation
alldata %>%
  group_by(cohort) %>%
  summarize(totalobs = length(unique(uniqueobs))) %>%
  ungroup() %>%
  .$totalobs %>%
  sum #364

#total contests
alldata %>% 
  filter(Actor!="Start" & Actor!="End") %>%
  nrow() #11243

#total hours
alldata %>%
  filter(Actor=="End") %>%
  summarize(total = sum(newtime)/60) #503.7253



## Results: Hierarchy Analysis ----

#1. Landau's Modified h' value
lapply(cohortdata, function(x) x %>% select(Actor,Recipient) %>% filter(Actor!="Start" & Actor!="End") %>%
         mutate(result=1) %>% get_wl_matrix %>% devries)

#store these results
hvals <- c(0.776851 ,0.7589294 ,0.9265734 ,0.7167664 ,0.7236098 ,0.7106357 ,0.7828357 ,0.6739308 ,0.7449238 ,0.9178322 ,0.9929958 ,0.8624266 ,0.8057608) #randomization test, so values will vary very slightly  if re-run
names(hvals)<-names(cohortdata)# store results - from above function
summary(hvals)


#2. Directional Consistency DC-
dcival <- lapply(cohortdata, function(x) x %>% select(Actor,Recipient) %>% filter(Actor!="Start" & Actor!="End") %>%
                   mutate(result=1) %>% get_wl_matrix %>% dci)
dcival<-unlist(dcival)
summary(dcival)


#3. Win proportions of all contests made by alphas 
totalwins <- lapply(cohortdata, function(x) x %>% select(Actor,Recipient) %>% filter(Actor!="Start" & Actor!="End") %>%
                      mutate(result=1) %>% get_wl_matrix %>% rowSums %>% sort)

winprops <- lapply(totalwins, function(x) x[12]/sum(x)) %>% unlist
names(winprops) <- names(cohortdata)
summary(winprops)

#4. Win percentage of alphas
wlmatrices <- lapply(cohortdata, function(x) x %>% select(Actor,Recipient) %>% mutate(score=1) %>% get_wl_matrix )
winpercts <- lapply(names(alphas), function(i) sum(wlmatrices[[i]][alphas[[i]],])/ sum(sum(wlmatrices[[i]][alphas[[i]],])+sum(wlmatrices[[i]][,alphas[[i]]])))
names(winpercts)<-names(alphas)         
summary(winpercts %>% unlist)



## Glicko Analysis for final Rank Order----
glickodfs <- lapply(cohortdata, function(x) x %>% filter(Actor!="Start") %>% filter(Recipient!="End") %>% 
                      arrange(cumetime) %>% mutate(event=row_number()))

dflist <- lapply(glickodfs, function(x) x %>% select(event, Actor, Recipient) %>% mutate(score=1))
df.list <- lapply(dflist, function(x) x %>% map_if(is.factor, as.character)) #PlayerRatings needs character vars

robjs <- lapply(df.list, glicko, cval=3, history=T) #get PlayerRatings object
robjs <- lapply(robjs, function(x) x %>% .$ratings %>% as.data.frame.matrix %>% mutate(Rank=1:12)) #Turn into df with rank
robjs



## Inter-Event Intervals----

# 1. Get IEIs for all Cohorts:

#IEI betweeen same individuals--
ieidf<-list()
for(j in 1:length(cohortdata)){
  bursts <- cohortdata[[j]] %>% filter(Actor!="Start") %>% filter(Actor!="End") %$% split(., .[,c('Actor','uniqueobs')]) 
  bursts.iei <- bursts %>% lapply(., function(x) x$cumetime - lag(x$cumetime))
  bursts.demo <- bursts %>% lapply(., function(x) x %>% select(Actor, day, uniqueobs) %>% head(1) )
  
  ttmp<-lapply(bursts.iei, cbind)
  ttmp1 <- do.call('rbind', lapply(names(bursts.demo), function(i) cbind(ttmp[[i]], bursts.demo[[i]])))
  ttmp1 <- ttmp1[complete.cases(ttmp1),] #drop NAs
  colnames(ttmp1)[1]<-"iei"
  ieidf[[j]] <- ttmp1
}
ieidf
names(ieidf)<-names(cohortdata)
ieidf<-Map(cbind, ieidf, cohort=names(cohortdata))


# 2. Add in Ranks (match Actor variable):
for (i in 1:length(robjs)){
  ieidf[[i]]$Rank <- robjs[[i]]$Rank[match(ieidf[[i]]$Actor, robjs[[i]]$Player)]
}

ieidf1 <- do.call('rbind', ieidf) #make into one df
ieidf1 <- ieidf1 %>% mutate(alpha = ifelse(Rank==1, 'yes', 'no')) #demark alpha/other 
ieidf1.alpha <- ieidf1 %>% filter(Rank==1) #all alpha IEIs
ieidf1.others <- ieidf1 %>% filter(Rank!=1) #all other IEIs


# 3. IEIs when considering all non-alpha individuals as 'other':
ieidfx<-list()

for(j in 1:length(cohortdata)){
  bursts <- cohortdata[[j]] %>% filter(Actor!="Start") %>% filter(Actor!="End") %>%
    filter(Actor!=alphas[[j]]) %$% split(., .[,c('uniqueobs')]) 
  bursts.iei <- bursts %>% lapply(., function(x) x$cumetime - lag(x$cumetime))
  bursts.demo <- bursts %>% lapply(., function(x) x %>% select(day, uniqueobs) %>% head(1) )
  
  ttmp<-lapply(bursts.iei, cbind)
  ttmp1 <- do.call('rbind', lapply(names(bursts.demo), function(i) cbind(ttmp[[i]], bursts.demo[[i]])))
  ttmp1 <- ttmp1[complete.cases(ttmp1),] #drop NAs
  colnames(ttmp1)[1]<-"iei"
  ieidfx[[j]] <- ttmp1
}

names(ieidfx)<-names(cohortdata)
ieidfx1<-Map(cbind, ieidfx, cohort=names(cohortdata))
ieidfx2 <- do.call('rbind', ieidfx1)

# 4. how many <4 minutes
ieidf1 %>% 
  group_by(alpha) %>%
  summarize(total=n(), less4 = sum(iei<4)) %>%
  mutate(pct = (100*less4)/total)

ieidfx2 %>%
  summarize(total=n(), less4 = sum(iei<4)) %>%
  mutate(pct = (100*less4)/total)


# Figures 1A - histograms of IEIs:

newieidf<-rbind(ieidfx2 %>% select(iei) %>% mutate(alpha="all_other"),
                ieidf1 %>% select(iei,alpha)) #make one large df so can plot in one panel:

newieidf$alpha <- factor(newieidf$alpha, levels=c("yes", "no", "all_other"))


ieiplot<-ggplot(newieidf, aes(iei)) + 
  geom_histogram(binwidth=1, fill="gray87",color='black') + 
  facet_wrap(~alpha, scales = "free_y", nrow=1) + 
  theme_minimal() + 
  xlab("Inter Event Interval (mins)") +
  ylab("Total frequency")
ieiplot


## Mixed-Effects Model, IEI~Rank:

head(ieidf1) #use df of IEIs broken down by cohort & Rank
sum(table(ieidf1$Rank)) #total N of IEIs- 9614

ieidf1$subject <- paste(ieidf1$Actor, ieidf1$cohort) #assign subject ID

fit1 <- lmer(log10(iei+.1) ~ 1 + (1|cohort/subject), ieidf1, REML=0)  #basic model
fit2 <- lmer(log10(iei+.1) ~ Rank + (1|cohort/subject), data = ieidf1, REML=0) #subject nested in cohort
fit3 <- lmer(log10(iei+.1) ~ Rank + (Rank|cohort/subject), data = ieidf1, REML=0)#Add Random Slopes

anova(fit1,fit2,fit3)
plot(fit2)
plot(fit3)
summary(fit2)
summary(fit3)

log10(1.0852) #0.0355 = fixed effect
1.0852 - .1  #remove .1 (added in model)   = 0.985 (approx one minute)



## Suppl Figure 1 - Boxplots of Alpha, Other, Other_All:

#all ranks
ieidf1.sum <- ieidf1 %>% group_by(cohort, Rank) %>% summarize( lwr=quantile(iei,0.25), median = median(iei), upr=quantile(iei,0.75))

#others_byid
ieidf1.sum1 <- ieidf1 %>% group_by(cohort, alpha) %>% summarize( lwr=quantile(iei,0.25), median = median(iei), upr=quantile(iei,0.75)) %>% filter(alpha=="no")
colnames(ieidf1.sum1)[2]<-"Rank"

#others_all
ieidfx2.sum1 <- ieidfx2 %>% group_by(cohort) %>% summarize(lwr=quantile(iei,0.25), median = median(iei), upr=quantile(iei,0.75))
ieidfx2.sum1$Rank <- "Other_All"
ieidfx2.sum1 <- ieidfx2.sum1[c(1,5,2:4)]

boxplotdf <- rbind(ieidf1.sum, ieidf1.sum1, ieidfx2.sum1) #combine df for plot
boxplotdf$Rank <- factor(boxplotdf$Rank, levels=c(1:12, "no", "Other_All")) #fix levels for plot

rankplot<-ggplot(boxplotdf, aes(factor(Rank), median)) + geom_point() + geom_boxplot(alpha=.4) + theme_minimal() +
  xlab("Rank") + ylab("Median IEI (mins)")
rankplot



#### Attacks per 60 minutes----

#match in ranks - will give "Start"/"End" as NAs
attackdata <- cohortdata
for (i in 1:length(robjs)){
  attackdata[[i]]$Rank <- robjs[[i]]$Rank[match(attackdata[[i]]$Actor, robjs[[i]]$Player)]
}

attackdata <- Map(cbind, attackdata, cohort=names(alphas)) #add in var cohort name
atfreq <- do.call('rbind', attackdata) #make one df
atfreq <- atfreq %>% mutate(alpha = ifelse(Rank==1, 'yes', 'no')) 
atfreq$uniqueid <- paste(atfreq$cohort, atfreq$uniqueobs, sep="-")

sem <- function(x) sd(x)/sqrt(length(x))


head(atfreq)

times <- atfreq %>% group_by(cohort,uniqueobs) %>% summarize(begintimes=min(newtime),totaltime=max(newtime))

totalatts <- atfreq %>% group_by(cohort,uniqueobs,alpha) %>% summarize(totalattacks = n()) %>% filter(alpha=='no'|alpha=='yes') #get total attacks by alpha v others
totalatts1 <- rbind(totalatts, cbind(unique(totalatts[,1:2]), alpha='no', totalattacks=0), cbind(unique(totalatts[,1:2]), alpha='yes', totalattacks=0) ) #add zeros in
totalatts1 <- totalatts1 %>% group_by(cohort,uniqueobs,alpha) %>% summarize(totalattacks=sum(totalattacks))
totalatts2 <- totalatts1 %>% full_join(times) %>% mutate(per60  = (60*totalattacks)/totaltime)

ggplot(totalatts2, aes(totalattacks)) + geom_histogram() + facet_wrap(~alpha)
ggplot(totalatts2, aes(per60)) + geom_histogram() + facet_wrap(~alpha)

#summary across cohorts
totalatts2 %>% 
  ungroup() %>% 
  group_by(cohort,alpha) %>% 
  summarize(totalattacks=sum(totalattacks), sumtime=sum(totaltime)) %>%
  mutate(per60 = (60*totalattacks)/sumtime) %>% 
  ungroup() %>%
  group_by(alpha) %>% 
  summarize(median= median(per60), mean=mean(per60), sem=sem(per60),lqr=quantile(per60, .25), uqr=quantile(per60, .75) )


####  Median STTC values versus sttc=0

#Get Time Data: Alpha vs Others - whole sample
startt<-list()
endt<-list()
alpha.all<-list()
others.all<-list()
for(i in 1:length(alphas)){
  dfy<-cohortdata[[i]]
  startt[[i]] <- dfy$cumetime[1]
  endt[[i]] <- dfy$cumetime[nrow(dfy)]
  dfz <- dfy %>% filter(Actor!="End") %>% filter(Actor!="Start")
  alpha.all[[i]] <-  dfz %>% filter(dfz$Actor==alphas[[i]]) %>% .$cumetime %>% as.numeric  #alpha male
  others.all[[i]] <- dfz %>% filter(dfz$Actor!=alphas[[i]]) %>% .$cumetime %>% as.numeric #all others
}

names(startt)<-names(alphas)
names(endt)<-names(alphas)
names(alpha.all)<-names(alphas)
names(others.all)<-names(alphas)


#dt=120secs,2mins
delta2.all <- lapply(names(alpha.all), function(i) tiling.corr(alpha.all[[i]], others.all[[i]], dt=2, rec.time=c(startt[[i]], endt[[i]])))
names(delta2.all)<-names(alphas)
t.test(unlist(delta2.all), mu=0, alternative=c("less"))

#dt=90secs,1.5mins
delta1.5.all <- lapply(names(alpha.all), function(i) tiling.corr(alpha.all[[i]], others.all[[i]], dt=1.5, rec.time=c(startt[[i]], endt[[i]])))
names(delta1.5.all)<-names(alphas)
t.test(unlist(delta1.5.all), mu=0, alternative=c("less"))

#dt=60secs,1mins
delta1.all <- lapply(names(alpha.all), function(i) tiling.corr(alpha.all[[i]], others.all[[i]], dt=1, rec.time=c(startt[[i]], endt[[i]])))
names(delta1.all)<-names(alphas)
t.test(unlist(delta1.all), mu=0, alternative=c("less"))


summary(unlist(delta2.all))
sem(unlist(delta2.all))

summary(unlist(delta1.5.all))
sem(unlist(delta1.5.all))

summary(unlist(delta1.all))
sem(unlist(delta1.all))



#### Bootstrapping ----
# need to source 'cumjoin.R' file.
# see separate file:  sttc_bootstrap.R



#### Randomization Test ----
 # see separate file:  sttc_randomization_sample.R



#### Cumulative by day change in STTC:----


#dt=90
matX1.5 <- matrix(nrow=length(alphas), ncol=23)
for(i in 1:length(alphas)){
  for(j in 1:23){
    temp1<-cohortdata[[i]] %>% filter(day<=j)
    vec <- temp1 %$% split(., uniqueobs) %>% lapply(., function(x) x$newtime)   #list of vectors of times
    temp1$cumetime <- cumjoin(vec, gap=2.01)
    xx<-temp1
    startt <- xx$cumetime[1] #start time
    endt <- xx$cumetime[nrow(xx)] #end time
    xy <- xx %>% filter(Actor!="End") %>% filter(Recipient!="Start") #remove start/ends
    aa <- xy %>% filter(Actor==alphas[[i]]) %>% .$cumetime %>% as.numeric #get alpha's time of events
    bb <- xy %>% filter(Actor!=alphas[[i]]) %>% .$cumetime %>% as.numeric #get time of events of others
    matX1.5[i,j] <- tiling.corr(aa,bb,dt=1.5, rec.time=c(startt, endt))
  }
}

matX1.5df <- matX1.5 %>% as.data.frame
rownames(matX1.5df)<-names(alphas)
colnames(matX1.5df)<-1:23
matXdf.long.dt1.5 <- reshape2::melt(as.matrix(matX1.5df))
matXdf.long.dt1.5



### dt=120
matX2 <- matrix(nrow=length(alphas), ncol=23)

for(i in 1:length(alphas)){
  for(j in 1:23){
    temp1<-cohortdata[[i]] %>% filter(day<=j)
    vec <- temp1 %$% split(., uniqueobs) %>% lapply(., function(x) x$newtime)   #list of vectors of times
    temp1$cumetime <- cumjoin(vec, gap=2.01)
    xx<-temp1
    startt <- xx$cumetime[1] #start time
    endt <- xx$cumetime[nrow(xx)] #end time
    xy <- xx %>% filter(Actor!="End") %>% filter(Recipient!="Start") #remove start/ends
    aa <- xy %>% filter(Actor==alphas[[i]]) %>% .$cumetime %>% as.numeric #get alpha's time of events
    bb <- xy %>% filter(Actor!=alphas[[i]]) %>% .$cumetime %>% as.numeric #get time of events of others
    matX2[i,j] <- tiling.corr(aa,bb,dt=2, rec.time=c(startt, endt))
  }
}


matX2df <- matX2 %>% as.data.frame
rownames(matX2df)<-names(alphas)
colnames(matX2df)<-1:23
matXdf.long.dt2 <- reshape2::melt(as.matrix(matX2df))
matXdf.long.dt2





### dt=60
matX1 <- matrix(nrow=length(alphas), ncol=23)

for(i in 1:length(alphas)){
  for(j in 1:23){
    temp1<-cohortdata[[i]] %>% filter(day<=j)
    vec <- temp1 %$% split(., uniqueobs) %>% lapply(., function(x) x$newtime)   #list of vectors of times
    temp1$cumetime <- cumjoin(vec, gap=2.01)
    xx<-temp1
    startt <- xx$cumetime[1] #start time
    endt <- xx$cumetime[nrow(xx)] #end time
    xy <- xx %>% filter(Actor!="End") %>% filter(Recipient!="Start") #remove start/ends
    aa <- xy %>% filter(Actor==alphas[[i]]) %>% .$cumetime %>% as.numeric #get alpha's time of events
    bb <- xy %>% filter(Actor!=alphas[[i]]) %>% .$cumetime %>% as.numeric #get time of events of others
    matX1[i,j] <- tiling.corr(aa,bb,dt=1, rec.time=c(startt, endt))
  }
}


matX1df <- matX1 %>% as.data.frame
rownames(matX1df)<-names(alphas)
colnames(matX1df)<-1:23
matXdf.long.dt1 <- reshape2::melt(as.matrix(matX1df))
matXdf.long.dt1

## Put together into long df
matXdf.long.dt2$dt = 2
matXdf.long.dt1.5$dt = 1.5
matXdf.long.dt1$dt = 1 
matXdf.long <- rbind(matXdf.long.dt2, matXdf.long.dt1.5, matXdf.long.dt1)

##remove values past last day of each cohort's observations
maxdays <- lapply(cohortdata, function(x) max(x$day))
matXdf.long$maxdays<-unlist(maxdays)
matXdf.long$value <- ifelse(matXdf.long$Var2>matXdf.long$maxdays, NA, matXdf.long$value)

matXdf.long <- matXdf.long %>% filter(Var2<=maxdays)


## Plotting Suppl Fig 4
ggplot(matXdf.long, aes(x=factor(Var2), y=value)) + geom_hline(yintercept=0, color="red") + geom_boxplot(outlier.colour=NA) + 
  facet_wrap(~dt) + theme_minimal()

cumeplot<-ggplot(matXdf.long, aes(x=factor(Var2), y=value, fill=factor(dt))) + geom_hline(yintercept=0, color="red") + geom_boxplot(outlier.colour=NA) + 
  theme_minimal() + xlab("Day") + ylab("Cumulative STTC value") +
  scale_fill_manual(values=c("gray20", "gray55", "gray90"))



#get pvalue with one-sample T-test each of 23 days.. dt =1.5
res<-NULL
for(i in 1:23) {res[[i]]<-matXdf.long %>% filter(Var2==i) %>% filter(dt==1.5) %>% .$value %>% as.numeric %>% 
                            t.test(., mu=0, alternative=c("less")) %>% .$p.value}
res




#### STTC by rank combination ----


##  Relationship Matrix Function:

rshpmat <- function(dfz, dtx = 1.5){
  
  myvals<-matrix(NA, 12,12)
  
  for(i in 1:12){
    for(j in 1:12){
      xx <- dfz
      startt <- xx$cumetime[1] #start time
      endt <- xx$cumetime[nrow(xx)] #end time
      xy <- xx %>% filter(Actor!="End") %>% filter(Actor!="Start") #remove start/ends
      aa <- xy %>% filter(Actor==i) %>% .$cumetime %>% as.numeric #get alpha's time of events
      bb <- xy %>% filter(Actor==j) %>% .$cumetime %>% as.numeric #get time of events of others
      myvals[i,j] <- tiling.corr(aa,bb,dt=dtx, rec.time=c(startt, endt))
    }
  }
  rownames(myvals)<-colnames(myvals)<-1:12
  myvals<-round(myvals,2)  #by ID
  return(myvals)
}

# get matrix of STTC values for each cohort by mouseID
mats <- lapply(cohortdata, rshpmat)



## ADD ranks to above matrix
for(i in 1:length(alphas)){
  rownames(mats[[i]]) <- robjs[[i]]$Rank[match(rownames(mats[[i]]), robjs[[i]]$Player)]
  colnames(mats[[i]]) <- rownames(mats[[i]])
  diag(mats[[i]])<-NA
}

#order by rank
matsX <- lapply(mats, function(x) x[as.character(1:12),as.character(1:12)])
matsX

se <- function(x) sqrt(var(x,na.rm=T)/length(x))

#Get Means for each Rank-Rank Combination Across cohorts
a <- array(unlist(matsX), c(12, 12, 13))#rows,cols,number in list
a1 <- apply(a, 1:2, mean, na.rm=T)
round(a1,3)
apply(a1, 1, mean,na.rm=T)
apply(a1, 1, se)

### Plot Matrix of Rank-Rank Combination Means
tty<-reshape2::melt(a1 %>% round(.,3))
tty$value1<-ifelse(tty$Var1==1, NA, tty$value) #Adds in NA for alpha values if against alpha's sequence for others...
tty1<-tty %>% gather(key,value,3:4) #longform for ggplot
tty1$key <- ifelse(tty1$key=="value", "inc. alpha", "exc. alpha")

rankplot1 <-   ggplot(tty1, aes(factor(Var2), value, fill=factor(key))) + geom_hline(yintercept=0, color="red")+ 
  geom_boxplot(outlier.colour=NA)+ scale_fill_manual(values=c("gray50", "gray99")) +
  theme_minimal() + xlab("Rank") + ylab("STTC")


## One-sample t-test
tty1 %>% filter(Var2==1) %>% filter(key=="exc. alpha") %>% .$value %>% t.test(., mu=0, alternative=c("less"))
tty1 %>% filter(Var2==1) %>% filter(key=="exc. alpha") %>% .$value %>% mean(., na.rm=T)
tty1 %>% filter(Var2==1) %>% filter(key=="exc. alpha") %>% .$value %>% se



### Get STTC value of each alpha male (mean across relationships)
alphasttc <- lapply(matsX,function(x) x[1,])
alphasttc.vals <- lapply(alphasttc, mean, na.rm=T) %>% unlist


#all days despotic males vs STTC
plot(winprops, alphasttc.vals)
cor.test(winprops, alphasttc.vals, method="s")

corplot<- data.frame(winprop=winprops, sttc=alphasttc.vals) %>%
  ggplot(., aes(winprop, sttc)) + geom_point(size=5) +
  stat_smooth(method="lm", se=F, color="red", lty=2) +
  theme_minimal() + xlab("Win proportion") + ylab("STTC")



### Are other ranks correlated with despotism ?
ranks.res <- NULL
for(i in 1:12){
  rank.sttc <- lapply(matsX,function(x) x[i,])
  rank.sttc.vals <- lapply(rank.sttc, mean, na.rm=T) %>% unlist
  ranks.res[[i]] <- cor.test(winprops, rank.sttc.vals, method="s")[[3]]
}

ranks.res


#### STTC means of relationships by alphs vs hierarchy measures:
cor.test(hvals, alphasttc.vals, method="s")
cor.test(dcival, alphasttc.vals, method="s")
cor.test(winprops, alphasttc.vals, method="s")






