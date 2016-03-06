### Bootstrap

#### Bootstrap Over 1000 resamples of all:

resres <- list()
for(j in 1:1000){

df.all.cohorts <- list()
for(i in 1:length(alphas)){
  tmp <- cohortdata[[i]] #
  len<-length(unique(tmp$uniqueobs)) 
  tmp1 <- split(tmp, tmp$uniqueobs)
  tmp2 <- sample(tmp1, len, replace=T) #bootstrap
  tmp3 <- Map(cbind, tmp2, newuniqueobs=1:len)
  dfy <- do.call('rbind', tmp3)
  vec <- tmp3 %>% lapply(., function(x) x$newtime)   #list of vectors of times
  dfy$cumetime <- cumjoin(vec, gap=2.01)
  alphamale <- alphas[[i]] #
  
  startt <- dfy$cumetime[1]
  endt <- dfy$cumetime[nrow(dfy)]
  dfz <- dfy %>% filter(Actor!="End") %>% filter(Actor!="Start")
  alpha.all <-  dfz %>% filter(dfz$Actor==alphamale) %>% .$cumetime %>% as.numeric  #alpha male
  others.all <- dfz %>% filter(dfz$Actor!=alphamale) %>% .$cumetime %>% as.numeric #all others
  
  aa<-tiling.corr(alpha.all, others.all, dt=2, rec.time=c(startt, endt))
  bb<-tiling.corr(alpha.all, others.all, dt=1.5, rec.time=c(startt, endt))
  cc<-tiling.corr(alpha.all, others.all, dt=1, rec.time=c(startt, endt))
  df.all.cohorts[[i]] <- data.frame(dt2=aa, dt1.5=bb, dt1=cc)
  
}
resres[[j]] <- do.call('rbind', df.all.cohorts)
}

#saveRDS(resres, "dtsbootstrap_final.RData")

resres1 <- Map(cbind, resres, bootstrap=1:1000)
resres2 <- do.call('rbind', resres1)
resres2$cohort <- names(alphas)
resres3 <- resres2 %>% group_by(bootstrap) %>% summarize(m2 = median(dt2), m1.5 = median(dt1.5), m1 = median(dt1))

#dt2
hist(resres3$m2)
a <- mean(resres3$m2)
s <- sd(resres3$m2)
n <- 1000
error <- qnorm(0.975)*s/sqrt(n)
left <- a-error
right <- a+error
left
right
mean(resres3$m2)

#dt1.5
hist(resres3$m1.5)
a <- mean(resres3$m1.5)
s <- sd(resres3$m1.5)
n <- 1000
error <- qnorm(0.975)*s/sqrt(n)
left <- a-error
right <- a+error
left
right
mean(resres3$m1.5)

#dt1
hist(resres3$m1)
a <- mean(resres3$m1)
s <- sd(resres3$m1)
n <- 1000
error <- qnorm(0.975)*s/sqrt(n)
left <- a-error
right <- a+error
left
right
mean(resres3$m1)


wilcox.test(resres3$m2, mu=0) #p-value < 2.2e-16
wilcox.test(resres3$m1.5, mu=0) #p-value < 2.2e-16
wilcox.test(resres3$m1, mu=0) #p-value < 2.2e-16

t.test(resres3$m2, mu=0) #p-value < 2.2e-16
t.test(resres3$m1.5, mu=0) #p-value < 2.2e-16
t.test(resres3$m1, mu=0) #p-value < 2.2e-16


#over all cohorts
resres4 <- resres2 %>% group_by(cohort) %>% summarize(m2 = median(dt2), m1.5 = median(dt1.5), m1 = median(dt1))
resres4
