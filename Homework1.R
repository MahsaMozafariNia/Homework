library(faraway)
data(pima)
d=pima
names(d)
d$diastolic[d$diastolic==0]=NA
d$glucose[d$glucose==0]=NA
d$triceps[d$triceps==0]=NA
d$bmi[d$bmi==0]=NA
d$insulin[d$insulin==0]=NA
x=is.na(d$triceps)
par(mfrow=c(1,2))
hist(d$age[x==TRUE],xlab="age for Missing Triceps",main="",col="red",border="black")
hist(d$age[x==FALSE],xlab="age for observed Triceps",main="",col="red",border="black")
#y=is.finite(d$triceps)
#hist(d$age[y],xlab="age for observed Triceps",main="Histogram of age for observed triceps",col="red",border="black")

#Question 2
mean_insulin=mean(d$insulin,na.rm=T)
larger=which(d$insulin>=mean_insulin)
larger
#larger=which(d$insulin>=mean_insulin & is.na(d$bmi)==FALSE)
smaller=which(d$insulin<=mean_insulin)
smaller
#mean(d$bmi[larger])
mean(d$bmi[smaller],na.rm=T)
mean(d$bmi[larger],na.rm=T)

#Question 3
#average and standard deviation  for insulin attribute for both levels of test attribute.
mean_insulin_test_1=mean(d$insulin[d$test==1],na.rm=T)
mean_insulin_test_1
sd_insulin_test_1=sd(d$insulin[d$test==1],na.rm=T)
sd_insulin_test_1

mean_insulin_test_0=mean(d$insulin[d$test==0],na.rm=T)
#tapply(d$insulin,d$test,mean,na.rm=T)
#tapply(d$insulin,d$test,sd,na.rm=T)
mean_insulin_test_0
sd_insulin_test_0=sd(d$insulin[d$test==0],na.rm=T)
sd_insulin_test_0
CIspositive=function(mean,sd,n){return(mean+1.96*(sd/sqrt(n)))}
CIsnegative=function(mean,sd,n){return(mean-1.96*(sd/sqrt(n)))}
n_1=length(which(d$test==1 & is.na(d$insulin)==FALSE))
n_0=length(which(d$test==0 & is.na(d$insulin)==FALSE))
CIspositive_test_1=c(CIspositive(mean_insulin_test_1,sd_insulin_test_1,n_1),CIsnegative(mean_insulin_test_1,sd_insulin_test_1,n_1))
CIspositive_test_0=c(CIspositive(mean_insulin_test_0,sd_insulin_test_0,n_0),CIsnegative(mean_insulin_test_0,sd_insulin_test_0,n_0))
c("test1",CIspositive_test_1,"test2",CIspositive_test_0)
