#Survival analysis

#stime <- c(2.2, 3, 8.4, 7.5) #survival time
#status <- c(1,0,1,0) #right censored? still alive?
#summary(survfit(Surv(aml$time[1:11],aml$status[1:11])))
#fit1 <- survfit(Surv(aml$time[1:11],aml$status[1:11]))
#plot(fit1)
#survdiff(); rho=0 gives log-rank test, rho=1 gives wilcoxon test
#summary( survreg(Surv(time, status) ~ age+sex, data=cancer, dist="weibull") )

#stress tolerance data

###harvest, control, bolting date###
modeldata<-co[!is.na(co$BoltDate),]
levels(modeldata$BoltedatH)
modeldata$BoltedatH<-factor(modeldata$BoltedatH, levels=c("n","y"))
modeldata$bolt.num<-as.numeric(modeldata$BoltedatH)

summary(survfit(Surv(BoltDate, bolt.num) ~ Origin+CrownDiam.mm, data=modeldata))

fit<-survfit(Surv(BoltDate, bolt.num) ~ Origin, data=modeldata)
plot(fit)
summary(fit)



summary( survreg(Surv(BoltDate, bolt.num) ~ Origin+(1|PopID), data=modeldata, dist="weibull") )