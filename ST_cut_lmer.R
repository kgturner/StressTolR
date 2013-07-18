#ST stress treatment - simulated herbivory
#Stress Tolerance, REML, using lme4
#mixed effect models 
library(lme4)
library(lsmeans)
library(ggplot2)
library(plyr)

#pop control means to look at stress treatments
co<-read.table("STControlsubset.txt", header=T, sep="\t", quote='"', row.names=1) #controlsubset
se <- function(x) sqrt(var(x)/length(x))
comeans<- ddply(co, .(PopID, Origin, Latitude), summarize, CtrlPopCount=length(PopID), CtrlPopLf=mean(LfCountH), CtrlPopLfSE=se(LfCountH),
                CtrlPopShoot=mean(ShootMass.g), CtrlPopShootSE=se(ShootMass.g))

####Cut, Origin * Lat####
cu<-read.table("STCutsubset.txt", header=T, sep="\t", quote='"', row.names=1) #cutsubset
head(cu)
cuLR <- lapply(names(cu)[c(13:14, 42)],function(n) CGtrait.LR.int(n,cu)) #crow, root, root.log, all gaussian
1-pchisq(10.85,1)
#non-gaussian?
# cu<- cbind(cu, bolt.bin=as.numeric(cu$BoltedatH)-1)
# write.table(cu, file="STCutsubset.txt", sep="\t", quote=F)
cuBatH <- CGtrait.LR.int("bolt.bin", cu, family=binomial)
cuP <- lapply(names(cu)[c(10,27)],function(n) CGtrait.LR.int(n,cu, family=poisson)) #lfcountH, boltdate, all poisson

# cumodels <- CGtrait.models("RootH.log",cu)
# cumodels
# qqnorm(resid(cumodels$model2), main="Q-Q plot for residuals")
# qqline(resid(cumodels$model2))
# shapiro.test(resid(cumodels$model2))

#####cut, root.log, harvest###
modeldata<-cu[!is.na(cu$RootH.log),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

modelobar<-lmer(RootH.log ~ Origin * Latitude + (Origin|PopID/Mom), family=gaussian,data=modeldata)
model1raw<-lmer(RootH.log ~ Origin * Latitude + (1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelobar, model1raw)
model2raw<-lmer(RootH.log ~ Origin * Latitude + (1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(RootH.log ~ Origin * Latitude + (1|blank), family=gaussian,data=modeldata) # Test population effect
print(anova(model2raw,model1raw), digits=22) # mom not sig
(lambda <- (-2)*(-191.6041864411309347815 - (-191.6041784571340258481)))
1-pchisq(1.596799e-05,1)
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(13.036,1)
modelI <- lmer(RootH.log ~ Origin + Latitude + (1|PopID), family=gaussian,data=modeldata)
anova(modelI,model2raw)

modelO <- lmer(RootH.log ~ Origin +(1|PopID), family=gaussian,data=modeldata)
anova(modelO,modelI) 

modelL <- lmer(RootH.log ~ Latitude +(1|PopID), family=gaussian,data=modeldata)
anova(modelL,modelI) 
modelI
CI.LS.poisson(modelI)

#testing nested range
# model1raw<-lmer(RootH.log ~ Origin * Latitude + (1|Origin/PopID/Mom), family=gaussian,data=modeldata)
# model2raw<-lmer(RootH.log ~ Origin * Latitude + (1|Origin/PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
# model3raw<-lmer(RootH.log ~ Origin * Latitude + (1|Origin/blank), family=gaussian,data=modeldata) # Test population effect
# print(anova(model2raw,model1raw), digits=22) # mom not sig
# (lambda <- (-2)*(-191.6041864411309347815 - (-191.6041784571340258481)))
# 1-pchisq(1.596799e-05,1)
# anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
# 1-pchisq(13.036,1)
modelI <- lmer(RootH.log ~ Origin + Latitude + (1|Origin/PopID), family=gaussian,data=modeldata)
# anova(modelI,model2raw)
# 
# modelO <- lmer(RootH.log ~ Origin +(1|Origin/PopID), family=gaussian,data=modeldata)
# anova(modelO,modelI) 
# 
modelL <- lmer(RootH.log ~ Latitude +(1|Origin/PopID), family=gaussian,data=modeldata)
# anova(modelL,modelI) 
# 
modelIo <- lmer(RootH.log ~ Origin + Latitude + (1|PopID), family=gaussian,data=modeldata)
# anova(modelI, modelIo)
# print(anova(modelI, modelIo), digits = 22)
# (lambda <- (-2)*(-192.3092150952082306503 - (-191.6328860987748043954)))
# 1-pchisq(1.352658,1)
modelIo
# modelI
# 
# modelIo2 <- lmer(RootH.log ~ Origin + Latitude + (Origin|PopID), family=gaussian,data=modeldata)
# modelIo2
# anova(modelIo, modelIo2)
# anova(modelI, modelIo2)
# 
# modelIo3 <- lmer(RootH.log ~ Origin + Latitude + (1|Origin) + (1|PopID), family=gaussian,data=modeldata)
# modelIo3
# 
# modelIo4 <- lmer(RootH.log ~ Origin + Latitude + (1|Origin + PopID), family=gaussian,data=modeldata)
# modelIo4
# 
# modelIo2o <- lmer(RootH.log ~ Latitude + (Origin|PopID), family=gaussian,data=modeldata)
# modelIo2o
# 
# modelIo5 <- lmer(RootH.log ~ Origin + Latitude + (1|Origin/PopID) -(1|Origin), family=gaussian,data=modeldata)
# modelIo5
# 
# modelIo6 <- lmer(RootH.log ~ Origin + Latitude + (1 - 1|Origin/PopID), family=gaussian,data=modeldata)
# modelIo6

modelIo7 <- lmer(RootH.log ~ Origin + Latitude + (1 - Origin|Origin/PopID), family=gaussian,data=modeldata)
modelIo7

#explicit tradeoff - using lf count
modeldata <- merge(modeldata, comeans, all.x=TRUE)
modeldata <- modeldata[!is.na(modeldata$CtrlPopLf),]

modelobar<-lmer(RootH.log ~ Origin * CtrlPopLf*Latitude +(Origin|PopID/Mom), family=gaussian,data=modeldata)
model1raw<-lmer(RootH.log ~ Origin * CtrlPopLf* Latitude + (1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelobar, model1raw)
model2raw<-lmer(RootH.log ~ Origin * CtrlPopLf* Latitude +(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(RootH.log ~ Origin * CtrlPopLf* Latitude +(1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(56.023,1)
modelI <- lmer(RootH.log ~ Origin * CtrlPopLf+ Latitude + (1|PopID), family=gaussian,data=modeldata)
anova(modelI, model2raw)

modelL <- lmer(RootH.log ~ Origin * CtrlPopLf+(1|PopID), family=gaussian,data=modeldata)
anova(modelL, modelI)

modelCint <- lmer(RootH.log ~ Origin + CtrlPopLf+Latitude+(1|PopID), family=gaussian,data=modeldata)
anova(modelI, modelCint)

modelC <- lmer(RootH.log ~ Origin +Latitude+(1|PopID), family=gaussian,data=modeldata)
anova(modelCint, modelC)

modelOraw<-lmer(RootH.log ~ Latitude+(1|PopID), family=gaussian,data=modeldata)
anova(modelOraw,modelC) #test for significance of origin - origin NOT sig....!

# qplot(data=modeldata,CtrlPopLf, RootH.log, color = Origin)+geom_point(position="jitter")
# moddata <- ddply(modeldata, .(PopID, Origin, Latitude, CtrlPopLf), summarize, popCount=length(PopID), popRootH.log=mean(RootH.log))
# qplot(data=moddata,CtrlPopLf, popRootH.log, color = Origin, xlab="Population mean leaf number in control",
#       ylab="Population mean days to RootH.log in herbivory", main="Performance in herbivory vs control, leaf no.") +geom_smooth(method=glm, se=TRUE)
# 
#explicit trade-off using shootmass
modeldata <- merge(modeldata, comeans, all.x=TRUE)
modeldata <- modeldata[!is.na(modeldata$CtrlPopShoot),]

modelobar<-lmer(RootH.log ~ Origin * CtrlPopShoot*Latitude +(Origin|PopID/Mom), family=gaussian,data=modeldata)
model1raw<-lmer(RootH.log ~ Origin * CtrlPopShoot* Latitude + (1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelobar, model1raw)
model2raw<-lmer(RootH.log ~ Origin * CtrlPopShoot* Latitude +(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(RootH.log ~ Origin * CtrlPopShoot* Latitude +(1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(56.023,1)

modelg <- glm(RootH.log ~ Origin*CtrlPopShoot*Latitude, family=gaussian,data=modeldata)
modelg1 <- glm(RootH.log ~ Origin*CtrlPopShoot+Latitude, family=gaussian,data=modeldata)
anova(modelg1, modelg, test="LRT") 
qchisq(pval,1,lower=FALSE)#chisq value

modelg3<- glm(RootH.log ~ Origin*CtrlPopShoot, family=gaussian,data=modeldata)
anova(modelg3,modelg1, test="LRT")
qchisq(pval,1,lower=FALSE)#chisq value
modelg2<- glm(RootH.log ~Origin +CtrlPopShoot+Latitude, family=gaussian,data=modeldata)
anova(modelg2,modelg1, test="LRT")
qchisq(pval,1,lower=FALSE)#chisq value

modelg4 <- glm(RootH.log ~Origin+Latitude, family=gaussian, data=modeldata)
anova(modelg4, modelg2, test="LRT")
modelg5 <- glm(RootH.log~CtrlPopShoot+Latitude, family=gaussian, data=modeldata)
anova(modelg2, modelg5, test="LRT")
# 
# qplot(data=modeldata,CtrlPopShoot, RootH.log, color = Origin)+geom_point(position="jitter")
# moddata <- ddply(modeldata, .(PopID, Origin, Latitude, CtrlPopShoot), summarize, popCount=length(PopID), popRootH.log=mean(RootH.log))
# qplot(data=moddata,CtrlPopShoot, popRootH.log, color = Origin, 
#       xlab="Population mean shoot mass in control treatment", 
#       ylab="Population mean days to RootH.log in herbivory treatment", main="Performance in herbivory vs. control treatments") +geom_smooth(method=glm, se=TRUE)
# 

##cut, crown
modeldata<-cu[!is.na(cu$CrownDiam.mm),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

modelobar<-lmer(CrownDiam.mm ~ Origin * Latitude + (Origin|PopID/Mom), family=gaussian,data=modeldata)
model1raw<-lmer(CrownDiam.mm ~ Origin * Latitude + (1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelobar, model1raw)
model2raw<-lmer(CrownDiam.mm ~ Origin * Latitude + (1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(CrownDiam.mm ~ Origin * Latitude + (1|blank), family=gaussian,data=modeldata) # Test population effect
print(anova(model2raw,model1raw), digits=22) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelI <- lmer(CrownDiam.mm ~ Origin + Latitude + (1|PopID), family=gaussian,data=modeldata)
modelO <- lmer(CrownDiam.mm ~ Origin +(1|PopID), family=gaussian,data=modeldata)
anova(modelO,modelI) 

modelL <- lmer(CrownDiam.mm ~ Latitude +(1|PopID), family=gaussian,data=modeldata)
anova(modelL,modelI)

lsmeans(modelI, ~Origin, conf=95)

#explicit tradeoff - using lf count
modeldata <- merge(modeldata, comeans, all.x=TRUE)
modeldata <- modeldata[!is.na(modeldata$CtrlPopLf),]

modelobar<-lmer(CrownDiam.mm ~ Origin * CtrlPopLf*Latitude +(Origin|PopID/Mom), family=gaussian,data=modeldata)
model1raw<-lmer(CrownDiam.mm ~ Origin * CtrlPopLf* Latitude + (1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelobar, model1raw)
model2raw<-lmer(CrownDiam.mm ~ Origin * CtrlPopLf* Latitude +(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(CrownDiam.mm ~ Origin * CtrlPopLf* Latitude +(1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(56.023,1)
modelI <- lmer(CrownDiam.mm ~ Origin * CtrlPopLf+ Latitude + (1|PopID), family=gaussian,data=modeldata)
anova(modelI, model2raw)

modelL <- lmer(CrownDiam.mm ~ Origin * CtrlPopLf+(1|PopID), family=gaussian,data=modeldata)
anova(modelL, modelI)

modelCint <- lmer(CrownDiam.mm ~ Origin + CtrlPopLf+Latitude+(1|PopID), family=gaussian,data=modeldata)
anova(modelI, modelCint)

modelC <- lmer(CrownDiam.mm ~ Origin+Latitude +(1|PopID), family=gaussian,data=modeldata)
anova(modelCint, modelC)

modelOraw<-lmer(CrownDiam.mm ~ Latitude+(1|PopID), family=gaussian,data=modeldata)
anova(modelOraw,modelC) #test for significance of origin - origin NOT sig....!

# qplot(data=modeldata,CtrlPopLf, CrownDiam.mm, color = Origin)+geom_point(position="jitter")
# moddata <- ddply(modeldata, .(PopID, Origin, Latitude, CtrlPopLf), summarize, popCount=length(PopID), popCrownDiam.mm=mean(CrownDiam.mm))
# qplot(data=moddata,CtrlPopLf, popCrownDiam.mm, color = Origin, xlab="Population mean leaf number in control",
#       ylab="Population mean days to CrownDiam.mm in herbivory", main="Performance in herbivory vs control, leaf no.") +geom_smooth(method=glm, se=TRUE)
# 
#explicit trade-off using shootmass
modeldata <- merge(modeldata, comeans, all.x=TRUE)
modeldata <- modeldata[!is.na(modeldata$CtrlPopShoot),]

modelobar<-lmer(CrownDiam.mm ~ Origin * CtrlPopShoot*Latitude +(Origin|PopID/Mom), family=gaussian,data=modeldata)
model1raw<-lmer(CrownDiam.mm ~ Origin * CtrlPopShoot* Latitude + (1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelobar, model1raw)
model2raw<-lmer(CrownDiam.mm ~ Origin * CtrlPopShoot* Latitude +(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(CrownDiam.mm ~ Origin * CtrlPopShoot* Latitude +(1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(56.023,1)
delOraw,modelC) #test for significance of origin - origin NOT sig....!
# 
modelg <- glm(CrownDiam.mm ~ Origin*CtrlPopShoot*Latitude, family=gaussian,data=modeldata)
modelg1 <- glm(CrownDiam.mm ~ Origin*CtrlPopShoot+Latitude, family=gaussian,data=modeldata)
anova(modelg1, modelg, test="LRT") 
qchisq(pval,1,lower=FALSE)#chisq value

modelg3<- glm(CrownDiam.mm ~ Origin*CtrlPopShoot, family=gaussian,data=modeldata)
anova(modelg3,modelg1, test="LRT")
qchisq(pval,1,lower=FALSE)#chisq value
modelg2<- glm(CrownDiam.mm ~Origin +CtrlPopShoot+Latitude, family=gaussian,data=modeldata)
anova(modelg2,modelg1, test="LRT")
qchisq(pval,1,lower=FALSE)#chisq value

modelg4 <- glm(CrownDiam.mm ~Origin+Latitude, family=gaussian, data=modeldata)
anova(modelg4, modelg2, test="LRT")
modelg5 <- glm(CrownDiam.mm~CtrlPopShoot+Latitude, family=gaussian, data=modeldata)
anova(modelg2, modelg5, test="LRT")

# qplot(data=modeldata,CtrlPopShoot, CrownDiam.mm, color = Origin)+geom_point(position="jitter")
# moddata <- ddply(modeldata, .(PopID, Origin, Latitude, CtrlPopShoot), summarize, popCount=length(PopID), popCrownDiam.mm=mean(CrownDiam.mm))
# qplot(data=moddata,CtrlPopShoot, popCrownDiam.mm, color = Origin, 
#       xlab="Population mean shoot mass in control treatment", 
#       ylab="Population mean days to CrownDiam.mm in herbivory treatment", main="Performance in herbivory vs. control treatments") +geom_smooth(method=glm, se=TRUE)


####cut, lf count, harvest, mom is sig, do by hand###
modeldata<-cu[!is.na(cu$LfCountH),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

modelobar<-lmer(LfCountH ~ Origin * Latitude + (Origin|PopID/Mom), family=poisson,data=modeldata)
model1raw<-lmer(LfCountH ~ Origin * Latitude + (1|PopID/Mom), family=poisson,data=modeldata)
anova(modelobar, model1raw)
model2raw<-lmer(LfCountH ~ Origin * Latitude + (Origin|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(LfCountH ~ Origin * Latitude + (Origin|blank), family=poisson,data=modeldata) # Test population effect
anova(model2raw,modelobar) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelI <- lmer(LfCountH ~ Origin + Latitude + (Origin|PopID/Mom), family=poisson,data=modeldata)
anova(modelI,modelobar)

modelL <- lmer(LfCountH ~ Origin +(Origin|PopID/Mom), family=poisson,data=modeldata)
anova(modelL,modelI) 

modelOraw<-lmer(LfCountH ~ Latitude + (Origin|PopID/Mom), family=poisson,data=modeldata)
anova(modelOraw,modelI) #test for significance of origin 

CI.LS.poisson(modelI)

#explicit tradeoff - using lf count
modeldata <- merge(modeldata, comeans, all.x=TRUE)
modeldata <- modeldata[!is.na(modeldata$CtrlPopLf),]

modelobar<-lmer(LfCountH ~ Origin * CtrlPopLf*Latitude +(Origin|PopID/Mom), family=poisson,data=modeldata)
model1raw<-lmer(LfCountH ~ Origin * CtrlPopLf* Latitude + (1|PopID/Mom), family=poisson,data=modeldata)
anova(modelobar, model1raw)
model2raw<-lmer(LfCountH ~ Origin * CtrlPopLf* Latitude +(Origin|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(LfCountH ~ Origin * CtrlPopLf* Latitude +(Origin|blank), family=poisson,data=modeldata) # Test population effect
anova(model2raw,modelobar) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(56.023,1)
modelI <- lmer(LfCountH ~ Origin * CtrlPopLf+ Latitude + (Origin|PopID/Mom), family=poisson,data=modeldata)
anova(modelI, modelobar)

modelL <- lmer(LfCountH ~ Origin * CtrlPopLf+(Origin|PopID/Mom), family=poisson,data=modeldata)
anova(modelL, modelI)

modelCint <- lmer(LfCountH ~ Origin + CtrlPopLf+Latitude+(Origin|PopID/Mom), family=poisson,data=modeldata)
anova(modelI, modelCint)

modelC <- lmer(LfCountH ~ Origin +Latitude+(Origin|PopID/Mom), family=poisson,data=modeldata)
anova(modelCint, modelC)

modelOraw<-lmer(LfCountH ~ Latitude+(Origin|PopID/Mom), family=poisson,data=modeldata)
anova(modelOraw,modelC) #test for significance of origin - origin NOT sig....!

# qplot(data=modeldata,CtrlPopLf, LfCountH, color = Origin)+geom_point(position="jitter")
# moddata <- ddply(modeldata, .(PopID, Origin, Latitude, CtrlPopLf), summarize, popCount=length(PopID), popLfCountH=mean(LfCountH))
# qplot(data=moddata,CtrlPopLf, popLfCountH, color = Origin, xlab="Population mean leaf number in control",
#       ylab="Population mean days to LfCountH in herbivory", main="Performance in herbivory vs control, leaf no.") +geom_smooth(method=glm, se=TRUE)
# 
#explicit trade-off using shootmass
modeldata <- merge(modeldata, comeans, all.x=TRUE)
modeldata <- modeldata[!is.na(modeldata$CtrlPopShoot),]

modelobar<-lmer(LfCountH ~ Origin * CtrlPopShoot*Latitude +(Origin|PopID/Mom), family=poisson,data=modeldata)
model1raw<-lmer(LfCountH ~ Origin * CtrlPopShoot* Latitude + (1|PopID/Mom), family=poisson,data=modeldata)
anova(modelobar, model1raw)
model2raw<-lmer(LfCountH ~ Origin * CtrlPopShoot* Latitude +(Origin|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(LfCountH ~ Origin * CtrlPopShoot* Latitude +(Origin|blank), family=poisson,data=modeldata) # Test population effect
anova(model2raw,modelobar) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(56.023,1)
modelI <- lmer(LfCountH ~ Origin * CtrlPopShoot+ Latitude + (Origin|PopID/Mom), family=poisson,data=modeldata)
anova(modelI, modelobar)

modelL <- lmer(LfCountH ~ Origin * CtrlPopShoot+(Origin|PopID/Mom), family=poisson,data=modeldata)
anova(modelL, modelI)

modelCint <- lmer(LfCountH ~ Origin + CtrlPopShoot+Latitude+(Origin|PopID/Mom), family=poisson,data=modeldata)
anova(modelI, modelCint)

modelC <- lmer(LfCountH ~ Origin +Latitude+(Origin|PopID/Mom), family=poisson,data=modeldata)
anova(modelCint, modelC)

modelOraw<-lmer(LfCountH ~ Latitude+(Origin|PopID/Mom), family=poisson,data=modeldata)
anova(modelOraw,modelC) #test for significance of origin - origin NOT sig....!

# qplot(data=modeldata,CtrlPopShoot, LfCountH, color = Origin)+geom_point(position="jitter")
# moddata <- ddply(modeldata, .(PopID, Origin, Latitude, CtrlPopShoot), summarize, popCount=length(PopID), popLfCountH=mean(LfCountH))
# qplot(data=moddata,CtrlPopShoot, popLfCountH, color = Origin, 
#       xlab="Population mean shoot mass in control treatment", 
#       ylab="Population mean days to LfCountH in herbivory treatment", main="Performance in herbivory vs. control treatments") +geom_smooth(method=glm, se=TRUE)


###cut, bolt.bin, binomial###
modeldata<-cu[!is.na(cu$BoltedatH),]
# xtabs(~Origin + BoltedatH, modeldata)
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

modelobar<-lmer(bolt.bin ~ Origin *Latitude +(Origin|PopID/Mom), family=binomial,data=modeldata)
model1<-lmer(bolt.bin ~ Origin *Latitude +(1|PopID/Mom), family=binomial,data=modeldata)
anova(modelobar, model1)
# with(modeldata, bolt.bin[order(Latitude)]) 
# model4<-lmer(bolt.bin ~ Origin+Latitude +(1|PopID/Mom), family=binomial,data=modeldata)
# model5 <- lmer(bolt.bin~ Origin+Latitude +(1|PopID), family=binomial,data=modeldata)
# anova(model5, model4)
# model6 <- lmer(bolt.bin~ Origin+Latitude +(1|blank), family=binomial,data=modeldata)
# anova(model6, model5)
model2<-lmer(bolt.bin ~ Origin *Latitude +(1|PopID), family=binomial,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(bolt.bin ~ Origin *Latitude +(1|blank), family=binomial,data=modeldata) # Test population effect
anova(model2,model1) # mom sig
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
print(anova(model3,model2), digits=22)
(lambda <- (-2)*(-92.33837575460984226083 - (-92.33837577481315861405)))
1-pchisq(-4.040663e-08,1)

#for models with no significant or nearly sig (basically p>0.2) random effects, use glm instead
modelI <- lmer(bolt.bin ~ Origin +Latitude +(1|blank), family=binomial,data=modeldata)
anova(modelI, model3)
modelL <- lmer(bolt.bin ~ Origin +(1|blank), family=binomial,data=modeldata)
anova(modelL, model3)
anova(modelI, modelL)
modelL
(lambda <- (-2)*(-199.54 - (-191.60)))
1-pchisq(lambda,1)
modelO<-lmer(bolt.bin ~ Latitude+(1|blank), family=binomial,data=modeldata)
anova(modelO,model3) #test for significance of origin - origin sig!
anova(modelO, modelI)
modelO2<- lmer(bolt.bin ~ (1|blank), family=binomial,data=modeldata)
anova(modelL, modelO2)
modelP <- lmer(bolt.bin ~ Origin + (1|PopID), family = binomial, data=modeldata)
anova(modelP, modelL)
model3

#calculating means 
# int<--172.961 #inv mean
# B<-173.314 #Originnat estimate from model summary
# # Native
# pN<-exp(int+B)/(exp(int+B)+1)
# # Introduced (B=0)
# pI<-exp(int)/(exp(int)+1)
# 
# pI # 6.5% 
# pN # 27%
# #check by looking at percentages
# summary(cu[cu$Origin=="nat",]) #146 rows, 39 boltedatH = 27%
# summary(cu[cu$Origin=="inv",]) #55 rows, 8 boltedatH = 6.4%

CI.LS.binomial(model3)
lsmeans(model1, ~Origin, conf=95)

CI.LS.binomial(model)

#also check glm
modelg <- glm(bolt.bin ~ Origin*Latitude, family=binomial,data=modeldata)
modelg1 <- glm(bolt.bin ~ Origin+Latitude, family=binomial,data=modeldata)
modelg2 <- glm(bolt.bin ~ Latitude, family=binomial,data=modeldata)
modelg3 <- glm(bolt.bin ~ Origin, family=binomial,data=modeldata)
# lrtest(modelg1, modelg) from library(epicalc)
anova(modelg1, modelg, test="LRT") 
qchisq(0.0001435,1,lower=FALSE)#chisq value
#1-pchisq(14.45563,1)#check that you get pvalue back

anova(modelg3,modelg1,test="LRT")
qchisq(0.9815,1,lower=FALSE)#chisq value
anova(modelg3,test="LRT")
qchisq(0.0003361,1,lower=FALSE)#chisq value

# modelg4 <- glm(bolt.bin ~ Latitude*Origin, family=binomial,data=modeldata)
# modelg5 <- glm(bolt.bin ~ Latitude+Origin, family=binomial,data=modeldata)
# anova(modelg5, modelg4)

CI.LS.binomial(modelg1)

qplot(data=modeldata,Latitude, bolt.bin, color = Origin)
moddata <- ddply(modeldata, .(PopID, Origin, Latitude), summarize, popBolt=length(BoltedatH), popBoltavg=mean(bolt.bin))
moddata
qplot(data=moddata,Latitude, popBoltavg, color = Origin) +geom_smooth(method=lm, se=FALSE)
qplot(data=moddata[moddata$Latitude<50,],Latitude, popBoltavg, color = Origin) +geom_smooth(method=lm, se=FALSE)

#explicit tradeoff - using lf count
modeldata <- merge(modeldata, comeans, all.x=TRUE)
modeldata <- modeldata[!is.na(modeldata$CtrlPopLf),]

modelobar<-lmer(bolt.bin ~ Origin * CtrlPopLf+Latitude +(Origin|PopID), family=binomial,data=modeldata)
model1raw<-lmer(bolt.bin ~ Origin * CtrlPopLf+ Latitude + (1|PopID/Mom), family=binomial,data=modeldata)
anova(modelobar, model1raw)
model2raw<-lmer(bolt.bin ~ Origin * CtrlPopLf+ Latitude +(1|PopID), family=binomial,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(bolt.bin ~ Origin * CtrlPopLf+ Latitude +(1|blank), family=binomial,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(56.023,1)

modelg <- glm(bolt.bin ~ Origin*CtrlPopLf*Latitude, family=binomial,data=modeldata)
modelg1 <- glm(bolt.bin ~ Origin*CtrlPopLf+Latitude, family=binomial,data=modeldata)
anova(modelg1, modelg) #'Deviance' is chisq value
1-pchisq(hisq, df)

modelg3<- glm(bolt.bin ~ Origin*CtrlPopLf, family=binomial,data=modeldata)
anova(modelg3,modelg1)
1-pchisq(5.5154, 1)
modelg2<- glm(bolt.bin ~Origin +CtrlPopLf, family=binomial,data=modeldata)
anova(modelg2,modelg3)
1-pchisq(4.8599, 1)

modelg4 <- glm(bolt.bin ~Origin, family=binomial, data=modeldata)
anova(modelg4, modelg2)
modelg5 <- glm(bolt.bin~CtrlPopLf, family=binomial, data=modeldata)
anova(modelg2, modelg5)

# qplot(data=modeldata,CtrlPopLf, bolt.bin, color = Origin)+geom_point(position="jitter")
# moddata <- ddply(modeldata, .(PopID, Origin, Latitude, CtrlPopLf), summarize, popCount=length(PopID), popbolt.bin=mean(bolt.bin))
# qplot(data=moddata,CtrlPopLf, popbolt.bin, color = Origin, xlab="Population mean leaf number in control",
#       ylab="Population mean days to bolt.bin in herbivory", main="Performance in herbivory vs control, leaf no.") +geom_smooth(method=glm, se=TRUE)
# 
#explicit trade-off using shootmass
modeldata <- merge(modeldata, comeans, all.x=TRUE)
modeldata <- modeldata[!is.na(modeldata$CtrlPopShoot),]

modelobar<-lmer(bolt.bin ~ Origin * CtrlPopShoot+Latitude +(Origin|PopID), family=binomial,data=modeldata)
model1raw<-lmer(bolt.bin ~ Origin * CtrlPopShoot+ Latitude + (1|PopID/Mom), family=binomial,data=modeldata)
anova(modelobar, model1raw)
model2raw<-lmer(bolt.bin ~ Origin * CtrlPopShoot+ Latitude +(1|PopID), family=binomial,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(bolt.bin ~ Origin * CtrlPopShoot+ Latitude +(1|blank), family=binomial,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(56.023,1)

modelg <- glm(bolt.bin ~ Origin*CtrlPopShoot*Latitude, family=binomial,data=modeldata)
modelg1 <- glm(bolt.bin ~ Origin*CtrlPopShoot+Latitude, family=binomial,data=modeldata)
anova(modelg1, modelg) #'Deviance' is chisq value
1-pchisq(hisq, df)

modelg3<- glm(bolt.bin ~ Origin*CtrlPopShoot, family=binomial,data=modeldata)
anova(modelg3,modelg1)
1-pchisq(5.5154, 1)
modelg2<- glm(bolt.bin ~Origin +CtrlPopShoot, family=binomial,data=modeldata)
anova(modelg2,modelg3)
1-pchisq(4.8599, 1)

modelg4 <- glm(bolt.bin ~Origin, family=binomial, data=modeldata)
anova(modelg4, modelg2)
modelg5 <- glm(bolt.bin~CtrlPopShoot, family=binomial, data=modeldata)
anova(modelg2, modelg5)
# 
# qplot(data=modeldata,CtrlPopShoot, bolt.bin, color = Origin)+geom_point(position="jitter")
# moddata <- ddply(modeldata, .(PopID, Origin, Latitude, CtrlPopShoot), summarize, popCount=length(PopID), popbolt.bin=mean(bolt.bin))
# qplot(data=moddata,CtrlPopShoot, popbolt.bin, color = Origin, 
#       xlab="Population mean shoot mass in control treatment", 
#       ylab="Population mean days to bolt.bin in herbivory treatment", main="Performance in herbivory vs. control treatments") +geom_smooth(method=glm, se=TRUE)


###cut, harvest, bolt date, mom is sig, do by hand###
#given that it's bolted....
modeldata<-cu[!is.na(cu$BoltDate),]
# xtabs(~Origin + BoltedatH, modeldata)
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

modelobar<-lmer(BoltDate ~ Origin *Latitude +(Origin|PopID/Mom), family=poisson,data=modeldata)
model1<-lmer(BoltDate ~ Origin *Latitude +(1|PopID/Mom), family=poisson,data=modeldata)
anova(modelobar, model1)
model2<-lmer(BoltDate ~ Origin *Latitude+(1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(BoltDate ~ Origin *Latitude+(1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2,model1) # mom sig
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelI <- lmer(BoltDate ~ Origin +Latitude+(1|PopID/Mom), family=poisson,data=modeldata)
anova(modelI, model1)

modelL<-lmer(BoltDate ~ Origin + (1|PopID/Mom), family=poisson,data=modeldata)
anova(modelL,modelI) #test for significant interaction btw Origin and Bolted - not sig

modelO<-lmer(BoltDate ~ Latitude + (1|PopID/Mom), family=poisson,data=modeldata)
anova(modelO,modelI) #test for significance of origin - origin not sig!

CI.LS.poisson(modelI)

#cut, boltdate, extra covariates#
modeldata<-cu[!is.na(cu$BoltDate),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

model1<-lmer(BoltDate ~ Origin * Latitude + CrownDiam.mm+(CrownDiam.mm|PopID/Mom), family=poisson,data=modeldata)
model2<-lmer(BoltDate ~ Origin * Latitude +CrownDiam.mm+(CrownDiam.mm|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(BoltDate ~ Origin * Latitude +CrownDiam.mm+(CrownDiam.mm|blank), family=poisson,data=modeldata) # Test population effect
anova(model2,model1) # mom sig
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelInt<-lmer(BoltDate ~ Origin + Latitude +CrownDiam.mm+(CrownDiam.mm|PopID/Mom), family=poisson,data=modeldata)
anova(modelInt,model1) #test for significant interaction btw Origin and Bolted - not sig

modelC0<-lmer(BoltDate ~ Origin + Latitude +(1|PopID/Mom), family=poisson,data=modeldata)
modelC1 <- lmer(BoltDate ~ Origin + Latitude +(CrownDiam.mm|PopID/Mom), family=poisson,data=modeldata)
modelC2 <- lmer(BoltDate ~ Origin + Latitude +CrownDiam.mm+(1|PopID/Mom), family=poisson,data=modeldata)
anova(modelC0, modelInt)#test for sig of crown - crown not sig
anova(modelC1, modelInt)
anova(modelC2,modelInt)

modelL <- lmer(BoltDate ~ Origin +(CrownDiam.mm|PopID/Mom), family=poisson,data=modeldata)
anova(modelL, modelC1)

modelO<-lmer(BoltDate ~ Latitude + (CrownDiam.mm|PopID/Mom), family=poisson,data=modeldata)
anova(modelO,modelC1) #test for significance of origin - origin not sig!
modelC1
int<-4.3403
#inv mean
B<--0.5395
#Originnat estimate from model summary
pI<-exp(int)
pN<-exp(int+B)
pI
pN
summary(cu[cu$Origin=="nat",]$BoltDate)

#explicit tradeoff - using lf count
modeldata <- merge(modeldata, comeans, all.x=TRUE)
modeldata <- modeldata[!is.na(modeldata$CtrlPopLf),]

modelobar<-lmer(BoltDate ~ Origin * CtrlPopLf+Latitude +(Origin|PopID/Mom), family=poisson,data=modeldata)
model1raw<-lmer(BoltDate ~ Origin * CtrlPopLf+ Latitude + (1|PopID/Mom), family=poisson,data=modeldata)
anova(modelobar, model1raw)
model2raw<-lmer(BoltDate ~ Origin * CtrlPopLf+ Latitude +(1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(BoltDate ~ Origin * CtrlPopLf+ Latitude +(1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(56.023,1)
# modelI <- lmer(BoltDate ~ Origin * CtrlPopLf+ Latitude + (1|PopID/Mom), family=poisson,data=modeldata)
# anova(modelI, model1raw)

modelL <- lmer(BoltDate ~ Origin * CtrlPopLf+(1|PopID/Mom), family=poisson,data=modeldata)
anova(modelL, model1raw)

modelCint <- lmer(BoltDate ~ Origin + CtrlPopLf+(1|PopID/Mom), family=poisson,data=modeldata)
anova(modelL, modelCint)

modelC <- lmer(BoltDate ~ Origin +(1|PopID/Mom), family=poisson,data=modeldata)
anova(modelCint, modelC)

modelOraw<-lmer(BoltDate ~ (1|PopID/Mom), family=poisson,data=modeldata)
anova(modelOraw,modelC) #test for significance of origin - origin NOT sig....!

# qplot(data=modeldata,CtrlPopLf, BoltDate, color = Origin)+geom_point(position="jitter")
# moddata <- ddply(modeldata, .(PopID, Origin, Latitude, CtrlPopLf), summarize, popCount=length(PopID), popBoltDate=mean(BoltDate))
# qplot(data=moddata,CtrlPopLf, popBoltDate, color = Origin, xlab="Population mean leaf number in control",
#       ylab="Population mean days to BoltDate in herbivory", main="Performance in herbivory vs control, leaf no.") +geom_smooth(method=glm, se=TRUE)
# 
#explicit trade-off using shootmass
modeldata <- merge(modeldata, comeans, all.x=TRUE)
modeldata <- modeldata[!is.na(modeldata$CtrlPopShoot),]

modelobar<-lmer(BoltDate ~ Origin * CtrlPopShoot+Latitude +(Origin|PopID/Mom), family=poisson,data=modeldata)
model1raw<-lmer(BoltDate ~ Origin * CtrlPopShoot+ Latitude + (1|PopID/Mom), family=poisson,data=modeldata)
anova(modelobar, model1raw)
model2raw<-lmer(BoltDate ~ Origin * CtrlPopShoot+ Latitude +(1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(BoltDate ~ Origin * CtrlPopShoot+ Latitude +(1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(56.023,1)
# modelI <- lmer(BoltDate ~ Origin * CtrlPopShoot+ Latitude + (1|PopID/Mom), family=poisson,data=modeldata)
# anova(modelI, model1raw)

modelL <- lmer(BoltDate ~ Origin * CtrlPopShoot+(1|PopID/Mom), family=poisson,data=modeldata)
anova(modelL, model1raw)

modelCint <- lmer(BoltDate ~ Origin + CtrlPopShoot+Latitude+(1|PopID/Mom), family=poisson,data=modeldata)
anova(model1raw, modelCint)

modelC <- lmer(BoltDate ~ Origin +Latitude+(1|PopID/Mom), family=poisson,data=modeldata)
anova(modelCint, modelC)

modelOraw<-lmer(BoltDate ~ Latitude + (1|PopID/Mom), family=poisson,data=modeldata)
anova(modelOraw,modelC) #test for significance of origin - origin NOT sig....!

# qplot(data=modeldata,CtrlPopShoot, BoltDate, color = Origin)+geom_point(position="jitter")
# moddata <- ddply(modeldata, .(PopID, Origin, Latitude, CtrlPopShoot), summarize, popCount=length(PopID), popBoltDate=mean(BoltDate))
# qplot(data=moddata,CtrlPopShoot, popBoltDate, color = Origin, 
#       xlab="Population mean shoot mass in control treatment", 
#       ylab="Population mean days to BoltDate in herbivory treatment", main="Performance in herbivory vs. control treatments") +geom_smooth(method=glm, se=TRUE)
