#Stress treatment - Nutrient deficiency
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

####Nut def, Origin * Lat####
n<-read.table("STNutsubset.txt", header=T, sep="\t", quote='"', row.names=1) #nutsubset
head(n)
# xtabs(~Origin+BoltedatH, modeldata) # no invasives bolted.........
# modeldata<-modeldata[modeldata$BoltedatH!="y",]#remove 7 bolted plants from nat SHOULD I???
nLR <- lapply(names(n)[c(11:12, 15:17, 49:51)],function(x) CGtrait.LR.int(x,n)) 
#lflgthH, lfwdthH, crown, shoot, root, root.log (pick one!)lxwH, all gaussian
names(nLR) <- names(al)[c(11:12, 15:17, 50:51)]
nLR #check out LRs of models. Model progression logical?
# nmodels <- CGtrait.models.int("RootMass.g",n)
# nmodels2 <- CGtrait.models.int("RootH.log",n)
# nmodels
# nRoot.lmer <- nmodels$model2
# nRootlog.lmer <- nmodels2$model2
# qqnorm(resid(nRootlog.lmer), main="Q-Q plot for residuals")
# qqline(resid(nRootlog.lmer))


#non-gaussian?
# n <- cbind(n, bolt.bin=as.numeric(n$BoltedatH)-1)
# write.table(n, file="STNutsubset.txt", sep="\t", quote=F)
#nBatH <- CGtrait.LR("bolt.bin", n, family=binomial)# no invasive bolted!
nlfcount <- CGtrait.LR.int("LfCountH",n, family=poisson) #lfcountH, all poisson

###nut def, lf count, harvest, mom sig, do by hand###
modeldata<-n[!is.na(n$LfCountH),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)
modeldata <- merge(modeldata, comeans, all.x=TRUE)

modelobar<-lmer(LfCountH ~ Origin * Latitude +(Origin|PopID/Mom), family=poisson,data=modeldata)
model1raw<-lmer(LfCountH ~ Origin * Latitude + (1|PopID/Mom), family=poisson,data=modeldata)
anova(modelobar, model1raw)
model2raw<-lmer(LfCountH ~ Origin * Latitude +(Origin|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(LfCountH ~ Origin * Latitude +(Origin|blank), family=poisson,data=modeldata) # Test population effect
anova(model2raw,modelobar) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(56.023,1)
modelI <- lmer(LfCountH ~ Origin + Latitude + (Origin|PopID/Mom), family=poisson,data=modeldata)
anova(modelI, modelobar)

modelL <- lmer(LfCountH ~ Origin +(Origin|PopID/Mom), family=poisson,data=modeldata)
anova(modelL, modelI)

modelOraw<-lmer(LfCountH ~ (Origin|PopID/Mom), family=poisson,data=modeldata)
anova(modelOraw,modelL) #test for significance of origin - origin NOT sig....!

CI.LS.poisson(modelL)

#explicit tradeoff - using lf count
modelobar<-lmer(LfCountH ~ Origin * CtrlPopLf*Latitude +(Origin|PopID/Mom), family=poisson,data=modeldata)
model1raw<-lmer(LfCountH ~ Origin * CtrlPopLf* Latitude + (1|PopID/Mom), family=poisson,data=modeldata)
anova(modelobar, model1raw)
model2raw<-lmer(LfCountH ~ Origin * CtrlPopLf* Latitude +(1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(LfCountH ~ Origin * CtrlPopLf* Latitude +(1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(56.023,1)
modelI <- lmer(LfCountH ~ Origin * CtrlPopLf+ Latitude + (1|PopID/Mom), family=poisson,data=modeldata)
anova(modelI, model1raw)

modelL <- lmer(LfCountH ~ Origin * CtrlPopLf+(1|PopID/Mom), family=poisson,data=modeldata)
anova(modelL, modelI)

modelCint <- lmer(LfCountH ~ Origin + CtrlPopLf+(1|PopID/Mom), family=poisson,data=modeldata)
anova(modelL, modelCint)

modelC <- lmer(LfCountH ~ Origin +(1|PopID/Mom), family=poisson,data=modeldata)
anova(modelCint, modelC)

modelOraw<-lmer(LfCountH ~ (1|PopID/Mom), family=poisson,data=modeldata)
anova(modelOraw,modelC) #test for significance of origin - origin NOT sig....!

qplot(data=modeldata,CtrlPopLf, LfCountH, color = Origin)
moddata <- ddply(modeldata, .(PopID, Origin, Latitude, CtrlPopLf), summarize, popCount=length(PopID), poplfMeanN=mean(LfCountH))
moddata
qplot(data=moddata,CtrlPopLf, poplfMeanN, color = Origin) +geom_smooth(method=glm, se=FALSE)

#explicit trade-off using shootmass
modelobar<-lmer(LfCountH ~ Origin * CtrlPopShoot*Latitude +(Origin|PopID/Mom), family=poisson,data=modeldata)
model1raw<-lmer(LfCountH ~ Origin * CtrlPopShoot* Latitude + (1|PopID/Mom), family=poisson,data=modeldata)
anova(modelobar, model1raw)
model2raw<-lmer(LfCountH ~ Origin * CtrlPopShoot* Latitude +(1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(LfCountH ~ Origin * CtrlPopShoot* Latitude +(1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(56.023,1)
modelI <- lmer(LfCountH ~ Origin * CtrlPopShoot+ Latitude + (1|PopID/Mom), family=poisson,data=modeldata)
anova(modelI, model1raw)

modelL <- lmer(LfCountH ~ Origin * CtrlPopShoot+(1|PopID/Mom), family=poisson,data=modeldata)
anova(modelL, modelI)

modelCint <- lmer(LfCountH ~ Origin + CtrlPopShoot+(1|PopID/Mom), family=poisson,data=modeldata)
anova(modelL, modelCint)

modelC <- lmer(LfCountH ~ Origin +(1|PopID/Mom), family=poisson,data=modeldata)
anova(modelCint, modelC)

modelOraw<-lmer(LfCountH ~ (1|PopID/Mom), family=poisson,data=modeldata)
anova(modelOraw,modelC) #test for significance of origin - origin NOT sig....!

qplot(data=modeldata,CtrlPopShoot, LfCountH, color = Origin)
moddata <- ddply(modeldata, .(PopID, Origin, Latitude, CtrlPopShoot), summarize, popCount=length(PopID), poplfMeanN=mean(LfCountH))
moddata
qplot(data=moddata,CtrlPopShoot, poplfMeanN, color = Origin) +geom_smooth(method=glm, se=FALSE, aes(group=1))

##nut def, shoot##
modeldata<-n[!is.na(n$ShootMass.g),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

modelobar<-lmer(ShootMass.g ~ Origin * Latitude + (Origin|PopID/Mom), family=gaussian,data=modeldata)
model1raw<-lmer(ShootMass.g ~ Origin * Latitude + (1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelobar, model1raw)
model2raw<-lmer(ShootMass.g ~ Origin * Latitude +(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(ShootMass.g ~ Origin * Latitude +(1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(15.96,1)
modelI <- lmer(ShootMass.g ~ Origin + Latitude + (1|PopID), family=gaussian,data=modeldata)
anova(modelI, model2raw)

modelL <- lmer(ShootMass.g ~ Origin +(1|PopID), family=gaussian,data=modeldata)
anova(modelL, modelI)

modelOraw<-lmer(ShootMass.g ~ Latitude +(1|PopID), family=gaussian,data=modeldata)
anova(modelOraw,modelI)

# modelO<-lmer(ShootMass.g ~ (1|PopID), family=gaussian,data=modeldata)
# anova(modelO,modelL)

lsmeans(modelI, ~Origin, conf=95)

#explicit tradeoff - using lf count
modeldata <- merge(modeldata, comeans, all.x=TRUE)
modeldata <- modeldata[!is.na(modeldata$CtrlPopLf),]

modelobar<-lmer(ShootMass.g ~ Origin * CtrlPopLf*Latitude +(Origin|PopID/Mom), family=gaussian,data=modeldata)
model1raw<-lmer(ShootMass.g ~ Origin * CtrlPopLf* Latitude + (1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelobar, model1raw)
model2raw<-lmer(ShootMass.g ~ Origin * CtrlPopLf* Latitude +(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(ShootMass.g ~ Origin * CtrlPopLf* Latitude +(1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(56.023,1)
modelI <- lmer(ShootMass.g ~ Origin * CtrlPopLf+ Latitude + (1|PopID), family=gaussian,data=modeldata)
anova(modelI, model2raw)

modelL <- lmer(ShootMass.g ~ Origin * CtrlPopLf+(1|PopID), family=gaussian,data=modeldata)
anova(modelL, modelI)

modelCint <- lmer(ShootMass.g ~ Origin + CtrlPopLf+(1|PopID), family=gaussian,data=modeldata)
anova(modelL, modelCint)

modelC <- lmer(ShootMass.g ~ Origin +(1|PopID), family=gaussian,data=modeldata)
anova(modelCint, modelC)

modelOraw<-lmer(ShootMass.g ~ (1|PopID), family=gaussian,data=modeldata)
anova(modelOraw,modelC) #test for significance of origin - origin NOT sig....!

qplot(data=modeldata,CtrlPopLf, ShootMass.g, color = Origin)+geom_point(position="jitter")
moddata <- ddply(modeldata, .(PopID, Origin, Latitude, CtrlPopLf), summarize, popCount=length(PopID), popShootMass.g=mean(ShootMass.g))
qplot(data=moddata,CtrlPopLf, popShootMass.g, color = Origin, xlab="Population mean leaf number in control",
      ylab="Population mean days to ShootMass.g in herbivory", main="Performance in herbivory vs control, leaf no.") +geom_smooth(method=glm, se=TRUE)

#explicit trade-off using shootmass
modeldata <- merge(modeldata, comeans, all.x=TRUE)
modeldata <- modeldata[!is.na(modeldata$CtrlPopShoot),]

modelobar<-lmer(ShootMass.g ~ Origin * CtrlPopShoot*Latitude +(Origin|PopID/Mom), family=gaussian,data=modeldata)
model1raw<-lmer(ShootMass.g ~ Origin * CtrlPopShoot* Latitude + (1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelobar, model1raw)
model2raw<-lmer(ShootMass.g ~ Origin * CtrlPopShoot* Latitude +(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(ShootMass.g ~ Origin * CtrlPopShoot* Latitude +(1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(56.023,1)

modelg <- glm(ShootMass.g ~ Origin*CtrlPopShoot*Latitude, family=gaussian,data=modeldata)
modelg1 <- glm(ShootMass.g ~ Origin*CtrlPopShoot+Latitude, family=gaussian,data=modeldata)
anova(modelg1, modelg) #'Deviance' is chisq value
1-pchisq(hisq, df)

modelg3<- glm(ShootMass.g ~ Origin*CtrlPopShoot, family=gaussian,data=modeldata)
anova(modelg3,modelg1)
1-pchisq(5.5154, 1)
modelg2<- glm(ShootMass.g ~Origin +CtrlPopShoot, family=gaussian,data=modeldata)
anova(modelg2,modelg3)
1-pchisq(4.8599, 1)

modelg4 <- glm(ShootMass.g ~Origin, family=gaussian, data=modeldata)
anova(modelg4, modelg2)
modelg5 <- glm(ShootMass.g~CtrlPopShoot, family=gaussian, data=modeldata)
anova(modelg2, modelg5)

qplot(data=modeldata,CtrlPopShoot, ShootMass.g, color = Origin)+geom_point(position="jitter")
moddata <- ddply(modeldata, .(PopID, Origin, Latitude, CtrlPopShoot), summarize, popCount=length(PopID), popShootMass.g=mean(ShootMass.g))
qplot(data=moddata,CtrlPopShoot, popShootMass.g, color = Origin, 
      xlab="Population mean shoot mass in control treatment", 
      ylab="Population mean days to ShootMass.g in herbivory treatment", main="Performance in herbivory vs. control treatments") +geom_smooth(method=glm, se=TRUE)


##nut, root.log
modeldata<-n[!is.na(n$RootH.log),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

modelobar<-lmer(RootH.log ~ Origin * Latitude + (Origin|PopID/Mom), family=gaussian,data=modeldata)
model1raw<-lmer(RootH.log ~ Origin * Latitude + (1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelobar, model1raw)
model2raw<-lmer(RootH.log ~ Origin * Latitude +(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(RootH.log ~ Origin * Latitude +(1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

1-pchisq(42.65,1)
modelI <- lmer(RootH.log ~ Origin + Latitude + (1|PopID), family=gaussian,data=modeldata)
modelL <- lmer(RootH.log ~ Origin +(1|PopID), family=gaussian,data=modeldata)
anova(modelL, modelI)

modelOraw<-lmer(RootH.log ~ Latitude +(1|PopID), family=gaussian,data=modeldata)
anova(modelOraw,modelI)

# modelO<-lmer(RootH.log ~ (1|PopID), family=gaussian,data=modeldata)
# anova(modelO,modelL)

CI.LS.gaussian(modelI)

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
anova(modelI, model1raw)

modelL <- lmer(RootH.log ~ Origin * CtrlPopLf+(1|PopID), family=gaussian,data=modeldata)
anova(modelL, modelI)

modelCint <- lmer(RootH.log ~ Origin + CtrlPopLf+(1|PopID), family=gaussian,data=modeldata)
anova(modelL, modelCint)

modelC <- lmer(RootH.log ~ Origin +(1|PopID), family=gaussian,data=modeldata)
anova(modelCint, modelC)

modelOraw<-lmer(RootH.log ~ (1|PopID), family=gaussian,data=modeldata)
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
modelI <- lmer(RootH.log ~ Origin * CtrlPopShoot+ Latitude + (1|PopID), family=gaussian,data=modeldata)
anova(modelI, model1raw)

modelL <- lmer(RootH.log ~ Origin * CtrlPopShoot+(1|PopID), family=gaussian,data=modeldata)
anova(modelL, modelI)

modelCint <- lmer(RootH.log ~ Origin + CtrlPopShoot+(1|PopID), family=gaussian,data=modeldata)
anova(modelL, modelCint)

modelC <- lmer(RootH.log ~ Origin +(1|PopID), family=gaussian,data=modeldata)
anova(modelCint, modelC)

modelOraw<-lmer(RootH.log ~ (1|PopID), family=gaussian,data=modeldata)
anova(modelOraw,modelC) #test for significance of origin - origin NOT sig....!


# qplot(data=modeldata,CtrlPopShoot, RootH.log, color = Origin)+geom_point(position="jitter")
# moddata <- ddply(modeldata, .(PopID, Origin, Latitude, CtrlPopShoot), summarize, popCount=length(PopID), popRootH.log=mean(RootH.log))
# qplot(data=moddata,CtrlPopShoot, popRootH.log, color = Origin, 
#       xlab="Population mean shoot mass in control treatment", 
#       ylab="Population mean days to rootmass in herbivory treatment", main="Performance in herbivory vs. control treatments") +geom_smooth(method=glm, se=TRUE)


##nut, crown
modeldata<-n[!is.na(n$CrownDiam.mm),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

modelobar<-lmer(CrownDiam.mm ~ Origin * Latitude + (Origin|PopID/Mom), family=gaussian,data=modeldata)
model1raw<-lmer(CrownDiam.mm ~ Origin * Latitude + (1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelobar, model1raw)
model2raw<-lmer(CrownDiam.mm ~ Origin * Latitude +(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(CrownDiam.mm ~ Origin * Latitude +(1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

1-pchisq(26.05,1)
modelI <- lmer(CrownDiam.mm ~ Origin + Latitude + (1|PopID), family=gaussian,data=modeldata)
modelL <- lmer(CrownDiam.mm ~ Origin +(1|PopID), family=gaussian,data=modeldata)
anova(modelL, modelI)

# modelOraw<-lmer(CrownDiam.mm ~ Latitude +(1|PopID), family=gaussian,data=modeldata)
# anova(modelOraw,modelI)

modelO<-lmer(CrownDiam.mm ~ (1|PopID), family=gaussian,data=modeldata)
anova(modelO,modelL)

lsmeans(modelL, ~Origin, conf=95)

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

modelCint <- lmer(CrownDiam.mm ~ Origin + CtrlPopLf+(1|PopID), family=gaussian,data=modeldata)
anova(modelL, modelCint)

modelC <- lmer(CrownDiam.mm ~ Origin +(1|PopID), family=gaussian,data=modeldata)
anova(modelCint, modelC)

modelOraw<-lmer(CrownDiam.mm ~ (1|PopID/Mom), family=gaussian,data=modeldata)
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
modelI <- lmer(CrownDiam.mm ~ Origin * CtrlPopShoot+ Latitude + (1|PopID), family=gaussian,data=modeldata)
anova(modelI, model2raw)

modelL <- lmer(CrownDiam.mm ~ Origin * CtrlPopShoot+(1|PopID), family=gaussian,data=modeldata)
anova(modelL, modelI)

modelCint <- lmer(CrownDiam.mm ~ Origin + CtrlPopShoot+(1|PopID), family=gaussian,data=modeldata)
anova(modelL, modelCint)

modelC <- lmer(CrownDiam.mm ~ Origin +(1|PopID), family=gaussian,data=modeldata)
anova(modelCint, modelC)

modelOraw<-lmer(CrownDiam.mm ~ (1|PopID), family=gaussian,data=modeldata)
anova(modelOraw,modelC) #test for significance of origin - origin NOT sig....!

# 
# qplot(data=modeldata,CtrlPopShoot, CrownDiam.mm, color = Origin)+geom_point(position="jitter")
# moddata <- ddply(modeldata, .(PopID, Origin, Latitude, CtrlPopShoot), summarize, popCount=length(PopID), popCrownDiam.mm=mean(CrownDiam.mm))
# qplot(data=moddata,CtrlPopShoot, popCrownDiam.mm, color = Origin, 
#       xlab="Population mean shoot mass in control treatment", 
#       ylab="Population mean days to crown in herbivory treatment", main="Performance in herbivory vs. control treatments") +geom_smooth(method=glm, se=TRUE)


##nut, lxwH
modeldata<-n[!is.na(n$lxwH),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

modelobar<-lmer(lxwH ~ Origin * Latitude + (Origin|PopID/Mom), family=gaussian,data=modeldata)
model1raw<-lmer(lxwH ~ Origin * Latitude + (1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelobar, model1raw)
model2raw<-lmer(lxwH ~ Origin * Latitude +(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(lxwH ~ Origin * Latitude +(1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

1-pchisq(7.97,1)
modelI <- lmer(lxwH ~ Origin + Latitude + (1|PopID), family=gaussian,data=modeldata)
modelL <- lmer(lxwH ~ Origin +(1|PopID), family=gaussian,data=modeldata)
anova(modelL, modelI)

# modelOraw<-lmer(lxwH ~ Latitude +(1|PopID), family=gaussian,data=modeldata)
# anova(modelOraw,modelI)

modelO<-lmer(lxwH ~ (1|PopID), family=gaussian,data=modeldata)
anova(modelO,modelL)

lsmeans(modelL, ~Origin, conf=95)

#explicit tradeoff - using lf count
modeldata <- merge(modeldata, comeans, all.x=TRUE)
modeldata <- modeldata[!is.na(modeldata$CtrlPopLf),]

modelobar<-lmer(lxwH ~ Origin * CtrlPopLf*Latitude +(Origin|PopID/Mom), family=gaussian,data=modeldata)
model1raw<-lmer(lxwH ~ Origin * CtrlPopLf* Latitude + (1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelobar, model1raw)
model2raw<-lmer(lxwH ~ Origin * CtrlPopLf* Latitude +(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(lxwH ~ Origin * CtrlPopLf* Latitude +(1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(56.023,1)
modelI <- lmer(lxwH ~ Origin * CtrlPopLf+ Latitude + (1|PopID), family=gaussian,data=modeldata)
anova(modelI, model2raw)

modelL <- lmer(lxwH ~ Origin * CtrlPopLf+(1|PopID), family=gaussian,data=modeldata)
anova(modelL, modelI)

modelCint <- lmer(lxwH ~ Origin + CtrlPopLf+(1|PopID), family=gaussian,data=modeldata)
anova(modelL, modelCint)

modelC <- lmer(lxwH ~ Origin +(1|PopID), family=gaussian,data=modeldata)
anova(modelCint, modelC)

modelOraw<-lmer(lxwH ~ (1|PopID), family=gaussian,data=modeldata)
anova(modelOraw,modelC) #test for significance of origin - origin NOT sig....!


# qplot(data=modeldata,CtrlPopLf, lxwH, color = Origin)+geom_point(position="jitter")
# moddata <- ddply(modeldata, .(PopID, Origin, Latitude, CtrlPopLf), summarize, popCount=length(PopID), poplxwH=mean(lxwH))
# qplot(data=moddata,CtrlPopLf, poplxwH, color = Origin, xlab="Population mean leaf number in control",
#       ylab="Population mean days to lxwH in herbivory", main="Performance in herbivory vs control, leaf no.") +geom_smooth(method=glm, se=TRUE)
# 
#explicit trade-off using shootmass
modeldata <- merge(modeldata, comeans, all.x=TRUE)
modeldata <- modeldata[!is.na(modeldata$CtrlPopShoot),]

# modelobar<-lmer(lxwH ~ Origin * CtrlPopShoot*Latitude +(Origin|PopID/Mom), family=gaussian,data=modeldata)
# model1raw<-lmer(lxwH ~ Origin * CtrlPopShoot* Latitude + (1|PopID/Mom), family=gaussian,data=modeldata)
# anova(modelobar, model1raw)
# model2raw<-lmer(lxwH ~ Origin * CtrlPopShoot* Latitude +(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
# model3raw<-lmer(lxwH ~ Origin * CtrlPopShoot* Latitude +(1|blank), family=gaussian,data=modeldata) # Test population effect
# anova(model2raw,model1raw) # mom not sig
# anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
# 1-pchisq(3.2628,1)

modelg <- glm(lxwH ~ Origin*CtrlPopShoot*Latitude, family=gaussian,data=modeldata)
modelg1 <- glm(lxwH ~ Origin*CtrlPopShoot+Latitude, family=gaussian,data=modeldata)
anova(modelg1, modelg) #'Deviance' is chisq value
1-pchisq(hisq, df)

modelg3<- glm(lxwH ~ Origin*CtrlPopShoot, family=gaussian,data=modeldata)
anova(modelg3,modelg1)
1-pchisq(5.5154, 1)
modelg2<- glm(lxwH ~Origin +CtrlPopShoot+ Latitude, family=gaussian,data=modeldata)
anova(modelg2,modelg1)
1-pchisq(4.8599, 1)

modelg4 <- glm(lxwH ~Origin+Latitude, family=gaussian, data=modeldata)
anova(modelg4, modelg2)
modelg5 <- glm(lxwH~CtrlPopShoot+Latitude, family=gaussian, data=modeldata)
anova(modelg5, modelg2)

modelg3.1 <- glm(lxwH ~ Origin+CtrlPopShoot, family=gaussian,data=modeldata)
anova(modelg3.1, modelg3)
summary(modelg3)
summary(modelg1)

qplot(data=modeldata,CtrlPopShoot, lxwH, color = Origin)+geom_point(position="jitter")
moddata <- ddply(modeldata, .(PopID, Origin, Latitude, CtrlPopShoot), summarize, popCount=length(PopID), poplxwH=mean(lxwH))
qplot(data=moddata,CtrlPopShoot, poplxwH, color = Origin, 
      xlab="Population mean shoot mass in control treatment", 
      ylab="Population mean lf size in nutrient treatment", main="Performance in nutrient vs. control treatments") +geom_smooth(method=glm, se=TRUE)
summary(modelg3)
