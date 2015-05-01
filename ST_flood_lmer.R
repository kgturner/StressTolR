#ST stress treatment - flooding
#Stress Tolerance, REML, using lme4
#mixed effect models 
# library(lme4)
library(lme4.0)
library(lsmeans)
library(ggplot2)
library(plyr)

#pop control means to look at stress treatments
co<-read.table("STControlsubset.txt", header=T, sep="\t", quote='"', row.names=1) #controlsubset
se <- function(x) sqrt(var(x)/length(x))
comeans<- ddply(co, .(PopID, Origin, Latitude), summarize, CtrlPopCount=length(PopID), CtrlPopLf=mean(LfCountH), CtrlPopLfSE=se(LfCountH),
                CtrlPopShoot=mean(ShootMass.g), CtrlPopShootSE=se(ShootMass.g))

####Flood, Origin * Lat####
f<-read.table("STFloodsubset.txt", header=T, sep="\t", quote='"', row.names=1) #floodsubset
head(f)
#no gaussian
fLR <- lapply(names(f)[19:21],function(n) CGtrait.LR.int(n,f, family=poisson)) #death, floatdate, all poisson
# names(fLR) <- names(f)[20:21]
# 
# fmodels <- lapply(names(f)[20:21],function(n) CGtrait.models(n,f))
# fmodels

####Flood, float####
modeldata<-f[!is.na(f$FloatDate),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

# modelobar<-lmer(FloatDate ~ Origin * Latitude +(Origin|PopID/Mom), family=poisson,data=modeldata)
# model1<-lmer(FloatDate ~ Origin * Latitude +(1|PopID/Mom), family=poisson,data=modeldata)
# anova(modelobar, model1)
# model2<-lmer(FloatDate ~ Origin * Latitude +(1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
# model3<-lmer(FloatDate ~ Origin * Latitude +(1|blank), family=poisson,data=modeldata) # Test population effect
# anova(model2,model1) # mom sig
# anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
# 
# modelI<-lmer(FloatDate ~ Origin + Latitude +(1|blank), family=poisson,data=modeldata)
# anova(modelI,model3) #test for significant interaction btw Origin and Bolted - not sig
# 
# modelL <- lmer(FloatDate ~ Origin + (1|blank), family=poisson,data=modeldata)
# anova(modelL, modelI)
# 
# modelO<-lmer(FloatDate ~ Latitude + (1|blank), family=poisson,data=modeldata)
# anova(modelO,modelI) #test for significance of origin - origin not sig

#try glm
modelg <- glm(FloatDate ~ Origin*Latitude, family=poisson,data=modeldata)
modelg1 <- glm(FloatDate ~ Origin+Latitude, family=poisson,data=modeldata)
anova(modelg1, modelg, test="LRT") 
qchisq(0.0001435,1,lower=FALSE)#chisq value

modelg3<- glm(FloatDate ~ Origin, family=poisson,data=modeldata)
anova(modelg3,modelg1, test="LRT")
1-pchisq(5.5154, 1)
modelg2<- glm(FloatDate ~ Latitude, family=poisson,data=modeldata)
anova(modelg2,modelg1, test="LRT")
1-pchisq(9.0533, 1)

CI.LS.poisson(modelg1)
summary(modelg1)

#overdispersion
library(AER)
dispersiontest(modelg1)

# #explicit tradeoff - using lf count
# modeldata <- merge(modeldata, comeans, all.x=TRUE)
# modeldata <- modeldata[!is.na(modeldata$CtrlPopLf),]
# 
# # modelobar<-lmer(FloatDate ~ Origin * CtrlPopLf+Latitude +(Origin|PopID/Mom), family=poisson,data=modeldata)
# # model1raw<-lmer(FloatDate ~ Origin * CtrlPopLf+ Latitude + (1|PopID/Mom), family=poisson,data=modeldata)
# # anova(modelobar, model1raw)
# # model2raw<-lmer(FloatDate ~ Origin * CtrlPopLf+ Latitude +(1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
# # model3raw<-lmer(FloatDate ~ Origin * CtrlPopLf+ Latitude +(1|blank), family=poisson,data=modeldata) # Test population effect
# # anova(model2raw,model1raw) # mom not sig
# # anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
# # 1-pchisq(56.023,1)
# 
# modelg <- glm(FloatDate ~ Origin*CtrlPopLf*Latitude, family=poisson,data=modeldata)
# modelg1 <- glm(FloatDate ~ Origin*CtrlPopLf+Latitude, family=poisson,data=modeldata)
# anova(modelg1, modelg) #'Deviance' is chisq value
# 1-pchisq(hisq, df)
# 
# modelg3<- glm(FloatDate ~ Origin*CtrlPopLf, family=poisson,data=modeldata)
# anova(modelg3,modelg1)
# 1-pchisq(5.5154, 1)
# modelg2<- glm(FloatDate ~Origin +CtrlPopLf+Latitude, family=poisson,data=modeldata)
# anova(modelg2,modelg1)
# 1-pchisq(4.8599, 1)
# 
# modelg4 <- glm(FloatDate ~Origin, family=poisson, data=modeldata)
# anova(modelg4, modelg2)
# modelg5 <- glm(FloatDate~CtrlPopLf, family=poisson, data=modeldata)
# anova(modelg2, modelg5)
# 
# qplot(data=modeldata,CtrlPopLf, FloatDate, color = Origin)+geom_point(position="jitter")
# moddata <- ddply(modeldata, .(PopID, Origin, Latitude, CtrlPopLf), summarize, popCount=length(PopID), popFloatDate=mean(FloatDate))
# qplot(data=moddata,CtrlPopLf, popFloatDate, color = Origin, xlab="Population mean leaf number in control",
#       ylab="Population mean days to FloatDate in drought", main="Performance in drought vs control, leaf no.") +geom_smooth(method=glm, se=TRUE)

#explicit trade-off using shootmass
modeldata <- merge(modeldata, comeans, all.x=TRUE)
modeldata <- modeldata[!is.na(modeldata$CtrlPopShoot),]

modelobar<-lmer(FloatDate ~ Origin * CtrlPopShoot*Latitude +(Origin|PopID/Mom), family=poisson,data=modeldata)
model1raw<-lmer(FloatDate ~ Origin * CtrlPopShoot* Latitude + (1|PopID/Mom), family=poisson,data=modeldata)
anova(modelobar, model1raw)
model2raw<-lmer(FloatDate ~ Origin * CtrlPopShoot* Latitude +(1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(FloatDate ~ Origin * CtrlPopShoot* Latitude +(1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(56.023,1)
modelI <- lmer(FloatDate ~ Origin * CtrlPopShoot+ Latitude + (1|PopID), family=poisson,data=modeldata)
anova(modelI, model2raw)

modelL <- lmer(FloatDate ~ Origin * CtrlPopShoot+(1|PopID), family=poisson,data=modeldata)
anova(modelL, modelI)

modelCint <- lmer(FloatDate ~ Origin + CtrlPopShoot+Latitude+(1|PopID), family=poisson,data=modeldata)
anova(modelI, modelCint)

modelC <- lmer(FloatDate ~ Origin +Latitude+(1|PopID), family=poisson,data=modeldata)
anova(modelCint, modelC)

modelOraw<-lmer(FloatDate ~ Latitude+(1|PopID), family=poisson,data=modeldata)
anova(modelOraw,modelC) #test for significance of origin - origin NOT sig....!

# qplot(data=modeldata,CtrlPopShoot, FloatDate, color = Origin)+geom_point(position="jitter")
# moddata <- ddply(modeldata, .(PopID, Origin, Latitude, CtrlPopShoot), summarize, popCount=length(PopID), popFloatDate=mean(FloatDate))
# qplot(data=moddata,CtrlPopShoot, popFloatDate, color = Origin, 
#       xlab="Population mean shoot mass in control treatment", 
#       ylab="Population mean days to FloatDate in drought treatment", main="Performance in drought vs. control treatments") +geom_smooth(method=glm, se=TRUE)


####Flood, yellow####
modeldata<-f[!is.na(f$Yellow),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

# modelobar<-lmer(Yellow ~ Origin * Latitude +(Origin|PopID/Mom), family=poisson,data=modeldata)
# model1<-lmer(Yellow ~ Origin * Latitude +(1|PopID/Mom), family=poisson,data=modeldata)
# print(anova(modelobar, model1), digits = 22)
# (lambda <- (-2)*(-17.81004578325275033990 - (-17.81004578681382355398)))
# 1-pchisq(7.122146e-09,4)
# 
# model2<-lmer(Yellow ~ Origin * Latitude +(1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
# model3<-lmer(Yellow ~ Origin * Latitude +(1|blank), family=poisson,data=modeldata) # Test population effect
# anova(model2,model1) # mom sig
# anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
# 
# modelI<-lmer(Yellow ~ Origin + Latitude +(1|blank), family=poisson,data=modeldata)
# anova(modelI,model3) #test for significant interaction btw Origin and Bolted - not sig
# 
# modelL <- lmer(Yellow ~ Origin + (1|blank), family=poisson,data=modeldata)
# anova(modelL, modelI)
# 
# modelO<-lmer(Yellow ~ (1|blank), family=poisson,data=modeldata)
# anova(modelO,modelL) #test for significance of origin - origin not sig

#try glm
modelg <- glm(Yellow ~ Origin*Latitude, family=poisson,data=modeldata)
modelg1 <- glm(Yellow ~ Origin+Latitude, family=poisson,data=modeldata)
anova(modelg1, modelg, test="LRT") 
qchisq(0.5512,1,lower=FALSE)#chisq value

modelg3<- glm(Yellow ~ Origin, family=poisson,data=modeldata)
anova(modelg3,modelg1, test="LRT")
1-pchisq(5.5154, 1)
modelg2<- glm(Yellow ~ Latitude, family=poisson,data=modeldata)
anova(modelg2,modelg1, test="LRT")
1-pchisq(9.0533, 1)

CI.LS.poisson(modelg3)
summary(modelg1)

#overdispersion
library(AER)
dispersiontest(modelg3)

# #explicit tradeoff - using lf count
# modeldata <- merge(modeldata, comeans, all.x=TRUE)
# modeldata <- modeldata[!is.na(modeldata$CtrlPopLf),]
# 
# # modelobar<-lmer(Yellow ~ Origin * CtrlPopLf*Latitude +(Origin|PopID), family=poisson,data=modeldata)
# # model1raw<-lmer(Yellow ~ Origin * CtrlPopLf* Latitude + (1|PopID/Mom), family=poisson,data=modeldata)
# # anova(modelobar, model1raw)
# # model2raw<-lmer(Yellow ~ Origin * CtrlPopLf* Latitude +(1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
# # model3raw<-lmer(Yellow ~ Origin * CtrlPopLf* Latitude +(1|blank), family=poisson,data=modeldata) # Test population effect
# # anova(model2raw,model1raw) # mom not sig
# # anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
# # 1-pchisq(56.023,1)
# 
# modelg <- glm(Yellow ~ Origin*CtrlPopLf*Latitude, family=poisson,data=modeldata)
# modelg1 <- glm(Yellow ~ Origin*CtrlPopLf+Latitude, family=poisson,data=modeldata)
# anova(modelg1, modelg) #'Deviance' is chisq value
# 1-pchisq(hisq, df)
# 
# modelg3<- glm(Yellow ~ Origin*CtrlPopLf, family=poisson,data=modeldata)
# anova(modelg3,modelg1)
# 1-pchisq(5.5154, 1)
# modelg2<- glm(Yellow ~Origin +CtrlPopLf, family=poisson,data=modeldata)
# anova(modelg2,modelg3)
# 1-pchisq(4.8599, 1)
# 
# modelg4 <- glm(Yellow ~Origin, family=poisson, data=modeldata)
# anova(modelg4, modelg2)
# modelg5 <- glm(Yellow~CtrlPopLf, family=poisson, data=modeldata)
# anova(modelg2, modelg5)
# 
# qplot(data=modeldata,CtrlPopLf, Yellow, color = Origin)+geom_point(position="jitter")
# moddata <- ddply(modeldata, .(PopID, Origin, Latitude, CtrlPopLf), summarize, popCount=length(PopID), popYellow=mean(Yellow))
# qplot(data=moddata,CtrlPopLf, popYellow, color = Origin, xlab="Population mean leaf number in control",
#       ylab="Population mean days to Yellow in drought", main="Performance in drought vs control, leaf no.") +geom_smooth(method=glm, se=TRUE)

#explicit trade-off using shootmass
modeldata <- merge(modeldata, comeans, all.x=TRUE)
modeldata <- modeldata[!is.na(modeldata$CtrlPopShoot),]

# modelobar<-lmer(Yellow ~ Origin * CtrlPopShoot*Latitude +(Origin|PopID), family=poisson,data=modeldata)
# model1raw<-lmer(Yellow ~ Origin * CtrlPopShoot* Latitude + (1|PopID/Mom), family=poisson,data=modeldata)
# anova(modelobar, model1raw)
# model2raw<-lmer(Yellow ~ Origin * CtrlPopShoot* Latitude +(1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
# model3raw<-lmer(Yellow ~ Origin * CtrlPopShoot* Latitude +(1|blank), family=poisson,data=modeldata) # Test population effect
# anova(model2raw,model1raw) # mom not sig
# anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
# 1-pchisq(56.023,1)

modelg <- glm(Yellow ~ Origin*CtrlPopShoot*Latitude, family=poisson,data=modeldata)
modelg1 <- glm(Yellow ~ Origin*CtrlPopShoot+Latitude, family=poisson,data=modeldata)
anova(modelg1, modelg) #'Deviance' is chisq value
1-pchisq(hisq, df)

modelg3<- glm(Yellow ~ Origin*CtrlPopShoot, family=poisson,data=modeldata)
anova(modelg3,modelg1)
1-pchisq(5.5154, 1)
modelg2<- glm(Yellow ~Origin +CtrlPopShoot, family=poisson,data=modeldata)
anova(modelg2,modelg3)
1-pchisq(4.8599, 1)

modelg4 <- glm(Yellow ~Origin, family=poisson, data=modeldata)
anova(modelg4, modelg2)
modelg5 <- glm(Yellow~CtrlPopShoot, family=poisson, data=modeldata)
anova(modelg2, modelg5)

qplot(data=modeldata,CtrlPopShoot, Yellow, color = Origin)+geom_point(position="jitter")
moddata <- ddply(modeldata, .(PopID, Origin, Latitude, CtrlPopShoot), summarize, popCount=length(PopID), popYellow=mean(Yellow))
qplot(data=moddata,CtrlPopShoot, popYellow, color = Origin, 
      xlab="Population mean shoot mass in control treatment", 
      ylab="Population mean days to Yellow in drought treatment", main="Performance in drought vs. control treatments") +geom_smooth(method=glm, se=TRUE)


####flood, death####
modeldata<-f[!is.na(f$Death),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

modelobar<-lmer(Death ~ Origin * Latitude +(Origin|PopID/Mom), family=poisson,data=modeldata)
model1<-lmer(Death ~ Origin * Latitude +(1|PopID/Mom), family=poisson,data=modeldata)
anova(modelobar, model1)
model2<-lmer(Death ~ Origin * Latitude +(1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Death ~ Origin * Latitude +(1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2,model1) # mom sig
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(7.0121, 1)
modelI<-lmer(Death ~ Origin + Latitude +(1|PopID), family=poisson,data=modeldata)
anova(modelI,model2) #test for significant interaction btw Origin and Bolted - not sig

modelL <- lmer(Death ~ Origin + (1|PopID), family=poisson,data=modeldata)
anova(modelL, modelI)

modelO<-lmer(Death ~ Latitude + (1|PopID), family=poisson,data=modeldata)
anova(modelO,modelI) #test for significance of origin - origin not sig

CI.LS.poisson(modelI)
summary(modelI)

# #explicit tradeoff - using lf count
# modeldata <- merge(modeldata, comeans, all.x=TRUE)
# modeldata <- modeldata[!is.na(modeldata$CtrlPopLf),]
# 
# modelobar<-lmer(Death ~ Origin * CtrlPopLf*Latitude +(Origin|PopID/Mom), family=poisson,data=modeldata)
# model1raw<-lmer(Death ~ Origin * CtrlPopLf* Latitude + (1|PopID/Mom), family=poisson,data=modeldata)
# anova(modelobar, model1raw)
# model2raw<-lmer(Death ~ Origin * CtrlPopLf* Latitude +(1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
# model3raw<-lmer(Death ~ Origin * CtrlPopLf* Latitude +(1|blank), family=poisson,data=modeldata) # Test population effect
# anova(model2raw,model1raw) # mom not sig
# anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
# 1-pchisq(56.023,1)
# modelI <- lmer(Death ~ Origin * CtrlPopLf+ Latitude + (1|PopID), family=poisson,data=modeldata)
# anova(modelI, model2raw)
# 
# modelL <- lmer(Death ~ Origin * CtrlPopLf+(1|PopID), family=poisson,data=modeldata)
# anova(modelL, modelI)
# 
# modelCint <- lmer(Death ~ Origin + CtrlPopLf+(1|PopID), family=poisson,data=modeldata)
# anova(modelL, modelCint)
# 
# modelC <- lmer(Death ~ Origin +(1|PopID), family=poisson,data=modeldata)
# anova(modelCint, modelC)
# 
# modelOraw<-lmer(Death ~ (1|PopID), family=poisson,data=modeldata)
# anova(modelOraw,modelC) #test for significance of origin - origin NOT sig....!
# 
# qplot(data=modeldata,CtrlPopLf, Death, color = Origin)+geom_point(position="jitter")
# moddata <- ddply(modeldata, .(PopID, Origin, Latitude, CtrlPopLf), summarize, popCount=length(PopID), popDeath=mean(Death))
# qplot(data=moddata,CtrlPopLf, popDeath, color = Origin, xlab="Population mean leaf number in control",
#       ylab="Population mean days to Death in drought", main="Performance in drought vs control, leaf no.") +geom_smooth(method=glm, se=TRUE)

#explicit trade-off using shootmass
modeldata <- merge(modeldata, comeans, all.x=TRUE)
modeldata <- modeldata[!is.na(modeldata$CtrlPopShoot),]

modelobar<-lmer(Death ~ Origin * CtrlPopShoot*Latitude +(Origin|PopID/Mom), family=poisson,data=modeldata)
model1raw<-lmer(Death ~ Origin * CtrlPopShoot* Latitude + (1|PopID/Mom), family=poisson,data=modeldata)
anova(modelobar, model1raw)
model2raw<-lmer(Death ~ Origin * CtrlPopShoot* Latitude +(1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(Death ~ Origin * CtrlPopShoot* Latitude +(1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(56.023,1)

modelI <- lmer(Death ~ Origin * CtrlPopShoot+ Latitude + (1|PopID/Mom), family=poisson,data=modeldata)
anova(modelI, model1raw)

modelL <- lmer(Death ~ Origin * CtrlPopShoot+(1|PopID/Mom), family=poisson,data=modeldata)
anova(modelL, modelI)

modelCint <- lmer(Death ~ Origin + CtrlPopShoot+Latitude+(1|PopID/Mom), family=poisson,data=modeldata)
anova(modelI, modelCint)

modelC <- lmer(Death ~ Origin +Latitude+(1|PopID/Mom), family=poisson,data=modeldata)
anova(modelCint, modelC)

modelOraw<-lmer(Death ~ Latitude+(1|PopID/Mom), family=poisson,data=modeldata)
anova(modelOraw,modelC) #test for significance of origin - origin NOT sig....!

# modelg <- glm(Death ~ Origin*CtrlPopShoot*Latitude, family=poisson,data=modeldata)
modelg1 <- glm(Death ~ Origin*CtrlPopShoot+Latitude, family=poisson,data=modeldata)
# anova(modelg1, modelg) #'Deviance' is chisq value
# 1-pchisq(hisq, df)
# 
# modelg3<- glm(Death ~ Origin*CtrlPopShoot, family=poisson,data=modeldata)
# anova(modelg3,modelg1)
# 1-pchisq(5.5154, 1)
# modelg2<- glm(Death ~Origin +CtrlPopShoot, family=poisson,data=modeldata)
# anova(modelg2,modelg3)
# 1-pchisq(4.8599, 1)
# 
# modelg4 <- glm(Death ~Origin, family=poisson, data=modeldata)
# anova(modelg4, modelg2)
# modelg5 <- glm(Death~CtrlPopShoot, family=poisson, data=modeldata)
# anova(modelg2, modelg5)

summary(modelI)
# summary(modelg1)

qplot(data=modeldata,CtrlPopShoot, Death, color = Origin)+geom_point(position="jitter")
moddata <- ddply(modeldata, .(PopID, Origin, Latitude, CtrlPopShoot), summarize, popCount=length(PopID), popDeath=mean(Death))
stfldeathTO <- moddata #for figure making
qplot(data=moddata,CtrlPopShoot, popDeath, color = Origin, 
      xlab="Population mean shoot mass in control treatment", 
      ylab="Population mean days to Death in flood treatment", main="Performance in flood vs. control treatments") +geom_smooth(method=glm, se=TRUE)
