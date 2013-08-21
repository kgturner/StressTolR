#ST stress treatment - drought
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
                CtrlPopShoot=mean(ShootMass.g, na.rm=TRUE), CtrlPopShootSE=se(ShootMass.g))

####Drought, Origin * Lat####
d<-read.table("STDroughtsubset.txt", header=T, sep="\t", quote='"', row.names=1) #droughtsubset
head(d)
#no gaussian
dLR <- lapply(names(d)[8:10],function(n) CGtrait.LR.int(n,d, family=poisson)) #wilt, totwilt, death, all poisson
names(dLR) <- names(d)[8:10]

# dmodels <- lapply(names(d)[8:9],function(n) CGtrait.models.int(n,d))
# dmodels

###Drought, death, popID not sig, do by hand###
modeldata<-d[!is.na(d$Death),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

modelobar<-lmer(Death ~ Origin * Latitude +(Origin|PopID/Mom), family=poisson,data=modeldata)
model1<-lmer(Death ~ Origin * Latitude +(1|PopID/Mom), family=poisson,data=modeldata)
print(anova(modelobar, model1), digits = 22)
(lambda <- (-2)*(-56.79383078568032772182 - (-56.79394081098869406787)))
1-pchisq(0.0002200506,4)
model2<-lmer(Death ~ Origin * Latitude +(1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Death ~ Origin * Latitude +(1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2,model1) # mom sig
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelI<-lmer(Death ~ Origin + Latitude +(1|blank), family=poisson,data=modeldata)
anova(modelI,model3) #test for significant interaction btw Origin and Bolted - not sig

modelL <- lmer(Death ~ Origin +(1|blank), family=poisson,data=modeldata)
anova(modelL, modelI)

modelO<-lmer(Death ~ Latitude+(1|blank), family=poisson,data=modeldata)
anova(modelO,modelI) #test for significance of origin - origin not sig!
#try glm
modelg <- glm(Death ~ Origin*Latitude, family=poisson,data=modeldata)
modelg1 <- glm(Death ~ Origin+Latitude, family=poisson,data=modeldata)
anova(modelg1, modelg, test="LRT") 
qchisq(0.6874,1,lower=FALSE)#chisq value

modelg3<- glm(Death ~ Origin, family=poisson,data=modeldata)
anova(modelg3,modelg1, test="LRT")
qchisq(0.0004164,1,lower=FALSE)#chisq value
modelg2<- glm(Death ~ Latitude, family=poisson,data=modeldata)
anova(modelg2,modelg1, test="LRT")
qchisq(0.1018,1,lower=FALSE)#chisq value

CI.LS.poisson(modelg1)

#extra covariate
modeldata <- modeldata[!is.na(modeldata$lxw),]
modelg1 <- glm(Death ~ Origin+Latitude, family=poisson,data=modeldata)
modelgS <- glm(Death ~ Origin+Latitude+lxw, family=poisson,data=modeldata)
anova(modelg1, modelgS)
modelgO <- glm(Death ~ Latitude+lxw, family=poisson,data=modeldata)
anova(modelgS, modelgO)

# #explicit tradeoff - using lf count
# modeldata <- merge(modeldata, comeans, all.x=TRUE)
# modeldata <- modeldata[!is.na(modeldata$CtrlPopLf),]
# 
# # modelobar<-lmer(Death ~ Origin * CtrlPopLf+Latitude +(Origin|PopID), family=poisson,data=modeldata)
# # model1raw<-lmer(Death ~ Origin * CtrlPopLf+ Latitude + (1|PopID/Mom), family=poisson,data=modeldata)
# # anova(modelobar, model1raw)
# # model2raw<-lmer(Death ~ Origin * CtrlPopLf+ Latitude +(1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
# # model3raw<-lmer(Death ~ Origin * CtrlPopLf+ Latitude +(1|blank), family=poisson,data=modeldata) # Test population effect
# # anova(model2raw,model1raw) # mom not sig
# # anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
# # 1-pchisq(56.023,1)
# 
# 
# modelg <- glm(Death ~ Origin*CtrlPopLf*Latitude, family=poisson,data=modeldata)
# modelg1 <- glm(Death ~ Origin*CtrlPopLf+Latitude, family=poisson,data=modeldata)
# anova(modelg1, modelg) #'Deviance' is chisq value
# 1-pchisq(5.5533, 3)
# 
# modelg3<- glm(Death ~ Origin*CtrlPopLf, family=poisson,data=modeldata)
# anova(modelg3,modelg1)
# 1-pchisq(5.5154, 1)
# modelg2<- glm(Death ~Origin +CtrlPopLf+ Latitude, family=poisson,data=modeldata)
# anova(modelg2,modelg1)
# 1-pchisq(4.8599, 1)
# 
# # modelg4 <- glm(Death ~Origin + Latitude, family=poisson, data=modeldata)
# # anova(modelg4, modelg2)
# # modelg5 <- glm(Death~CtrlPopLf +Latitude, family=poisson, data=modeldata)
# # anova(modelg2, modelg5)
# # 
# # qplot(data=modeldata,CtrlPopLf, Death, color = Origin)+geom_point(position="jitter")
# # moddata <- ddply(modeldata, .(PopID, Origin, Latitude, CtrlPopLf), summarize, popCount=length(PopID), popDeath=mean(Death))
# # qplot(data=moddata,CtrlPopLf, popDeath, color = Origin, xlab="Population mean leaf number in control",
# #       ylab="Population mean days to Death in drought", main="Performance in drought vs control, leaf no.") +geom_smooth(method=glm, se=TRUE)

#explicit trade-off using shootmass
modeldata <- merge(modeldata, comeans, all.x=TRUE)
modeldata <- modeldata[!is.na(modeldata$CtrlPopShoot),]

modelobar<-lmer(Death ~ Origin * CtrlPopShoot*Latitude +(Origin|PopID/Mom), family=poisson,data=modeldata)
model1raw<-lmer(Death ~ Origin * CtrlPopShoot+ Latitude + (1|PopID/Mom), family=poisson,data=modeldata)
anova(modelobar, model1raw)
model2raw<-lmer(Death ~ Origin * CtrlPopShoot+ Latitude +(1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(Death ~ Origin * CtrlPopShoot+ Latitude +(1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(3.543,1)

modelg <- glm(Death ~ Origin*CtrlPopShoot*Latitude, family=poisson,data=modeldata)
modelg1 <- glm(Death ~ Origin*CtrlPopShoot+Latitude, family=poisson,data=modeldata)
anova(modelg1, modelg, test="LRT") 
qchisq(0.8596,1,lower=FALSE)#chisq value

modelg3<- glm(Death ~ Origin*CtrlPopShoot, family=poisson,data=modeldata)
anova(modelg3,modelg1, test="LRT")
qchisq(0.05969,1,lower=FALSE)#chisq value
modelg2<- glm(Death ~Origin +CtrlPopShoot, family=poisson,data=modeldata)
anova(modelg2,modelg3, test="LRT")
qchisq(0.03689,1,lower=FALSE)#chisq value

modelg4 <- glm(Death ~Origin, family=poisson, data=modeldata)
anova(modelg4, modelg2, test="LRT")
qchisq(1.488e-05,1,lower=FALSE)#chisq value
modelg5 <- glm(Death~CtrlPopShoot, family=poisson, data=modeldata)
anova(modelg5, modelg2, test="LRT")

modelg3
summary(modelg3)
anova(modelg3, test="LRT")

CI.LS.poisson(modelg3, conf=95)

qplot(data=modeldata,CtrlPopShoot, Death, color = Origin)+geom_point(position="jitter")
moddata <- ddply(modeldata, .(PopID, Origin, Latitude, CtrlPopShoot), summarize, popCount=length(PopID), popDeath=mean(Death))

png("STdr_deathtradeoff_color.png",width=800, height = 600, pointsize = 16)
qplot(data=moddata,CtrlPopShoot, popDeath, color = Origin, 
      xlab="Population mean shoot mass in control treatment", 
      ylab="Population mean days to Death in drought treatment", main="Performance in drought vs. control treatments") +geom_smooth(method=glm, se=TRUE)
dev.off()

#remove inv outlier - largest pop in ctrl - US021
moddata <- moddata[moddata$PopID!="US021",]
qplot(data=moddata,CtrlPopShoot, popDeath, color = Origin, 
      xlab="Population mean shoot mass in control treatment", 
      ylab="Population mean days to Death in drought treatment", main="Performance in drought vs. control treatments") +geom_smooth(method=glm, se=TRUE)

modeldata <- modeldata[modeldata$PopID!="US021",]
modelg <- glm(Death ~ Origin*CtrlPopShoot*Latitude, family=poisson,data=modeldata)
modelg1 <- glm(Death ~ Origin*CtrlPopShoot+Latitude, family=poisson,data=modeldata)
anova(modelg1, modelg, test="LRT") 
qchisq(pval,1,lower=FALSE)#chisq value

modelg3<- glm(Death ~ Origin*CtrlPopShoot, family=poisson,data=modeldata)
anova(modelg3,modelg1, test="LRT")
qchisq(pval,1,lower=FALSE)#chisq value
modelg2<- glm(Death ~Origin +CtrlPopShoot, family=poisson,data=modeldata)
anova(modelg2,modelg3, test="LRT")
qchisq(pval,1,lower=FALSE)#chisq value

modelg4 <- glm(Death ~Origin, family=poisson, data=modeldata)
anova(modelg4, modelg2, test="LRT")
modelg5 <- glm(Death~CtrlPopShoot, family=poisson, data=modeldata)
anova(modelg5, modelg2, test="LRT")

modelg3
summary(modelg3)
anova(modelg3, test="LRT")


###drought, total wilt
modeldata<-d[!is.na(d$TotWilt),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

modelobar<-lmer(TotWilt ~ Origin * Latitude +(Origin|PopID/Mom), family=poisson,data=modeldata)
model1<-lmer(TotWilt ~ Origin * Latitude +(1|PopID/Mom), family=poisson,data=modeldata)
anova(modelobar, model1)
model2<-lmer(TotWilt ~ Origin * Latitude +(1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(TotWilt ~ Origin * Latitude +(1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2,model1) # mom sig
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(1.1142, 1)
# modelI<-lmer(TotWilt ~ Origin + Latitude +(1|blank), family=poisson,data=modeldata)
# anova(modelI,model3) #test for significant interaction btw Origin and Bolted - not sig
# 
# modelL <- lmer(TotWilt ~ Origin +(1|blank), family=poisson,data=modeldata)
# anova(modelL, modelI)
# modelO<-lmer(TotWilt ~ Latitude+(1|blank), family=poisson,data=modeldata)
# anova(modelO,modelI) #test for significance of origin - origin not sig!
modelI
int<-2.591741#inv mean
B<-0.061169#Originnat estimate from model summary
pI<-exp(int)
pN<-exp(int+B)
pI
pN
#try glm
modelg <- glm(TotWilt ~ Origin*Latitude, family=poisson,data=modeldata)
modelg1 <- glm(TotWilt ~ Origin+Latitude, family=poisson,data=modeldata)
anova(modelg1, modelg, test="LRT") 
qchisq(0.2744,1,lower=FALSE)#chisq value

modelg3<- glm(TotWilt ~ Origin, family=poisson,data=modeldata)
anova(modelg3,modelg1, test="LRT")
qchisq(0.005593,1,lower=FALSE)#chisq value
modelg2<- glm(TotWilt ~ Latitude, family=poisson,data=modeldata)
anova(modelg2,modelg1, test="LRT")
qchisq(0.2531,1,lower=FALSE)#chisq value

CI.LS.poisson(modelg1)

#extra covariates
modeldata <- modeldata[!is.na(modeldata$lxw),]
modelg1 <- glm(TotWilt ~ Origin+Latitude, family=poisson,data=modeldata)
modelgS <- glm(TotWilt ~ Origin+Latitude+lxw, family=poisson,data=modeldata)
anova(modelg1, modelgS)
modelgO <- glm(TotWilt ~ Latitude+lxw, family=poisson,data=modeldata)
anova(modelgS, modelgO)

# #explicit tradeoff - using lf count
# modeldata <- merge(modeldata, comeans, all.x=TRUE)
# modeldata <- modeldata[!is.na(modeldata$CtrlPopLf),]
# 
# # modelobar<-lmer(TotWilt ~ Origin * CtrlPopLf*Latitude +(Origin|PopID/Mom), family=poisson,data=modeldata)
# # model1raw<-lmer(TotWilt ~ Origin * CtrlPopLf* Latitude + (1|PopID/Mom), family=poisson,data=modeldata)
# # anova(modelobar, model1raw)
# # model2raw<-lmer(TotWilt ~ Origin * CtrlPopLf* Latitude +(1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
# # model3raw<-lmer(TotWilt ~ Origin * CtrlPopLf* Latitude +(1|blank), family=poisson,data=modeldata) # Test population effect
# # anova(model2raw,model1raw) # mom not sig
# # anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
# # 1-pchisq(56.023,1)
# 
# modelg <- glm(TotWilt ~ Origin*CtrlPopLf*Latitude, family=poisson,data=modeldata)
# modelg1 <- glm(TotWilt ~ Origin*CtrlPopLf+Latitude, family=poisson,data=modeldata)
# anova(modelg1, modelg) #'Deviance' is chisq value
# 1-pchisq(5.73,3)
# 
# modelg3<- glm(TotWilt ~ Origin*CtrlPopLf, family=poisson,data=modeldata)
# anova(modelg3,modelg1)
# 1-pchisq(5.5154, 1)
# modelg2<- glm(TotWilt ~Origin +CtrlPopLf+Latitude, family=poisson,data=modeldata)
# anova(modelg2,modelg1)
# 1-pchisq(3.0467, 1)
# 
# modelg4 <- glm(TotWilt ~Origin, family=poisson, data=modeldata)
# anova(modelg4, modelg2)
# modelg5 <- glm(TotWilt~CtrlPopLf, family=poisson, data=modeldata)
# anova(modelg2, modelg5)
# 
# qplot(data=modeldata,CtrlPopLf, TotWilt, color = Origin)+geom_point(position="jitter")
# moddata <- ddply(modeldata, .(PopID, Origin, Latitude, CtrlPopLf), summarize, popCount=length(PopID), popTotWilt=mean(TotWilt))
# qplot(data=moddata,CtrlPopLf, popTotWilt, color = Origin, xlab="Population mean leaf number in control",
#       ylab="Population mean days to TotWilt in drought", main="Performance in drought vs control, leaf no.") +geom_smooth(method=glm, se=TRUE)
# 
# summary(modelg3)

#explicit trade-off using shootmass
modeldata <- merge(modeldata, comeans, all.x=TRUE)
modeldata <- modeldata[!is.na(modeldata$CtrlPopShoot),]

modelobar<-lmer(TotWilt ~ Origin * CtrlPopShoot*Latitude +(Origin|PopID/Mom), family=poisson,data=modeldata)
model1raw<-lmer(TotWilt ~ Origin * CtrlPopShoot* Latitude + (1|PopID/Mom), family=poisson,data=modeldata)
anova(modelobar, model1raw)
model2raw<-lmer(TotWilt ~ Origin * CtrlPopShoot* Latitude +(1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(TotWilt ~ Origin * CtrlPopShoot* Latitude +(1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(56.023,1)

modelg <- glm(TotWilt ~ Origin*CtrlPopShoot*Latitude, family=poisson,data=modeldata)
modelg1 <- glm(TotWilt ~ Origin*CtrlPopShoot+Latitude, family=poisson,data=modeldata)
anova(modelg1, modelg, test="LRT") #'Deviance' is chisq value
1-pchisq(3.2808, 3)

modelg3<- glm(TotWilt ~ Origin*CtrlPopShoot, family=poisson,data=modeldata)
anova(modelg3,modelg1, test="LRT")
1-pchisq(5.5154, 1)
modelg2<- glm(TotWilt ~Origin +CtrlPopShoot+Latitude, family=poisson,data=modeldata)
anova(modelg2,modelg1, test="LRT")
1-pchisq(4.8599, 1)

modelg4 <- glm(TotWilt ~Origin+Latitude, family=poisson, data=modeldata)
anova(modelg4, modelg2, test="LRT")
modelg5 <- glm(TotWilt~Latitude, family=poisson, data=modeldata)
anova(modelg5, modelg4, test="LRT")

qplot(data=modeldata,CtrlPopShoot, TotWilt, color = Origin)+geom_point(position="jitter")
moddata <- ddply(modeldata, .(PopID, Origin, Latitude, CtrlPopShoot), summarize, popCount=length(PopID), popTotWilt=mean(TotWilt))

png("STdr_totwilttradeoff_color.png",width=800, height = 600, pointsize = 16)
qplot(data=moddata,CtrlPopShoot, popTotWilt, color = Origin, 
      xlab="Population mean shoot mass in control treatment", 
      ylab="Population mean days to TotWilt in drought treatment", main="Performance in drought vs. control treatments") +geom_smooth(method=glm, se=TRUE)
dev.off()

summary(modelg3)
summary(modelg1, test="LRT")

#remove inv outlier, largest inv pop US021
moddata <- moddata[moddata$PopID!="US021",]
qplot(data=moddata,CtrlPopShoot, popTotWilt, color = Origin, 
      xlab="Population mean shoot mass in control treatment", 
      ylab="Population mean days to TotWilt in drought treatment", main="Performance in drought vs. control treatments") +geom_smooth(method=glm, se=TRUE)

modeldata <- modeldata[modeldata$PopID!="US021",]
modelg <- glm(TotWilt ~ Origin*CtrlPopShoot*Latitude, family=poisson,data=modeldata)
modelg1 <- glm(TotWilt ~ Origin*CtrlPopShoot+Latitude, family=poisson,data=modeldata)
anova(modelg1, modelg, test="LRT") #'Deviance' is chisq value
1-pchisq(3.2808, 3)

modelg3<- glm(TotWilt ~ Origin*CtrlPopShoot, family=poisson,data=modeldata)
anova(modelg3,modelg1, test="LRT")
1-pchisq(5.5154, 1)
modelg2<- glm(TotWilt ~Origin +CtrlPopShoot+Latitude, family=poisson,data=modeldata)
anova(modelg2,modelg1, test="LRT")
1-pchisq(4.8599, 1)

modelg4 <- glm(TotWilt ~Origin+Latitude, family=poisson, data=modeldata)
anova(modelg4, modelg2, test="LRT")
modelg5 <- glm(TotWilt~Latitude, family=poisson, data=modeldata)
anova(modelg5, modelg4, test="LRT")

#remove inv outlier, largest inv pop US021
moddata <- moddata[moddata$PopID!="US021",]
qplot(data=moddata,CtrlPopShoot, popTotWilt, color = Origin, 
      xlab="Population mean shoot mass in control treatment", 
      ylab="Population mean days to TotWilt in drought treatment", main="Performance in drought vs. control treatments") +geom_smooth(method=glm, se=TRUE)

modeldata <- modeldata[modeldata$PopID!="US021",]
modelg <- glm(TotWilt ~ Origin*CtrlPopShoot*Latitude, family=poisson,data=modeldata)
modelg1 <- glm(TotWilt ~ Origin*CtrlPopShoot+Latitude, family=poisson,data=modeldata)
anova(modelg1, modelg, test="LRT") #'Deviance' is chisq value
1-pchisq(3.2808, 3)

modelg3<- glm(TotWilt ~ Origin*CtrlPopShoot, family=poisson,data=modeldata)
anova(modelg3,modelg1, test="LRT")
1-pchisq(5.5154, 1)
modelg2<- glm(TotWilt ~Origin +CtrlPopShoot+Latitude, family=poisson,data=modeldata)
anova(modelg2,modelg1, test="LRT")
1-pchisq(4.8599, 1)

modelg4 <- glm(TotWilt ~Origin+Latitude, family=poisson, data=modeldata)
anova(modelg4, modelg2, test="LRT")
modelg5 <- glm(TotWilt~Latitude, family=poisson, data=modeldata)
anova(modelg5, modelg4, test="LRT")

###drought, wilt
modeldata<-d[!is.na(d$Wilt),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

modelobar<-lmer(Wilt ~ Origin * Latitude +(Origin|PopID/Mom), family=poisson,data=modeldata)
model1<-lmer(Wilt ~ Origin * Latitude +(1|PopID/Mom), family=poisson,data=modeldata)
anova(modelobar, model1)
model2<-lmer(Wilt ~ Origin * Latitude +(1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Wilt ~ Origin * Latitude +(1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2,model1) # mom sig
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(2.1202, 1)
modelI<-lmer(Wilt ~ Origin + Latitude +(1|blank), family=poisson,data=modeldata)
anova(modelI,model3) #test for significant interaction btw Origin and Bolted - not sig

modelL <- lmer(Wilt ~ Origin +(1|blank), family=poisson,data=modeldata)
anova(modelL, modelI)
modelO<-lmer(Wilt ~ Latitude+(1|blank), family=poisson,data=modeldata)
anova(modelO,modelI) #test for significance of origin - origin not sig!

#try glm
modelg <- glm(Wilt ~ Origin*Latitude, family=poisson,data=modeldata)
modelg1 <- glm(Wilt ~ Origin+Latitude, family=poisson,data=modeldata)
anova(modelg1, modelg, test="LRT") 
qchisq(0.4786,1,lower=FALSE)#chisq value
anova(modelg1, modelg) #'Deviance' is chisq value
1-pchisq(0.50203, 1)

modelg3<- glm(Wilt ~ Origin, family=poisson,data=modeldata)
anova(modelg3,modelg1, test="LRT")
qchisq(0.01182,1,lower=FALSE)#chisq value
modelg2<- glm(Wilt ~ Latitude, family=poisson,data=modeldata)
anova(modelg2,modelg1, test="LRT")
1-pchisq(9.0533, 1)

CI.LS.poisson(modelg1)

# #explicit tradeoff - using lf count
# modeldata <- merge(modeldata, comeans, all.x=TRUE)
# modeldata <- modeldata[!is.na(modeldata$CtrlPopLf),]
# 
# # modelobar<-lmer(Wilt ~ Origin * CtrlPopLf*Latitude +(Origin|PopID), family=poisson,data=modeldata)
# # model1raw<-lmer(Wilt ~ Origin * CtrlPopLf* Latitude + (1|PopID/Mom), family=poisson,data=modeldata)
# # anova(modelobar, model1raw)
# # model2raw<-lmer(Wilt ~ Origin * CtrlPopLf* Latitude +(1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
# # model3raw<-lmer(Wilt ~ Origin * CtrlPopLf* Latitude +(1|blank), family=poisson,data=modeldata) # Test population effect
# # anova(model2raw,model1raw) # mom not sig
# # anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
# # 1-pchisq(56.023,1)
# 
# modelg <- glm(Wilt ~ Origin*CtrlPopLf*Latitude, family=poisson,data=modeldata)
# modelg1 <- glm(Wilt ~ Origin*CtrlPopLf+Latitude, family=poisson,data=modeldata)
# anova(modelg1, modelg) #'Deviance' is chisq value
# 1-pchisq(hisq, df)
# 
# modelg3<- glm(Wilt ~ Origin*CtrlPopLf, family=poisson,data=modeldata)
# anova(modelg3,modelg1)
# 1-pchisq(5.5154, 1)
# modelg2<- glm(Wilt ~Origin +CtrlPopLf, family=poisson,data=modeldata)
# anova(modelg2,modelg3)
# 1-pchisq(4.8599, 1)
# 
# modelg4 <- glm(Wilt ~Origin, family=poisson, data=modeldata)
# anova(modelg4, modelg2)
# modelg5 <- glm(Wilt~CtrlPopLf, family=poisson, data=modeldata)
# anova(modelg2, modelg5)
# 
# qplot(data=modeldata,CtrlPopLf, Wilt, color = Origin)+geom_point(position="jitter")
# moddata <- ddply(modeldata, .(PopID, Origin, Latitude, CtrlPopLf), summarize, popCount=length(PopID), popWilt=mean(Wilt))
# qplot(data=moddata,CtrlPopLf, popWilt, color = Origin, xlab="Population mean leaf number in control",
#       ylab="Population mean days to wilt in drought", main="Performance in drought vs control, leaf no.")+
#   geom_smooth(method=glm, family=poisson,se=TRUE)
# summary(modelg3)
# CI.LS.poisson(modelg3)

#explicit trade-off using shootmass
modeldata <- merge(modeldata, comeans, all.x=TRUE)
modeldata <- modeldata[!is.na(modeldata$CtrlPopShoot),]

modelobar<-lmer(Wilt ~ Origin * CtrlPopShoot*Latitude +(Origin|PopID), family=poisson,data=modeldata)
model1raw<-lmer(Wilt ~ Origin * CtrlPopShoot* Latitude + (1|PopID/Mom), family=poisson,data=modeldata)
anova(modelobar, model1raw)
model2raw<-lmer(Wilt ~ Origin * CtrlPopShoot* Latitude +(1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(Wilt ~ Origin * CtrlPopShoot* Latitude +(1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(56.023,1)

modelg <- glm(Wilt ~ Origin*CtrlPopShoot*Latitude, family=poisson,data=modeldata)
modelg1 <- glm(Wilt ~ Origin*CtrlPopShoot+Latitude, family=poisson,data=modeldata)
anova(modelg1, modelg, test="LRT") #'Deviance' is chisq value
1-pchisq(hisq, df)

modelg3<- glm(Wilt ~ Origin*CtrlPopShoot, family=poisson,data=modeldata)
anova(modelg3,modelg1, test="LRT")
1-pchisq(3.3075, 1)
modelg2<- glm(Wilt ~Origin +CtrlPopShoot, family=poisson,data=modeldata)
anova(modelg2,modelg3, test="LRT")
1-pchisq(4.8599, 1)

modelg4 <- glm(Wilt ~Origin, family=poisson, data=modeldata)
anova(modelg4, modelg2, test="LRT")
modelg5 <- glm(Wilt~CtrlPopShoot, family=poisson, data=modeldata)
anova(modelg5, modelg2, test="LRT")

summary(modelg3)

qplot(data=modeldata,CtrlPopShoot, Wilt, color = Origin)+geom_point(position="jitter")
moddata <- ddply(modeldata, .(PopID, Origin, Latitude, CtrlPopShoot), summarize, popCount=length(PopID), popWilt=mean(Wilt))
stdrwiltTO <- moddata #for making figures
png("ST_performance_drwilt_shoot.png",width=800, height = 600, pointsize = 16)

qplot(data=moddata,CtrlPopShoot, popWilt, color = Origin, 
      xlab="Population mean shoot mass in control treatment", 
      ylab="Population mean days to wilt in drought treatment", 
      main="Performance in drought vs. control treatments") +
  geom_smooth(method=glm, se=TRUE)
# family=poisson
dev.off()

#all data
qplot(data=modeldata, CtrlPopShoot, Wilt, color = Origin,
      xlab="Population mean shoot mass in control treatment",
      ylab="Days to wilt in drought treatment",
      main="Performance in drought vs. control")+
  geom_smooth(method=glm, family=poisson, se=TRUE)+
  geom_point(position="jitter")

#take out outlier inv - biggest invasive
moddata <- moddata[moddata$PopID!="US021",]

png("ST_performance_drwilt_shoot_Outlierremoved.png",width=800, height = 600, pointsize = 16)
qplot(data=moddata,CtrlPopShoot, popWilt, color = Origin, 
      xlab="Population mean shoot mass in control treatment", 
      ylab="Population mean days to wilt in drought treatment", 
      main="Performance in drought vs. control treatments") +
  geom_smooth(method=glm, se=TRUE)
dev.off()

modeldata <- modeldata[modeldata$PopID!="US021",]

modelg <- glm(Wilt ~ Origin*CtrlPopShoot*Latitude, family=poisson,data=modeldata)
modelg1 <- glm(Wilt ~ Origin*CtrlPopShoot+Latitude, family=poisson,data=modeldata)
anova(modelg1, modelg, test="LRT") #'Deviance' is chisq value
1-pchisq(hisq, df)

modelg3<- glm(Wilt ~ Origin*CtrlPopShoot, family=poisson,data=modeldata)
anova(modelg3,modelg1, test="LRT")
1-pchisq(3.3075, 1)
modelg2<- glm(Wilt ~Origin +CtrlPopShoot, family=poisson,data=modeldata)
anova(modelg2,modelg3, test="LRT")
1-pchisq(4.8599, 1)

modelg4 <- glm(Wilt ~Origin, family=poisson, data=modeldata)
anova(modelg4, modelg2, test="LRT")
modelg5 <- glm(Wilt~CtrlPopShoot, family=poisson, data=modeldata)
anova(modelg2, modelg5, test="LRT")

summary(modelg3)

# #take out outlier inv - most tolerant invasive
# moddata <- moddata[moddata$PopID!="US026",]
# qplot(data=moddata,CtrlPopShoot, popWilt, color = Origin, 
#       xlab="Population mean shoot mass in control treatment", 
#       ylab="Population mean days to wilt in drought treatment", 
#       main="Performance in drought vs. control treatments") +
#   geom_smooth(method=glm, se=TRUE)
# 
# modeldata <- modeldata[modeldata$PopID!="US026",]
# 
# modelg <- glm(Wilt ~ Origin*CtrlPopShoot*Latitude, family=poisson,data=modeldata)
# modelg1 <- glm(Wilt ~ Origin*CtrlPopShoot+Latitude, family=poisson,data=modeldata)
# anova(modelg1, modelg, test="LRT") #'Deviance' is chisq value
# 1-pchisq(hisq, df)
# 
# modelg3<- glm(Wilt ~ Origin*CtrlPopShoot, family=poisson,data=modeldata)
# anova(modelg3,modelg1, test="LRT")
# 1-pchisq(3.3075, 1)
# modelg2<- glm(Wilt ~Origin +CtrlPopShoot, family=poisson,data=modeldata)
# anova(modelg2,modelg3, test="LRT")
# 1-pchisq(4.8599, 1)
# 
# modelg4 <- glm(Wilt ~Origin, family=poisson, data=modeldata)
# anova(modelg4, modelg2, test="LRT")
# anova(modelg4, test="LRT")
# modelg5 <- glm(Wilt~CtrlPopShoot, family=poisson, data=modeldata)
# anova(modelg5, modelg2, test="LRT")
# 
# summary(modelg3)
# 
# #take out outlier nat/over all most tolerant - TR003
# moddata <- moddata[moddata$PopID!="TR003",]
# qplot(data=moddata,CtrlPopShoot, popWilt, color = Origin, 
#       xlab="Population mean shoot mass in control treatment", 
#       ylab="Population mean days to wilt in drought treatment", 
#       main="Performance in drought vs. control treatments") +
#   geom_smooth(method=glm, se=TRUE)
# 
# modeldata <- modeldata[modeldata$PopID!="TR003",]
# 
# modelg <- glm(Wilt ~ Origin*CtrlPopShoot*Latitude, family=poisson,data=modeldata)
# modelg1 <- glm(Wilt ~ Origin*CtrlPopShoot+Latitude, family=poisson,data=modeldata)
# anova(modelg1, modelg, test="LRT") #'Deviance' is chisq value
# 1-pchisq(hisq, df)
# 
# modelg3<- glm(Wilt ~ Origin*CtrlPopShoot, family=poisson,data=modeldata)
# anova(modelg3,modelg1, test="LRT")
# 1-pchisq(3.3075, 1)
# modelg2<- glm(Wilt ~Origin +CtrlPopShoot, family=poisson,data=modeldata)
# anova(modelg2,modelg3, test="LRT")
# 1-pchisq(4.8599, 1)
# 
# modelg4 <- glm(Wilt ~Origin, family=poisson, data=modeldata)
# anova(modelg4, modelg2, test="LRT")
# modelg5 <- glm(Wilt~CtrlPopShoot, family=poisson, data=modeldata)
# anova(modelg5, modelg2, test="LRT")
# 
# summary(modelg3)

#drought, wilt, extra covariates#
#lfcount1 as size
modeldata <- modeldata[!is.na(modeldata$LfCount1),]
modelg <- glm(Wilt ~ Origin*Latitude+LfCount1, family=poisson,data=modeldata)
modelg1 <- glm(Wilt ~ Origin+Latitude+LfCount1, family=poisson,data=modeldata)
anova(modelg1, modelg) #'Deviance' is chisq value
1-pchisq(4.2138, 3)

modelg3<- glm(Wilt ~ Origin+LfCount1, family=poisson,data=modeldata)
anova(modelg3,modelg1)
1-pchisq(5.5154, 1)
modelg2<- glm(Wilt ~ LfCount1, family=poisson,data=modeldata)
anova(modelg2,modelg3)
1-pchisq(7.6595, 1)

modelg4 <- glm(Wilt ~ Origin, family=poisson, data=modeldata)
anova(modelg4, modelg3)

#lxw as size
modeldata <- modeldata[!is.na(modeldata$lxw),]
modelg <- glm(Wilt ~ Origin*Latitude+lxw, family=poisson,data=modeldata)
modelg1 <- glm(Wilt ~ Origin+Latitude+lxw, family=poisson,data=modeldata)
anova(modelg1, modelg) #'Deviance' is chisq value
1-pchisq(chisq, df)

modelg3<- glm(Wilt ~ Origin+lxw, family=poisson,data=modeldata)
anova(modelg3,modelg1)
1-pchisq(3.0772, 1)
modelg2<- glm(Wilt ~ lxw, family=poisson,data=modeldata)
anova(modelg2,modelg3)
1-pchisq(3.7294, 1)

modelg4 <- glm(Wilt ~ Origin, family=poisson, data=modeldata)
anova(modelg4, modelg3)

# model1<-lmer(Wilt ~ Origin * Latitude+lxw+(lxw|PopID/Mom), family=poisson,data=modeldata)
# model2<-lmer(Wilt ~ Origin * Latitude+lxw+(lxw|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
# model3<-lmer(Wilt ~ Origin * Latitude+lxw+(lxw|blank), family=poisson,data=modeldata) # Test population effect
# anova(model2,model1) # mom sig
# anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
# 
# modelInt<-lmer(Wilt ~ Origin + Latitude+lxw+(lxw|PopID), family=poisson,data=modeldata)
# anova(modelInt,model2) #test for significant interaction btw Origin and Bolted - not sig
# 
# modelL0<-lmer(Wilt ~ Origin +Latitude+ (1|PopID), family=poisson,data=modeldata)
# modelL1<-lmer(Wilt ~ Origin +Latitude+ lxw+(1|PopID), family=poisson,data=modeldata)
# modelL2<-lmer(Wilt ~ Origin+Latitude + (lxw|PopID), family=poisson,data=modeldata)
# anova(modelL0, modelInt)#test for sig of size, approx by lf length
# anova(modelL1, modelInt)#test for sig of size
# anova(modelL2, modelInt)#test for sig of size
# 
# modelLat<-lmer(Wilt ~ Origin + lxw+(1|PopID), family=poisson,data=modeldata)
# anova(modelLat, modelL1)
# 
# modelO<-lmer(Wilt ~ lxw+(1|PopID), family=poisson,data=modeldata)
# anova(modelO,modelLat) #test for significance of origin - origin  sig!
# modelLat

# model1<-lmer(Wilt ~ Origin * Latitude+lxw+(1|PopID), family=poisson,data=modeldata)
# model2<-lmer(Wilt ~ Origin + Latitude+lxw+(1|PopID), family=poisson,data=modeldata)
# model3<-lmer(Wilt ~ Origin +lxw+(1|PopID), family=poisson,data=modeldata)
# model4<-lmer(Wilt ~ Origin * lxw+(1|PopID), family=poisson,data=modeldata)
# anova(model1, model2)
# anova(model2, model3)
# anova(model4, model3)
# model5<-lmer(Wilt ~ Origin +(1|PopID), family=poisson,data=modeldata)
# model6<-lmer(Wilt ~ lxw+(1|PopID), family=poisson,data=modeldata)
# anova(model3,model5)
# anova(model3, model6)

###is size different in this subset?###
# ###d, lxw, mom sig, do by hand###

modeldata<-d[!is.na(d$lxw),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

modelobar <- lmer(lxw ~ Origin *Latitude+(Origin|PopID/Mom), family=gaussian,data=modeldata)
model1<-lmer(lxw ~ Origin *Latitude+(1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelobar, model1)
model2<-lmer(lxw ~ Origin *Latitude+ (1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(lxw ~ Origin *Latitude+ (1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2,model1) # mom is sig!
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(96.674,1)
modelI <- lmer(lxw  ~ Origin + Latitude + (1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelI,model1)

modelL<-lmer(lxw ~ Origin +(1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelL, modelI)

modelO<-lmer(lxw ~ (1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelO,modelL) #test for significance of origin - origin not sig....?

# 
# lsmeans(modelL, ~ Origin, conf=95)
# 
# ####d, control, lf count####
modeldata<-d[!is.na(m1$LfCount1),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

modelobar <- lmer(LfCount1 ~ Origin *Latitude +(Origin|PopID/Mom), family=poisson,data=modeldata)
model1raw<-lmer(LfCount1 ~ Origin *Latitude +(1|PopID/Mom), family=poisson,data=modeldata)
anova(modelobar, model1raw)
model2raw<-lmer(LfCount1 ~ Origin *Latitude+(1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(LfCount1 ~ Origin *Latitude+(1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

#glm

modelg <- glm(LfCount1 ~ Origin*Latitude, family=poisson,data=modeldata)
modelg1 <- glm(LfCount1 ~ Origin+Latitude, family=poisson,data=modeldata)
anova(modelg1, modelg) #'Deviance' is chisq value
1-pchisq(chisq, df)

modelg3<- glm(LfCount1 ~ Origin, family=poisson,data=modeldata)
anova(modelg3,modelg1)
1-pchisq(5.5154, 1)
modelg2<- glm(LfCount1 ~ Latitude, family=poisson,data=modeldata)
anova(modelg2,modelg1)
1-pchisq(9.0533, 1)



# 
# qplot(data=modeldata,Latitude, LfCount1, color = Origin)
# moddata <- ddply(modeldata, .(PopID, Origin, Latitude), summarize, popLf=length(LfCount1), poplfavg=mean(LfCount1))
# moddata
# qplot(data=moddata,Latitude, poplfavg, color = Origin) +geom_smooth(method=lm, se=FALSE)
# qplot(data=moddata[moddata$Latitude<50,],Latitude, poplfavg, color = Origin) +geom_smooth(method=lm, se=FALSE)
