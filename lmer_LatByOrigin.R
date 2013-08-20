###ST mixed FX models, focused on Origin BY Latitude###
#Stress Tolerance, REML, using lme4
#mixed effect models 
library(lme4)
library(lsmeans)
library(ggplot2)
library(plyr)

# #if they make you do F tests for fixed fx
# library(lmerTest)
# step(fullmodel)
# #but breaks lsmeans, and custom functions like CI.LS.poisson, or CGtrait.LR.int
# #also chokes on complex random fx

#for each normal trait, compare this general set of models
modelobar<-lmer(trait  ~ Origin* Latitude +(Origin|PopID/Mom), family=gaussian,data=modeldata)#not inlcuded in CGtrait.LR functions
model1<-lmer(trait  ~ Origin* Latitude +(1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelobar, model1)
model2<-lmer(trait  ~ Origin* Latitude + (1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(trait  ~ Origin* Latitude + (1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2,model1) # mom is sig!
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
modelI <- lmer(trait  ~ Origin + Latitude + (1|PopID), family=family,data=modeldata)
anova(modelI,model2)
modelL<-lmer(trait  ~ Origin + (1|PopID), family=gaussian,data=modeldata)
anova(modelL, model1)
modelO<-lmer(trait ~ Latitude +(1|PopID), family=gaussian,data=modeldata)
anova(modelO,model1) #test for significance of origin - origin only marginally sig....!

#test for one trait, one df, specify non default family
lfcountLR<- CGtrait.LR.int("LfCount1",al, family=poisson)

#test for one trait, one df
shootmod <- CGtrait.models.int("ShootMass.gA", al) #test one trait

#for all traits in a df
#make sure all traits analyzed this way are the same distribution
names(al)#find col numbers for traits of interestes
alLR <- lapply(names(al)[8:13],function(n) CGtrait.LR.int(n,al))#apply func to all things in list
names(alLR) <- names(al)[8:13]
almodels <- lapply(names(al)[8:13],function(n) CGtrait.models.int(n,al))#apply func to all things in list
names(almodels) <- names(al)[8:13]

#to get one model
almodels[[1]][1] #first number is trait in column order of df, second number is model number
names(almodels[1]) #to verify trait

######Allo, Origin * Lat models######
al<-read.table("STAllosubset.txt", header=T, sep="\t", quote='"', row.names=1) #allosubset
head(al)
alLR <- lapply(names(al)[c(11:13, 20)],function(n) CGtrait.LR.int(n,al)) #crow, shoot, root, root.log, all gaussian
names(alLR) <- names(al)[c(11:13, 20)]
alLR #check out LRs of models. Model progression logical?
almodels <- CGtrait.models.int("CrownDiam.mmA",al)
almodels

###allo shoot, mom is sig, do by hand
modeldata<-al[!is.na(al$ShootMass.gA),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

# modelRack<-lmer(ShootMass.gA  ~ Origin* Latitude +(1|PopID/Mom)+ (1|Rack), family=gaussian,data=modeldata)
modelobar<-lmer(ShootMass.gA  ~ Origin* Latitude +(Origin|PopID/Mom), family=gaussian,data=modeldata)
model1raw<-lmer(ShootMass.gA  ~ Origin* Latitude +(1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelobar, model1raw)
# anova(modelRack, model1raw)
model2raw<-lmer(ShootMass.gA  ~ Origin* Latitude + (1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(ShootMass.gA  ~ Origin* Latitude + (1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom is sig!
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(13.097,1)

modelI <- lmer(ShootMass.gA  ~ Origin + Latitude + (1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelI,model1raw)

modelL<-lmer(ShootMass.gA  ~ Origin + (1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelL, modelI)

modelOraw<-lmer(ShootMass.gA ~ Latitude +(1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelOraw,modelI) #test for significance of origin - origin not sig

lsmeans(modelI, ~ Origin, conf=95)

###allo, root mass###
modeldata<-al[!is.na(al$RootA.log),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

# modelRack<-lmer(RootA.log  ~ Origin * Latitude+(1|PopID/Mom) + (1|Rack), family=gaussian,data=modeldata)
modelobar<-lmer(RootA.log  ~ Origin * Latitude+(Origin|PopID/Mom), family=gaussian,data=modeldata)
model1raw<-lmer(RootA.log  ~ Origin * Latitude+(1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelobar, model1raw)
# anova(modelRack, model1raw)
model2raw<-lmer(RootA.log  ~ Origin * Latitude+ (1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(RootA.log  ~ Origin * Latitude+ (1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom is sig!
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(13.428,1)
modelI <- lmer(RootA.log  ~ Origin + Latitude + (1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelI,model1raw)

modelL<-lmer(RootA.log  ~ Origin + (1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelL, modelI)

modelOraw<-lmer(RootA.log ~ Latitude + (1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelOraw,modelI) #test for significance of origin - origin not sig

CI.LS.poisson(modelI)

modelIRack <- lmer(RootA.log  ~ Origin + Latitude + (1|PopID/Mom)+ (1|Rack), family=gaussian,data=modeldata)
anova(modelIRack, modelI)
# modelORack<-lmer(RootA.log ~ Latitude + (1|PopID/Mom)+ (1|Rack), family=gaussian,data=modeldata)
# anova(modelIRack,modelORack)


###allo, crown###
modeldata<-al[!is.na(al$CrownDiam.mmA),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

modelobar <- lmer(CrownDiam.mmA  ~ Origin * Latitude+(Origin|PopID/Mom), family=gaussian,data=modeldata)
model1raw<-lmer(CrownDiam.mmA  ~ Origin * Latitude+(1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelobar, model1raw)
model2raw<-lmer(CrownDiam.mmA  ~ Origin * Latitude+ (1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(CrownDiam.mmA  ~ Origin * Latitude+ (1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom is sig!
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(18.974,1)
#mom marginally sig
# modelI <- lmer(CrownDiam.mmA  ~ Origin + Latitude + (1|PopID/Mom), family=gaussian,data=modeldata)
# anova(modelI,model1raw)
# 
# modelL<-lmer(CrownDiam.mmA  ~ Origin + (1|PopID/Mom), family=gaussian,data=modeldata)
# anova(modelL, modelI)
# 
# modelOraw<-lmer(CrownDiam.mmA ~ Latitude + (1|PopID/Mom), family=gaussian,data=modeldata)
# anova(modelOraw,modelI)


modelI <- lmer(CrownDiam.mmA  ~ Origin + Latitude + (1|PopID), family=gaussian,data=modeldata)
modelOraw<-lmer(CrownDiam.mmA ~ Latitude + (1|PopID), family=gaussian,data=modeldata)
anova(modelOraw,modelI)

lsmeans(modelI, ~Origin, conf=95)

#####m1, Origin * Lat#####
m1<-read.table("STm1subset.txt", header=T, sep="\t", quote='"', row.names=1) #m1subset
head(m1)
# m1<-cbind(m1,lxw=m1$LfLgth1*m1$LfWdth1, lxw.log=log(m1$LfLgth1*m1$LfWdth1))
m1lxw <- CGtrait.LR.int("lxw", m1)
m1lf <- CGtrait.LR.int("LfCount1", m1, family=poisson)#poisson distribution
m1lfmodels <- CGtrait.models.int("LfCount1", m1, family=poisson)

###m1, lxw, mom sig, do by hand###
# m1$lxw <- m1$LfLgth1*m1$LfWdth1
modeldata<-m1[!is.na(m1$lxw),]
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
#mom and popID sig, but not Origin! for either log or raw data

lsmeans(modelL, ~ Origin, conf=95)

####m1, control, lf count####
modeldata<-m1[!is.na(m1$LfCount1),]
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
# dchisq(X,df) #x is chi sq from table, df is df from table (0) + 1
(lambda <- (-2)*(-514.56 - (-434.10)))
1-pchisq(160.92,1)

modelI <- lmer(LfCount1  ~ Origin + Latitude + (1|PopID/Mom), family=poisson,data=modeldata)
anova(modelI,model1raw)

modelL<-lmer(LfCount1 ~ Origin +(1|PopID/Mom), family=poisson,data=modeldata)
anova(modelL, modelI)

modelOraw<-lmer(LfCount1 ~ Latitude+(1|PopID/Mom), family=poisson,data=modeldata)
anova(modelOraw,modelI) #test for significance of origin - origin NOT sig....!

model1raw
CI.LS.poisson(model1raw)
CI.LS.poisson(modelI)

qplot(data=modeldata,Latitude, LfCount1, color = Origin)
moddata <- ddply(modeldata, .(PopID, Origin, Latitude), summarize, popLf=length(LfCount1), poplfavg=mean(LfCount1))
moddata
qplot(data=moddata,Latitude, poplfavg, color = Origin) +geom_smooth(method=glm, se=TRUE)
qplot(data=moddata[moddata$Latitude<50,],Latitude, poplfavg, color = Origin) +geom_smooth(method=lm, se=FALSE)

####Control, Origin * Lat####
co<-read.table("STControlsubset.txt", header=T, sep="\t", quote='"', row.names=1) #controlsubset
head(co)
coLR <- lapply(names(co)[c(15:17,52:53)],function(n) CGtrait.LR.int(n,co)) #crow, shoot, root, RootH.log, lxw, all gaussian
names(coLR) <- names(co)[c(15:17,52:53)]
coLR #check out LRs of models. Model progression logical?
1-pchisq(31.753,1)
# co <- cbind(co, bolt.bin=as.numeric(co$BoltedatH)-1)
# write.table(co, file="STControlsubset.txt", sep="\t", quote=F)
coBatH <- CGtrait.LR.int("bolt.bin", co, family=binomial)
coP <- lapply(names(co)[c(10,27)],function(n) CGtrait.LR.int(n,co, family=poisson)) #lfcountH, boltdate, all poisson

comodels <- CGtrait.models.int("RootH.log",co)
comodels

#pop control means to look at stress treatments
se <- function(x) sqrt(var(x)/length(x))
comeans<- ddply(co, .(PopID, Origin, Latitude), summarize, CtrlPopCount=length(PopID), CtrlPopLf=mean(LfCountH), CtrlPopLfSE=se(LfCountH),
                CtrlPopShoot=mean(ShootMass.g), CtrlPopShootSE=se(ShootMass.g))

####control, lxw, mom sig, so do by hand####
modeldata<-co[!is.na(co$lxwH),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

modelobar <- lmer(lxwH ~ Origin *Latitude +(Origin|PopID/Mom), family=gaussian,data=modeldata)
model1raw<-lmer(lxwH ~ Origin *Latitude +(1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelobar, model1raw)
model2raw<-lmer(lxwH ~ Origin *Latitude +(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(lxwH ~ Origin *Latitude +(1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(3.8178,1)
modelI <- lmer(lxwH ~ Origin +Latitude +(1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelI, model1raw)

modelL<-lmer(lxwH ~ Origin +(1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelL, modelI)

modelOraw<-lmer(lxwH ~ Latitude +(1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelOraw,modelI) #test for significance of origin - origin NOT sig....!

modelOraw
lsmeans(modelI, ~ Origin, conf=95)
# with obar(barely non-sig, doesn't change anything except makes PopID sig)
# model2<-lmer(lxwH ~ Origin *Latitude +(Origin|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
# model3<-lmer(lxwH ~ Origin *Latitude +(Origin|blank), family=gaussian,data=modeldata) # Test population effect
# anova(model2,modelobar) # mom not sig
# anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
# 1-pchisq(8.173,1)
# modelI <- lmer(lxwH ~ Origin +Latitude +(Origin|PopID/Mom), family=gaussian,data=modeldata)
# anova(modelI, modelobar)
# 
# modelL<-lmer(lxwH ~ Origin +(Origin|PopID/Mom), family=gaussian,data=modeldata)
# anova(modelL, modelI)
# 
# modelO<-lmer(lxwH ~ Latitude +(Origin|PopID/Mom), family=gaussian,data=modeldata)
# anova(modelO,modelI) #test for significance of origin - origin NOT sig....!
# 
# modelOraw



##control, crown
modeldata<-co[!is.na(co$CrownDiam.mm),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

modelobar<-lmer(CrownDiam.mm ~ Origin *Latitude +(Origin|PopID/Mom), family=gaussian,data=modeldata)
model1raw<-lmer(CrownDiam.mm ~ Origin *Latitude +(1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelobar, model1raw)
model2raw<-lmer(CrownDiam.mm ~ Origin *Latitude +(Origin|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(CrownDiam.mm ~ Origin *Latitude +(Origin|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2raw,modelobar) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(3.8178,1)
modelI <- lmer(CrownDiam.mm ~ Origin +Latitude +(Origin|PopID), family=gaussian,data=modeldata)
print(anova(modelI, model2raw), digits=22)
(lambda <- (-2)*(-681.7873280911552456018 - (-681.8026479629145342187)))
1-pchisq(-0.03063974,1)
modelL<-lmer(CrownDiam.mm ~ Origin +(Origin|PopID), family=gaussian,data=modeldata)
anova(modelL, modelI)

modelOraw<-lmer(CrownDiam.mm ~ Latitude +(Origin|PopID), family=gaussian,data=modeldata)
print(anova(modelOraw,modelI), digits = 22)
(lambda <- (-2)*(-681.7698599890596824480 - (-681.7873280911552456018)))
1-pchisq(-0.0349362,1)

modelOraw
lsmeans(modelI, ~Origin, conf=95)

##control, shoot
modeldata<-co[!is.na(co$ShootMass.g),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

modelobar<-lmer(ShootMass.g ~ Origin *Latitude +(Origin|PopID/Mom), family=gaussian,data=modeldata)
model1raw<-lmer(ShootMass.g ~ Origin *Latitude +(1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelobar, model1raw)
model2raw<-lmer(ShootMass.g ~ Origin *Latitude +(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(ShootMass.g ~ Origin *Latitude +(1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2raw,modelobar) # mom not sig
anova(model3raw,model2raw)
modelI <- lmer(ShootMass.g ~ Origin +Latitude +(1|PopID), family=gaussian,data=modeldata)
anova(model)
modelL<-lmer(ShootMass.g ~ Origin +(1|PopID), family=gaussian,data=modeldata)
anova(modelL, modelI)

modelOraw<-lmer(ShootMass.g ~ Latitude +(1|PopID), family=gaussian,data=modeldata)
anova(modelOraw,modelI)

lsmeans(modelI, ~ Origin, conf=95)

##control, root
modeldata<-co[!is.na(co$RootH.log),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

modelobar<-lmer(RootH.log ~ Origin *Latitude +(Origin|PopID/Mom), family=gaussian,data=modeldata)
model1raw<-lmer(RootH.log ~ Origin *Latitude +(1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelobar, model1raw)
model2raw<-lmer(RootH.log ~ Origin *Latitude +(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(RootH.log ~ Origin *Latitude +(1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2raw,modelobar) # mom not sig
anova(model3raw,model2raw)
modelI <- lmer(RootH.log ~ Origin +Latitude +(1|PopID), family=gaussian,data=modeldata)
modelL<-lmer(RootH.log ~ Origin +(1|PopID), family=gaussian,data=modeldata)
anova(modelL, modelI)

modelOraw<-lmer(RootH.log ~ Latitude +(1|PopID), family=gaussian,data=modeldata)
anova(modelOraw,modelI)

CI.LS.poisson(modelI)

####control, lf count, mom sig so do by hand#####
#poisson on raw data
modeldata<-co[!is.na(co$LfCountH),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

modelobar<-lmer(LfCountH ~ Origin *Latitude +(Origin|PopID/Mom), family=poisson,data=modeldata)
model1raw<-lmer(LfCountH ~ Origin *Latitude +(1|PopID/Mom), family=poisson,data=modeldata)
anova(modelobar, model1raw)
model2raw<-lmer(LfCountH ~Origin *Latitude +(Origin|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(LfCountH ~ Origin *Latitude +(Origin|blank), family=poisson,data=modeldata) # Test population effect
anova(model2raw,modelobar) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(66.218,1)
modelI <-lmer(LfCountH ~ Origin +Latitude +(Origin|PopID/Mom), family=poisson,data=modeldata) 
anova(modelI,modelobar)

modelL<-lmer(LfCountH ~ Origin +(Origin|PopID/Mom), family=poisson,data=modeldata)
anova(modelL, modelI)

modelOraw<-lmer(LfCountH ~Latitude+(Origin|PopID/Mom), family=poisson,data=modeldata)
anova(modelOraw,modelI) #test for significance of origin - origin NOT sig....!

modelI
CI.poisson(modelI)
CI.poisson(modelL)
CI.LS.poisson(modelI)
#effect size, poisson
int<-2.766845#inv mean
B<-2.549578#Originnat estimate from model summary
pI<-exp(int)
pN<-exp(int+B)
pI
pN

#tested nested range
# model1raw<-lmer(LfCountH ~ Origin *Latitude +(1|Origin/PopID/Mom), family=poisson,data=modeldata)
# model2raw<-lmer(LfCountH ~Origin *Latitude +(1|Origin/PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
# model3raw<-lmer(LfCountH ~ Origin *Latitude +(1|Origin/blank), family=poisson,data=modeldata) # Test population effect
# anova(model2raw,model1raw) # mom not sig
# anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
# 1-pchisq(60.911,1)
# modelI <-lmer(LfCountH ~ Origin +Latitude +(1|Origin/PopID/Mom), family=poisson,data=modeldata) 
# anova(modelI,model1raw)
# 
# modelL<-lmer(LfCountH ~ Origin +(1|Origin/PopID/Mom), family=poisson,data=modeldata)
# anova(modelL, modelI)
# 
# modelOraw<-lmer(LfCountH ~Latitude+(1|Origin/PopID/Mom), family=poisson,data=modeldata)
# anova(modelOraw,modelI) 
# 
# modelIo <-lmer(LfCountH ~ Origin +Latitude +(1|PopID/Mom), family=poisson,data=modeldata) 
# anova(modelI,modelIo)
qplot(data=modeldata,Latitude, LfCountH, color = Origin)
moddata <- ddply(modeldata, .(PopID, Origin, Latitude), summarize, popCount=length(PopID), poplfavg=mean(LfCountH), SE= se(LfCountH))
moddata
qplot(data=moddata, poplfavg, SE, color = Origin) +geom_smooth(method=lm, se=FALSE)

###control, boltdate, mom sig, do by hand###
#only bolters
modeldata<-co[!is.na(co$BoltDate),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

modelobar<-lmer(BoltDate ~ Origin * Latitude +(Origin|PopID/Mom), family=poisson,data=modeldata)
model1<-lmer(BoltDate ~ Origin * Latitude +(1|PopID/Mom), family=poisson,data=modeldata)
anova(modelobar, model1)
model2<-lmer(BoltDate ~ Origin * Latitude +(1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(BoltDate ~ Origin * Latitude +(1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2,model1) # mom sig
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(154.74,1)
modelI<-lmer(BoltDate ~ Origin + Latitude +(1|PopID/Mom), family=poisson,data=modeldata)
anova(modelI, model1)

modelL<-lmer(BoltDate ~ Origin + (1|PopID/Mom), family=poisson,data=modeldata)
anova(modelL, modelI)

modelO<-lmer(BoltDate ~  (1|PopID/Mom), family=poisson,data=modeldata)
anova(modelO,modelL) #test for significance of origin - origin not sig!

CI.LS.poisson(modelL)

###control boltedatH, mom sig, do by hand, binomial
#all plants, not just bolters
modeldata<-co[!is.na(co$BoltedatH),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

modelobar<-lmer(bolt.bin ~ Origin * Latitude +(Origin|PopID/Mom), family=binomial,data=modeldata)
model1<-lmer(bolt.bin ~ Origin * Latitude +(1|PopID/Mom), family=binomial,data=modeldata)
anova(modelobar, model1)
model2<-lmer(bolt.bin ~ Origin * Latitude +(1|PopID), family=binomial,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(bolt.bin ~ Origin * Latitude +(1|blank), family=binomial,data=modeldata) # Test population effect
anova(model2,model1) # mom sig
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(0.5715,1)
modelI<-lmer(bolt.bin ~ Origin + Latitude +(1|PopID/Mom), family=binomial,data=modeldata)
anova(modelI, model1)

modelL<-lmer(bolt.bin ~ Origin + (1|PopID/Mom), family=binomial,data=modeldata)
anova(modelL, modelI) #are lat and origin different???? sig so yes?
modelL

modelO<-lmer(bolt.bin ~ Latitude + (1|PopID/Mom), family=binomial,data=modeldata)
anova(modelO,modelI) #test for significance of origin??? origin sig!
model1

# interaction.plot(response = modeldata$BoltedatH, x.factor = modeldata$Latitude, trace.factor = modeldata$Origin)
qplot(data=modeldata,Latitude, BoltedatH, color = Origin)
moddata <- ddply(modeldata, .(PopID, Origin, Latitude), summarize, popBolt=length(BoltedatH), popBoltavg=mean(bolt.bin))
moddata
qplot(data=moddata,Latitude, popBoltavg, color = Origin) +geom_smooth(method=glm, se=TRUE)

qplot(data=moddata[moddata$Latitude<52.5,],Latitude, popBoltavg, color = Origin) +geom_smooth(method=glm, se=TRUE)


#lsmean estimates
int<- -4.474240 #inv mean
B<--1.150381 #Originnat estimate from model summary
# Native
pN<-exp(B)/(exp(B)+1)
# Introduced (B=0)
pI<-exp(int)/(exp(int)+1)
pI  
pN 

CI.LS.binomial(modelI)#exclude sig int

#mean estimates
int<- -25.6701 #inv mean
B<-32.0353 #Originnat estimate from model summary
# Native
pN<-exp(int+B)/(exp(int+B)+1)
# Introduced (B=0)
pI<-exp(int)/(exp(int)+1)
pI  
pN 

# #try glm
# modelg <- glm(bolt.bin ~ Origin*Latitude, family=binomial,data=modeldata)
# modelg1 <- glm(bolt.bin ~ Origin+Latitude, family=binomial,data=modeldata)
# anova(modelg1, modelg) #'Deviance' is chisq value
# 1-pchisq(7.0643, 1)
# 
# modelg3<- glm(bolt.bin ~ Origin, family=binomial,data=modeldata)
# anova(modelg3,modelg1)
# 1-pchisq(11.109, 1)
# modelg2<- glm(bolt.bin ~ Latitude, family=binomial,data=modeldata)
# anova(modelg2,modelg1)
# 1-pchisq(39.947, 1)
# summary(modelg)
# 
# #mean estimates modelg
# int<- -21.3908 #inv mean
# B<-26.6516 #Originnat estimate from model summary
# # Native
# pN<-exp(int+B)/(exp(int+B)+1)
# # Introduced (B=0)
# pI<-exp(int)/(exp(int)+1)
# pI  
# pN 
# 
# #mean estimates modelL
# int<- -3.9821 #inv mean
# B<-2.8335 #Originnat estimate from model summary
# # Native
# pN<-exp(int+B)/(exp(int+B)+1)
# # Introduced (B=0)
# pI<-exp(int)/(exp(int)+1)
# pI  
# pN

#raw means
summary(modeldata[modeldata$Origin=="inv",]$BoltedatH)
120/125
5/125 #4% bolted
summary(modeldata[modeldata$Origin=="nat",]$BoltedatH)
183+78
183/261
78/261 #30% bolted

plot(modeldata[modeldata$Origin=="inv",]$Latitude, modeldata[modeldata$Origin=="inv",]$BoltedatH)
plot(modeldata[modeldata$Origin=="nat",]$Latitude, modeldata[modeldata$Origin=="nat",]$BoltedatH)

#testing nested range
# model1<-lmer(bolt.bin ~ Origin * Latitude +(1|Origin/PopID/Mom), family=binomial,data=modeldata)
# model2<-lmer(bolt.bin ~ Origin * Latitude +(1|Origin/PopID), family=binomial,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
# model3<-lmer(bolt.bin ~ Origin * Latitude +(1|Origin/blank), family=binomial,data=modeldata) # Test population effect
# anova(model2,model1) # mom sig
# anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
# 1-pchisq(0.5715,1)
# modelI<-lmer(bolt.bin ~ Origin + Latitude +(1|Origin/PopID/Mom), family=binomial,data=modeldata)
# anova(modelI, model1)
# 
# modelL<-lmer(bolt.bin ~ Origin + (1|Origin/PopID/Mom), family=binomial,data=modeldata)
# anova(modelL, modelI) #are lat and origin different???? sig so yes?
# modelL
# 
# modelO<-lmer(bolt.bin ~ Latitude + (1|Origin/PopID/Mom), family=binomial,data=modeldata)
# anova(modelO,modelI)
# 
# model1o<-lmer(bolt.bin ~ Origin * Latitude +(1|PopID/Mom), family=binomial,data=modeldata)
# anova(model1, model1o)

###control, bolt.bin, extra covariates###
modeldata<-co[!is.na(co$BoltedatH),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

model1<-lmer(bolt.bin ~ Origin * Latitude +(1|PopID/Mom), family=binomial,data=modeldata)
modele<-lmer(bolt.bin ~ Origin * Latitude +CrownDiam.mm+(1|PopID/Mom), family=binomial,data=modeldata)
anova(model1, modele)







####Mom, Origin * Lat####
mom<-read.table("STMomsubset.txt", header=T, sep="\t", quote='"', row.names=1) #momsubset
head(mom)
# momLR <- lapply(names(mom)[c(5:6,13, 17)],function(n) CGtrait.LR(n,mom)) #can't use func, because mom doesn't have Mom
#seedwt, germ avg date, sdwt.log (pick one!), avggermdate.log(I don't think this is the right transf...) all gaussian

# names(alLR) <- names(al)[11:13]
# alLR #check out LRs of models. Model progression logical?
# almodels <- CGtrait.models("CrownDiam.mmA",al)
# almodels
###mom, seedwt.log####
str(mom)
modeldata<-mom[!is.na(mom$Sdwt.log),]
# xtabs(~Origin+SeedAgeYrs, modeldata)
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)

model2<-lmer(Sdwt.log ~ Origin *Latitude+(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Sdwt.log ~ Origin *Latitude+(1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(76.54,1)

modelI <- lmer(Sdwt.log ~ Origin +Latitude+(1|PopID), family=gaussian,data=modeldata)
anova(modelI, model2)

modelL<-lmer(Sdwt.log ~ Origin + (1|PopID), family=gaussian, data=modeldata)
anova(modelL, modelI)

modelO<-lmer(Sdwt.log ~ (1|PopID), family=gaussian,data=modeldata)
anova(modelO,modelL) #test for significance of origin - origin not sig....?

CI.LS.poisson(modelL)

####Mom, germ count###
modeldata<-mom[!is.na(mom$GermCount),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)

model2<-lmer(GermCount ~ Origin * Latitude+ SeedCount +(1|PopID), family=poisson, data=modeldata)
model3<-lmer(GermCount ~ Origin * Latitude+SeedCount + (1|blank), family=poisson, data=modeldata)
anova(model3, model2)
1-pchisq(261.46,1)

modelI <- lmer(GermCount ~ Origin + Latitude+ SeedCount +(1|PopID), family=poisson, data=modeldata)
anova(modelI, model2)

modelL<-lmer(GermCount ~ Origin +SeedCount+ (1|PopID), family=poisson, data=modeldata)
anova(modelL, modelI)

modelO<-lmer(GermCount ~ SeedCount + (1|PopID), family=poisson, data=modeldata)
anova(modelO, modelL)

# modelL
# int<- -0.42767
# #inv mean
# B<-0.52217
# #Originnat estimate from model summary
# pI<-exp(int)
# pN<-exp(int+B)
# pI
# pN

CI.LS.poisson(modelL)
8.63508182678789/13.3*100
6.86236222603546/13.3*100
10.8657391870729/13.3*100
5.12261904337083/13.3*100
3.97977801862767/13.3*100
6.59364058514856/13.3*100

####Mom, germ count, with more covariates###
modeldata<-mom[!is.na(mom$GermCount),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
# xtabs(~Origin+SeedAgeYrs, modeldata)

model2<-lmer(GermCount ~ Origin + SeedCount + Latitude+SeedWt +SeedAgeYrs+(SeedWt|PopID), family=poisson, data=modeldata)
model3<-lmer(GermCount ~ Origin + SeedCount + Latitude+SeedWt + SeedAgeYrs+(SeedWt|blank), family=poisson, data=modeldata)
anova(model3, model2)

modelY<-lmer(GermCount ~ Origin + SeedCount + Latitude+ SeedWt + (SeedWt|PopID), family=poisson, data=modeldata)
anova(model2, modelY)

modelY2<-lmer(GermCount ~ Origin + SeedCount+ Latitude + SeedWt + (1|PopID), family=poisson, data=modeldata)
anova(modelY2, modelY)


modelW<-lmer(GermCount ~ Origin + SeedCount+ + Latitude(SeedWt|PopID), family=poisson, data=modeldata)
anova(modelW, modelY)

modelC<-lmer(GermCount ~ Origin + SeedWt + Latitude+(SeedWt|PopID), family=poisson, data=modeldata)
anova(modelC, modelY)

modelL<-lmer(GermCount ~ Origin + SeedCount + SeedWt + (SeedWt|PopID), family=poisson, data=modeldata)
anova(modelL, modelY)

modelO<-lmer(GermCount ~ SeedCount +SeedWt+ (SeedWt|PopID), family=poisson, data=modeldata)
anova(modelO, modelL)

modelY
int<- -1.33561
#inv mean
B<-0.37899
#Originnat estimate from model summary
pI<-exp(int)
pN<-exp(int+B)
pI
pN

####Mom, germ date####avg, so can't use poisson
modeldata<-mom[!is.na(mom$AvgGermDate.log),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)

model2<-lmer(AvgGermDate.log ~ Origin *Latitude+(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(AvgGermDate.log ~ Origin *Latitude+(1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(48.113,1)

modelI <- lmer(AvgGermDate.log ~ Origin +Latitude+(1|PopID), family=gaussian,data=modeldata) 
anova(modelI,model2)

modelL<-lmer(AvgGermDate.log ~ Origin +(1|PopID), family=, data=modeldata)
anova(modelL,model2)

modelO<-lmer(AvgGermDate.log ~ (1|PopID), family=gaussian,data=modeldata)
anova(modelO,modelL) #test for significance of origin - origin not sig....?

CI.LS.poisson(modelL)