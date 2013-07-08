###ST mixed FX models, focused on Origin BY Latitude###
#Stress Tolerance, REML, using lme4
#mixed effect models 
library(lme4)

#for each normal trait, compare this general set of models
model1<-lmer(trait  ~ Origin* Latitude +(1|PopID/Mom), family=gaussian,data=modeldata)
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
qplot(data=moddata,Latitude, poplfavg, color = Origin) +geom_smooth(method=lm, se=FALSE)
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
qplot(data=moddata[moddata$Latitude<50,],Latitude, popBoltavg, color = Origin) +geom_smooth(method=lm, se=FALSE)


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

modelobar<-lmer(LfCountH ~ Origin * Latitude + (Origin|PopID/Mom), family=poisson,data=modeldata)
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

CI.LS.poisson(modelI)

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

###cut, bolt.bin, binomial###
modeldata<-cu[!is.na(cu$BoltedatH),]
xtabs(~Origin + BoltedatH, modeldata)
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
anova(modelg1, modelg) #'Deviance' is chisq value
1-pchisq(14.456,1)
anova(modelg3,modelg1)
1-pchisq(0.0005405,1)
anova(modelg3)
1-pchisq(12.858,1)

# modelg4 <- glm(bolt.bin ~ Latitude*Origin, family=binomial,data=modeldata)
# modelg5 <- glm(bolt.bin ~ Latitude+Origin, family=binomial,data=modeldata)
# anova(modelg5, modelg4)

CI.LS.binomial(modelg1)

qplot(data=modeldata,Latitude, bolt.bin, color = Origin)
moddata <- ddply(modeldata, .(PopID, Origin, Latitude), summarize, popBolt=length(BoltedatH), popBoltavg=mean(bolt.bin))
moddata
qplot(data=moddata,Latitude, popBoltavg, color = Origin) +geom_smooth(method=lm, se=FALSE)
qplot(data=moddata[moddata$Latitude<50,],Latitude, popBoltavg, color = Origin) +geom_smooth(method=lm, se=FALSE)

###cut, harvest, bolt date, mom is sig, do by hand###
#given that it's bolted....
modeldata<-cu[!is.na(cu$BoltDate),]
xtabs(~Origin + BoltedatH, modeldata)
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
anova(modelg1, modelg) #'Deviance' is chisq value
1-pchisq(chisq, df)

modelg3<- glm(Death ~ Origin, family=poisson,data=modeldata)
anova(modelg3,modelg1)
1-pchisq(5.5154, 1)
modelg2<- glm(Death ~ Latitude, family=poisson,data=modeldata)
anova(modelg2,modelg1)
1-pchisq(9.0533, 1)

CI.LS.poisson(modelg1)

#extra covariate
modeldata <- modeldata[!is.na(modeldata$lxw),]
modelg1 <- glm(Death ~ Origin+Latitude, family=poisson,data=modeldata)
modelgS <- glm(Death ~ Origin+Latitude+lxw, family=poisson,data=modeldata)
anova(modelg1, modelgS)
1-pchisq(2.9445, 1)
# modelgO <- glm(Death ~ Latitude+lxw, family=poisson,data=modeldata)
# anova(modelgS, modelgO)


#drought, wilt, extra covariates#
modeldata<-d[!is.na(d$Wilt),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

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

model1<-lmer(Wilt ~ Origin * Latitude+lxw+(1|PopID), family=poisson,data=modeldata)
model2<-lmer(Wilt ~ Origin + Latitude+lxw+(1|PopID), family=poisson,data=modeldata)
model3<-lmer(Wilt ~ Origin +lxw+(1|PopID), family=poisson,data=modeldata)
model4<-lmer(Wilt ~ Origin * lxw+(1|PopID), family=poisson,data=modeldata)
anova(model1, model2)
anova(model2, model3)
anova(model4, model3)
model5<-lmer(Wilt ~ Origin +(1|PopID), family=poisson,data=modeldata)
model6<-lmer(Wilt ~ lxw+(1|PopID), family=poisson,data=modeldata)
anova(model3,model5)
anova(model3, model6)

#drought, total wilt
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
modelI<-lmer(TotWilt ~ Origin + Latitude +(1|blank), family=poisson,data=modeldata)
anova(modelI,model3) #test for significant interaction btw Origin and Bolted - not sig

modelL <- lmer(TotWilt ~ Origin +(1|blank), family=poisson,data=modeldata)
anova(modelL, modelI)
modelO<-lmer(TotWilt ~ Latitude+(1|blank), family=poisson,data=modeldata)
anova(modelO,modelI) #test for significance of origin - origin not sig!
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
anova(modelg1, modelg) #'Deviance' is chisq value
1-pchisq(chisq, df)

modelg3<- glm(TotWilt ~ Origin, family=poisson,data=modeldata)
anova(modelg3,modelg1)
1-pchisq(5.5154, 1)
modelg2<- glm(TotWilt ~ Latitude, family=poisson,data=modeldata)
anova(modelg2,modelg1)
1-pchisq(9.0533, 1)

CI.LS.poisson(modelg1)

#extra covariates
modeldata <- modeldata[!is.na(modeldata$lxw),]
modelg1 <- glm(TotWilt ~ Origin+Latitude, family=poisson,data=modeldata)
modelgS <- glm(TotWilt ~ Origin+Latitude+lxw, family=poisson,data=modeldata)
anova(modelg1, modelgS)
modelgO <- glm(TotWilt ~ Latitude+lxw, family=poisson,data=modeldata)
anova(modelgS, modelgO)

#drought, wilt
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
anova(modelg1, modelg) #'Deviance' is chisq value
1-pchisq(chisq, df)

modelg3<- glm(Wilt ~ Origin, family=poisson,data=modeldata)
anova(modelg3,modelg1)
1-pchisq(5.5154, 1)
modelg2<- glm(Wilt ~ Latitude, family=poisson,data=modeldata)
anova(modelg2,modelg1)
1-pchisq(9.0533, 1)

CI.LS.poisson(modelg1)

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

modelobar<-lmer(FloatDate ~ Origin * Latitude +(Origin|PopID/Mom), family=poisson,data=modeldata)
model1<-lmer(FloatDate ~ Origin * Latitude +(1|PopID/Mom), family=poisson,data=modeldata)
anova(modelobar, model1)
model2<-lmer(FloatDate ~ Origin * Latitude +(1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(FloatDate ~ Origin * Latitude +(1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2,model1) # mom sig
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelI<-lmer(FloatDate ~ Origin + Latitude +(1|blank), family=poisson,data=modeldata)
anova(modelI,model3) #test for significant interaction btw Origin and Bolted - not sig

modelL <- lmer(FloatDate ~ Origin + (1|blank), family=poisson,data=modeldata)
anova(modelL, modelI)

modelO<-lmer(FloatDate ~ Latitude + (1|blank), family=poisson,data=modeldata)
anova(modelO,modelI) #test for significance of origin - origin not sig

#try glm
modelg <- glm(FloatDate ~ Origin*Latitude, family=poisson,data=modeldata)
modelg1 <- glm(FloatDate ~ Origin+Latitude, family=poisson,data=modeldata)
anova(modelg1, modelg) #'Deviance' is chisq value
1-pchisq(chisq, df)

modelg3<- glm(FloatDate ~ Origin, family=poisson,data=modeldata)
anova(modelg3,modelg1)
1-pchisq(5.5154, 1)
modelg2<- glm(FloatDate ~ Latitude, family=poisson,data=modeldata)
anova(modelg2,modelg1)
1-pchisq(9.0533, 1)

CI.LS.poisson(modelg1)

####Flood, yellow####
modeldata<-f[!is.na(f$Yellow),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

modelobar<-lmer(Yellow ~ Origin * Latitude +(Origin|PopID/Mom), family=poisson,data=modeldata)
model1<-lmer(Yellow ~ Origin * Latitude +(1|PopID/Mom), family=poisson,data=modeldata)
print(anova(modelobar, model1), digits = 22)
(lambda <- (-2)*(-17.81004578325275033990 - (-17.81004578681382355398)))
1-pchisq(7.122146e-09,4)

model2<-lmer(Yellow ~ Origin * Latitude +(1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Yellow ~ Origin * Latitude +(1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2,model1) # mom sig
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelI<-lmer(Yellow ~ Origin + Latitude +(1|blank), family=poisson,data=modeldata)
anova(modelI,model3) #test for significant interaction btw Origin and Bolted - not sig

modelL <- lmer(Yellow ~ Origin + (1|blank), family=poisson,data=modeldata)
anova(modelL, modelI)

modelO<-lmer(Yellow ~ (1|blank), family=poisson,data=modeldata)
anova(modelO,modelL) #test for significance of origin - origin not sig

#try glm
modelg <- glm(Yellow ~ Origin*Latitude, family=poisson,data=modeldata)
modelg1 <- glm(Yellow ~ Origin+Latitude, family=poisson,data=modeldata)
anova(modelg1, modelg) #'Deviance' is chisq value
1-pchisq(chisq, df)

modelg3<- glm(Yellow ~ Origin, family=poisson,data=modeldata)
anova(modelg3,modelg1)
1-pchisq(5.5154, 1)
modelg2<- glm(Yellow ~ Latitude, family=poisson,data=modeldata)
anova(modelg2,modelg1)
1-pchisq(9.0533, 1)

CI.LS.poisson(modelg3)

##flood, death
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