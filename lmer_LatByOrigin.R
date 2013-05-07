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
modeldata<-al[!is.na(al$ShootA.log),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

model1raw<-lmer(ShootMass.gA  ~ Origin* Latitude +(1|PopID/Mom), family=gaussian,data=modeldata)
model2raw<-lmer(ShootMass.gA  ~ Origin* Latitude + (1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(ShootMass.gA  ~ Origin* Latitude + (1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom is sig!
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelI <- lmer(ShootMass.gA  ~ Origin + Latitude + (1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelI,model1raw)

# modelL<-lmer(ShootMass.gA  ~ Origin + (1|PopID/Mom), family=gaussian,data=modeldata)
# anova(modelL, model1raw)
# 
# modelOraw<-lmer(ShootMass.gA ~ Latitude +(1|PopID/Mom), family=gaussian,data=modeldata)
# anova(modelOraw,model1raw) #test for significance of origin - origin not sig

###allo, root mass###
modeldata<-al[!is.na(al$RootA.log),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

model1raw<-lmer(RootA.log  ~ Origin * Latitude+(1|PopID/Mom), family=gaussian,data=modeldata)
model2raw<-lmer(RootA.log  ~ Origin * Latitude+ (1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(RootA.log  ~ Origin * Latitude+ (1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom is sig!
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelI <- lmer(RootA.log  ~ Origin + Latitude + (1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelI,model1raw)

# modelL<-lmer(RootA.log  ~ Origin + (1|PopID/Mom), family=gaussian,data=modeldata)
# anova(modelL, model1raw)
# 
# modelOraw<-lmer(RootA.log ~ Latitude + (1|PopID/Mom), family=gaussian,data=modeldata)
# anova(modelOraw,model1raw) #test for significance of origin - origin not sig

#####m1, Origin * Lat#####
m1<-read.table("STm1subset.txt", header=T, sep="\t", quote='"', row.names=1) #m1subset
head(m1)
# m1<-cbind(m1,lxw=m1$LfLgth1*m1$LfWdth1, lxw.log=log(m1$LfLgth1*m1$LfWdth1))
m1lxw <- CGtrait.LR.int("lxw", m1)
m1lf <- CGtrait.LR.int("LfCount1", m1, family=poisson)#poisson distribution
m1lfmodels <- CGtrait.models.int("LfCount1", m1, family=poisson)

###m1, lxw, mom sig, do by hand###
modeldata<-m1[!is.na(m1$lxw),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

model1<-lmer(lxw ~ Origin *Latitude+(1|PopID/Mom), family=gaussian,data=modeldata)
model2<-lmer(lxw ~ Origin *Latitude+ (1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(lxw ~ Origin *Latitude+ (1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2,model1) # mom is sig!
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelI <- lmer(lxw  ~ Origin + Latitude + (1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelI,model1)

# modelL<-lmer(lxw ~ Origin +(1|PopID/Mom), family=gaussian,data=modeldata)
# anova(modelL, model1)
# 
# modelO<-lmer(lxw ~ (1|PopID/Mom), family=gaussian,data=modeldata)
# anova(modelO,modelL) #test for significance of origin - origin not sig....?
# #mom and popID sig, but not Origin! for either log or raw data

####m1, control, lf count####
modeldata<-m1[!is.na(m1$LfCount1),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

model1raw<-lmer(LfCount1 ~ Origin *Latitude +(1|PopID/Mom), family=poisson,data=modeldata)
model2raw<-lmer(LfCount1 ~ Origin *Latitude+(1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(LfCount1 ~ Origin *Latitude+(1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
dchisq(X,df) #x is chi sq from table, df is df from table (0) + 1
dchisq(160.9,1)

modelI <- lmer(LfCount1  ~ Origin + Latitude + (1|PopID/Mom), family=poisson,data=modeldata)
anova(modelI,model1raw)

modelL<-lmer(LfCount1 ~ Origin +(1|PopID/Mom), family=poisson,data=modeldata)
anova(modelL, modelI)

modelOraw<-lmer(LfCount1 ~ Latitude+(1|PopID/Mom), family=poisson,data=modeldata)
anova(modelOraw,model1raw) #test for significance of origin - origin NOT sig....!
model1raw
int<-0.29109 + 0.04405- (0.50455+0.01127)*1.96 #inv mean
B<- 1.51023 -0.03324 #Originnat estimate from model summary
pI<-exp(int)
pN<-exp(int+B)
pI
pN

####Control, Origin * Lat####
co<-read.table("STControlsubset.txt", header=T, sep="\t", quote='"', row.names=1) #controlsubset
head(co)
coLR <- lapply(names(co)[c(15:17,52:53)],function(n) CGtrait.LR.int(n,co)) #crow, shoot, root, RootH.log, lxw, all gaussian
names(coLR) <- names(co)[c(15:17,52:53)]
coLR #check out LRs of models. Model progression logical?

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

model1raw<-lmer(lxwH ~ Origin *Latitude +(1|PopID/Mom), family=gaussian,data=modeldata)
model2raw<-lmer(lxwH ~ Origin *Latitude +(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(lxwH ~ Origin *Latitude +(1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelI <- lmer(lxwH ~ Origin +Latitude +(1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelI, model1raw)

modelL<-lmer(lxwH ~ Origin +(1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelL, model1raw)

modelOraw<-lmer(lxwH ~ Latitude +(1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelOraw,modelL) #test for significance of origin - origin NOT sig....!

####control, lf count, mom sig so do by hand#####
#poisson on raw data
modeldata<-co[!is.na(co$LfCountH),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

model1raw<-lmer(LfCountH ~ Origin *Latitude +(1|PopID/Mom), family=poisson,data=modeldata)
model2raw<-lmer(LfCountH ~Origin *Latitude +(1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(LfCountH ~ Origin *Latitude +(1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
dchisq(60.911,1)
modelI <-lmer(LfCountH ~ Origin +Latitude +(1|PopID/Mom), family=poisson,data=modeldata) 
anova(modelI,model1raw)

modelL<-lmer(LfCountH ~ Origin +(1|PopID/Mom), family=poisson,data=modeldata)
anova(modelL, modelI)

modelOraw<-lmer(LfCountH ~Latitude+(1|PopID/Mom), family=poisson,data=modeldata)
anova(modelOraw,modelI) #test for significance of origin - origin NOT sig....!
anova()
###control, boltdate, mom sig, do by hand###
#only bolters
modeldata<-co[!is.na(co$BoltDate),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

model1<-lmer(BoltDate ~ Origin * Latitude +(1|PopID/Mom), family=poisson,data=modeldata)
model2<-lmer(BoltDate ~ Origin * Latitude +(1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(BoltDate ~ Origin * Latitude +(1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2,model1) # mom sig
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelI<-lmer(BoltDate ~ Origin + Latitude +(1|PopID/Mom), family=poisson,data=modeldata)
anova(modelI, model1)

modelL<-lmer(BoltDate ~ Origin + (1|PopID/Mom), family=poisson,data=modeldata)
anova(modelL, modelI)

modelO<-lmer(BoltDate ~  (1|PopID/Mom), family=poisson,data=modeldata)
anova(modelO,modelL) #test for significance of origin - origin not sig!

###control boltedatH, mom sig, do by hand, binomial
#all plants, not just bolters
modeldata<-co[!is.na(co$BoltedatH),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

model1<-lmer(bolt.bin ~ Origin * Latitude +(1|PopID/Mom), family=binomial,data=modeldata)
model2<-lmer(bolt.bin ~ Origin * Latitude +(1|PopID), family=binomial,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(bolt.bin ~ Origin * Latitude +(1|blank), family=binomial,data=modeldata) # Test population effect
anova(model2,model1) # mom sig
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
dchisq(0.5715,1)
modelI<-lmer(bolt.bin ~ Origin + Latitude +(1|PopID/Mom), family=binomial,data=modeldata)
anova(modelI, model1)

modelL<-lmer(bolt.bin ~ Origin + (1|PopID/Mom), family=binomial,data=modeldata)
anova(modelL, modelI) #are lat and origin different???? sig so yes?

modelO<-lmer(bolt.bin ~ Latitude + (1|PopID/Mom), family=binomial,data=modeldata)
anova(modelO,modelI) #test for significance of origin??? origin sig!
model1
#mean estimates
int<- -25.6701 #inv mean
B<-32.0353 #Originnat estimate from model summary
# Native
pN<-exp(int+B)/(exp(int+B)+1)
# Introduced (B=0)
pI<-exp(int)/(exp(int)+1)
pI  
pN 

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

model1raw<-lmer(LfCountH ~ Origin * Latitude + (1|PopID/Mom), family=poisson,data=modeldata)
model2raw<-lmer(LfCountH ~ Origin * Latitude +(1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(LfCountH ~ Origin * Latitude +(1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelI <- lmer(LfCountH ~ Origin + Latitude + (1|PopID/Mom), family=poisson,data=modeldata)
anova(modelI, model1raw)

modelL <- lmer(LfCountH ~ Origin +(1|PopID/Mom), family=poisson,data=modeldata)
anova(modelL, model1raw)

modelOraw<-lmer(LfCountH ~ Latitude +(1|PopID/Mom), family=poisson,data=modeldata)
anova(modelOraw,model1raw) #test for significance of origin - origin NOT sig....!

##nut def, shoot.log##
modeldata<-n[!is.na(n$ShootH.log),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

model1raw<-lmer(ShootH.log ~ Origin * Latitude + (1|PopID/Mom), family=gaussian,data=modeldata)
model2raw<-lmer(ShootH.log ~ Origin * Latitude +(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(ShootH.log ~ Origin * Latitude +(1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelI <- lmer(ShootH.log ~ Origin + Latitude + (1|PopID), family=gaussian,data=modeldata)
anova(modelI, model2raw)

modelL <- lmer(ShootH.log ~ Origin +(1|PopID), family=gaussian,data=modeldata)
anova(modelL, modelI)

modelOraw<-lmer(ShootH.log ~ Latitude +(1|PopID), family=gaussian,data=modeldata)
anova(modelOraw,modelI)

modelO<-lmer(ShootH.log ~ (1|PopID), family=gaussian,data=modeldata)
anova(modelO,modelL)

####Cut, Origin * Lat####
cu<-read.table("STCutsubset.txt", header=T, sep="\t", quote='"', row.names=1) #cutsubset
head(cu)
cuLR <- lapply(names(cu)[c(13:14, 42)],function(n) CGtrait.LR.int(n,cu)) #crow, root, root.log, all gaussian
dchisq(13.036,1)
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

model1raw<-lmer(RootH.log ~ Origin * Latitude + (1|PopID/Mom), family=gaussian,data=modeldata)
model2raw<-lmer(RootH.log ~ Origin * Latitude + (1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(RootH.log ~ Origin * Latitude + (1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
dchisq(13.036,1)
modelI <- lmer(RootH.log ~ Origin + Latitude + (1|PopID), family=gaussian,data=modeldata)
anova(modelI,model1raw)

modelO <- lmer(RootH.log ~ Origin +(1|PopID), family=gaussian,data=modeldata)
anova(modelO,modelI) 

modelL <- lmer(RootH.log ~ Latitude +(1|PopID), family=gaussian,data=modeldata)
anova(modelL,modelI) 

####cut, lf count, harvest, mom is sig, do by hand###
modeldata<-cu[!is.na(cu$LfCountH),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

model1raw<-lmer(LfCountH ~ Origin * Latitude + (1|PopID/Mom), family=poisson,data=modeldata)
model2raw<-lmer(LfCountH ~ Origin * Latitude + (1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(LfCountH ~ Origin * Latitude + (1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelI <- lmer(LfCountH ~ Origin + Latitude + (1|PopID/Mom), family=poisson,data=modeldata)
anova(modelI,model1raw)

# modelL <- lmer(LfCountH ~ Origin +(1|PopID/Mom), family=poisson,data=modeldata)
# anova(modelL,model1raw) 
# 
# modelOraw<-lmer(LfCountH ~ Latitude + (1|PopID/Mom), family=poisson,data=modeldata)
# anova(modelOraw,model1raw) #test for significance of origin 

###cut, bolt.bin, binomial###
modeldata<-cu[!is.na(cu$BoltedatH),]
xtabs(~Origin + BoltedatH, modeldata)
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

model1<-lmer(bolt.bin ~ Origin *Latitude +(1|PopID/Mom), family=binomial,data=modeldata)
model2<-lmer(bolt.bin ~ Origin *Latitude +(1|PopID), family=binomial,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(bolt.bin ~ Origin *Latitude +(1|blank), family=binomial,data=modeldata) # Test population effect
anova(model2,model1) # mom sig
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
print(anova(model3,model2), digits=23)
dchisq(0,1)
modelI <- lmer(bolt.bin ~ Origin +Latitude +(1|blank), family=binomial,data=modeldata)
anova(modelI, model3)

modelL <- lmer(bolt.bin ~ Origin +(1|blank), family=binomial,data=modeldata)
anova(modelL, model3)
anova(modelI, modelL)
modelL

(lambda <- (-2)*(-199.54 - (-191.60)))
dchisq(lambda,1)

modelO<-lmer(bolt.bin ~ Latitude+(1|blank), family=binomial,data=modeldata)
anova(modelO,model3) #test for significance of origin - origin sig!
anova(modelO, modelI)

modelO2<- lmer(bolt.bin ~ (1|blank), family=binomial,data=modeldata)
anova(modelL, modelO2)

modelP <- lmer(bolt.bin ~ Origin + (1|PopID), family = binomial, data=modeldata)
anova(modelP, modelL)

model3

int<--172.961 #inv mean
B<-173.314 #Originnat estimate from model summary
# Native
pN<-exp(int+B)/(exp(int+B)+1)
# Introduced (B=0)
pI<-exp(int)/(exp(int)+1)

pI # 6.5% 
pN # 27%
#check by looking at percentages
summary(cu[cu$Origin=="nat",]) #146 rows, 39 boltedatH = 27%
summary(cu[cu$Origin=="inv",]) #55 rows, 8 boltedatH = 6.4%
#also check glm
glm(bolt.bin ~ Origin, family=binomial,data=modeldata)

###cut, harvest, bolt date, mom is sig, do by hand###
#given that it's bolted....
modeldata<-cu[!is.na(cu$BoltDate),]
xtabs(~Origin + BoltedatH, modeldata)
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

model1<-lmer(BoltDate ~ Origin *Latitude +(1|PopID/Mom), family=poisson,data=modeldata)
model2<-lmer(BoltDate ~ Origin *Latitude+(1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(BoltDate ~ Origin *Latitude+(1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2,model1) # mom sig
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelI <- lmer(BoltDate ~ Origin +Latitude+(1|PopID/Mom), family=poisson,data=modeldata)
anova(modelI, model1)

# modelL<-lmer(BoltDate ~ Origin + (1|PopID/Mom), family=poisson,data=modeldata)
# anova(modelL,model1) #test for significant interaction btw Origin and Bolted - not sig
# 
# modelO<-lmer(BoltDate ~ Latitude + (1|PopID/Mom), family=poisson,data=modeldata)
# anova(modelO,model1) #test for significance of origin - origin not sig!

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

model1<-lmer(Death ~ Origin * Latitude +(1|PopID/Mom), family=poisson,data=modeldata)
model2<-lmer(Death ~ Origin * Latitude +(1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Death ~ Origin * Latitude +(1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2,model1) # mom sig
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelI<-lmer(Death ~ Origin + Latitude +(1|blank), family=poisson,data=modeldata)
anova(modelI,model3) #test for significant interaction btw Origin and Bolted - not sig

# modelL <- lmer(Death ~ Origin +(1|PopID), family=poisson,data=modeldata)
# 
# modelO<-lmer(Death ~ LfLgth1+(1|PopID), family=poisson,data=modeldata)
# anova(modelO,modelL1) #test for significance of origin - origin not sig!

#drought, wilt, extra covariates#
modeldata<-d[!is.na(d$Wilt),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

model1<-lmer(Wilt ~ Origin * Latitude+lxw+(lxw|PopID/Mom), family=poisson,data=modeldata)
model2<-lmer(Wilt ~ Origin * Latitude+lxw+(lxw|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Wilt ~ Origin * Latitude+lxw+(lxw|blank), family=poisson,data=modeldata) # Test population effect
anova(model2,model1) # mom sig
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelInt<-lmer(Wilt ~ Origin + Latitude+lxw+(lxw|PopID), family=poisson,data=modeldata)
anova(modelInt,model2) #test for significant interaction btw Origin and Bolted - not sig

modelL0<-lmer(Wilt ~ Origin +Latitude+ (1|PopID), family=poisson,data=modeldata)
modelL1<-lmer(Wilt ~ Origin +Latitude+ lxw+(1|PopID), family=poisson,data=modeldata)
modelL2<-lmer(Wilt ~ Origin+Latitude + (lxw|PopID), family=poisson,data=modeldata)
anova(modelL0, modelInt)#test for sig of size, approx by lf length
anova(modelL1, modelInt)#test for sig of size
anova(modelL2, modelInt)#test for sig of size

modelLat<-lmer(Wilt ~ Origin + lxw+(1|PopID), family=poisson,data=modeldata)
anova(modelLat, modelL1)

modelO<-lmer(Wilt ~ lxw+(1|PopID), family=poisson,data=modeldata)
anova(modelO,modelLat) #test for significance of origin - origin  sig!
modelLat
int<-1.693062
#inv mean
B<-0.151491
#Originnat estimate from model summary
pI<-exp(int)
pN<-exp(int+B)
pI
pN
summary(d[d$Origin=="inv",]$Wilt)
summary(d[d$Origin=="nat",]$Wilt)

#drought, total wilt
modeldata<-d[!is.na(d$TotWilt),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

model1<-lmer(TotWilt ~ Origin * Latitude +(1|PopID/Mom), family=poisson,data=modeldata)
model2<-lmer(TotWilt ~ Origin * Latitude +(1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(TotWilt ~ Origin * Latitude +(1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2,model1) # mom sig
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelI<-lmer(TotWilt ~ Origin + Latitude +(1|PopID), family=poisson,data=modeldata)
anova(modelI,model2) #test for significant interaction btw Origin and Bolted - not sig

modelL <- lmer(TotWilt ~ Origin +(1|PopID), family=poisson,data=modeldata)
anova(modelL, modelI)
modelO<-lmer(TotWilt ~ Latitude+(1|PopID), family=poisson,data=modeldata)
anova(modelO,modelI) #test for significance of origin - origin not sig!
modelI
int<-2.591741#inv mean
B<-0.061169#Originnat estimate from model summary
pI<-exp(int)
pN<-exp(int+B)
pI
pN

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

model1<-lmer(FloatDate ~ Origin * Latitude +(1|PopID/Mom), family=poisson,data=modeldata)
model2<-lmer(FloatDate ~ Origin * Latitude +(1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(FloatDate ~ Origin * Latitude +(1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2,model1) # mom sig
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelI<-lmer(FloatDate ~ Origin + Latitude +(1|blank), family=poisson,data=modeldata)
anova(modelI,model3) #test for significant interaction btw Origin and Bolted - not sig

# modelL <- lmer(FloatDate ~ Origin + (1|blank), family=poisson,data=modeldata)
# anova(model)
# 
# modelO<-lmer(FloatDate ~ (LfLgth1|PopID), family=poisson,data=modeldata)
# anova(modelO,modelL2) #test for significance of origin - origin not sig

####Flood, yellow####
modeldata<-f[!is.na(f$Yellow),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

model1<-lmer(Yellow ~ Origin * Latitude +(1|PopID/Mom), family=poisson,data=modeldata)
model2<-lmer(Yellow ~ Origin * Latitude +(1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Yellow ~ Origin * Latitude +(1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2,model1) # mom sig
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelI<-lmer(Yellow ~ Origin + Latitude +(1|PopID), family=poisson,data=modeldata)
anova(modelI,model3) #test for significant interaction btw Origin and Bolted - not sig

modelL <- lmer(Yellow ~ Origin + (1|PopID), family=poisson,data=modeldata)
anova(modelL, modelI)

modelO<-lmer(Yellow ~ (1|PopID), family=poisson,data=modeldata)
anova(modelO,modelL) #test for significance of origin - origin not sig

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

modelI <- lmer(Sdwt.log ~ Origin +Latitude+(1|PopID), family=gaussian,data=modeldata)
anova(modelI, model2)

# modelL<-lmer(Sdwt.log ~ Origin + (1|PopID), family=gaussian, data=modeldata)
# anova(modelL, model2)
# 
# modelO<-lmer(Sdwt.log ~ (1|PopID), family=gaussian,data=modeldata)
# anova(modelO,modelL) #test for significance of origin - origin not sig....?

####Mom, germ count###
modeldata<-mom[!is.na(mom$GermCount),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)

model2<-lmer(GermCount ~ Origin * Latitude+ SeedCount +(1|PopID), family=poisson, data=modeldata)
model3<-lmer(GermCount ~ Origin * Latitude+SeedCount + (1|blank), family=poisson, data=modeldata)
anova(model3, model2)

modelI <- lmer(GermCount ~ Origin + Latitude+ SeedCount +(1|PopID), family=poisson, data=modeldata)
anova(modelI, model2)

# modelL<-lmer(GermCount ~ Origin +SeedCount+ (1|PopID), family=poisson, data=modeldata)
# anova(modelL, model2)
# 
# modelO<-lmer(GermCount ~ SeedCount + (1|PopID), family=poisson, data=modeldata)
# anova(modelO, modelL)
# 
# modelL
# int<- -0.42767
# #inv mean
# B<-0.52217
# #Originnat estimate from model summary
# pI<-exp(int)
# pN<-exp(int+B)
# pI
# pN
#germ count with more covariates#
####Mom, germ count###
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

modelI <- lmer(AvgGermDate.log ~ Origin +Latitude+(1|PopID), family=gaussian,data=modeldata) 
anova(modelI,model2)

# modelL<-lmer(AvgGermDate.log ~ Origin +(1|PopID), family=, data=modeldata)
# anova(modelL,model2)
# 
# modelO<-lmer(AvgGermDate.log ~ (1|PopID), family=gaussian,data=modeldata)
# anova(modelO,modelL) #test for significance of origin - origin not sig....?
# 
# modelL