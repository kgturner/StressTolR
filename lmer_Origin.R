###ST mixed FX models, focused on Origin (no latitude)###
#Stress Tolerance, REML, using lme4
#mixed effect models 
library(lme4)

#for each normal trait, compare this general set of models
model1<-lmer(trait  ~ Origin +(1|PopID/Mom), family=gaussian,data=modeldata)
model2<-lmer(trait  ~ Origin + (1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(trait  ~ Origin + (1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2,model1) # mom is sig!
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
modelO<-lmer(trait ~ (1|PopID), family=gaussian,data=modeldata)
anova(modelO,model1) #test for significance of origin - origin only marginally sig....!

#test for one trait, one df, specify non default family
lfcountLR<- CGtrait.LR.O("LfCount1",al, family=poisson)

#test for one trait, one df
shootmod <- CGtrait.models.O("ShootMass.gA", al) #test one trait

#for all traits in a df
#make sure all traits analyzed this way are the same distribution
names(al)#find col numbers for traits of interestes
alLR <- lapply(names(al)[8:13],function(n) CGtrait.LR.O(n,al))#apply func to all things in list
names(alLR) <- names(al)[8:13]
almodels <- lapply(names(al)[8:13],function(n) CGtrait.models.O(n,al))#apply func to all things in list
names(almodels) <- names(al)[8:13]

#to get one model
almodels[[1]][1] #first number is trait in column order of df, second number is model number
names(almodels[1]) #to verify trait

######Allo, Origin models######
al<-read.table("STAllosubset.txt", header=T, sep="\t", quote='"', row.names=1) #allosubset
head(al)
alLR <- lapply(names(al)[c(11:13, 20)],function(n) CGtrait.LR.O(n,al)) #crow, shoot, root, root.log, all gaussian
names(alLR) <- names(al)[c(11:13, 20)]
alLR #check out LRs of models. Model progression logical?
almodels <- CGtrait.models("CrownDiam.mmA",al)
almodels

#####allo, shoot mass, mom sig, do by hand######
modeldata<-al[!is.na(al$ShootMass.gA),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

model1raw<-lmer(ShootMass.gA  ~ Origin+ (1|PopID/Mom), family=gaussian,data=modeldata)
model2raw<-lmer(ShootMass.gA  ~ Origin+ (1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(ShootMass.gA  ~ Origin+  (1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom is sig!
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelOraw<-lmer(ShootMass.gA ~ (1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelOraw,model1raw) #test for significance of origin - origin only marginally sig....!
model1raw

####allo, root mass, mom sig, do by hand###
modeldata<-al[!is.na(al$RootA.log),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

model1<-lmer(RootA.log ~ Origin +(1|PopID/Mom), family=gaussian,data=modeldata)
model2<-lmer(RootA.log ~ Origin+  (1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(RootA.log ~ Origin + (1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2,model1) # mom is sig!
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelO<-lmer(RootA.log ~ (1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelO,model1) #test for significance of origin - origin not sig....?

####allo, crown diam#####
modeldata<-al[!is.na(al$CrownDiam.mmA),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

model1raw<-lmer(CrownDiam.mmA  ~ Origin +(1|PopID/Mom), family=gaussian,data=modeldata)
model2raw<-lmer(CrownDiam.mmA  ~ Origin + (1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(CrownDiam.mmA  ~ Origin + (1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom is sig!
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelOraw<-lmer(CrownDiam.mmA ~ (1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelOraw,model1raw) #test for significance of origin - origin only marginally sig....!
model1raw

#####m1, Origin only#####
m1<-read.table("STm1subset.txt", header=T, sep="\t", quote='"', row.names=1) #m1subset
head(m1)
# m1<-cbind(m1,lxw=m1$LfLgth1*m1$LfWdth1, lxw.log=log(m1$LfLgth1*m1$LfWdth1))
m1lxw <- CGtrait.LR.O("lxw", m1)
m1lf <- CGtrait.LR.O("LfCount1", m1, family=poisson)#poisson distribution

####m1, control, lxw#####
modeldata<-m1[!is.na(m1$lxw),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

model1<-lmer(lxw ~ Origin +(1|PopID/Mom), family=gaussian,data=modeldata)
model2<-lmer(lxw ~ Origin + (1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(lxw ~ Origin + (1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2,model1) # mom is sig!
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelO<-lmer(lxw ~ (1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelO,model1) #test for significance of origin - origin not sig....?

####m1, control, lf count####
modeldata<-m1[!is.na(m1$LfCount1),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

model1raw<-lmer(LfCount1 ~ Origin  +(1|PopID/Mom), family=poisson,data=modeldata)
model2raw<-lmer(LfCount1 ~ Origin +(1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(LfCount1 ~ Origin +(1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelOraw<-lmer(LfCount1 ~ (1|PopID/Mom), family=poisson,data=modeldata)
anova(modelOraw,model1raw) #test for significance of origin - origin NOT sig....!

####Control, Origin only####
co<-read.table("STControlsubset.txt", header=T, sep="\t", quote='"', row.names=1) #controlsubset
head(co)
coLR <- lapply(names(co)[c(15:17,52:53)],function(n) CGtrait.LR.O(n,co)) #crow, shoot, root, RootH.log, lxw, all gaussian
names(coLR) <- names(co)[c(15:17,52:53)]
coLR #check out LRs of models. Model progression logical?

# co <- cbind(co, bolt.bin=as.numeric(co$BoltedatH)-1)
# write.table(co, file="STControlsubset.txt", sep="\t", quote=F)
coBatH <- CGtrait.LR.O("bolt.bin", co, family=binomial)
coP <- lapply(names(co)[c(10,27)],function(n) CGtrait.LR.O(n,co, family=poisson)) #lfcountH, boltdate, all poisson

comodels <- CGtrait.models.O("RootH.log",co)
comodels

####harvest control, lf count#####
modeldata<-co[!is.na(co$LfCountH),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

model1raw<-lmer(LfCountH ~ Origin + (1|PopID/Mom), family=poisson,data=modeldata)
model2raw<-lmer(LfCountH ~ Origin + (1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(LfCountH ~ Origin + (1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelOraw<-lmer(LfCountH ~(1|PopID/Mom), family=poisson,data=modeldata)
anova(modelOraw,model1raw) #test for significance of origin - origin NOT sig....!
model1raw
int<- 2.76588#inv mean
B<--0.21724#Originnat estimate from model summary
pI<-exp(int)
pN<-exp(int+B)
pI
pN

####harvest, control, lxw####
#co<-cbind(co, lxwH=co$LfLgthH*co$LfWdthH, lxwH.log=log(co$LfLgthH*co$LfWdthH))
modeldata<-co[!is.na(co$lxwH),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

model1raw<-lmer(lxwH ~ Origin +(1|PopID/Mom), family=gaussian,data=modeldata)
model2raw<-lmer(lxwH ~ Origin +(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(lxwH ~ Origin +(1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelOraw<-lmer(lxwH ~ (1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelOraw,model1raw) #test for significance of origin - origin NOT sig....!

#####harvest, control, boltdate###
#only bolters
modeldata<-co[!is.na(co$BoltDate),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

model1<-lmer(BoltDate ~ Origin +(1|PopID/Mom), family=poisson,data=modeldata)
model2<-lmer(BoltDate ~ Origin +(1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(BoltDate ~ Origin +(1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2,model1) # mom sig
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelO<-lmer(BoltDate ~ (1|PopID/Mom), family=poisson,data=modeldata)
anova(modelO,model1) #test for significance of origin - origin not sig!

# #control, bolt date, extra covariates#
# modeldata<-co[!is.na(co$BoltDate),]
# modeldata$blank<-1
# modeldata$blank<-as.factor(modeldata$blank)
# modeldata$Mom<-as.factor(modeldata$Mom)
# 
# model1<-lmer(BoltDate ~ Origin * CrownDiam.mm+(CrownDiam.mm|PopID/Mom), family=poisson,data=modeldata)
# model2<-lmer(BoltDate ~ Origin * CrownDiam.mm+(CrownDiam.mm|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
# model3<-lmer(BoltDate ~ Origin * CrownDiam.mm+(CrownDiam.mm|blank), family=poisson,data=modeldata) # Test population effect
# anova(model2,model1) # mom sig
# anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
# 
# modelInt<-lmer(BoltDate ~ Origin + CrownDiam.mm+(CrownDiam.mm|PopID/Mom), family=poisson,data=modeldata)
# anova(modelInt,model1) #test for significant interaction btw Origin and Bolted - not sig
# 
# modelC0<-lmer(BoltDate ~ Origin + (1|PopID/Mom), family=poisson,data=modeldata)
# modelC1 <- lmer(BoltDate ~ Origin + (CrownDiam.mm|PopID/Mom), family=poisson,data=modeldata)
# modelC2 <- lmer(BoltDate ~ Origin + CrownDiam.mm+(1|PopID/Mom), family=poisson,data=modeldata)
# anova(modelC0, modelInt)#test for sig of crown - crown not sig
# anova(modelC1, modelInt)
# anova(modelC2,modelInt)
# 
# modelO<-lmer(BoltDate ~ CrownDiam.mm + (CrownDiam.mm|PopID/Mom), family=poisson,data=modeldata)
# anova(modelO,modelInt) #test for significance of origin - origin not sig!

###Control, boltedatH, binomial, bolt.bin###
#all plants, not just bolters
modeldata<-co[!is.na(co$bolt.bin),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
# modeldata$bolt.bin<-as.numeric(modeldata$BoltedatH)-1
# str(modeldata$bolt.bin)
modeldata$Mom<-as.factor(modeldata$Mom)

model1<-lmer(bolt.bin ~ Origin +(1|PopID/Mom), family=binomial,data=modeldata)
model2<-lmer(bolt.bin ~ Origin +(1|PopID), family=binomial,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(bolt.bin ~ Origin +(1|blank), family=binomial,data=modeldata) # Test population effect
anova(model2,model1) # mom sig
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelO<-lmer(bolt.bin ~ (1|PopID/Mom), family=binomial,data=modeldata)
anova(modelO,model1) #test for significance of origin??? origin sig!
model1
int<- -3.9821 #inv mean
B<-2.8335 #Originnat estimate from model summary
# Native
pN<-exp(int+B)/(exp(int+B)+1)
# Introduced (B=0)
pI<-exp(int)/(exp(int)+1)
pI  
pN 
#check
summary(co[co$Origin=="nat",]) #261 rows, 78 boltedatH = 30%
summary(co[co$Origin=="inv",]) #125 rows, 5 boltedatH = 4%

####Nut def, Origin only####
n<-read.table("STNutsubset.txt", header=T, sep="\t", quote='"', row.names=1) #nutsubset
head(n)
# xtabs(~Origin+BoltedatH, modeldata) # no invasives bolted.........
# modeldata<-modeldata[modeldata$BoltedatH!="y",]#remove 7 bolted plants from nat SHOULD I???
nLR <- lapply(names(n)[c(11:12, 15:17, 50:51)],function(x) CGtrait.LR.O(x,n)) 
#lflgthH, lfwdthH, crown, shoot, root, root.log (pick one!)lxwH, all gaussian
names(nLR) <- names(al)[c(11:12, 15:17, 50:51)]
nLR #check out LRs of models. Model progression logical?
nmodels <- CGtrait.models.O("ShootMass.g",n)
# nmodels2 <- CGtrait.models.int("RootH.log",n)
nmodels
# nRoot.lmer <- nmodels$model2
# nRootlog.lmer <- nmodels2$model2
# qqnorm(resid(nRootlog.lmer), main="Q-Q plot for residuals")
# qqline(resid(nRootlog.lmer))

#non-gaussian?
# n <- cbind(n, bolt.bin=as.numeric(n$BoltedatH)-1)
# write.table(n, file="STNutsubset.txt", sep="\t", quote=F)
#nBatH <- CGtrait.LR("bolt.bin", n, family=binomial)# no invasive bolted!
nlfcount <- CGtrait.LR.O("LfCountH",n, family=poisson) #lfcountH, all poisson

#######nut def, lf count, harvest####
modeldata<-n[!is.na(n$LfCountH),]
# xtabs(~Origin+BoltedatH, modeldata) # no invasives bolted.........
# modeldata<-modeldata[modeldata$BoltedatH!="y",]#remove 7 bolted plants from nat
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

model1raw<-lmer(LfCountH ~ Origin +(1|PopID/Mom), family=poisson,data=modeldata)
model2raw<-lmer(LfCountH ~ Origin +(1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(LfCountH ~ Origin +(1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelOraw<-lmer(LfCountH ~ (1|PopID/Mom), family=poisson,data=modeldata)
anova(modelOraw,model1raw) #test for significance of origin - origin NOT sig....!

####Cut, Origin only####
cu<-read.table("STCutsubset.txt", header=T, sep="\t", quote='"', row.names=1) #cutsubset
head(cu)
cuLR <- lapply(names(cu)[c(13:14, 42)],function(n) CGtrait.LR.O(n,cu)) #crow, root, root.log, all gaussian

#non-gaussian?
# cu<- cbind(cu, bolt.bin=as.numeric(cu$BoltedatH)-1)
# write.table(cu, file="STCutsubset.txt", sep="\t", quote=F)
cuBatH <- CGtrait.LR.O("bolt.bin", cu, family=binomial)
cuP <- lapply(names(cu)[c(10,27)],function(n) CGtrait.LR.O(n,cu, family=poisson)) #lfcountH, boltdate, all poisson
xtabs(~Origin+BoltDate, cu)
cumodels <- CGtrait.models.O("bolt.bin",cu, family=binomial)
cumodels
int<--2.6742 #inv mean
B<-1.6649 #Originnat estimate from model summary
pN<-exp(int+B)/(exp(int+B)+1)# Native
pI<-exp(int)/(exp(int)+1)# Introduced (B=0)
pI # 6.5% 
pN # 27%
# qqnorm(resid(cumodels$model2), main="Q-Q plot for residuals")
# qqline(resid(cumodels$model2))
# shapiro.test(resid(cumodels$model2))

#######cut, lf count, harvest######
modeldata<-cu[!is.na(cu$LfCountH),]
# xtabs(~Origin+BoltedatH, modeldata)
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

model1raw<-lmer(LfCountH ~ Origin +(1|PopID/Mom), family=poisson,data=modeldata)
model2raw<-lmer(LfCountH ~ Origin +(1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(LfCountH ~ Origin +(1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelOraw<-lmer(LfCountH ~ (1|PopID/Mom), family=poisson,data=modeldata)
anova(modelOraw,model1raw) #test for significance of origin - origin IS sig....!

#####cut, harvest, boltdate###
#given that it's bolted....
modeldata<-cu[!is.na(cu$BoltDate),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

model1<-lmer(BoltDate ~ Origin +(1|PopID/Mom), family=poisson,data=modeldata)
model2<-lmer(BoltDate ~ Origin +(1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(BoltDate ~ Origin +(1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2,model1) # mom sig
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelO<-lmer(BoltDate ~ (1|PopID/Mom), family=poisson,data=modeldata)
anova(modelO,model1) #test for significance of origin - origin not sig!
model1
int<-4.3182
#inv mean
B<--0.5277
#Originnat estimate from model summary
pI<-exp(int)
pN<-exp(int+B)
pI
pN
summary(cu[cu$Origin=="nat",]$BoltDate)

#cut, boltdate, extra covariates#
modeldata<-cu[!is.na(cu$BoltDate),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

model1<-lmer(BoltDate ~ Origin * CrownDiam.mm+(CrownDiam.mm|PopID/Mom), family=poisson,data=modeldata)
model2<-lmer(BoltDate ~ Origin * CrownDiam.mm+(CrownDiam.mm|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(BoltDate ~ Origin * CrownDiam.mm+(CrownDiam.mm|blank), family=poisson,data=modeldata) # Test population effect
anova(model2,model1) # mom sig
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelInt<-lmer(BoltDate ~ Origin + CrownDiam.mm+(CrownDiam.mm|PopID/Mom), family=poisson,data=modeldata)
anova(modelInt,model1) #test for significant interaction btw Origin and Bolted - not sig

modelC0<-lmer(BoltDate ~ Origin + (1|PopID/Mom), family=poisson,data=modeldata)
modelC1 <- lmer(BoltDate ~ Origin + (CrownDiam.mm|PopID/Mom), family=poisson,data=modeldata)
modelC2 <- lmer(BoltDate ~ Origin + CrownDiam.mm+(1|PopID/Mom), family=poisson,data=modeldata)
anova(modelC0, modelInt)#test for sig of crown - crown not sig
anova(modelC1, modelInt)
anova(modelC2,modelInt)

modelO<-lmer(BoltDate ~ (CrownDiam.mm|PopID/Mom), family=poisson,data=modeldata)
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

####Drought, Origin only####
d<-read.table("STDroughtsubset.txt", header=T, sep="\t", quote='"', row.names=1) #droughtsubset
head(d)
#no gaussian
dLR <- lapply(names(d)[8:10],function(n) CGtrait.LR.O(n,d, family=poisson)) #wilt, totwilt, death, all poisson
names(dLR) <- names(d)[8:10]

# dmodels <- lapply(names(d)[8:9],function(n) CGtrait.models.int(n,d))
# dmodels

d <- cbind(d, lxw=d$LfLgth1*d$LfWdth1)
write.table(d, file="STDroughtsubset.txt", sep="\t", quote=F)

#drought, wilt, extra covariates#
modeldata<-d[!is.na(d$Wilt),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

model1<-lmer(Wilt ~ Origin * lxw+(lxw|PopID/Mom), family=poisson,data=modeldata)
model2<-lmer(Wilt ~ Origin * lxw+(lxw|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Wilt ~ Origin * lxw+(lxw|blank), family=poisson,data=modeldata) # Test population effect
anova(model2,model1) # mom sig
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelInt<-lmer(Wilt ~ Origin + lxw+(lxw|PopID), family=poisson,data=modeldata)
anova(modelInt,model2) #test for significant interaction btw Origin and Bolted - not sig

modelL0<-lmer(Wilt ~ Origin + (1|PopID), family=poisson,data=modeldata)
modelL1<-lmer(Wilt ~ Origin + lxw+(1|PopID), family=poisson,data=modeldata)
modelL2<-lmer(Wilt ~ Origin + (lxw|PopID), family=poisson,data=modeldata)
anova(modelL0, modelInt)#test for sig of size, approx by lf length
anova(modelL1, modelInt)#test for sig of size
anova(modelL2, modelInt)#test for sig of size

modelO<-lmer(Wilt ~ lxw+(1|PopID), family=poisson,data=modeldata)
anova(modelO,modelL1) #test for significance of origin - origin  sig!
modelL1
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

####Flood, Origin * Lat####
f<-read.table("STFloodsubset.txt", header=T, sep="\t", quote='"', row.names=1) #floodsubset
head(f)
#no gaussian
fLR <- lapply(names(f)[20:21],function(n) CGtrait.LR.O(n,f, family=poisson)) #death, floatdate, all poisson
# names(fLR) <- names(f)[20:21]
# 
# fmodels <- lapply(names(f)[20:21],function(n) CGtrait.models(n,f))
# fmodels
####Flood, death####
modeldata<-f[!is.na(f$Death),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

model1<-lmer(Death ~ Origin +(1|PopID/Mom), family=poisson,data=modeldata)
model2<-lmer(Death ~ Origin +(1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Death ~ Origin +(1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2,model1) # mom sig
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelO<-lmer(Death ~ (1|PopID/Mom), family=poisson,data=modeldata)
anova(modelO,model1) #test for significance of origin - origin not sig!

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
modeldata<-mom[!is.na(mom$Sdwt.log),]
# xtabs(~Origin+SeedAgeYrs, modeldata)
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)

model2<-lmer(Sdwt.log ~ Origin +(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Sdwt.log ~ Origin +(1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelO<-lmer(Sdwt.log ~ (1|PopID), family=gaussian,data=modeldata)
anova(modelO,model2) #test for significance of origin - origin not sig....?

####Mom, germ count###
modeldata<-mom[!is.na(mom$GermCount),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)

model2<-lmer(GermCount ~ Origin + SeedCount +(1|PopID), family=poisson, data=modeldata)
model3<-lmer(GermCount ~ Origin +SeedCount + (1|blank), family=poisson, data=modeldata)
anova(model3, model2)

modelO<-lmer(GermCount ~ SeedCount + (1|PopID), family=poisson, data=modeldata)
anova(modelO, model2)

model2
int<- -0.42767
#inv mean
B<-0.52217
#Originnat estimate from model summary
pI<-exp(int)
pN<-exp(int+B)
pI
pN

#germ count with more covariates#
####Mom, germ count###
modeldata<-mom[!is.na(mom$GermCount),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
# xtabs(~Origin+SeedAgeYrs, modeldata)

model2<-lmer(GermCount ~ Origin + SeedCount +SeedWt +SeedAgeYrs+(SeedWt|PopID), family=poisson, data=modeldata)
model3<-lmer(GermCount ~ Origin + SeedCount +SeedWt + SeedAgeYrs+(SeedWt|blank), family=poisson, data=modeldata)
anova(model3, model2)

modelY<-lmer(GermCount ~ Origin + SeedCount + SeedWt + (SeedWt|PopID), family=poisson, data=modeldata)
anova(model2, modelY)

modelY2<-lmer(GermCount ~ Origin + SeedCount + SeedWt + (1|PopID), family=poisson, data=modeldata)
anova(modelY2, modelY)


modelW<-lmer(GermCount ~ Origin + SeedCount+ (SeedWt|PopID), family=poisson, data=modeldata)
anova(modelW, modelY)

modelC<-lmer(GermCount ~ Origin + SeedWt +(SeedWt|PopID), family=poisson, data=modeldata)
anova(modelC, modelY)


modelO<-lmer(GermCount ~ SeedCount +SeedWt+ (SeedWt|PopID), family=poisson, data=modeldata)
anova(modelO, modelY)

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

model2<-lmer(AvgGermDate.log ~ Origin +(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(AvgGermDate.log ~ Origin +(1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelO<-lmer(AvgGermDate.log ~ (1|PopID), family=gaussian,data=modeldata)
anova(modelO,model2) #test for significance of origin - origin not sig....?

model2