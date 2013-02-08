#Stress Tolerance, REML, using lme4
#mixed effect models 

source("http://bioconductor.org/biocLite.R")
biocLite("lme4")
library(lme4)

#st data
#open data files
#m1dat <- read.table(file.choose(), header=T, sep="\t",quote='"', row.names=1) #measure1 - early stage control group
# m2dat <- read.table(file.choose(), header=T, sep="\t",quote='"', row.names=1) #measure2
# mHdat<- read.table(file.choose(), header=T, sep="\t", quote='"', row.names=1) #measure harvest
# deathdat<-read.table(file.choose(), header=T, sep="\t", quote='"', row.names=1) #death and bolt
#des <- read.table(file.choose(), header=T, sep="\t", quote='"', row.names=1) #stresstol design

#allodat<-read.table(file.choose(), header=T, sep="\t", quote='"', row.names=1) #allometry"
#cont<-read.table(file.choose(), header=T, sep="\t", quote='"', row.names=1) #control
# cut<-read.table(file.choose(), header=T, sep="\t", quote='"', row.names=1) #cut
# nut<-read.table(file.choose(), header=T, sep="\t", quote='"', row.names=1) #nut def
# flood<-read.table(file.choose(), header=T, sep="\t", quote='"', row.names=1) #flood
mom<-read.table(file.choose(), header=T, sep="\t", quote='"', row.names=1) #momsubset
#lfdisc<-read.table(file.choose(), header=T, sep="\t", quote='"', row.names=1) #lfdisc

co<-read.table(file.choose(), header=T, sep="\t", quote='"', row.names=1) #controlsubset
m1<-read.table(file.choose(), header=T, sep="\t", quote='"', row.names=1) #m1subset
al<-read.table(file.choose(), header=T, sep="\t", quote='"', row.names=1) #allosubset
n<-read.table(file.choose(), header=T, sep="\t", quote='"', row.names=1) #nutsubset
cu<-read.table(file.choose(), header=T, sep="\t", quote='"', row.names=1) #cutsubset
d<-read.table(file.choose(), header=T, sep="\t", quote='"', row.names=1) #droughtsubset
f<-read.table(file.choose(), header=T, sep="\t", quote='"', row.names=1) #floodsubset
lf<-read.table(file.choose(), header=T, sep="\t", quote='"', row.names=1) #STleafdisclong.txt

#mom as factor?
#d$Mom<-as.factor(d$Mom)

#start with tranformed traits
#remove small pops


####Allo#####
#####allo, shoot mass######
str(al)
modeldata<-al[!is.na(al$ShootA.log),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

# model1<-lmer(ShootA.log ~ Origin +Latitude+(1|PopID/Mom), family=gaussian,data=modeldata)
# model2<-lmer(ShootA.log ~ Origin + Latitude+(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
# model3<-lmer(ShootA.log ~ Origin + Latitude+(1|blank), family=gaussian,data=modeldata) # Test population effect
# anova(model2,model1) # mom is sig!
# anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
# 
# modelL<-lmer(ShootA.log ~ Origin +(1|PopID/Mom), family=gaussian,data=modeldata)
# anova(modelL, model1)
# 
# modelO<-lmer(ShootA.log ~ Latitude + (1|PopID/Mom), family=gaussian,data=modeldata)
# anova(modelO,model1) #test for significance of origin - origin not sig....?

#raw data, rather than transformed
model1raw<-lmer(ShootMass.gA  ~ Origin+ Latitude +(1|PopID/Mom), family=gaussian,data=modeldata)
model2raw<-lmer(ShootMass.gA  ~ Origin+ Latitude + (1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(ShootMass.gA  ~ Origin+ Latitude + (1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom is sig!
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelL<-lmer(ShootMass.gA  ~ Origin + (1|PopID), family=gaussian,data=modeldata)
anova(modelL, model1raw)

modelOraw<-lmer(ShootMass.gA ~ Latitude +(1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelOraw,model1raw) #test for significance of origin - origin only marginally sig....!

####allo, root mass###
modeldata<-al[!is.na(al$RootA.log),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

# model1<-lmer(RootA.log ~ Origin + Latitude+(1|PopID/Mom), family=gaussian,data=modeldata)
# model2<-lmer(RootA.log ~ Origin+ Latitude + (1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
# model3<-lmer(RootA.log ~ Origin + Latitude+ (1|blank), family=gaussian,data=modeldata) # Test population effect
# anova(model2,model1) # mom is sig!
# anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
# 
# modelL<-lmer(RootA.log ~ Origin + (1|PopID/Mom), family=gaussian,data=modeldata)
# anova(modelL, model1)
# 
# modelO<-lmer(RootA.log ~ Latitude+(1|PopID/Mom), family=gaussian,data=modeldata)
# anova(modelO,model1) #test for significance of origin - origin not sig....?

#raw data, rather than transformed
model1raw<-lmer(RootMass.gA  ~ Origin + Latitude+(1|PopID/Mom), family=gaussian,data=modeldata)
model2raw<-lmer(RootMass.gA  ~ Origin + Latitude+ (1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(RootMass.gA  ~ Origin + Latitude+ (1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom is sig!
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelL<-lmer(RootMass.gA  ~ Origin + (1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelL, model1raw)

modelOraw<-lmer(RootMass.gA ~ Latitude + (1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelOraw,model1raw) #test for significance of origin - origin only marginally sig....!

####allo, crown diam#####
modeldata<-al[!is.na(al$CrownA.log),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

# model1<-lmer(CrownA.log ~ Origin +(1|PopID/Mom), family=gaussian,data=modeldata)
# model2<-lmer(CrownA.log ~ Origin + (1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
# model3<-lmer(CrownA.log ~ Origin + (1|blank), family=gaussian,data=modeldata) # Test population effect
# anova(model2,model1) # mom is sig!
# anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
# 
# modelO<-lmer(CrownA.log ~ (1|PopID/Mom), family=gaussian,data=modeldata)
# anova(modelO,model1) #test for significance of origin - origin not sig....?

#raw data, rather than transformed
model1raw<-lmer(CrownDiam.mmA  ~ Origin +Latitude+(1|PopID/Mom), family=gaussian,data=modeldata)
model2raw<-lmer(CrownDiam.mmA  ~ Origin +Latitude+ (1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(CrownDiam.mmA  ~ Origin +Latitude+ (1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom is sig!
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelL<-lmer(CrownDiam.mmA  ~ Origin +(1|PopID), family=gaussian,data=modeldata)
anova(modelL, model2raw)

modelOraw<-lmer(CrownDiam.mmA ~ Latitude+(1|PopID), family=gaussian,data=modeldata)
anova(modelOraw,model2raw) #test for significance of origin - origin only marginally sig....!

####m1, early control, largest sample size#####
str(m1)
m1<-cbind(m1,lxw=m1$LfLgth1*m1$LfWdth1, lxw.log=log(m1$LfLgth1*m1$LfWdth1))
#m1<-cbind(m1, maxarea=m1$lxw*m1$LfCount1, maxarea.log=log(m1$lxw*m1$LfCount1))
####m1, control, lxw#####
modeldata<-m1[!is.na(m1$lxw),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

model1<-lmer(lxw ~ Origin +Latitude+(1|PopID/Mom), family=gaussian,data=modeldata)
model2<-lmer(lxw ~ Origin +Latitude+ (1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(lxw ~ Origin +Latitude+ (1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2,model1) # mom is sig!
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelL<-lmer(lxw ~ Origin +(1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelL, model1)

modelO<-lmer(lxw ~ (1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelO,modelL) #test for significance of origin - origin not sig....?
#mom and popID sig, but not Origin! for either log or raw data

####m1, control, lf count####
#poisson on raw data... origin still not sig
modeldata<-m1[!is.na(m1$LfCount1),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

model1raw<-lmer(LfCount1 ~ Origin +Latitude +(1|PopID/Mom), family=poisson,data=modeldata)
model2raw<-lmer(LfCount1 ~ Origin +Latitude+(1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(LfCount1 ~ Origin +Latitude+(1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelL<-lmer(LfCount1 ~ Origin +(1|PopID/Mom), family=poisson,data=modeldata)
anova(modelL, model1raw)

modelOraw<-lmer(LfCount1 ~ Latitude+(1|PopID/Mom), family=poisson,data=modeldata)
anova(modelOraw,model1raw) #test for significance of origin - origin NOT sig....!

#####m1, control, max lf area###
#mom and popID sig, but not Origin! for either log or raw data
####m1, control, lf lgth####
#mom and popID sig, but not Origin! for either log or raw data
#####m1, control, lf width####
#mom and popID sig, but not Origin! for either log or raw data

####Harvest control#####
#######shoot mass, harvest######
str(co)
#co<-co[co$ID!="RO002.22.h",]
modeldata<-co[!is.na(co$ShootH.log),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

# model1<-lmer(ShootH.log ~ Origin * BoltedatH+(BoltedatH|PopID/Mom), family=gaussian,data=modeldata)
# model2<-lmer(ShootH.log ~ Origin * BoltedatH+(BoltedatH|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
# model3<-lmer(ShootH.log ~ Origin * BoltedatH+(BoltedatH|blank), family=gaussian,data=modeldata) # Test population effect
# anova(model2,model1) # mom not sig
# anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
# 
# modelInt<-lmer(ShootH.log ~ Origin + BoltedatH+(BoltedatH|PopID), family=gaussian,data=modeldata)
# anova(modelInt,model2) #test for significant interaction btw Origin and Bolted - not sig
# 
# modelB0<-lmer(ShootH.log ~ Origin + (1|PopID), family=gaussian,data=modeldata)
# modelB1<-lmer(ShootH.log ~ Origin + BoltedatH+(1|PopID), family=gaussian,data=modeldata)
# modelB2<-lmer(ShootH.log ~ Origin + (BoltedatH|PopID), family=gaussian,data=modeldata)
# anova(modelB0, modelInt)#test for sig of bolting - bolting sig
# anova(modelB1, modelInt)#test for sig of bolting - not sig - bolting not needed in random effect
# anova(modelB2, modelInt)#test for sig of bolting - bolting sig
# 
# modelL
# 
# modelO<-lmer(ShootH.log ~ BoltedatH+(1|PopID), family=gaussian,data=modeldata)
# anova(modelO,modelB1) #test for significance of origin - origin not sig....?

#raw data, rather than transformed
model1raw<-lmer(ShootMass.g ~ Origin *Latitude* BoltedatH+(BoltedatH|PopID/Mom), family=gaussian,data=modeldata)
model2raw<-lmer(ShootMass.g ~ Origin *Latitude* BoltedatH+(BoltedatH|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(ShootMass.g ~ Origin *Latitude* BoltedatH+(BoltedatH|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelL<-lmer(ShootMass.g ~ Origin * BoltedatH+(BoltedatH|PopID), family=gaussian,data=modeldata)
anova(modelL, model2raw)
modelLint<-lmer(ShootMass.g ~ Origin +Latitude* BoltedatH+(BoltedatH|PopID), family=gaussian,data=modeldata)
anova(modelLint,model2raw)

modelIntraw<-lmer(ShootMass.g ~ Origin +Latitude+ BoltedatH+(BoltedatH|PopID), family=gaussian,data=modeldata)
anova(modelIntraw,modelLint) #test for significant interaction btw Origin and Bolted - not sig

modelB0raw<-lmer(ShootMass.g ~ Origin +Latitude+ (1|PopID), family=gaussian,data=modeldata)
modelB1raw<-lmer(ShootMass.g ~ Origin +Latitude+ BoltedatH+(1|PopID), family=gaussian,data=modeldata)
modelB2raw<-lmer(ShootMass.g ~ Origin +Latitude+ (BoltedatH|PopID), family=gaussian,data=modeldata)
anova(modelB0raw, modelIntraw)#test for sig of bolting - bolting sig
anova(modelB1raw, modelIntraw)#test for sig of bolting - sig - bolting IS needed in random effect!
anova(modelB2raw, modelIntraw)#test for sig of bolting - bolting sig

modelOraw<-lmer(ShootMass.g ~ Latitude+BoltedatH+(BoltedatH|PopID), family=gaussian,data=modeldata)
anova(modelOraw,modelIntraw) #test for significance of origin - origin IS sig....!

####harvest, control, root mass####
modeldata<-co[!is.na(co$RootH.log),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

# model1<-lmer(RootH.log ~ Origin * BoltedatH+(BoltedatH|PopID/Mom), family=gaussian,data=modeldata)
# model2<-lmer(RootH.log ~ Origin * BoltedatH+(BoltedatH|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
# model3<-lmer(RootH.log ~ Origin * BoltedatH+(BoltedatH|blank), family=gaussian,data=modeldata) # Test population effect
# anova(model2,model1) # mom not sig
# anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
# 
# modelInt<-lmer(RootH.log ~ Origin + BoltedatH+(BoltedatH|PopID), family=gaussian,data=modeldata)
# anova(modelInt,model2) #test for significant interaction btw Origin and Bolted - not sig
# 
# modelB0<-lmer(RootH.log ~ Origin + (1|PopID), family=gaussian,data=modeldata)
# modelB1<-lmer(RootH.log ~ Origin + BoltedatH+(1|PopID), family=gaussian,data=modeldata)
# modelB2<-lmer(RootH.log ~ Origin + (BoltedatH|PopID), family=gaussian,data=modeldata)
# anova(modelB0, modelInt)#test for sig of bolting - bolting sig
# anova(modelB1, modelInt)#test for sig of bolting - not sig - bolting not needed in random effect
# anova(modelB2, modelInt)#test for sig of bolting - bolting sig
# 
# modelO<-lmer(RootH.log ~ BoltedatH+(1|PopID), family=gaussian,data=modeldata)
# anova(modelO,modelB1) #test for significance of origin - origin not sig....?

#raw data, rather than transformed
model1raw<-lmer(RootMass.g ~ Origin *Latitude* BoltedatH+(BoltedatH|PopID/Mom), family=gaussian,data=modeldata)
model2raw<-lmer(RootMass.g ~ Origin *Latitude* BoltedatH+(BoltedatH|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(RootMass.g ~ Origin *Latitude* BoltedatH+(BoltedatH|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig!
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelL<-lmer(RootMass.g ~ Origin * BoltedatH+(BoltedatH|PopID), family=gaussian,data=modeldata)
anova(modelL, model2raw)

modelIntraw<-lmer(RootMass.g ~ Origin + BoltedatH+(BoltedatH|PopID), family=gaussian,data=modeldata)
anova(modelIntraw,modelL) #test for significant interaction btw Origin and Bolted - not sig

modelB0raw<-lmer(RootMass.g ~ Origin + (1|PopID), family=gaussian,data=modeldata)
modelB1raw<-lmer(RootMass.g ~ Origin + BoltedatH+(1|PopID), family=gaussian,data=modeldata)
modelB2raw<-lmer(RootMass.g ~ Origin + (BoltedatH|PopID), family=gaussian,data=modeldata)
anova(modelB0raw, modelIntraw)#test for sig of bolting - bolting not sig/marginal p=0.08
anova(modelB1raw, modelIntraw)#test for sig of bolting - not sig
anova(modelB2raw, modelIntraw)#test for sig of bolting - bolting not sig

modelOraw<-lmer(RootMass.g ~ BoltedatH+(1|PopID), family=gaussian,data=modeldata)
anova(modelOraw,modelB1raw) #test for significance of origin - origin NOT sig....! Roots suck

####harvest control, root crown diameter####
modeldata<-co[!is.na(co$CrownH.log),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

# model1<-lmer(CrownH.log ~ Origin * BoltedatH+(BoltedatH|PopID/Mom), family=gaussian,data=modeldata)
# model2<-lmer(CrownH.log ~ Origin * BoltedatH+(BoltedatH|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
# model3<-lmer(CrownH.log ~ Origin * BoltedatH+(BoltedatH|blank), family=gaussian,data=modeldata) # Test population effect
# anova(model2,model1) # mom not sig
# anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
# 
# modelInt<-lmer(CrownH.log ~ Origin + BoltedatH+(BoltedatH|PopID), family=gaussian,data=modeldata)
# anova(modelInt,model1) #test for significant interaction btw Origin and Bolted - not sig
# 
# modelB0<-lmer(CrownH.log ~ Origin + (1|PopID), family=gaussian,data=modeldata)
# modelB1<-lmer(CrownH.log ~ Origin + BoltedatH+(1|PopID), family=gaussian,data=modeldata)
# modelB2<-lmer(CrownH.log ~ Origin + (BoltedatH|PopID), family=gaussian,data=modeldata)
# anova(modelB0, modelInt)#test for sig of bolting - bolting not sig
# anova(modelB1, modelInt)#test for sig of bolting - not sig - bolting not needed in random effect
# anova(modelB2, modelInt)#test for sig of bolting - bolting not sig
# 
# modelO<-lmer(CrownH.log ~ (1|PopID), family=gaussian,data=modeldata)
# anova(modelO,modelB0) #test for significance of origin - origin not sig....?

#raw data, rather than transformed
model1raw<-lmer(CrownDiam.mm ~ Origin *Latitude * BoltedatH+(BoltedatH|PopID/Mom), family=gaussian,data=modeldata)
model2raw<-lmer(CrownDiam.mm ~ Origin *Latitude* BoltedatH+(BoltedatH|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(CrownDiam.mm ~ Origin *Latitude* BoltedatH+(BoltedatH|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelL<-lmer(CrownDiam.mm ~ Origin * BoltedatH+(BoltedatH|PopID), family=gaussian,data=modeldata)
anova(modelL,model2raw)

modelIntraw<-lmer(CrownDiam.mm ~ Origin + BoltedatH+(BoltedatH|PopID), family=gaussian,data=modeldata)
anova(modelIntraw,modelL) #test for significant interaction btw Origin and Bolted - not sig

modelB0raw<-lmer(CrownDiam.mm ~ Origin + (1|PopID), family=gaussian,data=modeldata)
modelB1raw<-lmer(CrownDiam.mm ~ Origin + BoltedatH+(1|PopID), family=gaussian,data=modeldata)
modelB2raw<-lmer(CrownDiam.mm ~ Origin + (BoltedatH|PopID), family=gaussian,data=modeldata)
anova(modelB0raw, modelIntraw)#test for sig of bolting - bolting not sig/marginal p=0.08
anova(modelB1raw, modelIntraw)#test for sig of bolting - not sig
anova(modelB2raw, modelIntraw)#test for sig of bolting - bolting not sig

modelOraw<-lmer(CrownDiam.mm ~ (1|PopID), family=gaussian,data=modeldata)
anova(modelB0raw,modelOraw) #test for significance of origin - origin NOT sig....! 

####harvest control, lf count#####
#poisson on raw data
modeldata<-co[!is.na(co$LfCountH),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

model1raw<-lmer(LfCountH ~ Origin *Latitude * BoltedatH+(BoltedatH|PopID/Mom), family=poisson,data=modeldata)
model2raw<-lmer(LfCountH ~ Origin *Latitude* BoltedatH+(BoltedatH|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(LfCountH ~ Origin *Latitude* BoltedatH+(BoltedatH|blank), family=poisson,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelL<-lmer(LfCountH ~ Origin * BoltedatH+(BoltedatH|PopID/Mom), family=poisson,data=modeldata)
anova(modelL, model1raw)
modelLint<-lmer(LfCountH ~ Origin +Latitude * BoltedatH+(BoltedatH|PopID/Mom), family=poisson,data=modeldata)
anova(modelLint, model1raw)

modelIntraw<-lmer(LfCountH ~ Origin + Latitude+BoltedatH+(BoltedatH|PopID/Mom), family=poisson,data=modeldata)
anova(modelIntraw,modelLint) #test for significant interaction btw Origin and Bolted - not sig

modelB0raw<-lmer(LfCountH ~ Origin + Latitude+ (1|PopID/Mom), family=poisson,data=modeldata)
modelB1raw<-lmer(LfCountH ~ Origin + Latitude+ BoltedatH+(1|PopID/Mom), family=poisson,data=modeldata)
modelB2raw<-lmer(LfCountH ~ Origin + Latitude+ (BoltedatH|PopID/Mom), family=poisson,data=modeldata)
anova(modelB0raw, modelIntraw)#test for sig of bolting - bolting not sig/marginal p=0.08
anova(modelB1raw, modelIntraw)#test for sig of bolting - not sig
anova(modelB2raw, modelIntraw)#test for sig of bolting - bolting not sig

modelOraw<-lmer(LfCountH ~Latitude+BoltedatH+(BoltedatH|PopID/Mom), family=poisson,data=modeldata)
anova(modelOraw,modelIntraw) #test for significance of origin - origin NOT sig....!

####harvest, control, lxw####
#co<-cbind(co, lxwH=co$LfLgthH*co$LfWdthH, lxwH.log=log(co$LfLgthH*co$LfWdthH))
modeldata<-co[!is.na(co$lxwH.log),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

# model1<-lmer(lxwH.log ~ Origin * BoltedatH+(BoltedatH|PopID/Mom), family=gaussian,data=modeldata)
# model2<-lmer(lxwH.log ~ Origin * BoltedatH+(BoltedatH|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
# model3<-lmer(lxwH.log ~ Origin * BoltedatH+(BoltedatH|blank), family=gaussian,data=modeldata) # Test population effect
# anova(model2,model1) # mom sig
# anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
# 
# modelInt<-lmer(lxwH.log ~ Origin + BoltedatH+(BoltedatH|PopID/Mom), family=gaussian,data=modeldata)
# anova(modelInt,model1) #test for significant interaction btw Origin and Bolted - not sig
# 
# modelB0<-lmer(lxwH.log ~ Origin + (1|PopID/Mom), family=gaussian,data=modeldata)
# modelB1<-lmer(lxwH.log ~ Origin + BoltedatH+(1|PopID/Mom), family=gaussian,data=modeldata)
# modelB2<-lmer(lxwH.log ~ Origin + (BoltedatH|PopID/Mom), family=gaussian,data=modeldata)
# anova(modelB0, modelInt)#test for sig of bolting - bolting sig
# anova(modelB1, modelInt)#test for sig of bolting - bolting sig
# anova(modelB2, modelInt)#test for sig of bolting - bolting sig
# 
# modelO<-lmer(lxwH.log ~ BoltedatH+(BoltedatH|PopID/Mom), family=gaussian,data=modeldata)
# anova(modelO,modelInt) #test for significance of origin - origin not sig!

#raw data, rather than transformed
model1raw<-lmer(lxwH ~ Origin *Latitude * BoltedatH+(BoltedatH|PopID/Mom), family=gaussian,data=modeldata)
model2raw<-lmer(lxwH ~ Origin *Latitude* BoltedatH+(BoltedatH|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(lxwH ~ Origin *Latitude* BoltedatH+(BoltedatH|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelL<-lmer(lxwH ~ Origin * BoltedatH+(BoltedatH|PopID/Mom), family=gaussian,data=modeldata)
anova(modelL, model1raw)

modelIntraw<-lmer(lxwH ~ Origin + BoltedatH+(BoltedatH|PopID/Mom), family=gaussian,data=modeldata)
anova(modelIntraw,modelL) #test for significant interaction btw Origin and Bolted - not sig

modelB0raw<-lmer(lxwH ~ Origin + (1|PopID/Mom), family=gaussian,data=modeldata)
modelB1raw<-lmer(lxwH ~ Origin + BoltedatH+(1|PopID/Mom), family=gaussian,data=modeldata)
modelB2raw<-lmer(lxwH ~ Origin + (BoltedatH|PopID/Mom), family=gaussian,data=modeldata)
anova(modelB0raw, modelIntraw)#test for sig of bolting - bolting not sig/marginal p=0.08
anova(modelB1raw, modelIntraw)#test for sig of bolting - not sig
anova(modelB2raw, modelIntraw)#test for sig of bolting - bolting not sig

modelOraw<-lmer(lxwH ~ BoltedatH+(1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelOraw,modelB1raw) #test for significance of origin - origin NOT sig....!

#####harvest, control, bolting###
#only bolters
modeldata<-co[!is.na(co$BoltDate),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

model1<-lmer(BoltDate ~ Origin + Latitude + CrownDiam.mm+(1|PopID/Mom), family=poisson,data=modeldata)
model2<-lmer(BoltDate ~ Origin + Latitude + CrownDiam.mm+(1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(BoltDate ~ Origin + Latitude + CrownDiam.mm+(1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2,model1) # mom sig
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelL<-lmer(BoltDate ~ Origin + CrownDiam.mm+(1|PopID/Mom), family=poisson,data=modeldata)
anova(modelL, model1)

modelInt<-lmer(BoltDate ~ Origin * CrownDiam.mm+(1|PopID/Mom), family=poisson,data=modeldata)
anova(modelInt,modelL) #test for significant interaction btw Origin and Bolted - not sig

modelC0<-lmer(BoltDate ~ Origin + (1|PopID/Mom), family=poisson,data=modeldata)
anova(modelC0, modelL)#test for sig of crown - crown not sig

modelO<-lmer(BoltDate ~ (1|PopID/Mom), family=poisson,data=modeldata)
anova(modelO,modelC0) #test for significance of origin - origin not sig!

#survival analysis?


#boltedatH, binomial
#all plants, not just bolters
modeldata<-co[!is.na(co$BoltedatH),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$bolt.bin<-as.numeric(modeldata$BoltedatH)-1
str(modeldata$bolt.bin)
modeldata$Mom<-as.factor(modeldata$Mom)

model1<-lmer(bolt.bin ~ Origin * Latitude+ CrownDiam.mm+(1|PopID/Mom), family=binomial,data=modeldata)
model2<-lmer(bolt.bin ~ Origin * Latitude+ CrownDiam.mm+(1|PopID), family=binomial,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(bolt.bin ~ Origin * Latitude+ CrownDiam.mm+(1|blank), family=binomial,data=modeldata) # Test population effect
anova(model2,model1) # mom sig
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelC0<-lmer(bolt.bin ~ Origin * Latitude+ (1|PopID/Mom), family=binomial,data=modeldata)
anova(modelC0, model1)#test for sig of size - crown not sig

modelC2<-lmer(bolt.bin ~ Origin +Latitude+ (1|PopID/Mom), family=binomial,data=modeldata)
anova(modelC0, modelC2) #do latitudinal clines differ in slope between ranges? Sig so yes

modelC3<-lmer(bolt.bin ~ Origin + (1|PopID/Mom), family=binomial,data=modeldata)
anova(modelC3, modelC2) #are lat and origin different???? sig so yes?

modelO<-lmer(bolt.bin ~ Latitude + (1|PopID/Mom), family=binomial,data=modeldata)
anova(modelO,modelC2) #test for significance of origin??? origin sig!

# modelO2<-lmer(bolt.bin ~  (1|PopID/Mom), family=binomial,data=modeldata)
# anova(modelC3, modelO2)

int<- -25.6701 #inv mean
B<-32.0353 #Originnat estimate from model summary
# Native
pN<-exp(int+B)/(exp(int+B)+1)
# Introduced (B=0)
pI<-exp(int)/(exp(int)+1)

pI  
pN 
#check
summary(co[co$Origin=="nat",]) #261 rows, 78 boltedatH = 30%
summary(co[co$Origin=="inv",]) #125 rows, 5 boltedatH = 4%

> cor(modeldata$Latitude,as.numeric(modeldata$Origin), method ="spearman")
[1] -0.08472472
> cor(modeldata$Latitude,as.numeric(modeldata$PopID), method="spearman")
[1] 0.01241965

# modelL<-lmer(bolt.bin ~ Origin * CrownDiam.mm+(1|PopID/Mom), family=binomial,data=modeldata)
# anova(modelL, model1)
# modelLint<-lmer(bolt.bin ~ Origin + Latitude * CrownDiam.mm+(1|PopID/Mom), family=binomial,data=modeldata)
# anova(modelLint, model1)

# modelInt<-lmer(bolt.bin ~ Origin * Latitude+ CrownDiam.mm+(1|PopID/Mom), family=binomial,data=modeldata)
# anova(modelInt,model1) #test for significant interaction btw Origin and crown - not sig

####Nut Def#####
#######nut def, shoot mass, harvest######
str(n)
modeldata<-n[!is.na(n$ShootH.log),]
xtabs(~Origin+BoltedatH, modeldata) # no invasives bolted.........
modeldata<-modeldata[modeldata$BoltedatH!="y",]#remove 7 bolted plants from nat
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

# model1<-lmer(ShootH.log ~ Origin +(1|PopID/Mom), family=gaussian,data=modeldata)
# model2<-lmer(ShootH.log ~ Origin +(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
# model3<-lmer(ShootH.log ~ Origin +(1|blank), family=gaussian,data=modeldata) # Test population effect
# anova(model2,model1) # mom not sig
# anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
# 
# modelO<-lmer(ShootH.log ~ (1|PopID), family=gaussian,data=modeldata)
# anova(modelO,model2) #test for significance of origin - origin not sig....?

#raw data, rather than transformed
model1raw<-lmer(ShootMass.g ~ Origin +(1|PopID/Mom), family=gaussian,data=modeldata)
model2raw<-lmer(ShootMass.g ~ Origin +(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(ShootMass.g ~ Origin +(1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelOraw<-lmer(ShootMass.g ~ (1|PopID), family=gaussian,data=modeldata)
anova(modelOraw,model2raw) #test for significance of origin - origin IS sig....!
model2raw
#######nut def, root mass, harvest######
str(n)
modeldata<-n[!is.na(n$RootH.log),]
xtabs(~Origin+BoltedatH, modeldata) # no invasives bolted.........
modeldata<-modeldata[modeldata$BoltedatH!="y",]#remove 7 bolted plants from nat
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

# model1<-lmer(RootH.log ~ Origin +(1|PopID/Mom), family=gaussian,data=modeldata)
# model2<-lmer(RootH.log ~ Origin +(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
# model3<-lmer(RootH.log ~ Origin +(1|blank), family=gaussian,data=modeldata) # Test population effect
# anova(model2,model1) # mom not sig
# anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
# 
# modelO<-lmer(RootH.log ~ (1|PopID), family=gaussian,data=modeldata)
# anova(modelO,model2) #test for significance of origin - origin not sig....?

#raw data, rather than transformed
model1raw<-lmer(RootMass.g ~ Origin +(1|PopID/Mom), family=gaussian,data=modeldata)
model2raw<-lmer(RootMass.g ~ Origin +(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(RootMass.g ~ Origin +(1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelOraw<-lmer(RootMass.g ~ (1|PopID), family=gaussian,data=modeldata)
anova(modelOraw,model2raw) #test for significance of origin - origin IS sig....!
model2raw

#######nut def, crown, harvest######
str(n)
modeldata<-n[!is.na(n$CrownH.log),]
xtabs(~Origin+BoltedatH, modeldata) # no invasives bolted.........
modeldata<-modeldata[modeldata$BoltedatH!="y",]#remove 7 bolted plants from nat
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

model1<-lmer(CrownH.log ~ Origin +(1|PopID/Mom), family=gaussian,data=modeldata)
model2<-lmer(CrownH.log ~ Origin +(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(CrownH.log ~ Origin +(1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2,model1) # mom not sig
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelO<-lmer(CrownH.log ~ (1|PopID), family=gaussian,data=modeldata)
anova(modelO,model2) #test for significance of origin - origin not sig....?

#raw data, rather than transformed
model1raw<-lmer(CrownDiam.mm ~ Origin +(1|PopID/Mom), family=gaussian,data=modeldata)
model2raw<-lmer(CrownDiam.mm ~ Origin +(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(CrownDiam.mm ~ Origin +(1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelOraw<-lmer(CrownDiam.mm ~ (1|PopID), family=gaussian,data=modeldata)
anova(modelOraw,model2raw) #test for significance of origin - origin IS sig....!

#######nut def, lf count, harvest####
str(n)
modeldata<-n[!is.na(n$LfCountH),]
xtabs(~Origin+BoltedatH, modeldata) # no invasives bolted.........
modeldata<-modeldata[modeldata$BoltedatH!="y",]#remove 7 bolted plants from nat
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

model1raw<-lmer(LfCountH ~ Origin +(1|PopID/Mom), family=poisson,data=modeldata)
model2raw<-lmer(LfCountH ~ Origin +(1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(LfCountH ~ Origin +(1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelOraw<-lmer(LfCountH ~ (1|PopID), family=poisson,data=modeldata)
anova(modelOraw,modelIntraw) #test for significance of origin - origin NOT sig....!

#######nut def, lxwH, harvest######
str(n)
#n<-cbind(n,lxwH=n$LfLgthH*n$LfWdthH,lxwH.log=log(n$LfLgthH*n$LfWdthH))
modeldata<-n[!is.na(n$lxwH.log),]
xtabs(~Origin+BoltedatH, modeldata) # no invasives bolted.........
modeldata<-modeldata[modeldata$BoltedatH!="y",]#remove 5 bolted plants from nat
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

model1<-lmer(lxwH.log ~ Origin +(1|PopID/Mom), family=gaussian,data=modeldata)
model2<-lmer(lxwH.log ~ Origin +(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(lxwH.log ~ Origin +(1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2,model1) # mom not sig
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelO<-lmer(lxwH.log ~ (1|PopID), family=gaussian,data=modeldata)
anova(modelO,model2) #test for significance of origin - origin not sig....?

#raw data, rather than transformed
model1raw<-lmer(lxwH ~ Origin +(1|PopID/Mom), family=gaussian,data=modeldata)
model2raw<-lmer(lxwH ~ Origin +(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(lxwH ~ Origin +(1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelOraw<-lmer(lxwH ~ (1|PopID), family=gaussian,data=modeldata)
anova(modelOraw,model2raw) #test for significance of origin - origin IS sig....!

####Cut#####no lf/shoot measures
#######cut, root mass, harvest######
str(cu)
modeldata<-cu[!is.na(cu$RootH.log),]
xtabs(~Origin+BoltedatH, modeldata)
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

# model1<-lmer(RootH.log ~ Origin * BoltedatH+(BoltedatH|PopID/Mom), family=gaussian,data=modeldata)
# model2<-lmer(RootH.log ~ Origin * BoltedatH+(BoltedatH|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
# model3<-lmer(RootH.log ~ Origin * BoltedatH+(BoltedatH|blank), family=gaussian,data=modeldata) # Test population effect
# anova(model2,model1) # mom not sig
# anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
# 
# modelInt<-lmer(RootH.log ~ Origin + BoltedatH+(BoltedatH|PopID), family=gaussian,data=modeldata)
# anova(modelInt,model2) #test for significant interaction btw Origin and Bolted - not sig
# 
# modelB0<-lmer(RootH.log ~ Origin + (1|PopID), family=gaussian,data=modeldata)
# modelB1<-lmer(RootH.log ~ Origin + BoltedatH+(1|PopID), family=gaussian,data=modeldata)
# modelB2<-lmer(RootH.log ~ Origin + (BoltedatH|PopID), family=gaussian,data=modeldata)
# anova(modelB0, modelInt)#test for sig of bolting - bolting sig
# anova(modelB1, modelInt)#test for sig of bolting - bolting sig
# anova(modelB2, modelInt)#test for sig of bolting - bolting sig
# 
# modelO<-lmer(RootH.log ~ (1|PopID), family=gaussian,data=modeldata)
# anova(modelO,model2) #test for significance of origin - origin not sig....?

#raw data, rather than transformed
model1raw<-lmer(RootMass.g ~ Origin * BoltedatH+(BoltedatH|PopID/Mom), family=gaussian,data=modeldata)
model2raw<-lmer(RootMass.g ~ Origin * BoltedatH+(BoltedatH|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(RootMass.g ~ Origin * BoltedatH+(BoltedatH|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelIntraw<-lmer(RootMass.g ~ Origin + BoltedatH+(BoltedatH|PopID), family=gaussian,data=modeldata)
anova(modelIntraw,model2raw) #test for significant interaction btw Origin and Bolted - not sig

modelB0raw<-lmer(RootMass.g ~ Origin + (1|PopID), family=gaussian,data=modeldata)
modelB1raw<-lmer(RootMass.g ~ Origin + BoltedatH+(1|PopID), family=gaussian,data=modeldata)
modelB2raw<-lmer(RootMass.g ~ Origin + (BoltedatH|PopID), family=gaussian,data=modeldata)
anova(modelB0raw, modelIntraw)#test for sig of bolting - bolting sig
anova(modelB1raw, modelIntraw)#test for sig of bolting - bolting sig
anova(modelB2raw, modelIntraw)#test for sig of bolting - bolting sig

modelOraw<-lmer(RootMass.g ~ (1|PopID), family=gaussian,data=modeldata)
anova(modelOraw,modelB0raw) #test for significance of origin - origin IS sig....!
modelB0raw

#######cut, crown, harvest######
str(cu)
modeldata<-cu[!is.na(cu$CrownH.log),]
xtabs(~Origin+BoltedatH, modeldata)
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

model1<-lmer(CrownH.log ~ Origin * BoltedatH+(BoltedatH|PopID/Mom), family=gaussian,data=modeldata)
model2<-lmer(CrownH.log ~ Origin * BoltedatH+(BoltedatH|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(CrownH.log ~ Origin * BoltedatH+(BoltedatH|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2,model1) # mom not sig
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelInt<-lmer(CrownH.log ~ Origin + BoltedatH+(BoltedatH|PopID), family=gaussian,data=modeldata)
anova(modelInt,model2) #test for significant interaction btw Origin and Bolted - not sig

modelB0<-lmer(CrownH.log ~ Origin + (1|PopID), family=gaussian,data=modeldata)
modelB1<-lmer(CrownH.log ~ Origin + BoltedatH+(1|PopID), family=gaussian,data=modeldata)
modelB2<-lmer(CrownH.log ~ Origin + (BoltedatH|PopID), family=gaussian,data=modeldata)
anova(modelB0, modelInt)#test for sig of bolting - bolting not sig
anova(modelB1, modelInt)#test for sig of bolting - bolting not sig
anova(modelB2, modelInt)#test for sig of bolting - bolting not sig

modelO<-lmer(CrownH.log ~ (1|PopID), family=gaussian,data=modeldata)
anova(modelO,modelB0) #test for significance of origin - origin not sig....?

#raw data, rather than transformed
model1raw<-lmer(CrownDiam.mm ~ Origin * BoltedatH+(BoltedatH|PopID/Mom), family=gaussian,data=modeldata)
model2raw<-lmer(CrownDiam.mm ~ Origin * BoltedatH+(BoltedatH|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(CrownDiam.mm ~ Origin * BoltedatH+(BoltedatH|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelIntraw<-lmer(CrownDiam.mm ~ Origin + BoltedatH+(BoltedatH|PopID), family=gaussian,data=modeldata)
anova(modelIntraw,model2raw) #test for significant interaction btw Origin and Bolted - not sig

modelB0raw<-lmer(CrownDiam.mm ~ Origin + (1|PopID), family=gaussian,data=modeldata)
modelB1raw<-lmer(CrownDiam.mm ~ Origin * BoltedatH+(1|PopID), family=gaussian,data=modeldata)
modelB2raw<-lmer(CrownDiam.mm ~ Origin + (BoltedatH|PopID), family=gaussian,data=modeldata)
anova(modelB0raw, model2raw)#test for sig of bolting - bolting not sig
anova(modelB1raw, model2raw)#test for sig of bolting - bolting not sig
anova(modelB2raw, model2raw)#test for sig of bolting - bolting not sig
#
modelOraw<-lmer(CrownDiam.mm ~ (1|PopID), family=gaussian,data=modeldata)
anova(modelOraw,modelB0raw) #test for significance of origin - origin not sig....!

#######cut, lf count, harvest######
str(cu)
modeldata<-cu[!is.na(cu$LfCountH),]
xtabs(~Origin+BoltedatH, modeldata)
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

#raw data, family=poisson
model1raw<-lmer(LfCountH ~ Origin * BoltedatH+(BoltedatH|PopID/Mom), family=poisson,data=modeldata)
model2raw<-lmer(LfCountH ~ Origin * BoltedatH+(BoltedatH|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(LfCountH ~ Origin * BoltedatH+(BoltedatH|blank), family=poisson,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelIntraw<-lmer(LfCountH ~ Origin + BoltedatH+(BoltedatH|PopID/Mom), family=poisson,data=modeldata)
anova(modelIntraw,model2raw) #test for significant interaction btw Origin and Bolted - not sig

modelB0raw<-lmer(LfCountH ~ Origin + (1|PopID/Mom), family=poisson,data=modeldata)
modelB1raw<-lmer(LfCountH ~ Origin + BoltedatH+(1|PopID/Mom), family=poisson,data=modeldata)
modelB2raw<-lmer(LfCountH ~ Origin + (BoltedatH|PopID/Mom), family=poisson,data=modeldata)
anova(modelB0raw, modelIntraw)#test for sig of bolting - bolting not sig
anova(modelB1raw, modelIntraw)#test for sig of bolting - bolting not sig
anova(modelB2raw, modelIntraw)#test for sig of bolting - bolting not sig
#
modelOraw<-lmer(LfCountH ~ BoltedatH+(BoltedatH|PopID/Mom), family=poisson,data=modeldata)
anova(modelOraw,modelIntraw) #test for significance of origin - origin IS sig....!

#####cut, harvest, bolting###
#given that it's bolted....
modeldata<-cu[!is.na(cu$BoltDate),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

model1<-lmer(BoltDate ~ Origin * CrownDiam.mm+(1|PopID/Mom), family=poisson,data=modeldata)
model2<-lmer(BoltDate ~ Origin * CrownDiam.mm+(1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(BoltDate ~ Origin * CrownDiam.mm+(1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2,model1) # mom sig
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelInt<-lmer(BoltDate ~ Origin + CrownDiam.mm+(1|PopID/Mom), family=poisson,data=modeldata)
anova(modelInt,model1) #test for significant interaction btw Origin and Bolted - not sig

modelC0<-lmer(BoltDate ~ Origin + (1|PopID/Mom), family=poisson,data=modeldata)
anova(modelC0, modelInt)#test for sig of crown - crown not sig

modelO<-lmer(BoltDate ~ (1|PopID/Mom), family=poisson,data=modeldata)
anova(modelO,modelInt) #test for significance of origin - origin not sig!
int<-4.33448
#inv mean
B<--0.60004
#Originnat estimate from model summary
pI<-exp(int)
pN<-exp(int+B)
pI
pN
summary(cu[cu$Origin=="nat",]$BoltDate)

#survival analysis?
#boltedatH, binomial
modeldata<-cu[!is.na(cu$BoltedatH),]
xtabs(~Origin + BoltedatH, modeldata)
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$bolt.bin<-as.numeric(modeldata$BoltedatH)-1
str(modeldata$bolt.bin)
modeldata$Mom<-as.factor(modeldata$Mom)

model1<-lmer(bolt.bin ~ Origin +(1|PopID/Mom), family=binomial,data=modeldata)
model2<-lmer(bolt.bin ~ Origin +(1|PopID), family=binomial,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(bolt.bin ~ Origin +(1|blank), family=binomial,data=modeldata) # Test population effect
anova(model2,model1) # mom sig
anova(model2,model3) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelO<-lmer(bolt.bin ~ (1|blank), family=binomial,data=modeldata)
anova(modelO,model3) #test for significance of origin - origin sig!
model3

int<--2.6742 #inv mean
B<-1.6649 #Originnat estimate from model summary
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

####Drought#####no lf/shoot/root measures
#######Drought, death######
str(d)
modeldata<-d[!is.na(d$Death),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

model1<-lmer(Death ~ Origin * LfLgth1+(LfLgth1|PopID/Mom), family=poisson,data=modeldata)
model2<-lmer(Death ~ Origin * LfLgth1+(LfLgth1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Death ~ Origin * LfLgth1+(LfLgth1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2,model1) # mom sig
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelInt<-lmer(Death ~ Origin + LfLgth1+(LfLgth1|PopID), family=poisson,data=modeldata)
anova(modelInt,model1) #test for significant interaction btw Origin and Bolted - not sig

modelL0<-lmer(Death ~ Origin + (1|PopID), family=poisson,data=modeldata)
modelL1<-lmer(Death ~ Origin + LfLgth1+(1|PopID), family=poisson,data=modeldata)
modelL2<-lmer(Death ~ Origin + (LfLgth1|PopID), family=poisson,data=modeldata)
anova(modelL0, modelInt)#test for sig of crown - crown not sig
anova(modelL1, modelInt)#test for sig of bolting - bolting not sig
anova(modelL2, modelInt)#test for sig of bolting - bolting not sig

modelO<-lmer(Death ~ LfLgth1+(1|PopID), family=poisson,data=modeldata)
anova(modelO,modelL1) #test for significance of origin - origin not sig!

#######Drought, first wilt######
str(d)
modeldata<-d[!is.na(d$Wilt),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

model1<-lmer(Wilt ~ Origin * LfLgth1+(LfLgth1|PopID/Mom), family=poisson,data=modeldata)
model2<-lmer(Wilt ~ Origin * LfLgth1+(LfLgth1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Wilt ~ Origin * LfLgth1+(LfLgth1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2,model1) # mom sig
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelInt<-lmer(Wilt ~ Origin + LfLgth1+(LfLgth1|PopID), family=poisson,data=modeldata)
anova(modelInt,model2) #test for significant interaction btw Origin and Bolted - not sig

modelL0<-lmer(Wilt ~ Origin + (1|PopID), family=poisson,data=modeldata)
modelL1<-lmer(Wilt ~ Origin * LfLgth1+(1|PopID), family=poisson,data=modeldata)
modelL2<-lmer(Wilt ~ Origin + (LfLgth1|PopID), family=poisson,data=modeldata)
anova(modelL0, model2)#test for sig of size, approx by lf length
anova(modelL1, model2)#test for sig of size
anova(modelL2, model2)#test for sig of size

modelO<-lmer(Wilt ~ LfLgth1+(1|PopID), family=poisson,data=modeldata)
anova(modelO,modelL1) #test for significance of origin - origin  sig!
modelL1
int<-3.05600
#inv mean
B<--0.92757
#Originnat estimate from model summary
pI<-exp(int)
pN<-exp(int+B)
pI
pN
summary(d[d$Origin=="inv",]$Wilt)
summary(d[d$Origin=="nat",]$Wilt)

######Drought, tot wilt####
modeldata<-d[!is.na(d$TotWilt),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

model1<-lmer(TotWilt ~ Origin * LfLgth1+(LfLgth1|PopID/Mom), family=poisson,data=modeldata)
model2<-lmer(TotWilt ~ Origin * LfLgth1+(LfLgth1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(TotWilt ~ Origin * LfLgth1+(LfLgth1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2,model1) # mom sig
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelInt<-lmer(TotWilt ~ Origin + LfLgth1+(LfLgth1|PopID), family=poisson,data=modeldata)
anova(modelInt,model1) #test for significant interaction btw Origin and Bolted - not sig

modelL0<-lmer(TotWilt ~ Origin + (1|PopID), family=poisson,data=modeldata)
modelL1<-lmer(TotWilt ~ Origin + LfLgth1+(1|PopID), family=poisson,data=modeldata)
modelL2<-lmer(TotWilt ~ Origin + (LfLgth1|PopID), family=poisson,data=modeldata)
anova(modelL0, modelInt)#test for sig of size, approx by lf length
anova(modelL1, modelInt)#test for sig of size
anova(modelL2, modelInt)#test for sig of size

modelO<-lmer(TotWilt ~ LfLgth1+(1|PopID), family=poisson,data=modeldata)
anova(modelO,modelL1) #test for significance of origin - origin  sig!

#######FLOOD########
####Flood, death####
str(f)
modeldata<-f[!is.na(f$Death),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

model1<-lmer(Death ~ Origin * LfLgth1+(LfLgth1|PopID/Mom), family=poisson,data=modeldata)
model2<-lmer(Death ~ Origin * LfLgth1+(LfLgth1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Death ~ Origin * LfLgth1+(LfLgth1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2,model1) # mom sig
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelInt<-lmer(Death ~ Origin + LfLgth1+(LfLgth1|PopID), family=poisson,data=modeldata)
anova(modelInt,model1) #test for significant interaction btw Origin and Bolted - not sig

modelL0<-lmer(Death ~ Origin + (1|PopID), family=poisson,data=modeldata)
modelL1<-lmer(Death ~ Origin + LfLgth1+(1|PopID), family=poisson,data=modeldata)
modelL2<-lmer(Death ~ Origin + (LfLgth1|PopID), family=poisson,data=modeldata)
anova(modelL0, modelInt)#test for sig of crown - crown not sig
anova(modelL1, modelInt)#test for sig of bolting - bolting not sig
anova(modelL2, modelInt)#test for sig of bolting - bolting not sig

modelO<-lmer(Death ~ LfLgth1+(1|PopID), family=poisson,data=modeldata)
anova(modelO,modelL1) #test for significance of origin - origin not sig!

####Flood, float####
str(f)
modeldata<-f[!is.na(f$FloatDate),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

model1<-lmer(FloatDate ~ Origin * LfLgth1+(LfLgth1|PopID/Mom), family=poisson,data=modeldata)
model2<-lmer(FloatDate ~ Origin * LfLgth1+(LfLgth1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(FloatDate ~ Origin * LfLgth1+(LfLgth1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2,model1) # mom sig
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelInt<-lmer(FloatDate ~ Origin + LfLgth1+(LfLgth1|PopID), family=poisson,data=modeldata)
anova(modelInt,model1) #test for significant interaction btw Origin and Bolted - not sig

modelL0<-lmer(FloatDate ~ Origin + (1|PopID), family=poisson,data=modeldata)
modelL1<-lmer(FloatDate ~ Origin + LfLgth1+(1|PopID), family=poisson,data=modeldata)
modelL2<-lmer(FloatDate ~ Origin + (LfLgth1|PopID), family=poisson,data=modeldata)
anova(modelL0, modelInt)#test for sig of crown - crown not sig
anova(modelL1, modelInt)#test for sig of bolting - bolting not sig
anova(modelL2, modelInt)#test for sig of bolting - bolting not sig

modelO<-lmer(FloatDate ~ (LfLgth1|PopID), family=poisson,data=modeldata)
anova(modelO,modelL2) #test for significance of origin - origin not sig

#########MOM################
####Mom, seedwt###
str(mom)
modeldata<-mom[!is.na(mom$Sdwt.log),]
xtabs(~Origin+SeedAgeYrs, modeldata) # no invasives bolted.........

modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)

model2<-lmer(Sdwt.log ~ Origin +SeedAgeYrs+(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Sdwt.log ~ Origin +SeedAgeYrs+(1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelA<-lmer(Sdwt.log ~ Origin + (1|PopID), family=gaussian, data=modeldata)
anova(modelA, model2)
modelA2<-lmer(Sdwt.log ~ Origin +(SeedAgeYrs|PopID), family=gaussian,data=modeldata)
anova(modelA2,model2)
modelA3<-lmer(Sdwt.log ~ Origin + SeedAgeYrs + (SeedAgeYrs|PopID), family=gaussian,data=modeldata)
anova(modelA2,modelA3)

modelO<-lmer(Sdwt.log ~ (SeedAgeYrs|PopID), family=gaussian,data=modeldata)
anova(modelO,modelA2) #test for significance of origin - origin not sig....?

#raw data, rather than transformed
model2raw<-lmer(SeedWt ~ Origin +SeedAgeYrs+(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(SeedWt ~ Origin +SeedAgeYrs+(1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelAraw<-lmer(SeedWt ~ Origin + (1|PopID), family=gaussian, data=modeldata)
anova(modelAraw, model2raw)
modelA2raw<-lmer(SeedWt ~ Origin +(SeedAgeYrs|PopID), family=gaussian,data=modeldata)
anova(modelA2raw,model2raw)
modelA3raw<-lmer(SeedWt ~ Origin + SeedAgeYrs + (SeedAgeYrs|PopID), family=gaussian,data=modeldata)
anova(modelA2raw,modelA3raw)

modelOraw<-lmer(SeedWt ~ (SeedAgeYrs|PopID), family=gaussian,data=modeldata)
anova(modelOraw,modelA2raw) #test for significance of origin - origin not sig....?

####Mom, germ count###
str(mom)
modeldata<-mom[!is.na(mom$GermCount),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
xtabs(~Origin+SeedAgeYrs, modeldata)

model2<-lmer(GermCount ~ Origin + SeedCount +(1|PopID), family=poisson, data=modeldata)
model3<-lmer(GermCount ~ Origin + SeedCount + (1|blank), family=poisson, data=modeldata)
#anova(model2, model1)
anova(model3, model2)

modelY<-lmer(GermCount ~ Origin + SeedCount + SeedAgeYrs + (1|PopID), family=poisson, data=modeldata)
anova(model2, modelY)

modelS0<-lmer(GermCount ~ Origin + (1|PopID), family=poisson, data=modeldata)
anova(modelS0, model2)

model2W<-lmer(GermCount ~ Origin + SeedCount+SeedWt +(1|PopID), family=poisson, data=modeldata)
model2W1<-lmer(GermCount ~ Origin + SeedCount +(SeedWt|PopID), family=poisson, data=modeldata)
model2W2<-lmer(GermCount ~ Origin + SeedCount+SeedWt +(SeedWt|PopID), family=poisson, data=modeldata)
anova(model2W, model2)
anova(model2W, model2W1)
anova(model2W2, model2W)

modelO<-lmer(GermCount ~ SeedCount +SeedWt+ (SeedWt|PopID), family=poisson, data=modeldata)
anova(modelO, model2W2)

model2W2
int<- -1.33561
#inv mean
B<-0.37899
#Originnat estimate from model summary
pI<-exp(int)
pN<-exp(int+B)
pI
pN

####Mom, germ date####avg, so can't use poisson
str(mom)
modeldata<-mom[!is.na(mom$GermAvgDate),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)

model2<-lmer(GermAvgDate ~ Origin +(SeedAgeYrs|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(GermAvgDate ~ Origin +(SeedAgeYrs|blank), family=gaussian,data=modeldata) # Test population effect
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelA<-lmer(GermAvgDate ~ Origin + (1|PopID), family=, data=modeldata)
anova(modelA, model2)
modelA2<-lmer(GermAvgDate ~ Origin +SeedAgeYrs+(1|PopID), family=gaussian,data=modeldata)
anova(modelA2,model2)
modelA3<-lmer(GermAvgDate ~ Origin + SeedAgeYrs + (SeedAgeYrs|PopID), family=gaussian,data=modeldata)
anova(modelA2,modelA3)

modelW<-lmer(GermAvgDate ~ Origin + SeedWt+(1|PopID), family=, data=modeldata)
anova(modelA,modelW)

modelO<-lmer(GermAvgDate ~ (1|PopID), family=gaussian,data=modeldata)
anova(modelO,modelA) #test for significance of origin - origin not sig....?

