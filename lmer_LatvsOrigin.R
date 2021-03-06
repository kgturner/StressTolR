###ST mixed FX models, focused on Origin and Latitude###
#Stress Tolerance, REML, using lme4
#mixed effect models 
library(lme4)

#open data files
mom<-read.table(file.choose(), header=T, sep="\t", quote='"', row.names=1) #momsubset
co<-read.table(file.choose(), header=T, sep="\t", quote='"', row.names=1) #controlsubset
m1<-read.table(file.choose(), header=T, sep="\t", quote='"', row.names=1) #m1subset
al<-read.table("STAllosubset.txt", header=T, sep="\t", quote='"', row.names=1) #allosubset
n<-read.table(file.choose(), header=T, sep="\t", quote='"', row.names=1) #nutsubset
cu<-read.table(file.choose(), header=T, sep="\t", quote='"', row.names=1) #cutsubset
d<-read.table(file.choose(), header=T, sep="\t", quote='"', row.names=1) #droughtsubset
f<-read.table(file.choose(), header=T, sep="\t", quote='"', row.names=1) #floodsubset

#for each normal trait, compare this general set of models
model1<-lmer(trait  ~ Origin+ Latitude +(1|PopID/Mom), family=gaussian,data=modeldata)
model2<-lmer(trait  ~ Origin+ Latitude + (1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(trait  ~ Origin+ Latitude + (1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2,model1) # mom is sig!
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
modelL<-lmer(trait  ~ Origin + (1|PopID), family=gaussian,data=modeldata)
anova(modelL, model1)
modelO<-lmer(trait ~ Latitude +(1|PopID), family=gaussian,data=modeldata)
anova(modelO,model1) #test for significance of origin - origin only marginally sig....!

#test for one trait, one df, specify non default family
lfcountLR<- CGtrait.LR("LfCount1",al, family=poisson)

#test for one trait, one df
shootLR<- CGtrait.LR("ShootMass.gA",al, family=gaussian)

#test for one trait, one df
shootmod <- CGtrait.models("ShootMass.gA", al) #test one trait

#for all traits in a df
#make sure all traits analyzed this way are the same distribution
names(al)#find col numbers for traits of interestes
alLR <- lapply(names(al)[8:13],function(n) CGtrait.LR(n,al))#apply func to all things in list
names(alLR) <- names(al)[8:13]
almodels <- lapply(names(al)[8:13],function(n) CGtrait.models(n,al))#apply func to all things in list
names(almodels) <- names(al)[8:13]

#to get one model
almodels[[1]][1] #first number is trait in column order of df, second number is model number
names(almodels[1]) #to verify trait

######Allo, Origin + Lat models######
al<-read.table("STAllosubset.txt", header=T, sep="\t", quote='"', row.names=1) #allosubset
head(al)
alLR <- lapply(names(al)[c(11:13, 20)],function(n) CGtrait.LR(n,al)) #crow, shoot, root, root.log, all gaussian
names(alLR) <- names(al)[c(11:13, 20)]
alLR #check out LRs of models. Model progression logical?
almodels <- CGtrait.models("CrownDiam.mmA",al)
almodels

###allo shoot, mom is sig, do by hand
modeldata<-al[!is.na(al$ShootA.log),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

model1raw<-lmer(ShootMass.gA  ~ Origin+ Latitude +(1|PopID/Mom), family=gaussian,data=modeldata)
model2raw<-lmer(ShootMass.gA  ~ Origin+ Latitude + (1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(ShootMass.gA  ~ Origin+ Latitude + (1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom is sig!
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelL<-lmer(ShootMass.gA  ~ Origin + (1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelL, model1raw)

modelOraw<-lmer(ShootMass.gA ~ Latitude +(1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelOraw,model1raw) #test for significance of origin - origin not sig

###allo, root mass###
modeldata<-al[!is.na(al$RootA.log),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

model1raw<-lmer(RootA.log  ~ Origin + Latitude+(1|PopID/Mom), family=gaussian,data=modeldata)
model2raw<-lmer(RootA.log  ~ Origin + Latitude+ (1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(RootA.log  ~ Origin + Latitude+ (1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom is sig!
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelL<-lmer(RootA.log  ~ Origin + (1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelL, model1raw)

modelOraw<-lmer(RootA.log ~ Latitude + (1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelOraw,model1raw) #test for significance of origin - origin not sig

#####m1, Origin + Lat#####
m1<-read.table("STm1subset.txt", header=T, sep="\t", quote='"', row.names=1) #m1subset
head(m1)
m1<-cbind(m1,lxw=m1$LfLgth1*m1$LfWdth1, lxw.log=log(m1$LfLgth1*m1$LfWdth1))
m1lxw <- CGtrait.LR("lxw", m1)
m1lf <- CGtrait.LR("LfCount1", m1, family=poisson)#poisson distribution

###m1, lxw, mom sig, do by hand###
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

####Control, Origin + Lat####
co<-read.table("STControlsubset.txt", header=T, sep="\t", quote='"', row.names=1) #controlsubset
head(co)
coLR <- lapply(names(co)[c(15:17,52:53)],function(n) CGtrait.LR(n,co)) #crow, shoot, root, RootH.log, lxw, all gaussian
names(coLR) <- names(co)[c(15:17,52:53)]
coLR #check out LRs of models. Model progression logical?

co <- cbind(co, bolt.bin=as.numeric(co$BoltedatH)-1)
write.table(co, file="STControlsubset.txt", sep="\t", quote=F)
coBatH <- CGtrait.LR("bolt.bin", co, family=binomial)
coP <- lapply(names(co)[c(10,27)],function(n) CGtrait.LR(n,co, family=poisson)) #lfcountH, boltdate, all poisson

comodels <- CGtrait.models("RootH.log",co)
comodels

###control, root, lat not sig, do by hand###
modeldata<-co[!is.na(co$RootH.log),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

model1raw<-lmer(RootMass.g ~ Origin + Latitude+(1|PopID/Mom), family=gaussian,data=modeldata)
model2raw<-lmer(RootMass.g ~ Origin + Latitude+(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(RootMass.g ~ Origin + Latitude+(1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig!
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelL<-lmer(RootMass.g ~ Origin +(1|PopID), family=gaussian,data=modeldata)
anova(modelL, model2raw)

modelOraw<-lmer(RootMass.g ~ (1|PopID), family=gaussian,data=modeldata)
anova(modelOraw,modelL) #test for significance of origin - origin NOT sig....! Roots suck

####control, lxw, mom sig, so do by hand####
modeldata<-co[!is.na(co$lxwH),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

model1raw<-lmer(lxwH ~ Origin +Latitude +(1|PopID/Mom), family=gaussian,data=modeldata)
model2raw<-lmer(lxwH ~ Origin +Latitude +(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(lxwH ~ Origin +Latitude +(1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

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

model1raw<-lmer(LfCountH ~ Origin +Latitude +(1|PopID/Mom), family=poisson,data=modeldata)
model2raw<-lmer(LfCountH ~Origin +Latitude +(1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(LfCountH ~ Origin +Latitude +(1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelL<-lmer(LfCountH ~ Origin +(1|PopID/Mom), family=poisson,data=modeldata)
anova(modelL, model1raw)

modelOraw<-lmer(LfCountH ~Latitude+(1|PopID/Mom), family=poisson,data=modeldata)
anova(modelOraw,model1raw) #test for significance of origin - origin NOT sig....!

int<-1.510174
#inv mean
B<--0.217268
#Originnat estimate from model summary
pI<-exp(int)
pN<-exp(int+B)
pI
pN

###control, boltdate, mom sig, do by hand###
#only bolters
modeldata<-co[!is.na(co$BoltDate),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

model1<-lmer(BoltDate ~ Origin + Latitude +(1|PopID/Mom), family=poisson,data=modeldata)
model2<-lmer(BoltDate ~ Origin + Latitude +(1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(BoltDate ~ Origin + Latitude +(1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2,model1) # mom sig
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelL<-lmer(BoltDate ~ Origin + (1|PopID/Mom), family=poisson,data=modeldata)
anova(modelL, model1)

modelO<-lmer(BoltDate ~  (1|PopID/Mom), family=poisson,data=modeldata)
anova(modelO,modelL) #test for significance of origin - origin not sig!

###control boltedatH, mom sig, do by hand, binomial
#all plants, not just bolters
modeldata<-co[!is.na(co$BoltedatH),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$bolt.bin<-as.numeric(modeldata$BoltedatH)-1
str(modeldata$bolt.bin)
modeldata$Mom<-as.factor(modeldata$Mom)

model1<-lmer(bolt.bin ~ Origin + Latitude +(1|PopID/Mom), family=binomial,data=modeldata)
model2<-lmer(bolt.bin ~ Origin + Latitude +(1|PopID), family=binomial,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(bolt.bin ~ Origin + Latitude +(1|blank), family=binomial,data=modeldata) # Test population effect
anova(model2,model1) # mom sig
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelL<-lmer(bolt.bin ~ Origin + (1|PopID/Mom), family=binomial,data=modeldata)
anova(modelL, model1) #are lat and origin different???? sig so yes?

modelO<-lmer(bolt.bin ~ Latitude + (1|PopID/Mom), family=binomial,data=modeldata)
anova(modelO,model1) #test for significance of origin??? origin sig!

#mean estimates, I don't understand.....
int<- 1.88428 #inv mean
B<-2.71785 #Originnat estimate from model summary
# Native
pN<-exp(int+B)/(exp(int+B)+1)
# Introduced (B=0)
pI<-exp(int)/(exp(int)+1)

pI  
pN 
#check
summary(co[co$Origin=="nat",]) #261 rows, 78 boltedatH = 30%
summary(co[co$Origin=="inv",]) #125 rows, 5 boltedatH = 4%

co.mod <- coefficients(with(co,glm(bolt.bin~Origin,family=binomial)))
exp(co.mod[1])/(exp(co.mod[1])+1) 

co.mod <- coefficients(with(co,glm(bolt.bin~Origin,family=binomial)))
exp(sum(co.mod))/(exp(sum(co.mod))+1) 

co.mod <- coefficients(with(co,glm(bolt.bin~Latitude,family=binomial)))
exp(sum(co.mod))/(exp(sum(co.mod))+1) 
with(co,plot(bolt.bin~Latitude))
curve(exp(co.mod[1]+co.mod[2]*x)/(1+exp(co.mod[1]+co.mod[2]*x)),add=TRUE)

str(co)
co.mod
with(co,tapply(bolt.bin,list(Origin,Latitude),mean))
with(co,tapply(bolt.bin,list(Origin),mean))
with(co,tapply(Latitude,list(Origin),mean))

summary(model1)

with(co,aggregate(bolt.bin,list(origin=Origin,latitude=Latitude,mom=Mom,PopID=PopID),length)) # how many moms
with(co,aggregate(bolt.bin,list(origin=Origin,latitude=Latitude,mom=Mom,PopID=PopID),sum)) # how many bolted
means <- with(co,aggregate(bolt.bin,list(origin=Origin,latitude=Latitude,mom=Mom,PopID=PopID),mean)) #mean boltage per mom per pop
popmeans <- with(means,aggregate(x,list(origin=origin,latitude=latitude,pop=PopID),mean))
summary(glm(x~origin+latitude,family=gaussian,data=popmeans))

with(popmeans,plot(x~origin))
tapply(popmeans$x,popmeans$origin,mean)
with(popmeans,plot(x~origin))

with(popmeans,plot(x~latitude))
## without nonbolters
summary(with(subset(popmeans,popmeans$x!=0),lm(x~latitude)))
abline(with(subset(popmeans,popmeans$x!=0),lm(x~latitude)))

with(co,aggregate(bolt.bin,list(origin=Origin,latitude=Latitude,mom=Mom,PopID=PopID),length))

co[which(co$Origin=="nat"&co$PopID=="RO004"),]
summary(co)
mean(co$Latitude)

library(lattice)
xyplot(jitter(bolt.bin)~Latitude,groups=Origin,data=co,auto.key=TRUE)
xyplot(bolt.bin~Latitude|Latitude,groups=Origin,data=co)

plot(with(co,ftable(Origin,Latitude,bolt.bin)))

with(co,table(Origin,Latitude,bolt.bin))

####Nut def, Origin + Lat####
n<-read.table("STNutsubset.txt", header=T, sep="\t", quote='"', row.names=1) #nutsubset
head(n)
# xtabs(~Origin+BoltedatH, modeldata) # no invasives bolted.........
# modeldata<-modeldata[modeldata$BoltedatH!="y",]#remove 7 bolted plants from nat SHOULD I???
nLR <- lapply(names(n)[c(11:12, 15:17, 50:51)],function(x) CGtrait.LR(x,n)) 
#lflgthH, lfwdthH, crown, shoot, root, root.log (pick one!)lxwH, all gaussian
names(nLR) <- names(al)[c(11:12, 15:17, 50:51)]
nLR #check out LRs of models. Model progression logical?
nmodels <- CGtrait.models("RootMass.g",n)
nmodels2 <- CGtrait.models("RootH.log",n)
nmodels
nRoot.lmer <- nmodels$model2
nRootlog.lmer <- nmodels2$model2
qqnorm(resid(nRootlog.lmer), main="Q-Q plot for residuals")
qqline(resid(nRootlog.lmer))


#non-gaussian?
n <- cbind(n, bolt.bin=as.numeric(n$BoltedatH)-1)
write.table(n, file="STNutsubset.txt", sep="\t", quote=F)

#nBatH <- CGtrait.LR("bolt.bin", n, family=binomial)# no invasive bolted!
nlfcount <- CGtrait.LR("LfCountH",n, family=poisson) #lfcountH, all poisson

###nut def, lxwH, harvest, lat not sig, do by hand###
str(n)
#n<-cbind(n,lxwH=n$LfLgthH*n$LfWdthH,lxwH.log=log(n$LfLgthH*n$LfWdthH))
modeldata<-n[!is.na(n$lxwH),]

modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

model1raw<-lmer(lxwH ~ Origin + Latitude +(1|PopID/Mom), family=gaussian,data=modeldata)
model2raw<-lmer(lxwH ~ Origin + Latitude + (1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(lxwH ~ Origin + Latitude + (1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelL <- lmer(lxwH ~ Origin +(1|PopID), family=gaussian,data=modeldata)
anova(modelL, model2raw)

modelOraw<-lmer(lxwH ~ (1|PopID), family=gaussian,data=modeldata)
anova(modelOraw,modelL) #test for significance of origin - origin IS sig..

###nut def, crown, harvest, lat not sig###
str(n)
modeldata<-n[!is.na(n$CrownDiam.mm),]

modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

model1raw<-lmer(CrownDiam.mm ~ Origin + Latitude +(1|PopID/Mom), family=gaussian,data=modeldata)
model2raw<-lmer(CrownDiam.mm ~ Origin +Latitude +(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(CrownDiam.mm ~ Origin +Latitude +(1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelL<-lmer(CrownDiam.mm ~ Origin +(1|PopID), family=gaussian,data=modeldata)
anova(model2raw, modelL)

modelOraw<-lmer(CrownDiam.mm ~ (1|PopID), family=gaussian,data=modeldata)
anova(modelOraw,modelL) #test for significance of origin - origin IS sig.

###nut def, shoot mass, harvest, lat not sig##
str(n)
modeldata<-n[!is.na(n$ShootMass.g),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

model1raw<-lmer(ShootMass.g ~ Origin + Latitude + (1|PopID/Mom), family=gaussian,data=modeldata)
model2raw<-lmer(ShootMass.g ~ Origin + Latitude +(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(ShootMass.g ~ Origin + Latitude +(1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelL <- lmer(ShootMass.g ~ Origin +(1|PopID), family=gaussian,data=modeldata)
anova(modelL, model2raw)

modelOraw<-lmer(ShootMass.g ~ (1|PopID), family=gaussian,data=modeldata)
anova(modelOraw,modelL) #test for significance of origin - origin IS sig....!
modelL

###nut def, lf count, harvest, mom sig, do by hand###
str(n)
modeldata<-n[!is.na(n$LfCountH),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

model1raw<-lmer(LfCountH ~ Origin + Latitude + (1|PopID/Mom), family=poisson,data=modeldata)
model2raw<-lmer(LfCountH ~ Origin + Latitude +(1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(LfCountH ~ Origin + Latitude +(1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelL <- lmer(LfCountH ~ Origin +(1|PopID/Mom), family=poisson,data=modeldata)
anova(modelL, model1raw)

modelOraw<-lmer(LfCountH ~ Latitude +(1|PopID/Mom), family=poisson,data=modeldata)
anova(modelOraw,model1raw) #test for significance of origin - origin NOT sig....!

####Cut, Origin + Lat####
cu<-read.table("STCutsubset.txt", header=T, sep="\t", quote='"', row.names=1) #cutsubset
head(cu)
cuLR <- lapply(names(cu)[c(13:14, 42)],function(n) CGtrait.LR(n,cu)) #crow, root, root.log, all gaussian

#non-gaussian?
cu<- cbind(cu, bolt.bin=as.numeric(cu$BoltedatH)-1)
write.table(cu, file="STCutsubset.txt", sep="\t", quote=F)
cuBatH <- CGtrait.LR("bolt.bin", cu, family=binomial)
cuP <- lapply(names(cu)[c(10,27)],function(n) CGtrait.LR(n,cu, family=poisson)) #lfcountH, boltdate, all poisson

cumodels <- CGtrait.models("RootH.log",cu)
cumodels
qqnorm(resid(cumodels$model2), main="Q-Q plot for residuals")
qqline(resid(cumodels$model2))
shapiro.test(resid(cumodels$model2))


####cut, lf count, harvest, mom is sig, do by hand###
modeldata<-cu[!is.na(cu$LfCountH),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

model1raw<-lmer(LfCountH ~ Origin + Latitude + (1|PopID/Mom), family=poisson,data=modeldata)
model2raw<-lmer(LfCountH ~ Origin + Latitude + (1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(LfCountH ~ Origin + Latitude + (1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelL <- lmer(LfCountH ~ Origin +(1|PopID/Mom), family=poisson,data=modeldata)
anova(modelL,model1raw) 

modelOraw<-lmer(LfCountH ~ Latitude + (1|PopID/Mom), family=poisson,data=modeldata)
anova(modelOraw,model1raw) #test for significance of origin 

###cut, harvest, bolt date, mom is sig, do by hand###
#given that it's bolted....
modeldata<-cu[!is.na(cu$BoltDate),]
xtabs(~Origin + BoltedatH, modeldata)
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

model1<-lmer(BoltDate ~ Origin +Latitude +(1|PopID/Mom), family=poisson,data=modeldata)
model2<-lmer(BoltDate ~ Origin +Latitude+(1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(BoltDate ~ Origin +Latitude+(1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2,model1) # mom sig
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelL<-lmer(BoltDate ~ Origin + (1|PopID/Mom), family=poisson,data=modeldata)
anova(modelL,model1) #test for significant interaction btw Origin and Bolted - not sig

modelO<-lmer(BoltDate ~ Latitude + (1|PopID/Mom), family=poisson,data=modeldata)
anova(modelO,model1) #test for significance of origin - origin not sig!

###cut, bolt.bin, binomial###
modeldata<-cu[!is.na(cu$BoltedatH),]
xtabs(~Origin + BoltedatH, modeldata)
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

model1<-lmer(bolt.bin ~ Origin +Latitude +(1|PopID/Mom), family=binomial,data=modeldata)
model2<-lmer(bolt.bin ~ Origin +Latitude +(1|PopID), family=binomial,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(bolt.bin ~ Origin +Latitude +(1|blank), family=binomial,data=modeldata) # Test population effect
anova(model2,model1) # mom sig
anova(model2,model3) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelL <- lmer(bolt.bin ~ Origin +(1|blank), family=binomial,data=modeldata)
anova(modelL, model3)

modelO<-lmer(bolt.bin ~ (1|blank), family=binomial,data=modeldata)
anova(modelO,modelL) #test for significance of origin - origin sig!
modelL

int<--2.6742 #inv mean
B<-1.6648 #Originnat estimate from model summary
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

####Drought, Origin + Lat####
d<-read.table("STDroughtsubset.txt", header=T, sep="\t", quote='"', row.names=1) #droughtsubset
head(d)
#no gaussian
dLR <- lapply(names(d)[8:10],function(n) CGtrait.LR(n,d, family=poisson)) #wilt, totwilt, death, all poisson
names(dLR) <- names(d)[8:10]

dmodels <- lapply(names(d)[8:9],function(n) CGtrait.models(n,d))
dmodels

int<-10.69128#inv mean
B<-0.28995#Originnat estimate from model summary
pI<-exp(int)
pN<-exp(int+B)
pI
pN
summary(d[d$Origin=="inv",]$TotWilt)
summary(d[d$Origin=="nat",]$TotWilt)

####Flood, Origin + Lat####
f<-read.table("STFloodsubset.txt", header=T, sep="\t", quote='"', row.names=1) #floodsubset
head(f)
#no gaussian
fLR <- lapply(names(f)[20:21],function(n) CGtrait.LR(n,f, family=poisson)) #wilt, totwilt, death, all poisson
names(fLR) <- names(f)[20:21]

fmodels <- lapply(names(f)[20:21],function(n) CGtrait.models(n,f))
fmodels

####Mom, Origin + Lat####
mom<-read.table("STMomsubset.txt", header=T, sep="\t", quote='"', row.names=1) #momsubset
head(mom)
# momLR <- lapply(names(mom)[c(5:6,13, 17)],function(n) CGtrait.LR(n,mom)) #can't use func, because mom doesn't have Mom
#seedwt, germ avg date, sdwt.log (pick one!), avggermdate.log(I don't think this is the right transf...) all gaussian

# names(alLR) <- names(al)[11:13]
# alLR #check out LRs of models. Model progression logical?
# almodels <- CGtrait.models("CrownDiam.mmA",al)
# almodels

str(mom)
modeldata<-mom[!is.na(mom$Sdwt.log),]
# xtabs(~Origin+SeedAgeYrs, modeldata)

modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)

model2<-lmer(Sdwt.log ~ Origin +Latitude+(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Sdwt.log ~ Origin +Latitude+(1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelL<-lmer(Sdwt.log ~ Origin + (1|PopID), family=gaussian, data=modeldata)
anova(modelL, model2)

modelO<-lmer(Sdwt.log ~ (1|PopID), family=gaussian,data=modeldata)
anova(modelO,modelL) #test for significance of origin - origin not sig....?

####Mom, germ count###
str(mom)
modeldata<-mom[!is.na(mom$GermCount),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)

model2<-lmer(GermCount ~ Origin + Latitude+ SeedCount +(1|PopID), family=poisson, data=modeldata)
model3<-lmer(GermCount ~ Origin + Latitude+SeedCount + (1|blank), family=poisson, data=modeldata)
anova(model3, model2)

modelL<-lmer(GermCount ~ Origin +SeedCount+ (1|PopID), family=poisson, data=modeldata)
anova(modelL, model2)

modelO<-lmer(GermCount ~ SeedCount + (1|PopID), family=poisson, data=modeldata)
anova(modelO, modelL)

modelL
int<- -0.42767
#inv mean
B<-0.52217
#Originnat estimate from model summary
pI<-exp(int)
pN<-exp(int+B)
pI
pN

####Mom, germ date####avg, so can't use poisson
str(mom)
modeldata<-mom[!is.na(mom$AvgGermDate.log),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)

model2<-lmer(AvgGermDate.log ~ Origin +Latitude+(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(AvgGermDate.log ~ Origin +Latitude+(1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelL<-lmer(AvgGermDate.log ~ Origin +(1|PopID), family=, data=modeldata)
anova(modelL,model2)

modelO<-lmer(AvgGermDate.log ~ (1|PopID), family=gaussian,data=modeldata)
anova(modelO,modelL) #test for significance of origin - origin not sig....?

modelL