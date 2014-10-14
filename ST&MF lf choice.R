# Leaf choice

######ST######
#arrange data
# lfchoice<- read.csv(file.choose(), header=T, sep=",", quote='"') #ST lf choice.csv
# str(lfchoice)
# head(lfchoice)
# #row.names(lfchoice)<-paste(lfchoice$Round, "_", lfchoice$defense)
# lfchoice<-lfchoice[,1:10]
lfdisc<-read.csv(file.choose(), header=T, sep=",", quote='"') #ST lf disc.csv
head(lfdisc)
# lfdisc<-lfdisc[,1:13]
write.table(lfdisc, file="STlfchoice.txt", sep="\t", quote=F)
lfdisc<-read.table(file.choose(), header=T, sep="\t", quote='"', row.names=1) #lfdisc
#transform
#lfdisc
head(lfdisc)
lfdisc<-cbind(lfdisc, 57.295*asin(sqrt(lfdisc$InvEat)),57.295*asin(sqrt(lfdisc$NatEat)))
colnames(lfdisc)[18]<-"InvEat.arsq"
colnames(lfdisc)[19]<-"NatEat.arsq"
#subset
summary(lfdisc)
head(lfdisc)
head(lfdisc[lfdisc$MostEat=="",])

#lfdisc[lfdisc$MostEat.Scan=="tie",]$MostEat.Scan<-"inv"
lfdisc$MostEat.Scan <- "tie"
lfdisc$MostEat.Scan <- ifelse(lfdisc$EatDiffI.N <0,
                              "inv", lfdisc$MostEat.Scan)
lfdisc$MostEat.Scan <- ifelse(lfdisc$EatDiffI.N >0 ,
                              "nat", lfdisc$MostEat.Scan)
lfdisc$MostEat.Scan<-as.factor(lfdisc$MostEat.Scan)
#lfdisc$MostEat.Scan <- factor(with(lfdisc, (EatDiffI.N<0)*2 + (EatDiffI.N>0)*1),
levels=1:3, labels=c("tie","inv","nat"))
summary(lfdisc$MostEat.Scan)
lfdisc$EatenRatioI.N<-lfdisc$InvEat/lfdisc$NatEat
lfdisc$Eatenlog<-log(lfdisc$EatenRatioI.N)
plot(lfdisc$Eatenlog)
abline(h=0)
row.names(lfdisc)<-paste(lfdisc$defense,lfdisc$Round, lfdisc$TrayID)
lfdisc$eat.bin<-as.numeric(lfdisc$MostEat.Scan)-1
str(lfdisc)

#wide to long
lfdiscI<-lfdisc[,-c(10:15,19)]
lfdiscN<-lfdisc[,-c(4:9,18)]
str(lfdiscI)
colnames(lfdiscI)[4]<-"PopID"
colnames(lfdiscI)[5]<-"mom.indiv"
colnames(lfdiscI)[6]<-"Mom"
colnames(lfdiscI)[7]<-"Indiv"
colnames(lfdiscI)[8]<-"Remains"
colnames(lfdiscI)[9]<-"Eaten"
colnames(lfdiscI)[12]<-"Eaten.arsq"
lfdiscI$"def.round.tray"<-row.names(lfdiscI)
summary(lfdiscI)
head(lfdiscI)
row.names(lfdiscI)<-NULL
lfdiscI$Origin<-"inv"

str(lfdiscN)
colnames(lfdiscN)[4]<-"PopID"
colnames(lfdiscN)[5]<-"mom.indiv"
colnames(lfdiscN)[6]<-"Mom"
colnames(lfdiscN)[7]<-"Indiv"
colnames(lfdiscN)[8]<-"Remains"
colnames(lfdiscN)[9]<-"Eaten"
colnames(lfdiscN)[12]<-"Eaten.arsq"
lfdiscN$"def.round.tray"<-row.names(lfdiscN)
summary(lfdiscN)
head(lfdiscN)
row.names(lfdiscN)<-NULL
lfdiscN$Origin<-"nat"
lf<-rbind(lfdiscI, lfdiscN)
lf$def.round.tray.or<-paste(lf$def.round.tray,lf$Origin)
lfcon<-lf[lf$defense=="const.",]
lfind<-lf[lf$defense=="induced",]

lfcon$defense<-"const"
lfind$defense<-"ind"
colnames(lfcon)[8]<-"Remains.pro"
colnames(lfcon)[9]<-"Eaten.pro"
lfcon$Eaten.mm <- lfcon$Eaten.pro*pi*(7.7/2)^2
colnames(lfind)[8]<-"Remains.pro"
colnames(lfind)[9]<-"Eaten.pro"
lfind$Eaten.mm<-lfind$Eaten.pro*6.3
lf<-rbind(lfcon, lfind)
lf$defense<-as.factor(lf$defense)
lf$Origin<-as.factor(lf$Origin)
lf$eat.bin<-as.integer(lf$eat.bin)
lf$Mom<-as.factor(lf$Mom)

lfI<-lf[lf$Origin=="inv",]
lfN<-lf[lf$Origin=="nat",]
lftie<-lf[lf$MostEat.Scan=="tie",]
lfIw<-lfI[lfI$MostEat.Scan=="inv",]
lfIl<-lfI[lfI$MostEat.Scan=="nat",]
lfNw<-lfN[lfN$MostEat.Scan=="nat",]
lfNl<-lfN[lfI$MostEat.Scan=="inv",]
lfIw$eat.bin<-1
lfIl$eat.bin<-0
lfNw$eat.bin<-1
lfNl$eat.bin<-0
lf2<-rbind(lfIw, lfIl, lfNw, lfNl, lftie)
lf<-lf2
lf$Eaten.log<-log(lf$Eaten.mm+1)
str(lf)
write.table(lf, file="STleafdisclong.txt", sep="\t", quote=F)
#add lat/long
lat<-read.table(file.choose(), header=T, sep="\t", quote='"', row.names=1) #popcoord.txt
colnames(lat)[3]<-"PopID"
lf<-merge(lf,lat, all.x=TRUE)

#fix eaten.mm, change eaten.log
#subset(lf,subset=defense=="const", select=Eaten.mm) <- lf$Eaten.pro*pi*(7.7/2)^2
lfcon<-lf[lf$defense=="const",]
lfind<-lf[lf$defense=="ind",]
lfcon$Eaten.mm <- lfcon$Eaten.pro*pi*(7.7/2)^2
lfind$Eaten.mm<-lfind$Eaten.pro*pi*(6.3/2)^2
lf<-rbind(lfcon, lfind)
lf$Eaten.log<-log(lf$Eaten.mm+1)

#fix mosteat.scan: inv-nat>0 means inv most eaten
lf$MostEat.Scan <- "tie"
lf$MostEat.Scan <- ifelse(lf$EatDiffI.N <0,
                              "nat", lf$MostEat.Scan)
lf$MostEat.Scan <- ifelse(lf$EatDiffI.N >0 ,
                              "inv", lf$MostEat.Scan)
lf$MostEat.Scan<-as.factor(lf$MostEat.Scan)
#also eat.bin
lf$eat.bin <- 2
lfI<-lf[lf$Origin=="inv",]
lfN<-lf[lf$Origin=="nat",]
lftie<-lf[lf$MostEat.Scan=="tie",]
lfIw<-lfI[lfI$MostEat.Scan=="inv",]
lfIl<-lfI[lfI$MostEat.Scan=="nat",]
lfNw<-lfN[lfN$MostEat.Scan=="nat",]
lfNl<-lfN[lfI$MostEat.Scan=="inv",]
lfIw$eat.bin<-1
lfIl$eat.bin<-0
lfNw$eat.bin<-1
lfNl$eat.bin<-0
lf<-rbind(lfIw, lfIl, lfNw, lfNl, lftie)

write.table(lf, file="STleafdisclong.txt", sep="\t", quote=F)

#######MF############
#arrange data

###leaf choice####
mflf<- read.csv(file.choose(), header=T, sep=",", quote='"') #MFlfchoice.csv
str(mflf)
row.names(mflf)<-paste(mflf$defense,mflf$Round,mflf$TrayPos)
mflf$EatDiffI.N<-mflf$Leaten.mm-mflf$Reaten.mm ### const has no scan data...
mflf<-mflf[,-19]
tail(mflf)
summary(mflf$EatDiffI.N)
levels(mflf$MostEat)
mflf$MostEat.Scan <- "tie"
mflf$MostEat.Scan <- ifelse(mflf$EatDiffI.N <0,
                            "nat", mflf$MostEat.Scan)
mflf$MostEat.Scan <- ifelse(mflf$EatDiffI.N >0 ,
                            "inv", mflf$MostEat.Scan)
mflf$MostEat.Scan<-as.factor(mflf$MostEat.Scan)
summary(mflf$MostEat.Scan)
mflf$MostEat.Scan<-as.character(mflf$MostEat.Scan)
mflf$MostEat.Scan <- ifelse(mflf$MostEat %in% "sk" ,
                            "sk", mflf$MostEat.Scan)
mflf$MostEat.Scan <- ifelse(mflf$MostEat %in% "NA (SK left)" ,
                            "sk", mflf$MostEat.Scan)
mflf$MostEat.Scan<-as.factor(mflf$MostEat.Scan)
summary(mflf$MostEat.Scan)
levels(mflf$MostEat.Scan)
#fix in excel????
write.table(mflf, file="MFlfchoice all.txt", sep="\t", quote=F)
mflf2<-read.table(file.choose(), header=T, sep="\t", quote='"', row.names=1) #mflf after excel fix
head(mflf2)
mflf<-mflf2
mflf$eat.bin<-as.numeric(mflf$MostEat.Scan)-1
write.table(mflf, file="MFlfchoice all2.txt", sep="\t", quote=F)
#diffusa only
levels(mflf$Lorigin)
levels(mflf$Rorigin)
mflf[mflf$Rorigin=="",]
mflf["Induced 1 6 82","Rorigin"]<-"SK"
mfld<-mflf[mflf$Lorigin=="inv",]
mfld<-mfld[mfld$Rorigin=="nat",]
#wide to long
mfldI<-mfld[,-c(11:17)]
mfldN<-mfld[,-c(4:10)]
str(mfldI)
head(mfldI)
colnames(mfldI)[4]<-"PopID"
colnames(mfldI)[5]<-"Barcode"
colnames(mfldI)[6]<-"CrossNum"
colnames(mfldI)[7]<-"Indiv"
colnames(mfldI)[8]<-"Origin"
colnames(mfldI)[9]<-"Eaten.mm"
colnames(mfldI)[10]<-"Rem.pro"
summary(mfldI)
mfldI$def.ro.tray.or<-paste(mfldI$defense,mfldI$Round,mfldI$TrayPos, mfldI$Origin)
head(mfldI)
row.names(mfldI)<-NULL
str(mfldI)
# levels(mfldI$Origin)
# mfldI$Origin=="SK"
str(mfldN)
colnames(mfldN)[4]<-"PopID"
colnames(mfldN)[5]<-"Barcode"
colnames(mfldN)[6]<-"CrossNum"
colnames(mfldN)[7]<-"Indiv"
colnames(mfldN)[8]<-"Origin"
colnames(mfldN)[9]<-"Eaten.mm"
colnames(mfldN)[10]<-"Rem.pro"
mfldN$def.ro.tray.or<-paste(mfldN$defense,mfldN$Round,mfldN$TrayPos, mfldN$Origin)
summary(mfldN)
head(mfldN)
row.names(mfldN)<-NULL
# mfldN$Origin<-"nat"
mfl<-rbind(mfldI, mfldN)
str(mfl)
tail(mfl)
mfl$eat.bin<-as.integer(mfl$eat.bin)
mfl$CrossNum<-as.factor(mfl$CrossNum)
mfl$Origin<-droplevels(mfl$Origin)
mfl$PopID<-droplevels(mfl$PopID)
mfl$MostEat.Scan<-droplevels(mfl$MostEat.Scan)
xtabs(~Origin + MostEat.Scan,mfl)
summary(mfl$MostEat.Scan)
lfI<-mfl[mfl$Origin %in% "inv",]
lfN<-mfl[mfl$Origin %in% "nat",]
lftie<-mfl[mfl$MostEat.Scan %in% "tie",]
lfIw<-lfI[lfI$MostEat.Scan %in% "inv",]
lfIl<-lfI[lfI$MostEat.Scan %in% "nat",]
lfNw<-lfN[lfN$MostEat.Scan %in% "nat",]
lfNl<-lfN[lfI$MostEat.Scan %in% "inv",]
lfIw$eat.bin<-1
lfIl$eat.bin<-0
lfNw$eat.bin<-1
lfNl$eat.bin<-0
mfl2<-rbind(lfIw, lfIl, lfNw, lfNl, lftie)
mfl<-mfl2
mfl$Eaten.log<-log(mfl$Eaten.mm+1)
str(mfl)
mfl$Barcode<-droplevels(mfl$Barcode)
#get data from mfm1
levels(unique(mfl$Barcode))
leafm1<-mfm1[mfm1$Barcode %in% unique(mfl$Barcode),]
str(leafm1)
str(mfl)
setdiff(levels(unique(mfl$Barcode)),leafm1$Barcode)
setdiff(leafm1$Barcode, levels(unique(mfl$Barcode)))
mfl2<-merge(leafm1, mfl, by="Barcode")
head(mfl2)
mfl<-mfl2[,-c(17:20)]
colnames(mfl)[2]<-"PopID"
colnames(mfl)[3]<-"CrossNum"
colnames(mfl)[4]<-"Indiv"
colnames(mfl)[5]<-"Origin"
head(mfl)
write.table(mfl, file="MFleafchoicelong.txt", sep="\t", quote=F)
###
#log plot of ratio?
# lfdisc$EatenRatioI.N<-lfdisc$InvEat/lfdisc$NatEat
# lfdisc$Eatenlog<-log(lfdisc$EatenRatioI.N)
# plot(lfdisc$Eatenlog)
# abline(h=0)

#add lat/long
lat<-read.table(file.choose(), header=T, sep="\t", quote='"', row.names=1) #popcoord.txt
colnames(lat)[3]<-"PopID"
mfl<-read.table(file.choose(), header=T, sep="\t", quote='"', row.names=1) #MFleafchoicelong.txt, dk only
mfl<-merge(mfl,lat, all.x=TRUE)

write.table(mfl, file="MFleafchoicelong.txt", sep="\t", quote=F)
mfl<-read.table(file.choose(), header=T, sep="\t", quote='"', row.names=1) #MFleafchoicelong.txt, dk only

#####MERGE ST AND MF###########
#FOR ULTIMATE POWER#
#tidy columns
lf<-subset(lf,select=-c(mom.indiv,Eaten.arsq, EatenRatioI.N, Eatenlog,def.round.tray, def.round.tray.or))
lf$Exp<-"st"
colnames(lf)[4]<-"TrayPos"#changed from TrayID
colnames(lf)[7]<-"Rem.pro"#changed from Remains.pro

mfl$Exp<-droplevels(mfl$Exp)
mfl$Trt<-droplevels(mfl$Trt)
mfl <- subset(mfl, select=-c(LfCount1, LfLgth1, LfWdth1, comments,def.ro.tray.or))

totlf<-merge(lf,mfl, all=TRUE )
subset(totlf, subset=PopID=="")
totlf<-totlf[totlf$PopID!="",]
write.table(totlf, file="ST&MFleaf.txt", sep="\t", quote=F)

#######LEAF DISC ANALYSIS#########
#using lmer
library(lme4)
####ST lf disc, most eaten, from scan, binomial####
str(lf)
modeldata<-lf[lf$eat.bin!=2,]#exclude ties and failed trials
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
xtabs(~Origin+eat.bin, modeldata)
modeldata$Mom<-as.factor(modeldata$Mom)

model1<-lmer(eat.bin ~ Origin +(1|PopID/Mom), family=binomial,data=modeldata)
model2<-lmer(eat.bin ~ Origin +(1|PopID), family=binomial,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(eat.bin ~ Origin +(1|blank), family=binomial,data=modeldata) # Test population effect
anova(model2,model1) # mom sig
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelO<-lmer(eat.bin ~ (1|PopID), family=binomial,data=modeldata)
anova(modelO,model2) #test for significance of origin - origin sig!

####lf disc, area eaten, gaussian####
#modeldata<-lf[!is.na(lf$Eaten.mm),]
modeldata<-lf[lf$eat.bin!=2,]#exclude ties and failed trials
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)


model1<-lmer(Eaten.mm ~ Origin + (1|TrayID) + (1|PopID/Mom), family = gaussian, data=modeldata)
model2<-lmer(Eaten.mm ~ Origin + (1|TrayID)+(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Eaten.mm ~ Origin + (1|TrayID)+(1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2, model1)
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelT<-lmer(Eaten.mm ~ Origin + (1|PopID/Mom), family=gaussian, data=modeldata)
anova(modelT, model1)

modelO<-lmer(Eaten.mm ~ (1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelO,model1) #test for significance of origin - origin not sig....?

#transformed
model1<-lmer(Eaten.log ~ Origin + (1|TrayID) + (1|PopID/Mom), family = gaussian, data=modeldata)
model2<-lmer(Eaten.log ~ Origin + (1|TrayID)+(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Eaten.log ~ Origin + (1|TrayID)+(1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2, model1)
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelT<-lmer(Eaten.log ~ Origin + (1|PopID/Mom), family=gaussian, data=modeldata)
anova(modelT, model1)

modelO<-lmer(Eaten.log ~ (1|PopID/Mom), family=gaussian,data=modeldata)
anova(model1,modelO) #test for significance of origin - origin not sig....?

#######MF LEAF DISC#########
####lf disc, most eaten, from scan, binomial####
str(mfl)
mfl$CrossNum<-as.factor(mfl$CrossNum)
#modeldata<-mfl[mfl$eat.bin!=2,]#exclude sk
modeldata<-mfl[mfl$eat.bin!=3,]#exclude ties and failed trials
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
xtabs(~Origin+eat.bin, modeldata)

model1<-lmer(eat.bin ~ Origin +defense+(1|PopID/CrossNum), family=binomial,data=modeldata)
model2<-lmer(eat.bin ~ Origin +defense+(1|PopID), family=binomial,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(eat.bin ~ Origin +defense+(1|blank), family=binomial,data=modeldata) # Test population effect
anova(model2,model1) # mom sig
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelD0<-lmer(eat.bin ~ Origin +(1|PopID), family=binomial,data=modeldata)
anova(modelD0, model2)

modelO<-lmer(eat.bin ~ (1|PopID), family=binomial,data=modeldata)
anova(modelO,modelD0) #test for significance of origin - origin sig!

####lf disc, area eaten, gaussian####
modeldata<-mfl[!is.na(mfl$Eaten.mm),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)

model1<-lmer(Eaten.mm ~ Origin + defense+(1|TrayPos) + (1|PopID/CrossNum), family = gaussian, data=modeldata)
model2<-lmer(Eaten.mm ~ Origin + defense+(1|TrayPos)+(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Eaten.mm ~ Origin + defense+(1|TrayPos)+(1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2, model1)
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelT<-lmer(Eaten.mm ~ Origin + defense+(1|PopID), family=gaussian, data=modeldata)
anova(modelT, model2)

modelD0<-lmer(Eaten.mm ~ Origin +(1|PopID), family=gaussian,data=modeldata)
anova(modelD0, modelT)

modelO<-lmer(Eaten.mm ~ defense+(1|PopID), family=gaussian,data=modeldata)
anova(modelO,modelT) #test for significance of origin - origin not sig....?

#transformed
model1<-lmer(Eaten.log ~ Origin + (1|TrayID) + (1|PopID/Mom), family = gaussian, data=modeldata)
model2<-lmer(Eaten.log ~ Origin + (1|TrayID)+(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Eaten.log ~ Origin + (1|TrayID)+(1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2, model1)
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelT<-lmer(Eaten.log ~ Origin + (1|PopID/Mom), family=gaussian, data=modeldata)
anova(modelT, model1)

modelO<-lmer(Eaten.log ~ (1|PopID/Mom), family=gaussian,data=modeldata)
anova(model1,modelO) #test for significance of origin - origin not sig....?