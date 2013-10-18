#Stress Tol CG

library(lattice)
library(gplots)
library(RColorBrewer)
library(preprocessCore)
library(Biobase)
library(limma)
library(pvclust)

source("http://bioconductor.org/biocLite.R")
biocLite("preprocessCore")
library(preprocessCore)

#load spreadsheets
mHdat2<- read.csv(file.choose(), header=T, sep=",", quote='"') #ST measure harvest.csv
write.table(mHdat2, file="STmHarvest.txt", sep="\t", quote=F)

m1dat<- read.csv(file.choose(), header=T, sep=",", quote='"') #ST measure 1.csv
str(m1dat)
head(m1dat)
row.names(m1dat)<-m1dat[,1]
m1dat<-m1dat[,2:10]
write.table(m1dat, file="STm1.txt", sep="\t", quote=F)

m2dat<- read.csv(file.choose(), header=T, sep=",", quote='"') #ST measure 2.csv
str(m2dat)
head(m2dat)
tail(m2dat)
m2dat<-m2dat[1:846,]
row.names(m2dat)<-m2dat[,1]
m2dat<-m2dat[,2:13]
write.table(m2dat, file="STm2.txt", sep="\t", quote=F)

deathdat<- read.csv(file.choose(), header=T, sep=",", quote='"') #ST death.csv
str(deathdat)
#deathdat[deathdat$ID=="#VALUE!",]
head(deathdat)
row.names(deathdat)<-deathdat[,1]
deathdat<-deathdat[,2:30]
write.table(deathdat, file="STdeath.txt", sep="\t", quote=F)

momdat<- read.csv(file.choose(), header=T, sep=",", quote='"') #ST mom dat.csv
str(momdat)
head(momdat)
row.names(momdat)<-momdat[,1]
momdat<-momdat[,1:12]
write.table(momdat, file="STmomdat.txt", sep="\t", quote=F)

#######make stress tables
colnames(m1dat)[7]<-"LfCount1"
colnames(m1dat)[8]<-"LfLgth1"
colnames(m1dat)[9]<-"LfWdth1"

colnames(m2dat)[7]<-"Trt"
colnames(m2dat)[8]<-"LfCount2"
colnames(m2dat)[9]<-"LfLgth2"
colnames(m2dat)[10]<-"LfWdth2"
colnames(m2dat)[11]<-"LfYellow2"
colnames(m2dat)[12]<-"BoltHt2"

colnames(mHdat2)[7]<-"Trt"
colnames(mHdat2)[8]<-"MsrDayH"
colnames(mHdat2)[9]<-"LfCountH"
colnames(mHdat2)[10]<-"LfLgthH"
colnames(mHdat2)[11]<-"LfWdthH"
colnames(mHdat2)[12]<-"BoltedatH"

colnames(deathdat)[8]<-"Wilt"
colnames(deathdat)[6]<-"Trt"
colnames(deathdat)[10]<-"Yellow"
colnames(deathdat)[14]<-"BoltDate"
colnames(deathdat)[16]<-"BCount1"
colnames(deathdat)[20]<-"BCount2"
colnames(deathdat)[24]<-"BCount3"
deathdat$ID<-row.names(deathdat)
levels(deathdat$Trt)
write.table(deathdat, file="STdeath.txt", sep="\t", quote=F)

#control
conth<-mHdat2[mHdat2$Trt=="cont",]
conth$ID<-row.names(conth)
cont2<-m2dat[m2dat$Trt=="control",]
cont2$Trt<-"cont"
cont2$ID<-row.names(cont2)
setdiff(row.names(conth),row.names(cont2))
setdiff(row.names(cont2), row.names(conth))
cont<-merge(conth, cont2, all.x=TRUE, all.y=TRUE)
row.names(cont)<-cont$ID
cont<-merge(cont,m1dat, all.x=TRUE)
head(cont)
contd<-deathdat[deathdat$Trt=="control",]
contd$Trt<-"cont"
row.names(cont)<-cont$ID
setdiff(row.names(cont),row.names(contd))
setdiff(row.names(contd), row.names(cont))
cont<-merge(cont, contd, all.x=TRUE, all.y=TRUE)
row.names(cont)<-cont$ID
cont<-cont[,-c(26:27,32),drop=FALSE]
hcont["US011.8.k",8]<-"US011.13.c"
cont["US011.8.k",4]<-13
cont["US011.8.k",5]<-"c"
cont<-cont[cont$ID!="US011.13.c",]
cont<-cont[cont$ID!="US023.41.d",]
head(cont)
write.table(cont, file="STcontrol.txt", sep="\t", quote=F)

#cut
cuth<-mHdat2[mHdat2$Trt=="cut",]
cuth$ID<-row.names(cuth)
cut2<-m2dat[m2dat$Trt=="cut",]
cut2$ID<-row.names(cut2)
setdiff(row.names(cuth),row.names(cut2))
setdiff(row.names(cut2), row.names(cuth))
cut<-merge(cuth, cut2, all.x=TRUE, all.y=TRUE)
row.names(cut)<-cut$ID
cut<-merge(cut,m1dat, all.x=TRUE)
head(cut)
cutd<-deathdat[deathdat$Trt=="cut",]
setdiff(row.names(cut),row.names(cutd))
setdiff(row.names(cutd), row.names(cut))
cut<-merge(cut, cutd, all.x=TRUE, all.y=TRUE)
row.names(cut)<-cut$ID
cut<-cut[,-c(11:12,16,19:20,26:27,30,36,40,44),drop=FALSE]
cut<-cut[cut$ID!="RU005.5.g",]
cut<-cut[cut$ID!="US021.3.a",]
head(cut)
write.table(cut, file="STcut.txt", sep="\t", quote=F)

#nut def
nuth<-mHdat2[mHdat2$Trt=="nut def",]
nuth$ID<-row.names(nuth)
nut2<-m2dat[m2dat$Trt=="nut def",]
nut2$ID<-row.names(nut2)
setdiff(row.names(nuth),row.names(nut2))
setdiff(row.names(nut2), row.names(nuth))
nut<-merge(nuth, nut2, all.x=TRUE, all.y=TRUE)
row.names(nut)<-nut$ID
nut<-merge(nut,m1dat, all.x=TRUE)
head(nut)
#non-unique rows: 'CA007.2.b', 'UA007.20.d'
nut[nut$ID=="UA007.20.d",]
nut[92,18]<-10
nut[92,19]<-18.3
nut[92,20]<-3.7
nut[92,21]<-"n"
nut[92,23]<-6
nut[92,24]<-13.6
nut[92,25]<-2.2
nut[nut$ID=="CA007.2.b",]
nut[73,18]<-13
nut[73,19]<-15.2
nut[73,20]<-4.3
nut[73,21]<-"n"
nut[73,23]<-10
nut[73,24]<-13
nut[73,25]<-3
nut<-nut[-93,]
nut<-nut[-74,]
row.names(nut)<-nut$ID
nutd<-deathdat[deathdat$Trt=="nut def",]
setdiff(row.names(nut),row.names(nutd))
setdiff(row.names(nutd), row.names(nut))
nut<-merge(nut, nutd, all.x=TRUE, all.y=TRUE)
row.names(nut)<-nut$ID
nut<-nut[,-c(26:27,29:31,36,40,44,47),drop=FALSE]
nut["UA002.15.c",26]<-14
head(nut)
write.table(nut, file="STnutdef.txt", sep="\t", quote=F)

#flood
fld<-deathdat[deathdat$Trt=="flood",]
flh<-mHdat[mHdat$Trt=="flood",]
colnames(flh)[1]<-"FRack"
fld$ID<-row.names(fld)
flh$ID<-row.names(flh)
setdiff(row.names(flh),row.names(fld))
setdiff(row.names(fld), row.names(flh))
flood<-merge(flh, fld, all.x=TRUE, all.y=TRUE)
row.names(flood)<-flood$ID
#dup 'GR003.12.e', 'US012.31.c' 
flood[flood$ID=="GR003.12.e",]
flood[109,18]<-43
flood[109,21]<-7
flood[109,24]<-"F2-26"
flood[109,25]<-50
flood[109,34]<-69
flood[109,35]<-1
flood[109,36]<-39
flood[109,1]<-4
flood[flood$ID=="US012.31.c",]
flood[177,1]<-28
flood[177,18]<-43
flood[177,21]<-7
flood[177,24]<-"F2-42"
flood<-flood[-118,]
flood<-flood[-13,]
row.names(flood)<-flood$ID
flood<-merge(flood, m1dat, all.x=TRUE)
head(flood)
row.names(flood)<-flood$ID
flood<-flood[,-c(19:20,26:29,33,37,40),drop=FALSE]
flood["US023.41.d",32]<-10
flood["US023.41.d",33]<-20
flood["US023.41.d",34]<-5
head(flood)
write.table(flood, file="STflood.txt", sep="\t", quote=F)

#drought
drd<-deathdat[deathdat$Trt=="drought",]
drd$ID<-row.names(drd)
dr<-merge(drd, m1dat, all.x=TRUE)
head(dr)
row.names(dr)<-dr$ID
dr<-dr[,-c(10,12,14:29),drop=FALSE]
dr["UA002.15.c",12]<-"UA002.14.a"
dr["UA002.15.c",13]<-13
dr["UA002.15.c",14]<-16.5
dr["UA002.15.c",15]<-4.2
row.names(dr)<-dr$ID
head(dr)
write.table(dr, file="STdrought.txt", sep="\t", quote=F)

#allo
allodat$ID<-row.names(allodat)
allo1$ID<-row.names(allo1)
allo1<-m1dat[row.names(allodat),]
setdiff(row.names(allodat),row.names(allo1))
setdiff(row.names(allo1), row.names(allodat))
allodat<-merge(allo1,allodat)
head(allodat)
row.names(allodat)<-allodat[,1]
write.table(allodat, file="STallo.txt", sep="\t", quote=F)

#cross check and iterate
intersect(row.names(allodat),row.names(cont))
intersect(row.names(allodat), row.names(cut))
intersect(row.names(allodat), row.names(nut))
intersect(row.names(allodat), row.names(flood))
intersect(row.names(allodat), row.names(dr))

intersect(row.names(cont), row.names(cut))
intersect(row.names(cont), row.names(nut))
intersect(row.names(cont), row.names(flood))
intersect(row.names(cont), row.names(dr))

intersect(row.names(cut), row.names(nut))
intersect(row.names(cut), row.names(flood))
intersect(row.names(cut), row.names(dr))

intersect(row.names(nut), row.names(flood))
intersect(row.names(nut), row.names(dr))

intersect(row.names(flood), row.names(dr))

#open data files
m1dat <- read.table(file.choose(), header=T, sep="\t",quote='"', row.names=1) #measure1
# m2dat <- read.table(file.choose(), header=T, sep="\t",quote='"', row.names=1) #measure2
# mHdat<- read.table(file.choose(), header=T, sep="\t", quote='"', row.names=1) #measure harvest
# deathdat<-read.table(file.choose(), header=T, sep="\t", quote='"', row.names=1) #death and bolt
#des <- read.table(file.choose(), header=T, sep="\t", quote='"', row.names=1) #stresstol design

allodat<-read.table(file.choose(), header=T, sep="\t", quote='"', row.names=1) #allometry"
cont<-read.table(file.choose(), header=T, sep="\t", quote='"', row.names=1) #control
cut<-read.table(file.choose(), header=T, sep="\t", quote='"', row.names=1) #cut
nut<-read.table(file.choose(), header=T, sep="\t", quote='"', row.names=1) #nut def
flood<-read.table(file.choose(), header=T, sep="\t", quote='"', row.names=1) #flood
dr<-read.table(file.choose(), header=T, sep="\t", quote='"', row.names=1) #drought

mom<-read.table(file.choose(), header=T, sep="\t", quote='"', row.names=1) #mom

#Transform datas!
#log transform continuous
#sqrt transform count data
#arcsine sqrt for proportions angularx=57.295*asin(sqrt(x))?
#or for percentages angularx=57.295*asin(sqrt(x/100))
#or maybe logit? logit=log(prop/(1-prop))

#m1dat
m1dat$ID<-row.names(m1dat)
m1dat<-cbind(m1dat,log(m1dat$LfLgth1),log(m1dat$LfWdth1))
colnames(m1dat)[11]<-"LfLgth1.log"
colnames(m1dat)[12]<-"LfWdth1.log"
m1dat<-cbind(m1dat,sqrt(m1dat$LfCount1))
colnames(m1dat)[13]<-"LfCount1.sq"

#allo
head(allodat)
allodat<-cbind(allodat, sqrt(allodat$LfCount1),log(allodat$LfLgth1),log(allodat$LfWdth1),log(allodat$CrownDiam.mmA), 
               log(allodat$ShootMass.gA), log(allodat$RootMass.gA), 57.295*asin(sqrt(allodat$Sm.Rm)))
allodat<-allodat[,1:20] #Sm:Rm is ratio, not proportion...
colnames(allodat)[15]<-"LfCount1.sq"
colnames(allodat)[16]<-"LfLgth1.log"
colnames(allodat)[17]<-"LfWdth1.log"
colnames(allodat)[18]<-"CrownA.log"
colnames(allodat)[19]<-"ShootA.log"
colnames(allodat)[20]<-"RootA.log"

#cont
head(cont)
cont<-cbind(cont, sqrt(cont$LfCount1),log(cont$LfLgth1),log(cont$LfWdth1),sqrt(cont$LfCount2),
            log(cont$LfLgth2),log(cont$LfWdth2),sqrt(cont$LfCountH),log(cont$LfLgthH),log(cont$LfWdthH),
            log(cont$CrownDiam.mm), log(cont$ShootMass.g), log(cont$RootMass.g))
colnames(cont)[41]<-"LfCount1.sq"
colnames(cont)[42]<-"LfLgth1.log"
colnames(cont)[43]<-"LfWdth1.log"
colnames(cont)[44]<-"LfCount2.sq"
colnames(cont)[45]<-"LfLgth2.log"
colnames(cont)[46]<-"LfWdth2.log"
colnames(cont)[47]<-"LfCountH.sq"
colnames(cont)[48]<-"LfLgthH.log"
colnames(cont)[49]<-"LfWdthH.log"
colnames(cont)[50]<-"CrownH.log"
colnames(cont)[51]<-"ShootH.log"
colnames(cont)[52]<-"RootH.log"

#cut
head(cut)
summary(cut)
cut<-cut[,-23]
cut<-cbind(cut, sqrt(cut$LfCount1),log(cut$LfLgth1),log(cut$LfWdth1),sqrt(cut$LfCount2),
            sqrt(cut$LfCountH),log(cut$CrownDiam.mm), log(cut$RootMass.g))
colnames(cut)[36]<-"LfCount1.sq"
colnames(cut)[37]<-"LfLgth1.log"
colnames(cut)[38]<-"LfWdth1.log"
colnames(cut)[39]<-"LfCount2.sq"
colnames(cut)[40]<-"LfCountH.sq"
colnames(cut)[41]<-"CrownH.log"
colnames(cut)[42]<-"RootH.log"

#drought
head(dr)
dr<-cbind(dr, sqrt(dr$LfCount1),log(dr$LfLgth1),log(dr$LfWdth1))
colnames(dr)[16]<-"LfCount1.sq"
colnames(dr)[17]<-"LfLgth1.log"
colnames(dr)[18]<-"LfWdth1.log"

#flood - few harvested...
head(flood)
summary(flood)
flood<-cbind(flood, sqrt(flood$LfCount1),log(flood$LfLgth1),log(flood$LfWdth1),sqrt(flood$LfCountH),
             log(flood$LfLgthH),log(flood$LfWdthH),
             log(flood$CrownDiam.mm), log(flood$ShootMass.g), log(flood$RootMass.g))
colnames(flood)[35]<-"LfCount1.sq"
colnames(flood)[36]<-"LfLgth1.log"
colnames(flood)[37]<-"LfWdth1.log"
colnames(flood)[38]<-"LfCountH.sq"
colnames(flood)[39]<-"LfLgthH.log"
colnames(flood)[40]<-"LfWdthH.log"
colnames(flood)[41]<-"CrownH.log"
colnames(flood)[42]<-"ShootH.log"
colnames(flood)[43]<-"RootH.log"

#nut def
head(nut)
nut<-cbind(nut, sqrt(nut$LfCount1),log(nut$LfLgth1),log(nut$LfWdth1),sqrt(nut$LfCount2),
            log(nut$LfLgth2),log(nut$LfWdth2),sqrt(nut$LfCountH),log(nut$LfLgthH),log(nut$LfWdthH),
            log(nut$CrownDiam.mm), log(nut$ShootMass.g), log(nut$RootMass.g))
colnames(nut)[39]<-"LfCount1.sq"
colnames(nut)[40]<-"LfLgth1.log"
colnames(nut)[41]<-"LfWdth1.log"
colnames(nut)[42]<-"LfCount2.sq"
colnames(nut)[43]<-"LfLgth2.log"
colnames(nut)[44]<-"LfWdth2.log"
colnames(nut)[45]<-"LfCountH.sq"
colnames(nut)[46]<-"LfLgthH.log"
colnames(nut)[47]<-"LfWdthH.log"
colnames(nut)[48]<-"CrownH.log"
colnames(nut)[49]<-"ShootH.log"
colnames(nut)[50]<-"RootH.log"

#mom
head(mom)
summary(mom)
mom<-cbind(mom, log(mom$SeedWt), 57.295*asin(sqrt(mom$GermPercent/100)))
colnames(mom)[13]<-"Sdwt.log"
colnames(mom)[14]<-"GermPerc.arsq"
write.table(mom, file="STmomdat.txt", sep="\t", quote=F)

####subsets of data

#remove small pops (<3)
# summary(d$Pop)
# d<-d[d$Pop!="CA008",]

summary(cont$PopID)
co<-cont[cont$PopID!="US011",]
co<-co[co$PopID!="US013",]
co<-co[co$PopID!="US017",]
co<-co[co$PopID!="US018",]
co<-co[co$PopID!="US019",]
co<-co[co$PopID!="RU002",]
co["CA008.2.b",13]<-"y"
co["TR004.14.d",13]<-"y"
co["BG001.25.a",13]<-"y"
co["GR001.24.e",13]<-"y"
co["TR001.19.f",13]<-"y"
error<-co[co$BoltedatH=="y",]
levels(error$BoltedatH)
dateerror<-co[!is.na(co$BoltDate),]
summary(dateerror)
error<-co[co$BoltedatH=="y",]
summary(error)
fixme<-as.vector(setdiff(row.names(error),row.names(dateerror)))
co["TR001.1.d",]
co["TR001.1.d","BoltedatH"]<-"y"
co["TR001.1.d","BoltDate"]<-86
co["TR001.1.d","BoltMsrDate3"]<-86
co["TR001.1.d","MaxBoltHt3"]<-3.5
co["RO005.14.e",]
co["RO005.14.e",13]<-"y"
co["RO005.14.e","BoltDate"]<-85
co["RO005.14.e","BoltMsrDate3"]<-85
co["RO005.14.e","MaxBoltHt3"]<-35.0
co["GR002.8.j",]
co["GR002.8.j",13]<-"y"
co["GR002.8.j","BoltDate"]<-85
co["GR002.8.j","BoltMsrDate3"]<-85
co["GR002.8.j","MaxBoltHt3"]<-25.0
co["TR001.12.g",]
co["TR001.12.g",13]<-"y"
co["TR001.12.g","BoltDate"]<-85
co["TR001.12.g","BoltMsrDate3"]<-85
co["TR001.12.g","MaxBoltHt3"]<-19
co["TR005.9.f",]
co["TR005.9.f",13]<-"y"
co["TR005.9.f","BoltDate"]<-85
co["TR005.9.f","BoltMsrDate3"]<-85
co["TR005.9.f","MaxBoltHt3"]<-5.0
co["UA004.15.d",]
co["UA004.15.d",13]<-"y"
co["UA004.15.d","BoltDate"]<-85
co["UA004.15.d","BoltMsrDate3"]<-85
co["UA004.15.d","MaxBoltHt3"]<-4.0
co["GR003.17.h",]
co["GR003.17.h",13]<-"y"
co["GR003.17.h","BoltDate"]<-85
co["GR003.17.h","BoltMsrDate3"]<-85
co["GR003.17.h","MaxBoltHt3"]<-14.0
co["BG001.17.f",]
co["BG001.17.f",13]<-"y"
co["BG001.17.f","BoltDate"]<-85
co["BG001.17.f","BoltMsrDate3"]<-85
co["BG001.17.f","MaxBoltHt3"]<-22.5
co["RO002.13.g",]
co["RO002.13.g",13]<-"y"
co["RO002.13.g","BoltDate"]<-85
co["RO002.13.g","BoltMsrDate3"]<-85
co["RO002.13.g","MaxBoltHt3"]<-6.5
co["RO003.4.i",]
co["RO003.4.i",13]<-"y"
co["RO003.4.i","BoltDate"]<-85
co["RO003.4.i","BoltMsrDate3"]<-85
co["RO003.4.i","MaxBoltHt3"]<-5.0
co["BG001.22.f",]
co["BG001.22.f",13]<-"y"
co["BG001.22.f","BoltDate"]<-86
co["BG001.22.f","BoltMsrDate3"]<-86
co["BG001.22.f","MaxBoltHt3"]<-3.5
co["TR001.10.b",]
co["TR001.10.b",13]<-"y"
co["TR001.10.b","BoltDate"]<-86
co["TR001.10.b","BoltMsrDate3"]<-86
co["TR001.10.b","MaxBoltHt3"]<-12.5
co["US003.27.b",]
co["US003.27.b",13]<-"y"
co["US003.27.b","BoltDate"]<-84
co["US003.27.b","BoltMsrDate3"]<-84
co["US003.27.b","MaxBoltHt3"]<-12.5
co["RO003.9.i",]
co["RO003.9.i",13]<-"y"
co["RO003.9.i","BoltDate"]<-84
co["RO003.9.i","BoltMsrDate3"]<-84
co["RO003.9.i","MaxBoltHt3"]<-9.5
co["HU001.24.b",]
co["HU001.24.b",13]<-"y"
co["HU001.24.b","BoltDate"]<-84
co["HU001.24.b","BoltMsrDate3"]<-84
co["HU001.24.b","MaxBoltHt3"]<-22.0
co["GR003.17.e",]
co["GR003.17.e",13]<-"y"
co["GR003.17.e","BoltDate"]<-84
co["GR003.17.e","BoltMsrDate3"]<-84
co["GR003.17.e","MaxBoltHt3"]<-16.9
co["UA001.18.d",]
co["UA001.18.d",13]<-"y"
co["UA001.18.d","BoltDate"]<-84
co["UA001.18.d","BoltMsrDate3"]<-84
co["UA001.18.d","MaxBoltHt3"]<-28.5
co["GR001.26.h",]
co["GR001.26.h",13]<-"y"
co["GR001.26.h","BoltDate"]<-84
co["GR001.26.h","BoltMsrDate3"]<-84
co["GR001.26.h","MaxBoltHt3"]<-10.5
co["RO005.17.e",]
co["RO005.17.e",13]<-"y"
co["RO005.17.e","BoltDate"]<-84
co["RO005.17.e","BoltMsrDate3"]<-84
co["RO005.17.e","MaxBoltHt3"]<-8.0
co["CA001.3.g",]
co["CA001.3.g",13]<-"y"
co["CA001.3.g","BoltDate"]<-85
co["CA001.3.g","BoltMsrDate3"]<-85
co["CA001.3.g","MaxBoltHt3"]<-10.5
co["RO001.21.i",]
co["RO001.21.i",13]<-"y"
co["RO001.21.i","BoltDate"]<-85
co["RO001.21.i","BoltMsrDate3"]<-85
co["RO001.21.i","MaxBoltHt3"]<-22.0
co["RU008.16.f",]
co["RU008.16.f",13]<-"y"
co["RU008.16.f","BoltDate"]<-85
co["RU008.16.f","BoltMsrDate3"]<-85
co["RU008.16.f","MaxBoltHt3"]<-9.0
co["RU004.5.a",]
co["RU004.5.a",13]<-"y"
co["RU004.5.a","BoltDate"]<-85
co["RU004.5.a","BoltMsrDate3"]<-85
co["RU004.5.a","MaxBoltHt3"]<-4.5
co["TR005.9.d",]
co["TR005.9.d",13]<-"y"
co["TR005.9.d","BoltDate"]<-86
co["TR005.9.d","BoltMsrDate3"]<-86
co["TR005.9.d","MaxBoltHt3"]<-8.5
co["RU001.6.h",]
co["RU001.6.h",13]<-"y"
co["RU001.6.h","BoltDate"]<-85
co["RU001.6.h","BoltMsrDate3"]<-85
co["RU001.6.h","MaxBoltHt3"]<-7.5
co["BG001.29.d",]
co["BG001.29.d",13]<-"y"
co["BG001.29.d","BoltDate"]<-85
co["BG001.29.d","BoltMsrDate3"]<-85
co["BG001.29.d","MaxBoltHt3"]<-4.5
error<-co[co$BoltedatH=="n",]
dateerror<-error[!is.na(error$BoltDate),]
summary(dateerror)
levels(co$BoltedatH)
error<-co[co$BoltedatH=="",]
co$BoltedatH<-droplevels(co$BoltedatH)
#fixme<-as.vector(setdiff(row.names(error),row.names(dateerror)))
co<-co[!is.na(co$ID),]
write.table(co, file="STControlsubset.txt", sep="\t", quote=F)
#don't trust bolt max ht, flower number, bolt rate type data. Date and y/n bolt is good though!

summary(m1dat$PopID)
m1<-m1dat[m1dat$PopID!="RU007",]
m1<-m1[m1$PopID!="US019",]
m1<-m1[m1$PopID!="US025",]
write.table(m1, file="STm1subset.txt", sep="\t", quote=F)

summary(allodat$PopID)
al<-allodat[allodat$PopID!="CA007",]
al<-al[al$PopID!="RU002",]
al<-al[al$PopID!="RU004",]
al<-al[al$PopID!="RU007",]
al<-al[al$PopID!="TR005",]
al<-al[al$PopID!="UA002",]
al<-al[al$PopID!="UA006",]
al<-al[al$PopID!="UA007",]
al<-al[al$PopID!="UA008",]
al<-al[al$PopID!="US011",]
al<-al[al$PopID!="US012",]
al<-al[al$PopID!="US013",]
al<-al[al$PopID!="US017",]
al<-al[al$PopID!="US018",]
al<-al[al$PopID!="US019",]
al<-al[al$PopID!="US020",]
al<-al[al$PopID!="US021",]
al<-al[al$PopID!="US023",]
al<-al[al$PopID!="US025",]
al<-al[al$PopID!="US026",]
write.table(al, file="STAllosubset.txt", sep="\t", quote=F)

summary(nut$PopID)
n<-nut[nut$PopID!="UA007",]
n<-n[n$PopID!="US013",]
n<-n[n$PopID!="US026",]
n<-n[row.names(n)!="UA002.14.a",]
error<-n[is.na(n$BoltDate),]
error<-n[n$BoltedatH=="n",]
summary(error)
n["TR003.6.d","BoltedatH"]<-"y"
n["TR003.6.d","BoltDate"]<-78
n["TR003.6.d","BoltMsrDate3"]<-78
n["TR003.6.d","MaxBoltHt3"]<-4.0
n["UA001.18.b",]
n["UA001.18.b","BoltedatH"]<-"y"
n["UA001.18.b","BoltDate"]<-80
n["UA001.18.b","BoltMsrDate3"]<-80
n["UA001.18.b","MaxBoltHt3"]<-5.0
error<-n[n$BoltedatH=="y",]
dateerror<-error[is.na(error$BoltDate),]
dateerror
levels(n$BoltedatH)
n<-n[!is.na(n$ID),]
summary(n$ ShootMass.g)
summary(n$ RootMass.g)
n[n$RootMass.g>10,]
n[n$ShootMass.g>3,]

write.table(n, file="STNutsubset.txt", sep="\t", quote=F)
#don't trust bolt max ht, flower number, bolt rate type data. Date and y/n bolt is good though!

summary(cut$PopID)
cu<-cut[cut$PopID!="RU005",]
cu<-cu[cu$PopID!="US018",]
cu<-cu[cu$PopID!="US021",]
cu$PopID<-droplevels(cu$PopID)
error<-error[is.na(error$BoltDate),]
error<-cu[cu$BoltedatH=="y",]
summary(error)
cu["RU004.6.b",]
cu["RU004.6.b","BoltedatH"]<-"y"
cu["RU004.6.b","BCount1"]<-NA
cu["RU004.6.b","BoltDate"]<-76
cu["RU004.6.b","BoltMsrDate3"]<-76
cu["RU004.6.b","MaxBoltHt3"]<-38.5
cu["GR001.14.l","BoltedatH"]<-"n"
cu["CA007.2.a","BoltDate"]<-77
cu["CA007.2.a","BoltMsrDate3"]<-77
cu["CA007.2.a","MaxBoltHt3"]<-32.5
cu["UA003.9A.a","BoltDate"]<-77
cu["UA003.9A.a","BoltMsrDate3"]<-77
cu["UA003.9A.a","MaxBoltHt3"]<-3
cu["TR003.2.f","BoltDate"]<-77
cu["TR003.2.f","BoltMsrDate3"]<-77
cu["TR003.2.f","MaxBoltHt3"]<-4
cu["CA001.3.j","BoltDate"]<-77
cu["CA001.3.j","BoltMsrDate3"]<-77
cu["CA001.3.j","MaxBoltHt3"]<-6.5
cu["CA008.4.b","BoltDate"]<-76
cu["CA008.4.b","BoltMsrDate3"]<-76
cu["CA008.4.b","MaxBoltHt3"]<-4.0
error<-cu[cu$BoltedatH=="n",]
dateerror<-error[!is.na(error$BoltDate),]
dateerror
cu[row.names(dateerror),"BoltedatH"]<-"y"
error<-cu[cu$BoltedatH=="y",]
dateerror<-error[is.na(error$BoltDate),]
dateerror
levels(cu$BoltedatH)
error<-cu[cu$BoltedatH=="",]
#dead or missing before harvest
cu<-cu[row.names(cu)!="CA007.20.a",]
cu<-cu[row.names(cu)!="US023.41.i",]
cu<-cu[row.names(cu)!="US002.17.c",]
cu<-cu[row.names(cu)!="US011.12.a",]
cu$BoltedatH<-droplevels(cu$BoltedatH)
cu<-cu[!is.na(cu$ID),]
write.table(cu, file="STCutsubset.txt", sep="\t", quote=F)
#don't trust bolt max ht, flower number, bolt rate type data. Date and y/n bolt is good though!

summary(dr$PopID)
d<-dr[dr$PopID!="CA007",]
d<-d[d$PopID!="RU007",]
d<-d[d$PopID!="UA007",]
d<-d[d$PopID!="US013",]
d<-d[d$PopID!="US017",]
d$PopID<-droplevels(d$PopID)
d<-d[!is.na(d$ID),]
summary(d$LfLgth1)
d[d$LfLgth1==4.2,]

write.table(d, file="STDroughtsubset.txt", sep="\t", quote=F)

summary(flood$PopID)
f<-flood[flood$PopID!="CA007",]
f<-f[f$PopID!="US013",]
f<-f[f$PopID!="US018",]
f<-f[f$PopID!="US021",]
f<-f[f$PopID!="RU007",]
f$PopID<-droplevels(f$PopID)
f$Trt<-droplevels(f$Trt)
f$Trt<-droplevels(f$Trt)
f<-f[!is.na(f$ID),]
f[f$BoltedatH!="n",]
write.table(f, file="STFloodsubset.txt", sep="\t", quote=F)

summary(mom)
head(mom[is.na(mom$GermAvgDate),])
summary(mom$PopID)
max(mom$GermPercent)
mom[mom$GermPercent==max(mom$GermPercent),]
mom["RU002.6","SeedCount"]<-5
mom["RU002.6","GermPercent"]<-100
plot(mom$SeedAgeYrs, mom$GermPercent, col=mom$Origin)
plot(mom$SeedAgeYrs, mom$GermPercent, col=mom$PopID)
write.table(mom, file="STMomsubset.txt", sep="\t", quote=F)


####add lat/long to data tables######
lat<-read.table(file.choose(), header=T, sep="\t", quote='"', row.names=1) #popcoord.txt
colnames(lat)[3]<-"PopID"
al<-merge(al,lat, all.x=TRUE)
mom<-merge(mom,lat, all.x=TRUE)
co<-merge(co,lat, all.x=TRUE)
m1<-merge(m1,lat, all.x=TRUE)
n<-merge(n,lat, all.x=TRUE)
cu<-merge(cu,lat, all.x=TRUE)
d<-merge(d,lat, all.x=TRUE)
f<-merge(f,lat, all.x=TRUE)

write.table(mom, file="STMomsubset.txt", sep="\t", quote=F)
write.table(f, file="STFloodsubset.txt", sep="\t", quote=F)
write.table(d, file="STDroughtsubset.txt", sep="\t", quote=F)
write.table(cu, file="STCutsubset.txt", sep="\t", quote=F)
write.table(n, file="STNutsubset.txt", sep="\t", quote=F)
write.table(al, file="STAllosubset.txt", sep="\t", quote=F)
write.table(m1, file="STm1subset.txt", sep="\t", quote=F)
write.table(co, file="STControlsubset.txt", sep="\t", quote=F)
#don't trust bolt max ht, flower number, bolt rate type data. Date and y/n bolt is good though!

################################

#at lat/long to des
m <- within(m, {
  + dms <- do.call(rbind, strsplit(as.character(DMS), ":"))
  + dec <- as.numeric(dms[,1]) + 
    +   (as.numeric(dms[,2]) + as.numeric(dms[,3])/60)/60
  + rm(dms)
  + })
> m




