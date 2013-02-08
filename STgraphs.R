#ST graphs

#plots of pop means from data, grouped by pop, trt
library("ggplot")
library("ggplot2")

#control harvest mass, crown
str(co)
unique(co$PopID)
co$PopID<-factor(co$PopID, c("CA001","CA007","CA008", "US001", "US002","US003","US012",
                                 "US014","US015", "US020", "US021", "US022", "US023",
                                 "US026","BG001","GR001", "GR002", "GR003", "HU001", "RO001", "RO002", "RO003",
                                 "RO004", "RO005","RU001", "RU003", "RU004", "RU005", "RU008", 
                                 "TR001", "TR003" ,"TR004", "TR005" ,"UA001" ,"UA002", "UA003","UA004",
                                 "UA005", "UA006", "UA008"))
print(levels(co$PopID))

png(filename="STContShootMeans.png", width=1500, bg="white")
p <- ggplot(data=co, aes(PopID, ShootMass.g, fill=Origin)) + 
  geom_boxplot()  
plot(p)
dev.off()

png(filename="STContcrownMeans.png", width=1500, bg="white")
p <- ggplot(data=co, aes(PopID, CrownDiam.mm, fill=Origin)) + 
  geom_boxplot()  
plot(p)
dev.off()

png(filename="STContRootMeans.png", width=1500, bg="white")
p <- ggplot(data=co, aes(PopID, RootMass.g, fill=Origin)) + 
  geom_boxplot()  
plot(p)
dev.off()

#m1, early control, lf traits
str(m1)
print(levels(m1$PopID))
m1$PopID<-factor(m1$PopID, c("CA001","CA007","CA008", "US001", "US002","US003","US011","US012","US013",
                             "US014","US015","US017","US018", "US020", "US021", "US022", "US023",
                             "US026","BG001","GR001", "GR002", "GR003", "HU001", "RO001", "RO002", "RO003",
                             "RO004", "RO005","RU001", "RU002","RU003", "RU004", "RU005", "RU008", 
                             "TR001", "TR003" ,"TR004", "TR005" ,"UA001" ,"UA002", "UA003","UA004",
                             "UA005", "UA006", "UA007","UA008"))

png(filename="STm1LfCountMeans.png", width=1800, bg="white")
p <- ggplot(data=m1, aes(PopID, LfCount1, fill=Origin)) + 
  geom_boxplot()  
plot(p)
dev.off()

png(filename="STm1LfLgthMeans.png", width=1800, bg="white")
p <- ggplot(data=m1, aes(PopID, LfLgth1, fill=Origin)) + 
  geom_boxplot()  
plot(p)
dev.off()

#allo, harvest traits
str(al)
print(levels(al$PopID))
al$PopID<-factor(al$PopID, c("CA001","CA008", "US001", "US002","US003",
                             "US014","US015", "US022", "BG001","GR001", "GR002", "GR003", "HU001", "RO001",
                             "RO002", "RO003","RO004", "RO005","RU001","RU003", "RU005", "RU008", 
                             "TR001", "TR003" ,"TR004", "UA001" , "UA003","UA004",
                             "UA005"))

png(filename="STAlloCrownMeans.png", width=1800, bg="white")
p <- ggplot(data=al, aes(PopID, CrownDiam.mmA, fill=Origin)) + 
  geom_boxplot()  
plot(p)
dev.off()

png(filename="STAlloShootMeans.png", width=1500, bg="white")
p <- ggplot(data=al, aes(PopID, ShootMass.gA, fill=Origin)) + 
  geom_boxplot()  
plot(p)
dev.off()

png(filename="STAlloRootMeans.png", width=1500, bg="white")
p <- ggplot(data=al, aes(PopID, RootMass.gA, fill=Origin)) + 
  geom_boxplot()  
plot(p)
dev.off()

histogram(modeldata$bolt.bin, modeldata$Origin)

plot(d$PopID, d$Wilt, col=d$Origin)
plot(d$PopID,d$LfLgth1, col=d$Origin)
levels(d$PopID)
d$PopID<-factor(d$PopID, levels=c("BG001",  "GR001", "GR002", "GR003", "HU001", "RO001", "RO002", "RO003", "RO004", "RO005",
                                  "RU001", "RU002", "RU003", "RU004", "RU005" ,"RU008" ,"TR001" ,"TR003", "TR004", "TR005", "UA001", "UA002",
                                  "UA003", "UA004", "UA005", "UA006", "UA008", "CA001", "CA008","US001", "US002", "US003", "US011", "US012", "US014" ,"US015",
                                  "US018", "US020", "US021" ,"US022", "US023", "US026"))
qplot(PopID, Wilt, data = d, geom="boxplot", col=Origin)
ggplot(data=d, aes(x=PopID, y=Wilt, group=Origin, colour=Origin)) + geom_line() + geom_point()

#####germ rate
plot(mom$SeedAgeYrs, mom$GermPercent, col=mom$Origin)

####order plots
levels(co$PopID)
co$PopID<-factor(co$PopID, levels=c("BG001",  "GR001", "GR002", "GR003", "HU001", "RO001", "RO002", "RO003", "RO004", "RO005",
                                  "RU001",  "RU003", "RU004", "RU005" ,"RU008" ,"TR001" ,"TR003", "TR004", "TR005", 
                                  "UA001", "UA002","UA003", "UA004", "UA005", "UA006", "UA008", "CA001", "CA007","CA008",
                                  "US001", "US002", "US003", "US012", "US014" ,"US015",
                                   "US020", "US021" ,"US022", "US023", "US026"))
levels(mom$PopID)
mom$PopID<-factor(mom$PopID, levels=c("BG001",  "GR001", "GR002", "GR003", "HU001", "RO001", "RO002", "RO003", "RO004", "RO005",
                                    "RU001", "RU002", "RU003", "RU004", "RU005","RU006", "RU007" ,"RU008" ,"TR001" ,"TR003", "TR004", "TR005", 
                                    "UA001", "UA002","UA003", "UA004", "UA005", "UA006","UA007", "UA008", "CA001", "CA007","CA008",
                                    "US001", "US002", "US003", "US006", "US007", "US008", "US009", "US010", "US011", "US012", "US013","US014" ,"US015",
                                      "US016", "US017", "US018", "US019","US020", "US021" ,"US022", "US023", "US024",
                                      "US025","US026"))
levels(n$PopID)
n$PopID<-factor(n$PopID, levels=c("BG001",  "GR001", "GR002", "GR003", "HU001", "RO001", "RO002", "RO003", "RO004", "RO005",
                                      "RU001", "RU002", "RU003", "RU004", "RU005" ,"RU008" ,"TR001" ,"TR003", "TR004", "TR005", 
                                      "UA001", "UA002","UA003", "UA004", "UA005", "UA006", "UA008", "CA001", "CA007","CA008",
                                      "US001", "US002", "US003",  "US011", "US012", "US014" ,"US015",
                                       "US018", "US020", "US021" ,"US022", "US023"))
levels(cu$PopID)
cu$PopID<-factor(cu$PopID, levels=c("BG001",  "GR001", "GR002", "GR003", "HU001", "RO001", "RO002", "RO003", "RO004", "RO005",
                                      "RU001", "RU002", "RU003", "RU004", "RU008" ,"TR001" ,"TR003", "TR004", "TR005", 
                                      "UA001", "UA002","UA003", "UA004", "UA005", "UA006", "UA008", "CA001", "CA007","CA008",
                                      "US001", "US002", "US003", "US011", "US012", "US014" ,"US015",
                                      "US020", "US022", "US023","US026"))

##############
#tiled trait distribution graphs in ggplot2
#control
p1<-mosaicplot(~ PopID + BoltedatH, data = co, color = TRUE)
mosaicplot(~Origin +BoltedatH, data=co, color=TRUE)

p2<-qplot(PopID, GermPercent, data = mom, col=Origin,size = I(3))
p3<-qplot(PopID, GermAvgDate, data = mom, size = I(3), col=Origin)
multiplot( p2, p3, cols=2)

#stresses
#nut
p1<-qplot(PopID, ShootMass.g, data = n, geom="boxplot", col=Origin, main="Nutrient Deficiency")
p2<-qplot(PopID, RootMass.g, data = n, geom="boxplot", col=Origin, main="Nutrient Deficiency")
multiplot( p1, p2, cols=2)
#cut
p1<-qplot(PopID, RootMass.g, data = cu, geom="boxplot", col=Origin, main="Simulated Herbivory")
p2<-qplot(PopID, BoltDate, data = cu, size = I(3), col=Origin,main="Simulated Herbivory")
p3<-mosaicplot(~ PopID + BoltedatH, data = cu, color = TRUE)
multiplot( p1, p2,  cols=2)
#drought
qplot(PopID,Wilt ,  data = d, size = I(3), col=Origin,main="Drought")

##############################
#checking for normality

names(al)[11:13]
alnorm <- for(i in al[c(11:13, 20)]){
  qqnorm(i)
  qqline(i)
} #crow, shoot, root
histogram(al$RootA.log)
qqnorm(al$RootA.log)#not normal...?
qqline(al$RootA.log)

histogram(m1$lxw)
qqnorm(m1$lxw)
qqline(m1$lxw)

names(co)[c(15:17,52:53)] #crow, shoot, root, root.log,lxw
conorm <- for(i in co[c(15:17,52:53)]){
  qqnorm(i)
  qqline(i)
}#"CrownDiam.mm" "ShootMass.g"  "RootMass.g"   "RootH.log"    "lxwH"

names(n)[c(11:12, 15:17, 50:51)]
nnorm <- for(i in n[c(11:12, 15:17, 50:51)]){
  qqnorm(i)
  qqline(i)
} #"LfLgthH"      "LfWdthH"      "CrownDiam.mm" "ShootMass.g"  "RootMass.g"   "RootH.log"    "lxwH"
# 
histogram(n$RootMass.g)
qqnorm(n$RootMass.g)
qqline(n$RootMass.g)

histogram(n$RootH.log)
qqnorm(n$RootH.log)
qqline(n$RootH.log)

shapiro.test(n$RootH.log)
shapiro.test(n$RootMass.g)
shapiro.test(n[n$Origin=="inv",]$ShootMass.g)
shapiro.test(n$ShootH.log)
shapiro.test(rnorm(100, mean = 5, sd = 3))
qqnorm(rnorm(100, mean = 5, sd = 3))
qqline(rnorm(100, mean = 5, sd = 3))


names(cu)[c(13:14, 42)]
nnorm <- for(i in cu[c(13:14, 42)]){
  qqnorm(i)
  qqline(i)
}#"CrownDiam.mm" "RootMass.g"   "RootH.log"

histogram(cu$RootMass.g)
qqnorm(cu$RootMass.g)
qqline(cu$RootMass.g)
histogram(cu$RootH.log)

names(mom)[c(5:6,13, 17)]
nnorm <- for(i in mom[c(5:6,13, 17)]){
  qqnorm(i)
  qqline(i)
}#"SeedWt"      "GermAvgDate" "Sdwt.log" "AvgGermDate.log"

histogram(mom$GermAvgDate)
qqnorm(mom$GermAvgDate)
qqline(mom$GermAvgDate)

histogram(mom$AvgGermDate.log)# I don't think this is the right transformation....

histogram(mom$SeedWt)
qqnorm(mom$SeedWt)
qqline(mom$SeedWt)

histogram(mom$Sdwt.log)
shapiro.test(mom$Sdwt.log)
shapiro.test(mom$SeedWt)

mom$AvgGermDate.log <- log(mom$GermAvgDate)
write.table(mom, file="STMomsubset.txt", sep="\t", quote=F)

qqnorm(resid(cumodels$model2), main="Q-Q plot for residuals")
qqline(resid(cumodels$model2))
shapiro.test(resid(cumodels$model2))

###########################
#barplot with se bars
se <- function(x) sqrt(var(x, na.rm=TRUE)/(length(na.omit(x))-1))

Hcont2<-h[h$Trt=="control",]
tapply(Hcont2$Shoot.mass.gH, Hcont2$Pop,mean,na.rm=TRUE)
tapply(Hcont2$Shoot.mass.gH, Hcont2$Pop,se)
plt <- barplot(tapply(Hcont2$Shoot.mass.gH, Hcont2$Pop,mean,na.rm=TRUE), ylim=c(0, 30))
y.se <- tapply(Hcont2$Shoot.mass.gH, Hcont2$Pop,se)
y.mean <- tapply(Hcont2$Shoot.mass.gH, Hcont2$Pop, mean, na.rm=TRUE)
# y.mean + y.se
# max(y.mean + y.se)
# c(0, max(y.mean + y.se, na.rm=TRUE))
ylim <- c(0, max(y.mean + y.se, na.rm=TRUE))

png(filename="Frmassbar.png", width=800, bg="white")
x<- barplot(y.mean,ylim=ylim, main="Shoot mass at harvest, control", col="blue")
arrows(x, y.mean - y.se, x, y.mean + y.se,code=3, length=0.03, angle=90)
dev.off()





########################
#qplot(PopID, BoltDay, data = mfco.dk1, geom="boxplot", col=Origin) 