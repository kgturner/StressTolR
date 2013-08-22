#ST figure
#box/whisker plots

#make graphdata

grdat <- merge(co, cu, all=TRUE)
# grdat <- merge(grdat, d, all=TRUE)
# grdat <- merge(grdat, f, all=TRUE)
grdat <- merge(grdat, n, all=TRUE)
# grdat <- merge(grdat, m1, all=TRUE)
gral <- al
grm1 <- m1
gral$RootMass.g <- gral$RootMass.gA
grm1$LfCountH <- grm1$LfCount1
gral$Trt <- "early cont"
grm1$Trt <- "early cont"
grdat <- merge(grdat, gral, all=TRUE)
grdat <- merge(grdat,grm1, all=TRUE)
grdat <- grdat[!is.na(grdat$PopID),]

levels(grdat$Trt)[levels(grdat$Trt)=="cont"] <- "Control"
levels(grdat$Trt)[levels(grdat$Trt)=="cut"] <- "Herbivory"
levels(grdat$Trt)[levels(grdat$Trt)=="nut def"] <- "Nutrient"
levels(grdat$Trt)[levels(grdat$Trt)=="early cont"] <- "Early Control"
# names(grdat)[names(grdat)=="Trt"] <- ""
levels(grdat$Origin)[levels(grdat$Origin)=="inv"] <- "Invasive"
levels(grdat$Origin)[levels(grdat$Origin)=="nat"] <- "Native"
grdat$Trt <- factor(grdat$Trt, c("Early Control", "Control", "Nutrient", "Herbivory"))

##plot size traits
# ggplot(mpg, aes(class, hwy, fill = factor(year)))+
#   geom_boxplot()
# 
# #reorder class according to median(hwy)
# ggplot(mpg, aes(reorder(class, hwy, median), hwy, fill = factor(year)))+
#   geom_boxplot()
head(m1)
# ggplot(m1, aes(Origin, LfCount1, fill=Origin))+geom_boxplot()#two boxes
# ggplot(m1, aes(factor(Rack), LfCount1, fill=Origin))+geom_boxplot()#many boxes
# ggplot(grdat, aes(Trt, CrownDiam.mm, fill=Origin))+geom_boxplot()
# ggplot(grdat, aes(Trt, lxwH, fill=Origin))+geom_boxplot()
# ggplot(grdat, aes(Trt, ShootMass.g, fill=Origin))+geom_boxplot()

###plot in color###
pdf("KTurnerFig2.pdf", useDingbats=FALSE, width=13.38)
# png("STsizebox_color.png",width=800, height = 600, pointsize = 16)
# postscript("KTurnerFig2.eps", horizontal = FALSE, onefile = FALSE, paper = "special", height = 7, width = 13.38)

p1 <- ggplot(grdat,aes(Trt, RootMass.g, fill=Origin))+
  geom_boxplot()+
  xlab("Stress Treatment")+ylab("Root mass [g]")+ 
  theme_bw()+
  theme(legend.justification=c(1,1), legend.position=c(1,1))
#legend position(left/right,top/bottom)
p1 <- p1  + annotate('point',x = "Control", y = 5, pch=16, color="red",parse=T, size=4)+
  annotate('point',x = "Nutrient", y = 5, pch=16, color="red",parse=T, size=4)+
  annotate('point',x = "Herbivory", y = 5, pch=8, color="red",parse=T, size=4)+
  annotate(geom="text", x="Early Control", y=12.5, label="(a)",fontface="bold", size=5)+
  theme(axis.title.x = element_text(size=15, face="bold", vjust=-0.4), 
        axis.title.y = element_text(size=15, face="bold"),axis.text.x = element_text(size=12 ))
# p1

p2 <- ggplot(grdat, aes(Trt, LfCountH, fill=Origin))+geom_boxplot()+xlab("Stress Treatment")+
  ylab("Number of basal leaves")+ theme_bw()+theme(legend.position="none")


p2 <- p2 +  annotate('point',x = "Control", y = 36, pch=8, color="red",parse=T, size=4)+
  annotate('point',x = "Control", y = 38, pch=8, color="red",parse=T, size=4)+
  annotate('point',x = "Control", y = 40, pch=8, color="red",parse=T, size=4)+
  annotate('point',x = "Herbivory", y = 40, pch=8, color="red",parse=T, size=4)+
  annotate(geom="text", x="Early Control", y=80, label="(b)",fontface="bold", size=5)+
  theme(axis.title.x = element_text(size=15, face="bold", vjust=-0.4), 
        axis.title.y = element_text(size=15, face="bold"),axis.text.x = element_text(size=12 ))
# p2

# multiplot(p1,p2, cols=2) #size plots only
multiplot(p1,p2,p3, cols=3) #all st plots, code for p3 LH plot below
dev.off()

# ###plot in bw###
# # col= with( grBatH1, interaction(Origin, Trt, BoltedatH))
# # 
# colors()[c(312,336,350,366,1,176, 345)]
# # colorset <- c("grey51","grey75", "grey51","grey75", "grey51","grey75","white","white","white","white","white","white")
# # cscale = scale_fill_manual(values=colorset)
# 
# pdf("ST size box_bw.pdf", useDingbats=FALSE)
# p1 <- ggplot(grdat,aes(Trt, RootMass.g, fill=Origin))+theme_bw()+
#   geom_boxplot()+
#   xlab("Stress Treatment")+ylab("Root mass (g)")+ 
#   theme(legend.position="none")+
#   scale_fill_manual(values=c("grey51","grey84"))
# p1 <- p1  + annotate('point',x = "Control", y = 5, pch=16, color="black",parse=T, size=4)+
#   annotate('point',x = "Nutrient", y = 5, pch=16, color="black",parse=T, size=4)+
#   annotate('point',x = "Herbivory", y = 5, pch=8, color="black",parse=T, size=4)+
#   theme(axis.title.x = element_text(size=15, face="bold", vjust=-0.4), 
#         axis.title.y = element_text(size=15, face="bold"),axis.text.x = element_text(size=12 ))
# 
# p2 <- ggplot(grdat, aes(Trt, LfCountH, fill=Origin))+ theme_bw()+
#   geom_boxplot()+
#   xlab("Stress Treatment")+ylab("Number of basal leaves")+ 
#   theme(legend.justification=c(0,1), legend.position=c(0,1),
#         legend.title = element_text(size=14, face="bold"),
#         legend.text = element_text(size = 13))+
#   scale_fill_manual(values=c("grey51","grey84"))
# #legend position(left/right,top/bottom)
# # 
# p2 <- p2 +  annotate('point',x = "Control", y = 36, pch=8, color="black",parse=T, size=4)+
#   annotate('point',x = "Control", y = 38, pch=8, color="black",parse=T, size=4)+
#   annotate('point',x = "Control", y = 40, pch=8, color="black",parse=T, size=4)+
#   annotate('point',x = "Herbivory", y = 40, pch=8, color="black",parse=T, size=4)+
#   theme(axis.title.x = element_text(size=15, face="bold", vjust=-0.4), 
#         axis.title.y = element_text(size=15, face="bold"),axis.text.x = element_text(size=12 ))
# 
# multiplot(p1,p2, cols=2)
# dev.off()
# 
# # #outlier check
# # summary(cu$LfCountH)
# # cu[cu$LfCountH==84,]
# # cu[cu$PopID=="RU008",]
# # 
# # summary(n$LfCountH)
# # n[n$LfCountH==72,]
# # 
# # summary(co$LfCountH)
# # co[co$LfCountH==64,]



######plot LH traits#####
# #base graphics
# grdatB$Trt <- droplevels(grdatB$Trt)
# mosaicplot(~Trt +BoltedatH+Origin, data=grdatB, color=TRUE)
# mosaicplot(~BoltedatH+Origin+Trt, data=grdatB, color=TRUE)
# mosaicplot(~Trt/Origin +BoltedatH, data=grdatB, color=TRUE)
# mosaicplot(~Trt/BoltedatH+Origin, data=grdatB, color=TRUE)

#ggplots
#graph data, aggregate by treatment
grdatB <- merge(co, cu, all=TRUE)
# grdat <- merge(grdat, d, all=TRUE)
# grdat <- merge(grdat, f, all=TRUE)
grdatB <- merge(grdatB, n, all=TRUE)

grBatH <- NULL
grBatH

grBatH2 <- ddply(grdatB, .(Trt, Origin), summarize, totcount = length(BoltedatH))
grBatH2$xmax <- cumsum(grBatH2$totcount)
grBatH2$xmin <- grBatH2$xmax-grBatH2$totcount
grBatH3 <- ddply(grdatB, .(Trt, Origin, BoltedatH), summarize, count = length(BoltedatH))
grBatH <- merge(grBatH2,grBatH3, all.y=TRUE)
grBatH$Trt <- factor(grBatH$Trt, c("cont","nut def","cut"))
# grBatH$xmin <- 0
# grBatH$xmax <- 855
# grBatH[1:2,]$xmax<- 125
# grBatH[3:4,]$xmin<- 125
# grBatH[3:4,]$xmax<- 386
# 
# grBatH[8:9,]$xmin<- 386
# grBatH[8:9,]$xmax<- 448
# grBatH[10:11,]$xmin<- 448
# grBatH[10:11,]$xmax<- 594
# 
# grBatH[5,]$xmin<- 594
# grBatH[5,]$xmax<- 675
# grBatH[6:7,]$xmin<- 675
# # grBatH[6:7,]$xmax<- 48

grBatH$Treatment <- paste(grBatH$Trt, grBatH$Origin, grBatH$BoltedatH)

#percentages
grBatHn <- grBatH[grBatH$BoltedatH=="n",]
grBatHn<- ddply(grBatHn, .(Treatment), transform, ymax = cumsum(count/totcount*100))
grBatHn <- ddply(grBatHn, .(Treatment), transform,
                 ymin = ymax-(count/totcount*100))

grBatHy <- grBatH[grBatH$BoltedatH=="y",]
grBatHy<- ddply(grBatHy, .(Treatment), transform, ymax = 100)
grBatHy <- ddply(grBatHy, .(Treatment), transform,
                 ymin = ymax-cumsum(count/totcount*100))

#absolute counts
# grBatH1<- ddply(grBatH, .(Treatment), transform, ymax = cumsum(count))
# grBatH1 <- ddply(grBatH1, .(Treatment), transform,
#                 ymin = ymax-count)

grBatH1 <- merge(grBatHn, grBatHy, all=TRUE)
#ggplot(grBatH1, aes(ymin = ymin, ymax = ymax, xmin=xmin, xmax=xmax,fill=Treatment))+ geom_rect(colour = I("grey"))+ scale_x_continuous(breaks=seq(16,80,32),labels=c("Control", "Herbivory", "Nutrient"))

#labels and tidying
levels(grBatH1$Origin)[levels(grBatH1$Origin)=="inv"] <- "Invasive"
levels(grBatH1$Origin)[levels(grBatH1$Origin)=="nat"] <- "Native"
# grBatH1[grBatH1$xmax==100,]$xmax <- 96
levels(grBatH1$BoltedatH)[levels(grBatH1$BoltedatH)=="n"] <- "Not Bolted"
levels(grBatH1$BoltedatH)[levels(grBatH1$BoltedatH)=="y"] <- "Bolted"
# origins <- c("Invasive", "Native","Invasive", "Native","Invasive", "Native")

# ######colored plot, sample-size in col width######
# 
# # pdf("ST bolted mosaic.pdf", useDingbats=FALSE)
# # p1 <- ggplot(grBatH1, aes(ymin = ymin, ymax = ymax, xmin=xmin, xmax=xmax, fill=Treatment))+ geom_rect(colour = I("grey"), size=1.5)+
# #   scale_x_continuous(breaks=c(125,448,675),labels=c("Control", "Herbivory", "Nutrient"), name="Stress Treatments")+
# #   scale_y_continuous(name="Percent Bolted at Harvest")
# # p1
# # # p1 + annotate(geom="text", x=grBatH1$xmin+8, y=105, label=grBatH1$Origin, size=3) +
# # #   annotate(geom="text", x=grBatH1$xmin+8, y=grBatH1$ymin+2, label=grBatH1$BoltedatH, size=2)+ 
# # #   theme(legend.position="none", axis.title.x = element_text(size=15, face="bold", vjust=-0.4), 
# # #         axis.title.y = element_text(size=15, face="bold"),axis.text.x = element_text(size=15 ))+ 
# # #   annotate('point',x = 16, y = 101, pch=8, color="red",parse=T, size=3)+annotate('point',x = 18, y = 101, pch=8, color="red",parse=T, size=3)+annotate('point',x = 14, y = 101, pch=8, color="red",parse=T, size=3)+
# # #   annotate('point',x = 48, y = 101, pch=8, color="red",parse=T,size=3)+annotate('point',x = 46, y = 101, pch=8, color="red",parse=T,size=3)+annotate('point',x = 50, y = 101, pch=8, color="red",parse=T,size=3)
# # dev.off()
# 
# # col=as.numeric(grBatH1 $Trt) + 3* (as.numeric(grBatH1 $Origin)-1)
# # # alph= 1/as.numeric(grBatH1$BoltedatH)
# # #colorset = c("red","darkred", "green","darkgreen", "blue",   "darkblue")
# # colorset = c("green", "blue", "darkred","darkgreen","darkblue","red")
# # mycolors = colorset[col]
# # grBatH1$colval = mycolors
# # cscale = scale_fill_manual(values=colorset)
# 
# # Or def colors, rather than give alpha, gives factor with 12 levels (one is unused)
# col= with( grBatH1, interaction(Origin, Trt, BoltedatH))
# # define a color set with a set value for each of the levels
# # goes across the top then across the bottom...
# # blue is unused...
# # colorset = c("darkorange",  "darkred",  "darkcyan", "purple",  "darkgreen",
# #              "orange", "red", "cyan","darkorchid1", "blue", "green", "darkblue")
# colors()[c(552,555,503,506,26,30,563,566,96,99,368,371, 48,51, 494, 497, 468, 471, 590, 618)]
# # colorset <- c("red3","orangered3", "darkorchid4","hotpink4","blue4","royalblue4","red","orangered","hotpink1","blue","royalblue1","darkorchid1")
# colorset <- c("chartreuse4","olivedrab4", "darkorchid4","mediumpurple4","steelblue3","royalblue4","chartreuse1","olivedrab1","mediumpurple1","skyblue1","royalblue1","darkorchid1")
# #for order here, going col1 top, bottom, col 2 top, bottom, etc
# #c(2,4,10,12,6,8,1,3,5,7,9) 9 is not used
# # create a special ggplot color scale with your colorset
# cscale = scale_fill_manual(values=colorset)
# # in the ggplot() aes, you will specify fill=factor(col) to get out the right values...
# 
# pdf("ST bolted mosaic_color.pdf", useDingbats=FALSE)
# p1 <- ggplot(grBatH1, aes(ymin = ymin, ymax = ymax, xmin=xmin, xmax=xmax, fill=factor(col)))+
#   geom_rect(colour = I("grey"), size=1.5)+
#   scale_x_continuous(breaks=c(125,448,675),labels=c("Control", "Herbivory", "Nutrient"), name="Stress Treatments") +
#   scale_y_continuous(name="Percent Bolted at Harvest") + cscale
# 
# # annotate 
# p1 + theme(panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank())+
#   annotate(geom="text", x=(grBatH1$xmax-grBatH1$xmin)/2 + grBatH1$xmin, y=105, label=grBatH1$Origin, size=5) +
#   annotate(geom="text", x=(grBatH1$xmax-grBatH1$xmin)/2 + grBatH1$xmin, y=grBatH1$ymin+2, label=grBatH1$BoltedatH, size=4)+ 
#   theme(legend.position="none", axis.title.x = element_text(size=15, face="bold", vjust=-0.4), 
#         axis.title.y = element_text(size=15, face="bold"),axis.text.x = element_text(size=15 ))+ 
#   annotate('point',x = 125, y = 102, pch=8, color="red",parse=T, size=3)+annotate('point',x = 140, y = 102, pch=8, color="red",parse=T, size=3)+annotate('point',x = 110, y = 102, pch=8, color="red",parse=T, size=3)+
#   annotate('point',x = 448, y = 102, pch=8, color="red",parse=T,size=3)+annotate('point',x = 463, y = 102, pch=8, color="red",parse=T,size=3)+annotate('point',x = 433, y = 102, pch=8, color="red",parse=T,size=3)
# dev.off()
# 
# ####may need to paint in textures or something...
# 
# #####black and white plot, sample size in col width#####
# col= with( grBatH1, interaction(Origin, Trt, BoltedatH))
# 
# colors()[c(312,336,350,366,1,176)]
# colorset <- c("grey51","grey84", "grey51","grey84", "grey51","grey84","white","white","white","white","white","white")
# cscale = scale_fill_manual(values=colorset)
# 
# 
# pdf("ST bolted mosaic_bw.pdf", useDingbats=FALSE)
# p1 <- ggplot(grBatH1, aes(ymin = ymin, ymax = ymax, xmin=xmin, xmax=xmax, fill=factor(col)))+
#   geom_rect(colour = I("grey"), size=1.5)+
#   scale_x_continuous(breaks=c(125,448,675),labels=c("Control", "Herbivory", "Nutrient"), name="Stress Treatments") +
#   scale_y_continuous(name="Percent Bolted at Harvest") + theme_bw()+cscale
# # annotate 
# p1 + theme(panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank())+
#   annotate(geom="text", x=(grBatH1$xmax-grBatH1$xmin)/2 + grBatH1$xmin, y=105, label=grBatH1$Origin, size=5) +
#   annotate(geom="text", x=(grBatH1$xmax-grBatH1$xmin)/2 + grBatH1$xmin, y=grBatH1$ymin+2, label=grBatH1$BoltedatH, size=4)+ 
#   theme(legend.position="none", axis.title.x = element_text(size=15, face="bold", vjust=-0.4), 
#         axis.title.y = element_text(size=15, face="bold"),axis.text.x = element_text(size=15 ))+ 
#   annotate('point',x = 125, y = 102, pch=8, color="black",parse=T, size=3)+annotate('point',x = 140, y = 102, pch=8, color="black",parse=T, size=3)+annotate('point',x = 110, y = 102, pch=8, color="black",parse=T, size=3)+
#   annotate('point',x = 448, y = 102, pch=8, color="black",parse=T,size=3)+annotate('point',x = 463, y = 102, pch=8, color="black",parse=T,size=3)+annotate('point',x = 433, y = 102, pch=8, color="black",parse=T,size=3)
# dev.off()
# 
# #####black and white plot, col width standard#####
# col= with( grBatH1, interaction(Origin, Trt, BoltedatH))
# 
# colors()[c(312,336,350,366,1,176)]
# colorset <- c("white","white","white","white","white","white","grey51","grey84", "grey84", "grey51","grey84","grey51")
# cscale = scale_fill_manual(values=colorset)
# 
# grBatHStd <- grBatH1
# grBatHStd$xmin <- c(0,0,20,20,80,100,100,40,40,60,60)
# grBatHStd$xmax <- grBatHStd$xmin + 20
# #reverse stacking, not bolted comes out as white?
# grBatHStd$RevStackymax  <-  grBatHStd$ymax - grBatHStd$ymin
# grBatHStd[grBatHStd$BoltedatH=="Not Bolted",]$RevStackymax  <-  100
# grBatHStd$RevStackymin <- grBatHStd$RevStackymax-grBatHStd$ymax
# grBatHStd[grBatHStd$RevStackymin<0,]$RevStackymin <- 0
# 
# pdf("ST bolted mosaic_bw.pdf", useDingbats=FALSE)
# 
# p1 <- ggplot(grBatHStd, aes(ymin = RevStackymin, ymax = RevStackymax, xmin=xmin, xmax=xmax, fill=factor(col)))+
#   geom_rect(colour = I("black"))+
#   scale_x_continuous(breaks=c(20,60,100),labels=c("Control", "Herbivory", "Nutrient"), name="Stress Treatments") +
#   scale_y_continuous(name="Percent Bolted at Harvest") + theme_bw()+cscale
# p1
# # annotate 
# p1 + theme(panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank())+
#   annotate(geom="text", x=(grBatHStd$xmax-grBatHStd$xmin)/2 + grBatHStd$xmin, y=105, label=grBatHStd$Origin, size=4) +
#   #annotate(geom="text", x=(grBatHStd$xmax-grBatHStd$xmin)/2 + grBatHStd$xmin, y=grBatHStd$ymin+2, label=grBatHStd$BoltedatH, size=4)+ 
#   theme(legend.position="none", axis.title.x = element_text(size=15, face="bold", vjust=-0.4), 
#         axis.title.y = element_text(size=15, face="bold"),axis.text.x = element_text(size=15 ))+ 
#   annotate('point',x = 20, y = 102, pch=8, color="black",parse=T, size=3)+annotate('point',x = 23, y = 102, pch=8, color="black",parse=T, size=3)+annotate('point',x = 17, y = 102, pch=8, color="black",parse=T, size=3)+
#   annotate('point',x = 60, y = 102, pch=8, color="black",parse=T,size=3)+annotate('point',x = 63, y = 102, pch=8, color="black",parse=T,size=3)+annotate('point',x = 57, y = 102, pch=8, color="black",parse=T,size=3)
# 
# dev.off()

#####color plot, rev stack, std col width####

# col= with( grBatH1, interaction(Origin, Trt, BoltedatH))
# 
# colors()[c(312,336,350,366,1,176)]
colorset <- c("white","white","white","white","white","white","#F8766D","#00BFC4", "#00BFC4", "#F8766D","#00BFC4","#F8766D")
cscale = scale_fill_manual(values=colorset)

grBatHStd <- grBatH1
grBatHStd$xmin <- c(0,0,20,20,80,100,100,40,40,60,60)
grBatHStd$xmax <- grBatHStd$xmin + 20
#reverse stacking, not bolted comes out as white?
grBatHStd$RevStackymax  <-  grBatHStd$ymax - grBatHStd$ymin
grBatHStd[grBatHStd$BoltedatH=="Not Bolted",]$RevStackymax  <-  100
grBatHStd$RevStackymin <- grBatHStd$RevStackymax-grBatHStd$ymax
grBatHStd[grBatHStd$RevStackymin<0,]$RevStackymin <- 0

# pdf("ST bolted mosaic_color.pdf", useDingbats=FALSE)
png("STboltedmosaic_color.png",width=800, height = 600, pointsize = 16)

p3 <- ggplot(grBatHStd, aes(ymin = RevStackymin, ymax = RevStackymax, xmin=xmin, xmax=xmax, fill=factor(col)))+
  geom_rect(colour = I("black"))+
  scale_x_continuous(breaks=c(20,60,100),labels=c("Control", "Herbivory", "Nutrient"), name="Stress Treatments") +
  scale_y_continuous(name="Percent Bolted at Harvest") + theme_bw()+cscale
# p3
# annotate 
p3 <- p3 + theme(panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank())+
  #annotate(geom="text", x=(grBatHStd$xmax-grBatHStd$xmin)/2 + grBatHStd$xmin, y=105, label=grBatHStd$Origin, size=4) +
  #annotate(geom="text", x=(grBatHStd$xmax-grBatHStd$xmin)/2 + grBatHStd$xmin, y=grBatHStd$ymin+2, label=grBatHStd$BoltedatH, size=4)+ 
  theme(legend.position="none", axis.title.x = element_text(size=15, face="bold", vjust=-0.4), 
        axis.title.y = element_text(size=15, face="bold"),axis.text.x = element_text(size=12 ))+ 
  annotate('point',x = 20, y = 102, pch=8, color="red",parse=T, size=4)+
  annotate('point',x = 25, y = 102, pch=8, color="red",parse=T, size=4)+
  annotate('point',x = 15, y = 102, pch=8, color="red",parse=T, size=4)+
  annotate('point',x = 60, y = 102, pch=8, color="red",parse=T,size=4)+
  annotate('point',x = 65, y = 102, pch=8, color="red",parse=T,size=4)+
  annotate('point',x = 55, y = 102, pch=8, color="red",parse=T,size=4)+
  annotate(geom="text", x=5, y=102, label="(c)",fontface="bold", size=5)

p3
dev.off()

#######germination trial plot########
levels(mom$Origin)[levels(mom$Origin)=="inv"] <- "Invasive"
levels(mom$Origin)[levels(mom$Origin)=="nat"] <- "Native"

# pdf("GerminationTrial_color.pdf", useDingbats=FALSE)
png("GerminationTrial_color.png", height = 500, width = 500, pointsize = 16)
qplot(data = mom, CollYear, GermPercent, color = Origin, 
      geom = "jitter", xlab = "Collection year",
      ylab = "Germination Rate (%)",
      main = "Germination Trial")

dev.off()


# #orfill=factor(col)
# #plotting 1,3,5 then 2,4,6 because of the levels
# 
# # Or def colors, rather than give alpha, gives factor with 12 levels (one is unused)
# col= with( grBatH1, interaction(Origin, Trt, BoltedatH))
# # define a color set with a set value for each of the levels
# # goes across the top then across the bottom...
# # blue is unused...
# # colorset = c("darkorange",  "darkred",  "darkcyan", "purple",  "darkgreen",
# #              "orange", "red", "cyan","darkorchid1", "blue", "green", "darkblue")
# colors()[c(552,555,503,506,26,30,563,566,96,99,368,371, 48,51, 494, 497, 468, 471)]
# # colorset <- c("red3","orangered3", "darkorchid4","hotpink4","blue4","royalblue4","red","orangered","hotpink1","blue","royalblue1","darkorchid1")
# colorset <- c("chartreuse4","olivedrab4", "darkorchid4","mediumpurple4","blue4","royalblue4","chartreuse1","olivedrab1","mediumpurple1","blue","royalblue1","darkorchid1")
# 
# # create a special ggplot color scale with your colorset
# cscale = scale_fill_manual(values=colorset)
# # in the ggplot() aes, you will specify fill=factor(col) to get out the right values...
# p1 <- ggplot(grBatH1, aes(ymin = ymin, ymax = ymax, xmin=xmin, xmax=xmax, fill=factor(col)))+
#   geom_rect(colour = I("grey"), size=1.5)+
#   scale_x_continuous(breaks=c(125,448,675),labels=c("Control", "Herbivory", "Nutrient"), name="Stress Treatments") +
#   scale_y_continuous(name="Percent Bolted at Harvest") + cscale
# p1
# 
# #chroma?
# grBatH1$chrom <- 100
# grBatH1[grBatH1$Boltedat=="y",]$chrom <- 50
# grBatH1$color <- c(30, 35,45,50,90, 105,110,60,65,75,80)
# # grBatH1 <- rbind(grBatH1, c("nut def", "inv",81, "y",0, 64, 80, "nut def inv y", 100.00000,  100.00000, 10,"#56B4E9"))
# grBatH1$Treatment <- as.factor(grBatH1$Treatment)
# # grBatH1$chrom <- as.integer(grBatH1$chrom)
# # grBatH1$color <- as.factor(grBatH1$color)
# grBatH1
# str(grBatH1)

#  
#
# 
# grid.text(expression(paste("ES " %+-% " ci")), x = 0.78,   y = .92,
#           gp = gpar(fontface = "italic", fontsize = 18))
# + scale_fill_hue(l=45, c=grBatH1$chrom)
# +scale_fill_brewer(palette="OrRd")
# 
#ggplot(grBatH, aes(Trt, BoltedatH, fill=Origin))+geom_bar(width=1)


#p <- ggplot(dfml, aes(ymin=ymin, ymax=ymax, xmin=xmin, xmax=xmax, fill=variable))

# p <- ggplot(dfml, aes(ymin=ymin, ymax=ymax, xmin=xmin, xmax=xmax, fill=variable))
# graph <- p + geom_rect(colour = I("black")) + xlab("Seed Types") + ylab("Proportion of subplots") +
#   theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(), 
#         axis.title.x = element_text(size=15, face="bold", vjust=-0.4), 
#         axis.title.y = element_text(size=15, face="bold", vjust=-0.001), 
#         axis.text.x = element_text(size=8, angle=0, color="black"), 
#         axis.text.y = element_text(size=8, color="black"), 
#         legend.title = element_text(size=20), legend.text = element_text(size=15), 
#         axis.ticks=element_blank(), panel.background = element_rect(fill='white', colour='white'), 
#         plot.margin = unit(c(1.5, 1, 1, 1), units="cm")) +
#   scale_x_discrete(breaks=xaxisbreaks, labels=xaxislabels) + scale_fill_discrete(name="Number of \nSeedlings") + 
#   coord_cartesian(ylim=c(0, 45), xlim=xaxislength)
# 
# graph + annotate(geom="text", x=xes, y=ys, label=labs, size=2) + theme(legend.position="none")


########drought trade-off really?##########
# # grBatH2 <- ddply(grdatB, .(Trt, Origin), summarize, totcount = length(BoltedatH))
# 
# qplot(data=d, lxw, Wilt, color=Origin) +geom_smooth(method=glm) +geom_point(position="jitter")
# qplot(data=d[d$Wilt<9,], lxw, Wilt, color=Origin) +geom_smooth(method=glm) +geom_point(position="jitter")
# 
# qplot(data=d, LfCount1, Wilt, color=Origin) +geom_smooth(method=glm) +geom_point(position="jitter")
# 
# d2 <- d
# d2$PopMom <- paste(d2$PopID, d2$Mom)
# d2 <- ddply(d2, .(PopMom, Origin), summarize, famMeanWilt=mean(Wilt), famMeanSize=mean(lxw), famCount=length(PopMom))
# qplot(data=d2, famMeanSize, famMeanWilt, color=Origin, xlab="Leaf size at 6 wks, family mean", ylab="Days to wilt, family mean")+
#   geom_point(position="jitter")
# summary(d2$famCount)
# 
# d3 <- ddply(d, .(PopID, Origin), summarize, popMeanWilt=mean(Wilt), popMeanLfSize=mean(lxw), popCount=length(PopID),popMeanLfCount=mean(LfCount1))
# 
# qplot(data=d3, popMeanSize, popMeanWilt, color=Origin, xlab="Leaf size at 6 wks, population mean", ylab="Days to wilt, population mean")
# summary(d3$popCount)
# 
# # qplot(data=d3, popMeanWilt, popMeanSize, color=Origin, ylab="Leaf size at 6 wks, population mean", xlab="Days to wilt, population mean")+
# #   geom_smooth(method=lm, se=FALSE)
# # qplot(data=d3, popMeanWilt, popMeanSize, color=Origin, ylab="Leaf size at 6 wks, population mean", xlab="Days to wilt, population mean")+
# #   geom_smooth()
# 
# qplot(data=d3[d3$popMeanWilt<9,], popMeanLfSize, popMeanWilt, color=Origin, xlab="Leaf size at 6 wks, population mean", ylab="Days to wilt, population mean")+
#   geom_smooth(method=glm, se=FALSE)
# 
# qplot(data=d3, popMeanLfCount, popMeanWilt, color=Origin, xlab="Leaf count at 6 wks, population mean", ylab="Days to wilt, population mean")+
#   geom_smooth(method=glm, se=FALSE)

###########trade-off figure###########
stdrwiltTO <- moddata #for making figures

levels(stdrwiltTO$Origin)[levels(stdrwiltTO$Origin)=="inv"] <- "Invasive"
levels(stdrwiltTO$Origin)[levels(stdrwiltTO$Origin)=="nat"] <- "Native"



# moddata <- ddply(modeldata, .(PopID, Origin, Latitude, CtrlPopShoot), summarize, popCount=length(PopID), popDeath=mean(Death))
# stfldeathTO <- moddata #for figure making
# qplot(data=moddata,CtrlPopShoot, popDeath, color = Origin, 
#       xlab="Population mean shoot mass in control treatment", 
#       ylab="Population mean days to Death in flood treatment", main="Performance in flood vs. control treatments") +geom_smooth(method=glm, se=TRUE)

# pdf("KTurnerFig3.pdf", useDingbats=FALSE, width=6.29, height=11)
# png("STtradeoff_color.png",width=800, height = 600, pointsize = 16)
postscript("KTurnerFig3.eps", horizontal = FALSE, onefile = FALSE, paper = "special", height = 11, width = 6.29)

p1 <- ggplot(stdrwiltTO,aes(CtrlPopShoot, popWilt, color=Origin))+ geom_point()+
  geom_smooth(method=glm, se=TRUE)+
  xlab("Population mean shoot mass [g] in control treatment")+
  ylab("Population mean days to wilt in drought treatment")+ 
  #title("Performance in drought vs. control treatments")+
  theme_bw()+
  theme(legend.justification=c(1,1), legend.position=c(1,1))
 
p1 <- p1 +  annotate('point',x = 1.94, y = 7, pch=8, color="red",parse=T,size=3) +
  annotate('point',x = 2.06, y = 7, pch=8, color="red",parse=T,size=3) +
  annotate(geom="text", x=2, y=7.3, label="Origin", size=5) +
  annotate(geom="text", x=2, y=6.7, label="Origin*Control mass", size=5) +
  annotate('point',x = 2, y = 6.4, pch=8, color="red",parse=T,size=3)+
  annotate(geom="text", x=1.25, y=1, label="(a)",fontface="bold", size=5)+
  theme(axis.title.x = element_text(size=15, face="bold", vjust=-0.4), 
        axis.title.y = element_text(size=15, face="bold"),axis.text.x = element_text(size=12 ))
# p1

p2 <- ggplot(stfldeathTO, aes(CtrlPopShoot, popDeath, color=Origin))+geom_point()+
  geom_smooth(method=glm, se=TRUE)+
  xlab("Population mean shoot mass [g] in control treatment")+
  ylab("Population mean days to death in flood treatment")+  
  theme_bw()+
  theme(legend.position="none")
#legend position(left/right,top/bottom)

p2 <- p2 +  annotate(geom="text", x=2, y=28.3, label="Origin", size=5)+
    annotate(geom="text", x=2, y=27.5, label="n.s.",fontface="italic", size=5)+
    annotate(geom="text", x=2, y=26.7, label="Origin*Control mass",size=5)+ 
  annotate('point',x = 1.94, y = 25.9, pch=8, color="red",parse=T, size=3)+
  annotate('point',x = 2.06, y = 25.9, pch=8, color="red",parse=T, size=3) +
  annotate(geom="text", x=1.25, y=11, label="(b)",fontface="bold", size=5) +
  theme(axis.title.x = element_text(size=15, face="bold", vjust=-0.4), 
        axis.title.y = element_text(size=15, face="bold"),axis.text.x = element_text(size=12 ))
# p2


multiplot(p1,p2, cols=1)
dev.off()

#########################################
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}