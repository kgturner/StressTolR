#ST figure
#box/whisker plots

#aggregate data by treatment, make graphdata
#aggregate(co, by=list(Origin=co$Origin),subset=co$LfCountH,mean)
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
p1 <- ggplot(grdat, aes(Trt, RootMass.g, fill=Origin))+geom_boxplot()+xlab("Stress Treatment")+ylab("Root Mass (g)")+ theme(legend.position="none")
p1 <- p1 + annotate('point',x = "Control", y = 5, pch=16, color="red",parse=T)+annotate('point',x = "Nutrient", y = 5, pch=16, color="red",parse=T)+
  annotate('point',x = "Herbivory", y = 5, pch=8, color="red",parse=T)

# ggplot(grdat, aes(Trt, ShootMass.g, fill=Origin))+geom_boxplot()
p2 <- ggplot(grdat, aes(Trt, LfCountH, fill=Origin))+geom_boxplot()+xlab("Stress Treatment")+ylab("Number of basal leaves")+ theme(legend.justification=c(0,1), legend.position=c(0,1))
#legend position(left/right,top/bottom)
p2 <- p2 +  annotate('point',x = "Control", y = 30, pch=8, color="red",parse=T)+annotate('point',x = "Control", y = 32, pch=8, color="red",parse=T)
# ggplot(grdat, aes(Trt, CrownDiam.mm, fill=Origin))+geom_boxplot()
# ggplot(grdat, aes(Trt, lxwH, fill=Origin))+geom_boxplot()

multiplot(p1,p2, cols=2)

#outlier check
summary(cu$LfCountH)
cu[cu$LfCountH==84,]
cu[cu$PopID=="RU008",]

summary(n$LfCountH)
n[n$LfCountH==72,]

summary(co$LfCountH)
co[co$LfCountH==64,]

##plot LH traits
ggplot(grdat, aes(Trt, BoltedatH, fill=Origin))+geom_bar(width=1)
#p <- ggplot(dfml, aes(ymin=ymin, ymax=ymax, xmin=xmin, xmax=xmax, fill=variable))

# p <- ggplot(dfml, aes(ymin=ymin, ymax=ymax, xmin=xmin, xmax=xmax, fill=variable))
# graph <- p + geom_rect(colour = I("black")) + xlab("Seed Types") + ylab("Proportion of subplots") + theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(), axis.title.x = element_text(size=15, face="bold", vjust=-0.4), axis.title.y = element_text(size=15, face="bold", vjust=-0.001), axis.text.x = element_text(size=8, angle=0, color="black"), axis.text.y = element_text(size=8, color="black"), legend.title = element_text(size=20), legend.text = element_text(size=15), axis.ticks=element_blank(), panel.background = element_rect(fill='white', colour='white'), plot.margin = unit(c(1.5, 1, 1, 1), units="cm")) + scale_x_discrete(breaks=xaxisbreaks, labels=xaxislabels) + scale_fill_discrete(name="Number of \nSeedlings") + coord_cartesian(ylim=c(0, 45), xlim=xaxislength)
# 
# graph + annotate(geom="text", x=xes, y=ys, label=labs, size=2) + theme(legend.position="none")

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