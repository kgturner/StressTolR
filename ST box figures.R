#ST figure
#box/whisker plots

#aggregate data by treatment, make graphdata
aggregate(co, by=list(Origin=co$Origin),subset=co$LfCountH,mean)
grdat <- merge(co, cu, all=TRUE)
grdat <- merge(grdat, d, all=TRUE)
grdat <- merge(grdat, f, all=TRUE)
grdat <- merge(grdat, n, all=TRUE)

##plot
# ggplot(mpg, aes(class, hwy, fill = factor(year)))+
#   geom_boxplot()
# 
# #reorder class according to median(hwy)
# ggplot(mpg, aes(reorder(class, hwy, median), hwy, fill = factor(year)))+
#   geom_boxplot()
head(m1)
ggplot(m1, aes(Origin, LfCount1, fill=Origin))+geom_boxplot()#two boxes
ggplot(m1, aes(factor(Rack), LfCount1, fill=Origin))+geom_boxplot()#many boxes
ggplot(grdat, aes(Trt, LfCountH, fill=Origin))+geom_boxplot()

ggplot(grdat, aes(Trt, BoltedatH, fill=Origin))+geom_bar(width=1)
#p <- ggplot(dfml, aes(ymin=ymin, ymax=ymax, xmin=xmin, xmax=xmax, fill=variable))

# p <- ggplot(dfml, aes(ymin=ymin, ymax=ymax, xmin=xmin, xmax=xmax, fill=variable))
# graph <- p + geom_rect(colour = I("black")) + xlab("Seed Types") + ylab("Proportion of subplots") + theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(), axis.title.x = element_text(size=15, face="bold", vjust=-0.4), axis.title.y = element_text(size=15, face="bold", vjust=-0.001), axis.text.x = element_text(size=8, angle=0, color="black"), axis.text.y = element_text(size=8, color="black"), legend.title = element_text(size=20), legend.text = element_text(size=15), axis.ticks=element_blank(), panel.background = element_rect(fill='white', colour='white'), plot.margin = unit(c(1.5, 1, 1, 1), units="cm")) + scale_x_discrete(breaks=xaxisbreaks, labels=xaxislabels) + scale_fill_discrete(name="Number of \nSeedlings") + coord_cartesian(ylim=c(0, 45), xlim=xaxislength)
# 
# graph + annotate(geom="text", x=xes, y=ys, label=labs, size=2) + theme(legend.position="none")