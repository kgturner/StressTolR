#mapKathrynTurnerAndy201304.r
#andy south 9/4/13

#to create a reprojected map and add sites
#answering query from Kathryn Turner 24/3/13 <kathryn.g.turner@gmail.com>

#A labmate and I are trying to make a modern map of Europe/Western Asia 
#and a map of North America - ideally both in one map, 
#in a Lambert azimuthal equal area projection, with the North Pole in the center 
#- and map collection locations onto it.

# setwd("C:\\rWorldMapNotes\\rworldmapUsers\\KathrynTurner\\")

library(rgdal) # Commands for reprojecting the vector data.
library(rworldmap) # Recently updated mapping program.
library(rworldxtra) # Add-ons for rworldmap.

#####quick redo, color countries, two legend, no germ trial####
#pink/turquoise
pdf("KTurnerFig1.pdf", useDingbats=FALSE, width=13.38)
# png("collectionMap_pinkturqoise.png", width=600, height = 600, pointsize = 16)
#svg("collectionMap_bw.svg", pointsize = 12)
# setEPS( horizontal = FALSE, onefile = FALSE, paper = "special") #340 mm is c. 13 in, height and width in in
# postscript("colMap_bw.eps")

# postscript("KTurnerFig1.eps", horizontal = FALSE, onefile = FALSE, paper = "special", height = 7, width = 13.38)

# eps <- function(file, onefile=FALSE, width=5.5, height = 5.5, horizontal=FALSE,paper="special", ...){
#   postscript(file=file,width=width,height=height,onefile=onefile,horizontal=horizontal,paper=paper,title=file,...)
#   par(bty='l')
# }

projectionCRS <- CRS("+proj=laea +lon_0=0.001 +lat_0=89.999 +ellps=sphere") #the ellps 'sphere' has a radius of 6370997.0m
par(mai=c(0,0,0.2,0)) #,xaxs="i",yaxs="i"
sPDF <- getMap()[-which(getMap()$ADMIN=='Antarctica')] 
sPDF <- spTransform(sPDF, CRS=projectionCRS)
setLims <- TRUE #FALSE back to whole world
#setLims <- FALSE
if ( !setLims )
{
  xlim <- ylim <- NA
} else
{
  ### TRY FIDDLING WITH THESE LIMITS ###
  xlimUnproj <- c(-52,120)
  ylimUnproj <- c(10,30)
  sPointsLims <- data.frame(x=xlimUnproj, y=ylimUnproj)
  coordinates(sPointsLims) = c("x", "y")
  proj4string(sPointsLims) <- CRS("+proj=longlat +ellps=WGS84")
  sPointsLims <- spTransform(sPointsLims, CRS=projectionCRS)
  xlim <- coordinates(sPointsLims)[,"x"]
  ylim <- coordinates(sPointsLims)[,"y"]  
}

# countries <- as.data.frame(cbind(sPDF$ADMIN))
# countries$country <- sPDF$ADMIN
# countries$color <- "lightgray"
# #countries <- reorder(countries$country, countries$V1)
# #countries <- unique(countries)
# 
# countries[countries$country %in% c("United States of America", "Canada"),]$color <- "red"
# grayco <- as.vector(countries$color)
# 
# mapCountryData(sPDF, nameColumnToPlot="SOVEREIGNT", mapTitle='Global range of C. diffusa', colourPalette=countries$color, borderCol ='gray24',addLegend = FALSE, xlim=xlim, ylim=ylim)

# sPDF <- getMap()
# #list of country names
# sPDF$ADMIN
#setup a color code column filled with 1's
sPDF$colCode <- 1
# tst <- sPDF@data
# View(tst)
#set codes for specified countries
sPDF$colCode[ which(sPDF$ADMIN %in% c("Canada","United States of America"))] <- 2
sPDF$colCode[ which(sPDF$ADMIN %in% c("Armenia","Azerbaijan", "Bulgaria", "Georgia", 
                                      "Greece", "Moldova", "Romania","Russia", "Turkey",
                                      "Ukraine", "Serbia"))] <- 3
sPDF$colCode[ which(sPDF$ADMIN %in% c("Poland", "Belarus", "Italy", "Syria", "Czech Republic",
                                      "Estonia", "Switzerland","Latvia","Lithuania", 
                                      "Slovenia", "Serbia","Austria","Belgium", "France",
                                      "Germany","Hungary","Luxembourg","Norway","Slovakia",
                                      "Spain", "United Kingdom", "Kazakhstan", "Turkmenistan", "China"))] <- 4
# tst <- sPDF@data
# View(tst)
#create a colour palette - note for each value not for each country
colourPalette <- c("lightgray","#F8766D","#00BFC4", "cadetblue1")
# colourPalette <- c("lightgray","lightgray")
# spName <- plotmath(italic("Centaurea diffusa"))
par(mar=c(0,0,0,0))
mapCountryData(sPDF, nameColumnToPlot="colCode", mapTitle=NA,
               colourPalette=colourPalette, borderCol ='gray24', addLegend = FALSE,
               xlim=xlim, ylim=ylim, catMethod=c(0,1,2,3,4))
#note that catMethod defines the breaks and values go in a category if they are <= upper end
#mapTitle=bquote(Global~range~of~italic(Centaurea)~italic(diffusa)) 


# # to plot states you can get data from here :
# #   http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/cultural/ne_10m_admin_1_states_provinces_shp.zip
# inHigh <- "ne_10m_admin_1_states_provinces_shp.shp"
# sPDFhigh <- readShapePoly(inHigh)
# # attributes <- sPDFhigh@data
# #test plotting
# mapPolys(sPDFhigh, nameColumnToPlot='admin', addLegend = FALSE)
# #then you could subset the countries you want, 
# sPDFhigh <- sPDFhigh[which(sPDFhigh$ADMIN %in% c("Canada","United States of America","Russia"))]
# # look up state names & do 
# mapPolys(sPDFhigh$name ,add=TRUE)
# # = c("Yukon","British Columbia", "Washington", "Oregon")

popInv <- read.csv("InvPopCoord.csv")
popInv.1 <- popInv[popInv$pch!=1,] #no germ trial
popInv.1[popInv.1$pch==19,]$pch <- 1 #broad cg
popInv.1[popInv.1$pch==15,]$pch <- 19 #mat fx
coordinates(popInv.1) = c("Longitude", "Latitude")
proj4string(popInv.1) <- CRS("+proj=longlat +ellps=WGS84")
sPointsDF <- spTransform(popInv.1, CRS=projectionCRS)
points(sPointsDF, pch=popInv.1$pch, cex=1.5)

popNat <- read.csv("NatPopCoord.csv")
popNat.1 <- popNat[popNat$pch!=1,] #no germ trial
popNat.1[popNat.1$pch==19,]$pch <- 1
popNat.1[popNat.1$pch==15,]$pch <- 19
coordinates(popNat.1) = c("Longitude", "Latitude")
proj4string(popNat.1) <- CRS("+proj=longlat +ellps=WGS84")
sPointsDFNat <- spTransform(popNat.1, CRS=projectionCRS)
points(sPointsDFNat, pch=popNat.1$pch, cex=1.5) #pch2 for triangles

llgridlines(sPDF, easts=c(-90,-180,0,90,180), norths=seq(0,90,by=15), 
            plotLabels=FALSE, ndiscr=1000) #ndiscr=num points in lines
#lat markings...

markings <- data.frame(Latitude=as.numeric(c(75,60,45,30,15,85,85)), Longitude=as.numeric(c(-45,-45,-45,-45,-45,0,180)),name=c("75", "60","45","30","15","0","180"))
coordinates(markings) = c("Longitude", "Latitude")
proj4string(markings) <- CRS("+proj=longlat +ellps=WGS84")
sPointsDFmark <- spTransform(markings, CRS=projectionCRS)
text(sPointsDFmark, labels = sPointsDFmark$name, cex=1.5) #pch2 for triangles

# pole <- data.frame(x=0, y=90)
# coordinates(pole) = c("x", "y")
# proj4string(pole) <- CRS("+proj=longlat +ellps=WGS84")
# pole <- spTransform(pole, CRS=projectionCRS)
# points(pole, pch=8, cex=2, lwd=2)

legend("bottomleft", c("Broad CG","Maternal CG"), 
       pch=c(1,19),  bg="white", title = "Sampled populations", cex=1.5)
legend("topright", c("Invasive", "Native","Present, status unknown"), fill=c("#F8766D","#00BFC4", "cadetblue1"),
       title="Origin", bg="white", cex=1.5)
box(lty="solid", col = "black")
# #shameless plug !
# mtext("map made using rworldmap", line=-1, side=1, adj=1, cex=0.6)

# 
dev.off()

#####map for ppt, pinkgr####
# png("BroadCGMap_pinkgreen.png", width=600, height = 600, pointsize = 16)
png("rangeMap_pinkgreen.png", width=600, height = 600, pointsize = 16)
projectionCRS <- CRS("+proj=laea +lon_0=0.001 +lat_0=89.999 +ellps=sphere") #the ellps 'sphere' has a radius of 6370997.0m
par(mai=c(0,0,0.2,0)) #,xaxs="i",yaxs="i"
sPDF <- getMap()[-which(getMap()$ADMIN=='Antarctica')] 
sPDF <- spTransform(sPDF, CRS=projectionCRS)
setLims <- TRUE #FALSE back to whole world
#setLims <- FALSE
if ( !setLims )
{
  xlim <- ylim <- NA
} else
{
  ### TRY FIDDLING WITH THESE LIMITS ###
  xlimUnproj <- c(-52,120)
  ylimUnproj <- c(10,30)
  sPointsLims <- data.frame(x=xlimUnproj, y=ylimUnproj)
  coordinates(sPointsLims) = c("x", "y")
  proj4string(sPointsLims) <- CRS("+proj=longlat +ellps=WGS84")
  sPointsLims <- spTransform(sPointsLims, CRS=projectionCRS)
  xlim <- coordinates(sPointsLims)[,"x"]
  ylim <- coordinates(sPointsLims)[,"y"]  
}


#setup a color code column filled with 1's
sPDF$colCode <- 4

#set codes for specified countries
sPDF$colCode[ which(sPDF$ADMIN %in% c("Canada","United States of America"))] <- 1
sPDF$colCode[ which(sPDF$ADMIN %in% c("Armenia","Azerbaijan", "Bulgaria", "Georgia", 
                                      "Greece", "Moldova", "Romania","Russia", "Turkey",
                                      "Ukraine", "Serbia"))] <- 2
sPDF$colCode[ which(sPDF$ADMIN %in% c("Poland", "Belarus", "Italy", "Syria", "Czech Republic",
                                      "Estonia", "Switzerland","Latvia","Lithuania", 
                                      "Slovenia", "Serbia","Austria","Belgium", "France",
                                      "Germany","Hungary","Luxembourg","Norway","Slovakia",
                                      "Spain", "United Kingdom", "Kazakhstan", "Turkmenistan", "China"))] <- 3

#create a colour palette - note for each value not for each country
colourPalette <- c("#F8766D","#00BA38", "palegreen","lightgray") #inv, nat, present/naturalized, extra countries

par(mar=c(0,0,0,0))
mapCountryData(sPDF, nameColumnToPlot="colCode", mapTitle=NA,
               colourPalette=colourPalette, borderCol ='gray24', addLegend = FALSE,
               xlim=xlim, ylim=ylim, catMethod=c(0,1,2,3,4))

popInv <- read.csv("InvPopCoord.csv")
popInv.1 <- popInv[popInv$pch!=1,] #no germ trial
popInv.1$pch <-  1 #broad cg
# popInv.1[popInv.1$pch==15,]$pch <- 19 #mat fx
coordinates(popInv.1) = c("Longitude", "Latitude")
proj4string(popInv.1) <- CRS("+proj=longlat +ellps=WGS84")
sPointsDF <- spTransform(popInv.1, CRS=projectionCRS)
points(sPointsDF, pch=popInv.1$pch, cex=1.5)

popNat <- read.csv("NatPopCoord.csv")
popNat.1 <- popNat[popNat$pch!=1,] #no germ trial
popNat.1$pch <- 17
# popNat.1[popNat.1$pch==15,]$pch <- 19
coordinates(popNat.1) = c("Longitude", "Latitude")
proj4string(popNat.1) <- CRS("+proj=longlat +ellps=WGS84")
sPointsDFNat <- spTransform(popNat.1, CRS=projectionCRS)
points(sPointsDFNat, pch=popNat.1$pch, cex=1.5) #pch2 for triangles

llgridlines(sPDF, easts=c(-90,-180,0,90,180), norths=seq(0,90,by=15), 
            plotLabels=FALSE, ndiscr=1000) #ndiscr=num points in lines
#lat markings...

markings <- data.frame(Latitude=as.numeric(c(75,60,45,30,15,85,85)), Longitude=as.numeric(c(-45,-45,-45,-45,-45,0,180)),name=c("75", "60","45","30","15","0","180"))
coordinates(markings) = c("Longitude", "Latitude")
proj4string(markings) <- CRS("+proj=longlat +ellps=WGS84")
sPointsDFmark <- spTransform(markings, CRS=projectionCRS)
text(sPointsDFmark, labels = sPointsDFmark$name, cex=1.5) #pch2 for triangles

# legend("bottomleft", c("Broad CG","Maternal CG"), 
#        pch=c(1,19),  bg="white", title = "Sampled populations", cex=1.5)
legend("bottomleft", c("Invasive C. diffusa", "Native C. diffusa","Present, status unknown"), fill=c("#F8766D","#00BA38", "palegreen"),
       title="Origin", bg="white", cex=1)
box(lty="solid", col = "black")
# #shameless plug !
# mtext("map made using rworldmap", line=-1, side=1, adj=1, cex=0.6)

# 
dev.off()


############################

#quick redo, color, two legends
pdf("collectionMap.pdf", useDingbats=FALSE)

projectionCRS <- CRS("+proj=laea +lon_0=0.001 +lat_0=89.999 +ellps=sphere") #the ellps 'sphere' has a radius of 6370997.0m
par(mai=c(0,0,0.2,0)) #,xaxs="i",yaxs="i"
sPDF <- getMap()[-which(getMap()$ADMIN=='Antarctica')] 
sPDF <- spTransform(sPDF, CRS=projectionCRS)
setLims <- TRUE #FALSE back to whole world
#setLims <- FALSE
if ( !setLims )
{
  xlim <- ylim <- NA
} else
{
  ### TRY FIDDLING WITH THESE LIMITS ###
  xlimUnproj <- c(-52,120)
  ylimUnproj <- c(10,30)
  sPointsLims <- data.frame(x=xlimUnproj, y=ylimUnproj)
  coordinates(sPointsLims) = c("x", "y")
  proj4string(sPointsLims) <- CRS("+proj=longlat +ellps=WGS84")
  sPointsLims <- spTransform(sPointsLims, CRS=projectionCRS)
  xlim <- coordinates(sPointsLims)[,"x"]
  ylim <- coordinates(sPointsLims)[,"y"]  
}
gw <- as.vector(c("ghostwhite","ghostwhite","ghostwhite","ghostwhite","ghostwhite","ghostwhite"))
#gray <- as.vector(c("lightgray","lightgray","lightgray","lightgray","lightgray","lightgray"))
mapCountryData(sPDF, nameColumnToPlot="continent", mapTitle='Global range of C. diffusa', colourPalette=gw, borderCol ='gray24',addLegend = FALSE, xlim=xlim, ylim=ylim)

popInv <- read.csv("InvPopCoord.csv")
coordinates(popInv) = c("Longitude", "Latitude")
proj4string(popInv) <- CRS("+proj=longlat +ellps=WGS84")
sPointsDF <- spTransform(popInv, CRS=projectionCRS)
points(sPointsDF, pch=popInv$pch, cex=1, col="red")

popNat <- read.csv("NatPopCoord.csv")
coordinates(popNat) = c("Longitude", "Latitude")
proj4string(popNat) <- CRS("+proj=longlat +ellps=WGS84")
sPointsDFNat <- spTransform(popNat, CRS=projectionCRS)
points(sPointsDFNat, col='blue', pch=popNat$pch, cex=1) #pch2 for triangles

llgridlines(sPDF, easts=c(-90,-180,0,90,180), norths=seq(0,90,by=15), 
            plotLabels=FALSE, ndiscr=1000) #ndiscr=num points in lines

# pole <- data.frame(x=0, y=90)
# coordinates(pole) = c("x", "y")
# proj4string(pole) <- CRS("+proj=longlat +ellps=WGS84")
# pole <- spTransform(pole, CRS=projectionCRS)
# points(pole, pch=8, cex=2, lwd=2)

legend("bottomleft", c("Germination trial", "Stress tolerance","Maternal effects"), pch=c(1,19,15), 
       col="red", title="Invaded Range", bg="white")
legend("topright", c("Germination trial", "Stress tolerance","Maternal effects"), pch=c(1,19,15),
       col="blue", title="Native Range", bg="white")

#shameless plug !
mtext("map made using rworldmap", line=-1, side=1, adj=1, cex=0.6, col='purple')

# 
dev.off()

#quick redo, b&w, one legend
#pdf("collectionMap_bw.pdf", useDingbats=FALSE)
#png("collectionMap_bw.png", width=600, height = 600, pointsize = 16)
#svg("collectionMap_bw.svg", pointsize = 12)
setEPS( horizontal = FALSE, onefile = FALSE, paper = "special")
postscript("colMap_bw.eps")

projectionCRS <- CRS("+proj=laea +lon_0=0.001 +lat_0=89.999 +ellps=sphere") #the ellps 'sphere' has a radius of 6370997.0m
par(mai=c(0,0,0.2,0)) #,xaxs="i",yaxs="i"
sPDF <- getMap()[-which(getMap()$ADMIN=='Antarctica')] 
sPDF <- spTransform(sPDF, CRS=projectionCRS)
setLims <- TRUE #FALSE back to whole world
#setLims <- FALSE
if ( !setLims )
{
  xlim <- ylim <- NA
} else
{
  ### TRY FIDDLING WITH THESE LIMITS ###
  xlimUnproj <- c(-52,120)
  ylimUnproj <- c(10,30)
  sPointsLims <- data.frame(x=xlimUnproj, y=ylimUnproj)
  coordinates(sPointsLims) = c("x", "y")
  proj4string(sPointsLims) <- CRS("+proj=longlat +ellps=WGS84")
  sPointsLims <- spTransform(sPointsLims, CRS=projectionCRS)
  xlim <- coordinates(sPointsLims)[,"x"]
  ylim <- coordinates(sPointsLims)[,"y"]  
}
#gw <- as.vector(c("ghostwhite","ghostwhite","ghostwhite","ghostwhite","ghostwhite","ghostwhite"))

#gray <- as.vector(c("lightgray","lightgray","lightgray","lightgray","lightgray","lightgray"))
#mapCountryData(sPDF, nameColumnToPlot="continent", mapTitle='Global range of C. diffusa', colourPalette=gray, borderCol ='gray24',addLegend = FALSE, xlim=xlim, ylim=ylim)

countries <- as.data.frame(cbind(sPDF$SOVEREIGNT))
countries$country <- sPDF$SOVEREIGNT
countries$color <- "lightgray"
#countries <- reorder(countries$country, countries$V1)
#countries <- unique(countries)

countries[countries$country %in% c("United States of America", "Canada"),]$color <- "red"
grayco <- as.vector(countries$color)

mapCountryData(sPDF, nameColumnToPlot="SOVEREIGNT", mapTitle='Global range of C. diffusa', colourPalette=grayco, borderCol ='gray24',addLegend = FALSE, xlim=xlim, ylim=ylim)


popInv <- read.csv("InvPopCoord.csv")
coordinates(popInv) = c("Longitude", "Latitude")
proj4string(popInv) <- CRS("+proj=longlat +ellps=WGS84")
sPointsDF <- spTransform(popInv, CRS=projectionCRS)
points(sPointsDF, pch=popInv$pch, cex=1)

popNat <- read.csv("NatPopCoord.csv")
coordinates(popNat) = c("Longitude", "Latitude")
proj4string(popNat) <- CRS("+proj=longlat +ellps=WGS84")
sPointsDFNat <- spTransform(popNat, CRS=projectionCRS)
points(sPointsDFNat, pch=popNat$pch, cex=1) #pch2 for triangles

llgridlines(sPDF, easts=c(-90,-180,0,90,180), norths=seq(0,90,by=15), 
            plotLabels=FALSE, ndiscr=1000) #ndiscr=num points in lines

# pole <- data.frame(x=0, y=90)
# coordinates(pole) = c("x", "y")
# proj4string(pole) <- CRS("+proj=longlat +ellps=WGS84")
# pole <- spTransform(pole, CRS=projectionCRS)
# points(pole, pch=8, cex=2, lwd=2)

legend("bottomleft", c("Germination trial", "Stress tolerance","Maternal effects"), pch=c(1,19,15),  bg="white", title = "Sampled populations")
# legend("topright", c("Germination trial", "Stress tolerance","Maternal effects"), pch=c(1,19,15), col="blue", title="Native Range", bg="white")

#shameless plug !
mtext("map made using rworldmap", line=-1, side=1, adj=1, cex=0.6)

# 
dev.off()

### details ####
##########################
### projection options ###
### you could try others ###

#projectionCRS <- CRS("+proj=robin +ellps=WGS84")
#Lambert azimuthal equal area projection
#from http://gis.stackexchange.com/questions/30054/defining-datum-for-lambert-azimuthal-equal-area-and-converting-to-geographic-coo
projectionCRS <- CRS("+proj=laea +lon_0=20 +lat_0=5 +ellps=sphere") #the ellps 'sphere' has a radius of 6370997.0m
#from https://stat.ethz.ch/pipermail/r-sig-geo/2007-April/001980.html
#this gave errors inc : 2 projected point(s) not finite 
#projectionCRS <- CRS("+proj=laea +lat_0=0 +lon_0=-80")

projectionCRS <- CRS("+proj=laea +lon_0=1 +lat_0=89 +ellps=sphere") #the ellps 'sphere' has a radius of 6370997.0m

################
### base map ###

# Determines the width of the margins for the map; the "i" designations make the map go to the edge of the plot window.
par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
# sPDF stands for spatial polygons dataframe (the map); this command also Excludes Antarctica (otherwise it crashes).
sPDF <- getMap()[-which(getMap()$ADMIN=='Antarctica')] 
# Transform from the default rworldmap projection 

sPDF <- spTransform(sPDF, CRS=projectionCRS)

#reproject xlim and ylim, so that we can zoom in on the map.
#change this between TRUE & FALSE to experiment with setting limits
#setLims <- TRUE

setLims <- FALSE
if ( !setLims )
{
  xlim <- ylim <- NA
} else
{
  ### TRY FIDDLING WITH THESE LIMITS ###
  xlimUnproj <- c(-180,180)
  ylimUnproj <- c(-90,90)
  sPointsLims <- data.frame(x=xlimUnproj, y=ylimUnproj)
  #promote to sp
  coordinates(sPointsLims) = c("x", "y")
  #set CRS (coordinate reference system) for the points
  #assuming WGS84
  proj4string(sPointsLims) <- CRS("+proj=longlat +ellps=WGS84")
  sPointsLims <- spTransform(sPointsLims, CRS=projectionCRS)
  #when I had 180 it gave
  #Error in `proj4string<-`(`*tmp*`, value = <S4 object of class "CRS">) : 
  #  Geographical CRS given to non-conformant data: 180
  xlim <- coordinates(sPointsLims)[,"x"]
  ylim <- coordinates(sPointsLims)[,"y"]  
}

#plot the projected map
mapCountryData(sPDF, nameColumnToPlot="continent", mapTitle='Invasive Range', colourPalette='heat', addLegend = FALSE, xlim=xlim, ylim=ylim)
# colourPalette should be set to either a vector of colours or one of :white2Black black2White palette heat topo terrain rainbow negpos8 negpos9


gray <- as.vector(c("lightgray","lightgray","lightgray","lightgray","lightgray","lightgray"))

##############
### points ###

# read GPS coordinates for Inv
popInv <- read.csv("InvPopCoord.csv")
#promote to a SpatialPointsDataFrame
coordinates(popInv) = c("Longitude", "Latitude")
#set CRS (coordinate reference system) for the points
#assuming WGS84
proj4string(popInv) <- CRS("+proj=longlat +ellps=WGS84")
#reproject the points so that they match the projection of the map.
sPointsDF <- spTransform(popInv, CRS=projectionCRS)
#plot points onto the map
points(sPointsDF, pch=popInv$pch, cex=1.5)

# read GPS coordinates for Nat
popNat <- read.csv("NatPopCoord.csv")
#promote to a SpatialPointsDataFrame
coordinates(popNat) = c("Longitude", "Latitude")
#set CRS (coordinate reference system) for the points
#assuming WGS84
proj4string(popNat) <- CRS("+proj=longlat +ellps=WGS84")
#reproject the points so that they match the projection of the map.
sPointsDFNat <- spTransform(popNat, CRS=projectionCRS)
#plot points onto the map
points(sPointsDFNat, col='blue', pch=popNat$pch, cex=1.5) #pch2 for triangles

####################
###latitude lines ###

llgridlines(sPDF, easts=c(-90,-180,0,90,180), norths=seq(0,90,by=15), 
            plotLabels=FALSE, ndiscr=1000) #ndiscr=num points in lines



#shameless plug !
mtext("map made using rworldmap", line=-1, side=1, adj=1, cex=0.6, col='purple')


######################3
#layer of range?

## load stuff
install.packages("devtools")
library(devtools)
install_github("rbison", "ropensci")
library(rbison)

## search for knapweed
res <- bison(species='Centaurea diffusa', count=1396) # preliminary search said there were 1396 records
bisonmap(res) # make map of points
bisonmap(res, tomap="county") # summary by county
bisonmap(res, tomap="state") # summary by states


##########shading with pattern?########
barplt> segments(mp, hh, mp, hh + 2*sqrt(1000*hh/100), col = mybarcol, lwd = 1.5)

barplt> stopifnot(dim(mp) == dim(hh))  # corresponding matrices

barplt> mtext(side = 1, at = colMeans(mp), line = -2,
              barplt+       text = paste("Mean", formatC(colMeans(hh))), col = "red")
barplt> barplot(VADeaths, angle = 15+10*1:5, density = 20, col = "black",
                barplt+         legend = rownames(VADeaths))

aa <- c(4,5,6)
barplot(aa, density=2, col='red', border="blue")

library(TeachingDemos)
tmp.dat <- table( sample( letters[1:4], 1000, replace=TRUE) )
tmp1 <- barplot(tmp.dat, col=NA, width=1, ylim=c(-5,max(tmp.dat)+5))
tmp2 <- par('usr')
tmpfun <- function(){
  tmp.x <- seq(tmp2[1], tmp2[2], length=50)
  tmp.y <- seq(tmp2[3], tmp2[4], length=50)
  points( expand.grid(x=tmp.x, y=tmp.y), pch='.' )
}
for (i in seq(along=tmp1) ){
  clipplot( tmpfun(), xlim=tmp1[i] + c(-0.5,0.5), ylim=c(0,tmp.dat[i]) )
} 
