#ST MF map figure for publication
#C.diffusa collection locations
#range?

library(maps)
library(mapdata)
library(maptools)  # For working with shapefiles.
library(scales)
library(mapproj)

#point files
popNat <- read.csv("NatPopCoord.csv")
popInv <- read.csv("InvPopCoord.csv")

#inv map
map("worldHires","Canada", xlim=c(-130,-100), ylim=c(25,55), col="gray90", fill=TRUE)
map("worldHires","usa", xlim=c(-130,-100), ylim=c(25,55), col="gray95", fill=TRUE, add=TRUE)
points(popInv$Longitude, popInv$Latitude, pch=popInv$pch, col = "red", cex=1.5)
# points(MFpopInv$Longitude, MFpopInv$Latitude, pch=15, col="red", cex=1.5)
# points(STpopInv$Longitude, STpopInv$Latitude, pch=19, col="red", cex=1.5)
# points(MompopInv$Longitude, MompopInv$Latitude, pch=1, col="red", cex=1.5)
legend("bottomleft", c("Germination trial", "Stress tolerance","Maternal effects"), pch=c(1,19,15), col="red", title="C. diffusa Invaded Range")
legend(locator(1))

#project it?
map("worldHires","Canada", xlim=c(-130,-100), ylim=c(30,55), col="gray90", fill=TRUE,projection="azequalarea", orientation= c(90,0,260), resolution=0)
map("worldHires","usa", xlim=c(-130,-100), ylim=c(30,55), col="gray95", fill=TRUE, projection="azequalarea", orientation= c(90,0,260), resolution=0,add=TRUE)

longitude <- popInv$Longitude # Defines the longitudes of the locations.
latitude <- popInv$Latitude # Defines the latitude of the locations.
projPoints <- mapproject(longitude, latitude, proj="")  # Projects the points using the same projection used for the map.
points(projPoints, pch=popInv$pch, col="red", cex=1) # See below.
# This past command plots the points and gives them a shape (pch) color (col) as well as a size (cex);
# Look here for pch numbers and their corresponding symbols: http://voteview.com/symbols_pch.htm;
# Change the color to any color you like ("red", etc); Increase the cex number to make points bigger;
# points(projPoints, pch=popInv$pch, col="blue", cex=1)  # For some reason this works better if you give the code twice.
lim <- c(-130,-100,30,55) # Creating the boundaries for the command below, based on the boundaries set up using the "map" command above.
map.grid(lim,nx=14,ny=7,labels=TRUE,pretty=TRUE, cex=0.5, col="red")
legend("topright", c("Germination trial", "Stress tolerance","Maternal effects"), pch=c(1,19,15), col="red", title="C. diffusa Invaded Range")
map.scale(-130,30, ratio=FALSE, relwidth=0.2, cex=0.5)

library(rworldmap)
library(rworldxtra)
newmap <- getMap(resolution = "coarse")
plot(newmap,xlim = c(-130, -60), ylim = c(30, 55),asp = 1)
points(popInv$Longitude, popInv$Latitude, pch=popInv$pch, col = "red", cex=1)
legend("topright", c("Germination trial", "Stress tolerance","Maternal effects"), pch=c(1,19,15), col="red", title="C. diffusa Invaded Range")


######nat map

library(rworldmap)
library(rworldxtra)
newmap <- getMap(resolution = "coarse")
plot(newmap,xlim = c(-10, 70), ylim = c(80, 95),asp = 1)
# plot(newmap,xlim = c(-20, 59), ylim = c(35, 71),asp = 0.5)
# 
# library(ggmap)
# library(mapproj)
# map <- get_map(location = 'Europe', zoom = 4)
# ggmap(map)


library(rgdal) # Commands for reprojecting the vector data.
library(rworldmap) # Recently updated mapping program.
library(rworldxtra) # Add-ons for rworldmap.
par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i") 
# Determines the width of the margins for the map; 
#the "i" designations make the map go to the edge of the plot window.
sPDF <- getMap()[-which(getMap()$ADMIN=='Antarctica')] 
# sPDF stands for spatial polygons dataframe (the map); 
#this command also Excludes Antarctica (otherwise it crashes).
sPDF <- spTransform(sPDF, CRS=CRS("+proj=robin +ellps=WGS84")) 
# Transform from the default rworldmap projection to the Robinson projection.
mapCountryData(sPDF, nameColumnToPlot="continent", mapTitle='rworldmap', 
               colourPalette='heat', addLegend = FALSE)





#map("worldHires","Europe", xlim=c(10,60), ylim=c(30,65), col="gray90", fill=TRUE)
map("world","Bulgaria", xlim=c(10,60), ylim=c(30,65), col="gray95", fill=TRUE, add=TRUE)
map("world","USSR", xlim=c(10,60), ylim=c(30,65), col="gray95", fill=TRUE, add=TRUE)
map("world","Turkey", xlim=c(10,60), ylim=c(30,65), col="gray95", fill=TRUE, add=TRUE)
map("world","Greece", xlim=c(10,60), ylim=c(30,65), col="gray95", fill=TRUE, add=TRUE)
map("world","Romania", xlim=c(10,60), ylim=c(30,65), col="gray95", fill=TRUE, add=TRUE)
#map("worldHires","Ukraine", xlim=c(10,60), ylim=c(30,65), col="gray90", fill=TRUE)
map("world","Hungary", xlim=c(10,60), ylim=c(30,65), col="gray95", fill=TRUE, add=TRUE)

# map("worldHires","Azerbaijan", xlim=c(10,60), ylim=c(30,65), col="gray90", fill=TRUE)
# map("worldHires","Armenia", xlim=c(10,60), ylim=c(30,65), col="gray95", fill=TRUE, add=TRUE)
# map("worldHires","Austria", xlim=c(10,60), ylim=c(30,65), col="gray95", fill=TRUE, add=TRUE)
# map("worldHires","Belgium", xlim=c(10,60), ylim=c(30,65), col="gray95", fill=TRUE, add=TRUE)
# map("worldHires","Great Britain", xlim=c(10,60), ylim=c(30,65), col="gray95", fill=TRUE, add=TRUE)



points(popNat$Longitude, popNat$Latitude, pch=popNat$pch, col = "red", cex=1)
legend("topright", c("Germination trial", "Stress tolerance","Maternal effects"), pch=c(1,19,15), col="red", title="C. diffusa Native Range")
#########################
#vectors/df for maps
pops <- levels(co$PopID)
setdiff(levels(cu$PopID),levels(co$PopID))
pops <- c(pops,setdiff(levels(cu$PopID),levels(co$PopID)))
setdiff(levels(al$PopID),levels(co$PopID))
setdiff(levels(d$PopID),levels(co$PopID))
pops <- c(pops,setdiff(levels(d$PopID),levels(co$PopID)))
setdiff(levels(f$PopID),levels(co$PopID))
pops <- c(pops,setdiff(levels(f$PopID),levels(co$PopID)))
setdiff(levels(n$PopID),levels(co$PopID))
pops <- c(pops,setdiff(levels(n$PopID),levels(co$PopID)))
setdiff(levels(m1$PopID),levels(co$PopID))
pops <- c(pops,setdiff(levels(m1$PopID),levels(co$PopID)))
pops <- unique(pops)

setdiff(levels(mom$PopID),pops) #include as different shape?
summary(mom$PopID)

coord<-read.table("Popcoord.txt", header=T, sep="\t", quote='"', row.names=1)

STpopI <- c("CA007","CA008","US012","US014","US015","US020","US021","US022",
            "US023","US026","US011","US018","US013","US017")

STpopN <- c("GR001","GR003","HU001","RO001","RO002", 
            "RO003","RO004","RO005","RU001","RU003","RU004","RU005", 
            "TR003","TR004","TR005","UA001","UA002", 
            "UA003","UA004","UA005","UA006","UA008","RU002","UA007")
MFpopI <- c("CA001","US001","US002","US003")
MFpopN <- c("BG001","GR002","RU008","TR001")
MompopI <- c("US006","US007","US008","US009","US010","US016","US019","US024","US025")
MompopN <- c("RU006","RU007")
STpopInv <- subset(coord, Pop %in% STpopI)
STpopInv
STpopNat<- subset(coord, Pop %in% STpopN)
MFpopInv<- subset(coord, Pop %in% MFpopI)
MFpopNat<- subset(coord, Pop %in% MFpopN)
MompopInv<- subset(coord, Pop %in% MompopI)
MompopNat<- subset(coord, Pop %in% MompopN)
STpopInv$pch <- 19
MFpopInv$pch <- 15
MompopInv$pch <- 1
popInv <- merge(STpopInv, MFpopInv, all = TRUE)
popInv <- merge(popInv, MompopInv, all = TRUE)
write.csv(popInv, file = "InvPopCoord.csv", row.names = FALSE)
#read.csv("foo.csv")

STpopNat$pch <- 19
MFpopNat$pch <- 15
MompopNat$pch <- 1
popNat <- merge(STpopNat, MFpopNat, all = TRUE)
popNat <- merge(popNat, MompopNat, all = TRUE)
write.csv(popNat, file = "NatPopCoord.csv", row.names = FALSE)