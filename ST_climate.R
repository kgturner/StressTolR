#general look at climate for ST populations
#see Extract.climate.data.R and Squishr.func.R
popWC <- read.table("Popcoord_worldclim.txt", header=T, sep="\t") #popcoord_worldclim.txt the set with some shifted points to produce data from worldclim

pop <- rbind(STpopInv, STpopNat, MFpopInv, MFpopNat)
rownames(pop) <- pop$Pop
pop <- pop[1:3]
pop <- pop[order(pop$Pop),]

popWC <- subset(popWC, Pop %in% pop$Pop)
rownames(popWC) <- popWC$Pop
popWC$Pop <- droplevels(popWC$Pop)

#load packages: raster, rgdal, foreach
library(rgdal)
library(raster)
library(foreach)
#Read names of all files in directory into a list
#from http://stackoverflow.com/questions/5319839/read-multiple-csv-files-into-separate-data-frames
filenames <- list.files(path="~/grad work/Centaurea diffusa/WorldClim_2013/")
#Load all geoTIFF files
for(i in filenames){
  filepath <- file.path("~/grad work/Centaurea diffusa/WorldClim_2013/",i)
  assign(i, raster(filepath))
}
#check that all files loaded properly by raster
#from http://stackoverflow.com/questions/15387727/use-object-names-as-list-names-in-r
list <- mget(filenames, envir=globalenv())
for(i in list){
  if (hasValues(i)==FALSE){
    print(i,"hasValues error")
  }
  if (inMemory(i)==TRUE){
    print(i, "inMemory error")
  }
  else{
    print("All checked out!")
  }
}
#load table of latitude and longitude for locations of interest
# pop <- read.table("pop.txt", header=T, sep="\t")
# > head(pop)
# Latitude Longitude Pop
# CA001 49.01794 -123.08185 CA001
# CA008 49.01208 -118.64571 CA008
# CA009 49.29610 -118.47383 CA009
# CA010 49.32018 -118.37044 CA010
# US014 40.12227 -101.28028 US014
# BG001 43.38943 28.45741 BG

#load location coordinates as SpatialPoints
for(i in popWC$Pop){
  assign(i,SpatialPoints(as.matrix(t(c(popWC[i,2], popWC[i,1])))))
}
#check that SpatialPoints load correctly from geoTIFFs
#no column should be entirely NAs (if they are, move points)
poplist <- mget(levels(popWC$Pop), envir=globalenv())
tiffvector <- unlist(list)
foreach(p=poplist, .combine='cbind') %:%
  foreach(t=tiffvector, .combine='rbind') %do%{
    is.na(extract(t,p))
  }



#make climate data table
climate <- foreach(t=tiffvector, .combine='cbind') %:%
  foreach(p=poplist, .combine='rbind') %do%{
    myValue<-extract(t, p)
  } #may take a while
#tidy table
clim <- as.data.frame(climate)
row.names(clim) <- pop$Pop
colnames(clim) <- filenames



#load squishr functions
vars <- squishsplit(clim)
STclimate <- do.call(cbind,lapply(unique(vars),squishr,dat=clim))
head(STclimate)

STclimate$Origin <- "nat"
STclimate$PopID <- rownames(STclimate)
STclimate[STclimate$PopID %in% STpopInv$Pop,]$Origin <- "inv"
STclimate[STclimate$PopID %in% MFpopInv$Pop,]$Origin <- "inv"

#write table
write.table(STclimate, file="STbioclimdata.txt")
# #load table
# clim <- read.table("STbioclimdata.txt", header=TRUE)

####climate summaries!!###
summary(subset(STclimate, Origin =="inv"))
summary(subset(STclimate, Origin =="nat"))


bc1 <- ggplot(data=STclimate, aes(bio1, fill=Origin))+geom_histogram(alpha=0.4, position="identity")+theme(legend.position="none")
bc2 <- ggplot(data=STclimate, aes(bio2, fill=Origin))+geom_histogram(alpha=0.4, position="identity")+theme(legend.position="none")
bc3 <- ggplot(data=STclimate, aes(bio3, fill=Origin))+geom_histogram(alpha=0.4, position="identity")+theme(legend.position="none")
bc4 <- ggplot(data=STclimate, aes(bio4, fill=Origin))+geom_histogram(alpha=0.4, position="identity")+theme(legend.position="none")
bc5 <- ggplot(data=STclimate, aes(bio5, fill=Origin))+geom_histogram(alpha=0.4, position="identity")+theme(legend.position="none")
bc6 <- ggplot(data=STclimate, aes(bio6, fill=Origin))+geom_histogram(alpha=0.4, position="identity")+theme(legend.position="none")
bc7 <- ggplot(data=STclimate, aes(bio7, fill=Origin))+geom_histogram(alpha=0.4, position="identity")+theme(legend.position="none")
bc8 <- ggplot(data=STclimate, aes(bio8, fill=Origin))+geom_histogram(alpha=0.4, position="identity")+theme(legend.position="none")
bc9 <- ggplot(data=STclimate, aes(bio9, fill=Origin))+geom_histogram(alpha=0.4, position="identity")+theme(legend.position="none")
bc10 <- ggplot(data=STclimate, aes(bio10, fill=Origin))+geom_histogram(alpha=0.4, position="identity")+theme(legend.position="none")
bc11 <- ggplot(data=STclimate, aes(bio11, fill=Origin))+geom_histogram(alpha=0.4, position="identity")+theme(legend.position="none")
bc12 <- ggplot(data=STclimate, aes(bio12, fill=Origin))+geom_histogram(alpha=0.4, position="identity")+theme(legend.position="none")
bc13 <- ggplot(data=STclimate, aes(bio13, fill=Origin))+geom_histogram(alpha=0.4, position="identity")+theme(legend.position="none")
bc14 <- ggplot(data=STclimate, aes(bio14, fill=Origin))+geom_histogram(alpha=0.4, position="identity")+theme(legend.position="none")
bc15 <- ggplot(data=STclimate, aes(bio15, fill=Origin))+geom_histogram(alpha=0.4, position="identity")+theme(legend.position="none")
bc16 <- ggplot(data=STclimate, aes(bio16, fill=Origin))+geom_histogram(alpha=0.4, position="identity")+theme(legend.position="none")
bc17 <- ggplot(data=STclimate, aes(bio17, fill=Origin))+geom_histogram(alpha=0.4, position="identity")+theme(legend.position="none")
bc18 <- ggplot(data=STclimate, aes(bio18, fill=Origin))+geom_histogram(alpha=0.4, position="identity")+theme(legend.position="none")
bc19 <- ggplot(data=STclimate, aes(bio19, fill=Origin))+geom_histogram(alpha=0.4, position="identity")+theme(legend.position="none")

pdf("STclimate.pdf", useDingbats=FALSE)
multiplot(bc1, bc2, bc3, bc4, bc5, bc6, bc7, bc8, bc9, bc10, bc11, bc12, bc13, bc14, bc15, bc16, bc17, bc18, bc19, cols=4)
dev.off()



# BIO1 = Annual Mean Temperature
# BIO2 = Mean Diurnal Range (Mean of monthly (max temp - min temp))
# BIO3 = Isothermality (BIO2/BIO7) (* 100)
# BIO4 = Temperature Seasonality (standard deviation *100)
# BIO5 = Max Temperature of Warmest Month
# BIO6 = Min Temperature of Coldest Month
# BIO7 = Temperature Annual Range (BIO5-BIO6)
# BIO8 = Mean Temperature of Wettest Quarter
# BIO9 = Mean Temperature of Driest Quarter
# BIO10 = Mean Temperature of Warmest Quarter
# BIO11 = Mean Temperature of Coldest Quarter
# BIO12 = Annual Precipitation
# BIO13 = Precipitation of Wettest Month
# BIO14 = Precipitation of Driest Month
# BIO15 = Precipitation Seasonality (Coefficient of Variation)
# BIO16 = Precipitation of Wettest Quarter
# BIO17 = Precipitation of Driest Quarter
# BIO18 = Precipitation of Warmest Quarter
# BIO19 = Precipitation of Coldest Quarter