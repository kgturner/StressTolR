#general look at climate for ST populations
#see Extract.climate.data.R and Squishr.func.R
popWC <- read.table(file.choose(), header=T, sep="\t") #popcoord_worldclim.txt the set with some shifted points to produce data from worldclim

pop <- rbind(popInv, popNat)
rownames(pop) <- pop$Pop
pop <- pop[1:3]
pop <- pop[order(pop$Pop),]

#popWC <- popWC[rownames(popWC) == rownames(pop),]


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




####NOTE: WorldClim data, even at highest res, is averaged over 1 km2.
#If your location is too close to a coast (i.e. less than 1 km2),
#there will not be any information (nothing but NAs) in this data set.
#Therefore, it may be necessary to approximate some locations by moving
#them inland. I did this using Google Earth.
#load location coordinates as SpatialPoints
for(i in popWC$Pop){
  assign(i,SpatialPoints(as.matrix(t(c(popWC[i,2], popWC[i,1])))))
}
#check that SpatialPoints load correctly from geoTIFFs
#no column should be entirely NAs (if they are, see note above)
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
#write table
write.table(clim, file="bioclimdata.txt")
#load table
clim <- read.table("bioclimdata.txt", header=TRUE)