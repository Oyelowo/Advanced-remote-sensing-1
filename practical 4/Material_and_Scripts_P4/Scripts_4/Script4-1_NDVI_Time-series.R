###Practical 4#######
###Script 4-1
###Create NDVI time-series for selected areas
###The parts of the script you need to fill or complement are marked with **




##**Load required packages (raster, maptools)
library(raster)
library(maptools)
 
##**Clear Environment e.g. rm(list = ls())
rm(list=ls())


#**set the work space for the folder where you saved the shapefiles files
setwd("C:/Users/oyelo/Desktop/advanced remote sensing/practical 4/Material_and_Scripts_P4/shapefile")
getwd()

#**read the shapefile of Forest and Savannah
shape.forest=readShapePoly("Forest.shp")  
shape.savannah=readShapePoly("savannah.shp")

#**define CRS for savannah
crs(shape.forest)="+proj=utm +zone=37 ellps=WGS84 +south "#set current projection of the forest shapefile
crs(shape.savannah)="+proj=utm +zone=37 ellps=WGS84 +south " #set current projection of the savannah shapefile

#**Change CRS to UTM using spTransform for savannah
shape.forest <-spTransform(shape.forest, CRS("+proj=utm +zone=37 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
shape.savannah<-spTransform(shape.savannah, CRS("+proj=utm +zone=37 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))

##**Change the working directory to the folder where you saved the Monthly ndvi 
setwd("C:/Users/oyelo/Desktop/advanced remote sensing/practical 4/Material_and_Scripts_P4/Task_1_2_and_3_MODIS_monthly_NDVI")
getwd()

ndvi.forest=as.numeric() #creates an empty numeric vector
ndvi.savannah=as.numeric() #creates an empty numeric vector

for (year in 2009:2013) {
for (month in 1:12){ #Start a loop to extract the data for each month

  raster.name=paste("MOD13Q1_Taita_",year,"_", month,".tif", sep="") #create the raster file name for a certain month
  month.ndvi.raster=raster(raster.name) #read the ndvi file for the respective month
  
  extract_ndvi_forest = extract(month.ndvi.raster, shape.forest, fun=mean, na.rm=TRUE) #extract ndvi inside the shapelfile and calculate the average of all pixels inside the shapefile
  extract_ndvi_savannah = extract(month.ndvi.raster, shape.savannah, fun=mean, na.rm=TRUE) #extract ndvi inside the shapelfile and calculate the average of all pixels inside the shapefile
  
  #now the following line inserts the ndvi value inside the empty vector
  ndvi.forest=c(ndvi.forest, as.numeric(extract_ndvi_forest))#note that this vector "grows" in every loop iteraction
  ndvi.savannah=c(ndvi.savannah, as.numeric(extract_ndvi_savannah))#note that this vector "grows" in every loop iteraction
  
  #**complement the loop to extract data from the Savannah area
  #now the following line inserts the ndvi value inside the empty vector
}
}

##** inspect your data to certify if you need to apply any scale factor
ndvi.forest <- ndvi.forest/10000
ndvi.forest

ndvi.savannah <- ndvi.savannah/10000
ndvi.savannah



start.ndvi=c(2009, 1) #creates a vector with the initial year and month of your dataset
ndvi.forest.ts<-ts(as.numeric(ndvi.forest), start=start.ndvi, freq=12) #Creates a time-series object


##** add one line to create a time-series for the savannah data
ndvi.savannah.ts<-ts(as.numeric(ndvi.savannah), start=start.ndvi, freq=12) #Creates a time-series object


##**plot your time-series using the plot() function
plot(ndvi.forest.ts, axes=FALSE,ann=FALSE, ylim=c(0,1), col="green")

par(new=T)
plot(ndvi.savannah.ts,main="NDVI time series", ylab="NDVI", ylim=c(0,1), col="red")

legend("bottomleft",c("forest","savanna"), lty=c(1,1), col=c("green","red"))

##**Use par(new=T) to add a second time-series in the same plot
##**don't forget to fix the y axe using the ylim parameter
##**Use different colours for each time-series and add legend to the graphic

setwd("C:/Users/oyelo/Desktop/advanced remote sensing/practical 4/Material_and_Scripts_P4/Results_test_p4")
save(ndvi.forest.ts, ndvi.savannah.ts, file = "TAITA_NDVI.RData") #This command saves the results in .RData format
#The RData format can store any variables from your workspace
#The advantage is that you can easily load again your results, without having to make all the calculations again
#it also keeps the format of the objects (for example, time-series objects remain like that when loaded by another script)
