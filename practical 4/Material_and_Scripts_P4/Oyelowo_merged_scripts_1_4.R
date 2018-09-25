
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
mean(ndvi.forest)

ndvi.savannah <- ndvi.savannah/10000
ndvi.savannah
summary(ndvi.savannah)



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




###Practical 4#######
###Script 4-2
###Perform BFAST analysis using NDVI time-series from selected areas in the Taita Hills
###The parts of the script you need to fill or complement are marked with **


##**Install and load the bfast package
#install.packages("bfast")
library(bfast)

##**Clear Environment
rm(list=ls())

##** set the workspace for the folder where you saved the RData file
setwd("C:/Users/oyelo/Desktop/advanced remote sensing/practical 4/Material_and_Scripts_P4/Results_test_p4")

load("TAITA_NDVI.RData") #Load your data

rdist <- 10/length(ndvi.forest.ts) #define minimal segment size between detected breaks (see help for details)
#the following line performs the BFAST analysis in the forest NDVI time-series object
#check R help for more information about the parameters used
fit <- bfast(ndvi.forest.ts ,h=rdist, season="harmonic", max.iter=1, breaks=2)

plot(fit, main="BFAST - Taita hills Forest site") #standard bfast plot

#**Repeat the bfast analysis using the savannah NDVI time-series
rdist <- 10/length(ndvi.savannah.ts) #define minimal segment size between detected breaks (see help for details)
#the following line performs the BFAST analysis in the forest NDVI time-series object
#check R help for more information about the parameters used
fit <- bfast(ndvi.savannah.ts ,h=rdist, season="harmonic", max.iter=1, breaks=2)

plot(fit, main="BFAST - Taita hills Savannah site") #standard bfast plot













##Practical 4
##Script 4-3
##Bi-temporal change detection
###The parts of the script you need to fill or complement are marked with **

##**Load raster package
library(raster)

##**Clear (environment
rm(list=ls())

##**Set the workspace for the folder where your NDVI imagery are located
setwd("C:/Users/oyelo/Desktop/advanced remote sensing/practical 4/Material_and_Scripts_P4/Task_1_2_and_3_MODIS_monthly_NDVI")
getwd()
##**create a list of all NDVI images for the year 2009 (use the "list.files" function)
ndvi2009 = list.files(pattern="*_2009_*")
ndvi2009
##**create a list of all NDVI images for the year 2013 (use the "list.files" function)
ndvi2013=list.files(pattern="*_2013_*")
ndvi2013

##**read all 2009 imagery using the "stack" function
ndvi.stack2009<-stack(ndvi2009)
##**read all 2013 imagery using the "stack" function
ndvi.stack2013<-stack(ndvi2013)

##**calculate the average of all 2009 ndvi images, using the "calc" function
mean.NDVI2009<- calc(ndvi.stack2009, fun=mean)

##**calculate the average of all 2013 ndvi images, using the "calc" function
mean.NDVI2013<- calc(ndvi.stack2013, fun=mean)

mean.NDVI2009<-mean.NDVI2009/10000
mean.NDVI2013<-mean.NDVI2013/10000

#**Take a look at your results using plot function
#par(mfrow = c(1, 2))
#plot(mean.NDVI2009)

#plot(mean.NDVI2013)

##**perform a image rationing analysis using the 2009 and 2013 mean ndvi imagery
r09.13<-mean.NDVI2009/mean.NDVI2013
r09.13

##**perform a image differencing analysis using the 2009 and 2013 mean ndvi imagery
d09.13<- mean.NDVI2009-mean.NDVI2013
d09.13

par(mar = c(2, 2, 2, 2), mfrow=c(4,2))
##**plot the histogram of the ratio image
hist(r09.13, main="NDVI_2009/NDVI_2013")
##**Plot the histogram of the differencing image
hist(d09.13, main="NDVI_2009-NDVI_2013")

##Classify the ratio and differencing image using thresholds obtained from a visual inspection of the histograms
class.ratio=r09.13
class.ratio[r09.13>0.9 & r09.13<1.1] <- 0 #0 represents no significant change
class.ratio[r09.13<0.9] <- 1 #1represents negative change
class.ratio[r09.13>1.1] <- 2  #2represents positive change
plot(class.ratio, main="class ratio", legend = T)
hist(class.ratio, main="class ratio")




class.diff=d09.13
class.diff[d09.13>(-0.05) & r09.13<0.05] <- 0 #0 represents no significant change
class.diff[d09.13<(-0.05)] <- 1 #1represents negative change
class.diff[d09.13> 0.05 ]<- 2  #2represents positive change
plot(class.diff, main="classified differencing", legend = T)
hist(class.diff, main="classified differencing")
plot.new()
legend("top", legend = c("Positive change", "Negative change", "No Change"), bty = c(1,1), fill = c("green" ,"yellow", "white"), cex= 1)

# PLOTS 
#par(mar = c(1, 1, 1, 1),mfrow=c(3,1))
#plot(class.ratio, legend = T)
#plot(class.diff, legend = T)
#plot.new()
#legend("top", legend = c("Positive change", "Negative change", "No Change"), bty = c(1,1), fill = c("green" ,"yellow", "white"), cex= 1)


##Classification example##

#Let's say that "im.ratio" is the result of my image rationing analysis and "class.ratio" is the classified image (that still doesn't exist)
#class.ratio=im.ratio #first create "class.ratio" as an exact copy of im.ratio
#class.ratio[im.ratio>0.9 & im.ratio<1.1] <- 1 #where the values in im.ration are between 0.9 and 1.1, the values of class.ratio is set to 1













###Practical 4#######
###Script 4-4
###Perform change vector analysis and plot polar histogram
###The parts of the script you need to fill or complement are marked with **

##**Install and load the plotrix package
#install.packages("plotrix")
library(plotrix)
##** load the raster and maptools packages
#library(raster)
#library(maptools)

##**Clear environment
rm(list = ls())

##**set the work space for the folder where you saved the MODIS red and NIR band files
setwd("C:/Users/oyelo/Desktop/advanced remote sensing/practical 4/Material_and_Scripts_P4/Tasks_4_MODIS_red_NIR")
getwd()

##**read red band from September 2013
red.date1<-raster("MOD13Q1.2013241.red.UTM_Taita.tif")

##**read red band from December 2013
red.date2<-raster("MOD13Q1.2013353.red.UTM_Taita.tif")

##**read NIR band from September 2013
nir.date1<-raster("MOD13Q1.2013241.NIR.UTM_Taita.tif")

##**read NIR band from December 2013
nir.date2<-raster("MOD13Q1.2013353.NIR.UTM_Taita.tif")

##The following lines calculate the change angles
##To understand the change angle calculation, refer to the equations in the lecture material
delta.NIR=nir.date2-nir.date1 #Calculated NIR difference between September and December
delta.red=red.date2-red.date1 #Calculated red difference between September and December
alfa=atan((delta.red)/(delta.NIR)) #Calculate the change angle

alfa=(alfa*180)/pi #Convert change angle from radian to degrees

###The following code convert the angle values to a 0 to 360 degrees range### see lecture material
alfa[delta.NIR<0 & delta.red>0]<-(alfa+180)[delta.NIR<0 & delta.red>0]
alfa[delta.NIR<0 & delta.red<0]<-(alfa+180)[delta.NIR<0 & delta.red<0]
alfa[delta.NIR>0 & delta.red<0]<-(alfa+360)[delta.NIR>0 & delta.red<0]
alfa[delta.NIR==0 & delta.red!=0]<-270

#plot(alfa, main="change in angle image")#visualize the change angle image

intervals=seq(0, 360, by=5)#create a vector describing the intervals of the histogram
#First I create a normal histogram, with the intervals between 0 to 360 and the bars width equal to 5
#(we just need this to obtain the count of each angle value.. so you don't need to present this normal histogram in your report)
h=hist(alfa, breaks=intervals) #creates the normal histogram 

frequency<-h$counts #counts the frequency of each angle
angle<-h$mids #gets the angle values


par(mfrow=c(1,2))
polar.plot(frequency,angle, main="CVA - Change angle",lwd=3,line.col=4) #Plot a polar histogram

#plot(frequency)
##**Calculate the change magnitude

#plot change in magnitude Histogram
change.mag<-sqrt((delta.red^2)+(delta.NIR^2))
hist(change.mag, main="Change in magnitude histogram")


par(mfrow=c(2,2))
#plot change angle image(from 0 to 360)
plot(alfa, main="change in angle image")#visualize the change angle image

##**Plot the image of the change magnitude
plot(change.mag, main="change in magnitude")


##**Calculate the NDVI for the September image
ndvi.sept<-(nir.date1-red.date1)/(nir.date1+red.date1)
plot(ndvi.sept, main="NDVI September")
##**Calculate the NDVI for the December image
ndvi.dec<-(nir.date2-red.date2)/(nir.date2+red.date2)
plot(ndvi.dec, main="NDVI December")














