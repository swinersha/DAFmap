
# This is a script to calculate and produce a standardised set of forest threat
# Maps
#
# Tom Swinfield
# 17-03-08
#

# HEADER -----------------------------

rm(list=ls())

library(maptools)
library(rgdal)
library(raster)
library(ggplot2)
library(splancs)
library(RColorBrewer)
library(rgeos)
library(lubridate)
library(dplyr)

source('R/area_lost.R')
source('R/change_prop_calc.R')
source('R/col_pal.R')
source('R/unique.col.R')

update_choice<-select.list(c('Yes', 'No'), preselect='visible', title='Would you like to update GLAD?')
if(update_choice=='Yes')
  source('R/Download GLAD.R')

sectors<-readOGR("data/Sektor patroli.shp")
deforest<-spTransform(deforest, sectors@proj4string)
sectors$Area<-sapply(sectors@polygons, function(x) x@area/10000) # The sector Areas in hectares


# Automate the GLAD assessment acccording to time.

glad16<-raster("Data/SEA_day_2016n.tif")
glad_name<-paste("data/SEA_day_", gsub("-", "", year(Sys.Date())), ".tif", sep="")

glad_name<-paste("^SEA_day_", gsub("-", "", year(Sys.Date())), "[[:digit:]]*.tif$", sep="")
glad_name<-list.files("data", pattern=glad_name, full.names=TRUE)
glad17<-raster(glad_name)

smart_path<-'data/SMART/2016'


# DEFORESTATION REKI data -----------------------------


plot(deforest, col=unique.col(deforest$TAHUN_PH, pal.name='Bulbasaur'))

# Calculate the area lost:
sectors$df12<-area.lost(sectors, df=deforest[deforest$TAHUN_PH==2012,])
sectors$df13<-area.lost(sectors, df=deforest[deforest$TAHUN_PH==2013,])
sectors$df14<-area.lost(sectors, df=deforest[deforest$TAHUN_PH==2014,])
sectors$df15<-area.lost(sectors, df=deforest[deforest$TAHUN_PH==2015,])
sectors$df16<-area.lost(sectors, df=deforest[deforest$TAHUN_PH==2016,])
# Calculate the rate of loss relative to the sector size:
sectors$df12_rate<-round(sectors$df12/sectors$Area *100, 1)
sectors$df13_rate<-round(sectors$df13/sectors$Area *100, 1)
sectors$df14_rate<-round(sectors$df14/sectors$Area *100, 1)
sectors$df15_rate<-round(sectors$df15/sectors$Area *100, 1)
sectors$df16_rate<-round(sectors$df16/sectors$Area *100, 1)



# DEFORESTATION GLAD data -----------------------------

# crop glad rasters to sector limits:
glad16<-crop(glad16, extent(spTransform(sectors, crs(glad16))))
glad16<-projectRaster(glad16, crs=crs(sectors))
glad17<-crop(glad17, extent(spTransform(sectors, crs(glad17))))
glad17<-projectRaster(glad17, crs=crs(sectors))

# Calculate deforestation within sectors:

sectors$df16_glad<-sapply(1:nrow(sectors), function(i) change_prop_calc(glad16, sectors[i,]))
sectors$df17_glad<-sapply(1:nrow(sectors), function(i) change_prop_calc(glad17, sectors[i,]))
# Convert current year to an annual % rate.
# Proportion of year:
year_prop<-1/(yday(Sys.Date())/365)
sectors$df17_glad<-sectors$df17_glad * year_prop

# A function to classify the threat level:
thrt_classify<-function(x, breaks, labels=threat_labels, include.lowest = TRUE)
  cut(x,
      breaks = breaks,
      labels = labels,
      include.lowest=TRUE) 

threat_labels<-c('low', 'med', 'high', 'extreme')

# Classifies the threats:
df_cut<-c(0,2,5,15,100)
sectors$df16_thrt<-thrt_classify(sectors$df16_glad, df_cut) 
sectors$df17_thrt<-thrt_classify(sectors$df17_glad, df_cut) 

# SMART -----------------------------

# Read in the SMART effort data:
effort_header<-names(read.csv(paste(smart_path, "/Patrol_effort_by_sector2.csv", sep='')))
effort<-read.csv(paste(smart_path, "/Patrol_effort_by_sector2.csv", sep=''), skip=1, head=FALSE)
names(effort)<-effort_header 
# Calculate the effort:
# The column without the 1 is the total distance; 
# the column with the one is the total number of patrols
effort_hours<-effort$Number.of.Patrol.Hours[match(sectors$Sector,effort$X)]
n_surveys<-effort$Number.of.Patrols[match(sectors$Sector,effort$X)]

# Adds the patrol effort by distance:
sectors$effort_hours<-effort_hours

# Read in the SMART threat data
encr<-read.csv(file.path(smart_path, "New_encroachment_observations_by_sector_000052.csv"))
logg<-read.csv(file.path(smart_path, "Illegal_logging_observations_by_sector_000051.csv"))
hunt<-read.csv(file.path(smart_path, "Hunting_observations_by_sector_000050.csv"))

# Add the threat data to the sectors:
# Threat observations:
sectors$encr<-encr$Count.Observations.Ancaman[match(sectors$Sector,encr$X)]
sectors$logg<-logg$Count.Observations.Ancaman[match(sectors$Sector,logg$X)]
sectors$hunt<-hunt$Count.Observations.Ancaman[match(sectors$Sector,hunt$X)]

abs_cut<-c(0,1,10,20,100)

sectors$encr_abs_thrt<-thrt_classify(sectors$encr, abs_cut) 
sectors$logg_abs_thrt<-thrt_classify(sectors$logg, abs_cut)
sectors$hunt_abs_thrt<-thrt_classify(sectors$hunt, abs_cut) 



# Convert NAs to 0s
sectors$encr[is.na(sectors$encr)]<-0
sectors$logg[is.na(sectors$logg)]<-0
sectors$hunt[is.na(sectors$hunt)]<-0


# Rates of threat detection:
sectors$encr_effort<-sectors$encr/(effort_hours/100)
sectors$logg_effort<-sectors$logg/(effort_hours/100)
sectors$hunt_effort<-sectors$hunt/(effort_hours/100)
sectors$no_survey<-effort_hours<4

# Classifies the threats:
obs_cut<-c(0,1,10,20,100)

sectors$encr_thrt<-thrt_classify(sectors$encr_effort, obs_cut) 
sectors$logg_thrt<-thrt_classify(sectors$logg_effort, obs_cut)
sectors$hunt_thrt<-thrt_classify(sectors$hunt_effort, obs_cut) 


# Plotting deforestation ----
par(mfrow=c(2,2), mar=(c(1,1,1,1)))

# REKI rate
# 2016
plot(sectors, col=col_pal(sectors$df16_rate, x_range=c(0,30)), main='REKI 2016')
plot(deforest[deforest$TAHUN_PH==2016,], col=rgb(0.1,0.0,0.9,0.8), add=TRUE, border=NA)
# 2017
#plot(sectors, col=col_pal(sectors$df17_rate, x_range=c(0,20)), main='REKI 2017')
#plot(deforest[deforest$TAHUN_PH==2017,], col=rgb(0.1,0.0,0.9,0.8), add=TRUE, border=NA)

# GLAD rate
# 2016
plot(sectors, col=col_pal(sectors$df16_glad, x_range=c(0,30)), main='GLAD 2016')
plot(glad16, col=rgb(0.1,0.0,0.9,0.8), add=TRUE, legend=FALSE)
# 2017
plot(sectors, col=col_pal(sectors$df17_glad, x_range=c(0,30)), main='GLAD 2017')
plot(glad17, col=rgb(0.1,0.0,0.9,0.8), add=TRUE, legend=FALSE)

# Plotting threats ----

# Absolute threat detections
par(mfrow=c(2,2), mar=(c(1,1,1,1)))
plot(sectors, col=col_pal(sectors$encr, sectors$no_survey), main='Encroachment')
plot(sectors, col=col_pal(sectors$logg, sectors$no_survey), main='Illegal logging')
plot(sectors, col=col_pal(sectors$hunt, sectors$no_survey), main='Hunting')

# Threat rates
par(mfrow=c(2,2), mar=(c(1,1,1,1)))
plot(sectors,
     col=col_pal_thrt(sectors$df16_glad, breaks=c(0,2,5,15,100)),
     main='Forest loss')
plot(sectors,
     col=col_pal_thrt(sectors$encr_effort, breaks=c(0,1,10,20,100), sectors$no_survey),
     main='Encoachment')
plot(sectors,
     col=col_pal_thrt(sectors$logg_effort, breaks=c(0,1,10,20,100), sectors$no_survey),
     main='Illegal logging')
plot(sectors,
     col=col_pal_thrt(sectors$hunt_effort, breaks=c(0,1,10,20,100), sectors$no_survey),
     main='Hunting')


# Plot the overall threat level map ----

# Function to calculated the overall threat level:
greatest_thrt<-function(x, levels){
  x<-factor(x, levels=levels)
  if(any(is.na(x)))
    y<-factor(NA, levels = levels)
  else
    y <- sort(x, decreasing =TRUE)[1]
  return(y)
}

# Number of observations:
tmp<-sectors@data[,c("encr_abs_thrt", "logg_abs_thrt", "hunt_abs_thrt", "df16_thrt")]
sectors$total_thrt_abs <- apply(tmp, 1, function(x) greatest_thrt(x, levels = threat_labels))

# Standardised Threat metric:
tmp<-sectors@data[,c("encr_thrt", "logg_thrt", "hunt_thrt", "df16_thrt")]
sectors$total_thrt <- apply(tmp, 1, function(x) greatest_thrt(x, levels = threat_labels))


par(mfrow=c(1,1), mar=(c(1,1,1,1)))
plot(sectors,
     col=col_pal_total_thrt(sectors$total_thrt_abs),
     main='Total threat level_observations')

par(mfrow=c(1,1), mar=(c(1,1,1,1)))
plot(sectors,
     col=col_pal_total_thrt(sectors$total_thrt),
     main='Total threat level_standardised')



writeOGR(sectors, "data/sectors_w_threats.shp",
         layer="sectors_w_threats",
         driver="ESRI Shapefile")

write.csv(sectors@data, "data/sectors_w_threats.csv")


# Plotting effort ----

# SMART Effort
#par(mfrow=c(2,2), mar=(c(1,1,1,1)))
#plot(sectors, col=col_pal(sectors$Jalan.Kaki, sectors$no_survey), main='Foot')
#plot(sectors, col=col_pal(sectors$Mobil, sectors$no_survey), main='Car')
#plot(sectors, col=col_pal(sectors$Motor, sectors$no_survey), main='Motorbike')
#plot(sectors, col=col_pal(sectors$Perahu.Ketek, sectors$no_survey), main='Boat')

# Some correlations:
ggplot(sectors@data, aes(y=logg, x=log(effort_hours))) + geom_point()
ggplot(sectors@data, aes(y=encr, x=log(effort_hours))) + geom_point()
ggplot(sectors@data, aes(y=hunt, x=log(effort_hours))) + geom_point()

par(mfrow=c(3,2), mar=(c(4,4,2,2)))
hist(sectors$df16_glad)
hist(sectors$df17_glad)
hist(sectors$encr_effort)
hist(sectors$logg_effort)
hist(sectors$hunt_effort)

