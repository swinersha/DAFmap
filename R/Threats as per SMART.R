
library(maptools)
library(rgdal)
library(raster)
library(ggplot2)
library(splancs)
library(RColorBrewer)
library(palettetown)
library(rgeos)

deforest<-readOGR("data/deforestation.shp")
sectors<-readOGR("data/Patrol sectors.shp")
deforest<-spTransform(deforest, sectors@proj4string)
sectors$Area<-sapply(sectors@polygons, function(x) x@area/10000) # The sector Areas in hectares

effort_header<-names(read.csv("data/SMART/Patrol_effort_by_sector_000034.csv"))
effort<-read.csv("data/SMART/Patrol_effort_by_sector_000034.csv", skip=2, head=FALSE) 
names(effort)<-effort_header 

# Calculate the effort:
# The column without the 1 is the total distance; 
# the column with the one is the total number of patrols
plot(effort$Jalan.Kaki~effort$Jalan.Kaki.1)
effort_dist<-effort[,c('Jalan.Kaki', 'Mobil', 'Motor', 'Perahu.Ketek', 'Speed.Boat')]
n_surveys<-effort[,c('Jalan.Kaki.1', 'Mobil.1', 'Motor.1', 'Perahu.Ketek.1', 'Speed.Boat.1')]
effort_dist$total<-rowSums(effort_dist, na.rm=TRUE)
n_surveys$total<-rowSums(n_surveys, na.rm=TRUE)

# Read in the SMART threat data
thrt<-read.csv("data/SMART/Threat_observations_by_sector_000028.csv")
encr<-read.csv("data/SMART/New_encroachment_observations_by_sector_000030.csv")
logg<-read.csv("data/SMART/Illegal_logging_observations_by_sector_000033.csv")
hunt<-read.csv("data/SMART/Hunting_observations_by_sector_000032.csv")

sectors$threat<-thrt$Count.Observations.Ancaman
sectors$encr<-encr$Count.Observations.Ancaman
sectors$logg<-logg$Count.Observations.Ancaman
sectors$hunt<-hunt$Count.Observations.Ancaman

sectors$threat_dist<-sectors$threat/effort_dist$total
sectors$encr_dist<-sectors$encr/effort_dist$total
sectors$logg_dist<-sectors$logg/effort_dist$total
sectors$hunt_dist<-sectors$hunt/effort_dist$total
sectors$no_survey<-effort_dist$total==0

sectors@data<-cbind(sectors@data, effort_dist)

col_pal<-function(x, no_survey, n_cols=10, x_range=NULL){
  col.pal<-colorRampPalette(colors=c('white', 'red'))
  if(is.null(x_range))
    x_cuts<-cut(x, seq(min(x, na.rm=T), max(x, na.rm=T), length = n_cols)) # bins the x values
  else
    x_cuts<-cut(x, seq(x_range[1], x_range[2], length = n_cols)) # bins the x values
  x_cuts<-as.numeric(x_cuts) # as a factor
  x_cols<-col.pal(n_cols)[x_cuts]
  x_cols[no_survey]<-'grey'
  return(x_cols)
}

par(mfrow=c(2,2), mar=(c(1,1,1,1)))
plot(sectors, col=col_pal(sectors$threat, no_survey), main='All threats')
plot(sectors, col=col_pal(sectors$encr, no_survey), main='Encroachment')
plot(sectors, col=col_pal(sectors$logg, no_survey), main='Illegal logging')
plot(sectors, col=col_pal(sectors$hunt, no_survey), main='Hunting')

par(mfrow=c(2,2), mar=(c(1,1,1,1)))
plot(sectors, col=col_pal(sectors$threat_dist, no_survey), main='All threats')
plot(sectors, col=col_pal(sectors$encr_dist, no_survey), main='Encroachment')
plot(sectors, col=col_pal(sectors$logg_dist, no_survey), main='Illegal logging')
plot(sectors, col=col_pal(sectors$hunt_dist, no_survey), main='Hunting')

par(mfrow=c(2,2), mar=(c(1,1,1,1)))
plot(sectors, col=col_pal(sectors$Jalan.Kaki, no_survey), main='Foot')
plot(sectors, col=col_pal(sectors$Mobil, no_survey), main='Car')
plot(sectors, col=col_pal(sectors$Motor, no_survey), main='Motorbike')
plot(sectors, col=col_pal(sectors$Perahu.Ketek, no_survey), main='Boat')



