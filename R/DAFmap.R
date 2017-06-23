#-----------------------------------------
#

#
# Tom Swinfield
# 17-02-21
#
#-----------------------------------------

rm(list=ls())

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


unique.col<-function(x, pal.name){
  x<-sort(x)
  u.x<-unique(x)
  col.pal<-ichooseyou(pal.name, length(u.x))
  y<-vector(mode='character', length=length(x))
  for(i in 1:length(col.pal))
    y[x==u.x[i]]<-col.pal[i]
  return(y)
}    
  
plot(deforest, col=unique.col(deforest$TAHUN_PH, pal.name='Bulbasaur'))

# Extract just the data for 2016:


area.lost<-function(sectors, df){
  area_lost<-vector(length=nrow(sectors), mode='numeric')
  for(i in 1:nrow(sectors)){
    intersected<-gIntersection(sectors[i,], df)
    if(!is.null(intersected))
      area_lost[i]<-sum(sapply(intersected@polygons[[1]]@Polygons, 
                           function(x) x@area/10000)) # the area of the intersection
    else
      area_lost[i]<-0
  }
  area_lost
  return(area_lost)
}


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

# Do this for 2015 and 2016 and 2017 
# correct for area
# standardise the colour

col.pal<-colorRampPalette(colors=c('white', 'red'))
cols12<-col.pal(10)[as.numeric(cut(sectors$df12_rate, seq(0, 20, length = 10)))]
cols13<-col.pal(10)[as.numeric(cut(sectors$df13_rate, seq(0, 20, length = 10)))]
cols14<-col.pal(10)[as.numeric(cut(sectors$df14_rate, seq(0, 20, length = 10)))]
cols15<-col.pal(10)[as.numeric(cut(sectors$df15_rate, seq(0, 20, length = 10)))]
cols16<-col.pal(10)[as.numeric(cut(sectors$df16_rate, seq(0, 20, length = 10)))]

par(mfrow=c(2,2), mar=(c(1,1,1,1)))
plot(sectors, col=cols13, main='2013')
plot(sectors, col=cols14, main='2014')
plot(sectors, col=cols15, main='2015')
plot(sectors, col=cols16, main='2016')




