#-----------------------------------------
#
# This function calculates the area deforested within each patrol sector
# It works by sequentially intersecting the deforestation shapefile with each
# sector and then calculating the area.
#
# Tom Swinfield
# 17-02-21
#
#-----------------------------------------


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
