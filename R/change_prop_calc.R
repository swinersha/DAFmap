
# This is a script to calculate the deforestation rate within each sector
# From the GLAD alerts
#
# Tom Swinfield
# 17-03-08
#

# HEADER -----------------------------


change_prop_calc<-function(img, poly){
  img<-crop(img, poly)
  img[is.na(values(img))]<-0
  img <- mask(img, poly, inverse=FALSE) # sets values outside of the polygon to NA
  change<-sum(na.omit(values(img))!=0)
  total<-sum(na.omit(values(img))==0)
  prop<-(change/total)*100
  return(prop)
}