#' sector_df_assess
#' 
#' This is a script to calculate the deforestation rate within each sector
#' From the GLAD alerts
#' 
#' @param sectors A SpatialPolygonsDataFrame describing the patrol sectors
#' @param glad_period A raster of the GLAD forest change alerts for the period of interest
#' @param sector_update Logical indicating whether the shapefile defining the sectors
#' has been updated since the function was last run. This should be set to TRUE the 
#' first time the function is run. 
#' @return  
#' @export
#' @author Tom Swinfield
#' @details The glad_period raster can be produced automatically by the function 
#' glad_by_date
#' 
#' Created 17-03-08

# This function is now deprecated in favour of the sector_df_assess (BELOW)
# change_prop_calc<-function(img, poly){
#   img<-crop(img, poly)
#   img[is.na(values(img))]<-0
#   img <- mask(img, poly, inverse=FALSE) # sets values outside of the polygon to NA
#   change<-sum(na.omit(values(img))!=0)
#   total<-sum(na.omit(values(img)) ==0) + change
#   prop<-(change/total)*100
#   return(prop)
# }


sector_df_assess<-function(sectors, glad_period, sector_update = FALSE){
  if(sector_update){
    # Create an object with reference to the cells relevant to each sector within the 
    # GLAD image:
    sector_ind<-extract(glad_period, sectors, cellnumbers=TRUE)
    save(sector_ind,file="data/GLAD/sector_ind")
  }
  else{
    # If it already exists, just load it:
    if(file.exists("data/GLAD/sector_ind")){
      load("data/GLAD/sector_ind")
    }
    else{
      cat("\"data/GLAD/sector_ind\" does not exist; please run sector_update first,\n")
      return(-1)
    }
  }
  # Calculate the amount of deforestation according to GLAD for the period
  sector_df<-sapply(sector_ind, function(x){
    x_out<<-x
    img<-glad_period[x[,"cell"]]
    change<-sum(na.omit(img)!=0)
    total<-sum(na.omit(img) ==0) + change
    prop<-(change/total)*100
    }  
  )
  return(sector_df)
}