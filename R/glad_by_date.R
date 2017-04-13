




load_glad_by_date <- function(date, poly) {
  regx <- paste("SEA_day_", year(date), ".*.tif$", sep = "")
  name <-
    list.files(path = "data/GLAD",
               pattern = regx,
               full.names = TRUE)
  data <- raster(name)
  data <- crop(data, extent(spTransform(poly, crs(data))))
  data <- projectRaster(data, crs = crs(poly))
}


glad_by_date <- function(from_date, to_date, sectors) {
  period_days <- to_date - from_date
  to_data <- load_glad_by_date(to_date, sectors)
  
  to_data[is.na(to_data)] <- 0
  to_data[to_data > yday(to_date)] <- 0
  
  if (year(from_date) == year(to_date)) {
    to_data[to_data < yday(from_date)] <- 0
    to_data[to_data != 0] <- 1
  }
  else{
    from_data <- load_glad_by_date(from_date, sectors)
    from_data[from_data < yday(from_date)] <- 0
    to_data[from_data != 0] <- 1
  }
  return(to_data)
}

# A function to convert from the analysis period to dates:
period_to_dates <- function(analysis_period) {
  if (analysis_period == "year to date")
    dates <- c(from_date = today() - period("1year"),
               to_date = today())
  if (analysis_period == "Quarter to date")
    dates <-
      c(from_date = today() - period("3months"),
        to_date = today())
  if (analysis_period == "Last year") {
    to_date <- floor_date(today(), "year") - days(1)
    from_date <- floor_date(to_date, "year")
    dates <- c(from_date = from_date, to_date = to_date)
  }
  if (analysis_period == "Last quarter") {
    to_date <- floor_date(today(), "quarter") - days(1)
    from_date <- floor_date(to_date, "quarter")
    dates <- c(from_date = from_date, to_date = to_date)
  }
  if (analysis_period == "Last month") {
    to_date <- floor_date(today(), "month") - days(1)
    from_date <- floor_date(to_date, "month")
    dates <- c(from_date = from_date, to_date = to_date)
  }
  if (analysis_period == "Last 60 days")
    dates <- c(from_date = today() - days(60), to_date = today())
  if (analysis_period == "Last 30 days")
    dates <- c(from_date = today() - days(30), to_date = today())
  
  id_names<-names(dates)
  names(dates)<-NULL
  dates<-as.list(dates)
  names(dates)<-id_names
  
  return(dates)
}


choose_dates<-function(){
  analysis_period<-select.list(c("Year to date", 
                               "Quarter to date",
                               "Last year",
                               "Last quarter",
                               "Last month",
                               "Last 60 days", 
                               "Last 30 days"
  ), title="Tolong buat seleksi periode", graphics=TRUE)
  period_to_dates(analysis_period)
}




