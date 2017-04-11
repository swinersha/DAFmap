library(lubridate)

glad_name<-paste("data/SEA_day_", gsub("-", "", Sys.Date()), ".tif", sep="")
glad_exists<-file.exists(pattern=glad_name, recursive=TRUE)

if(!glad_exists)
  download.file("http://glad.geog.umd.edu/alarm/SEA_day_2017n.tif", glad_name)

in_year<-list.files(pattern = paste("SEA_day_", year(Sys.Date()), sep=""), recursive=TRUE)

cleanup.ind<-!grepl(glad_name, in_year)
cleanup<-in_year[cleanup.ind]

if(length(cleanup)>=1)
  file.remove(cleanup)
