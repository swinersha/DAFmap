#-----------------------------------------
#
# Downloads glad data to the local drive
# It makes an effort to update the raster for the current year if necessary
# At the beginning of a new year the last years data is archived
# ... you still need to check this last bit. You should make sure this is the data
#     for the full year.
#
# Tom Swinfield
# 17-02-21
#
#-----------------------------------------

library(lubridate)


# vector from 2015 to today
current_year<-year(today())
years_avail<-rev(2016:current_year)

#year<-years_avail[3]

glad.files<-lapply(years_avail, function(year){
  year_regx<-paste("SEA_day_", year, ".*.tif$", sep="")
  glad_name<-list.files(path = "data/GLAD", pattern=year_regx, full.names = TRUE)
  if(length(glad_name)==0)
    glad_exists<-FALSE
  else
    glad_exists<-file.exists(pattern=glad_name)
  
  year_test<-year==current_year
  
  glad_link<-paste("http://glad.geog.umd.edu/alarm/SEA_day_", year, "n.tif", sep="")
  
  # For the current year update glad:
  if(glad_exists & year_test){
    # Check the date and test for fresh download:
    file_date<-unlist(regmatches(glad_name, gregexpr("[[:digit:]]+", glad_name)))
    file_date<-ymd(file_date)
    days_old <- as.numeric(today() - file_date)
    cat("Perhatian: data GLAD tahun ini sudah", days_old, "hari tua.\n")
    download_test<-select.list(c('Ya', 'Tidak'), preselect='Ya', title='Apakah Anda mau mengupdate GLAD?', graphics=TRUE)
    
    if(download_test=="Ya"){
      glad_name_new<-paste("data/GLAD/SEA_day_", gsub("-", "", today()), ".tif", sep="")
      download.file(glad_link, glad_name_new)
      if(glad_exists)
        file.remove(glad_name)
    }
  }
  
  # There is glad data but still as a temporary file from the previous year:
  if(glad_exists & !year_test){
  # Make sure the name is just the year
    file_date<-unlist(regmatches(glad_name, gregexpr("[[:digit:]]+", glad_name)))
    if(length(file_date)>4){
      glad_name_new<-paste("data/GLAD/SEA_day_", year, ".tif", sep="")
      download.file(glad_link, glad_name_new)
      if(glad_exists)
        file.remove(glad_name)
    }
  }
  
  # There is no glad data just download:  
  if(!glad_exists){
    if(year_test)
      glad_name_new<-paste("data/GLAD/SEA_day_", gsub("-", "", Sys.Date()), ".tif", sep="")
    else
      glad_name_new<-paste("data/GLAD/SEA_day_", year, ".tif", sep="")
    download.file(glad_link, glad_name_new)
  }
})