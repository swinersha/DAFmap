
# This is a script to calculate and produce a standardised set of forest threat
# Maps
#
# Tom Swinfield
# 17-03-08
#

# HEADER -----------------------------

rm(list=ls())

if (!require("pacman")) install.packages("pacman")
pacman::p_load(maptools, rgdal, raster, splancs, rgeos, lubridate, dplyr, rmarkdown, broom)

# These should be removed if pacman actually works.
library(maptools)
library(rgdal)
library(raster)
library(ggplot2)
library(splancs)
library(RColorBrewer)
library(rgeos)
library(lubridate)
library(dplyr)
library(rmarkdown)


source('R/area_lost.R')
source('R/change_prop_calc.R')
source('R/col_pal.R')
source('R/unique.col.R')
source('R/glad_by_date.R')
source('R/thrt_classify.R')

sectors<-readOGR("data/shapefiles/Sektor patroli.shp")
sectors$Area<-sapply(sectors@polygons, function(x) x@area/10000) # The sector Areas in hectares

smart_path<-dirname(file.choose()) #   'data/SMART/2016' # The user should point to the folder which contains the appropriate data.
#choose.dir() # Should work for windows  

# Set the analysis period - this must match that in the folder selected above:
analysis_period<-choose_dates()

# DEFORESTATION GLAD data -----------------------------

source('R/Download GLAD.R') # The first time this runs it will take a little time as new data are downloaded.

# Select the analysis period:
glad_period<-glad_by_date(analysis_period$from_date, analysis_period$to_date, sectors)

# Calculate deforestation within sectors:
sectors$df<-sapply(1:nrow(sectors), function(i) change_prop_calc(glad_period, sectors[i,]))
period_days <- analysis_period$to_date-analysis_period$from_date

# Convert zeros to NAs:
glad_period[glad_period==0]<-NA

# Convert current year to an annual % rate.
# Proportion of year:
year_prop<-1/(as.numeric(period_days)/365)
sectors$df<-sectors$df * year_prop

# Classifies the threats:
df_cut<-c(0,2,5,15,100)
sectors$df_thrt<-thrt_classify(sectors$df, df_cut) 

# SMART -----------------------------

# Read in the SMART effort data:
effort_header<-names(read.csv(file.path(smart_path, "Patrol_effort_by_sector_000054.csv")))
effort<-read.csv(paste(smart_path, "/Patrol_effort_by_sector_000054.csv", sep=''), skip=1, head=FALSE)
names(effort)<-effort_header 
# Calculate the effort:
# The column without the 1 is the total distance; 
# the column with the one is the total number of patrols
effort_hours<-effort$Number.of.Patrol.Hours[match(sectors$Sector,effort$X)]
effort_hours[is.na(effort_hours)]<-0
n_surveys<-effort$Number.of.Patrols[match(sectors$Sector,effort$X)]
n_surveys[is.na(n_surveys)]<-0

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

#abs_cut<-c(0,1,10,20,100)
#sectors$encr_abs_thrt<-thrt_classify(sectors$encr, abs_cut) 
#sectors$logg_abs_thrt<-thrt_classify(sectors$logg, abs_cut)
#sectors$hunt_abs_thrt<-thrt_classify(sectors$hunt, abs_cut) 


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


# Plotting threats ----

# Absolute threat detections
#par(mfrow=c(2,2), mar=(c(1,1,1,1)))
#plot(sectors, col=col_pal(sectors$encr, sectors$no_survey), main='Encroachment')
#plot(sectors, col=col_pal(sectors$logg, sectors$no_survey), main='Illegal logging')
#plot(sectors, col=col_pal(sectors$hunt, sectors$no_survey), main='Hunting')


# The overall threat level map ----

# Number of observations:
#tmp<-sectors@data[,c("encr_abs_thrt", "logg_abs_thrt", "hunt_abs_thrt", "df_thrt")]
#sectors$total_thrt_abs <- apply(tmp, 1, function(x) greatest_thrt(x, levels = threat_labels))

# Standardised Threat metric:
tmp<-sectors@data[,c("encr_thrt", "logg_thrt", "hunt_thrt", "df_thrt")]
sectors$total_thrt <- apply(tmp, 1, function(x) greatest_thrt(x, levels = threat_labels))

# Finally you need to make sure that you save back into the same folder.
# Ensure that data can not be overwritten.


# Save the threats per sector as a PNG:
png(filename = file.path(smart_path, "Tingkat ancaman per sektor.png"), 
    width = 400, height = 400, units = "mm", pointsize = 26,res=600)
par(mfrow=c(2,2), mar=c(1,1,1,1), oma=c(2,0,0,0))
plot(sectors,
     col=col_pal_thrt(sectors$df, breaks=c(0,2,5,15,100)),
     main='Pembukaan lahan')
plot(glad_period, col=rgb(1,0.3,0,0.8), add=TRUE, legend=FALSE)
plot(sectors,
     col=col_pal_thrt(sectors$encr_effort, breaks=c(0,1,10,20,100), no_survey = sectors$no_survey),
     main='Perambahan')
plot(sectors,
     col=col_pal_thrt(sectors$logg_effort, breaks=c(0,1,10,20,100), no_survey = sectors$no_survey),
     main='Pembalakan illegal')
plot(sectors,
     col=col_pal_thrt(sectors$hunt_effort, breaks=c(0,1,10,20,100), no_survey = sectors$no_survey),
     main='Perburuan')

plot_reset()
legend("bottom", 
       fill = c(brewer.pal(4, "Blues"), "grey"), 
       legend = c('Rendah', 'Sedang', 'Tinggi', 'Extrim', "Patroli tidak cukup"), 
       horiz=TRUE, text.width =  0.1, bty="n")
dev.off()

# Save the total threats per sector as a PNG:
png(filename = file.path(smart_path, "Tingkat ancaman total per sektor.png"), 
    width = 400, height = 400, units = "mm", pointsize = 26,res=600)
par(mfrow=c(1,1), mar=c(1,1,1,1), oma=c(2,0,0,0))
plot(sectors,
     col=col_pal_total_thrt(sectors$total_thrt),
     main='Total')
plot_reset()
legend("bottom", fill = brewer.pal(4, "Blues"), legend = c('Rendah', 'Sedang', 'Tinggi', 'Extrim'), horiz=TRUE, bty="n")
dev.off()

# Save the shapefile:
writeOGR(sectors, file.path(smart_path, "ancaman_per_sektor.shp"),
         layer="ancaman_per_sektor",
         driver="ESRI Shapefile")

# Save the shapefile:
write.csv(sectors@data, file.path(smart_path, "ancaman_per_sektor.csv"))

knitr::opts_chunk$set(echo = FALSE, comment = NA) # Suppress code print out.
rmarkdown::render("R/Report_gen.R", output_file = file.path(smart_path, "Laporan ancaman")) # Change this to be correct.
