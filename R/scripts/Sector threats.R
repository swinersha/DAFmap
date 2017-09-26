
# This is a script to calculate and produce a standardised set of forest threat
# Maps
#
# Tom Swinfield
# 17-03-08
#

# HEADER -----------------------------

rm(list=ls())

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
library(DAFmap)

# GLOBAL VARIABLES: ----

# !!!! If you update the sectors in any way...
# You must set sector_update to TRUE the next time the script is run !!!:
SECTOR_UPDATE<-FALSE

PATROL_DAYS_PER_YEAR<-250 # The total number of patrol days at each post.
HOURS_PER_PATROL<-4
PATROL_WEIGHT <- c(rendah=1, sedang=3, tinggi=6, extrim=12)
SECTOR_SHP<-"data/shapefiles/Sektor patroli.shp"
THREAT_LABELS<-c('rendah', 'sedang', 'tinggi', 'extrim')

# The weighting given to each threat level for calculating the patrol 
# effort necessary.

smart_path<-dirname(file.choose()) #   'data/SMART/2016' 
# The user should point to the folder which contains the appropriate data.
#choose.dir() # Should work for windows  

# Set the analysis period - this must match that in the folder selected above:
analysis_period<-choose_dates()
period_days <- as.numeric(analysis_period$to_date-analysis_period$from_date)
year_prop<-1/(period_days/365)

# Read in the patrol sector shapefile:
sectors<-readOGR(SECTOR_SHP)
sectors$Area<-sapply(sectors@polygons, function(x) x@area/10000) # The sector Areas in hectares
# Ensure that all the sectors have an office:
sectors@data$Kantor<-as.character(sectors@data$Kantor)
sectors@data$Kantor[is.na(sectors@data$Kantor)]<-"tanpa_kantor"
sectors@data$Kantor<-as.factor(sectors@data$Kantor)
# This wouldn't be necessary if the shapefile was made properly.



# DEFORESTATION GLAD data -----------------------------

download_GLAD() # The first time this runs it will take a little time as new data are downloaded.

# Select the analysis period:
glad_period<-glad_by_date(analysis_period$from_date, analysis_period$to_date, sectors)

# Calculate deforestation within sectors:
#sectors$df<-sapply(1:nrow(sectors), function(i) change_prop_calc(glad_period, sectors[i,]))
sectors$df<-sector_df_assess(sectors, glad_period, sector_update = SECTOR_UPDATE) 

# Convert zeros to NAs:
glad_period[glad_period==0]<-NA

# Convert deforestation in the period to an annual % rate.
sectors$df<-sectors$df * year_prop

# Classifies the threats:
df_cut<-c(0,2,5,15,100)
sectors$df_thrt<-thrt_classify(sectors$df, df_cut, THREAT_LABELS) 



# SMART -----------------------------

# Read in the SMART effort data:
effort_header<-names(read.csv(file.path(smart_path, "Usaha_patroli_per_sektor_000051.csv")))
effort<-read.csv(paste(smart_path, "/Usaha_patroli_per_sektor_000051.csv", sep=''), skip=1, head=FALSE)
names(effort)<-effort_header 
# Calculate the effort:
effort_hours<-effort$Number.of.Patrol.Hours[match(sectors$Sector,effort$X)]
effort_hours[is.na(effort_hours)]<-0
n_surveys<-effort$Number.of.Patrols[match(sectors$Sector,effort$X)]
n_surveys[is.na(n_surveys)]<-0

# Adds the patrol effort to the sectors data:
sectors$effort_hours<-effort_hours
sectors$n_surveys<-n_surveys

# Read in the SMART threat data
encr<-read.csv(file.path(smart_path, "Observasi_perambahan_baru_per_sektor_000058.csv"))
logg<-read.csv(file.path(smart_path, "Observasi_pembalakan_liar_per_sektor_000057.csv"))
hunt<-read.csv(file.path(smart_path, "Observasi_perburuan_per_sektor_000056.csv"))

# Add the threat data to the sectors:
# Threat observations:
sectors$encr<-encr$Count.Observations.Ancaman[match(sectors$Sector,encr$X)]
sectors$logg<-logg$Count.Observations.Ancaman[match(sectors$Sector,logg$X)]
sectors$hunt<-hunt$Count.Observations.Ancaman[match(sectors$Sector,hunt$X)]

# Convert NAs to 0s
sectors$encr[is.na(sectors$encr)]<-0
sectors$logg[is.na(sectors$logg)]<-0
sectors$hunt[is.na(sectors$hunt)]<-0

# Rates of threat detection:
sectors$encr_effort<-sectors$encr/(effort_hours/100)
sectors$logg_effort<-sectors$logg/(effort_hours/100)
sectors$hunt_effort<-sectors$hunt/(effort_hours/100)

# An insufficient patrol effort for assessment:
sectors$no_survey<-effort_hours<4

# Classifies the threats:
obs_cut<-c(0,1,10,20,100)
sectors$encr_thrt<-thrt_classify(sectors$encr_effort, obs_cut, THREAT_LABELS) 
sectors$logg_thrt<-thrt_classify(sectors$logg_effort, obs_cut, THREAT_LABELS)
sectors$hunt_thrt<-thrt_classify(sectors$hunt_effort, obs_cut, THREAT_LABELS) 


# Make a spaial object for plotting sector labels: ----
sectors_centroid<-gCentroid(sectors, byid = TRUE)


# Plotting threats ----

# The overall threat level map ----

# Standardised Threat metric:
tmp<-sectors@data[,c("encr_thrt", "logg_thrt", "hunt_thrt", "df_thrt")]
sectors$total_thrt <- apply(tmp, 1, function(x) greatest_thrt(x, levels = THREAT_LABELS))

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
png(
  filename = file.path(smart_path, "Tingkat ancaman total per sektor.png"),
  width = 400,
  height = 400,
  units = "mm",
  pointsize = 26,
  res = 600
)
par(
  mfrow = c(1, 1),
  mar = c(1, 1, 1, 1),
  oma = c(2, 0, 0, 0)
)
plot(sectors,
     col = col_pal_total_thrt(sectors$total_thrt),
     main = 'Total')
text(sectors_centroid, labels = sectors$Code, cex = 0.5)
plot_reset()
legend(
  "bottom",
  fill = brewer.pal(4, "Blues"),
  legend = c('Rendah', 'Sedang', 'Tinggi', 'Extrim'),
  horiz = TRUE,
  bty = "n"
)
dev.off()

# Assessing patrol effor relative to threats ----

# Calculate the patrol days needed for each sector by setting the threat level weighting: ----  

sectors$patrol_weight<-PATROL_WEIGHT[sectors$total_thrt]
sectors$patrol_weight[sectors$Prioritas=="Tidak prioritas"]<-NA # Excludes non priority sectors
sectors$patrol_weight[sectors$Type=="Pos"]<-NA # Excludes the posts

# Calculate the number of sectors at each threat level for each field office:
sector_threat<-sectors@data %>% 
  filter(!is.na(patrol_weight)) %>%
  group_by(Kantor) %>%
  summarise(n_sektor = length(total_thrt),
            n_rendah = sum(total_thrt=="rendah"),
            n_sedang = sum(total_thrt=="sedang"),
            n_tinggi = sum(total_thrt=="tinggi"),
            n_extrim = sum(total_thrt=="extrim")
  )

# Calculate the patrol effort required:
sector_effort<-sector_threat %>%
  mutate(rendah_w = n_rendah*PATROL_WEIGHT[1],
         sedang_w = n_sedang*PATROL_WEIGHT[2],
         tinggi_w = n_tinggi*PATROL_WEIGHT[3],
         extrim_w = n_extrim*PATROL_WEIGHT[4],
         total_w = rendah_w+sedang_w+tinggi_w+extrim_w,
         unit_w = PATROL_DAYS_PER_YEAR / total_w,
         hari_rendah = round(unit_w*PATROL_WEIGHT[1],1),
         hari_sedang = round(unit_w*PATROL_WEIGHT[2],1),
         hari_tinggi = round(unit_w*PATROL_WEIGHT[3],1),
         hari_extrim = round(unit_w*PATROL_WEIGHT[4],1),
         periode_rendah = round(period_days/hari_rendah,1),
         periode_sedang = round(period_days/hari_sedang,1),
         periode_tinggi = round(period_days/hari_tinggi,1),
         periode_extrim = round(period_days/hari_extrim,1)) %>%
  select(Kantor, hari_rendah:periode_extrim)

# Convert wide format to tidy format:
sector_effort_tidy<-sector_effort %>% 
  tidyr::gather(thrt, score, hari_rendah:periode_extrim) %>%
  tidyr::separate(thrt, into=c("type", "total_thrt"), sep="_") %>%
  tidyr::spread(type,score)

# !!! YOU NEED TO DO SOMETHING ABOUT TANPA KANTOR BECAUSE
# AT THE MOMENT IT IS GIVEN KPIs BUT IN REALITY NO TEAM 
# IS ASSIGNED TO DO THE WORK.




# Merge back into sectors ----

sectors@data<-sector_assign_kpi(sectors@data, sector_effort_tidy)
sectors$jam<-sectors$hari*HOURS_PER_PATROL # calculate the number of hours rquired.  



# Create outputs ----

# Save the shapefile:
writeOGR(sectors, file.path(smart_path, "ancaman_per_sektor.shp"),
         layer="ancaman_per_sektor",
         driver="ESRI Shapefile", 
         overwrite_layer = TRUE)

# Save the shapefile:
write.csv(sectors@data, file.path(smart_path, "ancaman_per_sektor.csv"))

knitr::opts_chunk$set(echo = FALSE, comment = NA) # Suppress code print out.
rmarkdown::render("R/scripts/Report_gen.R", output_file = file.path(smart_path, "Laporan ancaman.html")) # Change this to be correct.
