###############################################################################
#													#
#	UK OVERSEAS TERRITORIES ISLAND PRIORITISATION ASSESSMENT			#
#													#
###############################################################################

### written by Steffen Oppel on 23 January 2013 (steffen.oppel@rspb.org.uk)
### last update: 12 March 2013
### included loop to run all 4 combinations of log and lin threat and irreplaceability 
### included simple cost estimate based on Island Conservation equation (supplied by Nick Holmes on 6 Feb 2013)
### simplified island cost to ONLY implementation cost (excluding the surcharges for isolation, human pop etc., because they are experimental, email by Nick Holmes 9 March 2013


# Load necessary library
library(RODBC)
require(maps)
require(mapdata)
require(maptools)
require(geosphere)
require(sp)
require(rgdal)
library(ggmap)
library(plyr)
library(ggplot2)
require(gpclib)
require(rgeos)
library(ggmap)
library(mapproj)
library(raster)

#####################################################################################################################################################
#
# LOAD DATA FROM DATABASE
# 
#####################################################################################################################################################

setwd("A:\\RSPB\\Island_eradication_unit")
setwd("C:\\STEFFEN\\RSPB\\Island_eradication_unit")

IR <- odbcConnectAccess2007('UKOT_island_database2.accdb')
islands <- sqlQuery(IR, "SELECT * FROM tbl_islands")
odbcClose(IR)

top<-read.table("UKOT_species_prioritisation_final_ranking.csv", header=T, sep=",")
sel<-top$Island_Code[1:25]



### REDUCE LIST FOR TERRITORIES
islands$OT<-ifelse(islands[,5]=="Saint Helena, Ascension and Tristan da Cunha",as.character(islands[,6]),as.character(islands[,5]))
OT<-aggregate(Longitude~islands$OT, data=islands, FUN=mean)
OT$Latitude<-aggregate(Latitude~islands$OT, data=islands, FUN=mean)[,2]
names(OT)[1]<-"UKOT"


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PLOT THE DATA
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
xlow<-(-180)
xup<-180
yup<-90
ylow<-(-90)

pdf("Figure1.pdf", width=9, height=8)
par (mar=c(0,0,0,0), oma=c(0,0,0,0))
plot(Latitude~Longitude, data=OT, col=1, pch=16,cex=2, xlim=c(xlow,xup), ylim=c(ylow,yup), main="", frame=F, axes=F, xlab="", ylab="", cex.main=2)
with(OT, text(Latitude~Longitude, labels = row.names(OT), pos = 4))
map("worldHires", add=T, fill=T, col="lightgray")
par(new=T)
plot(Latitude~Longitude, data=islands[islands$Island_Code %in% sel,], col='red', pch=16,cex=1, xlim=c(xlow,xup), ylim=c(ylow,yup), main="", frame=F, axes=F, xlab="", ylab="", cex.main=2)
#legend(-11.7, -8.2, lty=1, col=c('red', 'green'), legend=c("female","male"), cex=2, bty='n')
dev.off()



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PLOT A BETTER MAP
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

###################################################################################################
#WORLDMAP Approach
world <- map_data("world")
worldmap <- ggplot(world, aes(x=long, y=lat, group=group), bg='white') +
  	geom_path() + 
	theme(line = element_blank(),
        text = element_blank(),
        line = element_blank(),
        title = element_blank(),	
	panel.background = element_blank(),
   	panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())
names(OT)<-c("group","long","lat")
OT$hjust<-c(0.85,-0.65,-0.65,-0.65,-0.72,1.25,-0.65,-0.75,-0.65,-0.45,-0.45,-0.45,0.55)
OT$vjust<-c(-0.65,-0.65,-0.65,-0.65,-0.05,-0.65,-0.65,1.05,-0.65,-0.65,-0.65,-0.65,-0.55)
worldmap + geom_point (data = OT, col="red", size = 5) + geom_text (data=OT, label = row.names(OT), hjust=OT$hjust, vjust=OT$vjust)

ggsave("Fig1.eps", plot = worldmap + geom_point (data = OT, col="red", size = 5) + geom_text (data=OT, label = row.names(OT), hjust=OT$hjust, vjust=OT$vjust), width = 14, height = 8)
ggsave("Fig1.jpg", plot = worldmap + geom_point (data = OT, col="red", size = 5) + geom_text (data=OT, label = row.names(OT), hjust=OT$hjust, vjust=OT$vjust), width = 14, height = 8)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PLOT A GOOGLE EARTH MAP
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

OTsp <- SpatialPoints(data.frame(OT$Longitude, OT$Latitude), proj4string=CRS("+proj=longlat + datum=wgs84"))
OTsp <- SpatialPointsDataFrame(OTsp, data=OT)


jpeg(filename = "world_map.jpg", width = 9, height = 8, units = "in", pointsize = 12, res=600,quality = 100)
df("world_map.pdf", width=12, height=11)

m <- get_map(bbox(extent(OTsp)), source="google", zoom=2)
ggmap(m)+
geom_point(data=OT, aes(x=Longitude, y=Latitude),col=2, size=3)

dev.off()

