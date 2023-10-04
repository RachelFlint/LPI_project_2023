####################
###LPI fw herps#####
####################
library("ggplot2")
theme_set(theme_bw())
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library('raster')

#load world dataset
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

IUCN_LPI_fw = read.csv('R_scripts+outputs/LPI_freshwater(IUCN).csv')
#plot map with locations of freshwater herptile populations (separated by class)
ggplot(data = world) +
  geom_sf() +
  geom_point(data = IUCN_LPI_fw, aes(x = Longitude, y = Latitude, fill = Class), size = 0.8, 
             shape = 23) 
ggsave("FW_herps_map_v2.pdf") #v2 is with added data to LPI 19/06/23
#if wanted, could highlight on map where new datapoints are (would mean setting up a new categroy (e.g. added) in dataset)

####################
####IUCN herps######
####################

#import polygon data from IUCN using read_sf - https://r-spatial.org/r/2018/10/25/ggplot2-sf.html
Amph_polygons = read_sf(dsn = "IUCN_amphibians/Amph_polygons", layer = "AMPHIBIANS") #got 9330 entries - a lot more than taxonomic data? multiple locations?
Rept_polygons1 = read_sf(dsn = "IUCN_reptiles/Rept_polygons", layer = "REPTILES_PART1") #6849 entries
Rept_polygons2 = read_sf(dsn = "IUCN_reptiles/Rept_polygons", layer = "REPTILES_PART2") #6848 entries

#filter data to freshwater = true
Amph_polygons_fw = subset(Amph_polygons, freshwater == 'true') #get 6299 entries 
Rept_polygons_fw1 = subset(Rept_polygons1, freshwater == 'true') #327 entries
Rept_polygons_fw2 = subset(Rept_polygons2, freshwater == 'true') #379 entries 

#filter to only extant populations? presence == 1
Amph_polygons_fw_extant = subset(Amph_polygons_fw, presence == 1) #5622 entries
Rept_polygons_fw1_extant = subset(Rept_polygons_fw1, presence == 1) #314 entries
Rept_polygons_fw2_extant = subset(Rept_polygons_fw2, presence == 1) #368 entries 

#remove extra column
Rept_polygons_fw1_extant = subset(Rept_polygons_fw1_extant, select = -OBJECTID)
#bind reptile data
Rept_polygons_fw_extant = rbind(Rept_polygons_fw1_extant, Rept_polygons_fw2_extant) #682 total 

#understand polygon data
class(Amph_polygons_fw) #four different classes of information

#save as polygons
st_write(Rept_polygons_fw_extant, 'IUCN_reptiles/Rept_polygons_fw_extant.shp')
st_write(Amph_polygons_fw_extant, 'IUCN_amphibians/Amph_polygons_fw_extant.shp')
#producing lots of warning messages e.g.In CPL_write_ogr(obj, dsn, layer, driver, as.character(dataset_options),  ... :#GDAL Message 1: Value 152340346 of field id_no of feature 696 not successfully written. Possibly due to too larger number with respect to field width
#try saving as geopackage - https://stackoverflow.com/questions/73112939/r-sf-st-write-error-feature-not-successfully-written

#st_write(Rept_polygons_fw_extant, 'IUCN_reptiles/Rept_polygons_fw_extant_gpkg.gpkg')
#st_write(Amph_polygons_fw_extant, 'IUCN_amphibians/Amph_polygons_fw_extant_gpkg.gpkg')
#no warnings this time, but can't transfer to desktop UCL easily or open  in arcgis

#try  redoing differently - same problem occurs. ASK MONNI
#write_sf(Rept_polygons_fw_extant, 'IUCN_reptiles/Rept_polygons_fw_extant.shp', driver = 'ESRI Shapefile')
#write_sf(Amph_polygons_fw_extant, 'IUCN_amphibians/Amph_polygons_fw_extant.shp', driver = 'ESRI Shapefile')

#Warnings just due to id_no field - doesn't matter? Try reading in shp files to check (deleted amph ones - use reptiles)
test = read_sf(dsn = 'IUCN_reptiles', layer = 'Rept_polygons_fw_extant') 
#gives 682 rows as expected, all id numbers (no NA values in id_no columns)


############################
###ALL DATA#########
########################

#loaded above files into ArcMap, used 'dissolve' and opted for multipolygon datasets

#load dissolved datasets:
Amph_polygons_final = read_sf(dsn = "IUCN_amphibians/Amphibians_fw_dissolved", layer = "Amphp_fw_extant_dissolved2") #4766 species
Rept_polygons_final = read_sf(dsn = "IUCN_reptiles/Reptiles_fw_dissolved", layer = 'Rept_fw_extant_dissolved2') #529 species
#note - '2' denotes multipolygon datasets (not separate polygons)

#FW AMPHIBIANS
Amph_polygons_final$count<-rep(1,dim(Amph_polygons_final)[1]) 
##creates a simple count = 1 field in the table of polygons, 
#this is used to sum up number of polygons overlapping a specific grid cell in the end

# check what the projection is 
st_crs(Amph_polygons_final) #WGS 84

library(raster)
wgsProj <- CRS("+init=epsg:4326")
#cellSize <- 96.486268/2 #check how to make this smaller
wgsExtent <- extent(-180, 180, -56, 84)
wgs <- raster(nrows=140*2, ncols=360*2, crs=wgsProj, ext=wgsExtent)#this creates an empty raster
##now we’re looping through all species and add to the empty raster
for (i in 1:nrow(Amph_polygons_final)){
  richnessraster<-rasterize(Amph_polygons_final[i,],wgs, getCover=T) ##creates a raster layer for the current polygon
  #have set this to getCover=T - this is more accurate than another method - gives a percentage of cover per cell
  richnessraster[values(richnessraster)>0]<-1 ## This is saying any cell coverage over 0 record as present (1).
  if(i==1) {rr<-richnessraster} else {rr<-rr+richnessraster}
  print(i)} ##this is just a counter to show us where we are in the operation

##then write raster and save it for future use
writeRaster(rr, 'Amphs_fw_extant', format="GTiff")
plot(rr)  ##to have a quick look at it.
#SAVED in project file 
###############################################################
############### RUN FROM HERE####################################

library("ggplot2")
theme_set(theme_bw())
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library('raster')


#ggplot (Monni's code) -  Need to add 'world' 
library(ggplot2)
library(rnaturalearth)
#Mapping for coastlines
coast <- ne_coastline(scale = "small", returnclass = "sf")
class(coast)
st_crs(coast)#WGS 1984

##set up Theme
My_Theme = theme(
  panel.background = element_rect(fill = "white",
                                  size = 0.5, linetype = "solid"))
##read in raster
RRamph<-raster("Amphs_fw_extant.tif")
##reclassify so anything with no data or 0 is same value
RRamph1 <- reclassify(RRamph, cbind(-Inf, 0, NA), right=TRUE)
##for plotting in ggplot, I had to convert to Spatial data frame first:
spdf <- as(RRamph1, "SpatialPixelsDataFrame")
spdfFINALamph <- as.data.frame(spdf)
colnames(spdfFINALamph) <- c("value", "x", "y")
#cropped it to my world layer – I think because I cut out Antarctica from the map
#world_crop <- crop(x = world, y = RR)

#plot overall species richness in sample - finalise this
ggplot()+
  My_Theme +
  geom_tile(data = spdfFINALamph, aes(x=x, y=y, fill=value), colour = NA) +
  scale_fill_gradientn(colours=c("darkslategrey","darkslategray1", "orange", "orangered", "orangered4")) +
  geom_sf(data=coast, linewidth = 0.2) + #changed this - need to refine
  xlab("Longitude") +
  ylab("Latitude") +
  labs(fill = "No. of species")
ggsave('Amphibian_fw_richness.png')
##########################################################

##calculate normalised species richness 
normSRamph<-RRamph1/maxValue(RRamph1)
#convert to Spatial dataframe again
normdf <- as(normSRamph, "SpatialPixelsDataFrame")
normdfFINALamph <- as.data.frame(normdf)
colnames(normdfFINALamph) <- c("value", "x", "y")
##plotting
ggplot()+
  My_Theme +
  geom_tile(data = normdfFINALamph, aes(x=x, y=y, fill=value)) +
  scale_fill_gradientn(colours=c("darkslategrey","darkslategray1", "orange", "orangered", "orangered4"), breaks = seq(0.2, 1.0, by=0.2)) +
  geom_sf(data=coast) +
  #coord_fixed() + #didn't work with this row
  xlab("Longitude") +
  ylab("Latitude") +
  labs(fill = "richness")
ggsave('Amphibian_fw_normalised.png')

######################################################

##FW REPTILES 
Rept_polygons_final$count<-rep(1,dim(Rept_polygons_final)[1]) ##creates a simple count = 1 field in the table of polygons, 
#this is used to sum up number of polygons overlapping a specific grid cell in the end

# check what the projection is 
st_crs(Rept_polygons_final) #WGS 84

library(raster)
wgsProj <- CRS("+init=epsg:4326")
#cellSize <- 96.486268/2
wgsExtent <- extent(-180, 180, -56, 84)
wgs <- raster(nrows=140*2, ncols=360*2, crs=wgsProj, ext=wgsExtent)#this creates an empty raster
##now we’re looping through all species and add to the empty raster
for (i in 1:nrow(Rept_polygons_final)){
  richnessraster<-rasterize(Rept_polygons_final[i,],wgs, getCover=T) ##creates a raster layer for the current polygon
  #have set this to getCover=T - this is more accurate than another method - gives a percentage of cover per cell
  richnessraster[values(richnessraster)>0]<-1 ## This is saying any cell coverage over 0 record as present (1).
  if(i==1) {rr<-richnessraster} else {rr<-rr+richnessraster}
  print(i)} ##this is just a counter to show us where we are in the operation

##then write raster and save it for future use
writeRaster(rr, 'Rept_fw_extant', format="GTiff")

plot(rr)

##to have a look at it. 

###RUN FROM HERE#############################
############################################

##plot species richness map in ggplot
##set up Theme
My_Theme = theme(
  panel.background = element_rect(fill = 'white',
                                  size = 0.5, linetype = "solid"))
##read in raster
RRrept<-raster('Rept_fw_extant.tif')
##reclassify so anything with no data or 0 is same value
RRrept1 <- reclassify(RRrept, cbind(-Inf, 0, NA), right=TRUE)
##for plotting in ggplot, I had to convert to Spatial data frame first:
spdf <- as(RRrept1, "SpatialPixelsDataFrame")
spdfFINALrept <- as.data.frame(spdf)
colnames(spdfFINALrept) <- c("value", "x", "y")
#cropped it to my world layer – I think because I cut out Antarctica from the map
#world_crop <- crop(x = world, y = RR)

#plot overall species richness in sample
ggplot()+
  My_Theme +
  geom_tile(data = spdfFINALrept, aes(x=x, y=y, fill=value)) +
  scale_fill_gradientn(colours=c("darkslategrey","darkslategray1", 'orange', "orangered")) +
  geom_sf(data=coast, linewidth = 0.2) + #changed this - need to refine
  xlab("Longitude") +
  ylab("Latitude") +
  labs(fill = "No. of species")
ggsave('Reptile_fw_richness.png')


#####################################
##calculate normalised species richness 
normSRrept<-RRrept1/maxValue(RRrept1)

#convert to Spatial dataframe again
normdf <- as(normSRrept, "SpatialPixelsDataFrame")
normdfFINALrept <- as.data.frame(normdf)
colnames(normdfFINALrept) <- c("value", "x", "y")

##plotting - refine: very griddy
ggplot()+
  My_Theme +
  geom_tile(data = normdfFINALrept, aes(x=x, y=y, fill=value)) +
  scale_fill_gradientn(colours=c("darkslategrey","darkslategray1", "orange", "orangered", "orangered4"), breaks = seq(0.2, 1.0, by=0.2)) +
  geom_sf(data=coast) +
  #coord_fixed() +
  xlab("Longitude") +
  ylab("Latitude") +
  labs(fill = "richness")
ggsave('Reptile_fw_normalised.png')







