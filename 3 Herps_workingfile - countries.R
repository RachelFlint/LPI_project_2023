
# 3 - LPD country representation

################################
#####Country level data######### - RUN FROM 'LOAD FINAL DATASETS'
################################
##################################
# load IUCN country datasets 
Amph_countries = read.csv('IUCN_amphibians/countries.csv')
Rept_countries = read.csv('IUCN_reptiles/countries.csv')

#filter to extant
Amph_extant = Amph_countries%>%
  filter(presence == 'Extant')
Rept_extant = Rept_countries%>%
  filter(presence == 'Extant')

#merge with IUCN main datasets
Amph_merged = merge(Amph_extant, IUCN_amph_assess, by.x = 'internalTaxonId', by.y = 'internalTaxonId', all.x = T, all.y = F)
Rept_merged = merge(Rept_extant, IUCN_rept_assess, by.x = 'internalTaxonId', by.y = 'internalTaxonId', all.x = T, all.y = F)

#filter to freshwater
Amph_merged_fw = Amph_merged%>%
  filter(systems %in% c("Freshwater (=Inland waters)",'Freshwater (=Inland waters)|Marine', 
                        "Terrestrial|Freshwater (=Inland waters)", "Terrestrial|Freshwater (=Inland waters)|Marine"))
length(unique(Amph_merged_fw$scientificName.x)) #4810 unique fw amph species

Rept_merged_fw = Rept_merged%>%
  filter(systems %in% c("Freshwater (=Inland waters)",'Freshwater (=Inland waters)|Marine', 
                        "Terrestrial|Freshwater (=Inland waters)", "Terrestrial|Freshwater (=Inland waters)|Marine"))
length(unique(Rept_merged_fw$scientificName.x)) #646 unique fw rept species

#remove excess columns
Amph_countries_edited_fw = subset(Amph_merged_fw, select = -c(10:31))#10074
Rept_countries_edited_fw = subset(Rept_merged_fw, select = -c(10:31))#2421

#add column of 1s to indicate presence
vec1 = rep(1, 10074)
vec2 = rep(1, 2421)
Amph_countries_edited_fw$Presence = vec1
Rept_countries_edited_fw$Presence = vec2

#FW AMPHIBIANS
#create a column per country
Amph_countries_fw = Amph_countries_edited_fw %>%
  pivot_wider(names_from= 'name', values_from = 'Presence')

#want to find richness in each country - create new dataframe
Amph_countries_fw_richness = data.frame('Countries' = colnames(subset(Amph_countries_fw, select = -c(1:8))),
                                        'Fw_amph_richness' = colSums(subset(Amph_countries_fw, select = -c(1:8)), na.rm = T)) #remove NA values, colsums sums each column

#FW REPTILES
#same again
Rept_countries_fw = Rept_countries_edited_fw %>%
  pivot_wider(names_from= 'name', values_from = 'Presence')
Rept_countries_fw_richness = data.frame('Countries' = colnames(subset(Rept_countries_fw, select = -c(1:8))),
                                        'Fw_rept_richness' = colSums(subset(Rept_countries_fw, select = -c(1:8)), na.rm = T)) #remove NA values
#note - could have just used group by and summarise...DO TO CHECK 

#combine
Fw_herp_richness = merge(Amph_countries_fw_richness, Rept_countries_fw_richness, 
                         by.x = 'Countries', by.y = 'Countries', all.x = T, all.y = T)

#Above gives IUCN richness data for each country for freshwater reptiles and amphibians
#need to compare these to LPI data, and plot on maps

#try plotting - edited from https://stackoverflow.com/questions/71858134/create-ggplot2-map-in-r-using-count-by-country
#library(maps)
#library(ggplot2)
#world_map <- map_data(map = "world")
#ggplot() +
#  geom_map(data = Amph_countries_fw_richness, aes(map_id = Countries, fill = Richness), map = world_map) +
#  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), colour = 'black', fill = NA) +
#  expand_limits(x = world_map$long, y = world_map$lat) +
#  scale_fill_gradientn(colours=c("darkslategrey","darkslategray1", "orange", "orangered", "orangered4"))+
#  theme_bw() +
#  coord_fixed()
#misses some (e.g. US) due to different naming - change names

###Second attempt 
Countries_list = levels(as.factor(world_map$region))
#change relevant countries in IUCN data - 
#note: british overseas territory,gibraltar, hong kong, macao, Tokelau not included in world_map regions - need to remove to avoid double counting
Fw_herp_richness[8,1] = 'Antigua'
Fw_herp_richness[25,1] = 'Bolivia'
Fw_herp_richness[26,1] = 'Bonaire'
Fw_herp_richness[31,1] = 'Brunei'
Fw_herp_richness[35,1] = 'Cape Verde'
Fw_herp_richness[45,1] = 'Cocos Islands'
Fw_herp_richness[47,1] = 'Republic of Congo'
Fw_herp_richness[48,1] = 'Democratic Republic of the Congo'
Fw_herp_richness[51,1] = 'Ivory Coast'
Fw_herp_richness[54,1] = 'Curacao'
Fw_herp_richness[56,1] = 'Czech Republic'
Fw_herp_richness[67,1] = 'Swaziland'
Fw_herp_richness[93,1] = 'Iran'
Fw_herp_richness[104,1] = 'North Korea'
Fw_herp_richness[105,1] = 'South Korea'
Fw_herp_richness[108,1] = 'Laos'
Fw_herp_richness[130,1] = 'Micronesia'
Fw_herp_richness[153,1] = 'Palestine'
Fw_herp_richness[163,1] = 'Reunion'
Fw_herp_richness[165,1] = 'Russia'
Fw_herp_richness[167,1] = 'Saint Barthelemy'
Fw_herp_richness[168,1] = 'Saint Helena'
Fw_herp_richness[169,1] = 'Saint Kitts'
Fw_herp_richness[171,1] = 'Saint Martin'
Fw_herp_richness[182,1] = 'Saint Martin' #remove?
Fw_herp_richness[172,1] = 'Saint Vincent'
Fw_herp_richness[195,1] = 'Syria'
Fw_herp_richness[196,1] = 'Taiwan'
Fw_herp_richness[198,1] = 'Tanzania'
Fw_herp_richness[204,1] = 'Trinidad'
Fw_herp_richness[212,1] = 'UK'
Fw_herp_richness[213,1] = 'USA'
Fw_herp_richness[217,1] = 'Venezuela'
Fw_herp_richness[218,1] = 'Vietnam'
Fw_herp_richness[219,1] = 'Virgin Islands'
Fw_herp_richness[220,1] = 'Virgin Islands' #remove?

write.csv(Fw_herp_richness, 'R_scripts+outputs/Fw_herp_richness_by_country.csv')

#do final edits in excel (removed extra rows/countries)

#FINAL DATASET########################################################################
Fw_herp_richness_final = read.csv('R_scripts+outputs/Fw_herp_richness_by_country_EDITED.csv')

#change NAs to 0s
Fw_herp_richness_final_edited = Fw_herp_richness_final %>%
  mutate(Fw_amph_richness = ifelse(is.na(Fw_amph_richness), 0, Fw_amph_richness), 
         Fw_rept_richness = ifelse(is.na(Fw_rept_richness), 0, Fw_rept_richness))
sum(Fw_herp_richness_final_edited$Fw_amph_richness) #10036 unique occurences of amphs
sum(Fw_herp_richness_final_edited$Fw_rept_richness) #2405 unique occurences of repts

view(Fw_herp_richness_final_edited)

#PLOT
#AMPHIBIANS
library(maps)
library(ggplot2)
world_map <- map_data(map = "world")
ggplot() +
  geom_map(data = Fw_herp_richness_final_edited, aes(map_id = Countries, fill = Fw_amph_richness), map = world_map) +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), colour = 'darkgrey', fill = NA, linewidth = 0.05) +
  expand_limits(x = world_map$long, y = world_map$lat) +
  scale_fill_gradientn(colours=c('white',  'darkturquoise', "darkslategrey", 'darkslateblue', 'darkorchid4'))+
  theme_classic() +
  ylim(-63,90)+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8)) +
  labs(fill = "Fw amphibian richness")+ 
  coord_fixed()
ggsave('R_scripts+outputs/Fw_amphibian_richness_bycountry.svg')

#REPTILES
library(maps)
library(ggplot2)
world_map <- map_data(map = "world")
ggplot() +
  geom_map(data = Fw_herp_richness_final_edited, aes(map_id = Countries, fill = Fw_rept_richness), map = world_map) +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), colour = 'darkgrey', fill = NA, linewidth = 0.05) +
  expand_limits(x = world_map$long, y = world_map$lat) +
  scale_fill_gradientn(colours=c('white', 'darkturquoise', "darkslategrey"))+
  theme_classic() +
  ylim(-63,90)+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8)) +
  labs(fill = "Fw reptile richness")+ #change legend title
  coord_fixed()
ggsave('R_scripts+outputs/Fw_reptile_richness_bycountry.svg')

#refine - sort legend etc in inkscape

#LPI countries############################################## - run from 'load final dataset'
LPI_countries = levels(as.factor(IUCN_LPI_fw$Country)) #58 countries (but names inconsistent)

#need to sum unique species in each country
#AMPHIBIANS
LPI_richness_amph = LPI_fw_amph %>%
  select(c(scientificName, Country))%>% #select necessary columns
  group_by(Country)%>%
  summarise(Country_richness_amph = length(unique(scientificName)))
#REPTILES
LPI_richness_rept = LPI_fw_rept %>%
  select(c(scientificName, Country))%>% #select necessary columns
  group_by(Country)%>%
  summarise(Country_richness_rept = length(unique(scientificName)))
#combine
LPI_richness = merge(LPI_richness_amph, LPI_richness_rept, by.x = 'Country', by.y = 'Country', all.x = T, all.y = T)
write.csv(LPI_richness, 'R_scripts+outputs/LPI_richness.csv')
#edit in excel and load dataset

#LOAD FINAL LPI DATASET##########################################
LPI_richness_final = read.csv('R_scripts+outputs/LPI_richness_edited.csv') #56 countries total
#EDITS:
#changed sudan to south sudan - wrongly inputted (seen when plotting)

#merge LPI and IUCN richness data
LPI_IUCN_richness = merge(Fw_herp_richness_final_edited, LPI_richness_final, by.x = 'Countries', by.y = 'Country', all.x = T, all.y = T)
#remame LPI columns
colnames(LPI_IUCN_richness)[4] = 'LPI_amph'
colnames(LPI_IUCN_richness)[5] = 'LPI_rept'
#change NAs to 0 in LPI columns
LPI_IUCN_richness = LPI_IUCN_richness %>%
  mutate(LPI_amph = ifelse(is.na(LPI_amph), 0, LPI_amph), #change NAs to 0s
         LPI_rept = ifelse(is.na(LPI_rept), 0, LPI_rept))
#add columns with percentage representation
#amphs
for (i in 1:218){
   LPI_IUCN_richness$Amph_percentage[i] = (LPI_IUCN_richness$LPI_amph[i]/LPI_IUCN_richness$Fw_amph_richness[i])*100 
}
#repts
for (i in 1:218){
    LPI_IUCN_richness$Rept_percentage[i] = (LPI_IUCN_richness$LPI_rept[i]/LPI_IUCN_richness$Fw_rept_richness[i])*100
}

#change NAs in percentage columns to 0 representation
LPI_IUCN_richness = LPI_IUCN_richness %>%
  mutate(Amph_percentage = ifelse(Amph_percentage == 'NaN', NA, Amph_percentage), #change Nans and infs to NAs
         Amph_percentage = ifelse(Amph_percentage == 'Inf', NA, Amph_percentage),
         Rept_percentage = ifelse(Rept_percentage == 'NaN', NA, Rept_percentage),
         Rept_percentage = ifelse(Rept_percentage == 'Inf', NA, Rept_percentage)) %>%
  mutate_at(c(2:7), as.numeric) #make all numbers in table numeric (from characters)

#save final dataset
write.csv(LPI_IUCN_richness, 'R_scripts+outputs/LPI_IUCN_richness_final.csv')

#PLOT MAPS WITH % represented
library(maps)
library(ggplot2)
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library('raster')
world_map <- map_data(map = "world")

#Amphibians
ggplot() +
  geom_map(data = LPI_IUCN_richness, aes(map_id = Countries, fill = Amph_percentage), map = world_map) +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), colour = 'darkgrey', fill = NA, linewidth = 0.05) +
  expand_limits(x = world_map$long, y = world_map$lat) +
  scale_fill_gradientn(colours = c('white', 'darkseagreen1', 'darkseagreen3', "darkseagreen4"))+
  theme_classic() +
  ylim(-63,90)+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8)) +
  labs(fill = "Fw amphibians represented (%)")+ #change legend title
  coord_fixed()+
  geom_point(data = LPI_fw_amph_norep, aes(x = Longitude, y = Latitude), size = 0.05, colour = 'red2',
             shape = 5) #refine, sort appearance
ggsave('R_scripts+outputs/Amphibian_LPI_representation.svg')

#Reptiles
ggplot() +
  geom_map(data = LPI_IUCN_richness, aes(map_id = Countries, fill = Rept_percentage), map = world_map) +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), colour = 'darkgrey', fill = NA, linewidth = 0.05) +
  #expand_limits(x = world_map$long, y = world_map$lat) +
  scale_fill_gradientn(colours = c('white', 'darkseagreen1', 'darkseagreen3', "darkseagreen4", 'darkslategray4'))+
  theme_classic() +
  ylim(-63,90)+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8)) +
  labs(fill = "Fw reptiles represented (%)")+ #change legend title
  coord_fixed()+
  geom_point(data = LPI_fw_rept_norep, aes(x = Longitude, y = Latitude), size = 0.05, colour = 'red2',
             shape = 5) #refine, sort appearance
ggsave('R_scripts+outputs/Reptile_LPI_representation.svg')
ggsave('R_scripts+outputs/Reptile_LPI_representation.svg')