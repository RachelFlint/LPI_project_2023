# Final plots code

install.packages('ggplot2')
library('ggplot2')

#ORDER REPRESENTATION - Fig1
Amph_orders_edited$Order = factor(Amph_orders_edited$Order, levels = c("Anura","Caudata","Gymnophiona" ))
ggplot(Amph_orders_edited, aes(x = Group, y = Percentage, fill = Order))+
  geom_bar(stat="identity", colour = 'black')+
  scale_fill_brewer(palette = "Pastel2")+
  ylab('Percentage (%)')+
  theme_classic()+
  scale_x_discrete(labels=c('IUCN_Percent' = 'IUCN', 'LPI_Percent' = 'LPD'))+
  theme(text = element_text(size = 20)) 
ggsave('R_scripts+outputs/Amph_orders.svg')

ggplot(Rept_orders_edited, aes(x = Group, y = Percentage, fill = Order))+
  geom_bar(stat="identity", colour = 'black')+
  scale_fill_brewer(palette = "Pastel2")+
  ylab('Percentage (%)')+
  theme_classic()+
  scale_x_discrete(labels=c('IUCN_Percent' = 'IUCN', 'LPI_Percent' = 'LPD'))+
  theme(text = element_text(size = 20)) 
ggsave('R_scripts+outputs/Rept_orders.svg')

#SPATIAL REPRESENTATION - Fig 2
#A+B - Amphibians
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


#C+D - REPTILES
library(maps)
library(ggplot2)
world_map <- map_data(map = "world")

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
ggsave('R_scripts+outputs/Reptile_LPI_representation.png')

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

#REALM REPRESENTATION - Fig 3

colours = c(IUCN = 'red4', LPI = 'lightblue')
#plot bar chart - reptiles
ggplot(Fw_rept_realms, aes(fill=Group, y = realm, x = Percent))+
  geom_bar(position="dodge", stat="identity", width = 0.8)+
  theme_classic()+
  ggtitle('Realm representation - fw reptiles')+
  scale_fill_manual(values = colours, labels = c('IUCN', 'LPI' = "LPD"))+
  ylab('Biogeographical realm')+
  xlab('Percentage (%)')+
  theme(text = element_text(size = 15))
ggsave('R_scripts+outputs/Fw_rept_realms.svg')

#plot bar chart - amphibians
ggplot(Fw_amph_realms, aes(fill=Group, y = realm, x = Percent))+
  geom_bar(position="dodge", stat="identity", width = 0.8)+
  theme_classic()+
  ggtitle('Realm representation - fw amphibians')+
  scale_fill_manual(values = colours, labels = c('IUCN', 'LPI' = "LPD"))+
  ylab('Biogeographical realm')+
  xlab('Percentage (%)')+
  theme(text = element_text(size = 15))
ggsave('R_scripts+outputs/Fw_amph_realms.svg')

#THREAT REPRESENTATION - fig 4

#plot bar chart - amphibians
ggplot(Fw_amph_threats_percent2, aes(fill=Threatened,  x = Group, y = Percent))+
  geom_bar( stat="identity")+
  theme_classic()+
  scale_fill_brewer(palette = 'Oranges')+
  scale_x_discrete(labels = c('IUCN', 'LPI' = 'LPD'))+
  ylab('Percentage (%)')+
  labs(fill = 'Threat Category')+
  theme(text = element_text(size = 15))+
  ggtitle('Threat representation - fw amphibians')
ggsave('R_scripts+outputs/Fw_amph_threats2.svg')

#plot bar chart - reptiles
ggplot(Fw_rept_threats_percent2, aes(fill=Threatened,  x = Group, y = Percent))+
  geom_bar( stat="identity")+
  theme_classic()+
  scale_fill_brewer(palette = 'Oranges')+
  scale_x_discrete(labels = c('IUCN', 'LPI' = 'LPD'))+
  ylab('Percentage (%)')+
  labs(fill = 'Threat Category')+
  theme(text = element_text(size = 15))+
  ggtitle('Threat representation - fw reptiles')
ggsave('R_scripts+outputs/Fw_rept_threats2.svg')

#TRAITS REPRESENTATION - fig 5
#PLOT##################
library(scales)
fill_colours = c('LPI' = 'lightblue', 'Not LPI' = 'pink')
line_colours = c('LPI' = 'dodgerblue4', 'Not LPI' = 'deeppink4')

#Amphibians#########
#density plot
ggplot()+
  geom_density(data = Amph_trait_fw_merged, aes(x = Body_length_mm, colour = Group, fill = Group), alpha = 0.8)+
  #geom_vline(data = Fw_amph_metrics, aes(xintercept = Median, colour = Group))+
  scale_fill_manual(values = fill_colours, labels = c('Not LPI' = 'Not LPD', 'LPI' = 'LPD'))+
  scale_colour_manual(values = line_colours, labels = c('Not LPI' = 'Not LPD', 'LPI' = 'LPD')) +
  scale_x_log10(labels = comma)+
  xlab('Body length (mm)') + ylab('Density')+
  theme_classic()+
  theme(text = element_text(size = 15))+
  ggtitle('Fw amphibian traits')
ggsave('R_scripts+outputs/Freshwater amphibian traits.svg')

#boxplot
ggplot()+
  geom_boxplot(data = Amph_trait_fw_merged, aes(x = Group, y=  Body_length_mm, colour = Group, fill = Group), outlier.alpha = 0.4 )+
  geom_point(data = Fw_amph_metrics, aes(x = Group, y = Mean), colour = 'darkgreen', size = 4, shape = 18)+ #plot mean points
  scale_colour_manual(values = line_colours) +
  scale_fill_manual(values = fill_colours)+
  scale_y_log10(labels = comma)+
  scale_x_discrete(labels = c('Not LPI' = 'Not LPD', 'LPI' = 'LPD'))+
  theme_classic()+
  ylab('Body length (mm)')+
  theme(text = element_text(size = 15))+
  theme(legend.position = 'none')+
  ggtitle('Fw amphibian traits')
ggsave('R_scripts+outputs/Freshwater amphibian traits boxplot.svg')

#Reptiles#######################
ggplot()+
  geom_density(data = Rept_trait_fw_merged, aes(x = body_mass_g, colour = Group,fill = Group), alpha = 0.8)+
  scale_fill_manual(values = fill_colours, labels = c('Not LPI' = 'Not LPD', 'LPI' = 'LPD'))+
  scale_colour_manual(values = line_colours, labels = c('Not LPI' = 'Not LPD', 'LPI' = 'LPD')) +
  scale_x_log10(labels = comma, limits = c(0.1, 50000000))+
  xlab('Body mass (g)') + ylab('Density')+
  theme_classic()+
  theme(text = element_text(size = 15))+
  ggtitle('Fw reptile traits')
ggsave('R_scripts+outputs/Freshwater reptile traits.svg')

#boxplot
ggplot()+
  geom_boxplot(data = Rept_trait_fw_merged, aes(x = Group, y=  body_mass_g, colour = Group, fill = Group), outlier.alpha = 0.4 )+
  geom_point(data = Fw_rept_metrics, aes(x = Group, y = Mean), colour = 'darkgreen', size = 4, shape = 18)+ #plot mean points
  scale_colour_manual(values = line_colours) +
  scale_fill_manual(values = fill_colours)+
  scale_x_discrete(labels = c('Not LPI' = 'Not LPD', 'LPI' = 'LPD'))+
  scale_y_log10(labels = comma)+
  theme_classic()+
  ylab('Body mass (g)')+
  theme(legend.position = 'none')+
  theme(text = element_text(size = 15))+
  ggtitle('Fw reptile traits')
ggsave('R_scripts+outputs/Freshwater reptile traits boxplot.svg')

#LPI TRENDS - fig 6
#Fw amphibian and reptile trends
ggplot_multi_lpi(lpis,names=c("Freshwater reptiles", "Freshwater amphibians"), xlims=c(1970, 2017), ylims=c(0, 2))+
  theme_classic()+
  xlab('Year')+
  theme(text = element_text(size = 13))+
  labs(color='Taxonomic Group', fill = 'Taxonomic Group')
ggsave('R_scripts+outputs/Freshwater_herp_LPIs.svg')

#MIXED MODELS - fig 7
#with data
ggplot()+
  geom_point(data = Amph_model_data, aes(x = TS_length, y = Total_lambda, colour = Threat_status))+
  theme_classic()+
  xlab('Time Series Length')+
  ylab('Total lambda (λtot)')+
  labs(colour = 'Threat Status')+
  theme(text = element_text(size = 13))+
  scale_colour_manual(values=c("#56B4E9",  "darkred", "lightgray"))+
  geom_abline(intercept = -0.02890033, slope = -0.01222089, colour = "#56B4E9")+
  geom_abline(intercept = -0.24464962, slope = -0.01222089, colour = "darkred")+
  geom_abline(intercept = 0.03086236, slope = -0.01222089, colour = "#999999")
ggsave('R_scripts+outputs/Mixed_model_wwithdata.svg')

#Predictive model:
plot_model(LPI_mixed5, type = 'pred', terms = c( 'Threat_status','TS_length[2,10,20]'))+
  theme_bw()+
  xlab('Threat status')+
  theme(text = element_text(size = 13))+
  ylab('Total Lambda (λtot)')+
  labs(colour = 'Time Series Length (yrs)')
ggsave(filename = 'R_scripts+outputs/Predicted_mixedmodel.svg')

#effect on lambda:
plot_model(LPI_mixed5, show.intercept = TRUE, show.data = TRUE)+
  theme_bw()+
  ylab('Effect on total lambda (λtot)')+
  theme(text = element_text(size = 13))+
  xlab('Variables')
ggsave(filename = 'R_scripts+outputs/mixedmodel_effects.svg')




#GLOBAL RICHNESS MAPS (IUCN data) - SF1
ggplot()+
  My_Theme +
  geom_tile(data = spdfFINALamph, aes(x=x, y=y, fill=value), colour = NA) +
  scale_fill_gradientn(colours=c("darkslategrey","darkslategray1", "orange", "orangered", "orangered4")) +
  geom_sf(data=coast, linewidth = 0.2) + #changed this - need to refine
  xlab("Longitude") +
  ylab("Latitude") +
  labs(fill = "No. of species")
ggsave('Amphibian_fw_richness.png')

ggplot()+
  My_Theme +
  geom_tile(data = spdfFINALrept, aes(x=x, y=y, fill=value)) +
  scale_fill_gradientn(colours=c("darkslategrey","darkslategray1", 'orange', "orangered")) +
  geom_sf(data=coast, linewidth = 0.2) + #changed this - need to refine
  xlab("Longitude") +
  ylab("Latitude") +
  labs(fill = "No. of species")
ggsave('Reptile_fw_richness.png')

#LPD data - orders by IUCN realms - SF2a/b
#LPD Amphibians
ggplot(data = LPI_amph_order_realms)+
  geom_bar(aes(x = realm,y = Count, fill = Order), stat = 'identity')+
  theme_classic()+
  scale_fill_brewer(palette = "Pastel2")+
  theme(text = element_text(size = 13))+
  xlab('IUCN realm')+
  scale_x_discrete(guide = guide_axis(angle = 45))
ggsave('R_scripts+outputs/Orders_by_realm_amph_LPD.svg')

ggplot(data = LPI_rept_order_realms) +
  geom_bar(aes(x = realm,y = Count, fill = Order), stat = 'identity')+
  theme_classic()+
  scale_fill_brewer(palette = "Pastel2")+
  theme(text = element_text(size = 13))+
  xlab('IUCN realm')+
  scale_x_discrete(guide = guide_axis(angle = 45))
ggsave('R_scripts+outputs/Orders_by_realm_rept_LPD.svg')

#LPD data - orders by IUCN threat categories - SF2c/d
# FIGURES
#LPD Amphibians
ggplot(data = LPI_amph_realm_threats)+ 
  geom_bar(aes(x = realm, y = count,fill = Threatened), stat = 'identity')+
  theme_classic()+
  scale_fill_brewer(palette = 'Oranges')+
  theme(text = element_text(size = 13))+
  labs(fill = 'IUCN threat category')+
  xlab('IUCN realm')+
  scale_x_discrete(guide = guide_axis(angle = 45))
ggsave('R_scripts+outputs/Threats_by_realm_amph_LPD.svg')

#LPD reptiles
ggplot(data = LPI_rept_realm_threats)+ 
  geom_bar(aes(x = realm, y = count,fill = Threatened), stat = 'identity')+
  theme_classic()+
  scale_fill_brewer(palette = 'Oranges')+
  theme(text = element_text(size = 13))+
  labs(fill = 'IUCN threat category')+
  xlab('IUCN realm')+
  scale_x_discrete(guide = guide_axis(angle = 45))
ggsave('R_scripts+outputs/Threats_by_realm_rept_LPD.svg')

#LPD body sizes split by order, realms and threats - SF3

#density plots by order (just LPD data)
#amphibians
ggplot()+
  geom_density(data = LPI_fw_amph_mixed, aes(x = Body_length_mm, fill = Order), alpha = 0.8)+
  scale_x_log10(labels = comma)+
  xlab('Body length (mm)') + ylab('Density')+
  scale_fill_brewer(palette = "Pastel2")+
  theme_classic()+
  theme(text = element_text(size = 15))+
  ggtitle('Fw amphibian traits by order (LPD)')
ggsave('R_scripts+outputs/Freshwater amphibian traits by order (LPD).svg')
#reptiles
ggplot()+
  geom_density(data = LPI_fw_rept_mixed, aes(x = body_mass_g, fill = Order), alpha = 0.8)+
  scale_x_log10(labels = comma)+
  xlab('Body mass (g))') + ylab('Density')+
  scale_fill_brewer(palette = "Pastel2")+
  theme_classic()+
  theme(text = element_text(size = 15))+
  ggtitle('Fw reptile traits by order (LPD)')
ggsave('R_scripts+outputs/Freshwater reptile traits by order (LPD).svg')

#by IUCN realm
#amphibians
ggplot()+
  geom_boxplot(data = LPI_fw_amph_mixed2, aes(x = realm, y = Body_length_mm, fill = realm))+
  scale_y_log10(labels = comma)+
  xlab('Realm') + ylab('Body length (mm)')+
  theme_classic()+
  theme(legend.position = 'none')+
  theme(text = element_text(size = 15))+
  xlab('IUCN realm')+
  scale_x_discrete(guide = guide_axis(angle = 45))+
  ggtitle('Fw amphibian traits by realm (LPD)')
ggsave('R_scripts+outputs/Freshwater amphibian traits by realm (LPD).svg')

#reptiles
ggplot()+
  geom_boxplot(data = LPI_fw_rept_mixed2, aes(x = realm, y  = body_mass_g, fill = realm))+
  scale_y_log10(labels = comma)+
  xlab('Realm') + ylab('Body mass (g)')+
  theme_classic()+
  theme(legend.position = 'none')+
  theme(text = element_text(size = 15))+
  xlab('IUCN realm')+
  scale_x_discrete(guide = guide_axis(angle = 45))+
  ggtitle('Fw reptile traits by realm (LPD)')
ggsave('R_scripts+outputs/Freshwater reptile traits by realm (LPD).svg')

#by threat category
#amphibians 
ggplot()+
  geom_boxplot(data = LPI_fw_amph_mixed2[!LPI_fw_amph_mixed2$Threatened == 'DD', ], 
               aes(x = Threatened, y = Body_length_mm, fill = Threatened))+
  scale_y_log10(labels = comma)+
  ylab('Body length (mm)') + xlab('Threat category')+
  theme_classic()+
  theme(text = element_text(size = 15))+
  scale_fill_brewer(palette = 'Oranges')+
  ggtitle('Fw amphibian traits by threat category (LPD)')
ggsave('R_scripts+outputs/Freshwater amphibian traits by threat category (LPD).svg')
#reptiles
ggplot()+
  geom_boxplot(data = LPI_fw_rept_mixed2[!is.na(LPI_fw_rept_mixed2$Threatened), ], 
               aes(x = Threatened, y = body_mass_g, fill = Threatened))+
  scale_y_log10(labels = comma)+
  ylab('Body mass (g)') + xlab('Threat category')+
  theme_classic()+
  theme(text = element_text(size = 15))+
  scale_fill_brewer(palette = 'Oranges')+
  ggtitle('Fw reptile traits by threat category (LPD)')
ggsave('R_scripts+outputs/Freshwater reptile traits by threat category (LPD).svg')


#LPI trends split by orders - SF4
#Reptiles split by orders 
ggplot_multi_lpi(rept_lpis,names=c("Crocodylians (17 sp)", "Squamates (20 sp)", 'Testudines (30 sp)'), xlims=c(1970, 2017), ylims=c(0, 6))+
  theme_classic()+
  xlab('Year')+
  theme(text = element_text(size = 13))+
  labs(color='Taxonomic Group', fill = 'Taxonomic Group')
ggsave('R_scripts+outputs/Freshwater_rept_LPIs_split_edited.png')

#Amphibians split by orders
ggplot_multi_lpi(amph_lpis,names=c("Anura (241 sp)", "Caudata (43 sp)"), xlims=c(1970, 2017), ylims=c(0, 2))+
  theme_classic()+
  xlab('Year')+
  theme(text = element_text(size = 13))+
  labs(color='Taxonomic Group', fill = 'Taxonomic Group')
ggsave('R_scripts+outputs/Freshwater_amph_LPIs_split.png')


