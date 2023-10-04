
#######################################################################################
#TRAITS - see other scripts Adrienne function + Taxon_search_final for taxon matching###
#######################################################################################

#load trait data
Amph_traits = read.csv('Amph_trait_data/Etard_2020_tax_corrected/Amphibians.csv') #6990 entries
Vert_traits = read.csv('Reptile_trait_data/meiri_et_al._2021_v3.csv') #edited only LPI rows - emys blandingii and crocodylus johnsoni in v3
Rept_traits = Vert_traits %>%
  filter(Class == 'Reptilia') #11240 entries

#see Taxon_search_final for synonym code
Amph_trait_synonyms = read.csv('Amph_trait_data/Amph_traits_synonymsv2_SUCCESS.csv')
Rept_trait_synonyms = read.csv('Reptile_trait_data/Rept_traits_synonyms.csv')

#Amphibians#######################################

#merge trait dataset with synonyms
Amph_trait_merged = merge(Amph_traits, Amph_trait_synonyms, by.x = 'Best_guess_binomial', by.y = 'Species', all.x = T, all.y = T) #merge dataset with trait data
#have 6990 datapoints, need to merge with IUCN data
Amph_trait_IUCN = merge(Amph_trait_merged, IUCN_amph_assess, by.x = 'Accepted', by.y = 'scientificName', all.x = T, all.y = T) #merge trait data with IUCN with accepted names
#now have 7985 - includes those in IUCN without trait data, and those in trait data without IUCN
#note - some will be replicates (due to e.g. subspecies in trait dataset) - NOT REMOVING from complete datasets, will remove from LPI

#to see how many are in IUCN:
Amph_trait_IUCN_only = merge(Amph_trait_merged, IUCN_amph_assess, by.x = 'Accepted', by.y = 'scientificName', all.x = F, all.y = T)
#7594

#Filter to freshwater species - includes IUCN fw and those under LPI fw. NB some are not in trait data
Amph_trait_fw = Amph_trait_IUCN %>%
  filter(systems %in% c("Freshwater (=Inland waters)",'Freshwater (=Inland waters)|Marine', 
                        "Terrestrial|Freshwater (=Inland waters)", "Terrestrial|Freshwater (=Inland waters)|Marine")|
           Best_guess_binomial %in% c('Eleutherodactylus planirostris', 'Plethodon cinereus')| #not fw under IUCN but are in LPI
           Accepted %in% 'Plethodon glutinosus') #Plethodon glutinosus - no trait data available AND not fw under IUCN but in LPI
#5037 data entries

#see how many NA
sum(is.na(Amph_trait_fw$Body_length_mm)) #1040 without body size data now (out of those in IUCN)
sum(is.na(Amph_trait_fw$assessmentId)) #0 not in IUCN (others filtered out)

#how many have body size data
sum(complete.cases(Amph_trait_fw$Body_length_mm)) #3997

#refine dataset- updated 14.07#

#fw amphs data (all)
Amph_trait_fw_all = Amph_trait_fw %>%
  dplyr::select(c(Accepted, Best_guess_binomial, Body_length_mm)) %>%
  mutate(InLPI = rep(NA, times = 5037)) %>%  #add column for whether in LPI
  rename(Scientific_Name = Accepted) #rename column
#identify just those in LPI  - nb LPI_fw_amph_names contains IUCN names of species in LPI
for (i in 1:5037){
  if ((Amph_trait_fw_all$Scientific_Name[i] %in% LPI_fw_amph_names$IUCN.name) == T)
  {Amph_trait_fw_all$InLPI[i] = T}
  else {Amph_trait_fw_all$InLPI[i] = F}
}
write.csv(Amph_trait_fw_all, 'R_scripts+outputs/Amph_trait_fw_all.csv')

#filter to those not in LPI
Amph_trait_fw_notLPI = Amph_trait_fw_all %>%
  filter(Amph_trait_fw_all$InLPI == F) %>%
  mutate(Group = rep('Not LPI', times = 4744))
#4744 values - has removed 293 from 5037, consistent with LPI trait dataset below
missing_fwamph_traits_notLPI = subset(Amph_trait_fw_notLPI,is.na(Amph_trait_fw_notLPI$Body_length_mm) == T) 
#1019 'not LPIs' missing body size data (4744-1019 = 3725 not-LPD with body size data)


#create LPI trait datasets (want to keep LPI names for later mixed model)############ 

#LPI_fw_amph_traits = merge(LPI_fw_amph_names, Amph_trait_fw, 
#                           by.x = 'IUCN name', by.y = 'Accepted', all.x = T) 
#refine LPI data
#LPI_fw_amph_traits_edited = LPI_fw_amph_traits %>%
#  dplyr::select(c('IUCN name', 'LPI name', Best_guess_binomial, Body_length_mm))%>%
#  mutate(Group = rep('LPI', times = 293),
#         InLPI = rep( TRUE, times = 293))%>%
#  rename(Traits_name = Best_guess_binomial)
#293: adds extra 9 to LPI- NEED TO REMOVE EXTRA SUBSPECIES

#write.csv(LPI_fw_amph_traits_edited, 'R_scripts+outputs/LPI_fw_amph_traits.csv')

#reload edited final dataset#################################
#i.e. includes only those species with exact matching binomials in LPD and trait data
LPI_fw_amph_traits = read.csv('R_scripts+outputs/LPI_fw_amph_traits_edited.csv') #nb can't combine with Amph_traits_fw_notLPI as is - different columns
#284 entries - excludes subspecies

#check missing data
missing_fwamph_traits_LPI = subset(LPI_fw_amph_traits,is.na(LPI_fw_amph_traits$Body_length_mm) == T) 
#18 LPIs missing body size data (284-18 = 266 LPD with data) need to remove NA values in calculations

#need to bind edited LPI traits and non LPI traits - only ones containing body size data###################
LPI_fw_amph_traits_2 = LPI_fw_amph_traits %>%
  dplyr::select(c('IUCN.name', 'Traits_name', 'Body_length_mm', 'Group')) %>%
  filter(!is.na(Body_length_mm))%>%
  rename('IUCN name' = 'IUCN.name')
Amph_trait_fw_notLPI2 = Amph_trait_fw_notLPI %>%
  dplyr::select(c('Scientific_Name', 'Best_guess_binomial', 'Body_length_mm', 'Group')) %>%
  filter(!is.na(Body_length_mm))%>%
  rename('IUCN name' = 'Scientific_Name', 'Traits_name' = 'Best_guess_binomial')

Amph_trait_fw_merged = rbind.data.frame(Amph_trait_fw_notLPI2,LPI_fw_amph_traits_2) #use in plotting
levels(as.factor(Amph_trait_fw_merged$Group))
#reorder levels
Amph_trait_fw_merged$Group = factor(Amph_trait_fw_merged$Group, levels = c('Not LPI', 'LPI'))

#Reptiles##########################################################################
Rept_trait_merged = merge(Rept_traits, Rept_trait_synonyms, by.x = 'binomial_2020', by.y = 'Species', all.x = T, all.y = T)
Rept_trait_IUCN = merge(Rept_trait_merged, IUCN_rept_assess, by.x = 'Accepted', by.y = 'scientificName', all.x = T, all.y = T)
#above contains all trait and all IUCN data

#to see number if only IUCN names:
Rept_trait_IUCN_only = merge(Rept_trait_merged, IUCN_rept_assess, by.x = 'Accepted', by.y = 'scientificName', all.x = F, all.y = T)
#10750

#need to include 2 missing reptiles in LPI, and those not included as fw under IUCN - worked out by looking at missing data
Rept_trait_fw = Rept_trait_IUCN %>%
  filter(systems %in% c("Freshwater (=Inland waters)",'Freshwater (=Inland waters)|Marine', 
                        "Terrestrial|Freshwater (=Inland waters)", "Terrestrial|Freshwater (=Inland waters)|Marine")|
           binomial_2020 %in% c('Trachemys dorbigni', 'Hydromedusa tectifera', 'Intellagama lesueurii')) #last one not fw under IUCN (first two missing)
#663 fw species with trait data

#see how many NA
sum(is.na(Rept_trait_fw$body_mass_g)) #27 without body mass data 
sum(is.na(Rept_trait_fw$assessmentId)) #2 not in IUCN (as expected)
#how many have body size data
sum(complete.cases(Rept_trait_fw$body_mass_g)) #636

#refine datasest###
#All fw rept data
Rept_trait_fw_all = Rept_trait_fw %>%
  dplyr::select(c(Accepted, binomial_2020, body_mass_g))%>%
  mutate(InLPI = rep(NA, times = 663)) %>%  #add column for whether in LPI
  rename(Scientific_Name = Accepted) #rename column

#identify those in LPI and remove
#LPI_fw_rept_names contains all those in LPI
for (i in 1:663){
  if ((Rept_trait_fw_all$Scientific_Name[i] %in% LPI_fw_rept_names$IUCN.name) == T)
  {Rept_trait_fw_all$InLPI[i] = T}
  else if ((Rept_trait_fw_all$binomial_2020[i] %in% LPI_fw_rept_names$IUCN.name) == T) #removes 2 fw reptiles not in IUCN but in LPI
  {Rept_trait_fw_all$InLPI[i] = T}
  else {Rept_trait_fw_all$InLPI[i] = F}
}
write.csv(Rept_trait_fw_all, 'R_scripts+outputs/Rept_trait_fw_all.csv')
#filter to those not in LPI
Rept_trait_fw_notLPI = Rept_trait_fw_all %>%
  filter(Rept_trait_fw_all$InLPI == F)%>%
  mutate(Group = rep('Not LPI', times = 592))

#592 values - has removed 71 from 663 - consistent with below
missing_fwrept_traitsnotLPI = subset(Rept_trait_fw_notLPI,is.na(Rept_trait_fw_notLPI$body_mass_g) == T)
#25 'not LPDs' missing body mass data (592-25 = 567 with body size data)


#create LPI trait dataset - keeping LPI names#####################################
#LPI_fw_rept_traits = merge(LPI_fw_rept_names, Rept_trait_IUCN, 
#                          by.x = 'IUCN name', by.y = 'Accepted', all.x = T) #71: adds extra 4 to LPI, but nb get no values for 2 species not in IUCN
#refine LPI data
#LPI_fw_rept_traits_edited = LPI_fw_rept_traits %>%
#  dplyr::select(c('IUCN name', 'LPI name', binomial_2020, body_mass_g))%>%
#  mutate(Group = rep('LPI', times =71),
#         InLPI = rep( TRUE, times = 71))%>%
#  rename(Traits_name = binomial_2020)

#write csv and remove duplicates in excel
#write.csv(LPI_fw_rept_traits_edited, 'R_scripts+outputs/LPI_fw_rept_traits.csv')

#BUT need to include 2 entries not in IUCN
#Rept_trait_fw_LPI = Rept_trait_fw_all %>%
#  filter(Rept_trait_fw_all$InLPI == T)
#added these in manually to final dataset below

#load edited LPI dataset################################
LPI_fw_rept_traits = read.csv('R_scripts+outputs/LPI_fw_rept_traits_edited.csv') #excludes subspecies

missing_fwrept_traitsLPI = subset(LPI_fw_rept_traits,is.na(LPI_fw_rept_traits$body_mass_g) == T) 
#2 LPIs missing trait data (67-2 = 65 LPDs with body size data)

#bind edited LPI and not LPI trait datasets - only ones containing body size data#######
LPI_fw_rept_traits_2 = LPI_fw_rept_traits %>%
  dplyr::select(c('IUCN.name', 'Traits_name', 'body_mass_g', 'Group')) %>%
  filter(!is.na(body_mass_g))%>%
  rename('IUCN name' = 'IUCN.name')
Rept_trait_fw_notLPI2 = Rept_trait_fw_notLPI %>%
  dplyr::select(c('Scientific_Name', 'binomial_2020', 'body_mass_g', 'Group')) %>%
  filter(!is.na(body_mass_g))%>%
  rename('IUCN name' = 'Scientific_Name', 'Traits_name' = 'binomial_2020')

Rept_trait_fw_merged = rbind.data.frame(Rept_trait_fw_notLPI2, LPI_fw_rept_traits_2) #use in plotting
levels(as.factor(Rept_trait_fw_merged$Group))
#reorder levels
Rept_trait_fw_merged$Group = factor(Rept_trait_fw_merged$Group, levels = c('Not LPI', 'LPI'))


########################################################################

####METRIC CALCULATIONS 
#calculate mean and median for freshwater amphs
LPI_fw_amph_metrics = c('Mean' = mean(LPI_fw_amph_traits$Body_length_mm, na.rm = T), 'Median' = median(LPI_fw_amph_traits$Body_length_mm, na.rm = T))
NotLPI_fw_amph_metrics = c('Mean' = mean(Amph_trait_fw_notLPI$Body_length_mm, na.rm = T), 'Median' = median(Amph_trait_fw_notLPI$Body_length_mm, na.rm = T))
Fw_amph_metrics = data.frame(Group = c('Not LPI', 'LPI'),
                             Mean = c(NotLPI_fw_amph_metrics[1], LPI_fw_amph_metrics[1]),
                             Median = c(NotLPI_fw_amph_metrics[2], LPI_fw_amph_metrics[2]))

#calculate mean and median for freshwater repts
LPI_fw_rept_metrics = c('Mean' = mean(LPI_fw_rept_traits$body_mass_g, na.rm = T), 'Median' = median(LPI_fw_rept_traits$body_mass_g, na.rm = T))
NotLPI_fw_rept_metrics = c('Mean' = mean(Rept_trait_fw_notLPI$body_mass_g, na.rm = T), 'Median' = median(Rept_trait_fw_notLPI$body_mass_g, na.rm = T))
Fw_rept_metrics = data.frame(Group = c('Not LPI', 'LPI'),
                             Mean = c(NotLPI_fw_rept_metrics[1], LPI_fw_rept_metrics[1]),
                             Median = c(NotLPI_fw_rept_metrics[2], LPI_fw_rept_metrics[2]))


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
  ggtitle('Fw amphibian traits')
ggsave('R_scripts+outputs/Freshwater amphibian traits.svg')

#boxplot
ggplot()+
  geom_boxplot(data = Amph_trait_fw_merged, aes(x = Group, y=  Body_length_mm, colour = Group, fill = Group))+
  geom_point(data = Fw_amph_metrics, aes(x = Group, y = Mean), colour = 'darkgreen', size = 4, shape = 18)+ #plot mean points
  scale_colour_manual(values = line_colours) +
  scale_fill_manual(values = fill_colours)+
  scale_y_log10(labels = comma)+
  scale_x_discrete(labels = c('Not LPI' = 'Not LPD', 'LPI' = 'LPD'))+
  theme_classic()+
  ylab('Body length (mm)')+
  theme(legend.position = 'none')+
  ggtitle('Fw amphibian traits')
ggsave('R_scripts+outputs/Freshwater amphibian traits boxplot.svg')

#Reptiles#######################
ggplot()+
  geom_density(data = Rept_trait_fw_merged, aes(x = body_mass_g, colour = Group,fill = Group), alpha = 0.8)+
  scale_fill_manual(values = fill_colours, labels = c('Not LPI' = 'Not LPD', 'LPI' = 'LPD'))+
  scale_colour_manual(values = line_colours, labels = c('Not LPI' = 'Not LPD', 'LPI' = 'LPD')) +
  scale_x_log10(labels = comma)+
  xlab('Body mass (g)') + ylab('Density')+
  theme_classic()+
  ggtitle('Fw reptile traits')
ggsave('R_scripts+outputs/Freshwater reptile traits.svg')

#boxplot
ggplot()+
  geom_boxplot(data = Rept_trait_fw_merged, aes(x = Group, y=  body_mass_g, colour = Group, fill = Group))+
  geom_point(data = Fw_rept_metrics, aes(x = Group, y = Mean), colour = 'darkgreen', size = 4, shape = 18)+ #plot mean points
  scale_colour_manual(values = line_colours) +
  scale_fill_manual(values = fill_colours)+
  scale_x_discrete(labels = c('Not LPI' = 'Not LPD', 'LPI' = 'LPD'))+
  scale_y_log10(labels = comma)+
  theme_classic()+
  ylab('Body mass (g)')+
  theme(legend.position = 'none')+
  ggtitle('Fw reptile traits')
ggsave('R_scripts+outputs/Freshwater reptile traits boxplot.svg')

#test normality
#Amphibians
qqnorm(log(Amph_trait_fw_merged$Body_length_mm, base = 10))
qqline(log(Amph_trait_fw_merged$Body_length_mm, base = 10))
shapiro.test(log(Amph_trait_fw_merged$Body_length_mm, base = 10)) #not normal

#Reptiles
qqnorm(log(Rept_trait_fw_merged$body_mass_g, base = 10))
qqline(log(Rept_trait_fw_merged$body_mass_g, base = 10))
shapiro.test(log(Rept_trait_fw_merged$body_mass_g, base = 10)) #not normal

##########STATISTICAL ANALYSIS########## 
#Asymptotic two-sample Kolmogorov-Smirnov test - Reptiles
Rept_kstest = ks.test(log(Rept_trait_fw_notLPI$body_mass_g, base =10),
                      log(LPI_fw_rept_traits$body_mass_g, base = 10))
#D = 0.49833, p-value = 5.278e-13

#Asymptotic two-sample Kolmogorov-Smirnov test - Amphibians
Amph_kstest = ks.test(log(Amph_trait_fw_notLPI$Body_length_mm, base = 10), 
                      log(LPI_fw_amph_traits$Body_length_mm, base = 10))
#D = 0.17229, p-value = 7.94e-073
#calculate percentage overlaps
install.packages('overlapping')
library(overlapping)
Overlap_repts<-list(LPI = na.omit(log(LPI_fw_rept_traits$body_mass_g, base = 10)), notLPI = na.omit(log(Rept_trait_fw_notLPI$body_mass_g, base = 10)))
overlap(Overlap_repts) #59% overlap

Overlap_amphs<-list(LPI = na.omit(log(LPI_fw_amph_traits$Body_length_mm, base = 10)), notLPI = na.omit(log(Amph_trait_fw_notLPI$Body_length_mm, base = 10)))
overlap(Overlap_amphs) #83% overlap


