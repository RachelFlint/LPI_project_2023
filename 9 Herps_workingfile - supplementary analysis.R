#SUPPLEMENTARY ANALYSIS - why do we see these biases?

#####################Why do we see the order/threat biases we do? Due to realm biases?#######################

#Tables of orders/threats vs realms (for supp analysis S2 and S4)##########################################

#IUCN#############
IUCN_amph_order_realms = IUCN_freshwater_amph_merged %>% #need order info
  group_by(orderName, realm)%>%
  summarise(Count = length(unique(scientificName)))
IUCN_rept_order_realms = IUCN_freshwater_rept_merged %>% #need order info
  group_by(orderName, realm)%>%
  summarise(Count = length(unique(scientificName)))

#LPI - also use IUCN realms#################
LPI_amph_order_realms = IUCN_LPI_fw %>%
  filter(Class == 'Amphibia')%>%
  group_by(Order, realm) %>%
  summarise(Count = length(unique(Binomial)))

#one NA realm value for testudines- missing from IUCN database but in LPD
missing_rept = IUCN_LPI_fw %>%
  filter(is.na(realm)) #two testudines, both in Neotropical fw realm

LPI_rept_order_realms =  IUCN_LPI_fw %>%
  filter(Class == 'Reptilia')%>%
  mutate(realm = replace_na(realm, 'Neotropical'))%>% #replace missing realm values 
  group_by(Order, realm) %>%
  summarise(Count = length(unique(Binomial))) #use LPD binomials as missing IUCN ones for two testudines
#see tables s2.3 + s2.4 for orders split by realm (species level)

#FIGURES#######################
#LPD Amphibians
ggplot(data = LPI_amph_order_realms)+
  geom_bar(aes(x = realm,y = Count, fill = Order), stat = 'identity')+
  theme_classic()+
  scale_fill_brewer(palette = "Pastel2")+
  scale_x_discrete(guide = guide_axis(angle = 45))
ggsave('R_scripts+outputs/Orders_by_realm_amph_LPD.svg')

ggplot(data = LPI_rept_order_realms) +
  geom_bar(aes(x = realm,y = Count, fill = Order), stat = 'identity')+
  theme_classic()+
  scale_fill_brewer(palette = "Pastel2")+
  scale_x_discrete(guide = guide_axis(angle = 45))
ggsave('R_scripts+outputs/Orders_by_realm_rept_LPD.svg')

##### Why do we see these threat category biases in amphibians? due to realm biases?

#tables of threat data by realm (LPD data)
LPI_amph_realm_threats = IUCN_fw_amph_LPI2 %>% #using IUCN realms, not LPD, and using my broader 'threatened' categories in IUCN_fw_amph_LPI2
  group_by(Threatened, realm)%>% 
  summarise(count = length(unique(scientificName)))

LPI_rept_realm_threats = IUCN_fw_rept_LPI2 %>% 
  group_by(Threatened, realm)%>%
  summarise(count = length(unique(scientificName)))

#see supp analysis tables s4.4 and 4.5 for threat data split by realm

# FIGURES
#LPD Amphibians
ggplot(data = LPI_amph_realm_threats)+ 
  geom_bar(aes(x = realm, y = count,fill = Threatened), stat = 'identity')+
  theme_classic()+
  scale_fill_brewer(palette = 'Oranges')+
  scale_x_discrete(guide = guide_axis(angle = 45))
ggsave('R_scripts+outputs/Threats_by_realm_amph_LPD.svg')

#LPD reptiles
ggplot(data = LPI_rept_realm_threats)+ 
  geom_bar(aes(x = realm, y = count,fill = Threatened), stat = 'identity')+
  theme_classic()+
  scale_fill_brewer(palette = 'Oranges')+
  scale_x_discrete(guide = guide_axis(angle = 45))
ggsave('R_scripts+outputs/Threats_by_realm_rept_LPD.svg')


####ORDERS SPLIT BY THREATS - why do we see the split LPI trends we do? 
LPI_fw_amph_mixed3 = merge(LPI_fw_amph_names, IUCN_fw_amph_LPI2, by.x = 'IUCN.name', by.y = 'scientificName') #want my broader 'threatened' categories in IUCN_fw_amph_LPI2
LPI_fw_rept_mixed3 = merge(LPI_fw_rept_names, IUCN_fw_rept_LPI2, by.x = 'IUCN.name', by.y = 'scientificName')

#tables of threat data by order (LPD data)
LPI_amph_realm_orders = LPI_fw_amph_mixed3 %>% 
  group_by(Threatened, Order)%>% 
  summarise(count = length(unique(IUCN.name)))

LPI_rept_realm_orders = LPI_fw_rept_mixed3  %>% 
  group_by(Threatened, Order)%>%
  summarise(count = length(unique(IUCN.name)))

#plot bar charts
#amphibians
ggplot(data = LPI_amph_realm_orders)+ 
  geom_bar(aes(x = Order, y = count,fill = Threatened), stat = 'identity')+
  theme_classic()+
  scale_fill_brewer(palette = 'Oranges')
ggsave('R_scripts+outputs/Threats_by_order_amph_LPD.svg')

#reptiles
ggplot(data = LPI_rept_realm_orders)+ 
  geom_bar(aes(x = Order, y = count,fill = Threatened), stat = 'identity')+
  theme_classic()+
  scale_fill_brewer(palette = 'Oranges')
ggsave('R_scripts+outputs/Threats_by_order_rept_LPD.svg')



##################### Why do we see the body size biases we do?#############################

# LPD body size  by order, realm and threat category - fig SF3
#load datasets
LPI_fw_amph_traits = read.csv('R_scripts+outputs/LPI_fw_amph_traits_edited.csv') 
LPI_fw_rept_traits = read.csv('R_scripts+outputs/LPI_fw_rept_traits_edited.csv')

#SPLIT BY ORDER
#merge with LPI taxonomic info
LPI_fw_amph_mixed = merge(LPI_fw_amph_traits, LPI_fw_amph_names, by.x = 'LPI.name', by.y = 'LPI.name', all.x = T, all.y = F)
LPI_fw_rept_mixed = merge(LPI_fw_rept_traits, LPI_fw_rept_names, by.x = 'LPI.name', by.y = 'LPI.name', all.x = T, all.y = F)

levels(as.factor(LPI_fw_amph_mixed$Order))

#plot density plots by order (just LPD data)
#amphibians
ggplot()+
  geom_density(data = LPI_fw_amph_mixed, aes(x = Body_length_mm, fill = Order), alpha = 0.8)+
  scale_x_log10(labels = comma)+
  xlab('Body length (mm)') + ylab('Density')+
  theme_classic()+
  ggtitle('Fw amphibian traits by order (LPD)')
ggsave('R_scripts+outputs/Freshwater amphibian traits by order (LPD).png')

#anova test
anova_amph_order = aov(log(Body_length_mm, base = 10) ~ Order, data = LPI_fw_amph_mixed)
summary(anova_amph_order) #sig difference

#reptiles
ggplot()+
  geom_density(data = LPI_fw_rept_mixed, aes(x = body_mass_g, fill = Order), alpha = 0.8)+
  scale_x_log10(labels = comma)+
  xlab('Body mass (g))') + ylab('Density')+
  theme_classic()+
  ggtitle('Fw reptile traits by order (LPD)')
ggsave('R_scripts+outputs/Freshwater reptile traits by order (LPD).png')

#anova test
anova_rept_order = aov(log(body_mass_g, base = 10) ~ Order, data = LPI_fw_rept_mixed)
summary(anova_rept_order) #sig difference

#SPLIT BY REALM (IUCN realm)
#merge with IUCN assessment info
LPI_fw_amph_mixed2 = merge(LPI_fw_amph_traits, IUCN_amph_assess, by.x = 'IUCN.name', by.y = 'scientificName', all.x = T, all.y = F)
LPI_fw_rept_mixed2 = merge(LPI_fw_rept_traits, IUCN_rept_assess, by.x = 'IUCN.name', by.y = 'scientificName', all.x = T, all.y = F)
LPI_fw_rept_mixed2 = LPI_fw_rept_mixed2 %>% 
  mutate(realm = replace_na(realm, 'Neotropical'))#add in two neotropical species not in IUCN

#amphibians
ggplot()+
  geom_boxplot(data = LPI_fw_amph_mixed2, aes(x = realm, y = Body_length_mm, fill = realm))+
  scale_y_log10(labels = comma)+
  xlab('Realm') + ylab('Body length (mm)')+
  theme_classic()+
  theme(legend.position = 'none')+
  ggtitle('Fw amphibian traits by realm (LPD)')
ggsave('R_scripts+outputs/Freshwater amphibian traits by realm (LPD).png')

#anova test
anova_amph_realm = aov(log(Body_length_mm, base = 10) ~ realm, data = LPI_fw_amph_mixed2)
summary(anova_amph_realm) #sig difference

#reptiles
ggplot()+
  geom_boxplot(data = LPI_fw_rept_mixed2, aes(x = realm, y  = body_mass_g, fill = realm))+
  scale_y_log10(labels = comma)+
  xlab('Realm') + ylab('Body mass (g)')+
  theme_classic()+
  theme(legend.position = 'none')+
  ggtitle('Fw reptile traits by realm (LPD)')
ggsave('R_scripts+outputs/Freshwater reptile traits by realm (LPD).png')

anova_rept_realm= aov(log(body_mass_g, base = 10) ~ realm, data = LPI_fw_rept_mixed2)
summary(anova_rept_realm) #sig difference

#SPLIT BY BROAD THREAT CATEGORY
#amphibians - make box plots
ggplot()+
  geom_boxplot(data = LPI_fw_amph_mixed2[!LPI_fw_amph_mixed2$Threatened == 'DD', ], 
               aes(x = Threatened, y = Body_length_mm, fill = Threatened), alpha = 0.5)+
  scale_y_log10(labels = comma)+
  ylab('Body length (mm)') + xlab('Threat category')+
  theme_classic()+
  ggtitle('Fw amphibian traits by threat category (LPD)')
ggsave('R_scripts+outputs/Freshwater amphibian traits by threat category (LPD).png')

#anova test
anova_amph_threats = aov(log(Body_length_mm, base = 10) ~ Threatened, data = LPI_fw_amph_mixed2)
summary(anova_amph_threats) #no sig difference

#reptiles
ggplot()+
  geom_boxplot(data = LPI_fw_rept_mixed2[!is.na(LPI_fw_rept_mixed2$Threatened), ], 
               aes(x = Threatened, y = body_mass_g, fill = Threatened), alpha = 0.5)+
  scale_y_log10(labels = comma)+
  ylab('Body mass (g)') + xlab('Threat category')+
  theme_classic()+
  ggtitle('Fw reptile traits by threat category (LPD)')
ggsave('R_scripts+outputs/Freshwater reptile traits by threat category (LPD).png')

#anova test
anova_rept_threats = aov(log(body_mass_g, base = 10) ~ Threatened, data = LPI_fw_rept_mixed2)
summary(anova_rept_threats) #no sig difference




###IUCN DATA - including for info - not putting in report##################################

#Amph_trait_fw, Rept_trait_fw includes IUCN fw data and trait data (with lots of NAs)
#filter out NA values
Amph_trait_fw2 = Amph_trait_fw %>%
  filter(!is.na(Body_length_mm))
Rept_trait_fw2 = Rept_trait_fw %>%
  filter(!is.na(body_mass_g))

#BY ORDER (using trait dataset order) - best available 
#amphibians
ggplot()+
  geom_density(data = Amph_trait_fw, aes(x = Body_length_mm, fill = Order.x), alpha = 0.8)+
  scale_x_log10(labels = comma)+
  xlab('Body length (mm)') + ylab('Density')+
  theme_classic()+
  ggtitle('Fw amphibian traits by order (IUCN)')
ggsave('R_scripts+outputs/Freshwater amphibian traits by order (IUCN).png')

#reptiles
ggplot()+
  geom_density(data = Rept_trait_fw2, aes(x = body_mass_g, fill = order), alpha = 0.8)+
  scale_x_log10(labels = comma)+
  xlab('Body mass (g)') + ylab('Density')+
  theme_classic()+
  ggtitle('Fw reptile traits by order (IUCN)')
ggsave('R_scripts+outputs/Freshwater reptile traits by order (IUCN).png')


#By REALM
#amphibians
ggplot()+
  geom_boxplot(data = Amph_trait_fw2, aes(x = realm, y = Body_length_mm, fill = realm), alpha = 0.8)+
  scale_y_log10(labels = comma)+
  xlab('Realm') + ylab('Body length (mm)')+
  theme_classic()+
  ggtitle('Fw amphibian traits by realm (IUCN)')
ggsave('R_scripts+outputs/Freshwater amphibian traits by realm (IUCN).png')

#reptiles
ggplot()+
  geom_boxplot(data = Rept_trait_fw2, aes(x = realm, y = body_mass_g, fill = realm), alpha = 0.8)+
  scale_y_log10(labels = comma)+
  xlab('Realm') + ylab('Body mass (g)')+
  theme_classic()+
  ggtitle('Fw reptile traits by realm (IUCN)')
ggsave('R_scripts+outputs/Freshwater reptile traits by realm (IUCN).png')

#by threat category
#amphibians
ggplot()+
  geom_boxplot(data = Amph_trait_fw2, aes(x = redlistCategory, y = Body_length_mm, fill = redlistCategory), alpha = 0.8)+
  scale_y_log10(labels = comma)+
  xlab('Red List cateogry') + ylab('Body length (mm)')+
  theme_classic()+
  ggtitle('Fw amphibian traits by threat category (IUCN)')
ggsave('R_scripts+outputs/Freshwater amphibian traits by threat category (IUCN).png')

#reptiles
ggplot()+
  geom_boxplot(data = Rept_trait_fw2, aes(x = redlistCategory, y = body_mass_g, fill = redlistCategory), alpha = 0.8)+
  scale_y_log10(labels = comma)+
  xlab('Red List cateogry') + ylab('Body mass (g)')+
  theme_classic()+
  ggtitle('Fw reptile traits by threat category (IUCN)')
ggsave('R_scripts+outputs/Freshwater reptile traits by threat category (IUCN).png')


