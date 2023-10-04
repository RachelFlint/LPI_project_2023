# 4 - LPD threat representation

#######################
#THREATS DATA + tests##
######################

#proportions of threat categories for IUCN freshwater reptiles and amphibians##########################

#AMPHIBIANS#
#find levels
levels(as.factor(IUCN_amph_assess$redlistCategory)) #i.e. all amphs

#combine extinct categories 
for (i in 1:length(unique(IUCN_amph_assess$scientificName))){
  if (IUCN_amph_assess$redlistCategory[i] == "Extinct in the Wild"){IUCN_amph_assess$redlistCategory[i] = 'Extinct'}
}

#filter to fw, calculate number in each category
IUCN_fw_amph_threats = IUCN_amph_assess%>% 
  filter(systems %in% c("Freshwater (=Inland waters)", "Terrestrial|Freshwater (=Inland waters)"))%>%     #doesn't include those not in IUCN fw
  group_by(redlistCategory) %>%
  summarise(Total_amph_species_IUCN = length(unique(scientificName)))

#add proportions + percentages
IUCN_fw_amph_threats$proportion_fw_amph_IUCN = (IUCN_fw_amph_threats$Total_amph_species_IUCN/sum(IUCN_fw_amph_threats$Total_amph_species_IUCN))
IUCN_fw_amph_threats$percent_amph_IUCN =IUCN_fw_amph_threats$proportion_fw_amph_IUCN*100


#REPTILES#
#find levels
levels(as.factor(IUCN_rept_assess$redlistCategory)) #i.e. all reptiles

#edit defunct categories - "Lower Risk/conservation dependent" "Lower Risk/least concern""Lower Risk/near threatened" , combine EITW with extinct
  for (i in 1:length(unique(IUCN_rept_assess$scientificName))){
         if (IUCN_rept_assess$redlistCategory[i] == 'Lower Risk/conservation dependent') {IUCN_rept_assess$redlistCategory[i] = 'Near Threatened'}
        else if (IUCN_rept_assess$redlistCategory[i] == 'Lower Risk/near threatened'){IUCN_rept_assess$redlistCategory[i] = 'Near Threatened'}
         else if (IUCN_rept_assess$redlistCategory[i] == "Lower Risk/least concern"){IUCN_rept_assess$redlistCategory[i] = 'Least Concern'}
         else if (IUCN_rept_assess$redlistCategory[i] == "Extinct in the Wild"){IUCN_rept_assess$redlistCategory[i] = 'Extinct'}
       }

#filter to fw, calculate number in each threat category
IUCN_fw_rept_threats = IUCN_rept_assess%>%
  filter(systems %in% c("Freshwater (=Inland waters)",'Freshwater (=Inland waters)|Marine', 
                        "Terrestrial|Freshwater (=Inland waters)", "Terrestrial|Freshwater (=Inland waters)|Marine"))%>%     #doesn't include those not in IUCN fw
  group_by(redlistCategory) %>%
  summarise(Total_rept_species_IUCN = length(unique(scientificName)))

#add proportions + percentages
IUCN_fw_rept_threats$proportion_fw_rept_IUCN = (IUCN_fw_rept_threats$Total_rept_species_IUCN/sum(IUCN_fw_rept_threats$Total_rept_species_IUCN))
IUCN_fw_rept_threats$percent_rept_IUCN =IUCN_fw_rept_threats$proportion_fw_rept_IUCN*100

#merge
IUCN_fw_herps_threats = merge(IUCN_fw_amph_threats, IUCN_fw_rept_threats, by.x = 'redlistCategory', by.y = 'redlistCategory',
                             all.x = T, all.y = T)
#NOTE: above don't include ones classified as fw only in LPD or ones not in IUCN (4+2 species) - can't easily as have to filter fw from entire IUCN dataset

#LPI amphibians and reptiles ###################################################

#Identify for IUCN datasets whether species are in fw LPD or not
#AMPHIBIANS: IUCN_amph_assess - 7486 entries - include all as some not fw under IUCN
for (i in 1:7486){
  if ((IUCN_amph_assess$scientificName[i] %in% LPI_fw_amph_names$IUCN.name) == T)
  {IUCN_amph_assess$InLPI[i] = T}
  else {IUCN_amph_assess$InLPI[i] = F}
}

#REPTILES: IUCN_rept_assess - 10222 entries
for (i in 1:10222 ){
  if ((IUCN_rept_assess$scientificName[i] %in% LPI_fw_rept_names$IUCN.name) == T)
  {IUCN_rept_assess$InLPI[i] = T}
  else {IUCN_rept_assess$InLPI[i] = F}
}

#now, filter to those that are in LPI
#AMPHIBIANS
IUCN_fw_amph_LPI = IUCN_amph_assess %>%
  filter(InLPI == T)
#284 species - includes 3 not listed as fw under IUCN

#REPTILES
IUCN_fw_rept_LPI = IUCN_rept_assess %>%
  filter(InLPI == T)
#65 species - includes  1 not listed as fw but missing 2 not listed under IUCN

#assess number of amphs in each threat category
LPI_fw_amph_threats = IUCN_fw_amph_LPI %>%
  group_by(redlistCategory) %>%
  summarise(Total_amph_species_LPI = length(unique(scientificName)))
#add proportions
LPI_fw_amph_threats$proportion_amph_LPI = LPI_fw_amph_threats$Total_amph_species_LPI/sum(LPI_fw_amph_threats$Total_amph_species_LPI)
LPI_fw_amph_threats$percent_amph_LPI = LPI_fw_amph_threats$proportion_amph_LPI*100

#assess number of repts in each threat category
LPI_fw_rept_threats = IUCN_fw_rept_LPI %>%
  group_by(redlistCategory) %>%
  summarise(Total_rept_species_LPI = length(unique(scientificName)))
#add proportions
LPI_fw_rept_threats$proportion_rept_LPI = LPI_fw_rept_threats$Total_rept_species_LPI/sum(LPI_fw_rept_threats$Total_rept_species_LPI)
LPI_fw_rept_threats$percent_rept_LPI = LPI_fw_rept_threats$proportion_rept_LPI*100


#merge 
LPI_fw_herps_threats = merge(LPI_fw_amph_threats, LPI_fw_rept_threats, 
                             by.x = 'redlistCategory', by.y = 'redlistCategory', all.x = T, all.y = T)

#ORGANISE DATA######################

#merge IUCN and LPI datasets
Fw_amph_threats = merge(IUCN_fw_amph_threats, LPI_fw_amph_threats, by.x = 'redlistCategory', by.y = 'redlistCategory',
                        all.x = T, all.y = T)
Fw_amph_threats = Fw_amph_threats %>%
  mutate(Total_amph_species_LPI= ifelse(is.na(Total_amph_species_LPI), 0, Total_amph_species_LPI)) #change NAs to 0
write.csv(Fw_amph_threats, 'R_scripts+outputs/Fw_amph_threats_LPI_IUCN.csv') 

Fw_rept_threats = merge(IUCN_fw_rept_threats, LPI_fw_rept_threats, by.x = 'redlistCategory', by.y = 'redlistCategory',
                        all.x = T, all.y = T)
Fw_rept_threats = Fw_rept_threats %>%
  mutate(Total_rept_species_LPI= ifelse(is.na(Total_rept_species_LPI), 0, Total_rept_species_LPI)) #change NAs to 0
write.csv(Fw_rept_threats, 'R_scripts+outputs/Fw_rept_threats_LPI_IUCN.csv')

#merge
Fw_herp_threats = merge(Fw_amph_threats, Fw_rept_threats, by.x = 'redlistCategory', by.y = 'redlistCategory', all.x = T, all.y = T)

# select key columns + reorganise tables for plotting####################
library(tidyverse)
Fw_amph_threats_percent = Fw_amph_threats%>%
  dplyr::select(redlistCategory,percent_amph_LPI, percent_amph_IUCN)%>%
  rename(LPI = percent_amph_LPI)%>%
  rename(IUCN = percent_amph_IUCN)%>%
  pivot_longer(cols=2:3, names_to = 'Group', values_to = 'Percent')%>%
  mutate(Percent = ifelse(is.na(Percent), 0, Percent)) #change NAs to 0

Fw_rept_threats_percent = Fw_rept_threats%>%
  dplyr::select(redlistCategory,  percent_rept_LPI, percent_rept_IUCN)%>%
  rename(LPI = percent_rept_LPI)%>%
  rename(IUCN = percent_rept_IUCN)%>%
  pivot_longer(cols=2:3, names_to = 'Group', values_to = 'Percent')%>%
  mutate(Percent = ifelse(is.na(Percent), 0, Percent)) #change NAs to 0

#PLOT
colours = c(IUCN = 'red4', LPI = 'lightblue')
#plot bar chart - amphibians
ggplot(Fw_amph_threats_percent, aes(fill=Group, y = redlistCategory, x = Percent))+
  geom_bar(position="dodge", stat="identity")+
  theme_bw()+
  scale_fill_manual(values = colours)+
  ggtitle('Threat representation - fw amphibians')
ggsave('R_scripts+outputs/Fw_amph_threats.png')

#plot bar chart - reptiles
ggplot(Fw_rept_threats_percent, aes(fill=Group, y = redlistCategory, x = Percent))+
  geom_bar(position="dodge", stat="identity")+
  theme_bw()+
  scale_fill_manual(values = colours)+
  ggtitle('Threat representation - fw reptiles')
ggsave('R_scripts+outputs/Fw_rept_threats.png')

###PROPORTION TESTS###############################################

#bonf corrections
bonf_amph = 0.05/7 #0.00714
bonf_rept = 0.05/7 

#REPTILES
x1_rept = Fw_herp_threats$Total_rept_species_IUCN
n1_rept = sum(Fw_herp_threats$Total_rept_species_IUCN)
x2_rept = Fw_herp_threats$Total_rept_species_LPI
n2_rept = sum(Fw_herp_threats$Total_rept_species_LPI)

Prop_test_rept = data.frame('Threat' = Fw_herp_threats$redlistCategory,
                             'X.squared' = NA,
                             'DF' = NA,
                             'P_value' = NA)
for (i in 1:7) {
  Prop_test_rept$X.squared[i] <- prop.test(c(x1_rept[i], x2_rept[i]), c(n1_rept, n2_rept), alternative = "two.sided", conf.level = 0.95)$statistic 
  Prop_test_rept$DF[i] <- prop.test(c(x1_rept[i], x2_rept[i]), c(n1_rept, n2_rept), alternative = "two.sided", conf.level = 0.95)$parameter 
  Prop_test_rept$P_value[i] <- prop.test(c(x1_rept[i], x2_rept[i]), c(n1_rept, n2_rept), alternative = "two.sided", conf.level = 0.95)$p.value
}
#none siginificant
#In prop.test(c(x1_rept[i], x2_rept[i]), c(n1_rept, n2_rept), alternative = "two.sided",  :Chi-squared approximation may be incorrect

#Amphibians
x1_amph = Fw_herp_threats$Total_amph_species_IUCN
n1_amph = sum(Fw_herp_threats$Total_amph_species_IUCN)
x2_amph = Fw_herp_threats$Total_amph_species_LPI
n2_amph = sum(Fw_herp_threats$Total_amph_species_LPI)

Prop_test_amph = data.frame('Threat' = Fw_herp_threats$redlistCategory,
                             'X.squared' = NA,
                             'DF' = NA,
                             'P_value' = NA)
for (i in 1:7) {
  Prop_test_amph$X.squared[i] <- prop.test(c(x1_amph[i], x2_amph[i]), c(n1_amph, n2_amph), alternative = "two.sided", conf.level = 0.95)$statistic
  Prop_test_amph$DF[i] <- prop.test(c(x1_amph[i], x2_amph[i]), c(n1_amph, n2_amph), alternative = "two.sided", conf.level = 0.95)$parameter
  Prop_test_amph$P_value[i] <- prop.test(c(x1_amph[i], x2_amph[i]), c(n1_amph, n2_amph), alternative = "two.sided", conf.level = 0.95)$p.value
}


#################################################
#####Split into threatened and not threatened#####
################################################
#assign categories into two main groups (threatened/not threatened)

#AMPHIBIANS#
#check levels
levels(as.factor(IUCN_amph_assess$redlistCategory)) #i.e. all amphs
#add new column to dataset
IUCN_amph_assess =  IUCN_amph_assess %>%
  mutate(Threatened = NA)

for (i in 1:length(unique(IUCN_amph_assess$scientificName))){
  if (IUCN_amph_assess$redlistCategory[i] == "Critically Endangered"){IUCN_amph_assess$Threatened[i] = 'Threatened'}
  else if (IUCN_amph_assess$redlistCategory[i] == "Endangered"){IUCN_amph_assess$Threatened[i] = 'Threatened'}
  else if (IUCN_amph_assess$redlistCategory[i] == "Vulnerable"){IUCN_amph_assess$Threatened[i] = 'Threatened'}
  else if (IUCN_amph_assess$redlistCategory[i] == "Extinct"){IUCN_amph_assess$Threatened[i] = 'Extinct'}
  else if (IUCN_amph_assess$redlistCategory[i] == "Data Deficient"){IUCN_amph_assess$Threatened[i] = 'DD'}
  else if (IUCN_amph_assess$redlistCategory[i] == "Least Concern"){IUCN_amph_assess$Threatened[i] = 'Not Threatened'}
  else if (IUCN_amph_assess$redlistCategory[i] == "Near Threatened"){IUCN_amph_assess$Threatened[i] = 'Not Threatened'}
}
#filter to fw, calculate number in each category
IUCN_fw_amph_threats2 = IUCN_amph_assess%>% 
  filter(systems %in% c("Freshwater (=Inland waters)", "Terrestrial|Freshwater (=Inland waters)"))%>%     #doesn't include those not in IUCN fw
  group_by(Threatened) %>%
  summarise(Total_amph_species_IUCN = length(unique(scientificName)))

#add proportions + percentages
IUCN_fw_amph_threats2$proportion_fw_amph_IUCN = (IUCN_fw_amph_threats2$Total_amph_species_IUCN/sum(IUCN_fw_amph_threats2$Total_amph_species_IUCN))
IUCN_fw_amph_threats2$percent_amph_IUCN =IUCN_fw_amph_threats2$proportion_fw_amph_IUCN*100


#REPTILES
#check levels
levels(as.factor(IUCN_rept_assess$redlistCategory)) 
#add new column to dataset
IUCN_rept_assess =  IUCN_rept_assess %>%
  mutate(Threatened = NA)

for (i in 1:length(unique(IUCN_rept_assess$scientificName))){
  if (IUCN_rept_assess$redlistCategory[i] == "Critically Endangered"){IUCN_rept_assess$Threatened[i] = 'Threatened'}
  else if (IUCN_rept_assess$redlistCategory[i] == "Endangered"){IUCN_rept_assess$Threatened[i] = 'Threatened'}
  else if (IUCN_rept_assess$redlistCategory[i] == "Vulnerable"){IUCN_rept_assess$Threatened[i] = 'Threatened'}
  else if (IUCN_rept_assess$redlistCategory[i] == "Extinct"){IUCN_rept_assess$Threatened[i] = 'Extinct'}
  else if (IUCN_rept_assess$redlistCategory[i] == "Data Deficient"){IUCN_rept_assess$Threatened[i] = 'DD'}
  else if (IUCN_rept_assess$redlistCategory[i] == "Least Concern"){IUCN_rept_assess$Threatened[i] = 'Not Threatened'}
  else if (IUCN_rept_assess$redlistCategory[i] == "Near Threatened"){IUCN_rept_assess$Threatened[i] = 'Not Threatened'}
}

#Calculate number in each category (for IUCN data)
#filter to fw, calculate number in each category
IUCN_fw_rept_threats2 = IUCN_rept_assess%>%
  filter(systems %in% c("Freshwater (=Inland waters)",'Freshwater (=Inland waters)|Marine', 
                        "Terrestrial|Freshwater (=Inland waters)", "Terrestrial|Freshwater (=Inland waters)|Marine"))%>%     #doesn't include those not in IUCN fw
  group_by(Threatened) %>%
  summarise(Total_rept_species_IUCN = length(unique(scientificName)))

#add proportions + percentages
IUCN_fw_rept_threats2$proportion_fw_rept_IUCN = (IUCN_fw_rept_threats2$Total_rept_species_IUCN/sum(IUCN_fw_rept_threats2$Total_rept_species_IUCN))
IUCN_fw_rept_threats2$percent_rept_IUCN =IUCN_fw_rept_threats2$proportion_fw_rept_IUCN*100

#CALCULATE NUMBERS IN LPI 
#filter to those that are in LPI
#AMPHIBIANS
IUCN_fw_amph_LPI2 = IUCN_amph_assess %>%
  filter(InLPI == T)
#284 species - includes 3 not listed as fw under IUCN

#REPTILES
IUCN_fw_rept_LPI2 = IUCN_rept_assess %>%
  filter(InLPI == T)
#65 species - includes  1 not listed as fw but missing 2 not listed under IUCN

#assess number of amphs in each threat category
LPI_fw_amph_threats2 = IUCN_fw_amph_LPI2 %>%
  group_by(Threatened) %>%
  summarise(Total_amph_species_LPI = length(unique(scientificName)))
#add proportions
LPI_fw_amph_threats2$proportion_amph_LPI = LPI_fw_amph_threats2$Total_amph_species_LPI/sum(LPI_fw_amph_threats2$Total_amph_species_LPI)
LPI_fw_amph_threats2$percent_amph_LPI = LPI_fw_amph_threats2$proportion_amph_LPI*100

#assess number of repts in each threat category
LPI_fw_rept_threats2 = IUCN_fw_rept_LPI2 %>%
  group_by(Threatened) %>%
  summarise(Total_rept_species_LPI = length(unique(scientificName)))
#add proportions
LPI_fw_rept_threats2$proportion_rept_LPI = LPI_fw_rept_threats2$Total_rept_species_LPI/sum(LPI_fw_rept_threats2$Total_rept_species_LPI)
LPI_fw_rept_threats2$percent_rept_LPI = LPI_fw_rept_threats2$proportion_rept_LPI*100

#merge IUCN and LPI datasets
#amphs
Fw_amph_threats2 = merge(IUCN_fw_amph_threats2, LPI_fw_amph_threats2, by.x = 'Threatened', by.y = 'Threatened',
                        all.x = T, all.y = T)
Fw_amph_threats2 = Fw_amph_threats2 %>%
  mutate(Total_amph_species_LPI= ifelse(is.na(Total_amph_species_LPI), 0, Total_amph_species_LPI)) #change NAs to 0
write.csv(Fw_amph_threats2, 'R_scripts+outputs/Fw_amph_threats2.csv') 

#repts
Fw_rept_threats2 = merge(IUCN_fw_rept_threats2, LPI_fw_rept_threats2, by.x = 'Threatened', by.y = 'Threatened',
                        all.x = T, all.y = T)
Fw_rept_threats2 = Fw_rept_threats2 %>%
  mutate(Total_rept_species_LPI= ifelse(is.na(Total_rept_species_LPI), 0, Total_rept_species_LPI)) #change NAs to 0
write.csv(Fw_rept_threats2, 'R_scripts+outputs/Fw_rept_threats2.csv')

#merge
Fw_herp_threats2 = merge(Fw_amph_threats2, Fw_rept_threats2, by.x = 'Threatened', by.y = 'Threatened')

##chisq tests for overall threat distributions###
Amph_threat_chisq = chisq.test(x = Fw_amph_threats2$Total_amph_species_LPI, p = Fw_amph_threats2$proportion_fw_amph_IUCN) #significant
Rept_threat_chisq = chisq.test(x = Fw_rept_threats2$Total_rept_species_LPI, p = Fw_rept_threats2$proportion_fw_rept_IUCN) #not significant
#getting errors - may want to remove DD and extinct categories?

# select key columns + reorganise tables for plotting####################
library(tidyverse)
Fw_amph_threats_percent2 = Fw_amph_threats2%>%
  dplyr::select(Threatened,percent_amph_LPI, percent_amph_IUCN)%>%
  rename(LPI = percent_amph_LPI)%>%
  rename(IUCN = percent_amph_IUCN)%>%
  pivot_longer(cols=2:3, names_to = 'Group', values_to = 'Percent')%>%
  mutate(Percent = ifelse(is.na(Percent), 0, Percent)) #change NAs to 0

Fw_rept_threats_percent2 = Fw_rept_threats2%>%
  dplyr::select(Threatened,  percent_rept_LPI, percent_rept_IUCN)%>%
  rename(LPI = percent_rept_LPI)%>%
  rename(IUCN = percent_rept_IUCN)%>%
  pivot_longer(cols=2:3, names_to = 'Group', values_to = 'Percent')%>%
  mutate(Percent = ifelse(is.na(Percent), 0, Percent)) #change NAs to 0

#PLOT

#plot bar chart - amphibians
ggplot(Fw_amph_threats_percent2, aes(fill=Threatened,  x = Group, y = Percent))+
  geom_bar( stat="identity")+
  theme_classic()+
  scale_fill_brewer(palette = 'Oranges')+
  scale_x_discrete(labels = c('IUCN', 'LPI' = 'LPD'))+
  ggtitle('Threat representation - fw amphibians')
ggsave('R_scripts+outputs/Fw_amph_threats2.svg')

#plot bar chart - reptiles
ggplot(Fw_rept_threats_percent2, aes(fill=Threatened,  x = Group, y = Percent))+
  geom_bar( stat="identity")+
  theme_classic()+
  scale_fill_brewer(palette = 'Oranges')+
  scale_x_discrete(labels = c('IUCN', 'LPI' = 'LPD'))+
  ggtitle('Threat representation - fw reptiles')
ggsave('R_scripts+outputs/Fw_rept_threats2.svg')

#Proportion tests
#bonf corrections
bonf_amph2 = 0.05/4 #0.0125
bonf_rept2 = 0.05/4 

#REPTILES
x1_rept = Fw_herp_threats2$Total_rept_species_IUCN
n1_rept = sum(Fw_herp_threats2$Total_rept_species_IUCN)
x2_rept = Fw_herp_threats2$Total_rept_species_LPI
n2_rept = sum(Fw_herp_threats2$Total_rept_species_LPI)

Prop_test_rept2 = data.frame('Threat' = Fw_herp_threats2$Threatened,
                             'X.squared' = NA,
                             'DF' = NA,
                            'P_value' = NA)
for (i in 1:4) {
  Prop_test_rept2$X.squared[i] <- prop.test(c(x1_rept[i], x2_rept[i]), c(n1_rept, n2_rept), alternative = "two.sided", conf.level = 0.95)$statistic 
  Prop_test_rept2$DF[i] <- prop.test(c(x1_rept[i], x2_rept[i]), c(n1_rept, n2_rept), alternative = "two.sided", conf.level = 0.95)$parameter 
  Prop_test_rept2$P_value[i] <- prop.test(c(x1_rept[i], x2_rept[i]), c(n1_rept, n2_rept), alternative = "two.sided", conf.level = 0.95)$p.value
}
#only significant for DD category
Fw_herp_threats2$Threatened
#broken down:
DD_rept = prop.test(c(x1_rept[1], x2_rept[1]), c(n1_rept, n2_rept), alternative = "two.sided", conf.level = 0.95)
Extinct_rept= prop.test(c(x1_rept[2], x2_rept[2]), c(n1_rept, n2_rept), alternative = "two.sided", conf.level = 0.95)
Notthreat_rept = prop.test(c(x1_rept[3], x2_rept[3]), c(n1_rept, n2_rept), alternative = "two.sided", conf.level = 0.95)
Threat_rept = prop.test(c(x1_rept[4], x2_rept[4]), c(n1_rept, n2_rept), alternative = "two.sided", conf.level = 0.95)

#AMPHIBIANS
x1_amph = Fw_herp_threats2$Total_amph_species_IUCN
n1_amph = sum(Fw_herp_threats2$Total_amph_species_IUCN)
x2_amph = Fw_herp_threats2$Total_amph_species_LPI
n2_amph = sum(Fw_herp_threats2$Total_amph_species_LPI)

Prop_test_amph2 = data.frame('Threat' = Fw_herp_threats2$Threatened,
                             'X.squared' = NA,
                             'DF' = NA,
                             'P_value' = NA)
for (i in 1:4) {
  Prop_test_amph2$X.squared[i] <- prop.test(c(x1_amph[i], x2_amph[i]), c(n1_amph, n2_amph), alternative = "two.sided", conf.level = 0.95)$statistic
  Prop_test_amph2$DF[i] <- prop.test(c(x1_amph[i], x2_amph[i]), c(n1_amph, n2_amph), alternative = "two.sided", conf.level = 0.95)$parameter
  Prop_test_amph2$P_value[i] <- prop.test(c(x1_amph[i], x2_amph[i]), c(n1_amph, n2_amph), alternative = "two.sided", conf.level = 0.95)$p.value
}
#all significant apart from extinct category

#broken down:
DD_amph = prop.test(c(x1_amph[1], x2_amph[1]), c(n1_amph, n2_amph), alternative = "two.sided", conf.level = 0.95)
Extinct_amph = prop.test(c(x1_amph[2], x2_amph[2]), c(n1_amph, n2_amph), alternative = "two.sided", conf.level = 0.95)
Notthreat_amph = prop.test(c(x1_amph[3], x2_amph[3]), c(n1_amph, n2_amph), alternative = "two.sided", conf.level = 0.95)
Threat_amph = prop.test(c(x1_amph[4], x2_amph[4]), c(n1_amph, n2_amph), alternative = "two.sided", conf.level = 0.95)