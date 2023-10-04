# 2 - LPD realm representation

########################################
###Realms - figuress + statistical tests#
########################################

#proportions of realms for IUCN freshwater reptiles and amphibians##########################

#AMPHIBIANS#
#find levels
#IUCN_realms_amph = as.factor(IUCN_freshwater_amph$realm) #doesn't include ones not classified as fw in IUCN
#levels(IUCN_realms_amph)

#number in each realm
#IUCN_fw_amph_realms = IUCN_freshwater_amph%>%
#  group_by(realm) %>%
#  summarise(Total_amph_species_IUCN = length(unique(scientificName)))
#write.csv(IUCN_fw_amph_realms, 'R_scripts+outputs/IUCN_fw_amph_realms.csv') #edit in excel to add duplicates for multiple regions and summarise

#LOAD FINALISED DATA
IUCN_fw_amph_realms = read.csv('R_scripts+outputs/IUCN_fw_amph_realms_edited.csv') #now has duplicates of species in regions, and NA ones removed
#add proportions + percentages - total includes multiple species occurrences
sum(IUCN_fw_amph_realms$Total_amph_species_IUCN) # = 5190 (rather than 4950 species)
IUCN_fw_amph_realms$proportion_fw_amph_IUCN = (IUCN_fw_amph_realms$Total_amph_species_IUCN/sum(IUCN_fw_amph_realms$Total_amph_species_IUCN))
IUCN_fw_amph_realms$percent_amph_IUCN =IUCN_fw_amph_realms$proportion_fw_amph_IUCN*100

#REPTILES#
#find levels
#IUCN_realms_rept = as.factor(IUCN_freshwater_repts$realm)
#levels(IUCN_realms_rept)

#IUCN_fw_repts_realms = IUCN_freshwater_repts%>%
#  group_by(realm) %>%
#  summarise(Total_rept_species_IUCN = length(unique(scientificName)))
#write.csv(IUCN_fw_repts_realms, 'R_scripts+outputs/IUCN_fw_repts_realms.csv')

#LOAD FINALISED DATA
IUCN_fw_repts_realms = read.csv('R_scripts+outputs/IUCN_fw_repts_realms_edited.csv') #has duplicates of species in multiple realms
#add proportions + percentages
sum(IUCN_fw_repts_realms$Total_rept_species_IUCN) #723 rather than 648
IUCN_fw_repts_realms$proportion_fw_repts_IUCN = (IUCN_fw_repts_realms$Total_rept_species_IUCN/sum(IUCN_fw_repts_realms$Total_rept_species_IUCN) )
IUCN_fw_repts_realms$percent_rept_IUCN =IUCN_fw_repts_realms$proportion_fw_repts_IUCN*100

#merge
IUCN_fw_herps_realms = merge(IUCN_fw_amph_realms, IUCN_fw_repts_realms, by.x = 'realm', by.y = 'realm',
                             all.x = T, all.y = T)
#NOTE: above don't include ones classified as fw in LPD and not IUCN, or non IUCN ones (4+2 species species)


#Realms in LPI freshwater data######################## - Using LPI occurerence realms 

#Amphibians#
#under 'terrestrial' categorisation
#LPI_fw_amph_realms1 = IUCN_LPI_fw %>%
#  filter(Class == 'Amphibia')%>% #filter to amphibians
#  filter(System == 'Terrestrial')%>%
#  group_by(T_realm) %>% 
#  summarise(Total_amph_species_LPI_terr = length(unique(scientificName))) #want unique species in each realm - some null values

#identify null values
LPI_fw_amph_realms1_NULL = IUCN_LPI_fw %>%
  filter(Class == 'Amphibia')%>% #filter to amphibians
  filter(System == 'Terrestrial')%>%
  filter(T_realm == 'NULL') #all neotropical species

LPI_fw_amph_realms1 = IUCN_LPI_fw %>%
  filter(Class == 'Amphibia')%>% #filter to amphibians
  filter(System == 'Terrestrial')%>%
  mutate(T_realm = replace(T_realm, T_realm == 'NULL', 'Neotropical'))%>% #change NULL values to Neotropical
  group_by(T_realm) %>% 
  summarise(Total_amph_species_LPI_terr = length(unique(scientificName))) #want unique species in each realm 
  
#under 'freshwater' categorisation
LPI_fw_amph_realms2 = IUCN_LPI_fw %>%
  filter(Class == 'Amphibia')%>% #filter to amphibians
  filter(System == 'Freshwater')%>%
  group_by(FW_realm) %>% 
  summarise(Total_amph_species_LPI_fw = length(unique(scientificName))) #want unique species in each realm
#combine in excel
write.csv(LPI_fw_amph_realms1, 'R_scripts+outputs/LPI_fw_amph_realms1.csv')
write.csv(LPI_fw_amph_realms2, 'R_scripts+outputs/LPI_fw_amph_realms2.csv')
#read in final
LPI_fw_amph_realms = read.csv('R_scripts+outputs/LPI_fw_amph_realms_edited.csv') # made oceanian = 0

#add proportions +percentage
sum(LPI_fw_amph_realms$Total_amph_species_LPI) #304 occurrences (species exist in multiple realms)
LPI_fw_amph_realms$proportion_amph_LPI = LPI_fw_amph_realms$Total_amph_species_LPI/sum(LPI_fw_amph_realms$Total_amph_species_LPI)
LPI_fw_amph_realms$percent_amph_LPI = LPI_fw_amph_realms$proportion_amph_LPI*100


#Reptiles#
#under 'terrestrial'
LPI_fw_rept_realms1 = IUCN_LPI_fw %>%
  filter(Class == 'Reptilia')%>% #filter to reptiles
  filter(System == 'Terrestrial') %>%
  group_by(T_realm)%>% 
  summarise(Total_rept_species_LPI = length(unique(scientificName)))#want unique species in each realm
#under 'freshwater'
LPI_fw_rept_realms2 = IUCN_LPI_fw %>%
  filter(Class == 'Reptilia')%>% #filter to reptiles
  filter(System == 'Freshwater') %>%
  group_by(FW_realm)%>% 
  summarise(Total_rept_species_LPI = length(unique(scientificName)))#want unique species in each realm
#under 'marine'
LPI_fw_rept_realms3 = IUCN_LPI_fw %>%
  filter(Class == 'Reptilia')%>% #filter to reptiles
  filter(System == 'Marine') %>%
  group_by(realm) #1 - Crocodylus acutus - Neotropical
#combine in excel
write.csv(LPI_fw_rept_realms1, 'R_scripts+outputs/LPI_fw_rept_realms1.csv')
write.csv(LPI_fw_rept_realms2, 'R_scripts+outputs/LPI_fw_rept_realms2.csv')
write.csv(LPI_fw_rept_realms3, 'R_scripts+outputs/LPI_fw_rept_realms3.csv')
#load final
LPI_fw_rept_realms = read.csv('R_scripts+outputs/LPI_fw_rept_realms_edited.csv') #made oceanian = 0

#add proportions +percentages
sum(LPI_fw_rept_realms$Total_rept_species_LPI) #77 occurrences (species exist in multiple realms)
LPI_fw_rept_realms$proportion_rept_LPI = LPI_fw_rept_realms$Total_rept_species_LPI/sum(LPI_fw_rept_realms$Total_rept_species_LPI)
LPI_fw_rept_realms$percent_rept_LPI = LPI_fw_rept_realms$proportion_rept_LPI*100

#merge
LPI_fw_herps_realms = merge(LPI_fw_amph_realms, LPI_fw_rept_realms, by.x = 'LPI_realm', by.y = 'LPI_realm',
                            all.x = T, all.y = T)

#ORGANISE DATA######################

#merge IUCN and LPI datasets
Fw_herps_realms = merge(IUCN_fw_herps_realms, LPI_fw_herps_realms, by.x = 'realm', by.y = 'LPI_realm',
                        all.x = T, all.y = T)
write.csv(Fw_herps_realms, 'R_scripts+outputs/Fw_herps_realms_LPI_IUCN.csv')

#separate to repts+amphs, select key columns, reorganise table
library(tidyverse)
Fw_amph_realms = Fw_herps_realms%>%
  dplyr::select(realm,  percent_amph_LPI, percent_amph_IUCN)%>%
  rename(LPI = percent_amph_LPI)%>%
  rename(IUCN = percent_amph_IUCN)%>%
  pivot_longer(cols=2:3, names_to = 'Group', values_to = 'Percent')

Fw_rept_realms = Fw_herps_realms%>%
  dplyr::select(realm,  percent_rept_LPI, percent_rept_IUCN)%>%
  rename(LPI = percent_rept_LPI)%>%
  rename(IUCN = percent_rept_IUCN)%>%
  pivot_longer(cols=2:3, names_to = 'Group', values_to = 'Percent')

#PLOT###########################

colours = c(IUCN = 'red4', LPI = 'lightblue')
#plot bar chart - reptiles
ggplot(Fw_rept_realms, aes(fill=Group, y = realm, x = Percent))+
  geom_bar(position="dodge", stat="identity", width = 0.8)+
  theme_classic()+
  ggtitle('Realm representation - fw reptiles')+
  scale_fill_manual(values = colours, labels = c('IUCN', 'LPI' = "LPD"))+
  ylab('Biogeographical realm')+
  xlab('Percentage (%)')+
  theme(axis.text = element_text(size = 10),
        text = element_text(size = 10),
        legend.text = element_text(size = 10))
ggsave('R_scripts+outputs/Fw_rept_realms.svg')

#plot bar chart - amphibians
ggplot(Fw_amph_realms, aes(fill=Group, y = realm, x = Percent))+
  geom_bar(position="dodge", stat="identity", width = 0.8)+
  theme_classic()+
  ggtitle('Realm representation - fw amphibians')+
  scale_fill_manual(values = colours, labels = c('IUCN', 'LPI' = "LPD"))+
  ylab('Biogeographical realm')+
  xlab('Percentage (%)')+
  theme(axis.text = element_text(size = 10),
        text = element_text(size = 10),
        legend.text = element_text(size = 10))
ggsave('R_scripts+outputs/Fw_amph_realms.svg')


#Proportion TESTS# - 

#prop.test() - for where n>30, 
#Two sample z-test comparing two proportions : prop.test(c(x1,x2), c(n1,n2)) where n is total samples

#REPTILES
#bonferroni correction - multiple pairwise tests increases chance of type 1 error (rejecting the null hypothesis)
FWER = 1-0.95^7 # 30% family wise error rate if alpha = 0.05, and doing 7 pairwise tests
bonf_correction_rept = 0.05/7 #0.00714 is now level of significance

x1_rept = Fw_herps_realms$Total_rept_species_IUCN
n1_rept = sum(Fw_herps_realms$Total_rept_species_IUCN)
x2_rept = Fw_herps_realms$Total_rept_species_LPI
n2_rept = sum(Fw_herps_realms$Total_rept_species_LPI)

Prop_test_rept = data.frame('Realm' = Fw_herps_realms$realm,
                            'X.squared' = NA,
                            'DF' = NA,
                            'P_value' = NA)
for (i in 1:7) {
  Prop_test_rept$X.squared[i] <- prop.test(c(x1_rept[i], x2_rept[i]), c(n1_rept, n2_rept), alternative = "two.sided", conf.level = 0.95)$statistic 
  Prop_test_rept$DF[i] <- prop.test(c(x1_rept[i], x2_rept[i]), c(n1_rept, n2_rept), alternative = "two.sided", conf.level = 0.95)$parameter 
  Prop_test_rept$P_value[i] <- prop.test(c(x1_rept[i], x2_rept[i]), c(n1_rept, n2_rept), alternative = "two.sided", conf.level = 0.95)$p.value
}

#broken down  - 
Afrotropical_rept = prop.test(c(x1_rept[1], x2_rept[1]), c(n1_rept, n2_rept), alternative = "two.sided", conf.level = 0.95)
Australasian_rept = prop.test(c(x1_rept[2], x2_rept[2]), c(n1_rept, n2_rept), alternative = "two.sided", conf.level = 0.95)
Indomalayan_rept = prop.test(c(x1_rept[3], x2_rept[3]), c(n1_rept, n2_rept), alternative = "two.sided", conf.level = 0.95)
Nearctic_rept = prop.test(c(x1_rept[4], x2_rept[4]), c(n1_rept, n2_rept), alternative = "two.sided", conf.level = 0.95)
Neotropical_rept = prop.test(c(x1_rept[5], x2_rept[5]), c(n1_rept, n2_rept), alternative = "two.sided", conf.level = 0.95)
Oceanian_rept = prop.test(c(x1_rept[6], x2_rept[6]), c(n1_rept, n2_rept), alternative = "two.sided", conf.level = 0.95) #issue - not enough data
Palearctic_rept = prop.test(c(x1_rept[7], x2_rept[7]), c(n1_rept, n2_rept), alternative = "two.sided", conf.level = 0.95)

#redo for loop with 1 sample test of proportion
Fw_herps_realms$realm #"Afrotropical" "Australasian" "Indomalayan"  "Nearctic"     "Neotropical"  "Oceanian"     "Palearctic" 
x <-  Fw_herps_realms$Total_rept_species_LPI #number of successes
p <- Fw_herps_realms$proportion_fw_repts_IUCN #probability of success
n <- sum(Fw_herps_realms$Total_rept_species_LPI, na.rm = T) #number of trials
Prop_test_rept2 = data.frame('Realm' = Fw_herps_realms$realm,
                            'P_value' = NA)
for (i in 1:7) {
  Prop_test_rept2$P_value[i] <- prop.test(x[i], n, p[i], alternative = "two.sided", conf.level = 0.95)$p.value
} #get similar results

#overall chisq test
Rept_realm_chisq = chisq.test(x = Fw_herps_realms$Total_rept_species_LPI, p = Fw_herps_realms$proportion_fw_repts_IUCN)


#AMPHIBIANS#############################################################
#bonferroni correction
bonf_correction_amph = 0.05/7 #0.00714 is now level of significance

#using prop.test() with 2 sample test of proportion
x1_amph = Fw_herps_realms$Total_amph_species_IUCN
n1_amph = sum(Fw_herps_realms$Total_amph_species_IUCN)
x2_amph = Fw_herps_realms$Total_amph_species_LPI
n2_amph = sum(Fw_herps_realms$Total_amph_species_LPI)

Prop_test_amph = data.frame('Realm' = Fw_herps_realms$realm,
                            'X.squared' = NA,
                            'DF' = NA,
                            'P_value' = NA)
for (i in 1:7) {
  Prop_test_amph$X.squared[i] <- prop.test(c(x1_amph[i], x2_amph[i]), c(n1_amph, n2_amph), alternative = "two.sided", conf.level = 0.95)$statistic
  Prop_test_amph$DF[i] <- prop.test(c(x1_amph[i], x2_amph[i]), c(n1_amph, n2_amph), alternative = "two.sided", conf.level = 0.95)$parameter
  Prop_test_amph$P_value[i] <- prop.test(c(x1_amph[i], x2_amph[i]), c(n1_amph, n2_amph), alternative = "two.sided", conf.level = 0.95)$p.value
}


Afrotropical_amph = prop.test(c(x1_amph[1], x2_amph[1]), c(n1_amph, n2_amph), alternative = "two.sided", conf.level = 0.95)
Australasian_amph = prop.test(c(x1_amph[2], x2_amph[2]), c(n1_amph, n2_amph), alternative = "two.sided", conf.level = 0.95)
Indomalayan_amph = prop.test(c(x1_amph[3], x2_amph[3]), c(n1_amph, n2_amph), alternative = "two.sided", conf.level = 0.95)
Nearctic_amph = prop.test(c(x1_amph[4], x2_amph[4]), c(n1_amph, n2_amph), alternative = "two.sided", conf.level = 0.95)
Neotropical_amph = prop.test(c(x1_amph[5], x2_amph[5]), c(n1_amph, n2_amph), alternative = "two.sided", conf.level = 0.95)
Oceanian_amph = prop.test(c(x1_amph[6], x2_amph[6]), c(n1_amph, n2_amph), alternative = "two.sided", conf.level = 0.95)
Palearctic_amph = prop.test(c(x1_amph[7], x2_amph[7]), c(n1_amph, n2_amph), alternative = "two.sided", conf.level = 0.95)

#overall chisq test
Amph_realm_chisq = chisq.test(x = Fw_herps_realms$Total_amph_species_LPI, p = Fw_herps_realms$proportion_fw_amph_IUCN)


