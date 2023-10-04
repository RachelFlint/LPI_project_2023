#1 - DATA ORGANISATION AND TAXONOMIC REPRESENTATION

#install packages and import libraries
install.packages("dplyr")
install.packages("ggplot2")
install.packages('tidyverse')
install.packages('taxize')


#########################
##LPI and IUCN datasets##
#########################
##KEY - load this library before doing anything
library(tidyverse)
library(dplyr)
#load LPI data####################################

#with no corrected lines and no added data
#LPI_herps_original = read.csv('LPI_herps_input_data/LPI_herps.csv') #1542 population entries
#length(unique(LPI_herps_original$Binomial)) #580 unique species

#WITH CORRECTED DATA and ADDED DATA POINTS (Red list ID corrected, NULL orders added)
LPI_herps = read.csv('LPI_herps_input_data/LPI_herps_final.csv') #1624 population entries
length(unique(LPI_herps$Binomial)) #619 unique species

#check how many don't have IUCN ids
LPI_herps_notIUCN = LPI_herps %>%
  filter(Red_List_taxon_ID == 'NULL') #6 terrestrial amphibians (not needed), 2 freshwater reptiles 

#IUCN data##########################################################

IUCN_amph_taxon = read.csv('IUCN_amphibians/Amphibian_taxonomy.csv') #taxonomic info
IUCN_amph_assess= read.csv('IUCN_amphibians/Amphibian_assessments.csv') #assessment info

IUCN_rept_taxon = read.csv('IUCN_reptiles/Reptile_taxonomy.csv')
IUCN_rept_assess= read.csv('IUCN_reptiles/Reptile_assessments.csv')   

IUCN_all_herps = rbind.data.frame(IUCN_amph_assess, IUCN_rept_assess)

###########################
##Full taxonomic datasets##
###########################

##wildfinder database## - reptiles and amphibians (old - 2017? from Louise)
Wildfinder_herps = read.csv('Wildfinder_data/Wildfinder_herptiles.csv')
#note - contains multiple (duplicate) species entries. Divided by biome

##amphibians database##
#April release - amphibiaweb github
Amphibiaweb_april23 = read.delim('Amphibiaweb_database/amphib_names_20230401.txt')
#only contains 8607 entries, website claims 8643

#website daily update release (accessed 15/05/23)
Amphibiaweb_may23 = read.delim('Amphibiaweb_database/amphib_names_daily.txt')
#contains 8643 entries

##reptile database##
#accessed 15/05/23, version April 2023
Reptiles_april23 = read.csv('Reptile_Database/reptile_checklist_2023_04.csv')
#contains 12000 entries

total_herps = length(Amphibiaweb_may23$species) + length(Reptiles_april23$Species) #20643 herps total

##########################################
##Taxonomic representation (all species)##
##########################################

#calculate numbers of UNIQUE species in LPI
LPI_unique = length(unique(LPI_herps$Binomial)) #619 unique species - was 580, I added 39 to dataset 
#proportions of unique species in LPI compared to total
LPI_taxon_rep = LPI_unique/total_herps #3.0% representation (from 2.8% before)

#proportion of unique species in IUCN compared to total
IUCN_unique = length(unique(IUCN_all_herps$scientificName)) #17708
IUCN_taxon_rep = IUCN_unique/total_herps #85.8% representation

################################
##Freshwater species in IUCN ##
################################

#IUCN systems as factors - check levels
IUCN_systems_amph = as.factor(IUCN_amph_assess$systems)
levels(IUCN_systems_amph) #3 levels: "Freshwater (=Inland waters)","Terrestrial", "Terrestrial|Freshwater (=Inland waters)"
IUCN_systems_rept = as.factor(IUCN_rept_assess$systems)
levels(IUCN_systems_rept) #7 levels: "Freshwater (=Inland waters)","Freshwater (=Inland waters)|Marine","Marine","Terrestrial","Terrestrial|Freshwater (=Inland waters)","Terrestrial|Freshwater (=Inland waters)|Marine","Terrestrial|Marine" 

#filter IUCN datasets to freshwater 
IUCN_freshwater_repts = IUCN_rept_assess %>%
  filter(systems %in% c("Freshwater (=Inland waters)",'Freshwater (=Inland waters)|Marine', "Terrestrial|Freshwater (=Inland waters)", "Terrestrial|Freshwater (=Inland waters)|Marine"))
IUCN_freshwater_amph = IUCN_amph_assess %>%
  filter(systems %in% c("Freshwater (=Inland waters)", "Terrestrial|Freshwater (=Inland waters)")) #4950 of 7486 IUCN amphibians are fw - 66%
#combine
IUCN_freshwater_herps = rbind.data.frame(IUCN_freshwater_amph, IUCN_freshwater_repts)  #5598 entries
#sense check
#IUCN_freshwater_check = IUCN_all_herps %>%
  #filter(systems %in% c("Freshwater (=Inland waters)",'Freshwater (=Inland waters)|Marine', 
                       # "Terrestrial|Freshwater (=Inland waters)", "Terrestrial|Freshwater (=Inland waters)|Marine")) #5598 entries
#5598 species of freshwater herps in IUCN list

#save dataframe 
write.csv(IUCN_freshwater_herps, 'R_scripts+outputs/IUCN_freshwater_herps.csv')

################################
##Freshwater species in LPI ####
################################
# use IUCN classification rather than LPI (more broad)

#merge: includes all LPI entries, not all IUCN
IUCN_LPI_corrected = merge(IUCN_all_herps, LPI_herps, by.x = str_trim('internalTaxonId'), by.y = str_trim('Red_List_taxon_ID'), all.x = F, all.y = T)
write.csv(IUCN_LPI_corrected, 'R_scripts+outputs/LPI_allherps(IUCN).csv')

#filter to just freshwater herps in LPI, including those listed as fw in LPI but not IUCN
IUCN_LPI_fw = IUCN_LPI_corrected%>%
  filter( (systems %in% c("Freshwater (=Inland waters)",'Freshwater (=Inland waters)|Marine', 
                                 "Terrestrial|Freshwater (=Inland waters)", 
                          "Terrestrial|Freshwater (=Inland waters)|Marine"))| 
            System %in% 'Freshwater') #895 entries in dataset

#Calculate how many unique species:
length(unique(str_trim(IUCN_LPI_fw$Binomial))) #351 unique - includes 2 reptiles not in IUCN 
length(unique(str_trim(IUCN_LPI_fw$scientificName))) #350 unique - 2 reptiles not in IUCN both classified as 'NA'

#save dataframe 
write.csv(IUCN_LPI_fw, 'R_scripts+outputs/LPI_freshwater(IUCN).csv')


###############################
#ORGANISE LPI fw data##
###############################
#Need LPI fw entries classified into reptiles and amphibians, and generate unique species entries (i.e. eliminate repeats)

library(tidyverse)
LPI_fw_herps = select(IUCN_LPI_fw, -c(4:23)) #now contains mainly LPI data, except first 3 columns

#Amphibians - all fw population data 
LPI_fw_amph = filter(LPI_fw_herps, LPI_fw_herps$Class == 'Amphibia') #643 population entries
Replicate_amphs = filter(LPI_fw_amph, LPI_fw_amph$Replicate == 1) #26 replicates

#unique species
length(unique(LPI_fw_amph$scientificName)) #284 unique IUCN species
length(unique(LPI_fw_amph$Binomial)) #284 unique LPI binomials

#Reptiles - all fw population data
LPI_fw_rept = filter(LPI_fw_herps, LPI_fw_herps$Class == 'Reptilia') #first column = Red list id, fourth = LPI id
write.csv(LPI_fw_herps, 'R_scripts+outputs/LPI_fw_herps.csv')
write.csv(LPI_fw_amph, 'R_scripts+outputs/LPI_fw_amph.csv')
write.csv(LPI_fw_rept, 'R_scripts+outputs/LPI_fw_rept.csv')

#add in 2 missing LPI fw reptiles to IUCN binomial column and load edited dataset:
LPI_fw_rept = read.csv('R_scripts+outputs/LPI_fw_rept_edited.csv') #252 population entries
Replicate_repts = filter(LPI_fw_rept, LPI_fw_rept$Replicate == 1) #26 replicates
#unique species
length(unique(LPI_fw_rept$scientificName)) #67 unique IUCN species
length(unique(LPI_fw_rept$Binomial)) #67 unique LPI binomials

#Pick out unique LPI (and IUCN) species names

#Amphibians
LPI_fw_amph_names = unique(LPI_fw_amph[c('Binomial', 'scientificName', 'Class', 'Order', 'Family', 'Genus', 'Species')]) #286 entries
colnames(LPI_fw_amph_names) = c('LPI.name', 'IUCN.name', 'Class', 'Order', 'Family', 'Genus', 'Species')
#edit amphibians in excel (didn't merge well due to missing data - i.e. NULL entries in LPI taxon info)
write.csv(LPI_fw_amph_names, 'R_scripts+outputs/LPI_fw_amph_names.csv')
#load edited dataset - removed replicates and added in taxonomic info. 
LPI_fw_amph_names = read.csv('R_scripts+outputs/LPI_fw_amph_names_edited.csv')

#Reptiles
LPI_fw_rept_names = unique(LPI_fw_rept[c('Binomial', 'scientificName', 'Class', 'Order', 'Family', 'Genus', 'Species')]) #67 species
colnames(LPI_fw_rept_names) = c('LPI.name', 'IUCN.name', 'Class', 'Order', 'Family', 'Genus', 'Species')
write.csv(LPI_fw_rept_names, 'R_scripts+outputs/LPI_fw_rept_names.csv')

#create datasets with no replicates
LPI_fw_rept_norep = LPI_fw_rept %>%
  filter(Replicate == 0) #211 entries
LPI_fw_amph_norep = LPI_fw_amph %>%
  filter(Replicate == 0) #617 entries
LPI_fw_herps_norep = LPI_fw_herps %>%
  filter(Replicate == 0) #828 total entries


####IUCN data - filter to those in LPI#########################

#Identify for IUCN datasets whether species are in fw LPD or not
#AMPHIBIANS: IUCN_amph_assess - 7486 entries - include all as some not fw under IUCN
for (i in 1:7486){
  if ((IUCN_amph_assess$scientificName[i] %in% LPI_fw_amph_names$IUCN.name) == T)
  {IUCN_amph_assess$InLPI[i] = T}
  else {IUCN_amph_assess$InLPI[i] = F}
}

#REPTILES: IUCN_rept_assess - 10222 entries
for (i in 1:10222 ){
  if ((IUCN_rept_assess$scientificName[i] %in% LPI_fw_rept_names$`IUCN name`) == T)
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

##########################################
####Taxonomic representation of fw species
#########################################
LPI_fw_rept_total = length(unique(LPI_fw_rept$Binomial))
LPI_fw_amph_total = length(unique(LPI_fw_amph$Binomial))

LPI_rept = LPI_herps%>%
  filter(Class == 'Reptilia')
LPI_rept_total = length(unique(LPI_rept$Binomial))

LPI_amph = LPI_herps%>%
  filter(Class == 'Amphibia')
LPI_amph_total = length(unique(LPI_amph$Binomial))

IUCN_fw_amph_total = length(unique(IUCN_freshwater_amph$scientificName))
IUCN_fw_rept_total = length(unique(IUCN_freshwater_repts$scientificName))
IUCN_amph_total =length(unique(IUCN_amph_assess$scientificName))
IUCN_rept_total =length(unique(IUCN_rept_assess$scientificName))

LPI_fw_amph_total/LPI_amph_total*100 #77.0%
IUCN_fw_amph_total/IUCN_amph_total*100 #66.1%

LPI_fw_rept_total/LPI_rept_total*100 #26.8%
IUCN_fw_rept_total/IUCN_rept_total*100 #6.3%

#Proportion tests
Prop_amph = prop.test(c(LPI_fw_amph_total, IUCN_fw_amph_total), c(LPI_amph_total, IUCN_amph_total)) #significant
Prop_rept = prop.test(c(LPI_fw_rept_total, IUCN_fw_rept_total), c(LPI_rept_total, IUCN_rept_total)) #significant

#Representation of fw species by order#####
#merge IUCN taxon info with fw filtered data
IUCN_freshwater_amph_merged = merge(IUCN_freshwater_amph, IUCN_amph_taxon, by.x = 'scientificName', by.y = 'scientificName', all.x = T, all.y = F)
IUCN_freshwater_rept_merged = merge(IUCN_freshwater_repts, IUCN_rept_taxon, by.x = 'scientificName', by.y = 'scientificName', all.x = T, all.y = F)

#filter to orders for IUCN
levels(as.factor(IUCN_freshwater_amph_merged$orderName)) #"ANURA"       "CAUDATA"     "GYMNOPHIONA"
Fw_Anura_IUCN = IUCN_freshwater_amph_merged %>%
  filter(orderName == 'ANURA')
Fw_Caudata_IUCN = IUCN_freshwater_amph_merged %>%
  filter(orderName == 'CAUDATA')
Fw_Gymno_IUCN = IUCN_freshwater_amph_merged %>%
  filter(orderName == 'GYMNOPHIONA')

levels(as.factor(IUCN_freshwater_rept_merged$orderName)) #"CROCODYLIA" "SQUAMATA"   "TESTUDINES"
Fw_Squamate_IUCN = IUCN_freshwater_rept_merged %>%
  filter(orderName == 'SQUAMATA')
Fw_Testu_IUCN = IUCN_freshwater_rept_merged %>%
  filter(orderName == 'TESTUDINES')
Fw_Croc_IUCN = IUCN_freshwater_rept_merged %>%
  filter(orderName == 'CROCODYLIA')

#filter to orders for LPI
Fw_Anura_LPI_names = LPI_fw_amph_names %>%
  filter(Order == 'Anura')
Fw_Caudata_LPI_names = LPI_fw_amph_names %>%
  filter(Order == 'Caudata')
Fw_Gymno_LPI_names = LPI_fw_amph_names %>%
  filter(Order == 'Gymnophiona') #none present
Fw_Squamate_LPI_names = LPI_fw_rept_names %>%
  filter(Order == 'Squamata')
Fw_Testu_LPI_names = LPI_fw_rept_names %>%
  filter(Order == 'Testudines')
Fw_Croc_LPI_names = LPI_fw_rept_names %>%
  filter(Order == 'Crocodylia')

#create table
Fw_herp_orders = data.frame('Order' = c('Anura', 'Caudata', 'Gymnophiona', 'Squamata', 'Testudines', 'Crocodylia'),
                            'IUCN_count' = c(length(Fw_Anura_IUCN$scientificName), length(Fw_Caudata_IUCN$scientificName),
                                             length(Fw_Gymno_IUCN$scientificName),
                                             length(Fw_Squamate_IUCN$scientificName), length(Fw_Testu_IUCN$scientificName),
                                             length(Fw_Croc_IUCN$scientificName)),
                            'LPI_count' = c(length(Fw_Anura_LPI_names$IUCN.name), length(Fw_Caudata_LPI_names$IUCN.name),
                                            length(Fw_Gymno_LPI_names$IUCN.name),
                                            length(Fw_Squamate_LPI_names$IUCN.name), length(Fw_Testu_LPI_names$IUCN.name),
                                            length(Fw_Croc_LPI_names$IUCN.name)),
                            'IUCN_prop' = NA,
                            'LPI_prop' = NA)
#add proportions

Fw_herp_orders$IUCN_prop[1] = Fw_herp_orders$IUCN_count[1]/4950
Fw_herp_orders$IUCN_prop[2] = Fw_herp_orders$IUCN_count[2]/4950
Fw_herp_orders$IUCN_prop[3] = Fw_herp_orders$IUCN_count[3]/4950
Fw_herp_orders$IUCN_prop[4] = Fw_herp_orders$IUCN_count[4]/648
Fw_herp_orders$IUCN_prop[5] = Fw_herp_orders$IUCN_count[5]/648
Fw_herp_orders$IUCN_prop[6] = Fw_herp_orders$IUCN_count[6]/648

Fw_herp_orders$LPI_prop[1] = Fw_herp_orders$LPI_count[1]/284
Fw_herp_orders$LPI_prop[2] = Fw_herp_orders$LPI_count[2]/284
Fw_herp_orders$LPI_prop[3] = Fw_herp_orders$LPI_count[3]/284
Fw_herp_orders$LPI_prop[4] = Fw_herp_orders$LPI_count[4]/67
Fw_herp_orders$LPI_prop[5] = Fw_herp_orders$LPI_count[5]/67
Fw_herp_orders$LPI_prop[6] = Fw_herp_orders$LPI_count[6]/67

#do chi sq test for given probabilities p
Amph_chisq = chisq.test(x = Fw_herp_orders$LPI_count[1:3], p = Fw_herp_orders$IUCN_prop[1:3]) #significant
Rept_chisq = chisq.test(x = Fw_herp_orders$LPI_count[4:6], p = Fw_herp_orders$IUCN_prop[4:6]) #significant

#Proportion tests
#bonf corrections
bonf_amph = 0.05/3 #0.0167
bonf_rept = 0.05/3 

#Amphibians
x1_amph = Fw_herp_orders$IUCN_count[1:3]
n1_amph = sum(Fw_herp_orders$IUCN_count[1:3])
x2_amph = Fw_herp_orders$LPI_count[1:3]
n2_amph = sum(Fw_herp_orders$LPI_count[1:3])

Prop_test_amph_orders = data.frame('Order' = Fw_herp_orders$Order[1:3],
                                   'X.squared' = NA,
                                   'DF' = NA,
                                   'P_value' = NA)
for (i in 1:3) {
  Prop_test_amph_orders$X.squared[i] <- prop.test(c(x1_amph[i], x2_amph[i]), c(n1_amph, n2_amph), alternative = "two.sided", conf.level = 0.95)$statistic 
  Prop_test_amph_orders$DF[i] <- prop.test(c(x1_amph[i], x2_amph[i]), c(n1_amph, n2_amph), alternative = "two.sided", conf.level = 0.95)$parameter 
  Prop_test_amph_orders$P_value[i] <- prop.test(c(x1_amph[i], x2_amph[i]), c(n1_amph, n2_amph), alternative = "two.sided", conf.level = 0.95)$p.value
}


#REPTILES
x1_rept = Fw_herp_orders$IUCN_count[4:6]
n1_rept = sum(Fw_herp_orders$IUCN_count[4:6])
x2_rept = Fw_herp_orders$LPI_count[4:6]
n2_rept = sum(Fw_herp_orders$LPI_count[4:6])

Prop_test_rept_orders = data.frame('Order' = Fw_herp_orders$Order[4:6],
                             'X.squared' = NA,
                             'DF' = NA,
                             'P_value' = NA)
for (i in 1:3) {
  Prop_test_rept_orders$X.squared[i] <- prop.test(c(x1_rept[i], x2_rept[i]), c(n1_rept, n2_rept), alternative = "two.sided", conf.level = 0.95)$statistic 
  Prop_test_rept_orders$DF[i] <- prop.test(c(x1_rept[i], x2_rept[i]), c(n1_rept, n2_rept), alternative = "two.sided", conf.level = 0.95)$parameter 
  Prop_test_rept_orders$P_value[i] <- prop.test(c(x1_rept[i], x2_rept[i]), c(n1_rept, n2_rept), alternative = "two.sided", conf.level = 0.95)$p.value
}

#do stacked bar charts########################################
Amph_orders = Fw_herp_orders[1:3,]
Rept_orders = Fw_herp_orders[4:6,]

#add percentages
Amph_orders$LPI_Percent = Amph_orders$LPI_prop*100
Amph_orders$IUCN_Percent = Amph_orders$IUCN_prop*100

Rept_orders$LPI_Percent = Rept_orders$LPI_prop*100
Rept_orders$IUCN_Percent = Rept_orders$IUCN_prop*100

#save as csv
write.csv(Fw_herp_orders, 'R_scripts+outputs/Fw_herp_orders.csv')

#reorganise
Amph_orders_edited = Amph_orders%>%
  dplyr::select(c(1,6,7))%>%
  pivot_longer(cols = 2:3, names_to = 'Group', values_to = 'Percentage')
Rept_orders_edited = Rept_orders%>%
  dplyr::select(c(1,6,7))%>%
  pivot_longer(cols = 2:3, names_to = 'Group', values_to = 'Percentage')

#PLOT
ggplot(Amph_orders_edited, aes(x = Group, y = Percentage, fill = Order))+
  geom_bar(stat="identity", colour = 'black')+
  theme_classic()+
  scale_x_discrete(labels=c('IUCN_Percent' = 'IUCN', 'LPI_Percent' = 'LPD'))
ggsave('R_scripts+outputs/Amph_orders.svg')

ggplot(Rept_orders_edited, aes(x = Group, y = Percentage, fill = Order))+
  geom_bar(stat="identity", colour = 'black')+
  scale_fill_brewer()+
  theme_classic()+
  scale_x_discrete(labels=c('IUCN_Percent' = 'IUCN', 'LPI_Percent' = 'LPD'))
ggsave('R_scripts+outputs/Rept_orders.svg')




  
