
#############################
###RUNNING THE LPI MODEL#####
############################

##Using the rlpi package - from github https://github.com/Zoological-Society-of-London/rlpi

#install and load necessary packages
install.packages("devtools")
# Install from main ZSL repository online
install_github("Zoological-Society-of-London/rlpi", dependencies=TRUE)


# Load libraries
library(devtools)
library(rlpi)
library(dplyr)

#see LPI example data script
#look at underlying data - may only want to start at year where you have sufficient data points. Can use summarise_lpi function for this - counts raw data, not interpolated/modelled data
#LPIMain function refers to name of columns - searches for relevant years

#run and plot LPIs with species level lambdas###################################
#remove replicates
LPI_fw_rept_norep = LPI_fw_rept %>%
  filter(Replicate == 0) #211 entries
LPI_fw_amph_norep = LPI_fw_amph %>%
  filter(Replicate == 0) #617 entries

#FW reptiles
index_vector = rep(TRUE, nrow(LPI_fw_rept))
infile_fw_rept <- create_infile(LPI_fw_rept_norep, index_vector=index_vector, name="Fw_rept") #creates Fw_rept_pops.txt and Fw_rept_infile.txt in main folder
Fw_rept_lpi <- LPIMain(infile = infile_fw_rept, basedir = 'LPI_model_outputs/Fw_reptile_outputs', REF_YEAR = 1970, PLOT_MAX = 2017, BOOT_STRAP_SIZE = 10000, VERBOSE=FALSE) #bootstrap shows 95% intervals - tend to select 10,000
# Remove NAs (trailing years with no data)
Fw_rept_lpi <- Fw_rept_lpi[complete.cases(Fw_rept_lpi), ]
# Plot the resulting index
ggplot_lpi(Fw_rept_lpi, title = "Fw_rept_lpi", xlims=c(1970, 2017), ylim=c(0, 2)) #only have data to 2017
#Number of species: 67  - as expected using LPI binomials -  need to adapt to fit with IUCN
ggsave('Freshwater_reptiles_LPI.png')
write.csv(Fw_rept_lpi, 'LPI_model_outputs/Fw_reptile_outputs/Fw_rept_lpi_results.csv')

#check summary
summarise_lpi(infile_fw_rept)
Summary_rept = read.table('Fw_rept_pops.txt.nsp_year.txt') #shows number of data point for each year

#FW amphibians
index_vector2 = rep(TRUE, nrow(LPI_fw_amph))
infile_fw_amph <- create_infile(LPI_fw_amph_norep, index_vector=index_vector2, name="Fw_amph")
617
Fw_amph_lpi <- Fw_amph_lpi[complete.cases(Fw_amph_lpi), ]
# Plot the resulting index
ggplot_lpi(Fw_amph_lpi, title = "Fw_amph_lpi", xlims=c(1970, 2017), ylim=c(0, 2)) #only have data to 2017
ggsave('Freshwater_amphibians_LPI.png')
write.csv(Fw_amph_lpi, 'LPI_model_outputs/Fw_amphibian_outputs/Fw_amph_lpi_results.csv')


#Number of species: 284
length(unique(LPI_fw_amph$Binomial)) #284 
#very narrow confidence intervals = trend is consistent
#can plot on same graph but not combined

#check summary 
summarise_lpi(infile_fw_amph) 
Summary_amph = read.table('Fw_amph_pops.txt.nsp_year.txt') 

#Plot above LPIs together############################

# combine the two LPIs together in a list
lpis <- list(Fw_rept_lpi, Fw_amph_lpi) 

# And plot them together 
ggplot_multi_lpi(lpis,names=c("Freshwater reptiles", "Freshwater amphibians"), xlims=c(1970, 2017), ylims=c(0, 2))+
  theme_classic()+
  xlab('Year')+
  labs(color='Taxonomic Group', fill = 'Taxonomic Group')
ggsave('R_scripts+outputs/Freshwater_herp_LPIs.png')


#GET LAMBDAS########################
#first, extract lambda files for species
Fw_rept_lambdas = read.csv('LPI_model_outputs/Fw_reptile_outputs/Fw_rept_pops_lambda.csv')
Fw_amph_lambdas = read.csv('LPI_model_outputs/Fw_amphibian_outputs/Fw_amph_pops_lambda.csv')

class(Fw_rept_lambdas$X1971) #numeric 

#Fw_rept_lambdas_sum = Fw_rept_lambdas%>%
#  group_by(Binomial)%>%
#  summarise(Total_lambda = rowSums(pick(where(is.numeric), -c(1:4)), na.rm = T))
#or
Fw_rept_lambdas_sum = Fw_rept_lambdas%>%
  group_by(Binomial)%>%
  summarise(Total_lambda = sum(c_across(5:50), na.rm = T)) #or use mutate to just add on a row

Fw_amph_lambdas_sum = Fw_amph_lambdas%>%
  group_by(Binomial)%>%
  summarise(Total_lambda = sum(c_across(5:50), na.rm = T))


#splitting reptiles into Orders #################################################
levels(as.factor(LPI_fw_rept$Order)) #3 orders

LPI_fw_crocs = LPI_fw_rept_norep %>%
  filter(Order == 'Crocodylia') #103
LPI_fw_testu = LPI_fw_rept_norep %>%
  filter(Order == 'Testudines') #80
LPI_fw_squa = LPI_fw_rept_norep %>%
  filter(Order == 'Squamata') #28

index_vector5 = rep(TRUE, nrow(LPI_fw_crocs))
infile_fw_crocs <- create_infile(LPI_fw_crocs, index_vector=index_vector5, name="Fw_crocs")
Fw_crocs_lpi <- LPIMain(infile = infile_fw_crocs, basedir = 'LPI_model_outputs/Fw_reptile_outputs/crocs', REF_YEAR = 1970, PLOT_MAX = 2017, BOOT_STRAP_SIZE = 10000, VERBOSE=FALSE)
# Remove NAs (trailing years with no data)
Fw_crocs_lpi <- Fw_crocs_lpi[complete.cases(Fw_crocs_lpi), ]


index_vector6 = rep(TRUE, nrow(LPI_fw_testu))
infile_fw_testu <- create_infile(LPI_fw_testu, index_vector=index_vector6, name="Fw_testu")
Fw_testu_lpi <- LPIMain(infile = infile_fw_testu, basedir = 'LPI_model_outputs/Fw_reptile_outputs/testudines', REF_YEAR = 1970, PLOT_MAX = 2017, BOOT_STRAP_SIZE = 10000, VERBOSE=FALSE)
# Remove NAs (trailing years with no data)
Fw_testu_lpi <- Fw_testu_lpi[complete.cases(Fw_testu_lpi), ]


index_vector7 = rep(TRUE, nrow(LPI_fw_squa))
infile_fw_squa <- create_infile(LPI_fw_squa, index_vector=index_vector7, name="Fw_squa")
Fw_squa_lpi <- LPIMain(infile = infile_fw_squa, basedir = 'LPI_model_outputs/Fw_reptile_outputs/squamates', REF_YEAR = 1970, PLOT_MAX = 2017, BOOT_STRAP_SIZE = 10000, VERBOSE=FALSE)
# Remove NAs (trailing years with no data)
Fw_squa_lpi <- Fw_squa_lpi[complete.cases(Fw_squa_lpi), ] #something very odd going on

#check summaries 
summarise_lpi(infile_fw_squa)
Summary_squa = read.table('Fw_squa_pops.txt.nsp_year.txt') #start from 1983

summarise_lpi(infile_fw_testu)
Summary_testu = read.table('Fw_testu_pops.txt.nsp_year.txt') #start from 1972

summarise_lpi(infile_fw_crocs)
Summary_crocs = read.table('Fw_crocs_pops.txt.nsp_year.txt') #start from 1971

#run edited versions
index_vector = rep(TRUE, nrow(LPI_fw_crocs))
infile_fw_crocs2 <- create_infile(LPI_fw_crocs, index_vector=index_vector, name="Fw_crocs2")
Fw_crocs_lpi <- LPIMain(infile = infile_fw_crocs2, basedir = 'LPI_model_outputs/Fw_reptile_outputs/crocs', REF_YEAR = 1971, PLOT_MAX = 2017, BOOT_STRAP_SIZE = 10000, VERBOSE=FALSE)
# Remove NAs (trailing years with no data)
Fw_crocs_lpi <- Fw_crocs_lpi[complete.cases(Fw_crocs_lpi), ]
write.csv(Fw_crocs_lpi, 'LPI_model_outputs/Fw_reptile_outputs/crocs/Fw_crocs_lpi_results.csv')

index_vector = rep(TRUE, nrow(LPI_fw_testu))
infile_fw_testu2 <- create_infile(LPI_fw_testu, index_vector=index_vector, name="Fw_testu2")
Fw_testu_lpi <- LPIMain(infile = infile_fw_testu2, basedir = 'LPI_model_outputs/Fw_reptile_outputs/testudines', REF_YEAR = 1972, PLOT_MAX = 2017, BOOT_STRAP_SIZE = 10000, VERBOSE=FALSE)
# Remove NAs (trailing years with no data)
Fw_testu_lpi <- Fw_testu_lpi[complete.cases(Fw_testu_lpi), ]
write.csv(Fw_testu_lpi, 'LPI_model_outputs/Fw_reptile_outputs/testudines/Fw_testu_lpi_results.csv')

index_vector = rep(TRUE, nrow(LPI_fw_squa))
infile_fw_squa2 <- create_infile(LPI_fw_squa, index_vector=index_vector, name="Fw_squa2")
Fw_squa_lpi <- LPIMain(infile = infile_fw_squa2, basedir = 'LPI_model_outputs/Fw_reptile_outputs/squamates', REF_YEAR = 1983, PLOT_MAX = 2017, BOOT_STRAP_SIZE = 10000, VERBOSE=FALSE)
# Remove NAs (trailing years with no data)
Fw_squa_lpi <- Fw_squa_lpi[complete.cases(Fw_squa_lpi), ] 
write.csv(Fw_squa_lpi, 'LPI_model_outputs/Fw_reptile_outputs/squamates/Fw_squa_lpi_results.csv')

# can combine the rept LPIs together in a list
rept_lpis <- list(Fw_crocs_lpi, Fw_squa_lpi, Fw_testu_lpi) 
# And plot them together 
ggplot_multi_lpi(rept_lpis,names=c("Crocodylians (17 sp)", "Squamates (20 sp)", 'Testudines (30 sp)'), xlims=c(1970, 2017), ylims=c(0, 5))+
  theme_classic()
ggsave('Freshwater_rept_LPIs_split_edited.png')


#split amphibians into orders####################################
levels(as.factor(LPI_fw_amph$Order)) #anura, caudata (edited NULL values)

LPI_fw_anura = LPI_fw_amph_norep %>%
  filter(Order == 'Anura')
LPI_fw_caudata = LPI_fw_amph_norep%>%
  filter(Order == 'Caudata')

index_vector8 = rep(TRUE, nrow(LPI_fw_anura))
infile_fw_anura <- create_infile(LPI_fw_anura, index_vector=index_vector8, name="Fw_anura")
Fw_anura_lpi <- LPIMain(infile = infile_fw_anura, basedir = 'LPI_model_outputs/Fw_amphibian_outputs/anurans', REF_YEAR = 1970, PLOT_MAX = 2017, BOOT_STRAP_SIZE = 10000, VERBOSE=FALSE)
# Remove NAs (trailing years with no data)
Fw_anura_lpi <- Fw_anura_lpi[complete.cases(Fw_anura_lpi), ] 
write.csv(Fw_anura_lpi, 'LPI_model_outputs/Fw_amphibian_outputs/anurans/Fw_anura_lpi_results.csv')

index_vector9 = rep(TRUE, nrow(LPI_fw_caudata))
infile_fw_caudata <- create_infile(LPI_fw_caudata, index_vector=index_vector9, name="Fw_caudata")
Fw_caudata_lpi <- LPIMain(infile = infile_fw_caudata, basedir = 'LPI_model_outputs/Fw_amphibian_outputs/caudata', REF_YEAR = 1970, PLOT_MAX = 2017, BOOT_STRAP_SIZE = 10000, VERBOSE=FALSE)
# Remove NAs (trailing years with no data)
Fw_caudata_lpi <- Fw_caudata_lpi[complete.cases(Fw_caudata_lpi), ] 
write.csv(Fw_caudata_lpi, 'LPI_model_outputs/Fw_amphibian_outputs/caudata/Fw_caudata_lpi_results.csv')

# combine the amph LPIs together in a list
amph_lpis <- list(Fw_anura_lpi, Fw_caudata_lpi) 
# And plot them together
ggplot_multi_lpi(amph_lpis,names=c("Anura (241 sp)", "Caudata (43 sp)"), xlims=c(1970, 2017), ylims=c(0, 2))+
 theme_classic()
ggsave('Freshwater_amph_LPIs_split.png')

##########################################
#### Population level lambdas################
########################################
# Load library
library(rlpi)
library(dplyr)
library(stringr)

# Append population IDs to binomials
LPI_fw_amph_pop = LPI_fw_amph_norep %>%
  mutate(Binomial = paste(str_trim(Binomial), ID, sep = '_')) #remove white space and add ID
write.csv(LPI_fw_amph_pop, 'R_scripts+outputs/LPI_fw_amph_pop.csv')

LPI_fw_rept_pop = LPI_fw_rept_norep %>%
  mutate(Binomial = paste(str_trim(Binomial), ID, sep = '_'))
write.csv(LPI_fw_rept_pop, 'R_scripts+outputs/LPI_fw_rept_pop.csv')

# run lpi to get population level lambdas
#FW amphibians
index_vector3 = rep(TRUE, nrow(LPI_fw_amph_pop))
infile_fw_amph_pop <- create_infile(LPI_fw_amph_pop, index_vector=index_vector3, name="Fw_amph_pop")
Fw_amph_lpi_pop <- LPIMain(infile = infile_fw_amph_pop, basedir = 'LPI_model_outputs/Fw_amphibian_pop_outputs', REF_YEAR = 1970, PLOT_MAX = 2017, BOOT_STRAP_SIZE = 10000, VERBOSE=FALSE)

#FW reptiles
index_vector4 = rep(TRUE, nrow(LPI_fw_rept_pop))
infile_fw_rept_pop <- create_infile(LPI_fw_rept_pop, index_vector=index_vector4, name="Fw_rept_pop")
Fw_rept_lpi_pop <- LPIMain(infile = infile_fw_rept_pop, basedir = 'LPI_model_outputs/Fw_reptile_pop_outputs', REF_YEAR = 1970, PLOT_MAX = 2017, BOOT_STRAP_SIZE = 10000, VERBOSE=FALSE)

#extract lambdas
Fw_rept_pop_lambdas = read.csv('LPI_model_outputs/Fw_reptile_pop_outputs/Fw_rept_pop_pops_lambda.csv')
Fw_amph_pop_lambdas = read.csv('LPI_model_outputs/Fw_amphibian_pop_outputs/Fw_amph_pop_pops_lambda.csv')

#Lambda sums################################
Fw_rept_pop_lambdas_sum = Fw_rept_pop_lambdas%>%
  group_by(Binomial)%>%
  summarise(Total_lambda = sum(c_across(5:50), na.rm = T)) #or use mutate to just add on a row

Fw_amph_pop_lambdas_sum = Fw_amph_pop_lambdas%>%
  group_by(Binomial)%>%
  summarise(Total_lambda = sum(c_across(5:50), na.rm = T))

#merge with lpi data
LPI_fw_amph_pop = merge(LPI_fw_amph_pop, Fw_amph_pop_lambdas_sum, by.x = 'Binomial', by.y = 'Binomial')
LPI_fw_rept_pop = merge(LPI_fw_rept_pop, Fw_rept_pop_lambdas_sum, by.x = 'Binomial', by.y = 'Binomial')

#Average lambdas############################
Fw_rept_pop_lambdas_mean = Fw_rept_pop_lambdas%>%
  group_by(Binomial)%>%
  summarise(Mean_lambda = mean(c_across(5:50), na.rm = T)) 

Fw_amph_pop_lambdas_mean = Fw_amph_pop_lambdas%>%
  group_by(Binomial)%>%
  summarise(Mean_lambda = mean(c_across(5:50), na.rm = T))

#merge with lpi data
LPI_fw_amph_pop = merge(LPI_fw_amph_pop, Fw_amph_pop_lambdas_mean, by.x = 'Binomial', by.y = 'Binomial')
LPI_fw_rept_pop = merge(LPI_fw_rept_pop, Fw_rept_pop_lambdas_mean, by.x = 'Binomial', by.y = 'Binomial')

write.csv(LPI_fw_amph_pop, 'R_scripts+outputs/LPI_fw_amph_pop_lambdas.csv')
write.csv(LPI_fw_rept_pop, 'R_scripts+outputs/LPI_fw_rept_pop_lambdas.csv')
