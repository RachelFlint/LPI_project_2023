#####################
#Mixed Model runs#### (see workingfile0 for draft)
#####################
install.packages('lme4')
install.packages('sjPlot')
install.packages('performance')
library(lme4)
library(tidyr)

#PREPARE DATA ######################################

#retrieve LPI trait data (edited versions)
LPI_fw_amph_traits = read.csv('R_scripts+outputs/LPI_fw_amph_traits_edited.csv') #284 points
LPI_fw_rept_traits = read.csv('R_scripts+outputs/LPI_fw_rept_traits_edited.csv') #67 points
#merge with LPI (with lambdas)
LPI_fw_amph_pop_mixed = merge(LPI_fw_amph_traits, LPI_fw_amph_pop, by.x = str_trim('IUCN.name'), by.y = str_trim('scientificName'), all.x = T, all.y = T)
LPI_fw_rept_pop_mixed = merge(LPI_fw_rept_traits, LPI_fw_rept_pop, by.x = str_trim('IUCN.name'), by.y = str_trim('scientificName'), all.x = T, all.y = T)

#add columns for logged body size
LPI_fw_amph_pop_mixed$log_body_size = log(LPI_fw_amph_pop_mixed$Body_length_mm, base = 10) #want base 10?
LPI_fw_rept_pop_mixed$log_body_size = log(LPI_fw_rept_pop_mixed$body_mass_g, base = 10) 

#merge with IUCN_amph_taxon to get taxonomic extra info (only for amphibians - missing some tax. info)
LPI_fw_amph_mixed_IUCN = merge(LPI_fw_amph_pop_mixed, IUCN_amph_taxon, by.x = 'IUCN.name', by.y = 'scientificName', all.x = T, all.y = F)
#(for reptiles, just use LPI family data as some not in IUCN)

#do simple plot
plot(x = LPI_fw_amph_mixed_IUCN$log_body_size, y = LPI_fw_amph_mixed_IUCN$Total_lambda) #no obvious trend

#need to include time series length in model###############
#extract ts length 
Amph_tslengths = read.delim('LPI_model_outputs/Fw_amphibian_pop_outputs/Fw_amph_pop_pops_Minmax.txt', sep = ',')
Amph_tslengths$TS_length = Amph_tslengths$max_year - Amph_tslengths$min_year

Rept_tslengths = read.delim('LPI_model_outputs/Fw_reptile_pop_outputs/Fw_rept_pop_pops_Minmax.txt', sep = ',')
Rept_tslengths$TS_length = Rept_tslengths$max_year - Rept_tslengths$min_year

#merge with datasets
LPI_fw_amph_pop_mixed = merge(LPI_fw_amph_mixed_IUCN, Amph_tslengths, by.x = 'ID', by.y = 'ID')
LPI_fw_rept_pop_mixed = merge(LPI_fw_rept_pop_mixed, Rept_tslengths, by.x = 'ID', by.y = 'ID')

#check levels in threat status data
levels(as.factor(LPI_fw_amph_pop_mixed$Threat_status))
levels(as.factor(LPI_fw_rept_pop_mixed$Threat_status))

#MAKE MODEL DATA - select essential columns, regroup threats, remove NA values#######################
#AMPHIBIANS
Amph_model_data = LPI_fw_amph_pop_mixed %>% #using IUCN family and genus categories
  dplyr::select(IUCN.name, Total_lambda, Mean_lambda, log_body_size, orderName, familyName, genusName, Location, Threat_status, TS_length, FW_realm, T_realm) %>%
  mutate(Threat_status = replace(Threat_status, Threat_status == 'NULL', 'Unknown'),
         Threat_status = replace(Threat_status, Threat_status == 'Unknown – no information', 'Unknown'),
         Threat_status = replace(Threat_status, Threat_status == 'Unknown (large data set)', 'Unknown'),
         Threat_status = replace(Threat_status, Threat_status == 'Unknown (no information)', 'Unknown'),
         Threat_status = replace(Threat_status, Threat_status == 'Threatened', 'Threats present'),
         Threat_status = replace(Threat_status, Threat_status == 'No threats', 'No threats present'))%>%
  tibble::add_column(Realm = NA) %>% #add empty realm column
  tidyr::drop_na(log_body_size) #reduced from 617 rows to 562, changed all threat status unknowns to same value
#merge realm columns
for (i in 1:length(Amph_model_data$IUCN.name)){
  if (Amph_model_data$FW_realm[i] == 'NULL') {Amph_model_data$Realm[i] = Amph_model_data$T_realm[i]}
  else {Amph_model_data$Realm[i] = Amph_model_data$FW_realm[i]}
}
Missing_amph_realms = filter(Amph_model_data, Amph_model_data$Realm=='NULL') 
#have 6 null values values for Neotropical realm, added below
for (i in 1:length(Amph_model_data$IUCN.name)){
  if (Amph_model_data$Realm[i] == 'NULL') {Amph_model_data$Realm[i] = 'Neotropical'}
}

#make model data into file
write.csv(Amph_model_data, 'R_scripts+outputs/Amph_model_data.csv')

#REPTILES
Rept_model_data = LPI_fw_rept_pop_mixed %>% #using LPI family and genus categories for reptiles
  dplyr::select(IUCN.name, Total_lambda, Mean_lambda, log_body_size, Order, Family, Genus, Location, Threat_status, TS_length, FW_realm, T_realm, M_realm) %>%
  mutate(Threat_status = replace(Threat_status, Threat_status == 'NULL', 'Unknown'),
         Threat_status = replace(Threat_status, Threat_status == 'Unknown – no information', 'Unknown'),
         Threat_status = replace(Threat_status, Threat_status == 'Unknown (large data set)', 'Unknown'),
         Threat_status = replace(Threat_status, Threat_status == 'Unknown (no information)', 'Unknown'),
         Threat_status = replace(Threat_status, Threat_status == 'Threatened', 'Threats present'),
         Threat_status = replace(Threat_status, Threat_status == 'No threats', 'No threats present'))%>%
  tibble::add_column(Realm = NA) %>% #add empty realm column
  tidyr::drop_na(log_body_size) #reduces from 211 rows to 203
#merge realm columns
for (i in 1:length(Rept_model_data$IUCN.name)){
  if (Rept_model_data$FW_realm[i] == 'NULL' & Rept_model_data$T_realm[i] == 'NULL') {Rept_model_data$Realm[i] = Rept_model_data$M_realm[i]}
  else if(Rept_model_data$T_realm[i] == 'NULL'& Rept_model_data$M_realm[i] == 'NULL') {Rept_model_data$Realm[i] = Rept_model_data$FW_realm[i]}
  else if (Rept_model_data$M_realm[i] == 'NULL' &Rept_model_data$FW_realm[i] == 'NULL'){Rept_model_data$Realm[i] = Rept_model_data$T_realm[i]}
}
sum(Rept_model_data$Realm=='NULL') #no Null values 

#make model data into file
write.csv(Rept_model_data, 'R_scripts+outputs/Rept_model_data.csv')


#RUN MODELS################################
#Summed lambdas - AMPHIBIANS###########

library(lme4)
library(sjPlot)
library(performance)

#NULL models
LPI_mixed_null = lmer(data = Amph_model_data, Total_lambda ~ 1 + 
                        (1|familyName) + (1|genusName) + (1|IUCN.name) + (1|Location) )
AIC(LPI_mixed_null) 

LPI_mixed_null2 = lmer(data = Amph_model_data, Total_lambda ~ 1 + 
                        (1|familyName/genusName) + (1|IUCN.name) + (1|Location))
AIC(LPI_mixed_null2) 

LPI_mixed_null3 = lmer(data = Amph_model_data, Total_lambda ~ 1 + 
                         (1|familyName) + (1|IUCN.name) + (1|Location))
AIC(LPI_mixed_null3) 

#Null model with TS_length  included

LPI_mixed_null4 = lmer(data = Amph_model_data, Total_lambda ~ 1 + 
                         (1|familyName) + (1|genusName) + (1|IUCN.name) + (1|Location) + (1|TS_length))
AIC(LPI_mixed_null4) 

#with IUCN.name nested - this makes most sense
LPI_mixed_null5 = lmer(data = Amph_model_data, Total_lambda ~ 1 + 
                         (1|familyName/genusName/IUCN.name) + (1|Location))
AIC(LPI_mixed_null5) 

#with IUCN nested in family only
LPI_mixed_null6 = lmer(data = Amph_model_data, Total_lambda ~ 1 + 
                         (1|familyName/IUCN.name) + (1|Location))

#with location nested in realm
LPI_mixed_null7 = lmer(data = Amph_model_data, Total_lambda ~ 1 + 
                         (1|familyName/genusName/IUCN.name) + (1|Realm/Location))

#just IUCN name
LPI_mixed_null8 = lmer(data = Amph_model_data, Total_lambda ~ 1 + 
                         (1|IUCN.name) + (1|Location))

LPI_mixed_null9 = lmer(data = Amph_model_data, Total_lambda ~ 1 + 
                         (1|IUCN.name) )



Amph_nulls = compare_performance(LPI_mixed_null, LPI_mixed_null2,LPI_mixed_null3,LPI_mixed_null4, LPI_mixed_null5, LPI_mixed_null6, LPI_mixed_null7, LPI_mixed_null8, LPI_mixed_null9) #LPI_mixed_null2 performs best (nested model)
#null, null2 and null5 get same AIC and weight- use null5 as most scientifically sound


#Try different mixed models (with null5 set up) ###################################
#1
LPI_mixed1 = lmer(data = Amph_model_data, Total_lambda ~ log_body_size + 
                    (1|familyName/genusName/IUCN.name) + (1|Location))

LPI_mixed2 = lmer(data = Amph_model_data, Total_lambda ~ TS_length + 
                    (1|familyName/genusName/IUCN.name) + (1|Location))

LPI_mixed3 = lmer(data = Amph_model_data, Total_lambda ~ Threat_status + 
                    (1|familyName/genusName/IUCN.name) + (1|Location))

LPI_mixed4 = lmer(data = Amph_model_data, Total_lambda ~ Realm + 
                    (1|familyName/genusName/IUCN.name) + (1|Location))
#compare performances
library(performance)
Amph_mixed1 = compare_performance(LPI_mixed1,LPI_mixed2,LPI_mixed3,LPI_mixed4, LPI_mixed_null5)
#threat status most powerful predictor

#TS_length + threat_status
LPI_mixed5= lmer(data = Amph_model_data, Total_lambda ~ Threat_status + TS_length  + 
                   (1|familyName/genusName/IUCN.name) + (1|Location))
Amph_mixed2 = compare_performance(LPI_mixed1,LPI_mixed2,LPI_mixed3,LPI_mixed4, LPI_mixed5, LPI_mixed_null5)
#TS-length improves model 

#include realm
LPI_mixed6= lmer(data = Amph_model_data, Total_lambda ~ Threat_status + TS_length  + Realm +
                   (1|familyName/genusName/IUCN.name) + (1|Location))
Amph_mixed3 = compare_performance(LPI_mixed1,LPI_mixed2,LPI_mixed3,LPI_mixed4, LPI_mixed5, LPI_mixed6, LPI_mixed_null5)
#realm makes model worse 

#include body size (interactive and additive)
LPI_mixed7= lmer(data = Amph_model_data, Total_lambda ~ log_body_size*Threat_status  + TS_length+
                   (1|familyName/genusName/IUCN.name) + (1|Location))


LPI_mixed8= lmer(data = Amph_model_data, Total_lambda ~ log_body_size + Threat_status  + TS_length+
                   (1|familyName/genusName/IUCN.name) + (1|Location))

#include taxonomic order
LPI_mixed9= lmer(data = Amph_model_data, Total_lambda ~  orderName+
                   (1|familyName/genusName/IUCN.name) + (1|Location))

LPI_mixed10= lmer(data = Amph_model_data, Total_lambda ~  orderName+ Threat_status+
                   (1|familyName/genusName/IUCN.name) + (1|Location))

LPI_mixed11= lmer(data = Amph_model_data, Total_lambda ~  orderName+ Threat_status+TS_length+
                    (1|familyName/genusName/IUCN.name) + (1|Location))

LPI_mixed12= lmer(data = Amph_model_data, Total_lambda ~  orderName+ TS_length+
                    (1|familyName/genusName/IUCN.name) + (1|Location))


Amph_mixed4 = compare_performance(LPI_mixed1,LPI_mixed2,LPI_mixed3,LPI_mixed4,LPI_mixed5,LPI_mixed6,LPI_mixed7,LPI_mixed8, LPI_mixed9,LPI_mixed10, LPI_mixed11,LPI_mixed12, LPI_mixed_null5)
#LPI_mixed5 performing best with AIC (weights) - 1299.6 (0.276)

 
#PLOT LPI_mixed5 #############################

#Model effects:
summary(LPI_mixed5)
Summary_stats = summary(LPI_mixed5)$coefficients
plot_model(LPI_mixed5, show.intercept = TRUE, show.data = TRUE)+
  theme_bw()+
  ylab('Effect on λtot')+
  xlab('Fixed effect')
ggsave('R_scipts+outputs/Amph_fixedeffects.png')


#shows info in summary(LPI_mixed5)

#check levels
levels(as.factor(Amph_model_data$Threat_status))
#Predictive model:
plot_model(LPI_mixed5, type = 'pred', terms = c( 'Threat_status','TS_length[2,10,20]'))+
  theme_sjplot2()+
  xlab('Threat status')+
  ylab('Total Lambda (λtot)')+
  labs(colour = 'Time Series Length (yrs)')

#extract data - shows how other values get fixed
TS2 = get_model_data(LPI_mixed5, type = 'pred', terms = c( 'Threat_status', 'TS_length[2]'))
TS10 = get_model_data(LPI_mixed5, type = 'pred', terms = c( 'Threat_status', 'TS_length[10]'))
TS20 = get_model_data(LPI_mixed5, type = 'pred', terms = c( 'Threat_status', 'TS_length[20]')) 

TSall = get_model_data(LPI_mixed5, type = 'pred', terms = c( 'Threat_status', 'TS_length[2,10,20]'))
#load in edited data 
Model_predicts = read.csv('R_scripts+outputs/Mixed model predictions.csv')

#check residuals
qqnorm(residuals(LPI_mixed5))
qqline(residuals(LPI_mixed5))
#not very normal

#PLOT USING GGPLOT
#with actual data 
ggplot()+
  geom_point(data = Amph_model_data, aes(x = TS_length, y = Total_lambda, colour = Threat_status))+
  theme_classic()+
  xlab('Time Series Length')+
  ylab('Total lambda (λtot)')+
  labs(colour = 'Threat Status')+
  scale_colour_manual(values=c("#56B4E9",  "darkred", "#999999"))+
  geom_abline(intercept = -0.02890033, slope = -0.01222089, colour = "#56B4E9")+
  geom_abline(intercept = -0.24464962, slope = -0.01222089, colour = "darkred")+
  geom_abline(intercept = 0.03086236, slope = -0.01222089, colour = "#999999")
ggsave('R_scipts+outputs/Mixed_model_wwithdata.svg')

#against body size (for reference)
ggplot()+
  geom_point(data = Amph_model_data, aes(x = log_body_size, y = Total_lambda, colour = Threat_status))+
  theme_classic()+
  theme(text = element_text(size = 13))+
  xlab('log(Body Size (mm)')+
  ylab('Total lambda (λtot)')+
  labs(colour = 'Threat Status')

#predictive plot - can't get values to dodge atm
#ggplot(data = Model_predicts, aes(x = Threat_status, y = predicted, colour = TS_length, group=TS_length))+
#  geom_point(aes(x= Threat_status, y = predicted))+
#  geom_errorbar(data = Model_predicts, aes(ymin=conf.low, ymax=conf.high), width=.2)

#try using effects package
install.packages('effects')
library(effects)
e <- allEffects(LPI_mixed5)
plot(e)


#Mixed models - REPTILES###############################################################################

#NULL models
LPI_mixed_null_rept = lmer(data = Rept_model_data, Total_lambda ~ 1 + 
                             (1|Family/Genus/IUCN.name))
AIC(LPI_mixed_null_rept)

#with nested
LPI_mixed_null_rept2 = lmer(data = Rept_model_data, Total_lambda ~ 1 + 
                              (1|Family/Genus/IUCN.name) + (1|Location) )
AIC(LPI_mixed_null_rept2) 

#just IUCN name
LPI_mixed_null_rept3 = lmer(data = Rept_model_data, Total_lambda ~ 1 + 
                              (1|IUCN.name) + (1|Location) + (1|Realm) )

LPI_mixed_null_rept4 = lmer(data = Rept_model_data, Total_lambda ~ 1 + 
                              (1|IUCN.name) + (1|Location))
LPI_mixed_null_rept5 = lmer(data = Rept_model_data, Total_lambda ~ 1 + 
                              (1|IUCN.name) )
LPI_mixed_null_rept6 = lmer(data = Rept_model_data, Total_lambda ~ 1 + 
                              (1|Family/IUCN.name) )
LPI_mixed_null_rept7 = lmer(data = Rept_model_data, Total_lambda ~ 1 + 
                              (1|Genus/IUCN.name) )

Rept_nulls = compare_performance(LPI_mixed_null_rept, LPI_mixed_null_rept2, LPI_mixed_null_rept3, LPI_mixed_null_rept4, LPI_mixed_null_rept5,LPI_mixed_null_rept6, LPI_mixed_null_rept7) 
#null_rept5 best

#mixed models
LPI_mixed_rept1 = lmer(data = Rept_model_data, Total_lambda ~ log_body_size + 
                    (1|IUCN.name) )

LPI_mixed_rept2 = lmer(data = Rept_model_data, Total_lambda ~ TS_length + 
                    (1|IUCN.name))

LPI_mixed_rept3 = lmer(data = Rept_model_data, Total_lambda ~ Threat_status + 
                    (1|IUCN.name))

LPI_mixed_rept4 = lmer(data = Rept_model_data, Total_lambda ~ Realm + 
                    (1|IUCN.name))

compare_performance(LPI_mixed_rept1,LPI_mixed_rept2, LPI_mixed_rept3, LPI_mixed_rept4, LPI_mixed_null_rept5)
#no improvement to null model

#include order
LPI_mixed_rept5 = lmer(data = Rept_model_data, Total_lambda ~ Order +
                         (1|IUCN.name))

#TS_length + threat_status
LPI_mixed_rept6 = lmer(data = Rept_model_data, Total_lambda ~ Threat_status + TS_length+
                         (1|IUCN.name))

#include body size (interactive and additive)
LPI_mixed_rept7= lmer(data = Rept_model_data, Total_lambda ~ log_body_size*Threat_status  + 
                        (1|IUCN.name))

LPI_mixed_rept8= lmer(data = Rept_model_data, Total_lambda ~ log_body_size + Threat_status  + 
                        (1|IUCN.name))

#order, threat status and ts length
LPI_mixed_rept9 = lmer(data = Rept_model_data, Total_lambda ~ Threat_status + TS_length+ Order +
                         (1|IUCN.name))


#compare performance
Rept_mixed = compare_performance(LPI_mixed_null_rept5, LPI_mixed_rept1,LPI_mixed_rept2,LPI_mixed_rept3,LPI_mixed_rept4,
                    LPI_mixed_rept5,LPI_mixed_rept6,LPI_mixed_rept7,LPI_mixed_rept8, LPI_mixed_rept9)
#LPI_mixed_rept5 (order as fixed effect) is best one

#Plot model effects:
summary(LPI_mixed_rept5)
Summary_stats2 = summary(LPI_mixed_rept5)$coefficients

plot_model(LPI_mixed_rept5, show.intercept = TRUE, show.data = TRUE)+
  theme_bw()+
  ylab('Effect on λtot')+
  xlab('Fixed effect')
ggsave('R_scipts+outputs/Reptile_fixedeffects.png')

  
  





