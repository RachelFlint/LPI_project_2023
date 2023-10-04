
#################################################
#try adrienne's again
library('stringdist')
library('pbapply')
library('rredlist')
library('tidyverse')
Sys.setenv(IUCN_KEY = "9bb4facb6d23f48efbf424bb05c0c1ef1cf6f468393bc745d42179ac4aca5fee")
# now call the variable that you set
Sys.getenv("IUCN_KEY")
# if this returns the token key then you can delete the Sys.setenv line. This does not need to be in your code anymore,
# and you can save that as a R variable 
apikey <- Sys.getenv("IUCN_KEY")

#create SpeciesDF with only one column to start with, and first 10 values
Etard_2020_amph = Amph_traits$Best_guess_binomial
first10 = Etard_2020_amph[1:10]

test2 = Redlist_synonyms(SpeciesDF = data.frame('Etard_2020' = first10)) #doesn't work

#try subset of function 'FunctionToApply_RL
test3 = cbind(data.frame('Species' = 'Babina chapaensis'), data.frame(FunctionToApply_RL('Babina chapaensis')))
columns = colnames(test3) #extract column names
#works sort of!

#create empty dataframe
First10 <- data.frame(matrix(ncol = 9, nrow = 0))
colnames(First10) = columns

#try for loop
for (i in 1:length(first10)){
 First10[i,] = cbind(data.frame(first10[i]), data.frame(FunctionToApply_RL(first10[i])))
}
#works!

######WORKING CODE###########
library('stringdist')
library('pbapply')
library('rredlist')
library('tidyverse')
Sys.setenv(IUCN_KEY = "9bb4facb6d23f48efbf424bb05c0c1ef1cf6f468393bc745d42179ac4aca5fee")
# now call the variable that you set
Sys.getenv("IUCN_KEY")
# if this returns the token key then you can delete the Sys.setenv line. This does not need to be in your code anymore,
# and you can save that as a R variable 
apikey <- Sys.getenv("IUCN_KEY")
#nb - make sure FunctionToApply_RL is loaded to system

test4 = FunctionToApply_RL('Babina chapaensis')
test3 = cbind(data.frame('Species' = 'Babina chapaensis'), data.frame(FunctionToApply_RL('Babina chapaensis')))
columns = colnames(test3) #extract column names

#Amphibians
Amph_traits = read.csv('Amph_trait_data/Etard_2020_tax_corrected/Amphibians.csv') #6990 entries
Etard_2020_amph = Amph_traits$Best_guess_binomial

Amph_trait_synonymsv2 = data.frame(matrix(ncol = 9, nrow = 0))
colnames(Amph_trait_synonymsv2) = columns
for (i in 1:length(Etard_2020_amph)){
  Amph_trait_synonymsv2[i,] = cbind(data.frame(Etard_2020_amph[i]), data.frame(FunctionToApply_RL(Etard_2020_amph[i])))
}
write.csv(Amph_trait_synonymsv2, 'Amph_trait_data/Amph_traits_synonymsv2_SUCCESS.csv')

#Reptiles
#load vertebrate trait dataset (with LPI reptile names corrected)
Vert_traits = read.csv('Reptile_trait_data/meiri_et_al._2021_v3.csv') #edited only LPI rows - emys blandingii and crocodylus johnsoni in v3
Rept_traits = Vert_traits %>%
  filter(Class == 'Reptilia')
#11240 reptiles in trait dataset (more than IUCN) - only body mass data, not size
Meiri_2021_rept = Rept_traits$binomial_2020

Rept_trait_synonyms = data.frame(matrix(ncol = 9, nrow = 0))
colnames(Rept_trait_synonyms) = columns
for (i in 1:length(Meiri_2021_rept)){
  Rept_trait_synonyms[i,] = cbind(data.frame(Meiri_2021_rept[i]), data.frame(FunctionToApply_RL(Meiri_2021_rept[i])))
  print(i)
}
write.csv(Rept_trait_synonyms, 'Reptile_trait_data/Rept_traits_synonyms.csv')

