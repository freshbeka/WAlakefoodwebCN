## additional analysis for Erin.
## Using the reformatted  data 


##First, read in packages that are needed
library(readxl) #To read the excel file 
library(tidyverse) # To organize and plot
library(janitor) # to clean names
library(broom) # to tidy output

# Review the sheet names in order to select the correct one.  
excel_sheets("data/TrophicTransfer_R_Reformatted_v2.xlsx")

## read in the worksheet with the isotope data
reformat <- read_excel("data/TrophicTransfer_R_Reformatted_v2.xlsx", #name and location of the data file
                       sheet = "Reformat", #name of the sheet with the data
                       na = "NA") #which expression is used in the datasheet to represent "NA"



## Total arsenic in sediment vs total arsenic in community ####
ggplot(reformat, aes(x=littoral_sediment_totAs_mean, y = periphyton_totAs_mean)) +
  geom_point()
sed_per_lm<-lm(periphyton_totAs_mean ~ littoral_sediment_totAs_mean, reformat)
summary(sed_per_lm)

ggplot(reformat, aes(x=littoral_sediment_totAs_mean, y = phytoplankton_totAs_mean)) +
  geom_point()
sed_phy_lm<-lm(phytoplankton_totAs_mean ~ littoral_sediment_totAs_mean, reformat)
summary(sed_phy_lm)

ggplot(reformat, aes(x=littoral_sediment_totAs_mean, y = zooplankton_totAs_mean)) +
  geom_point()
sed_zoo_lm<-lm(zooplankton_totAs_mean ~ littoral_sediment_totAs_mean, reformat)
summary(sed_zoo_lm)
tidy(sed_zoo_lm)

ggplot(reformat, aes(x=littoral_sediment_totAs_mean, y = snail_totAs_mean)) +
  geom_point()
sed_sn_lm<-lm(snail_totAs_mean ~ littoral_sediment_totAs_mean, reformat)
summary(sed_sn_lm)
tidy(sed_sn_lm)

ggplot(reformat, aes(x=littoral_sediment_totAs_mean, y = sunfish_totAs_mean)) +
  geom_point()
sed_sun_lm<-lm(sunfish_totAs_mean ~ littoral_sediment_totAs_mean, reformat)
summary(sed_sun_lm)
tidy(sed_sun_lm)

## Total arsenic in sediment vs inorganic arsenic in community ####

ggplot(reformat, aes(x=littoral_sediment_totAs_mean, y = periphyton_iAs_mean)) +
  geom_point()
ised_per_lm<-lm(periphyton_iAs_mean ~ littoral_sediment_totAs_mean, reformat)
summary(ised_per_lm)

ggplot(reformat, aes(x=littoral_sediment_totAs_mean, y = phytoplankton_iAs_mean)) +
  geom_point()
ised_phy_lm<-lm(phytoplankton_iAs_mean ~ littoral_sediment_totAs_mean, reformat)
summary(ised_phy_lm)

ggplot(reformat, aes(x=littoral_sediment_totAs_mean, y = zooplankton_iAs_mean)) +
  geom_point()
ised_zoo_lm<-lm(zooplankton_iAs_mean ~ littoral_sediment_totAs_mean, reformat)
summary(ised_zoo_lm)


ggplot(reformat, aes(x=littoral_sediment_totAs_mean, y = snail_iAs_mean)) +
  geom_point()
ised_sn_lm<-lm(snail_iAs_mean ~ littoral_sediment_totAs_mean, reformat)
summary(ised_sn_lm)


ggplot(reformat, aes(x=littoral_sediment_totAs_mean, y = sunfish_iAs_mean)) +
  geom_point()
ised_sun_lm<-lm(sunfish_iAs_mean ~ littoral_sediment_totAs_mean, reformat)
summary(ised_sun_lm)

