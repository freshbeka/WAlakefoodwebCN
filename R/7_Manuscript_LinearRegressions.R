## additional analysis for Erin.
## Using the reformatted  data emailed to beka april 8, 2022 from Erin.
## Update 4/30/2023, I verified this data is still correct and the data we are using in the manuscript.

##First, read in packages that are needed
library(readxl) #To read the excel file 
library(tidyverse) # To organize and plot
library(janitor) # to clean names
library(broom) # to tidy output

# Review the sheet names in order to select the correct one.  
excel_sheets("data/TrophicTransfer_R_Reformatted_v3.xlsx")

## read in the worksheet with the isotope data
reformat <- read_excel("data/TrophicTransfer_R_Reformatted_v3.xlsx", #name and location of the data file
                       sheet = "Reformat", #name of the sheet with the data
                       na = "NA") #which expression is used in the datasheet to represent "NA"


##11/22/2022 - If I need to do this again, I'd like to write a function where I put in the response variable and I append a dataframe or tibble that has the test statistics coming out of the "glance()" function in broom. Doing it manually (litterally cut/paste) today for time.

## oxicH2O_dissAs_mean vs Total arsenic in sediment vs total arsenic in community ####
ggplot(reformat, aes(x=littoral_sediment_totAs_mean, y = oxicH2O_dissAs_mean, color = Lake)) +
  geom_point()
sed_oxic_lm<-lm(oxicH2O_dissAs_mean ~ littoral_sediment_totAs_mean, reformat)
summary(sed_oxic_lm)
glance(sed_oxic_lm)


## Total arsenic in sediment vs total arsenic in community ####

## periphyton
ggplot(reformat, aes(x=littoral_sediment_totAs_mean, y = periphyton_totAs_mean, color = Lake)) +
  geom_point()
sed_per_lm<-lm(periphyton_totAs_mean ~ littoral_sediment_totAs_mean, reformat)
summary(sed_per_lm)
glance(sed_per_lm)

## phytoplankton 
ggplot(reformat, aes(x=littoral_sediment_totAs_mean, y = phytoplankton_totAs_mean, color = Lake)) +
  geom_point()
sed_phy_lm<-lm(phytoplankton_totAs_mean ~ littoral_sediment_totAs_mean, reformat)
glance(sed_phy_lm)

##snail ##
ggplot(reformat, aes(x=littoral_sediment_totAs_mean, y = snail_totAs_mean)) +
  geom_point()
sed_sn_lm<-lm(snail_totAs_mean ~ littoral_sediment_totAs_mean, reformat)
summary(sed_sn_lm)
glance(sed_sn_lm)

#Zoop ##

ggplot(reformat, aes(x=littoral_sediment_totAs_mean, y = zooplankton_totAs_mean)) +
  geom_point()
sed_zoo_lm<-lm(zooplankton_totAs_mean ~ littoral_sediment_totAs_mean, reformat)
summary(sed_zoo_lm)
glance(sed_zoo_lm)

#fish

ggplot(reformat, aes(x=littoral_sediment_totAs_mean, y = sunfish_totAs_mean)) +
  geom_point()
sed_sun_lm<-lm(sunfish_totAs_mean ~ littoral_sediment_totAs_mean, reformat)
summary(sed_sun_lm)
glance(sed_sun_lm)

## Total arsenic in sediment vs inorganic arsenic in community ####

ggplot(reformat, aes(x=littoral_sediment_totAs_mean, y = periphyton_iAs_mean)) +
  geom_point()+
  geom_smooth(method = "lm")
ised_per_lm<-lm(periphyton_iAs_mean ~ littoral_sediment_totAs_mean, reformat)
summary(ised_per_lm)

ggplot(reformat, aes(x=littoral_sediment_totAs_mean, y = phytoplankton_iAs_mean)) +
  geom_point()+
  geom_smooth(method = "lm")
ised_phy_lm<-lm(phytoplankton_iAs_mean ~ littoral_sediment_totAs_mean, reformat)
summary(ised_phy_lm)

ggplot(reformat, aes(x=littoral_sediment_totAs_mean, y = zooplankton_iAs_mean)) +
  geom_point()+
  geom_smooth(method = "lm")
ised_zoo_lm<-lm(zooplankton_iAs_mean ~ littoral_sediment_totAs_mean, reformat)
summary(ised_zoo_lm)


ggplot(reformat, aes(x=littoral_sediment_totAs_mean, y = snail_iAs_mean)) +
  geom_point()+
  geom_smooth(method = "lm")
ised_sn_lm<-lm(snail_iAs_mean ~ littoral_sediment_totAs_mean, reformat)
summary(ised_sn_lm)

ggplot(reformat, aes(x=littoral_sediment_totAs_mean, y = sunfish_iAs_mean)) +
  geom_point()+
  geom_smooth(method = "lm")
ised_sun_lm<-lm(sunfish_iAs_mean ~ littoral_sediment_totAs_mean, reformat)
summary(ised_sun_lm)

