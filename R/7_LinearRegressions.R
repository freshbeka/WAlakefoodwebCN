## additional analysis for Erin.
## Using the reformatted  data emailed to beka april 8, 2022 from Erin.


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

#all organisms
long<-reformat %>% select(
                    littoral_sediment_totAs_mean,
                    periphyton_iAs_mean,
                    phytoplankton_iAs_mean,
                    zooplankton_iAs_mean,
                    snail_iAs_mean,
                    sunfish_iAs_mean) %>% 
  pivot_longer(periphyton_iAs_mean:sunfish_iAs_mean, names_to = "species", values_to = "organism_iAs")

Pump <- "#e66101"
  Blue <- "#2c7bb6"
    zoo <- "#4d4d4d" #"#bababa"  #"#b35806"
      snl <- "#9970ab" #"#4d4d4d"
        phyt <- "#66bd63"
          peri <- "#b8e186"
            chron <- "#d73027"
              macro <- "#de77ae"

ggplot(long, aes(x=littoral_sediment_totAs_mean, 
                 y = organism_iAs, 
                 color = species, 
                 fill = species)) +
  geom_point()+
  geom_smooth(method = "lm", 
              aes(group = species)) +
  theme_bw() +
  scale_color_manual(values=c(peri,
                              phyt,
                              snl,
                              Pump,
                              zoo))+
  scale_fill_manual(values=c(peri,
                             phyt,
                             snl,
                             Pump,
                             zoo))+
  labs(x = expression(paste(
    "Total As in sediment (",
    mu, g, "/", g,
    ")", sep="")),
    y = expression(paste(
      "% inorganic As (",
      mu, g, "/", g,
      ")", sep="")))

ggsave("figs/predict_iAs_all.png", width = 7.5, height = 4.5, units = "in" )

ggplot(long %>% filter(species != "periphyton_iAs_mean"), aes(x=littoral_sediment_totAs_mean , y = organism_iAs, color = species, fill = species)) +
  geom_point()+
  geom_smooth(method = "lm", 
              aes(group = species)) +
  theme_bw() +
  scale_color_manual(values=c(phyt,
                              snl,
                              Pump,
                              zoo))+
  scale_fill_manual(values=c(phyt,
                             snl,
                             Pump,
                             zoo))+
  labs(x = expression(paste(
    "Total As in sediment (",
    mu, g, "/", g,
    ")", sep="")),
       y = expression(paste(
         "% inorganic As (",
         mu, g, "/", g,
         ")", sep="")))

ggsave("figs/predict_iAs_noperi.png", width = 7.5, height = 4.5, units = "in" )
