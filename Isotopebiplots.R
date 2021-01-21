# This script takes preliminary C & N isotope data from Angle and Killarny lakes
# and plots them

library(tidyverse) # this package is for data wrangling and tidying
library(ggplot2) # this package is for plotting
library(patchwork) # this package is for making multipanel plots

alldata.messy <-read_csv("AngKilCN.csv")
head(alldata.messy)

#First I need to go through the sample ID
# and add a column to separate the lakes by a unique ID.
alldata.messy[grep("ANG", alldata.messy$Sample_ID),"Lake"] <- "Angle"
alldata.messy[grep("KIL", alldata.messy$Sample_ID),"Lake"] <- "Killarney"

#I'm going to make different data sets for each lake.
##now I'm going to consolodate these values into a mean and SD


#Angle
AngleCN <- alldata.messy %>% 
  select(Sample_ID, Type_of_Material, d13C_VPDB, d15N_air, Lake) %>% 
  filter(Lake == "Angle")

summaryANG <-AngleCN %>% 
  group_by(Type_of_Material) %>% 
  summarize(aveC = mean(d13C_VPDB),
            aveN = mean(d15N_air),
            sdC = sd(d13C_VPDB),
            sdN = sd(d15N_air))

#Killarney
KilCN <- alldata.messy %>% 
  select(Sample_ID, Type_of_Material, d13C_VPDB, d15N_air, Lake) %>% 
  filter(Lake == "Killarney")

summaryKIL <-KilCN %>% 
  group_by(Type_of_Material) %>% 
  summarize(aveC = mean(d13C_VPDB),
            aveN = mean(d15N_air),
            sdC = sd(d13C_VPDB),
            sdN = sd(d15N_air))


#all individual samples, basic plots
# In my opinion these are too busy.
pA<-ggplot(data = AngleCN, aes(x = d13C_VPDB, 
                         y = d15N_air, 
                           color = Type_of_Material)) +
  geom_point() +
  theme_minimal() +
  labs(x = expression(paste(delta^{13}, "C (\u2030)")),
       y = expression(paste(delta^{15}, "N (\u2030)")))

pK<-ggplot(data = KilCN, aes(x = d13C_VPDB, y = d15N_air, color = Type_of_Material)) +
  geom_point() +
  theme_minimal() +
  labs(x = expression(paste(delta^{13}, "C (\u2030)")),
       y = expression(paste(delta^{15}, "N (\u2030)")))

##Summary plots
pAsum<-ggplot(data = summaryANG, 
       aes(x = aveC, 
           y = aveN, 
           color = Type_of_Material)) +
  geom_point() +
  theme_minimal() +
  labs(x = expression(paste(delta^{13}, "C (\u2030)")),
       y = expression(paste(delta^{15}, "N (\u2030)")))

pKsum<-ggplot(data = summaryKIL, 
       aes(x = aveC, 
           y = aveN, 
           color = Type_of_Material)) +
  geom_point() +
  theme_minimal() +
  labs(x = expression(paste(delta^{13}, "C (\u2030)")),
       y = expression(paste(delta^{15}, "N (\u2030)")))

pA/pAsum

pK/pKsum



