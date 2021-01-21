# Two plots

library(tidyverse)
library(ggplot2)

alldata.messy <-read_csv("AngKilCN.csv")
head(alldata.messy)

#First I need to go through the sample ID
# and add a column to separate the lakes by a unique ID.
alldata.messy[grep("ANG", alldata.messy$Sample_ID),"Lake"] <- "Angle"
alldata.messy[grep("KIL", alldata.messy$Sample_ID),"Lake"] <- "Killarney"

#Now I'm going to make two data sets, one for each lake.

AngleCN <- alldata.messy %>% 
  select(Sample_ID, Type_of_Material, d13C_VPDB, d15N_air, Lake) %>% 
  filter(Lake == "Angle")

##now I'm going to consolode these values into a mean and SD
summaryANG <-AngleCN %>% 
  group_by(Type_of_Material) %>% 
  summarize(aveC = mean(d13C_VPDB),
            aveN = mean(d15N_air),
            sdC = sd(d13C_VPDB),
            sdN = sd(d15N_air))

KilCN <- alldata.messy %>% 
  select(Sample_ID, Type_of_Material, d13C_VPDB, d15N_air, Lake) %>% 
  filter(Lake == "Killarney")

summaryKIL <-KilCN %>% 
  group_by(Type_of_Material) %>% 
  summarize(aveC = mean(d13C_VPDB),
            aveN = mean(d15N_air),
            sdC = sd(d13C_VPDB),
            sdN = sd(d15N_air))


#all samples only, basic plot
ggplot(data = AngleCN, aes(x = d13C_VPDB, 
                         y = d15N_air, 
                           color = Type_of_Material)) +
  geom_point() +
  theme_minimal() +
  labs(x = expression(paste(delta^{13}, "C (\u2030)")),
       y = expression(paste(delta^{15}, "N (\u2030)")))

ggplot(data = KilCN, aes(x = d13C_VPDB, y = d15N_air, color = Type_of_Material)) +
  geom_point() +
  theme_minimal() +
  labs(x = expression(paste(delta^{13}, "C (\u2030)")),
       y = expression(paste(delta^{15}, "N (\u2030)")))


