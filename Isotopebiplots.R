# This script takes preliminary C & N isotope data from Angle and Killarny lakes
# and plots them


### Libraries ####
library(tidyverse) # this package is for data wrangling and tidying
library(ggplot2) # this package is for plotting
library(patchwork) # this package is for making multipanel plots
library(RColorBrewer) #this package is for the 7 different categories


### Clean the data ####
alldata.messy <-read_csv("AngKilCN.csv")
head(alldata.messy)

#First I need to go through the sample ID
# and add a column to separate the lakes by a unique ID.
alldata.messy[grep("ANG", alldata.messy$Sample_ID),"Lake"] <- "Angle"
alldata.messy[grep("KIL", alldata.messy$Sample_ID),"Lake"] <- "Killarney"


# If we want polygons, we need a way to categorize the data. 
alldata.messy[grep("phytoplankton | periphyton | plants", alldata.messy$Type_of_Material),"TrophicGuild"] <- "PrimaryProducer"
alldata.messy[grep("insects | snail | zooplankton", alldata.messy$Type_of_Material),"TrophicGuild"] <- "PrimaryConsumer"
alldata.messy[grep("fish", alldata.messy$Type_of_Material),"TrophicGuild"] <- "Top Consumer"

#I'm going to make different data sets for each lake.

#Angle
AngleCN <- alldata.messy %>% 
  select(Sample_ID, Type_of_Material, d13C_VPDB, d15N_air, Lake, TrophicGuild) %>% 
  filter(Lake == "Angle")

#consolidate these values into a mean and SD
summaryANG <- AngleCN %>% 
  group_by(Type_of_Material) %>% 
  summarize(aveC = mean(d13C_VPDB),
            aveN = mean(d15N_air),
            sdC = sd(d13C_VPDB),
            sdN = sd(d15N_air))

# If we want polygons, we need a way to categorize the data. 
summaryANG[grep("phytoplankton | periphyton | plants", summaryANG$Type_of_Material),"TrophicGuild"] <- "PrimaryProducer"
summaryANG[grep("insects | snail | zooplankton", summaryANG$Type_of_Material),"TrophicGuild"] <- "PrimaryConsumer"
summaryANG[grep("fish", summaryANG$Type_of_Material),"TrophicGuild"] <- "Top Consumer"

#Killarney
KilCN <- alldata.messy %>% 
  select(Sample_ID, Type_of_Material, d13C_VPDB, d15N_air, Lake, TrophicGuild) %>% 
  filter(Lake == "Killarney")

summaryKIL <-KilCN %>% group_by(Type_of_Material) %>% 
  summarize(aveC = mean(d13C_VPDB),
            aveN = mean(d15N_air),
            sdC = sd(d13C_VPDB),
            sdN = sd(d15N_air))

summaryKIL[grep("phytoplankton | periphyton | plants", summaryKIL$Type_of_Material),"TrophicGuild"] <- "PrimaryProducer"
summaryKIL[grep("insects | snail | zooplankton", summaryKIL$Type_of_Material),"TrophicGuild"] <- "PrimaryConsumer"
summaryKIL[grep("fish", summaryKIL$Type_of_Material),"TrophicGuild"] <- "Top Consumer"

##Plots ####

# Extract a vector that is a convex hull of the points according to category.
pp<-AngleCN %>% filter(TrophicGuild == "PrimaryProducer")
AngChull.pp<-pp[chull(pp$d13C_VPDB, pp$d15N_air),]
pc<-AngleCN %>% filter(TrophicGuild == "PrimaryConsumer")
AngChull.pc<-pc[chull(pc$d13C_VPDB, pc$d15N_air),]
AngChull <- bind_rows(AngChull.pp,AngChull.pc)

# all individual samples, plotted with a convex hull around the values for 
# primary producers and primary consumers
A1<-ggplot(data = AngleCN, aes(x = d13C_VPDB, 
                           y = d15N_air, 
                           color = Type_of_Material)) +
  geom_point() +
  scale_color_brewer(palette = "Dark2") +
  geom_polygon(data=AngChull, 
               aes(x=d13C_VPDB, y=d15N_air, 
                   group = TrophicGuild,  
                   fill = TrophicGuild), 
               alpha = 0.2,
               linetype = "blank") +
  theme_minimal() +
  labs(x = expression(paste(delta^{13}, "C (\u2030)")),
       y = expression(paste(delta^{15}, "N (\u2030)")))


#Also, just a simple plot with means and sds for each type of material sampled.
A2<-ggplot(data = summaryANG, 
       aes(x = aveC, 
           y = aveN, 
           color = Type_of_Material)) +
  geom_point() +
  scale_color_brewer(palette = "Dark2") +
  geom_errorbar(aes(xmin=aveC-sdC, xmax=aveC+sdC), width=.2,
                position=position_dodge(0.05)) +
  geom_errorbar(aes( ymin=aveN-sdN, ymax=aveN+sdN), width=.2,
                position=position_dodge(0.05)) +
  theme_minimal() +
  labs(x = expression(paste(delta^{13}, "C (\u2030)")),
       y = expression(paste(delta^{15}, "N (\u2030)"))) +
  theme(legend.position="left")

A1<-A1 + guides(color = FALSE) + theme(legend.position="left")
A1 + A2 

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
  geom_polygon(aes(color = TrophicGuild, fill = TrophicGuild), alpha = .2) +
  geom_errorbar(aes(xmin=aveC-sdC, xmax=aveC+sdC), width=.2,
                position=position_dodge(0.05)) +
  geom_errorbar(aes( ymin=aveN-sdN, ymax=aveN+sdN), width=.2,
                position=position_dodge(0.05)) +
  theme_minimal() +
  labs(x = expression(paste(delta^{13}, "C (\u2030)")),
       y = expression(paste(delta^{15}, "N (\u2030)")))

pKsum<-ggplot(data = summaryKIL, 
       aes(x = aveC, 
           y = aveN, 
           color = Type_of_Material)) +
  geom_point() +
  geom_polygon(aes(color = TrophicGuild, fill = TrophicGuild), alpha = .2) +
  geom_errorbar(aes(xmin=aveC-sdC, xmax=aveC+sdC), width=.2,
                position=position_dodge(0.05)) +
  geom_errorbar(aes( ymin=aveN-sdN, ymax=aveN+sdN), width=.2,
                position=position_dodge(0.05)) +
  theme_minimal() +
  labs(x = expression(paste(delta^{13}, "C (\u2030)")),
       y = expression(paste(delta^{15}, "N (\u2030)")))

pA/pAsum #this stacks the Angle lakes plots
ggsave("AngleLake.png")

pK/pKsum #this stacks the Killarney lake plots
ggsave("KillarneyLake.png")



