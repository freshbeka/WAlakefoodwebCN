# This script takes C & N isotope data from several Washington lowland lakes, organizes it, and plots it.

library(readxl) #To read the excel file 
library(tidyverse) # To organize and plot
library(gridExtra) # used for grid.arrange (to call plots via a loop and plot as grid)
library(ggrepel) #for labeling species

# Review the sheet names in order to select the correct one.  
excel_sheets("data/WA_Lake_SI_Data2021_PRELIM.xlsx")

## read in the worksheet with the isotope data
SIdata <- read_excel("data/WA_Lake_SI_Data2021_PRELIM.xlsx", sheet = "SI_Data")
# Not read it yet is a sheet with lat-long ("Overview") and a summary of crayfish sightings ("Lake Summary")

head(SIdata) #check info.

## first, I want to understand how many lakes have multiple sampling events.
SIdata %>% group_by(Lake) %>% #group according to lake name
  summarise(years = n_distinct(Year)) %>% # tally up the number of years
  filter(years>1) # filter the years that greater than 0

#  Nine lakes have multiple sampling events, especially pine lake.
# I want to give each sampling even a  unique identifier so I don't accidentally pool the wrong data. The easiest way is to create a combo of Lake and Year (probably).
SIdata_tidy <-SIdata %>% 
  unite(col="Lake_Year", c(Lake, Year), sep="_", remove = FALSE) %>% #add new column called Lake_Year
  filter(!is.na(Identity)) #remove the lake without ID info (Pine 2013)


#Now I want to view the groups and see what is in each group.
SIdata_tidy %>% distinct(Group) # great, this is helpful.

SIdata_tidy %>% group_by(Lake_Year,Group) %>% summarize(total= n())

#To quickly get a sense of each of the sampling events, I'm going to loop through each of the lakes and plot it.

#how many lakes in all
n_distinct(SIdata_tidy$Lake_Year)
#there are 40 lakes. That is too many for one grid. I'll do two grids of 20

p <- list() #create an empty list
start<-0 #create an index if I cant get all lakes  into one grid
for (i in 1:20){
  i <- i+start
  lake.name <-  unique(SIdata_tidy$Lake_Year)[i] #pull the relevant data
  onelake <- SIdata_tidy %>% filter(Lake_Year == lake.name)
  p[[i-start]]<-ggplot(data = onelake, aes(x = d13C, y = d15N, color = Group)) +
    geom_point() + 
    theme_minimal() +
    theme(axis.title = element_blank()) +
    ggtitle(lake.name)
}
do.call(grid.arrange,p)

p <- list() #create an empty list
start<-20 #use index to start at the 21st lake.
for (i in 1:20){
  i <- i+start
  lake.name <-  unique(SIdata_tidy$Lake_Year)[i] #pull the relevant data
  onelake <- SIdata_tidy %>% filter(Lake_Year == lake.name)
  p[[i-start]]<-ggplot(data = onelake, aes(x = d13C, y = d15N, color = Group)) +
    geom_point() + 
    theme_minimal() +
    theme(axis.title = element_blank()) +
    ggtitle(lake.name)
}
do.call(grid.arrange,p)

# Now I want to take the mean of each major group (fish species) for each lake."Identity" is the co,umn to summarize.

SImeans <- SIdata_tidy %>% 
  group_by(Lake_Year, Identity, Group) %>% #separate by lake and species and group
  summarise(d13C_mean = mean(d13C), #obtain mean and SD for each lake/species
            d13C_sd = sd(d13C),
            d15N_mean = mean(d15N),
            d15N_sd = sd(d15N))

##take a peek at a lake
  lake.name <-  unique(SImeans$Lake_Year)[3] #pull the relevant data
  onelake <- SImeans %>% filter(Lake_Year == lake.name) 
  ggplot(data = onelake, aes(x = d13C_mean, y = d15N_mean, color = Group, lable = Identity)) +
    geom_point() + 
    geom_text_repel(aes(label = Identity, color = 'white', size = 1),  show.legend = FALSE) +
    geom_errorbar(aes(xmin=d13C_mean-d13C_sd, xmax=d13C_mean+d13C_sd), width=.2,
                  position=position_dodge(0.05)) +
    geom_errorbar(aes( ymin=d15N_mean-d15N_sd, ymax=d15N_mean+d15N_sd), width=.2,
                  position=position_dodge(0.05)) +
    theme_minimal() +
    theme(axis.title = element_blank()) +
    ggtitle(lake.name)

#Can I ID diet for just pumpkin seed vs blue gill?
ggplot(data = onelake, aes(x = d13C_mean, y = d15N_mean, color = Group)) +
  geom_point() + 
  geom_point(data = SIdata_tidy %>% filter(Lake_Year == lake.name) %>% filter(Identity == "Lepomis gibbosus"), aes(x = d13C, y = d15N), color = "black") + 
  geom_point(data = SIdata_tidy %>% filter(Lake_Year == lake.name) %>% filter(Group == "Aquatic invertebrate"), aes(x = d13C, y = d15N), color = "gray") +
  geom_errorbar(aes(xmin=d13C_mean-d13C_sd, xmax=d13C_mean+d13C_sd), width=.2,
                  position=position_dodge(0.05)) +
  geom_errorbar(aes( ymin=d15N_mean-d15N_sd, ymax=d15N_mean+d15N_sd), width=.2,
                  position=position_dodge(0.05)) +
  theme_minimal() +
  theme(axis.title = element_blank()) +
  ggtitle(lake.name)
  
# For each lake I'm going to need to select prey items to complete simple mixing models.
# I could also select a mean and sd value and a monte carlo that includes uncertainty. 
# I want to ID what fish may be eating and look for regional patterns. First, I'm going to look at the more common species of fish.
  
SImeans %>% filter(Group == "Fish") %>% group_by(Identity) %>% tally() %>% arrange(-n)
# The most common species is pumpkinseed (Lepomis gibbosus), largemouth bass (Micropterus salmoides), and yellow perch (Perca flavescens)

# I'll start with pumpkinseed.
# Note, now that I look at the lakes, I see that when a sample is 1 or 2, it means that the same sample was run twice. I should not treat these as separate samples, rather they are getting at precision of the instruments used to measure C & N, and/or how well homogenized each sample is. I'll have to consider out how to deal with thes replicates.

##I think I need to look at more quantitative differences related to N and C. For example, how much more enriched in N are the pumpkinseed relative to the crayfish?






