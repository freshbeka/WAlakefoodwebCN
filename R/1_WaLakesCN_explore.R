#=== === === === === === === ===
# Script started by Rebekah Stiling Spring 2021, stilir@uw.edu
# This script takes C & N isotope data from several Washington lowland lakes, organizes it, and plots it.
# This script also subsets the lakes into just the lakes that will be used for a food web component of an arsenic study with Jim Gawal and Erin Hull.
#=== === === === === === === ===

library(readxl) #To read the excel file 
library(tidyverse) # To organize and plot

# Review the sheet names in order to select the correct one.  
excel_sheets("data/WA_Lake_SI_Data2021_PRELIM.xlsx")

## read in the worksheet with the isotope data
original <- read_excel("data/WA_Lake_SI_Data2021_PRELIM.xlsx", sheet = "SI_Data")

#give it a new name so as to be able to reference the original
SIdata <- original 
head(SIdata) #check info.

#changes per Julian: 
# 1a. combine adult and juvenile M_salmoides. To do this, I will remove the word "juv" and "adu" so that the Micropterus are all the same.

#First i archive the old list
SIdata$Identity_old <- SIdata$Identity  

SIdata$Identity <- str_replace(SIdata$Identity, "Micropterus salmoides juv", "Micropterus salmoides")
SIdata$Identity <- str_replace(SIdata$Identity, "Micropterus salmoides adu", "Micropterus salmoides")

# 1b. Five Mile needs to be Fivemile
SIdata$Lake <- str_replace(SIdata$Lake, "Five Mile", "Fivemile")

# 2. Remove Cresent, Cascade, Pleasant, Whatcom, Sunday, Fern, and Wynoochee)
lakelist <- c("Crescent", "Cascade", "Pleasant", "Whatcom", "Sunday", "Fern", "Wynoochee")
SIdata<-SIdata %>% filter(!(Lake %in% lakelist)) 

# 3. Remove Nerka, Clarkii, and P. oregonensis
fishlist <- c("Oncorhynchus nerka", "Oncorhynchus clarkii", "Ptychocheilus oregonensis")
SIdata<-SIdata %>% filter(!(Identity %in% fishlist)) 

# 4. Now I remove Frogs, Bread, bird
removelist <- c("Bread", "Bird", "Frog")
SIdata <-SIdata %>% filter(!(Group %in% removelist)) 

## How many lakes have multiple sampling events?
SIdata %>% group_by(Lake) %>% #group according to lake name
  summarise(years = n_distinct(Year)) %>% # tally up the number of years
  filter(years>1) # filter the years that greater than 0

#  ten lakes have multiple sampling events, especially pine lake.
# I want to give each sampling even a  unique identifier so I don't accidentally pool the wrong data. The easiest way is to create a combo of Lake and Year (probably).
SIdata_tidy <-SIdata %>% 
  unite(col="Lake_Year", c(Lake, Year), sep="_", remove = FALSE) %>% #add new column called Lake_Year
  filter(!is.na(Identity)) #remove the lake without ID info (Pine 2013)

#Now I want to view the groups and see what is in each group.
SIdata_tidy %>% distinct(Group) # great, this is helpful.

#To quickly get a sense of each of Julian's sampling events, I'm going to loop through each of the lakes and plot it.

#how many lakes in all
n_distinct(SIdata_tidy$Lake_Year)
#there are 33 lakes. That is too many for one grid. I'll do two grids of 16/17

set1<-unique(SIdata_tidy$Lake_Year)[1:16]
set2<-unique(SIdata_tidy$Lake_Year)[17:33]

##same idea, but with ggplot and facet wrap
p1<-ggplot(data = SIdata_tidy %>% filter(Lake_Year %in% set1), aes(x = d13C, y = d15N, color = Group)) +
  geom_point() + 
  theme_bw() +
  facet_wrap(~ Lake_Year)

p2<-ggplot(data = SIdata_tidy %>% filter(Lake_Year %in% set2), aes(x = d13C, y = d15N, color = Group)) +
  geom_point() + 
  theme_bw() +
  facet_wrap(~ Lake_Year)

ggsave("figs/Biplot1-16.png",p1,  width = 16, height = 9, units = "in" )
ggsave("figs/Biplot17-33.png",p2,  width = 16, height = 9, units = "in" )

## All of Julian's together
ggplot(data = SIdata_tidy, aes(x = d13C, y = d15N, color = Group)) +
  geom_point() + 
  theme_bw() +
  facet_wrap(~ Lake_Year, ncol = 7)

ggsave("figs/Biplot1-33.png",  width = 16, height = 9, units = "in" ) # It could be fun to play with this and learn to manipulate

SIdata_tidy 

write_csv(SIdata_tidy, "data/SI_tidy.csv")


## Now I read in the Bothell data, then combine the two datasets ####
# Review the sheet names in order to select the correct one.  
excel_sheets("data/UW_BothelSI.xlsx")

## read in the worksheet with the isotope data
bothell <- read_excel("data/UW_BothelSI.xlsx", sheet = "ForMerge")

# create a combo of Lake and Year 
bothell_lakeyear <-bothell %>% 
  unite(col="Lake_Year", c(Lake, Year), sep="_", remove = FALSE) %>% #add new column called Lake_Year
  filter(!is.na(Identity))
##Problem! the bothell data was not collected at the same sampling event! Grr.. These data are spread over mulitple years/months. I presume they want them all plotted together, but that is not a clean/tidy presentation of data.

#What I need to do, is give the true bothell_lake_year one column, so as to preserve that information, and then renam the off years because Lake_Year is my unique identifier for each lake for plotting.

bothell_lakeyear$old_Lake_Year <- bothell_lakeyear$Lake_Year

bothell_lakeyear$Lake_Year <-str_replace(bothell_lakeyear$Lake_Year, "Angle_2020", "Angle_2019")
bothell_lakeyear$Lake_Year <-str_replace(bothell_lakeyear$Lake_Year, "Angle_2018", "Angle_2019")
bothell_lakeyear$Lake_Year <-str_replace(bothell_lakeyear$Lake_Year, "Killarney_2020", "Killarney_2019")
bothell_lakeyear$Lake_Year <-str_replace(bothell_lakeyear$Lake_Year, "Killarney_2018", "Killarney_2019")

#Now I merge the two datasets

all.si.data <-bind_rows(bothell_lakeyear,SIdata_tidy)


# This subsets the list of lakes that will be used in the publication. One sampling event per lake. This includes Angle and Killarney, the most recent sampling events from Jim annd Erin

lakes4paper <- c("Angle_2019","Cottage_2009", "Desire_2014", "Fenwick_2014", "Fivemile_2014",
                 "Geneva_2014", "Killarney_2019","Langlois_2013","Martha_2012", "North_2014", 
                 "Padden_2012","Pine_2016", "Shoecraft_2013", "Silver_2014", 
                 "Star_2013","Steel_2009", "Trout_2009","Walsh_2012", "Wilderness_2014")

SIdata_subset <- all.si.data %>% filter((Lake_Year %in% lakes4paper)) 

# check the plots
ggplot(data = SIdata_subset, aes(x = d13C, y = d15N, color = Group)) +
  geom_point() + 
  theme_bw() +
  facet_wrap(~ Lake_Year)

# 

write_csv(SIdata_subset, "data/SI_subset.csv")


# END INITIAL WRANGLING/CLEANING #### 
