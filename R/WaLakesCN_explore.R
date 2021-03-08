# This script takes preliminary C & N isotope data from several washington lakes and plots it.

library(readxl) #To read the excel file 
library(tidyverse) # to organize and plot

## read in the worksheet with the isotope data
SIdata <- read_excel("data/WA_Lake_SI_Data2021_PRELIM.xlsx", sheet = "SI_Data")
# Not read it yet is a sheet with lat-long ("Overview") and a summary of crayfish sightings ("Lake Summary")

head(SIdata) #check info.

## first, I want to understand how many lakes have multiple sampling events.

SIdata %>% group_by(Lake) %>% #group according to lake name
  summarise(years = n_distinct(Year)) %>% # tally up the number of years
  filter(years>1) # filter the years that greater than 0

#  Nine lakes have multiple sampling events, especially pine lake.
# I think I want to give each sampling even a  unique identifer  so I don't accidentally pool the wrong data.

