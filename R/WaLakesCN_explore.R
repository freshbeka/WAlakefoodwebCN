# This script takes C & N isotope data from several Washington lowland lakes, organizes it, and plots it.

library(readxl) #To read the excel file 
library(tidyverse) # To organize and plot

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
  unite(col="Lake_Year", c(Lake, Year), sep="_", remove = FALSE) #create a new column called Lake_Year, based on the data from Lake & Year, separate with _, don't remove the previous Lake and Year Columns.

#Now I want to view the groups and see what is in each group.
SIdata_tidy %>% distinct(Group) # great, this is helpful.

SIdata_tidy %>% group_by(Lake_Year,Group) %>% summarize(total= n())


