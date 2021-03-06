# This script takes preliminary C & N isotope data from several washington lakes and plots it.

library(readxl) #To read the excel file 

## read in the worksheet with the isotope data
SIdata <- read_excel("data/WA_Lake_SI_Data2021_PRELIM.xlsx", sheet = "SI_Data")
# Not read it yet is a sheet with lat-long ("Overview") and a summary of crayfish sightings ("Lake Summary")

head(SIdata) #check info.



