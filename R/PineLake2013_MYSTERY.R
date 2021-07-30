#=== === === === === === === ===
# Script started by Rebekah Stiling Summer 2021, stilir@uw.edu
# This script takes all of the isotope data from several years of Pine lake sampling and plots it in order to guess what samples from 2013 are which species.
#=== === === === === === === ===


library(readxl) #To read the excel file 
library(tidyverse) # To organize and plot

# Review the sheet names in order to select the correct one.  
excel_sheets("data/WA_Lake_SI_Data2021.xlsx")

## read in the worksheet with the isotope data
original <- read_excel("data/WA_Lake_SI_Data2021.xlsx", sheet = "SI_Data")

#give it a new name so as to be able to reference the original
SIdata <- original 
head(SIdata) #check info.

SIdata_tidy <- SIdata %>% 
  unite(col="Lake_Year", c(Lake, Year), sep="_", remove = FALSE) #add new column called Lake_Year composed of the info in the lake and year columns.

##Now, I only want the Pine Lake data.
pine <-SIdata_tidy %>% filter(Lake == "Pine")

# Give the Pine_2013 NA category "Mystery"
pine %>% filter(Lake_Year == "Pine_2013") %>% filter(is.na(Identity))

## What I am going to do is plot each "GROUP" together, and maybe for the interim label "Identity" with numbers so I can assign the number 

#to plot the groups, they need to be renamed so they match. I presume it is fish and inverts that we want to try to ID.

pine$Group2.0 <- pine$Group
pine<-mutate(pine,  Group2.0 =  if_else(Group2.0 == "algal scrape", "Algae",
                                  if_else(Group2.0 == "invertebrates", "Aquatic invertebrate",
                                    if_else(Group2.0 == "fish muscle", "Fish",
                                      if_else(Group2.0 == "snail muscle", "Gastropod",
                                        if_else(Group2.0 == "crayfish muscle", "Crayfish",
                                          if_else(Group2.0 == "terrestrial plant", "Terrestrial plant",
                                            if_else(Group2.0 == "macrophyte", "Aquatic plant",
                                              Group2.0))))))))

#Now I filter for the categories that I know I want to plot, shrinking the data.
keeplist <- c("Aquatic invertebrate", "Crayfish", "Fish", "Gastropod", "Terrestrial plant")
SIdata<-SIdata %>% filter(!(Lake %in% lakelist)) 


ggplot(data = pine, aes(x = d13C, y = d15N, color = Lake_Year)) + 
  geom_point()+ 
  theme_bw() +
  facet_wrap(~ Group2.0)

ggplot(data = SIdata_tidy %>% filter(Lake_Year %in% set1), aes(x = d13C, y = d15N, color = Group)) +
  geom_point() + 
  theme_bw() +
  facet_wrap(~ Lake_Year)

