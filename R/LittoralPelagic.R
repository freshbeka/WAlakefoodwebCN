## Gastropod vs zooplankton when available.

## Plan here, search for the lakes that have "end member data" and quickly calculate pelagic vs littoral reliance by the pumpkinseed.

library(readxl) #To read the excel file 
library(tidyverse) # To organize and plot
library(gridExtra) # used for grid.arrange (to call plots via a loop and plot as grid)
library(ggrepel) #for labeling species
library(patchwork) #for multi panel plot

# Review the sheet names in order to select the correct one.  
excel_sheets("data/WA_Lake_SI_Data2021_PRELIM.xlsx")

## read in the worksheet with the isotope data
SIdata <- read_excel("data/WA_Lake_SI_Data2021_PRELIM.xlsx", sheet = "SI_Data")
# Not read it yet is a sheet with lat-long ("Overview") and a summary of crayfish sightings ("Lake Summary")

head(SIdata) #check info.

# I want to give each sampling even a  unique identifier so I don't accidentally pool the wrong data. The easiest way is to create a combo of Lake and Year (probably).
SIdata_tidy <-SIdata %>% 
  unite(col="Lake_Year", c(Lake, Year), sep="_", remove = FALSE) %>% #add new column called Lake_Year
  filter(!is.na("Identity")) #remove the lake without ID info (Pine 2013)

##get the means for each identity
SImeans <- SIdata_tidy %>% 
  group_by(Lake_Year, Identity, Group) %>% #separate by lake and species and group
  dplyr::summarise(d13C_mean = mean(d13C), #obtain mean and SD for each lake/species
            d13C_sd = sd(d13C),
            d15N_mean = mean(d15N),
            d15N_sd = sd(d15N))

## Isolate lakes that have both gastropod and zooplankton values.
Gastronames <-SImeans %>% filter(Group == "Gastropod") %>% pull(Lake_Year)
Gastronames <- unique(Gastronames)
Gastronames<-as_tibble(Gastronames)

Zoopnames <-SImeans %>% filter(Group == "Zooplankton") %>% pull(Lake_Year)
Zoopnames <- unique(Zoopnames)
Zoopnames<-as_tibble(Zoopnames)

End.memberlakes<-inner_join(Gastronames, Zoopnames) %>% pull()

# For each lakes, I create a mixing model for pelagic vs littoral reliance by pumpkinseed.
#I'm only doing this for 
End.memberlakes

# calculate the coupling score for each fish ####
#create empty dataframe
df <- tibble(
  Lake_Year = character(),
  Identity = character(),
  littoral.reliance = numeric(),
  d13C_mean = numeric(),
  d15N_mean = numeric(),
  richness = numeric())

j<-2
i<-1

for (j in 1:length(End.memberlakes)){   
  data <- SImeans %>% filter(Lake_Year == End.memberlakes[j])
  
  for (i in 1:nrow(data %>% filter(Group == "Fish"))){   
    Cfish <- data %>% filter(Group == "Fish") %>% slice(i) %>% pull(d13C_mean) ## all fish
    Nfish <- data %>% filter(Group == "Fish") %>% slice(i) %>% pull(d15N_mean) ## all fish
    lit <-data %>% filter(Group == "Gastropod") %>% slice(i) %>% pull(d13C_mean) ## I determine littoral 
    pel <-data %>% filter(Group == "Zooplankton") %>% slice(i) %>% pull(d13C_mean) ## I determine pelagic
    a <-data %>% filter(Group == "Fish") %>% slice(i) %>% pull(Lake_Year)
    b <-data %>% filter(Group == "Fish") %>% slice(i) %>% pull(Identity)
    c <- (Cfish - pel)/(lit - pel) ##I calculate the reliance as if I didn't have end-member data.
    d <- data %>% filter(Group == "Fish") %>% nrow()
    df <- df %>% add_row(Lake_Year =a,
                         Identity = b,
                         littoral.reliance = c,
                         d13C_mean = Cfish,
                         d15N_mean = Nfish,
                         richness = d)
  }
  
}
warni

