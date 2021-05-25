## Gastropod vs zooplankton when available.

## For beka, starting on Tuesday May 25 - these mixing models need to be separated into individual and lake. Two different scripits. This is because they look the same and then I thought i edited the individual one, but it was the group one. 

## Plan here, search for the lakes that have "end member data" and quickly calculate pelagic vs littoral reliance by the pumpkinseed. If this is the dirctoin we want to go, I'll need to be stratgic about getting end members for the remaining lakes that don't have zoops and snails.

library(tidyverse) # To organize and plot
library(ggrepel) #for labeling species
library(patchwork) #for multi-panel plot


# read in the tidy data format of Julian and Bothell combined
SIdata_subset <- read_csv("data/SI_subset.csv")


##get the means for each identity
SImeans <- SIdata_subset %>% 
  group_by(Lake_Year, Identity, Group) %>% #separate by lake and species and group
  dplyr::summarise(d13C_mean = mean(d13C), #obtain mean and SD for each lake/species
            d13C_sd = sd(d13C),
            d15N_mean = mean(d15N),
            d15N_sd = sd(d15N)) %>% ungroup()  ## Ungroup is key, otherwise the mixing model will not work because "select(i)" cannot distinguish rows

## Isolate lakes that have both gastropod and zooplankton values.
Gastronames <-SImeans %>% filter(Group == "Gastropod") %>% pull(Lake_Year) 
Gastronames <- unique(Gastronames)
Gastronames<-as_tibble(Gastronames)

##Which lakes have more than 1 gastropod or zoop?
#check duplicates 
SImeans %>% 
  filter(Group == "Gastropod") %>% 
  group_by(Lake_Year) %>% 
  summarize(gast = n())

Zoopnames <-SImeans %>% filter(Group == "Zooplankton") %>% pull(Lake_Year)
Zoopnames <- unique(Zoopnames)
Zoopnames<-as_tibble(Zoopnames)

End.memberlakes<-inner_join(Gastronames, Zoopnames) %>% pull()

# For each lakes, I create a mixing model for pelagic vs littoral reliance by pumpkinseed.
#I'm only doing this for 
End.memberlakes

#which lakes do I not have zoops and snails for?
SImeans %>% select(Lake_Year) %>% distinct() %>% filter(!(Lake_Year %in% End.memberlakes)) 
## I will need to estimate or use some other data for endmembers in these 6 lakes.




ggsave("figs/density_littoralreliance_lake.png",p2,  width = 10, height = 6, units = "in" )



