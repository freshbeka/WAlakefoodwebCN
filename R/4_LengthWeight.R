#=== === === === === === === ===
# Script started by Rebekah Stiling June 25 2021, stilir@uw.edu
# This script takes data from several Washington lowland lakes and looks at PKS Length Weight Ratio
# It turns out the data is a bit wonky, so the LWR are not trustworthy until further cleaning.
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

PKSlakes<- SIdata %>% filter(Identity == "Lepomis gibbosus")

PKSlakes<-PKSlakes %>% filter(!is.na(Weight))

PKSlakes <- PKSlakes %>% filter(!is.na(Length))
PKSlakes$Weight <-as.numeric(PKSlakes$Weight)

PKSlakes$LWR <- PKSlakes$Length/PKSlakes$Weight

PKSlakes %>% group_by(Lake) %>% summarise(meanLWR = mean(LWR), sdLWR = sd(LWR))



length.weight<-ggplot(data = PKSlakes, mapping = aes(x = Length, y = Weight, color = Lake)) +
  geom_point() + 
  theme_bw() + 
  labs(title = "PKS only")

length.weight 
## Hmm, some of these are not accurate. Maybe different units?

ggsave("figs/length.weight.wonky.png",
       length.weight,  
       width = 6, height = 5, units = "in" )
