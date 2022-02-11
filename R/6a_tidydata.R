#This takes the excel data and changes it to longform

library(readxl) #To read the excel file 
library(tidyverse) # To organize and plot
library(janitor) # to clean names
library(broom) # to tidy output

# Review the sheet names in order to select the correct one.  
excel_sheets("data/data for stats tests 1.26.22_2_RRS.xlsx")

## read in the worksheet with the isotope data
original <- read_excel("data/data for stats tests 1.26.22_2_RRS.xlsx", 
                       sheet = "littoral sed vs organism tot As")

## OK, there is a lot happening on each sheet. I will need to parse out these groups, I think, into little tibbles
sed_per<- original %>% slice(2:9) %>% clean_names() %>%  
  mutate(total_as_in_littoral_sediment_mg_as_g = parse_number(total_as_in_littoral_sediment_mg_as_g)) %>% 
  mutate(total_as_in_periphyton_mg_as_g = parse_number(total_as_in_periphyton_mg_as_g))

sed_per_long<-sed_per %>% select(1:2,4) %>% pivot_longer(!x1, names_to = "habtiat", values_to = "AveAs")

##phyto
original2 <- read_excel("data/data for stats tests 1.26.22_2_RRS.xlsx",  
                        sheet = "littoral sed vs organism tot As",
                        skip = 11)

sed_phy<- original2 %>% slice(2:10) %>% clean_names() %>%  
  mutate(total_as_in_littoral_sediment_mg_as_g = parse_number(total_as_in_littoral_sediment_mg_as_g)) %>% 
  mutate(total_as_in_phytoplankton_mg_as_g = parse_number(total_as_in_phytoplankton_mg_as_g))

sed_phy_long<-sed_phy %>% select(1:2,4) %>% pivot_longer(!x1, names_to = "habtiat", values_to = "AveAs")

##phyto
original3 <- read_excel("data/data for stats tests 1.26.22_2_RRS.xlsx",  
                        sheet = "littoral sed vs organism tot As",
                        skip = 23)

sed_zoo<- original3 %>% slice(2:5) %>% clean_names() %>%  
  mutate(total_as_in_littoral_sediment_mg_as_g = parse_number(total_as_in_littoral_sediment_mg_as_g)) %>% 
  mutate(total_as_in_zooplankton_mg_as_g = parse_number(total_as_in_zooplankton_mg_as_g))

sed_zoo_long<-sed_zoo %>% select(1:2,4) %>% pivot_longer(!x1, names_to = "habtiat", values_to = "AveAs")

##snail
original4 <- read_excel("data/data for stats tests 1.26.22_2_RRS.xlsx",  
                        sheet = "littoral sed vs organism tot As",
                        skip = 30)

sed_snail<- original4 %>% slice(2:8) %>% clean_names() %>%  
  mutate(total_as_in_littoral_sediment_mg_as_g = parse_number(total_as_in_littoral_sediment_mg_as_g)) %>% 
  mutate(total_as_in_snail_mg_as_g = parse_number(total_as_in_snail_mg_as_g))

sed_snail_long<-sed_snail %>% select(1:2,4) %>% pivot_longer(!x1, names_to = "habtiat", values_to = "AveAs")

##sunfish
original5 <- read_excel("data/data for stats tests 1.26.22_2_RRS.xlsx",  
                        sheet = "littoral sed vs organism tot As",
                        skip = 40)

sed_sun<- original5 %>% slice(2:6) %>% clean_names() %>%  
  mutate(total_as_in_littoral_sediment_mg_as_g = parse_number(total_as_in_littoral_sediment_mg_as_g)) %>% 
  mutate(total_as_in_sunfish_mg_as_g = parse_number(total_as_in_sunfish_mg_as_g))

sed_sun_long<-sed_sun %>% select(1:2,4) %>% pivot_longer(!x1, names_to = "habtiat", values_to = "AveAs")

TotAs <-bind_rows(sed_per_long, sed_phy_long, sed_zoo_long, sed_snail_long, sed_sun_long) %>% 
  mutate(AveAs = round(AveAs, 1)) %>% distinct()

TotAs1 <- TotAs %>% mutate(habtiat = str_remove(habtiat, "total_as_in_"))
TotAs_long <- TotAs1 %>% mutate(habtiat = str_remove(habtiat, "_mg_as_g"))

TotAs_orgs_wide <- TotAs_long %>% pivot_wider(names_from = habtiat, values_from = AveAs)
write.csv(TotAs_orgs_wide, "data/As_orgs.csv")

