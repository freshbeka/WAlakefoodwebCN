# This script takes the trophic transfer results and plots them with isotopes


library(googledrive) #to read in the data directly from googledrive
library(readxl) #to convert the excel file into a R object
library(janitor) # to clean up rownames
library(tidyverse) #for organizing and plotting

##First, call the file from the google drive and save it in the data file location
drive_download("https://docs.google.com/spreadsheets/d/1qe0RBzG0_HZdpPLTUukH7BOrrH4Qblte/edit#gid=890212752", 
               path = "/Users/rebekahstiling/Desktop/RProjects/WAlakefoodwebCN/data/Trophic2.xlsx",
               overwrite = TRUE)

## The file was saved locally to my machine, now I want to read it into RStudio 

#check the sheets
excel_sheets("data/Trophic2.xlsx")

trophic_messy <- read_excel("data/Trophic2.xlsx", 
                            sheet = "results", 
                            skip = 2,
                            .name_repair = make_clean_names)

#Now I want to separate out the pieces of data that I know I need, and clean up the tibble

#First I want to consolidate the values that have C & N

isotopes_messy <- trophic_messy %>% drop_na(uc_davis_id)

#some rows have the column names repeated, so remove them
isotopes_messy <- isotopes_messy %>% filter(species != "Species")

#change some columns from character to numeric
cols.num <- c("fillet_as", "liver_as", "gill_as","d13cvpdb","d15n_air")
isotopes_messy[cols.num] <- sapply(isotopes_messy[cols.num],as.numeric)

##Lake info

isotopes_messy<-mutate(isotopes_messy, lake = if_else(lake == "Angle", "Angle Lake", lake))

isotopes_messy<-mutate(isotopes_messy, lake = if_else(lake == "Killarney", "Lake Killarney", lake))

##Change CMS to snail for ASLO
isotopes_messy<-mutate(isotopes_messy, species = if_else(species == "Chinese mystery", "snail", species))

isotopes_messy<-mutate(isotopes_messy, species = if_else(species == "Chironomid", "chironomid", species))



isotopes_messy<-mutate(isotopes_messy, species = if_else(species == "Pumpkinseed", "pumpkinseed (Lepomis gibbosus)", species))

isotopes_messy<-mutate(isotopes_messy, species = if_else(species == "Bluegill", "bluegill (Lepomis macrochirus)", species))

isotopes_messy<-mutate(isotopes_messy, species = if_else(species == "surface periphyton", "periphyton", species))

isotopes_messy<-mutate(isotopes_messy, species = if_else(species == "macrophytes", "macrophyte", species))

#factor the taxa from high to low trophic level
isotopes_messy$species <- factor(isotopes_messy$species, # Relevel species factor
                         levels = c("pumpkinseed (Lepomis gibbosus)", 
                         "bluegill (Lepomis macrochirus)", 
                         "zooplankton", 
                         "snail", 
                         "phytoplankton", 
                         "periphyton", 
                         "chironomid", 
                         "macrophyte"))

Pump <- "#e66101"
Blue <- "#2c7bb6"
zoo <- "#4d4d4d" #"#bababa"  #"#b35806"
snl <- "#9970ab" #"#4d4d4d"
phyt <- "#66bd63"
peri <- "#b8e186"
chron <- "#d73027"
macro <- "#de77ae"

pump_sp <- expression(paste("pumpkinseed (", italic("L. gibbosus"), ")"))
blue_sp <- expression(paste("bluegill (", italic("L. macrochirus"), ")"))

ggplot(data = isotopes_messy, aes(x = d13cvpdb, y = d15n_air, color = species)) +
  geom_point(aes(size = fillet_as, color = species), alpha = .75) +
  scale_size(range = c(2, 18), breaks = c(1, 10, 100, 600))  +
  facet_grid(. ~ lake) +
  theme_bw() +
  scale_color_manual(values=c(Pump,
                              Blue,
                              zoo,
                              snl,
                              phyt,
                              peri,
                              chron,
                              macro),
                     labels = c(pump_sp, 
                                blue_sp, 
                                "zooplankton", 
                                "snail", 
                                "phytoplankton", 
                                "periphyton", 
                                "chironomid", 
                                "macrophyte")) +
  theme(text=element_text(size=12,  
                          family="serif"),
        legend.text.align = 0) + #for some reason the text begam right aligned
  labs(x = expression(italic(delta)^13*C),
       y = expression(italic(delta)^15*N), 
       color = "Taxa",
       size = expression(paste(
         "Total As (",
         mu,"g ", g^-1,
         ")", sep=""))) 
 



ggsave("figs/comparAs_manuscript_Fig2.png", width = 6.5, height = 4.5, units = "in" )


