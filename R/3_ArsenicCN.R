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


##Change CMS to snail for ASLO
isotopes_messy<-mutate(isotopes_messy, species = if_else(species == "Chinese mystery", "snail", species))

isotopes_messy<-mutate(isotopes_messy, species = if_else(species == "Chironomid", "chironomid", species))

# start with plotting the C & N from Angle

ggplot(data = isotopes_messy %>% filter(lake == "Angle"), aes(x = d13cvpdb, y = d15n_air, shape = species)) +
  geom_point(aes(size = fillet_as, color = -fillet_as)) +
  scale_shape_manual(values=seq(2,8)) +
  theme_minimal() 

ggplot(data = isotopes_messy %>% filter(lake == "Killarney"), aes(x = d13cvpdb, y = d15n_air, shape = species)) +
  geom_point(aes(size = fillet_as)) +
  scale_shape_manual(values=seq(2,8)) +
  theme_minimal() 

ggplot(data = isotopes_messy %>% filter(lake == "Angle"), aes(x = d13cvpdb, y = d15n_air, color = species)) +
  geom_point(aes(size = fillet_as, color = species), alpha = .75) +
  scale_size(range = c(0.5, 12))  +
  theme_minimal() +
  labs(title = "Angle Lake", x = "d13C", y = "d15N")

ggplot(data = isotopes_messy %>% filter(lake == "Killarney"), aes(x = d13cvpdb, y = d15n_air, color = species)) +
  geom_point(aes(size = fillet_as, color = species), alpha = .75) +
  scale_size(range = c(0.5, 12))  +
  theme_minimal() +
  labs(title = "Killarney Lake", x = "d13C", y = "d15N")

#combine with facet wrap

isotopes_messy$species <- factor(isotopes_messy$species, # Relevel species factor
                         levels = c("Pumpkinseed", "Bluegill", "zooplankton", "snail", "phytoplankton", "surface periphyton", "chironomid", "macrophytes"))

Pump <- "#e66101"
Blue <- "#2c7bb6"
zoo <- "#4d4d4d" #"#bababa"  #"#b35806"
snl <- "#9970ab" #"#4d4d4d"
phyt <- "#66bd63"
peri <- "#b8e186"
chron <- "#d73027"
macro <- "#de77ae"


ggplot(data = isotopes_messy, aes(x = d13cvpdb, y = d15n_air, color = species)) +
  geom_point(aes(size = fillet_as, color = species), alpha = .75) +
  scale_size(range = c(3, 17))  +
  facet_grid(. ~ lake) +
  theme_bw() +
  scale_color_manual(values=c(Pump,
                              Blue,
                              zoo,
                              snl,
                              phyt,
                              peri,
                              chron,
                              macro)) +
  labs(x = expression(italic(delta)^13*C),
       y = expression(italic(delta)^15*N), 
       size = expression(paste(
         "Total As (",
         mu, g, "/", g,
         ")", sep="")))

ggsave("figs/comparAs_ALSO.png", width = 7.5, height = 4.5, units = "in" )


