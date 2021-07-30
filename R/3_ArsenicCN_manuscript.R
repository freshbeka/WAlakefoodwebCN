# This script takes the trophic transfer results and plots them with isotopes


library(googledrive) #to read in the data directly from googledrive
library(readxl) #to convert the excel file into a R object
library(janitor) # to clean up rownames
library(tidyverse) #for organizing and plotting

##First, call the file from the google drive and save it in the data file location
drive_download("https://docs.google.com/spreadsheets/d/1qe0RBzG0_HZdpPLTUukH7BOrrH4Qblte/edit#gid=890212752", 
               path = "/Users/rebekahstiling/Desktop/RProjects/WAlakefoodwebCN/data/Trophic3.xlsx",
               overwrite = TRUE)

## The file was saved locally to my machine, now I want to read it into RStudio 

#check the sheets
excel_sheets("data/Trophic3.xlsx")

trophic_messy <- read_excel("data/Trophic3.xlsx", 
                            sheet = "results", 
                            skip = 2,
                            col_types = c("text","text","text","text", "date", 
                                          "numeric","numeric","numeric", "text","date",
                                          "numeric","numeric","numeric","numeric","numeric",
                                          "numeric", "numeric", "text", "text", "date",
                                          "numeric", "numeric", "numeric", "numeric", "numeric",
                                          "text"),
                            .name_repair = make_clean_names)
                            
                            
col_types(trophic_messy)
#Now I want to separate out the pieces of data that I know I need, and clean up the tibble

#some rows have the column names repeated, so remove them
trophic_messy <- trophic_messy %>% filter(species != "Species")

##Lake info

trophic_messy<-mutate(trophic_messy, lake = if_else(lake == "Angle", "Angle Lake", lake))

trophic_messy<-mutate(trophic_messy, lake = if_else(lake == "Killarney", "Lake Killarney", lake))

##Change names to to scientific names and uncapitolized
#trophic_messy<-mutate(trophic_messy, species = if_else(species == "Chinese mystery", "snail", species))

trophic_messy<-mutate(trophic_messy, species = if_else(species == "Chironomid", "chironomid", species))

trophic_messy<-mutate(trophic_messy, species = if_else(species == "Pumpkinseed", "pumpkinseed (Lepomis gibbosus)", species))

trophic_messy<-mutate(trophic_messy, species = if_else(species == "Bluegill", "bluegill (Lepomis macrochirus)", species))

trophic_messy<-mutate(trophic_messy, species = if_else(species == "surface periphyton", "periphyton (surface)", species))

trophic_messy<-mutate(trophic_messy, species = if_else(species == "macrophytes", "macrophyte", species))
trophic_messy$percent_inorganic<-(trophic_messy$as_v+ trophic_messy$as_iii)/(trophic_messy$as_v+
                                                               trophic_messy$as_iii+
                                                               trophic_messy$dma+
                                                               trophic_messy$mma+
                                                               trophic_messy$combined_arseno_sugars)
#First I want to consolidate the values that have C & N and As
isotopes_messy <- trophic_messy %>% drop_na(uc_davis_id)
# First, remote the d15Nvalues that don't have percent inorganic with them.
onlyAs.N <- isotopes_messy %>% filter(!is.na(percent_inorganic))
# Second, record group data means as species, lake combos
gd <- onlyAs.N %>% 
  group_by(species, lake) %>% 
  summarise(percent_inorganic = mean(percent_inorganic),
            d15n_air  = mean(d15n_air),
            count = n())

#Plot the data we have with the means as center points
ggplot(data = onlyAs.N, 
       aes(x = percent_inorganic, 
           y = d15n_air, 
           color = lake,
           fill = lake,
           shape = species)) +
  geom_point() + 
  geom_point(data=gd, size = 4) +
  scale_shape_manual(values=c(8,15:18,25)) + 
  theme_bw() +
  labs(x = "% inorganic As",
       y = expression(italic(delta)^15*N), 
       color = "Lake",
       shape = "Species")

## Next step, go back to "trophic_messy" and figure out how to get zooplankton and phytoplaknton values organized into the tibble for plotting.

## Then lable with tags similar to the littoral reliance plot.

# A look at all 25 symbols
df2 <- data.frame(x = 1:5 , y = 1:25, z = 1:25)
p <- ggplot(df2, aes(x, y))
p + geom_point(aes(shape = z), size = 4) +
  scale_shape_identity()
# While all symbols have a foreground colour, symbols 19-25 also take a
# background colour (fill)
p + geom_point(aes(shape = z), size = 4, colour = "Red") +
  scale_shape_identity()
p + geom_point(aes(shape = z), size = 4, colour = "Red", fill = "Black") +
  scale_shape_identity()
