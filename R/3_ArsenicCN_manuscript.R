# This script takes the trophic transfer results and plots them with isotopes


library(googledrive) #to read in the data directly from googledrive
library(readxl) #to convert the excel file into a R object
library(janitor) # to clean up rownames
library(tidyverse) #for organizing and plotting
library(broom) #for tidy model output

##First, call the file from the google drive and save it in the data file location. This needs to be adjusted based on whomever is using this script, and based on what machine you are using.
drive_download("https://docs.google.com/spreadsheets/d/1qe0RBzG0_HZdpPLTUukH7BOrrH4Qblte/edit#gid=890212752", 
               
               path = "/GitProjects/Arsenic/data/Trophic4.xlsx",
             #  path = "/Users/rebekahstiling/Desktop/Projects/WAlakefoodwebCN/data/Trophic3.xlsx",
               overwrite = TRUE)

## The file was saved locally to my machine, now I want to read it into RStudio 

#check the sheets
excel_sheets("data/Trophic4.xlsx")

trophic_messy <- read_excel("data/Trophic4.xlsx", 
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

## Next step, go back to "trophic_messy" and figure out how to get zooplankton and phytoplaknton values organized into the tibble for plotting.

#zooplanktotn
zoop<-trophic_messy %>% filter(species == "zooplankton")

Angle.zoo.in <- zoop %>% 
  filter(!is.na(percent_inorganic) & lake == "Angle Lake") %>% 
  select(lake, species, percent_inorganic, d15n_air)

Angle.N15mean.zoop <- zoop %>% 
  filter(!is.na(d15n_air) & lake == "Angle Lake") %>% 
  select(lake, species,d15n_air) %>% 
  summarise(ave = mean(d15n_air))

Angle.zoo.in<-Angle.zoo.in %>% mutate(d15n_air = pull(Angle.N15mean.zoop))

Kiln.zoo.in <- zoop %>% 
  filter(!is.na(percent_inorganic) & lake == "Lake Killarney") %>% 
  select(lake, species, percent_inorganic, d15n_air)

Kiln.N15mean.zoop <- zoop %>% 
  filter(!is.na(d15n_air) & lake == "Lake Killarney") %>% 
  select(lake, species,d15n_air) %>% 
  summarise(ave = mean(d15n_air))

Kiln.zoo.in<-Kiln.zoo.in %>% mutate(d15n_air = pull(Kiln.N15mean.zoop))
colnames(Kiln.zoo.in)

#phytoplankton
phy<-trophic_messy %>% filter(species == "phytoplankton")

Angle.phy.in <- phy %>%   ##Unlike everywhere else, there are 4 here. I'll take the mean of the center two because they are the same date.
  filter(!is.na(percent_inorganic) & lake == "Angle Lake") %>% 
  select(lake, species, collection_date, percent_inorganic, d15n_air)     

two<-Angle.phy.in %>% slice(c(1,4))
formean<-Angle.phy.in %>% slice(c(2,3))
meanInOrg <-formean %>% summarize(ave = mean(percent_inorganic))
row2<-formean %>% slice(1) %>% mutate(percent_inorganic = pull(meanInOrg))
for.data <-bind_rows(two,row2)
Angle.phy.innew <-for.data %>% select(lake, species, percent_inorganic, d15n_air)   

Angle.N15mean.phy <- phy %>% 
  filter(!is.na(d15n_air) & lake == "Angle Lake") %>% 
  select(lake, species,d15n_air) %>% 
  summarise(ave = mean(d15n_air))

Angle.phy.innew<-Angle.phy.innew %>% mutate(d15n_air = pull(Angle.N15mean.phy))

Kiln.phy.in <- phy %>% 
  filter(!is.na(percent_inorganic) & lake == "Lake Killarney") %>% 
  select(lake, species, percent_inorganic, d15n_air)

Kiln.N15mean.phy <- phy %>% 
  filter(!is.na(d15n_air) & lake == "Lake Killarney") %>% 
  select(lake, species,d15n_air) %>% 
  summarise(ave = mean(d15n_air))

Kiln.phy.in<-Kiln.phy.in %>% mutate(d15n_air = pull(Kiln.N15mean.phy))


sampled<-onlyAs.N %>% select(lake, species, percent_inorganic, d15n_air)

d15NAs_data<-bind_rows(Angle.zoo.in,Kiln.zoo.in,Angle.phy.innew,Kiln.phy.in,sampled)

write_csv(d15NAs_data, "data/d15NAs_data.csv" ) #Save data for plotting starting with data if necessary



# Second, record group data means as species, lake combos
gd <- d15NAs_data %>% 
  group_by(species, lake) %>% 
  summarise(percent_inorganic = mean(percent_inorganic),
            d15n_air  = mean(d15n_air),
            count = n())

#Plot the data we have with the means as center points
ggplot(data = d15NAs_data, 
       aes(x = percent_inorganic, 
           y = d15n_air, 
           color = lake,
           fill = lake,
           shape = species)) + 
  geom_smooth(method = "lm", 
              aes(group = lake)) +
  geom_point() + 
  geom_point(data=gd, size = 4) +
  scale_shape_manual(values=c(3,7,8,15:18,25)) + 
  theme_bw() +
  labs(x = "% inorganic As",
       y = expression(italic(delta)^15*N), 
       color = "Lake",
       shape = "Species") 

# Test H_0: percent_inorganic*lake = 0 for difference in slopes.
mod <- lm(d15n_air~percent_inorganic+percent_inorganic:lake,data=d15NAs_data)
summary(mod)
tidy(mod)

mod.alt <- lm(d15n_air~percent_inorganic*lake,data=d15NAs_data)
summary(mod.alt)
tidy(mod.alt)
anova(mod.alt)

angmod <- lm(d15n_air~percent_inorganic,data=d15NAs_data %>% filter(lake == "Angle Lake"))
summary(angmod)
kilmod <- lm(d15n_air~percent_inorganic,data=d15NAs_data %>% filter(lake == "Lake Killarney"))
summary(kilmod)
tidy(mod)

# Test relationship with and without groups. No difference
tidy(anova(lm(d15n_air~percent_inorganic,data=d15NAs_data),lm(d15n_air~percent_inorganic*lake,data=d15NAs_data)))



ggplot(data = d15NAs_data, 
       aes(x = percent_inorganic, 
           y = d15n_air, 
           color = lake,
           fill = lake,
           shape = species)) + 
  geom_smooth(method = "lm", aes(group = lake)) +
  geom_point() + 
  geom_point(data=gd, size = 4) +
  scale_shape_manual(values=c(3,7,8,15:18,25)) + 
  theme_bw() +
  labs(x = "% inorganic As",
       y = expression(italic(delta)^15*N), 
       color = "Lake",
       shape = "Species") 


## adding plots switching axis
ggplot(data = d15NAs_data, 
       aes(x = d15n_air, 
           y = percent_inorganic, 
           color = lake,
           fill = lake,
           shape = species)) + 
  geom_smooth(method = "lm", 
              aes(group = lake)) +
  geom_point() + 
  #geom_point(data=gd, size = 4) +
  scale_shape_manual(values=c(3,7,8,15:18,25)) + 
  theme_bw() +
  labs(x = expression(italic(delta)^15*N ,
       y = "% inorganic As" ), 
       color = "Lake",
       shape = "Species") 

mod.swap <- lm(percent_inorganic~ d15n_air*lake,data=d15NAs_data)
summary(mod.swap)
tidy(mod.swap)
anova(mod.swap)
