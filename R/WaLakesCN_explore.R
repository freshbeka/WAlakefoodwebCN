# This script takes C & N isotope data from several Washington lowland lakes, organizes it, and plots it.

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

## first, I want to understand how many lakes have multiple sampling events.
SIdata %>% group_by(Lake) %>% #group according to lake name
  summarise(years = n_distinct(Year)) %>% # tally up the number of years
  filter(years>1) # filter the years that greater than 0

#  Nine lakes have multiple sampling events, especially pine lake.
# I want to give each sampling even a  unique identifier so I don't accidentally pool the wrong data. The easiest way is to create a combo of Lake and Year (probably).
SIdata_tidy <-SIdata %>% 
  unite(col="Lake_Year", c(Lake, Year), sep="_", remove = FALSE) %>% #add new column called Lake_Year
  filter(!is.na(Identity)) #remove the lake without ID info (Pine 2013)


#Now I want to view the groups and see what is in each group.
SIdata_tidy %>% distinct(Group) # great, this is helpful.

SIdata_tidy %>% group_by(Lake_Year,Group) %>% summarize(total= n())

#To quickly get a sense of each of the sampling events, I'm going to loop through each of the lakes and plot it.

#how many lakes in all
n_distinct(SIdata_tidy$Lake_Year)
#there are 40 lakes. That is too many for one grid. I'll do two grids of 20

p <- list() #create an empty list
start<-0 #create an index if I cant get all lakes  into one grid
for (i in 1:20){
  i <- i+start
  lake.name <-  unique(SIdata_tidy$Lake_Year)[i] #pull the relevant data
  onelake <- SIdata_tidy %>% filter(Lake_Year == lake.name)
  p[[i-start]]<-ggplot(data = onelake, aes(x = d13C, y = d15N, color = Group)) +
    geom_point() + 
    theme_minimal() +
    theme(axis.title = element_blank()) +
    ggtitle(lake.name)
}
do.call(grid.arrange,p)

p <- list() #create an empty list
start<-20 #use index to start at the 21st lake.
for (i in 1:20){
  i <- i+start
  lake.name <-  unique(SIdata_tidy$Lake_Year)[i] #pull the relevant data
  onelake <- SIdata_tidy %>% filter(Lake_Year == lake.name)
  p[[i-start]]<-ggplot(data = onelake, aes(x = d13C, y = d15N, color = Group)) +
    geom_point() + 
    theme_minimal() +
    theme(axis.title = element_blank()) +
    ggtitle(lake.name)
}
do.call(grid.arrange,p)

SIdata_tidy
# Note, now that I look at the lakes, I see that when a sample is 1 or 2, it means that the same sample was run twice. I should not treat these as separate samples, rather they are getting at precision of the instruments used to measure C & N, and/or how well homogenized each sample is. I am going to take the mean of the replicates, and replace with a single values. I don't think this is consistent among the lakes. Sometimes 1 and 1 are the same sample split into 2, other times it is ID-1, ID-2 that are the same organism. I'll need to dig in.

## 



# Now I want to take the mean of each major group (fish species) for each lake."Identity" is the co,umn to summarize.

SImeans <- SIdata_tidy %>% 
  group_by(Lake_Year, Identity, Group) %>% #separate by lake and species and group
  summarise(d13C_mean = mean(d13C), #obtain mean and SD for each lake/species
            d13C_sd = sd(d13C),
            d15N_mean = mean(d15N),
            d15N_sd = sd(d15N))

##take a peek at a lake
  lake.name <-  unique(SImeans$Lake_Year)[3] #pull the relevant data
  onelake <- SImeans %>% filter(Lake_Year == lake.name) 
  ggplot(data = onelake, aes(x = d13C_mean, y = d15N_mean, color = Group, lable = Identity)) +
    geom_point() + 
    geom_text_repel(aes(label = Identity, color = 'white', size = 1),  show.legend = FALSE) +
    geom_errorbar(aes(xmin=d13C_mean-d13C_sd, xmax=d13C_mean+d13C_sd), width=.2,
                  position=position_dodge(0.05)) +
    geom_errorbar(aes( ymin=d15N_mean-d15N_sd, ymax=d15N_mean+d15N_sd), width=.2,
                  position=position_dodge(0.05)) +
    theme_minimal() +
    theme(axis.title = element_blank()) +
    ggtitle(lake.name)

#Can I ID diet for just pumpkin seed vs blue gill?
ggplot(data = onelake, aes(x = d13C_mean, y = d15N_mean, color = Group)) +
  geom_point() + 
  geom_point(data = SIdata_tidy %>% filter(Lake_Year == lake.name) %>% filter(Identity == "Lepomis gibbosus"), aes(x = d13C, y = d15N), color = "black") + 
  geom_point(data = SIdata_tidy %>% filter(Lake_Year == lake.name) %>% filter(Group == "Aquatic invertebrate"), aes(x = d13C, y = d15N), color = "gray") +
  geom_errorbar(aes(xmin=d13C_mean-d13C_sd, xmax=d13C_mean+d13C_sd), width=.2,
                  position=position_dodge(0.05)) +
  geom_errorbar(aes( ymin=d15N_mean-d15N_sd, ymax=d15N_mean+d15N_sd), width=.2,
                  position=position_dodge(0.05)) +
  theme_minimal() +
  theme(axis.title = element_blank()) +
  ggtitle(lake.name)
  
# For each lake I'm going to need to select prey items to complete simple mixing models.
# I could also select a mean and sd value and a monte carlo that includes uncertainty. 
# I want to ID what fish may be eating and look for regional patterns. First, I'm going to look at the more common species of fish.
  
SImeans %>% filter(Group == "Fish") %>% group_by(Identity) %>% tally() %>% arrange(-n)
# The most common species is pumpkinseed (Lepomis gibbosus), largemouth bass (Micropterus salmoides), and yellow perch (Perca flavescens)
samples<-SImeans %>% group_by(Identity) %>% tally() %>% arrange(-n)

# I'll start with pumpkinseed.


##I think I need to look at more quantitative differences related to N and C. For example, how much more enriched in N are the pumpkinseed relative to the crayfish?



## creating a loop that calculates the relationship between pumpkinseed and a few other key species. I need to know what known prey are in the lakes, and what pumpkinseed may normally eat.

df <- tibble(
  Lake_Year = character(),
  pump.d13C = numeric(),
  pump.d15N = numeric(),
  CMS.d13C = numeric(),
  CMS.d15N = numeric(),
  C.diff = numeric(),
  N.diff = numeric())


lake.name <-  unique(SImeans$Lake_Year)

for (i in 1:length(lake.name)){
  onelake <- SImeans %>% filter(Lake_Year == lake.name[i]) 
  pump.d13C <-onelake %>% filter(Identity == "Lepomis gibbosus") %>% select(d13C_mean) %>%pull()
  pump.d15N <-onelake %>% filter(Identity == "Lepomis gibbosus") %>% select(d15N_mean) %>%pull()
  CMS.d13C <-onelake %>% filter(Identity == "Bellayma chinesis") %>% select(d13C_mean) %>%pull()
  CMS.d15N <-onelake %>% filter(Identity == "Bellayma chinesis") %>% select(d15N_mean) %>%pull()
  C.diff <-pump.d13C - CMS.d13C
  N.diff <-pump.d15N - CMS.d15N
  df <- df %>% add_row(Lake_Year =lake.name[i],
                       pump.d13C = pump.d13C,
                       pump.d15N = pump.d15N,
                       CMS.d13C = CMS.d13C,
                       CMS.d15N = CMS.d15N,
                       C.diff = C.diff,
                       N.diff = N.diff)
}

pks.CMS <-df
pks.CMS$theta <- atan(pks.CMS$N.diff/pks.CMS$C.diff)
pks.CMS$value <- sqrt((pks.CMS$N.diff^2) + (pks.CMS$C.diff^2))

ggplot(data = pks.CMS, aes(x = "CMS", y = N.diff)) +
  geom_boxplot() + 
  geom_jitter() +
  theme_minimal()

ggplot(data = pks.CMS, aes(y = C.diff)) +
  geom_boxplot() + theme_minimal()


##This works, and it indicates that the pumpkn seed are generally enriched relative to the CMS, but not really that great of a plot.
ggplot(data = pks.CMS, aes(x = atan(N.diff/C.diff), y = sqrt((N.diff^2) + (C.diff^2)))) +
         coord_polar() +
         geom_segment(aes(y=0, xend=atan(N.diff/C.diff), yend=sqrt((N.diff^2) + (C.diff^2)))) +
  scale_x_continuous(limits=c(0, 360), breaks=c(0, 90, 180, 270)) +
         theme_minimal()


ggplot(data = pks.CMS, aes(x = theta, y = value)) +
  coord_polar() +
  geom_segment(aes(y=0, xend=theta, yend=value)) +
  scale_x_continuous(limits=c(0, 360), breaks=c(0, 90, 180, 270)) +
  theme_minimal()

##Alternative vector, to keep it long form. CMS ####
df <- tibble(
  Lake_Year = character(),
  comparison = character(),
  isotope = character(),
  difference = numeric())

lake.name <-  unique(SImeans$Lake_Year)

for (i in 1:length(lake.name)){
  onelake <- SImeans %>% filter(Lake_Year == lake.name[i]) 
  main.d13C <-onelake %>% filter(Identity == "Lepomis gibbosus") %>% select(d13C_mean) %>%pull()
  main.d15N <-onelake %>% filter(Identity == "Lepomis gibbosus") %>% select(d15N_mean) %>%pull()
  comp.d13C <-onelake %>% filter(Identity == "Bellayma chinesis") %>% select(d13C_mean) %>%pull()
  comp.d15N <-onelake %>% filter(Identity == "Bellayma chinesis") %>% select(d15N_mean) %>%pull()
  C.diff <-main.d13C - comp.d13C
  N.diff <-main.d15N - comp.d15N
  comp <- "pump.CMS"
  df <- df %>% add_row(Lake_Year =lake.name[i],
                       comparison = comp,
                       isotope = "carbon",
                       difference = C.diff)
  df <- df %>% add_row(Lake_Year =lake.name[i],
                       comparison = comp,
                       isotope = "nitrogen",
                       difference = N.diff)
}

p1<-ggplot(data = df, aes(x = isotope, y = difference, fill = isotope)) +
  geom_boxplot() + 
  geom_jitter() + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_minimal() + 
  xlab(label = "pumpkinseed and CMS")
p1

##Alternative vector, to keep it long form. periphyton ####
df <- tibble(
  Lake_Year = character(),
  comparison = character(),
  isotope = character(),
  difference = numeric())

lake.name <-  unique(SImeans$Lake_Year)

for (i in 1:length(lake.name)){
  onelake <- SImeans %>% filter(Lake_Year == lake.name[i]) 
  main.d13C <-onelake %>% filter(Identity == "Lepomis gibbosus") %>% select(d13C_mean) %>%pull()
  main.d15N <-onelake %>% filter(Identity == "Lepomis gibbosus") %>% select(d15N_mean) %>%pull()
  comp.d13C <-onelake %>% filter(Identity == "Periphyton") %>% select(d13C_mean) %>%pull()
  comp.d15N <-onelake %>% filter(Identity == "Periphyton") %>% select(d15N_mean) %>%pull()
  C.diff <-main.d13C - comp.d13C
  N.diff <-main.d15N - comp.d15N
  comp <- "pump.periphyton"
  df <- df %>% add_row(Lake_Year =lake.name[i],
                       comparison = comp,
                       isotope = "carbon",
                       difference = C.diff)
  df <- df %>% add_row(Lake_Year =lake.name[i],
                       comparison = comp,
                       isotope = "nitrogen",
                       difference = N.diff)
}

##Write a function for these comparisons####
compfunction<- function(ident.main, ident.comp, comp.name) {
  df <- tibble(
    Lake_Year = character(),
    comparison = character(),
    isotope = character(),
    difference = numeric())
  lake.name <-  unique(SImeans$Lake_Year)
  for (i in 1:length(lake.name)){
  onelake <- SImeans %>% filter(Lake_Year == lake.name[i]) 
  main.d13C <-onelake %>% filter(Identity == ident.main) %>% select(d13C_mean) %>%pull()
  main.d15N <-onelake %>% filter(Identity == ident.main) %>% select(d15N_mean) %>%pull()
  comp.d13C <-onelake %>% filter(Identity == ident.comp) %>% select(d13C_mean) %>%pull()
  comp.d15N <-onelake %>% filter(Identity == ident.comp) %>% select(d15N_mean) %>%pull()
  C.diff <-main.d13C - comp.d13C
  N.diff <-main.d15N - comp.d15N
  comp <- comp.name
  df <- df %>% add_row(Lake_Year =lake.name[i],
                       comparison = comp,
                       isotope = "carbon",
                       difference = C.diff)
  df <- df %>% add_row(Lake_Year =lake.name[i],
                       comparison = comp,
                       isotope = "nitrogen",
                       difference = N.diff)
}
  return(df)
}


pumpkinseed.CMS <-compfunction("Lepomis gibbosus","Bellayma chinesis","pump.CMS" ) 
p1<-ggplot(data = pumpkinseed.CMS, aes(x = isotope, y = difference, fill = isotope)) +
  geom_boxplot() + 
  geom_jitter() + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_minimal() + 
  xlab(label = "pumpkinseed and CMS")
p1


pumpkinseed.periphyton <-compfunction("Lepomis gibbosus","Periphyton","pump.periphyton" ) 
p2<-ggplot(data = pumpkinseed.periphyton, aes(x = isotope, y = difference, fill = isotope)) +
  geom_boxplot() + 
  geom_jitter() + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_minimal() + 
  xlab(label = "pumpkinseed and periphyton")
p2

pumpkinseed.Chironomidae <-compfunction("Lepomis gibbosus","Chironomidae","pump.Chironomidae" ) 
p3<-ggplot(data = pumpkinseed.Chironomidae, aes(x = isotope, y = difference, fill = isotope)) +
  geom_boxplot() + 
  geom_jitter() + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_minimal() + 
  xlab(label = "pumpkinseed and Chironomidae")
p3

pumpkinseed.signal <-compfunction("Lepomis gibbosus","Pacifastacus leniusculus","pump.Crayfish" ) 
p4<-ggplot(data = pumpkinseed.signal, aes(x = isotope, y = difference, fill = isotope)) +
  geom_boxplot() + 
  geom_jitter() + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_minimal() + 
  xlab(label = "pumpkinseed and signal crayfish")
p4

#isopoda ####
pumpkinseed.isopod <-compfunction("Lepomis gibbosus","Isopoda","pump.Crayfish" ) 
p5<-ggplot(data = pumpkinseed.isopod, aes(x = isotope, y = difference, fill = isotope)) +
  geom_boxplot() + 
  geom_jitter() + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_minimal() + 
  xlab(label = "pumpkinseed and isopod")
p5


#amphipods ####
pumpkinseed.amphipod <-compfunction("Lepomis gibbosus","Gammaridae","pump.Crayfish" ) 
p6<-ggplot(data = pumpkinseed.amphipod, aes(x = isotope, y = difference, fill = isotope)) +
  geom_boxplot() + 
  geom_jitter() + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_minimal() + 
  xlab(label = "pumpkinseed and Gammaridae")
p6

fullplot<-(p1 + p3 + p5)/(p6 + p4 + p2)

# ggsave("figs/comparisons.png",fullplot,  width = 10, height = 6, units = "in" )