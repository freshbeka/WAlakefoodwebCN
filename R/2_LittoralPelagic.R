## Gastropod vs zooplankton when available.

## Plan here, search for the lakes that have "end member data" and quickly calculate pelagic vs littoral reliance by the pumpkinseed. If this is the dirctoin we want to go, I'll need to be stratgic about getting end members for the remaining lakes that don't have zoops and snails.

library(tidyverse) # To organize and plot
library(ggrepel) #for labeling species
library(patchwork) #for multi-panel plot


# read in the tidy data
SIdata_tidy <- read_csv("data/SI_tidy.csv")


##get the means for each identity
SImeans <- SIdata_tidy %>% 
  group_by(Lake_Year, Identity, Group) %>% #separate by lake and species and group
  dplyr::summarise(d13C_mean = mean(d13C), #obtain mean and SD for each lake/species
            d13C_sd = sd(d13C),
            d15N_mean = mean(d15N),
            d15N_sd = sd(d15N)) %>% 
  ungroup()  ## Ungroup is key, otherwise the mixing model will not work because "select(i)" cannot distinguish rows

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

# calculate the littoral reliance value for each species ####
#create empty dataframe
df <- tibble(
  Lake_Year = character(),
  Identity = character(),
  littoral.reliance = numeric(),
  d13C_mean = numeric(),
  d15N_mean = numeric(),
  richness = numeric(),
  group = character())

j<-7
i<-3

for (j in 1:length(SIdata_tidy)){   
  data <- SImeans %>% filter(Lake_Year == End.memberlakes[j])
  
  for (i in 1:nrow(data %>% filter(Group == "Fish"))){   
    Cfish <- data %>% filter(Group == "Fish") %>% slice(i) %>% pull(d13C_mean) ## all fish
    Nfish <- data %>% filter(Group == "Fish") %>% slice(i) %>% pull(d15N_mean) ## all fish
    lit <-data %>% filter(Group == "Gastropod") %>% pull(d13C_mean) %>% mean() ## I determine littoral, and I take the mean incase there are more than 1 value.
    pel <-data %>% filter(Group == "Zooplankton") %>%  pull(d13C_mean) %>% mean()## I determine pelagic. I take the mean incase there are more than 1 value.
    a <-data %>% filter(Group == "Fish") %>% slice(i) %>% pull(Lake_Year)
    b <-data %>% filter(Group == "Fish") %>% slice(i) %>% pull(Identity)
    c <- (Cfish - pel)/(lit - pel) ##I calculate the reliance as if I didn't have end-member data.
    d <- data %>% filter(Group == "Fish") %>% nrow()
    e <- "Fish"
    df <- df %>% add_row(Lake_Year =a,
                         Identity = b,
                         littoral.reliance = c,
                         d13C_mean = Cfish,
                         d15N_mean = Nfish,
                         richness = d,
                         group = e)
  }
  
}

fish_reliance <- df

##Now do it for crayfish ####

#Of the endmember lakes, which have crayfish?
crayfish.lakes<- SImeans %>% filter(Group == "Crayfish") %>% pull(Lake_Year) %>% unique()
crayfish.lakes<-as_tibble(crayfish.lakes)

End.lakes <-as_tibble(End.memberlakes)

Cray.end.lakes <-inner_join(End.lakes, crayfish.lakes) %>% pull()

df <- tibble(
  Lake_Year = character(),
  Identity = character(),
  littoral.reliance = numeric(),
  d13C_mean = numeric(),
  d15N_mean = numeric(),
  richness = numeric(),
  group = character())

j<-7
i<-2

for (j in 1:length(Cray.end.lakes)){   
  data <- SImeans %>% filter(Lake_Year == Cray.end.lakes[j])
  
  for (i in 1:nrow(data %>% filter(Group == "Crayfish"))){   
    Cfish <- data %>% filter(Group == "Crayfish") %>% slice(i) %>% pull(d13C_mean) ## all fish
    Nfish <- data %>% filter(Group == "Crayfish") %>% slice(i) %>% pull(d15N_mean) ## all fish
    lit <-data %>% filter(Group == "Gastropod") %>% pull(d13C_mean) %>% mean() ## I determine littoral, and I take the mean incase there are more than 1 value.
    pel <-data %>% filter(Group == "Zooplankton") %>%  pull(d13C_mean) %>% mean()## I determine pelagic. I take the mean incase there are more than 1 value.
    a <-data %>% filter(Group == "Crayfish") %>% slice(i) %>% pull(Lake_Year)
    b <-data %>% filter(Group == "Crayfish") %>% slice(i) %>% pull(Identity)
    c <- (Cfish - pel)/(lit - pel) ##I calculate the reliance as if I didn't have end-member data.
    d <- data %>% filter(Group == "Fish") %>% nrow()
    e <- "Crayfish"
    df <- df %>% add_row(Lake_Year =a,
                         Identity = b,
                         littoral.reliance = c,
                         d13C_mean = Cfish,
                         d15N_mean = Nfish,
                         richness = d,
                         group = e)
  }
  
}

crayfish_reliance <- df

reliance <-bind_rows(crayfish_reliance, fish_reliance)

p1 <- ggplot(data = reliance, aes(x = littoral.reliance, y = Identity)) +
  geom_boxplot(aes(fill = group)) + 
  geom_jitter() + 
  theme_minimal()

ggsave("figs/boxplot_littoralreliance_species.png",p1,  width = 10, height = 6, units = "in" )

### Now I do an individual consumer mixing model. ####

SIdata_tidy

df <- tibble(
  Lake_Year = character(),
  Identity = character(),
  littoral.reliance = numeric(),
  d13C = numeric(),
  richness = numeric(),
  group = character())

for (j in 1:length(End.memberlakes)){   
  data <- SIdata_tidy %>% filter(Lake_Year == End.memberlakes[j])
  
  for (i in 1:nrow(data %>% filter(Group == "Fish"))){   
    Cfish <- data %>% filter(Group == "Fish") %>% slice(i) %>% pull(d13C) ## all fish
    lit <-data %>% filter(Group == "Gastropod") %>% pull(d13C) %>% mean() ## I determine littoral, and I take the mean incase there are more than 1 value.
    pel <-data %>% filter(Group == "Zooplankton") %>%  pull(d13C) %>% mean()## I determine pelagic. I take the mean incase there are more than 1 value.
    a <-data %>% filter(Group == "Fish") %>% slice(i) %>% pull(Lake_Year)
    b <-data %>% filter(Group == "Fish") %>% slice(i) %>% pull(Identity)
    c <- (Cfish - pel)/(lit - pel) ##I calculate the reliance as if I didn't have end-member data.
    d <- data %>% filter(Group == "Fish") %>% nrow()
    df <- df %>% add_row(Lake_Year =a,
                         Identity = b,
                         littoral.reliance = c,
                         d13C = Cfish,
                         richness = d)
  }
  
}

individual.reliance <- df

L.gibbosus <-individual.reliance %>% filter(Identity == "Lepomis gibbosus")

p2 <- ggplot(data = L.gibbosus, aes(x = littoral.reliance, y = Identity)) +
  geom_boxplot()  + 
  theme_minimal()

ggsave("figs/density_littoralreliance_lake.png",p2,  width = 10, height = 6, units = "in" )



