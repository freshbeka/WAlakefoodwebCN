
## Run 2a, then immediately run 2b.

# Confirm subset is still available
SIdata_subset

### Now I do an individual consumer mixing model. ####

df <- tibble(
  Lake_Year = character(),
  Identity = character(),
  littoral.reliance = numeric())

j<-7
i<-3

for (j in 1:length(End.memberlakes)){   
  data <- SIdata_subset %>% filter(Lake_Year == End.memberlakes[j])
  
  for (i in 1:nrow(data %>% filter(Group == "Fish"))){   
    Cfish <- data %>% filter(Group == "Fish") %>% slice(i) %>% pull(d13C) ## all fish
    lit <-data %>% filter(Group == "Gastropod") %>% pull(d13C) %>% mean() ## I determine littoral, and I take the mean incase there are more than 1 value.
    pel <-data %>% filter(Group == "Zooplankton") %>%  pull(d13C) %>% mean()## I determine pelagic. I take the mean incase there are more than 1 value.
    a <-data %>% filter(Group == "Fish") %>% slice(i) %>% pull(Lake_Year)
    b <-data %>% filter(Group == "Fish") %>% slice(i) %>% pull(Identity)
    c <- (Cfish - pel)/(lit - pel) ##I calculate the reliance as if I didn't have end-member data.
    df <- df %>% add_row(Lake_Year =a,
                         Identity = b,
                         littoral.reliance = c)
  }
  
}

individual.reliance <- df

L.gibbosus <- individual.reliance %>% 
  filter(Identity == "Lepomis gibbosus") 
L.gibbosus_Angle <- individual.reliance %>% 
  filter(Identity == "Lepomis gibbosus") %>% filter(Lake_Year == "Angle_2019")
L.macrochirus <-individual.reliance %>% 
  filter(Identity == "Lepomis macrochirus")

##OK, in order to get this legend to work, I think I need to make some new columns.

## Regional lakes
#A column for linetype
L.gibbosus$linetype <- "solid"
#A column for color
L.gibbosus$legend.name <- "Pumpkinseed, regional lakes"  
#A column for size <-
L.gibbosus$linesize <- 0.5

## Angle lake
# color
L.gibbosus$legend.name <- L.gibbosus %>% 
  filter(Lake_Year == "Angle_2019") %>%  
  str_replace(legend.name, "Pumpkinseed, regional lakes", "Pumpkinseed, Angle Lake") 
#A column for size <-
L.gibbosus$linesize <- 1.5

##Killarney
#A column for linetype
L.macrochirus$linetype <- "twodash"
#A column for color
L.macrochirus$legend.name <- "Bluegill, Killarney Lake"  
#A column for size <-
L.macrochirus$linesize <- 1.5


plot. <- bind_rows(L.gibbosus,L.macrochirus)

A19 <- "#e66101"
K19 <- "#2c7bb6"
oth <- "#fdb863"


p2 <- ggplot(data = L.gibbosus, 
           aes(x = littoral.reliance, 
               color = Lake_Year)) +
  geom_density(size = .5)  + 
  theme_minimal() +
  scale_color_manual(values=c(A19, #pumpkinseed/angle_2019
                              rep(oth,3), 
                              K19,  #bluegill/killarney
                              rep(oth,5))) +
  geom_density(data = L.gibbosus_Angle, 
               size = 1.5,
               aes(x = littoral.reliance))+
  geom_density(data = L.macrochirus, 
               size = 1.5,
               linetype = "twodash", ## remove this to remove dashed lines
               aes(x = littoral.reliance)) +
  labs(colour="Lake", x = "Littoral reliance") +
  theme(legend.position="none")
  

p2


ggsave("figs/density_littoralreliance_indlake.png",p2,  width = 10, height = 6, units = "in" )

ggsave("figs/density_littoralreliance_indlake_BWsafe.png",p2,  width = 10, height = 6, units = "in" )
