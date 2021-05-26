
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

Angle.Killarney.individuals <- bind_rows(L.gibbosus,L.macrochirus)

A19 <- "#fdae61"
K19 <- "#2c7bb6"
oth <- "#abd9e9"


p2 <- ggplot(data = Angle.Killarney.individuals, 
           aes(x = littoral.reliance, 
               color = Lake_Year)) +
  geom_density()  + 
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
               aes(x = littoral.reliance)) +
  labs(colour="Lake", x = "Littoral reliance") +
  theme(legend.position="none")
  

p2


ggsave("figs/density_littoralreliance_indlake.png",p2,  width = 10, height = 6, units = "in" )
