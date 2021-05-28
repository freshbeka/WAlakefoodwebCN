
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

##OK, in order to get this legend to work, I think I need to make some a new column for the three different categories.

## Regional lakes
#A column for plotting category
L.gibbosus <- L.gibbosus %>% mutate(legend.name = if_else(Lake_Year == "Angle_2019",  
                                            "Pumpkinseed, Angle Lake", 
                                            "Pumpkinseed, regional lakes"))

#A column for plotting category
L.macrochirus$legend.name <- "Bluegill, Killarney Lake"  

plot.data <- bind_rows(L.macrochirus,L.gibbosus)

A19 <- "#e66101"
K19 <- "#2c7bb6"
oth <- "#fdb863"





p1<-ggplot(data = L.gibbosus, 
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
## easiest plot, no legend, but difficult to create legend for!
  
## line legend below, I had to use "stat_density" instead of geom_density in order to get lines in my legend rather than boxes!
p2<- ggplot(data = plot.data, 
         aes(x = littoral.reliance, 
             group = Lake_Year,
             color = legend.name,
             linetype = legend.name,
             linesize = legend.name)) +
  stat_density(aes(size = legend.name), geom = "line", position = "identity")  + 
  geom_density(data = plot.data %>% filter(Lake_Year == 'Angle_2019'),
               aes(x = littoral.reliance, 
                   group = Lake_Year,
                   color = legend.name,
                   linetype = legend.name,
                   size = legend.name),
               show.legend = FALSE) +
  geom_density(data = plot.data %>% filter(Lake_Year == 'Killarney_2019'),
               aes(x = littoral.reliance, 
                   group = Lake_Year,
                   color = legend.name,
                   linetype = legend.name,
                   size = legend.name),
               show.legend = FALSE) +
  theme_minimal() +
  scale_color_manual(values=c(K19, #bluegill/killarney 
                              A19,  #pumpkinseed/angle_2019
                              oth)) +
  scale_size_manual(values = c(1.5, 
                               1.5, 
                               0.5) ) +
  scale_linetype_manual(values = c("dotted", 
                                   "solid", 
                                   "solid")) +
  labs(colour="Lake", linetype = "Lake", x = "Littoral reliance") + 
  scale_x_continuous(name = "Littoral reliance", limits = c(-0.75,1.75), 
                     breaks = c(0.0,0.5,1.0)) +
  guides(size = FALSE) +
  guides(fill=TRUE) +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(override.aes = list(size = c(2,2,1))))



p2

ggsave("figs/density_littoralreliance_indlake.png",p1,  width = 10, height = 6, units = "in" )

ggsave("figs/density_littoralreliance_indlake_BWsafe.png",p2,  width = 7, height = 4, units = "in" )


