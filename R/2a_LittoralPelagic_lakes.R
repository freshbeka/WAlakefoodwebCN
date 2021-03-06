## Gastropod vs zooplankton when available.

## Plan here, search for the lakes that have "end member data" and quickly calculate pelagic vs littoral reliance by the pumpkinseed. Then be strategic about getting endmembers for the remaining lakes that don't have zoops and snails.

library(tidyverse) # To organize and plot
library(ggrepel) #for labeling species
library(patchwork) #for multi-panel plot
library(cowplot) #draw image function


# read in the tidy data format of Julian and Bothell combined
SIdata_subset <- read_csv("data/SI_subset.csv")


##get the means for each identity
SImeans <- SIdata_subset %>% 
  group_by(Lake_Year, Identity, Group) %>% #separate by lake and species and group
  dplyr::summarise(d13C_mean = mean(d13C), #obtain mean and SD for each lake/species
            d13C_sd = sd(d13C),
            d15N_mean = mean(d15N),
            d15N_sd = sd(d15N)) %>% ungroup()  ## Ungroup is key, otherwise the mixing model will not work because "select(i)" cannot distinguish rows

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

#which lakes do I not have zoops and snails for?
SImeans %>% select(Lake_Year) %>% distinct() %>% filter(!(Lake_Year %in% End.memberlakes)) 
## I will need to estimate or use some other data for endmembers in these 6 lakes.

# calculate the littoral reliance value for each fish species ####
#create empty dataframe
df <- tibble(
  Lake_Year = character(),
  Identity = character(),
  littoral.reliance = numeric())

j<-7
i<-3

for (j in 1:length(End.memberlakes)){   
  data <- SImeans %>% filter(Lake_Year == End.memberlakes[j])
  
  for (i in 1:nrow(data %>% filter(Group == "Fish"))){   
    Cfish <- data %>% filter(Group == "Fish") %>% slice(i) %>% pull(d13C_mean) ## all fish
    lit <-data %>% filter(Group == "Gastropod") %>% pull(d13C_mean) %>% mean() ## I determine littoral, and I take the mean incase there are more than 1 value.
    pel <-data %>% filter(Group == "Zooplankton") %>%  pull(d13C_mean) %>% mean()## I determine pelagic. I take the mean incase there are more than 1 value.
    a <-data %>% filter(Group == "Fish") %>% slice(i) %>% pull(Lake_Year)
    b <-data %>% filter(Group == "Fish") %>% slice(i) %>% pull(Identity)
    c <- (Cfish - pel)/(lit - pel) ##I calculate the reliance as if I didn't have end-member data.
    df <- df %>% add_row(Lake_Year =a,
                         Identity = b,
                         littoral.reliance = c)
  }
  
}

reliance <- df


#This plot needs cleaning up.
# Drop Cottidae and Ambloplites rupestris
# Color the fish to match the other plot
# Expand the Z axis to go to  0.00 


reliance<-reliance %>% filter(!(Identity %in% c("Cottidae spp.","Ambloplites rupestris")))

reliance$common <- reliance$Identity

reliance<-mutate(reliance, common = 
                   if_else(common == "Micropterus salmoides", "largemouth bass (Micropterus salmoides)", 
                           if_else(common == "Perca flavescens","yellow perch (Perca flavescens)", 
                                   if_else(common == "Lepomis gibbosus","pumpkinseed (Lepomis gibbosus)", 
                                           if_else(common == "Lepomis macrochirus","bluegill (Lepomis macrochirus)", common)))))


reliance$Identity <- factor(reliance$Identity , levels=c("Micropterus salmoides",
                                                         "Perca flavescens",
                                                         "Lepomis macrochirus",
                                                         "Lepomis gibbosus"))

reliance$common <- factor(reliance$common , levels=c("largemouth bass (Micropterus salmoides)",
                                                         "yellow perch (Perca flavescens)",
                                                         "Lepomis macrochirus","bluegill (Lepomis macrochirus)",
                                                         "pumpkinseed (Lepomis gibbosus)"))

reliance$littoral.reliance.percent<-reliance$littoral.reliance*100


p1 <- ggplot(data = reliance, 
             aes(x = littoral.reliance.percent, y = common, color = common, label = Lake_Year)) +
  geom_boxplot() + 
  geom_point() + 
  geom_label_repel(fill = alpha(c("white"),0.8),
                   aes(label=ifelse(Lake_Year == "Angle_2019", "Angle Lake",
                                   ifelse(Lake_Year == "Killarney_2019", "Lake Killarney",''))),
                  box.padding = 0.5, 
                  min.segment.length = 0) +
  theme_bw() +
  scale_color_manual(values=c("black", # M. Salmoides
                              "black", # Perch
                              "#2c7bb6", #BlueGill
                              "#e66101")) + #PKS
  scale_x_continuous(name = "littoral resource use (%)", limits = c(0,110), 
                     breaks = c(0.0,20,40, 60, 80, 100)) + 
  theme(axis.text.y = element_text(color = c("black","black","#2c7bb6","#e66101"))) +
  labs(y = "Species") +
  theme(legend.position = "none")

p1
p.annotate<-ggdraw(p1) + 
  draw_image(image='data/ps.jpg', x = 0.17, y = 0.7, 
             width=0.15, height=0.15) +
  draw_image(image='data/bg.jpg', x = 0.17, y = 0.47, 
             width=0.15, height=0.15) +
  draw_image(image='data/yp.jpg', x = 0.17, y = 0.27, 
             width=0.15, height=0.15) +
  draw_image(image='data/lmb.jpg', x = 0.17, y = 0.07, 
             width=0.15, height=0.15)
p.annotate
ggsave("figs/boxplot_littoralreliance_species_ASLO.png",p.annotate,  width = 7, height = 5, units = "in" )

##How many additional lakes:

