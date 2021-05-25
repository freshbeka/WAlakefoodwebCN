
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

p1 <- ggplot(data = reliance, aes(x = littoral.reliance, y = Identity)) +
  geom_boxplot() + 
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

j<-7
i<-3

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
L.macrochirus <-individual.reliance %>% filter(Identity == "Lepomis macrochirus")

p2 <- ggplot(data = L.gibbosus, aes(x = littoral.reliance, y = Identity)) +
  geom_boxplot()  + 
  theme_minimal()