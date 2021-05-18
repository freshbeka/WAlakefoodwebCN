### Cut text I'm afraid to 100% delete

###  Delete####

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
#End delete####



### Old way of facet wrap #

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
#### End old way of facet wrap ###