#=== === === === === === === ===
# Script started by Rebekah Stiling July 2021, stilir@uw.edu
# This script takes all of the isotope data from several years of Pine lake sampling and plots and evaluates it to guess what samples from 2013 are which species.
#=== === === === === === === ===


library(readxl) #To read the excel file 
library(tidyverse) # To organize and plot
library(RColorBrewer) # To change colors

# Review the sheet names in order to select the correct one.  
excel_sheets("data/WA_Lake_SI_Data2021.xlsx")

## read in the worksheet with the isotope data
original <- read_excel("data/WA_Lake_SI_Data2021.xlsx", sheet = "SI_Data")

#give it a new name so I can reference the original if needed
SIdata <- original 
head(SIdata) #check info.

SIdata_tidy <- SIdata %>% 
  unite(col="Lake_Year", c(Lake, Year), sep="_", remove = FALSE) #add new column called Lake_Year composed of the info in the lake and year columns.

##Now, I only want the Pine Lake data.
pine <-SIdata_tidy %>% filter(Lake == "Pine")

# Peek at the Pine_2013 NAs 
pine %>% filter(Lake_Year == "Pine_2013") %>% filter(is.na(Identity))

## Oops! I need the terms to match between Pine 2013 and the rest of the pine lake data. Easy fix.

pine$Group2.0 <- pine$Group #Create new column in order to preserve the original information.
pine<-mutate(pine,  Group2.0 =  if_else(Group2.0 == "algal scrape", "Algae",
                                  if_else(Group2.0 == "invertebrates", "Aquatic invertebrate",
                                    if_else(Group2.0 == "fish muscle", "Fish",
                                      if_else(Group2.0 == "snail muscle", "Gastropod",
                                        if_else(Group2.0 == "crayfish muscle", "Crayfish",
                                          if_else(Group2.0 == "terrestrial plant", "Terrestrial plant",
                                            if_else(Group2.0 == "macrophyte", "Aquatic plant",
                                              Group2.0))))))))

#I'm also going to change the adu salmoides to regular. Hopefully that doesn't matter... (check with JDO)
pine<-mutate(pine, Identity =  if_else(Identity == "Micropterus salmoides adu", "Micropterus salmoides",Identity))

#Now I filter for the categories that I know I want to plot, reducing the data I'm tracking.
keeplist <- c("Aquatic invertebrate", "Aquatic plant", "Crayfish", "Fish", "Gastropod", "Terrestrial plant") 
mystpine <- pine %>% filter((Group2.0 %in% keeplist)) 
# Also, there is only one Ameiurus nebulosus captured ever (in 2011), so I'm dropping it.
mystpine<-mystpine %>% filter(is.na(Identity)|Identity != "Ameiurus nebulosus") ### NOTE!! Filter with != drops NA's so I had to say "is.na OR not A.n" (how very annoying!) 

#plot the groups of data by lake, to confirm the data is consistent between groups.
p1 <-ggplot(data = mystpine, aes(x = d13C, y = d15N, color = Lake_Year)) + 
  geom_point()+ 
  theme_bw() +
  facet_wrap(~ Group2.0)
p1
ggsave("figs/Pine2013Mystery_overview.png",p1,  width = 7, height = 5, units = "in" )
# The good news is that nothing seems crazy or are major outliers for their groups.

## Now I'll do this Group by Group. The first category is fish.
fish <- mystpine %>% filter(Group2.0 == "Fish")
sp <-ggplot(data = fish, 
           aes(x = d13C, y = d15N, color = Identity)) + 
  geom_point()+ 
  theme_bw()
sp
## Ok, that is a lot of fish, and they are in a cloud. We may be able to separate bass from pumpkinseed though...Hmmm.

ggplot(data = fish, aes(x = d13C, y = d15N, color = Lake_Year)) + 
  geom_point()+ 
  theme_bw()

## Now that I've looked at this data, this is going to be tough... 
## Let me grab the fish from the other lakes, then find some species means, and add those to the plot
species_mu <-fish %>% filter(!is.na(Identity)) %>% 
  group_by(Identity) %>% 
  summarize(d13C = mean(d13C),
            d15N = mean(d15N))

mean.sp<-sp + geom_point(data = species_mu, cex = 5) #This line adds the summary data from above, to the species plot
mean.sp
##So, what is the tally of species captured from Pine? 
#what are the counts, through time that each species have?

counts <-fish %>% group_by(Identity, Year) %>% summarise(count = n())
# I want to plot the counts through time, is there a trend?
ggplot(data = counts %>% filter(!is.na(Identity)), aes(x = Year, y = count, color = Identity, fill = Identity)) +
  geom_line(alpha = 0.25) +
  geom_point() +
  theme_bw()
## Ooops, some points are the same (10), I'll jitter a bit

ggplot(data = counts %>% filter(!is.na(Identity)), aes(x = Year, y = count, color = Identity)) +
  geom_line(alpha = 0.25) +
  geom_point(size = 3, 
             position = position_jitter(width = 0.5,
                                        height = 0.00,
                                        seed = 7384)) +
  theme_bw() +
  scale_color_brewer(palette = "Set2")

ggplot(data = counts %>% filter(!is.na(Identity)), aes(x = Year, y = count, fill = Identity)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme_bw() +
  scale_fill_brewer(palette = "Set2")
## I suppose these are not surveys targeted at population abundance. I don't know if the lack of species or changes in counts mean anything. JDO will know.


# What if I put a hull around each species, then we tried to see where the groups were?

# Extract a vector for each species that serves as the convex hull 
lg<-mystpine %>% filter(Group2.0 == "Fish") %>% filter(Identity == "Lepomis gibbosus")
lgChull<-lg[chull(lg$d13C, lg$d15N),]
ms<-mystpine %>% filter(Group2.0 == "Fish") %>% filter(Identity == "Micropterus salmoides")
msChull<-ms[chull(ms$d13C, ms$d15N),]
md<-mystpine %>% filter(Group2.0 == "Fish") %>% filter(Identity == "Micropterus dolomieu")
mdChull<-md[chull(md$d13C, md$d15N),]
om<-mystpine %>% filter(Group2.0 == "Fish") %>% filter(Identity == "Oncorhynchus mykiss")
omChull<-om[chull(om$d13C, om$d15N),]
pf<-mystpine %>% filter(Group2.0 == "Fish") %>% filter(Identity == "Perca flavescens")
pfChull<-pf[chull(pf$d13C, pf$d15N),]

speciesChull <- bind_rows(lgChull,msChull,mdChull,omChull,pfChull)

# all individual samples, plotted with a convex hull around the values for different species

hulls<-mean.sp + geom_polygon(data=speciesChull, 
               aes(x=d13C, y=d15N, 
                   group = Identity,  
                   fill = Identity), 
               alpha = 0.2,
               linetype = "blank")
hulls

# Then, on top of this, what if I turn each NA into a ## then we can plot just the numbers and id what is what. Trying this...

IDNAs <- mystpine %>% filter(Group2.0 == "Fish") %>% filter(is.na(Identity))
IDNAs$ID <- 1:nrow(IDNAs)

nums <-hulls + geom_text(data = IDNAs, 
                  color = "black",
                  size = 5,
                  aes(x=d13C, y=d15N, 
                      label = ID))
nums
ggsave("figs/Pine2013Mystery_hulls&numbers.png",nums,  width = 7, height = 5, units = "in" )
## This is a plot we could work with, or at least start working with. I'll stop here.
