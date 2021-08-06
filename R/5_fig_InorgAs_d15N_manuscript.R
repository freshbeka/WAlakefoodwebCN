
# Arsenic and N manuscript plot

d15N_As_tidy <-read_csv("data/d15NAs_data.csv" ) 

library(ggrepel) #for labeling species


## Plotting means only
# Second, record group data means as species, lake combos
gd <- d15N_As_tidy %>% 
  group_by(species, lake) %>% 
  summarise(percent_inorganic = mean(percent_inorganic),
            d15n_air  = mean(d15n_air),
            count = n())


p1<-ggplot(data = gd, 
       aes(x = percent_inorganic, 
           y = d15n_air, 
           color = lake)) + #,
 #          shape = species)) + 
  geom_point(size = 4) + 
  geom_smooth(method = "lm", 
              aes(group = lake), 
              linetype="dashed",
              se = FALSE) +
  geom_label_repel(fill = alpha(c("white"),0.8),
                   aes(label=species),
                   min.segment.length = 0)+
#  scale_shape_manual(values=c(3,7,8,15:18,25)) + 
  scale_color_manual(values=c("#e66101",
                              "#2c7bb6" )) + #PKS
  theme_bw() +
  labs(x = "% inorganic As",
       y = expression(italic(delta)^15*N), 
       color = "Lake",
       shape = "Species") +
  theme(legend.position = c(0.87, 0.90))

p1
ggsave("figs/InOrgAs vs d15N.png",p1,  width = 6, height = 5, units = "in" )

## Plotting all points 

p2<-ggplot(data = d15N_As_tidy, 
       aes(x = percent_inorganic, 
           y = d15n_air, 
           color = lake,
           fill = lake,
           shape = species,
           group = lake)) + 
  geom_smooth(method = "lm", alpha = 0.1) +
  geom_point() + 
  geom_point(data=gd, size = 4) +
  scale_shape_manual(values=c(3,7,8,15:18,25)) + 
  scale_color_manual(values=c("#e66101",
                              "#2c7bb6" )) + #PKS
  scale_fill_manual(values=c("#e66101",
                              "#2c7bb6" )) + 
  theme_bw() +
  labs(x = "% inorganic As",
       y = expression(italic(delta)^15*N), 
       color = "Lake",
       shape = "Species") 
p2
ggsave("figs/InOrgAs vs d15N_allpts&means.png",p2,  width = 8, height = 5, units = "in" )


# establish levels so the plotting order is as desired
d15N_As_tidy$species <- factor(d15N_As_tidy$species , 
                               levels=c("pumpkinseed (Lepomis gibbosus)",
                                                     "bluegill (Lepomis macrochirus)",
                                                     "Chinese mystery",
                                                     "chironomid",
                                                     "zooplankton",
                                                     "phytoplankton",
                                                     "periphyton (surface)",
                                                     "macrophyte"))


p3<-ggplot(data = d15N_As_tidy, 
           aes(x = percent_inorganic, 
               y = d15n_air)) + 
  geom_point(aes(shape = species, 
                 color = lake, 
                 fill = lake),
             size = 3) + 
  geom_smooth(method = "lm", 
              alpha = 0.1, 
              aes(group = lake,
                  fill = lake,
                  color = lake),
              level=0.90) +
  scale_shape_manual(values=c(16,17,15,8, 3, 9, 7, 18)) +
  scale_color_manual(values=c("#e66101",
                              "#2c7bb6" )) + #PKS
  scale_fill_manual(values=c("#e66101",
                             "#2c7bb6"), 
                    guide = 'none') + 
  theme_bw() +
  labs(x = "% inorganic As",
       y = expression(italic(delta)^15*N), 
       color = "Lake",
       shape = "Species") 
p3
ggsave("figs/InOrgAs vs d15N_allpts.png",p3,  width = 8, height = 5, units = "in" )


