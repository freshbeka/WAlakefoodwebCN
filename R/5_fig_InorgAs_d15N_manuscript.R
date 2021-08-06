
# Arsenic and N manuscript plot

d15N_As_tidy <-read_csv("data/d15NAs_data.csv" ) 

library(ggrepel) #for labeling species



# Second, record group data means as species, lake combos
gd <- d15N_As_tidy %>% 
  group_by(species, lake) %>% 
  summarise(percent_inorganic = mean(percent_inorganic),
            d15n_air  = mean(d15n_air),
            count = n())


ggplot(data = gd, 
       aes(x = percent_inorganic, 
           y = d15n_air, 
           color = lake)) + #,
 #          shape = species)) + 
  geom_point(size = 4) + 
  geom_smooth(method = "lm", aes(group = lake), se = FALSE) +
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
       shape = "Species") 


