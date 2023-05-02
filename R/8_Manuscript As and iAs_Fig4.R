# Script for figure 4

##First, read in packages that are needed
library(readxl) #To read the excel file 
library(tidyverse) # to plot
library(ggpubr) #to add slop and R2 equations
library(patchwork)

# Review the sheet names in order to select the correct one.  
excel_sheets("data/TrophicTransfer_R_Reformatted_v3.xlsx")

## read in the worksheet with the isotope data
reformat <- read_excel("data/TrophicTransfer_R_Reformatted_v3.xlsx", #name and location of the data file
                       sheet = "Reformat", #name of the sheet with the data
                       na = "NA") #which expression is used in the datasheet to represent "NA"


#all organisms
long<-reformat |> select(
  littoral_sediment_totAs_mean,
  periphyton_iAs_mean,
  phytoplankton_iAs_mean,
  zooplankton_iAs_mean,
  snail_iAs_mean,
  sunfish_iAs_mean) |>
  pivot_longer(periphyton_iAs_mean:sunfish_iAs_mean, names_to = "Taxa", values_to = "organism_iAs")

#drop the unit description
long$Taxa <- str_replace_all(long$Taxa, "_iAs_mean", "")

#order the orgs for factors
long$Taxa_f = factor(long$Taxa, 
                        levels=c('phytoplankton',
                                 "periphyton",
                                 'zooplankton',
                                 'snail',
                                 'sunfish'))



# 
# Pump <- "#e66101"
# Blue <- "#2c7bb6"
# zoo <- "#4d4d4d" #"#bababa"  #"#b35806"
# snl <- "#9970ab" #"#4d4d4d"
# phyt <- "#66bd63"
# peri <- "#b8e186"
# chron <- "#d73027"
# macro <- "#de77ae"
                
## Manuscript figure 4 ####
ggplot(long, aes(x=littoral_sediment_totAs_mean, 
                 y = organism_iAs
                 # , 
                 # color = Taxa, 
                 # fill = Taxa
                 )) +
  geom_point()+
  geom_smooth(method = "lm", color = "black") +
  theme_bw() +
  theme(text=element_text(size=12,  
                          family="serif")) +
  # scale_color_manual(values=c(peri,
  #                             phyt,
  #                             snl,
  #                             Pump,
  #                             zoo),
  #                    guide = "none")+
  # scale_fill_manual(values=c(peri,
  #                            phyt,
  #                            snl,
  #                            Pump,
  #                            zoo),
  #                   guide="none")+
  stat_regline_equation(aes(label = paste(..eq.label.., ..rr.label.., sep = "*`,`~")),
                        label.y.npc = 1,
                        label.x.npc = "left",
                        show.legend = FALSE) +
  labs(x = expression(paste("Total As in sediment (",mu, "g ", g^-1,")", sep="")),
       y = expression(paste("inorganic As ( ",mu, "g ", g^-1,")", sep=""))) +
  facet_wrap(vars(Taxa_f), 
             scales = "free_y",
             ncol = 2,
             as.table = F) 
# +
#   theme(legend.position = c(.95, .75),
#         legend.justification = c(1, 0))

expression(paste("Total As (",mu,"g ", g^-1,")", sep=""))
ggsave("figs/TotAsvTotinAs_Fig4.png", width = 6, height = 7, units = "in" )

##struggling with adding equation. 
              
p1 <- ggplot(reformat, aes(x=littoral_sediment_totAs_mean,
                           y = periphyton_iAs_mean,
                           color = peri,
                           fill = peri)) +
  geom_point()+
  geom_smooth(method = "lm") +
  theme_bw() +
  scale_color_manual(values=c(peri))+
  scale_fill_manual(values=c(peri))+
  labs(x = expression(paste("Total As in sediment (",mu, g, "/", g,")", sep="")),
       y = expression(paste("% inorganic As (",mu, g, "/", g,")", sep="")))           
p1          

p2 <- ggplot(reformat, aes(x=littoral_sediment_totAs_mean,
                           y = phytoplankton_iAs_mean,
                           color = phyt,
                           fill = phyt)) +
  geom_point()+
  geom_smooth(method = "lm") +
  theme_bw() +
  scale_color_manual(values=c(phyt))+
  scale_fill_manual(values=c(phyt))+
  labs(x = expression(paste("Total As in sediment (",mu, g, "/", g,")", sep="")),
       y = expression(paste("% inorganic As (",mu, g, "/", g,")", sep="")))           
p2      
              
p1+p2 + plot_layout(guides = "collect")
## practice figs    


        
                
ggplot(long, aes(x=littoral_sediment_totAs_mean, 
                 y = organism_iAs, 
                 color = species, 
                 fill = species)) +
  geom_point()+
  geom_smooth(method = "lm", 
              aes(group = species)) +
  theme_bw() +
  scale_color_manual(values=c(peri,
                              phyt,
                              snl,
                              Pump,
                              zoo))+
  scale_fill_manual(values=c(peri,
                             phyt,
                             snl,
                             Pump,
                             zoo))+
  labs(x = expression(paste(
    "Total As in sediment (",
    mu, g, "/", g,
    ")", sep="")),
    y = expression(paste(
      "% inorganic As (",
      mu, g, "/", g,
      ")", sep="")))              
              
ggsave("figs/predict_iAs_all.png", width = 7.5, height = 4.5, units = "in" )
              
ggplot(long %>% filter(species != "periphyton_iAs_mean"), aes(x=littoral_sediment_totAs_mean , y = organism_iAs, color = species, fill = species)) +
                geom_point()+
                geom_smooth(method = "lm", 
                            aes(group = species)) +
                theme_bw() +
                scale_color_manual(values=c(phyt,
                                            snl,
                                            Pump,
                                            zoo))+
                scale_fill_manual(values=c(phyt,
                                           snl,
                                           Pump,
                                           zoo))+
                labs(x = expression(paste(
                  "Total As in sediment (",
                  mu, g, "/", g,
                  ")", sep="")),
                  y = expression(paste(
                    "% inorganic As (",
                    mu, g, "/", g,
                    ")", sep="")))
              
ggsave("figs/predict_iAs_noperi.png", width = 7.5, height = 4.5, units = "in" )
              
