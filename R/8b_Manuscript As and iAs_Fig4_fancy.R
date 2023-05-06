# Script for figure 4

##First, read in packages that are needed
library(readxl) #To read the excel file 
library(tidyverse) # to plot
library(ggpmisc) #for adding slope, R2, and p
library(ggh4x) #for modifying the layout


# Review the sheet names in order to select the correct one.  
excel_sheets("data/TrophicTransfer_R_Reformatted_v3.xlsx")

## read in the worksheet with the isotope data
reformat <- read_excel("data/TrophicTransfer_R_Reformatted_v3.xlsx", #name and location of the data file
                       sheet = "Reformat", #name of the sheet with the data
                       na = "NA") #which expression is used in the datasheet to represent "NA"

#all organisms
long<-reformat  %>%  select(
  littoral_sediment_totAs_mean,
  periphyton_iAs_mean,
  phytoplankton_iAs_mean,
  zooplankton_iAs_mean,
  snail_iAs_mean,
  sunfish_iAs_mean)  %>% 
  pivot_longer(periphyton_iAs_mean:sunfish_iAs_mean, names_to = "Taxa", values_to = "organism_iAs")

#drop the unit description
long$Taxa <- str_replace_all(long$Taxa, "_iAs_mean", "")

#order the organism factors so the plots will be in the desired order
# long$Taxa_f = factor(long$Taxa,
#                      levels=c('sunfish',
#                               'zooplankton',
#                               'snail',
#                               'phytoplankton',
#                               'periphyton'),
#                      labels = c('(e) sunfish',
#                                 '(c) zooplankton',
#                                 '(d) snail',
#                                 '(a) phytoplankton',
#                                 '(b) periphyton'))

long$Taxa_f = factor(long$Taxa,
                     levels=c('periphyton',
                              "phytoplankton",
                              'snail',
                              'zooplankton',
                              'sunfish'),
                     labels = c('(e) periphyton',
                                "(d) phytoplankton",
                                '(c) snail',
                                '(b) zooplankton',
                                '(a) sunfish'))



## Manuscript figure 4 ####
#https://stackoverflow.com/questions/7549694/add-regression-line-equation-and-r2-on-graph (mainly referenced the long answer provided by package author)
## Now, in order to center the graph, I needed to work outside the normal ggplot capabilities. 

#The package ggh4x allowed for this 
#https://teunbrand.github.io/ggh4x/articles/Facets.html

p<-ggplot(long, aes(x=littoral_sediment_totAs_mean, 
                 y = organism_iAs)) +
  geom_point()+
  geom_smooth(method = "lm", color = "black") +
  theme_bw() +
  theme(text=element_text(size=12,  
                          family="serif")) +
  stat_poly_eq(use_label("eq"),
               size = 3,
               family="serif") + 
  stat_poly_eq(use_label(c("R2", "p")),
               size = 3,
               rr.digits = 3,
               label.y = .85,
               family="serif") +
  labs(x = expression(paste(mu, "g As ", g^-1, sep="")),
       y = expression(paste(mu, "g iAs ", g^-1, sep=""))) +
  geom_hline(data = data.frame(yint=.035,Taxa_f="(a) sunfish"), 
               aes(yintercept = yint), 
               linetype = "dotted") +
  facet_wrap(vars(Taxa_f), 
             scales = "free_y",
             ncol = 2) #this makes the top corner empty instead of the bottom

p

design <- "
  #AA#
  BBCC
  DDEE
"
# 
# #same result
# design <- matrix(c(7,1,1,6,
#                    2,2,3,3,
#                    4,4,5,5),
#                  byrow=TRUE,
#                  nrow = 3)


p
p <- p + facet_manual(vars(Taxa_f), 
                 scales = "free_y",
                 design = design) +
  force_panelsizes(cols = c(1, .75), 
                   rows = c(1.5),
                   respect = TRUE) +
  theme (panel.spacing.y = unit (1, "lines"))

p


ggsave("figs/TotAsvTotinAs_Fig4_center.png", width = 6, height = 7, units = "in" )
#This is not looking good. The two columns are not quite the same widths and the numbers are slightly overlapping.


