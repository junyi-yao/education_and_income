library(ggmosaic)
library(dplyr)
library(tidyverse)
library(RColorBrewer)

data_facet <- data_p %>% group_by(Gender, Educational.Attainment, Personal.Income) %>%
  summarise(Freq = sum(Population.Count), Year =Year) %>% ungroup()

colors <- rev(brewer.pal(8,'Blues'))
#mosaic(Personal.Income ~ Gender ,direction = c('v','h'), 
       #data_facet,  highlighting_fill = colors,
       #labeling = labeling_border(tl_labels = c(TRUE,TRUE), rot_labels = c(5,0,0,60)))
#Notice that once library ggmosaic is loaded, mosaic can no longer be used
#In ggmosaic, y is in reverse order#in ggmosaic, y is in reverse order. We change the level order before we make a plot
data_facet$Personal.Income<- fct_rev(data_facet$Personal.Income)
ggplot(data_facet)+
  geom_mosaic(aes(weight = Freq, x = product(Gender), fill = Personal.Income))+
  scale_fill_manual("legend", values = c("[75k,inf)" = colors[1], "[50k,75k)" = colors[2], 
                                         "[35k,50k)" = colors[3], "[25k,35k)" = colors[4], 
                                         "[15k,25k)" = colors[5], "[10k,15k)" = colors[6], 
                                         "[5k,10k)" = colors[7], "No Income" = colors[8]))+
  facet_wrap(vars(Educational.Attainment))


#Is the gap between rich and poor affected by gender?
ggplot(data_facet)+
  geom_mosaic(aes(weight = Freq, x = product(Educational.Attainment), fill = Personal.Income))+
  scale_fill_manual("legend", values = c("[75k,inf)" = colors[1], "[50k,75k)" = colors[2], 
                                         "[35k,50k)" = colors[3], "[25k,35k)" = colors[4], 
                                         "[15k,25k)" = colors[5], "[10k,15k)" = colors[6], 
                                         "[5k,10k)" = colors[7], "No Income" = colors[8]))+
  facet_wrap(vars(Gender))

#Or maybe we don't need include education factor? In the stackedbar.R
#What can we say?


#Over years
ggplot(data_facet)+
  geom_mosaic(aes(weight = Freq, x = product(Educational.Attainment), fill = Personal.Income))+
  scale_fill_manual("legend", values = c("[75k,inf)" = colors[1], "[50k,75k)" = colors[2], 
                                         "[35k,50k)" = colors[3], "[25k,35k)" = colors[4], 
                                         "[15k,25k)" = colors[5], "[10k,15k)" = colors[6], 
                                         "[5k,10k)" = colors[7], "No Income" = colors[8]))+
  facet_grid(rows = vars(Gender), cols = vars(Year))

