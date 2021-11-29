library(dplyr)
library(ggplot2)
library(tidyverse)
library(vcd)
library(RColorBrewer)


#filter on data_p when making function
#make it a function
plot_stackedbar<-function(year = 0){
  if(year == 0){
  data_stack <- data_p  %>% 
    group_by(Educational.Attainment, Personal.Income) %>%
    summarise(Freq = sum(Population.Count)) %>% ungroup()
  }else{
  data_stack <- data_p %>% filter(Year == year) %>% group_by(Educational.Attainment, Personal.Income) %>%
  summarise(Freq = sum(Population.Count)) %>% ungroup()
  }
colors <- rev(brewer.pal(8,'Blues'))
mosaic(Personal.Income ~ Educational.Attainment ,direction = c('v','h'), 
       data_stack,  highlighting_fill = colors,
       labeling = labeling_border(tl_labels = c(TRUE,TRUE), rot_labels = c(5,0,0,60)))
}

plot_stackedbar()
#draw supposed graph
data_supposed <- data_stack %>% mutate(all = sum(Freq)) %>%
  group_by(Personal.Income) %>% mutate(total = sum(Freq)) %>% ungroup %>%
  group_by(Educational.Attainment) %>% mutate(ratio = sum(Freq)/all) %>% ungroup %>%
  summarise(Educational.Attainment = Educational.Attainment, Personal.Income = Personal.Income, 
            supposed = ratio * total)
mosaic(Personal.Income ~ Educational.Attainment ,direction = c('v','h'), 
       data_supposed,  highlighting_fill = colors,
       labeling = labeling_border(tl_labels = c(TRUE,TRUE), rot_labels = c(5,0,0,60)))

#Over years
plot_stackedbar(2008)
plot_stackedbar(2009)
plot_stackedbar(2010)
plot_stackedbar(2011)
plot_stackedbar(2012)
plot_stackedbar(2013)
plot_stackedbar(2014)

