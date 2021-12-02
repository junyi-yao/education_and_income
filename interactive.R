library(plotly)
library(ggplot2)
library(rCharts)
#population proportion for salary level for different year
#lineplot
#interactive in showing the exact data when mouse is on points
data_inter<-data_p %>% group_by(Year, Personal.Income) %>%  #if we study gender, add Gender
  mutate(total = sum(Population.Count)) %>% group_by(Year) %>% 
  mutate(year_total = sum(Population.Count)) %>% ungroup() %>%
  summarise(Year = Year, Personal.Income = Personal.Income, ratio = total/year_total) %>% 
  unique()

fig1 <- ggplot(data_inter) + geom_point(aes(x = Year, y = ratio, color = Personal.Income))+
  geom_line(aes(x = Year, y = ratio, group = Personal.Income, color = Personal.Income))

ggplotly(fig1)

data_inter_gender<-data_p %>% group_by(Gender,Year, Personal.Income) %>%  #if we study gender, add Gender
  mutate(total = sum(Population.Count)) %>% group_by(Gender, Year) %>% 
  mutate(year_total_G = sum(Population.Count)) %>% ungroup() %>%
  summarise(Year = Year, Gender = Gender, Personal.Income = Personal.Income, ratio = total/year_total_G) %>% 
  unique()

fig2 <- ggplot(data_inter_gender) + geom_point(aes(x = Year, y = ratio, color = Personal.Income))+
  geom_line(aes(x = Year, y = ratio, group = Personal.Income, color = Personal.Income))+facet_grid(~Gender)

ggplotly(fig2)


#This part!!!(not included in 06-interactive)
#Choices for users, bar plots
nPlot(ratio ~ Year, group = 'Personal.Income', data = data_inter, type = 'multiBarChart')
#total
n1<-nPlot(ratio ~ Year, group = 'Personal.Income', data = data_inter, type = 'multiBarChart')
n1 $ yAxis(tickFormat = "#! function(d) {return  d3.format(',.2f')(d)} !#")
n1

#draw different gender separately
#male
data_inter_m <- data_inter_gender %>% filter(Gender == 'Male')
nPlot(ratio ~ Year, group = 'Personal.Income', data = data_inter_m, type = 'multiBarChart')
nm<-nPlot(ratio ~ Year, group = 'Personal.Income', data = data_inter_m, type = 'multiBarChart')
nm $ yAxis(tickFormat = "#! function(d) {return  d3.format(',.2f')(d)} !#")
nm
#female
data_inter_f <- data_inter_gender %>% filter(Gender == 'Female')
nPlot(ratio ~ Year, group = 'Personal.Income', data = data_inter_m, type = 'multiBarChart')
nf<-nPlot(ratio ~ Year, group = 'Personal.Income', data = data_inter_f, type = 'multiBarChart')
nf $ yAxis(tickFormat = "#! function(d) {return  d3.format(',.2f')(d)} !#")
nf

