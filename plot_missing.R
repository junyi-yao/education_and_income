library(tidyverse)
library(patchwork)
library(ggnewscale)


plot_missing<- function(dataset, percent){
missing_patterns <- data.frame(is.na(dataset)) %>%
  group_by_all() %>%
  count(name = "count", sort = TRUE) %>%
  ungroup()

missing_patterns_large <- missing_patterns[,-length(missing_patterns)]
order <- sort(colSums(missing_patterns_large), decreasing = TRUE)
missing_patterns_tidy <- missing_patterns_large %>% rownames_to_column("pattern") %>%
  gather(variable, missing, -pattern)

missing_patterns_tidy$missing<-as.numeric(missing_patterns_tidy$missing)

missing_patterns_tidy$variable <- factor(missing_patterns_tidy$variable,levels = names(order))
allexist <- which(rowSums(missing_patterns_large)==0)
missing_patterns_tidy[missing_patterns_tidy$pattern==allexist,]$missing<-2

middle<-ceiling(length(levels(missing_patterns_tidy$variable))/2)
#y-axis need to be sorted by numbers, not by character (string of numbers)
missing_patterns_tidy$pattern<- factor(missing_patterns_tidy$pattern,
                                       levels = unique(sort(as.numeric(missing_patterns_tidy$pattern))))

p1<-ggplot(data = missing_patterns_tidy, 
           mapping = aes(x = fct_reorder(variable, -missing, sum), y = fct_rev(pattern), fill = factor(missing))) +
  geom_tile(color = "white")+ 
  scale_fill_manual(values = c(alpha('grey', 0.5), alpha('#B79ADE',1), alpha('grey',1)))+
  xlab('variable')+ylab('missing pattern')+
  annotate("text",x = levels(missing_patterns_tidy$variable)[middle] ,y = as.character(allexist),label = "complete cases")+
  theme_classic() + guides(fill = "none")


NA_nums<-colSums(data.frame(is.na(dataset)))
upper<-data.frame(NA_nums) %>% rownames_to_column('variable')
upper<-upper[order(upper[,2], decreasing =TRUE),]
#A function for getting percentage
#Input: an array with frequency
#Output: an array with percentage
get_percentage<-function(arry){
  return (arry/nrow(dataset)*100)
}

#For p2,p3
#If percent is True, draw the percentage version. Otherwise, draw the count version
if (percent == FALSE){
p2<-ggplot(upper, aes(x = fct_inorder(variable), y=NA_nums))+geom_col(fill = 'blue', alpha = 0.3)+
  scale_y_continuous(breaks = seq(0,max(upper$NA_nums),10))+
  theme_bw()+theme(panel.grid.major.x=element_blank())+
  xlab('')+ylab('num rows missing:')}
else{
  p2<-ggplot(upper, aes(x = fct_inorder(variable), y=get_percentage(NA_nums)))+geom_col(fill = 'blue', alpha = 0.3)+
    theme_bw()+theme(panel.grid.major.x=element_blank())+
    ylim(0,100)+xlab('')+ylab('% rows missing:')}

missing_patterns_right<-missing_patterns %>% rownames_to_column("pattern")
right <- missing_patterns_right[c('pattern','count')]
#set alpha
right$special <- 0
right[right$pattern == allexist,]$special <- 1
if (percent == FALSE){
p3<-ggplot(right)+
  geom_col(mapping = aes(x = fct_rev(fct_inorder(pattern)), y = count, alpha = factor(special)), 
           fill= 'blue')+
  scale_alpha_manual(values = c(0.3,0.5))+
  theme_bw()+theme(panel.grid.major.y=element_blank())+
  theme(legend.position = "none")+
  ylab('row count')+xlab('')+coord_flip()
}else{
p3<-ggplot(right)+
    geom_col(mapping = aes(x = fct_rev(fct_inorder(pattern)), y = get_percentage(count), alpha = factor(special)), 
             fill= 'blue')+
    scale_alpha_manual(values = c(0.3,0.5))+
    theme(legend.position = "none")+
    ylim(0,100)+
    theme_bw()+theme(panel.grid.major.y=element_blank())+
    theme(legend.position = "none")+
    ylab('% count')+xlab('')+coord_flip()
}

return (p2 + plot_spacer() + p1 + p3 + plot_layout(height = c(1,4), widths = c(4,1)))
}



