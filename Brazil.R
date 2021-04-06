## Tidy Tuesday April 6th 2021 ## 
library(tidyverse)

tuesdata <- tidytuesdayR::tt_load(2021, week = 15)

d <- tuesdata$brazil_loss %>% select(-2)

dl<-d %>% pivot_longer(!c(entity,year),names_to='cause',values_to='value') %>% 
  group_by(year) %>% 
  mutate(pct=round(value/sum(value),4)*100) 

mypal<-c('rosybrown','darkred','lightblue','goldenrod','darkgreen','gray70',
         'darkseagreen4','gray20','saddlebrown','dodgerblue4','palegreen3')

dl %>% 
  mutate(cause=gsub('[[:punct:] ]+',' ',cause)) %>% 
ggplot(aes(x=2,y=pct,fill=cause))+
  geom_bar(stat='identity',color='black',size=0.04)+
  coord_polar('y',start=0)+
  xlim(1, 2.5)+
  labs(title='Deforestation in Brazil')+
  scale_fill_manual(values=mypal)+
  facet_wrap(~year,nrow=1,strip.position='bottom')+
  theme(legend.position = "bottom",
        title=element_text(size=20),
        text=element_text(family="Montserrat"),
        legend.title = element_blank(),
        legend.key.width = unit(.6, "lines"),
        legend.spacing.x = unit(0.35,'cm'),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        strip.background = element_rect(
          color="coral4", fill="wheat3", size=0.6, linetype="solid"),
        strip.text.x=(element_text(
          size=15, color='saddlebrown')),
        panel.background = element_blank())+
  guides(fill = guide_legend(nrow = 1))


  
  
  
  
