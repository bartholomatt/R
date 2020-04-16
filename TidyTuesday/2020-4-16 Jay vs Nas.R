rm(list = ls())

library(clipr)
library(tidyverse)
library(readr)

polls <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/polls.csv')
rankings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/rankings.csv')

#first thing is to get the artist column cleaned up

rankingsClean = rankings %>%  
  mutate(primary = str_remove(rankings$artist,"ft.*") %>% 
    str_trim(), 
    featuring = str_remove(rankings$artist,".*ft.") %>% 
      str_trim()) %>% 
  mutate(featuring = if_else(primary == featuring,'' , featuring))

beef = rankingsClean %>% 
  filter(str_detect(primary,'Nas')|str_detect(primary,'JAY-Z') | str_detect(featuring,'Nas') | str_detect(featuring,'JAY-Z')) %>% 
  mutate(primary = if_else(str_detect(primary,'JAY-Z'), 'JAY-Z',primary)) %>% 
  mutate(primary = if_else(str_detect(primary,'Nas'), 'Nas',primary)) %>% 
  mutate(featuring = if_else(str_detect(featuring,'JAY-Z'), 'JAY-Z',featuring)) %>% 
  mutate(featuring = if_else(str_detect(featuring,'Nas'), 'Nas',featuring)) %>% 
  select(title, points,year ,primary,featuring) %>% 
  gather('type','artist',-c(1,2,3)) %>% 
  filter(artist %in% c('JAY-Z','Nas'))


totalPoints = beef %>% 
  group_by(artist) %>% 
  summarize(totalPoints = sum(points), totalSongs = n())
  
  
beef %>% 
  group_by(artist, year) %>% 
  summarize(totalPoints = sum(points), nSongs = n()) %>% 
  mutate(totalPoints = if_else(artist == 'Nas', totalPoints * -1, totalPoints)) %>% 
  ggplot(aes(x = year, y = totalPoints, color = artist)) + 
  geom_segment(aes(x = year, xend = year, y=0,yend = totalPoints), size = 3) +
  geom_point(aes(size = nSongs)) + 
  coord_flip() +
  geom_hline(aes(yintercept = 0))+
  scale_x_reverse() + 
  theme_classic() + 
  theme(axis.text.x = element_blank()) + 
  ggtitle('Nas vs Jay: A Rivalry for the Ages')
  #geom_label(aes(label = abs(totalPoints)), position = 'identity', stat = 'unique')
  

