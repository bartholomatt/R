#tidyTuesday 2019 week 5: Dairy Data. 
library(readr)
library(dplyr)
library(ggplot2)
#pulling in the cheese cheet 
cheeses = read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-29/clean_cheese.csv')
glimpse(cheeses)
#ITS NOT VERY TIDY 
library(tidyr)
tidy_cheeses = cheeses %>% 
  gather(key = 'type', value = 'amount', 2:17)
#that's better 

ggplot(tidy_cheeses, aes( x= Year, y = amount, fill = type)) + geom_area()
#theres too much going on here and also the categories are not super intuitive. Lets try something else. Moving to the milk products dataset 

milks = read_csv('https://github.com/rfordatascience/tidytuesday/raw/master/data/2019/2019-01-29/milk_products_facts.csv')
#i just wanna look at frozen stuff! 
frozen_stuff = milks %>% 
  select(c(1,11:14)) %>% 
  gather(key = 'category', value = 'amount', 2:5)

ggplot(frozen_stuff,  aes( x= year, y = amount, fill = category)) + 
  geom_area() + 
  scale_fill_brewer(palette = 'Blues') + 
  theme_minimal() + 
  ylab('pounds consumed per person') + 
  theme( panel.grid.major = element_blank(),
         panel.grid.minor = element_blank())
  
