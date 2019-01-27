library(dplyr)
library(ggplot2)
library(readr)
prisoners = read_csv('https://github.com/rfordatascience/tidytuesday/blob/master/data/2019/2019-01-22/prison_population.csv?raw=true')

glimpse(prisoners)

summary = prisoners %>% 
  group_by(region, year) %>% 
  summarize(total_prisoners = sum(prison_population, na.rm = TRUE)) %>% 
  filter(total_prisoners != 0) 

ggplot(summary, aes(x= year, y = total_prisoners, color = region)) + geom_line()

#lets focus in a bit more at the micro level. It would be interesting to see 
#which US counties have experienced the highest and lowest changes in prisoner population since 2000 
#to deal with the spikiness we see in some places, we'll create cols that average the pops at a few key periods 


highest_changers = prisoners %>% 
  filter(pop_category == 'Total') %>% 
  mutate(place_name = paste(county_name, ',' , state)) %>% 
  group_by(place_name) %>% 
  summarize(#the numbers have some spikes so i'm taking a 3 year average to get a better picture of average prisoner levels at each snapshot
    yr_2000_avg = mean(prison_population[year %in% c(1998,1999,2000)], na.rm = TRUE),
    yr_2010_avg = mean(prison_population[year %in% c(2008,2009,2010)], na.rm = TRUE)
    ) %>% 
  filter(yr_2010_avg > 1000) %>% 
  mutate(pct_change = ((yr_2010_avg - yr_2000_avg) / yr_2000_avg)*100) %>% 
  filter(!is.nan(pct_change)) %>% 
  mutate(trend = ifelse(pct_change > 0, 'positive','negative')) %>% 
  arrange(pct_change)

top_tens = highest_changers[c(1:10,  (nrow(highest_changers)-9):nrow(highest_changers)),] #how do you slice like this in tidyverse!??

colors = c('negative' = 'blue', "positive" = 'red')
top_tens %>% 
  mutate(place_name = factor(place_name, place_name)) %>% 
  ggplot(aes(x = place_name,y = pct_change)) +
    geom_segment(aes(xend = place_name, y = 0, yend = pct_change, color = trend, size = 2, alpha = 0.7)) +
    geom_point(aes(color = trend), size=4, alpha=0.9) +
    theme_light() +
    coord_flip() + 
    scale_color_manual(values = colors ) + 
    theme(legend.position="none") + 
    ylab('Percent Change in Prisoner Population 2000 - 2010') + 
    xlab('') + 
    scale_size_identity()
        
