library(dplyr)
library(ggplot2)
library(readr)
library(gganimate)
library(gifski)
library(png)
housing = read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-05/state_hpi.csv')

make_date = function(month,year){
  month = ifelse(nchar(month) == 1, paste0('0',month),month)
  return(as.Date(paste0(year,'-',month,'-01')))
}

new_housing = housing %>% 
  mutate(aDate = make_date(month,year)) %>% 
  group_by(state) %>% 
  mutate(change_from_last_year = (price_index - lag(price_index,n = 12))/lag(price_index,n = 12)*100)

graph_it = function(times,states,pal){
  new_housing %>% 
    filter(state %in% states & year %in% times) %>% 
    ggplot(aes (x = aDate, y = change_from_last_year, color = state)) + 
    #geom_segment(aes(xend = aDate, y = 0, yend = change_from_last_year, group = seq_along(aDate)), size = .5, alpha = 0.7) +
    theme_light() + 
    theme(legend.position = 'none') + 
    geom_area(aes(fill = state)) +
    geom_point(size = .5,aes(group = seq_along(aDate))) + 
    xlab('') + 
    ylab('% price channge from previous year') + 
    facet_wrap( ~ state) +
    scale_color_brewer(palette = pal) + 
    scale_fill_brewer(palette = pal)
}

animate_it = function(startGraph){
  startGraph +
  transition_reveal(aDate)
}  

housing_meltdown = graph_it(2000:2012,c('CA','NV','FL','AZ'),'PuOr')
housing_meltdown
animate_it(housing_meltdown)

