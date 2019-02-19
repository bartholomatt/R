setwd('/Users/mb/Documents/GitHub/R/TidyTuesday')
library(tidyverse)
phd_raw = read_csv('https://github.com/rfordatascience/tidytuesday/raw/master/data/2019/2019-02-19/phd_by_field.csv')
glimpse(phd_raw)

phd_clean = phd_raw %>% 
  group_by(year,broad_field) %>% 
  summarize(n_phd = sum(n_phds, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  mutate(total_phd = sum(n_phd)) %>% 
  ungroup() %>% 
  mutate(prop_phd = n_phd/total_phd)
  


ggplot(phd_clean, aes(x = year, y = prop_phd, fill = broad_field)) +
  geom_area() + 
  scale_fill_manual(palette = 'PuOr')

