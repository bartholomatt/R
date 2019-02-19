library(tidyverse)
fed_rd <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-12/fed_r_d_spending.csv")
energy_spend <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-12/energy_spending.csv")
climate_spend <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-12/climate_spending.csv")

clean_fed = fed_rd %>% 
  group_by(year) %>% 
  mutate(total_rd = sum(rd_budget))  %>% 
  ungroup() %>% 
  mutate(prop_rd = rd_budget/total_rd)

ggplot(clean_fed, aes(x = year, y = prop_rd)) + 
  geom_area(aes(fill = department)) + 
  theme_minimal() + 
  ylab('proportion of total federal R&D spending')
  
