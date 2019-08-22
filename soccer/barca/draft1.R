library(tidyverse)
library(StatsBombR)

View(FreeCompetitions())

df = FreeCompetitions() %>% 
  filter(competition_id == 11 & season_id == 41)


matches = FreeMatches(df)

date_2008 = StatsBombFreeEvents(MatchesDF = matches, Parallel = T)


#Xavi = ID 20131
#Iniesta = ID 5216
xaviIniesta = date_2008 %>% 
  filter(play_pattern.name == 'Regular Play' & type.name == 'Pass' & player.id == 20131 )


date_2008 %>%  
  filter(play_pattern.name == 'Regular Play' & type.name == 'Pass' & is.na(pass.recipient.id) == TRUE 
         & player.id == 20131) %>% 
  View()
