library(tidyverse)
#setwd('Documents/GitHub/R/epl/')
epl = read_csv('EPL_Set.csv')
glimpse(epl)
#do some basic cleanup - first off we don't need division, halftime results
 
epl_tidy = epl %>% 
  select(-c(Div, HTHG, HTAG, HTR)) %>% 
  mutate(home_win = ifelse(FTR == 'H',1,0), 
         away_win = ifelse(FTR == 'A',1,0), 
         draw = ifelse(FTR == 'D',1,0)) 

glimpse(epl_tidy)

team_explorer = function(teamName, season){
  homeGames = filter(epl_tidy, HomeTeam == teamName & Season == season) %>% 
    summarize(wins = sum(home_win) , losses = sum(away_win),draws = sum(draw), GF = sum(FTHG), GA = sum(FTAG))
  awayGames = filter(epl_tidy, AwayTeam == teamName & Season == season) %>% 
    summarize(wins = sum(away_win),losses = sum(home_win), draws = sum(draw), GF = sum(FTAG), GA = sum(FTHG))
  combined = rbind(homeGames,awayGames)
  tabulated = data.frame(teamName = teamName,
                     season = season,
                     W = sum(combined$wins),
                     D = sum(combined$draws),
                     L = sum(combined$losses),
                     totalPoints = sum(combined$wins) * 3 + sum(combined$draws) ,
                     GF = sum(combined$GF),
                     GA = sum(combined$GA)) %>% 
    mutate(GD = GF - GA) %>% 
    
  return(tabulated)
}

team_explorer('Liverpool','2001-02')

season_explorer = function(season){
  teamsA = unique(filter(epl_tidy, Season == season)$HomeTeam)
  df = do.call(rbind, Map(function(x) team_explorer(x,season), x = teamsA)) %>% 
    data.frame() %>% 
    arrange(desc(totalPoints), desc(GD)) %>% 
    mutate(place = order(desc(totalPoints)))
  return(df)
}
season_explorer('2001-02')

allSeasons = unique(epl_tidy$Season)

megaSheet = do.call(rbind, Map(function(x) season_explorer(x), x = allSeasons)) %>% 
  data.frame()


megaSheet %>% 
  filter(place == 2) %>% 
  View()
