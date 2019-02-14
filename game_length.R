library(dplyr)
library(ggplot2)
library(readr)
setwd('/Users/mb/')
plays_2018 = readr::read_csv('/Users/mb/Downloads/gl2010_18/GL2018.TXT', col_names = FALSE)

gamesheet_analyser = function(play_df){
  plays_cleaned = play_df %>% 
    select(c(1,10,11,12,13,19,23,26,31,32,33,39,46,48,49,51,54,59,60,61,67,74,76,77))
  
  names(plays_cleaned) = c('date','visitor_score','home_score','length_in_outs','day_night','length_in_minutes',
                           'visitor_hits','visitor_HR','visitor_walks', 'visitor_intentional_walks','visitor_ks',
                           'visitor_pitchers_used','visitor_errors','visitor_dp','visitor_tp',
                           'home_hits','home_HR','home_walks','home_intentional_walks','home_ks',
                           'home_pitchers_used','home_errors','home_dp','home_tp')
  
  plays_for_model = plays_cleaned %>%  
    filter(length_in_outs > 50 & length_in_outs < 55)  %>% # we just want the 9 inning games 
    mutate(day_game = ifelse(day_night == 'D',1,0),
           non_hr_hits = visitor_hits + home_hits - visitor_HR - home_HR, 
           homeruns = home_HR + visitor_HR,
           unintentional_walks = visitor_walks + home_walks - visitor_intentional_walks - home_intentional_walks, 
           intentional_walks = home_intentional_walks + visitor_intentional_walks, 
           strikeouts = home_ks + visitor_ks, 
           pitching_changes = home_pitchers_used + visitor_pitchers_used - 2, 
           errors = home_errors + visitor_errors, 
           double_plays = home_dp + visitor_dp, 
           visitor_win = ifelse(visitor_score > home_score, 1,0)) %>%
    select(c(6,25:34))
  
  length_model = lm(data = plays_for_model, length_in_minutes ~ .)    
  return(length_model)
  }

summary(length_model)
just_features = select(plays_for_model, -length_in_minutes)

actual_lengths    = plays_for_model$length_in_minutes
predicted_lengths = predict(length_model,just_features)

residuals = data.frame(actual_lengths, predicted_lengths) %>% 
  mutate(residual = predicted_lengths - actual_lengths)

ggplot(residuals, aes(x = actual_lengths, y = predicted_lengths)) + geom_point(alpha = .7) +
  coord_fixed(ratio = 1, xlim = c(120,250), ylim = c(120,250)) + 
  geom_abline(intercept = 0, slope = 1, color = 'red')

ggplot(residuals, aes(x = residual)) + geom_density()
