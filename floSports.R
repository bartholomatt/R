library(dplyr)
library(datetime)

user_id = rep(1,7)
start_ts = as.datetime('01-01-2019 13:10)#',format='%m-%d-%Y %H:%M')

start_ts = as.datetime(c('01-01-2019 13:10',
                         '01-01-2019 13:15',
                         '01-01-2019 13:18',
                         '01-01-2019 13:45',
                         '01-02-2019 17:05',
                         '01-02-2019 17:10',
                         '01-02-2019 17:30'), format='%m-%d-%Y %H:%M')
))



end_ts = as.datetime(c('01-01-2019 14:10',
                         '01-01-2019 13:20',
                         '01-01-2019 13:40',
                         '01-01-2019 14:45',
                         '01-02-2019 17:20',
                         '01-02-2019 17:20',
                         '01-02-2019 18:00'), format='%m-%d-%Y %H:%M')
))

views = data.frame(user_id, login = start_ts, logout = end_ts)

tidy_views = views %>% 
  gather(key = event_type, value = 'timestamp', -1) %>% 
  group_by(user_id) %>% 
  arrange(timestamp) %>% 
  mutate(login_count = cumsum(event_type == 'login'), 
         logout_count = cumsum(event_type == 'logout'),
         full_logout = login_count == logout_count, 
         session_number = cumsum(lag(full_logout,1, default = 0))) %>% 
  group_by(session_number) %>% 
  summarize(session_start = min(timestamp), 
            session_end = max(timestamp),
            session_length = as.numeric(session_end - session_start)/60)

