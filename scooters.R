#setwd("/Users/mb/Documents/GitHub/R")
library(tidyverse)

all_trips = read_csv('/Users/mb/Documents/GitHub/raw_data/Dockless_Vehicle_Trips.csv')
#data pulled from https://data.austintexas.gov/Transportation-and-Mobility/Dockless-Vehicle-Trips/7d8e-dm7r 
#I pulled on Mar 13th 2019. it's updated very frequently so any replications may be slightly different. 

#lets explore with some graphs 
names(all_trips)

names(all_trips) = make.names(names(all_trips))

all_trips_cleaned = all_trips %>% 
  filter(Trip.Duration >= 30 & Trip.Duration >= 0 & Trip.Duration < 100000 & Trip.Distance >= 0)

all_trips_cleaned$date = as.Date(all_trips_cleaned$Start.Time,"%m/%d/%Y")

trips_sampled = sample_n(all_trips_cleaned, 1000)

trips_by_day = all_trips_cleaned %>% 
  group_by(date, Vehicle.Type) %>% 
  summarize(total_trips = n())

ggplot(trips_by_day, aes(x = date, y = total_trips)) + geom_line(aes(color = Vehicle.Type))
#lots of people rode last weekend. 

summary(all_trips_cleaned)

trips_by_scooter = all_trips_cleaned %>% 
  filter(Vehicle.Type == 'scooter') %>% 
  group_by(Device.ID) %>% 
  summarize(total_trips = n(),
            first_recorded_trip = min(date), 
            last_recorded_trip = max(date), 
            active_use_days =  length(unique(date)),
            average_duration = mean(Trip.Duration)/60, #convert seconds to minutes
            average_distance = mean(Trip.Distance)/1609.34) %>% # convert meters to miles
  ungroup() %>% 
  mutate(still_in_service = ifelse(last_recorded_trip > (Sys.Date() -7),1,0), 
         days_in_service = as.numeric(last_recorded_trip - first_recorded_trip + 1),
         rides_per_day = total_trips/days_in_service,
         estimated_lifetime_revenue = total_trips + .15*average_duration * total_trips,
         revenue_per_use_day = estimated_lifetime_revenue/active_use_days, 
         revenue_per_ride = estimated_lifetime_revenue/total_trips)


over_6_mo = trips_by_scooter %>% 
  filter(first_recorded_trip < (Sys.Date() - 180))

summary(over_6_mo)
                                                                 