require (ggplot2)
require (zoo)
require (lubridate)

#####
## Loading data
acled.path = ""
acled = read.csv (acled.path, stringsAsFactors = F) %>% select (data_id, event_id_cnty, event_date, timestamp, event_type, sub_event_type, actor1, assoc_actor_1, inter1, interaction, admin1, admin2, admin3, location, latitude, longitude, fatalities)

for (i in 1:nrow(acled)) {
  if (acled$assoc_actor_1[i] == "") {
    acled$assoc_actor_1[i] = acled$actor1[i]
  }
}

#####
## Date and time
acled$event_date = as.Date(acled$event_date, "%d %b %Y")
acled$weekdays = weekdays(acled$event_date)
acled$weekdays = factor(acled$weekdays, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
acled$monthyear = as.yearmon(acled$event_date)
acled = acled %>% filter (event_date >= as.Date("2021-02-01")) %>% filter (event_date < as.Date("2022-05-21"))

#####
## Normalizing state and region
snr.path = ""
snr = read.csv (snr.path, stringsAsFactors = F)
acled = left_join(acled, snr, by = "admin1")