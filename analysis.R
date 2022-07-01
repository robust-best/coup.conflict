require (ggplot2)
require (zoo)
require (reshape2)
require (lubridate)

#####
## Total fatalities
acled %>% group_by (event_id_cnty) %>% filter (duplicated(event_id_cnty) != T) %>% ungroup() %>% .$fatalities %>% sum()

acled %>% group_by (event_id_cnty) %>% filter (duplicated(event_id_cnty) != T) %>% ungroup() %>% filter (fatalities != 10) %>% filter (fatalities != 100) %>% .$fatalities %>% sum()

#####
## D.Day
d.day.before = as.Date("2021-06-07")
d.day.after = as.Date("2021-12-07")
d.day = as.Date("2021-09-07")
d.day.df = acled %>% filter(event_date > d.day.before) %>% filter (event_date < d.day.after)
d.day.df$d.day = ifelse (d.day.df$event_date < d.day, "Before", "After")
d.day.subevent = d.day.df %>% filter (event_type != "Riots") %>% group_by (sub_event_type, d.day) %>% summarize (Total = n()) %>% ungroup () ### Remove Riots due to small data
d.day.subevent %>% dcast (sub_event_type ~ d.day) %>% mutate (Before = Before*-1) %>% write.csv ("DDay.csv", row.names = F)

#####
## Military airstrike on civilians
military.air.id = airstrike %>% filter (sub_event_type == "Air/drone strike") %>% filter (actor1 == "Military Forces of Myanmar (2021-)") %>% .$event_id_cnty
airstrike %>% filter (event_id_cnty %in% military.air.id) %>% filter (actor1 != "Military Forces of Myanmar (2021-)") %>% .$actor1 %>% length()
airstrike %>% filter (event_id_cnty %in% military.air.id) %>% filter (actor1 == "Civilians (Myanmar)") %>% .$fatalities %>% sum()

#####
## Pyu Saw Htee
pst.actor.index = which(str_detect(acled$actor1, "Pyu Saw Htee") == T)
pst.asso.index = which(str_detect(acled$assoc_actor_1, "Pyu Saw Htee") == T)
pst.index = unique (c(pst.actor.index, pst.asso.index))
pst.event = acled$event_id_cnty [ pst.index]
pst.df = acled %>% filter (event_id_cnty %in% pst.event)