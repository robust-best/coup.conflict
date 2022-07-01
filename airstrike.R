require(tidyverse)
require(gtools)

#####
# Pre-processing
#####
airstrike = acled %>% filter (sub_event_type %in% c("Armed clash", "Air/drone strike")) %>% filter (!(fatalities %in% c(10,12,100)))
airstrike.geo = airstrike %>% filter (sub_event_type == "Air/drone strike") %>% .$SNR %>% unique ()
airstrike = airstrike %>% filter (SNR %in% airstrike.geo)

#####
# Analysis : Fatalities each State and Region
#####
## Analysis : Attack patterns for air strike
#####
day.gap = data.frame ()
for (i in 1:length(airstrike.geo)) {
  temp.date = airstrike %>% filter (SNR == airstrike.geo[i]) %>% filter (sub_event_type == "Air/drone strike") %>% filter (!duplicated(event_id_cnty)) %>% .$event_date %>% sort()
  temp.gap = c()
  if (length(temp.date) > 1) {
    for (j in 1:length(temp.date)-1) {
      temp.gap = c(temp.gap, as.numeric(temp.date[j] - temp.date[j+1], units = "days"))
    }
  } else if (length(temp.date) == 1) {
    temp.gap = c(temp.gap, NA)
  }
  temp.df = data.frame (admin = airstrike.geo[i], time.gap = abs(temp.gap))
  day.gap = smartbind(day.gap, temp.df)
}

#####
## Analysis : Frequency of frequency of Air/drone strike in a day in each state and region
#####
air.freq = airstrike %>% filter (sub_event_type == "Air/drone strike") %>% filter (!(duplicated(event_id_cnty))) %>% group_by (event_date, SNR) %>% summarize (Total = n()) %>% ungroup () %>% filter (Total != 1)

#####
## Analysis : Same day, different State and region air strike
#####
air.sameday = airstrike %>% filter (sub_event_type == "Air/drone strike") %>% filter (!(duplicated(event_id_cnty))) %>% select (event_id_cnty, event_date, SNR) 
dup.day = air.sameday$event_date[which((duplicated(air.sameday$event_date)) == T)]
air.sameday = air.sameday %>% filter (event_date %in% dup.day)
air.day = unique (air.sameday$event_date)
for (i in 1:length(air.day)) {
  temp.snr = air.sameday %>% filter (event_date == air.day[i]) %>% .$SNR %>% unique ()
  if (length(temp.snr) == 1) {
    air.sameday = air.sameday %>% filter (event_date != air.day[i])
  }
}
air.sameday = air.sameday %>% group_by (event_date) %>% filter (!(duplicated(SNR))) %>% ungroup ()
air.SNR = unique (air.sameday$SNR)
air.df = matrix (ncol = length(air.SNR), nrow = length(air.SNR))
air.df = as.data.frame(air.df)
names(air.df) = air.SNR
rownames(air.df) = air.SNR
for (i in 1:length(air.SNR)) {
  snr.date = air.sameday [which(air.sameday$SNR == air.SNR[i]),] %>% .$event_date %>% unique ()
  snr.freq = air.sameday %>% filter (event_date %in% snr.date) %>% filter (SNR != air.SNR[i]) %>% group_by (SNR) %>% summarize (Total = n())
  snr.freq.snr = unique (snr.freq$SNR)
  for (j in 1:length(snr.freq.snr)) {
    air.df [which(air.SNR == air.SNR[i]), which(names(air.df) == snr.freq.snr[j])] = snr.freq[which(snr.freq$SNR == snr.freq.snr[j]),] %>% .$Total
  }
}
air.df [is.na(air.df)] = 0
for (i in 1:nrow(air.df)) {
  for (j in i:ncol(air.df)) {
    air.df[i,j] = NA
  }
}
air.df = air.df[-1,]
air.df = air.df[,-ncol(air.df)]



#####
## Analysis : Fatalities in Armed clash 7 days before air strike in each state and region
#####
air.fatalities = data.frame ()
for (i in 1:length(airstrike.geo)) {
  temp.date = airstrike %>% filter (sub_event_type == "Air/drone strike") %>% filter (SNR == airstrike.geo[i]) %>% filter(!duplicated(event_id_cnty)) %>% .$event_date %>% unique()
  temp.fatalities = c()
  for (j in 1:length(temp.date)) {
    temp.fatalities = c(temp.fatalities, airstrike %>% filter (sub_event_type == "Armed clash") %>% filter (SNR == airstrike.geo[i]) %>% filter (!(duplicated(event_id_cnty))) %>% filter (event_date >= temp.date[j] - 7) %>% filter (event_date < temp.date[j]) %>% .$fatalities %>% sum())
  }
  temp.df = data.frame(SNR = airstrike.geo[i], fatalities = temp.fatalities)
  air.fatalities = smartbind(air.fatalities, temp.df)
}

#####
## Analysis : Fatalities in Armed clash for 7 days range, exclude fatalities in Armed clash 7 days before air strike in each state and region
#####
nonair.fatalities = airstrike %>% filter (sub_event_type == "Armed clash")
for (i in 1:length(airstrike.geo)) {
  temp.date = airstrike %>% filter (sub_event_type == "Air/drone strike") %>% filter (SNR == airstrike.geo[i]) %>% filter(!duplicated(event_id_cnty)) %>% .$event_date %>% unique()
  for (j in 1:length(temp.date)) {
    temp.event = airstrike %>% filter (!(duplicated(event_id_cnty))) %>% filter (event_date >= temp.date[j] - 7) %>% filter (event_date < temp.date[j]) %>% filter (sub_event_type == "Armed clash") %>% filter (SNR == airstrike.geo[i]) %>% .$event_id_cnty
    nonair.fatalities = nonair.fatalities %>% filter (!(event_id_cnty %in% temp.event))
  }
}
date.range = as.numeric(max(nonair.fatalities$event_date) - min(nonair.fatalities$event_date), units = "days")
nonair.fatalities.7days = data.frame ()
for (i in 1:length(airstrike.geo)) {
  date.fatalities = c()
  for (j in 1:round(date.range/7)) {
    temp.fatalities = nonair.fatalities %>% filter (!(duplicated(event_id_cnty))) %>% filter (SNR == airstrike.geo[i]) %>% filter (event_date > ((min(nonair.fatalities$event_date)) + ((j-1)*7))) %>% filter (event_date <= ((min(nonair.fatalities$event_date)) + ((j)*7))) %>% .$fatalities %>% sum()
    date.fatalities = c(date.fatalities, temp.fatalities)
  }
  temp.df = data.frame(SNR = airstrike.geo[i], fatalities = date.fatalities)
  nonair.fatalities.7days = smartbind(nonair.fatalities.7days, temp.df)
}
fatalities = rbind.data.frame(air.fatalities %>% mutate (type = "Fatalities before 7 days airstrike"), nonair.fatalities.7days %>% mutate (type = "Fatalities 7 days range not in airstrike"))

#####
## Extensive sliding-window method # Excluded in the paper
#####
nonair.fatalities.sliding = data.frame ()
for (i in 1:length(airstrike.geo)) {
  date.fatalities = c()
  for (j in 1:date.range) {
    temp.fatalities = nonair.fatalities %>% filter (!(duplicated(event_id_cnty))) %>% filter (SNR == airstrike.geo[i]) %>% filter (event_date > ((min(nonair.fatalities$event_date)) + ((j-1)*7))) %>% filter (event_date < ((min(nonair.fatalities$event_date)) + ((j)*7))) %>% .$fatalities %>% sum()
    date.fatalities = c(date.fatalities, temp.fatalities)
  }
  temp.df = data.frame(SNR = airstrike.geo[i], fatalities = date.fatalities)
  nonair.fatalities.sliding = smartbind(nonair.fatalities.sliding, temp.df)
}

#####
# Air strike associated groups
a.groups = c(acled %>% filter (sub_event_type == "Air/drone strike") %>% .$actor1, acled %>% filter (sub_event_type == "Air/drone sstrike") %>% .$assoc_actor_1)
a.groups = data.frame (x = sample(1:100, length(unique(a.groups))), y = sample (1:100, length(unique(a.groups))), group = unique(a.groups))

#####
# Analysis : Whole country fatalities # Excluded in the paper
#####
## Analysis : Attack patterns for air strike
#####
wc.temp.date = airstrike %>% filter (sub_event_type == "Air/drone strike") %>% filter (!duplicated(event_id_cnty)) %>% .$event_date %>% sort()
wc.temp.gap = c()
for (j in 1:length(wc.temp.date)-1) {
  wc.temp.gap = c(wc.temp.gap, as.numeric(wc.temp.date[j] - wc.temp.date[j+1], units = "days"))
}
wc.day.gap = data.frame (time.gap = abs(wc.temp.gap))

#####
## Analysis : Fatalities in Armed clash 7 days before air strike
#####
temp.date = airstrike %>% filter (sub_event_type == "Air/drone strike") %>% filter(!duplicated(event_id_cnty)) %>% .$event_date %>% unique()
temp.fatalities = c()
for (j in 1:length(temp.date)) {
  temp.fatalities = c(temp.fatalities, airstrike %>% filter (sub_event_type == "Armed clash") %>% filter (!(duplicated(event_id_cnty))) %>% filter (event_date >= temp.date[j] - 7) %>% filter (event_date < temp.date[j]) %>% .$fatalities %>% sum())
}
wc.air.fatalities = data.frame (fatalities = temp.fatalities)

#####
## Analysis : Fatalities in Armed clash for 7 days range, exclude fatalities in Armed clash 7 days before air strike
#####
wc.nonair.fatalities = airstrike %>% filter (sub_event_type == "Armed clash")
temp.date = airstrike %>% filter (sub_event_type == "Air/drone strike") %>% filter(!duplicated(event_id_cnty)) %>% .$event_date %>% unique() 
for (j in 1:length(temp.date)) {
  temp.event = airstrike %>% filter (!(duplicated(event_id_cnty))) %>% filter (event_date >= temp.date[j] - 7) %>% filter (event_date < temp.date[j]) %>% filter (sub_event_type == "Armed clash") %>% .$event_id_cnty
  wc.nonair.fatalities = wc.nonair.fatalities %>% filter (!(event_id_cnty %in% temp.event))
}
date.range = as.numeric(max(wc.nonair.fatalities$event_date) - min(wc.nonair.fatalities$event_date), units = "days")
wc.nonair.fatalities.7days = data.frame ()
wc.date.fatalities = c()
for (j in 1:round(date.range/7)) {
  temp.fatalities = nonair.fatalities %>% filter (!(duplicated(event_id_cnty))) %>% filter (event_date > ((min(nonair.fatalities$event_date)) + ((j-1)*7))) %>% filter (event_date <= ((min(nonair.fatalities$event_date)) + ((j)*7))) %>% .$fatalities %>% sum()
  wc.date.fatalities = c(wc.date.fatalities, temp.fatalities)
}
wc.nonair.fatalities.7days = data.frame (fatalities = wc.date.fatalities)
wc.fatalities = rbind.data.frame(wc.air.fatalities %>% mutate (type = "Fatalities before 7 days air strike"), wc.nonair.fatalities.7days %>% mutate (type = "Fatalities 7 days range not in air strike"))

#####
## Extensive sliding-window method
#####
wc.nonair.fatalities.sliding = data.frame ()
date.fatalities = c()
for (j in 1:date.range) {
  temp.fatalities = nonair.fatalities %>% filter (!(duplicated(event_id_cnty))) %>% filter (event_date > ((min(nonair.fatalities$event_date)) + ((j-1)*7))) %>% filter (event_date <= ((min(nonair.fatalities$event_date)) + ((j)*7))) %>% .$fatalities %>% sum()
  date.fatalities = c(date.fatalities, temp.fatalities)
}
wc.nonair.fatalities.sliding = data.frame (fatalities = date.fatalities)