color.theme = c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a", "#ffff99", "#b15928", "#ffed6f", "#8dd3c7", "#80b1d3")

require (tidyverse)
require (gplots)
require (RColorBrewer)
require (rgdal)

##### 
## MFE

acled %>% filter (!(duplicated(event_id_cnty))) %>% mutate(monthyear = as.Date(monthyear)) %>% group_by (monthyear, event_type) %>% summarize(fatalities = sum(fatalities, na.rm = T)) %>% ggplot () + geom_line (aes(monthyear, fatalities, col = event_type)) + theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) + scale_x_date(date_breaks = "1 month", minor_breaks = NULL, date_labels = "%B %Y") + labs (x = "", y = "Total Fatality", col = "") + scale_color_manual(values = color.theme[4:9])
ggsave("MFE.png", bg = "transparent")

#####
## MET

acled %>% filter (!(duplicated(event_id_cnty))) %>% mutate (monthyear = as.Date(monthyear)) %>% group_by (monthyear, event_type) %>% summarize(total = n()) %>% ungroup() %>% ggplot () + geom_histogram (aes(monthyear, total, fill = event_type), stat = "identity") + theme_minimal() + theme (axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) + scale_x_date(date_breaks = "1 month", minor_breaks = NULL, date_labels = "%B %Y") + labs (x = "", y = "Total", fill = "") + scale_fill_manual(values = color.theme[4:9])
ggsave("MET.png", bg = "transparent")

#####
## SAT

acled %>% filter (!(duplicated(event_id_cnty))) %>% filter (sub_event_type == "Sexual violence") %>% group_by (actor1, SNR) %>% summarize (total = n()) %>% arrange(desc(total)) %>% ggplot () + geom_bar(stat = "identity", aes(reorder(SNR, -total), total, fill = actor1)) + theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) + labs (x = "States and Regions", y = "Total cases", fill = "") + scale_fill_manual(values = color.theme[2:4])
ggsave ("SAT.png", bg = "transparent")

#####
## SNRF

acled %>% filter (!(duplicated(event_id_cnty))) %>% group_by (SNR) %>% summarize (fatalities = sum(fatalities, na.rm = T)) %>% ungroup () %>% arrange (desc(fatalities)) %>% ggplot () + geom_bar(aes(reorder(SNR, -fatalities), fatalities), fill = color.theme[2], stat = "identity") + theme_minimal() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + labs (x = "States and Regions", y = "Total Fatality")
ggsave ("SNRF.png", bg = "transparent")

#####
## WDSNRAS

acled %>% filter (!(duplicated(event_id_cnty))) %>% filter (sub_event_type == "Air/drone strike") %>% group_by (weekdays, SNR) %>% summarize (total = n()) %>% ungroup () %>% ggplot () + geom_bar(aes(weekdays, total, fill = SNR), stat = "identity") + theme_minimal() + labs (x = "", y = "Total", fill = "") + scale_fill_manual(values = color.theme[2:11])
ggsave ("WDSNRAS.png", bg = "transparent")

#####
## WDSNRS

acled %>% filter (!(duplicated(event_id_cnty))) %>% filter (sub_event_type == "Shelling/artillery/missile attack") %>% group_by (weekdays, SNR) %>% summarize (total = n()) %>% ungroup () %>% ggplot () + geom_bar(aes(weekdays, total, fill = SNR), stat = "identity") + theme_minimal() + labs (x = "", y = "Total", fill = "") + scale_fill_manual(values = color.theme[13:1])
ggsave("WDSNRS.png", bg = "transparent")

#####
## WDSNRB

acled %>% filter (!(duplicated(event_id_cnty))) %>% filter (event_type == "Battles") %>% group_by (weekdays, SNR) %>% summarize (total = n()) %>% ungroup () %>% ggplot () + geom_bar(aes(weekdays, total, fill = SNR), stat = "identity") + theme_minimal() + labs (x = "", y = "Total", fill = "") + scale_fill_manual(values = color.theme[15:1])
ggsave ("WDSNRB.png", bg = "transparent")

#####
## EVF

acled %>% filter (!(duplicated(event_id_cnty))) %>% group_by (event_type) %>% summarize (fatalities = sum(fatalities)) %>% ungroup() %>% ggplot (aes(reorder(event_type, -fatalities), fatalities)) + geom_bar(stat = "identity", fill = "skyblue") + geom_text(aes(label = fatalities), vjust = 0) + theme_minimal() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + labs (x = "Event type", y = "Total Fatality")
ggsave ("EVF.png", bg = "transparent")

#####
## Day gap

day.gap %>% filter (time.gap < 50) %>% group_by (time.gap) %>% summarize (total = n()) %>% ggplot () + geom_col (aes(time.gap, total), fill = color.theme[2]) + theme_minimal() + labs(x = "Day gap", y = "Count")
ggsave ("DayGap.png", bg = "transparent")

#####
## Freq of Air Freq

air.freq %>% ggplot () + geom_bar (aes(Total, fill = SNR)) + theme_minimal() + scale_fill_manual(values = color.theme[4:9]) + labs (x = "Numbers of Air/drone strike frequency in a day", y = "Count")
ggsave ("FreqFreq.png", bg = "transparent")

#####
## Heatmap

pal <- colorRampPalette(c("#6a3d9a", "#ffff99"))(n = 299)

png(file = "AirSameday.png", bg = "transparent", width = 2695, height = 1783, units = "px", pointsize = 50)
heatmap.2(as.matrix(air.df), cellnote = air.df, notecol = "black", density.info = "none", trace = "none", dendrogram = "none", Colv = NA, Rowv = NA, key = T, margins = c(6,6), col = pal)
dev.off()

#####
## Air Fatalities

fatalities %>% filter (SNR %in% c("Kachin", "Kayah", "Kayin", "Sagaing")) %>% ggplot () + geom_boxplot(aes(type, fatalities), col = color.theme[2]) + facet_wrap(~ SNR) + theme_minimal() + theme (axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + labs (x = "", y = "Fatalities")
ggsave ("AirFatalities.png", bg = "transparent")

#####
## Pyu Saw Htee Events

pst.df %>% filter (!(duplicated(event_id_cnty))) %>% group_by (sub_event_type) %>% summarize (Total = n()) %>% ggplot () + geom_col (aes(reorder(sub_event_type, -Total), Total), fill = color.theme[2]) + theme_minimal() + theme (axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + labs (x = "", y = "Total")
ggsave ("PST.png", bg = "transparent")

#####
## Air strike associated groups

a.groups %>% ggplot (aes(x,y)) + geom_point() + geom_label(aes(label = group)) + theme_minimal() + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) + labs (x = "", y = "")
ggsave ("AirAssociate.png", bg = "transparent")

########################### Mapping ##########################

mmr = map_data("world") %>% filter (region == "Myanmar")

#####
## Bloodshed

ggplot (bloodshed, aes(long, lat)) + lims(x = c(92, 102), y = c(9.5, 29)) + coord_cartesian(xlim = c(92, 102), ylim = c(9.5, 29)) + geom_polygon(data = mmr, aes(long, lat, group = group), fill = NA, col = "black") + stat_density2d(aes(fill = ..level.., alpha = ..level..), geom = "polygon", h = 1.5) + scale_fill_gradient(low = "#fb9a99", high = "#e31a1c") + theme_void() + theme (legend.position = "none") + coord_quickmap()
ggsave ("Bloodshed.png", bg = "transparent", width = 480, height = 1000, units = "px")

#####
## Explosion

ggplot (explosion, aes(long, lat)) + lims(x = c(92, 102), y = c(9.5, 29)) + coord_cartesian(xlim = c(92, 102), ylim = c(9.5, 29)) + geom_polygon(data = mmr, aes(long, lat, group = group), fill = NA, col = "black") + stat_density2d(aes(fill = ..level.., alpha = ..level..), geom = "polygon", h = 1.5) + scale_fill_gradient(low = "#ffff99", high = "#b15928") + theme_void() + theme (legend.position = "none") + coord_quickmap()
ggsave ("Explosion.png", bg = "transparent", width = 480, height = 1000, units = "px")

#####
## Battles

ggplot (battle, aes(long, lat)) + lims(x = c(92, 102), y = c(9.5, 29)) + coord_cartesian(xlim = c(92, 102), ylim = c(9.5, 29)) + geom_polygon(data = mmr, aes(long, lat, group = group), fill = NA, col = "black") + stat_density2d(aes(fill = ..level.., alpha = ..level..), geom = "polygon", h = 1.5) + scale_fill_gradient(low = "#a6cee3", high = "#1f78b4") + theme_void() + theme (legend.position = "none") + coord_quickmap()
ggsave ("Battle.png", bg = "transparent", width = 480, height = 1000, units = "px")

#####
## Protest

ggplot (protest, aes(long, lat)) + lims(x = c(92, 102), y = c(9.5, 29)) + coord_cartesian(xlim = c(92, 102), ylim = c(9.5, 29)) + geom_polygon(data = mmr, aes(long, lat, group = group), fill = NA, col = "black") + stat_density2d(aes(fill = ..level.., alpha = ..level..), geom = "polygon", h = 1.5) + scale_fill_gradient(low = "#a6cee3", high = "#33a02c") + theme_void() + theme (legend.position = "none") + coord_quickmap()
ggsave ("Protest.png", bg = "transparent", width = 480, height = 1000, units = "px")
