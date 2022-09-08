library(tidyverse)
library(sf)
library(svglite)

world_coordinates <- map_data("world")
world_coordinates <- world_coordinates[which(! world_coordinates$region %in% c("Antarctica", 
                                                                               "French Southern and Antarctic Lands",
                                                                               "South Georgia")),]

world_coordinates$region[which(world_coordinates$region == "USA")] <- "United States of America"
world_coordinates$region[which(world_coordinates$region == "Russia")] <- "Russian Federation"
world_coordinates$region[which(world_coordinates$region == "Iran")] <- "Iran (Islamic Republic of)"
world_coordinates$region[which(world_coordinates$region == "UK")] <- "United Kingdom"
world_coordinates$region[which(world_coordinates$region == "Greenland")] <- "Denmark"
world_coordinates$region[which(world_coordinates$region == "Venezuela")] <- "Venezuela (Bolivarian Republic of)"
world_coordinates$region[which(world_coordinates$region == "French Guiana")] <- "France"
world_coordinates$region[which(world_coordinates$region == "Bolivia")] <- "Bolivia (Plurinational State of)"
world_coordinates$region[which(world_coordinates$region == "Ivory Coast")] <- "Cote d'Ivoire"
world_coordinates$region[which(world_coordinates$region == "Guinea-Bissau")] <- "Guinea Bissau"
world_coordinates$region[which(world_coordinates$region == "Republic of Congo")] <- "Congo"
world_coordinates$region[which(world_coordinates$region == "Guinea-Bissau")] <- "Guinea Bissau"
world_coordinates$region[which(world_coordinates$region == "Tanzania")] <- "United Republic of Tanzania"
world_coordinates$region[which(world_coordinates$region == "Moldova")] <- "Republic of Moldova"
world_coordinates$region[which(world_coordinates$region == "Syria")] <- "Syrian Arab Republic"
world_coordinates$region[which(world_coordinates$region == "Vietnam")] <- "Viet Nam"
world_coordinates$region[which(world_coordinates$region == "Laos")] <- "Lao People's Democratic Republic"
world_coordinates$region[which(world_coordinates$region == "North Korea")] <- "Democratic People's Republic of Korea"
world_coordinates$region[which(world_coordinates$region == "South Korea")] <- "Republic of Korea"
world_coordinates$region[which(world_coordinates$region == "Swaziland")] <- "Eswatini"
world_coordinates$region[which(world_coordinates$region == "New Caledonia")] <- "France"
world_coordinates$region[which(world_coordinates$region == "Falkland Islands")] <- "United Kingdom"
world_coordinates$region[which(world_coordinates$region == "Palestine")] <- "Occupied Palestinian Territory"

timeseries <- read.csv("JAMAPeds_excess_deaths_update_2022/output/orphanhood_timeseries_no_omit_who_adjusted.csv")
timeseries_max <- timeseries[which(timeseries$date == as.Date("2022-05-01")),]
timeseries_max <- select(timeseries_max, country, primary_secondary)

world_join <- left_join(world_coordinates, timeseries_max, by = c( "region" = "country"))

world_join$cat = ifelse(world_join$primary_secondary >= 0 & world_join$primary_secondary < 10000, 1,
                               ifelse(world_join$primary_secondary >= 10000 & world_join$primary_secondary < 20000, 2,
                                      ifelse(world_join$primary_secondary >= 20000 & world_join$primary_secondary < 25000, 3,
                                             ifelse(world_join$primary_secondary >= 25000 & world_join$primary_secondary < 50000, 4, 
                                                    ifelse(world_join$primary_secondary >= 50000 & world_join$primary_secondary < 100000, 5, 
                                                           ifelse(world_join$primary_secondary >= 100000 & world_join$primary_secondary < 150000, 6, 
                                                                  ifelse(world_join$primary_secondary >= 150000, 7, 0)))))))


world_join$cat = factor(world_join$cat, 
                        labels = c("0-9,999", "10,000-19,999", "20,000-24,999", 
                                   "25,000-49,999", "50,000-99,999", "100,000-149,999", "150,000+"))

p  <-   ggplot() + 
  geom_map(data = world_join, 
           map = world_join, aes(long, lat, map_id = region, fill =  cat)) + 
  theme_void() +
  scale_fill_manual(name = "Primary and/or secondary caregiver loss \n",
              values = c("#fdd49e", "#fdbb84", "#fc8d59", "#ef6548", "#d7301f", "#b30000", "#7f0000")) +
  theme(legend.position = "bottom", legend.key.width = unit(2, 'cm'), legend.title = element_blank())
print(p)
#ggsave("JAMAPeds_excess_deaths_update_2022/figures/map_may_2022.pdf", p, width = 13)
saveRDS(p, "JAMAPeds_excess_deaths_update_2022/figures/map_may_2022.RDS")
