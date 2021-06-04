library(tidyverse)
library(geofacet)
library(broom)
library(maptools)
library(rgeos)
library(rgdal)
library(colorspace)
library(sf)
library(ggalt)
library(ggthemes)


options(scipen = 999)
#------------------------------------------------------------------------------------------------
# Rates
rates <- read.csv("orphans_tab2.csv")
rates <- select(rates, country, orphans_per_thousand, primary_per_thousand, all_per_thousand)
rates <- rates[order(rates$all_per_thousand, decreasing = T),]
rates_long <- gather(rates, key = "sim", value = "value", -country)
rates_long$sim <- factor(rates_long$sim, levels = c("all_per_thousand",
                                                    "primary_per_thousand",
                                                    "orphans_per_thousand"),
                         labels = c("Loss of primary and/or secondary caregivers",
                                    "Loss of primary caregivers",
                                    "Orphanhood"))
rates_long$country <- factor(rates_long$country, 
                             levels = rates$country[order(rates$all_per_thousand, decreasing = F)])

p_r <- ggplot(rates_long) + 
  geom_bar(aes(x = value, y = country, fill = sim), stat = "identity", position = "dodge") + 
  theme_bw() + theme(legend.position = "bottom") + 
  scale_x_continuous(expand=expansion(mult=c(0,0.05))) + 
  scale_fill_manual(name = "", values = c("#43a2ca", "#a8ddb5", "#e0f3db"), guide = guide_legend(reverse = TRUE, ncol = 3)) + 
  ylab("Country") + xlab("Rates per thousand")
print(p_r)

ggsave(filename = "figures/rates.pdf", p_r)

#------------------------------------------------------------------------------------------------
# Regions
r_parents <- readRDS("data/region_estimates_pa.RDS")
r_primary <- readRDS("data/region_estimates_p.RDS")
r_primary_secondary <- readRDS("data/region_estimates_ps.RDS")
regions <- left_join(r_parents, r_primary, by = c("region"))
regions <- left_join(regions, r_primary_secondary, by = c("region"))

# Makes region table
regions_tab <- select(regions, region, text_pa, text_p, text_ps)

names(regions_tab) <- c("Region", "Minimum estimates of orphanhood", 
                    "Minimum estimates of primary caregiver loss",
                    "Minimum estimates of primary and/or secondary caregiver loss")
write_csv(regions_tab, file = "region_estimates.csv")

#------------------------------------------------------------------------------------------------
# Makes stacked bar plot
regions_mean <- regions %>% 
  select(region, mean_pa, mean_p, mean_ps) %>%
  mutate("Death of one or more parents" = mean_pa,
         "Death of one or more co-habiting grandparents" = mean_p - mean_pa,
         "Death of one or more secondary grandparents" = mean_ps - mean_p) %>%
  select(-mean_pa, -mean_p, -mean_ps)
regions_mean_long = gather(regions_mean, key = "type", value = "value", -region)
regions_mean_long$type = factor(regions_mean_long$type, 
                                levels = c("Death of one or more secondary grandparents",
                                           "Death of one or more co-habiting grandparents", 
                                           "Death of one or more parents"))
regions_mean_long$region = factor(regions_mean_long$region, 
                                  levels = c("Western Pacific", "South-East Asia", "European",
                                             "Eastern Mediterranean", "Americas", "African "))

p <- ggplot(regions_mean_long[order(regions_mean_long$type, decreasing=T),], aes(fill=type, y=region, x=value)) + 
  geom_bar(position="stack", stat="identity") + xlab("\nNumber of children who suffered") + ylab("Region\n") + 
  theme_bw() + theme(legend.position = "bottom") + 
  scale_x_continuous(expand=expansion(mult=c(0,0.05))) + 
  scale_fill_manual(name = "", values = c("#e0f3db", "#a8ddb5", "#43a2ca"), guide = guide_legend(reverse = TRUE, ncol = 1))

ggsave(filename = "figures/region_totals_stacked_bar.pdf", p)

#------------------------------------------------------------------------------------------------
# Makes unstacked bar plot
regions_mean <- regions %>% 
  select(region, mean_pa, mean_p, mean_ps) %>%
  mutate("Death of one or more parents" = mean_pa,
         "Death of one or more primary caregivers" = mean_p,
         "Death of one or more primary and/or secondary caregivers" = mean_ps) %>%
  select(-mean_pa, -mean_p, -mean_ps)
regions_mean_long = gather(regions_mean, key = "type", value = "value", -region)
regions_mean_long$type = factor(regions_mean_long$type, 
                                levels = c("Death of one or more primary and/or secondary caregivers",
                                           "Death of one or more primary caregivers", 
                                           "Death of one or more parents"))
regions_mean_long$region = factor(regions_mean_long$region, 
                                  levels = c("Western Pacific", "South-East Asia", "European",
                                             "Eastern Mediterranean", "Americas", "African "))

p2 <- ggplot(regions_mean_long[order(regions_mean_long$type, decreasing=T),], aes(fill=type, y=region, x=value)) + 
  geom_bar(position = "dodge", stat="identity") + xlab("\nNumber of children who suffered") + ylab("Region\n") + 
  theme_bw() + theme(legend.position = "bottom") + 
  scale_x_continuous(expand=expansion(mult=c(0,0.05))) + 
  scale_fill_manual(name = "", values = c("#e0f3db", "#a8ddb5", "#43a2ca"), guide = guide_legend(reverse = TRUE, ncol = 1))

ggsave(filename = "figures/region_totals_unstacked_bar.pdf", p2)

#------------------------------------------------------------------------------------------------
# # Makes ladder plot
# region_mean_data <- regions %>%
#   select(region, mean_pa, mean_p, mean_ps) %>%
#   gather(key = "type", value = "mean", -region)
# region_mean_data$type = ifelse(region_mean_data$type == "mean_pa", "pa", 
#                                ifelse(region_mean_data$type == "mean_p", "p", "ps"))
# region_lower_data <- regions %>%
#   select(region, lower_pa, lower_p, lower_ps) %>%
#   gather(key = "type", value = "lower", -region)
# region_lower_data$type = ifelse(region_lower_data$type == "lower_pa", "pa", 
#                                ifelse(region_lower_data$type == "lower_p", "p", "ps"))
# region_upper_data <- regions %>%
#   select(region, upper_pa, upper_p, upper_ps) %>%
#   gather(key = "type", value = "upper", -region)
# region_upper_data$type = ifelse(region_upper_data$type == "upper_pa", "pa", 
#                                ifelse(region_upper_data$type == "upper_p", "p", "ps"))
# 
# region_ladder <- left_join(region_mean_data, region_lower_data, by = c("region", "type"))
# region_ladder <- left_join(region_ladder, region_upper_data, by = c("region", "type"))
# 
# p2 <- ggplot(region_ladder) + #, aes(fill=type, y=region, x=value)) + 
#   geom_pointrange(aes(x = mean, xmin = lower, xmax = upper, y = region, col = type), size = 0.2) + #+ xlab("\nNumber of children who suffered") + ylab("Region\n") + 
#   theme_bw() + theme(legend.position = "bottom") + 
#   #scale_x_continuous(expand=expansion(mult=c(0,0.05))) + 
#   scale_color_manual(name = "", values = c("#e0f3db", "#a8ddb5", "#43a2ca"), guide = guide_legend(reverse = TRUE, ncol = 1))
# print(p2)
# ggsave(filename = "figures/region_totals_ladder.pdf", p)

#------------------------------------------------------------------------------------------------

primary_secondary <- readRDS("data/country_estimates_ps_report.RDS")
prim_sec = select(primary_secondary, country, mean, region)
# get country names to match
prim_sec <- rbind(prim_sec, data.frame(country = "Antigua", 
                                       mean = prim_sec$mean[which(prim_sec$country == "Antigua and Barbuda")], 
                                       region = prim_sec$region[which(prim_sec$country == "Antigua and Barbuda")]))
prim_sec <- rbind(prim_sec, data.frame(country = "Barbuda", 
                                       mean = prim_sec$mean[which(prim_sec$country == "Antigua and Barbuda")], 
                                       region = prim_sec$region[which(prim_sec$country  == "Antigua and Barbuda")]))
prim_sec <- rbind(prim_sec, data.frame(country = "Bolivia", 
                                       mean = prim_sec$mean[which(prim_sec$country == "Bolivia (Plurinational State of)")], 
                                       region = prim_sec$region[which(prim_sec$country  == "Bolivia (Plurinational State of)")]))
prim_sec <- rbind(prim_sec, data.frame(country = "Brunei", 
                                       mean = prim_sec$mean[which(prim_sec$country == "Brunei Darussalam")], 
                                       region = prim_sec$region[which(prim_sec$country  == "Brunei Darussalam")]))
prim_sec <- rbind(prim_sec, data.frame(country = "Ivory Coast", 
                                       mean = prim_sec$mean[which(prim_sec$country == "Cote d'Ivoire")], 
                                       region = prim_sec$region[which(prim_sec$country  == "Cote d'Ivoire")]))
prim_sec <- rbind(prim_sec, data.frame(country = "Republic of Congo", 
                                       mean = prim_sec$mean[which(prim_sec$country == "Congo")], 
                                       region = prim_sec$region[which(prim_sec$country  == "Congo")]))
prim_sec <- rbind(prim_sec, data.frame(country = "Cape Verde", 
                                       mean = prim_sec$mean[which(prim_sec$country == "Cabo Verde")], 
                                       region = prim_sec$region[which(prim_sec$country  == "Cabo Verde")]))
prim_sec <- rbind(prim_sec, data.frame(country = "UK", 
                                       mean = prim_sec$mean[which(prim_sec$country == "England & Wales")] + 
                                         prim_sec$mean[which(prim_sec$country == "Scotland & Northern Ireland")], 
                                       region = prim_sec$region[which(prim_sec$country  == "England & Wales")]))
prim_sec <- rbind(prim_sec, data.frame(country = "Gambia", 
                                       mean = prim_sec$mean[which(prim_sec$country == "Gambia (Republic of The)")], 
                                       region = prim_sec$region[which(prim_sec$country  == "Gambia (Republic of The)")]))
prim_sec <- rbind(prim_sec, data.frame(country = "Guinea-Bissau", 
                                       mean = prim_sec$mean[which(prim_sec$country == "Guinea Bissau")], 
                                       region = prim_sec$region[which(prim_sec$country  == "Guinea Bissau")]))
prim_sec <- rbind(prim_sec, data.frame(country = "Iran", 
                                       mean = prim_sec$mean[which(prim_sec$country == "I.R. Iran")], 
                                       region = prim_sec$region[which(prim_sec$country  == "I.R. Iran")]))
prim_sec <- rbind(prim_sec, data.frame(country = "South Korea", 
                                       mean = prim_sec$mean[which(prim_sec$country == "Republic of Korea")], 
                                       region = prim_sec$region[which(prim_sec$country  == "Republic of Korea")]))
prim_sec <- rbind(prim_sec, data.frame(country = "Russia", 
                                       mean = prim_sec$mean[which(prim_sec$country == "Russian Federation")], 
                                       region = prim_sec$region[which(prim_sec$country  == "Russian Federation")]))
prim_sec <- rbind(prim_sec, data.frame(country = "Swaziland", 
                                       mean = prim_sec$mean[which(prim_sec$country == "Eswatini")], 
                                       region = prim_sec$region[which(prim_sec$country  == "Eswatini")]))
prim_sec <- rbind(prim_sec, data.frame(country = "Syria", 
                                       mean = prim_sec$mean[which(prim_sec$country == "Syrian Arab Republic")], 
                                       region = prim_sec$region[which(prim_sec$country  == "Syrian Arab Republic")]))
prim_sec <- rbind(prim_sec, data.frame(country = "Trinidad", 
                                       mean = prim_sec$mean[which(prim_sec$country == "Trinidad and Tobago")], 
                                       region = prim_sec$region[which(prim_sec$country  == "Trinidad and Tobago")]))
prim_sec <- rbind(prim_sec, data.frame(country = "Tobago", 
                                       mean = prim_sec$mean[which(prim_sec$country == "Trinidad and Tobago")], 
                                       region = prim_sec$region[which(prim_sec$country  == "Trinidad and Tobago")]))
prim_sec <- rbind(prim_sec, data.frame(country = "Taiwan", 
                                       mean = prim_sec$mean[which(prim_sec$country == "Trinidad and Tobago")], 
                                       region = prim_sec$region[which(prim_sec$country  == "Trinidad and Tobago")]))
prim_sec <- rbind(prim_sec, data.frame(country = "Tanzania", 
                                       mean = prim_sec$mean[which(prim_sec$country == "United Republic of Tanzania")], 
                                       region = prim_sec$region[which(prim_sec$country  == "United Republic of Tanzania")]))
prim_sec <- rbind(prim_sec, data.frame(country = "Venezuela", 
                                       mean = prim_sec$mean[which(prim_sec$country == "Venezuela (Bolivarian Republic of)")], 
                                       region = prim_sec$region[which(prim_sec$country  == "Venezuela (Bolivarian Republic of)")]))
prim_sec <- rbind(prim_sec, data.frame(country = "Vietnam", 
                                       mean = prim_sec$mean[which(prim_sec$country == "Viet Nam")], 
                                       region = prim_sec$region[which(prim_sec$country  == "Viet Nam")]))
prim_sec <- rbind(prim_sec, data.frame(country = "Saint Vincent", 
                                       mean = prim_sec$mean[which(prim_sec$country == "Saint Vincent and the Grenadines")], 
                                       region = prim_sec$region[which(prim_sec$country  == "Saint Vincent and the Grenadines")]))
prim_sec <- rbind(prim_sec, data.frame(country = "Grenadines", 
                                       mean = prim_sec$mean[which(prim_sec$country == "Saint Vincent and the Grenadines")], 
                                       region = prim_sec$region[which(prim_sec$country  == "Saint Vincent and the Grenadines")]))
prim_sec <- rbind(prim_sec, data.frame(country = "Moldova", 
                                       mean = prim_sec$mean[which(prim_sec$country == "Republic of Moldova")], 
                                       region = prim_sec$region[which(prim_sec$country  == "Republic of Moldova")]))
prim_sec <- rbind(prim_sec, data.frame(country = "Laos", 
                                       mean = prim_sec$mean[which(prim_sec$country == "Lao People's Democratic Republic")], 
                                       region = prim_sec$region[which(prim_sec$country  == "Lao People's Democratic Republic")]))
prim_sec <- rbind(prim_sec, data.frame(country = "Macedonia", 
                                       mean = prim_sec$mean[which(prim_sec$country == "North Macedonia")], 
                                       region = prim_sec$region[which(prim_sec$country  == "North Macedonia")]))
prim_sec <- rbind(prim_sec, data.frame(country = "Palestine", 
                                       mean = prim_sec$mean[which(prim_sec$country == "Occupied Palestinian Territory")], 
                                       region = prim_sec$region[which(prim_sec$country  == "Occupied Palestinian Territory")]))



wrld <- map_data("world")
wrld <- wrld[wrld$region != "Antarctica",]
wrld <- left_join(wrld, prim_sec, by = c("region" = "country"))
wrld$mean[which(wrld$region == "Greenland")] = 0
gg <- ggplot(wrld) + geom_cartogram(map=wrld, data = wrld,
                                aes(x=long, y=lat, map_id=region,fill= mean),
                                color="#2b2b2b", size=0.15) + 
  coord_proj("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") +
  theme_map() +
  labs(fill = "Primary and/or secondary caregiver loss")
gg

ggsave("figures/map.pdf", gg, width = 12, height = 6)
