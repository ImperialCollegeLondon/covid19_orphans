# This file produces a time series of orphans.
library(covid19.analytics)
library(tidyverse)
library(scales)
library(ggrepel)
library(ggsci)

source("R/shiny_table.R")

d <- read.csv("data/time_series_covid19_deaths_global.csv")
d2 <- select(d, -Long, -Lat, -Province.State)

deaths_country = d2 %>% 
  group_by(Country.Region) %>%
  summarise_all(sum) 

deaths_country = deaths_country[which(!deaths_country$Country.Region %in% c("Diamond Princess", "Holy See", "MS Zaandam")),]
deaths_country$Country.Region[which(deaths_country$Country.Region == "Taiwan*")] = "Taiwan"
deaths_country$Country.Region[which(deaths_country$Country.Region == "Iran")] = "I.R. Iran"
deaths_country$Country.Region[which(deaths_country$Country.Region == "Russia")] = "Russian Federation"
deaths_country$Country.Region[which(deaths_country$Country.Region == "Bolivia")] = "Bolivia (Plurinational State of)"
deaths_country$Country.Region[which(deaths_country$Country.Region == "Brunei")] = "Brunei Darussalam"
deaths_country$Country.Region[which(deaths_country$Country.Region == "Burma")] = "Myanmar"
deaths_country$Country.Region[which(deaths_country$Country.Region == "Congo (Brazzaville)")] = "Congo"
deaths_country$Country.Region[which(deaths_country$Country.Region == "Congo (Kinshasa)")] = "Democratic Republic of the Congo"
deaths_country$Country.Region[which(deaths_country$Country.Region == "Venezuela")] = "Venezuela (Bolivarian Republic of)"
deaths_country$Country.Region[which(deaths_country$Country.Region == "Vietnam")] = "Viet Nam"
deaths_country$Country.Region[which(deaths_country$Country.Region == "Syria")] = "Syrian Arab Republic"
deaths_country$Country.Region[which(deaths_country$Country.Region == "Moldova")] = "Republic of Moldova"
deaths_country$Country.Region[which(deaths_country$Country.Region == "Tanzania")] = "United Republic of Tanzania"
deaths_country$Country.Region[which(deaths_country$Country.Region == "Korea, South")] = "Republic of Korea"
deaths_country$Country.Region[which(deaths_country$Country.Region == "Laos")] = "Lao People's Democratic Republic"
deaths_country$Country.Region[which(deaths_country$Country.Region == "US")] <- "United States of America"
deaths_country$Country.Region[which(deaths_country$Country.Region == "I.R. Iran")] <- "Iran (Islamic Republic of)"
deaths_country$Country.Region[which(deaths_country$Country.Region  == "Gambia")] <- "Gambia (Republic of The)"
deaths_country$Country.Region[which(deaths_country$Country.Region == "Guinea-Bissau")] <- "Guinea Bissau"
deaths_country$Country.Region[which(deaths_country$Country.Region  == "Czechia")] <- "Czech Republic"
deaths_country$Country.Region[which(deaths_country$Country.Region  == "United Kingdom")] <- "England & Wales"
deaths_country$Country.Region[which(deaths_country$Country.Region  == "West Bank and Gaza")] <- "Occupied Palestinian Territory"

# Multiply study country deaths by multipliers
multipliers <- read.csv('data/multipliers.csv', header = FALSE)
names(multipliers) <- c("Country", "mult")
#multipliers = multipliers[which(multipliers$Country != "Peru"),]
deaths_country <- left_join(deaths_country, multipliers, by = c("Country.Region" = "Country"))
deaths_country$mult <- ifelse(is.na(deaths_country$mult), 1, deaths_country$mult)
deaths_country[, 2:(ncol(deaths_country)-1)] <- deaths_country[,2:(ncol(deaths_country)-1)] * t(deaths_country[,ncol(deaths_country)]) 

# Join JHU with tfr data
data = readRDS("data/tfr.RDS")
combined_data <- left_join(data, deaths_country, by = c("country" = "Country.Region"))
combined_data$sd = (combined_data$tfr_u-combined_data$tfr_l)/(2*1.96)

# Calculate orphans
dates <- names(combined_data)
dates <- dates[5:(length(dates)-2)]

parents = NULL
primary = NULL
primary_secondary = NULL

# Could be re-written so just add new column on end of spreadsheet each day
for (i in 1:length(dates)){
  c_data <- combined_data[,c("country", "tfr", "tfr_l",  "tfr_u", "sd", dates[i])]
  c_data[,dates[i]][is.na(c_data[,dates[i]])] <- 0
  names(c_data) <- c("country", "tfr", "tfr_l", "tfr_u", "sd",  "total_deaths")
  orphans <- calculate_all_orphans_time_series(c_data, dates[i])
  
  primary_secondary <- rbind(primary_secondary, orphans[[1]])
  primary <- rbind(primary, orphans[[2]])
  parents <- rbind(parents, orphans[[3]])
}

ps_children <- primary_secondary %>% select(country, date, central) %>%
  group_by(date) %>% summarise(total = sum(central))

deaths_jhu = d2 %>% select(-Country.Region) %>% summarise_all(sum) 
deaths_jhu_multiplier <- deaths_jhu*4629314/3180204

dats <- sub('.', '', ps_children$date)
dats <- as.Date(dats, format = "%m.%d.%y")

df <- data.frame(date = as.Date(dats), 
                 orphans = ps_children$total)
df <- df[order(df$date),]
df$covid = unlist(deaths_jhu)
df$excess = unlist(deaths_jhu_multiplier)

df_long <- gather(df, key = key, value = value, -date)
df_long$key = factor(df_long$key, levels = c("excess", "covid", "orphans"),
                     labels = c("Excess deaths", "COVID-19 deaths", "Orphanhood &/or caregiver loss"))
df_long$annotation <- sprintf("%0.1f", df_long$value/1e6)
annotations <- df_long[df_long$date %in% as.Date(c("2020-09-01", "2020-12-31", "2021-02-28", "2021-04-30")),]
p <- ggplot(df_long %>% filter(date >= "2020-03-01" & date <= "2021-04-30")) +
  geom_line(aes(date, value/1e6, col = key)) + theme_bw() + 
  scale_y_continuous(expand  = expansion(0,0), limits = c(0,  5))  + 
  geom_text_repel(data = annotations, aes(x = date, y = value/1e6, label = annotation), direction = "y", nudge_y = 0.1) +
  geom_point(data = annotations, aes(x = date, y = value/1e6, col = key)) +
  annotate("text", label = "Excess Deaths \n(millions)", y = 3.7, x = as.Date("2021-04-20"), size = 3, colour = "black", hjust=1) +
  annotate("text", label = "COVID-19 deaths \n(millions)", y = 2.4, x = as.Date("2021-04-20"), size = 3, colour = "black", hjust=1) +
  annotate("text", label = "Children affected \n(millions)", y = 0.9, x = as.Date("2021-04-20"), size = 3, colour = "black", hjust=1) +
  scale_x_date(expand  = expansion(0,0), date_breaks = "1 month", labels = date_format("%b-%Y")) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust=1)) +  
  xlab("")  + ylab("Millions of people") + scale_colour_manual("", values  = c("darkorchid3", "darkorchid4", "deepskyblue2")) + theme(legend.position = "none")
print(p)
ggsave("figures/time_series_line.pdf", p, width =  10,  height = 6)


