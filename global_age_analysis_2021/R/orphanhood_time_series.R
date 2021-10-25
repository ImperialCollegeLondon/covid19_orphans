# This file produces a time series of orphans.
library(tidyverse)
library(scales)
library(ggrepel)
library(ggsci)

source("global_age_analysis_2021/R/time_series_functions.R")

#d <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
#d <- write.csv(d, "data/covid_timeseries.csv")
d <- read.csv("global_age_analysis_2021/data/covid_timeseries.csv")
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
deaths_country$Country.Region[which(deaths_country$Country.Region == "US")] <- "USA" #United States of America"
#deaths_country$Country.Region[which(deaths_country$Country.Region == "I.R. Iran")] <- "Iran (Islamic Republic of)"
deaths_country$Country.Region[which(deaths_country$Country.Region  == "Gambia")] <- "Gambia (Republic of The)"
deaths_country$Country.Region[which(deaths_country$Country.Region == "Guinea-Bissau")] <- "Guinea Bissau"
deaths_country$Country.Region[which(deaths_country$Country.Region  == "Czechia")] <- "Czech Republic"
deaths_country$Country.Region[which(deaths_country$Country.Region  == "United Kingdom")] <- "England & Wales"
deaths_country$Country.Region[which(deaths_country$Country.Region  == "West Bank and Gaza")] <- "Occupied Palestinian Territory"
deaths_country$Country.Region[which(deaths_country$Country.Region  == "Micronesia")] <- "Micronesia (Federated States of)"

# Multiply study country deaths by multipliers
multipliers <- read.csv('global_age_analysis_2021/data/multipliers.csv', header = FALSE)
names(multipliers) <- c("Country", "mult")
omultipliers = multipliers[which(multipliers$Country != "Peru"),]
deaths_country <- left_join(deaths_country, multipliers, by = c("Country.Region" = "Country"))
deaths_country$mult <- ifelse(is.na(deaths_country$mult), 1, deaths_country$mult)
deaths_country[, 2:(ncol(deaths_country)-1)] <- deaths_country[,2:(ncol(deaths_country)-1)] * t(deaths_country[,ncol(deaths_country)]) 

# Join JHU with tfr data
data = readRDS("global_age_analysis_2021/data/tfr_covariates.RDS")
data$sd = (data$tfr_u-data$tfr_l)/(2*1.96)
data$europe = ifelse(data$who_region == "European", 1, 0) 
data = select(data, "country", "tfr", "tfr_l",  "tfr_u", "sd", "who_region", "europe")

combined_data <- left_join(data, deaths_country, by = c("country" = "Country.Region"))

# Calculate orphans
dates <- names(combined_data)
dates <- dates[9:(length(dates)-1)]

parents = NULL

# Could be re-written so just add new column on end of spreadsheet each day
for (i in 1:length(dates)){
  print(i)
  c_data <- combined_data[,c("country", "tfr", "tfr_l",  "tfr_u", "sd", "who_region", "europe", dates[i])]
  c_data[,dates[i]][is.na(c_data[,dates[i]])] <- 0
  names(c_data) <- c("country", "tfr", "tfr_l", "tfr_u", "sd",  "who_region", "europe", "total_deaths")
  orphans <- calculate_all_orphans_time_series(c_data, dates[i])

  parents <- rbind(parents, orphans)
}

children <- parents %>% select(country, date, central) %>%
  group_by(date) %>% summarise(total = sum(central))

children_region <- parents %>% select(country, region, date, central) %>%
  group_by(date, region) %>% summarise(total = sum(central))

deaths_jhu = d2 %>% select(-Country.Region, -X) %>% summarise_all(sum) 
deaths_jhu_multiplier <- deaths_jhu*4629314/3180204 ###### Need to recalculate this for new data set

d2_region <- left_join(deaths_country, data, by = c("Country.Region"= "country"))
deaths_jhu_region = d2_region %>% select(-Country.Region, -X) %>% group_by(who_region) %>% summarise_all(sum) 

tmp = d2_region[which(is.na(d2_region$who_region)),]

dats <- sub('.', '', children$date)
dats <- as.Date(dats, format = "%m.%d.%y")

df <- data.frame(date = as.Date(dats), 
                 orphans = children$total)
df <- df[order(df$date),]
df$covid = unlist(deaths_jhu)
df$excess = unlist(deaths_jhu_multiplier)

df_region <- data.frame(date = rep(as.Date(dats), 7), 
                        orphans = children_region$total)
df_region <- df_region[order(df_region$date),]
df$covid = unlist(deaths_jhu)

saveRDS(df,file = "global_age_analysis_2021/data/age_outputs/orphanhoodtime_series.RDS")
df = readRDS(file = "global_age_analysis_2021/data/age_outputs/orphanhoodtime_series.RDS")

saveRDS(df_region,file = "global_age_analysis_2021/data/age_outputs/orphanhoodtime_series_region.RDS")
df_region = readRDS(file = "global_age_analysis_2021/data/age_outputs/orphanhoodtime_series_region.RDS")

df_long <- gather(df, key = key, value = value, -date)
df_long$key = factor(df_long$key, levels = c("excess", "covid", "orphans"),
                     labels = c("Excess deaths", "COVID-19 deaths", "Orphanhood &/or caregiver loss"))
p <- ggplot(df_long %>% filter(date >= "2020-03-01")) +
  geom_line(aes(date, value/1e6, col = key)) + 
  geom_vline(xintercept = as.Date("2021-04-30"), linetype = "dashed") + 
  theme_bw() + 
  scale_y_continuous(expand  = expansion(0,0), limits = c(0, 7.5))  + 
  annotate("text", label = "Excess Deaths \n(millions)", y = 6, x = as.Date("2021-10-15"), size = 3, colour = "black", hjust=1) +
  annotate("text", label = "COVID-19 deaths \n(millions)", y = 3.9, x = as.Date("2021-10-15"), size = 3, colour = "black", hjust=1) +
  annotate("text", label = "Orphanhood \n(millions)", y = 2.8, x = as.Date("2021-10-15"), size = 3, colour = "black", hjust=1) +
  scale_x_date(expand  = expansion(0,0), date_breaks = "1 month", labels = date_format("%b-%Y")) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust=1)) +  
  xlab("")  + ylab("Millions of people") + 
  scale_colour_manual("", values  = c("darkorchid3", "darkorchid4", "deepskyblue2")) + theme(legend.position = "none")
print(p)

ggsave("global_age_analysis_2021/figures/fig_1_time_series_line.pdf", p, width =  10,  height = 6)

df_region_long <- gather(df_region, key = key, value = value, -date)
df_long$key = factor(df_long$key, levels = c("excess", "covid", "orphans"),
                     labels = c("Excess deaths", "COVID-19 deaths", "Orphanhood &/or caregiver loss"))
p <- ggplot(df_long %>% filter(date >= "2020-03-01")) +
  geom_line(aes(date, value/1e6, col = key)) + 
  geom_vline(xintercept = as.Date("2021-04-30"), linetype = "dashed") + 
  theme_bw() + 
  scale_y_continuous(expand  = expansion(0,0), limits = c(0, 7.5))  + 
  annotate("text", label = "Excess Deaths \n(millions)", y = 6, x = as.Date("2021-10-15"), size = 3, colour = "black", hjust=1) +
  annotate("text", label = "COVID-19 deaths \n(millions)", y = 3.9, x = as.Date("2021-10-15"), size = 3, colour = "black", hjust=1) +
  annotate("text", label = "Orphanhood \n(millions)", y = 2.8, x = as.Date("2021-10-15"), size = 3, colour = "black", hjust=1) +
  scale_x_date(expand  = expansion(0,0), date_breaks = "1 month", labels = date_format("%b-%Y")) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust=1)) +  
  xlab("")  + ylab("Millions of people") + 
  scale_colour_manual("", values  = c("darkorchid3", "darkorchid4", "deepskyblue2")) + theme(legend.position = "none")
print(p)

ggsave("global_age_analysis_2021/figures/fig_1_time_series_line.pdf", p, width =  10,  height = 6)