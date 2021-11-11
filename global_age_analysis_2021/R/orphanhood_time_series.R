# This file produces a time series of orphans.
library(tidyverse)
library(scales)
library(ggrepel)
library(ggsci)
library(ggpubr)

source("global_age_analysis_2021/R/time_series_functions.R")

#d <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
#d <- write.csv(d, "global_age_analysis_2021/data/covid_timeseries.csv")
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

ps_children_all = NULL

# Could be re-written so just add new column on end of spreadsheet each day
for (i in 1:length(dates)){
  print(i)
  c_data <- combined_data[,c("country", "tfr", "tfr_l",  "tfr_u", "sd", "who_region", "europe", dates[i])]
  c_data[,dates[i]][is.na(c_data[,dates[i]])] <- 0
  names(c_data) <- c("country", "tfr", "tfr_l", "tfr_u", "sd",  "who_region", "europe", "total_deaths")
  ps_children <- calculate_all_orphans_time_series(c_data, dates[i])

  ps_children_all <- rbind(ps_children_all, ps_children)
}

children <- ps_children_all %>% select(country, date, central) %>%
  group_by(date) %>% summarise(total = sum(central))

ps_children_all$region = ifelse(ps_children_all$region == "Eastern European", "European", ps_children_all$region)
children_region <- ps_children_all %>% select(country, region, date, central) %>%
  group_by(date, region) %>% summarise(total = sum(central))

deaths_jhu = d2 %>% select(-Country.Region, -X) %>% summarise_all(sum) 
deaths_excess = deaths_country %>% select(-Country.Region, -X) %>% summarise_all(sum) 
deaths_excess$mult = NULL

d2_region <- left_join(d2, data, by = c("Country.Region"= "country"))
d2_region <- d2_region[!d2_region$Country.Region %in%  c("Summer Olympics 2020", "Taiwan"),] 
d2_region$who_region =  ifelse(d2_region$who_region == "Eastern European", "European", d2_region$who_region)
deaths_jhu_region = d2_region %>% select(-Country.Region, -X, -tfr, -tfr_l, -tfr_u, -sd, -europe) %>% 
  group_by(who_region) %>% summarise_all(sum) 

deaths_country_region <- left_join(deaths_country, data, by = c("Country.Region"= "country"))
deaths_country_region <- deaths_country_region[!deaths_country_region$Country.Region %in%  c("Summer Olympics 2020", "Taiwan"),]
deaths_country_region$who_region =  ifelse(deaths_country_region$who_region == "Eastern European", "European", deaths_country_region$who_region)
deaths_country_region_excess = deaths_country_region %>% select(-Country.Region, -X, -tfr, -tfr_l, -tfr_u, -sd, -europe) %>% 
  group_by(who_region) %>% summarise_all(sum) 

deaths_jhu_region_long <- gather(deaths_jhu_region, key = "date", value =  "covid", -who_region)
deaths_jhu_region_long$date  <- str_remove(deaths_jhu_region_long$date,  "X")
deaths_jhu_region_long$date <- as.Date(deaths_jhu_region_long$date, format = "%m.%d.%y")

deaths_excess_region_long <- gather(deaths_country_region_excess, key = "date", value =  "excess", -who_region)
deaths_excess_region_long$date  <- str_remove(deaths_excess_region_long$date,  "X")
deaths_excess_region_long$date <- as.Date(deaths_excess_region_long$date, format = "%m.%d.%y")


dats <- sub('.', '', children$date)
dats <- as.Date(dats, format = "%m.%d.%y")

df <- data.frame(date = as.Date(dats), 
                 orphans = children$total)
df <- df[order(df$date),]
df$covid = unlist(deaths_jhu)
df$excess = unlist(deaths_excess)
print(df$covid[df$date == max(df$date)])

children_region$date <- str_remove(children_region$date,  "X")
children_region$date <- as.Date(children_region$date, format = "%m.%d.%y")

df_region <- children_region
names(df_region)  <- c("date", "region", "orphans")
df_region <- df_region[order(df_region$date),]

df_region = left_join(df_region, deaths_jhu_region_long,  by = c("region"= "who_region", "date"))
df_region = left_join(df_region, deaths_excess_region_long,  by = c("region"= "who_region", "date"))

saveRDS(df,file = "global_age_analysis_2021/data/age_outputs/orphanhoodtime_series.RDS")
df = readRDS(file = "global_age_analysis_2021/data/age_outputs/orphanhoodtime_series.RDS")

saveRDS(df_region,file = "global_age_analysis_2021/data/age_outputs/orphanhoodtime_series_region.RDS")
df_region = readRDS(file = "global_age_analysis_2021/data/age_outputs/orphanhoodtime_series_region.RDS")

df_long <- gather(df, key = key, value = value, -date)
df_long$key = factor(df_long$key, levels = c("excess", "covid", "orphans"),
                     labels = c("Excess deaths", "COVID-19 deaths", "Orphanhood &/or caregiver loss"))
p_global <- ggplot(df_long %>% filter(date >= "2020-03-01" & key != "Excess deaths" & date <= "2021-10-31")) +
  geom_line(aes(date, value/1e6, col = key)) + 
  geom_vline(xintercept = as.Date("2021-04-30"), linetype = "dashed") + 
  theme_bw() + 
  scale_y_continuous(expand  = expansion(0,0), limits = c(0, 6.0))  + 
  annotate("text", label = "COVID-19 deaths \n(millions)", y = 4.1, x = as.Date("2021-10-15"), size = 3, colour = "black", hjust=1) +
  annotate("text", label = "Orphanhood &/or caregiver loss \n(millions)", y = 5.75, x = as.Date("2021-10-15"), size = 3, colour = "black", hjust=1) +
  scale_x_date(expand  = expansion(0,0), date_breaks = "1 month", labels = date_format("%b-%Y")) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust=1)) +  
  xlab("")  + ylab("Millions of people") + 
  scale_colour_manual("", values  = c("darkorchid4", "deepskyblue2")) 

df_region_long <- gather(df_region, key = key, value = value, -date, -region)
df_region_long$key = factor(df_region_long$key, levels = c("excess", "covid", "orphans"),
                     labels = c("Excess deaths", "COVID-19 deaths", "Orphanhood &/or caregiver loss"))
df_region_long <- df_region_long[order(df_region_long$date),]
p_region <- ggplot(df_region_long %>% filter(date >= "2020-03-01" & key != "Excess deaths" & date <= "2021-10-31")) +
  geom_line(aes(date, value/1e6, col = key)) + 
  geom_vline(xintercept = as.Date("2021-04-30"), linetype = "dashed") + 
  theme_bw() + 
  scale_x_date(expand  = expansion(0,0), date_breaks = "2 month", labels = date_format("%b-%Y")) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust=1)) +  
  xlab("")  + ylab("Millions of people") + 
  facet_wrap(~region, scales = "free") + 
  scale_colour_manual("", values  = c("darkorchid4", "deepskyblue2"))

p <- ggarrange(p_global, p_region, ncol = 1, labels = "AUTO", common.legend = TRUE, legend= "bottom")
ggsave("global_age_analysis_2021/figures/time_series_line_no_excess.pdf", p, width =  12,  height = 12)


p_global_excess <- ggplot(df_long %>% filter(date >= "2020-03-01" & date <= "2021-10-31")) +
  geom_line(aes(date, value/1e6, col = key)) + 
  geom_vline(xintercept = as.Date("2021-04-30"), linetype = "dashed") + 
  theme_bw() + 
  scale_y_continuous(expand  = expansion(0,0), limits = c(0, 11.5))  + 
  annotate("text", label = "COVID-19 deaths \n(millions)", y = 3.7, x = as.Date("2021-10-25"), size = 3, colour = "black", hjust=1) +
  annotate("text", label = "Orphanhood &/or caregiver loss \n(millions)", y = 6.1, x = as.Date("2021-10-25"), size = 3, colour = "black", hjust=1) +
  annotate("text", label = "Excess deaths \n(millions)", y = 9.3, x = as.Date("2021-10-25"), size = 3, colour = "black", hjust=1) +
  scale_x_date(expand  = expansion(0,0), date_breaks = "1 month", labels = date_format("%b-%Y")) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust=1)) +  
  xlab("")  + ylab("Millions of people\n") + 
  scale_colour_manual("", values  = c("darkorchid3", "darkorchid4", "deepskyblue2")) 
p_global_excess
p_region_excess <- ggplot(df_region_long %>% filter(date >= "2020-03-01" & date <= "2021-10-31")) +
  geom_line(aes(date, value/1e6, col = key)) + 
  geom_vline(xintercept = as.Date("2021-04-30"), linetype = "dashed") + 
  theme_bw() + 
  scale_x_date(expand  = expansion(0,0), date_breaks = "2 month", labels = date_format("%b-%Y")) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust=1)) +  
  xlab("")  + ylab("Millions of people\n") + 
  facet_wrap(~region, scales = "free") + 
  scale_colour_manual("", values  = c("darkorchid3", "darkorchid4", "deepskyblue2"))

p_excess <- ggarrange(p_global_excess, p_region_excess, ncol = 1, labels = "AUTO", common.legend = TRUE, legend= "bottom")
ggsave("global_age_analysis_2021/figures/fig_1_time_series_line_excess.pdf", p_excess, width =  12,  height = 12)


study_period = df_region_long[which(df_region_long$date == as.Date("2021-04-30") & df_region_long$key == "Orphanhood &/or caregiver loss"),]
whole_period = df_region_long[which(df_region_long$date == as.Date("2021-10-31") & df_region_long$key == "Orphanhood &/or caregiver loss"),]

difference_data = left_join(study_period, whole_period, by = c("region", "key"))
difference_data$diff = difference_data$value.y - difference_data$value.x
difference_data$percentage = difference_data$diff /difference_data$value.x * 100
print(difference_data)
  
