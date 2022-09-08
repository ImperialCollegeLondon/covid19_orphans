# This file produces a time series of orphans.
library(tidyverse)
library(readxl)

source("excess_deaths_update_2022/R/orphanhood_functions.R")

# Read in WHO data
load("excess_deaths_update_2022/data/excess.distribution.Rda")
d_country = df.dist.2
d_country = select(d_country, Country, sample, excess)
names(d_country) = c("country", "sample", "excess")

# Check global
global = df.dist.2 %>% 
  group_by(sample) %>%
  summarise(excess = sum(excess))
print(sprintf("%f [%f - %f]", mean(global$excess/1e6), quantile(global$excess, probs = 0.025)/1e6, quantile(global$excess, probs = 0.975)/1e6))

d_country$country[which(d_country$country == "The United Kingdom")] <- "England & Wales"
d_country$country[which(d_country$country == "Côte d'Ivoire")] <- "Cote d'Ivoire"
d_country$country[which(d_country$country == "Czechia")] <- "Czech Republic"
d_country$country[which(d_country$country == "Gambia")] <- "Gambia (Republic of The)"
d_country$country[which(d_country$country == "Guinea-Bissau")] <- "Guinea Bissau"
d_country$country[which(d_country$country == "Democratic People's Republic of Korea")] <- "Dem. People's Republic of Korea"

d_country_mean_negative = d_country %>% 
  group_by(country) %>%
  summarise(mean = mean(excess)) %>% 
  filter(mean < 0)
write.csv(d_country_mean_negative, "excess_deaths_update_2022/output/negative_excess_deaths_who.csv")

d_country$date = as.Date("2021/12/31")
d_country_wide = d_country %>% 
  group_by(country, sample, date) %>%
  summarise(excess = sum(excess))  %>%
  spread(key = sample, value = excess)

# Remove Turkmenistan
#d_country_wide = d_country_wide[which(d_country_wide$country != "Turkmenistan"),] 

deaths_country_who = d_country_wide

#---------------------------------------------------------------------------------------
# Read in jhu data
d <- readRDS("excess_deaths_update_2022/data/downloaded_timeseries_jhu.RDS")
d2 <- select(d, Country.Region, X5.1.22, X12.31.21)

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
deaths_country$Country.Region[which(deaths_country$Country.Region  == "Micronesia")] <- "Micronesia (Federated States of)"

#---------------------------------------------------------------------------------------
# Multiply JHU data by multipliers for study countries
multipliers_study <- read.csv('excess_deaths_update_2022/data/multipliers.csv', header = FALSE)
names(multipliers_study) <- c("Country.Region", "mult_study")
remove_study = c("Brazil", "India", "Mexico", "Peru", "South Africa", "Iran (Islamic Republic of)", "Colombia", "Russian Federation")
multipliers_study = multipliers_study[!multipliers_study$Country.Region %in% remove_study,]

## Switch KENYA
multipliers_study = rbind(multipliers_study, data.frame("Country.Region" = "Kenya",
                                                        "mult_study" = 10))

deaths_country = left_join(deaths_country, multipliers_study, by = ("Country.Region"))
deaths_country$mult_study[is.na(deaths_country$mult_study)] = 1
deaths_country$X5.1.22 = deaths_country$X5.1.22*deaths_country$mult_study
deaths_country$X12.31.21 = deaths_country$X12.31.21*deaths_country$mult_study
deaths_country$time_multiplier = deaths_country$X5.1.22 / deaths_country$X12.31.21

# Work out which countries need to switch out
mean_who = data.frame("country" = deaths_country_who$country,
                      "mean_excess" = rowMeans(deaths_country_who[,3:1002]))
mean_who = right_join(deaths_country, mean_who, by = c("Country.Region" = "country"))
mean_who$switch = ifelse(mean_who$X12.31.21 > mean_who$mean_excess, 1, 0)
mean_who = select(mean_who, Country.Region, switch, X12.31.21)

# Remove countries with no JHU data and negative excess
mean_who = mean_who[!is.na(mean_who$switch),]

# Switch out data
deaths_country_dec = right_join(deaths_country_who, mean_who, by = c("country" = "Country.Region"))
for (i in 1:1000){
  deaths_country_dec[,2+i] = ifelse(deaths_country_dec$switch == 1, deaths_country_dec$X12.31.21, unlist(deaths_country_dec[,2+i]))
}

#---------------------------------------------------------------------------------------
# Multiply by factor to get April deaths
deaths_country_apr = deaths_country_dec
deaths_country_apr = left_join(deaths_country_apr, deaths_country, by = c("country" = "Country.Region"))
for (i in 1:length(deaths_country_apr$country)){
  if (is.infinite(deaths_country_apr$time_multiplier[i]) | is.nan(deaths_country_apr$time_multiplier[i])){
    deaths_country_apr[i, 3:1002] = as.list(rep(deaths_country_apr$X5.1.22[i], 1000)) # Countries with no deaths at first time point
  } else {
    deaths_country_apr[i, 3:1002] = deaths_country_apr[i,3:1002] * deaths_country_apr$time_multiplier[i]
  }
}

deaths_country_apr$date = as.Date("2022-05-01")
deaths_country = rbind(deaths_country_dec, deaths_country_apr)

# Removes countries that are not in WHO timeseries
deaths_country = deaths_country[!deaths_country$country %in% c("Kiribati", "Marshall Islands", 
                                                               "Micronesia (Federated States of)", 
                                                               "Palau", "Samoa", "Solomon Islands", "Tonga"),]
# Join JHU with tfr data
data = readRDS("excess_deaths_update_2022/data/tfr_who.RDS")
data$country[which(data$country == "USA")] <- "United States of America"
data$country[which(data$country == "I.R. Iran")] <- "Iran (Islamic Republic of)"
data$sd = (data$tfr_u-data$tfr_l)/(2*1.96)
data$europe = ifelse(data$who_region == "European", 1, 0) 
data = select(data, "country", "tfr", "tfr_l",  "tfr_u", "sd", "who_region", "europe")
combined_data <- left_join(deaths_country, data, by = c("country"))
#tmp = unique(combined_data$country[is.na(combined_data$tfr)])
#print(length(tmp))
#tmp = unique(combined_data$country[is.na(combined_data$who_region)])
#print(length(tmp))

num_countries = print(sprintf("Number countries WHO: %s", length(unique(combined_data$country))))

# Calculate orphans
parents = NULL
primary = NULL
primary_secondary = NULL

dates = unique(combined_data$date)

for (i in 1:length(dates)){
  print(dates[i])
  samples = combined_data[which(combined_data$date == dates[i]), c(1,3:1002),]
  c_data <- combined_data[which(combined_data$date == dates[i]), 
                          c("country", "tfr", "tfr_l",  "tfr_u", "sd", "who_region", "europe")]
  names(c_data) <- c("country", "tfr", "tfr_l", "tfr_u", "sd",  "who_region", "europe")
  orphans <- calculate_all_orphans_time_series(c_data = c_data, date = dates[i], 
                                               uncertainty = TRUE, death_uncertainty = TRUE,
                                               num_samples = 5000, source = "who", samples = samples)
  
  primary_secondary <- rbind(primary_secondary, orphans[[1]])
  primary <- rbind(primary, orphans[[2]])
  parents <- rbind(parents, orphans[[3]])
}

primary_secondary$country[primary_secondary$country == "England & Wales"] <- "United Kingdom"
names(primary_secondary) <- c("country", "region", "date", "primary_secondary", "ps_lower", "ps_upper")

primary$country[primary$country == "England & Wales"] <- "United Kingdom"
names(primary) <- c("country", "region", "date", "primary", "p_lower", "p_upper")

parents$country[parents$country == "England & Wales"] <- "United Kingdom"
names(parents) <- c("country", "region", "date", "orphanhood", "or_lower", "or_upper")

# Join (including uncertainty bounds) 
dat_uncertainty <- left_join(primary_secondary, select(primary, country, date, primary, 
                                                       p_lower, p_upper), by = c("country", "date"))

dat_uncertainty <- left_join(dat_uncertainty,  select(parents, country, date, orphanhood, 
                                                      or_lower, or_upper), by = c("country", "date"))

write.csv(dat_uncertainty, "excess_deaths_update_2022/output/who_uncertainty_all.csv", row.names = FALSE)

print(dat_uncertainty[dat_uncertainty$country == "Global" & dat_uncertainty$date == "2021-12-31",])
print(dat_uncertainty[dat_uncertainty$country == "Global" & dat_uncertainty$date == "2022-05-01",])

write.csv(dat_uncertainty[dat_uncertainty$country == "Global",], 
          "excess_deaths_update_2022/output/who_uncertainty_global.csv", row.names = FALSE)

# Checking uncertainty intervals
#reg = dat_uncertainty[dat_uncertainty$country == dat_uncertainty$region,]
#print(sum(reg$primary_secondary < reg$ps_lower & reg$primary_secondary > reg$ps_upper))
#print(sum(reg$primary < reg$p_lower & reg$primary_secondary > reg$p_upper))
#print(sum(reg$orphanhood < reg$or_lower & reg$primary_secondary > reg$or_upper))

