library(dplyr)
library(readr)
library(readxl)

format_tfr_europe <- function(orphans_path, deaths_path){
  # Reads in orphans
  orphans = data.frame(readRDS(orphans_path))
  orphans$country = row.names(orphans)
  orphans$country[which(orphans$country == "USA")] = "US"
  
  # Reads in deaths
  deaths = read.csv(deaths_path, stringsAsFactors = FALSE)
  deaths_country = deaths %>% 
    group_by(Country_Region) %>%
    summarise(total_deaths = sum(Deaths)) 
  england = sum(deaths$Deaths[(deaths$Province_State == "England" | 
                                 deaths$Province_State == "Wales")])
  deaths_country = rbind(deaths_country, data.frame("Country_Region" = "England", 
                                                    "total_deaths" = england))
  deaths_country$total_deaths[deaths_country$Country_Region == "United Kingdom"] = 
    deaths_country$total_deaths[deaths_country$Country_Region == "United Kingdom"] - england
  deaths_country = deaths_country[which(!deaths_country$Country_Region %in% c("Diamond Princess", "Holy See", "MS Zaandam")),]
  deaths_country$Country_Region[which(deaths_country$Country_Region == "Taiwan*")] = "Taiwan"
  deaths_country$Country_Region[which(deaths_country$Country_Region == "Iran")] = "I.R. Iran"
  deaths_country$Country_Region[which(deaths_country$Country_Region == "Russia")] = "Russian Federation"
  deaths_country$Country_Region[which(deaths_country$Country_Region == "Bolivia")] = "Bolivia (Plurinational State of)"
  deaths_country$Country_Region[which(deaths_country$Country_Region == "Brunei")] = "Brunei Darussalam"
  deaths_country$Country_Region[which(deaths_country$Country_Region == "Burma")] = "Myanmar"
  deaths_country$Country_Region[which(deaths_country$Country_Region == "Congo (Brazzaville)")] = "Congo"
  deaths_country$Country_Region[which(deaths_country$Country_Region == "Congo (Kinshasa)")] = "Democratic Republic of the Congo"
  deaths_country$Country_Region[which(deaths_country$Country_Region == "Venezuela")] = "Venezuela (Bolivarian Republic of)"
  deaths_country$Country_Region[which(deaths_country$Country_Region == "Vietnam")] = "Viet Nam"
  deaths_country$Country_Region[which(deaths_country$Country_Region == "Syria")] = "Syrian Arab Republic"
  deaths_country$Country_Region[which(deaths_country$Country_Region == "Moldova")] = "Republic of Moldova"
  deaths_country$Country_Region[which(deaths_country$Country_Region == "Tanzania")] = "United Republic of Tanzania"
  deaths_country$Country_Region[which(deaths_country$Country_Region == "Korea, South")] = "Republic of Korea"
  deaths_country$Country_Region[which(deaths_country$Country_Region == "Laos")] = "Lao People's Democratic Republic"
  deaths_country$Country_Region[which(deaths_country$Country_Region == "Micronesia")] = "Micronesia (Federated States of)"
  
  # Remove Taiwan
  deaths_country <- deaths_country[which(deaths_country$Country_Region != "Taiwan"),]
  # Remove summer olympics
  deaths_country <- deaths_country[which(deaths_country$Country_Region != "Summer Olympics 2020"),]
  
  # Read in WHO regions
  regions <- read.csv(file = "global_age_analysis_2021/data/who_regions.csv", stringsAsFactor = FALSE)
  deaths_country = left_join(deaths_country, regions)
  
  # Read in covariates
  fertility = read_excel("global_age_analysis_2021/data/WPP2019_FERT_F04_TOTAL_FERTILITY.xlsx", sheet = 2)
  names(fertility) <- fertility[12,]
  fertility <- fertility[13:length(fertility$Index), ]
  fertility = select(fertility, "Region, subregion, country or area *", "2020-2025" )
  names(fertility) = c("location_name", "tfr")
  
  fertility_l = read_excel("global_age_analysis_2021/data/WPP2019_FERT_F04_TOTAL_FERTILITY.xlsx", sheet = 4)
  names(fertility_l) <- fertility_l[12,]
  fertility_l <- fertility_l[13:length(fertility_l$Index), ]
  fertility_l = select(fertility_l, "Region, subregion, country or area *", "2020-2025" )
  names(fertility_l) = c("location_name", "tfr_l")
  
  fertility_u = read_excel("global_age_analysis_2021/data/WPP2019_FERT_F04_TOTAL_FERTILITY.xlsx", sheet = 3)
  names(fertility_u) <- fertility_u [12,]
  fertility_u <- fertility_u [13:length(fertility_u $Index), ]
  fertility_u = select(fertility_u , "Region, subregion, country or area *", "2020-2025" )
  names(fertility_u) = c("location_name", "tfr_u")
  
  fertility = left_join(fertility, fertility_l)
  fertility = left_join(fertility, fertility_u)
  
  fertility$location_name[fertility$location_name == "Iran (Islamic Republic of)"] = "I.R. Iran"
  fertility$location_name[fertility$location_name == "United States of America"] = "US"
  fertility$location_name[fertility$location_name == "Côte d'Ivoire"] = "Cote d'Ivoire"
  fertility$location_name[fertility$location_name == "Micronesia (Fed. States of)"] = "Micronesia (Federated States of)"
  
  
  ihme_tfr <- read.csv("global_age_analysis_2021/data/IHME_GBD_2019_FERTILITY_1950_2019_TFR_Y2020M10D27.CSV")
  ihme_tfr <- ihme_tfr[which(ihme_tfr$year_id == "2019"),]
  
  england = fertility[which(fertility$location_name == "United Kingdom"),]
  england$location_name = "England"
  
  #world bank
  west_bank = data.frame(location_name = "West Bank and Gaza",
                         tfr = 3.56,
                         tfr_l = 3.56 - 0.127551*1.96,
                         tfr_u = 3.56 + 0.127551*1.96)
  
  liechtenstein = data.frame(location_name = "Liechtenstein",
                             tfr = 1.47,
                             tfr_l = 1.47 - 0.127551*1.96,
                             tfr_u = 1.47 + 0.127551*1.96)
  
  kosovo = data.frame(location_name = "Kosovo",
                      tfr = 1.97,
                      tfr_l = 1.97 - 0.127551*1.96,
                      tfr_u = 1.97 + 0.127551*1.96)
  
  
  # IHME
  tmp <- ihme_tfr[which(ihme_tfr$location_name == "Andorra"),]
  andorra = data.frame(location_name = "Andorra",
                       tfr = tmp$val,
                       tfr_l = tmp$lower,
                       tfr_u = tmp$upper)
  
  tmp <- ihme_tfr[which(ihme_tfr$location_name == "Dominica"),]
  dominica = data.frame(location_name = "Dominica",
                        tfr = tmp$val,
                        tfr_l = tmp$lower,
                        tfr_u = tmp$upper)
  
  tmp <- ihme_tfr[which(ihme_tfr$location_name == "Marshall Islands"),]
  marshall = data.frame(location_name = "Marshall Islands",
                        tfr = tmp$val,
                        tfr_l = tmp$lower,
                        tfr_u = tmp$upper)
  
  tmp <- ihme_tfr[which(ihme_tfr$location_name == "Monaco"),]
  monaco = data.frame(location_name = "Monaco",
                      tfr = tmp$val,
                      tfr_l = tmp$lower,
                      tfr_u = tmp$upper)
  
  tmp <- ihme_tfr[which(ihme_tfr$location_name == "Saint Kitts and Nevis"),]
  kits = data.frame(location_name = "Saint Kitts and Nevis",
                    tfr = tmp$val,
                    tfr_l = tmp$lower,
                    tfr_u = tmp$upper)
  
  tmp <- ihme_tfr[which(ihme_tfr$location_name == "San Marino"),]
  marino = data.frame(location_name = "San Marino",
                      tfr = tmp$val,
                      tfr_l = tmp$lower,
                      tfr_u = tmp$upper)
  
  tmp <- ihme_tfr[which(ihme_tfr$location_name == "Palau"),]
  palau = data.frame(location_name = "Palau",
                     tfr = tmp$val,
                     tfr_l = tmp$lower,
                     tfr_u = tmp$upper)
  
  fertility = rbind(fertility, west_bank, liechtenstein, england, andorra, dominica, marshall, monaco, kits, marino, kosovo,  palau)
  
  covariates = left_join(deaths_country, fertility, by = c("Country_Region" = "location_name"))
  covariates$tfr <- as.numeric(covariates$tfr)
  covariates$tfr_l <- as.numeric(covariates$tfr_l)
  covariates$tfr_u <- as.numeric(covariates$tfr_u)
  
  # Join with orphans
  joined = full_join(orphans, covariates, by = c("country" = "Country_Region"))
  joined$country[which(joined$country == "England")] = "England & Wales"
  joined$country[which(joined$country == "United Kingdom")] = "Scotland & Northern Ireland"
  joined$country[which(joined$country == "US")] = "USA"
  joined$fitting_deaths = ifelse(!is.na(joined$deaths), joined$deaths, joined$total_deaths)
  
  joined$all <- ifelse(is.na(joined$all), 0, joined$all)
  
  # Change names to match WHO
  joined$country[which(joined$country == "Gambia")] <- "Gambia (Republic of The)"
  joined$country[which(joined$country == "Guinea-Bissau")] <- "Guinea Bissau"
  joined$country[which(joined$country == "Czechia")] <- "Czech Republic"
  joined$country[which(joined$country == "West Bank and Gaza")] <- "Occupied Palestinian Territory"
  
  saveRDS(joined, "global_age_analysis_2021/data/tfr_covariates.RDS")
  
  #---------------------------------------------------------------------------------------------------------
  # Calculator
  joined$country[which(joined$country == "USA")] <- "United States of America"
  joined$country[which(joined$country == "I.R. Iran")] <- "Iran (Islamic Republic of)"
  joined$country[which(joined$country == "Gambia")] <- "Gambia (Republic of The)"
  joined$country[which(joined$country == "Guinea-Bissau")] <- "Guinea Bissau"
  joined$country[which(joined$country == "Czechia")] <- "Czech Republic"
  joined$country[which(joined$country == "West Bank and Gaza")] <- "Occupied Palestinian Territory"
  
  # Make data for shiny app
  joined$parents <- joined$mother + joined$father + joined$both
  joined$parents_ratio <- joined$parents / joined$fitting_deaths
  joined$primary_ratio <- joined$primary_loss / joined$fitting_deaths
  df <- select(joined, country, parents_ratio, primary_ratio, ratio)
  df <- df[!is.na(df$ratio),]
  saveRDS(df, "global_age_analysis_2021/data/shiny/calculated_ratios.RDS")
  
  df2 <- select(joined, country, fitting_deaths)
  names(df2) <- c("country", "total_deaths")
  saveRDS(df2, "global_age_analysis_2021/data/shiny/fitted_deaths.RDS")
  
  joined$europe = ifelse(joined$who_region == "European", 1, 0)
  df3 <- select(joined, country, europe,  tfr, tfr_l, tfr_u)
  saveRDS(df3, "global_age_analysis_2021/data/shiny/tfr.RDS")
}
