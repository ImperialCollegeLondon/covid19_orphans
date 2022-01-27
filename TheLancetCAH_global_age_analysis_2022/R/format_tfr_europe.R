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
  regions <- read.csv(file = "TheLancetCAH_global_age_analysis_2022/data/who_regions.csv", stringsAsFactor = FALSE)
  deaths_country = left_join(deaths_country, regions)
  
  # Read in covariates
  fertility = read_excel("TheLancetCAH_global_age_analysis_2022/data/WPP2019_FERT_F04_TOTAL_FERTILITY.xlsx", sheet = 2)
  names(fertility) <- as.character(fertility[12,])
  fertility <- fertility[13:length(fertility$Index), ]
  fertility = select(fertility, "Region, subregion, country or area *", "2020-2025" )
  names(fertility) = c("location_name", "tfr")
  
  fertility_l = read_excel("TheLancetCAH_global_age_analysis_2022/data/WPP2019_FERT_F04_TOTAL_FERTILITY.xlsx", sheet = 4)
  names(fertility_l) <- as.character(fertility_l[12,])
  fertility_l <- fertility_l[13:length(fertility_l$Index), ]
  fertility_l = select(fertility_l, "Region, subregion, country or area *", "2020-2025" )
  names(fertility_l) = c("location_name", "tfr_l")
  
  fertility_u = read_excel("TheLancetCAH_global_age_analysis_2022/data/WPP2019_FERT_F04_TOTAL_FERTILITY.xlsx", sheet = 3)
  names(fertility_u) <- as.character(fertility_u [12,])
  fertility_u <- fertility_u [13:length(fertility_u $Index), ]
  fertility_u = select(fertility_u , "Region, subregion, country or area *", "2020-2025" )
  names(fertility_u) = c("location_name", "tfr_u")
  
  fertility = left_join(fertility, fertility_l)
  fertility = left_join(fertility, fertility_u)
  
  fertility$location_name[fertility$location_name == "Iran (Islamic Republic of)"] = "I.R. Iran"
  fertility$location_name[fertility$location_name == "United States of America"] = "US"
  fertility$location_name[fertility$location_name == "Côte d'Ivoire"] = "Cote d'Ivoire"
  fertility$location_name[fertility$location_name == "Micronesia (Fed. States of)"] = "Micronesia (Federated States of)"
  
  ihme_tfr <- read.csv("TheLancetCAH_global_age_analysis_2022/data/IHME_GBD_2019_FERTILITY_1950_2019_TFR_Y2020M10D27.CSV")
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
  
  saveRDS(joined, "TheLancetCAH_global_age_analysis_2022/data/tfr_covariates.RDS")
  
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
  saveRDS(df, "TheLancetCAH_global_age_analysis_2022/data/shiny/calculated_ratios.RDS")
  
  df2 <- select(joined, country, fitting_deaths)
  names(df2) <- c("country", "total_deaths")
  saveRDS(df2, "TheLancetCAH_global_age_analysis_2022/data/shiny/fitted_deaths.RDS")
  
  joined$europe = ifelse(joined$who_region == "European", 1, 0)
  df3 <- select(joined, country, europe,  tfr, tfr_l, tfr_u)
  saveRDS(df3, "TheLancetCAH_global_age_analysis_2022/data/shiny/tfr.RDS")
}

format_tfr_europe_excess <- function(orphans_path, deaths_path){
  # Reads in orphans
  orphans_path = "TheLancetCAH_global_age_analysis_2022/orphans.RDS"
  orphans = data.frame(readRDS(orphans_path))
  orphans$country = row.names(orphans)
  orphans$country[which(orphans$country == "USA")] = "US"
  orphans$country[which(orphans$country == "England")] = "United Kingdom"
  
  # Reads in deaths
  excess_deaths = excess = readRDS("TheLancetCAH_global_age_analysis_2022/data/exces_deaths.RDS")
  excess$country = countrycode(excess$iso3c, "iso3c", 
                               "country.name")
  excess_oct = excess[which(excess$date == as.Date("2021-11-01")),]
  excess_oct$country[which(excess_oct$country == "Iran")] <- "I.R. Iran"
  excess_oct$country[which(excess_oct$country == "United States")] <- "US"
  excess_oct$country[which(excess_oct$country == "Russia")] <- "Russian Federation"
  excess_oct$country[which(excess_oct$country == "Bolivia")] = "Bolivia (Plurinational State of)"
  excess_oct$country[which(excess_oct$country == "Brunei")] = "Brunei Darussalam"
  excess_oct$country[which(excess_oct$country == "Myanmar (Burma)")] = "Myanmar"
  excess_oct$country[which(excess_oct$country == "Congo - Brazzaville")] = "Congo"
  excess_oct$country[which(excess_oct$country == "Congo - Kinshasa")] = "Democratic Republic of the Congo"
  excess_oct$country[which(excess_oct$country == "Venezuela")] = "Venezuela (Bolivarian Republic of)"
  excess_oct$country[which(excess_oct$country == "Vietnam")] = "Viet Nam"
  excess_oct$country[which(excess_oct$country == "Syria")] = "Syrian Arab Republic"
  excess_oct$country[which(excess_oct$country == "Moldova")] = "Republic of Moldova"
  excess_oct$country[which(excess_oct$country == "Tanzania")] = "United Republic of Tanzania"
  excess_oct$country[which(excess_oct$country == "Laos")] = "Lao People's Democratic Republic"
  excess_oct$country[which(excess_oct$country == "Côte d’Ivoire")] = "Cote d'Ivoire"
  excess_oct$country[which(excess_oct$country == "Antigua & Barbuda")] = "Antigua and Barbuda"
  excess_oct$country[which(excess_oct$country == "Bosnia & Herzegovina")] = "Bosnia and Herzegovina"
  excess_oct$country[which(excess_oct$country == "Cape Verde")] = "Cabo Verde"
  excess_oct$country[which(excess_oct$country == "St. Kitts & Nevis")] = "Saint Kitts and Nevis"
  excess_oct$country[which(excess_oct$country == "St. Lucia")] = "Saint Lucia"
  excess_oct$country[which(excess_oct$country == "North Korea")] = "Dem. People's Republic of Korea"
  excess_oct$country[which(excess_oct$country == "São Tomé & Príncipe")] = "Sao Tome and Principe"
  excess_oct$country[which(excess_oct$country == "Trinidad & Tobago")] = "Trinidad and Tobago"
  excess_oct$country[which(excess_oct$country == "St. Vincent & Grenadines")] = "Saint Vincent and the Grenadines"
  excess_oct$country[which(excess_oct$country == "Curaçao")] = "Curacao"
  
  # Remove Taiwan
  excess_oct <- excess_oct[which(excess_oct$country != "Taiwan"),]
  # Remove Vatican city (zero tfr)
  excess_oct <- excess_oct[which(excess_oct$country != "Vatican City"),]
  
  # Remove countries with negative excess deaths 
  excess_oct <- excess_oct[which(excess_oct$cumulative_estimated_daily_excess_deaths > 0),]
  
  # Read in WHO regions
  regions <- read.csv(file = "TheLancetCAH_global_age_analysis_2022/data/who_regions.csv", stringsAsFactor = FALSE)
  deaths_country = left_join(excess_oct, regions, by = c("country" = "Country_Region"))
  
  # Read in covariates
  fertility = read_excel("TheLancetCAH_global_age_analysis_2022/data/WPP2019_FERT_F04_TOTAL_FERTILITY.xlsx", sheet = 2)
  names(fertility) <- fertility[12,]
  fertility <- fertility[13:length(fertility$Index), ]
  fertility = select(fertility, "Region, subregion, country or area *", "2020-2025" )
  names(fertility) = c("location_name", "tfr")
  
  fertility_l = read_excel("TheLancetCAH_global_age_analysis_2022/data/WPP2019_FERT_F04_TOTAL_FERTILITY.xlsx", sheet = 4)
  names(fertility_l) <- fertility_l[12,]
  fertility_l <- fertility_l[13:length(fertility_l$Index), ]
  fertility_l = select(fertility_l, "Region, subregion, country or area *", "2020-2025" )
  names(fertility_l) = c("location_name", "tfr_l")
  
  fertility_u = read_excel("TheLancetCAH_global_age_analysis_2022/data/WPP2019_FERT_F04_TOTAL_FERTILITY.xlsx", sheet = 3)
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
  fertility$location_name[fertility$location_name == "Curaçao"] = "Curacao"
  
  
  ihme_tfr <- read.csv("TheLancetCAH_global_age_analysis_2022/data/IHME_GBD_2019_FERTILITY_1950_2019_TFR_Y2020M10D27.CSV")
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
  
  cayman = data.frame(location_name = "Cayman Islands",
                      tfr = 1.83,
                      tfr_l = 1.83 - 0.127551*1.96,
                      tfr_u =  1.83 + 0.127551*1.96)
  
  gibraltar = data.frame(location_name = "Gibraltar",
                      tfr = 1.91,
                      tfr_l = 1.91 - 0.127551*1.96,
                      tfr_u = 1.91 + 0.127551*1.96)
  
  hongkong = data.frame(location_name = "Hong Kong SAR China",
                         tfr = 1.05,
                         tfr_l = 1.05 - 0.127551*1.96,
                         tfr_u = 1.05 + 0.127551*1.96)
  
  montserrat = data.frame(location_name = "Montserrat",
                        tfr = 1.31,
                        tfr_l = 1.31 - 0.127551*1.96,
                        tfr_u = 1.31 + 0.127551*1.96)
  
  pal_ter = data.frame(location_name = "Palestinian Territories",
                       tfr = 3.56,
                       tfr_l = 3.56 - 0.127551*1.96,
                       tfr_u = 3.56 + 0.127551*1.96)

  maarten = data.frame(location_name = "Sint Maarten",
                      tfr =  2.03,
                      tfr_l =  2.03 - 0.127551*1.96,
                      tfr_u =  2.03 + 0.127551*1.96)
  
  bvi = data.frame(location_name = "British Virgin Islands",
                       tfr =  1.34,
                       tfr_l =  1.34 - 0.127551*1.96,
                       tfr_u =  1.34 + 0.127551*1.96)
  
  turks = data.frame(location_name = "Turks & Caicos Islands",
                     tfr =  1.7,
                     tfr_l =  1.7 - 0.127551*1.96,
                     tfr_u =  1.7 + 0.127551*1.96)

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
  
  tmp <- ihme_tfr[which(ihme_tfr$location_name == "Bermuda"),]
  bermuda = data.frame(location_name = "Bermuda",
                     tfr = tmp$val,
                     tfr_l = tmp$lower,
                     tfr_u = tmp$upper)
  
  fertility = rbind(fertility, west_bank, liechtenstein, england, andorra, dominica, marshall, monaco, 
                    kits, marino, kosovo,  palau, bermuda, cayman, gibraltar, hongkong, montserrat,
                    pal_ter, maarten, bvi, turks)
  
  covariates = left_join(deaths_country, fertility, by = c("country" = "location_name"))
  covariates$tfr <- as.numeric(covariates$tfr)
  covariates$tfr_l <- as.numeric(covariates$tfr_l)
  covariates$tfr_u <- as.numeric(covariates$tfr_u)
  
  # Join with orphans
  joined = full_join(orphans, covariates, by = c("country"))
  joined$country[which(joined$country == "England")] = "England & Wales"
  joined$country[which(joined$country == "US")] = "USA"
  joined$fitting_deaths = ifelse(!is.na(joined$deaths), joined$deaths, joined$cumulative_estimated_daily_excess_deaths)
  
  joined$all <- ifelse(is.na(joined$all), 0, joined$all)
  
  # Change names to match WHO
  joined$country[which(joined$country == "Gambia")] <- "Gambia (Republic of The)"
  joined$country[which(joined$country == "Guinea-Bissau")] <- "Guinea Bissau"
  joined$country[which(joined$country == "Czechia")] <- "Czech Republic"
  joined$country[which(joined$country == "West Bank and Gaza")] <- "Occupied Palestinian Territory"
  
  saveRDS(joined, "TheLancetCAH_global_age_analysis_2022/data/tfr_covariates_excess.RDS")
  
  # #---------------------------------------------------------------------------------------------------------
  # # Calculator
  # joined$country[which(joined$country == "USA")] <- "United States of America"
  # joined$country[which(joined$country == "I.R. Iran")] <- "Iran (Islamic Republic of)"
  # joined$country[which(joined$country == "Gambia")] <- "Gambia (Republic of The)"
  # joined$country[which(joined$country == "Guinea-Bissau")] <- "Guinea Bissau"
  # joined$country[which(joined$country == "Czechia")] <- "Czech Republic"
  # joined$country[which(joined$country == "West Bank and Gaza")] <- "Occupied Palestinian Territory"
  # 
  # # Make data for shiny app
  # joined$parents <- joined$mother + joined$father + joined$both
  # joined$parents_ratio <- joined$parents / joined$fitting_deaths
  # joined$primary_ratio <- joined$primary_loss / joined$fitting_deaths
  # df <- select(joined, country, parents_ratio, primary_ratio, ratio)
  # df <- df[!is.na(df$ratio),]
  # saveRDS(df, "TheLancetCAH_global_age_analysis_2022/data/shiny/calculated_ratios.RDS")
  # 
  # df2 <- select(joined, country, fitting_deaths)
  # names(df2) <- c("country", "total_deaths")
  # saveRDS(df2, "TheLancetCAH_global_age_analysis_2022/data/shiny/fitted_deaths.RDS")
  # 
  # joined$europe = ifelse(joined$who_region == "European", 1, 0)
  # df3 <- select(joined, country, europe,  tfr, tfr_l, tfr_u)
  # saveRDS(df3, "TheLancetCAH_global_age_analysis_2022/data/shiny/tfr.RDS")
}