library(dplyr)
library(readr)

# Reads in orphans
orphans = data.frame(readRDS("orphans.RDS"))
orphans$country = row.names(orphans)
orphans$country[which(orphans$country == "USA")] = "US"

# Reads in deaths
deaths = read.csv("data/03-31-2021.csv", stringsAsFactors = FALSE)
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

# Read in WHO regions
regions <- read_csv(file = "data/who_regions.csv")
deaths_country = left_join(deaths_country, regions)

# Read in covariates
fertility = read_excel("data/WPP2019_FERT_F04_TOTAL_FERTILITY.xlsx", sheet = 2)
names(fertility) <- fertility[12,]
fertility <- fertility[13:length(fertility$Index), ]
fertility = select(fertility, "Region, subregion, country or area *", "2020-2025" )
names(fertility) = c("location_name", "tfr")

fertility_l = read_excel("data/WPP2019_FERT_F04_TOTAL_FERTILITY.xlsx", sheet = 4)
names(fertility_l) <- fertility_l[12,]
fertility_l <- fertility_l[13:length(fertility_l$Index), ]
fertility_l = select(fertility_l, "Region, subregion, country or area *", "2020-2025" )
names(fertility_l) = c("location_name", "tfr_l")

fertility_u = read_excel("data/WPP2019_FERT_F04_TOTAL_FERTILITY.xlsx", sheet = 3)
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

west_bank = fertility[which(fertility$location_name == "Israel"),]
west_bank$location_name = "West Bank and Gaza"
england = fertility[which(fertility$location_name == "United Kingdom"),]
england$location_name = "England"
andorra = fertility[which(fertility$location_name == "Spain"),]
andorra$location_name = "Andorra"
liechtenstein = fertility[which(fertility$location_name == "Austria"),]
liechtenstein$location_name = "Liechtenstein"
dominica = fertility[which(fertility$location_name == "Guadeloupe"),]
dominica$location_name = "Dominica"
marshall = fertility[which(fertility$location_name == "Micronesia (Federated States of)"),]
marshall$location_name = "Marshall Islands"
monaco = fertility[which(fertility$location_name == "France"),]
monaco$location_name = "Monaco"
kits = fertility[which(fertility$location_name == "Antigua and Barbuda"),]
kits$location_name = "Saint Kitts and Nevis"
marino = fertility[which(fertility$location_name == "Italy"),]
marino$location_name = "San Marino"
kosovo = fertility[which(fertility$location_name == "Serbia"),]
kosovo$location_name = "Kosovo"
fertility = rbind(fertility, west_bank, liechtenstein, england, andorra, dominica, marshall, monaco, kits, marino, kosovo)

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

# Change names for Shiny
joined$country[which(joined$country == "USA")] <- "United States of America"
joined$country[which(joined$country == "I.R. Iran")] <- "Iran (Islamic Republic of)"
joined$country[which(joined$country == "Gambia")] <- "Gambia (Republic of The)"
joined$country[which(joined$country == "Guinea-Bissau")] <- "Guinea Bissau"
joined$country[which(joined$country == "Czechia")] <- "Czech Republic"
joined$country[which(joined$country == "West Bank and Gaza")] <- "Occupied Palestinian Territory"

saveRDS(joined, "data/tfr_covariates.RDS")