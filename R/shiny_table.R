library(dplyr)
library(lubridate)
library(matrixStats)
library(RCurl)
library(stringr)

logistic_model <- function(tfr, coef){
  return(coef[3] * (exp(coef[1] + coef[2] * tfr))/(1 + exp(coef[1] + coef[2] * tfr)))
}

round.choose <- function(x, roundTo, dir = 1) {
  if(dir == 1) {  ##ROUND UP
    x + (roundTo - x %% roundTo)
  } else {
    if(dir == 0) {  ##ROUND DOWN
      x - (x %% roundTo)
    }
  }
}

calculate_orphans <- function(country_data, coeffs, study_ratios){
  set.seed(10)
  
  names(study_ratios) <- c("country", "ratio")
  country_data <- left_join(country_data, study_ratios, by = "country")
  country_data$estimated_ratio <- logistic_model(country_data$tfr, coef = coeffs)
  country_data$central_ratio <- ifelse(is.na(country_data$ratio), 
                                       country_data$estimated_ratio, country_data$ratio)
  
  central <-  country_data$central_ratio * country_data$total_deaths
  
  # Calculate uncertainty
  n = 1000
  estimated_ratio <- matrix(nrow = length(country_data$country), ncol = n)
  estimated_orphans <- matrix(nrow = length(country_data$country), ncol = n)
  
  for (i in 1:n){
    rn <- rnorm(length(country_data$country), mean = country_data$tfr, sd = country_data$sd)
    estimated_ratio[, i] <- logistic_model(rn, coeffs)
    estimated_orphans[, i] <- estimated_ratio[, i] * country_data$total_deaths
  }
  uq <- rowQuantiles(estimated_orphans, probs = c(0.025, 0.975))
  if (length(country_data$country) == 1){
    orphans <- paste0(format(signif(central, 2), digits = 0), 
                      " [", round.choose(uq[1], 100, 0), " - ", 
                      round.choose(uq[2], 100, 1), "]")
  } else {
    orphans <- paste0(format(signif(central, 2), digits = 0), 
                      " [", round.choose(uq[,1], 100, 0), " - ", 
                      round.choose(uq[,2], 100, 1), "]")
  }

  orphans[1:21] <- format(signif(central[1:21], 2), digits = 0)
  return (orphans)
}

calculate_orphans_time <- function(country_data, coeffs, study_ratios, date){
  set.seed(10)
  
  names(study_ratios) <- c("country", "ratio")
  country_data <- left_join(country_data, study_ratios, by = "country")
  country_data$estimated_ratio <- logistic_model(country_data$tfr, coef = coeffs)
  country_data$central_ratio <- ifelse(is.na(country_data$ratio), 
                                       country_data$estimated_ratio, country_data$ratio)
  
  central <-  country_data$central_ratio * country_data$total_deaths
  
  # Calculate uncertainty
  n = 1000
  estimated_ratio <- matrix(nrow = length(country_data$country), ncol = n)
  estimated_orphans <- matrix(nrow = length(country_data$country), ncol = n)
  
  for (i in 1:n){
    rn <- rnorm(length(country_data$country), mean = country_data$tfr, sd = country_data$sd)
    estimated_ratio[, i] <- logistic_model(rn, coeffs)
    estimated_orphans[, i] <- estimated_ratio[, i] * country_data$total_deaths
  }
  uq <- rowQuantiles(estimated_orphans, probs = c(0.025, 0.975))
 
    
  orphans <- data.frame(country = country_data$country,
                        date = rep(date, length(country_data$country)),
                        central = central,
                        lower = uq[,1],
                        upper = uq[,2])
  
  orphans$lower[1:21] <- orphans$central[1:21] 
  orphans$upper[1:21] <- orphans$central[1:21] 
  return (orphans)
}

calculate_all_orphans <- function(combined_data, date){
  # Read study data in
  study_ratios <- readRDS("data/calculated_ratios.RDS")
  
  # Calculate
  ps_orphans <- calculate_orphans(combined_data, readRDS("data/primary_secondary_coefficients.RDS"), 
                                  study_ratios[,c(1,4)])
  primary_orphans <- calculate_orphans(combined_data, readRDS("data/primary_coefficients.RDS"), 
                                       study_ratios[,c(1,3)])
  parents_orphans <- calculate_orphans(combined_data, readRDS("data/parent_coefficients.RDS"), 
                                       study_ratios[,c(1,2)])
  
  df <- data.frame("Date" = rep(date, length(combined_data$country)),
                   "Country" = combined_data$country,
                   "Total deaths" = combined_data$total_deaths,
                   "Total fertility rate" = combined_data$tfr, 
                   "Parents" = parents_orphans,
                   "Primary" = primary_orphans,
                   "Primary and Secondary" = ps_orphans)
  
  return (df)
}

calculate_all_orphans_time_series <- function(c_data, date){
  # Read study data in
  study_ratios <- readRDS("data/calculated_ratios.RDS")
  
  # Calculate
  ps_orphans <- calculate_orphans_time(c_data, readRDS("data/primary_secondary_coefficients.RDS"), 
                                  study_ratios[,c(1,4)], date)
  primary_orphans <- calculate_orphans_time(c_data, readRDS("data/primary_coefficients.RDS"), 
                                       study_ratios[,c(1,3)], date)
  parents_orphans <- calculate_orphans_time(c_data, readRDS("data/parent_coefficients.RDS"), 
                                       study_ratios[,c(1,2)], date)
  
  dat <- list(ps_orphans, 
              primary_orphans, 
              parents_orphans)
  
  return (dat)
}

orphans_from_study <- function(country, date){
  deaths <- readRDS("data/fitted_deaths.RDS")

  # Join study with tfr data
  data = readRDS("data/tfr.RDS")
  combined_data <- left_join(data, deaths, by = "country")
  combined_data$sd = (combined_data$tfr_u-combined_data$tfr_l)/(2*1.96)
  
  # Calculate orphans
  orphans <- calculate_all_orphans(combined_data, date)
  orphans <- orphans[which(orphans$Country %in% country),]
  
  return(orphans)
}

orphans_from_jhu <- function(country, date){
  date_vec <- str_split(date, "-")
  # Read in JHU data
  jhu_url <- getURL(paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/", 
                           date_vec[[1]][2], "-", date_vec[[1]][3], "-", date_vec[[1]][1],".csv"))
  deaths <- read.csv(text = jhu_url, stringsAsFactors = FALSE)
  
  # Format JHU data
  deaths_country = deaths %>% 
    group_by(Country_Region) %>%
    summarise(total_deaths = sum(Deaths)) 
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
  deaths_country$Country_Region[which(deaths_country$Country_Region == "US")] <- "United States of America"
  deaths_country$Country_Region[which(deaths_country$Country_Region == "I.R. Iran")] <- "Iran (Islamic Republic of)"
  deaths_country$Country_Region[which(deaths_country$Country_Region  == "Gambia")] <- "Gambia (Republic of The)"
  deaths_country$Country_Region[which(deaths_country$Country_Region == "Guinea-Bissau")] <- "Guinea Bissau"
  deaths_country$Country_Region[which(deaths_country$Country_Region  == "Czechia")] <- "Czech Republic"
  
  england = sum(deaths$Deaths[(deaths$Province_State == "England" | 
                                 deaths$Province_State == "Wales")])
  deaths_country = rbind(deaths_country, data.frame("Country_Region" = "England & Wales", 
                                                    "total_deaths" = england))
  deaths_country$total_deaths[deaths_country$Country_Region == "United Kingdom"] = 
    deaths_country$total_deaths[deaths_country$Country_Region == "United Kingdom"] - england
  deaths_country$Country_Region[deaths_country$Country_Region == "United Kingdom"] = "Scotland & Northern Ireland"
  
  # Multiply study country deaths by multipliers
  multipliers <- read.csv('data/multipliers.csv', header = FALSE)
  names(multipliers) <- c("Country", "mult")
  multipliers = multipliers[multipliers$Country != "Peru",]
  deaths_country <- left_join(deaths_country, multipliers, by = c("Country_Region" = "Country"))
  deaths_country$mult <- ifelse(is.na(deaths_country$mult), 1, deaths_country$mult)
  deaths_country$total_deaths <- deaths_country$total_deaths * deaths_country$mult
  
  # Join JHU with tfr data
  data = readRDS("data/tfr.RDS")
  combined_data <- left_join(data, deaths_country, by = c("country" = "Country_Region"))
  combined_data$sd = (combined_data$tfr_u-combined_data$tfr_l)/(2*1.96)
  
  # Calculate orphans
  orphans <- calculate_all_orphans(combined_data, date)
  orphans <- orphans[which(orphans$Country %in% country),]
  
  return(orphans)
}

orphans_from_own <- function(country, deaths, date){
  # Combine tfr and deaths,
  combined_data = readRDS("data/tfr.RDS")
  combined_data$sd = (combined_data$tfr_u-combined_data$tfr_l)/(2*1.96)
  combined_data$total_deaths <- rep(as.numeric(deaths), length(combined_data$country))

  # Calculate orphans
  orphans <- calculate_all_orphans(combined_data, date)
  orphans <- orphans[which(orphans$Country %in% country),]
  return(orphans)
  
}

estimate_data <- function(country, date, data, own_data){
  orphans <- NULL
  study_ratios <- readRDS("data/calculated_ratios.RDS")
  if (data == 1){
    orphans <- orphans_from_study(country, as.Date("2021-04-30"))
    orphans$Country <- ifelse(orphans$Country %in% study_ratios$country, paste0(orphans$Country, "*"), orphans$Country)
    orphans$Date <- format(orphans$Date,'%Y-%m-%d')
    orphans$Total.deaths <- format(orphans$Total.deaths, digits = 0)
    names(orphans) <- c("Date", "Country", "Deaths", "Total fertility rate", "Estimates of orphanhood",
                        "Estimates of orphanhood or loss of primary caregivers",
                        "Estimates of loss of primary and/or secondary caregivers")
  } else if (data == 2){
    orphans <- orphans_from_jhu(country, date)
    orphans$Country <- ifelse(orphans$Country %in% study_ratios$country, paste0(orphans$Country, "*"), orphans$Country)
    orphans$Date <- format(orphans$Date,'%Y-%m-%d')
    orphans$Total.deaths <- format(orphans$Total.deaths, digits = 0)
    names(orphans) <- c("Date", "Country", "Deaths", "Total fertility rate", "Estimates of orphanhood",
                        "Estimates of orphanhood or loss of primary caregivers",
                        "Estimates of loss of primary and/or secondary caregivers")
  } else if (data == 3){
    orphans <- orphans_from_own(country, own_data, date)
    orphans$Country <- ifelse(orphans$Country %in% study_ratios$country, paste0(orphans$Country, "*"), orphans$Country)
    orphans$Date <- format(orphans$Date,'%Y-%m-%d')
    orphans$Total.deaths <- format(orphans$Total.deaths, digits = 0)
    names(orphans) <- c("Date", "Country", "Deaths", "Total fertility rate", "Estimates of orphanhood",
                        "Estimates of orphanhood or death of primary grandparent caregiver",
                        "Estimates of orphanhood or death of primary and/or secondary grandparent caregiver")
  }
  return (orphans)
}

