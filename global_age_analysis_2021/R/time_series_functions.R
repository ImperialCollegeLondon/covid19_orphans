library(dplyr)
library(lubridate)
library(matrixStats)
library(RCurl)
library(stringr)
source("global_age_analysis_2021/R/utils.R")

logistic_model <- function(tfr, europe,  coef){
  return(coef[4] * (exp(coef[1] + coef[2] * tfr + coef[3] * europe))/(1 + exp(coef[1] + coef[2] * tfr + coef[3]  * europe)))
}

calculate_orphans_time <- function(country_data, coeffs, study_ratios, date){
  set.seed(10)
  
  names(study_ratios) <- c("country", "ratio")
  country_data <- left_join(country_data, study_ratios, by = "country")
  country_data$estimated_ratio <- logistic_model(country_data$tfr, country_data$europe, coef = coeffs)
  country_data$central_ratio <- ifelse(is.na(country_data$ratio), 
                                       country_data$estimated_ratio, country_data$ratio)
  
  central <-  country_data$central_ratio * country_data$total_deaths
  
  # Calculate uncertainty
  n = 1000
  estimated_ratio <- matrix(nrow = length(country_data$country), ncol = n)
  estimated_orphans <- matrix(nrow = length(country_data$country), ncol = n)
  
  for (i in 1:n){
    rn <- rnorm(length(country_data$country), mean = country_data$tfr, sd = country_data$sd)
    estimated_ratio[, i] <- logistic_model(rn, country_data$europe, coeffs)
    estimated_orphans[, i] <- estimated_ratio[, i] * country_data$total_deaths
  }
  uq <- rowQuantiles(estimated_orphans, probs = c(0.025, 0.975))
 
    
  orphans <- data.frame(country = country_data$country,
                        region = country_data$who_region,
                        date = rep(date, length(country_data$country)),
                        central = central,
                        lower = uq[,1],
                        upper = uq[,2])
  
  orphans$lower[1:21] <- orphans$central[1:21] 
  orphans$upper[1:21] <- orphans$central[1:21] 
  return (orphans)
}

calculate_all_orphans_time_series <- function(c_data, date){
  # Read study data in
  study_ratios <- readRDS("global_age_analysis_2021/data/shiny/calculated_ratios.RDS")
  
  # Calculate
  parents_orphans <- calculate_orphans_time(c_data, readRDS("global_age_analysis_2021/data/shiny/parent_coefficients.RDS"), 
                                       study_ratios[,c(1,2)], date)
  
  return (parents_orphans)
}