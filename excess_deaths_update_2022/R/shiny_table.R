library(dplyr)
library(lubridate)
library(matrixStats)
library(RCurl)
library(stringr)
round.choose <- function(x, roundTo, dir = 1) {
  if(dir == 1) {  ##ROUND UP
    x + (roundTo - x %% roundTo)
  } else {
    if(dir == 0) {  ##ROUND DOWN
      x - (x %% roundTo)
    }
  }
}


study_countries <- c("England & Wales", "France", "Germany", "Italy", "Poland",
                     "Russian Federation", "Spain", "Argentina", "Brazil",
                     "Colombia", "Mexico", "Peru", "United States of America", 
                     "Kenya", "Malawi", "Nigeria", "South Africa", "Zimbabwe",
                     "India", "Iran (Islamic Republic of)", "Philippines")
study_countries <- study_countries[order(study_countries)]


logistic_model <- function(tfr, europe,  coef){
  return(coef[4] * (exp(coef[1] + coef[2] * tfr + coef[3] * europe))/(1 + exp(coef[1] + coef[2] * tfr + coef[3]  * europe)))
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

calculate_orphans_time <- function(country_data, coeffs, study_ratios, date, 
                                   uncertainty, death_uncertainty, num_samples){
  set.seed(10)
  
  names(study_ratios) <- c("country", "ratio")
  country_data <- left_join(country_data, study_ratios, by = "country")
  country_data$estimated_ratio <- logistic_model(country_data$tfr, country_data$europe, coef = coeffs)
  country_data$central_ratio <- ifelse(is.na(country_data$ratio), 
                                       country_data$estimated_ratio, country_data$ratio)
  
  central <-  country_data$central_ratio * country_data$total_deaths
  global_central = sum(central)

  regions = data.frame(who_regions = country_data$who_region, 
                       central = central)
  
  regions$who_regions[which(regions$who_regions == "Eastern European")] = "European"
  regions_central = regions %>%
    group_by(who_regions) %>%
    summarise(central = sum(central))
  
  if (uncertainty == FALSE){
    
    orphans <- data.frame(country = country_data$country,
                          region = country_data$who_region,
                          date = rep(date, length(country_data$country)),
                          central = central)
    
    global <- data.frame(country = "Global",
                         region = "Global",
                         date = date, 
                         central = global_central)
    regions_central$date = date
    regions_central$region = regions_central$who_regions
    names(regions_central) = c("country", "central", "date", "region")

    regions <- rbind(global, regions_central)
    orphans <- rbind(regions, orphans)
    
  } else {
    # Calculate uncertainty
    n = num_samples
    estimated_ratio <- matrix(nrow = length(country_data$country), ncol = n)
    estimated_orphans <- matrix(nrow = length(country_data$country), ncol = n)
    
    for (i in 1:n){
      rn <- rnorm(length(country_data$country), mean = country_data$tfr, sd = country_data$sd)
      estimated_ratio[, i] <- logistic_model(rn, country_data$europe, coeffs)
      
      #Use study country ratio
      estimated_ratio[, i] <- ifelse(is.na(country_data$ratio), estimated_ratio[, i], country_data$ratio)
      
      if (death_uncertainty == TRUE){
        # standard deviations for the deaths
        deaths_sd <- (country_data$upper - country_data$lower) / (1.9*2)
        
        dn <- rnorm(length(country_data$country), mean = country_data$total_deaths, sd = deaths_sd)
        estimated_orphans[, i] <- estimated_ratio[, i] * dn
      
        } else{
        estimated_orphans[, i] <- estimated_ratio[, i] * country_data$total_deaths
      }
      
    }
    
    # Save orphanhood samples for use later
    estimated_orphans_ = as.data.frame(estimated_orphans)
    estimated_orphans_$country = country_data$country
    saveRDS(estimated_orphans_, file = "excess_deaths_update_2022/output/orphanhood_samples.RDS")
    
    uq <- rowQuantiles(estimated_orphans, probs = c(0.025, 0.975))
    
    orphans <- data.frame(country = country_data$country,
                          region = country_data$who_region,
                          date = rep(date, length(country_data$country)),
                          central = central,
                          lower = uq[,1],
                          upper = uq[,2])
    
    # aggregate uncertainties
    estimated_orphans_$region = country_data$who_region
    estimated_orphans_$region[which(estimated_orphans_$region == "Eastern European")] = "European"
    
    # regional uncertainties
    regions_ <- aggregate(estimated_orphans_[ , 1:(length(estimated_orphans_)-2)],
                          by = list(estimated_orphans_$region), FUN = sum) 
    
    regions_ <- as.matrix(select(regions_, colnames(regions_)[2:length(colnames(regions_))]))
    regions_uq <- rowQuantiles(regions_,
                               probs = c(0.025, 0.975))
    
    region_names <- unique(estimated_orphans_$region)[order(unique(estimated_orphans_$region))]
    regions__ <- data.frame(country = region_names,
                            region = region_names,
                            date = rep(date, length(region_names)),
                            lower = regions_uq[,1],
                            upper = regions_uq[,2])
    regions__ = left_join(regions__, regions_central, by=c("region" = "who_regions"))
    
    # global uncertainties
    global_ <- as.data.frame(colSums(as.data.frame(estimated_orphans)))
    colnames(global_) <- "estimates"
    global_uq <- quantile(global_$estimates, probs = c(0.025, 0.975))
    
    global__ <- data.frame(country = c("Global"),
                           region = c("Global"),
                           date = date,
                           central = global_central,
                           lower = global_uq[1],
                           upper = global_uq[2])
    
    global_sorted <- sort(unlist(global_))
    quants_regions = NULL
    for (i in 1:length(region_names)){
      regions_sorted <- sort(regions_[i,])
      percent = regions_sorted / global_sorted
      percent_quantiles = quantile(percent, probs = c(0.025, 0.975)) * 100
      quants_regions = rbind(quants_regions, data.frame(region = region_names[i], 
                                                        quantiles = sprintf("%.1f%% [%.1f%%-%.1f%%]", 
                                                                            round(regions_central[i,2]/ global_central * 100, 1),
                                                                            round.choose(percent_quantiles[[1]], 0.1, 0), 
                                                                            round.choose(percent_quantiles[[2]], 0.1, 1))))
    }
    print(quants_regions)
  

    
    regions__ <- rbind(global__, regions__)
    orphans <- rbind(regions__, orphans)
    
  }
  
  return (orphans)
}


calculate_all_orphans_time_series <- function(c_data, date, uncertainty, death_uncertainty, num_samples){
  # Read study data in
  study_ratios <- readRDS("excess_deaths_update_2022/data/calculated_ratios.RDS")
  
  # Calculate
  ps_orphans <- calculate_orphans_time(c_data, readRDS("excess_deaths_update_2022/data/primary_secondary_coefficients.RDS"), 
                                  study_ratios[,c(1,4)], date, uncertainty = uncertainty, 
                                  death_uncertainty = death_uncertainty, num_samples)
  primary_orphans <- calculate_orphans_time(c_data, readRDS("excess_deaths_update_2022/data/primary_coefficients.RDS"), 
                                       study_ratios[,c(1,3)], date, uncertainty = uncertainty, 
                                       death_uncertainty = death_uncertainty, num_samples)
  parents_orphans <- calculate_orphans_time(c_data, readRDS("excess_deaths_update_2022/data/parent_coefficients.RDS"), 
                                       study_ratios[,c(1,2)], date, uncertainty = uncertainty, 
                                       death_uncertainty = death_uncertainty,  num_samples)
  
  dat <- list(ps_orphans, 
              primary_orphans, 
              parents_orphans)
  
  return (dat)
}




