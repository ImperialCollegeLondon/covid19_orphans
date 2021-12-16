library(tidyverse)

# Argentina
sample_deaths_argentina <- function(month = "", seed){
  set.seed(seed)
  d_merge = read.csv(paste0('global_age_analysis_2021/data/Argentina/covid19_deaths', month, '.csv'), stringsAsFactors = FALSE)
  d_merge$deaths <- rpois(length(d_merge$deaths), d_merge$deaths)
  
  write.csv(d_merge, paste0('global_age_analysis_2021/data/Argentina/covid19_deaths', month, '_un.csv'), row.names=FALSE)
}

# Brazil
sample_deaths_brazil <- function(month = "", seed){
  set.seed(seed)
  d_merge = read.csv(paste0('global_age_analysis_2021/data/Brazil/covid19_deaths', month, '.csv'), stringsAsFactors = FALSE)
  d_merge$COVID19_deaths <- rpois(length(d_merge$COVID19_deaths), d_merge$COVID19_deaths)
  
  write.csv(d_merge, paste0('global_age_analysis_2021/data/Brazil/covid19_deaths', month, '_un.csv'), row.names=FALSE)
}

# Colombia
sample_deaths_colombia <- function(month = "", seed){
  set.seed(seed)
  d_merge = read.csv(paste0('global_age_analysis_2021/data/Colombia/covid19_deaths_all', month, '.csv'), stringsAsFactors = FALSE)
  d_merge$deaths <- rpois(length(d_merge$deaths), d_merge$deaths)
  
  write.csv(d_merge, paste0('global_age_analysis_2021/data/Colombia/covid19_deaths_all', month, '_un.csv'), row.names=FALSE)
}

# England and Wales
sample_deaths_england_wales <- function(month = "", seed){
  set.seed(seed)
  d_deaths = read.csv(paste0('global_age_analysis_2021/data/UK/deaths_all_england_wales', month, '.csv'), stringsAsFactors = FALSE)
  d_merge = d_deaths
  
  if (month != "_diff"){
    d_merge$nb_excess[which(d_merge$nb_excess < 0)] <- 0
    d_merge$nb_excess_sample <- rpois(length(d_merge$nb_excess), d_merge$nb_excess)
    
    d_merge$nb_covid19_sample <- rpois(length(d_merge$nb_covid19), d_merge$nb_covid19)
    
    d_merge$deaths <- ifelse(d_merge$nb_excess_sample > d_merge$nb_covid19_sample, d_merge$nb_excess_sample,  d_merge$nb_covid19_sample)
  } else {
    d_merge$deaths[which(d_merge$deaths < 0)] <- 0
    d_merge$deaths <- rpois(length(d_merge$deaths), d_merge$deaths)
  }

  write.csv(d_merge, paste0('global_age_analysis_2021/data/UK/deaths_all_england_wales', month, '_un.csv'), row.names=FALSE)
}

# France
sample_deaths_france <- function(month = "", seed){
  set.seed(seed)
  d_merge = read.csv(paste0('global_age_analysis_2021/data/France/france_all', month, '.csv'), stringsAsFactors = FALSE)
  if (month != "_diff"){
    d_merge$excess_deaths[which(d_merge$excess_deaths < 0)] <- 0
    d_merge$nb_excess_sample <- rpois(length(d_merge$excess_deaths), d_merge$excess_deaths)
    
    d_merge$nb_covid19_sample <- rpois(length(d_merge$covid_deaths), d_merge$covid_deaths)
    
    d_merge$deaths <- ifelse(d_merge$nb_excess_sample > d_merge$nb_covid19_sample, d_merge$nb_excess_sample,  d_merge$nb_covid19_sample)
  }
  else {
    d_merge$deaths[which(d_merge$deaths < 0)] <- 0
    d_merge$deaths <- rpois(length(d_merge$deaths), d_merge$deaths)
  }
  write.csv(d_merge, paste0('global_age_analysis_2021/data/France/france_all', month, '_un.csv'), row.names=FALSE)
}

# Germany
sample_deaths_germany <- function(month = "", seed){
  set.seed(seed)
  d_merge = read.csv(paste0('global_age_analysis_2021/data/Germany/covid19_deaths', month, '.csv'), stringsAsFactors = FALSE)
  d_merge$deaths <- rpois(length(d_merge$deaths), d_merge$deaths)
  
  write.csv(d_merge, paste0('global_age_analysis_2021/data/Germany/covid19_deaths', month, '_un.csv'), row.names=FALSE)
}

# India
sample_deaths_india <- function(month = "", seed){
  set.seed(seed)
  d_merge = read.csv(paste0('global_age_analysis_2021/data/India/all_covid_deaths', month, '.csv'), stringsAsFactors = FALSE)
  d_merge$deaths <- rpois(length(d_merge$deaths), d_merge$deaths)
  
  write.csv(d_merge, paste0('global_age_analysis_2021/data/India/all_covid_deaths', month, '_un.csv'), row.names=FALSE)
}

# Iran
sample_deaths_iran <- function(month = "", seed){
  set.seed(seed)
  d_merge = read.csv(paste0('global_age_analysis_2021/data/Iran/iran_all', month, '.csv'), stringsAsFactors = FALSE)
  d_merge$deaths <- rpois(length(d_merge$deaths), d_merge$deaths)
  
  write.csv(d_merge, paste0('global_age_analysis_2021/data/Iran/iran_all', month, '_un.csv'), row.names=FALSE)
}

# Italy
sample_deaths_italy <- function(month = "", seed){
  d_merge = read.csv(paste0('global_age_analysis_2021/data/Italy/italy_all', month, '.csv'), stringsAsFactors = FALSE)
  set.seed(seed)
  if (month != "_diff"){
    d_merge$excess_deaths[which(d_merge$excess_deaths < 0)] <- 0
    d_merge$nb_excess_sample <- rpois(length(d_merge$excess_deaths), d_merge$excess_deaths)
    
    d_merge$nb_covid19_sample <- rpois(length(d_merge$covid_deaths), d_merge$covid_deaths)
    
    d_merge$deaths <- ifelse(d_merge$nb_excess_sample > d_merge$nb_covid19_sample, d_merge$nb_excess_sample,  d_merge$nb_covid19_sample)
  
  } else {
    d_merge$deaths[which(d_merge$deaths < 0)] <- 0
    d_merge$deaths <- rpois(length(d_merge$deaths), d_merge$deaths)
  }

  write.csv(d_merge, paste0('global_age_analysis_2021/data/Italy/italy_all', month, '_un.csv'), row.names=FALSE)
}

# Kenya
sample_deaths_kenya <- function(month = "", seed){
  set.seed(seed)
  d_merge = read.csv(paste0('global_age_analysis_2021/data/Kenya/covid19_deaths', month, '.csv'), stringsAsFactors = FALSE)
  d_merge$deaths <- rpois(length(d_merge$deaths), d_merge$deaths)
  
  write.csv(d_merge, paste0('global_age_analysis_2021/data/Kenya/covid19_deaths', month, '_un.csv'), row.names=FALSE)
}

# Malawi
sample_deaths_malawi <- function(month = "", seed){
  set.seed(seed)
  d_merge = read.csv(paste0('global_age_analysis_2021/data/Malawi/covid19_deaths', month, '.csv'), stringsAsFactors = FALSE)
  d_merge$deaths <- rpois(length(d_merge$deaths), d_merge$deaths)
  
  write.csv(d_merge, paste0('global_age_analysis_2021/data/Malawi/covid19_deaths', month, '_un.csv'), row.names=FALSE)
}

# Mexico
sample_deaths_mexico <- function(month = "", seed){
  set.seed(seed)
  d_merge = read.csv(paste0('global_age_analysis_2021/data/Mexico/covid19_deaths', month, '.csv'), stringsAsFactors = FALSE)
  d_merge$deaths <- rpois(length(d_merge$deaths), d_merge$deaths)
  write.csv(d_merge, paste0('global_age_analysis_2021/data/Mexico/covid19_deaths', month, '_un.csv'), row.names=FALSE)
}

# Nigeria
sample_deaths_nigeria <- function(month = "", seed){
  set.seed(seed)
  d_merge = read.csv(paste0('global_age_analysis_2021/data/Nigeria/covid19_deaths', month, '.csv'), stringsAsFactors = FALSE)
  d_merge$deaths[which(d_merge$deaths< 0)] = 0
  d_merge$deaths <- rpois(length(d_merge$deaths), d_merge$deaths)
  
  write.csv(d_merge, paste0('global_age_analysis_2021/data/Nigeria/covid19_deaths', month, '_un.csv'), row.names=FALSE)
}

# Peru
sample_deaths_peru <- function(month = "", seed){
  set.seed(seed)
  d_merge = read.csv(paste0('global_age_analysis_2021/data/Peru/covid19_deaths', month, '.csv'), stringsAsFactors = FALSE)
  d_merge$COVID19_deaths <- rpois(length(d_merge$COVID19_deaths), d_merge$COVID19_deaths)
  
  write.csv(d_merge, paste0('global_age_analysis_2021/data/Peru/covid19_deaths', month, '_un.csv'), row.names=FALSE)
}

# Philippines
sample_deaths_philippines <- function(month = "", seed){
  set.seed(seed)
  d_merge = read.csv(paste0('global_age_analysis_2021/data/Philippines/covid19_deaths', month, '.csv'), stringsAsFactors = FALSE)
  d_merge$COVID19_deaths <- rpois(length(d_merge$COVID19_deaths), d_merge$COVID19_deaths)
  write.csv(d_merge, paste0('global_age_analysis_2021/data/Philippines/covid19_deaths', month, '_un.csv'), row.names=FALSE)
}

# Poland
sample_deaths_poland <- function(month = "", seed){
  set.seed(seed)
  d_merge = read.csv(paste0('global_age_analysis_2021/data/Poland/poland_all', month, '.csv'), stringsAsFactors = FALSE)
  d_merge$death[which(d_merge$death < 0)] <- 0
  d_merge$death <- rpois(length(d_merge$death), d_merge$death)
  write.csv(d_merge, paste0('global_age_analysis_2021/data/Poland/poland_all', month, '_un.csv'), row.names=FALSE)
}

# Spain
sample_deaths_spain <- function(month = "", seed){
  set.seed(seed)
  d_merge = read.csv(paste0('global_age_analysis_2021/data/Spain/spain_all', month, '.csv'), stringsAsFactors = FALSE)
  if (month != "_diff"){
    d_merge$excess_deaths[which(d_merge$excess_deaths < 0)] <- 0
    d_merge$nb_excess_sample <- rpois(length(d_merge$excess_deaths), d_merge$excess_deaths)
    
    d_merge$nb_covid19_sample <- rpois(length(d_merge$covid_deaths), d_merge$covid_deaths)
    
    d_merge$deaths <- ifelse(d_merge$nb_excess_sample > d_merge$nb_covid19_sample, d_merge$nb_excess_sample,  d_merge$nb_covid19_sample)
  
  } else {
    d_merge$deaths[which(d_merge$deaths < 0)] <- 0
    d_merge$deaths <- rpois(length(d_merge$deaths), d_merge$deaths)
  }
  
  write.csv(d_merge, paste0('global_age_analysis_2021/data/Spain/spain_all', month, '_un.csv'), row.names=FALSE)
}

# South Africa
sample_deaths_south_africa <- function(month = "", seed){
  set.seed(seed)
  d_merge = read.csv(paste0('global_age_analysis_2021/data/SouthAfrica/covid19_deaths', month, '.csv'), stringsAsFactors = FALSE)
  d_merge$COVID19_deaths <- rpois(length(d_merge$COVID19_deaths), d_merge$COVID19_deaths)
  write.csv(d_merge, paste0('global_age_analysis_2021/data/SouthAfrica/covid19_deaths', month, '_un.csv'), row.names=FALSE)
}

# Usa
sample_deaths_usa <- function(month = "", seed){
  set.seed(seed)
  d_merge = read.csv(paste0('global_age_analysis_2021/data/USA/usa_all', month, '.csv'), stringsAsFactors = FALSE)
  if (month != "_diff"){
    d_merge$excess_deaths[which(d_merge$excess_deaths < 0)] <- 0
    d_merge$nb_excess_sample <- rpois(length(d_merge$excess_deaths), d_merge$excess_deaths)
    
    d_merge$nb_covid19_sample <- rpois(length(d_merge$covid_deaths), d_merge$covid_deaths)
    
    d_merge$deaths <- ifelse(d_merge$nb_excess_sample > d_merge$nb_covid19_sample, d_merge$nb_excess_sample,  d_merge$nb_covid19_sample)
  
    } else {
      d_merge$deaths[which(d_merge$deaths < 0)] <- 0
      d_merge$deaths <- rpois(length(d_merge$deaths), d_merge$deaths)
  }
  write.csv(d_merge, paste0('global_age_analysis_2021/data/USA/usa_all', month, '_un.csv'), row.names=FALSE)
}

# Zimbabwe
sample_deaths_zimbabwe <- function(month = "", seed){
  set.seed(seed)
  d_merge = read.csv(paste0('global_age_analysis_2021/data/Zimbabwe/covid19_deaths', month, '.csv'), stringsAsFactors = FALSE)
  d_merge$deaths <- rpois(length(d_merge$deaths), d_merge$deaths)
  write.csv(d_merge, paste0('global_age_analysis_2021/data/Zimbabwe/covid19_deaths', month, '_un.csv'), row.names=FALSE)
}
