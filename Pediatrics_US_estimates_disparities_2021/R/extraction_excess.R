library(rjson)
library(reshape2)
library(data.table)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(tidyr)
library(repr)
library(ggpubr)
library(readr)
library(readxl)

Sys.setlocale("LC_TIME", "English")

# USA - by state 
process_usa_state = function(rep=000){
  if(rep==000) set.seed(10)
  # url = https://data.cdc.gov/NCHS/AH-Quarterly-Excess-Deaths-by-State-Sex-Age-and-Ra/jqg8-ycmh
  excess <- data.table(read.csv('data/AH_Quarterly_Excess_Deaths_by_State__Sex__Age__and_Race_Q2_2021.csv'))

  excess$End.Date <- as.Date(excess$End.Date,format=c("%m/%d/%Y"))
  excess$Start.Date <- as.Date(excess$Start.Date,format=c("%m/%d/%Y"))
  excess[StateName=='New York City',StateName:='New York']
  excess <- subset(excess, AgeGroup!='50-54 Years' & AgeGroup!='All Ages' & AgeGroup!='Not stated' & 
                     Sex!='All Sexes' & RaceEthnicity!='All Race/Ethnicity Groups')
  setnames(excess,c('State','StateName','Deaths..weighted.','Number.above.average..weighted.','COVID19..weighted.','RaceEthnicity'),
           c('Statecode','State','deaths','excess_deaths','covid_deaths','Race.and.Hispanic.Origin.Group'))
  excess[, age:=gsub(' Years','',AgeGroup)]
  
  # group Native Hawaiian/PI with NH Asian
  excess[Race.and.Hispanic.Origin.Group=='Non-Hispanic Native Hawaiian or Other Pacific Islander',Race.and.Hispanic.Origin.Group:='Non-Hispanic Asian']
  
  excess <- subset(excess,Type=='Cumulative Estimates From 2020, Quarter 2 Through Most Recent Quarter')

  # for bootstrap replicates sample from death counts
  if(rep!=000){
    # sample from excess deaths
    samp <- rpois(length(excess[!is.na(excess_deaths) & excess_deaths>0,excess_deaths]),lambda=excess[!is.na(excess_deaths) & excess_deaths>0,excess_deaths])
    excess[!is.na(excess_deaths) & excess_deaths>0, excess_deaths2:=samp]
    # sample negative excess deaths
    samp <- rpois(length(excess[!is.na(excess_deaths) & excess_deaths<0,excess_deaths]),lambda=excess[!is.na(excess_deaths) & excess_deaths<0,-excess_deaths])
    excess[!is.na(excess_deaths) & excess_deaths<0, excess_deaths2:=-samp]
    # sample from covid deaths
    samp <- rpois(length(excess[!is.na(covid_deaths),covid_deaths]),lambda=excess[!is.na(covid_deaths),covid_deaths])
    excess[!is.na(covid_deaths), covid_deaths2:=samp]
    
    # impute missings
    y <- sum(is.na(excess$excess_deaths2)) #count the NAs
    excess[is.na(excess_deaths2), excess_deaths2:=  sample(1:9, y, replace = TRUE)]
    y <- sum(is.na(excess$covid_deaths2)) #count the NAs
    excess[is.na(covid_deaths2), covid_deaths2:=  sample(1:9, y, replace = TRUE)]
    
    # sum over deaths to aggregate Hawaiian/PI with NH Asian
    excess <- excess[, list(covid_deaths=sum(covid_deaths2,na.rm=T),excess_deaths = sum(excess_deaths2,na.rm=T)),
                     by=c('State','Sex','Race.and.Hispanic.Origin.Group','age')]
  }else{

    # impute covid/excess deaths for suppressed counts
    y <- sum(is.na(excess$covid_deaths)) #count the NAs
    excess[is.na(covid_deaths), covid_deaths:=  sample(1:9, y, replace = TRUE)]
    
    y <- sum(is.na(excess$excess_deaths)) #count the NAs
    excess[is.na(excess_deaths), excess_deaths:=  sample(1:9, y, replace = TRUE)]
    
    # sum over deaths to aggregate Hawaiian/PI with NH Asian
    excess <- excess[, list(covid_deaths=sum(covid_deaths,na.rm=T),
                            excess_deaths = sum(excess_deaths,na.rm=T)),
                     by=c('State','Sex','Race.and.Hispanic.Origin.Group','age')]
  }

  excess[Sex=='Female (F)',gender:='Female']
  excess[Sex=='Male (M)',gender:='Male']
  
  excess[,deaths:= ifelse(covid_deaths > excess_deaths, covid_deaths, excess_deaths)]
  excess <- subset(excess,select=c('State', 'age', 'Race.and.Hispanic.Origin.Group', 'gender','covid_deaths', 'excess_deaths', 'deaths'))
  
  # save deaths including children
  write_csv(excess, path = 'data/usa_states_ALL.csv')
  
  # save deaths excluding children
  excess <- subset(excess,age!='0-14')
  write_csv(excess, path = 'data/usa_states.csv')
}

