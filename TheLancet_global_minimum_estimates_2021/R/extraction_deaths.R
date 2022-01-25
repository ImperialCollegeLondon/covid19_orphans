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

# Excess:
process_euro_excess_new = function(){
  list_excel = readxl::excel_sheets('TheLancet_global_minimum_estimates_2021/data/Six_euro_Mar_2021/DEMO_R_MWK_05_six_countries_2021.xlsx')
  list_excel = list_excel[3:length(list_excel)]
  x <- lapply(list_excel, function(X) readxl::read_excel('TheLancet_global_minimum_estimates_2021/data/Six_euro_Mar_2021/DEMO_R_MWK_05_six_countries_2021.xlsx', sheet = X))
  names(x) <- list_excel
  
  age_data = data.table()
  age_summary = NULL
  for (i in seq(length(list_excel))){
    data = x[[i]]
    data = as.data.table(data)
    age = strsplit(data[[3]][5], ' ')[[1]]
    age = age[length(age)]
    
    age = gsub('[[Y]','', age)
    age = gsub('[]]', '', age)
    if (age == '_GE90'){
      age = gsub('_GE', '', age)
    }
    if (age == '_LT5'){
      age = gsub('_LT', '0-', age)
    }
    gender = strsplit(data[[3]][6], ' ')[[1]][2]
    gender = gsub('[[]','', gender)
    gender = gsub('[]]', '', gender)
    start_id = which(data[,1] == 'TIME')
    end_id = which(data[,1] == 'Special value')
    df = data[(start_id+1):(end_id-2),]
    date = data[[1]][(start_id+1):(end_id-2)]
    week = NaN
    day = NaN
    for (i in seq(1:nrow(df))){
      week = c(week,strsplit(date[i], '-W')[[1]][2])
      day = c(day,strsplit(date[i], '-W')[[1]][1])
    }
    week = week[-1]
    day = day[-1]
    df[,week:= week]
    df[,year:= day]
    setnames(df, c(2:11), c('Spain', '1', 'France', '2', 'Italy', '3', 'Poland', '4', 'UK', '5'))
    df = df %>% select(year, week, Spain, France, Italy, Poland, UK)
    dt = sapply(df,function(x){as.numeric(gsub(":",NA, x))})
    dt = as.data.table(dt[which(df$year != '2011'),])
    df = dt 
    df = df[which(! df$year %in% c('2020', '2021')),]
    df = df %>% group_by(week) %>% mutate(avg_spain = mean(Spain[!is.na(Spain)]), 
                                          avg_france = mean(France[!is.na(France)]), 
                                          avg_italy = mean(Italy[!is.na(Italy)]), 
                                          avg_poland = mean(Italy[!is.na(Poland)]), 
                                          avg_uk = mean(UK[!is.na(UK)])) %>% ungroup() 
    df = unique(df %>% select(-year,-Spain, -France, -Italy, -Poland, -UK))
    df = as.data.table(df)
    dt = dt[which(dt$year %in% c('2020', '2021')),]
    dt = merge(dt, df,by = 'week')
    dataset = dt %>% group_by(week) %>% mutate(spain = ifelse(is.na(Spain), NA, Spain - avg_spain),
                                               france = ifelse(is.na(France), NA, France - avg_france),
                                               italy = ifelse(is.na(Italy), NA, Italy - avg_italy),
                                               poland = ifelse(is.na(Poland), NA, Poland - avg_poland),
                                               uk = ifelse(is.na(UK), NA, UK - avg_uk)) %>% ungroup()

    dataset_summary = dataset %>% select(week, year, spain, france, italy,poland, uk)
    dataset_summary = dataset_summary %>% group_by(week, year) %>%
      summarise(spain = sum(spain),
                france = sum(france),
                italy = sum(italy),
                poland = sum(poland),
                uk = sum(uk))
    dataset_summary$age = age
    dataset_summary$gender = gender
    age_summary = rbind(age_summary, dataset_summary)  
    cat(paste0('=====>processed dataset for ', gender, ' at age ', age, ' dataset\n'))
  }
  write.csv(age_summary, file = paste0("TheLancet_global_minimum_estimates_2021/data/Six_euro_Mar_2021/euro_excess.csv"), row.names = FALSE)
}

# Process Argentina - covid only, updated to 31st dec
process_argentina_covid19 = function(){
  data = read.csv('TheLancet_global_minimum_estimates_2021/data/Argentina/Argentina - Deaths by age and sex (rates per 100,000)-apr.csv')
  data =  reshape2::melt(data, id.vars = c('date', 'age'), variable.name =  'gender', value.name = 'rate')
  data$age <- as.character(data$age)
  data_pop = readxl::read_xlsx('TheLancet_global_minimum_estimates_2021/data/fertility/pop.xlsx', sheet = 2)
  countries = c('Argentina')
  names(data_pop) = as.character(data_pop[1,])
  data_pop = as.data.table(data_pop) %>% filter(Location %in% countries, Time == '2020') %>% select(Location, Time, Age, Female, Male)
  setnames(data_pop, 1:ncol(data_pop), c('country', 'year', 'age', 'Female', 'Male'))
  data_pop =  reshape2::melt(data_pop, id.vars = c('country', 'year', 'age'), variable.name =  'gender', value.name = 'pop')
  data_pop$age = as.character(data_pop$age)
  data_pop$pop = as.numeric(data_pop$pop)
  data_pop$age = ifelse(data_pop$age %in% c('0-4', '5-9', '10-14', '15-19'), '0-19',
                        ifelse(data_pop$age %in% c('20-24', '25-29', '30-34', '35-39'),'20-39',
                               ifelse(data_pop$age %in% c('40-44', '45-49', '50-54', '55-59'), '40-59',
                                      '60+')))
  data_pop = data_pop %>% group_by(age, gender) %>% mutate(pop = sum(pop)) %>% ungroup() %>% distinct() %>%
    select(-year)
  data_pop$age <- as.character(data_pop$age)
  data_pop = as.data.table(data_pop)
  data = as.data.table(data)
  data_combine = merge(data, data_pop, by = c('age', 'gender'))
  data_combine[,COVID19_deaths := rate * pop/100]
  
  # Selects deaths from JHU
  deaths_data = read.csv("TheLancet_global_minimum_estimates_2021/data/04-30-2021.csv")
  arg <- deaths_data[which(deaths_data$Country_Region == "Argentina"),]
  total = arg$Deaths
  data_combine$date = '2021-04-30'
  
  d_merge = data_combine%>% select(country, date,age, gender, COVID19_deaths)
  d_merge$rate = d_merge$COVID19_deaths/sum(d_merge$COVID19_deaths)
  d_merge$deaths = round(d_merge$rate*total)
  
  write_csv(file = 'TheLancet_global_minimum_estimates_2021/data/Argentina/covid19_deaths.csv', d_merge)
  d_merge
}

# Process Brazil - covid only, updated to march
process_brazil = function(age_range){
  options(repr.plot.width=15, repr.plot.height=15)
  ##  check: url:https://opendatasus.saude.gov.br/dataset/bd-srag-2020
  # the latest version, update weekly
  url = 'https://s3-sa-east-1.amazonaws.com/ckan.saude.gov.br/SRAG/2020/INFLUD-10-05-2021.csv'
  file_name = strsplit(url, '/')
  file_name = file_name[[1]][length(file_name[[1]])]
  download.file(url, file.path('TheLancet_global_minimum_estimates_2021/data/Brazil/', file_name))
  df_SIVEP_ORIGINAL=read_csv2(file.path('TheLancet_global_minimum_estimates_2021/data/Brazil/', file_name))
  
  df_SIVEP_ORIGINAL->df_SIVEP
  df_SIVEP = df_SIVEP[which(df_SIVEP$CLASSI_FIN==5),]
  df_SIVEP = df_SIVEP[which(df_SIVEP$EVOLUCAO==2),]
  df_SIVEP$DT_EVOLUCA = dmy(df_SIVEP$DT_EVOLUCA)
  df_SIVEP$DT_NASC = dmy(df_SIVEP$DT_NASC)
  df_SIVEP = df_SIVEP[,c("CS_SEXO","DT_EVOLUCA","DT_NASC")] #CS_SEXO: 1-Masculino 2-Feminino 9-Ignorado
  df_SIVEP$Age_years = as.numeric(difftime(df_SIVEP$DT_EVOLUCA, df_SIVEP$DT_NASC, units = "days")/365.2425)
  df_SIVEP$Deaths = 1
  df_SIVEP$Age_group <- cut(df_SIVEP$Age_years, age_range, include.lowest = TRUE)
  df_SIVEP=df_SIVEP[c("DT_EVOLUCA","CS_SEXO","Age_group","Deaths")]
  df_SIVEP = aggregate(. ~DT_EVOLUCA+CS_SEXO+Age_group, data=df_SIVEP, sum, na.rm=TRUE)
  df_SIVEP = df_SIVEP[order(df_SIVEP$DT_EVOLUCA),]
  colnames(df_SIVEP) = c("date","gender","age","deaths")

  df_SIVEP = as.data.table(df_SIVEP)
  df_SIVEP[, total_COVID19_deaths:= cumsum(deaths),by = list(gender, age)]
  df_SIVEP$age = gsub('[[]','', df_SIVEP$age)
  df_SIVEP$age = gsub('[(]','', df_SIVEP$age)
  df_SIVEP$age = gsub('[]]','', df_SIVEP$age)
  df_SIVEP$age = gsub(',Inf','+', df_SIVEP$age)
  df_SIVEP$age = gsub(',','-', df_SIVEP$age)
  
  df_SIVEP = df_SIVEP[-which(df_SIVEP$gender == 'I'),]
  df_SIVEP.plt <- df_SIVEP 
  df_SIVEP.plt$agesex = paste0(df_SIVEP$gender, df_SIVEP$age)
  
  if(!file.exists(file.path('data','Brazil')))
  {
    dir.create(file.path('data','Brazil'))
  }
  
  #write_csv(path = paste0("TheLancet_global_minimum_estimates_2021/data/Brazil/","brazil.csv"),df_SIVEP)
  #write_csv(path = paste0("TheLancet_global_minimum_estimates_2021/data/Brazil/","brazil_plot.csv"),df_SIVEP.plt)
  
  df = df_SIVEP.plt[order(-date, agesex),]
  df = df[which(df$date <= as.Date("2021-04-30")),]
  agesex_group = unique(df$agesex)
  tmp = data.table()
  for (i in 1:length(agesex_group)){
    tmp = rbind(tmp,df[which(df$agesex == agesex_group[i])[1],])
  }
  tmp$date = max(tmp$date)
  tmp = tmp[order(-gender, age),]
  write_csv(path = paste0("TheLancet_global_minimum_estimates_2021/data/Brazil/","brazil_summary.csv"),tmp)
  
  d_merge = tmp
  d_merge$date = as.Date(d_merge$date)
  d_merge = d_merge[which(d_merge$date <= as.Date("2021-04-30")),]
  d_merge = d_merge %>% 
    select(date, gender, age, total_COVID19_deaths) 
  
  # Selects deaths from JHU
  deaths_data = read.csv("TheLancet_global_minimum_estimates_2021/data/04-30-2021.csv")
  bra <- deaths_data[which(deaths_data$Country_Region == "Brazil"),]
  total = sum(bra$Deaths)
  
  setnames(d_merge, 'total_COVID19_deaths', 'deaths')
  d_merge$date = rep('2021-04-30', length(d_merge$date))
  d_merge$rate = d_merge$deaths/sum(d_merge$deaths)
  d_merge$deaths = round(d_merge$rate * total)
  setnames(d_merge, 'deaths', 'COVID19_deaths')
  write_csv(d_merge %>% select(-rate), path = 'TheLancet_global_minimum_estimates_2021/data/Brazil/covid19_deaths.csv')
}

#Colombia
process_colombia_covid19 = function(){
  data = readxl::read_xlsx('TheLancet_global_minimum_estimates_2021/data/Colombia/COVID19_Colombia_by_sex_and_age.xlsx')
  data = reshape2::melt(data, id.vars = c('Week', 'sex 0=F 1=M'), variable.name = 'age', value.name = 'covid19_deaths')
  data = as.data.table(data)
  setnames(data, 2, 'gender')
  data$gender = ifelse(data$gender == 0,'female','male')
  data = data %>% filter(!is.na(Week)) %>% filter(age != 'total')
  data$covid19_deaths = as.numeric(data$covid19_deaths)
  data = data %>% group_by(age, gender) %>% mutate(nb_deaths = sum(covid19_deaths)) %>% ungroup() %>% 
    select(age, gender, nb_deaths) %>% distinct()
  data$rate = data$nb_deaths/sum(data$nb_deaths)
  
  # Selects deaths from JHU
  deaths_data = read.csv("TheLancet_global_minimum_estimates_2021/data/04-30-2021.csv")
  col <- deaths_data[which(deaths_data$Country_Region == "Colombia"),]
  total = sum(col$Deaths)
  data$date = '2021-04-30'
  
  data$deaths = round(data$rate* total)
  data = data %>% select(-rate, -nb_deaths)
  write_csv(data, path = 'TheLancet_global_minimum_estimates_2021/data/Colombia/covid19_deaths_all.csv')
}

#England and wales
process_england_wales = function(){ # Just use excess
  #url_previous = 'https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/adhocs/11485fiveyearaverageweeklydeathsbysexandagegroupenglandandwalesdeathsoccurringbetween2015and2019/fiveyearavgweeklydeaths2015to2019.xlsx'
  #download.file(url_previous, 'TheLancet_global_minimum_estimates_2021/data/UK/england_wales_pre_deaths.xlsx')
  data = readxl::read_xlsx('TheLancet_global_minimum_estimates_2021/data/UK/england_wales_pre_deaths.xlsx', sheet = 2)
  names(data) = as.character(data[2,])
  data = data[3:nrow(data),]
  data = data[which(!is.na(data$`Age group`)),]
  data = data %>% filter(`Age group` != 'All ages',`Age group` != 'All Ages')
  data$Sex = c(rep('male',19),rep('female',19))
  
  data = reshape2::melt(data, id.vars = c('Sex', 'Age group'), variable.name = 'week', value.name = 'value')
  setnames(data, 1:ncol(data), c('gender', 'age', 'week', 'value'))
  
  url = 'https://download.ons.gov.uk/downloads/datasets/weekly-deaths-age-sex/editions/covid-19/versions/30.csv'
  #download.file(url, 'TheLancet_global_minimum_estimates_2021/data/UK/england_wales_deaths-v30.csv')
  data2 = read.csv('TheLancet_global_minimum_estimates_2021/data/UK/england_wales_deaths-v30.csv')
  
 
  
   week_split <- function(x){
    return(as.double(strsplit(x, "-")[[1]][2]))
  }
  data2$week = sapply(as.character(data2$week.number), week_split)
  data2 = data2[which(!(data2$calendar.years == "2021" & data2$week > 17)),]  #week 17 includes 30th Apr
  data2$week <- NULL
  setnames(data2, 1:ncol(data2), c('value', 'empty', 'year', 'time', 'geographiy', 'region', 'week_nb',
                                   'week', 'gender','Gender', 'age', 'age_group', 'deaths', 'Deaths'))
  data2 = data2 %>% select(week_nb, week, year, gender, age, value,deaths) %>% filter(year >= '2015', gender != 'all', age != 'all')
  data2 = data2 %>% arrange(week, year, gender, age)
  data2$age = as.character(data2$age)
  data_2020 = copy(data2)
  
  data2 = data_2020 %>%filter(deaths == 'total-registered-deaths') %>% select(-deaths, -week)
  data2$age = ifelse(data2$age %in% c('0-1', '1-4'), '00-04',
                     ifelse(data2$age == '5-9', '05-09', data2$age))
  data2 = data2 %>% group_by(week_nb, gender, age, year) %>% summarize(deaths = sum(value))  %>% arrange(week_nb, age, desc(gender))
  data2$week_nb = gsub('week-','Week ',data2$week_nb)
  setnames(data2,c('week_nb', 'deaths'),c('week', 'value_20'))
  
  setnames(data, 'value', 'value_pre')
  data$value_pre = as.numeric(data$value_pre)
  
  
  dt = left_join(data2, data, by=c('week', 'age', 'gender'))
  dt[is.na(dt)] <- 0
  dt = dt %>% mutate(deaths = value_20 - value_pre)
  
  
  data_COVID19_d = data_2020 %>% filter(deaths == 'deaths-involving-covid-19-registrations')
  data_COVID19_d$age = ifelse(data_COVID19_d$age %in% c('0-1', '1-4'), '00-04',
                              ifelse(data_COVID19_d$age == '5-9', '05-09', data_COVID19_d$age))
  data_COVID19_d = data_COVID19_d %>% 
    group_by(week_nb, gender, age, year) %>% 
    summarize(deaths = sum(value)) 
  data_COVID19_d$week = gsub('week-','Week ',data_COVID19_d$week_nb)
  
  d_deaths = left_join(data_COVID19_d, dt, by= c('week', 'age', 'gender', 'year'))
  d_deaths[is.na(d_deaths)] <- 0
  setnames(d_deaths, c('deaths.x', 'deaths.y'), c('covid19_deaths', 'excess_deaths'))
  
  write_csv(path = 'TheLancet_global_minimum_estimates_2021/data/UK/deaths_all_england_wales.csv', d_deaths)
}

# France
process_france = function(){
  tmp = readxl::read_xlsx('TheLancet_global_minimum_estimates_2021/data/France/Deaths-Age-Sex_Covid-19_France_06-05.xlsx', sheet = 2)
  data = tmp[7:16, c(1,c(50, 52, 55))]
  data = data.frame(data)
  names(data) = c('age', 'male', 'female', 'both')
  data$male = as.numeric(data$male)
  data$female = as.numeric(data$female)
  data$both = as.numeric(data$both)
  
  data = data %>% 
    select(-both) %>%
    gather(key = "gender", value = "covid_deaths", -age)
  data$gender = ifelse(data$gender  == 'male', 'Male', 'Female')
  
  excess_death = read.csv('TheLancet_global_minimum_estimates_2021/data/euro_excess.csv')
  excess_death = as.data.table(excess_death) %>% select(week, year, france, age, gender)
  excess_death = excess_death[!is.na(excess_death$france),]
  excess_death$age = as.character(excess_death$age)
  
  excess_death$age = ifelse(excess_death$age %in% c('0-5', '5-9'), '0-9',
                            ifelse(excess_death$age %in% c('10-14', '15-19'), '10-19',
                                   ifelse(excess_death$age %in% c('20-24', '25-29'), '20-29',
                                          ifelse(excess_death$age %in% c('30-34', '35-39'), '30-39',
                                                 ifelse(excess_death$age %in% c('40-44', '45-49'), '40-49',
                                                        ifelse(excess_death$age %in% c('50-54', '55-59'), '50-59',
                                                               ifelse(excess_death$age %in% c('60-64', '65-69'), '60-69',
                                                                      ifelse(excess_death$age %in% c('70-74', '75-79'), '70-79',
                                                                             ifelse(excess_death$age %in% c('80-84', '85-89'), '80-89',
                                                                                    '90+')))))))))
  excess_death = excess_death %>%  group_by(gender, year, week, age) %>%
    mutate(France= sum(france)) %>% select(-c(france)) %>% ungroup() %>% distinct()
  excess_death$gender = as.character(excess_death$gender)
  excess_death =  excess_death[which(excess_death$gender != 'T'),]
  excess_death$gender = ifelse(excess_death$gender == 'M', 'Male', 'Female')
  
  excess_death = excess_death %>%
    filter((year == 2020 & week >= 10)  | (year == 2021 & week <= 17)) %>%
    group_by(age, gender) %>%
    summarise("excess_deaths" = sum(France))
  
  d_merge = left_join(data, excess_death, by = c('gender', 'age' = 'age'))
  write_csv(path = paste0("TheLancet_global_minimum_estimates_2021/data/France/","france_all.csv"),d_merge)
  
  total_covid = sum(d_merge$covid_deaths)
  
  # Selects deaths from JHU
  deaths_data = read.csv("TheLancet_global_minimum_estimates_2021/data/04-30-2021.csv")
  fra <- deaths_data[which(deaths_data$Country_Region == "France"),]
  total = sum(fra$Deaths)
  
  rate = total / total_covid
  d_merge$covid_deaths = round(d_merge$covid_deaths * rate)
  d_merge$deaths = ifelse(d_merge$covid_deaths > d_merge$excess_deaths, d_merge$covid_deaths, d_merge$excess_deaths)
  d_merge = d_merge %>% select(age, gender, covid_deaths, excess_deaths, deaths)
  write_csv(d_merge, path = 'TheLancet_global_minimum_estimates_2021/data/France/france_all.csv')
}

# Germany
process_germany = function(){
  tmp = readxl::read_xlsx('TheLancet_global_minimum_estimates_2021/data/Germany/Deaths-Age-Sex_Covid-19_Germany_06-05.xlsx', sheet = 2)
  data = tmp[7:16, c(1,c(15,17,20))]
  data = data.frame(data)
  names(data) = c('age', 'male', 'female', 'both')
  data$age[which(data$age == '90-99')] <- "90+"
  data$male = as.numeric(data$male)
  data$female = as.numeric(data$female)
  data$both = as.numeric(data$both)
  
  data = data %>% 
    select(-both) %>%
    gather(key = "gender", value = "deaths", -age)
  data$gender = ifelse(data$gender  == 'male', 'Male', 'Female')
  total_covid = sum(data$deaths)
  
  # Selects deaths from JHU
  deaths_data = read.csv("TheLancet_global_minimum_estimates_2021/data/04-30-2021.csv")
  ger <- deaths_data[which(deaths_data$Country_Region == "Germany"),]
  total = sum(ger$Deaths)
  
  rate = total/total_covid
  data$deaths = round(data$deaths * rate)
  data$age = as.character(data$age)
  data$age = ifelse(data$age == '90-99', '90+', data$age)
  write_csv(data, path = 'TheLancet_global_minimum_estimates_2021/data/Germany/covid19_deaths.csv')
}

# India
process_india_covid = function(){
  total_deaths = 211839 # 30th April 162960 # 31st March 157194 # on 28th feb from https://www.covid19india.org
  demog <- read_excel("TheLancet_global_minimum_estimates_2021/data/India/jghs-2-e17-i002.xlsx")
  
  tot_deaths = data.frame(demog[c(13), c(3, 5)])
  colnames(tot_deaths) <- c("Male", "Female")
  tot_deaths$Male <- as.numeric(tot_deaths$Male)
  tot_deaths$Female <- as.numeric(tot_deaths$Female)
  
  demog_cut <- demog[c(3:11), c(1, 3, 5)]
  colnames(demog_cut) <- c("age", "Male", "Female")
  demog_cut$age <- c("0-4", "5-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")
  demog_cut$Male <- as.numeric(demog_cut$Male)/100 * tot_deaths$Male/(tot_deaths$Male + tot_deaths$Female)
  demog_cut$Female <- as.numeric(demog_cut$Female)/100 * tot_deaths$Female/(tot_deaths$Male + tot_deaths$Female)
  
  data <- gather(demog_cut, key = "sex", value = "freq", -age)
  data$deaths <- round(data$freq * total_deaths)
  
  d_summary <- select(data, sex, age, deaths)
  write_csv(d_summary %>% arrange(age, sex), path = 'TheLancet_global_minimum_estimates_2021/data/India/all_covid_deaths.csv')
  d_summary$age = ifelse(d_summary$age %in% c('0-4', '5-19'), '0-19',
                         ifelse(d_summary$age %in% c('20-29','30-39', '40-49'), '20-49',
                                ifelse(d_summary$age %in% c("50-59","60-69"),'50-69', '70+')))
  d_summary = d_summary %>% group_by(age, sex) %>% 
    mutate(covid_deaths = sum(deaths)) %>% 
    ungroup() %>% 
    select(-deaths) %>% 
    distinct()
  d_summary  = d_summary %>% arrange(age, sex) %>% filter(age != '0-19')
  write_csv(d_summary, path = 'TheLancet_global_minimum_estimates_2021/data/India/covid_deaths.csv')
}

# Iran
process_iran <- function(){
  covid_data = data.frame(age = c("0-9", "10-19", "20-29", "30-39", "40-49", 
                                  "50-59", "60-69", "70-79", "80+"),
                          deaths = c(0, 0, 1, 11, 13, 54, 71, 52, 37))
  
  male_ratio = 167/(72 + 167)
  female_ratio = 72/(72+167)
  
  covid_data$Female = covid_data$deaths * female_ratio
  covid_data$Male = covid_data$deaths * male_ratio

  covid_data = covid_data %>% 
    select(age, Female, Male) %>%
    gather(key = "gender", value = "deaths", -age)
  total_covid = sum(covid_data$deaths)
  
  # Selects deaths from JHU
  deaths_data = read.csv("TheLancet_global_minimum_estimates_2021/data/04-30-2021.csv")
  iran <- deaths_data[which(deaths_data$Country_Region == "Iran"),]
  total = sum(iran$Deaths)
  
  rate = total / total_covid
  covid_data$deaths = round(covid_data$deaths * rate)
  write_csv(covid_data, file = 'TheLancet_global_minimum_estimates_2021/data/Iran/iran_all.csv')
}

# Italy
process_italy = function(){
  tmp = readxl::read_xlsx('TheLancet_global_minimum_estimates_2021/data/Italy/Deaths-Age-Sex_Covid-19_Italy_06-05.xlsx', sheet = 2)
  data = tmp[7:16, c(1,c(8,10,13))]
  data = data.frame(data)
  names(data) = c('age', 'male', 'female', 'both')
  data$male = as.numeric(data$male)
  data$female = as.numeric(data$female)
  data$both = as.numeric(data$both)
  
  if (sum(data$male) + sum(data$female) != sum(data$both)){
    stop()
  }
  
  data = data %>% 
    select(-both) %>%
    gather(key = "gender", value = "covid_deaths", -age)
  data$gender = ifelse(data$gender  == 'male', 'Male', 'Female')
  
  excess_death = read.csv('TheLancet_global_minimum_estimates_2021/data/euro_excess.csv')
  excess_death = as.data.table(excess_death) %>% select(week, year, italy, age, gender)
  excess_death = excess_death[!is.na(excess_death$italy),]
  excess_death$age = as.character(excess_death$age)
  
  excess_death$age = ifelse(excess_death$age %in% c('0-5', '5-9'), '0-9',
                            ifelse(excess_death$age %in% c('10-14', '15-19'), '10-19',
                                   ifelse(excess_death$age %in% c('20-24', '25-29'), '20-29',
                                          ifelse(excess_death$age %in% c('30-34', '35-39'), '30-39',
                                                 ifelse(excess_death$age %in% c('40-44', '45-49'), '40-49',
                                                        ifelse(excess_death$age %in% c('50-54', '55-59'), '50-59',
                                                               ifelse(excess_death$age %in% c('60-64', '65-69'), '60-69',
                                                                      ifelse(excess_death$age %in% c('70-74', '75-79'), '70-79',
                                                                             ifelse(excess_death$age %in% c('80-84', '85-89'), '80-89',
                                                                                    '90+')))))))))
  excess_death = excess_death %>%  group_by(gender, week, year, age) %>%
    mutate(excess_death = sum(italy)) %>% 
    select(-c(italy)) %>% 
    ungroup() %>% 
    distinct()
  
  excess_death$gender = as.character(excess_death$gender)
  excess_death =  excess_death[which(excess_death$gender != 'T'),]
  excess_death$gender = ifelse(excess_death$gender == 'M', 'Male', 'Female')
  
  excess_death = excess_death %>%
    filter((week >= 10 & year == "2020") | (week <= 17 & year == "2021")) %>%
    group_by(age, gender) %>%
    summarise("excess_deaths" = sum(excess_death))
  
  d_merge = left_join(data, excess_death, by = c('gender', 'age' = 'age'))
  write_csv(path = paste0("TheLancet_global_minimum_estimates_2021/data/Italy/","italy_all.csv"),d_merge)
  
  total_covid = sum(d_merge$covid_deaths)
  
  # Selects deaths from JHU
  deaths_data = read.csv("TheLancet_global_minimum_estimates_2021/data/04-30-2021.csv")
  ita <- deaths_data[which(deaths_data$Country_Region == "Italy"),]
  total = sum(ita$Deaths)
  
  rate = total / total_covid
  d_merge$covid_deaths = round(d_merge$covid_deaths * rate)
  d_merge$deaths = ifelse(d_merge$covid_deaths > d_merge$excess_deaths, d_merge$covid_deaths, d_merge$excess_deaths)
  d_merge = d_merge %>% select(age, gender, covid_deaths, excess_deaths, deaths)
  write_csv(d_merge, path = 'TheLancet_global_minimum_estimates_2021/data/Italy/italy_all.csv')
}

# Kenya
process_kenya_covid19 = function(){
  data = read.csv('TheLancet_global_minimum_estimates_2021/data/Kenya/covid19_deaths_raw.csv')
  data$date = '2020-07-27'
  data$rate = data$deaths/sum(data$deaths)
  
  # Selects deaths from JHU
  deaths_data = read.csv("TheLancet_global_minimum_estimates_2021/data/04-30-2021.csv")
  ken <- deaths_data[which(deaths_data$Country_Region == "Kenya"),]
  total = sum(ken$Deaths)
  
  data$deaths = round(data$rate * total)
  data = data %>% select(-rate)
  write_csv(data, path = 'TheLancet_global_minimum_estimates_2021/data/Kenya/covid19_deaths.csv')
}

# Malawi
process_malawi = function(){
  data = read_xls('TheLancet_global_minimum_estimates_2021/data/Malawi/malawi.xls')
  data = gather(data, key = gender, value = deaths, -Age)
  data$rate = data$deaths/sum(data$deaths)
  
  # Selects deaths from JHU
  deaths_data = read.csv("TheLancet_global_minimum_estimates_2021/data/04-30-2021.csv")
  mal <- deaths_data[which(deaths_data$Country_Region == "Malawi"),]
  total = sum(mal$Deaths)
  
  data$deaths = round(data$rate * total)
  data = data %>% select(-rate)
  names(data) <- c("age", "gender", "deaths")
  write_csv(data, file = 'TheLancet_global_minimum_estimates_2021/data/Malawi/covid19_deaths.csv')
}

# Mexico
process_mexico_covid19 = function(){
  data = read.csv('TheLancet_global_minimum_estimates_2021/data/Mexico/Mexico - Deaths by age and sex (rates per 100,000)-apr.csv', fileEncoding="latin1")
  setnames(data, 1:2, c('date', 'age'))
  
  data =  reshape2::melt(data, id.vars = c('date', 'age'), variable.name =  'gender', value.name = 'rate')
  
  data_pop = readxl::read_xlsx('TheLancet_global_minimum_estimates_2021/data/fertility/pop.xlsx', sheet = 2)
  countries = c('Mexico')
  names(data_pop) = as.character(data_pop[1,])
  data_pop = as.data.table(data_pop) %>% filter(Location %in% countries, Time == '2020') %>% select(Location, Time, Age, Female, Male)
  setnames(data_pop, 1:ncol(data_pop), c('country', 'year', 'age', 'Female', 'Male'))
  data_pop =  reshape2::melt(data_pop, id.vars = c('country', 'year', 'age'), variable.name =  'gender', value.name = 'pop')
  data_pop$age = as.character(data_pop$age)
  data_pop$pop = as.numeric(data_pop$pop)
  data_pop$age = ifelse(data_pop$age %in% c('80-84', '85-89', '90-94', '95-99', '100+'), '80+', data_pop$age)
  data_pop = data_pop %>% group_by(age, gender) %>% mutate(pop = sum(pop)) %>% ungroup() %>% distinct() %>%
    select(-year)
  data_pop = as.data.table(data_pop)
  data = as.data.table(data)
  data_combine= merge(data, data_pop, by = c('age', 'gender'))
  data_combine[,COVID19_deaths := round(rate * pop/100)]
  
  # Selects deaths from JHU
  deaths_data = read.csv("TheLancet_global_minimum_estimates_2021/data/04-30-2021.csv")
  mex <- deaths_data[which(deaths_data$Country_Region == "Mexico"),]
  total = sum(mex$Deaths)
  
  data_combine$rate = data_combine$COVID19_deaths/sum(data_combine$COVID19_deaths)
  data_combine$deaths = round(data_combine$rate * total)
  write_csv(data_combine %>% select(age, gender, deaths), path = 'TheLancet_global_minimum_estimates_2021/data/Mexico/covid19_deaths.csv')
}

# Nigeria
process_nigeria_covid19 = function(){
  d_merge = read.csv('TheLancet_global_minimum_estimates_2021/data/Nigeria/covid19_deaths_raw_apr.csv')
  setnames(d_merge, 'COVID19_deaths', 'deaths')
  
  # Selects deaths from JHU
  deaths_data = read.csv("TheLancet_global_minimum_estimates_2021/data/04-30-2021.csv")
  nig <- deaths_data[which(deaths_data$Country_Region == "Nigeria"),]
  total = sum(nig$Deaths)
  
  d_merge$date = '2021-04-30'
  d_merge$rate = d_merge$deaths/sum(d_merge$deaths)
  d_merge$deaths = round(d_merge$rate * total)
  write_csv(d_merge %>% select(-rate), path = 'TheLancet_global_minimum_estimates_2021/data/Nigeria/covid19_deaths.csv')
}

# Peru
process_peru_covid19 = function(){
  d_merge = read.csv('TheLancet_global_minimum_estimates_2021/data/Peru/covid19_deaths_raw.csv')

  # Selects deaths from JHU
  deaths_data = read.csv("TheLancet_global_minimum_estimates_2021/data/04-30-2021.csv")
  per <- deaths_data[which(deaths_data$Country_Region == "Peru"),]
  total = sum(per$Deaths)
  
  d_merge$date = '2021-04-30'
  d_merge$rate = d_merge$deaths/sum(d_merge$deaths)
  d_merge$deaths = round(d_merge$rate * total)
  setnames(d_merge, 'deaths', 'COVID19_deaths')
  
  write_csv(d_merge %>% select(-rate), path = 'TheLancet_global_minimum_estimates_2021/data/Peru/covid19_deaths.csv')
}

# Philippines 

process_philippines = function(){
  data_1 <- read_csv("TheLancet_global_minimum_estimates_2021/data/Philippines/DOH COVID Data Drop_ 20210509 - 04 Case Information 2021.csv")
  data_2 <- read_csv("TheLancet_global_minimum_estimates_2021/data/Philippines/DOH COVID Data Drop_ 20210509 - 04 Case Information 2020.csv")
  data <- rbind(data_1, data_2)
  data$DateDied <- as.Date(data$DateDied)
  
  data_sub <- data[which(data$RemovalType == 'DIED'),]
  data_sub <- data_sub[which(data_sub$DateDied <= as.Date('2021-04-30')),]

  d <- data_sub %>%
    group_by(AgeGroup, Sex) %>%
    summarise(deaths = n())
  d <- d[which(!is.na(d$AgeGroup)),]
  d$AgeGroup <- gsub(' to ', '-', d$AgeGroup)
  
  setnames(d, 'Sex', 'gender')
  setnames(d, 'AgeGroup', 'age')
  d$gender <- ifelse(d$gender == "MALE", "male", "female")
  
  # Selects deaths from JHU
  deaths_data = read.csv("TheLancet_global_minimum_estimates_2021/data/04-30-2021.csv")
  phi <- deaths_data[which(deaths_data$Country_Region == "Philippines"),]
  total = sum(phi$Deaths)
  
  if (total > sum(d$deaths)){
    d$date = '2021-04-30'
    d$rate = d$deaths/sum(d$deaths)
    d$deaths = round(d$rate * total)
    setnames(d, 'deaths', 'COVID19_deaths')
    
    write_csv(d %>% select(-rate), path = 'TheLancet_global_minimum_estimates_2021/data/Philippines/covid19_deaths.csv')
  } else {
    
    setnames(d, 'deaths', 'COVID19_deaths')
    write_csv(d, path = 'TheLancet_global_minimum_estimates_2021/data/Philippines/covid19_deaths.csv')
  }
}

# Poland
process_poland_covid19 = function(){
  data = readxl::read_xlsx('TheLancet_global_minimum_estimates_2021/data/Poland/poland-2020-by-age-and-gender.xlsx', sheet = 2)
  setnames(data, 1:3, c('age', 'Female', 'Male'))
  data = data[3:nrow(data),1:3]
  data$country = 'Poland'
  data$date = '2020/10/09'
  data =  reshape2::melt(data, id.vars = c('country','date', 'age'), variable.name =  'gender', value.name = 'COVID19_deaths')
  write_csv(data, path = 'TheLancet_global_minimum_estimates_2021/data/Poland/covid19_deaths.csv')
  
  data$COVID19_deaths = as.numeric(data$COVID19_deaths)
  write_csv(path = paste0("TheLancet_global_minimum_estimates_2021/data/Poland/","poland_deaths.csv"),data)  
  total_covid = sum(data$COVID19_deaths)
  
  # Selects deaths from JHU
  deaths_data = read.csv("TheLancet_global_minimum_estimates_2021/data/04-30-2021.csv")
  pol <- deaths_data[which(deaths_data$Country_Region == "Poland"),]
  total = sum(pol$Deaths)
  
  rate = total/total_covid
  data$date = '2021-04-30'
  data$COVID19_deaths = data$COVID19_deaths * rate
  
  data$death = data$COVID19_deaths
  data = select(data, age, gender, death)
  
  write_csv(data, path = 'TheLancet_global_minimum_estimates_2021/data/Poland/poland_all.csv')
}

# Russia
process_russia_excess = function(){
  # Need to weight by age before weight by sex
  total_excess = 403878
  demog <- read_excel("TheLancet_global_minimum_estimates_2021/data/Russia/demo24.xls")
  demog_2019 <- demog[33,]
  men <- sum(as.numeric(demog_2019[c(2,4,6,8,10,12)]))
  women <- sum(as.numeric(demog_2019[c(3,5,7,9,11,13)]))
  prop_excess_men <- men/(men + women)
  
  data_pop = readxl::read_xlsx('TheLancet_global_minimum_estimates_2021/data/Russia/pop.xlsx', sheet = 2)
  ages <- data_pop[1, 5:25]
  data_pop = as.data.frame(data_pop[c(7,8),5:25])
  colnames(data_pop) = ages
  data_pop$sex = c("Female", "Male")
  data_pop <- gather(data_pop, key = "age", value = "pop", -sex)
  data_pop$pop <- as.numeric(data_pop$pop) * 1000
  data_pop <- data_pop %>% group_by(age) %>% summarise("pop" = sum(pop))
  
  ifrs_data = c(0, 0.01, 0.01, 0.02, 0.03, 0.04, 0.06, 0.09, 0.15, 0.23, 0.36, 0.57,
                0.89, 1.39, 2.17, 3.39, 5.3, 8.28, 16.19, 16.19, 16.19)
  data_pop$ifr <- rep(ifrs_data)
  data_pop$pop_weighted_ifr <- data_pop$pop * data_pop$ifr / 100
  data_pop <- data_pop %>% mutate("rate" = pop_weighted_ifr/sum(pop_weighted_ifr))
  data_pop$total_excess_deaths <- data_pop$rate * total_excess
  data_pop$weighted_excess_deaths <- round(data_pop$total_excess_deaths * data_pop$rate)
  
  data_pop_sex <- rbind(data_pop, data_pop)
  data_pop_sex$sex <- rep(c("Female", "Male"), each = length(data_pop$age))
  data_pop_sex$prop <- rep(c(1-prop_excess_men, prop_excess_men), each = length(data_pop$age))
  data_pop_sex$weighted_excess_deaths <- round(data_pop_sex$prop * data_pop_sex$total_excess_deaths)
  
  
  d_summary <- select(data_pop_sex, sex, age, weighted_excess_deaths)
  write_csv(d_summary %>% arrange(age, sex), path = 'TheLancet_global_minimum_estimates_2021/data/Russia/all_excess_deaths.csv')
  d_summary$age = ifelse(d_summary$age %in% c('0-4', '5-9', '10-14'), '0-14',
                         ifelse(d_summary$age %in% c('45-49','50-54', '55-59', '60-64'), '45-64',
                                ifelse(d_summary$age %in% c("65-69","70-74","75-79","80-84","85-89" ,"90-94", "95-99", "100+" ),'65+', '15-44')))
  d_summary = d_summary %>% group_by(age, sex) %>% 
    mutate(excess_deaths = sum(weighted_excess_deaths)) %>% 
    ungroup() %>% 
    select(-weighted_excess_deaths) %>% 
    distinct()
  d_summary  = d_summary %>% arrange(age, sex) %>% filter(age != '0-14')
  write_csv(d_summary, path = 'TheLancet_global_minimum_estimates_2021/data/Russia/excess_deaths.csv')
}


# Spain
process_spain = function(){
  tmp = readxl::read_xlsx('TheLancet_global_minimum_estimates_2021/data/Spain/Deaths-Age-Sex_Covid-19_Spain_06-05.xlsx', sheet = 6)
  data = tmp[7:15, c(1,c(43,45,48))]
  data = data.frame(data)
  names(data) = c('age', 'male', 'female', 'both')
  data$male = as.numeric(data$male)
  data$female = as.numeric(data$female)
  data$both = as.numeric(data$both)
  
  data = data %>% 
    select(-both) %>%
    gather(key = "gender", value = "covid_deaths", -age)
  data$gender = ifelse(data$gender  == 'male', 'Male', 'Female')
  
  excess_death = read.csv('TheLancet_global_minimum_estimates_2021/data/euro_excess.csv')
  excess_death = as.data.table(excess_death) %>% select(week, year, spain, age, gender)
  excess_death = excess_death[!is.na(excess_death$spain),]
  excess_death$age = as.character(excess_death$age)
  
  excess_death$age = ifelse(excess_death$age %in% c('0-5', '5-9'), '0-9',
                            ifelse(excess_death$age %in% c('10-14', '15-19'), '10-19',
                                   ifelse(excess_death$age %in% c('20-24', '25-29'), '20-29',
                                          ifelse(excess_death$age %in% c('30-34', '35-39'), '30-39',
                                                 ifelse(excess_death$age %in% c('40-44', '45-49'), '40-49',
                                                        ifelse(excess_death$age %in% c('50-54', '55-59'), '50-59',
                                                               ifelse(excess_death$age %in% c('60-64', '65-69'), '60-69',
                                                                      ifelse(excess_death$age %in% c('70-74', '75-79'), '70-79',
                                                                                    '80+'))))))))
  excess_death = excess_death %>%  
    filter((week >= 10 & year == "2020") | (week <= 17 & year == "2021")) %>%
    group_by(gender, week, age) %>%
    mutate(excess_death = sum(spain)) %>% select(-c(spain)) %>% ungroup() %>% distinct()
  excess_death$gender = as.character(excess_death$gender)
  excess_death =  excess_death[which(excess_death$gender != 'T'),]
  excess_death$gender = ifelse(excess_death$gender == 'M', 'Male', 'Female')
  
  excess_death = excess_death %>%
    group_by(age, gender) %>%
    summarise("excess_deaths" = sum(excess_death))
  
  d_merge = left_join(data, excess_death, by = c('gender', 'age' = 'age'))
  write_csv(path = paste0("TheLancet_global_minimum_estimates_2021/data/Spain/","spain_all.csv"),d_merge)
  
  total_covid = sum(d_merge$covid_deaths)
  
  # Selects deaths from JHU
  deaths_data = read.csv("TheLancet_global_minimum_estimates_2021/data/04-30-2021.csv")
  spa <- deaths_data[which(deaths_data$Country_Region == "Spain"),]
  total = sum(spa$Deaths)
  
  rate = total / total_covid
  d_merge$covid_deaths = round(d_merge$covid_deaths * rate)
  d_merge$deaths = ifelse(d_merge$covid_deaths > d_merge$excess_deaths, d_merge$covid_deaths, d_merge$excess_deaths)
  d_merge = d_merge %>% select(age, gender, covid_deaths, excess_deaths, deaths)
  write_csv(d_merge, path = 'TheLancet_global_minimum_estimates_2021/data/Spain/spain_all.csv')
}

# South Africa
process_sa_covid19 = function(){
  d_merge = read.csv('TheLancet_global_minimum_estimates_2021/data/SouthAfrica/covid19_deaths_raw_apr.csv')

  # Selects deaths from JHU
  deaths_data = read.csv("TheLancet_global_minimum_estimates_2021/data/04-30-2021.csv")
  sa <- deaths_data[which(deaths_data$Country_Region == "South Africa"),]
  total = sum(sa$Deaths)
  
  d_merge$date = '2021-04-30'
  d_merge$rate = d_merge$deaths/sum(d_merge$deaths)
  d_merge$deaths = round(d_merge$rate * total)
  setnames(d_merge, 'deaths', 'COVID19_deaths')
  ####  add 
  d_merge$age = as.character(d_merge$age)
  d_merge$age = ifelse(d_merge$age == 'Oct-19', '10-19', d_merge$age)
  write_csv(d_merge %>% select(-rate), path = 'TheLancet_global_minimum_estimates_2021/data/SouthAfrica/covid19_deaths.csv')
}

# USA
process_usa = function(){
  tmp = readxl::read_xlsx('TheLancet_global_minimum_estimates_2021/data/USA/Deaths-Age-Sex_Covid-19_USA_06.05.xlsx', sheet = 2)
  data = tmp[7:16, c(1,c(15, 17, 20))]
  data = data.frame(data)
  names(data) = c('age', 'male', 'female', 'both')
  data$male = as.numeric(data$male)
  data$female = as.numeric(data$female)
  data$both = as.numeric(data$both)
  
  data_covid = data %>% 
    select(-both) %>%
    gather(key = "gender", value = "covid_deaths", -age)
  data_covid$gender = ifelse(data_covid$gender  == 'male', 'Male', 'Female')
  
  #url = 'https://data.cdc.gov/api/views/m74n-4hbs/rows.csv?accessType=DOWNLOAD'
  #download.file(url, 'TheLancet_global_minimum_estimates_2021/data/USA/usa_excess_mortality_all_apr.csv')
  data = read.csv('TheLancet_global_minimum_estimates_2021/data/USA/usa_excess_mortality_all_apr.csv')
  date = unique(data$AnalysisDate)
  data = as.data.table(data %>% filter(RaceEthnicity == 'All Race/Ethnicity Groups',
                                       Sex %in% c('Female (F)', 'Male (M)'),
                                       AgeGroup != 'Not stated',
                                       AgeGroup != 'All Ages') %>%
                         select(Time.Period, MMWRweek, Weekending, Sex, AgeGroup, Deaths..unweighted., Deaths..weighted.))
  
  data$AgeGroup = as.character(data$AgeGroup)
  # 'Weighting of provisional counts is done to account for potential underreporting in the most recent weeks
  data = data%>% select(-Deaths..unweighted., -Weekending)
  setnames(data, 1:ncol(data), c('year', 'week', 'gender', 'age', 'value'))
  data$age = gsub(' Years', '', data$age )
  data$gender = as.character(data$gender)
  data$gender = ifelse(data$gender == 'Female (F)', 'Female',
                       ifelse(data$gender == 'Male (M)', 'Male', ''))
  
  tmp_pre19 = data %>%filter(! year %in% c('2020', '2021')) %>% 
    group_by(week, gender, age) %>% 
    mutate(avg_death = sum(value)/5) %>% 
    select(-value, -year) %>% 
    distinct()
  tmp_20 = data %>%filter(year %in% c('2020', '2021'))
  dataset = merge(tmp_20, tmp_pre19, by= c('week', 'gender', 'age'))
  dataset = as.data.table(dataset)
  dataset[,excess_deaths := value- avg_death]
  dataset = dataset %>% select(week, year, gender, age, excess_deaths) %>%
    arrange(week, age, desc(gender))
  write_csv(path = ("TheLancet_global_minimum_estimates_2021/data/USA/excess_deaths.csv"),dataset)
  excess_death = copy(dataset)
  excess_death$age = ifelse(excess_death$age %in% c('20-24', '15-19'), '15-24',
                            ifelse(excess_death$age %in% c('30-34', '25-29'), '25-34',
                                   ifelse(excess_death$age %in% c('40-44', '35-39'), '35-44',
                                          ifelse(excess_death$age %in% c('50-54', '45-49'), '45-54',
                                                 ifelse(excess_death$age %in% c('60-64', '55-59'), '55-64',
                                                        ifelse(excess_death$age %in% c('70-74', '65-69'), '65-74',
                                                               ifelse(excess_death$age %in% c('80-84', '75-79'), '75-84',excess_death$age
                                                               )))))))
  excess_death = excess_death %>%  
    filter((week >= 10 & year == "2020") | (week <= 17 & year == "2021")) %>%
    group_by(gender, week, age) %>%
    mutate(excess_death = sum(excess_deaths)) %>% 
    select(-c(excess_deaths)) %>% ungroup() %>% distinct()
  
  excess_death = excess_death %>%
    group_by(age, gender) %>%
    summarise("excess_deaths" = sum(excess_death))
  
  data_covid$age <- ifelse(data_covid$age %in% c('1-4', '5-14'), '0-14', data_covid$age)
  data_covid <- data_covid %>%
    group_by(age, gender) %>%
    summarise(covid_deaths = sum(covid_deaths))
  d_merge = left_join(data_covid, excess_death, by = c('gender', 'age' = 'age'))
  write_csv(path = paste0("TheLancet_global_minimum_estimates_2021/data/USA/","usa_all.csv"),d_merge)
  
  total_covid = sum(d_merge$covid_deaths)
  
  # Selects deaths from JHU
  deaths_data = read.csv("TheLancet_global_minimum_estimates_2021/data/04-30-2021.csv")
  us <- deaths_data[which(deaths_data$Country_Region == "US"),]
  total = sum(us$Deaths)
  
  rate = total / total_covid
  d_merge$covid_deaths = round(d_merge$covid_deaths * rate)
  d_merge$deaths = ifelse(d_merge$covid_deaths > d_merge$excess_deaths, d_merge$covid_deaths, d_merge$excess_deaths)
  #d_merge$deaths = d_merge$excess_deaths
  d_merge = d_merge %>% select(age, gender, excess_deaths,covid_deaths, deaths)
  write_csv(d_merge, path = 'TheLancet_global_minimum_estimates_2021/data/USA/usa_all.csv')
}

# Zimbabwe
process_zimbabwe = function(){
  data = read_xlsx('TheLancet_global_minimum_estimates_2021/data/Zimbabwe/zimbabwe_covid_raw.xlsx')
  data = data %>%
    select(Category, Male, Female) %>%
    gather( key = gender, value = rate, -Category)
  
  # Selects deaths from JHU
  deaths_data = read.csv("TheLancet_global_minimum_estimates_2021/data/04-30-2021.csv")
  zim <- deaths_data[which(deaths_data$Country_Region == "Zimbabwe"),]
  total = sum(zim$Deaths)
  
  data$deaths = round(data$rate * total / 100)
  data = data %>% select(-rate)
  names(data) <- c("age", "gender", "deaths")
  write_csv(data, file = 'TheLancet_global_minimum_estimates_2021/data/Zimbabwe/covid19_deaths.csv')
}

