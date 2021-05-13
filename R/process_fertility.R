library(readxl)
library(data.table)
library(tidyverse)
library(rjson)

# Code for processing fertility from IHME and UNSD and fertility plot
process_fertility_plots = function(country){
  if (country == 'england_wales'){
    data_combine = read.csv('data/UK/england_wales_fertility_all.csv')
    data_combine$gender = ifelse(data_combine$gender == 'M', 'Male', 'Female')
    names(data_combine)[which(names(data_combine) == 'rate')] = "fertility_rate"
  } else{
    data_father = read.csv(paste0('data/', country, '/', tolower(country), '_fertility_m_all.csv'))
    data_mother = read.csv(paste0('data/', country, '/', tolower(country), '_fertility_f.csv'))
    data_mother$gender = 'Female'
    data_father$gender = 'Male'
    data_father = data_father %>% filter(!age %in% c('under 15-NA', '80+-NA'))
    data_mother = data_mother %>% filter(!age %in% c('under 15-NA', '80+-NA'))
    data_mother$fertility_rate = ifelse(data_mother$age %in% c('50-54', '55-59', '60-64', 
                                                     '65-69', '70-74', '75-79', 
                                                     '80+',  '65+', '55+'), 
                              NA, data_mother$fertility_rate)
    data_father$fertility_rate = ifelse(data_father$age %in% c('80+'), NA, data_father$fertility_rate)
    
    if (country %in% c('Brazil', 'Colombia', 'India', 'Kenya', 'Nigeria', 'Peru', 'SouthAfrica')){
      data_combine = rbind(data_father, data_mother)
      data_combine$fertility_rate = data_combine$fertility_rate * 1000
      
    } else {
      if (country == "Iran"){
        names(data_father) = c("age", "year", "gender", "fertility_rate")
      }
      data_father = data_father %>% select(year, age, gender, fertility_rate)
      data_mother$year = data_mother$date

      data_mother = data_mother %>% select(year, age, gender, fertility_rate)
      #setnames(data_mother, 'afr', 'fertility_rate')
      data_combine = rbind(data_father, data_mother)
      #setnames(data_combine, 'fertility_rate', 'rate')
    }
  }
  data_combine$year = as.character(data_combine$year)
  ggplot(data_combine) +
    geom_point(aes(x = age, y = fertility_rate, color = year)) +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1)) +
    guides(col = guide_legend(nrow = 7)) +
    labs(x = 'Age') +
    facet_wrap(~ gender, 
               strip.position = "left", 
               labeller = as_labeller(c(Female = "Fertility Rate per 1000 women", 
                                        Male = "Fertility Rate per 1000 men") ) ) + 
    theme(strip.background =element_rect(fill="white")) + 
    ylab(NULL) +
    theme(strip.background = element_blank(),
          strip.placement = "outside")
  ggsave(paste0("figures/fertility_", tolower(country), ".pdf"), width = 10, height = 4)
}

# Process fertility data
process_fertility_all_countries = function(){
  all_combine = NaN
  list_file = list.files('data/fertility_update', '.json')
  list_file = sort(list_file, decreasing = TRUE)
  for (name_file in list_file)
  {
    year = strsplit(name_file, '_')[[1]][1]
    table_name = strsplit(strsplit(name_file, '_')[[1]][2], '.json')[[1]][1]
    json_data = suppressWarnings(fromJSON(paste(readLines(paste0('data/fertility_update/',name_file)))))
    data = as.data.table(unlist(json_data))
    data$country = names(json_data)
    setnames(data, 1, 'idx')
    dataset = readxl::read_xls(paste0('data/fertility_update/', strsplit(name_file, '.json')[[1]][1],'.xls'), sheet = 1)
    if (name_file %in% c( '2003_10.json', '2004_10.json')){
      dataset = as.data.table(dataset[,c(1,3)])
      names(dataset) = c('age', 'value')
      cat('\n===>process', year, 'table', table_name)
      tmp = NaN
      for (i in 1:nrow(data)){
        df = dataset[data[[1]][i]: (data[[1]][i]+11),]
        df$date = dataset[data[[1]][i]-1,1]
        df = df[-(1:2),]
        df$country = data[[2]][i]
        tmp = rbind(tmp, df ,fill = TRUE)
      }
      tmp$age = gsub(' -', '-', tmp$age)
    } else{
      dataset =  as.data.table(dataset[,1:2])
      names(dataset) = c('age', 'value')
      cat('\n===>process', year, 'table', table_name)
      tmp = NaN
      for (i in 1:nrow(data)){
        df = dataset[data[[1]][i]: (data[[1]][i]+13),]
        df$date = strsplit(df[[1]][1], ' ')[[1]][1]
        df = df[-(1:2),]
        df$country = data[[2]][i]
        tmp = rbind(tmp, df ,fill = TRUE)
      }
    }

    tmp = tmp[-1,]
    tmp = tmp %>% select(country, date, age, value)
    tmp$gender = ifelse(table_name == '10', 'Female','Male')
    tmp$age = unlist(lapply(tmp$age, function(x) strsplit(x, '\\.')[[1]][1]))
    all_combine = rbind(all_combine, tmp, fill = TRUE)
    cat('\n ===> processed', year, 'table', table_name)
  }
  all_combine = all_combine[-1,]
  all_combine = all_combine %>% select(-x)%>% unique() %>% arrange(country, date, age, desc(gender))
  all_combine = all_combine[which(all_combine$date >= '2003'),]
  all_combine$date = as.character(all_combine$date)
  all_combine$date = gsub('[*]', '',all_combine$date)
  all_combine = all_combine %>% filter(!is.na(date), !is.na(value))
  all_combine = all_combine %>% arrange(country, date, gender, age)
  all_combine = all_combine %>% distinct(country, date, age,gender, .keep_all = TRUE)
  setnames(all_combine, 'date', 'year')
  write_csv(path = "data/fertility_update/parents_live_births.csv", all_combine)
}

# England and Wales
process_england_wales_fertility = function(){
  # url = 'https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2flivebirths%2fdatasets%2fbirthsbyparentscharacteristics%2f2018/parentscharacteristics2018workbook.xls'
  #download.file(url, 'data/fertility/england_wales_births.xls')
  data = readxl::read_xls('data/UK/england_wales_births.xls', sheet = 11)
  d_name = as.character(data[6,])
  data = data[7:nrow(data),]
  d_name[1:2] = c('year', 'ignore')
  names(data) = d_name
  data = data %>% filter(!is.na(ignore)) %>% select(-ignore)
  data = data %>% filter(year >= '2002')
  data = reshape2::melt(data, id.vars = c('year'), variable.name = 'age', value.name = 'rate')
  data$age = as.character(data$age)
  data$age = gsub(' to ','-', data$age)
  data$age = gsub(' and over', '+', data$age)
  data$age = gsub('Under ', '0-', data$age)
  data$gender = 'M'
  data$country = 'England_Wales'
  data$rate = as.numeric(data$rate)
  
  pop = read.csv('data/UK/pop_england_wales.csv')
  pop = pop[,c(3:24)]
  pop = reshape2::melt(pop, id.vars = c('country', 'age', 'sex'), variable.name = 'year', value.name = 'value')
  
  pop = pop %>% group_by(year, age, sex) %>% mutate(pop = sum(value)) %>% ungroup() %>%
    select(-value, -country) %>% distinct()
  pop = pop %>% filter(sex == 1, age %in% c(seq(20)-1, seq(60,90)))
  pop$year = as.character(pop$year)
  pop$year = gsub('population_', '',pop$year)
  pop = pop %>% filter(year >= '2002')
  pop_save = copy(pop)
  
  pop$age = ifelse(pop$age %in% seq(15,19), '15-19',
                   ifelse(pop$age %in% seq(60,64), '60-64', 'others'))
  
  pop = pop %>% group_by(age, sex, year) %>% mutate(pop_nb = sum(pop)) %>% ungroup() %>%
    select(-pop) %>% filter(age != 'others') %>% distinct()
  
  pop1 = copy(pop_save)
  pop1$age = ifelse(pop1$age %in% seq(60,90), '60+',
                    '0-20')
  
  pop1 = pop1 %>% group_by(age, sex, year) %>% mutate(pop_nb = sum(pop)) %>% ungroup() %>%
    select(-pop) %>% filter(age != 'others') %>% distinct()
  
  
  setnames(pop1, 'sex', 'gender')
  pop1$gender = 'M'
  setnames(pop, 'sex', 'gender')
  pop$gender = 'M'
  d_data = merge(data, pop1, by = c('age', 'gender', 'year'))
  d_data = as.data.table(d_data)
  d_data[,births := rate * pop_nb/1000]
  d_data$age = as.character(d_data$age)
  d_data$age = ifelse(d_data$age == '0-20', '15-19', '60-64')
  setnames(d_data, c('rate', 'pop_nb'), c('rate_pre', 'pop_pre'))
  dd_data = merge(d_data , pop, by = c('age', 'gender', 'year'))
  dd_data[, rate := 1000* births/pop_nb]
  data_all = rbind(dd_data %>% select(year, age, rate, gender, country), data %>% filter(age != '0-20', age!= '60+'))
  data_all = data_all %>% arrange(year, age)
  ##  per 1000 men
  write_csv(data_all, file = 'data/UK/england_wales_fertility_m.csv')
  
  # female
  # url = 'https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2flivebirths%2fdatasets%2fbirthsbyparentscharacteristics%2f2018/parentscharacteristics2018workbook.xls'
  #download.file(url, 'data/fertility/england_wales_births.xls')
  data = readxl::read_xls('data/UK/england_wales_births.xls', sheet = 5)
  d_name = as.character(data[6,])
  d_name[1:2] = c('year', 'ignore')
  data = data[7:nrow(data),]
  names(data) = d_name
  data = data %>% filter(!is.na(year), !is.na(ignore))
  data = data %>% filter(year >= '2002') %>% select(-ignore)
  data = data[,-2]
  data = reshape2::melt(data, id.vars = c('year'), variable.name = 'age', value.name = 'value')
  data$age = as.character(data$age)
  data$age = gsub(' to ','-', data$age)
  # assume women younger than 15 cannot birth
  data$age = gsub('Under 20', '15-19', data$age)
  # assume women older than 49 won't be able to birth
  data$age = gsub('45 and over', '45-49', data$age)
  data$gender = 'F'
  data$country = 'England_Wales'
  data$value = as.numeric(data$value)
  write_csv(data, file = 'data/UK/england_births_f.csv')
  
  pop = read.csv('data/UK/pop_england_wales.csv')
  pop = pop[,c(3:24)]
  pop = reshape2::melt(pop, id.vars = c('country', 'age', 'sex'), variable.name = 'year', value.name = 'value')
  
  pop = pop %>% group_by(year, age, sex) %>% mutate(pop = sum(value)) %>% ungroup() %>%
    select(-value, -country) %>% distinct()
  pop$age = ifelse(pop$age %in% seq(15,19), '15-19',
                   ifelse(pop$age %in% seq(20,24), '20-24',
                          ifelse(pop$age %in% seq(25,29), '25-29',
                                 ifelse(pop$age %in% seq(30,34), '30-34',
                                        ifelse(pop$age %in% seq(35,39), '35-39',
                                               ifelse(pop$age %in% seq(40,44), '40-44',
                                                      ifelse(pop$age %in% seq(45,49), '45-49', 'others')))))))
  
  pop = pop %>% group_by(age, sex, year) %>% mutate(pop_nb = sum(pop)) %>% ungroup() %>%
    select(-pop) %>% distinct()
  pop$year = gsub('population_', '',pop$year)
  pop = pop %>% filter(year >= '2002')
  pop$sex = ifelse(pop$sex == 1, 'M','F')
  setnames(pop, 'sex', 'gender')
  d_data = merge(data, pop, by = c('age', 'gender', 'year'))
  d_data = as.data.table(d_data)
  d_data[,rate := 1000* value /pop_nb]
  #d_data$gender = as.factor('F')
  write_csv(d_data, path = 'data/UK/england_wales_fertility_f.csv')
  
  d_fertility_m = read.csv('data/UK/england_wales_fertility_m.csv')
  d_all = rbind(d_data %>% select(-pop_nb, -value), d_fertility_m)
  write_csv(d_all, path = 'data/UK/england_wales_fertility_all.csv')
}

# France
process_france_fertility = function(){
  data_fertility = read.csv('data/fertility/unsd_live_births.csv')
  data_fertility = data_fertility %>% filter(country == 'France',
                                             age != 'Unknown - Inconnu',
                                             gender == 'Male')
  
  data_pop = readxl::read_xlsx('data/pop.xlsx', sheet = 2)
  countries = c('France')
  names(data_pop) = as.character(data_pop[1,])
  # data (thousand)
  data_pop = as.data.table(data_pop) %>% filter(Location %in% countries) %>% select(Location, Time, Age, Female, Male)
  setnames(data_pop, 1:ncol(data_pop), c('country', 'year', 'age', 'Female', 'Male'))
  data_pop =  reshape2::melt(data_pop, id.vars = c('country', 'year', 'age'), variable.name =  'gender', value.name = 'pop')
  data_pop = as.data.table(data_pop %>% filter(gender == 'Male'))  
  
  data_pop$age = as.character(data_pop$age)
  `%notin%` = Negate(`%in%`)
  data_pop = data_pop %>% filter(age %notin% c('0-4', '5-9', '10-14'))
  data_pop$age = ifelse(data_pop$age %in% c( '65-69', '70-74', '75-79', 
                                             '80-84', '85-89', '90-94', '95-99', '100+'), '65+', data_pop$age)
  
  data_pop$pop = as.numeric(data_pop$pop)
  data_pop = data_pop %>% group_by(age, year) %>% mutate(pop = sum(pop)) %>% ungroup() %>% distinct()
  ## population * 1000
  
  data_fertility = as.data.table(data_fertility)
  data_fertility$age = as.character(data_fertility$age)
  data_fertility$age = gsub(' - ', '-', data_fertility$age)
  data_fertility$age = gsub(' [+]', '+', data_fertility$age)
  
  # assume male are able to birth from 15 years old
  data_fertility$age = ifelse(data_fertility$age == '0-19', '15-19', data_fertility$age)
  data_fertility$year = as.character(data_fertility$year)
  data_fertility$value = as.numeric(as.character(data_fertility$value))
  data_combine= merge(data_fertility, data_pop, by = c('country','year', 'age', 'gender'))
  data_combine[,fertility_rate := value / (pop)]
  # live births per 1000 male
  write_csv(path = 'data/France/fertility_m.csv', data_combine)
  
  data_combine = read.csv('data/France/fertility_m.csv')
  data_combine$year = as.character(data_combine$year)
  ggplot(data_combine) +
    geom_point(aes(x = age, y = fertility_rate, color = year)) +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))
  
  data_combine_first = data_combine %>% filter(year == '2007')
  tmp = copy(data_combine_first)
  for (i in seq(2002,2006)) {
    tmp$year = i
    data_combine = rbind(tmp, data_combine)
  }
  data_combine_mid = data_combine %>% filter(year == '2012')
  data_combine_mid$year = '2011'
  data_combine = rbind(data_combine_mid, data_combine)
  
  data_combine_last = data_combine %>% filter(year == '2017')
  tmp = copy(data_combine_last)
  for (i in seq(2018,2020)) {
    tmp$year = i
    data_combine = rbind(data_combine, tmp)
  }
  
  
  data_combine = data_combine %>% arrange(year, age)
  write_csv(path = 'data/France/france_fertility_m_all.csv', data_combine)
  
  # from IHME get the female fertility rates
  #url = 'http://ghdx.healthdata.org/sites/default/files/record-attached-files/IHME_GBD_2019_FERTILITY_1950_2019_ASFR_0.zip'
  #download.file(url, 'data/afr.zip')
  #data = fread(unzip('data/afr.zip', files = 'IHME_GBD_2019_FERTILITY_1950_2019_ASFR_Y2020M10D27.CSV'))
  data = read.csv("data/IHME_GBD_2019_FERTILITY_1950_2019_ASFR_Y2020M10D27.CSV")
  data = data %>% select(location_name, age_group_name, year_id, val)
  countries = c('France')
  data = data %>% filter(location_name %in% countries & year_id %in% seq(2003, 2020))
  data$age_group_name = as.character(data$age_group_name)
  data$age_group_name = gsub(' to ', '-', data$age_group_name)
  setnames(data, 1:4, c('country', 'age', 'date', 'afr'))
  data$gender = 'Female'
  data = data %>% filter(age != '10-14')
  d_rate_ihme = copy(data)
  d_rate_ihme$fertility_rate = d_rate_ihme$afr * 1000
  
  write_csv(file = 'data/France/france_fertility_f_ihme.csv', d_rate_ihme)
  
  # from WPP
  data = read_excel("data/WPP2019_FERT_F07_AGE_SPECIFIC_FERTILITY.xlsx")
  names(data) <- data[12,]
  data = data[which(data$`Region, subregion, country or area *` == "France"),]

  d_wpp <- select(data, Period, "15-19", "20-24", "25-29", "30-34", "35-39", "40-44",  "45-49")
  d_wpp_long <- gather(d_wpp, -Period, key = "age", value = "fertility_rate")
  d_wpp_long$country = "France"
  d_wpp_long$gender = "Female"
  
  d_wpp_long = d_wpp_long[d_wpp_long$Period %in% c("2000-2005", "2005-2010", "2010-2015", "2015-2020"),]
  
  d_wpp_long_rep = d_wpp_long %>% slice(rep(1:n(), each = 5))
  d_wpp_long_rep$date = rep(2000:2019, 7)
  d_wpp_long_rep = d_wpp_long_rep[d_wpp_long_rep$date > 2002,]
  
  d_rate_ihme$date = factor(d_rate_ihme$date)
  d_wpp_long_rep$date = factor(d_wpp_long_rep$date)
  
  d_wpp_long_rep$fertility_rate = as.numeric(d_wpp_long_rep$fertility_rate)
  
  #ggplot() +
  #  geom_point(data = d_rate_ihme, aes(age, fertility_rate, col = date)) +
  #  geom_point(data = d_wpp_long_rep, aes(age, fertility_rate, col = date), shape = 15) +
  #  facet_wrap(~date)
  
  write_csv(path = 'data/France/france_fertility_f.csv', d_wpp_long_rep)
}

compare_fertility_france = function(){
  data_fertility = read.csv('data/fertility/unsd_live_births.csv')
  data_fertility = data_fertility %>% filter(country == 'France',
                                             age != 'Unknown - Inconnu',
                                             gender == 'Female')
  
  data_pop = readxl::read_xlsx('data/pop.xlsx', sheet = 2)
  countries = c('France')
  names(data_pop) = as.character(data_pop[1,])
  # data (thousand)
  data_pop = as.data.table(data_pop) %>% filter(Location %in% countries) %>% select(Location, Time, Age, Female, Male)
  setnames(data_pop, 1:ncol(data_pop), c('country', 'year', 'age', 'Female', 'Male'))
  data_pop =  reshape2::melt(data_pop, id.vars = c('country', 'year', 'age'), variable.name =  'gender', value.name = 'pop')
  data_pop = as.data.table(data_pop %>% filter(gender == 'Female'))  
  
  data_pop$age = as.character(data_pop$age)
  `%notin%` = Negate(`%in%`)
  data_pop = data_pop %>% filter(age %notin% c('0-4', '5-9', '10-14'))
  data_pop$age = ifelse(data_pop$age %in% c( '50-54', '55-59', '60-64', '65-69', '70-74', '75-79', 
                                             '80-84', '85-89', '90-94', '95-99', '100+'), '50+', data_pop$age)
  
  data_pop$pop = as.numeric(data_pop$pop)
  data_pop = data_pop %>% group_by(age, year) %>% mutate(pop = sum(pop)) %>% ungroup() %>% distinct()
  ## population * 1000
  
  data_fertility = as.data.table(data_fertility)
  data_fertility$age = as.character(data_fertility$age)
  data_fertility$age = gsub(' - ', '-', data_fertility$age)
  #data_fertility$age = gsub(' [+]', '+', data_fertility$age)
  
  data_fertility$year = as.character(data_fertility$year)
  data_fertility$value = as.numeric(as.character(data_fertility$value))
  data_combine= merge(data_fertility, data_pop, by = c('country','year', 'age', 'gender'))
  data_combine[,fertility_rate := value / (pop)]
  # live births per 1000 male

  data_combine = data_combine %>% select(year, age, fertility_rate)
  data_combine$source = 'UNSD'
  
  data_combine_f = read.csv('data/France/france_fertility_f.csv')
  data_combine_f = data_combine_f %>% select(date, age, fertility_rate)
  data_combine_f$source = 'UNWPP'
  setnames(data_combine_f, 'date', 'year')
  
  data_combine_f_ihme = read.csv('data/France/france_fertility_f_ihme.csv')
  data_combine_f_ihme = data_combine_f_ihme %>% select(date, age, fertility_rate)
  data_combine_f_ihme$source = 'IHME'
  setnames(data_combine_f_ihme, 'date', 'year')
  
  data_combine = rbind(data_combine, data_combine_f, data_combine_f_ihme )
  data_combine$year = as.character(data_combine$year)
  data_combine$age = as.character(data_combine$age)
  data_combine$age = ifelse(data_combine$age == '50-54', '50+', data_combine$age)
  
  
  
  
  ggplot(data_combine) +
    geom_point(aes(x = age, y = fertility_rate, color = year, shape = source)) +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1)) +
    guides(col = guide_legend(nrow = 7)) +
    labs(x = 'Age of woman', y = 'Fertility Rate per 1000 women')
  ggsave("figures/compare_fertility_france.pdf", width = 7, height = 5)
}

# Germany
process_germany_fertility = function(){
  data_fertility = read.csv('data/fertility/unsd_live_births.csv')
  data_fertility = data_fertility %>% filter(country == 'Germany',
                                             age != 'Unknown - Inconnu',
                                             gender == 'Male')
  
  data_pop = readxl::read_xlsx('data/fertility/pop.xlsx', sheet = 2)
  countries = c('Germany')
  names(data_pop) = as.character(data_pop[1,])
  # data (thousand)
  data_pop = as.data.table(data_pop) %>% filter(Location %in% countries) %>% select(Location, Time, Age, Female, Male)
  setnames(data_pop, 1:ncol(data_pop), c('country', 'year', 'age', 'Female', 'Male'))
  data_pop =  reshape2::melt(data_pop, id.vars = c('country', 'year', 'age'), variable.name =  'gender', value.name = 'pop')
  data_pop = as.data.table(data_pop %>% filter(gender == 'Male'))  
  
  data_pop$age = as.character(data_pop$age)
  `%notin%` = Negate(`%in%`)
  data_pop = data_pop %>% filter(age %notin% c('0-4', '5-9', '10-14'))
  data_pop$age = ifelse(data_pop$age %in% c( '65-69', '70-74', '75-79', 
                                             '80-84', '85-89', '90-94', '95-99', '100+'), '65+', data_pop$age)
  
  data_pop$pop = as.numeric(data_pop$pop)
  data_pop = data_pop %>% group_by(age, year) %>% mutate(pop = sum(pop)) %>% ungroup() %>% distinct()
  ## population * 1000
  
  data_fertility = as.data.table(data_fertility)
  data_fertility$age = as.character(data_fertility$age)
  data_fertility$age = gsub(' - ', '-', data_fertility$age)
  data_fertility$age = gsub(' [+]', '+', data_fertility$age)
  
  # assume male are able to birth from 15 years old
  data_fertility$age = ifelse(data_fertility$age == '0-19', '15-19', data_fertility$age)
  data_fertility$year = as.character(data_fertility$year)
  data_fertility$value = as.numeric(as.character(data_fertility$value))
  data_combine= merge(data_fertility, data_pop, by = c('country','year', 'age', 'gender'))
  data_combine[,fertility_rate := value / (pop)]
  # live births per 1000 male
  write_csv(path = 'data/Germany/germany_fertility_m.csv', data_combine)
  data_combine = read.csv('data/Germany/germany_fertility_m.csv')
  data_combine$year = as.character(data_combine$year)
  ggplot(data_combine) +
    geom_point(aes(x = age, y = fertility_rate, color = year)) +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))
  
  data_combine_first = data_combine %>% filter(year == '2007')
  tmp = copy(data_combine_first)
  for (i in seq(2002,2006)) {
    tmp$year = i
    data_combine = rbind(tmp, data_combine)
  }
  data_combine_first = data_combine %>% filter(year == '2011')
  tmp = copy(data_combine_first)
  for (i in seq(2008,2010)) {
    tmp$year = i
    data_combine = rbind(tmp, data_combine)
  }
  
  
  data_combine_last = data_combine %>% filter(year == '2015')
  tmp = copy(data_combine_last)
  for (i in seq(2016,2020)) {
    tmp$year = i
    data_combine = rbind(data_combine, tmp)
  }
  
  
  data_combine = data_combine %>% arrange(year, age)
  write_csv(path = 'data/Germany/germany_fertility_m_all.csv', data_combine)
  
  # from WPP
  data = read_excel("data/WPP2019_FERT_F07_AGE_SPECIFIC_FERTILITY.xlsx")
  names(data) <- data[12,]
  data = data[which(data$`Region, subregion, country or area *` == "Germany"),]
  
  d_wpp <- select(data, Period, "15-19", "20-24", "25-29", "30-34", "35-39", "40-44",  "45-49")
  d_wpp_long <- gather(d_wpp, -Period, key = "age", value = "fertility_rate")
  d_wpp_long$country = "Germany"
  d_wpp_long$gender = "Female"
  
  d_wpp_long = d_wpp_long[d_wpp_long$Period %in% c("2000-2005", "2005-2010", "2010-2015", "2015-2020"),]
  
  d_wpp_long_rep = d_wpp_long %>% slice(rep(1:n(), each = 5))
  d_wpp_long_rep$date = rep(2000:2019, 7)
  d_wpp_long_rep = d_wpp_long_rep[d_wpp_long_rep$date > 2002,]
  
  d_wpp_long_rep$date = factor(d_wpp_long_rep$date)
  
  d_wpp_long_rep$fertility_rate = as.numeric(d_wpp_long_rep$fertility_rate)
  
  write_csv(path = 'data/Germany/germany_fertility_f.csv', d_wpp_long_rep)
}

# Iran
process_iran_fertility = function(){

  # from WPP
  data = read_excel("data/WPP2019_FERT_F07_AGE_SPECIFIC_FERTILITY.xlsx")
  names(data) <- data[12,]
  data = data[which(data$`Region, subregion, country or area *` == 'Iran (Islamic Republic of)'),]
  
  d_wpp <- select(data, Period, "15-19", "20-24", "25-29", "30-34", "35-39", "40-44",  "45-49")
  d_wpp_long <- gather(d_wpp, -Period, key = "age", value = "fertility_rate")
  d_wpp_long$country = 'Iran (Islamic Republic of)'
  d_wpp_long$gender = "Female"
  
  d_wpp_long = d_wpp_long[d_wpp_long$Period %in% c("2000-2005", "2005-2010", "2010-2015", "2015-2020"),]
  
  d_wpp_long_rep = d_wpp_long %>% slice(rep(1:n(), each = 5))
  d_wpp_long_rep$date = rep(2000:2019, 7)
  d_wpp_long_rep = d_wpp_long_rep[d_wpp_long_rep$date > 2002,]
  
  d_wpp_long_rep$date = factor(d_wpp_long_rep$date)
  
  d_wpp_long_rep$fertility_rate = as.numeric(d_wpp_long_rep$fertility_rate)

  write_csv(path = 'data/Iran/iran_fertility_f.csv', d_wpp_long_rep)
  
  iran <- select(d_wpp_long_rep, age, date, gender, fertility_rate)
  
  iran$age <- rep(c("20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54"), each = 17)
  iran = rbind(iran, data.frame(age = rep("15-19", 17),
                                date = c("2003", "2004","2005", "2006", "2007", "2008", "2009", 
                                         "2010", "2011", "2012", "2013", "2014", "2015", "2016", 
                                       "2017", "2018", "2019"),
                                   gender = rep("Male", 17),
                                   fertility_rate = rep(0, 17)))
  
  iran = rbind(iran, data.frame(age = rep("55+", 17),
                                date = c("2003", "2004","2005", "2006", "2007", "2008", "2009", 
                                         "2010", "2011", "2012", "2013", "2014", "2015", "2016", 
                                         "2017", "2018", "2019"),
                                gender = rep("Male", 17),
                                fertility_rate = rep(0, 17)))
  
  tmp <- iran[which(iran$date == "2019"),]
  tmp$date <- rep("2020", length(tmp$date))
  m_iran <- rbind(iran, tmp)
  m_iran$gender = "Male"
  
  write_csv(path = 'data/Iran/iran_fertility_m_all.csv', m_iran)
  
  ggplot() + 
    geom_point(data = d_wpp_long_rep %>% filter(date == 2015), aes(age, fertility_rate), col = "pink") + 
    geom_point(data = m_iran %>% filter(date == 2015), aes(age, fertility_rate), col = "blue")

}

# Italy
process_italy_fertility = function(){
  data_fertility = read.csv('data/fertility/unsd_live_births.csv')
  data_fertility = data_fertility %>% filter(country == 'Italy',
                                             age != 'Unknown - Inconnu',
                                             gender == 'Male')
  
  data_pop = readxl::read_xlsx('data/pop.xlsx', sheet = 2)
  countries = c('Italy')
  names(data_pop) = as.character(data_pop[1,])
  # data (thousand)
  data_pop = as.data.table(data_pop) %>% filter(Location %in% countries) %>% select(Location, Time, Age, Female, Male)
  setnames(data_pop, 1:ncol(data_pop), c('country', 'year', 'age', 'Female', 'Male'))
  data_pop =  reshape2::melt(data_pop, id.vars = c('country', 'year', 'age'), variable.name =  'gender', value.name = 'pop')
  data_pop = as.data.table(data_pop %>% filter(gender == 'Male'))  
  
  data_pop$age = as.character(data_pop$age)
  `%notin%` = Negate(`%in%`)
  data_pop = data_pop %>% filter(age %notin% c('0-4', '5-9', '10-14'))
  data_pop$age = ifelse(data_pop$age %in% c( '65-69', '70-74', '75-79', 
                                             '80-84', '85-89', '90-94', '95-99', '100+'), '65+', data_pop$age)
  
  data_pop$pop = as.numeric(data_pop$pop)
  data_pop = data_pop %>% group_by(age, year) %>% mutate(pop = sum(pop)) %>% ungroup() %>% distinct()
  
  ## population * 1000
  
  data_fertility = as.data.table(data_fertility)
  data_fertility$age = as.character(data_fertility$age)
  data_fertility$age = gsub(' - ', '-', data_fertility$age)
  data_fertility$age = gsub(' [+]', '+', data_fertility$age)
  
  data_fertility = data_fertility %>% filter(year != '2006')
  data_fertility$value = gsub('-', '0', data_fertility$value)
  # assume male are able to birth from 15 years old
  data_fertility$age = ifelse(data_fertility$age == '0-19', '15-19', data_fertility$age)
  data_fertility$year = as.character(data_fertility$year)
  data_fertility$value = as.numeric(as.character(data_fertility$value))
  data_fertility = as.data.table(data_fertility)
  data_combine= merge(data_fertility, data_pop, by = c('country','year', 'age', 'gender'))
  data_combine[,fertility_rate := value / (pop)]
  # live births per 1000 male
  write_csv(path = 'data/Italy/italy_fertility_m.csv', data_combine)
  
  data_combine = read.csv('data/Italy/italy_fertility_m.csv')
  data_combine$year = as.character(data_combine$year)
  ggplot(data_combine) +
    geom_point(aes(x = age, y = fertility_rate, color = year)) +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))
  
  data_combine_first = data_combine %>% filter(year == '2007')
  tmp = copy(data_combine_first)
  for (i in seq(2002,2006)) {
    tmp$year = i
    data_combine = rbind(tmp, data_combine)
  }
  data_combine_mid = data_combine %>% filter(year == '2011')
  data_combine_mid$year = '2012'
  data_combine = rbind(data_combine_mid, data_combine)
  data_combine_mid = data_combine %>% filter(year == '2014')
  data_combine_mid$year = '2015'
  data_combine = rbind(data_combine_mid, data_combine)
  
  data_combine_last = data_combine %>% filter(year == '2017')
  tmp = copy(data_combine_last)
  for (i in seq(2018,2020)) {
    tmp$year = i
    data_combine = rbind(data_combine, tmp)
  }
  
  data_combine = data_combine %>% arrange(year, age)
  write_csv(path = 'data/Italy/italy_fertility_m_all.csv', data_combine)
  
  # from WPP
  data = read_excel("data/WPP2019_FERT_F07_AGE_SPECIFIC_FERTILITY.xlsx")
  names(data) <- data[12,]
  data = data[which(data$`Region, subregion, country or area *` == 'Italy'),]
  
  d_wpp <- select(data, Period, "15-19", "20-24", "25-29", "30-34", "35-39", "40-44",  "45-49")
  d_wpp_long <- gather(d_wpp, -Period, key = "age", value = "fertility_rate")
  d_wpp_long$country = 'Italy'
  d_wpp_long$gender = "Female"
  
  d_wpp_long = d_wpp_long[d_wpp_long$Period %in% c("2000-2005", "2005-2010", "2010-2015", "2015-2020"),]
  
  d_wpp_long_rep = d_wpp_long %>% slice(rep(1:n(), each = 5))
  d_wpp_long_rep$date = rep(2000:2019, 7)
  d_wpp_long_rep = d_wpp_long_rep[d_wpp_long_rep$date > 2002,]

  d_wpp_long_rep$date = factor(d_wpp_long_rep$date)
  
  d_wpp_long_rep$fertility_rate = as.numeric(d_wpp_long_rep$fertility_rate)
  
  write_csv(path = 'data/Italy/italy_fertility_f.csv', d_wpp_long_rep)
}

# Mexico
process_mexico_fertility = function(){
  data_fertility = read.csv('data/fertility/unsd_live_births.csv')
  data_fertility = data_fertility %>% filter(country == 'Mexico',
                                             age != 'Unknown - Inconnu',
                                             gender == 'Male')
  
  data_pop = readxl::read_xlsx('data/fertility/pop.xlsx', sheet = 2)
  countries = c('Mexico')
  names(data_pop) = as.character(data_pop[1,])
  # data (thousand)
  data_pop = as.data.table(data_pop) %>% filter(Location %in% countries) %>% select(Location, Time, Age, Female, Male)
  setnames(data_pop, 1:ncol(data_pop), c('country', 'year', 'age', 'Female', 'Male'))
  data_pop =  reshape2::melt(data_pop, id.vars = c('country', 'year', 'age'), variable.name =  'gender', value.name = 'pop')
  data_pop = as.data.table(data_pop %>% filter(gender == 'Male'))  
  
  data_pop$age = as.character(data_pop$age)
  `%notin%` = Negate(`%in%`)
  data_pop = data_pop %>% filter(age %notin% c('0-4', '5-9', '10-14'))
  data_pop$age = ifelse(data_pop$age %in% c( '65-69', '70-74', '75-79', 
                                             '80-84', '85-89', '90-94', '95-99', '100+'), '65+', data_pop$age)
  
  
  data_pop$pop = as.numeric(data_pop$pop)
  data_pop = data_pop %>% group_by(age, year) %>% mutate(pop = sum(pop)) %>% ungroup() %>% distinct()
  ## population * 1000
  
  data_fertility = as.data.table(data_fertility)
  data_fertility$age = as.character(data_fertility$age)
  data_fertility$age = gsub(' - ', '-', data_fertility$age)
  data_fertility$age = gsub(' [+]', '+', data_fertility$age)
  
  data_fertility = data_fertility %>%filter(year != '2005', year!= '2006')
  # assume male are able to birth from 15 years old
  data_fertility$age = ifelse(data_fertility$age == '0-19', '15-19', data_fertility$age)
  data_fertility$year = as.character(data_fertility$year)
  data_fertility$value = as.numeric(as.character(data_fertility$value))
  data_fertility = as.data.table(data_fertility)
  data_combine= merge(data_fertility, data_pop, by = c('country','year', 'age', 'gender'))
  data_combine[,fertility_rate := value / (pop)]
  # live births per 1000 male
  write_csv(file = 'data/Mexico/mexico_fertility_m.csv', data_combine)
  
  data_combine = read.csv('data/Mexico/mexico_fertility_m.csv')
  data_combine$year = as.character(data_combine$year)
  ggplot(data_combine) +
    geom_point(aes(x = age, y = fertility_rate, color = year)) +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))
  
  
  data_combine_first = data_combine %>% filter(year == '2008')
  tmp = copy(data_combine_first)
  for (i in seq(2002,2007)) {
    tmp$year = i
    data_combine = rbind(tmp, data_combine)
  }
  data_combine_mid = data_combine %>% filter(year == '2011')
  data_combine_mid$year = '2012'
  data_combine = rbind(data_combine_mid, data_combine)
  
  data_combine_last = data_combine %>% filter(year == '2016')
  tmp = copy(data_combine_last)
  for (i in seq(2017,2020)) {
    tmp$year = i
    data_combine = rbind(data_combine, tmp)
  }
  
  data_combine = data_combine %>% arrange(year, age)
  write_csv(path = 'data/Mexico/mexico_fertility_m_all.csv', data_combine)
  
  # from WPP
  data = read_excel("data/WPP2019_FERT_F07_AGE_SPECIFIC_FERTILITY.xlsx")
  names(data) <- data[12,]
  data = data[which(data$`Region, subregion, country or area *` == 'Mexico'),]
  
  d_wpp <- select(data, Period, "15-19", "20-24", "25-29", "30-34", "35-39", "40-44",  "45-49")
  d_wpp_long <- gather(d_wpp, -Period, key = "age", value = "fertility_rate")
  d_wpp_long$country = 'Mexico'
  d_wpp_long$gender = "Female"
  
  d_wpp_long = d_wpp_long[d_wpp_long$Period %in% c("2000-2005", "2005-2010", "2010-2015", "2015-2020"),]
  
  d_wpp_long_rep = d_wpp_long %>% slice(rep(1:n(), each = 5))
  d_wpp_long_rep$date = rep(2000:2019, 7)
  d_wpp_long_rep = d_wpp_long_rep[d_wpp_long_rep$date > 2002,]

  d_wpp_long_rep$date = factor(d_wpp_long_rep$date)
  
  d_wpp_long_rep$fertility_rate = as.numeric(d_wpp_long_rep$fertility_rate)
  
  write_csv(path = 'data/Mexico/mexico_fertility_f.csv', d_wpp_long_rep)
}

process_philippines_fertility = function(){
  data_fertility = read.csv('data/fertility/unsd_live_births.csv')
  data_fertility = data_fertility %>% filter(country == 'Philippines',
                                             age != 'Unknown - Inconnu',
                                             gender == 'Male')
  
  data_pop = readxl::read_xlsx('data/pop.xlsx', sheet = 2)
  countries = c('Philippines')
  names(data_pop) = as.character(data_pop[1,])
  # data (thousand)
  data_pop = as.data.table(data_pop) %>% filter(Location %in% countries) %>% select(Location, Time, Age, Female, Male)
  setnames(data_pop, 1:ncol(data_pop), c('country', 'year', 'age', 'Female', 'Male'))
  data_pop =  reshape2::melt(data_pop, id.vars = c('country', 'year', 'age'), variable.name =  'gender', value.name = 'pop')
  data_pop = as.data.table(data_pop %>% filter(gender == 'Male'))  
  
  data_pop$age = as.character(data_pop$age)
  `%notin%` = Negate(`%in%`)
  data_pop = data_pop %>% filter(age %notin% c('0-4', '5-9', '10-14'))
  data_pop$age = ifelse(data_pop$age %in% c( '65-69', '70-74', '75-79', 
                                             '80-84', '85-89', '90-94', '95-99', '100+'), '65+', data_pop$age)
  
  data_pop$pop = as.numeric(data_pop$pop)
  data_pop = data_pop %>% group_by(age, year) %>% mutate(pop = sum(pop)) %>% ungroup() %>% distinct()
  ## population * 1000
  
  data_fertility = as.data.table(data_fertility)
  data_fertility$age = as.character(data_fertility$age)
  data_fertility$age = gsub(' - ', '-', data_fertility$age)
  data_fertility$age = gsub(' [+]', '+', data_fertility$age)
  
  # assume male are able to birth from 15 years old
  data_fertility$age = ifelse(data_fertility$age == '0-19', '15-19', data_fertility$age)
  data_fertility$year = as.character(data_fertility$year)
  data_fertility$value[data_fertility$value == "-"] <- 0
  data_fertility$value = as.numeric(data_fertility$value)
  data_combine= merge(data_fertility, data_pop, by = c('country','year', 'age', 'gender'))
  data_combine[,fertility_rate := value / (pop)]
  # live births per 1000 male
  write_csv(file = 'data/Philippines/philippines_fertility_m.csv', data_combine)
  
  data_combine = read.csv('data/Philippines/philippines_fertility_m.csv')
  data_combine$year = as.character(data_combine$year)
  ggplot(data_combine) +
    geom_point(aes(x = age, y = fertility_rate, color = year)) +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))
  
  data_combine_first = data_combine %>% filter(year == '2005')
  tmp = copy(data_combine_first)
  for (i in seq(2002,2004)) {
    tmp$year = i
    data_combine = rbind(tmp, data_combine)
  }
  
  data_combine_mid = data_combine %>% filter(year == '2007')
  tmp = data_combine_mid
  for (i in c(2006)) {
    tmp$year = i
    data_combine = rbind(tmp, data_combine)
  }
  
  data_combine_mid = data_combine %>% filter(year == '2009')
  tmp = data_combine_mid
  for (i in c(2008)) {
    tmp$year = i
    data_combine = rbind(tmp, data_combine)
  }
  
  data_combine_mid = data_combine %>% filter(year == '2012')
  tmp = data_combine_mid
  for (i in seq(2010,2011)) {
    tmp$year = i
    data_combine = rbind(tmp, data_combine)
  }
  
  data_combine_mid = data_combine %>% filter(year == '2014')
  tmp = data_combine_mid
  for (i in c(2013)) {
    tmp$year = i
    data_combine = rbind(tmp, data_combine)
  }
  
  data_combine_last = data_combine %>% filter(year == '2017')
  tmp = copy(data_combine_last)
  for (i in seq(2018,2020)) {
    tmp$year = i
    data_combine = rbind(data_combine, tmp)
  }
  
  
  data_combine = data_combine %>% arrange(year, age)
  write_csv(path = 'data/Philippines/philippines_fertility_m_all.csv', data_combine)
  
  # from WPP
  data = read_excel("data/WPP2019_FERT_F07_AGE_SPECIFIC_FERTILITY.xlsx")
  names(data) <- data[12,]
  data = data[which(data$`Region, subregion, country or area *` == "Philippines"),]
  
  d_wpp <- select(data, Period, "15-19", "20-24", "25-29", "30-34", "35-39", "40-44",  "45-49")
  d_wpp_long <- gather(d_wpp, -Period, key = "age", value = "fertility_rate")
  d_wpp_long$country = "Philippines"
  d_wpp_long$gender = "Female"
  
  d_wpp_long = d_wpp_long[d_wpp_long$Period %in% c("2000-2005", "2005-2010", "2010-2015", "2015-2020"),]
  
  d_wpp_long_rep = d_wpp_long %>% slice(rep(1:n(), each = 5))
  d_wpp_long_rep$date = rep(2000:2019, 7)
  d_wpp_long_rep = d_wpp_long_rep[d_wpp_long_rep$date > 2002,]
  
  d_wpp_long_rep$date = factor(d_wpp_long_rep$date)
  
  d_wpp_long_rep$fertility_rate = as.numeric(d_wpp_long_rep$fertility_rate)

  write_csv(path = 'data/Philippines/philippines_fertility_f.csv', d_wpp_long_rep)
}

# Poland
process_poland_fertility = function(){
  data_fertility = read.csv('data/fertility/unsd_live_births.csv')
  data_fertility = data_fertility %>% filter(country == 'Poland',
                                             age != 'Unknown - Inconnu',
                                             gender == 'Male')
  
  data_pop = readxl::read_xlsx('data/fertility/pop.xlsx', sheet = 2)
  countries = c('Poland')
  names(data_pop) = as.character(data_pop[1,])
  # data (thousand)
  data_pop = as.data.table(data_pop) %>% filter(Location %in% countries) %>% select(Location, Time, Age, Female, Male)
  setnames(data_pop, 1:ncol(data_pop), c('country', 'year', 'age', 'Female', 'Male'))
  data_pop =  reshape2::melt(data_pop, id.vars = c('country', 'year', 'age'), variable.name =  'gender', value.name = 'pop')
  data_pop = as.data.table(data_pop %>% filter(gender == 'Male'))  
  #data_pop = data_pop %>% filter(age %in% c('0-4', '5-9', '10-14', '15-19', '20-24', '25-29',
  #                                          '30-34', '35-39', '40-44', 
  #                                          '45-49', '50-54'))
  
  data_pop$age = as.character(data_pop$age)
  `%notin%` = Negate(`%in%`)
  data_pop = data_pop %>% filter(age %notin% c('0-4', '5-9', '10-14'))
  data_pop$pop = as.numeric(data_pop$pop)
  
  data_fertility = as.data.table(data_fertility)
  data_fertility$age = as.character(data_fertility$age)
  data_fertility$age = gsub(' - ', '-', data_fertility$age)
  data_fertility$age = gsub(' [+]', '+', data_fertility$age)
  
  # assume male are able to birth from 15 years old
  data_fertility$age = ifelse(data_fertility$age == '0-19', '15-19', data_fertility$age)
  data_fertility$year = as.character(data_fertility$year)
  data_fertility$value = as.numeric(as.character(data_fertility$value))
  
  data_pop$age = ifelse(data_pop$age %in% c( '50-54', '55-59', '60-64', '65-69', '70-74', '75-79', 
                                             '80-84', '85-89', '90-94', '95-99', '100+'), '50+', data_pop$age)
  data_pop = data_pop %>% group_by(age, year) %>% mutate(pop = sum(pop)) %>% ungroup() %>% distinct()
  
  # 2009-2012  60+ ===> 50+
  year_2009 = data_fertility %>% filter(year %in% c('2009', '2010', '2011', '2012'), age %in% c('50-54', '55-59', '60+'))
  year_2009$age = '50+'
  year_2009 = year_2009 %>% group_by(age, year) %>% mutate(value = sum(value)) %>% ungroup() %>% distinct()
  
  # 2015-2017
  year_2015 = data_fertility %>% filter(year %in% c('2015', '2016', '2017'), age %in% c('50-54', '55-59', '60-64', '65+'))
  year_2015$age = '50+'
  year_2015 = year_2015 %>% group_by(age, year) %>% mutate(value = sum(value)) %>% ungroup() %>% distinct()
  
  data_fertility = rbind(year_2009, year_2015, data_fertility)
  data_combine= merge(data_fertility, data_pop, by = c('country','year', 'age', 'gender'))
  data_combine = data.table(data_combine)
  data_combine[,fertility_rate := value / (pop)]
  
  # live births per 1000 male
  write_csv(path = 'data/Poland/poland_fertility_m.csv', data_combine)
  data_combine$year = as.character(data_combine$year)
  ggplot(data_combine) +
    geom_point(aes(x = age, y = fertility_rate, color = year)) +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))
  
  data_combine_first = data_combine %>% filter(year == '2007')
  tmp = copy(data_combine_first)
  for (i in seq(2002,2006)) {
    tmp$year = i
    data_combine = rbind(tmp, data_combine)
  }
  data_combine_mid = data_combine %>% filter(year == '2009')
  data_combine_mid$year = '2008'
  data_combine = rbind(data_combine_mid, data_combine)
  data_combine_mid = data_combine %>% filter(year == '2012')
  data_combine_mid$year = '2013'
  data_combine = rbind(data_combine_mid, data_combine)
  data_combine_mid = data_combine %>% filter(year == '2015')
  data_combine_mid$year = '2014'
  data_combine = rbind(data_combine_mid, data_combine)
  
  data_combine_last = data_combine %>% filter(year == '2017')
  tmp = copy(data_combine_last)
  for (i in seq(2018,2020)) {
    tmp$year = i
    data_combine = rbind(data_combine, tmp)
  }
  
  
  data_combine = data_combine %>% arrange(year, age)
  write_csv(path = 'data/Poland/poland_fertility_m_all.csv', data_combine)
  
  # from WPP
  data = read_excel("data/WPP2019_FERT_F07_AGE_SPECIFIC_FERTILITY.xlsx")
  names(data) <- data[12,]
  data = data[which(data$`Region, subregion, country or area *` == 'Poland'),]
  
  d_wpp <- select(data, Period, "15-19", "20-24", "25-29", "30-34", "35-39", "40-44",  "45-49")
  d_wpp_long <- gather(d_wpp, -Period, key = "age", value = "fertility_rate")
  d_wpp_long$country = 'Poland'
  d_wpp_long$gender = "Female"
  
  d_wpp_long = d_wpp_long[d_wpp_long$Period %in% c("2000-2005", "2005-2010", "2010-2015", "2015-2020"),]
  
  d_wpp_long_rep = d_wpp_long %>% slice(rep(1:n(), each = 5))
  d_wpp_long_rep$date = rep(2000:2019, 7)
  d_wpp_long_rep = d_wpp_long_rep[d_wpp_long_rep$date > 2002,]

  d_wpp_long_rep$date = factor(d_wpp_long_rep$date)
  
  d_wpp_long_rep$fertility_rate = as.numeric(d_wpp_long_rep$fertility_rate)
  
  write_csv(path = 'data/Poland/poland_fertility_f.csv', d_wpp_long_rep)
}

# Russia
process_russia_fertility = function(){
  data_fertility = read.csv('data/fertility/unsd_live_births.csv')
  data_fertility = data_fertility %>% filter(country == 'Russian Federation',
                                             age != 'Unknown - Inconnu',
                                             gender == 'Male')
  
  data_pop = readxl::read_xlsx('data/fertility/pop.xlsx', sheet = 2)
  countries = c('Russian Federation')
  names(data_pop) = as.character(data_pop[1,])
  # data (thousand)
  data_pop = as.data.table(data_pop) %>% filter(Location %in% countries) %>% select(Location, Time, Age, Female, Male)
  setnames(data_pop, 1:ncol(data_pop), c('country', 'year', 'age', 'Female', 'Male'))
  data_pop =  reshape2::melt(data_pop, id.vars = c('country', 'year', 'age'), variable.name =  'gender', value.name = 'pop')
  data_pop = as.data.table(data_pop %>% filter(gender == 'Male'))  
  
  data_pop$age = as.character(data_pop$age)
  `%notin%` = Negate(`%in%`)
  data_pop = data_pop %>% filter(age %notin% c('0-4', '5-9', '10-14'))
  data_pop$age = ifelse(data_pop$age %in% c('60-64', '65-69', '70-74', '75-79', 
                                            '80-84', '85-89', '90-94', '95-99', '100+'), '60+',
                        data_pop$age)
  data_pop$pop = as.numeric(data_pop$pop)
  data_pop = data_pop %>% group_by(age, year) %>% mutate(pop = sum(pop)) %>% ungroup() %>% distinct()
  ## population * 1000
  
  data_fertility = as.data.table(data_fertility)
  data_fertility$age = as.character(data_fertility$age)
  data_fertility$age = gsub(' - ', '-', data_fertility$age)
  data_fertility$age = gsub(' [+]', '+', data_fertility$age)
  
  # assume male are able to birth from 15 years old
  data_fertility$age = ifelse(data_fertility$age == '0-19', '15-19', data_fertility$age)
  data_fertility$year = as.character(data_fertility$year)
  data_fertility$value = as.numeric(as.character(data_fertility$value))
  data_combine= merge(data_fertility, data_pop, by = c('country','year', 'age', 'gender'))
  data_combine[,fertility_rate := value / (pop)]
  # live births per 1000 male
  write_csv(path = 'data/Russia/russia_fertility_m.csv', data_combine)
  
  data_combine = read.csv('data/Russia/russia_fertility_m.csv')
  data_combine$year = as.character(data_combine$year)
  ggplot(data_combine) +
    geom_point(aes(x = age, y = fertility_rate, color = year)) +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))
  
  data_combine_first = data_combine %>% filter(year == '2011')
  tmp = copy(data_combine_first)
  for (i in seq(2003,2010)) {
    tmp$year = i
    data_combine = rbind(tmp, data_combine)
  }
  data_combine_last = data_combine %>% filter(year == '2012')
  tmp = copy(data_combine_last)
  for (i in seq(2013,2020)) {
    tmp$year = i
    data_combine = rbind(data_combine, tmp)
  }
  
  
  data_combine = data_combine %>% arrange(year, age)
  write_csv(path = 'data/Russia/russia_fertility_m_all.csv', data_combine)
  
  # from WPP
  data = read_excel("data/WPP2019_FERT_F07_AGE_SPECIFIC_FERTILITY.xlsx")
  names(data) <- data[12,]
  data = data[which(data$`Region, subregion, country or area *` == 'Russian Federation'),]
  
  d_wpp <- select(data, Period, "15-19", "20-24", "25-29", "30-34", "35-39", "40-44",  "45-49")
  d_wpp_long <- gather(d_wpp, -Period, key = "age", value = "fertility_rate")
  d_wpp_long$country = 'Russian Federation'
  d_wpp_long$gender = "Female"
  
  d_wpp_long = d_wpp_long[d_wpp_long$Period %in% c("2000-2005", "2005-2010", "2010-2015", "2015-2020"),]
  
  d_wpp_long_rep = d_wpp_long %>% slice(rep(1:n(), each = 5))
  d_wpp_long_rep$date = rep(2000:2019, 7)
  d_wpp_long_rep = d_wpp_long_rep[d_wpp_long_rep$date > 2002,]

  d_wpp_long_rep$date = factor(d_wpp_long_rep$date)
  
  d_wpp_long_rep$fertility_rate = as.numeric(d_wpp_long_rep$fertility_rate)
  
  write_csv(path = 'data/Russia/russia_fertility_f.csv', d_wpp_long_rep)
}

# Spain
process_spain_fertility = function(){
  data_fertility = read.csv('data/fertility/unsd_live_births.csv')
  data_fertility = data_fertility %>% filter(country == 'Spain',
                                             age != 'Unknown - Inconnu',
                                             gender == 'Male')
  
  data_pop = readxl::read_xlsx('data/pop.xlsx', sheet = 2)
  countries = c('Spain')
  names(data_pop) = as.character(data_pop[1,])
  # data (thousand)
  data_pop = as.data.table(data_pop) %>% filter(Location %in% countries) %>% select(Location, Time, Age, Female, Male)
  setnames(data_pop, 1:ncol(data_pop), c('country', 'year', 'age', 'Female', 'Male'))
  data_pop =  reshape2::melt(data_pop, id.vars = c('country', 'year', 'age'), variable.name =  'gender', value.name = 'pop')
  data_pop = as.data.table(data_pop %>% filter(gender == 'Male'))  

  
  data_pop$age = as.character(data_pop$age)
  `%notin%` = Negate(`%in%`)
  data_pop = data_pop %>% filter(age %notin% c('0-4', '5-9', '10-14'))
  data_pop$age = ifelse(data_pop$age %in% c( '65-69', '70-74', '75-79', 
                                             '80-84', '85-89', '90-94', '95-99', '100+'), '65+', data_pop$age)
  
  
  data_pop$pop = as.numeric(data_pop$pop)
  data_pop = data_pop %>% group_by(age, year) %>% mutate(pop = sum(pop)) %>% ungroup() %>% distinct()
  ## population * 1000
  
  data_fertility = as.data.table(data_fertility)
  data_fertility$age = as.character(data_fertility$age)
  data_fertility$age = gsub(' - ', '-', data_fertility$age)
  data_fertility$age = gsub(' [+]', '+', data_fertility$age)
  
  # assume male are able to birth from 15 years old
  data_fertility$age = ifelse(data_fertility$age == '0-19', '15-19', data_fertility$age)
  data_fertility$year = as.character(data_fertility$year)
  data_fertility$value = as.numeric(as.character(data_fertility$value))
  data_combine= merge(data_fertility, data_pop, by = c('country','year', 'age', 'gender'))
  data_combine[,fertility_rate := value / (pop)]
  # live births per 1000 male
  write_csv(path = 'data/Spain/spain_fertility_m.csv', data_combine)
  
  data_combine = read.csv('data/Spain/spain_fertility_m.csv')
  data_combine$year = as.character(data_combine$year)
  ggplot(data_combine) +
    geom_point(aes(x = age, y = fertility_rate, color = year)) +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))
  
  data_combine_first = data_combine %>% filter(year == '2007')
  tmp = copy(data_combine_first)
  for (i in seq(2002,2006)) {
    tmp$year = i
    data_combine = rbind(tmp, data_combine)
  }
  data_combine_mid = data_combine %>% filter(year == '2009')
  data_combine_mid$year = '2008'
  data_combine = rbind(data_combine_mid, data_combine)
  data_combine_mid = data_combine %>% filter(year == '2013')
  data_combine_mid$year = '2014'
  data_combine = rbind(data_combine_mid, data_combine)
  data_combine_mid = data_combine %>% filter(year == '2016')
  data_combine_mid$year = '2015'
  data_combine = rbind(data_combine_mid, data_combine)
  
  data_combine_last = data_combine %>% filter(year == '2017')
  tmp = copy(data_combine_last)
  for (i in seq(2018,2020)) {
    tmp$year = i
    data_combine = rbind(data_combine, tmp)
  }
  
  data_combine = data_combine %>% arrange(year, age)
  write_csv(path = 'data/Spain/spain_fertility_m_all.csv', data_combine)

  # from WPP
  data = read_excel("data/WPP2019_FERT_F07_AGE_SPECIFIC_FERTILITY.xlsx")
  names(data) <- data[12,]
  data = data[which(data$`Region, subregion, country or area *` == 'Spain'),]
  
  d_wpp <- select(data, Period, "15-19", "20-24", "25-29", "30-34", "35-39", "40-44",  "45-49")
  d_wpp_long <- gather(d_wpp, -Period, key = "age", value = "fertility_rate")
  d_wpp_long$country = 'Spain'
  d_wpp_long$gender = "Female"
  
  d_wpp_long = d_wpp_long[d_wpp_long$Period %in% c("2000-2005", "2005-2010", "2010-2015", "2015-2020"),]
  
  d_wpp_long_rep = d_wpp_long %>% slice(rep(1:n(), each = 5))
  d_wpp_long_rep$date = rep(2000:2019, 7)
  d_wpp_long_rep = d_wpp_long_rep[d_wpp_long_rep$date > 2002,]

  d_wpp_long_rep$date = factor(d_wpp_long_rep$date)
  
  d_wpp_long_rep$fertility_rate = as.numeric(d_wpp_long_rep$fertility_rate)

  write_csv(path = 'data/Spain/spain_fertility_f.csv', d_wpp_long_rep)
}

# USA
process_usa_fertility = function(){
  data_fertility = read.csv('data/fertility/unsd_live_births.csv')
  data_fertility = data_fertility %>% filter(country == 'United States of America',
                                             age != 'Unknown - Inconnu',
                                             gender == 'Male')

  data_pop = readxl::read_xlsx('data/pop.xlsx', sheet = 2)
  countries = c('United States of America')
  names(data_pop) = as.character(data_pop[1,])
  # data (thousand)
  data_pop = as.data.table(data_pop) %>% filter(Location %in% countries) %>% select(Location, Time, Age, Female, Male)
  setnames(data_pop, 1:ncol(data_pop), c('country', 'year', 'age', 'Female', 'Male'))
  data_pop =  reshape2::melt(data_pop, id.vars = c('country', 'year', 'age'), variable.name =  'gender', value.name = 'pop')
  data_pop = as.data.table(data_pop %>% filter(gender == 'Male'))  
  #data_pop = data_pop %>% filter(age %in% c('0-4', '5-9', '10-14', '15-19', '20-24', '25-29',
  #                                          '30-34', '35-39', '40-44', 
  #                                          '45-49', '50-54'))
  
  data_pop$age = as.character(data_pop$age)
  `%notin%` = Negate(`%in%`)
  data_pop = data_pop %>% filter(age %notin% c('0-4', '5-9', '10-14'))
  data_pop$age = ifelse(data_pop$age %in% c( '15-19'), '15-19',
                        ifelse(data_pop$age %in% c('55-59','60-64', '65-69', '70-74', '75-79', 
                                                   '80-84', '85-89', '90-94', '95-99', '100+'), '55+',
                               data_pop$age))
  data_pop$pop = as.numeric(data_pop$pop)
  data_pop = data_pop %>% group_by(age, year) %>% mutate(pop = sum(pop)) %>% ungroup() %>% distinct()
  ## population * 1000
  
  data_fertility = as.data.table(data_fertility)
  data_fertility$age = as.character(data_fertility$age)
  data_fertility$age = gsub(' - ', '-', data_fertility$age)
  data_fertility$age = gsub(' [+]', '+', data_fertility$age)
  # delete the data in year 2006, because the age groups are not the same
  data_fertility = data_fertility %>% filter(year != '2006')
  # assume male are able to birth from 15 years old
  data_fertility$age = ifelse(data_fertility$age == '0-19', '15-19', data_fertility$age)
  data_fertility$year = as.character(data_fertility$year)
  data_fertility$value = as.numeric(as.character(data_fertility$value))
  data_fertility = as.data.table(data_fertility)
  data_fertility = rbind(data_fertility, data.table(country = 'United States of America', year = '2009' ,age = '55+', value = 0,gender = 'Male'))
  data_fertility = data_fertility %>% arrange(year, age)
  data_fertility = as.data.table(data_fertility)
  data_combine= merge(data_fertility, data_pop, by = c('country','year', 'age', 'gender'))
  data_combine[,fertility_rate := value / (pop)]
  # live births per 1000 male
  write_csv(path = 'data/USA/usa_fertility_m.csv', data_combine)
  
  data_combine = read.csv('data/USA/usa_fertility_m.csv')
  data_combine$year = as.character(data_combine$year)
  ggplot(data_combine) +
    geom_point(aes(x = age, y = fertility_rate, color = year)) +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))
  
  data_combine_first = data_combine %>% filter(year == '2008')
  tmp = copy(data_combine_first)
  for (i in seq(2003,2007)) {
    tmp$year = i
    data_combine = rbind(tmp, data_combine)
  }
  data_combine_mid = data_combine %>% filter(year == '2009')
  data_combine_mid$year = '2010'
  data_combine = rbind(data_combine_mid, data_combine)
  data_combine_mid = data_combine %>% filter(year == '2012')
  data_combine_mid$year = '2011'
  data_combine = rbind(data_combine_mid, data_combine)
  
  
  
  data_combine_last = data_combine %>% filter(year == '2015')
  tmp = copy(data_combine_last)
  for (i in seq(2016,2020)) {
    tmp$year = i
    data_combine = rbind(data_combine, tmp)
  }
  
  
  data_combine = data_combine %>% arrange(year, age)
  write_csv(path = 'data/USA/usa_fertility_m_all.csv', data_combine)
  

  # from WPP
  data = read_excel("data/WPP2019_FERT_F07_AGE_SPECIFIC_FERTILITY.xlsx")
  names(data) <- data[12,]
  data = data[which(data$`Region, subregion, country or area *` == 'United States of America'),]
  
  d_wpp <- select(data, Period, "15-19", "20-24", "25-29", "30-34", "35-39", "40-44",  "45-49")
  d_wpp_long <- gather(d_wpp, -Period, key = "age", value = "fertility_rate")
  d_wpp_long$country = 'United States of America'
  d_wpp_long$gender = "Female"
  
  d_wpp_long = d_wpp_long[d_wpp_long$Period %in% c("2000-2005", "2005-2010", "2010-2015", "2015-2020"),]
  
  d_wpp_long_rep = d_wpp_long %>% slice(rep(1:n(), each = 5))
  d_wpp_long_rep$date = rep(2000:2019, 7)
  d_wpp_long_rep = d_wpp_long_rep[d_wpp_long_rep$date > 2002,]
  
  d_wpp_long_rep$date = factor(d_wpp_long_rep$date)
  
  d_wpp_long_rep$fertility_rate = as.numeric(d_wpp_long_rep$fertility_rate)
  
  
  write_csv(path = 'data/USA/usa_fertility_f.csv', d_wpp_long_rep)
}
