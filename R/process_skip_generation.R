# Code to process skip generation proportions
library(readxl)

# IFRS: 
ifrs <- data.frame(age = c("60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-94", "95-99", "100+"),
                  ifr = c(0.89, 1.39, 2.17, 3.39, 5.3, 8.28, 16.19, 16.19, 16.19))

pop_data_m <- read_xlsx("data/WPP2019_POP_F07_2_POPULATION_BY_AGE_MALE.xlsx")
names(pop_data_m) <- pop_data_m[12,]
pop_data_m <- pop_data_m[13:3837, c(3, 8:29)]
pop_data_m  <- pop_data_m[pop_data_m$`Reference date (as of 1 July)` == "2020",]

pop_data_f <- read_xlsx("data/WPP2019_POP_F07_3_POPULATION_BY_AGE_FEMALE.xlsx")
names(pop_data_f) <- pop_data_f[12,]
pop_data_f <- pop_data_f[13:3837, c(3, 8:29)]
pop_data_f  <- pop_data_f[pop_data_f$`Reference date (as of 1 July)` == "2020",]

# Argentina
process_argentina_skip_generation = function(){
  d_summary = read.csv('data/Argentina/covid19_deaths.csv')
  d_summary$age = as.character(d_summary$age)
  data = d_summary %>% filter(age == '60+') %>% group_by(gender) %>% mutate(grand_deaths = sum(deaths
  )) %>% ungroup() %>%
    select(age, gender, grand_deaths) %>% distinct()
  
  pop_m <- pop_data_m[which(pop_data_m$`Region, subregion, country or area *` == "Argentina"),]
  pop_m <- gather(pop_m[, 15:23], key = age, value = pop)
  pop_m <- left_join(pop_m, ifrs)
  pop_m$weight <- as.numeric(pop_m$pop)*pop_m$ifr
  percent_60_85_m <- sum(pop_m$weight[pop_m$age %in% c("60-64", "65-69", "70-74", "75-79", "80-84")])/
    sum(pop_m$weight)
  
  pop_f <- pop_data_f[which(pop_data_f$`Region, subregion, country or area *` == "Argentina"),]
  pop_f <- gather(pop_f[, 15:23], key = age, value = pop)
  pop_f <- left_join(pop_f, ifrs)
  pop_f$weight <- as.numeric(pop_f$pop)*pop_f$ifr
  percent_60_85_f <- sum(pop_f$weight[pop_f$age %in% c("60-64", "65-69", "70-74", "75-79", "80-84")])/
    sum(pop_f$weight)
  
  data$grand_deaths[data$gender == "Male"] = data$grand_deaths[data$gender == "Male"] * percent_60_85_m
  data$grand_deaths[data$gender == "Female"] = data$grand_deaths[data$gender == "Female"] * percent_60_85_f
  
  data = data[order(data$gender),]
  skip_gen = read_xlsx("data/skip_gen.xlsx")
  skip_gen = skip_gen[which(skip_gen == "Argentina"),]
  data$skip_generation = c(skip_gen$sg_female, skip_gen$sg_male)*100
  data$value = data$grand_deaths * data$skip_generation/100
  data$'older persons co-residing' = c(skip_gen$mg_female, skip_gen$mg_male)*100
  data$number = data$`older persons co-residing` * data$grand_deaths/100 * 0.89
  data$number = round(data$number)
  data$grand_deaths = round(data$grand_deaths)
  data$value = round(data$value)
  
  write_csv(path = 'data/Argentina/skip_generation_argentina.csv', data)
  print(data)
}

# Brazil
process_brazil_skip_generation = function(){
  d_summary = read.csv('data/Brazil/covid19_deaths.csv')
  setnames(d_summary, c('COVID19_deaths'), c('deaths'))
  d_summary$age = as.character(d_summary$age)
  d_summary$age = ifelse(d_summary$age %in% c("60-65","65-69","65-70","70-75","75-80","80-85"),'60+', 'others')
  # 30.2% male 28.9% female
  data = d_summary %>% filter(age == '60+') %>% group_by(gender) %>% mutate(grand_deaths = sum(deaths
  )) %>% ungroup() %>%
    select(age, gender, grand_deaths) %>% distinct()
  data = data %>% arrange(gender)
  skip_gen = read_xlsx("data/skip_gen.xlsx")
  skip_gen = skip_gen[which(skip_gen == "Brazil"),]
  data$skip_generation = c(skip_gen$sg_female, skip_gen$sg_male)*100
  data$value = data$grand_deaths * data$skip_generation/100
  data$'older persons co-residing' = c(skip_gen$mg_female, skip_gen$mg_male)*100
  data$number = data$`older persons co-residing` * data$grand_deaths/100 * 0.89
  data$number = round(data$number)
  data$grand_deaths = round(data$grand_deaths)
  data$value = round(data$value)
  write_csv(path = 'data/Brazil/skip_generation_brazil.csv', data)
  print(data)
}

# Colombia
process_colombia_skip_generation = function(){
  d_summary = read.csv('data/Colombia/covid19_deaths_all.csv')
  d_summary$age = as.character(d_summary$age)
  
  pop_m <- pop_data_m[which(pop_data_m$`Region, subregion, country or area *` == "Colombia"),]
  pop_m <- gather(pop_m[, 15:23], key = age, value = pop)
  pop_m <- left_join(pop_m, ifrs)
  pop_m$weight <- as.numeric(pop_m$pop)*pop_m$ifr
  pop_m <- pop_m[5:9,]
  percent_80_85_m <- sum(pop_m$weight[pop_m$age %in% c("80-84")])/
    sum(pop_m$weight)
  
  pop_f <- pop_data_f[which(pop_data_f$`Region, subregion, country or area *` == "Colombia"),]
  pop_f <- gather(pop_f[, 15:23], key = age, value = pop)
  pop_f <- left_join(pop_f, ifrs)
  pop_f <- pop_f[5:9,]
  pop_f$weight <- as.numeric(pop_f$pop)*pop_f$ifr
  percent_80_85_f <- sum(pop_f$weight[pop_f$age %in% c("80-84")])/
    sum(pop_f$weight)
  
  d_summary$deaths[d_summary$gender == "male" & d_summary$age == "80+"] = 
    d_summary$deaths[d_summary$gender == "male" & d_summary$age == "80+"] * percent_80_85_m
  d_summary$deaths[d_summary$gender == "female" & d_summary$age == "80+"] = 
    d_summary$deaths[d_summary$gender == "female" & d_summary$age == "80+"] * percent_80_85_f
  
  d_summary$age = ifelse(d_summary$age %in% c("60-64", "65-69","70-74","75-79","80+"),'60+', 'others')
  # 4.69% male 6% female
  data = d_summary %>% filter(age == '60+') %>% group_by(gender) %>% mutate(grand_deaths = sum(deaths
  )) %>% ungroup() %>%
    select(age, gender, grand_deaths) %>% distinct()
  skip_gen = read_xlsx("data/skip_gen.xlsx")
  skip_gen = skip_gen[which(skip_gen == "Colombia"),]
  data$skip_generation = c(skip_gen$sg_female, skip_gen$sg_male)*100
  data$value = data$grand_deaths * data$skip_generation/100
  data$'older persons co-residing' = c(skip_gen$mg_female, skip_gen$mg_male)*100
  data$number = data$`older persons co-residing` * data$grand_deaths/100 * 0.89
  data$number = round(data$number)
  data$grand_deaths = round(data$grand_deaths)
  data$value = round(data$value)
  
  write_csv(path = 'data/Colombia/skip_generation_colombia.csv', data)
  print(data)
}

# England and Wales
process_skip_generation_england_wales = function(max_excess_covid = 0){
  data = read.csv('data/UK/deaths_all_england_wales.csv')
  data$week = as.numeric(sapply(as.character(data$week), function(x) strsplit(x, " ")[[1]][2]))
  data = data[which((data$year.x == "2020" &  data$week >=10) | 
                              (data$year.x == "2021" &  data$week <= 8)),]
  # Summarise data for age groups
  data = data %>% group_by(age, gender) %>%
    summarise(covid = sum(covid19_deaths),
              excess = sum(excess_deaths))
  data$deaths = ifelse(data$excess > data$covid, data$excess, data$covid)
  data$age = as.character(data$age)
  data$age = ifelse(data$age  %in% c('60-64', '65-69', '70-74', '75-79',
                                     '80-84'), '60+', data$age)

  data = data %>% filter(age == '60+') %>% 
    group_by(gender) %>% 
    mutate(grand_deaths = sum(deaths)) %>% 
    ungroup() %>%
    select(age, gender, grand_deaths) %>% 
    distinct()

  skip_gen = read_xlsx("data/skip_gen.xlsx")
  skip_gen = skip_gen[which(skip_gen == "England"),]
  data$skip_generation = c(skip_gen$sg_female, skip_gen$sg_male)*100
  data$value = data$grand_deaths * data$skip_generation/100
  data$'older persons co-residing' = c(skip_gen$mg_female, skip_gen$mg_male)*100
  data$number = data$`older persons co-residing` * data$grand_deaths/100 * 0.89
  data$number = round(data$number)
  data$grand_deaths = round(data$grand_deaths)
  data$value = round(data$value)
  setnames(data, 'grand_deaths', '60+ deaths')
  write_csv(path = 'data/UK/skip_generation_england_wales.csv', data)

  print(data)
}

# France
process_france_skip_generation = function(){
  d_summary = read.csv('data/France/france_all.csv')
  d_summary$age = as.character(d_summary$age)
  
  pop_m <- pop_data_m[which(pop_data_m$`Region, subregion, country or area *` == "France"),]
  pop_m <- gather(pop_m[, 15:23], key = age, value = pop)
  pop_m <- left_join(pop_m, ifrs)
  pop_m$weight <- as.numeric(pop_m$pop)*pop_m$ifr
  pop_m <- pop_m[5:6,]
  percent_80_85_m <- sum(pop_m$weight[pop_m$age %in% c("80-84")])/
    sum(pop_m$weight)
  
  pop_f <- pop_data_f[which(pop_data_f$`Region, subregion, country or area *` == "France"),]
  pop_f <- gather(pop_f[, 15:23], key = age, value = pop)
  pop_f <- left_join(pop_f, ifrs)
  pop_f$weight <- as.numeric(pop_f$pop)*pop_f$ifr
  pop_f <- pop_f[5:6,]
  percent_80_85_f <- sum(pop_f$weight[pop_f$age %in% c("80-84")])/
    sum(pop_f$weight)
  
  d_summary$deaths[d_summary$gender == "Male" & d_summary$age == "80-89"] = 
    d_summary$deaths[d_summary$gender == "Male" & d_summary$age == "80-89"] * percent_80_85_m
  d_summary$deaths[d_summary$gender == "Female" & d_summary$age == "80-89"] = 
    d_summary$deaths[d_summary$gender == "Female" & d_summary$age == "80-89"] * percent_80_85_f
  
  d_summary$age = ifelse(d_summary$age %in% c("60-69","70-79","80-89"),'60+', 'others')
  #% female % male 
  data = d_summary %>% filter(age == '60+') %>% 
    group_by(gender) %>% 
    mutate(grand_deaths = sum(deaths)) %>% 
    ungroup() %>%
    select(age, gender, grand_deaths) %>% 
    distinct()
  data = data[order(data$gender),]
  skip_gen = read_xlsx("data/skip_gen.xlsx")
  skip_gen_uk = skip_gen[which(skip_gen == "England"),]
  data$skip_generation = c(skip_gen_uk$sg_female, skip_gen_uk$sg_male)*100
  data$value = data$grand_deaths * data$skip_generation/100
  skip_gen_fra = skip_gen[which(skip_gen == "France"),]
  data$'older persons co-residing' = c(skip_gen_fra$mg_female, skip_gen_fra$mg_male)*100
  data$number = data$`older persons co-residing` * data$grand_deaths/100 * 0.89
  data$number = round(data$number)
  data$grand_deaths = round(data$grand_deaths)
  data$value = round(data$value)
  write_csv(path = paste0('data/France/skip_generation_france.csv'), data)
  print(data)
}

# Germany
process_germany_skip_generation = function(){
  country = 'germany'
  d_summary = read.csv('data/Germany/covid19_deaths.csv')
  #setnames(d_summary, c('daily_covid19'), c('deaths'))
  d_summary = d_summary %>% group_by(age, gender) %>% mutate(nb_deaths = sum(deaths)) %>% ungroup() %>% 
    select(age, gender, nb_deaths) %>% distinct()
  setnames(d_summary, 'nb_deaths', 'deaths')
  d_summary$age = as.character(d_summary$age)
  
  pop_m <- pop_data_m[which(pop_data_m$`Region, subregion, country or area *` == "Germany"),]
  pop_m <- gather(pop_m[, 15:23], key = age, value = pop)
  pop_m <- left_join(pop_m, ifrs)
  pop_m$weight <- as.numeric(pop_m$pop)*pop_m$ifr
  pop_m <- pop_m[5:6,]
  percent_80_85_m <- sum(pop_m$weight[pop_m$age %in% c("80-84")])/
    sum(pop_m$weight)
  
  pop_f <- pop_data_f[which(pop_data_f$`Region, subregion, country or area *` == "Germany"),]
  pop_f <- gather(pop_f[, 15:23], key = age, value = pop)
  pop_f <- left_join(pop_f, ifrs)
  pop_f$weight <- as.numeric(pop_f$pop)*pop_f$ifr
  pop_f <- pop_f[5:6,]
  percent_80_85_f <- sum(pop_f$weight[pop_f$age %in% c("80-84")])/
    sum(pop_f$weight)
  
  d_summary$deaths[d_summary$gender == "Male" & d_summary$age == "80-89"] = 
    d_summary$deaths[d_summary$gender == "Male" & d_summary$age == "80-89"] * percent_80_85_m
  d_summary$deaths[d_summary$gender == "Female" & d_summary$age == "80-89"] = 
    d_summary$deaths[d_summary$gender == "Female" & d_summary$age == "80-89"] * percent_80_85_f
  
  d_summary$age = ifelse(d_summary$age %in% c("60-69","70-79","80-89"),'60+', 'others')
  #% female % male 
  data = d_summary %>% filter(age == '60+') %>% group_by(gender) %>% mutate(grand_deaths = sum(deaths
  )) %>% ungroup() %>%
    select(age, gender, grand_deaths) %>% distinct()
  data <- data[order(data$gender),]
  skip_gen = read_xlsx("data/skip_gen.xlsx")
  skip_gen = skip_gen[which(skip_gen == "England"),]
  data$skip_generation = c(skip_gen$sg_female, skip_gen$sg_male)*100
  data$value = data$grand_deaths * data$skip_generation/100
  data$'older persons co-residing' = c(skip_gen$mg_female, skip_gen$mg_male)*100
  data$number = data$`older persons co-residing` * data$grand_deaths/100 * 0.89
  data$number = round(data$number)
  data$grand_deaths = round(data$grand_deaths)
  data$value = round(data$value)
  write_csv(path = paste0('data/Germany/skip_generation_', country,'.csv'), data)
  print(data)
}

# India
process_india_skip_generation = function(){
  country = 'india'
  d_summary = read.csv('data/India/all_covid_deaths.csv')
  
  pop_m <- pop_data_m[which(pop_data_m$`Region, subregion, country or area *` == "India"),]
  pop_m <- gather(pop_m[, 15:23], key = age, value = pop)
  pop_m <- left_join(pop_m, ifrs)
  pop_m$weight <- as.numeric(pop_m$pop)*pop_m$ifr
  pop_m <- pop_m[5:9,]
  percent_80_85_m <- sum(pop_m$weight[pop_m$age %in% c("80-84")])/
    sum(pop_m$weight)
  
  pop_f <- pop_data_f[which(pop_data_f$`Region, subregion, country or area *` == "India"),]
  pop_f <- gather(pop_f[, 15:23], key = age, value = pop)
  pop_f <- left_join(pop_f, ifrs)
  pop_f <- pop_f[5:9,]
  pop_f$weight <- as.numeric(pop_f$pop)*pop_f$ifr
  percent_80_85_f <- sum(pop_f$weight[pop_f$age %in% c("80-84")])/
    sum(pop_f$weight)
  
  d_summary$deaths[d_summary$sex == "Male" & d_summary$age == "80+"] = 
    d_summary$deaths[d_summary$sex == "Male" & d_summary$age == "80+"] * percent_80_85_m
  d_summary$deaths[d_summary$sex == "Female" & d_summary$age == "80+"] = 
    d_summary$deaths[d_summary$sex == "Female" & d_summary$age == "80+"] * percent_80_85_f
  
  d_summary$age = ifelse(d_summary$age %in% c("60-69", "70-79", "80+"),'60+', 'others')
  #% female % male 
  data = d_summary %>% filter(age == '60+') %>% group_by(sex) %>% 
    mutate(grand_deaths = sum(deaths)) %>% 
    ungroup() %>%
    select(age, sex, grand_deaths) %>% 
    distinct()
  skip_gen = read_xlsx("data/skip_gen.xlsx")
  skip_gen = skip_gen[which(skip_gen == "India"),]
  data$skip_generation = c(skip_gen$sg_female, skip_gen$sg_male)*100
  data$value = data$grand_deaths * data$skip_generation/100
  data$'older persons co-residing' = c(skip_gen$mg_female, skip_gen$mg_male)*100
  data$number = data$`older persons co-residing` * data$grand_deaths/100 * 0.89
  data$number = round(data$number)
  data$grand_deaths = round(data$grand_deaths)
  data$value = round(data$value)
  write_csv(path = paste0('data/India/skip_generation_', country,'.csv'), data)
  print(data)
}

# Iran
process_iran_skip_generation = function(){
  country = 'iran'
  d_summary = read.csv('data/Iran/iran_all.csv')
  d_summary$age = as.character(d_summary$age)
  
  pop_m <- pop_data_m[which(pop_data_m$`Region, subregion, country or area *` == "Iran (Islamic Republic of)"),]
  pop_m <- gather(pop_m[, 15:23], key = age, value = pop)
  pop_m <- left_join(pop_m, ifrs)
  pop_m$weight <- as.numeric(pop_m$pop)*pop_m$ifr
  pop_m <- pop_m[5:9,]
  percent_80_85_m <- sum(pop_m$weight[pop_m$age %in% c("80-84")])/
    sum(pop_m$weight)
  
  pop_f <- pop_data_f[which(pop_data_f$`Region, subregion, country or area *` == "Iran (Islamic Republic of)"),]
  pop_f <- gather(pop_f[, 15:23], key = age, value = pop)
  pop_f <- left_join(pop_f, ifrs)
  pop_f$weight <- as.numeric(pop_f$pop)*pop_f$ifr
  pop_f <- pop_f[5:9,]
  percent_80_85_f <- sum(pop_f$weight[pop_f$age %in% c("80-84")])/
    sum(pop_f$weight)
  
  d_summary$deaths[d_summary$gender == "Male" & d_summary$age == "80+"] = 
    d_summary$deaths[d_summary$gender == "Male" & d_summary$age == "80+"] * percent_80_85_m
  d_summary$deaths[d_summary$gender == "Female" & d_summary$age == "80+"] = 
    d_summary$deaths[d_summary$gender == "Female" & d_summary$age == "80+"] * percent_80_85_f
  
  d_summary$age = ifelse(d_summary$age %in% c("60-69","70-79","80+"),'60+', 'others')
  data = d_summary %>% filter(age == '60+') %>% group_by(gender) %>% mutate(grand_deaths = sum(deaths
  )) %>% ungroup() %>%
    select(age, gender, grand_deaths) %>% distinct()
  data = data[order(data$gender),]
  skip_gen = read_xlsx("data/skip_gen.xlsx")
  skip_gen = skip_gen[which(skip_gen == "Iran"),]
  data$skip_generation = c(skip_gen$sg_female, skip_gen$sg_male)*100
  data$value = data$grand_deaths * data$skip_generation/100
  data$'older persons co-residing' = c(skip_gen$mg_female, skip_gen$mg_male)*100
  data$number = data$`older persons co-residing` * data$grand_deaths/100 * 0.89
  data$number = round(data$number)
  data$grand_deaths = round(data$grand_deaths)
  data$value = round(data$value)
  write_csv(path = paste0('data/Iran/skip_generation_', country,'.csv'), data)
  print(data)
}

# Italy
process_italy_skip_generation = function(){
  country = 'italy'
  d_summary = read.csv('data/Italy/italy_all.csv')
  d_summary$age = as.character(d_summary$age)
  
  pop_m <- pop_data_m[which(pop_data_m$`Region, subregion, country or area *` == "Italy"),]
  pop_m <- gather(pop_m[, 15:23], key = age, value = pop)
  pop_m <- left_join(pop_m, ifrs)
  pop_m$weight <- as.numeric(pop_m$pop)*pop_m$ifr
  pop_m <- pop_m[5:6,]
  percent_80_85_m <- sum(pop_m$weight[pop_m$age %in% c("80-84")])/
    sum(pop_m$weight)
  
  pop_f <- pop_data_f[which(pop_data_f$`Region, subregion, country or area *` == "Italy"),]
  pop_f <- gather(pop_f[, 15:23], key = age, value = pop)
  pop_f <- left_join(pop_f, ifrs)
  pop_f$weight <- as.numeric(pop_f$pop)*pop_f$ifr
  pop_f <- pop_f[5:6,]
  percent_80_85_f <- sum(pop_f$weight[pop_f$age %in% c("80-84")])/
    sum(pop_f$weight)
  
  d_summary$deaths[d_summary$gender == "Male" & d_summary$age == "80-89"] = 
    d_summary$deaths[d_summary$gender == "Male" & d_summary$age == "80-89"] * percent_80_85_m
  d_summary$deaths[d_summary$gender == "Female" & d_summary$age == "80-89"] = 
    d_summary$deaths[d_summary$gender == "Female" & d_summary$age == "80-89"] * percent_80_85_f
  
  d_summary$age = ifelse(d_summary$age %in% c("60-69","70-79","80-89"),'60+', 'others')
  data = d_summary %>% filter(age == '60+') %>% group_by(gender) %>% mutate(grand_deaths = sum(deaths
  )) %>% ungroup() %>%
    select(age, gender, grand_deaths) %>% distinct()
  data = data[order(data$gender),]
  skip_gen = read_xlsx("data/skip_gen.xlsx")
  skip_gen = skip_gen[which(skip_gen == "Italy"),]
  data$skip_generation = c(skip_gen$sg_female, skip_gen$sg_male)*100
  data$value = data$grand_deaths * data$skip_generation/100
  data$'older persons co-residing' = c(skip_gen$mg_female, skip_gen$mg_male)*100
  data$number = data$`older persons co-residing` * data$grand_deaths/100 * 0.89
  data$number = round(data$number)
  data$grand_deaths = round(data$grand_deaths)
  data$value = round(data$value)
  write_csv(path = paste0('data/fertility/skip_generation_', country,'.csv'), data)
  print(data)
}

# Kenya
process_kenya_skip_generation = function(){
  country = 'kenya'
  d_summary = read.csv('data/Kenya/covid19_deaths.csv')
  
  pop_m <- pop_data_m[which(pop_data_m$`Region, subregion, country or area *` == "Kenya"),]
  pop_m <- gather(pop_m[, 15:23], key = age, value = pop)
  pop_m <- left_join(pop_m, ifrs)
  pop_m$weight <- as.numeric(pop_m$pop)*pop_m$ifr
  percent_60_85_m <- sum(pop_m$weight[pop_m$age %in% c("60-64", "65-69", "70-74", "75-79", "80-84")])/
    sum(pop_m$weight)
  
  pop_f <- pop_data_f[which(pop_data_f$`Region, subregion, country or area *` == "Kenya"),]
  pop_f <- gather(pop_f[, 15:23], key = age, value = pop)
  pop_f <- left_join(pop_f, ifrs)
  pop_f$weight <- as.numeric(pop_f$pop)*pop_f$ifr
  percent_60_85_f <- sum(pop_f$weight[pop_f$age %in% c("60-64", "65-69", "70-74", "75-79", "80-84")])/
    sum(pop_f$weight)
  
  data = d_summary %>% filter(age == '60+') %>% select(age, gender, deaths)
  data$deaths[data$gender == "Male"] = data$deaths[data$gender == "Male"] * percent_60_85_m
  data$deaths[data$gender == "Female"] = data$deaths[data$gender == "Female"] * percent_60_85_f

  data$deaths = as.numeric(data$deaths)
  data = data[order(data$gender),]
  skip_gen = read_xlsx("data/skip_gen.xlsx")
  skip_gen = skip_gen[which(skip_gen == "Kenya"),]
  data$skip_generation = c(skip_gen$sg_female, skip_gen$sg_male)*100
  data$value = data$deaths * data$skip_generation/100
  data$'older persons co-residing' = c(skip_gen$mg_female, skip_gen$mg_male)*100
  data$number = data$`older persons co-residing` * data$deaths/100 * 0.89
  data$number = round(data$number)
  data$value = round(data$value)
  write_csv(path = paste0('data/fertility/skip_generation_', country,'.csv'), data)
  print(data)
}

# Malawi
process_malawi_skip_generation = function(){
  country = 'malawi'
  d_summary = read.csv('data/Malawi/covid19_deaths.csv')
  d_summary$age = as.character(d_summary$age)
  
  pop_m <- pop_data_m[which(pop_data_m$`Region, subregion, country or area *` == "Malawi"),]
  pop_m <- gather(pop_m[, 15:23], key = age, value = pop)
  pop_m <- left_join(pop_m, ifrs)
  pop_m <- pop_m[3:9,]
  pop_m$weight <- as.numeric(pop_m$pop)*pop_m$ifr
  percent_70_85_m <- sum(pop_m$weight[pop_m$age %in% c("70-74", "75-79", "80-84")])/
    sum(pop_m$weight)
  
  pop_f <- pop_data_f[which(pop_data_f$`Region, subregion, country or area *` == "Malawi"),]
  pop_f <- gather(pop_f[, 15:23], key = age, value = pop)
  pop_f <- left_join(pop_f, ifrs)
  pop_f <- pop_f[3:9,]
  pop_f$weight <- as.numeric(pop_f$pop)*pop_f$ifr
  percent_70_85_f <- sum(pop_f$weight[pop_f$age %in% c("70-74", "75-79", "80-84")])/
    sum(pop_f$weight)
  
  d_summary$deaths[d_summary$gender == "Male" & d_summary$age == "70+"] = 
    d_summary$deaths[d_summary$gender == "Male" & d_summary$age == "70+"] * percent_70_85_m
  d_summary$deaths[d_summary$gender == "Female" & d_summary$age == "70+"] = 
    d_summary$deaths[d_summary$gender == "Female" & d_summary$age == "70+"] * percent_70_85_f
  
  
  d_summary$age = ifelse(d_summary$age %in% c("60-69", "70+"),'60+', 'others')
  
  data = d_summary %>% filter(age == '60+') %>% group_by(gender) %>% mutate(grand_deaths = sum(deaths
  )) %>% ungroup() %>%
    select(age, gender, grand_deaths) %>% distinct()
  data = data[order(data$gender),]
  skip_gen = read_xlsx("data/skip_gen.xlsx")
  skip_gen = skip_gen[which(skip_gen == "Malawi"),]
  data$skip_generation = c(skip_gen$sg_female, skip_gen$sg_male)*100
  data$value = data$grand_deaths * data$skip_generation/100
  data$'older persons co-residing' = c(skip_gen$mg_female, skip_gen$mg_male)*100
  data$number = data$`older persons co-residing` * data$grand_deaths/100 * 0.89
  data$number = round(data$number)
  data$grand_deaths = round(data$grand_deaths)
  data$value = round(data$value)
  write_csv(path = paste0('data/fertility/skip_generation_', country,'.csv'), data)
  print(data)
}

# Mexico
process_mexico_skip_generation = function(){
  country = 'mexico'
  d_summary = read.csv('data/Mexico/covid19_deaths.csv')
  d_summary$age = as.character(d_summary$age)
  
  pop_m <- pop_data_m[which(pop_data_m$`Region, subregion, country or area *` == "Mexico"),]
  pop_m <- gather(pop_m[, 15:23], key = age, value = pop)
  pop_m <- left_join(pop_m, ifrs)
  pop_m$weight <- as.numeric(pop_m$pop)*pop_m$ifr
  pop_m <- pop_m[5:9,]
  percent_80_85_m <- sum(pop_m$weight[pop_m$age %in% c("80-84")])/
    sum(pop_m$weight)
  
  pop_f <- pop_data_f[which(pop_data_f$`Region, subregion, country or area *` == "Mexico"),]
  pop_f <- gather(pop_f[, 15:23], key = age, value = pop)
  pop_f <- left_join(pop_f, ifrs)
  pop_f$weight <- as.numeric(pop_f$pop)*pop_f$ifr
  pop_f <- pop_f[5:9,]
  percent_80_85_f <- sum(pop_f$weight[pop_f$age %in% c("80-84")])/
    sum(pop_f$weight)
  
  d_summary$deaths[d_summary$gender == "Male" & d_summary$age == "80+"] = 
    d_summary$deaths[d_summary$gender == "Male" & d_summary$age == "80+"] * percent_80_85_m
  d_summary$deaths[d_summary$gender == "Female" & d_summary$age == "80+"] = 
    d_summary$deaths[d_summary$gender == "Female" & d_summary$age == "80+"] * percent_80_85_f
  
  d_summary$age = ifelse(d_summary$age %in% c("60-64", '65-69',"70-74",'75-79', "80+"),'60+', 'others')
  # 4.4% female 3.4% male 
  data = d_summary %>% filter(age == '60+') %>% group_by(gender) %>% mutate(grand_deaths = sum(deaths
  )) %>% ungroup() %>%
    select(age, gender, grand_deaths) %>% distinct()
  data = data[order(data$gender),]
  skip_gen = read_xlsx("data/skip_gen.xlsx")
  skip_gen = skip_gen[which(skip_gen == "Mexico"),]
  data$skip_generation = c(skip_gen$sg_female, skip_gen$sg_male)*100
  data$value = data$grand_deaths * data$skip_generation/100
  data$'older persons co-residing' = c(skip_gen$mg_female, skip_gen$mg_male)*100
  data$number = data$`older persons co-residing` * data$grand_deaths/100 * 0.89
  data$number = round(data$number)
  data$grand_deaths = round(data$grand_deaths)
  data$value = round(data$value)
  write_csv(path = paste0('data/fertility/skip_generation_', country,'.csv'), data)
  print(data)
}

# Nigeria
process_nigeria_skip_generation = function(){
  country = 'nigeria'
  d_summary = read.csv('data/Nigeria/covid19_deaths.csv')
  d_summary$age = as.character(d_summary$age)
  d_summary$age = ifelse(d_summary$age %in% c("60-64", "65-69", "70-74", "75-79", "80-84"),'60+', 'others')
  
  data = d_summary %>% filter(age == '60+') %>% group_by(gender) %>% mutate(grand_deaths = sum(deaths
  )) %>% ungroup() %>%
    select(age, gender, grand_deaths) %>% distinct()
  # 14.2% female 6.9% male 
  data = data[order(data$gender),]
  skip_gen = read_xlsx("data/skip_gen.xlsx")
  skip_gen = skip_gen[which(skip_gen == "Nigeria"),]
  data$skip_generation = c(skip_gen$sg_female, skip_gen$sg_male)*100
  data$value = data$grand_deaths * data$skip_generation/100
  data$'older persons co-residing' = c(skip_gen$mg_female, skip_gen$mg_male)*100
  data$number = data$`older persons co-residing` * data$grand_deaths/100 * 0.89
  data$number = round(data$number)
  data$grand_deaths = round(data$grand_deaths)
  data$value = round(data$value)
  write_csv(path = paste0('data/fertility/skip_generation_', country,'.csv'), data)
  print(data)
}

# Peru
process_peru_skip_generation = function(){
  country = 'peru'
  d_summary = read.csv('data/Peru/covid19_deaths.csv')
  d_summary$age = as.character(d_summary$age)
  
  pop_m <- pop_data_m[which(pop_data_m$`Region, subregion, country or area *` == "Peru"),]
  pop_m <- gather(pop_m[, 15:23], key = age, value = pop)
  pop_m <- left_join(pop_m, ifrs)
  pop_m$weight <- as.numeric(pop_m$pop)*pop_m$ifr
  pop_m <- pop_m[5:6,]
  percent_80_85_m <- sum(pop_m$weight[pop_m$age %in% c("80-84")])/
    sum(pop_m$weight)
  
  pop_f <- pop_data_f[which(pop_data_f$`Region, subregion, country or area *` == "Peru"),]
  pop_f <- gather(pop_f[, 15:23], key = age, value = pop)
  pop_f <- left_join(pop_f, ifrs)
  pop_f$weight <- as.numeric(pop_f$pop)*pop_f$ifr
  pop_f <- pop_f[5:6,]
  percent_80_85_f <- sum(pop_f$weight[pop_f$age %in% c("80-84")])/
    sum(pop_f$weight)
  
  d_summary$COVID19_deaths[d_summary$gender == "Male" & d_summary$age == "80-89"] = 
    d_summary$COVID19_deaths[d_summary$gender == "Male" & d_summary$age == "80-89"] * percent_80_85_m
  d_summary$COVID19_deaths[d_summary$gender == "Female" & d_summary$age == "80-89"] = 
    d_summary$COVID19_deaths[d_summary$gender == "Female" & d_summary$age == "80-89"] * percent_80_85_f
  
  
  d_summary$age = ifelse(d_summary$age %in% c("60-69","70-79","80-89"),'60+', 'others')
  
  data = d_summary %>% filter(age == '60+') %>% group_by(gender) %>% mutate(grand_deaths = sum(COVID19_deaths
  )) %>% ungroup() %>%
    select(age, gender, grand_deaths) %>% distinct()
  data = data[order(data$gender),]
  skip_gen = read_xlsx("data/skip_gen.xlsx")
  skip_gen = skip_gen[which(skip_gen == "Peru"),]
  data$skip_generation = c(skip_gen$sg_female, skip_gen$sg_male)*100
  data$value = data$grand_deaths * data$skip_generation/100
  data$'older persons co-residing' = c(skip_gen$mg_female, skip_gen$mg_male)*100
  data$number = data$`older persons co-residing` * data$grand_deaths/100 * 0.89
  data$number = round(data$number)
  data$grand_deaths = round(data$grand_deaths)
  data$value = round(data$value)
  write_csv(path = paste0('data/fertility/skip_generation_', country,'.csv'), data)
  print(data)
}

# Philippines
process_philippines_skip_generation = function(){
  country = 'philippines'
  d_summary = read.csv('data/Philippines/covid19_deaths.csv')
  d_summary$age = as.character(d_summary$age)
  
  pop_m <- pop_data_m[which(pop_data_m$`Region, subregion, country or area *` == "Philippines"),]
  pop_m <- gather(pop_m[, 15:23], key = age, value = pop)
  pop_m <- left_join(pop_m, ifrs)
  pop_m$weight <- as.numeric(pop_m$pop)*pop_m$ifr
  pop_m <- pop_m[5:6,]
  percent_80_85_m <- sum(pop_m$weight[pop_m$age %in% c("80-84")])/
    sum(pop_m$weight)
  
  pop_f <- pop_data_f[which(pop_data_f$`Region, subregion, country or area *` == "Philippines"),]
  pop_f <- gather(pop_f[, 15:23], key = age, value = pop)
  pop_f <- left_join(pop_f, ifrs)
  pop_f$weight <- as.numeric(pop_f$pop)*pop_f$ifr
  pop_f <- pop_f[5:6,]
  percent_80_85_f <- sum(pop_f$weight[pop_f$age %in% c("80-84")])/
    sum(pop_f$weight)
  
  d_summary$COVID19_deaths[d_summary$gender == "Male" & d_summary$age == "80+"] = 
    d_summary$COVID19_deaths[d_summary$gender == "Male" & d_summary$age == "80+"] * percent_80_85_m
  d_summary$COVID19_deaths[d_summary$gender == "Female" & d_summary$age == "80+"] = 
    d_summary$COVID19_deaths[d_summary$gender == "Female" & d_summary$age == "80+"] * percent_80_85_f
  
  
  d_summary$age = ifelse(d_summary$age %in% c("60-64", "65-69","70-75","75-79", "80+"),'60+', 'others')
  
  data = d_summary %>% filter(age == '60+') %>% group_by(gender) %>% mutate(grand_deaths = sum(COVID19_deaths
  )) %>% ungroup() %>%
    select(age, gender, grand_deaths) %>% distinct()
  data = data[order(data$gender),]
  skip_gen = read_xlsx("data/skip_gen.xlsx")
  skip_gen = skip_gen[which(skip_gen == "Philippines"),]
  data$skip_generation = c(skip_gen$sg_female, skip_gen$sg_male)*100
  data$value = data$grand_deaths * data$skip_generation/100
  data$'older persons co-residing' = c(skip_gen$mg_female, skip_gen$mg_male)*100
  data$number = data$`older persons co-residing` * data$grand_deaths/100 * 0.89
  data$number = round(data$number)
  data$grand_deaths = round(data$grand_deaths)
  data$value = round(data$value)
  write_csv(path = paste0('data/fertility/skip_generation_', country,'.csv'), data)
  print(data)
}

# Poland
process_poland_skip_generation = function(){
  country = 'poland'
  d_summary = read.csv('data/Poland/poland_all.csv')
  setnames(d_summary, c('death'), c('deaths'))
  d_summary$age = as.character(d_summary$age)
  
  pop_m <- pop_data_m[which(pop_data_m$`Region, subregion, country or area *` == "Poland"),]
  pop_m <- gather(pop_m[, 15:23], key = age, value = pop)
  pop_m <- left_join(pop_m, ifrs)
  pop_m$weight <- as.numeric(pop_m$pop)*pop_m$ifr
  pop_m <- pop_m[5:6,]
  percent_80_85_m <- sum(pop_m$weight[pop_m$age %in% c("80-84")])/
    sum(pop_m$weight)
  
  pop_f <- pop_data_f[which(pop_data_f$`Region, subregion, country or area *` == "Poland"),]
  pop_f <- gather(pop_f[, 15:23], key = age, value = pop)
  pop_f <- left_join(pop_f, ifrs)
  pop_f$weight <- as.numeric(pop_f$pop)*pop_f$ifr
  pop_f <- pop_f[5:6,]
  percent_80_85_f <- sum(pop_f$weight[pop_f$age %in% c("80-84")])/
    sum(pop_f$weight)
  
  d_summary$deaths[d_summary$gender == "Male" & d_summary$age == "80-89"] = 
    d_summary$deaths[d_summary$gender == "Male" & d_summary$age == "80-89"] * percent_80_85_m
  d_summary$deaths[d_summary$gender == "Female" & d_summary$age == "80-89"] = 
    d_summary$deaths[d_summary$gender == "Female" & d_summary$age == "80-89"] * percent_80_85_f
  
  d_summary$age = ifelse(d_summary$age %in% c("60-69","70-79","80-89"),'60+', 'others')
  # 2.9% female 1.7% male 
  d_summary = d_summary[order(d_summary$gender),]
  data = d_summary %>% filter(age == '60+') %>% group_by(gender) %>% mutate(grand_deaths = sum(deaths
  )) %>% ungroup() %>%
    select(age, gender, grand_deaths) %>% distinct()
  skip_gen = read_xlsx("data/skip_gen.xlsx")
  skip_gen = skip_gen[which(skip_gen == "Poland"),]
  data$skip_generation = c(skip_gen$sg_female, skip_gen$sg_male)*100
  data$value = data$grand_deaths * data$skip_generation/100
  data$'older persons co-residing' = c(skip_gen$mg_female, skip_gen$mg_male)*100
  data$number = data$`older persons co-residing` * data$grand_deaths/100 * 0.89
  data$number = round(data$number)
  data$grand_deaths = round(data$grand_deaths)
  data$value = round(data$value)
  write_csv(path = paste0('data/fertility/skip_generation_', country,'.csv'), data)
  print(data)
}

# Russia
process_russia_skip_generation = function(){
  country = 'russia'
  d_summary = read.csv('data/Russia/all_excess_deaths.csv')
  d_summary$age = ifelse(d_summary$age %in% c("60-64", "65-69","70-74", "75-79", "80-84"),'60+', 'others')
  #% female % male 
  data = d_summary %>% filter(age == '60+') %>% group_by(sex) %>% 
    mutate(grand_deaths = sum(weighted_excess_deaths)) %>% 
    ungroup() %>%
    select(age, sex, grand_deaths) %>% 
    distinct()
  data = data[order(data$sex),]
  skip_gen = read_xlsx("data/skip_gen.xlsx")
  skip_gen = skip_gen[which(skip_gen == "Russia"),]
  data$skip_generation = c(skip_gen$sg_female, skip_gen$sg_male)*100
  data$value = data$grand_deaths * data$skip_generation/100
  data$'older persons co-residing' = c(skip_gen$mg_female, skip_gen$mg_male)*100
  data$number = data$`older persons co-residing` * data$grand_deaths/100 * 0.89
  data$number = round(data$number)
  data$grand_deaths = round(data$grand_deaths)
  data$value = round(data$value)
  write_csv(path = paste0('data/fertility/skip_generation_', country,'.csv'), data)
  print(data)
}

# Spain
process_spain_skip_generation = function(){
  country = 'spain'
  d_summary = read.csv('data/Spain/spain_all.csv')
  #setnames(d_summary, c('death'), c('deaths'))
  d_summary = d_summary %>% group_by(age, gender) %>% mutate(nb_deaths = sum(deaths)) %>% ungroup() %>% 
    select(age, gender, nb_deaths) %>% distinct()
  setnames(d_summary, 'nb_deaths', 'deaths')
  d_summary$age = as.character(d_summary$age)
  
  pop_m <- pop_data_m[which(pop_data_m$`Region, subregion, country or area *` == "Spain"),]
  pop_m <- gather(pop_m[, 15:23], key = age, value = pop)
  pop_m <- left_join(pop_m, ifrs)
  pop_m$weight <- as.numeric(pop_m$pop)*pop_m$ifr
  pop_m <- pop_m[5:9,]
  percent_80_85_m <- sum(pop_m$weight[pop_m$age %in% c("80-84")])/
    sum(pop_m$weight)
  
  pop_f <- pop_data_f[which(pop_data_f$`Region, subregion, country or area *` == "Spain"),]
  pop_f <- gather(pop_f[, 15:23], key = age, value = pop)
  pop_f <- left_join(pop_f, ifrs)
  pop_f$weight <- as.numeric(pop_f$pop)*pop_f$ifr
  pop_f <- pop_f[5:9,]
  percent_80_85_f <- sum(pop_f$weight[pop_f$age %in% c("80-84")])/
    sum(pop_f$weight)
  
  d_summary$deaths[d_summary$gender == "Male" & d_summary$age == "80+"] = 
    d_summary$deaths[d_summary$gender == "Male" & d_summary$age == "80+"] * percent_80_85_m
  d_summary$deaths[d_summary$gender == "Female" & d_summary$age == "80+"] = 
    d_summary$deaths[d_summary$gender == "Female" & d_summary$age == "80+"] * percent_80_85_f
  
  d_summary$age = ifelse(d_summary$age %in% c("60-69","70-79","80+"),'60+', 'others')
  # 0.9% female 0.6% male 
  data = d_summary %>% filter(age == '60+') %>% group_by(gender) %>% mutate(grand_deaths = sum(deaths
  )) %>% ungroup() %>%
    select(age, gender, grand_deaths) %>% distinct()
  data = data[order(data$gender),]
  skip_gen = read_xlsx("data/skip_gen.xlsx")
  skip_gen = skip_gen[which(skip_gen == "Spain"),]
  data$skip_generation = c(skip_gen$sg_female, skip_gen$sg_male)*100
  data$value = data$grand_deaths * data$skip_generation/100
  data$'older persons co-residing' = c(skip_gen$mg_female, skip_gen$mg_male)*100
  data$number = data$`older persons co-residing` * data$grand_deaths/100 * 0.89
  data$number = round(data$number)
  data$grand_deaths = round(data$grand_deaths)
  data$value = round(data$value)
  write_csv(path = paste0('data/fertility/skip_generation_', country,'.csv'), data)
  print(data)
}

# South Africa
process_south_africa_skip_generation = function(){
  d_summary = read.csv('data/SouthAfrica/covid19_deaths.csv')
  setnames(d_summary, c('COVID19_deaths'), c('deaths'))
  d_summary = d_summary %>% group_by(age, gender) %>% mutate(nb_deaths = sum(deaths)) %>% ungroup() %>% 
    select(age, gender, nb_deaths) %>% distinct()
  setnames(d_summary, 'nb_deaths', 'deaths')
  d_summary$age = as.character(d_summary$age)
  
  pop_m <- pop_data_m[which(pop_data_m$`Region, subregion, country or area *` == "South Africa"),]
  pop_m <- gather(pop_m[, 15:23], key = age, value = pop)
  pop_m <- left_join(pop_m, ifrs)
  pop_m$weight <- as.numeric(pop_m$pop)*pop_m$ifr
  pop_m <- pop_m[5:9,]
  percent_80_85_m <- sum(pop_m$weight[pop_m$age %in% c("80-84")])/
    sum(pop_m$weight)
  
  pop_f <- pop_data_f[which(pop_data_f$`Region, subregion, country or area *` == "South Africa"),]
  pop_f <- gather(pop_f[, 15:23], key = age, value = pop)
  pop_f <- left_join(pop_f, ifrs)
  pop_f$weight <- as.numeric(pop_f$pop)*pop_f$ifr
  pop_f <- pop_f[5:9,]
  percent_80_85_f <- sum(pop_f$weight[pop_f$age %in% c("80-84")])/
    sum(pop_f$weight)
  
  d_summary$deaths[d_summary$gender == "Male" & d_summary$age == "80+"] = 
    d_summary$deaths[d_summary$gender == "Male" & d_summary$age == "80+"] * percent_80_85_m
  d_summary$deaths[d_summary$gender == "Female" & d_summary$age == "80+"] = 
    d_summary$deaths[d_summary$gender == "Female" & d_summary$age == "80+"] * percent_80_85_f
  
  d_summary$age = ifelse(d_summary$age %in% c("60-69","70-79","80+" ),'60+', 'others')
  # 9.67% male 19.43% female
  data = d_summary %>% filter(age == '60+') %>% group_by(gender) %>% mutate(grand_deaths = sum(deaths
  )) %>% ungroup() %>%
    select(age, gender, grand_deaths) %>% distinct()
  data = data[order(data$gender),]
  skip_gen = read_xlsx("data/skip_gen.xlsx")
  skip_gen = skip_gen[which(skip_gen == "South Africa"),]
  data$skip_generation = c(skip_gen$sg_female, skip_gen$sg_male)*100
  data$value = data$grand_deaths * data$skip_generation/100
  data$'older persons co-residing' = c(skip_gen$mg_female, skip_gen$mg_male)*100
  data$number = data$`older persons co-residing` * data$grand_deaths/100 * 0.89
  data$number = round(data$number)
  data$grand_deaths = round(data$grand_deaths)
  data$value = round(data$value)
  write_csv(path = 'data/fertility/skip_generation_south_africa.csv', data)
  print(data)
}

# USA
process_usa_skip_generation = function(){
  country = 'usa'
  d_summary = read.csv('data/USA/usa_all.csv')
  d_summary = d_summary %>% group_by(age, gender) %>% mutate(nb_deaths = sum(deaths)) %>% ungroup() %>% 
    select(age, gender, nb_deaths) %>% distinct()
  setnames(d_summary, 'nb_deaths', 'deaths')
  d_summary$age = as.character(d_summary$age)
  d_summary_60 = d_summary %>% filter( age == '55-64')
  d_summary_60$age = '60-64'
  d_summary_60$deaths = d_summary_60$deaths/2
  d_summary = rbind(d_summary, d_summary_60)
  d_summary$age = ifelse(d_summary$age %in% c("60-64",'65-74', "75-84"),'60+', 'others')
  d_summary = d_summary[order(d_summary$gender),]
  # 2.2% female 1.6% male 
  data = d_summary %>% filter(age == '60+') %>% group_by(gender) %>% mutate(grand_deaths = sum(deaths
  )) %>% ungroup() %>%
    select(age, gender, grand_deaths) %>% distinct()
  skip_gen = read_xlsx("data/skip_gen.xlsx")
  skip_gen = skip_gen[which(skip_gen == "USA"),]
  data$skip_generation = c(skip_gen$sg_female, skip_gen$sg_male)*100
  data$value = data$grand_deaths * data$skip_generation/100
  data$'older persons co-residing' = c(skip_gen$mg_female, skip_gen$mg_male)*100
  data$number = data$`older persons co-residing` * data$grand_deaths/100 * 0.89
  data$number = round(data$number)
  data$grand_deaths = round(data$grand_deaths)
  data$value = round(data$value)
  write_csv(path = paste0('data/fertility/skip_generation_', country,'.csv'), data)
  print(data)
}

# Zimbabwe
process_zimbabwe_skip_generation = function(){
  country = 'zimbabwe'
  d_summary = read.csv('data/Zimbabwe/covid19_deaths.csv')
  d_summary$age = as.character(d_summary$age)
  
  pop_m <- pop_data_m[which(pop_data_m$`Region, subregion, country or area *` == "Zimbabwe"),]
  pop_m <- gather(pop_m[, 15:23], key = age, value = pop)
  pop_m <- left_join(pop_m, ifrs)
  pop_m$weight <- as.numeric(pop_m$pop)*pop_m$ifr
  pop_m <- pop_m[5:6,]
  percent_80_85_m <- sum(pop_m$weight[pop_m$age %in% c("80-84")])/
    sum(pop_m$weight)
  
  pop_f <- pop_data_f[which(pop_data_f$`Region, subregion, country or area *` == "Zimbabwe"),]
  pop_f <- gather(pop_f[, 15:23], key = age, value = pop)
  pop_f <- left_join(pop_f, ifrs)
  pop_f$weight <- as.numeric(pop_f$pop)*pop_f$ifr
  pop_f <- pop_f[5:6,]
  percent_80_85_f <- sum(pop_f$weight[pop_f$age %in% c("80-84")])/
    sum(pop_f$weight)
  
  d_summary$deaths[d_summary$gender == "Male" & d_summary$age == "81-90"] = 
    d_summary$deaths[d_summary$gender == "Male" & d_summary$age == "81-90"] * percent_80_85_m
  d_summary$deaths[d_summary$gender == "Female" & d_summary$age == "81-90"] = 
    d_summary$deaths[d_summary$gender == "Female" & d_summary$age == "81-90"] * percent_80_85_f
  
  d_summary$age = ifelse(d_summary$age %in% c("61-70", "71-80", "81-90"), '61+', 'others')
  
  data = d_summary %>% filter(age == '61+') %>% group_by(gender) %>% mutate(grand_deaths = sum(deaths
  )) %>% ungroup() %>%
    select(age, gender, grand_deaths) %>% distinct()
  data = data[order(data$gender),]
  skip_gen = read_xlsx("data/skip_gen.xlsx")
  skip_gen = skip_gen[which(skip_gen == "Zimbabwe"),]
  data$skip_generation = c(skip_gen$sg_female, skip_gen$sg_male)*100
  data$value = data$grand_deaths * data$skip_generation/100
  data$'older persons co-residing' = c(skip_gen$mg_female, skip_gen$mg_male)*100
  data$number = data$`older persons co-residing` * data$grand_deaths/100 * 0.89
  data$number = round(data$number)
  data$grand_deaths = round(data$grand_deaths)
  data$value = round(data$value)
  write_csv(path = paste0('data/fertility/skip_generation_', country,'.csv'), data)
  print(data)
}
