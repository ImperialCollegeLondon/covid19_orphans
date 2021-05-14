source(file.path("R","process_children_parents.R"))
source(file.path("R","process_fertility.R"))
source(file.path("R","process_child_mortality.R"))

# Brazil
process_number_children_brazil <- function(){
  # Calculate number of children from different aged fathers
  cat(sprintf("Processing number of children of fathers\n"))
  data = readRDS('data/Brazil/male_fertility.RDS')
  data$y2016 = data$y2015
  data$y2017 = data$y2015
  data$y2018 = data$y2015
  data$y2019 = data$y2015
  data$y2020 = data$y2015
  data = reshape2::melt(data, id.vars = c('ages'), 
                        variable.name = 'year', value.name = 'fertility_rate')
  setnames(data, 'ages', 'age')
  data$age = as.character(data$age)
  data$age = gsub('[(]', '', data$age)
  data$age = gsub('[]]', '', data$age)
  data$year = as.character(data$year)
  data$year = gsub('y', '', data$year)
  data$fertility_rate[which(is.na(data$fertility_rate))] = 0
  data$age = unlist(lapply(data$age,function(x){ ifelse(x != '80+', paste0(strsplit(x, '-')[[1]][1], '-', 
                                                                           as.numeric(strsplit(x, '-')[[1]][2])-1), x)}))
  
  data$fertility_rate[data$age == "80+"] = 0
  data_f = copy(data)
  data_f$date = data_f$year
  data_f$gender = 'M'
  write_csv(data_f, path = paste0('data/Brazil/brazil_fertility_m_all.csv'))
  process_children_father_80_plus("Brazil", data_f)
  is_child_mortality_needed = 0
  add_child_mortality(is_child_mortality_needed, "Brazil")
 
  # Calculate number of children from different aged mothers
  cat(sprintf("Processing number of children of mothers\n"))
  data = readRDS('data/Brazil/female_fertility.RDS')
  data$y2016 = data$y2015
  data$y2017 = data$y2015
  data$y2018 = data$y2015
  data$y2019 = data$y2015
  data$y2020 = data$y2015
  data = reshape2::melt(data, id.vars = c('ages'), 
                        variable.name = 'year', value.name = 'fertility_rate')
  setnames(data, 'ages', 'age')
  data$age = as.character(data$age)
  data$age = gsub('[(]', '', data$age)
  data$age = gsub('[]]', '', data$age)
  data$year = as.character(data$year)
  data$year = gsub('y', '', data$year)
  data = data %>% filter(!is.na(fertility_rate))
  data$age = unlist(lapply(data$age,function(x){ paste0(strsplit(x, '-')[[1]][1], '-', 
                                                        as.numeric(strsplit(x, '-')[[1]][2])-1)}))
  data_f = copy(data)
  data_f$date = data_f$year
  data_f$gender = 'F'  
  write_csv(data_f, path = paste0('data/Brazil/brazil_fertility_f.csv'))
  is_child_mortality_needed = 0
  process_children_all("Brazil", is_child_mortality_needed, data_f)
  
  process_fertility_plots("Brazil")
}

# Colombia
process_number_children_colombia <- function(){
  # Calculate number of children from different aged fathers
  cat(sprintf("Processing number of children of fathers\n"))
  data = readRDS('data/Colombia/male_fertility.RDS')
  data$y2016 = data$y2015
  data$y2017 = data$y2015
  data$y2018 = data$y2015
  data$y2019 = data$y2015
  data$y2020 = data$y2015
  data = reshape2::melt(data, id.vars = c('ages'), 
                        variable.name = 'year', value.name = 'fertility_rate')
  setnames(data, 'ages', 'age')
  data$age = as.character(data$age)
  data$age = gsub('[(]', '', data$age)
  data$age = gsub('[]]', '', data$age)
  data$year = as.character(data$year)
  data$year = gsub('y', '', data$year)
  data$fertility_rate[which(is.na(data$fertility_rate))] = 0
  data$age = unlist(lapply(data$age,function(x){ ifelse(x != '80+', paste0(strsplit(x, '-')[[1]][1], '-', 
                                                                           as.numeric(strsplit(x, '-')[[1]][2])-1), x)}))
  data$fertility_rate[data$age == "80+"] = 0
  data_f = copy(data)
  data_f$date = data_f$year
  data_f$gender = 'M'
  
  data_summary = data %>% group_by(age) %>% mutate(rate = mean(fertility_rate)) %>%
    ungroup %>% select(-fertility_rate, -year) %>% distinct()

  write_csv(data_f, file = paste0('data/Colombia/colombia_fertility_m_all.csv'))
  process_children_father_80_plus("Colombia", data_f)
  is_child_mortality_needed = 0
  add_child_mortality(is_child_mortality_needed, 'Colombia')
  
  # Calculate number of children from different aged mothers
  cat(sprintf("Processing number of children of mothers\n"))
  data = readRDS('data/Colombia/female_fertility.RDS')
  data$y2016 = data$y2015
  data$y2017 = data$y2015
  data$y2018 = data$y2015
  data$y2019 = data$y2015
  data$y2020 = data$y2015
  data = reshape2::melt(data, id.vars = c('ages'), 
                        variable.name = 'year', value.name = 'fertility_rate')
  setnames(data, 'ages', 'age')
  data$age = as.character(data$age)
  data$age = gsub('[(]', '', data$age)
  data$age = gsub('[]]', '', data$age)
  data$year = as.character(data$year)
  data$year = gsub('y', '', data$year)
  data = data %>% filter(!is.na(fertility_rate))
  data$age = unlist(lapply(data$age,function(x){ paste0(strsplit(x, '-')[[1]][1], '-', 
                                                        as.numeric(strsplit(x, '-')[[1]][2])-1)}))
  data_f = copy(data)
  data_f$date = data_f$year
  data_f$gender = 'F'
  write_csv(data_f, path = paste0('data/Colombia/colombia_fertility_f.csv'))
  
  is_child_mortality_needed = 0
  process_children_all("Colombia", is_child_mortality_needed, data_f)
  
  # Make fertility plots
  process_fertility_plots("Colombia")
}

# England and wales
process_number_children_england_wales <- function(){
  # Calculating fertility rates
  process_england_wales_fertility()
  # Adapting fertility with infant mortality
  cat(sprintf("Processing child mortality rates\n"))
  process_children_mortality_england_wales()
  process_infant_mortality_england_wales()
  process_child_mortality_england_wales()
  
  # fathers
  cat(sprintf("Processing number of children of fathers\n"))
  data_f = read.csv('data/UK/england_wales_fertility_m.csv')
  data_f$fertility_rate <- data_f$rate/1000
  data_f$date = data_f$year
  data_f$age = as.character(data_f$age)
  d_2019 = data_f[which(data_f$date == '2018'),]
  d_2019$date = '2019'
  d_2020 = copy(d_2019)
  d_2020$date = '2020'
  data_f = rbind(data_f, d_2019, d_2020)
  process_children_father_england_wales(data_f)
  is_child_mortality_needed = 1
  add_child_mortality(is_child_mortality_needed, "UK")
  
  # mothers
  cat(sprintf("Processing number of children of mothers\n"))
  data = read.csv('data/UK/england_wales_fertility_f.csv')
  data_f = copy(data)
  data_f$fertility_rate <- data_f$rate/1000
  data_f$date = data_f$year
  data_f$gender = 'F'
  d_2019 = data_f[which(data_f$date == '2018'),]
  d_2019$date = '2019'
  data_f = rbind(data_f, d_2019)
  d_2020 = copy(d_2019)
  d_2020$date = '2020'
  data_f = rbind(data_f, d_2020)
  process_children_all("UK", is_child_mortality_needed, data_f)
  
  cat(sprintf("Processing fertility rates\n"))
  process_fertility_plots("england_wales")
}

# France
process_number_children_france <- function(){
  # fertility
  cat(sprintf("Processing Fertility rates\n"))
  process_france_fertility()

  # child mortality
  cat(sprintf("Processing child mortality rates\n"))
  process_child_mortality('France', 'France')
  
  # fathers
  cat(sprintf("Processing number of children of fathers\n"))
  data_f = read.csv(paste0('data/France/france_fertility_m_all.csv'))
  data_f$fertility_rate <- data_f$fertility_rate/1000
  data_f$date = data_f$year
  data_f$age = as.character(data_f$age)
  process_children_father_65_plus("France", data_f)
  is_child_mortality_needed = 1
  add_child_mortality(is_child_mortality_needed, "France")
  
  # mothers
  cat(sprintf("Processing number of children of mothers\n"))
  data = read.csv(paste0('data/France/france_fertility_f.csv'))
  data_f = copy(data)
  data_f$fertility_rate = data_f$fertility_rate/1000
  d_2019 = data_f[which(data_f$date == '2019'),]
  d_2020 = copy(d_2019)
  d_2020$date = '2020'
  data_f = rbind(data_f, d_2020)
  
  process_children_all("France", is_child_mortality_needed, data_f)

  process_fertility_plots("France")
}

# Germany
process_number_children_germany <- function(){
  # fertility
  cat(sprintf("Processing Fertility rates\n"))
  process_germany_fertility()
  # child mortality
  cat(sprintf("Processing child mortality rates\n"))
  process_child_mortality('Germany', 'Germany')
  
  # fathers
  cat(sprintf("Processing number of children of fathers\n"))
  data_f = read.csv(paste0('data/Germany/germany_fertility_m_all.csv'))
  data_f$fertility_rate <- data_f$fertility_rate/1000
  data_f$date = data_f$year
  data_f$age = as.character(data_f$age)
  process_children_father_65_plus("Germany", data_f)
  is_child_mortality_needed = 1
  add_child_mortality(is_child_mortality_needed, "Germany")
  
  # mothers
  cat(sprintf("Processing number of children of mothers\n"))
  data = read.csv(paste0('data/Germany/germany_fertility_f.csv'))
  data_f = copy(data)
  data_f$fertility_rate = data_f$fertility_rate/1000
  d_2019 = data_f[which(data_f$date == '2019'),]
  d_2020 = copy(d_2019)
  d_2020$date = '2020'
  data_f = rbind(data_f, d_2020)
  process_children_all("Germany", is_child_mortality_needed, data_f)
  
  cat(sprintf("Processing fertility rates\n"))
  process_fertility_plots("Germany")
}

# India
process_number_children_india <- function(){
  cat(sprintf("Processing number of children of fathers\n"))
  data = readRDS('data/India/male_fertility.RDS')
  data$y2016 = data$y2015
  data$y2017 = data$y2016
  data$y2018 = data$y2016
  data$y2019 = data$y2016
  data$y2020 = data$y2016
  data = reshape2::melt(data, id.vars = c('ages'), 
                        variable.name = 'year', value.name = 'fertility_rate')
  setnames(data, 'ages', 'age')
  data$age = as.character(data$age)
  data$age = gsub('[(]', '', data$age)
  data$age = gsub('[]]', '', data$age)
  data$year = as.character(data$year)
  data$year = gsub('y', '', data$year)
  data$fertility_rate[which(is.na(data$fertility_rate))] = 0
  data$age = unlist(lapply(data$age,function(x){ ifelse(x != '80+', paste0(strsplit(x, '-')[[1]][1], '-', 
                                                                           as.numeric(strsplit(x, '-')[[1]][2])-1), x)}))
  data$fertility_rate[data$age == "80+"] = 0
  data_f = copy(data)
  data_f$date = data_f$year
  data_f$gender = 'M'
  write_csv(data_f, path = paste0('data/India/india_fertility_m_all.csv'))
  process_children_father_80_plus("India", data_f)
  is_child_mortality_needed = 0
  add_child_mortality(is_child_mortality_needed, "India")
  
  # mothers
  cat(sprintf("Processing number of children of mothers\n"))
  data = readRDS('data/India/female_fertility.RDS')
  data$y2016 = data$y2015
  data$y2017 = data$y2015
  data$y2018 = data$y2015
  data$y2019 = data$y2015
  data$y2020 = data$y2015
  data = reshape2::melt(data, id.vars = c('ages'), 
                        variable.name = 'year', value.name = 'fertility_rate')
  setnames(data, 'ages', 'age')
  data$age = as.character(data$age)
  data$age = gsub('[(]', '', data$age)
  data$age = gsub('[]]', '', data$age)
  data$year = as.character(data$year)
  data$year = gsub('y', '', data$year)
  data = data %>% filter(!is.na(fertility_rate))
  data$age = unlist(lapply(data$age,function(x){ paste0(strsplit(x, '-')[[1]][1], '-', 
                                                        as.numeric(strsplit(x, '-')[[1]][2])-1)}))
  data_f = copy(data)
  data_f$date = data_f$year
  data_f$gender = 'F'
  write_csv(data_f, path = paste0('data/India/india_fertility_f.csv'))
  process_children_all("India", is_child_mortality_needed, data_f)
  
  process_fertility_plots("India")
}

# Iran
process_number_children_iran <- function(){
  # fertility
  cat(sprintf("Processing Fertility rates\n"))
  process_iran_fertility()
  # child mortality
  cat(sprintf("Processing child mortality rates\n"))
  process_child_mortality('Iran', 'Iran')
  
  # fathers
  cat(sprintf("Processing number of children of fathers\n"))
  data_f = read.csv(paste0('data/Iran/iran_fertility_m_all.csv'))
  data_f$fertility_rate = data_f$fertility_rate/1000
  data_f$age = as.character(data_f$age)
  process_children_father_55_plus("Iran", data_f)
  is_child_mortality_needed = 1
  add_child_mortality(is_child_mortality_needed, "Iran")
  
  # mothers
  cat(sprintf("Processing number of children of mothers\n"))
  data = read.csv(paste0('data/Iran/iran_fertility_f.csv'))
  data_f = copy(data)
  data_f$fertility_rate = data_f$fertility_rate/1000
  d_2019 = data_f[which(data_f$date == '2019'),]
  d_2020 = copy(d_2019)
  d_2020$date = '2020'
  data_f = rbind(data_f, d_2020)
  process_children_all("Iran", is_child_mortality_needed, data_f)
  
  process_fertility_plots("Iran")
}


# Italy
process_number_children_italy <- function(){
  # fertility
  cat(sprintf("Processing Fertility rates\n"))
  process_italy_fertility()
  # child mortality
  cat(sprintf("Processing child mortality rates\n"))
  process_child_mortality('Italy', 'Italy')
  
  # fathers
  cat(sprintf("Processing number of children of fathers\n"))
  data_f = read.csv(paste0('data/Italy/italy_fertility_m_all.csv'))
  data_f$fertility_rate <- data_f$fertility_rate/1000
  data_f$date = data_f$year
  data_f$age = as.character(data_f$age)
  process_children_father_65_plus("Italy", data_f)
  is_child_mortality_needed = 1
  add_child_mortality(is_child_mortality_needed, "Italy")
  
  # mothers
  cat(sprintf("Processing number of children of mothers\n"))
  data = read.csv(paste0('data/Italy/italy_fertility_f.csv'))
  data_f = copy(data)
  data_f$fertility_rate = data_f$fertility_rate/1000
  d_2019 = data_f[which(data_f$date == '2019'),]
  d_2020 = copy(d_2019)
  d_2020$date = '2020'
  data_f = rbind(data_f, d_2020)
  process_children_all("Italy", is_child_mortality_needed, data_f)

  process_fertility_plots("Italy")
}

# Kenya
process_number_children_kenya <- function(){
  # fathers
  cat(sprintf("Processing number of children of fathers\n"))
  data = readRDS('data/Kenya/male_fertility.RDS')
  data$y2015 = data$y2014
  data$y2016 = data$y2015
  data$y2017 = data$y2016
  data$y2018 = data$y2016
  data$y2019 = data$y2016
  data$y2020 = data$y2016
  data = reshape2::melt(data, id.vars = c('ages'), 
                        variable.name = 'year', value.name = 'fertility_rate')
  setnames(data, 'ages', 'age')
  data$age = as.character(data$age)
  data$age = gsub('[(]', '', data$age)
  data$age = gsub('[]]', '', data$age)
  data$year = as.character(data$year)
  data$year = gsub('y', '', data$year)
  data$fertility_rate[which(is.na(data$fertility_rate))] = 0
  data$age = unlist(lapply(data$age,function(x){ ifelse(x != '80+', paste0(strsplit(x, '-')[[1]][1], '-', 
                                                                           as.numeric(strsplit(x, '-')[[1]][2])-1), x)}))
  data$fertility_rate[data$age == "80+"] = 0
  data_f = copy(data)
  data_f$date = data_f$year
  data_f$gender = 'M'
  write_csv(data_f, path = paste0('data/Kenya/kenya_fertility_m_all.csv'))
  process_children_father_80_plus("Kenya", data_f)
  is_child_mortality_needed = 0
  add_child_mortality(is_child_mortality_needed, "Kenya")
  
  # mothers
  cat(sprintf("Processing number of children of mothers\n"))
  data = readRDS('data/Kenya/female_fertility.RDS')
  data$y2015 = data$y2014
  data$y2016 = data$y2015
  data$y2017 = data$y2016
  data$y2018 = data$y2016
  data$y2019 = data$y2016
  data$y2020 = data$y2016
  data = reshape2::melt(data, id.vars = c('ages'), 
                        variable.name = 'year', value.name = 'fertility_rate')
  setnames(data, 'ages', 'age')
  data$age = as.character(data$age)
  data$age = gsub('[(]', '', data$age)
  data$age = gsub('[]]', '', data$age)
  data$year = as.character(data$year)
  data$year = gsub('y', '', data$year)
  data$fertility_rate[which(is.na(data$fertility_rate))] = 0
  data$age = unlist(lapply(data$age,function(x){ ifelse(x != '80+', paste0(strsplit(x, '-')[[1]][1], '-', 
                                                                           as.numeric(strsplit(x, '-')[[1]][2])-1), x)}))
  data_f = copy(data)
  data_f$date = data_f$year
  data_f$gender = 'F'
  write_csv(data_f, path = paste0('data/Kenya/kenya_fertility_f.csv'))
  is_child_mortality_needed = 0
  process_children_all("Kenya", is_child_mortality_needed, data_f)

  process_fertility_plots("Kenya")
}

# Malawi
process_number_children_malawi <- function(){
  # fathers
  cat(sprintf("Processing number of children of fathers\n"))
  
  data = readRDS('data/Malawi/male_fertility.RDS')
  data$y2016 = data$y2015
  data$y2017 = data$y2015
  data$y2018 = data$y2015
  data$y2019 = data$y2015
  data$y2020 = data$y2015
  data = reshape2::melt(data, id.vars = c('ages'), 
                        variable.name = 'year', value.name = 'fertility_rate')
  setnames(data, 'ages', 'age')
  data$age = as.character(data$age)
  data$age = gsub('[(]', '', data$age)
  data$age = gsub('[]]', '', data$age)
  data$year = as.character(data$year)
  data$year = gsub('y', '', data$year)
  data$fertility_rate[which(is.na(data$fertility_rate))] = 0
  data$age = unlist(lapply(data$age,function(x){ ifelse(x != '80+', paste0(strsplit(x, '-')[[1]][1], '-', 
                                                                           as.numeric(strsplit(x, '-')[[1]][2])-1), x)}))
  data$fertility_rate[data$age == "80+"] = 0
  data_f = copy(data)
  data_f$date = data_f$year
  data_f$gender = 'M'
  write_csv(data_f, path = paste0('data/Malawi/malawi_fertility_m_all.csv'))
  process_children_father_80_plus("Malawi", data_f)
  is_child_mortality_needed = 0
  add_child_mortality(is_child_mortality_needed, "Malawi")
  
  # mothers
  cat(sprintf("Processing number of children of mothers\n"))
  data = readRDS('data/Malawi/female_fertility.RDS')
  data$y2016 = data$y2015
  data$y2017 = data$y2015
  data$y2018 = data$y2015
  data$y2019 = data$y2015
  data$y2020 = data$y2015
  data = reshape2::melt(data, id.vars = c('ages'), 
                        variable.name = 'year', value.name = 'fertility_rate')
  setnames(data, 'ages', 'age')
  data$age = as.character(data$age)
  data$age = gsub('[(]', '', data$age)
  data$age = gsub('[]]', '', data$age)
  data$year = as.character(data$year)
  data$year = gsub('y', '', data$year)
  data$fertility_rate[which(is.na(data$fertility_rate))] = 0
  data$age = unlist(lapply(data$age,function(x){ ifelse(x != '80+', paste0(strsplit(x, '-')[[1]][1], '-', 
                                                                           as.numeric(strsplit(x, '-')[[1]][2])-1), x)}))
  data_f = copy(data)
  data_f$date = data_f$year
  data_f$gender = 'F'
  write_csv(data_f, path = paste0('data/Malawi/malawi_fertility_f.csv'))
  is_child_mortality_needed = 0
  process_children_all("Malawi", is_child_mortality_needed, data_f)
  
  process_fertility_plots("Malawi")
  
}

# Mexico
process_number_children_mexico <- function(){
  # fertility
  cat(sprintf("Processing Fertility rates\n"))
  process_mexico_fertility()
  # child mortality
  cat(sprintf("Processing child mortality rates\n"))
  process_child_mortality('Mexico', 'Mexico')
  
  # fathers
  cat(sprintf("Processing number of children of fathers\n"))
  data_f = read.csv(paste0('data/Mexico/mexico_fertility_m_all.csv'))
  data_f$fertility_rate <- data_f$fertility_rate/1000
  data_f$date = data_f$year
  data_f$age = as.character(data_f$age)
  process_children_father_65_plus("Mexico", data_f)
  is_child_mortality_needed = 1
  add_child_mortality(is_child_mortality_needed, "Mexico")
  
  # mothers
  cat(sprintf("Processing number of children of mothers\n"))
  data = read.csv(paste0('data/Mexico/mexico_fertility_f.csv'))
  data_f = copy(data)
  data_f$fertility_rate = data_f$fertility_rate/1000
  d_2019 = data_f[which(data_f$date == '2019'),]
  d_2020 = copy(d_2019)
  d_2020$date = '2020'
  data_f = rbind(data_f, d_2020)
  process_children_all("Mexico", is_child_mortality_needed, data_f)
  
  process_fertility_plots("Mexico")
}

# Nigeria
process_number_children_nigeria <- function(){
  # fathers
  cat(sprintf("Processing number of children of fathers\n"))

  data = readRDS('data/Nigeria/male_fertility.RDS')
  data$y2019 = data$y2018
  data$y2020 = data$y2018
  data = reshape2::melt(data, id.vars = c('ages'), 
                        variable.name = 'year', value.name = 'fertility_rate')
  setnames(data, 'ages', 'age')
  data$age = as.character(data$age)
  data$age = gsub('[(]', '', data$age)
  data$age = gsub('[]]', '', data$age)
  data$year = as.character(data$year)
  data$year = gsub('y', '', data$year)
  data$fertility_rate[which(is.na(data$fertility_rate))] = 0
  data$age = unlist(lapply(data$age,function(x){ ifelse(x != '80+', paste0(strsplit(x, '-')[[1]][1], '-', 
                                                                           as.numeric(strsplit(x, '-')[[1]][2])-1), x)}))
  data$fertility_rate[data$age == "80+"] = 0
  data_f = copy(data)
  data_f$date = data_f$year
  data_f$gender = 'M'
  write_csv(data_f, path = paste0('data/Nigeria/nigeria_fertility_m_all.csv'))
  process_children_father_80_plus("Nigeria", data_f)
  is_child_mortality_needed = 0
  add_child_mortality(is_child_mortality_needed, "Nigeria")
  
  # mothers
  cat(sprintf("Processing number of children of mothers\n"))
  data = readRDS('data/Nigeria/female_fertility.RDS')
  
  data$y2019 = data$y2018
  data$y2020 = data$y2018
  data = reshape2::melt(data, id.vars = c('ages'), 
                        variable.name = 'year', value.name = 'fertility_rate')
  setnames(data, 'ages', 'age')
  data$age = as.character(data$age)
  data$age = gsub('[(]', '', data$age)
  data$age = gsub('[]]', '', data$age)
  data$year = as.character(data$year)
  data$year = gsub('y', '', data$year)
  data$fertility_rate[which(is.na(data$fertility_rate))] = 0
  data$age = unlist(lapply(data$age,function(x){ ifelse(x != '80+', paste0(strsplit(x, '-')[[1]][1], '-', 
                                                                           as.numeric(strsplit(x, '-')[[1]][2])-1), x)}))
  data_f = copy(data)
  data_f$date = data_f$year
  data_f$gender = 'F'
  write_csv(data_f, path = paste0('data/Nigeria/nigeria_fertility_f.csv'))
  is_child_mortality_needed = 0
  process_children_all("Nigeria", is_child_mortality_needed, data_f)

  process_fertility_plots("Nigeria")
}

# Peru
process_number_children_peru <- function(){
  # fathers
  cat(sprintf("Processing number of children of fathers\n"))
  data = readRDS('data/Peru/male_fertility.RDS')
  data$y2013 = data$y2012
  data$y2014 = data$y2012
  data$y2015 = data$y2012
  data$y2016 = data$y2015
  data$y2017 = data$y2016
  data$y2018 = data$y2016
  data$y2019 = data$y2016
  data$y2020 = data$y2016
  data = reshape2::melt(data, id.vars = c('ages'), 
                        variable.name = 'year', value.name = 'fertility_rate')
  setnames(data, 'ages', 'age')
  data$age = as.character(data$age)
  data$age = gsub('[(]', '', data$age)
  data$age = gsub('[]]', '', data$age)
  data$year = as.character(data$year)
  data$year = gsub('y', '', data$year)
  data$fertility_rate[which(is.na(data$fertility_rate))] = 0
  data$age = unlist(lapply(data$age,function(x){ ifelse(x != '80+', paste0(strsplit(x, '-')[[1]][1], '-', 
                                                                           as.numeric(strsplit(x, '-')[[1]][2])-1), x)}))
  data$fertility_rate[data$age == "80+"] = 0
  data_f = copy(data)
  data_f$date = data_f$year
  data_f$gender = 'M'  
  write_csv(data_f, path = paste0('data/Peru/peru_fertility_m_all.csv'))
  process_children_father_80_plus("Peru", data_f)
  is_child_mortality_needed = 0
  add_child_mortality(is_child_mortality_needed, "Peru")
  
  # mothers
  cat(sprintf("Processing number of children of mothers\n"))
  data = readRDS('data/Peru/female_fertility.RDS')
  data$y2013 = data$y2012
  data$y2014 = data$y2012
  data$y2015 = data$y2012
  data$y2016 = data$y2015
  data$y2017 = data$y2016
  data$y2018 = data$y2016
  data$y2019 = data$y2016
  data$y2020 = data$y2016
  data = reshape2::melt(data, id.vars = c('ages'), 
                        variable.name = 'year', value.name = 'fertility_rate')
  setnames(data, 'ages', 'age')
  data$age = as.character(data$age)
  data$age = gsub('[(]', '', data$age)
  data$age = gsub('[]]', '', data$age)
  data$year = as.character(data$year)
  data$year = gsub('y', '', data$year)
  data$fertility_rate[which(is.na(data$fertility_rate))] = 0
  data$age = unlist(lapply(data$age,function(x){ ifelse(x != '80+', paste0(strsplit(x, '-')[[1]][1], '-', 
                                                                           as.numeric(strsplit(x, '-')[[1]][2])-1), x)}))
  data_f = copy(data)
  data_f$date = data_f$year
  data_f$gender = 'F'
  write_csv(data_f, path = paste0('data/Peru/peru_fertility_f.csv'))
  is_child_mortality_needed = 0
  process_children_all("Peru", is_child_mortality_needed, data_f)
  
  process_fertility_plots("Peru")
}

# Philippines
process_number_children_philippines <- function(){
  # fertility
  cat(sprintf("Processing Fertility rates\n"))
  process_philippines_fertility()
  # child mortality
  cat(sprintf("Processing child mortality rates\n"))
  process_child_mortality('Philippines', 'Philippines')
  
  # fathers
  cat(sprintf("Processing number of children of fathers\n"))
  data_f = read.csv(paste0('data/Philippines/philippines_fertility_m_all.csv'))
  data_f$fertility_rate <- data_f$fertility_rate/1000
  data_f$date = data_f$year
  data_f$age = as.character(data_f$age)
  process_children_father_65_plus("Philippines", data_f)
  is_child_mortality_needed = 1
  add_child_mortality(is_child_mortality_needed, "Philippines")
  
  # mothers
  cat(sprintf("Processing number of children of mothers\n"))
  data = read.csv(paste0('data/Philippines/philippines_fertility_f.csv'))
  data_f = copy(data)
  data_f$fertility_rate = data_f$fertility_rate/1000
  d_2019 = data_f[which(data_f$date == '2019'),]
  d_2020 = copy(d_2019)
  d_2020$date = '2020'
  data_f = rbind(data_f, d_2020)
  process_children_all("Philippines", is_child_mortality_needed, data_f)
  
  process_fertility_plots("Philippines")
}

# Poland
process_number_children_poland <- function(){
  # fertility
  cat(sprintf("Processing Fertility rates\n"))
  process_poland_fertility()
  # child mortality
  cat(sprintf("Processing child mortality rates\n"))
  process_child_mortality('Poland', 'Poland')
  
  # fathers
  cat(sprintf("Processing number of children of fathers\n"))
  data_f = read.csv(paste0('data/Poland/poland_fertility_m_all.csv'))
  data_f$fertility_rate <- data_f$fertility_rate/1000
  data_f$date = data_f$year
  data_f$age = as.character(data_f$age)
  process_children_father_50_plus("Poland", data_f)
  is_child_mortality_needed = 1
  add_child_mortality(is_child_mortality_needed, "Poland")
  
  # mothers
  cat(sprintf("Processing number of children of mothers\n"))
  data = read.csv(paste0('data/Poland/poland_fertility_f.csv'))
  data_f = copy(data)
  data_f$fertility_rate = data_f$fertility_rate/1000
  d_2019 = data_f[which(data_f$date == '2019'),]
  d_2020 = copy(d_2019)
  d_2020$date = '2020'
  data_f = rbind(data_f, d_2020)
  process_children_all("Poland", is_child_mortality_needed, data_f)
  
  process_fertility_plots("Poland")
}

# Russia
process_number_children_russia <- function(){
  # fertility
  cat(sprintf("Processing Fertility rates\n"))
  process_russia_fertility()
  # child mortality
  cat(sprintf("Processing child mortality rates\n"))
  process_child_mortality('Russia', 'russian_federation')

  # fathers
  cat(sprintf("Processing number of children of fathers\n"))
  data_f = read.csv(paste0('data/Russia/russia_fertility_m_all.csv'))
  data_f$fertility_rate <- data_f$fertility_rate/1000
  data_f$date = data_f$year
  data_f$age = as.character(data_f$age)
  process_children_father_60_plus('Russia', data_f)
  is_child_mortality_needed = 1
  add_child_mortality(is_child_mortality_needed, 'Russia')
  
  # mothers
  cat(sprintf("Processing number of children of mothers\n"))
  country = 'Russia'
  data = read.csv(paste0('data/Russia/russia_fertility_f.csv'))
  data_f = copy(data)
  data_f$fertility_rate = data_f$fertility_rate/1000
  d_2019 = data_f[which(data_f$date == '2019'),]
  d_2020 = copy(d_2019)
  d_2020$date = '2020'
  data_f = rbind(data_f, d_2020)
  process_children_all("Russia", is_child_mortality_needed, data_f)
  
  process_fertility_plots('Russia')
}

# Spain
process_number_children_spain <- function(){
  cat(sprintf("Processing Fertility rates\n"))
  process_spain_fertility()
  # child mortality
  cat(sprintf("Processing child mortality rates\n"))
  process_child_mortality('Spain', 'Spain')
  
  # fathers
  cat(sprintf("Processing number of children of fathers\n"))
  data_f = read.csv(paste0('data/Spain/spain_fertility_m_all.csv'))
  data_f$fertility_rate <- data_f$fertility_rate/1000
  data_f$date = data_f$year
  data_f$age = as.character(data_f$age)
  process_children_father_65_plus("Spain", data_f)
  is_child_mortality_needed = 1
  add_child_mortality(is_child_mortality_needed, "Spain")
  
  # mothers
  cat(sprintf("Processing number of children of mothers\n"))
  data = read.csv(paste0('data/Spain/spain_fertility_f.csv'))
  data_f = copy(data)
  data_f$fertility_rate = data_f$fertility_rate/1000
  d_2019 = data_f[which(data_f$date == '2019'),]
  d_2020 = copy(d_2019)
  d_2020$date = '2020'
  data_f = rbind(data_f, d_2020)
  process_children_all("Spain", is_child_mortality_needed, data_f)
  
  process_fertility_plots("Spain")
}

# South Africa
process_number_children_south_africa <- function(){
  # fathers
  cat(sprintf("Processing number of children of fathers\n"))
  data = readRDS('data/SouthAfrica/male_fertility.RDS')
  data$y2017 = data$y2016
  data$y2018 = data$y2016
  data$y2019 = data$y2016
  data$y2020 = data$y2016
  data = reshape2::melt(data, id.vars = c('ages'), 
                        variable.name = 'year', value.name = 'fertility_rate')
  setnames(data, 'ages', 'age')
  data$age = as.character(data$age)
  data$age = gsub('[(]', '', data$age)
  data$age = gsub('[]]', '', data$age)
  data$year = as.character(data$year)
  data$year = gsub('y', '', data$year)
  data$fertility_rate[which(is.na(data$fertility_rate))] = 0
  data$age = unlist(lapply(data$age,function(x){ ifelse(x != '80+', paste0(strsplit(x, '-')[[1]][1], '-', 
                                                                           as.numeric(strsplit(x, '-')[[1]][2])-1), x)}))
  data$fertility_rate[data$age == "80+"] = 0
  data_f = copy(data)
  data_f$date = data_f$year
  data_f$gender = 'M'
  write_csv(data_f, path = paste0('data/SouthAfrica/southafrica_fertility_m_all.csv'))
  process_children_father_80_plus('SouthAfrica', data_f)
  is_child_mortality_needed = 0
  add_child_mortality(is_child_mortality_needed, "SouthAfrica")
  
  # mothers
  cat(sprintf("Processing number of children of mothers\n"))
  data = readRDS('data/SouthAfrica/female_fertility.RDS')
  
  data$y2017 = data$y2016
  data$y2018 = data$y2016
  data$y2019 = data$y2016
  data$y2020 = data$y2016
  data = reshape2::melt(data, id.vars = c('ages'), 
                        variable.name = 'year', value.name = 'fertility_rate')
  setnames(data, 'ages', 'age')
  data$age = as.character(data$age)
  data$age = gsub('[(]', '', data$age)
  data$age = gsub('[]]', '', data$age)
  data$year = as.character(data$year)
  data$year = gsub('y', '', data$year)
  data = data %>% filter(!is.na(fertility_rate))
  data$age = unlist(lapply(data$age,function(x){ paste0(strsplit(x, '-')[[1]][1], '-', 
                                                        as.numeric(strsplit(x, '-')[[1]][2])-1)}))
  data_f = copy(data)
  data_f$date = data_f$year
  data_f$gender = 'F'
  write_csv(data_f, path = paste0('data/SouthAfrica/southafrica_fertility_f.csv'))
  is_child_mortality_needed = 0
  process_children_all("SouthAfrica", is_child_mortality_needed, data_f)
  
  process_fertility_plots('SouthAfrica')
}

# USA
process_number_children_usa <- function(){
  cat(sprintf("Processing Fertility rates\n"))
  process_usa_fertility()

  # child mortality
  cat(sprintf("Processing child mortality rates\n"))
  process_child_mortality('USA', 'United States of America')
  
  # fathers
  cat(sprintf("Processing number of children of fathers\n"))
  data_f = read.csv(paste0('data/USA/usa_fertility_m_all.csv'))
  data_f$fertility_rate <- data_f$fertility_rate/1000
  data_f$date = data_f$year
  data_f$age = as.character(data_f$age)
  process_children_father_55_plus("USA", data_f)
  is_child_mortality_needed = 1
  add_child_mortality(is_child_mortality_needed, "USA")
  
  # mothers
  cat(sprintf("Processing number of children of mothers\n"))
  data = read.csv(paste0('data/USA/usa_fertility_f.csv'))
  data_f = copy(data)
  data_f$fertility_rate = data_f$fertility_rate/1000
  d_2019 = data_f[which(data_f$date == '2019'),]
  d_2020 = copy(d_2019)
  d_2020$date = '2020'
  data_f = rbind(data_f, d_2020)
  process_children_all("USA", is_child_mortality_needed, data_f)
  
  process_fertility_plots("USA")
}

# Zimbabwe
process_number_children_zimbabwe<- function(){
  # fathers
  cat(sprintf("Processing number of children of fathers\n"))
  
  data = readRDS('data/Zimbabwe/male_fertility.RDS')
  data$y2016 = data$y2015
  data$y2017 = data$y2015
  data$y2018 = data$y2015
  data$y2019 = data$y2015
  data$y2020 = data$y2015
  data = reshape2::melt(data, id.vars = c('ages'), 
                        variable.name = 'year', value.name = 'fertility_rate')
  setnames(data, 'ages', 'age')
  data$age = as.character(data$age)
  data$age = gsub('[(]', '', data$age)
  data$age = gsub('[]]', '', data$age)
  data$year = as.character(data$year)
  data$year = gsub('y', '', data$year)
  data$fertility_rate[which(is.na(data$fertility_rate))] = 0
  data$age = unlist(lapply(data$age,function(x){ ifelse(x != '80+', paste0(strsplit(x, '-')[[1]][1], '-', 
                                                                           as.numeric(strsplit(x, '-')[[1]][2])-1), x)}))
  data$fertility_rate[data$age == "80+"] = 0
  data_f = copy(data)
  data_f$date = data_f$year
  data_f$gender = 'M'
  write_csv(data_f, path = paste0('data/Zimbabwe/zimbabwe_fertility_m_all.csv'))
  process_children_father_80_plus("Zimbabwe", data_f)
  is_child_mortality_needed = 0
  add_child_mortality(is_child_mortality_needed, "Zimbabwe")
  
  # mothers
  cat(sprintf("Processing number of children of mothers\n"))
  data = readRDS('data/Zimbabwe/female_fertility.RDS')
  data$y2016 = data$y2015
  data$y2017 = data$y2015
  data$y2018 = data$y2015
  data$y2019 = data$y2015
  data$y2020 = data$y2015
  data = reshape2::melt(data, id.vars = c('ages'), 
                        variable.name = 'year', value.name = 'fertility_rate')
  setnames(data, 'ages', 'age')
  data$age = as.character(data$age)
  data$age = gsub('[(]', '', data$age)
  data$age = gsub('[]]', '', data$age)
  data$year = as.character(data$year)
  data$year = gsub('y', '', data$year)
  data$fertility_rate[which(is.na(data$fertility_rate))] = 0
  data$age = unlist(lapply(data$age,function(x){ ifelse(x != '80+', paste0(strsplit(x, '-')[[1]][1], '-', 
                                                                           as.numeric(strsplit(x, '-')[[1]][2])-1), x)}))
  data_f = copy(data)
  data_f$date = data_f$year
  data_f$gender = 'F'
  write_csv(data_f, path = paste0('data/Zimbabwe/zimbabwe_fertility_f.csv'))
  is_child_mortality_needed = 0
  process_children_all("Zimbabwe", is_child_mortality_needed, data_f)
  
  process_fertility_plots("Zimbabwe")
  
}
