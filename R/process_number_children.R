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
  process_child_mortality('france', 'France')
  
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
  process_child_mortality('germany', 'Germany')
  
  # fathers
  cat(sprintf("Processing number of children of fathers\n"))
  data_f = read.csv(paste0('data/fertility/germany_fertility_m_all.csv'))
  data_f$fertility_rate <- data_f$fertility_rate/1000
  data_f$date = data_f$year
  data_f$age = as.character(data_f$age)
  process_children_father_65_plus("germany", data_f)
  is_child_mortality_needed = 1
  add_child_mortality(is_child_mortality_needed, "germany")
  
  # mothers
  cat(sprintf("Processing number of children of mothers\n"))
  data = read.csv(paste0('data/fertility/germany_fertility_f.csv'))
  data_f = copy(data)
  data_f$fertility_rate = data_f$fertility_rate/1000
  d_2019 = data_f[which(data_f$date == '2019'),]
  d_2020 = copy(d_2019)
  d_2020$date = '2020'
  data_f = rbind(data_f, d_2020)
  process_children_all("germany", is_child_mortality_needed, data_f)
  
  cat(sprintf("Processing fertility rates\n"))
  process_fertility_plots("germany")
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
  write_csv(data_f, path = paste0('data/fertility/india_fertility_m_all.csv'))
  process_children_father_80_plus("india", data_f)
  is_child_mortality_needed = 0
  add_child_mortality(is_child_mortality_needed, "india")
  
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
  write_csv(data_f, path = paste0('data/fertility/india_fertility_f.csv'))
  process_children_all("india", is_child_mortality_needed, data_f)
  
  process_fertility_plots("india")
}

# Iran
process_number_children_iran <- function(){
  # fertility
  cat(sprintf("Processing Fertility rates\n"))
  process_iran_fertility()
  # child mortality
  cat(sprintf("Processing child mortality rates\n"))
  process_child_mortality('iran', 'Iran')
  
  # fathers
  cat(sprintf("Processing number of children of fathers\n"))
  data_f = read.csv(paste0('data/fertility/iran_fertility_m_all.csv'))
  data_f$fertility_rate = data_f$fertility_rate/1000
  data_f$age = as.character(data_f$age)
  process_children_father_55_plus("iran", data_f)
  is_child_mortality_needed = 1
  add_child_mortality(is_child_mortality_needed, "iran")
  
  # mothers
  cat(sprintf("Processing number of children of mothers\n"))
  data = read.csv(paste0('data/fertility/iran_fertility_f.csv'))
  data_f = copy(data)
  data_f$fertility_rate = data_f$fertility_rate/1000
  d_2019 = data_f[which(data_f$date == '2019'),]
  d_2020 = copy(d_2019)
  d_2020$date = '2020'
  data_f = rbind(data_f, d_2020)
  process_children_all("iran", is_child_mortality_needed, data_f)
  
  process_fertility_plots("iran")
}


# Italy
process_number_children_italy <- function(){
  # fertility
  cat(sprintf("Processing Fertility rates\n"))
  process_italy_fertility()
  # child mortality
  cat(sprintf("Processing child mortality rates\n"))
  process_child_mortality('italy', 'Italy')
  
  # fathers
  cat(sprintf("Processing number of children of fathers\n"))
  data_f = read.csv(paste0('data/fertility/italy_fertility_m_all.csv'))
  data_f$fertility_rate <- data_f$fertility_rate/1000
  data_f$date = data_f$year
  data_f$age = as.character(data_f$age)
  process_children_father_65_plus("italy", data_f)
  is_child_mortality_needed = 1
  add_child_mortality(is_child_mortality_needed, "italy")
  
  # mothers
  cat(sprintf("Processing number of children of mothers\n"))
  data = read.csv(paste0('data/fertility/italy_fertility_f.csv'))
  data_f = copy(data)
  data_f$fertility_rate = data_f$fertility_rate/1000
  d_2019 = data_f[which(data_f$date == '2019'),]
  d_2020 = copy(d_2019)
  d_2020$date = '2020'
  data_f = rbind(data_f, d_2020)
  process_children_all("italy", is_child_mortality_needed, data_f)

  process_fertility_plots("italy")
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
  write_csv(data_f, path = paste0('data/fertility/kenya_fertility_m_all.csv'))
  process_children_father_80_plus("kenya", data_f)
  is_child_mortality_needed = 0
  add_child_mortality(is_child_mortality_needed, "kenya")
  
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
  write_csv(data_f, path = paste0('data/fertility/kenya_fertility_f.csv'))
  is_child_mortality_needed = 0
  process_children_all("kenya", is_child_mortality_needed, data_f)

  process_fertility_plots("kenya")
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
  write_csv(data_f, path = paste0('data/fertility/malawi_fertility_m_all.csv'))
  process_children_father_80_plus("malawi", data_f)
  is_child_mortality_needed = 0
  add_child_mortality(is_child_mortality_needed, "malawi")
  
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
  write_csv(data_f, path = paste0('data/fertility/malawi_fertility_f.csv'))
  is_child_mortality_needed = 0
  process_children_all("malawi", is_child_mortality_needed, data_f)
  
  process_fertility_plots("malawi")
  
}

# Mexico
process_number_children_mexico <- function(){
  # fertility
  cat(sprintf("Processing Fertility rates\n"))
  process_mexico_fertility()
  # child mortality
  cat(sprintf("Processing child mortality rates\n"))
  process_child_mortality('mexico', 'Mexico')
  
  # fathers
  cat(sprintf("Processing number of children of fathers\n"))
  data_f = read.csv(paste0('data/fertility/mexico_fertility_m_all.csv'))
  data_f$fertility_rate <- data_f$fertility_rate/1000
  data_f$date = data_f$year
  data_f$age = as.character(data_f$age)
  process_children_father_65_plus("mexico", data_f)
  is_child_mortality_needed = 1
  add_child_mortality(is_child_mortality_needed, "mexico")
  
  # mothers
  cat(sprintf("Processing number of children of mothers\n"))
  data = read.csv(paste0('data/fertility/mexico_fertility_f.csv'))
  data_f = copy(data)
  data_f$fertility_rate = data_f$fertility_rate/1000
  d_2019 = data_f[which(data_f$date == '2019'),]
  d_2020 = copy(d_2019)
  d_2020$date = '2020'
  data_f = rbind(data_f, d_2020)
  process_children_all("mexico", is_child_mortality_needed, data_f)
  
  process_fertility_plots("mexico")
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
  write_csv(data_f, path = paste0('data/fertility/nigeria_fertility_m_all.csv'))
  process_children_father_80_plus("nigeria", data_f)
  is_child_mortality_needed = 0
  add_child_mortality(is_child_mortality_needed, "nigeria")
  
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
  write_csv(data_f, path = paste0('data/fertility/nigeria_fertility_f.csv'))
  is_child_mortality_needed = 0
  process_children_all("nigeria", is_child_mortality_needed, data_f)

  process_fertility_plots("nigeria")
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
  write_csv(data_f, path = paste0('data/fertility/peru_fertility_m_all.csv'))
  process_children_father_80_plus("peru", data_f)
  is_child_mortality_needed = 0
  add_child_mortality(is_child_mortality_needed, "peru")
  
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
  write_csv(data_f, path = paste0('data/fertility/peru_fertility_f.csv'))
  is_child_mortality_needed = 0
  process_children_all("peru", is_child_mortality_needed, data_f)
  
  process_fertility_plots("peru")
}

# Philippines
process_number_children_philippines <- function(){
  # fertility
  cat(sprintf("Processing Fertility rates\n"))
  process_philippines_fertility()
  # child mortality
  cat(sprintf("Processing child mortality rates\n"))
  process_child_mortality('philippines', 'Philippines')
  
  # fathers
  cat(sprintf("Processing number of children of fathers\n"))
  data_f = read.csv(paste0('data/fertility/philippines_fertility_m_all.csv'))
  data_f$fertility_rate <- data_f$fertility_rate/1000
  data_f$date = data_f$year
  data_f$age = as.character(data_f$age)
  process_children_father_65_plus("philippines", data_f)
  is_child_mortality_needed = 1
  add_child_mortality(is_child_mortality_needed, "philippines")
  
  # mothers
  cat(sprintf("Processing number of children of mothers\n"))
  data = read.csv(paste0('data/fertility/philippines_fertility_f.csv'))
  data_f = copy(data)
  data_f$fertility_rate = data_f$fertility_rate/1000
  d_2019 = data_f[which(data_f$date == '2019'),]
  d_2020 = copy(d_2019)
  d_2020$date = '2020'
  data_f = rbind(data_f, d_2020)
  process_children_all("philippines", is_child_mortality_needed, data_f)
  
  process_fertility_plots("philippines")
}

# Poland
process_number_children_poland <- function(){
  # fertility
  cat(sprintf("Processing Fertility rates\n"))
  process_poland_fertility()
  # child mortality
  cat(sprintf("Processing child mortality rates\n"))
  process_child_mortality('poland', 'Poland')
  
  # fathers
  cat(sprintf("Processing number of children of fathers\n"))
  data_f = read.csv(paste0('data/fertility/poland_fertility_m_all.csv'))
  data_f$fertility_rate <- data_f$fertility_rate/1000
  data_f$date = data_f$year
  data_f$age = as.character(data_f$age)
  process_children_father_50_plus("poland", data_f)
  is_child_mortality_needed = 1
  add_child_mortality(is_child_mortality_needed, "poland")
  
  # mothers
  cat(sprintf("Processing number of children of mothers\n"))
  data = read.csv(paste0('data/fertility/poland_fertility_f.csv'))
  data_f = copy(data)
  data_f$fertility_rate = data_f$fertility_rate/1000
  d_2019 = data_f[which(data_f$date == '2019'),]
  d_2020 = copy(d_2019)
  d_2020$date = '2020'
  data_f = rbind(data_f, d_2020)
  process_children_all("poland", is_child_mortality_needed, data_f)
  
  process_fertility_plots("poland")
}

# Russia
process_number_children_russia <- function(){
  # fertility
  cat(sprintf("Processing Fertility rates\n"))
  process_russia_fertility()
  # child mortality
  cat(sprintf("Processing child mortality rates\n"))
  process_child_mortality('russian_federation', 'Russian_federation')

  # fathers
  cat(sprintf("Processing number of children of fathers\n"))
  data_f = read.csv(paste0('data/fertility/russian_federation_fertility_m_all.csv'))
  data_f$fertility_rate <- data_f$fertility_rate/1000
  data_f$date = data_f$year
  data_f$age = as.character(data_f$age)
  process_children_father_60_plus('russian_federation', data_f)
  is_child_mortality_needed = 1
  add_child_mortality(is_child_mortality_needed, 'russian_federation')
  
  # mothers
  cat(sprintf("Processing number of children of mothers\n"))
  country = 'russian_federation'
  data = read.csv(paste0('data/fertility/russian_federation_fertility_f.csv'))
  data_f = copy(data)
  data_f$fertility_rate = data_f$fertility_rate/1000
  d_2019 = data_f[which(data_f$date == '2019'),]
  d_2020 = copy(d_2019)
  d_2020$date = '2020'
  data_f = rbind(data_f, d_2020)
  process_children_all("russian_federation", is_child_mortality_needed, data_f)
  
  process_fertility_plots('russian_federation')
}

# Spain
process_number_children_spain <- function(){
  cat(sprintf("Processing Fertility rates\n"))
  process_spain_fertility()
  # child mortality
  cat(sprintf("Processing child mortality rates\n"))
  process_child_mortality('spain', 'Spain')
  
  # fathers
  cat(sprintf("Processing number of children of fathers\n"))
  data_f = read.csv(paste0('data/fertility/spain_fertility_m_all.csv'))
  data_f$fertility_rate <- data_f$fertility_rate/1000
  data_f$date = data_f$year
  data_f$age = as.character(data_f$age)
  process_children_father_65_plus("spain", data_f)
  is_child_mortality_needed = 1
  add_child_mortality(is_child_mortality_needed, "spain")
  
  # mothers
  cat(sprintf("Processing number of children of mothers\n"))
  data = read.csv(paste0('data/fertility/spain_fertility_f.csv'))
  data_f = copy(data)
  data_f$fertility_rate = data_f$fertility_rate/1000
  d_2019 = data_f[which(data_f$date == '2019'),]
  d_2020 = copy(d_2019)
  d_2020$date = '2020'
  data_f = rbind(data_f, d_2020)
  process_children_all("spain", is_child_mortality_needed, data_f)
  
  process_fertility_plots("spain")
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
  write_csv(data_f, path = paste0('data/fertility/south_africa_fertility_m_all.csv'))
  process_children_father_80_plus('south_africa', data_f)
  is_child_mortality_needed = 0
  add_child_mortality(is_child_mortality_needed, "south_africa")
  
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
  write_csv(data_f, path = paste0('data/fertility/south_africa_fertility_f.csv'))
  is_child_mortality_needed = 0
  process_children_all("south_africa", is_child_mortality_needed, data_f)
  
  process_fertility_plots('south_africa')
}

# Uganda
process_number_children_uganda <- function(){
  # fathers
  cat(sprintf("Processing number of children of fathers\n"))
  
  data = readRDS('data/Uganda/male_fertility.RDS')
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
  data_f$gender = 'M'
  write_csv(data_f, path = paste0('data/fertility/uganda_fertility_m_all.csv'))
  process_children_father_80_plus("uganda", data_f)
  is_child_mortality_needed = 0
  add_child_mortality(is_child_mortality_needed, "uganda")
  
  # mothers
  cat(sprintf("Processing number of children of mothers\n"))
  data = readRDS('data/Uganda/female_fertility.RDS')
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
  write_csv(data_f, path = paste0('data/fertility/uganda_fertility_f.csv'))
  is_child_mortality_needed = 0
  process_children_all("uganda", is_child_mortality_needed, data_f)
  
  process_fertility_plots("uganda")
  
}

# USA
process_number_children_usa <- function(high_fertility = TRUE){
  cat(sprintf("Processing Fertility rates\n"))
  if (high_fertility == TRUE){
    process_usa_fertility()
  }else {
    process_usa_fertility_2()
  }

  # child mortality
  cat(sprintf("Processing child mortality rates\n"))
  process_child_mortality('usa', 'United States of America')
  
  # fathers
  cat(sprintf("Processing number of children of fathers\n"))
  data_f = read.csv(paste0('data/fertility/usa_fertility_m_all.csv'))
  data_f$fertility_rate <- data_f$fertility_rate/1000
  data_f$date = data_f$year
  data_f$age = as.character(data_f$age)
  process_children_father_55_plus("usa", data_f)
  is_child_mortality_needed = 1
  add_child_mortality(is_child_mortality_needed, "usa")
  
  # mothers
  cat(sprintf("Processing number of children of mothers\n"))
  data = read.csv(paste0('data/fertility/usa_fertility_f.csv'))
  data_f = copy(data)
  data_f$fertility_rate = data_f$fertility_rate/1000
  d_2019 = data_f[which(data_f$date == '2019'),]
  d_2020 = copy(d_2019)
  d_2020$date = '2020'
  data_f = rbind(data_f, d_2020)
  process_children_all("usa", is_child_mortality_needed, data_f)
  
  process_fertility_plots("usa")
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
  write_csv(data_f, path = paste0('data/fertility/zimbabwe_fertility_m_all.csv'))
  process_children_father_80_plus("zimbabwe", data_f)
  is_child_mortality_needed = 0
  add_child_mortality(is_child_mortality_needed, "zimbabwe")
  
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
  write_csv(data_f, path = paste0('data/fertility/zimbabwe_fertility_f.csv'))
  is_child_mortality_needed = 0
  process_children_all("zimbabwe", is_child_mortality_needed, data_f)
  
  process_fertility_plots("zimbabwe")
  
}
