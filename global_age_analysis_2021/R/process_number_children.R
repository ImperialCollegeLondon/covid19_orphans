source(file.path("global_age_analysis_2021/R","process_children_parents.R"))
source(file.path("global_age_analysis_2021/R","process_fertility.R"))
source(file.path("global_age_analysis_2021/R","process_child_mortality.R"))
source(file.path("global_age_analysis_2021/R","sample_fertility.R"))

# Brazil
process_number_children_brazil <- function(uncertainty = FALSE, seed = NA){
  # Calculate number of children from different aged fathers
  cat(sprintf("Processing number of children of fathers\n"))
  data = readRDS('global_age_analysis_2021/data/Brazil/male_fertility.RDS')
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
  
  if (uncertainty == TRUE){
    data_f = sample_fertility(data_f, "Brazil", "Male", seed)
  }
  
  if (uncertainty == FALSE){
    write.csv(data_f, file = paste0('global_age_analysis_2021/data/Brazil/brazil_fertility_m_all.csv'), row.names=FALSE)
  } else {
    write.csv(data_f, file = paste0('global_age_analysis_2021/data/Brazil/brazil_fertility_m_all_un.csv'), row.names=FALSE)
  }
  
  process_children_father_80_plus("Brazil", data_f, uncertainty = uncertainty)
  is_child_mortality_needed = 0
  add_child_mortality(is_child_mortality_needed, "Brazil", uncertainty = uncertainty)
 
  # Calculate number of children from different aged mothers
  cat(sprintf("Processing number of children of mothers\n"))
  data = readRDS('global_age_analysis_2021/data/Brazil/female_fertility.RDS')

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
  
  if (uncertainty == TRUE){
    data_f = sample_fertility(data_f, "Brazil", "Female", seed)
  }
  
  if (uncertainty == FALSE){
    write.csv(data_f, file = paste0('global_age_analysis_2021/data/Brazil/brazil_fertility_f.csv'), row.names=FALSE)
  } else {
    write.csv(data_f, file = paste0('global_age_analysis_2021/data/Brazil/brazil_fertility_f_un.csv'), row.names=FALSE)
  }
 
  is_child_mortality_needed = 0
  process_children_all("Brazil", is_child_mortality_needed, data_f, uncertainty = uncertainty)
  
  if (uncertainty == FALSE){
    process_fertility_plots("Brazil")
  }
 
}

# Colombia
process_number_children_colombia <- function(uncertainty = FALSE, seed = NA){
  # Calculate number of children from different aged fathers
  cat(sprintf("Processing number of children of fathers\n"))
  data = readRDS('global_age_analysis_2021/data/Colombia/male_fertility.RDS')
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
  
  data_summary = data %>% group_by(age) %>% summarise(rate = mean(fertility_rate))

  if (uncertainty == TRUE){
    data_f = sample_fertility(data_f, "Colombia", "Male", seed)
  }
  
  if (uncertainty == FALSE){
    write.csv(data_f, file = paste0('global_age_analysis_2021/data/Colombia/colombia_fertility_m_all.csv'), row.names=FALSE)
  } else {
    write.csv(data_f, file = paste0('global_age_analysis_2021/data/Colombia/colombia_fertility_m_all_un.csv'), row.names=FALSE)
  }
  
  process_children_father_80_plus("Colombia", data_f, uncertainty = uncertainty)
  is_child_mortality_needed = 0
  add_child_mortality(is_child_mortality_needed, 'Colombia', uncertainty = uncertainty)
  
  # Calculate number of children from different aged mothers
  cat(sprintf("Processing number of children of mothers\n"))
  data = readRDS('global_age_analysis_2021/data/Colombia/female_fertility.RDS')
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
  
  if (uncertainty == TRUE){
    data_f = sample_fertility(data_f, "Colombia", "Female", seed)
  }
  
  if (uncertainty == FALSE){
    write.csv(data_f, file = paste0('global_age_analysis_2021/data/Colombia/colombia_fertility_f.csv'), row.names=FALSE)
  } else {
    write.csv(data_f, file = paste0('global_age_analysis_2021/data/Colombia/colombia_fertility_f_un.csv'), row.names=FALSE)
  }
  
  is_child_mortality_needed = 0
  process_children_all("Colombia", is_child_mortality_needed, data_f, uncertainty = uncertainty)
  
  # Make fertility plots
  if (uncertainty == FALSE){
    process_fertility_plots("Colombia")
  }

}

# England and wales
process_number_children_england_wales <- function(uncertainty = FALSE, seed = NA){
  
  # Calculating fertility rates
  process_england_wales_fertility(uncertainty = uncertainty, seed)
  
  # Adapting fertility with infant mortality
  cat(sprintf("Processing child mortality rates\n"))
  process_children_mortality_england_wales()
  process_infant_mortality_england_wales()
  process_child_mortality_england_wales()
  
  # fathers
  cat(sprintf("Processing number of children of fathers\n"))
  if (uncertainty == FALSE){
    data_f = read.csv('global_age_analysis_2021/data/UK/england_wales_fertility_m.csv')
  } else {
    data_f = read.csv('global_age_analysis_2021/data/UK/england_wales_fertility_m_un.csv')
  }
  
  data_f$fertility_rate <- data_f$rate/1000
  data_f$date = data_f$year
  data_f$age = as.character(data_f$age)
  d_2019 = data_f[which(data_f$date == '2018'),]
  d_2019$date = '2019'
  d_2020 = copy(d_2019)
  d_2020$date = '2020'
  data_f = rbind(data_f, d_2019, d_2020)
  process_children_father_england_wales(data_f, uncertainty = uncertainty)
  is_child_mortality_needed = 1
  add_child_mortality(is_child_mortality_needed, "UK",  uncertainty = uncertainty)
  
  # mothers
  cat(sprintf("Processing number of children of mothers\n"))
  if (uncertainty == FALSE){
    data = read.csv('global_age_analysis_2021/data/UK/england_wales_fertility_f.csv')
  } else {
    data = read.csv('global_age_analysis_2021/data/UK/england_wales_fertility_f_un.csv')
  }

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
  process_children_all("UK", is_child_mortality_needed, data_f, uncertainty = uncertainty)
  
  if (uncertainty ==  FALSE){
    cat(sprintf("Processing fertility rates\n"))
    process_fertility_plots("england_wales")
  }

}

# France
process_number_children_france <- function(uncertainty = FALSE, seed = NA){
  # fertility
  cat(sprintf("Processing Fertility rates\n"))
  process_france_fertility(uncertainty = uncertainty, seed)

  # child mortality
  cat(sprintf("Processing child mortality rates\n"))
  process_child_mortality('France', 'France')
  
  # fathers
  cat(sprintf("Processing number of children of fathers\n"))
  if (uncertainty == FALSE){
    data_f = read.csv(paste0('global_age_analysis_2021/data/France/france_fertility_m_all.csv'))
  } else {
    data_f = read.csv(paste0('global_age_analysis_2021/data/France/france_fertility_m_all_un.csv'))
  }
  
  data_f$fertility_rate <- data_f$fertility_rate/1000
  data_f$date = data_f$year
  data_f$age = as.character(data_f$age)
  process_children_father_65_plus("France", data_f, uncertainty = uncertainty)
  is_child_mortality_needed = 1
  add_child_mortality(is_child_mortality_needed, "France", uncertainty = uncertainty)
  
  # mothers
  cat(sprintf("Processing number of children of mothers\n"))
  if (uncertainty == FALSE){
    data = read.csv(paste0('global_age_analysis_2021/data/France/france_fertility_f.csv'))
  } else {
    data = read.csv(paste0('global_age_analysis_2021/data/France/france_fertility_f_un.csv'))
  }

  data_f = copy(data)
  data_f$fertility_rate = data_f$fertility_rate/1000
  d_2019 = data_f[which(data_f$date == '2019'),]
  d_2020 = copy(d_2019)
  d_2020$date = '2020'
  data_f = rbind(data_f, d_2020)
  
  process_children_all("France", is_child_mortality_needed, data_f, uncertainty = uncertainty)

  process_fertility_plots("France")
}

# Germany
process_number_children_germany <- function(uncertainty = FALSE, seed = NA){
  # fertility
  cat(sprintf("Processing Fertility rates\n"))
  process_germany_fertility(uncertainty = uncertainty, seed)
  # child mortality
  cat(sprintf("Processing child mortality rates\n"))
  process_child_mortality('Germany', 'Germany')
  
  # fathers
  cat(sprintf("Processing number of children of fathers\n"))
  if (uncertainty == FALSE){
    data_f = read.csv(paste0('global_age_analysis_2021/data/Germany/germany_fertility_m_all.csv'))
  } else {
    data_f = read.csv(paste0('global_age_analysis_2021/data/Germany/germany_fertility_m_all_un.csv'))
  }

  data_f$fertility_rate <- data_f$fertility_rate/1000
  data_f$date = data_f$year
  data_f$age = as.character(data_f$age)
  process_children_father_65_plus("Germany", data_f, uncertainty = uncertainty)
  is_child_mortality_needed = 1
  add_child_mortality(is_child_mortality_needed, "Germany", uncertainty = uncertainty)
  
  # mothers
  cat(sprintf("Processing number of children of mothers\n"))
  if (uncertainty == FALSE){
    data = read.csv(paste0('global_age_analysis_2021/data/Germany/germany_fertility_f.csv'))
  } else {
    data = read.csv(paste0('global_age_analysis_2021/data/Germany/germany_fertility_f_un.csv'))
  }
  
  data_f = copy(data)
  data_f$fertility_rate = data_f$fertility_rate/1000
  d_2019 = data_f[which(data_f$date == '2019'),]
  d_2020 = copy(d_2019)
  d_2020$date = '2020'
  data_f = rbind(data_f, d_2020)
  process_children_all("Germany", is_child_mortality_needed, data_f, uncertainty = uncertainty)
  
  if (uncertainty == FALSE){
    cat(sprintf("Processing fertility rates\n"))
    process_fertility_plots("Germany")
  }

}

# India
process_number_children_india <- function(uncertainty = FALSE, seed = NA){
  cat(sprintf("Processing number of children of fathers\n"))
  data = readRDS('global_age_analysis_2021/data/India/male_fertility.RDS')
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
  
  if (uncertainty == TRUE){
    data_f = sample_fertility(data_f, "India", "Male", seed)
  }
  
  if (uncertainty == FALSE){
    write.csv(data_f, file = paste0('global_age_analysis_2021/data/India/india_fertility_m_all.csv'), row.names=FALSE)
  } else {
    write.csv(data_f, file = paste0('global_age_analysis_2021/data/India/india_fertility_m_all_un.csv'), row.names=FALSE)
  }
  
  process_children_father_80_plus("India", data_f, uncertainty = uncertainty)
  is_child_mortality_needed = 0
  add_child_mortality(is_child_mortality_needed, "India", uncertainty = uncertainty)
  
  # mothers
  cat(sprintf("Processing number of children of mothers\n"))
  data = readRDS('global_age_analysis_2021/data/India/female_fertility.RDS')
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
  data_f = data_f[which(data_f$age != "80+-NA" ),]
  
  if (uncertainty == TRUE){
    data_f = sample_fertility(data_f, "India", "Female", seed)
  }
  
  if (uncertainty == FALSE){
    write.csv(data_f, file = paste0('global_age_analysis_2021/data/India/india_fertility_f.csv'), row.names=FALSE)
  } else {
    write.csv(data_f, file = paste0('global_age_analysis_2021/data/India/india_fertility_f_un.csv'), row.names=FALSE)
  }

  process_children_all("India", is_child_mortality_needed, data_f, uncertainty = uncertainty)
  
  if (uncertainty == FALSE){
    process_fertility_plots("India")
  }

}

# Iran
process_number_children_iran <- function(uncertainty = FALSE, seed = NA){
  # fertility
  cat(sprintf("Processing Fertility rates\n"))
  process_iran_fertility(uncertainty = uncertainty, seed)
  # child mortality
  cat(sprintf("Processing child mortality rates\n"))
  process_child_mortality('Iran', 'Iran')
  
  # fathers
  cat(sprintf("Processing number of children of fathers\n"))
  if (uncertainty == FALSE){
    data_f = read.csv(paste0('global_age_analysis_2021/data/Iran/iran_fertility_m_all.csv'))
  } else {
    data_f = read.csv(paste0('global_age_analysis_2021/data/Iran/iran_fertility_m_all_un.csv'))
  }

  data_f$fertility_rate = data_f$fertility_rate/1000
  data_f$age = as.character(data_f$age)
  process_children_father_55_plus("Iran", data_f, uncertainty = uncertainty)
  is_child_mortality_needed = 1
  add_child_mortality(is_child_mortality_needed, "Iran", uncertainty = uncertainty)
  
  # mothers
  cat(sprintf("Processing number of children of mothers\n"))
  if (uncertainty == FALSE){
    data = read.csv(paste0('global_age_analysis_2021/data/Iran/iran_fertility_f.csv'))
  } else {
    data = read.csv(paste0('global_age_analysis_2021/data/Iran/iran_fertility_f_un.csv'))
  }

  data_f = copy(data)
  data_f$fertility_rate = data_f$fertility_rate/1000
  d_2019 = data_f[which(data_f$date == '2019'),]
  d_2020 = copy(d_2019)
  d_2020$date = '2020'
  data_f = rbind(data_f, d_2020)
  process_children_all("Iran", is_child_mortality_needed, data_f, uncertainty = uncertainty)
  
  if (uncertainty == FALSE){
    process_fertility_plots("Iran")
  }
  
}


# Italy
process_number_children_italy <- function(uncertainty = FALSE, seed = NA){
  # fertility
  cat(sprintf("Processing Fertility rates\n"))
  process_italy_fertility(uncertainty = uncertainty, seed)
  # child mortality
  cat(sprintf("Processing child mortality rates\n"))
  process_child_mortality('Italy', 'Italy')
  
  # fathers
  cat(sprintf("Processing number of children of fathers\n"))
  if (uncertainty == FALSE){
    data_f = read.csv(paste0('global_age_analysis_2021/data/Italy/italy_fertility_m_all.csv'))
  } else {
    data_f = read.csv(paste0('global_age_analysis_2021/data/Italy/italy_fertility_m_all_un.csv'))
  }

  data_f$fertility_rate <- data_f$fertility_rate/1000
  data_f$date = data_f$year
  data_f$age = as.character(data_f$age)
  process_children_father_65_plus("Italy", data_f, uncertainty = uncertainty)
  is_child_mortality_needed = 1
  add_child_mortality(is_child_mortality_needed, "Italy", uncertainty = uncertainty)
  
  # mothers
  cat(sprintf("Processing number of children of mothers\n"))
  if (uncertainty == FALSE){
    data = read.csv(paste0('global_age_analysis_2021/data/Italy/italy_fertility_f.csv'))
  } else {
    data = read.csv(paste0('global_age_analysis_2021/data/Italy/italy_fertility_f_un.csv'))
  }
  
  data_f = copy(data)
  data_f$fertility_rate = data_f$fertility_rate/1000
  d_2019 = data_f[which(data_f$date == '2019'),]
  d_2020 = copy(d_2019)
  d_2020$date = '2020'
  data_f = rbind(data_f, d_2020)
  process_children_all("Italy", is_child_mortality_needed, data_f, uncertainty = uncertainty)

  if (uncertainty == FALSE){
    process_fertility_plots("Italy")
  }
  
}

# Kenya
process_number_children_kenya <- function(uncertainty = FALSE, seed = NA){
  # fathers
  cat(sprintf("Processing number of children of fathers\n"))
  data = readRDS('global_age_analysis_2021/data/Kenya/male_fertility.RDS')
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
  
  if (uncertainty == TRUE){
    data_f = sample_fertility(data_f, "Kenya", "Male", seed)
  }
  
  if (uncertainty == FALSE){
    write.csv(data_f, file = paste0('global_age_analysis_2021/data/Kenya/kenya_fertility_m_all.csv'), row.names=FALSE)
  } else {
    write.csv(data_f, file = paste0('global_age_analysis_2021/data/Kenya/kenya_fertility_m_all_un.csv'), row.names=FALSE)
  }
  
  process_children_father_80_plus("Kenya", data_f, uncertainty = uncertainty)
  is_child_mortality_needed = 0
  add_child_mortality(is_child_mortality_needed, "Kenya", uncertainty = uncertainty)
  
  # mothers
  cat(sprintf("Processing number of children of mothers\n"))
  data = readRDS('global_age_analysis_2021/data/Kenya/female_fertility.RDS')
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
  
  if (uncertainty == TRUE){
    data_f = sample_fertility(data_f, "Kenya", "Female", seed)
  }
  
  if (uncertainty == FALSE){
    write.csv(data_f, file = paste0('global_age_analysis_2021/data/Kenya/kenya_fertility_f.csv'), row.names=FALSE)
  } else {
    write.csv(data_f, file = paste0('global_age_analysis_2021/data/Kenya/kenya_fertility_f_un.csv'), row.names=FALSE)
  }
  
  is_child_mortality_needed = 0
  process_children_all("Kenya", is_child_mortality_needed, data_f, uncertainty = uncertainty)

  if (uncertainty == FALSE){
    process_fertility_plots("Kenya")
  }
  
}

# Malawi
process_number_children_malawi <- function(uncertainty = FALSE, seed = NA){
  # fathers
  cat(sprintf("Processing number of children of fathers\n"))
  
  data = readRDS('global_age_analysis_2021/data/Malawi/male_fertility.RDS')
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
  if (uncertainty == TRUE){
    data_f = sample_fertility(data_f, "Malawi", "Male", seed)
  }
  
  if (uncertainty == FALSE){
    write.csv(data_f, file = paste0('global_age_analysis_2021/data/Malawi/malawi_fertility_m_all.csv'), row.names=FALSE)
  } else {
    write.csv(data_f, file = paste0('global_age_analysis_2021/data/Malawi/malawi_fertility_m_all_un.csv'), row.names=FALSE)
  }
  
  process_children_father_80_plus("Malawi", data_f, uncertainty = uncertainty)
  is_child_mortality_needed = 0
  add_child_mortality(is_child_mortality_needed, "Malawi", uncertainty = uncertainty)
  
  # mothers
  cat(sprintf("Processing number of children of mothers\n"))
  data = readRDS('global_age_analysis_2021/data/Malawi/female_fertility.RDS')
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
  if (uncertainty == TRUE){
    data_f = sample_fertility(data_f, "Malawi", "Female", seed)
  }
  
  if (uncertainty == FALSE){
    write.csv(data_f, file = paste0('global_age_analysis_2021/data/Malawi/malawi_fertility_f.csv'), row.names=FALSE)
  } else {
    write.csv(data_f, file = paste0('global_age_analysis_2021/data/Malawi/malawi_fertility_f_un.csv'), row.names=FALSE)
  }
  
  is_child_mortality_needed = 0
  process_children_all("Malawi", is_child_mortality_needed, data_f, uncertainty = uncertainty)
  
  if (uncertainty == FALSE){
    process_fertility_plots("Malawi")
  }
  
}

# Mexico
process_number_children_mexico <- function(uncertainty = FALSE, seed = NA){
  # fertility
  cat(sprintf("Processing Fertility rates\n"))
  process_mexico_fertility(uncertainty = uncertainty, seed)
  # child mortality
  cat(sprintf("Processing child mortality rates\n"))
  process_child_mortality('Mexico', 'Mexico')
  
  # fathers
  cat(sprintf("Processing number of children of fathers\n"))
  if (uncertainty == FALSE){
    data_f = read.csv(paste0('global_age_analysis_2021/data/Mexico/mexico_fertility_m_all.csv'))
  } else {
    data_f = read.csv(paste0('global_age_analysis_2021/data/Mexico/mexico_fertility_m_all_un.csv'))
  }
  
  data_f$fertility_rate <- data_f$fertility_rate/1000
  data_f$date = data_f$year
  data_f$age = as.character(data_f$age)
  process_children_father_65_plus("Mexico", data_f, uncertainty = uncertainty)
  is_child_mortality_needed = 1
  add_child_mortality(is_child_mortality_needed, "Mexico", uncertainty = uncertainty)
  
  # mothers
  cat(sprintf("Processing number of children of mothers\n"))
  if (uncertainty == FALSE){
    data = read.csv(paste0('global_age_analysis_2021/data/Mexico/mexico_fertility_f.csv'))
  } else {
    data = read.csv(paste0('global_age_analysis_2021/data/Mexico/mexico_fertility_f_un.csv'))
  } 

  
  data_f = copy(data)
  data_f$fertility_rate = data_f$fertility_rate/1000
  d_2019 = data_f[which(data_f$date == '2019'),]
  d_2020 = copy(d_2019)
  d_2020$date = '2020'
  data_f = rbind(data_f, d_2020)
  process_children_all("Mexico", is_child_mortality_needed, data_f, uncertainty = uncertainty)
  
  if (uncertainty == FALSE){
    process_fertility_plots("Mexico")
  }
  
}

# Nigeria
process_number_children_nigeria <- function(uncertainty = FALSE, seed = NA){
  # fathers
  cat(sprintf("Processing number of children of fathers\n"))

  data = readRDS('global_age_analysis_2021/data/Nigeria/male_fertility.RDS')
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
  
  if (uncertainty == TRUE){
    data_f = sample_fertility(data_f, "Nigeria", "Male", seed)
  }
  
  if (uncertainty == FALSE){
    write.csv(data_f, file = paste0('global_age_analysis_2021/data/Nigeria/nigeria_fertility_m_all.csv'), row.names=FALSE)
  } else {
    write.csv(data_f, file = paste0('global_age_analysis_2021/data/Nigeria/nigeria_fertility_m_all_un.csv'), row.names=FALSE)
  }
  
  process_children_father_80_plus("Nigeria", data_f, uncertainty = uncertainty)
  is_child_mortality_needed = 0
  add_child_mortality(is_child_mortality_needed, "Nigeria", uncertainty = uncertainty)
  
  # mothers
  cat(sprintf("Processing number of children of mothers\n"))
  data = readRDS('global_age_analysis_2021/data/Nigeria/female_fertility.RDS')
  
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
  if (uncertainty == TRUE){
    data_f = sample_fertility(data_f, "Nigeria", "Female", seed)
  }
  
  if (uncertainty == FALSE){
    write.csv(data_f, file = paste0('global_age_analysis_2021/data/Nigeria/nigeria_fertility_f.csv'), row.names=FALSE)
  } else {
    write.csv(data_f, file = paste0('global_age_analysis_2021/data/Nigeria/nigeria_fertility_f_un.csv'), row.names=FALSE)
  }
  is_child_mortality_needed = 0
  process_children_all("Nigeria", is_child_mortality_needed, data_f, uncertainty = uncertainty)

  if (uncertainty == FALSE){
    process_fertility_plots("Nigeria")
  }
  
}

# Peru
process_number_children_peru <- function(uncertainty = FALSE, seed = NA){
  # fathers
  cat(sprintf("Processing number of children of fathers\n"))
  data = readRDS('global_age_analysis_2021/data/Peru/male_fertility.RDS')
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
  if (uncertainty == TRUE){
    data_f = sample_fertility(data_f, "Peru", "Male", seed)
  }
  
  if (uncertainty == FALSE){
    write.csv(data_f, file = paste0('global_age_analysis_2021/data/Peru/peru_fertility_m_all.csv'), row.names=FALSE)
  } else {
    write.csv(data_f, file = paste0('global_age_analysis_2021/data/Peru/peru_fertility_m_all_un.csv'), row.names=FALSE)
  }
  process_children_father_80_plus("Peru", data_f, uncertainty = uncertainty)
  is_child_mortality_needed = 0
  add_child_mortality(is_child_mortality_needed, "Peru", uncertainty = uncertainty)
  
  # mothers
  cat(sprintf("Processing number of children of mothers\n"))
  data = readRDS('global_age_analysis_2021/data/Peru/female_fertility.RDS')
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
  if (uncertainty == TRUE){
    data_f = sample_fertility(data_f, "Peru", "Female", seed)
  }
  
  if (uncertainty == FALSE){
    write.csv(data_f, file = paste0('global_age_analysis_2021/data/Peru/peru_fertility_f.csv'), row.names=FALSE)
  } else {
    write.csv(data_f, file = paste0('global_age_analysis_2021/data/Peru/peru_fertility_f_un.csv'), row.names=FALSE)
  }
  is_child_mortality_needed = 0
  process_children_all("Peru", is_child_mortality_needed, data_f, uncertainty = uncertainty)
  
  if (uncertainty == FALSE){
    process_fertility_plots("Peru")
  }
 
}

# Philippines
process_number_children_philippines <- function(uncertainty = FALSE, seed = NA){
  # fertility
  cat(sprintf("Processing Fertility rates\n"))
  process_philippines_fertility(uncertainty = uncertainty, seed)
  # child mortality
  cat(sprintf("Processing child mortality rates\n"))
  process_child_mortality('Philippines', 'Philippines')
  
  # fathers
  cat(sprintf("Processing number of children of fathers\n"))
  if (uncertainty == FALSE){
    data_f = read.csv(paste0('global_age_analysis_2021/data/Philippines/philippines_fertility_m_all.csv'))
  } else {
    data_f = read.csv(paste0('global_age_analysis_2021/data/Philippines/philippines_fertility_m_all_un.csv'))
  }
  
  data_f$fertility_rate <- data_f$fertility_rate/1000
  data_f$date = data_f$year
  data_f$age = as.character(data_f$age)
  process_children_father_65_plus("Philippines", data_f, uncertainty = uncertainty)
  is_child_mortality_needed = 1
  add_child_mortality(is_child_mortality_needed, "Philippines",  uncertainty = uncertainty)
  
  # mothers
  cat(sprintf("Processing number of children of mothers\n"))
  if (uncertainty == FALSE){
    data = read.csv(paste0('global_age_analysis_2021/data/Philippines/philippines_fertility_f.csv'))
  } else {
    data = read.csv(paste0('global_age_analysis_2021/data/Philippines/philippines_fertility_f_un.csv'))
  }
  
  data_f = copy(data)
  data_f$fertility_rate = data_f$fertility_rate/1000
  d_2019 = data_f[which(data_f$date == '2019'),]
  d_2020 = copy(d_2019)
  d_2020$date = '2020'
  data_f = rbind(data_f, d_2020)
  process_children_all("Philippines", is_child_mortality_needed, data_f, uncertainty = uncertainty)
  
  if (uncertainty == FALSE){
    process_fertility_plots("Philippines")
  }
}

# Poland
process_number_children_poland <- function(uncertainty = FALSE, seed){
  # fertility
  cat(sprintf("Processing Fertility rates\n"))
  process_poland_fertility(uncertainty = uncertainty, seed)
  # child mortality
  cat(sprintf("Processing child mortality rates\n"))
  process_child_mortality('Poland', 'Poland')
  
  # fathers
  cat(sprintf("Processing number of children of fathers\n"))
  if (uncertainty == FALSE){
    data_f = read.csv(paste0('global_age_analysis_2021/data/Poland/poland_fertility_m_all.csv'))
  } else {
    data_f = read.csv(paste0('global_age_analysis_2021/data/Poland/poland_fertility_m_all_un.csv'))
  }
  
  data_f$fertility_rate <- data_f$fertility_rate/1000
  data_f$date = data_f$year
  data_f$age = as.character(data_f$age)
  process_children_father_50_plus("Poland", data_f, uncertainty = uncertainty)
  is_child_mortality_needed = 1
  add_child_mortality(is_child_mortality_needed, "Poland", uncertainty = uncertainty)
  
  # mothers
  cat(sprintf("Processing number of children of mothers\n"))
  if (uncertainty == FALSE){
    data = read.csv(paste0('global_age_analysis_2021/data/Poland/poland_fertility_f.csv')) 
  } else {
    data = read.csv(paste0('global_age_analysis_2021/data/Poland/poland_fertility_f_un.csv'))
  }
  
  data_f = copy(data)
  data_f$fertility_rate = data_f$fertility_rate/1000
  d_2019 = data_f[which(data_f$date == '2019'),]
  d_2020 = copy(d_2019)
  d_2020$date = '2020'
  data_f = rbind(data_f, d_2020)

  process_children_all("Poland", is_child_mortality_needed, data_f, uncertainty = uncertainty)
  
  if (uncertainty == FALSE){
    process_fertility_plots("Poland")
  }
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
  data_f = read.csv(paste0('global_age_analysis_2021/data/Russia/russia_fertility_m_all.csv'))
  data_f$fertility_rate <- data_f$fertility_rate/1000
  data_f$date = data_f$year
  data_f$age = as.character(data_f$age)
  process_children_father_60_plus('Russia', data_f)
  is_child_mortality_needed = 1
  add_child_mortality(is_child_mortality_needed, 'Russia')
  
  # mothers
  cat(sprintf("Processing number of children of mothers\n"))
  country = 'Russia'
  data = read.csv(paste0('global_age_analysis_2021/data/Russia/russia_fertility_f.csv'))
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
process_number_children_spain <- function(uncertainty = FALSE, seed = NA){
  cat(sprintf("Processing Fertility rates\n"))
  process_spain_fertility(uncertainty = uncertainty, seed)
  # child mortality
  cat(sprintf("Processing child mortality rates\n"))
  process_child_mortality('Spain', 'Spain')
  
  # fathers
  cat(sprintf("Processing number of children of fathers\n"))
  if (uncertainty == FALSE){
    data_f = read.csv(paste0('global_age_analysis_2021/data/Spain/spain_fertility_m_all.csv'))
  } else {
    data_f = read.csv(paste0('global_age_analysis_2021/data/Spain/spain_fertility_m_all_un.csv'))
  }
  
  data_f$fertility_rate <- data_f$fertility_rate/1000
  data_f$date = data_f$year
  data_f$age = as.character(data_f$age)
  process_children_father_65_plus("Spain", data_f, uncertainty = uncertainty)
  is_child_mortality_needed = 1
  add_child_mortality(is_child_mortality_needed, "Spain", uncertainty = uncertainty)
  
  # mothers
  cat(sprintf("Processing number of children of mothers\n"))
  if (uncertainty == FALSE){
    data = read.csv(paste0('global_age_analysis_2021/data/Spain/spain_fertility_f.csv'))
  } else {
    data = read.csv(paste0('global_age_analysis_2021/data/Spain/spain_fertility_f_un.csv'))
  }
  
  data_f = copy(data)
  data_f$fertility_rate = data_f$fertility_rate/1000
  d_2019 = data_f[which(data_f$date == '2019'),]
  d_2020 = copy(d_2019)
  d_2020$date = '2020'
  data_f = rbind(data_f, d_2020)
  process_children_all("Spain", is_child_mortality_needed, data_f, uncertainty = uncertainty)
  
  if (uncertainty == FALSE){
    process_fertility_plots("Spain")
  }
  
}

# South Africa
process_number_children_south_africa <- function(uncertainty = FALSE, seed = NA){
  # fathers
  cat(sprintf("Processing number of children of fathers\n"))
  data = readRDS('global_age_analysis_2021/data/SouthAfrica/male_fertility.RDS')
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
  if (uncertainty == TRUE){
    data_f = sample_fertility(data_f, "South Africa", "Male", seed)
  }
  
  if (uncertainty == FALSE){
    write.csv(data_f, file = paste0('global_age_analysis_2021/data/SouthAfrica/southafrica_fertility_m_all.csv'), row.names=FALSE)
  } else {
    write.csv(data_f, file = paste0('global_age_analysis_2021/data/SouthAfrica/southafrica_fertility_m_all_un.csv'), row.names=FALSE)
  }
  process_children_father_80_plus('SouthAfrica', data_f, uncertainty = uncertainty)
  is_child_mortality_needed = 0
  add_child_mortality(is_child_mortality_needed, "SouthAfrica", uncertainty = uncertainty)
  
  # mothers
  cat(sprintf("Processing number of children of mothers\n"))
  data = readRDS('global_age_analysis_2021/data/SouthAfrica/female_fertility.RDS')
  
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
  if (uncertainty == TRUE){
    data_f = sample_fertility(data_f, "South Africa", "Female", seed)
  }
  
  if (uncertainty == FALSE){
    write.csv(data_f, file = paste0('global_age_analysis_2021/data/SouthAfrica/southafrica_fertility_f.csv'), row.names=FALSE)
  } else {
    write.csv(data_f, file = paste0('global_age_analysis_2021/data/SouthAfrica/southafrica_fertility_f_un.csv'), row.names=FALSE)
  }
  is_child_mortality_needed = 0
  process_children_all("SouthAfrica", is_child_mortality_needed, data_f, uncertainty = uncertainty)
  
  if (uncertainty == FALSE){
    process_fertility_plots('SouthAfrica')
  }
  
}

# USA
process_number_children_usa <- function(uncertainty = FALSE, seed = NA){
  cat(sprintf("Processing Fertility rates\n"))
  process_usa_fertility(uncertainty = uncertainty, seed)

  # child mortality
  cat(sprintf("Processing child mortality rates\n"))
  process_child_mortality('USA', 'United States of America')
  
  # fathers
  cat(sprintf("Processing number of children of fathers\n"))
  if (uncertainty == FALSE){
    data_f = read.csv(paste0('global_age_analysis_2021/data/USA/usa_fertility_m_all.csv'))
  } else {
    data_f = read.csv(paste0('global_age_analysis_2021/data/USA/usa_fertility_m_all_un.csv'))
  }
  
  data_f$fertility_rate <- data_f$fertility_rate/1000
  data_f$date = data_f$year
  data_f$age = as.character(data_f$age)
  process_children_father_55_plus("USA", data_f, uncertainty = uncertainty)
  is_child_mortality_needed = 1
  add_child_mortality(is_child_mortality_needed, "USA", uncertainty = uncertainty)
  
  # mothers
  cat(sprintf("Processing number of children of mothers\n"))
  if (uncertainty == FALSE){
    data = read.csv(paste0('global_age_analysis_2021/data/USA/usa_fertility_f.csv'))
  } else {
    data = read.csv(paste0('global_age_analysis_2021/data/USA/usa_fertility_f_un.csv'))
  }
  
  data_f = copy(data)
  data_f$fertility_rate = data_f$fertility_rate/1000
  d_2019 = data_f[which(data_f$date == '2019'),]
  d_2020 = copy(d_2019)
  d_2020$date = '2020'
  data_f = rbind(data_f, d_2020)
  process_children_all("USA", is_child_mortality_needed, data_f, uncertainty = uncertainty)
  
  if (uncertainty == FALSE){
    process_fertility_plots("USA")
  }
  
}

# Zimbabwe
process_number_children_zimbabwe<- function(uncertainty = FALSE, seed = NA){
  # fathers
  cat(sprintf("Processing number of children of fathers\n"))
  
  data = readRDS('global_age_analysis_2021/data/Zimbabwe/male_fertility.RDS')
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
  if (uncertainty == TRUE){
    data_f = sample_fertility(data_f, "Zimbabwe", "Male", seed)
  }
  
  if (uncertainty == FALSE){
    write.csv(data_f, file = paste0('global_age_analysis_2021/data/Zimbabwe/zimbabwe_fertility_m_all.csv'), row.names=FALSE)
  } else {
    write.csv(data_f, file = paste0('global_age_analysis_2021/data/Zimbabwe/zimbabwe_fertility_m_all_un.csv'), row.names=FALSE)
  }
  process_children_father_80_plus("Zimbabwe", data_f, uncertainty = uncertainty)
  is_child_mortality_needed = 0
  add_child_mortality(is_child_mortality_needed, "Zimbabwe", uncertainty = uncertainty)
  
  # mothers
  cat(sprintf("Processing number of children of mothers\n"))
  data = readRDS('global_age_analysis_2021/data/Zimbabwe/female_fertility.RDS')
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
  if (uncertainty == TRUE){
    data_f = sample_fertility(data_f, "Zimbabwe", "Female", seed)
  }
  
  if (uncertainty == FALSE){
    write.csv(data_f, file = paste0('global_age_analysis_2021/data/Zimbabwe/zimbabwe_fertility_f.csv'), row.names=FALSE)
  } else {
    write.csv(data_f, file = paste0('global_age_analysis_2021/data/Zimbabwe/zimbabwe_fertility_f_un.csv'), row.names=FALSE)
  }
  is_child_mortality_needed = 0
  process_children_all("Zimbabwe", is_child_mortality_needed, data_f, uncertainty = uncertainty)
  
  if (uncertainty == FALSE){
    process_fertility_plots("Zimbabwe")
  }
}
