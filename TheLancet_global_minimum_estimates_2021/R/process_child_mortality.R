# Process child & infant mortality

add_child_mortality = function(is_child_mortality_needed, country){
  children = read.csv(paste0('TheLancet_global_minimum_estimates_2021/data/', country, '/child_raw_m.csv'))
  plot_c = as.data.frame(as.numeric(as.character(unlist(children))))
  plot_c$father_age = rep(1:100,18)
  plot_c$child_age =sort(rep(seq(18)-1, 100))
  setnames(plot_c, 1, 'prob')

  if (is_child_mortality_needed){

    child_m_matrix = read.csv(paste0('TheLancet_global_minimum_estimates_2021/data/', country, '/child_mortality_rate.csv'))
    names(child_m_matrix) = paste0(seq(0:17)-1, 'years')
    child_and_m = as.matrix(children) * (1-as.matrix(child_m_matrix))
    child_and_m = as.data.frame(child_and_m)
    write_csv(file = paste0('TheLancet_global_minimum_estimates_2021/data/', country, '/child_all_m.csv'), child_and_m)

    plot_c_and_m = as.data.frame(as.numeric(as.character(unlist(child_and_m))))
    plot_c_and_m$father_age = rep(1:100,18)
    plot_c_and_m$child_age =sort(rep(seq(18)-1, 100))
    setnames(plot_c_and_m, 1, 'prob')


    # ggplot(as.data.frame(plot_c_and_m), aes(x=child_age, y=father_age, fill=prob)) +
    #   geom_tile(color = "white")+
    #   theme(axis.text.x = element_text(angle = 90)) +
    #   labs(x= "child age", y="father age") +
    #   scale_fill_gradient2(low = "yellow", high = "red")
    
    plot_c_and_m$gender = 'male'
    write_csv(file = paste0('TheLancet_global_minimum_estimates_2021/data/', country, '/child_all_list_m.csv'), plot_c_and_m)
  } else{
    child_and_m = copy(children)
    child_and_m = as.data.frame(child_and_m)
    write_csv(file = paste0('TheLancet_global_minimum_estimates_2021/data/', country, '/child_all_m.csv'), child_and_m)

    plot_c_and_m = copy(plot_c)
    plot_c_and_m$gender = 'male'
    write_csv(file = paste0('TheLancet_global_minimum_estimates_2021/data/', country, '/child_all_list_m.csv'), plot_c_and_m)

  }
  child_and_m = read.csv(paste0('TheLancet_global_minimum_estimates_2021/data/', country, '/child_all_m.csv'))
  ddf = as.data.frame(apply(child_and_m, 1, sum))
  names(ddf) = 'children'
  ddf$gender = 'male'
  ddf$age = 1:100
  write_csv(file = paste0('TheLancet_global_minimum_estimates_2021/data/', country, '/children_m.csv'),ddf)
}
# 
# # Multiple countries
# process_children_mortality_update = function(){
#   data = readxl::read_xlsx('TheLancet_global_minimum_estimates_2021/data/NumberDeaths.xlsx', sheet = 2)
#   names(data) = as.character(data[1,])
#   data = data[6:nrow(data),]
#   data = data %>% select(Location, Time, '0-4', '5-9', '10-14', '15-19') %>% filter(!is.na(Time))
#   data_00 = data %>% filter(Time == '2000 - 2005')
#   for (i in seq(2003, 2005)){
#     data_00$Time = i
#     data = rbind(data_00, data)
#   }
#   data_00 = data %>% filter(Time == '2005 - 2010')
#   for (i in seq(2006, 2010)){
#     data_00$Time = i
#     data = rbind(data, data_00)
#   }
#   data_00 = data %>% filter(Time == '2010 - 2015')
#   for (i in seq(2011, 2015)){
#     data_00$Time = i
#     data = rbind(data, data_00)
#   }
#   data_00 = data %>% filter(Time == '2015 - 2020')
#   for (i in seq(2016, 2020)){
#     data_00$Time = i
#     data = rbind(data,data_00)
#   }
#   data = reshape2::melt(data, id.vars = c('Location', 'Time'), variable.name = 'age', value.name = 'value')
#   setnames(data, 1:2, c('country', 'year'))
#   data_pop = readxl::read_xlsx('TheLancet_global_minimum_estimates_2021/data/fertility_update/pop.xlsx', sheet = 2)
#   countries = as.character(unique(data$country))
#   #countries[4] = 'Iran'
#   names(data_pop) = as.character(data_pop[1,])
#   # data (thousand)
#   data_pop = as.data.table(data_pop) %>% 
#     filter(Location %in% countries) %>% 
#     select(Location, Time, Age, Female, Male)
#   setnames(data_pop, 1:ncol(data_pop), c('country', 'year', 'age', 'Female', 'Male'))
#   
#   data_pop$pop = as.numeric(str_trim(gsub(" ", "", data_pop$Female, fixed = TRUE))) + as.numeric(str_trim(gsub(" ", "", data_pop$Male, fixed = TRUE)))
#   
#   data_pop$age = as.character(data_pop$age)
#   data_pop = data_pop %>% filter(age %in% c('0-4', '5-9', '10-14', '15-19'))
#   
#   d_merge = merge(data_pop, data, by = c('year', 'age', 'country'))
#   d_merge$mortality = as.numeric(d_merge$value)/as.numeric(d_merge$pop)
#   d_merge = d_merge %>% select(country, year, age, mortality)
#   d_merge$country[which(d_merge$country == "Iran (Islamic Republic of)")] = 'Iran'
#   write_csv(d_merge, path = 'TheLancet_global_minimum_estimates_2021/data/children/mortality_rate_all.csv')
# }

# Multiple countries
process_child_mortality = function(country, countries){
  # rate per children 
  if (country != "Russia"){
    child = read.csv(paste0('TheLancet_global_minimum_estimates_2021/data/child_mortality_rate', '.csv'))
    child = child %>% filter(country == countries)
    
  }else{
    child = read.csv('TheLancet_global_minimum_estimates_2021/data/Russia/mortality_rate_all.csv')
  }
  
  child_m_matrix = matrix(rep(0, 100*18), nrow = 100)
  
  child_m_matrix[49:15,1] = child$mortality[which(child$year == 2020 & child$age == '0-4')]
  child_m_matrix[50:16,2] = child$mortality[which(child$year == 2019 & child$age == '0-4')]
  child_m_matrix[51:17,3] = child$mortality[which(child$year == 2018 & child$age == '0-4')]
  child_m_matrix[52:18,4] = child$mortality[which(child$year == 2017 & child$age == '0-4')]
  child_m_matrix[53:19,5] = child$mortality[which(child$year == 2016 & child$age == '0-4')]
  child_m_matrix[54:20,6] = child$mortality[which(child$year == 2015 & child$age == '5-9')]
  child_m_matrix[55:21,7] = child$mortality[which(child$year == 2014 & child$age == '5-9')]
  child_m_matrix[56:22,8] = child$mortality[which(child$year == 2013 & child$age == '5-9')]
  child_m_matrix[57:23,9] = child$mortality[which(child$year == 2012 & child$age == '5-9')]
  child_m_matrix[58:24,10] = child$mortality[which(child$year == 2011 & child$age == '5-9')]
  child_m_matrix[59:25,11] = child$mortality[which(child$year == 2010 & child$age == '10-14')]
  child_m_matrix[60:26,12] = child$mortality[which(child$year == 2009 & child$age == '10-14')]
  child_m_matrix[61:27,13] = child$mortality[which(child$year == 2008 & child$age == '10-14')]
  child_m_matrix[62:28,14] = child$mortality[which(child$year == 2007 & child$age == '10-14')]
  child_m_matrix[63:29,15] = child$mortality[which(child$year == 2006 & child$age == '10-14')]
  child_m_matrix[64:30,16] = child$mortality[which(child$year == 2005 & child$age == '15-19')]
  child_m_matrix[65:31,17] = child$mortality[which(child$year == 2004 & child$age == '15-19')]
  child_m_matrix[66:32,18] = child$mortality[which(child$year == 2003 & child$age == '15-19')]
  
  
  
  child_m_matrix = as.data.frame(child_m_matrix)
  names(child_m_matrix) = paste0(seq(0:17)-1, 'years')
  
  write_csv(path = paste0('TheLancet_global_minimum_estimates_2021/data/', country, '/child_mortality_rate.csv'), child_m_matrix)
}  

# England and Wales
process_children_mortality_england_wales = function(){
  #url = 'https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fchildmortalitystatisticschildhoodinfantandperinatalchildhoodinfantandperinatalmortalityinenglandandwales%2f2018/cms2018workbookf.xls'
  #download.file(url, 'TheLancet_global_minimum_estimates_2021/data/children/mortality_england_wales.xls')
  data = readxl::read_xls('TheLancet_global_minimum_estimates_2021/data/UK/mortality_england_wales.xls', sheet = 5)
  names(data) = as.character(data[6,])
  data = data[7:nrow(data),c(1,12:15, (ncol(data)-3):ncol(data))]
  setnames(data, 1:ncol(data), c('year', 'nb_1-4', 'nb_5-9', 'nb_10-14', 'nb_1-15', '1-4', '5-9', '10-14', '1-15'))
  data = data %>% filter(year >= '2002') 
  data = data[which(!is.na(data$`nb_1-4`)),]
  data2 = copy(data)
  data = do.call(cbind,lapply(data[,2:ncol(data)],as.numeric))
  
  data = as.data.table(data)
  data$`nb_15` = data$`nb_1-15` - data$`nb_1-4` - data$`nb_5-9` - data$`nb_10-14`
  data = as.data.table(cbind(data2$year,data))
  setnames(data, 1, 'year')
  
  pop = read.csv('TheLancet_global_minimum_estimates_2021/data/UK/pop_england_wales.csv')
  pop = pop[,c(3:24)]
  pop = reshape2::melt(pop, id.vars = c('country', 'age', 'sex'), variable.name = 'year', value.name = 'value')
  
  pop = pop %>% group_by(year, age, sex) %>% mutate(pop = sum(value)) %>% ungroup() %>%
    select(-value, -country) %>% distinct()
  
  pop = pop%>% filter(age %in% seq(15))
  pop = pop %>% group_by(year) %>% mutate(nb_pop = sum(pop)) %>% ungroup() %>% select(-age, -pop,-sex) %>% distinct()
  pop$year = as.character(pop$year)
  pop$year = gsub('population_', '',pop$year)
  d_merge = merge(pop, data, by = 'year')
  d_merge$`15` = d_merge$`nb_15` / d_merge$nb_pop * 1e5
  
  data = d_merge %>% select(year, `1-4`, `5-9`, `10-14`, `15`)
  data = reshape2::melt(data, id.vars = c('year'), variable.name =  'age', value.name = 'mortality')
  data$age = as.character(data$age)
  data$age = paste0(data$age, ' years')
  write_csv(data, file = 'TheLancet_global_minimum_estimates_2021/data/UK/mortality_rate_england_wales.csv')
}

process_infant_mortality_england_wales = function(){
  #url = 'https://www.ons.gov.uk/generator?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/bulletins/childhoodinfantandperinatalmortalityinenglandandwales/2018/478bbfbf&format=csv'
  #download.file(url, 'TheLancet_global_minimum_estimates_2021/data/UK/infant_england_wales.csv')
  data = read.csv('TheLancet_global_minimum_estimates_2021/data/UK/infant_england_wales.csv')
  data  = data[7:nrow(data),]
  names(data) = c('year', 'mortality')
  data$age = '0 year'
  child_m = read.csv('TheLancet_global_minimum_estimates_2021/data/UK/mortality_rate_england_wales.csv')
  data$year = as.character(data$year)
  data$mortality = as.numeric(as.character(data$mortality))/1e3 * 1e5
  data = rbind(as.data.table(data) %>% filter(year >= 2002) %>% select(year, age, mortality), child_m)
  data$mortality = as.numeric(data$mortality)
  write_csv(data, path = 'TheLancet_global_minimum_estimates_2021/data/UK/mortality_rate_all_england_wales.csv')
}

# Makes child mortality matrix for E and W
process_child_mortality_england_wales = function(){
  # rate 1e5 per children 
  child = read.csv('TheLancet_global_minimum_estimates_2021/data/UK/mortality_rate_all_england_wales.csv')
  
  child_m_matrix = matrix(rep(0, 100*18), nrow = 100)
  
  # Assumes the child mortality from 16-17 is same as 15
  # Assumes the child mortality for 2019, 2020 is same as 2018
  # the child mortality is for per 100,000 children
  child_19 = child[which(child$year == 2018),]
  child_19$year = 2019
  child_20 = copy(child_19)
  child_20$year = 2020
  child = rbind(child, child_19, child_20)
  child$mortality = child$mortality /1e5
  
  child_m_matrix[49:15,1] = child$mortality[which(child$year == 2020 & child$age == '0 year')]
  child_m_matrix[50:16,2] = child$mortality[which(child$year == 2019 & child$age == '1-4 years')]
  child_m_matrix[51:17,3] = child$mortality[which(child$year == 2018 & child$age == '1-4 years')]
  child_m_matrix[52:18,4] = child$mortality[which(child$year == 2017 & child$age == '1-4 years')]
  child_m_matrix[53:19,5] = child$mortality[which(child$year == 2016 & child$age == '1-4 years')]
  child_m_matrix[54:20,6] = child$mortality[which(child$year == 2015 & child$age == '5-9 years')]
  child_m_matrix[55:21,7] = child$mortality[which(child$year == 2014 & child$age == '5-9 years')]
  child_m_matrix[56:22,8] = child$mortality[which(child$year == 2013 & child$age == '5-9 years')]
  child_m_matrix[57:23,9] = child$mortality[which(child$year == 2012 & child$age == '5-9 years')]
  child_m_matrix[58:24,10] = child$mortality[which(child$year == 2011 & child$age == '5-9 years')]
  child_m_matrix[59:25,11] = child$mortality[which(child$year == 2010 & child$age == '10-14 years')]
  child_m_matrix[60:26,12] = child$mortality[which(child$year == 2009 & child$age == '10-14 years')]
  child_m_matrix[61:27,13] = child$mortality[which(child$year == 2008 & child$age == '10-14 years')]
  child_m_matrix[62:28,14] = child$mortality[which(child$year == 2007 & child$age == '10-14 years')]
  child_m_matrix[63:29,15] = child$mortality[which(child$year == 2006 & child$age == '10-14 years')]
  child_m_matrix[64:30,16] = child$mortality[which(child$year == 2005 & child$age == '15 years')]
  child_m_matrix[65:31,17] = child$mortality[which(child$year == 2004 & child$age == '15 years')]
  child_m_matrix[66:32,18] = child$mortality[which(child$year == 2003 & child$age == '15 years')]
  
  child_m_matrix = as.data.frame(child_m_matrix)
  names(child_m_matrix) = paste0(seq(0:17)-1, 'years')
  write_csv(path = 'TheLancet_global_minimum_estimates_2021/data/UK/child_mortality_rate.csv', child_m_matrix)
}  


process_child_mortality_russia = function(){
  data_04 = read.csv('TheLancet_global_minimum_estimates_2021/data/Russia/russian_federation_0-4.csv')
  data_04 = data_04[,c(1,4:5)]
  setnames(data_04, 1:3, c('country', 'year', 'mortality'))
  data_04$age = '0-4'  
  data_59 = read.csv('TheLancet_global_minimum_estimates_2021/data/Russia/russian_federation_5-9.csv')
  data_59 = data_59[,c(1,4:5)]
  setnames(data_59, 1:3, c('country', 'year', 'mortality'))
  data_59$age = '5-9'  
  data_1014 = read.csv('TheLancet_global_minimum_estimates_2021/data/Russia/russian_federation_10-14.csv')
  data_1014 = data_1014[,c(1,4:5)]
  setnames(data_1014, 1:3, c('country', 'year', 'mortality'))
  data_1014$age = '10-14'  
  data_1519 = read.csv('TheLancet_global_minimum_estimates_2021/data/Russia/russian_federation_15-19.csv')
  data_1519 = data_1519[,c(1,4:5)]
  setnames(data_1519, 1:3, c('country', 'year', 'mortality'))
  data_1519$age = '15-19'
  data = rbind(data_04, data_59, data_1014, data_1519)
  data_19 = data %>% filter(year == '2019')
  data_19$year = '2020'
  data = rbind(data, data_19)
  data = data %>% arrange(year, age)
  data$mortality = data$mortality/1000
  write_csv(data,path = paste0('TheLancet_global_minimum_estimates_2021/data/children/mortality_rate_all_russain_federation.csv'))
  
}




