sample_fertility <- function(data, country, gender, seed){
  
  # Extract population
  data_pop = readxl::read_xlsx('TheLancetCAH_global_age_analysis_2022/data/fertility/pop.xlsx', sheet = 2)
  names(data_pop) = as.character(data_pop[1,])
  # data (thousand)
  data_pop = as.data.table(data_pop) %>% filter(Location %in% country) %>% select(Location, Time, Age, Female, Male)
  setnames(data_pop, 1:ncol(data_pop), c('country', 'year', 'age', 'Female', 'Male'))
  data_pop =  reshape2::melt(data_pop, id.vars = c('country', 'year', 'age'), variable.name =  'gender', value.name = 'pop')
  if (gender == "Male"){
    data_pop = as.data.table(data_pop %>% filter(gender == "Male"))  
  } else  {
    data_pop = as.data.table(data_pop %>% filter(gender == "Female")) 
  }
  
  
  data_pop$age = as.character(data_pop$age)
  `%notin%` = Negate(`%in%`)
  data_pop = data_pop %>% filter(age %notin% c('0-4', '5-9', '10-14'))
  data_pop$age = ifelse(data_pop$age %in% c('80-84', '85-89', '90-94', '95-99', '100+'), '80+', data_pop$age)
  data_pop$pop <- str_replace(data_pop$pop, " ", "")
  
  data_pop$pop = as.numeric(data_pop$pop)
  data_pop = data_pop %>% group_by(age, year) %>% mutate(pop = sum(pop)) %>% ungroup() %>% distinct()
  data_pop$gender = ifelse(data_pop$gender[1] == "Male", "M", "F")
  
  data <- data[which(data$year > 2001),]
  
  births = left_join(data, data_pop, c('year', "gender", "age"))
  births <- births[which(births$age != "under 15-NA"),]
  
  births$num_births =  round(births$fertility_rate/1000 * births$pop * 1000)
  
  set.seed(seed)
  births$sample_births = rpois(length(births$num_births), births$num_births)
  
  births$fertility_rate <- births$sample_births / births$pop
  
  data <- births %>% select(-pop, -num_births, -sample_births, - country)
  
  return (data)
}