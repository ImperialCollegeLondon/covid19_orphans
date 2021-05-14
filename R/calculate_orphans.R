# Argentina
process_orphans_argentina = function(){
  d_merge = read.csv('data/Argentina/covid19_deaths.csv', stringsAsFactors = FALSE)
  country = 'colombia'
  d_children = read.csv(paste0('data/Colombia/','children.csv'), stringsAsFactors = FALSE)
  d_children$age = paste0(d_children$age%/%20 * 20,'-',d_children$age %/% 20 *20+19)
  d_children$age = ifelse(d_children$age %in% c('60-79', '80-99', '100-119'), '60+', d_children$age)
  d_children = d_children %>% group_by(age, gender) %>% mutate(nb_c = mean(children)) %>%
    select(-children) %>% ungroup()%>%distinct()
  d_merge$gender = tolower(as.character(d_merge$gender))
  
  d_m1 = merge(d_merge, d_children, by = c('age', 'gender')) 
  d_m1 = as.data.table(d_m1)
  d_m1[, orphans := round(deaths * nb_c)]
  d_m1$age = as.character(d_m1$age)
  d_m1$age = ifelse(d_m1$age == '0-19', '00-19', d_m1$age)
  d_m1 = d_m1%>% arrange(age)                  
  write_csv(file = paste0('data/Argentina/orphans_all.csv'), d_m1)
  
  p <- ggplot(d_m1, aes(x = age, y = orphans, fill = gender)) +
    geom_bar(stat="identity", position=position_dodge()) +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))+
    xlab('Age of Parent') +
    ylab('Number of Orphans')+
    guides(fill=guide_legend(title="Sex of Parent"))
  ggsave(filename = "figures/orphans_all_age_argentina.pdf", p, width = 6, height = 5)

  d_summary = d_m1 %>% select(age, gender, deaths, orphans)
  
  d_summary = d_summary %>% group_by(age, gender) %>% 
    mutate(covid19_deaths = as.integer(round(sum(deaths))),
           nb_orphans = as.integer( round(sum(orphans)))) %>% 
    ungroup() %>% 
    select(-orphans, -deaths) %>%distinct()
  write_csv(path = 'data/Argentina/all_data.csv', d_summary)
  
  d_summary %>% filter(age != '00-19') %>% group_by(gender) %>% mutate(#excess1 = round(sum(excess)),
    deaths= round(sum(covid19_deaths)),
    orphans= round( sum( nb_orphans))) %>% ungroup()
}

# Brazil
process_orphans_brazil = function(){
  country = 'brazil'
  d_deaths = read.csv('data/Brazil/covid19_deaths.csv', stringsAsFactors = FALSE)
  d_merge = copy(d_deaths)
  d_merge$age = as.character(d_merge$age)
  setnames(d_merge, 'COVID19_deaths', 'deaths')
  d_children = read.csv(paste0('data/Brazil/children.csv'), stringsAsFactors = FALSE)
  d_children$age = paste0(d_children$age%/%5 * 5,'-',d_children$age %/% 5 *5+5)
  d_children$age = ifelse(d_children$age %in% c('100-105'), '100+', d_children$age)
  d_children = d_children %>% group_by(age, gender) %>% mutate(nb_c = mean(children)) %>%
    select(-children) %>% ungroup()%>%distinct()
  d_merge$gender = as.character(d_merge$gender)
  d_merge$gender = ifelse(d_merge$gender == 'F', 'female','male')
  
  d_m1 = merge( d_children,d_merge, by = c('age', 'gender')) 
  d_m1 = as.data.table(d_m1)
  d_m1[, orphans := round(deaths * nb_c)]

  d_m1$age = as.character(d_m1$age)
  d_m1$age = ifelse(d_m1$age == '5-10', '05-10', d_m1$age)
  d_m1$age = ifelse(d_m1$age == '95-100', '95-99', d_m1$age)
  d_m1$age = ifelse(d_m1$age == '100+', '99+', d_m1$age)
  d_m1 = d_m1%>% arrange(age) 
  
  write_csv(path = paste0('data/Brazil/orphans_all.csv'), d_m1)
  
  p <- ggplot(d_m1, aes(x = age, y = orphans, fill = gender)) +
    geom_bar(stat="identity", position=position_dodge()) +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))+
    xlab( 'Age of Parent') +
    ylab('Number of Orphans')+
    guides(fill=guide_legend(title="Sex of Parent"))
  ggsave(filename = "figures/orphans_all_age_brazil.pdf", p, width = 6, height = 5)
  d_summary = d_m1 %>% select(age, gender, deaths, orphans)
  
  d_summary$age = ifelse(d_summary$age %in% c('0-5', '5-10', '10-15'), '0-14',
                         ifelse(d_summary$age %in% c('50-55','55-60','60-65'), '50-64',
                                ifelse(d_summary$age %in% c('15-20','20-25','25-30','30-35','35-40', '40-45', '45-50'), '15-49','65+')))
  d_summary = d_summary %>% group_by(age, gender) %>% mutate(#excess = sum(excess_death),
    covid19_deaths = as.integer(round(sum(deaths))),
    
    nb_orphans = as.integer(round( sum(orphans)))) %>% ungroup() %>% 
    select(-orphans, -deaths) %>%distinct() %>% arrange(age)
  write_csv(path = 'data/Brazil/all_data.csv', d_summary)
  d_summary %>% filter(age != '0-14') %>% group_by(gender) %>% mutate(#excess = sum(excess_death),
    deaths= as.integer(sum(covid19_deaths)),
    
    orphans= as.integer( sum( nb_orphans))) %>% ungroup()
  
}

# Colombia
process_orphans_colombia = function(){
  country = 'colombia'
  d_merge = read.csv('data/Colombia/covid19_deaths_all.csv', stringsAsFactors = FALSE)
  d_children = read.csv(paste0('data/Colombia/children.csv'), stringsAsFactors = FALSE)
  d_children$age = paste0(d_children$age%/%5 * 5,'-',d_children$age %/% 5 *5+4)
  d_children$age = ifelse(d_children$age %in% c('80-84', '85-89', '90-94', '95-99', '100-104'), '80+', d_children$age)
  d_children = d_children %>% group_by(age, gender) %>% mutate(nb_c = mean(children)) %>%
    select(-children) %>% ungroup()%>%distinct()
  d_merge$gender = as.character(d_merge$gender)
  
  d_m1 = merge(d_merge, d_children, by = c('age', 'gender')) 
  d_m1 = as.data.table(d_m1)
  d_m1[, orphans := round(deaths * nb_c)]
  d_m1$age = as.character(d_m1$age)
  d_m1$age = ifelse(d_m1$age == '0-4', '00-04', 
                    ifelse(d_m1$age == '5-9','05-09' ,d_m1$age))
  d_m1 = d_m1%>% arrange(age)                  
  write_csv(file = paste0('data/Colombia/orphans_all.csv'), d_m1)
  p <- ggplot(d_m1, aes(x = age, y = orphans, fill = gender)) +
    geom_bar(stat="identity", position=position_dodge()) +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))+
    xlab('Age of Parent') +
    ylab('Number of Orphans')+
    guides(fill=guide_legend(title="Sex of Parent"))
  ggsave(filename = "figures/orphans_all_age_colombia.pdf", p, width = 6, height = 5)

  d_summary = d_m1 %>% select(age, gender, deaths, orphans)
  
  d_summary$age = ifelse(d_summary$age %in% c('00-04', '05-09', '10-14'), '0-14',
                         ifelse(d_summary$age %in% c('45-49','50-54', '55-59', '60-64'), '45-64',
                                ifelse(d_summary$age %in% c("65-69","70-74","75-79","80+" ),'65+', '15-44')))
  d_summary = d_summary %>% group_by(age, gender) %>% mutate(#excess = sum(excess_death),
    covid19_deaths = as.integer(round(sum(deaths))),
    
    nb_orphans = as.integer( round(sum(orphans)))) %>% ungroup() %>% 
    select(-orphans, -deaths) %>%distinct()

  write_csv(file = 'data/Colombia/all_data.csv', d_summary)
  
  d_summary %>% filter(age != '0-14') %>% group_by(gender) %>% mutate(#excess1 = round(sum(excess)),
    deaths= round(sum(covid19_deaths)),
    orphans= round( sum( nb_orphans))) %>% ungroup()
}

# England and wales
process_orphans_england_wales = function(){
  d_deaths = read.csv('data/UK/deaths_all_england_wales.csv', stringsAsFactors = FALSE)
  d_deaths$week = as.numeric(sapply(d_deaths$week, function(x) strsplit(x, " ")[[1]][2]))
  d_deaths = d_deaths[which((d_deaths$year.x == "2020" &  d_deaths$week >=10) | 
                              (d_deaths$year.x == "2021" &  d_deaths$week <= 8)),]
  d_merge = d_deaths %>% group_by(age, gender) %>% 
    mutate(nb_covid19 = sum(covid19_deaths),
           nb_excess = sum(excess_deaths)) %>% 
    ungroup() %>% 
    select(age, gender, nb_covid19, nb_excess) %>% 
    distinct()

  d_merge$deaths <- ifelse(d_merge$nb_excess > d_merge$nb_covid19, d_merge$nb_excess,  d_merge$nb_covid19)
  d_merge$gender = as.character(d_merge$gender)
  
  d_children = read.csv('data/UK/children.csv', stringsAsFactors = FALSE)
  d_children$age = paste0(d_children$age%/%5 * 5,'-',d_children$age %/% 5 *5+4)
  d_children$age = ifelse(d_children$age %in% c('90-94', '95-99', '100-104'), '90+', d_children$age)
  d_children = d_children %>% group_by(age, gender) %>% mutate(nb_c = mean(children)) %>%
    select(-children) %>% ungroup()%>%distinct()

  d_children$age = ifelse(d_children$age == '0-4', '00-04', 
                       ifelse(d_children$age == '5-9','05-09' , d_children$age))
  
  
  d_m1 = merge(d_merge, d_children, by = c('age', 'gender')) 
  d_m1 = as.data.table(d_m1)
  
  d_m1[, orphans := deaths * nb_c]
  write_csv(path = 'data/UK/england_wales_orphans_all.csv', d_m1)
  
  p <- ggplot(d_m1, aes(x = age, y = orphans, fill = gender)) +
    geom_bar(stat="identity", position=position_dodge()) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))+
    #labs(x = 'Age of Parents', y = 'Fertility Rate per 1000 people')+
    xlab( 'Age of Parent') +
    ylab('Number of Orphans')+
    guides(fill=guide_legend(title="Sex of Parent"))
  ggsave(filename = "figures/orphans_all_age_england_wales.pdf", p, width = 6, height = 5)
    
  d_summary = d_m1 %>% select(age, gender, nb_covid19, nb_excess, deaths,  orphans)
  
  d_summary$age = ifelse(d_summary$age %in% c('00-04', '05-09', '10-14'), '0-14',
                         ifelse(d_summary$age %in% c('45-49','50-54', '55-59', '60-64'), '45-64',
                                ifelse(d_summary$age %in% c("65-69","70-74","75-79","80-84","85-89" ,"90+" ),'65+', '15-44')))
  d_summary = d_summary %>% group_by(age, gender) %>% 
    mutate(excess =  as.integer(round(sum(nb_excess))),
           covid19_deaths = as.integer(round(sum(nb_covid19))),
           nb_deaths = as.integer(round(sum(deaths))),
           nb_orphans = as.integer(round(sum(orphans)))) %>% 
    ungroup() %>% 
    select(-orphans, -deaths, -nb_covid19, -nb_excess) %>%
    distinct()
  
  write_csv(path = 'data/UK/england_wales_all_data.csv', d_summary)
  
  d_summary = d_summary %>% 
    filter(age != '0-14') %>%
    group_by(gender) %>% 
    mutate(total_excess = as.integer(round(sum(excess))),
           total_covid = as.integer(round(sum(covid19_deaths))),
           total_max = as.integer(round(sum(nb_deaths))),
           total_orphans = as.integer(round(sum(nb_orphans)))) %>% 
    ungroup() %>% 
    distinct()
  
  print(d_summary)
}

# France
process_orphans_france = function(){
  d_deaths = read.csv('data/France/france_all.csv', stringsAsFactors = FALSE)
  d_merge = copy(d_deaths)

  d_children = read.csv(paste0('data/France/children.csv'), stringsAsFactors = FALSE)
  d_children$age = paste0((d_children$age)%/%10 * 10 ,'-',(d_children$age) %/% 10 *10+9)
  d_children$age = ifelse(d_children$age %in% c('90-99', '100-109'), '90+', d_children$age)
  d_children = d_children %>% group_by(age, gender) %>% mutate(nb_c = mean(children)) %>%
    select(-children) %>% ungroup()%>%distinct()
  d_merge$gender = as.character(d_merge$gender)
  d_children$gender = ifelse(d_children$gender == 'female', 'Female', 'Male')
  
  d_m1 = merge(d_merge, d_children, by = c('age', 'gender')) 
  d_m1 = as.data.table(d_m1)
  d_m1[, orphans := round(deaths * nb_c)]

  d_m1 = d_m1%>% arrange(age) 

  write_csv(path = paste0('data/France/france_orphans_all.csv'), d_m1)
  
  p <- ggplot(d_m1, aes(x = age, y = orphans, fill = gender)) +
    geom_bar(stat="identity", position=position_dodge()) +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))+
    xlab( 'Age of Parent') +
    ylab('Number of Orphans')+
    guides(fill=guide_legend(title="Sex of Parent"))
  ggsave(filename = "figures/orphans_all_age_france.pdf", p, width = 6, height = 5)
  
  d_summary = d_m1 %>% select(age, gender,
                              excess_deaths, covid_deaths, 
                              deaths, orphans)
  d_summary$age = as.character(d_summary$age)
  d_summary$age = ifelse(d_summary$age %in% c('70-79', '80-89', '90+'), '70+',
                         ifelse(d_summary$age %in% c('50-59','60-69'), '50-69',
                                ifelse(d_summary$age %in% c('20-29', '30-39', '40-49'), '20-49','0-19')))
  d_summary = d_summary %>% group_by(age, gender) %>% 
    mutate(nb_excess = as.integer(round((sum(excess_deaths)))),
           nb_covid = as.integer(round((sum(covid_deaths)))),
          nb_deaths = as.integer(round((sum(deaths)))),
           nb_orphans = as.integer(round(sum(orphans)))) %>% 
    ungroup() %>% 
    select(-orphans, -deaths, -covid_deaths, -excess_deaths) %>%
    distinct()
  d_summary = select(d_summary, age, gender, nb_excess, nb_covid, nb_deaths, nb_orphans)
  write_csv(path = 'data/France/all_data.csv', d_summary)
  d_summary %>% filter(age != '0-19') %>% 
    group_by(gender) %>% 
    mutate(total_excess = round(sum(nb_excess)),
           total_covid19 = round(sum(nb_covid)),
           total_deaths= round(sum(nb_deaths)),
           orphans= round(sum(nb_orphans))) %>% 
    ungroup()
}

# Germany
process_orphans_germany = function(){
  country = 'germany'
  d_merge = read.csv('data/Germany/covid19_deaths.csv', stringsAsFactors = FALSE)
  d_children = read.csv(paste0('data/Germany/children.csv'))
  d_children$age = paste0((d_children$age)%/%10 * 10 ,'-',(d_children$age) %/% 10 *10+9)
  d_children$age = ifelse(d_children$age %in% c('90-99', '100-109'), '90+', d_children$age)
  d_children = d_children %>% group_by(age, gender) %>% mutate(nb_c = mean(children)) %>%
    select(-children) %>% ungroup()%>%distinct()
  d_merge$gender = as.character(d_merge$gender)
  d_children$gender = ifelse(d_children$gender == 'female', 'Female', 'Male')
  
  d_m1 = merge(d_merge, d_children, by = c('age', 'gender')) 
  d_m1 = as.data.table(d_m1)
  d_m1[, orphans := round(deaths * nb_c)]
  d_m1$age = as.character(d_m1$age)
  #d_m1$age = ifelse(d_m1$age == '100+', '99+', d_m1$age)
  d_m1 = d_m1%>% arrange(age) 
  write_csv(path = paste0('data/Germany/orphans_all.csv'), d_m1)
  
  p <- ggplot(d_m1, aes(x = age, y = orphans, fill = gender)) +
    geom_bar(stat="identity", position=position_dodge()) +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))+
    xlab( 'Age of Parent') +
    ylab('Number of Orphans')+
    guides(fill=guide_legend(title="Sex of Parent"))
  ggsave(filename = "figures/orphans_all_age_germany.pdf", p, width = 6, height = 5)

  d_summary = d_m1 %>% select(age, gender, deaths, orphans)
  d_summary$age = as.character(d_summary$age)
  d_summary$age = ifelse(d_summary$age %in% c('70-79', '80-89', '90+'), '70+',
                         ifelse(d_summary$age %in% c('50-59','60-69'), '50-69',
                                ifelse(d_summary$age %in% c('20-29', '30-39', '40-49'), '20-49','0-19')))
  d_summary = d_summary %>% group_by(age, gender) %>% 
    mutate( COVID19_deaths = as.integer(round(sum(deaths))),
            nb_orphans = as.integer(round(sum(orphans)))) %>% ungroup() %>% 
    select(-orphans, -deaths) %>%distinct()
  
  write_csv(path = 'data/Germany/all_data.csv', d_summary)
  d_summary %>% filter(age != '0-19') %>% group_by(gender) %>% 
    mutate(covid19 = round(sum(COVID19_deaths)),
           orphans= round( sum( nb_orphans))) %>% ungroup()
}






# India
process_orphans_india = function(){
  country = 'india'
  d_deaths = read.csv('data/India/all_covid_deaths.csv', stringsAsFactors = FALSE)
  d_merge = copy(d_deaths)
  d_merge$age = as.character(d_merge$age)
  
  d_children = read.csv(paste0('data/India/children.csv'), stringsAsFactors = FALSE)
  d_children$age = paste0(d_children$age%/%5 * 5,'-',d_children$age %/% 5 *5+4)
  d_children$age = ifelse(d_children$age %in% c('5-9', '10-14', '15-19'), '5-19',
                          ifelse(d_children$age %in% c('20-24','25-29'), '20-29',
                                 ifelse(d_children$age %in% c('30-34','35-39'), '30-39',
                                        ifelse(d_children$age %in% c('40-44','45-49'), '40-49',
                                               ifelse(d_children$age %in% c('50-54','55-59'), '50-59',
                                                      ifelse(d_children$age %in% c('60-64','65-69'), '60-69',
                                                             ifelse(d_children$age %in% c('70-74','75-79'), '70-79',
                                                                    ifelse(d_children$age %in% c('80-84','85-89','90-94','95-99','100-104'), '80+','0-4'))))))))
  
  d_children = d_children %>% group_by(age, gender) %>% mutate(nb_c = mean(children)) %>%
    select(-children) %>% ungroup()%>%distinct()
  d_children$gender = ifelse(d_children$gender == "female", "Female", "Male")
  d_merge$sex = as.character(d_merge$sex)
  
  d_m1 = left_join(d_merge, d_children, by = c('age', 'sex' = 'gender')) 
  d_m1 = as.data.table(d_m1)
  d_m1[, orphans := round(deaths * nb_c)]
  d_m1$age = as.character(d_m1$age)
  d_m1$age = ifelse(d_m1$age == '5-19', '05-19', d_m1$age)
  d_m1$age = ifelse(d_m1$age == '0-4', '00-04', d_m1$age)
  d_m1$age = factor(d_m1$age, levels = c("00-04", "05-19", "20-29", "30-39", "40-49",  "50-59", "60-69", "70-79", "80+"))
  d_m1 = d_m1 %>% arrange(age) 
  
  write_csv(path = paste0('data/India/orphans_all.csv'), d_m1)
  
  p <- ggplot(d_m1, aes(x = age, y = orphans, fill = sex)) +
    geom_bar(stat="identity", position=position_dodge()) +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))+
    xlab( 'Age of Parent') +
    ylab('Number of Orphans')+
    guides(fill=guide_legend(title="Sex of Parent"))
  ggsave(filename = "figures/orphans_all_age_india.pdf", p, width = 6, height = 5)
  
  d_summary = d_m1 %>% select(age, sex, deaths, orphans)
  d_summary$age = ifelse(d_summary$age %in% c('00-04', '05-19'), '0-19',
                         ifelse(d_summary$age %in% c('20-29','30-39', '40-49'), '20-49',
                                ifelse(d_summary$age %in% c("50-59","60-69"),'50-69', '70+')))
  d_summary = d_summary %>% group_by(age, sex) %>% 
    mutate(covid_deaths = as.integer(round(sum(deaths))),
           nb_orphans = as.integer(round( sum(orphans)))) %>% 
    ungroup() %>% 
    select(-orphans, -deaths) %>%
    distinct() %>% 
    arrange(age)
  write_csv(path = 'data/India/all_data.csv', d_summary)
  d_summary %>% filter(age != '0-19') %>% 
    group_by(sex) %>% 
    mutate(deaths= as.integer(sum(covid_deaths)),
           orphans= as.integer(sum( nb_orphans))) %>% 
    ungroup()
}

# Iran
process_orphans_iran = function(){
  d_merge = read.csv('data/Iran/iran_all.csv', stringsAsFactors = FALSE)
  d_children = read.csv(paste0('data/Iran/children.csv'), stringsAsFactors = FALSE)
  d_children$age = paste0((d_children$age)%/%10 * 10 ,'-',(d_children$age) %/% 10 *10+9)
  d_children$age = ifelse(d_children$age %in% c('80-89', '90-99', '100-109'), '80+', d_children$age)
  d_children = d_children %>% 
    group_by(age, gender) %>% mutate(nb_c = mean(children)) %>%
    select(-children) %>% ungroup()%>%distinct()
  d_merge$gender = as.character(d_merge$gender)
  d_children$gender = ifelse(d_children$gender == 'female', 'Female', 'Male')
  
  d_m1 = merge(d_merge, d_children, by = c('age', 'gender')) 
  d_m1 = as.data.table(d_m1)
  d_m1[, orphans := round(deaths * nb_c)]
  d_m1 = d_m1%>% arrange(age) 
  write_csv(path = paste0('data/Iran/iran_orphans_all.csv'), d_m1)
  
  p <- ggplot(d_m1, aes(x = age, y = orphans, fill = gender)) +
    geom_bar(stat="identity", position=position_dodge()) +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))+
    xlab( 'Age of Parent') +
    ylab('Number of Orphans')+
    guides(fill=guide_legend(title="Sex of Parent"))
  ggsave(filename = "figures/orphans_all_age_iran.pdf", p, width = 6, height = 5)
  
  d_summary = d_m1 %>% 
    select(age, gender,deaths, orphans)
  d_summary$age = as.character(d_summary$age)
  d_summary$age = ifelse(d_summary$age %in% c('70-79', '80-89', '90+'), '70+',
                         ifelse(d_summary$age %in% c('50-59','60-69'), '50-69',
                                ifelse(d_summary$age %in% c('20-29', '30-39', '40-49'), '20-49','0-19')))
  d_summary = d_summary %>% group_by(age, gender) %>% 
    mutate(max_deaths = as.integer(round((sum(deaths)))),
           nb_orphans = as.integer(round(sum(orphans)))) %>% ungroup() %>% 
    select(-orphans, -deaths) %>% 
    distinct()
  write_csv(path = 'data/Iran/all_data.csv', d_summary)
  d_summary %>% filter(age != '0-19') %>% group_by(gender) %>% 
    mutate(total_deaths= round(sum(max_deaths)),
           orphans= round( sum( nb_orphans))) %>% ungroup()
}

# Italy
process_orphans_italy = function(){
  d_merge = read.csv('data/Italy/italy_all.csv', stringsAsFactors = FALSE)
  d_children = read.csv(paste0('data/Italy/children.csv'), stringsAsFactors = FALSE)
  d_children$age = paste0((d_children$age)%/%10 * 10 ,'-',(d_children$age) %/% 10 *10+9)
  d_children$age = ifelse(d_children$age %in% c('90-99', '100-109'), '90+', d_children$age)
  d_children = d_children %>% group_by(age, gender) %>% mutate(nb_c = mean(children)) %>%
    select(-children) %>% ungroup()%>%distinct()
  d_merge$gender = as.character(d_merge$gender)
  d_children$gender = ifelse(d_children$gender == 'female', 'Female', 'Male')
  
  d_m1 = merge(d_merge, d_children, by = c('age', 'gender')) 
  d_m1 = as.data.table(d_m1)
  d_m1[, orphans := round(deaths * nb_c)]
  d_m1 = d_m1%>% arrange(age) 
  write_csv(path = paste0('data/Italy/italy_orphans_all.csv'), d_m1)
  
  p <- ggplot(d_m1, aes(x = age, y = orphans, fill = gender)) +
    geom_bar(stat="identity", position=position_dodge()) +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))+
    xlab( 'Age of Parent') +
    ylab('Number of Orphans')+
    guides(fill=guide_legend(title="Sex of Parent"))
  ggsave(filename = "figures/orphans_all_age_italy.pdf", p, width = 6, height = 5)
  
  d_summary = d_m1 %>% 
    select(age, gender,excess_deaths, covid_deaths,deaths, orphans)
  d_summary$age = as.character(d_summary$age)
  d_summary$age = ifelse(d_summary$age %in% c('70-79', '80-89', '90+'), '70+',
                         ifelse(d_summary$age %in% c('50-59','60-69'), '50-69',
                                ifelse(d_summary$age %in% c('20-29', '30-39', '40-49'), '20-49','0-19')))
  d_summary = d_summary %>% group_by(age, gender) %>% 
    mutate(nb_excess = round(sum(excess_deaths)),
           nb_covid = round(sum(covid_deaths)),
           max_deaths = as.integer(round((sum(deaths)))),
           nb_orphans = as.integer(round(sum(orphans)))) %>% ungroup() %>% 
    select(-orphans, -deaths, -covid_deaths, -excess_deaths) %>% 
    distinct()
  write_csv(path = 'data/Italy/all_data.csv', d_summary)
  d_summary %>% filter(age != '0-19') %>% group_by(gender) %>% 
    mutate(total_excess = sum(nb_excess),
           total_covid = sum(nb_covid),
           total_deaths= round(sum(max_deaths)),
           orphans= round( sum( nb_orphans))) %>% ungroup()
}

# Kenya
process_orphans_kenya = function(){
  country = 'kenya'
  d_deaths = read.csv('data/Kenya/covid19_deaths.csv', stringsAsFactors = FALSE)
  d_merge = copy(d_deaths)
  d_children = read.csv(paste0('data/Kenya/children.csv'), stringsAsFactors = FALSE)
  d_children$age = paste0((d_children$age)%/%10 * 10 ,'-',(d_children$age) %/% 10 *10+9)
  d_children$age = ifelse(d_children$age %in% c('60-69', '70-79', '80-89', '90-99', '100-109'), '60+', d_children$age)
  d_children = d_children %>% group_by(age, gender) %>% mutate(nb_c = mean(children)) %>%
    select(-children) %>% ungroup()%>%distinct()
  d_merge$gender = as.character(d_merge$gender)
  d_children$gender = ifelse(d_children$gender == 'female', 'Female', 'Male')
  
  d_m1 = merge(d_merge, d_children, by = c('age', 'gender')) 
  d_m1 = as.data.table(d_m1)
  d_m1[, orphans := round(deaths * nb_c)]
  d_m1$age = as.character(d_m1$age)
  d_m1 = d_m1%>% arrange(age) 
  write_csv(path = paste0('data/Kenya/orphans_all.csv'), d_m1)
  
  p <- ggplot(d_m1, aes(x = age, y = orphans, fill = gender)) +
    geom_bar(stat="identity", position=position_dodge()) +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))+
    xlab( 'Age of Parent') +
    ylab('Number of Orphans')+
    guides(fill=guide_legend(title="Sex of Parent"))
  ggsave(filename = "figures/orphans_all_age_kenya.pdf", p, width = 6, height = 5)
  d_summary = d_m1 %>% select(age, gender, deaths, orphans)
  d_summary$age = as.character(d_summary$age)
  d_summary$age = ifelse(d_summary$age %in% c('0-9', '10-19'), '0-19',
                         ifelse(d_summary$age %in% c('20-29', '30-39', '40-49'), '20-49',d_summary$age))
  d_summary = d_summary %>% group_by(age, gender) %>% mutate( COVID19_deaths = round(sum(deaths)),
                                                              
                                                              nb_orphans = round(sum(orphans))) %>% ungroup() %>% 
    select(-orphans, -deaths) %>%distinct()
  write_csv(path = 'data/Kenya/all_data.csv', d_summary)
  d_summary %>% filter(age != '0-19') %>% group_by(gender) %>% mutate(covid19 = round(sum(COVID19_deaths)),
                                                                      orphans= round( sum( nb_orphans))) %>% ungroup()
}

# Malawi
process_orphans_malawi = function(){
  country = 'malawi'
  d_deaths = read.csv('data/Malawi/covid19_deaths.csv', stringsAsFactors = FALSE)
  d_merge = copy(d_deaths)
  d_merge$age = as.character(d_merge$age)
  d_children = read.csv(paste0('data/Malawi/children.csv'), stringsAsFactors = FALSE)
  d_children$age = paste0((d_children$age)%/%10 * 10 ,'-',(d_children$age) %/% 10 *10+9)
  d_children$age = ifelse(d_children$age %in% c('70-79', '80-89', '90-99', '100-109'), '70+', d_children$age)
  d_children = d_children %>% group_by(age, gender) %>% mutate(nb_c = mean(children)) %>%
    select(-children) %>% ungroup()%>%distinct()
  d_merge$gender = as.character(d_merge$gender)
  d_children$gender = ifelse(d_children$gender == 'female', 'Female', 'Male')
  d_m1 = merge(d_merge, d_children, by = c('age', 'gender'))
  d_m1 = as.data.table(d_m1)
  d_m1[, orphans := round(deaths * nb_c)]
  d_m1$age = as.character(d_m1$age)
  d_m1 = d_m1%>% arrange(age)
  write_csv(path = paste0('data/Malawi/orphans_all.csv'), d_m1)
  p <- ggplot(d_m1, aes(x = age, y = orphans, fill = gender)) +
    geom_bar(stat="identity", position=position_dodge()) +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))+
    xlab( 'Age of Parent') +
    ylab('Number of Orphans')+
    guides(fill=guide_legend(title="Sex of Parent"))
  ggsave(filename = "figures/orphans_all_age_malawi.pdf", p, width = 6, height = 5)

  d_summary = d_m1 %>% select(age, gender, deaths, orphans)
  d_summary$age = as.character(d_summary$age)
  d_summary$age = ifelse(d_summary$age %in% c('0-9', '10-19'), '0-19',
                         ifelse(d_summary$age %in% c('20-29', '30-39', '40-49'), '20-49',
                                ifelse(d_summary$age %in% c('50-59', '60-69'), '50-69', '70+')))
  d_summary = d_summary %>% group_by(age, gender) %>%
    mutate( COVID19_deaths = as.integer(round(sum(deaths))),
            nb_orphans = as.integer(round(sum(orphans)))) %>% ungroup() %>%
    select(-orphans, -deaths) %>%distinct()
  write_csv(path = 'data/Malawi/all_data.csv', d_summary)
  d_summary %>% filter(age != '0-19') %>% group_by(gender) %>%
    mutate(covid19 = round(sum(COVID19_deaths)),
           orphans= round( sum( nb_orphans))) %>% ungroup()
}

# Mexico
process_orphans_mexico = function(){
  country = 'mexico'
  d_deaths = read.csv('data/Mexico/covid19_deaths.csv', stringsAsFactors = FALSE)
  d_merge = copy(d_deaths)
  d_children = read.csv(paste0('data/Mexico/children.csv'), stringsAsFactors = FALSE)
  d_children$age = paste0(d_children$age%/%5 * 5,'-',d_children$age %/% 5 *5+4)
  d_children$age = ifelse(d_children$age %in% c('80-84', '85-89','90-94', '95-99', '100-104'), '80+', d_children$age)
  d_children = d_children %>% group_by(age, gender) %>% mutate(nb_c = mean(children)) %>%
    select(-children) %>% ungroup()%>%distinct()
  d_merge$gender = as.character(d_merge$gender)
  d_children$gender = ifelse(d_children$gender == 'female', 'Female', 'Male')
  
  d_m1 = merge(d_merge, d_children, by = c('age', 'gender')) 
  d_m1 = as.data.table(d_m1)
  d_m1[, orphans := round(deaths * nb_c)]
  d_m1$age = as.character(d_m1$age)
  d_m1$age = ifelse(d_m1$age == '0-4', '00-04', 
                    ifelse(d_m1$age == '5-9','05-09' ,d_m1$age))
  d_m1 = d_m1%>% arrange(age) 
  write_csv(path = paste0('data/Mexico/', country,'_orphans_all.csv'), d_m1)
  
  p <- ggplot(d_m1, aes(x = age, y = orphans, fill = gender)) +
    geom_bar(stat="identity", position=position_dodge()) +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))+
    xlab( 'Age of Parent') +
    ylab('Number of Orphans')+
    guides(fill=guide_legend(title="Sex of Parent"))
  ggsave(filename = "figures/orphans_all_age_mexico.pdf", p, width = 6, height = 5)

  d_summary = d_m1 %>% select(age, gender, deaths, orphans)
  d_summary$age = as.character(d_summary$age)
  d_summary$age = ifelse(d_summary$age %in% c('65-69', '70-74', '75-79', '80+'), '65+',
                         ifelse(d_summary$age %in% c('45-49', '50-54','55-59', '60-64'), '45-64',
                                ifelse(d_summary$age %in% c('00-04', '05-09', '10-14'), '0-14', '15-44')))
  d_summary = d_summary %>% group_by(age, gender) %>% 
    mutate( COVID19_deaths = as.integer(round(sum(deaths))),
            nb_orphans = as.integer(round(sum(orphans)))) %>% ungroup() %>% 
    select(-orphans, -deaths) %>%distinct()
  write_csv(path = 'data/Mexico/all_data.csv', d_summary)
  d_summary %>% filter(age != '0-14') %>% group_by(gender) %>% 
    mutate(covid19 = round(sum(COVID19_deaths)),
           orphans= round( sum( nb_orphans))) %>% ungroup()
}

# Nigeria
process_orphans_nigeria = function(){
  country = 'nigeria'
  d_deaths = read.csv('data/Nigeria/covid19_deaths.csv', stringsAsFactors = FALSE)
  d_merge = copy(d_deaths)
  d_merge$age = as.character(d_merge$age)
  d_children = read.csv(paste0('data/Nigeria/children.csv'), stringsAsFactors = FALSE)
  d_children$age = paste0((d_children$age)%/%5 * 5 ,'-',(d_children$age) %/% 5 *5+4)
  d_children$age = ifelse(d_children$age %in% c('85-89', '90-94', '95-99', '100-104'), '85+', d_children$age)
  d_children = d_children %>% group_by(age, gender) %>% mutate(nb_c = mean(children)) %>%
    select(-children) %>% ungroup()%>%distinct()
  d_merge$gender = as.character(d_merge$gender)
  d_children$gender = ifelse(d_children$gender == 'female', 'Female', 'Male')
  d_children$age = ifelse(d_children$age == '1-10', '0-10', d_children$age)
  d_m1 = merge(d_merge, d_children, by = c('age', 'gender')) 
  d_m1 = as.data.table(d_m1)
  d_m1[, orphans := round(deaths * nb_c)]
  d_m1$age = as.character(d_m1$age)
  d_m1 = d_m1%>% arrange(age) 
  d_m1$age <- factor(d_m1$age, levels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", 
                                          "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+"))
  write_csv(path = paste0('data/Nigeria/orphans_all.csv'), d_m1)
  
  p <- ggplot(d_m1, aes(x = age, y = orphans, fill = gender)) +
    geom_bar(stat="identity", position=position_dodge()) +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))+
    xlab( 'Age of Parent') +
    ylab('Number of Orphans')+
    guides(fill=guide_legend(title="Sex of Parent"))
  ggsave(filename = "figures/orphans_all_age_nigeria.pdf", p, width = 6, height = 5)

  d_summary = d_m1 %>% select(age, gender, deaths, orphans)
  d_summary$age = as.character(d_summary$age)
  d_summary$age = ifelse(d_summary$age %in% c('0-4', '5-9', '10-14', '15-19'), '0-19',
                         ifelse(d_summary$age %in% c('20-24', '25-29', '30-34', '35-39', '40-44', '45-49'), '20-49',
                                ifelse(d_summary$age %in% c('50-54', '55-59', '60-64', '64-69'), '50-69', '70+')))
  d_summary = d_summary %>% group_by(age, gender) %>% 
    mutate( COVID19_deaths = as.integer(round(sum(deaths))),
            nb_orphans = as.integer(round(sum(orphans)))) %>% ungroup() %>% 
    select(-orphans, -deaths) %>%distinct()
  write_csv(path = 'data/Nigeria/all_data.csv', d_summary)
  d_summary %>% filter(age != '0-19') %>% group_by(gender) %>% 
    mutate(covid19 = round(sum(COVID19_deaths)),
           orphans= round( sum( nb_orphans))) %>% ungroup()
}

# Peru
process_orphans_peru = function(){
  country = 'peru'
  d_deaths = read.csv('data/Peru/covid19_deaths.csv', stringsAsFactors = FALSE)
  setnames(d_deaths, 'COVID19_deaths','deaths')
  d_merge = copy(d_deaths)
  d_children = read.csv(paste0('data/Peru/children.csv'), stringsAsFactors = FALSE)
  d_children$age = paste0((d_children$age)%/%10 * 10 ,'-',(d_children$age) %/% 10 *10+9)
  d_children$age = ifelse(d_children$age %in% c('90-99','100-109'), '90+', d_children$age)
  d_children = d_children %>% group_by(age, gender) %>% mutate(nb_c = mean(children)) %>%
    select(-children) %>% ungroup()%>%distinct()
  d_merge$gender = as.character(d_merge$gender)
  d_children$gender = ifelse(d_children$gender == 'female', 'Female', 'Male')
  
  d_m1 = merge(d_merge, d_children, by = c('age', 'gender')) 
  d_m1 = as.data.table(d_m1)
  d_m1[, orphans := round(deaths * nb_c)]
  d_m1$age = as.character(d_m1$age)
  d_m1 = d_m1%>% arrange(age) 

  write_csv(path = paste0('data/Peru/orphans_all.csv'), d_m1)
  
  p <- ggplot(d_m1, aes(x = age, y = orphans, fill = gender)) +
    geom_bar(stat="identity", position=position_dodge()) +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))+
    xlab( 'Age of Parent') +
    ylab('Number of Orphans')+
    guides(fill=guide_legend(title="Sex of Parent"))
  ggsave(filename = "figures/orphans_all_age_peru.pdf", p, width = 6, height = 5)

  d_summary = d_m1 %>% select(age, gender, deaths, orphans)
  d_summary$age = as.character(d_summary$age)
  d_summary$age = ifelse(d_summary$age %in% c('70-79', '80-89', '90+'), '70+',
                         ifelse(d_summary$age %in% c('50-59','60-69'), '50-69',
                                ifelse(d_summary$age %in% c('20-29', '30-39', '40-49'), '20-49','0-19')))
  d_summary = d_summary %>% group_by(age, gender) %>% 
    mutate( COVID19_deaths = as.integer(round(sum(deaths))),
            nb_orphans = as.integer(round(sum(orphans)))) %>% ungroup() %>% 
    select(-orphans, -deaths) %>%distinct()
  write_csv(path = 'data/Peru/all_data.csv', d_summary)
  d_summary %>% filter(age != '0-19') %>% group_by(gender) %>% 
    mutate(covid19 = round(sum(COVID19_deaths)),
           orphans= round( sum( nb_orphans))) %>% ungroup()
}

# Philippines
process_orphans_philippines= function(){
  country = 'philippines'
  d_deaths = read.csv('data/Philippines/covid19_deaths.csv', stringsAsFactors = FALSE)
  d_merge = copy(d_deaths)
  d_children = read.csv(paste0('data/Philippines/children.csv'), stringsAsFactors = FALSE)
  d_children$age = paste0((d_children$age)%/%5 * 5 ,'-',(d_children$age) %/% 5 *5+4)
  d_children$age = ifelse(d_children$age %in% c('80-84', "85-89", "90-94", "95-99", '100-104', '105-109'), '80+', d_children$age)
  d_children = d_children %>% group_by(age, gender) %>% mutate(nb_c = mean(children)) %>%
    select(-children) %>% ungroup()%>%distinct()
  d_merge$gender = as.character(d_merge$gender)
  
  d_m1 = merge(d_merge, d_children, by = c('age', 'gender')) 
  d_m1 = as.data.table(d_m1)
  d_m1[, orphans := round(COVID19_deaths * nb_c)]
  d_m1 = d_m1%>% arrange(age) 
  write_csv(path = paste0('data/Philippines/orphans_all.csv'), d_m1)
  
  d_m1$age <- factor(d_m1$age, levels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", 
                                          "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80+"))
  p <- ggplot(d_m1, aes(x = age, y = orphans, fill = gender)) +
    geom_bar(stat="identity", position=position_dodge()) +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))+
    xlab( 'Age of Parent') +
    ylab('Number of Orphans')+
    guides(fill=guide_legend(title="Sex of Parent"))
  ggsave(filename = "figures/orphans_all_age_philippines.pdf", p, width = 6, height = 5)
  
  d_summary = d_m1 %>% select(age, gender, COVID19_deaths, orphans)
  d_summary$age = as.character(d_summary$age)
  d_summary$age = ifelse(d_summary$age %in% c('0-4', '5-9', '10-14'), '0-14',
                         ifelse(d_summary$age %in% c('45-49','50-54', '55-59', '60-64'), '45-64',
                                ifelse(d_summary$age %in% c("65-69","70-74","75-79","80+"),'65+', '15-44')))
  d_summary = d_summary %>% group_by(age, gender) %>% 
    mutate(max_deaths = as.integer(round((sum(COVID19_deaths)))),
           nb_orphans = as.integer(round(sum(orphans)))) %>% ungroup() %>% 
    select(-orphans, -COVID19_deaths) %>%distinct()
  write_csv(path = 'data/Philippines/all_data.csv', d_summary)
  d_summary %>% filter(age != '0-14') %>% group_by(gender) %>% 
    mutate(total_deaths= round(sum(max_deaths)),
           orphans= round( sum( nb_orphans))) %>% 
    ungroup()
}

# Poland
process_orphans_poland = function(){
  country = 'poland'
  d_deaths = read.csv('data/Poland/poland_all.csv', stringsAsFactors = FALSE)
  d_merge = copy(d_deaths)
  d_children = read.csv(paste0('data/Poland/children.csv'), stringsAsFactors = FALSE)
  d_children$age = paste0((d_children$age)%/%10 * 10 ,'-',(d_children$age) %/% 10 *10+9)
  d_children$age = ifelse(d_children$age %in% c('90-99', '100-109'), '90+', d_children$age)
  d_children = d_children %>% group_by(age, gender) %>% mutate(nb_c = mean(children)) %>%
    select(-children) %>% ungroup()%>%distinct()
  d_merge$gender = as.character(d_merge$gender)
  d_children$gender = ifelse(d_children$gender == 'female', 'Female', 'Male')
  
  d_m1 = merge(d_merge, d_children, by = c('age', 'gender')) 
  d_m1 = as.data.table(d_m1)
  d_m1[, orphans := round(death * nb_c)]
  d_m1 = d_m1%>% arrange(age) 
  write_csv(path = paste0('data/Poland/orphans_all.csv'), d_m1)
  
  p <- ggplot(d_m1, aes(x = age, y = orphans, fill = gender)) +
    geom_bar(stat="identity", position=position_dodge()) +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))+
    xlab( 'Age of Parent') +
    ylab('Number of Orphans')+
    guides(fill=guide_legend(title="Sex of Parent"))
  ggsave(filename = "figures/orphans_all_age_poland.pdf", p, width = 6, height = 5)
  d_summary = d_m1 %>% select(age, gender, death, orphans)
  d_summary$age = as.character(d_summary$age)
  d_summary$age = ifelse(d_summary$age %in% c('70-79', '80-89', '90+'), '70+',
                         ifelse(d_summary$age %in% c('50-59','60-69'), '50-69',
                                ifelse(d_summary$age %in% c('20-29', '30-39', '40-49'), '20-49','0-19')))
  d_summary = d_summary %>% group_by(age, gender) %>% 
    mutate(max_deaths = as.integer(round((sum(death)))),
           nb_orphans = as.integer(round(sum(orphans)))) %>% ungroup() %>% 
    select(-orphans, -death) %>%distinct()
  write_csv(path = 'data/Poland/all_data.csv', d_summary)
  d_summary %>% filter(age != '0-19') %>% group_by(gender) %>% 
    mutate(total_deaths= round(sum(max_deaths)),
           orphans= round( sum( nb_orphans))) %>% 
    ungroup()
}

# Russia
process_orphans_russia = function(){
  country = 'russian_federation'
  d_deaths = read.csv('data/Russia/all_excess_deaths.csv', stringsAsFactors = FALSE)
  d_merge = copy(d_deaths)
  
  d_children = read.csv(paste0('data/Russia/children.csv'), stringsAsFactors = FALSE)
  d_children$age = paste0((d_children$age)%/%5 * 5 ,'-',(d_children$age) %/% 5 *5+4)
  d_children$age = ifelse(d_children$age %in% c('100-104'), '100+', d_children$age)
  d_children = d_children %>% group_by(age, gender) %>% mutate(nb_c = mean(children)) %>%
    select(-children) %>% ungroup()%>%distinct()
  d_merge$sex = as.character(d_merge$sex)
  d_children$gender = ifelse(d_children$gender == 'female', 'Female', 'Male')
  
  d_m1 = left_join(d_merge, d_children, by = c('age', 'sex' = 'gender')) 
  d_m1 = as.data.table(d_m1)
  setnames(d_m1, 'weighted_excess_deaths', 'deaths')
  
  d_m1[, orphans := round(deaths * nb_c)]
  d_m1$age = factor(d_m1$age, levels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39",  "40-44", "45-49",
                                         "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-94", 
                                         "95-99", "100+"))
  d_m1 <- d_m1 %>% arrange(age) 
  write_csv(path = paste0('data/Russia/orphans_all.csv'), d_m1)
  
  p <- ggplot(d_m1, aes(x = age, y = orphans, fill = sex)) +
    geom_bar(stat="identity", position=position_dodge()) +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))+
    xlab( 'Age of Parent') +
    ylab('Number of Orphans')+
    guides(fill=guide_legend(title="Sex of Parent"))
  ggsave(filename = "figures/orphans_all_age_russia.pdf", p, width = 6, height = 5)
  
  d_summary = d_m1 %>% select(age, sex,
                              deaths, orphans)
  d_summary$age = as.character(d_summary$age)
  d_summary$age = ifelse(d_summary$age %in% c('0-4', '5-9', '10-14'), '0-14',
                         ifelse(d_summary$age %in% c('45-49','50-54', '55-59', '60-64'), '45-64',
                                ifelse(d_summary$age %in% c("65-69","70-74","75-79","80-84","85-89",
                                                            "90-94", "95-99", "100+" ),'65+', '15-44')))
  d_summary = d_summary %>% group_by(age, sex) %>% 
    mutate(max_deaths = as.integer(round((sum(deaths)))),
           nb_orphans = as.integer(round(sum(orphans)))) %>% 
    ungroup() %>% 
    select(-orphans, -deaths) %>%
    distinct()
  write_csv(path = 'data/Russia/all_data.csv', d_summary)
  d_summary %>% filter(age != '0-14') %>% 
    group_by(sex) %>% 
    mutate(total_deaths= round(sum(max_deaths)),
           orphans= round( sum( nb_orphans))) %>% 
    ungroup()
  
}

# Spain
process_orphans_spain = function(){
  country = 'spain'
  d_deaths = read.csv('data/Spain/spain_all.csv', stringsAsFactors = FALSE)
  d_merge = d_deaths
  d_children = read.csv(paste0('data/Spain/children.csv'), stringsAsFactors = FALSE)
  d_children$age = paste0((d_children$age)%/%10 * 10 ,'-',(d_children$age) %/% 10 *10+9)
  d_children$age = ifelse(d_children$age %in% c('80-89', '90-99','100-109'), '80+', d_children$age)
  d_children = d_children %>% group_by(age, gender) %>% mutate(nb_c = mean(children)) %>%
    select(-children) %>% ungroup()%>%distinct()
  d_merge$gender = as.character(d_merge$gender)
  d_children$gender = ifelse(d_children$gender == 'female', 'Female', 'Male')
  
  d_m1 = merge(d_merge, d_children, by = c('age', 'gender')) 
  d_m1 = as.data.table(d_m1)
  d_m1[, orphans := round(deaths * nb_c)]
  d_m1 = d_m1%>% arrange(age) 
  write_csv(path = paste0('data/Spain/orphans_all.csv'), d_m1)
  
  p <- ggplot(d_m1, aes(x = age, y = orphans, fill = gender)) +
    geom_bar(stat="identity", position=position_dodge()) +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))+
    xlab( 'Age of Parent') +
    ylab('Number of Orphans')+
    guides(fill=guide_legend(title="Sex of Parent"))
  ggsave(filename = "figures/orphans_all_age_spain.pdf", p, width = 6, height = 5)
  
  d_summary = d_m1 %>% select(age, gender, excess_deaths,covid_deaths, 
                              deaths, orphans)
  d_summary$age = as.character(d_summary$age)
  d_summary$age = ifelse(d_summary$age %in% c('70-79', '80+'), '70+',
                         ifelse(d_summary$age %in% c('50-59','60-69'), '50-69',
                                ifelse(d_summary$age %in% c('20-29', '30-39', '40-49'), '20-49','0-19')))
  d_summary = d_summary %>% group_by(age, gender) %>% 
    mutate(nb_excess = round(sum(excess_deaths)),
           nb_covid = round(sum(covid_deaths)),
           max_deaths = as.integer(round((sum(deaths)))),
           nb_orphans = as.integer(round(sum(orphans)))) %>% ungroup() %>% 
    select(-orphans, -deaths, -covid_deaths, -excess_deaths) %>%distinct()
  write_csv(path = 'data/Spain/all_data.csv', d_summary)
  as.data.frame(d_summary %>% filter(age != '0-19') %>% group_by(gender) %>% 
                  mutate(total_excess = round(sum(nb_excess)),
                         total_covid = round(sum(nb_covid)),
                         total_deaths= round(sum(max_deaths)),
                         orphans= round( sum( nb_orphans))) %>% ungroup())
}

# South Africa
process_orphans_south_africa = function(){
  country = 'south_africa'
  d_deaths = read.csv('data/SouthAfrica/covid19_deaths.csv', stringsAsFactors = FALSE)
  d_merge = copy(d_deaths)
  d_merge$age = as.character(d_merge$age)
  setnames(d_merge, 'COVID19_deaths', 'deaths')
  d_children = read.csv(paste0('data/SouthAfrica/children.csv'), stringsAsFactors = FALSE)
  d_children$age = paste0(d_children$age%/%10 * 10,'-',d_children$age %/% 10 *10+9)
  d_children$age = ifelse(d_children$age %in% c('80-89', '90-99', '100-109'), '80+', d_children$age)
  d_children = d_children %>% group_by(age, gender) %>% mutate(nb_c = mean(children)) %>%
    select(-children) %>% ungroup()%>%distinct()
  d_merge$gender = as.character(d_merge$gender)
  d_merge$gender = ifelse(d_merge$gender == 'Female', 'female','male')
  
  d_m1 = merge(d_merge, d_children, by = c('age', 'gender')) 
  d_m1 = as.data.table(d_m1)
  d_m1[, orphans := round(deaths * nb_c)]
  #d_m = merge(d_m1, f_ew, by = c('age', 'gender'))
  d_m1$age = as.character(d_m1$age)
  d_m1 = d_m1%>% arrange(age) 
  write_csv(path = paste0('data/SouthAfrica/orphans_all.csv'), d_m1)
  p <- ggplot(d_m1, aes(x = age, y = orphans, fill = gender)) +
    geom_bar(stat="identity", position=position_dodge()) +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))+
    xlab( 'Age of Parent') +
    ylab('Number of Orphans')+
    guides(fill=guide_legend(title="Sex of Parent"))
  ggsave(filename = "figures/orphans_all_age_south_africa.pdf", p, width = 6, height = 5)

  d_summary = d_m1 %>% select(age, gender, deaths, orphans)
  
  d_summary$age = ifelse(d_summary$age %in% c('70-79', '80-89', '90+'), '70+',
                         ifelse(d_summary$age %in% c('50-59','60-69'), '50-69',
                                ifelse(d_summary$age %in% c('20-29', '30-39', '40-49'), '20-49','0-19')))
  d_summary = d_summary %>% group_by(age, gender) %>% mutate(#excess = sum(excess_death),
    covid19_deaths = as.integer(round(sum(deaths))),
    
    nb_orphans = as.integer(round( sum(orphans)))) %>% ungroup() %>% 
    select(-orphans, -deaths) %>%distinct()
  
  write_csv(path = 'data/SouthAfrica/all_data.csv', d_summary)
  d_summary %>% filter(age != '0-19') %>% group_by(gender) %>% mutate(#excess = sum(excess_death),
    deaths= as.integer(sum(covid19_deaths)),
    
    orphans= as.integer( sum( nb_orphans))) %>% ungroup()
}

# USA
process_orphans_usa = function(){
  country = 'usa'
  d_deaths = read.csv('data/USA/usa_all.csv', stringsAsFactors = FALSE)
  d_merge = d_deaths

  d_children = read.csv(paste0('data/USA/children.csv'), stringsAsFactors = FALSE)
  d_children$age = paste0((d_children$age-5)%/%10 * 10 +5,'-',(d_children$age-5) %/% 10 *10+9 +5)
  d_children$age = ifelse(d_children$age %in% c('-5-4', '5-14'), '0-14',
                          ifelse(d_children$age %in% c('85-94', '95-104'), '85+', d_children$age))
  d_children = d_children %>% group_by(age, gender) %>% mutate(nb_c = mean(children)) %>%
    select(-children) %>% ungroup()%>%distinct()
  d_merge$gender = as.character(d_merge$gender)
  d_children$gender = ifelse(d_children$gender == 'female', 'Female', 'Male')
  
  d_m1 = merge(d_merge, d_children, by = c('age', 'gender')) 
  d_m1 = as.data.table(d_m1)
  d_m1[, orphans := round(deaths * nb_c)]
  d_m1 = d_m1%>% arrange(age) 
  write_csv(path = paste0('data/USA/orphans_all.csv'), d_m1)
  
  p <- ggplot(d_m1, aes(x = age, y = orphans, fill = gender)) +
    geom_bar(stat="identity", position=position_dodge()) +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))+
    xlab( 'Age of Parent') +
    ylab('Number of Orphans')+
    guides(fill=guide_legend(title="Sex of Parent"))
  ggsave(filename = "figures/orphans_all_age_usa.pdf", p, width = 6, height = 5)
 
  
  d_summary = d_m1 %>% select(age, gender, excess_deaths, covid_deaths,
                              deaths, orphans)
  d_summary$age = as.character(d_summary$age)
  d_summary$age = ifelse(d_summary$age %in% c('65-74', '75-84', '85+'), '65+',
                         ifelse(d_summary$age %in% c('45-54','55-64'), '45-64',
                                ifelse(d_summary$age %in% c('0-14'), '0-14', '15-44')))
  d_summary = d_summary %>% group_by(age, gender) %>% 
    mutate(excess = round(sum(excess_deaths)),
           COVID19 = round(sum(covid_deaths)),
           max_deaths = as.integer(round((sum(deaths)))),
           nb_orphans = as.integer(round(sum(orphans)))) %>% ungroup() %>% 
    select(-orphans, -deaths, -excess_deaths, -covid_deaths) %>%
    distinct()

  write_csv(path = 'data/USA/all_data.csv', d_summary)
  as.data.frame(d_summary %>% filter(age != '0-14') %>% 
                  group_by(gender) %>% 
                  mutate(total_excess = sum(excess),
                         total_covid19 = round(sum(COVID19)),
                        total_deaths= round(sum(max_deaths)),
                        orphans= round( sum( nb_orphans))) %>% 
                  ungroup())
}

# Zimbabwe
process_orphans_zimbabwe = function(){
  country = 'zimbabwe'
  d_deaths = read.csv('data/Zimbabwe/covid19_deaths.csv', stringsAsFactors = FALSE)
  d_merge = copy(d_deaths)
  d_merge$age = as.character(d_merge$age)
  d_children = read.csv(paste0('data/Zimbabwe/children.csv'), stringsAsFactors = FALSE)
  d_children$age = paste0((d_children$age)%/%10 * 10 + 1 ,'-',(d_children$age) %/% 10 *10+10)
  d_children$age = ifelse(d_children$age %in% c('91-100', '101-110'), '91+', d_children$age)
  d_children = d_children %>% group_by(age, gender) %>% mutate(nb_c = mean(children)) %>%
    select(-children) %>% ungroup()%>%distinct()
  d_merge$gender = as.character(d_merge$gender)
  d_children$gender = ifelse(d_children$gender == 'female', 'Female', 'Male')
  d_m1 = merge(d_merge, d_children, by = c('age', 'gender')) 
  d_m1 = as.data.table(d_m1)
  d_m1[, orphans := round(deaths * nb_c)]
  d_m1$age = as.character(d_m1$age)
  d_m1 = d_m1%>% arrange(age) 
  write_csv(path = paste0('data/Zimbabwe/orphans_all.csv'), d_m1)
  
  p <- ggplot(d_m1, aes(x = age, y = orphans, fill = gender)) +
    geom_bar(stat="identity", position=position_dodge()) +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))+
    xlab( 'Age of Parent') +
    ylab('Number of Orphans')+
    guides(fill=guide_legend(title="Sex of Parent"))
  ggsave(filename = "figures/orphans_all_age_zimbabwe.pdf", p, width = 6, height = 5)
  
  d_summary = d_m1 %>% select(age, gender, deaths, orphans)
  d_summary$age = as.character(d_summary$age)
  d_summary$age = ifelse(d_summary$age %in% c('1-10', '11-20'), '1-20',
                         ifelse(d_summary$age %in% c('21-30', '31-40', '41-50'), '21-50',
                                ifelse(d_summary$age %in% c('51-60', '61-70'), '51-70', '71+')))
  d_summary = d_summary %>% group_by(age, gender) %>% 
    mutate( COVID19_deaths = as.integer(round(sum(deaths))),
            nb_orphans = as.integer(round(sum(orphans)))) %>% ungroup() %>% 
    select(-orphans, -deaths) %>%distinct()
  write_csv(path = 'data/Zimbabwe/all_data.csv', d_summary)
  d_summary %>% filter(age != '1-20') %>% group_by(gender) %>% 
    mutate(covid19 = round(sum(COVID19_deaths)),
           orphans= round( sum( nb_orphans))) %>% ungroup()
}