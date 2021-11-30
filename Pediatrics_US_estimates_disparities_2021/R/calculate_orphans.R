
# USA by state
process_orphans_usa_bystate = function(country,s,r){
  #state = gsub('usa_([A-Za-z]+)_*','\\1',country)
  d_deaths = read.csv('data/USA/usa_states.csv', stringsAsFactors = FALSE)
  d_merge <- subset(d_deaths,State==s & Race.and.Hispanic.Origin.Group==r)

  d_children = read.csv(paste0('data/USA/', country,'_children.csv'), stringsAsFactors = FALSE)
  d_children$age = ifelse(d_children$age %in% c(1:14), '0-14',
                          ifelse(d_children$age %in% c(15:29), '15-29',
                                 ifelse(d_children$age %in% c(30:49), '30-49',
                                        ifelse(d_children$age %in% c(50:64), '50-64',
                                               ifelse(d_children$age %in% c(65:74), '65-74',
                                                   ifelse(d_children$age %in% c(75:84), '75-84',
                                                                           '85+'))))))
  # truncate male fertility to 60 (so no men over 77 have children under 18)
  d_children <- data.table(d_children)
  d_children$ageid <- rep(seq(1,100,1),2)
  d_children[gender=='male' & ageid>77,children:=0]
  d_children[,ageid:=NULL]

  d_children = d_children %>% group_by(age, gender) %>% mutate(nb_c = mean(children)) %>%
    select(-children) %>% ungroup()%>%distinct()
  d_merge$gender = as.character(d_merge$gender)
  d_children$gender = ifelse(d_children$gender == 'female', 'Female', 'Male')
  
  d_m1 = merge(d_merge, d_children, by = c('age', 'gender')) 
  d_m1 = as.data.table(d_m1)
  d_m1[, orphans := round(deaths * nb_c)]
  d_m1 = d_m1%>% arrange(age) 
  write_csv(path = paste0('data/USA/',country,'_orphans_all.csv'), d_m1)
  
  p <- ggplot(d_m1, aes(x = age, y = orphans, fill = gender)) +
    geom_bar(stat="identity", position=position_dodge()) +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))+
    xlab( 'Age of Parent') +
    ylab('Number of Orphans')+
    guides(fill=guide_legend(title="Sex of Parent"))
  ggsave(filename = paste0("figures/orphans_all_age_",country,".png"), p, width = 6, height = 5)
  
  
  d_summary = d_m1 %>% select(age, gender, excess_deaths, covid_deaths,
                              deaths, orphans)
  d_summary$age = as.character(d_summary$age)
  d_summary$age = ifelse(d_summary$age %in% c('65-74', '75-84', '85+'), '65+',
                         ifelse(d_summary$age %in% c('30-49','50-64'), '30-64',
                                ifelse(d_summary$age %in% c('0-14'), '0-14', '15-29')))
  d_summary = d_summary %>% group_by(age, gender) %>% 
    mutate(excess = round(sum(excess_deaths)),
           COVID19 = round(sum(covid_deaths)),
           max_deaths = as.integer(round((sum(deaths)))),
           nb_orphans = as.integer(round(sum(orphans)))) %>% ungroup() %>% 
    select(-orphans, -deaths, -excess_deaths, -covid_deaths) %>%
    distinct()
  
  write_csv(path = paste0('data/USA/all_data_',country,'.csv'), d_summary)
  as.data.frame(d_summary %>% filter(age != '0-14') %>% 
                  group_by(gender) %>% 
                  mutate(total_excess = sum(excess),
                         total_covid19 = round(sum(COVID19)),
                         total_deaths= round(sum(max_deaths)),
                         orphans= round( sum( nb_orphans))) %>% 
                  ungroup())
}
