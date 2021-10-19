
combine_orphans <- function(country, grand_parents){
  data <- read.csv( paste0('data/USA/all_data_',country,'.csv'))
  names(data) = c('age', "gender", "excess", "covid19_deaths", "deaths", "nb_orphans")
  data$gender <- tolower(data$gender)

  ifrs <- data.frame(age = c("15-44", "20-49", "21-50", "15-49", "20-39", "15-29",
                             "45-64", "50-69", "51-70", "50-59", "50-64", "40-59", "30-64", 
                             "65+", "70+", "71+", "60+"),
                     ifr = c(0.0004, 0.0004, 0.0004, 0.0004, 0.0004,0.0004,
                             0.0057,  0.0057, 0.0057, 0.0057, 0.0057, 0.0057,0.0057,
                             0.0139, 0.0139, 0.0139, 0.0139))
  
  data_join <- left_join(data, ifrs, by = "age")
  data_join <- data_join[!is.na(data_join$ifr),]
  data_join$double <- data_join$nb_orphans * data_join$ifr * 0.37
  
  max_orphan <- data_join %>% 
    group_by(age) %>% mutate("min" = min(double)) %>% ungroup() %>%
    mutate(orphans = nb_orphans - min) %>%
    select(age, gender, orphans, min)
  
  double_orphans <- round(sum(max_orphan$min[max_orphan$gender =='female'], na.rm = TRUE))
  f_orphans <- round(sum(max_orphan$orphans[max_orphan$gender =='female'], na.rm = TRUE))
  m_orphans <- round(sum(max_orphan$orphans[max_orphan$gender =='male'], na.rm = TRUE))

  names(grand_parents) <- c("age", "gender", "grand_deaths", "skip_generation", "value", 
                            "coresiding_caregiver", "value_cc", "older persons co-residing", "number")
  grand_parents$cc_double <- grand_parents$value_cc * 0.0217 * 0.37
  grand_parents$gender <- tolower(grand_parents$gender)
  grand_parents$gender <- ifelse(grand_parents$gender == "f", "female", 
                                 ifelse(grand_parents$gender == "m", "male", grand_parents$gender))
  cc_orphan <- grand_parents %>% 
    select(age, gender, value_cc, cc_double) %>%
    mutate("min" = min(cc_double)) %>%
    mutate(orphans = value_cc - min) %>%
    select(age, gender, orphans, min)
  cc_double <- round(cc_orphan$min[1])
  f_cc <- round(cc_orphan$orphans[cc_orphan$gender == "female"])
  m_cc <- round(cc_orphan$orphans[cc_orphan$gender == "male"])

  grand_parents$sg_double <- grand_parents$value * 0.0217 * 0.37
  grand_parents$gender <- tolower(grand_parents$gender)
  grand_parents$gender <- ifelse(grand_parents$gender == "f", "female", 
                                 ifelse(grand_parents$gender == "m", "male", grand_parents$gender))
  sg_orphan <- grand_parents %>% 
    select(age, gender, value, sg_double) %>%
    mutate("min" = min(sg_double)) %>%
    mutate(orphans = value - min) %>%
    select(age, gender, orphans, min)
  sg_double <- round(sg_orphan$min[1])
  f_sg <- round(sg_orphan$orphans[sg_orphan$gender == "female"])
  m_sg <- round(sg_orphan$orphans[sg_orphan$gender == "male"])
  ifr <- 0.0139
  sar <- 0.37
  
  #### Assumption that skip generation and cohabiting are mutually exclusive
  grand_parents$gparent_plus_parent = (grand_parents$`older persons co-residing` - 
                                         grand_parents$skip_generation)/100*grand_parents$grand_deaths
  p_gp_m_f <- 1 - (2*ifr*sar + ifr^2*sar^2) #proportion of cases that have lost one or more parents
  grand_parents$lost_parents <- grand_parents$gparent_plus_parent * p_gp_m_f # proportion of mult generation households who haven't lost a parent
  # in us data categories are already mutually excl. so add cases losing non-skipgen grandparent
  grand_parents$lost_parents <- grand_parents$number * p_gp_m_f # proportion of mult generation households who haven't lost a parent
  grand_parents$lost_coresgparents <- grand_parents$value_cc * p_gp_m_f # proportion of mult generation households who haven't lost a parent
  
  grand_parents$cc_double <- grand_parents$lost_coresgparents * 0.0217 * 0.37 
  cc_orphan <- grand_parents %>% 
    select(age, gender, lost_coresgparents, cc_double) %>%
    mutate("min" = min(cc_double)) %>%
    mutate(orphans = lost_coresgparents - min) %>%
    select(age, gender, orphans, min)
  cc_double <- round(cc_orphan$min[1])
  f_cc <- round(cc_orphan$orphans[cc_orphan$gender == "female"])
  m_cc <- round(cc_orphan$orphans[cc_orphan$gender == "male"])
    
  grand_parents$mult_double <- grand_parents$lost_parents * 0.0217 * 0.37 
  mult_orphan <- grand_parents %>% 
    select(age, gender, lost_parents, mult_double) %>%
    mutate("min" = min(mult_double)) %>%
    mutate(orphans = lost_parents - min) %>%
    select(age, gender, orphans, min)
  mult_double <- round(mult_orphan$min[1])
  f_mult <- round(mult_orphan$orphans[mult_orphan$gender == "female"])
  m_mult <- round(mult_orphan$orphans[mult_orphan$gender == "male"])
  
  primary <- sum(f_orphans + m_orphans + double_orphans + f_sg +  m_sg +  sg_double + f_cc + m_cc + cc_double)
  all <- sum(f_orphans + m_orphans + double_orphans + f_sg + m_sg + 
               sg_double + f_cc + m_cc + cc_double + f_mult + m_mult + mult_double)
  comb <- c(f_orphans, m_orphans, double_orphans, f_sg, m_sg, sg_double, f_cc, m_cc, cc_double, primary, 
            f_mult, m_mult, mult_double, all)
  print(comb)
  

  comb <- c(sum(data$deaths), comb, comb[11]/sum(data$deaths))
  
  return(comb)
}



