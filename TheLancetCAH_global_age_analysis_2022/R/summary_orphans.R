source("TheLancetCAH_global_age_analysis_2022/R/process_fertility.R") 
source("TheLancetCAH_global_age_analysis_2022/R/colombia_excess.R")

combine_orphans <- function(country, grand_parents, month = ""){
  if (country == "EnglandWales"){
    data <- read.csv("TheLancetCAH_global_age_analysis_2022/data/UK/england_wales_all_data.csv")
    names(data) = c('age', "gender", "excess", "covid19_deaths", "deaths", "nb_orphans")
    data$gender <- tolower(data$gender)
  } else if (country %in% c("France", "Italy", "Spain", "USA")){
    data <- read.csv(paste0("TheLancetCAH_global_age_analysis_2022/data/", country, "/all_data.csv"))
    names(data) = c('age', "gender", "excess", "covid19_deaths", "deaths", "nb_orphans")
    data$gender <- tolower(data$gender)
  } else {
    data <- read.csv(paste0("TheLancetCAH_global_age_analysis_2022/data/", country, "/all_data.csv"))
    names(data) = c('age', "gender", "deaths", "nb_orphans")
    data$gender <- tolower(data$gender)
  }

  ifrs <- data.frame(age = c("15-44", "20-49", "21-50", "15-49", "20-39",
                             "45-64", "50-69", "51-70", "50-59", "50-64", "40-59",
                             "65+", "70+", "71+", "60+"),
                     ifr = c(0.0004, 0.0004, 0.0004, 0.0004, 0.0004,
                             0.0057,  0.0057, 0.0057, 0.0057, 0.0057, 0.0057,
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
                            "older persons co-residing", "number")
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
                                         grand_parents$skip_generation)/100*(grand_parents$grand_deaths * 0.89)
  p_gp_m_f <- 1 - (2*ifr*sar + ifr^2*sar^2) #proportion of cases that have lost one or more parents
  grand_parents$lost_parents <- grand_parents$gparent_plus_parent * p_gp_m_f # proportion of mult generation households who haven't lost a parent
  grand_parents$mult_double <- grand_parents$lost_parents * 0.0217 * 0.37 
  mult_orphan <- grand_parents %>% 
    select(age, gender, lost_parents, mult_double) %>%
    mutate("min" = min(mult_double)) %>%
    mutate(orphans = lost_parents - min) %>%
    select(age, gender, orphans, min)
  mult_double <- round(mult_orphan$min[1])
  f_mult <- round(mult_orphan$orphans[sg_orphan$gender == "female"])
  m_mult <- round(mult_orphan$orphans[sg_orphan$gender == "male"])
  
  orphans <- sum(f_orphans + m_orphans + double_orphans)
  primary <- sum(f_orphans + m_orphans + double_orphans + f_sg +  m_sg +  sg_double)
  all <- sum(f_orphans + m_orphans + double_orphans + f_sg + m_sg + 
               sg_double + f_mult + m_mult + mult_double)
  comb <- c(f_orphans, m_orphans, double_orphans, orphans,
            f_sg, m_sg, sg_double, primary, 
            f_mult, m_mult, mult_double, all)
  print(comb)
  
  if (country == "Brazil"){ # "The total of excess deaths estimated in this study between March and May 2020 (39,146) across the country was 33.5% higher than the number of deaths accumulated by COVID-19 as of May 31, as reported by the Ministry of Health (29,314 deaths)"
    factor = 1.335
    comb <- round(comb * factor)
    comb[4] <- sum(comb[1:3])
    comb[8] <- sum(comb[4:7])
    comb[12] <- sum(comb[c(8:11)])
    print(comb)
    deaths = round(sum(data$deaths) * factor)
    comb <- c(deaths, comb, comb[12]/deaths)

  } else if (country == "India"){ #https://www.cgdev.org/sites/default/files/three-new-estimates-indias-all-cause-excess-mortality-during-covid-19-pandemic.pdf
    factor = 399489 / 3.97e6 #0.766 #0.15 
    comb <- round(comb / factor)
    comb[4] <- sum(comb[1:3])
    comb[8] <- sum(comb[4:7])
    comb[12] <- sum(comb[c(8:11)])
    print(comb)
    deaths = round(sum(data$deaths) / factor)
    comb <- c(deaths, comb, comb[12]/deaths)
    
  } else if (country == "Mexico"){# 254,625 more deaths -- from all causes - 118,598 confirmed Covid-19 deaths - end nov
    # Change to inflate covid deaths by 60%
    #https://www.theguardian.com/world/2021/mar/28/mexico-covid-death-toll-rise-60-percent
    factor =  254625/118598#321000/201429 #
    comb <- round(comb * factor)
    comb[4] <- sum(comb[1:3])
    comb[8] <- sum(comb[4:7])
    comb[12] <- sum(comb[c(8:11)])
    print(comb)
    deaths = round(sum(data$deaths) * factor)
    comb <- c(deaths, comb, comb[12]/deaths)
    
  } else if (country == "Peru" & month == ""){# EM of 36,322. Meanwhile, there were only 9860 officially COVID-19 deaths (Stand June 30, 2020)
    factor = 36322/9860
    comb <- round(comb * factor)
    comb[4] <- sum(comb[1:3])
    comb[8] <- sum(comb[4:7])
    comb[12] <- sum(comb[c(8:11)])
    print(comb)
    deaths = round(sum(data$deaths) * factor)
    comb <- c(deaths, comb, comb[12]/deaths)
    
  } else if (country == "SouthAfrica") { # https://www.samrc.ac.za/reports/report-weekly-deaths-south-africa
    #83,918 excess may to 5th Jan 30524 covid deaths JHU
    factor = 83918/30524
    comb <- round(comb * factor)
    comb[4] <- sum(comb[1:3])
    comb[8] <- sum(comb[4:7])
    comb[12] <- sum(comb[c(8:11)])
    print(comb)
    deaths = round(sum(data$deaths) * factor)
    comb <- c(deaths, comb, comb[12]/deaths)
    
  } else if (country == "Iran") {
    #2 numbers https://www.medrxiv.org/content/10.1101/2020.12.07.20245621v1.full.pdf fig 5
    factor = mean(21919/9507, 58198/24478)
    comb <- round(comb * factor)
    comb[4] <- sum(comb[1:3])
    comb[8] <- sum(comb[4:7])
    comb[12] <- sum(comb[c(8:11)])
    print(comb)
    deaths = round(sum(data$deaths) * factor)
    comb <- c(deaths, comb, comb[12]/deaths)
    
  } else if (country == "Colombia") {
    # excess deaths week 10 to week 44 from source.  COVID deaths from JHU on 1st Nov
    factor = calculate_colombia_excess()/31796
    comb <- round(comb * factor)
    comb[4] <- sum(comb[1:3])
    comb[8] <- sum(comb[4:7])
    comb[12] <- sum(comb[c(8:11)])
    print(comb)
    deaths = round(sum(data$deaths) * factor)
    comb <- c(deaths, comb, comb[12]/deaths)
    
  } else {
    comb <- c(sum(data$deaths), comb, comb[12]/sum(data$deaths))
  }

  return(comb)
}


