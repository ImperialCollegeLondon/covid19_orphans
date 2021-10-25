library(tidyverse)
library(data.table)

# Argentina
process_orphans_age_argentina = function(uncertainty = FALSE, month = ""){
  if (uncertainty == FALSE){
    d_merge = read.csv(paste0('global_age_analysis_2021/data/Argentina/covid19_deaths', month, '.csv'), stringsAsFactors = FALSE)
    d_children = read.csv(paste0('global_age_analysis_2021/data/Colombia/child_all_list_both_sex.csv'), stringsAsFactors = FALSE)
  } else {
    d_merge = read.csv(paste0('global_age_analysis_2021/data/Argentina/covid19_deaths', month, '_un.csv'), stringsAsFactors = FALSE)
    d_children = read.csv(paste0('global_age_analysis_2021/data/Colombia/child_all_list_both_sex_un.csv'), stringsAsFactors = FALSE)
  }
  
  d_children$parents_age = paste0((d_children$parents_age)%/%20 * 20 ,'-',(d_children$parents_age) %/% 20 *20+19)
  d_children$parents_age = ifelse(d_children$parents_age %in% c('60-79', '80-99', '100-119'), '60+', d_children$parents_age)
  
  d_children = d_children %>% group_by(parents_age, gender, child_age) %>% mutate(nb_c = mean(prob)) %>%
    select(-prob) %>% ungroup()%>%distinct()
  d_merge$gender = as.character(d_merge$gender)
  d_children$gender = ifelse(d_children$gender == 'female', 'Female', 'Male')
  d_children$nb_c[which(d_children$gender == "Male" & d_children$parents_age %in% c('80-89', '90+'))] = 0
  
  # d_plot <- d_children
  # d_plot$nb_c[which(d_plot$nb_c == 0)] <- NA
  # p_female <- ggplot(d_plot %>% filter(gender == "Female")) +
  #   geom_tile(aes(parents_age, child_age, fill = nb_c))
  # p_female
  # 
  # p_male <- ggplot(d_plot %>% filter(gender == "Male")) +
  #   geom_tile(aes(parents_age, child_age, fill = nb_c))
  # p_male
  
  d_m1 = left_join(d_merge, d_children, by = c('age'='parents_age', 'gender')) 
  d_m1 = as.data.table(d_m1)
  d_m1[, orphans := round(deaths * nb_c)]
  
  d_m1 = d_m1%>% arrange(age) 
  print(sprintf("Total number of orphans %d:", sum(d_m1$orphans)))
  
  # d_plot <- d_m1
  # d_plot$orphans[which(d_plot$orphans == 0)] <- NA
  # p_female <- ggplot(d_plot %>% filter(gender == "Female")) +
  #   geom_tile(aes(age, child_age, fill = orphans))
  # p_female
  # 
  # p_male <- ggplot(d_plot %>% filter(gender == "Male")) +
  #   geom_tile(aes(age, child_age, fill = orphans))
  # p_male
  
  if (uncertainty == FALSE){
    write_csv(file = paste0('global_age_analysis_2021/data/Argentina/argentina_orphans_age_all.csv'), d_m1)
  }
  
  
  d_summary = d_m1 %>% select(age, gender, deaths, child_age, orphans) %>% 
    group_by(gender, child_age) %>%
    summarise(orphans = sum(orphans))
  
  #p <- ggplot(d_summary) +
  #  geom_line(aes(child_age, orphans, col = gender)) + 
  #  xlab("Age of child") + ylab("Number of orphans") + labs(col = "Gender of parent") + theme_bw()
  #ggsave("global_age_analysis_2021/figures/age_argentina.png", p)
  
  if (uncertainty == FALSE){
    write_csv(path = 'global_age_analysis_2021/data/Argentina/all_data_age.csv', d_summary)
  }
  
  #print(d_summary)
  
  d_summary$category = ifelse(d_summary$child_age %in% c(0, 1, 2, 3, 4), "[0-5)",
                              ifelse(d_summary$child_age %in% c(5, 6, 7, 8, 9), "[5-10)", "[10-18)"))
  d_summary$category <- factor(d_summary$category, levels = c("[0-5)", "[5-10)", "[10-18)"))
  
  d_sum <- d_summary %>%
    group_by(category, gender) %>%
    summarise(orphans = sum(orphans))
  d_sum$orphans_percent <- d_sum$orphans/sum(d_sum$orphans) * 100
  
  # p <- ggplot(d_sum) +
  #   geom_bar(aes(category, orphans, fill = gender), stat = "identity", position=position_dodge()) + 
  #   xlab("Age of child") + ylab("Percent of orphans") + labs(fill = "Gender of parent") + theme_bw()
  # p
  # ggsave("global_age_analysis_2021/figures/age_argentina.png", p)
  
  return(d_sum)
}

# Brazil
process_orphans_age_brazil = function(uncertainty  = FALSE, month = ""){
  if (uncertainty == FALSE){
    d_deaths = read.csv(paste0('global_age_analysis_2021/data/Brazil/covid19_deaths', month, '.csv'), stringsAsFactors = FALSE)
    d_children = read.csv(paste0('global_age_analysis_2021/data/Brazil/child_all_list_both_sex.csv'), stringsAsFactors = FALSE)
  } else {
    d_deaths = read.csv(paste0('global_age_analysis_2021/data/Brazil/covid19_deaths', month, '_un.csv'), stringsAsFactors = FALSE)
    d_children = read.csv(paste0('global_age_analysis_2021/data/Brazil/child_all_list_both_sex_un.csv'), stringsAsFactors = FALSE)
  }
  
  d_merge = copy(d_deaths)
  
  d_children$parents_age = paste0((d_children$parents_age)%/%5 * 5 ,'-',(d_children$parents_age) %/% 5 *5+5)
  d_children$parents_age = ifelse(d_children$parents_age %in% c('100-105'), '100+', d_children$parents_age)
  
  d_children = d_children %>% group_by(parents_age, gender, child_age) %>% mutate(nb_c = mean(prob)) %>%
    select(-prob) %>% ungroup()%>%distinct()
  d_merge$gender = as.character(d_merge$gender)
  d_children$gender = ifelse(d_children$gender == 'female', 'Female', 'Male')
  d_merge$gender = ifelse(d_merge$gender == 'F', 'Female', 'Male')
  #d_children$nb_c[which(d_children$gender == "Male" & d_children$parents_age %in% c('80-89', '90'))] = 0
  
  # d_plot <- d_children
  # d_plot$parents_age[which(d_plot$parents_age == "5-10")] <- "05-10"
  # d_plot$nb_c[which(d_plot$nb_c == 0)] <- NA
  # p_female <- ggplot(d_plot %>% filter(gender == "Female")) +
  #   geom_tile(aes(parents_age, child_age, fill = nb_c))
  # p_female
  # 
  # p_male <- ggplot(d_plot %>% filter(gender == "Male")) +
  #   geom_tile(aes(parents_age, child_age, fill = nb_c))
  # p_male
  # 
  d_m1 = left_join(d_merge, d_children, by = c('age'='parents_age', 'gender')) 
  d_m1 = as.data.table(d_m1)
  d_m1[, orphans := round(COVID19_deaths * nb_c)]
  
  d_m1 = d_m1%>% arrange(age) 
  print(sprintf("Total number of orphans %d:", sum(d_m1$orphans)))
  
  # d_plot <- d_m1
  # d_plot$age[which(d_plot$age == "5-10")] <- "05-10"
  # d_plot$orphans[which(d_plot$orphans == 0)] <- NA
  # p_female <- ggplot(d_plot %>% filter(gender == "Female")) +
  #   geom_tile(aes(age, child_age, fill = orphans))
  # p_female
  # 
  # p_male <- ggplot(d_plot %>% filter(gender == "Male")) +
  #   geom_tile(aes(age, child_age, fill = orphans))
  # p_male
  
  if (uncertainty == FALSE){
    write_csv(file = paste0('global_age_analysis_2021/data/Brazil/brazil_orphans_age_all.csv'), d_m1)
  }
  
  d_summary = d_m1 %>% select(age, gender, COVID19_deaths, child_age, orphans) %>% 
    group_by(gender, child_age) %>%
    summarise(orphans = sum(orphans))
  
  # p <- ggplot(d_summary) +
  #   geom_line(aes(child_age, orphans, col = gender)) + 
  #   xlab("Age of child") + ylab("Number of orphans") + labs(col = "Gender of parent") + theme_bw()
  # p
  # ggsave("global_age_analysis_2021/figures/age_brazil.png", p)
  
  if (uncertainty == FALSE){
    write_csv(path = 'global_age_analysis_2021/data/Brazil/all_data_age.csv', d_summary)
  }
  
  #print(d_summary)
  
  d_summary$category = ifelse(d_summary$child_age %in% c(0, 1, 2, 3, 4), "[0-5)",
                              ifelse(d_summary$child_age %in% c(5, 6, 7, 8, 9), "[5-10)", "[10-18)"))
  d_summary$category <- factor(d_summary$category, levels = c("[0-5)", "[5-10)", "[10-18)"))
  
  d_sum <- d_summary %>%
    group_by(category, gender) %>%
    summarise(orphans = sum(orphans))
  d_sum$orphans_percent <- d_sum$orphans/sum(d_sum$orphans) * 100
  
  # p <- ggplot(d_sum) +
  #   geom_bar(aes(category, orphans, fill = gender), stat = "identity", position=position_dodge()) + 
  #   xlab("Age of child") + ylab("Percent of orphans") + labs(fill = "Gender of parent") + theme_bw()
  # p
  # ggsave("global_age_analysis_2021/figures/age_brazil.png", p)
  
  return(d_sum)
}

# Colombia
process_orphans_age_colombia = function(uncertainty = FALSE, month = ""){
  if (uncertainty == FALSE){
    d_deaths = read.csv(paste0('global_age_analysis_2021/data/Colombia/covid19_deaths_all', month, '.csv'), stringsAsFactors = FALSE)
    d_children = read.csv(paste0('global_age_analysis_2021/data/Colombia/child_all_list_both_sex.csv'), stringsAsFactors = FALSE)
  } else {
    d_deaths = read.csv(paste0('global_age_analysis_2021/data/Colombia/covid19_deaths_all', month, '_un.csv'), stringsAsFactors = FALSE)
    d_children = read.csv(paste0('global_age_analysis_2021/data/Colombia/child_all_list_both_sex_un.csv'), stringsAsFactors = FALSE)
  }
  
  d_merge = copy(d_deaths)
  d_children$parents_age = paste0((d_children$parents_age)%/%5 * 5 ,'-',(d_children$parents_age) %/% 5 *5+4)
  
  if (month %in% c("", "_diff")){
    d_children$parents_age = ifelse(d_children$parents_age %in% c('80-84', '85-89', '90-94', '95-99', '100-105'), 
                                    '80+', d_children$parents_age)
  } else {
    d_children$parents_age = ifelse(d_children$parents_age %in% c('85-89', '90-94', '95-99', '100-105'), 
                                    '85+', d_children$parents_age)
  }
  
  d_children = d_children %>% group_by(parents_age, gender, child_age) %>% mutate(nb_c = mean(prob)) %>%
    select(-prob) %>% ungroup()%>%distinct()
  d_merge$gender = as.character(d_merge$gender)
  d_children$gender = ifelse(d_children$gender == 'female', 'Female', 'Male')
  d_merge$gender = ifelse(d_merge$gender == 'female', 'Female', 'Male')
  #d_children$nb_c[which(d_children$gender == "Male" & d_children$parents_age %in% c('80-89', '90+'))] = 0
  
  # d_plot <- d_children
  # d_plot$nb_c[which(d_plot$nb_c == 0)] <- NA
  # p_female <- ggplot(d_plot %>% filter(gender == "Female")) +
  #   geom_tile(aes(parents_age, child_age, fill = nb_c))
  # p_female
  # 
  # p_male <- ggplot(d_plot %>% filter(gender == "Male")) +
  #   geom_tile(aes(parents_age, child_age, fill = nb_c))
  # p_male
  
  d_m1 = left_join(d_merge, d_children, by = c('age'='parents_age', 'gender')) 
  d_m1 = as.data.table(d_m1)
  d_m1[, orphans := round(deaths * nb_c)]
  
  d_m1 = d_m1%>% arrange(age) 
  print(sprintf("Total number of orphans %d:", sum(d_m1$orphans)))
  
  # d_plot <- d_m1
  # d_plot$age[which(d_plot$age == "5-9")] <- "05-9"
  # d_plot$orphans[which(d_plot$orphans == 0)] <- NA
  # p_female <- ggplot(d_plot %>% filter(gender == "Female")) +
  #   geom_tile(aes(age, child_age, fill = orphans))
  # p_female
  # 
  # p_male <- ggplot(d_plot %>% filter(gender == "Male")) +
  #   geom_tile(aes(age, child_age, fill = orphans))
  # p_male
  
  if (uncertainty == FALSE){
    write_csv(file = paste0('global_age_analysis_2021/data/Colombia/colombia_orphans_age_all.csv'), d_m1)
  }

  d_summary = d_m1 %>% select(age, gender, deaths, child_age, orphans) %>% 
    group_by(gender, child_age) %>%
    summarise(orphans = sum(orphans))
  # 
  # p <- ggplot(d_summary) +
  #   geom_line(aes(child_age, orphans, col = gender)) + 
  #   xlab("Age of child") + ylab("Number of orphans") + labs(col = "Gender of parent") + theme_bw()
  # p
  # ggsave("global_age_analysis_2021/figures/age_colombia.png", p)
  
  if (uncertainty == FALSE){
    write_csv(path = 'global_age_analysis_2021/data/Colombia/all_data_age.csv', d_summary)
  }
  
  #print(d_summary)
  
  d_summary$category = ifelse(d_summary$child_age %in% c(0, 1, 2, 3, 4), "[0-5)",
                              ifelse(d_summary$child_age %in% c(5, 6, 7, 8, 9), "[5-10)", "[10-18)"))
  d_summary$category <- factor(d_summary$category, levels = c("[0-5)", "[5-10)", "[10-18)"))
  
  d_sum <- d_summary %>%
    group_by(category, gender) %>%
    summarise(orphans = sum(orphans))
  d_sum$orphans_percent <- d_sum$orphans/sum(d_sum$orphans) * 100
  
  # p <- ggplot(d_sum) +
  #   geom_bar(aes(category, orphans, fill = gender), stat = "identity", position=position_dodge()) + 
  #   xlab("Age of child") + ylab("Percent of orphans") + labs(fill = "Gender of parent") + theme_bw()
  # p
  # ggsave("global_age_analysis_2021/figures/age_colombia.png", p)
  
  return(d_sum)
}

# England & Wales
process_orphans_age_england_wales = function(uncertainty = FALSE, month = ""){
  if (uncertainty == FALSE){
    d_merge = read.csv(paste0('global_age_analysis_2021/data/UK/deaths_all_england_wales', month, '.csv'), stringsAsFactors = FALSE)
    d_merge <- d_merge[which(d_merge$age != "all-ages"),]
    d_merge$gender = as.character(d_merge$gender)
  
    d_children = read.csv(paste0('global_age_analysis_2021/data/UK/child_all_list_both_sex.csv'), stringsAsFactors = FALSE)
  
  } else {
    d_merge = read.csv(paste0('global_age_analysis_2021/data/UK/deaths_all_england_wales', month, '_un.csv'), stringsAsFactors = FALSE)
    d_merge <- d_merge[which(d_merge$age != "all-ages"),]
    d_merge$gender = as.character(d_merge$gender)
    d_children = read.csv(paste0('global_age_analysis_2021/data/UK/child_all_list_both_sex_un.csv'), stringsAsFactors = FALSE)
  }

  d_children$parents_age = paste0((d_children$parents_age)%/%5 * 5 ,'-',(d_children$parents_age) %/% 5 * 5+4)
  d_children$parents_age[which(d_children$parents_age == "0-4")] <- "00-04"
  d_children$parents_age[which(d_children$parents_age == "5-9")] <- "05-09"
  d_children$parents_age = ifelse(d_children$parents_age %in% c('90-94', '95-99', '100-104'), '90+', d_children$parents_age)
  
  d_children = d_children %>% group_by(parents_age, gender, child_age) %>% mutate(nb_c = mean(prob)) %>%
    select(-prob) %>% ungroup()%>%distinct()
  d_merge$gender = as.character(d_merge$gender)
  d_children$gender = ifelse(d_children$gender == 'female', 'Female', 'Male')
  d_merge$gender = ifelse( d_merge$gender == 'female', 'Female', 'Male')
  
  d_m1 = left_join(d_merge, d_children, by = c('age'='parents_age', 'gender')) 
  d_m1 = as.data.table(d_m1)
  d_m1[, orphans := round(deaths * nb_c)]
  
  d_m1 = d_m1%>% arrange(age) 
  print(sprintf("Total number of orphans %d:", sum(d_m1$orphans)))
  
  if (uncertainty == FALSE){
    write_csv(file = paste0('global_age_analysis_2021/data/UK/england_wales_orphans_age_all.csv'), d_m1)
  }
  
  d_summary = d_m1 %>% select(age, gender, deaths, child_age, orphans) %>% 
    group_by(gender, child_age) %>%
    summarise(orphans = sum(orphans))
  
  if (uncertainty == FALSE){
    write_csv(file = 'global_age_analysis_2021/data/UK/all_data_age.csv', d_summary)
  }
  
  d_summary$category = ifelse(d_summary$child_age %in% c(0, 1, 2, 3, 4), "[0-5)",
                              ifelse(d_summary$child_age %in% c(5, 6, 7, 8, 9), "[5-10)", "[10-18)"))
  d_summary$category <- factor(d_summary$category, levels = c("[0-5)", "[5-10)", "[10-18)"))
  
  d_sum <- d_summary %>%
    group_by(category, gender) %>%
    summarise(orphans = sum(orphans))
  d_sum$orphans_percent <- d_sum$orphans/sum(d_sum$orphans) * 100
  
  return(d_sum)
}

# France
process_orphans_age_france = function(uncertainty = FALSE, month = ""){
  if (uncertainty == FALSE){
    d_deaths = read.csv(paste0('global_age_analysis_2021/data/France/france_all', month, '.csv'), stringsAsFactors = FALSE)
    d_children = read.csv(paste0('global_age_analysis_2021/data/France/child_all_list_both_sex.csv'), stringsAsFactors = FALSE)
  } else {
    d_deaths = read.csv(paste0('global_age_analysis_2021/data/France/france_all', month, '_un.csv'), stringsAsFactors = FALSE)
    d_children = read.csv(paste0('global_age_analysis_2021/data/France/child_all_list_both_sex_un.csv'), stringsAsFactors = FALSE)
  }

  d_merge = copy(d_deaths)
  
  
  d_children$parents_age = paste0((d_children$parents_age)%/%10 * 10 ,'-',(d_children$parents_age) %/% 10 *10+9)
  d_children$parents_age = ifelse(d_children$parents_age %in% c('90-99', '100-109'), '90+', d_children$parents_age)
  
  d_children = d_children %>% group_by(parents_age, gender, child_age) %>% mutate(nb_c = mean(prob)) %>%
    select(-prob) %>% ungroup()%>%distinct()
  d_merge$gender = as.character(d_merge$gender)
  d_children$gender = ifelse(d_children$gender == 'female', 'Female', 'Male')
  #d_children$nb_c[which(d_children$gender == "Male" & d_children$parents_age %in% c('80-89', '90+'))] = 0
  
  # d_plot <- d_children
  # d_plot$nb_c[which(d_plot$nb_c == 0)] <- NA
  # p_female <- ggplot(d_plot %>% filter(gender == "Female")) +
  #   geom_tile(aes(parents_age, child_age, fill = nb_c))
  # p_female
  # 
  # p_male <- ggplot(d_plot %>% filter(gender == "Male")) +
  #   geom_tile(aes(parents_age, child_age, fill = nb_c))
  # p_male
  
  d_m1 = left_join(d_merge, d_children, by = c('age'='parents_age', 'gender')) 
  d_m1 = as.data.table(d_m1)
  d_m1[, orphans := round(deaths * nb_c)]
  
  d_m1 = d_m1%>% arrange(age) 
  print(sprintf("Total number of orphans %d:", sum(d_m1$orphans)))
  
  # d_plot <- d_m1
  # d_plot$orphans[which(d_plot$orphans == 0)] <- NA
  # p_female <- ggplot(d_plot %>% filter(gender == "Female")) +
  #   geom_tile(aes(age, child_age, fill = orphans))
  # p_female
  # 
  # 
  # p_male <- ggplot(d_plot %>% filter(gender == "Male")) +
  #   geom_tile(aes(age, child_age, fill = orphans))
  # p_male
  
  if (uncertainty == FALSE){
    write_csv(file = paste0('global_age_analysis_2021/data/France/france_orphans_age_all.csv'), d_m1)
  }
  
  
  d_summary = d_m1 %>% select(age, gender, deaths, child_age, orphans) %>% 
    group_by(gender, child_age) %>%
    summarise(orphans = sum(orphans))
  
  # p <- ggplot(d_summary) +
  #   geom_line(aes(child_age, orphans, col = gender)) + 
  #   xlab("Age of child") + ylab("Number of orphans") + labs(col = "Gender of parent") + theme_bw()
  # p
  # ggsave("global_age_analysis_2021/figures/age_france.png", p)
  # 
  if (uncertainty == FALSE){
    write_csv(path = 'global_age_analysis_2021/data/France/all_data_age.csv', d_summary)
  }

  #print(d_summary)
  
  d_summary$category = ifelse(d_summary$child_age %in% c(0, 1, 2, 3, 4), "[0-5)",
                              ifelse(d_summary$child_age %in% c(5, 6, 7, 8, 9), "[5-10)", "[10-18)"))
  d_summary$category <- factor(d_summary$category, levels = c("[0-5)", "[5-10)", "[10-18)"))
  
  d_sum <- d_summary %>%
    group_by(category, gender) %>%
    summarise(orphans = sum(orphans))
  d_sum$orphans_percent <- d_sum$orphans/sum(d_sum$orphans) * 100
  
  # if (uncertainty == FALSE){
  #   p <- ggplot(d_sum) +
  #     geom_bar(aes(category, orphans, fill = gender), stat = "identity", position=position_dodge()) + 
  #     xlab("Age of child") + ylab("Percent of orphans") + labs(fill = "Gender of parent") + theme_bw()
  #   p
  #   ggsave("global_age_analysis_2021/figures/age_france.png", p)
  #   
  # }
 
  return(d_sum)
}

# Germany
process_orphans_age_germany = function(uncertainty = FALSE, month = ""){
  if (uncertainty == FALSE){
    d_deaths = read.csv(paste0('global_age_analysis_2021/data/Germany/covid19_deaths', month, '.csv'), stringsAsFactors = FALSE)
  } else {
    d_deaths = read.csv(paste0('global_age_analysis_2021/data/Germany/covid19_deaths', month, '_un.csv'), stringsAsFactors = FALSE)
  }

  d_merge = copy(d_deaths)
  
  if (uncertainty == FALSE){
    d_children = read.csv(paste0('global_age_analysis_2021/data/Germany/child_all_list_both_sex.csv'), stringsAsFactors = FALSE)
  } else {
    d_children = read.csv(paste0('global_age_analysis_2021/data/Germany/child_all_list_both_sex_un.csv'), stringsAsFactors = FALSE)
  }
  
  d_children$parents_age = paste0((d_children$parents_age)%/%10 * 10 ,'-',(d_children$parents_age) %/% 10 *10+9)
  d_children$parents_age = ifelse(d_children$parents_age %in% c('90-99', '100-109'), '90+', d_children$parents_age)
  
  d_children = d_children %>% group_by(parents_age, gender, child_age) %>% mutate(nb_c = mean(prob)) %>%
    select(-prob) %>% ungroup()%>%distinct()
  d_merge$gender = as.character(d_merge$gender)
  d_children$gender = ifelse(d_children$gender == 'female', 'Female', 'Male')
  #d_children$nb_c[which(d_children$gender == "Male" & d_children$parents_age %in% c('80-89', '90+'))] = 0
  
  # d_plot <- d_children
  # d_plot$nb_c[which(d_plot$nb_c == 0)] <- NA
  # p_female <- ggplot(d_plot %>% filter(gender == "Female")) +
  #   geom_tile(aes(parents_age, child_age, fill = nb_c))
  # p_female
  # 
  # p_male <- ggplot(d_plot %>% filter(gender == "Male")) +
  #   geom_tile(aes(parents_age, child_age, fill = nb_c))
  # p_male
  
  d_m1 = left_join(d_merge, d_children, by = c('age'='parents_age', 'gender')) 
  d_m1 = as.data.table(d_m1)
  d_m1[, orphans := round(deaths * nb_c)]
  
  d_m1 = d_m1%>% arrange(age) 
  print(sprintf("Total number of orphans %d:", sum(d_m1$orphans)))
  
  # d_plot <- d_m1
  # d_plot$orphans[which(d_plot$orphans == 0)] <- NA
  # p_female <- ggplot(d_plot %>% filter(gender == "Female")) +
  #   geom_tile(aes(age, child_age, fill = orphans))
  # p_female
  # 
  # p_male <- ggplot(d_plot %>% filter(gender == "Male")) +
  #   geom_tile(aes(age, child_age, fill = orphans))
  # p_male
  # 
  
  if (uncertainty == FALSE){
    write_csv(file = paste0('global_age_analysis_2021/data/Germany/germany_orphans_age_all.csv'), d_m1)
  }

  
  d_summary = d_m1 %>% select(age, gender, deaths, child_age, orphans) %>% 
    group_by(gender, child_age) %>%
    summarise(orphans = sum(orphans))
  
  # p <- ggplot(d_summary) +
  #   geom_line(aes(child_age, orphans, col = gender)) + 
  #   xlab("Age of child") + ylab("Number of orphans") + labs(col = "Gender of parent") + theme_bw()
  # p
  # ggsave("global_age_analysis_2021/figures/age_germany.png", p)
  # 
  if (uncertainty == FALSE){
    write_csv(path = 'global_age_analysis_2021/data/Germany/all_data_age.csv', d_summary)
  }
  #print(d_summary)
  
  d_summary$category = ifelse(d_summary$child_age %in% c(0, 1, 2, 3, 4), "[0-5)",
                              ifelse(d_summary$child_age %in% c(5, 6, 7, 8, 9), "[5-10)", "[10-18)"))
  d_summary$category <- factor(d_summary$category, levels = c("[0-5)", "[5-10)", "[10-18)"))
  
  d_sum <- d_summary %>%
    group_by(category, gender) %>%
    summarise(orphans = sum(orphans))
  d_sum$orphans_percent <- d_sum$orphans/sum(d_sum$orphans) * 100
  
  # p <- ggplot(d_sum) +
  #   geom_bar(aes(category, orphans, fill = gender), stat = "identity", position=position_dodge()) + 
  #   xlab("Age of child") + ylab("Percent of orphans") + labs(fill = "Gender of parent") + theme_bw()
  # p
  # ggsave("global_age_analysis_2021/figures/age_germany.png", p)
  
  return(d_sum)
}

# India
process_orphans_age_india = function(uncertainty = FALSE, month = ""){
  if (uncertainty == FALSE){
    d_deaths = read.csv(paste0('global_age_analysis_2021/data/India/all_covid_deaths', month, '.csv'), stringsAsFactors = FALSE)
    d_children = read.csv(paste0('global_age_analysis_2021/data/India/child_all_list_both_sex.csv'), stringsAsFactors = FALSE)
  } else {
    d_deaths = read.csv(paste0('global_age_analysis_2021/data/India/all_covid_deaths', month, '_un.csv'), stringsAsFactors = FALSE)
    d_children = read.csv(paste0('global_age_analysis_2021/data/India/child_all_list_both_sex_un.csv'), stringsAsFactors = FALSE)
  }
  
  d_merge = copy(d_deaths)
  d_merge$age[which(d_merge$age == "5-19")] <- "05-19"
  
  d_children$parents_age = paste0((d_children$parents_age)%/%5 * 5 ,'-',(d_children$parents_age) %/% 5 *5+4)
  d_children$parents_age = ifelse(d_children$parents_age %in% c('5-9', '10-14', '15-19'), "05-19", 
                                  ifelse(d_children$parents_age %in% c('20-24', '25-29'), '20-29',
                                         ifelse(d_children$parents_age %in% c('30-34', '35-39'), '30-39',
                                                ifelse(d_children$parents_age %in% c('40-44', '45-49'), '40-49',
                                                       ifelse(d_children$parents_age %in% c('50-54', '55-59'), '50-59',
                                                              ifelse(d_children$parents_age %in% c('60-64', '65-69'), '60-69',
                                                                     ifelse(d_children$parents_age %in% c('70-74', '75-79'), '70-79',
                                                                            ifelse(d_children$parents_age %in% c('80-84', '85-89', '90-94', 
                                                                                                                 '95-99', "100-104"), '80+', '0-4'))))))))
  
  
  d_children = d_children %>% group_by(parents_age, gender, child_age) %>% mutate(nb_c = mean(prob)) %>%
    select(-prob) %>% ungroup()%>%distinct()
  names(d_merge)[which(names(d_merge) == "sex")] <- "gender"
  #names(d_merge) <- c("X", "gender", "age", "deaths")
  d_merge$gender = as.character(d_merge$gender)
  d_children$gender = ifelse(d_children$gender == 'female', 'Female', 'Male')
  #d_children$nb_c[which(d_children$gender == "Male" & d_children$parents_age %in% c('80+'))] = 0
  
  # d_plot <- d_children
  # d_plot$nb_c[which(d_plot$nb_c == 0)] <- NA
  # p_female <- ggplot(d_plot %>% filter(gender == "Female")) +
  #   geom_tile(aes(parents_age, child_age, fill = nb_c))
  # p_female
  # 
  # p_male <- ggplot(d_plot %>% filter(gender == "Male")) +
  #   geom_tile(aes(parents_age, child_age, fill = nb_c))
  # p_male
  
  d_m1 = left_join(d_merge, d_children, by = c('age'='parents_age', 'gender')) 
  d_m1 = as.data.table(d_m1)
  d_m1[, orphans := round(deaths * nb_c)]
  
  d_m1 = d_m1%>% arrange(age) 
  print(sprintf("Total number of orphans %d:", sum(d_m1$orphans)))
  
  # d_plot <- d_m1
  # d_plot$orphans[which(d_plot$orphans == 0)] <- NA
  # p_female <- ggplot(d_plot %>% filter(gender == "Female")) +
  #   geom_tile(aes(age, child_age, fill = orphans))
  # p_female
  # 
  # 
  # p_male <- ggplot(d_plot %>% filter(gender == "Male")) +
  #   geom_tile(aes(age, child_age, fill = orphans))
  # p_male
  # 
  if (uncertainty == FALSE){
    write_csv(file = paste0('global_age_analysis_2021/data/India/india_orphans_age_all.csv'), d_m1)
  }
  
  
  d_summary = d_m1 %>% select(age, gender, deaths, child_age, orphans) %>% 
    group_by(gender, child_age) %>%
    summarise(orphans = sum(orphans))
  
  # p <- ggplot(d_summary) +
  #   geom_line(aes(child_age, orphans, col = gender)) + 
  #   xlab("Age of child") + ylab("Number of orphans") + labs(col = "Gender of parent") + theme_bw()
  # p
  # ggsave("global_age_analysis_2021/figures/age_india.png", p)
  
  if (uncertainty == FALSE){
    write_csv(path = 'global_age_analysis_2021/data/India/all_data_age.csv', d_summary)
  }
  
  #print(d_summary)
  
  d_summary$category = ifelse(d_summary$child_age %in% c(0, 1, 2, 3, 4), "[0-5)",
                              ifelse(d_summary$child_age %in% c(5, 6, 7, 8, 9), "[5-10)", "[10-18)"))
  d_summary$category <- factor(d_summary$category, levels = c("[0-5)", "[5-10)", "[10-18)"))
  
  d_sum <- d_summary %>%
    group_by(category, gender) %>%
    summarise(orphans = sum(orphans))
  d_sum$orphans_percent <- d_sum$orphans/sum(d_sum$orphans) * 100
  
  # p <- ggplot(d_sum) +
  #   geom_bar(aes(category, orphans, fill = gender), stat = "identity", position=position_dodge()) + 
  #   xlab("Age of child") + ylab("Percent of orphans") + labs(fill = "Gender of parent") + theme_bw()
  # p
  # ggsave("global_age_analysis_2021/figures/age_india.png", p)
  
  return(d_sum)
}

# Iran
process_orphans_age_iran = function(uncertainty = FALSE, month = ""){
  if (uncertainty == FALSE){
    d_deaths = read.csv(paste0('global_age_analysis_2021/data/Iran/iran_all', month, '.csv'), stringsAsFactors = FALSE)
    d_children = read.csv(paste0('global_age_analysis_2021/data/Iran/child_all_list_both_sex.csv'), stringsAsFactors = FALSE)
  } else {
    d_deaths = read.csv(paste0('global_age_analysis_2021/data/Iran/iran_all', month, '_un.csv'), stringsAsFactors = FALSE)
    d_children = read.csv(paste0('global_age_analysis_2021/data/Iran/child_all_list_both_sex_un.csv'), stringsAsFactors = FALSE)
  }

  d_merge = copy(d_deaths)

  d_children$parents_age = paste0((d_children$parents_age)%/%10 * 10 ,'-',(d_children$parents_age) %/% 10 *10+9)
  d_children$parents_age = ifelse(d_children$parents_age %in% c('80-89', '90-99', '100-109'), '80+', d_children$parents_age)
  
  d_children = d_children %>% group_by(parents_age, gender, child_age) %>% mutate(nb_c = mean(prob)) %>%
    select(-prob) %>% ungroup()%>%distinct()
  d_merge$gender = as.character(d_merge$gender)
  d_children$gender = ifelse(d_children$gender == 'female', 'Female', 'Male')
  #d_children$nb_c[which(d_children$gender == "Male" & d_children$parents_age %in% c('80-89', '90+'))] = 0
  
  # d_plot <- d_children
  # d_plot$nb_c[which(d_plot$nb_c == 0)] <- NA
  # p_female <- ggplot(d_plot %>% filter(gender == "Female")) +
  #   geom_tile(aes(parents_age, child_age, fill = nb_c))
  # p_female
  # 
  # p_male <- ggplot(d_plot %>% filter(gender == "Male")) +
  #   geom_tile(aes(parents_age, child_age, fill = nb_c))
  # p_male
  # 
  d_m1 = left_join(d_merge, d_children, by = c('age'='parents_age', 'gender')) 
  d_m1 = as.data.table(d_m1)
  d_m1[, orphans := round(deaths * nb_c)]
  
  d_m1 = d_m1%>% arrange(age) 
  print(sprintf("Total number of orphans %d:", sum(d_m1$orphans)))
  
  # d_plot <- d_m1
  # d_plot$orphans[which(d_plot$orphans == 0)] <- NA
  # p_female <- ggplot(d_plot %>% filter(gender == "Female")) +
  #   geom_tile(aes(age, child_age, fill = orphans))
  # p_female
  # 
  # 
  # p_male <- ggplot(d_plot %>% filter(gender == "Male")) +
  #   geom_tile(aes(age, child_age, fill = orphans))
  # p_male
  # 
  if (uncertainty == FALSE){
    write_csv(file = paste0('global_age_analysis_2021/data/Iran/iran_orphans_age_all.csv'), d_m1)
  }

  d_summary = d_m1 %>% select(age, gender, deaths, child_age, orphans) %>% 
    group_by(gender, child_age) %>%
    summarise(orphans = sum(orphans))
  
  # p <- ggplot(d_summary) +
  #   geom_line(aes(child_age, orphans, col = gender)) + 
  #   xlab("Age of child") + ylab("Number of orphans") + labs(col = "Gender of parent") + theme_bw()
  # p
  # ggsave("global_age_analysis_2021/figures/age_iran.png", p)
  # 
  if (uncertainty == FALSE){
    write_csv(path = 'global_age_analysis_2021/data/Iran/all_data_age.csv', d_summary)
  }

  # print(d_summary)
  
  d_summary$category = ifelse(d_summary$child_age %in% c(0, 1, 2, 3, 4), "[0-5)",
                              ifelse(d_summary$child_age %in% c(5, 6, 7, 8, 9), "[5-10)", "[10-18)"))
  d_summary$category <- factor(d_summary$category, levels = c("[0-5)", "[5-10)", "[10-18)"))
  
  d_sum <- d_summary %>%
    group_by(category, gender) %>%
    summarise(orphans = sum(orphans))
  d_sum$orphans_percent <- d_sum$orphans/sum(d_sum$orphans) * 100
  
  # p <- ggplot(d_sum) +
  #   geom_bar(aes(category, orphans, fill = gender), stat = "identity", position=position_dodge()) + 
  #   xlab("Age of child") + ylab("Percent of orphans") + labs(fill = "Gender of parent") + theme_bw()
  # p
  # ggsave("global_age_analysis_2021/figures/age_iran.png", p)
  
  return(d_sum)
}

# Italy
process_orphans_age_italy = function(uncertainty = FALSE, month = ""){
  if (uncertainty == FALSE){
    d_deaths = read.csv(paste0('global_age_analysis_2021/data/Italy/italy_all', month, '.csv'), stringsAsFactors = FALSE)
    d_children = read.csv(paste0('global_age_analysis_2021/data/Italy/child_all_list_both_sex.csv'), stringsAsFactors = FALSE)
  } else {
    d_deaths = read.csv(paste0('global_age_analysis_2021/data/Italy/italy_all', month, '_un.csv'), stringsAsFactors = FALSE)
    d_children = read.csv(paste0('global_age_analysis_2021/data/Italy/child_all_list_both_sex_un.csv'), stringsAsFactors = FALSE)
  }
  
  d_merge = copy(d_deaths)
  
  d_children$parents_age = paste0((d_children$parents_age)%/%10 * 10 ,'-',(d_children$parents_age) %/% 10 *10+9)
  d_children$parents_age = ifelse(d_children$parents_age %in% c('90-99','100-109'), '90+', d_children$parents_age)
  
  d_children = d_children %>% group_by(parents_age, gender, child_age) %>% mutate(nb_c = mean(prob)) %>%
    select(-prob) %>% ungroup()%>%distinct()
  d_merge$gender = as.character(d_merge$gender)
  d_children$gender = ifelse(d_children$gender == 'female', 'Female', 'Male')
  #d_children$nb_c[which(d_children$gender == "Male" & d_children$parents_age %in% c('80-89', '90+'))] = 0
  
  # d_plot <- d_children
  # d_plot$nb_c[which(d_plot$nb_c == 0)] <- NA
  # p_female <- ggplot(d_plot %>% filter(gender == "Female")) +
  #   geom_tile(aes(parents_age, child_age, fill = nb_c))
  # p_female
  # 
  # p_male <- ggplot(d_plot %>% filter(gender == "Male")) +
  #   geom_tile(aes(parents_age, child_age, fill = nb_c))
  # p_male
  
  d_m1 = left_join(d_merge, d_children, by = c('age'='parents_age', 'gender')) 
  d_m1 = as.data.table(d_m1)
  d_m1[, orphans := round(deaths * nb_c)]
  
  d_m1 = d_m1%>% arrange(age) 
  print(sprintf("Total number of orphans %d:", sum(d_m1$orphans)))
  
  # d_plot <- d_m1
  # d_plot$orphans[which(d_plot$orphans == 0)] <- NA
  # p_female <- ggplot(d_plot %>% filter(gender == "Female")) +
  #   geom_tile(aes(age, child_age, fill = orphans))
  # p_female
  # 
  # 
  # p_male <- ggplot(d_plot %>% filter(gender == "Male")) +
  #   geom_tile(aes(age, child_age, fill = orphans))
  # p_male
  
  if (uncertainty == FALSE){
    write_csv(file = paste0('global_age_analysis_2021/data/Italy/italy_orphans_age_all.csv'), d_m1)
  }
  
  d_summary = d_m1 %>% select(age, gender, deaths, child_age, orphans) %>% 
    group_by(gender, child_age) %>%
    summarise(orphans = sum(orphans))
  
  # p <- ggplot(d_summary) +
  #   geom_line(aes(child_age, orphans, col = gender)) + 
  #   xlab("Age of child") + ylab("Number of orphans") + labs(col = "Gender of parent") + theme_bw()
  # p
  # ggsave("global_age_analysis_2021/figures/age_italy.png", p)
  
  if (uncertainty == FALSE){
    write_csv(path = 'global_age_analysis_2021/data/Italy/all_data_age.csv', d_summary)
  }
  
  #print(d_summary)
  
  d_summary$category = ifelse(d_summary$child_age %in% c(0, 1, 2, 3, 4), "[0-5)",
                              ifelse(d_summary$child_age %in% c(5, 6, 7, 8, 9), "[5-10)", "[10-18)"))
  d_summary$category <- factor(d_summary$category, levels = c("[0-5)", "[5-10)", "[10-18)"))
  
  d_sum <- d_summary %>%
    group_by(category, gender) %>%
    summarise(orphans = sum(orphans))
  d_sum$orphans_percent <- d_sum$orphans/sum(d_sum$orphans) * 100
  
  # p <- ggplot(d_sum) +
  #   geom_bar(aes(category, orphans, fill = gender), stat = "identity", position=position_dodge()) + 
  #   xlab("Age of child") + ylab("Percent of orphans") + labs(fill = "Gender of parent") + theme_bw()
  # p
  # ggsave("global_age_analysis_2021/figures/age_italy.png", p)
  
  return(d_sum)
}

# Kenya
process_orphans_age_kenya = function(uncertainty = FALSE, month = ""){
  if (uncertainty == FALSE){
    d_merge = read.csv(paste0('global_age_analysis_2021/data/Kenya/covid19_deaths', month, '.csv'), stringsAsFactors = FALSE)
    d_children = read.csv(paste0('global_age_analysis_2021/data/Kenya/child_all_list_both_sex.csv'), stringsAsFactors = FALSE)
  } else {
    d_merge = read.csv(paste0('global_age_analysis_2021/data/Kenya/covid19_deaths', month, '_un.csv'), stringsAsFactors = FALSE)
    d_children = read.csv(paste0('global_age_analysis_2021/data/Kenya/child_all_list_both_sex_un.csv'), stringsAsFactors = FALSE)
  }
  
  d_children$parents_age = paste0((d_children$parents_age)%/%10 * 10 ,'-',(d_children$parents_age) %/% 10 *10+9)
  d_children$parents_age = ifelse(d_children$parents_age %in% c('60-69', '70-79', '80-89', '90-99','100-109'), '60+', d_children$parents_age)
  
  d_children = d_children %>% group_by(parents_age, gender, child_age) %>% mutate(nb_c = mean(prob)) %>%
    select(-prob) %>% ungroup()%>%distinct()
  d_merge$gender = as.character(d_merge$gender)
  d_children$gender = ifelse(d_children$gender == 'female', 'Female', 'Male')
  #d_children$nb_c[which(d_children$gender == "Male" & d_children$parents_age %in% c('80-89', '90+'))] = 0
  
  # d_plot <- d_children
  # d_plot$nb_c[which(d_plot$nb_c == 0)] <- NA
  # p_female <- ggplot(d_plot %>% filter(gender == "Female")) +
  #   geom_tile(aes(parents_age, child_age, fill = nb_c))
  # p_female
  # 
  # p_male <- ggplot(d_plot %>% filter(gender == "Male")) +
  #   geom_tile(aes(parents_age, child_age, fill = nb_c))
  # p_male
  
  d_m1 = left_join(d_merge, d_children, by = c('age'='parents_age', 'gender')) 
  d_m1 = as.data.table(d_m1)
  d_m1[, orphans := round(deaths * nb_c)]
  
  d_m1 = d_m1%>% arrange(age) 
  print(sprintf("Total number of orphans %d:", sum(d_m1$orphans)))
  
  # d_plot <- d_m1
  # d_plot$orphans[which(d_plot$orphans == 0)] <- NA
  # p_female <- ggplot(d_plot %>% filter(gender == "Female")) +
  #   geom_tile(aes(age, child_age, fill = orphans))
  # p_female
  # 
  # p_male <- ggplot(d_plot %>% filter(gender == "Male")) +
  #   geom_tile(aes(age, child_age, fill = orphans))
  # p_male
  # 
  
  if (uncertainty == FALSE){
    write_csv(file = paste0('global_age_analysis_2021/data/Kenya/kenya_orphans_age_all.csv'), d_m1)
  }
  
  d_summary = d_m1 %>% select(age, gender, deaths, child_age, orphans) %>% 
    group_by(gender, child_age) %>%
    summarise(orphans = sum(orphans))
  
  # p <- ggplot(d_summary) +
  #   geom_line(aes(child_age, orphans, col = gender)) + 
  #   xlab("Age of child") + ylab("Number of orphans") + labs(col = "Gender of parent") + theme_bw()
  # p
  # ggsave("global_age_analysis_2021/figures/age_kenya.png", p)
  
  if (uncertainty == FALSE){
    write_csv(path = 'global_age_analysis_2021/data/Kenya/all_data_age.csv', d_summary)
  }
  
  #print(d_summary)
  
  d_summary$category = ifelse(d_summary$child_age %in% c(0, 1, 2, 3, 4), "[0-5)",
                              ifelse(d_summary$child_age %in% c(5, 6, 7, 8, 9), "[5-10)", "[10-18)"))
  d_summary$category <- factor(d_summary$category, levels = c("[0-5)", "[5-10)", "[10-18)"))
  
  d_sum <- d_summary %>%
    group_by(category, gender) %>%
    summarise(orphans = sum(orphans))
  d_sum$orphans_percent <- d_sum$orphans/sum(d_sum$orphans) * 100
  
  # p <- ggplot(d_sum) +
  #   geom_bar(aes(category, orphans, fill = gender), stat = "identity", position=position_dodge()) + 
  #   xlab("Age of child") + ylab("Percent of orphans") + labs(fill = "Gender of parent") + theme_bw()
  # p
  # ggsave("global_age_analysis_2021/figures/age_kenya.png", p)
  return(d_sum)
}

# Malawi
process_orphans_age_malawi = function(uncertainty = FALSE, month = ""){
  if (uncertainty == FALSE){
    d_merge = read.csv(paste0('global_age_analysis_2021/data/Malawi/covid19_deaths', month, '.csv'), stringsAsFactors = FALSE)
    d_children = read.csv(paste0('global_age_analysis_2021/data/Malawi/child_all_list_both_sex.csv'), stringsAsFactors = FALSE)
  } else {
    d_merge = read.csv(paste0('global_age_analysis_2021/data/Malawi/covid19_deaths', month, '_un.csv'), stringsAsFactors = FALSE)
    d_children = read.csv(paste0('global_age_analysis_2021/data/Malawi/child_all_list_both_sex_un.csv'), stringsAsFactors = FALSE)
  }
  
  d_children$parents_age = paste0((d_children$parents_age)%/%10 * 10 ,'-',(d_children$parents_age) %/% 10 *10+9)
  d_children$parents_age = ifelse(d_children$parents_age %in% c('70-79', '80-89', '90-99','100-109'), '70+', d_children$parents_age)
  
  d_children = d_children %>% group_by(parents_age, gender, child_age) %>% mutate(nb_c = mean(prob)) %>%
    select(-prob) %>% ungroup()%>%distinct()
  d_merge$gender = as.character(d_merge$gender)
  d_children$gender = ifelse(d_children$gender == 'female', 'Female', 'Male')
  #d_children$nb_c[which(d_children$gender == "Male" & d_children$parents_age %in% c('80-89', '90+'))] = 0
  
  # d_plot <- d_children
  # d_plot$nb_c[which(d_plot$nb_c == 0)] <- NA
  # p_female <- ggplot(d_plot %>% filter(gender == "Female")) +
  #   geom_tile(aes(parents_age, child_age, fill = nb_c))
  # p_female
  # 
  # p_male <- ggplot(d_plot %>% filter(gender == "Male")) +
  #   geom_tile(aes(parents_age, child_age, fill = nb_c))
  # p_male
  
  d_m1 = left_join(d_merge, d_children, by = c('age'='parents_age', 'gender')) 
  d_m1 = as.data.table(d_m1)
  d_m1[, orphans := round(deaths * nb_c)]
  
  d_m1 = d_m1%>% arrange(age) 
  print(sprintf("Total number of orphans %d:", sum(d_m1$orphans)))
  
  # d_plot <- d_m1
  # d_plot$orphans[which(d_plot$orphans == 0)] <- NA
  # p_female <- ggplot(d_plot %>% filter(gender == "Female")) +
  #   geom_tile(aes(age, child_age, fill = orphans))
  # p_female
  # 
  # p_male <- ggplot(d_plot %>% filter(gender == "Male")) +
  #   geom_tile(aes(age, child_age, fill = orphans))
  # p_male
  
  if (uncertainty == FALSE){
    write_csv(file = paste0('global_age_analysis_2021/data/Malawi/malawi_orphans_age_all.csv'), d_m1)
  }
  
  
  d_summary = d_m1 %>% select(age, gender, deaths, child_age, orphans) %>% 
    group_by(gender, child_age) %>%
    summarise(orphans = sum(orphans))
  
  # p <- ggplot(d_summary) +
  #   geom_line(aes(child_age, orphans, col = gender)) + 
  #   xlab("Age of child") + ylab("Number of orphans") + labs(col = "Gender of parent") + theme_bw()
  # p
  # ggsave("global_age_analysis_2021/figures/age_malawi.png", p)
  # 
  if (uncertainty == FALSE){
    write_csv(path = 'global_age_analysis_2021/data/Malawi/all_data_age.csv', d_summary)
  }
  
  #print(d_summary)
  
  d_summary$category = ifelse(d_summary$child_age %in% c(0, 1, 2, 3, 4), "[0-5)",
                              ifelse(d_summary$child_age %in% c(5, 6, 7, 8, 9), "[5-10)", "[10-18)"))
  d_summary$category <- factor(d_summary$category, levels = c("[0-5)", "[5-10)", "[10-18)"))
  
  d_sum <- d_summary %>%
    group_by(category, gender) %>%
    summarise(orphans = sum(orphans))
  d_sum$orphans_percent <- d_sum$orphans/sum(d_sum$orphans) * 100
  
  # p <- ggplot(d_sum) +
  #   geom_bar(aes(category, orphans, fill = gender), stat = "identity", position=position_dodge()) + 
  #   xlab("Age of child") + ylab("Percent of orphans") + labs(fill = "Gender of parent") + theme_bw()
  # p
  # ggsave("global_age_analysis_2021/figures/age_malawi.png", p)
  return(d_sum)
}

# Mexico
process_orphans_age_mexico = function(uncertainty = FALSE, month = ""){
  if (uncertainty == FALSE){
    d_deaths = read.csv(paste0('global_age_analysis_2021/data/Mexico/covid19_deaths', month, '.csv'), stringsAsFactors = FALSE)
    d_children = read.csv(paste0('global_age_analysis_2021/data/Mexico/child_all_list_both_sex.csv'), stringsAsFactors = FALSE)
  } else {
    d_deaths = read.csv(paste0('global_age_analysis_2021/data/Mexico/covid19_deaths', month, '_un.csv'), stringsAsFactors = FALSE)
    d_children = read.csv(paste0('global_age_analysis_2021/data/Mexico/child_all_list_both_sex_un.csv'), stringsAsFactors = FALSE)
  }
  
  d_merge = copy(d_deaths)
  
  d_children$parents_age = paste0((d_children$parents_age)%/%5 * 5 ,'-',(d_children$parents_age) %/% 5 *5+4)
  d_children$parents_age = ifelse(d_children$parents_age %in% c('80-84', '85-89', '90-94', '95-99', '100-104'), '80+', d_children$parents_age)
  
  d_children = d_children %>% group_by(parents_age, gender, child_age) %>% mutate(nb_c = mean(prob)) %>%
    select(-prob) %>% ungroup()%>%distinct()
  d_merge$gender = as.character(d_merge$gender)
  d_children$gender = ifelse(d_children$gender == 'female', 'Female', 'Male')
  #d_children$nb_c[which(d_children$gender == "Male" & d_children$parents_age %in% c('80-89', '90+'))] = 0
  
  d_plot <- d_children
  
  # d_plot$nb_c[which(d_plot$nb_c == 0)] <- NA
  # d_plot$parents_age[which(d_plot$parents_age == "5-9")] <- "05-9"
  # p_female <- ggplot(d_plot %>% filter(gender == "Female")) +
  #   geom_tile(aes(parents_age, child_age, fill = nb_c))
  # p_female
  # 
  # p_male <- ggplot(d_plot %>% filter(gender == "Male")) +
  #   geom_tile(aes(parents_age, child_age, fill = nb_c))
  # p_male
  
  d_m1 = left_join(d_merge, d_children, by = c('age'='parents_age', 'gender')) 
  d_m1 = as.data.table(d_m1)
  d_m1[, orphans := round(deaths * nb_c)]
  
  d_m1 = d_m1%>% arrange(age) 
  print(sprintf("Total number of orphans %d:", sum(d_m1$orphans)))
  
  # d_plot <- d_m1
  # d_plot$age[which(d_plot$age == "5-9")] <- "05-9"
  # d_plot$orphans[which(d_plot$orphans == 0)] <- NA
  # p_female <- ggplot(d_plot %>% filter(gender == "Female")) +
  #   geom_tile(aes(age, child_age, fill = orphans))
  # p_female
  # 
  # 
  # p_male <- ggplot(d_plot %>% filter(gender == "Male")) +
  #   geom_tile(aes(age, child_age, fill = orphans))
  # p_male
  # 
  if (uncertainty == FALSE){
    write_csv(file = paste0('global_age_analysis_2021/data/Mexico/mexico_orphans_age_all.csv'), d_m1)
  }
  
  
  d_summary = d_m1 %>% select(age, gender, deaths, child_age, orphans) %>% 
    group_by(gender, child_age) %>%
    summarise(orphans = sum(orphans))
  
  # p <- ggplot(d_summary) +
  #   geom_line(aes(child_age, orphans, col = gender)) + 
  #   xlab("Age of child") + ylab("Number of orphans") + labs(col = "Gender of parent") + theme_bw()
  # p
  # ggsave("global_age_analysis_2021/figures/age_mexico.png", p)
  
  if (uncertainty == FALSE){
    write_csv(path = 'global_age_analysis_2021/data/Mexico/all_data_age.csv', d_summary)
  }
  
  #print(d_summary)
  
  d_summary$category = ifelse(d_summary$child_age %in% c(0, 1, 2, 3, 4), "[0-5)",
                              ifelse(d_summary$child_age %in% c(5, 6, 7, 8, 9), "[5-10)", "[10-18)"))
  d_summary$category <- factor(d_summary$category, levels = c("[0-5)", "[5-10)", "[10-18)"))
  
  d_sum <- d_summary %>%
    group_by(category, gender) %>%
    summarise(orphans = sum(orphans))
  d_sum$orphans_percent <- d_sum$orphans/sum(d_sum$orphans) * 100
  
  # p <- ggplot(d_sum) +
  #   geom_bar(aes(category, orphans, fill = gender), stat = "identity", position=position_dodge()) + 
  #   xlab("Age of child") + ylab("Percent of orphans") + labs(fill = "Gender of parent") + theme_bw()
  # p
  # ggsave("global_age_analysis_2021/figures/age_mexico.png", p)
  
  return(d_sum)
}

# Nigeria
process_orphans_age_nigeria = function(uncertainty = FALSE, month = ""){
  if (uncertainty == FALSE){
    d_merge = read.csv(paste0('global_age_analysis_2021/data/Nigeria/covid19_deaths', month, '.csv'), stringsAsFactors = FALSE)
    d_children = read.csv(paste0('global_age_analysis_2021/data/Nigeria/child_all_list_both_sex.csv'), stringsAsFactors = FALSE)
  } else {
    d_merge = read.csv(paste0('global_age_analysis_2021/data/Nigeria/covid19_deaths', month, '_un.csv'), stringsAsFactors = FALSE)
    d_children = read.csv(paste0('global_age_analysis_2021/data/Nigeria/child_all_list_both_sex_un.csv'), stringsAsFactors = FALSE)
  }
  
  d_merge$age[which(d_merge$age == "05-Sep")] = '5-9'
  d_merge$age[which(d_merge$age == "Oct-14")] = '10-14'
  
  
  d_children$parents_age = paste0((d_children$parents_age)%/%5 * 5 ,'-',(d_children$parents_age) %/% 5 *5+4)
  d_children$parents_age = ifelse(d_children$parents_age %in% c('85-89', '90-94', '95-99', '100-104'), '85+', d_children$parents_age)
  
  d_children = d_children %>% group_by(parents_age, gender, child_age) %>% mutate(nb_c = mean(prob)) %>%
    select(-prob) %>% ungroup()%>%distinct()
  d_merge$gender = as.character(d_merge$gender)
  d_children$gender = ifelse(d_children$gender == 'female', 'Female', 'Male')
  #d_children$nb_c[which(d_children$gender == "Male" & d_children$parents_age %in% c('80-89', '90+'))] = 0
  
  # d_plot <- d_children
  # d_plot$nb_c[which(d_plot$nb_c == 0)] <- NA
  # d_plot$parents_age[which(d_plot$parents_age == "5-9")] <- "05-9"
  # p_female <- ggplot(d_plot %>% filter(gender == "Female")) +
  #   geom_tile(aes(parents_age, child_age, fill = nb_c))
  # p_female
  # 
  # p_male <- ggplot(d_plot %>% filter(gender == "Male")) +
  #   geom_tile(aes(parents_age, child_age, fill = nb_c))
  # p_male
  
  d_m1 = left_join(d_merge, d_children, by = c('age'='parents_age', 'gender')) 
  d_m1 = as.data.table(d_m1)
  d_m1[, orphans := round(deaths * nb_c)]
  
  d_m1 = d_m1%>% arrange(age) 
  print(sprintf("Total number of orphans %d:", sum(d_m1$orphans)))
  
  # d_plot <- d_m1
  # d_plot$age[which(d_plot$age == "5-9")] <- "05-9"
  # d_plot$orphans[which(d_plot$orphans == 0)] <- NA
  # p_female <- ggplot(d_plot %>% filter(gender == "Female")) +
  #   geom_tile(aes(age, child_age, fill = orphans))
  # p_female
  # 
  # 
  # p_male <- ggplot(d_plot %>% filter(gender == "Male")) +
  #   geom_tile(aes(age, child_age, fill = orphans))
  # p_male
  
  if (uncertainty == FALSE){
    write_csv(file = paste0('global_age_analysis_2021/data/Nigeria/nigeria_orphans_age_all.csv'), d_m1)
  }
  
  
  d_summary = d_m1 %>% select(age, gender, deaths, child_age, orphans) %>% 
    group_by(gender, child_age) %>%
    summarise(orphans = sum(orphans))
  
  # p <- ggplot(d_summary) +
  #   geom_line(aes(child_age, orphans, col = gender)) + 
  #   xlab("Age of child") + ylab("Number of orphans") + labs(col = "Gender of parent") + theme_bw()
  # p
  # ggsave("global_age_analysis_2021/figures/age_nigeria.png", p)
  # 
  if (uncertainty == FALSE){
    write_csv(path = 'global_age_analysis_2021/data/Nigeria/all_data_age.csv', d_summary)
  }
  
  #print(d_summary)
  
  d_summary$category = ifelse(d_summary$child_age %in% c(0, 1, 2, 3, 4), "[0-5)",
                              ifelse(d_summary$child_age %in% c(5, 6, 7, 8, 9), "[5-10)", "[10-18)"))
  d_summary$category <- factor(d_summary$category, levels = c("[0-5)", "[5-10)", "[10-18)"))
  
  d_sum <- d_summary %>%
    group_by(category, gender) %>%
    summarise(orphans = sum(orphans))
  d_sum$orphans_percent <- d_sum$orphans/sum(d_sum$orphans) * 100
  
  # p <- ggplot(d_sum) +
  #   geom_bar(aes(category, orphans, fill = gender), stat = "identity", position=position_dodge()) + 
  #   xlab("Age of child") + ylab("Percent of orphans") + labs(fill = "Gender of parent") + theme_bw()
  # p
  # ggsave("global_age_analysis_2021/figures/age_nigeria.png", p)
  # 
  return(d_sum)
}

# Peru
process_orphans_age_peru = function(uncertainty = FALSE, month = ""){
  if (uncertainty == FALSE){
    d_merge = read.csv(paste0('global_age_analysis_2021/data/Peru/covid19_deaths', month, '.csv'), stringsAsFactors = FALSE)
    d_children = read.csv(paste0('global_age_analysis_2021/data/Peru/child_all_list_both_sex.csv'), stringsAsFactors = FALSE)
  } else {
    d_merge = read.csv(paste0('global_age_analysis_2021/data/Peru/covid19_deaths', month, '_un.csv'), stringsAsFactors = FALSE)
    d_children = read.csv(paste0('global_age_analysis_2021/data/Peru/child_all_list_both_sex_un.csv'), stringsAsFactors = FALSE)
  }
  d_merge$age[which(d_merge$age == "Oct-19")] = '10-19'
  
  d_children$parents_age = paste0((d_children$parents_age)%/%10 * 10 ,'-',(d_children$parents_age) %/% 10 *10 + 9)
  d_children$parents_age = ifelse(d_children$parents_age %in% c('90-99', '100-109'), '90+', d_children$parents_age)
  
  d_children = d_children %>% group_by(parents_age, gender, child_age) %>% mutate(nb_c = mean(prob)) %>%
    select(-prob) %>% ungroup()%>%distinct()
  d_merge$gender = as.character(d_merge$gender)
  d_children$gender = ifelse(d_children$gender == 'female', 'Female', 'Male')
  #d_children$nb_c[which(d_children$gender == "Male" & d_children$parents_age %in% c('80-89', '90+'))] = 0
  
  # d_plot <- d_children
  # d_plot$nb_c[which(d_plot$nb_c == 0)] <- NA
  # p_female <- ggplot(d_plot %>% filter(gender == "Female")) +
  #   geom_tile(aes(parents_age, child_age, fill = nb_c))
  # p_female
  # 
  # p_male <- ggplot(d_plot %>% filter(gender == "Male")) +
  #   geom_tile(aes(parents_age, child_age, fill = nb_c))
  # p_male
  
  d_m1 = left_join(d_merge, d_children, by = c('age'='parents_age', 'gender')) 
  d_m1 = as.data.table(d_m1)
  d_m1[, orphans := round(COVID19_deaths * nb_c)]
  
  d_m1 = d_m1%>% arrange(age) 
  print(sprintf("Total number of orphans %d:", sum(d_m1$orphans)))
  
  # d_plot <- d_m1
  # d_plot$orphans[which(d_plot$orphans == 0)] <- NA
  # p_female <- ggplot(d_plot %>% filter(gender == "Female")) +
  #   geom_tile(aes(age, child_age, fill = orphans))
  # p_female
  # 
  # 
  # p_male <- ggplot(d_plot %>% filter(gender == "Male")) +
  #   geom_tile(aes(age, child_age, fill = orphans))
  # p_male
  # 
  if (uncertainty == FALSE){
    write_csv(file = paste0('global_age_analysis_2021/data/Peru/peru_orphans_age_all.csv'), d_m1)
  }
  
  
  d_summary = d_m1 %>% select(age, gender, COVID19_deaths, child_age, orphans) %>% 
    group_by(gender, child_age) %>%
    summarise(orphans = sum(orphans))
  
  # p <- ggplot(d_summary) +
  #   geom_line(aes(child_age, orphans, col = gender)) + 
  #   xlab("Age of child") + ylab("Number of orphans") + labs(col = "Gender of parent") + theme_bw()
  # p
  # ggsave("global_age_analysis_2021/figures/age_peru.png", p)
  
  if (uncertainty == FALSE){
    write_csv(path = 'global_age_analysis_2021/data/Peru/all_data_age.csv', d_summary)
  }
  
  #print(d_summary)
  
  d_summary$category = ifelse(d_summary$child_age %in% c(0, 1, 2, 3, 4), "[0-5)",
                              ifelse(d_summary$child_age %in% c(5, 6, 7, 8, 9), "[5-10)", "[10-18)"))
  d_summary$category <- factor(d_summary$category, levels = c("[0-5)", "[5-10)", "[10-18)"))
  
  d_sum <- d_summary %>%
    group_by(category, gender) %>%
    summarise(orphans = sum(orphans))
  d_sum$orphans_percent <- d_sum$orphans/sum(d_sum$orphans) * 100
  
  # p <- ggplot(d_sum) +
  #   geom_bar(aes(category, orphans, fill = gender), stat = "identity", position=position_dodge()) + 
  #   xlab("Age of child") + ylab("Percent of orphans") + labs(fill = "Gender of parent") + theme_bw()
  # p
  # ggsave("global_age_analysis_2021/figures/age_peru.png", p)
  return(d_sum)
}

# Philippines
process_orphans_age_philippines = function(uncertainty = FALSE, month = ""){
  if (uncertainty == FALSE){
    d_deaths = read.csv(paste0('global_age_analysis_2021/data/Philippines/covid19_deaths', month, '.csv'), stringsAsFactors = FALSE)
    d_children = read.csv(paste0('global_age_analysis_2021/data/Philippines/child_all_list_both_sex.csv'), stringsAsFactors = FALSE)
  } else {
    d_deaths = read.csv(paste0('global_age_analysis_2021/data/Philippines/covid19_deaths', month, '_un.csv'), stringsAsFactors = FALSE)
    d_children = read.csv(paste0('global_age_analysis_2021/data/Philippines/child_all_list_both_sex_un.csv'), stringsAsFactors = FALSE)
  }
  
  d_merge = copy(d_deaths)
  
  d_children$parents_age = paste0((d_children$parents_age)%/%5 * 5,'-',(d_children$parents_age) %/% 5 * 5 + 4)
  d_children$parents_age = ifelse(d_children$parents_age %in% c('80-84', '85-89', '90-94', '95-99', '100-104'), 
                                  '80+', d_children$parents_age)
  
  d_children = d_children %>% group_by(parents_age, gender, child_age) %>% mutate(nb_c = mean(prob)) %>%
    select(-prob) %>% ungroup()%>%distinct()
  d_merge$gender = as.character(d_merge$gender)
  d_children$gender = ifelse(d_children$gender == 'female', 'Female', 'Male')
  d_merge$gender = ifelse(d_merge$gender == 'female', 'Female', 'Male')
  #d_children$nb_c[which(d_children$gender == "Male" & d_children$parents_age %in% c('80-89', '90+'))] = 0
  
  # d_plot <- d_children
  # d_plot$parents_age[which(d_plot$parents_age == "5-9")] <- "05-9"
  # d_plot$nb_c[which(d_plot$nb_c == 0)] <- NA
  # p_female <- ggplot(d_plot %>% filter(gender == "Female")) +
  #   geom_tile(aes(parents_age, child_age, fill = nb_c))
  # p_female
  # 
  # p_male <- ggplot(d_plot %>% filter(gender == "Male")) +
  #   geom_tile(aes(parents_age, child_age, fill = nb_c))
  # p_male
  
  d_m1 = left_join(d_merge, d_children, by = c('age'='parents_age', 'gender')) 
  d_m1 = as.data.table(d_m1)
  d_m1[, orphans := round(COVID19_deaths * nb_c)]
  
  d_m1 = d_m1%>% arrange(age) 
  print(sprintf("Total number of orphans %d:", sum(d_m1$orphans)))
  
  # d_plot <- d_m1
  # d_plot$age[which(d_plot$age == "5-9")] <- "05-9"
  # d_plot$orphans[which(d_plot$orphans == 0)] <- NA
  # p_female <- ggplot(d_plot %>% filter(gender == "Female")) +
  #   geom_tile(aes(age, child_age, fill = orphans))
  # p_female
  # 
  # 
  # p_male <- ggplot(d_plot %>% filter(gender == "Male")) +
  #   geom_tile(aes(age, child_age, fill = orphans))
  # p_male
  
  if (uncertainty == FALSE){
    write_csv(file = paste0('global_age_analysis_2021/data/Philippines/philippines_orphans_age_all.csv'), d_m1)
  }
  
  d_summary = d_m1 %>% select(age, gender, COVID19_deaths, child_age, orphans) %>% 
    group_by(gender, child_age) %>%
    summarise(orphans = sum(orphans))
  
  # p <- ggplot(d_summary) +
  #   geom_line(aes(child_age, orphans, col = gender)) + 
  #   xlab("Age of child") + ylab("Number of orphans") + labs(col = "Gender of parent") + theme_bw()
  # p
  # ggsave("global_age_analysis_2021/figures/age_philippines.png", p)
  
  if (uncertainty == FALSE){
    write_csv(path = 'global_age_analysis_2021/data/Philippines/all_data_age.csv', d_summary)
  }
  
  #print(d_summary)
  
  d_summary$category = ifelse(d_summary$child_age %in% c(0, 1, 2, 3, 4), "[0-5)",
                              ifelse(d_summary$child_age %in% c(5, 6, 7, 8, 9), "[5-10)", "[10-18)"))
  d_summary$category <- factor(d_summary$category, levels = c("[0-5)", "[5-10)", "[10-18)"))
  
  d_sum <- d_summary %>%
    group_by(category, gender) %>%
    summarise(orphans = sum(orphans))
  d_sum$orphans_percent <- d_sum$orphans/sum(d_sum$orphans) * 100
  
  # p <- ggplot(d_sum) +
  #   geom_bar(aes(category, orphans, fill = gender), stat = "identity", position=position_dodge()) + 
  #   xlab("Age of child") + ylab("Percent of orphans") + labs(fill = "Gender of parent") + theme_bw()
  # p
  # ggsave("global_age_analysis_2021/figures/age_philippines.png", p)
  return(d_sum)
}

# Poland
process_orphans_age_poland = function(uncertainty = FALSE, month = month){
  if (uncertainty == FALSE){
    d_deaths = read.csv(paste0('global_age_analysis_2021/data/Poland/poland_all', month, '.csv'), stringsAsFactors = FALSE)
    d_children = read.csv(paste0('global_age_analysis_2021/data/Poland/child_all_list_both_sex.csv'), stringsAsFactors = FALSE)
  } else {
    d_deaths = read.csv(paste0('global_age_analysis_2021/data/Poland/poland_all', month, '_un.csv'), stringsAsFactors = FALSE)
    d_children = read.csv(paste0('global_age_analysis_2021/data/Poland/child_all_list_both_sex_un.csv'), stringsAsFactors = FALSE)
  }
 
  d_merge = copy(d_deaths)
  
  d_children$parents_age = paste0((d_children$parents_age)%/%10 * 10,'-',(d_children$parents_age) %/% 10 * 10 + 9)
  d_children$parents_age = ifelse(d_children$parents_age %in% c('90-99', '100-109'), 
                                  '90+', d_children$parents_age)
  
  d_children = d_children %>% group_by(parents_age, gender, child_age) %>% mutate(nb_c = mean(prob)) %>%
    select(-prob) %>% ungroup()%>%distinct()
  d_merge$gender = as.character(d_merge$gender)
  d_children$gender = ifelse(d_children$gender == 'female', 'Female', 'Male')
  #d_children$nb_c[which(d_children$gender == "Male" & d_children$parents_age %in% c('80-89', '90+'))] = 0
  
  # d_plot <- d_children
  # d_plot$nb_c[which(d_plot$nb_c == 0)] <- NA
  # p_female <- ggplot(d_plot %>% filter(gender == "Female")) +
  #   geom_tile(aes(parents_age, child_age, fill = nb_c))
  # p_female
  # 
  # p_male <- ggplot(d_plot %>% filter(gender == "Male")) +
  #   geom_tile(aes(parents_age, child_age, fill = nb_c))
  # p_male
  
  d_m1 = left_join(d_merge, d_children, by = c('age'='parents_age', 'gender')) 
  d_m1 = as.data.table(d_m1)
  d_m1[, orphans := round(death * nb_c)]
  
  d_m1 = d_m1%>% arrange(age) 
  print(sprintf("Total number of orphans %d:", sum(d_m1$orphans)))
  
  # d_plot <- d_m1
  # d_plot$orphans[which(d_plot$orphans == 0)] <- NA
  # p_female <- ggplot(d_plot %>% filter(gender == "Female")) +
  #   geom_tile(aes(age, child_age, fill = orphans))
  # p_female
  # 
  # 
  # p_male <- ggplot(d_plot %>% filter(gender == "Male")) +
  #   geom_tile(aes(age, child_age, fill = orphans))
  # p_male
  # 
  
  if (uncertainty == FALSE){
    write_csv(file = paste0('global_age_analysis_2021/data/Poland/poland_orphans_age_all.csv'), d_m1)
  } else {
    write_csv(file = paste0('global_age_analysis_2021/data/Poland/poland_orphans_age_all_un.csv'), d_m1)
  }
 
  
  d_summary = d_m1 %>% select(age, gender, death, child_age, orphans) %>% 
    group_by(gender, child_age) %>%
    summarise(orphans = sum(orphans))
  
  # p <- ggplot(d_summary) +
  #   geom_line(aes(child_age, orphans, col = gender)) + 
  #   xlab("Age of child") + ylab("Number of orphans") + labs(col = "Gender of parent") + theme_bw()
  # p
  # ggsave("global_age_analysis_2021/figures/age_poland.png", p)
  
  if (uncertainty == FALSE){
    write_csv(path = 'global_age_analysis_2021/data/Poland/all_data_age.csv', d_summary)
  } else {
    write_csv(path = 'global_age_analysis_2021/data/Poland/all_data_age_un.csv', d_summary)
  }
  
  #print(d_summary)
  
  d_summary$category = ifelse(d_summary$child_age %in% c(0, 1, 2, 3, 4), "[0-5)",
                              ifelse(d_summary$child_age %in% c(5, 6, 7, 8, 9), "[5-10)", "[10-18)"))
  d_summary$category <- factor(d_summary$category, levels = c("[0-5)", "[5-10)", "[10-18)"))
  
  d_sum <- d_summary %>%
    group_by(category, gender) %>%
    summarise(orphans = sum(orphans))
  d_sum$orphans_percent <- d_sum$orphans/sum(d_sum$orphans) * 100
  
  # p <- ggplot(d_sum) +
  #   geom_bar(aes(category, orphans, fill = gender), stat = "identity", position=position_dodge()) + 
  #   xlab("Age of child") + ylab("Percent of orphans") + labs(fill = "Gender of parent") + theme_bw()
  # p
  # ggsave("global_age_analysis_2021/figures/age_poland.png", p)
  # 
  return(d_sum)
}

# Russia
process_orphans_age_russia = function(month = ""){
  d_deaths = read.csv(paste0('global_age_analysis_2021/data/Russia/all_excess_deaths', month, '.csv'), stringsAsFactors = FALSE)
  d_merge = copy(d_deaths)
  names(d_merge) <- c("gender", "age", "deaths")
  
  d_children = read.csv(paste0('global_age_analysis_2021/data/Russia/child_all_list_both_sex.csv'), stringsAsFactors = FALSE)
  d_children$parents_age = paste0((d_children$parents_age)%/%5 * 5 ,'-',(d_children$parents_age) %/% 5 * 5 + 4)
  d_children$parents_age = ifelse(d_children$parents_age %in% c('100-104'), '100+', d_children$parents_age)
  
  d_children = d_children %>% group_by(parents_age, gender, child_age) %>% mutate(nb_c = mean(prob)) %>%
    select(-prob) %>% ungroup()%>%distinct()
  d_merge$gender = as.character(d_merge$gender)
  d_children$gender = ifelse(d_children$gender == 'female', 'Female', 'Male')
  #d_children$nb_c[which(d_children$gender == "Male" & d_children$parents_age %in% c('80-89', '90+'))] = 0
  
  # d_plot <- d_children
  # d_plot$parents_age[which(d_plot$parents_age == "5-9")] <- "05-9"
  # d_plot$nb_c[which(d_plot$nb_c == 0)] <- NA
  # p_female <- ggplot(d_plot %>% filter(gender == "Female")) +
  #   geom_tile(aes(parents_age, child_age, fill = nb_c))
  # p_female
  # 
  # p_male <- ggplot(d_plot %>% filter(gender == "Male")) +
  #   geom_tile(aes(parents_age, child_age, fill = nb_c))
  # p_male
  
  d_m1 = left_join(d_merge, d_children, by = c('age'='parents_age', 'gender')) 
  d_m1 = as.data.table(d_m1)
  d_m1[, orphans := round(deaths * nb_c)]
  
  d_m1 = d_m1%>% arrange(age) 
  print(sprintf("Total number of orphans %d:", sum(d_m1$orphans)))
  
  # d_plot <- d_m1
  # d_plot$age[which(d_plot$age == "5-9")] <- "05-9"
  # d_plot$orphans[which(d_plot$orphans == 0)] <- NA
  # p_female <- ggplot(d_plot %>% filter(gender == "Female")) +
  #   geom_tile(aes(age, child_age, fill = orphans))
  # p_female
  # 
  # 
  # p_male <- ggplot(d_plot %>% filter(gender == "Male")) +
  #   geom_tile(aes(age, child_age, fill = orphans))
  # p_male
  
  write_csv(file = paste0('global_age_analysis_2021/data/Russia/russia_orphans_age_all.csv'), d_m1)
  
  d_summary = d_m1 %>% select(age, gender, deaths, child_age, orphans) %>% 
    group_by(gender, child_age) %>%
    summarise(orphans = sum(orphans))
  
  # p <- ggplot(d_summary) +
  #   geom_line(aes(child_age, orphans, col = gender)) + 
  #   xlab("Age of child") + ylab("Number of orphans") + labs(col = "Gender of parent") + theme_bw()
  # p
  # ggsave("global_age_analysis_2021/figures/age_russia.png", p)
  # 
  write_csv(path = 'global_age_analysis_2021/data/Russia/all_data_age.csv', d_summary)
  #print(d_summary)
  
  d_summary$category = ifelse(d_summary$child_age %in% c(0, 1, 2, 3, 4), "[0-5)",
                              ifelse(d_summary$child_age %in% c(5, 6, 7, 8, 9), "[5-10)", "[10-18)"))
  d_summary$category <- factor(d_summary$category, levels = c("[0-5)", "[5-10)", "[10-18)"))
  
  d_sum <- d_summary %>%
    group_by(category, gender) %>%
    summarise(orphans = sum(orphans))
  d_sum$orphans <- d_sum$orphans/sum(d_sum$orphans) * 100
  
  p <- ggplot(d_sum) +
    geom_bar(aes(category, orphans, fill = gender), stat = "identity", position=position_dodge()) + 
    xlab("Age of child") + ylab("Percent of orphans") + labs(fill = "Gender of parent") + theme_bw()
  p
  ggsave("global_age_analysis_2021/figures/age_russia.png", p)
  
  return(d_sum)
}

# South Africa
process_orphans_age_southafrica = function(uncertainty = FALSE, month = ""){
  if (uncertainty == FALSE){
    d_deaths = read.csv(paste0('global_age_analysis_2021/data/SouthAfrica/covid19_deaths', month, '.csv'), stringsAsFactors = FALSE)
    d_children = read.csv(paste0('global_age_analysis_2021/data/SouthAfrica/child_all_list_both_sex.csv'), stringsAsFactors = FALSE)
  } else {
    d_deaths = read.csv(paste0('global_age_analysis_2021/data/SouthAfrica/covid19_deaths', month, '_un.csv'), stringsAsFactors = FALSE)
    d_children = read.csv(paste0('global_age_analysis_2021/data/SouthAfrica/child_all_list_both_sex_un.csv'), stringsAsFactors = FALSE)
  }
  
  d_merge = copy(d_deaths)
  d_merge$age = as.character(d_merge$age)
  setnames(d_merge, 'COVID19_deaths', 'deaths')
  
  
  d_children$parents_age = paste0((d_children$parents_age)%/%10 * 10 ,'-',(d_children$parents_age) %/% 10 *10+9)
  d_children$parents_age = ifelse(d_children$parents_age %in% c('80-89', '90-99', '100-109'), '80+', d_children$parents_age)
  
  d_children = d_children %>% group_by(parents_age, gender, child_age) %>% mutate(nb_c = mean(prob)) %>%
    select(-prob) %>% ungroup()%>%distinct()
  d_merge$gender = as.character(d_merge$gender)
  d_children$gender = ifelse(d_children$gender == 'female', 'Female', 'Male')
  #d_children$nb_c[which(d_children$gender == "Male" & d_children$parents_age %in% c('80-89', '90+'))] = 0
  
  # d_plot <- d_children
  # d_plot$nb_c[which(d_plot$nb_c == 0)] <- NA
  # p_female <- ggplot(d_plot %>% filter(gender == "Female")) +
  #   geom_tile(aes(parents_age, child_age, fill = nb_c))
  # p_female
  # 
  # p_male <- ggplot(d_plot %>% filter(gender == "Male")) +
  #   geom_tile(aes(parents_age, child_age, fill = nb_c))
  # p_male
  
  d_m1 = left_join(d_merge, d_children, by = c('age'='parents_age', 'gender')) 
  d_m1 = as.data.table(d_m1)
  d_m1[, orphans := round(deaths * nb_c)]
  
  d_m1 = d_m1%>% arrange(age) 
  print(sprintf("Total number of orphans %d:", sum(d_m1$orphans)))
  
  # d_plot <- d_m1
  # d_plot$orphans[which(d_plot$orphans == 0)] <- NA
  # p_female <- ggplot(d_plot %>% filter(gender == "Female")) +
  #   geom_tile(aes(age, child_age, fill = orphans))
  # p_female
  # 
  # 
  # p_male <- ggplot(d_plot %>% filter(gender == "Male")) +
  #   geom_tile(aes(age, child_age, fill = orphans))
  # p_male
  
  if (uncertainty == FALSE){
    write_csv(file = paste0('global_age_analysis_2021/data/SouthAfrica/southafrica_orphans_age_all.csv'), d_m1)
  }
  
  
  d_summary = d_m1 %>% select(age, gender, deaths, child_age, orphans) %>% 
    group_by(gender, child_age) %>%
    summarise(orphans = sum(orphans))
  
  # p <- ggplot(d_summary) +
  #   geom_bar(aes(child_age, orphans, fill = gender), stat="identity", position=position_dodge()) + 
  #   xlab("Age of child") + ylab("Number of orphans") + labs(fill = "Gender of parent") + theme_bw()
  # p
  # ggsave("global_age_analysis_2021/figures/age_southafrica.png", p)
  
  if (uncertainty == FALSE){
    write_csv(path = 'global_age_analysis_2021/data/SouthAfrica/all_data_age.csv', d_summary)
  }

  #print(d_summary)
  
  d_summary$category = ifelse(d_summary$child_age %in% c(0, 1, 2, 3, 4), "[0-5)",
                              ifelse(d_summary$child_age %in% c(5, 6, 7, 8, 9), "[5-10)", "[10-18)"))
  d_summary$category <- factor(d_summary$category, levels = c("[0-5)", "[5-10)", "[10-18)"))
  
  d_sum <- d_summary %>%
    group_by(category, gender) %>%
    summarise(orphans = sum(orphans))
  d_sum$orphans_percent <- d_sum$orphans/sum(d_sum$orphans) * 100
  
  # p <- ggplot(d_sum) +
  #   geom_bar(aes(category, orphans, fill = gender), stat = "identity", position=position_dodge()) + 
  #   xlab("Age of child") + ylab("Percent of orphans") + labs(fill = "Gender of parent") + theme_bw()
  # p
  # ggsave("global_age_analysis_2021/figures/age_southafrica.png", p)
  
  return(d_sum)
}

# Spain
process_orphans_age_spain = function(uncertainty = FALSE, month = ""){
  if (uncertainty == FALSE){
    d_deaths = read.csv(paste0('global_age_analysis_2021/data/Spain/spain_all', month, '.csv'), stringsAsFactors = FALSE)
    d_children = read.csv(paste0('global_age_analysis_2021/data/Spain/child_all_list_both_sex.csv'), stringsAsFactors = FALSE)
  } else {
    d_deaths = read.csv(paste0('global_age_analysis_2021/data/Spain/spain_all', month, '_un.csv'), stringsAsFactors = FALSE)
    d_children = read.csv(paste0('global_age_analysis_2021/data/Spain/child_all_list_both_sex_un.csv'), stringsAsFactors = FALSE)
  }
  
  d_merge = copy(d_deaths)
  
  d_children$parents_age = paste0((d_children$parents_age)%/%10 * 10 ,'-',(d_children$parents_age) %/% 10 *10+9)
  d_children$parents_age = ifelse(d_children$parents_age %in% c('80-89', '90-99','100-109'), '80+', d_children$parents_age)
  
  d_children = d_children %>% group_by(parents_age, gender, child_age) %>% mutate(nb_c = mean(prob)) %>%
    select(-prob) %>% ungroup()%>%distinct()
  d_merge$gender = as.character(d_merge$gender)
  d_children$gender = ifelse(d_children$gender == 'female', 'Female', 'Male')
  #d_children$nb_c[which(d_children$gender == "Male" & d_children$parents_age %in% c('80-89', '90+'))] = 0
  
  # d_plot <- d_children
  # d_plot$nb_c[which(d_plot$nb_c == 0)] <- NA
  # p_female <- ggplot(d_plot %>% filter(gender == "Female")) +
  #   geom_tile(aes(parents_age, child_age, fill = nb_c))
  # p_female
  # 
  # p_male <- ggplot(d_plot %>% filter(gender == "Male")) +
  #   geom_tile(aes(parents_age, child_age, fill = nb_c))
  # p_male
  
  d_m1 = left_join(d_merge, d_children, by = c('age'='parents_age', 'gender')) 
  d_m1 = as.data.table(d_m1)
  d_m1[, orphans := round(deaths * nb_c)]
  
  d_m1 = d_m1%>% arrange(age) 
  print(sprintf("Total number of orphans %d:", sum(d_m1$orphans)))
  
  # d_plot <- d_m1
  # d_plot$orphans[which(d_plot$orphans == 0)] <- NA
  # p_female <- ggplot(d_plot %>% filter(gender == "Female")) +
  #   geom_tile(aes(age, child_age, fill = orphans))
  # p_female
  # 
  # 
  # p_male <- ggplot(d_plot %>% filter(gender == "Male")) +
  #   geom_tile(aes(age, child_age, fill = orphans))
  # p_male
  
  if (uncertainty == FALSE){
    write_csv(file = paste0('global_age_analysis_2021/data/Spain/spain_orphans_age_all.csv'), d_m1)
  }
  
  
  d_summary = d_m1 %>% select(age, gender, deaths, child_age, orphans) %>% 
    group_by(gender, child_age) %>%
    summarise(orphans = sum(orphans))
  
  # p <- ggplot(d_summary) +
  #   geom_line(aes(child_age, orphans, col = gender)) + 
  #   xlab("Age of child") + ylab("Number of orphans") + labs(col = "Gender of parent") + theme_bw()
  # p
  # ggsave("global_age_analysis_2021/figures/age_spain.png", p)
  
  if (uncertainty == FALSE){
    write_csv(path = 'global_age_analysis_2021/data/Spain/all_data_age.csv', d_summary)
  }
  
  #print(d_summary)
  
  d_summary$category = ifelse(d_summary$child_age %in% c(0, 1, 2, 3, 4), "[0-5)",
                              ifelse(d_summary$child_age %in% c(5, 6, 7, 8, 9), "[5-10)", "[10-18)"))
  d_summary$category <- factor(d_summary$category, levels = c("[0-5)", "[5-10)", "[10-18)"))
  
  d_sum <- d_summary %>%
    group_by(category, gender) %>%
    summarise(orphans = sum(orphans))
  d_sum$orphans_percent <- d_sum$orphans/sum(d_sum$orphans) * 100
  
  # p <- ggplot(d_sum) +
  #   geom_bar(aes(category, orphans, fill = gender), stat = "identity", position=position_dodge()) + 
  #   xlab("Age of child") + ylab("Percent of orphans") + labs(fill = "Gender of parent") + theme_bw()
  # p
  # ggsave("global_age_analysis_2021/figures/spain.png", p)
  # 
  return(d_sum)
}

# USA
process_orphans_age_usa = function(uncertainty = FALSE, month = ""){
  if (uncertainty == FALSE){
    d_deaths = read.csv(paste0('global_age_analysis_2021/data/USA/usa_all', month, '.csv'), stringsAsFactors = FALSE)
    d_children = read.csv(paste0('global_age_analysis_2021/data/USA/child_all_list_both_sex.csv'), stringsAsFactors = FALSE)
  } else {
    d_deaths = read.csv(paste0('global_age_analysis_2021/data/USA/usa_all', month, '_un.csv'), stringsAsFactors = FALSE)
    d_children = read.csv(paste0('global_age_analysis_2021/data/USA/child_all_list_both_sex_un.csv'), stringsAsFactors = FALSE)
  }
  
  d_merge = copy(d_deaths)

  d_children$parents_age = paste0((d_children$parents_age)%/%5 * 5 ,'-',(d_children$parents_age) %/% 5 *5+4)
  d_children$parents_age = ifelse(d_children$parents_age %in% c('15-19', '20-24'), '15-24',
                                  ifelse(d_children$parents_age %in% c('25-29', '30-34'), '25-34',
                                         ifelse(d_children$parents_age %in% c('35-39', '40-44'), '35-44',
                                                ifelse(d_children$parents_age %in% c('45-49', '50-54'), '45-54',
                                                       ifelse(d_children$parents_age %in% c('55-59', '60-64'), '55-64',
                                                              ifelse(d_children$parents_age %in% c('65-69', '70-74'), '65-74',
                                                                     ifelse(d_children$parents_age %in% c('75-79', '80-84'), '75-84',
                                                                            ifelse(d_children$parents_age %in% c('85-89', '90-94', 
                                                                                                                 '95-99', "100-104"), '85+', '0-14'))))))))
  d_children = d_children %>% group_by(parents_age, gender, child_age) %>% mutate(nb_c = mean(prob)) %>%
    select(-prob) %>% ungroup()%>%distinct()
  d_merge$gender = as.character(d_merge$gender)
  d_children$gender = ifelse(d_children$gender == 'female', 'Female', 'Male')
  #d_children$nb_c[which(d_children$gender == "Male" & d_children$parents_age %in% c('80-89', '90+'))] = 0
  
  # d_plot <- d_children
  # d_plot$nb_c[which(d_plot$nb_c == 0)] <- NA
  # p_female <- ggplot(d_plot %>% filter(gender == "Female")) +
  #   geom_tile(aes(parents_age, child_age, fill = nb_c))
  # p_female
  # 
  # p_male <- ggplot(d_plot %>% filter(gender == "Male")) +
  #   geom_tile(aes(parents_age, child_age, fill = nb_c))
  # p_male
  
  d_m1 = left_join(d_merge, d_children, by = c('age'='parents_age', 'gender')) 
  d_m1 = as.data.table(d_m1)
  d_m1[, orphans := round(deaths * nb_c)]
  
  d_m1 = d_m1%>% arrange(age) 
  print(sprintf("Total number of orphans %d:", sum(d_m1$orphans)))
  
  # d_plot <- d_m1
  # d_plot$orphans[which(d_plot$orphans == 0)] <- NA
  # p_female <- ggplot(d_plot %>% filter(gender == "Female")) +
  #   geom_tile(aes(age, child_age, fill = orphans))
  # p_female
  # 
  # 
  # p_male <- ggplot(d_plot %>% filter(gender == "Male")) +
  #   geom_tile(aes(age, child_age, fill = orphans))
  # p_male
  
  if (uncertainty == FALSE){
    write_csv(file = paste0('global_age_analysis_2021/data/USA/usa_orphans_age_all.csv'), d_m1)
  }
  
  
  d_summary = d_m1 %>% select(age, gender, deaths, child_age, orphans) %>% 
    group_by(gender, child_age) %>%
    summarise(orphans = sum(orphans))
  
  # p <- ggplot(d_summary) +
  #   geom_bar(aes(child_age, orphans, fill = gender), stat="identity", position=position_dodge()) + 
  #   xlab("Age of child") + ylab("Number of orphans") + labs(fill = "Gender of parent") + theme_bw()
  # p
  # ggsave("global_age_analysis_2021/figures/age_usa.png", p)
  # 
  
  if (uncertainty == FALSE){
    write_csv(path = 'global_age_analysis_2021/data/USA/all_data_age.csv', d_summary)
}
  
  #print(d_summary)
  
  d_summary$category = ifelse(d_summary$child_age %in% c(0, 1, 2, 3, 4), "[0-5)",
                              ifelse(d_summary$child_age %in% c(5, 6, 7, 8, 9), "[5-10)", "[10-18)"))
  d_summary$category <- factor(d_summary$category, levels = c("[0-5)", "[5-10)", "[10-18)"))
  
  d_sum <- d_summary %>%
    group_by(category, gender) %>%
    summarise(orphans = sum(orphans))
  d_sum$orphans_percent <- d_sum$orphans/sum(d_sum$orphans) * 100
  
  # p <- ggplot(d_sum) +
  #   geom_bar(aes(category, orphans, fill = gender), stat = "identity", position=position_dodge()) + 
  #   xlab("Age of child") + ylab("Percent of orphans") + labs(fill = "Gender of parent") + theme_bw()
  # p
  # ggsave("global_age_analysis_2021/figures/age_usa.png", p)
  
  return(d_sum)
  
}

# Zimbabwe
process_orphans_age_zimbabwe = function(uncertainty = FALSE, month = ""){
  if (uncertainty == FALSE){
    d_merge = read.csv(paste0('global_age_analysis_2021/data/Zimbabwe/covid19_deaths', month, '.csv'), stringsAsFactors = FALSE)
    d_children = read.csv(paste0('global_age_analysis_2021/data/Zimbabwe/child_all_list_both_sex.csv'), stringsAsFactors = FALSE)
  } else {
    d_merge = read.csv(paste0('global_age_analysis_2021/data/Zimbabwe/covid19_deaths', month, '_un.csv'), stringsAsFactors = FALSE)
    d_children = read.csv(paste0('global_age_analysis_2021/data/Zimbabwe/child_all_list_both_sex_un.csv'), stringsAsFactors = FALSE)
  }
  
  d_children$parents_age = paste0((d_children$parents_age)%/%10 * 10 + 1,'-',(d_children$parents_age) %/% 10 *10 + 10)
  d_children$parents_age = ifelse(d_children$parents_age %in% c('91-100', '101-110'), '91+', d_children$parents_age)
  
  d_children = d_children %>% group_by(parents_age, gender, child_age) %>% mutate(nb_c = mean(prob)) %>%
    select(-prob) %>% ungroup()%>%distinct()
  d_merge$gender = as.character(d_merge$gender)
  d_children$gender = ifelse(d_children$gender == 'female', 'Female', 'Male')
  #d_children$nb_c[which(d_children$gender == "Male" & d_children$parents_age %in% c('80-89', '90+'))] = 0
  
  # d_plot <- d_children
  # d_plot$nb_c[which(d_plot$nb_c == 0)] <- NA
  # p_female <- ggplot(d_plot %>% filter(gender == "Female")) +
  #   geom_tile(aes(parents_age, child_age, fill = nb_c))
  # p_female
  # 
  # p_male <- ggplot(d_plot %>% filter(gender == "Male")) +
  #   geom_tile(aes(parents_age, child_age, fill = nb_c))
  # p_male
  
  d_m1 = left_join(d_merge, d_children, by = c('age'='parents_age', 'gender')) 
  d_m1 = as.data.table(d_m1)
  d_m1[, orphans := round(deaths * nb_c)]
  
  d_m1 = d_m1%>% arrange(age) 
  print(sprintf("Total number of orphans %d:", sum(d_m1$orphans)))
  
  # d_plot <- d_m1
  # d_plot$orphans[which(d_plot$orphans == 0)] <- NA
  # p_female <- ggplot(d_plot %>% filter(gender == "Female")) +
  #   geom_tile(aes(age, child_age, fill = orphans))
  # p_female
  # 
  # 
  # p_male <- ggplot(d_plot %>% filter(gender == "Male")) +
  #   geom_tile(aes(age, child_age, fill = orphans))
  # p_male
  
  if (uncertainty == FALSE){
    write_csv(file = paste0('global_age_analysis_2021/data/Zimbabwe/zimbabwe_orphans_age_all.csv'), d_m1)
  }
  
  
  d_summary = d_m1 %>% select(age, gender, deaths, child_age, orphans) %>% 
    group_by(gender, child_age) %>%
    summarise(orphans = sum(orphans))
  
  # p <- ggplot(d_summary) +
  #   geom_line(aes(child_age, orphans, col = gender)) + 
  #   xlab("Age of child") + ylab("Number of orphans") + labs(col = "Gender of parent") + theme_bw()
  # p
  # ggsave("global_age_analysis_2021/figures/age_zimbabwe.png", p)
  # 
  if (uncertainty == FALSE){
    write_csv(path = 'global_age_analysis_2021/data/Zimbabwe/all_data_age.csv', d_summary)
  }
  
  #print(d_summary)
  
  d_summary$category = ifelse(d_summary$child_age %in% c(0, 1, 2, 3, 4), "[0-5)",
                              ifelse(d_summary$child_age %in% c(5, 6, 7, 8, 9), "[5-10)", "[10-18)"))
  d_summary$category <- factor(d_summary$category, levels = c("[0-5)", "[5-10)", "[10-18)"))
  
  d_sum <- d_summary %>%
    group_by(category, gender) %>%
    summarise(orphans = sum(orphans))
  d_sum$orphans_percent <- d_sum$orphans/sum(d_sum$orphans) * 100
  
  # p <- ggplot(d_sum) +
  #   geom_bar(aes(category, orphans, fill = gender), stat = "identity", position=position_dodge()) + 
  #   xlab("Age of child") + ylab("Percent of orphans") + labs(fill = "Gender of parent") + theme_bw()
  # p
  # ggsave("global_age_analysis_2021/figures/age_zimbabwe.png", p)
  # 
  return(d_sum)
}