library(readxl)
library(data.table)
library(tidyverse)
library(rjson)

process_fertility_usa_states_plots = function(country,s,r){
    data_father = read.csv(paste0('data/fertility/', 'usa_states_fertility_m_all.csv'))
    #setnames(data_father, c("country", "age",  "date","fertility_rate", "gender"))
    data_mother = read.csv(paste0('data/fertility/', 'usa_states_fertility_f.csv'))
    if(all(c(nrow(subset(data_father,state==s & race.eth==r))>0,nrow(subset(data_mother,state==s & race.eth==r))>0))){
      data_father <- subset(data_father,state==s & race.eth==r)
      data_mother <- subset(data_mother,state==s & race.eth==r)
      data_mother$gender = 'Female'
      data_father$gender = 'Male'
      data_father = data_father %>% filter(!age %in% c('under 15-NA', '80+-NA','0-14'))
      data_mother = data_mother %>% filter(!age %in% c('under 15-NA', '80+-NA','0-14'))
      data_mother$fertility_rate = ifelse(data_mother$age %in% c('50-54', '55-59', '60-64', 
                                                                 '65-69', '70-74', '75-79', 
                                                                 '80+',  '65+', '55+','50+','Unknown'), 
                                          NA, data_mother$fertility_rate)
      data_father$fertility_rate = ifelse(data_father$age %in% c('80+','Unknown','Unknown or Not Stated'), NA, data_father$fertility_rate)
      
      data_father = data_father %>% select(year, age, gender, fertility_rate)
      #data_mother$year = data_mother$date
      
      data_mother = data_mother %>% select(year, age, gender, fertility_rate)
      #setnames(data_mother, 'afr', 'fertility_rate')
      data_combine = rbind(data_father, data_mother)
      #setnames(data_combine, 'fertility_rate', 'rate')
  
      data_combine$year = as.character(data_combine$year)
      ggplot(data_combine) +
        geom_point(aes(x = age, y = fertility_rate, color = year)) +
        theme_bw()+
        theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1)) +
        guides(col = guide_legend(nrow = 7)) +
        labs(x = 'Age') +
        facet_wrap(~ gender, 
                   strip.position = "left", 
                   labeller = as_labeller(c(Female = "Fertility Rate per 1000 women", 
                                            Male = "Fertility Rate per 1000 men") ) ,scales="free_x") + 
        theme(strip.background =element_rect(fill="white")) + 
        ylab(NULL) +
        theme(strip.background = element_blank(),
              strip.placement = "outside")
      ggsave(paste0("figures/fertility_",country, ".png"), width = 10, height = 4)
    }
}

# USA by state
process_usa_states_fertility = function(rep=000){
  
  if(rep==000) set.seed(100)
  # Bridged-Race Population Estimates 1990-2019
  # https://wonder.cdc.gov/controller/datarequest/D163;jsessionid=2215DEF81E8899902B8456ABD15D

  indir.pop <- file.path('data','pop')
  infiles <- data.table(F=list.files(indir.pop, pattern='pop_f', full.names=TRUE, recursive=TRUE))
  data_pop_f = read.delim(infiles[1,F],header = TRUE, sep = "\t")
  
  for(i in 2:nrow(infiles)){
    infile <- infiles[i,F]
    cat('Process',infile,'\n')
    tmp <- read.delim(infile,header = TRUE, sep = "\t")
    data_pop_f <- merge(data_pop_f,tmp,by=c('State','State.Code','Yearly.July.1st.Estimates','Yearly.July.1st.Estimates.Code',
                                            'Age.Group', 'Age.Group.Code', 'Race', 'Race.Code', 'Ethnicity', 'Ethnicity.Code', 'Population','Notes'),all=T)
  }
  data_pop_f <- data_pop_f[!is.na(data_pop_f$State.Code),]
  
  data_pop_f <- data_pop_f %>%
    mutate(age.cat:= case_when(Age.Group.Code %in% c('1','1-4','5-9','10-14') ~ '0-14',
                               Age.Group.Code %in% c('15-19') ~ '15-19',
                               Age.Group.Code %in% c('20-24') ~ '20-24',
                               Age.Group.Code %in% c('25-29') ~ '25-29',
                               Age.Group.Code %in% c('30-34') ~ '30-34',
                               Age.Group.Code %in% c('35-39') ~ '35-39',
                               Age.Group.Code %in% c('40-44') ~ '40-44',
                               Age.Group.Code %in% c('45-49') ~ '45-49',
                               Age.Group.Code %in% c('50-54','55-59','60-64','65-69','70-74','75-79','80-84') ~ '50+',
                               TRUE ~'unknown'),
           race.eth:= case_when(Ethnicity=='Hispanic or Latino'~'Hispanic',
                                Ethnicity=='Not Hispanic or Latino' & Race=='American Indian or Alaska Native'~'Non-Hispanic American Indian or Alaska Native',
                                Ethnicity=='Not Hispanic or Latino' & Race=='Asian or Pacific Islander'~'Non-Hispanic Asian',
                                Ethnicity=='Not Hispanic or Latino' & Race=='Black or African American'~'Non-Hispanic Black',
                                Ethnicity=='Not Hispanic or Latino' & Race=='White'~'Non-Hispanic White',
                                TRUE~'Unknown'
                                ))
  data_pop_f <- data.table(data_pop_f)
  setnames(data_pop_f,c('State','Yearly.July.1st.Estimates','Race','Ethnicity','Population'),c('state','year','race','hispanic','population'))
  data_pop_f_agec <- data_pop_f[, list(population=sum(population)),by=c('state','year',
                                                                        'age.cat', 'race.eth')]
  
  write.csv(file = 'data/pop/usa_states_population_f.csv', data_pop_f_agec)

  # for men

  indir.pop <- file.path('data','pop')
  infiles <- data.table(F=list.files(indir.pop, pattern='pop_m', full.names=TRUE, recursive=FALSE))
  data_pop_m = read.delim(infiles[1,F],header = TRUE, sep = "\t")
  
  for(i in 2:nrow(infiles)){
    infile <- infiles[i,F]
    cat('Process',infile,'\n')
    tmp <- read.delim(infile,header = TRUE, sep = "\t")
    data_pop_m <- merge(data_pop_m,tmp,by=c('States','States.Code','Yearly.July.1st.Estimates','Yearly.July.1st.Estimates.Code',
                                            'Five.Year.Age.Groups', 'Five.Year.Age.Groups.Code', 'Race', 'Race.Code', 'Ethnicity', 'Ethnicity.Code', 'Population','Notes'),all=T)
  }
  data_pop_m <- data_pop_m[!is.na(data_pop_m$States.Code),]
  
  data_pop_m <- data_pop_m %>%
    mutate(age.cat:= case_when(Five.Year.Age.Groups.Code %in% c('1','1-4','5-9','10-14') ~ '0-14',
                               Five.Year.Age.Groups.Code %in% c('15-19') ~ '15-19',
                               Five.Year.Age.Groups.Code %in% c('20-24') ~ '20-24',
                               Five.Year.Age.Groups.Code %in% c('25-29') ~ '25-29',
                               Five.Year.Age.Groups.Code %in% c('30-34') ~ '30-34',
                               Five.Year.Age.Groups.Code %in% c('35-39') ~ '35-39',
                               Five.Year.Age.Groups.Code %in% c('40-44') ~ '40-44',
                               Five.Year.Age.Groups.Code %in% c('45-49') ~ '45-49',
                               Five.Year.Age.Groups.Code %in% c('50-54') ~ '50-54',
                               Five.Year.Age.Groups.Code %in% c('55-59','60-64','65-69','70-74','75-79','80-84','85+') ~ '55+',
                               TRUE ~'Unknown'),
             race.eth:= case_when(Ethnicity=='Hispanic or Latino'~'Hispanic',
                              Ethnicity=='Not Hispanic or Latino' & Race=='American Indian or Alaska Native'~'Non-Hispanic American Indian or Alaska Native',
                              Ethnicity=='Not Hispanic or Latino' & Race=='Asian'~'Non-Hispanic Asian',
                              Ethnicity=='Not Hispanic or Latino' & Race=='Native Hawaiian or Other Pacific Islander'~'Non-Hispanic Asian',
                              Ethnicity=='Not Hispanic or Latino' & Race=='More than one race'~'Non-Hispanic More than one race',
                              Ethnicity=='Not Hispanic or Latino' & Race=='Black or African American'~'Non-Hispanic Black',
                              Ethnicity=='Not Hispanic or Latino' & Race=='White'~'Non-Hispanic White',
                              TRUE~'Unknown'))
  data_pop_m <- data.table(data_pop_m)
  setnames(data_pop_m,c('States','Yearly.July.1st.Estimates','Race','Ethnicity','Population'),c('state','year','race','hispanic','population'))
  data_pop_m_agec <- data_pop_m[, list(population=sum(population)),by=c('state','year',
                                                                        'age.cat', 'race.eth')]
  write.csv(file = 'data/pop/usa_states_population_m.csv', data_pop_m_agec)
  
  ## fertility data
  # from CDC wonder
  ## url: https://wonder.cdc.gov/wonder/help/natality.html
  indir.bir <- file.path('data')
  infiles <- data.table(F=list.files(indir.bir, pattern='fertility_women_', full.names=TRUE, recursive=FALSE))
  
  cat('Process',infiles[1,F],'\n')
  data_fertility = read.delim(infiles[1,F],header = TRUE, sep = "\t")
  data_fertility <- data_fertility[!is.na(data_fertility$State.Code),]
  for(i in 2:nrow(infiles)){
    infile <- infiles[i,F]
    cat('Process',infile,'\n')
    tmp <- read.delim(infile,header = TRUE, sep = "\t")
    tmp <- tmp[!is.na(tmp$State.Code),]
    data_fertility <- merge(data_fertility,tmp,by=c('State','State.Code','Age.of.Mother.9', 'Age.of.Mother.9.Code', 'Mother.s.Bridged.Race', 'Mother.s.Bridged.Race.Code',
                                                    'Mother.s.Hispanic.Origin', 'Mother.s.Hispanic.Origin.Code', 'Year', 'Year.Code', 'Births','Fertility.Rate','Female.Population','Notes'),all=T)
  }
  data_fertility <- data.table(data_fertility)
  data_fertility[, age:= gsub('([A-Za-z0-9]+) years*','\\1',data_fertility$Age.of.Mother.9)]
  data_fertility[age=='Under 15',age:='0-14']
  data_fertility[age=='50 and over',age:='50+']
  data_fertility[age=='50 years and over',age:='50+']
  data_fertility[age=='Unknown or Not Stated',age:='unknown']
  data_fertility$age = as.character(data_fertility$age)
  data_fertility$age = gsub(' - ', '-', data_fertility$age)
  data_fertility$age = gsub(' [+]', '+', data_fertility$age)
  data_fertility$age = gsub('Under ', '0-', data_fertility$age)
  data_fertility$age = gsub(' and over', '+', data_fertility$age)
  data_fertility$age = ifelse(data_fertility$age == '0-19', '15-19', data_fertility$age)
  
  data_fertility <- data_fertility %>% mutate(race.eth:= case_when(Mother.s.Hispanic.Origin=='Hispanic or Latino'~'Hispanic',
                                                                   Mother.s.Hispanic.Origin=='Not Hispanic or Latino' & Mother.s.Bridged.Race=='American Indian or Alaska Native'~'Non-Hispanic American Indian or Alaska Native',
                                                                   Mother.s.Hispanic.Origin=='Not Hispanic or Latino' & Mother.s.Bridged.Race=='Asian or Pacific Islander'~'Non-Hispanic Asian',
                                                                   Mother.s.Hispanic.Origin=='Not Hispanic or Latino' & Mother.s.Bridged.Race=='Black or African American'~'Non-Hispanic Black',
                                                                   Mother.s.Hispanic.Origin=='Not Hispanic or Latino' & Mother.s.Bridged.Race=='White'~'Non-Hispanic White',
                                                                   TRUE~'Unknown'))
  data_fertility <- subset(data_fertility,select = c('State','Year','age','Mother.s.Bridged.Race',
                                                     'Mother.s.Hispanic.Origin','race.eth','Fertility.Rate','Births'))
  setnames(data_fertility,c('State','Year','Mother.s.Bridged.Race','Mother.s.Hispanic.Origin','Fertility.Rate','Births'),
           c('state','year','race','hispanic','fertility_rate','births'))
  
  data_fertility = as.data.table(data_fertility)
  data_fertility[, gender:='Female']
  
  data_fertility$year = as.numeric(as.character(data_fertility$year))
  data_fertility$births = as.numeric(as.character(data_fertility$births))
  data_fertility = as.data.table(data_fertility)
  data_fertility = data_fertility %>% filter(age != '0-15') %>% arrange(year, age)
  data_fertility = as.data.table(data_fertility)
  data_fertility <- subset(data_fertility,!is.na(year))
  setnames(data_fertility,'age','age.cat')
  data_fertility <- data_fertility[, list(births=sum(births)),by=c('state','year','age.cat','race.eth','gender')]
  data_combine= merge(data_fertility, data_pop_f_agec, by.x = c('state','year', 'age.cat','race.eth'),by.y=c('state','year','age.cat','race.eth'),all.x=T)
  setnames(data_combine,'age.cat','age')
  if(rep!=000){
    samp <- rpois(length(data_combine[!is.na(births),births]),lambda=data_combine[!is.na(births),births])
    data_combine[!is.na(births), births:=samp]
  }
  data_combine[,fertility_rate := births / (population)*1000]
  # fill in missing with means
  do <- as.data.table(tidyr::crossing(state=unique(droplevels(as.factor(data_combine$state))),year=seq(2003,2019,1),
                                      age=unique(data_combine$age),race.eth=unique(data_combine$race.eth)))
  fert_f <- merge(data_combine,do,by=c('state','year','age','race.eth'),all=T)
  fert_f <- fert_f[fert_f$state!="",]
  write.csv(file = 'data/fertility/usa_states_fertility_f.csv', fert_f)
  
  
  ## for mens (nb. mens race category defined differently from women's)
  data_fertility = data.table(read.delim('data/fertility_men_2016-2019.txt',header = TRUE, sep = "\t"))
  data_fertility <- subset(data_fertility,!is.na(State.of.Residence.Code))
  data_fertility[, age:= gsub('([A-Za-z0-9]+) years*','\\1',data_fertility$Age.of.Father)]
  data_fertility[age=='Under 15',age:='0-14']
  data_fertility[age=='55 and older',age:='55+']
  data_fertility[age=='Unknown or Not Stated',age:='Unknown']
  
  data_fertility <- data_fertility %>% mutate(
              race.eth:= case_when(Father.s.Hispanic.Origin=='Hispanic or Latino'~'Hispanic',
                                   Father.s.Hispanic.Origin=='Not Hispanic or Latino' & Father.s.Single.Race.6=='American Indian or Alaska Native'~'Non-Hispanic American Indian or Alaska Native',
                                   Father.s.Hispanic.Origin=='Not Hispanic or Latino' & Father.s.Single.Race.6=='Asian'~'Non-Hispanic Asian',
                                   Father.s.Hispanic.Origin=='Not Hispanic or Latino' & Father.s.Single.Race.6=='Native Hawaiian or Other Pacific Islander'~'Non-Hispanic Asian',
                                   Father.s.Hispanic.Origin=='Not Hispanic or Latino' & Father.s.Single.Race.6=='More than one race'~'Non-Hispanic More than one race',
                                   Father.s.Hispanic.Origin=='Not Hispanic or Latino' & Father.s.Single.Race.6=='Black or African American'~'Non-Hispanic Black',
                                   Father.s.Hispanic.Origin=='Not Hispanic or Latino' & Father.s.Single.Race.6=='White'~'Non-Hispanic White',
                                   TRUE~'Unknown'))
  data_fertility$age.cat <- data_fertility$age
  data_fertility <- subset(data_fertility,select = c('State.of.Residence','Year','age','age.cat',
                                                     'Father.s.Hispanic.Origin','race.eth','Births'))
  setnames(data_fertility,c('State.of.Residence','Year','Father.s.Hispanic.Origin','Births'),
           c('state','year','hispanic','births'))
  data_fertility[, gender:='Male']
  
  data_fertility = as.data.table(data_fertility)
  data_fertility$age = as.character(data_fertility$age)
  data_fertility$age = gsub(' - ', '-', data_fertility$age)
  data_fertility$age = gsub(' [+]', '+', data_fertility$age)
  data_fertility$age = gsub('Under ', '0-', data_fertility$age)
  data_fertility$age = gsub(' and over', '+', data_fertility$age)
  data_fertility$age = gsub(' and older', '+', data_fertility$age)
  # assume male are able to birth from 15 years old
  data_fertility$age = ifelse(data_fertility$age == '0-19', '15-19', data_fertility$age)
  data_fertility$year = as.numeric(as.character(data_fertility$year))
  data_fertility$births = as.numeric(as.character(data_fertility$births))
  data_fertility$births[is.na(data_fertility$births)] <- 0
  
  data_fertility = data_fertility %>% filter(age != '0-15') %>% arrange(year, age)
  data_fertility = as.data.table(data_fertility)
  data_fertility <- subset(data_fertility,!is.na(year))
  data_fertility <- data_fertility[, list(births=sum(births)),by=c('state','year','age.cat','race.eth','gender')]
  data_combine= merge(data_fertility, data_pop_m_agec, by = c('state','year','age.cat','race.eth'),all.x=T)
  setnames(data_combine,'age.cat','age')
  if(rep!=000){
    samp <- rpois(length(data_combine[!is.na(births),births]),lambda=data_combine[!is.na(births),births])
    data_combine[!is.na(births), births:=samp]
  }
  data_combine[,fertility_rate := births / (population) * 1000]
  data_combine <- subset(data_combine,age!='Unknown')
  # live births per 1000 men
  write.csv(file = 'data/fertility/usa_states_fertility_m.csv', data_combine)
 
  
  #### use relationship between year and fertility for women to obtain historical fertility of men
  fert_m = as.data.table(read.csv('data/fertility/usa_states_fertility_m.csv'))
  fert_m <- subset(fert_m,age!='0-14')
  
  # read female data
  fert_f = as.data.table(read.csv('data/fertility/usa_states_fertility_f.csv'))
  fert_f <- subset(fert_f,age!='0-14')
  
  # drop obs which are missing from males & females for some strata to fit model with gender predictor
  fert <- merge(fert_m,fert_f,by=c('state','age','year','race.eth','gender','births','population','fertility_rate'),all=T)
  fert <- subset(fert,!is.na(births) & !is.na(population))
  counts <- fert[, list(nobs=length(births)),by=c('state','age','race.eth')]
  
  # drop strata not in female data
  fert[, flag:=1]
  ss <- subset(fert,gender=='Female' & !is.na(births) & !is.na(population),select=c('state','age','race.eth','flag'))
  ss <- unique(ss)
  fert[, flag:=NULL]
  fert <- merge(fert,ss,by=c('state','age','race.eth'),all=T)
  fert2 <- subset(fert,!is.na(flag))
  fert2[, flag:=NULL]
  
  # drop strata not in male data
  fert2[, flag:=1]
  ss <- subset(fert2,gender=='Male' & !is.na(births) & !is.na(population),select=c('state','age','race.eth','flag'))
  ss <- unique(ss)
  fert2[, flag:=NULL]
  fert2 <- merge(fert2,ss,by=c('state','age','race.eth'),all=T)
  fert2 <- subset(fert2,!is.na(flag))
  
  # fix gender levels for predictions
  fert2$gender <- factor(fert2$gender,levels=c('Male','Female'))
  
  # fit poisson glm
  dt <- fert2[,list(intercept=summary(glm(births~year+ gender+offset(log(population+1)), family = poisson(link = "log"),maxit=100))$coefficients[1],
                    yearhat=summary(glm(births~year+ gender+offset(log(population+1)), family = poisson(link = "log"),maxit=100))$coefficients[2],
                    sexhat=summary(glm(births~year+ gender+offset(log(population+1)), family = poisson(link = "log"),maxit=100))$coefficients[3]),by=c('age','race.eth','state')]

  # model all states by race/ethnicity/age where not enough observations at state-level
  fert[, flag:=1]
  ss <- subset(fert,gender=='Female' & !is.na(births) & !is.na(population),select=c('age','race.eth','flag'))
  ss <- unique(ss)
  fert[, flag:=NULL]
  fert <- merge(fert,ss,by=c('age','race.eth'),all=T)
  fert2 <- subset(fert,!is.na(flag))
  fert2[, flag:=NULL]
  
  fert2[, flag:=1]
  ss <- subset(fert2,gender=='Male' & !is.na(births) & !is.na(population),select=c('age','race.eth','flag'))
  ss <- unique(ss)
  fert2[, flag:=NULL]
  fert2 <- merge(fert2,ss,by=c('age','race.eth'),all=T)
  fert2 <- subset(fert2,!is.na(flag))
  fert2 <- subset(fert2,!is.na(births) & !is.na(population))
  
  dav <- fert2[,list(meanintercept=summary(glm(births~year+ gender+offset(log(population+1)), family = poisson(link = "log"),maxit=100))$coefficients[1],
                     meanyearhat=summary(glm(births~year+ gender+offset(log(population+1)), family = poisson(link = "log"),maxit=100))$coefficients[2],
                     meansexhat=summary(glm(births~year+ gender+offset(log(population+1)), family = poisson(link = "log"),maxit=100))$coefficients[3]),by=c('age','race.eth')]
  do <- as.data.table(tidyr::crossing(state=unique(dt$state),
                                      age=unique(dt$age),race.eth=unique(dt$race.eth)))
  dt <- merge(dt,do,by=c('state','age','race.eth'),all=T)
  
  dt <- merge(dt,dav,by=c('age','race.eth'),all=T)
  dt <- merge(dt,counts,by=c('state','age','race.eth'),all.x=T)
  # any that are missing, or with fewer than 10 observations to fit model use national coefficients
  dt[is.na(intercept) | nobs<10,intercept:=meanintercept]
  dt[is.na(yearhat) | nobs<10,yearhat:=meanyearhat]
  dt[is.na(sexhat) | nobs<10,sexhat:=meansexhat]
  set(dt,NULL,c('meanyearhat','meanintercept','meansexhat'),NULL)
  
  ## impute female missing
  fert_tmp <- list()
  # gender coefficient for females
  for (i in seq(2003,2019)) {
    tmp = data.table(dt)
    tmp[, year:=i]
    tmp[, fertility_rate_imp:=exp(intercept + yearhat*i + sexhat)*1000]
    fert_tmp[[as.character(i)]] <- tmp
  }
  fert_tmp <- do.call(rbind,fert_tmp)
  fert_f = merge(fert_tmp,fert_f,by=c('state','age','year','race.eth'),all=T)
  fert_f[is.na(births) | is.na(population), fertility_rate:=fertility_rate_imp]
  fert_f[,gender:='Female']
  set(fert_f, NULL, c('intercept','yearhat','sexhat','nobs','fertility_rate_imp'), NULL)
  fert_f = fert_f %>% arrange(year, age)
  fert_f <- subset(fert_f,age!='0-14' & age!= '50-54' & race.eth!='Non-Hispanic More than one race' & race.eth!='Unknown')
  write.csv(file = 'data/fertility/usa_states_fertility_f.csv', fert_f)
  
  ## impute men
  # 50+ men - fit model to male data 2016-2018 (across states to ensure sufficient observations)
  fert2 <- subset(fert_m,!is.na(births) & !is.na(population))
  fifty <- fert2[gender=='Male' & age %in% c('50-54','55+') & race.eth!='Unknown',list(intercept=summary(glm(births~year+offset(log(population+1)), family = poisson(link = "log"),maxit=100))$coefficients[1],
                                                                                        yearhat=summary(glm(births~year+ offset(log(population+1)), family = poisson(link = "log"),maxit=100))$coefficients[2]),
                                                                                       by=c('age','race.eth')]
  do <- as.data.table(tidyr::crossing(state=unique(dt$state),
                                      age=droplevels(unique(as.factor(fifty$age))),race.eth=unique(as.factor(fifty$race.eth))))
  fifty <- merge(fifty,do,by=c('age','race.eth'),all=T)
  
  dt <- merge(subset(dt,! age %in% c('50-54','50+','55+')),fifty,by=c('state','race.eth','age','intercept','yearhat'),all=T)
  
  fert_tmp <- list()
  for (i in seq(2003,2019)) {
    tmp = data.table(dt)
    tmp[, year:=i]
    tmp[, fertility_rate_imp:=exp(intercept + yearhat*i)*1000]
    fert_tmp[[as.character(i)]] <- tmp
  }
  fert_tmp <- do.call(rbind,fert_tmp)
  fert_m = merge(fert_tmp,fert_m,by=c('state','age','year','race.eth'),all=T)
  fert_m[is.na(fertility_rate), fertility_rate:=fertility_rate_imp]
  
  fert_m[,gender:='Male']
  set(fert_m, NULL, c('births','population'), NULL)
  fert_m = fert_m %>% arrange(year, age)
  fert_m <- subset(fert_m,age!='0-14' & race.eth!='Non-Hispanic More than one race' & race.eth!='Unknown')
  write.csv(file = 'data/fertility/usa_states_fertility_m_all.csv', fert_m)
  
}
