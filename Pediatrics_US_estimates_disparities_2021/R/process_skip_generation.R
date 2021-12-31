# Code to process skip generation proportions
library(readxl)

# USA
process_usa_bystate_skip_generation = function(country,s,r){
  #country = 'usa'
  d_summary = read.csv('data/usa_states.csv')
  d_summary <- subset(d_summary,State==s & Race.and.Hispanic.Origin.Group==r)
  #setnames(d_summary, c('death'), c('deaths'))
  d_summary = d_summary %>% group_by(age, gender) %>% mutate(nb_deaths = sum(deaths)) %>% ungroup() %>% 
    select(age, gender, nb_deaths) %>% distinct()
  setnames(d_summary, 'nb_deaths', 'deaths')
  d_summary$age = as.character(d_summary$age)
  
  d_summary_30 = d_summary %>% filter( age %in% c('25-34'))
  d_summary_30$age = '30-34'
  d_summary_30$deaths = d_summary_30$deaths/2
  
  d_summary = rbind(d_summary, d_summary_30)
  
  d_summary$age = ifelse(d_summary$age %in% c("30-49","50-64",'65-74', "75-84","85+" ),'30+', 'others')
  d_summary = d_summary[order(d_summary$gender),]
  
  d_summary <- data.table(d_summary)
  data <- d_summary[, list(grand_deaths=sum(deaths)),by=c('age','gender')]
  
  ## read in grandparent data
  dat <- read.csv('data/ACSST5Y2019.S1002_grandparentdata_2021-03-24T054217.csv',header = T,stringsAsFactors = F)
  vars <- read.csv('data/grandparents_variables.csv',stringsAsFactors = F)
  
  pc <- subset(vars,group!='' & category=='primary caregiver')
  cr <- subset(vars,group!='' & category=='coresident')
  
  dg <- dat[,c('NAME',pc$GEO_ID)]
  colnames(dg) <- c('state',pc$group)
  dg <- subset(dg, state!='Geographic Area Name')
  dg <- data.table(reshape2::melt(dg,id.vars=c('state','Total_pc','Male','Female','sg'),
                                  variable.name='race.eth',value.name='prop',na.rm=F))
  hisp <- subset(dg,race.eth=='Hispanic',select=c('state','prop'))
  setnames(hisp,'prop','Hispanic')
  dg <- merge(dg, hisp,by=c('state'),all.x=T)
  
  dg[, cat:='primary caregiver']
  dg[, Total_pc:=as.numeric(Total_pc)]
  dg[, Male:=as.numeric(Male)]
  dg[, Female:=as.numeric(Female)]
  dg[, sg:=as.numeric(sg)]
  dg[, prop:=as.numeric(prop)] # prop of each race
  dg[, Hispanic:=as.numeric(Hispanic)] # prop hispanic
  dg[race.eth!='Hispanic',prop:=(prop/100)*(1-(Hispanic/100))*100]
  dg[, sg_female:=Total_pc*(sg/100)*(Female/100)*(prop/100)]
  dg[, sg_male:=Total_pc*(sg/100)*(Male/100)*(prop/100)]
  dg[, mg_female:=Total_pc*(1-(sg/100))*(Female/100)*(prop/100)]
  dg[, mg_male:=Total_pc*(1-(sg/100))*(Male/100)*(prop/100)]
  
  
  tmp <- dat[,c('NAME',cr$GEO_ID)]
  colnames(tmp) <- c('state',cr$group)
  tmp <- subset(tmp, state!='Geographic Area Name')
  tmp <- data.table(reshape2::melt(tmp,id.vars=c('state','Total_cr','Male','Female'),
                                   variable.name='race.eth',value.name='prop',na.rm=F))
  hisp <- subset(tmp,race.eth=='Hispanic',select=c('state','prop'))
  setnames(hisp,'prop','Hispanic')
  tmp <- merge(tmp, hisp,by=c('state'),all.x=T)
  
  tmp[, cat:='coresident']
  tmp[, Total_cr:=as.numeric(Total_cr)]
  tmp[, Male:=as.numeric(Male)]
  tmp[, Female:=as.numeric(Female)]
  tmp[, prop:=as.numeric(prop)]
  tmp[, Hispanic:=as.numeric(Hispanic)]
  tmp[race.eth!='Hispanic',prop:=(prop/100)*(1-(Hispanic/100))*100]
  
  dg <- merge(tmp,subset(dg,select=c('state','race.eth','sg_female','sg_male','mg_female','mg_male')),by=c('state','race.eth'),all=T)
  
  dg[prop==0, prop:=0.001]
  dg[, cr_female:=(Total_cr*(Female/100)*(prop/100)) - sg_female - mg_female]
  dg[, cr_male:=(Total_cr*(Male/100)*(prop/100)) - sg_male - mg_male]
  
  #dg[cr_female<0, cr_female:=(Total_cr*(Female/100)*(0.1/100)) - sg_female - mg_female]
  #dg[cr_male<0, cr_male:=(Total_cr*(Male/100)*(0.1/100)) - sg_male - mg_male]
  
  dg[cr_female<0, cr_female:= 0]
  dg[cr_male<0, cr_male:= 0]
  
  dg[, age:='30+']
  
  # add Native Hawaiian/PI to NH Asian
  dg[race.eth=='Non-Hispanic Native Hawaiian or Other Pacific Islander',race.eth:='Non-Hispanic Asian']
  dg <- dg[, list(sg_female=sum(sg_female,na.rm=T),sg_male=sum(sg_male,na.rm=T),
                  mg_female=sum(mg_female,na.rm=T),mg_male=sum(mg_male,na.rm=T),
                  cr_female=sum(cr_female,na.rm=T),cr_male=sum(cr_male,na.rm=T)),by=c('state','race.eth','cat','age')]

  
  # get population over 30
  # for men
  data_pop_m = read.delim('data/pop/pop_m_2018-2019.txt',header = TRUE, sep = "\t")
  data_pop_m <- data_pop_m[!is.na(data_pop_m$States.Code),]
  data_pop_m <- subset(data_pop_m,Yearly.July.1st.Estimates==2019)
  
  data_pop_m <- data_pop_m %>%
    mutate(age:= case_when(Five.Year.Age.Groups.Code %in% c('30-34','35-39','40-44', '45-49','50-54','55-59','60-69','70-74','75-79','80-84','85+') ~ '30+',
                           TRUE ~'Under 30'),
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
  data_pop_m_agec <- data_pop_m[, list(population_m=sum(population)),by=c('state',
                                                                          'age', 'race.eth')]
  
  # women
  data_pop_f = read.delim('data/pop/pop_f_2019_singlerace.txt',header = TRUE, sep = "\t")
  data_pop_f <- data_pop_f[!is.na(data_pop_f$States.Code),]
  
  data_pop_f <- data_pop_f %>%
    mutate(age:= case_when(Five.Year.Age.Groups.Code %in% c('30-34','35-39','40-44', '45-49','50-54','55-59','60-69','70-74','75-79','80-84','85+') ~ '30+',
                           TRUE ~'Under 30'),
           race.eth:= case_when(Ethnicity=='Hispanic or Latino'~'Hispanic',
                                Ethnicity=='Not Hispanic or Latino' & Race=='American Indian or Alaska Native'~'Non-Hispanic American Indian or Alaska Native',
                                Ethnicity=='Not Hispanic or Latino' & Race=='Asian'~'Non-Hispanic Asian',
                                Ethnicity=='Not Hispanic or Latino' & Race=='Native Hawaiian or Other Pacific Islander'~'Non-Hispanic Asian',
                                Ethnicity=='Not Hispanic or Latino' & Race=='More than one race'~'Non-Hispanic More than one race',
                                Ethnicity=='Not Hispanic or Latino' & Race=='Black or African American'~'Non-Hispanic Black',
                                Ethnicity=='Not Hispanic or Latino' & Race=='White'~'Non-Hispanic White',
                                TRUE~'Unknown'))
  data_pop_f <- data.table(data_pop_f)
  setnames(data_pop_f,c('States','Race','Ethnicity','Population'),c('state','race','hispanic','population'))
  data_pop_f_agec <- data_pop_f[, list(population_f=sum(population)),by=c('state',
                                                                          'age', 'race.eth')]
  
  # merge with grandparent data
  dg <- merge(dg, subset(data_pop_f_agec,age %in% c('30+')),by=c('state','race.eth','age'),all.x=T)
  dg <- merge(dg, subset(data_pop_m_agec,age %in% c('30+')),by=c('state','race.eth','age'),all.x=T)
  dg[, sg_female:=sg_female/population_f]
  dg[, sg_male:=sg_male/population_m]
  dg[, mg_female:=mg_female/population_f]
  dg[, mg_male:=mg_male/population_m]
  dg[, cr_female:=cr_female/population_f]
  dg[, cr_male:=cr_male/population_m]
  
  gen = dg[which(state == s & race.eth==r),]
  skip_gen <- subset(gen,select=c('age','sg_female','sg_male'))
  skip_gen <- data.table(reshape2::melt(skip_gen,id.vars=c('age'),
                                        variable.name='gender',value.name='sg',na.rm=F))
  skip_gen[gender=='sg_female',gender:='Female']
  skip_gen[gender=='sg_male',gender:='Male']
  multi_gen <- subset(gen,select=c('age','mg_female','mg_male'))
  multi_gen <- data.table(reshape2::melt(multi_gen,id.vars=c('age'),
                                         variable.name='gender',value.name='mg',na.rm=F))
  multi_gen[gender=='mg_female',gender:='Female']
  multi_gen[gender=='mg_male',gender:='Male']
  cores <- subset(gen,select=c('age','cr_female','cr_male'))
  cores <- data.table(reshape2::melt(cores,id.vars=c('age'),
                                         variable.name='gender',value.name='cr',na.rm=F))
  cores[gender=='cr_female',gender:='Female']
  cores[gender=='cr_male',gender:='Male']
  
  data <- merge(data, skip_gen,by=c('age','gender'))
  data <- merge(data, multi_gen,by=c('age','gender'))
  data <- merge(data, cores,by=c('age','gender'))
  
  data[,skip_generation:=sg*100]
  data[,value:= grand_deaths * skip_generation/100]
  data[,coresiding_caregiver:=mg*100]
  data[,value_cc:= grand_deaths * coresiding_caregiver/100]
  data[,'older persons co-residing':= cr*100]
  data[,number:= `older persons co-residing` * grand_deaths/100]
  data[,number:= round(number)]
  data[,grand_deaths:= round(grand_deaths)]
  data[,value:= round(value)]
  data[,value_cc:= round(value_cc)]
  data[,sg:=NULL]
  data[,mg:=NULL]
  data[,cr:=NULL]
  write_csv(path = paste0('data/fertility/skip_generation_', "usa","_",gsub(' ','',s),"_",gsub(' ','',r),'.csv'), data)
  
  print(data)
}


