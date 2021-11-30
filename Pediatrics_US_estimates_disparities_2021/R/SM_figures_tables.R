library(ggsci)
require(data.table)
`%notin%` <- Negate(`%in%`)

# Table all states

home <- args$source_dir
orphans <- readRDS("orphans_usa_allstates_0.RDS")
codes <- data.table(read.csv('data/USA/state_codes.csv'))

do <- data.table(orphans)
do <- subset(do,race.eth!='Unknown')
do <- data.table(state=do$state,race.eth=do$race.eth,orphans=do$primary_loss,primary_loss=do$primary_loss)
st <- do[, list(orphans=sum(orphans)),by=c('state','race.eth')]
do <- do[, list(orphans=sum(orphans)),by=c('state')]

variable <- "orphans"
tmp <- do
tmp = tmp[order(get(variable))]
states = unique(tmp$state)

st$state <- factor(st$state,levels=states)

ans <- dcast(st,state~race.eth,value.var='orphans')
ans <- ans[order(-state),]
ans[, Total:=rowSums(ans[,2:ncol(ans)])]

ans <- ans[, c('state','Hispanic','Non-Hispanic American Indian or Alaska Native',
               'Non-Hispanic Asian','Non-Hispanic Black','Non-Hispanic White','Total')]
ans[`Non-Hispanic Asian`<0,`Non-Hispanic Asian`:=0]

saveRDS(ans,file='tables/SM_table_states_primary_caregivers.RDS')

# rates of orphanhood

# load population data
pop_m <- read.csv(file = 'data/USA/pop/usa_states_population_m.csv')
pop_f <- read.csv(file = 'data/USA/pop/usa_states_population_f.csv')

pop_m <- subset(pop_m,year==2019)
pop_f <- subset(pop_f,year==2019)
pop <- data.table(rbind(pop_m,pop_f))

pop <- subset(pop,race.eth %notin% c('Non-Hispanic Native Hawaiian or Other Pacific Islander','Non-Hispanic More than one race','Unknown'))
pop_a <- subset(pop,age.cat!='0-14')

pop_t <- pop_a[,list(pop_t=sum(population)),by=c('state')]
pop_r <- pop_a[,list(pop_r=sum(population)),by=c('state','race.eth')]
pop_r_us <- pop_a[,list(pop_r=sum(population)),by=c('race.eth')]
# get population under 18

pop_by_age <- data.table(read.csv('data/USA/pop_US_1y.csv'))
pop_by_age[, pop:=Population*1000]
pop_min <- sum(pop_by_age$pop[pop_by_age$Age<=17])

pop_kids <- subset(pop,age.cat %in% c('0-14','15-19'))
pop_kids <- pop_kids[, list(pop=sum(population)),by=c('state')]
pop_kids[, ratio:= pop/sum(pop)]
pop_kids[, pop_kids:=round(ratio*pop_min,0)]


do <- merge(do,subset(pop_kids,select=c('state','pop_kids')),by=c('state'),all=T)
st <- merge(st,subset(pop_kids,select=c('state','pop_kids')),by=c('state'),all=T)

do[, Total:=round(orphans/pop_kids*100000,0)]
st[, orphans1e5:=round(orphans/pop_kids*100000,0)]

variable <- "Total"
tmp <- do
tmp = tmp[order(get(variable))]
states = unique(tmp$state)

st$state <- factor(st$state,levels=states)

ans <- dcast(st,state~race.eth,value.var='orphans1e5')
ans <- merge(ans,subset(do,select=c('state','Total')),by=c('state'),all=T)
ans <- ans[order(-state),]
ans[, Total:=round(Total,0)]

ans <- ans[, c('state','Hispanic','Non-Hispanic American Indian or Alaska Native',
               'Non-Hispanic Asian','Non-Hispanic Black','Non-Hispanic White','Total')]
ans[`Non-Hispanic Asian`<0,`Non-Hispanic Asian`:=0]
saveRDS(ans,file='tables/SM_table_states_primary_caregivers_rates.RDS')


########
## Plots of mortality data

d_deaths <- data.table(read.csv('data/USA/usa_states.csv'))

dat <- d_deaths[,list(excess_deaths=sum(deaths),covid_deaths=sum(covid_deaths)),by=c('age','gender')]
dat <- melt(dat,id.vars=c('age','gender'))
dat$variable <- factor(dat$variable,levels=c('covid_deaths','excess_deaths'),labels=c('Covid-19 deaths','Excess deaths'))
ggplot(data=subset(dat,age!='Not stated' & variable=='Excess deaths')) + 
	geom_bar(aes(x=age,y=value,fill=gender),stat='identity', position = "dodge") +
	scale_y_continuous(labels = comma) +
	facet_grid(.~gender,scale="free") +
	theme_bw(base_size=30) +
	theme(strip.background = element_blank(),
				legend.position="bottom") +
	labs(x='Age group',y='COVID-19 associated deaths',fill='')
ggsave('figures/quarterly_excess_deaths_data_gender.png',width=18,height=10)


dat <- d_deaths[,list(excess_deaths=sum(deaths),covid_deaths=sum(covid_deaths)),by=c('age','gender','Race.and.Hispanic.Origin.Group')]
dat <- melt(dat,id.vars=c('age','gender','Race.and.Hispanic.Origin.Group'))
dat$variable <- factor(dat$variable,levels=c('covid_deaths','excess_deaths'),labels=c('Covid-19 deaths','Excess deaths'))
dat[Race.and.Hispanic.Origin.Group=='Non-Hispanic American Indian or Alaska Native', Race.and.Hispanic.Origin.Group:= 'Non-Hispanic American Indian \nor Alaska Native']
ggplot(data=subset(dat,age!='Not stated' & Race.and.Hispanic.Origin.Group!='Other' & variable=='Excess deaths')) + 
	geom_bar(aes(x=age,y=value,fill=gender),stat='identity', position = "dodge") +
	scale_y_continuous(labels = comma) +
	facet_grid(Race.and.Hispanic.Origin.Group~gender,scale="free") +
	theme_bw(base_size=30) +
	theme(strip.background = element_blank(),
				legend.position="bottom") +
	labs(x='Age group',y='COVID-19 associated deaths',fill='')
ggsave('figures/quarterly_excess_deaths_data_byrace.png',width=25,height=32)


# summarise population data by age groupings
data_pop_f = read.delim('data/USA/pop/pop_f_2003-2006.txt',header = TRUE, sep = "\t")

indir.pop <- file.path('data','USA','pop')
infiles <- data.table(F=list.files(indir.pop, pattern='pop_f', full.names=TRUE, recursive=TRUE))

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
														 Age.Group.Code %in% c('15-19','20-24','25-29') ~ '15-29',
														 Age.Group.Code %in% c('30-34','35-39','40-44','45-49') ~ '30-49',
														 Age.Group.Code %in% c('50-54','55-59','60-64') ~ '50-64',
														 Age.Group.Code %in% c('65-69','70-74') ~ '65-74',
														 Age.Group.Code %in% c('75-79','80-84') ~ '75-84',
														 Age.Group.Code %in% c('85+') ~ '85+',
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
data_pop_f$gender <- 'female'
#####
data_pop_m = read.delim('data/USA/pop/pop_m_2016-2018.txt',header = TRUE, sep = "\t")

indir.pop <- file.path('data','USA','pop')
infiles <- data.table(F=list.files(indir.pop, pattern='pop_m', full.names=TRUE, recursive=FALSE))

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
														 Five.Year.Age.Groups.Code %in% c('15-19','20-24','25-29') ~ '15-29',
														 Five.Year.Age.Groups.Code %in% c('30-34','35-39','40-44','45-49') ~ '30-49',
														 Five.Year.Age.Groups.Code %in% c('50-54','55-59','60-64') ~ '50-64',
														 Five.Year.Age.Groups.Code %in% c('65-69','70-74') ~ '65-74',
														 Five.Year.Age.Groups.Code %in% c('75-79','80-84') ~ '75-84',
														 Five.Year.Age.Groups.Code %in% c('85+') ~ '85+',
														 TRUE ~'unknown'),
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
data_pop_m$gender <- 'male'

data_pop_m <- subset(data_pop_m,select=c('state','year','age.cat','gender','race.eth','population'))
data_pop_f <- subset(data_pop_f,select=c('state','year','age.cat','gender','race.eth','population'))
data <- merge(data_pop_m,data_pop_f,by=c('state','year','age.cat','gender','race.eth','population'),all=T)
data <- subset(data,year==2019 & age.cat!='0-14')


pop <- data[, list(pop=sum(population)),by=c('age.cat','gender')]
pop[gender=='female',gender:='Female']
pop[gender=='male',gender:='Male']
setnames(pop,'age.cat','age')

d_deaths <- subset(d_deaths,Race.and.Hispanic.Origin.Group!='Other' & Race.and.Hispanic.Origin.Group!='Non-Hispanic More than one race')
dat <- d_deaths[,list(excess_deaths=sum(deaths)),by=c('age','gender')]

dat <- merge(dat,pop,by=c('age','gender'),all=T)

dat[, deaths1e5:=excess_deaths/pop*100000]

dat <- melt(dat,id.vars=c('age','gender'))
dat$variable <- factor(dat$variable,levels=c('covid_deaths','excess_deaths','deaths1e5'),labels=c('Covid-19 deaths','Excess deaths','Excess deaths per 100k'))
ggplot(data=subset(dat,age!='Not stated' & variable=='Excess deaths per 100k')) + 
	geom_bar(aes(x=age,y=value,fill=gender),stat='identity', position = "dodge") +
	scale_y_continuous(labels = comma) +
	facet_grid(.~gender,scale="free") +
	theme_bw(base_size=30) +
	theme(strip.background = element_blank(),
				legend.position="bottom") +
	labs(x='Age group',y='COVID-19 associated deaths per 100k',fill='')
ggsave('figures/quarterly_excess_death_rate_data_gender.png',width=18,height=10)


pop <- data[, list(pop=sum(population)),by=c('age.cat','race.eth','gender')]
pop[gender=='female',gender:='Female']
pop[gender=='male',gender:='Male']
setnames(pop,'age.cat','age')

d_deaths <- subset(d_deaths,Race.and.Hispanic.Origin.Group!='Other' & Race.and.Hispanic.Origin.Group!='Non-Hispanic More than one race')
dat <- d_deaths[,list(excess_deaths=sum(deaths)),by=c('age','gender','Race.and.Hispanic.Origin.Group')]

setnames(dat,c('Race.and.Hispanic.Origin.Group'),c('race.eth'))
dat <- merge(dat,pop,by=c('age','gender','race.eth'),all=T)

dat[, deaths1e5:=excess_deaths/pop*100000]

dat <- melt(dat,id.vars=c('age','gender','race.eth'))
dat[race.eth=='Non-Hispanic American Indian or Alaska Native', race.eth:= 'Non-Hispanic American Indian \nor Alaska Native']
dat$race.eth=factor(dat$race.eth,levels=c('Hispanic','Non-Hispanic Asian','Non-Hispanic Black','Non-Hispanic White','Non-Hispanic American Indian \nor Alaska Native'))
ggplot(data=subset(dat,age!='Not stated' & race.eth!='Other' & race.eth!='Non-Hispanic More than one race' & variable=='deaths1e5')) + 
	geom_bar(aes(x=age,y=value,fill=gender),stat='identity', position = "dodge") +
	scale_y_continuous(labels = comma) +
	facet_grid(race.eth~gender,scale="free") +
	theme_bw(base_size=30) +
	theme(strip.background = element_blank(),
				legend.position="bottom") +
	labs(x='Age group',y='COVID-19 associated deaths per 100k',fill='')
ggsave('figures/quarterly_excess_death_rate_data_byrace.png',width=25,height=32)



####
# number of children plots

country <- 'usa_Arizona_Non-HispanicWhite'
ddf = read.csv(paste0('data/USA/', country,'_children_f.csv'))
ddf_2 = read.csv(paste0('data/USA/', country,'_children_m.csv'))
# truncate fert for men for usa states analysis
if(grepl('usa_',country)) ddf_2$children[ddf_2$age>77] <- 0
dat1 = data.table(rbind(ddf, ddf_2))
dat1[, race.eth:='Non-Hispanic White']

country <- 'usa_Arizona_Hispanic'
ddf = read.csv(paste0('data/USA/', country,'_children_f.csv'))
ddf_2 = read.csv(paste0('data/USA/', country,'_children_m.csv'))
# truncate fert for men for usa states analysis
if(grepl('usa_',country)) ddf_2$children[ddf_2$age>77] <- 0
dat2 = data.table(rbind(ddf, ddf_2))
dat2[, race.eth:='Hispanic']

country <- 'usa_Arizona_Non-HispanicBlack'
ddf = read.csv(paste0('data/USA/', country,'_children_f.csv'))
ddf_2 = read.csv(paste0('data/USA/', country,'_children_m.csv'))
# truncate fert for men for usa states analysis
if(grepl('usa_',country)) ddf_2$children[ddf_2$age>77] <- 0
dat3 = data.table(rbind(ddf, ddf_2))
dat3[, race.eth:='Non-Hispanic Black']

country <- 'usa_Arizona_Non-HispanicAsian'
ddf = read.csv(paste0('data/USA/', country,'_children_f.csv'))
ddf_2 = read.csv(paste0('data/USA/', country,'_children_m.csv'))
# truncate fert for men for usa states analysis
if(grepl('usa_',country)) ddf_2$children[ddf_2$age>77] <- 0
dat4 = data.table(rbind(ddf, ddf_2))
dat4[, race.eth:='Non-Hispanic Asian']

country <- 'usa_Arizona_Non-HispanicAmericanIndianorAlaskaNative'
ddf = read.csv(paste0('data/USA/', country,'_children_f.csv'))
ddf_2 = read.csv(paste0('data/USA/', country,'_children_m.csv'))
# truncate fert for men for usa states analysis
if(grepl('usa_',country)) ddf_2$children[ddf_2$age>77] <- 0
dat5 = data.table(rbind(ddf, ddf_2))
dat5[, race.eth:='Non-Hispanic American Indian or Alaska Native']

ddf <- rbind(dat1,dat2)
p = ggplot(ddf) +
	geom_point(aes(x = age, y = children, colour = gender)) + 
	theme_bw()+
	facet_grid(.~race.eth) +
	xlab( 'Age of Parent') +
	ylab('Number of Children')+
	guides(color=guide_legend(title="Sex of Parent")) +
	theme(strip.background = element_blank())
ggsave(filename = 'figures/number_children_AZ.png', p, width = 10, height = 5)


ddf <- rbind(dat1,dat2,dat3,dat4,dat5)
ddf[race.eth=='Non-Hispanic American Indian or Alaska Native',race.eth:='Non-Hispanic American Indian \nor Alaska Native']
p = ggplot(ddf) +
	geom_point(aes(x = age, y = children, colour = gender)) + 
	theme_bw(base_size=30)+
	facet_wrap(.~race.eth,ncol=2) +
	xlab( 'Age of Parent') +
	ylab('Number of Children')+
	guides(color=guide_legend(title="Sex of Parent")) +
	theme(strip.background = element_blank())
ggsave(filename = 'figures/number_children_AZ_allraces.png', p, width = 16, height = 19)

####
# grandparents
## read in grandparent data
dat <- read.csv('data/USA/ACSST5Y2019.S1002_grandparentdata_2021-03-24T054217.csv',header = T,stringsAsFactors = F)
vars <- read.csv('data/USA/grandparents_variables.csv',stringsAsFactors = F)

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
data_pop_m = read.delim('data/USA/pop/pop_m_2018-2019.txt',header = TRUE, sep = "\t")
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
data_pop_f = read.delim('data/USA/pop_f_2019_singlerace.txt',header = TRUE, sep = "\t")
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
dg <- subset(dg, state!='Puerto Rico' & race.eth!='Unknown')

dat <- dg[, list(sg_female=sum(sg_female),sg_male=sum(sg_male),mg_female=sum(mg_female),mg_male=sum(mg_male),cr_female=sum(cr_female),
								 cr_male=sum(cr_male),pop_f=sum(population_f),pop_m=sum(population_m)),by=c('race.eth')]

dat[, sg_female:=sg_female/pop_f]
dat[, sg_male:=sg_male/pop_m]
dat[, mg_female:=mg_female/pop_f]
dat[, mg_male:=mg_male/pop_m]
dat[, cr_female:=cr_female/pop_f]
dat[, cr_male:=cr_male/pop_m]
dat[, pop_f:=NULL]
dat[, pop_m:=NULL]

dat2 <- melt(dat,id.vars=c('race.eth'))
dat2[grepl('_female',variable), gender:='Female']
dat2[grepl('_male',variable), gender:='Male']
dat2[, variable:=gsub('_male','',variable)]
dat2[, variable:=gsub('_female','',variable)]
dat2[variable=='sg', gp:='Custodial grandparent']
dat2[variable=='mg', gp:='Co-residing grandparent (primary care)']
dat2[variable=='cr', gp:='Co-residing grandparent (secondary care)']

dat2$gp <- factor(dat2$gp,levels=c('Custodial grandparent','Co-residing grandparent (primary care)','Co-residing grandparent (secondary care)'))
dat2[race.eth=='Non-Hispanic American Indian or Alaska Native',race.eth:='Non-Hispanic American Indian \nor Alaska Native']
dat2$race.eth <- factor(dat2$race.eth,levels=c("Hispanic", "Non-Hispanic American Indian \nor Alaska Native",
																							 "Non-Hispanic Black"  ,  "Non-Hispanic Asian",                        
																							 "Non-Hispanic White","Non-Hispanic More than one race"))
ggplot(data=subset(dat2, race.eth!='Other' & race.eth!='Non-Hispanic More than one race')) + 
	geom_bar(stat='identity',aes(x=race.eth,y=value,fill=gp)) +
	scale_y_continuous(labels = percent) +
	facet_grid(.~gender,scale="free") +
	theme_bw(base_size=30) +
	theme(strip.background = element_blank(),
				legend.position="bottom",
				axis.text.x = element_text(angle=60, vjust = 0.5, hjust=0.5)) +
	labs(x='Age group',y='Population who are \ngrandparent caregivers',fill='') +
	scale_fill_nejm()
ggsave('figures/grandparents_byrace.png',width=25,height=18)



#### plot population structure
# load population data
pop_m <- data.table(read.csv(file = 'data/USA/pop/usa_states_population_m.csv'))
pop_f <- data.table(read.csv(file = 'data/USA/pop/usa_states_population_f.csv'))

pop_m <- subset(pop_m,year==2019)
pop_f <- subset(pop_f,year==2019)
pop_m[, gender:='Male']
pop_f[, gender:='Female']
#pop_f[race.eth=='Non-Hispanic Native Hawaiian or Other Pacific Islander', race.eth:='Non-Hispanic Asian']
pop <- data.table(rbind(pop_m,pop_f))

####

# summarise population data by age groupings
data_pop_f = read.delim('data/USA/pop/pop_f_2003-2006.txt',header = TRUE, sep = "\t")

indir.pop <- file.path('data','USA','pop')
infiles <- data.table(F=list.files(indir.pop, pattern='pop_f', full.names=TRUE, recursive=TRUE))

for(i in 2:nrow(infiles)){
	infile <- infiles[i,F]
	cat('Process',infile,'\n')
	tmp <- read.delim(infile,header = TRUE, sep = "\t")
	data_pop_f <- merge(data_pop_f,tmp,by=c('State','State.Code','Yearly.July.1st.Estimates','Yearly.July.1st.Estimates.Code',
																					'Age.Group', 'Age.Group.Code', 'Race', 'Race.Code', 'Ethnicity', 'Ethnicity.Code', 'Population','Notes'),all=T)
}
data_pop_f <- data_pop_f[!is.na(data_pop_f$State.Code),]

data_pop_f <- data_pop_f %>%
	mutate(race.eth:= case_when(Ethnicity=='Hispanic or Latino'~'Hispanic',
				 										 Ethnicity=='Not Hispanic or Latino' & Race=='American Indian or Alaska Native'~'Non-Hispanic American Indian or Alaska Native',
				 										 Ethnicity=='Not Hispanic or Latino' & Race=='Asian or Pacific Islander'~'Non-Hispanic Asian',
				 										 Ethnicity=='Not Hispanic or Latino' & Race=='Black or African American'~'Non-Hispanic Black',
				 										 Ethnicity=='Not Hispanic or Latino' & Race=='White'~'Non-Hispanic White',
				 										 TRUE~'Unknown'
				 ))
data_pop_f <- data.table(data_pop_f)
data_pop_f[, age.cat:=Age.Group.Code]
data_pop_f[age.cat %in%c('1','1-4'), age.cat:='0-4']
setnames(data_pop_f,c('State','Yearly.July.1st.Estimates','Race','Ethnicity','Population'),c('state','year','race','hispanic','population'))
data_pop_f_agec <- data_pop_f[, list(population=sum(population)),by=c('state','year',
																																			'age.cat', 'race.eth')]
data_pop_f$gender <- 'Female'
#####
data_pop_m = read.delim('data/USA/pop/pop_m_2016-2018.txt',header = TRUE, sep = "\t")

indir.pop <- file.path('data','USA','pop')
infiles <- data.table(F=list.files(indir.pop, pattern='pop_m', full.names=TRUE, recursive=FALSE))

for(i in 2:nrow(infiles)){
	infile <- infiles[i,F]
	cat('Process',infile,'\n')
	tmp <- read.delim(infile,header = TRUE, sep = "\t")
	data_pop_m <- merge(data_pop_m,tmp,by=c('States','States.Code','Yearly.July.1st.Estimates','Yearly.July.1st.Estimates.Code',
																					'Five.Year.Age.Groups', 'Five.Year.Age.Groups.Code', 'Race', 'Race.Code', 'Ethnicity', 'Ethnicity.Code', 'Population','Notes'),all=T)
}
data_pop_m <- data_pop_m[!is.na(data_pop_m$States.Code),]

data_pop_m <- data_pop_m %>%
	mutate(race.eth:= case_when(Ethnicity=='Hispanic or Latino'~'Hispanic',
				 										 Ethnicity=='Not Hispanic or Latino' & Race=='American Indian or Alaska Native'~'Non-Hispanic American Indian or Alaska Native',
				 										 Ethnicity=='Not Hispanic or Latino' & Race=='Asian'~'Non-Hispanic Asian',
				 										 Ethnicity=='Not Hispanic or Latino' & Race=='Native Hawaiian or Other Pacific Islander'~'Non-Hispanic Asian',
				 										 Ethnicity=='Not Hispanic or Latino' & Race=='More than one race'~'Non-Hispanic More than one race',
				 										 Ethnicity=='Not Hispanic or Latino' & Race=='Black or African American'~'Non-Hispanic Black',
				 										 Ethnicity=='Not Hispanic or Latino' & Race=='White'~'Non-Hispanic White',
				 										 TRUE~'Unknown'))
data_pop_m <- data.table(data_pop_m)
data_pop_m <- data.table(data_pop_m)
data_pop_m[, age.cat:=Five.Year.Age.Groups.Code]
data_pop_m[age.cat %in%c('1','1-4'), age.cat:='0-4']

setnames(data_pop_m,c('States','Yearly.July.1st.Estimates','Race','Ethnicity','Population'),c('state','year','race','hispanic','population'))
data_pop_m_agec <- data_pop_m[, list(population=sum(population)),by=c('state','year',
																																			'age.cat', 'race.eth')]
data_pop_m$gender <- 'Male'

data_pop_m <- subset(data_pop_m,select=c('state','year','age.cat','gender','race.eth','population'))
data_pop_f <- subset(data_pop_f,select=c('state','year','age.cat','gender','race.eth','population'))
data <- merge(data_pop_m,data_pop_f,by=c('state','year','age.cat','gender','race.eth','population'),all=T)
data <- subset(data,year==2019)
####
pop <- subset(data,race.eth %notin% c('Non-Hispanic Native Hawaiian or Other Pacific Islander','Non-Hispanic More than one race','Unknown'))
pop <- subset(pop,age.cat!='Unknown')
pop <- pop[,list(pop=sum(population)),by=c('race.eth','age.cat','gender')]
pop[gender=='Female', pop:=-pop]
pop$age.cat <- factor(pop$age.cat,levels=c('0-4','5-9','10-14','15-19','20-24', '25-29', '30-34',
																					 '35-39', '40-44', '45-49', '50-54', '55-59', '60-64', '65-69' ,
																					 '70-74','75-79' ,'80-84', '85+'))

g <- ggplot() + 
	geom_bar(data=subset(pop,gender == "Female"), aes(x = age.cat, y = pop, fill = race.eth), stat = "identity") + 
	geom_bar(data=subset(pop,gender == "Male"), aes(x = age.cat, y = pop, fill = race.eth), stat = "identity") + 
	geom_hline(aes(yintercept=0),colour="black",size=2,linetype=2) +
	scale_y_continuous(breaks = seq(-12000000, 12000000,2000000), 
										 labels = c(paste0(seq(12, 0, -2),"m"), paste0(seq(2, 12, 2),"m"))) +
	labs(y="Population size",x="",fill="") +
	coord_flip() + 
	scale_fill_manual(values=pal.r) +
	theme_bw(base_size=40) +
	theme(legend.position="bottom",
				legend.text=element_text(size=30))#,

g <- annotate_figure(g,top=text_grob("Female         Male",size=40,hjust=0.4))
ggsave('figures/pop_pyramid.png',g, width=30,height=30)



