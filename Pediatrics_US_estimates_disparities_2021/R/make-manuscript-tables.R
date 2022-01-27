#################
## table 1
#################
`%notin%` <- Negate(`%in%`)

# load population data
pop_m <- read.csv(file = 'data/pop/usa_states_population_m.csv')
pop_f <- read.csv(file = 'data/pop/usa_states_population_f.csv')

pop_m <- subset(pop_m,year==2019)
pop_f <- subset(pop_f,year==2019)
pop <- data.table(rbind(pop_m,pop_f))
pop <- subset(pop,race.eth %notin% c('Non-Hispanic Native Hawaiian or Other Pacific Islander','Non-Hispanic More than one race','Unknown'))

pop_t <- pop[,list(pop_t=sum(population)),by=c('state')]
#adult pop only
pop_a <-  subset(pop,age.cat %notin% c('0-14'))
# population by state and race
pop_r <- pop_a[,list(pop_r=sum(population)),by=c('state','race.eth')]

# use population by 1-year age band to get population under 18
pop_by_age <- data.table(read.csv('data/pop/pop_US_1y.csv'))
pop_by_age[, pop:=Population*1000]
pop_min <- sum(pop_by_age$pop[pop_by_age$Age<=17])

pop_kids <- subset(pop,age.cat %in% c('0-14','15-19'))
pop_kids <- pop_kids[, list(pop=sum(population)),by=c('race.eth')]
pop_kids[, ratio:= pop/sum(pop)]
pop_kids[, pop_kids:=round(ratio*pop_min,0)]
pop_kids[, ratio:=NULL]
pop_kids[, p:=pop_kids/sum(pop_kids)]

# orphans
orphans <- readRDS(file = "orphans_usa_allstates_0.RDS")
do <- data.table(orphans)
do <- subset(do,race.eth!='Unknown' & race.eth!='Other')
# ensure any negative orphans are 0
do[mother<0,mother:=0]
do[father<0,father:=0]
do[both<0,both:=0]
do[sg_grandmother<0,sg_grandmother:=0]
do[sg_grandfather<0,sg_grandfather:=0]
do[sg_both<0,sg_both:=0]
do[cc_grandmother<0,cc_grandmother:=0]
do[cc_grandfather<0,cc_grandfather:=0]
do[cc_both<0,cc_both:=0]
do[mg_grandmother<0,mg_grandmother:=0]
do[mg_grandfather<0,mg_grandfather:=0]
do[mg_both<0,mg_both:=0]

do[,Parents:=mother+father+both]
do[,SkipgenGparents:=sg_grandmother+sg_grandfather+sg_both]
do[,CaregivingGparents:=cc_grandmother+cc_grandfather+cc_both]
do[,CoresGparents:=mg_grandmother+mg_grandfather+mg_both]

ans <- do[, list(Deaths=sum(deaths),Parents=sum(Parents),
								 `Skipgen grandparents`=sum(SkipgenGparents),
								 `Caregiving grandparents`=sum(CaregivingGparents),
								 `Co-residing grandparents`=sum(CoresGparents),
								 `Primary caregiver`=sum(primary_loss),
								 Total=sum(all)),by=c('race.eth')]

pop_r_sum <- pop_r[, list(pop_r=sum(pop_r)),by='race.eth']
ans <- merge(ans,pop_r_sum,by=c('race.eth'),all=T)

do <- merge(ans,pop_kids,by.x=c('race.eth'),by.y=('race.eth'),all.x=T)
do[,orphans1e5:=Total/pop_kids*1e5]
do[is.na(orphans1e5),orphans1e5:=0]
do[,deaths1e5:=Deaths/pop_r*1e5]
do[is.na(deaths1e5),deaths1e5:=0]

# calculate rate ratio
nhw <- do$orphans1e5[do$race.eth=='Non-Hispanic White']
da <- do[, list(rr=round(orphans1e5/nhw,digits=2)),by=c('race.eth')]
do <- merge(do,da,by=c('race.eth'),all.x=T)

set(do, NULL, c('pop','pop_kids','pop_r','p'), NULL)

setnames(do,c('race.eth','orphans1e5','deaths1e5','rr'),c('Race/ethnicity','Orphans per 100k minors','Deaths per 100k','Rate ratio\n(Orphans per100k minors/orphans per100k Non-Hispanic White minors)'))
do <- do[order(do$`Race/ethnicity`),]

# calculate totals
total <- do[, list(`Race/ethnicity`='Total',Deaths=sum(Deaths),Parents=sum(Parents),
									 `Skipgen grandparents`=sum(`Skipgen grandparents`),
									 `Caregiving grandparents`=sum(`Caregiving grandparents`),
									 `Co-residing grandparents`=sum(`Co-residing grandparents`),
									 `Primary caregiver`=sum(`Primary caregiver`),Total=sum(Total),
									 pop_kids=sum(pop_kids$pop_kids),pop=sum(pop_r_sum$pop_r))]
total[, orphans1e5:=Total/pop_kids*1e5]
total[, deaths1e5:=Deaths/pop*1e5]
total[, rr:='']
total[, pop_kids:=NULL]
total[, pop:=NULL]
setnames(total,c('orphans1e5','deaths1e5','rr'),c('Orphans per 100k minors','Deaths per 100k','Rate ratio\n(Orphans per100k minors/orphans per100k Non-Hispanic White minors)'))

ans <- rbind(do,total)

ans$`Race/ethnicity` <- factor(ans$`Race/ethnicity`,levels=c('Non-Hispanic White','Non-Hispanic Black',
																														 'Non-Hispanic American Indian or Alaska Native',
																														 'Non-Hispanic Asian','Hispanic','Total'))

ans <- ans[order(ans$`Race/ethnicity`),]
saveRDS(ans,file='tables/Tables_agg_deaths.RDS')
write.csv(ans, file = 'tables/Tables_agg_deaths.csv')

tab1 <- subset(ans,select=c('Race/ethnicity','Parents','Skipgen grandparents','Caregiving grandparents','Primary caregiver',
														'Co-residing grandparents','Total'))
setnames(tab1,c('Skipgen grandparents','Caregiving grandparents', 'Co-residing grandparents'),
				 c('Custodial grandparents','Co-residing grandparents providing primary care','Other co-residing grandparents'))

write.csv(tab1, file = 'tables/Table1.csv')

tab2 <- subset(ans,select=c('Race/ethnicity','Deaths per 100k','Orphans per 100k minors','Rate ratio\n(Orphans per100k minors/orphans per100k Non-Hispanic White minors)'))
setnames(tab2,c('Deaths per 100k','Orphans per 100k minors','Rate ratio\n(Orphans per100k minors/orphans per100k Non-Hispanic White minors)'),
				 c('Deaths per 100k residents','Orphans per 100k children of each race and ethnicity', 'Rate ratio (Orphans per 100k children/orphans per 100k non-Hispanic White children)'))
tab2[, `proportionate burden per child by race and ethnicity`:= paste0('1 of ',floor(100000/`Orphans per 100k children of each race and ethnicity`))]

fert_f <- data.table(read.csv('data/fertility/usa_states_fertility_f.csv'))
fert_f <- subset(fert_f,age!='0-14' & race.eth!='Unknown' & age!='50+')
db <- fert_f[, list(births=sum(births,na.rm=T),pop=sum(population,na.rm=T)),by=c('race.eth','age')]
db[,fertility_rate := births / (pop)]
tfr_r <- db[, list(tfr=round(sum(fertility_rate)*5,2)),by=c('race.eth')]

db <- fert_f[, list(births=sum(births,na.rm=T),pop=sum(population,na.rm=T)),by=c('age')]
db[,fertility_rate := births / (pop)]
tfr <- db[, list(tfr=round(sum(fertility_rate)*5,2))]
tfr[, race.eth:='Total']

tfr <- merge(tfr_r,tfr,by=c('race.eth','tfr'),all=T)

tab2 <- merge(tab2, tfr, by.x=c('Race/ethnicity'),by.y=c('race.eth'),all=T)

tab2 <- tab2[, c(1,2,6,3,5,4)]
tab2$`Race/ethnicity` <- factor(tab2$`Race/ethnicity`,levels=c('Non-Hispanic White','Non-Hispanic Black',
																														 'Non-Hispanic American Indian or Alaska Native',
																														 'Non-Hispanic Asian','Hispanic','Total'))
tab2 <- tab2[order(tab2$`Race/ethnicity`),]
tab2[, `Deaths per 100k residents`:=round(`Deaths per 100k residents`,0)]
tab2[, `Orphans per 100k children of each race and ethnicity`:=round(`Orphans per 100k children of each race and ethnicity`,0)]
write.csv(tab2, file = 'tables/Table2.csv')


	