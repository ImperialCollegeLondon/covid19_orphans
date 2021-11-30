
#chmod -R u+rw ~/orphans/1000_reps

require(data.table)
# summarise bootstrap replicates

args <- list( 
  outdir= '~/orphans/1000_reps'
)

## command line parsing if any
args_line <-  as.list(commandArgs(trailingOnly=TRUE))
if(length(args_line) > 0) 
{
  stopifnot(args_line[[1]]=='-outdir')

  args <- list()
  args[['outdir']] <- args_line[[2]]
} 

setwd(args$outdir)

indir <- args$outdir

infiles <- data.table(F=list.files(indir, pattern='orphans_usa_allstates', full.names=TRUE, recursive=FALSE))
infiles[, REP:=gsub('orphans_usa_allstates_([0-9]+).RDS','\\1',basename(F))]

df <- list()
for(i in 1:nrow(infiles)){
	infile <- infiles[i,F]
	df[[i]] <- data.table(readRDS(infile))
	df[[i]][, REP:=infiles[i,REP]]
}
ds <- do.call('rbind',df)

# by race/eth
dt <- ds[, list(primary_loss=sum(primary_loss),all=sum(all)),by=c('REP','race.eth')]
dt <- dt[, list(q= quantile(primary_loss, prob=c(0.025, 0.975),na.rm=T),
								q_label=c('CL','CU')),by=c('race.eth')]		

ans <- dcast.data.table(dt, race.eth~q_label, value.var='q')
ans[, L:= paste0( ' [',  round(CL, d=1),'-', round(CU, d=1),']')]

# total
dt <- ds[, list(primary_loss=sum(primary_loss),all=sum(all)),by=c('REP')]
dt <- dt[, list(q= quantile(primary_loss, prob=c(0.025, 0.975),na.rm=T),
								q_label=c('CL','CU'))]		
dt[, race.eth:='Total']

ans2 <- dcast.data.table(dt, race.eth~q_label, value.var='q')
ans2[, L:= paste0( round(M, d=1), ' [',  round(CL, d=1),'-', round(CU, d=1),']')]

ans <- rbind(ans,ans2)

saveRDS(ans,file=file.path(indir,'summary.RDS'))

####################
## Table 1
####################
do <- subset(ds,race.eth!='Unknown' & race.eth!='Other')
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

ans <- do[, list(Parents=sum(Parents),
                 `Skipgen grandparents`=sum(SkipgenGparents),
                 `Caregiving grandparents`=sum(CaregivingGparents),
                 `Co-residing grandparents`=sum(CoresGparents),
                 `Primary caregiver`=sum(primary_loss),
                 Total=sum(all)),by=c('REP','race.eth')]

# calculate totals
total <- do[, list(race.eth='Total',Parents=sum(Parents),
                   `Skipgen grandparents`=sum(SkipgenGparents),
                   `Caregiving grandparents`=sum(CaregivingGparents),
                   `Co-residing grandparents`=sum(CoresGparents),
                   `Primary caregiver`=sum(primary_loss),Total=sum(all)),by=c('REP')]

ans <- rbind(ans,total)

ans$`Race/ethnicity` <- factor(ans$race.eth,levels=c('Non-Hispanic White','Non-Hispanic Black',
                                                             'Non-Hispanic American Indian or Alaska Native',
                                                             'Non-Hispanic Asian','Hispanic','Total'))

dt <- ans[, list(Parents= quantile(Parents, prob=c(0.025, 0.975),na.rm=T),
                 `Skipgen grandparents`= quantile(`Skipgen grandparents`, prob=c(0.025, 0.975),na.rm=T),
                 `Caregiving grandparents`= quantile(`Caregiving grandparents`, prob=c(0.025, 0.975),na.rm=T),
                 `Co-residing grandparents`= quantile(`Co-residing grandparents`, prob=c(0.025, 0.975),na.rm=T),
                 `Primary caregiver`= quantile(`Primary caregiver`, prob=c(0.025, 0.975),na.rm=T),
                 Total= quantile(Total, prob=c(0.025, 0.975),na.rm=T),
                q_label=c('CL','CU')),by=c('`Race/ethnicity`')]		

ans2 <- dcast.data.table(dt, `Race/ethnicity`~q_label, value.var='Parents')
ans2[, Parents:= paste0(' [',  round(CL, d=0),'-', round(CU, d=0),']')]
ans3 <- subset(ans2,select=c(`Race/ethnicity`,Parents))

ans2 <- dcast.data.table(dt, `Race/ethnicity`~q_label, value.var='Skipgen grandparents')
ans2[, `Skipgen grandparents`:= paste0(' [',  round(CL, d=0),'-', round(CU, d=0),']')]
ans3 <- merge(ans3,subset(ans2,select=c(`Race/ethnicity`,`Skipgen grandparents`)),all=T)

ans2 <- dcast.data.table(dt, `Race/ethnicity`~q_label, value.var='Caregiving grandparents')
ans2[, `Caregiving grandparents`:= paste0(' [',  round(CL, d=0),'-', round(CU, d=0),']')]
ans3 <- merge(ans3,subset(ans2,select=c(`Race/ethnicity`,`Caregiving grandparents`)),all=T)

ans2 <- dcast.data.table(dt, `Race/ethnicity`~q_label, value.var='Co-residing grandparents')
ans2[, `Co-residing grandparents`:= paste0(' [',  round(CL, d=0),'-', round(CU, d=0),']')]
ans3 <- merge(ans3,subset(ans2,select=c(`Race/ethnicity`,`Co-residing grandparents`)),all=T)

ans2 <- dcast.data.table(dt, `Race/ethnicity`~q_label, value.var='Primary caregiver')
ans2[, `Primary caregiver`:= paste0(' [',  round(CL, d=0),'-', round(CU, d=0),']')]
ans3 <- merge(ans3,subset(ans2,select=c(`Race/ethnicity`,`Primary caregiver`)),all=T)

ans2 <- dcast.data.table(dt, `Race/ethnicity`~q_label, value.var='Total')
ans2[, Total:= paste0(' [',  round(CL, d=0),'-', round(CU, d=0),']')]
ans3 <- merge(ans3,subset(ans2,select=c(`Race/ethnicity`,Total)),all=T)

write.csv(ans3,file=file.path(indir,'table1_CIs.csv'))


####################
## Table 2
####################

`%notin%` <- Negate(`%in%`)

# load population data
pop_m <- read.csv(file = '~/git/covid19orphans/data/USA/pop/usa_states_population_m.csv')
pop_f <- read.csv(file = '~/git/covid19orphans/data/USA/pop/usa_states_population_f.csv')

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
pop_by_age <- data.table(read.csv('~/git/covid19orphans/data/USA/pop_US_1y.csv'))
pop_by_age[, pop:=Population*1000]
pop_min <- sum(pop_by_age$pop[pop_by_age$Age<=17])

pop_kids <- subset(pop,age.cat %in% c('0-14','15-19'))
pop_kids <- pop_kids[, list(pop=sum(population)),by=c('race.eth')]
pop_kids[, ratio:= pop/sum(pop)]
pop_kids[, pop_kids:=round(ratio*pop_min,0)]
pop_kids[, ratio:=NULL]
pop_kids[, p:=pop_kids/sum(pop_kids)]

# orphans
do <- subset(ds,race.eth!='Unknown' & race.eth!='Other')
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
                 Total=sum(all)),by=c('REP','race.eth')]

pop_r_sum <- pop_r[, list(pop_r=sum(pop_r)),by='race.eth']
ans <- merge(ans,pop_r_sum,by=c('race.eth'),all=T)

do <- merge(ans,pop_kids,by.x=c('race.eth'),by.y=c('race.eth'),all.x=T)
do[,orphans1e5:=Total/pop_kids*1e5]
do[is.na(orphans1e5),orphans1e5:=0]
do[,deaths1e5:=Deaths/pop_r*1e5]
do[is.na(deaths1e5),deaths1e5:=0]

# calculate rate ratio
nhw <- do[do$race.eth=='Non-Hispanic White',c('REP','orphans1e5')]
setnames(nhw,'orphans1e5','nhw')
do <- merge(do,nhw,by=c('REP'),all=T)
da <- do[, list(rr=round(orphans1e5/nhw,digits=2)),by=c('REP','race.eth')]
do[, nhw:=NULL]
do <- merge(do,da,by=c('REP','race.eth'),all.x=T)

set(do, NULL, c('pop','pop_kids','pop_r','p'), NULL)

setnames(do,c('race.eth','orphans1e5','deaths1e5','rr'),c('Race/ethnicity','Orphans per 100k minors','Deaths per 100k','Rate ratio\n(Orphans per100k minors/orphans per100k Non-Hispanic White minors)'))
do <- do[order(do$`Race/ethnicity`),]

# calculate totals
total <- do[, list(`Race/ethnicity`='Total',Deaths=sum(Deaths),Parents=sum(Parents),
                   `Skipgen grandparents`=sum(`Skipgen grandparents`),
                   `Caregiving grandparents`=sum(`Caregiving grandparents`),
                   `Co-residing grandparents`=sum(`Co-residing grandparents`),
                   `Primary caregiver`=sum(`Primary caregiver`),Total=sum(Total),
                   pop_kids=sum(pop_kids$pop_kids),pop=sum(pop_r_sum$pop_r)),by=c('REP')]
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

tab2 <- subset(ans,select=c('REP','Race/ethnicity','Deaths per 100k','Orphans per 100k minors','Rate ratio\n(Orphans per100k minors/orphans per100k Non-Hispanic White minors)'))
setnames(tab2,c('Deaths per 100k','Orphans per 100k minors','Rate ratio\n(Orphans per100k minors/orphans per100k Non-Hispanic White minors)'),
         c('Deaths per 100k residents','Orphans per 100k children of each race and ethnicity', 'Rate ratio (Orphans per 100k children/orphans per 100k non-Hispanic White children)'))
tab2[, `proportionate burden per child by race and ethnicity`:= paste0('1 of ',floor(100000/`Orphans per 100k children of each race and ethnicity`))]

# read in fertility data
jobtag2 <- 'mortality_data_update_covidd_1000_reps'
indir2 <- file.path('/rds/general/user/ablenkin/home/orphans',jobtag2)
infiles <- data.table(F=list.files(indir2, pattern='usa_states_fertility_f', full.names=TRUE, recursive=FALSE))
infiles[, REP:=gsub('usa_states_fertility_f_([0-9]+).csv','\\1',basename(F))]

df <- list()
for(i in 1:nrow(infiles)){
  infile <- infiles[i,F]
  df[[i]] <- data.table(read.csv(infile))
  df[[i]][, REP:=infiles[i,REP]]
}
fert_f <- do.call('rbind',df)
fert_f <- subset(fert_f,age!='0-14' & race.eth!='Unknown' & age!='50+')

db <- fert_f[, list(births=sum(births,na.rm=T),pop=sum(population,na.rm=T)),by=c('REP','race.eth','age')]
db[,fertility_rate := births / (pop)]
tfr_r <- db[, list(tfr=round(sum(fertility_rate)*5,2)),by=c('REP','race.eth')]

db <- fert_f[, list(births=sum(births,na.rm=T),pop=sum(population,na.rm=T)),by=c('REP','age')]
db[,fertility_rate := births / (pop)]
tfr <- db[, list(tfr=round(sum(fertility_rate)*5,2)),by=c('REP')]
tfr[, race.eth:='Total']

tfr <- merge(tfr_r,tfr,by=c('race.eth','tfr','REP'),all=T)

tab2 <- merge(tab2, tfr, by.x=c('Race/ethnicity','REP'),by.y=c('race.eth','REP'),all=T)

tab2 <- tab2[, c(1,2,3,7,4,6,5)]
tab2$`Race/ethnicity` <- factor(tab2$`Race/ethnicity`,levels=c('Non-Hispanic White','Non-Hispanic Black',
                                                               'Non-Hispanic American Indian or Alaska Native',
                                                               'Non-Hispanic Asian','Hispanic','Total'))
tab2 <- tab2[order(tab2$`Race/ethnicity`),]
tab2[, `Deaths per 100k residents`:=round(`Deaths per 100k residents`,0)]
tab2[, `Orphans per 100k children of each race and ethnicity`:=round(`Orphans per 100k children of each race and ethnicity`,0)]
tab2[, `Rate ratio (Orphans per 100k children/orphans per 100k non-Hispanic White children)`:=as.numeric(`Rate ratio (Orphans per 100k children/orphans per 100k non-Hispanic White children)`)]

dt <- tab2[, list(`Deaths per 100k residents`= quantile(`Deaths per 100k residents`, prob=c(0.025, 0.975),na.rm=T),
                  tfr= quantile(tfr, prob=c(0.025, 0.975),na.rm=T),
                  `Orphans per 100k children of each race and ethnicity`= quantile(`Orphans per 100k children of each race and ethnicity`, prob=c(0.025, 0.975),na.rm=T),
                 `Rate ratio (Orphans per 100k children/orphans per 100k non-Hispanic White children)`= quantile(`Rate ratio (Orphans per 100k children/orphans per 100k non-Hispanic White children)`, prob=c(0.025, 0.975),na.rm=T),
                 q_label=c('CL','CU')),by=c('`Race/ethnicity`')]		

dt[, `proportionate burden per child by race and ethnicity`:=floor(100000/`Orphans per 100k children of each race and ethnicity`)]

ans2 <- dcast.data.table(dt, `Race/ethnicity`~q_label, value.var='Deaths per 100k residents')
ans2[, `Deaths per 100k residents`:= paste0(' [',  round(CL, d=0),'-', round(CU, d=0),']')]
ans3 <- subset(ans2,select=c(`Race/ethnicity`,`Deaths per 100k residents`))

ans2 <- dcast.data.table(dt, `Race/ethnicity`~q_label, value.var='tfr')
ans2[, TFR:= paste0(' [',  round(CL, d=2),'-', round(CU, d=2),']')]
ans3 <- merge(ans3,subset(ans2,select=c(`Race/ethnicity`,TFR)),all=T)

ans2 <- dcast.data.table(dt, `Race/ethnicity`~q_label, value.var='Orphans per 100k children of each race and ethnicity')
ans2[, `Orphans per 100k children of each race and ethnicity`:= paste0(' [',  round(CL, d=0),'-', round(CU, d=0),']')]
ans3 <- merge(ans3,subset(ans2,select=c(`Race/ethnicity`,`Orphans per 100k children of each race and ethnicity`)),all=T)

ans2 <- dcast.data.table(dt, `Race/ethnicity`~q_label, value.var='proportionate burden per child by race and ethnicity')
ans2[, `proportionate burden per child by race and ethnicity`:= paste0(' [',  round(CU, d=0),'-', round(CL, d=0),']')]
ans3 <- merge(ans3,subset(ans2,select=c(`Race/ethnicity`,`proportionate burden per child by race and ethnicity`)),all=T)

ans2 <- dcast.data.table(dt, `Race/ethnicity`~q_label, value.var='Rate ratio (Orphans per 100k children/orphans per 100k non-Hispanic White children)')
ans2[, `Rate ratio (Orphans per 100k children/orphans per 100k non-Hispanic White children)`:= paste0(' [',  round(CL, d=2),'-', round(CU, d=2),']')]
ans3 <- merge(ans3,subset(ans2,select=c(`Race/ethnicity`,`Rate ratio (Orphans per 100k children/orphans per 100k non-Hispanic White children)`)),all=T)

ans3$`Race/ethnicity` <- factor(ans3$`Race/ethnicity`,levels=c('Non-Hispanic White','Non-Hispanic Black',
                                                               'Non-Hispanic American Indian or Alaska Native',
                                                               'Non-Hispanic Asian','Hispanic','Total'))
ans3 <- ans3[order(ans3$`Race/ethnicity`),]

write.csv(ans3, file = file.path(indir,'table2_CIs.csv'))


############
# SM tables
# table 1

# by state/race/eth

do <- data.table(REP=ds$REP,state=ds$state,race.eth=ds$race.eth,orphans=ds$primary_loss,primary_loss=ds$primary_loss)
st <- do[, list(orphans=sum(orphans)),by=c('REP','state','race.eth')]
do <- do[, list(orphans=sum(orphans)),by=c('REP','state')]

central <- readRDS('SM_table_states_primary_caregivers.RDS')
states = unique(central$state)

st$state <- factor(st$state,levels=states)

ans <- dcast(st,REP+state~race.eth,value.var='orphans')
#ans <- ans[order(-state),]
ans[, Total:=rowSums(ans[,3:ncol(ans)])]

ans <- ans[, c('REP','state','Hispanic','Non-Hispanic American Indian or Alaska Native',
               'Non-Hispanic Asian','Non-Hispanic Black','Non-Hispanic White','Total')]
ans[`Non-Hispanic Asian`<0,`Non-Hispanic Asian`:=0]

dt <- ans[, list(Hispanic= quantile(Hispanic, prob=c(0.025, 0.975),na.rm=T),
                 `Non-Hispanic American Indian or Alaska Native` = quantile(`Non-Hispanic American Indian or Alaska Native`, prob=c(0.025, 0.975),na.rm=T),
                 `Non-Hispanic Asian` = quantile(`Non-Hispanic Asian`, prob=c(0.025, 0.975),na.rm=T),
                 `Non-Hispanic Black` = quantile(`Non-Hispanic Black`, prob=c(0.025, 0.975),na.rm=T),
                 `Non-Hispanic White` = quantile(`Non-Hispanic White`, prob=c(0.025, 0.975),na.rm=T),
                 Total = quantile(Total, prob=c(0.025, 0.975),na.rm=T),
                q_label=c('CL','CU')),by=c('state')]

r1 <- dcast.data.table(dt, state~q_label, value.var='Hispanic')
r1[, L:= paste0('[',  round(CL, d=0),'-', round(CU, d=0),']')]
r1 <- subset(r1, select=c('state','L'))
r1[, race.eth:='Hispanic']
r2 <- dcast.data.table(dt, state~q_label, value.var='Non-Hispanic American Indian or Alaska Native')
r2[, L:= paste0('[',  round(CL, d=0),'-', round(CU, d=0),']')]
r2 <- subset(r2, select=c('state','L'))
r2[, race.eth:='Non-Hispanic American Indian or Alaska Native']
r3 <- dcast.data.table(dt, state~q_label, value.var='Non-Hispanic Asian')
r3[, L:= paste0('[',  round(CL, d=0),'-', round(CU, d=0),']')]
r3 <- subset(r3, select=c('state','L'))
r3[, race.eth:='Non-Hispanic Asian']
r4 <- dcast.data.table(dt, state~q_label, value.var='Non-Hispanic Black')
r4[, L:= paste0('[',  round(CL, d=0),'-', round(CU, d=0),']')]
r4 <- subset(r4, select=c('state','L'))
r4[, race.eth:='Non-Hispanic Black']
r5 <- dcast.data.table(dt, state~q_label, value.var='Non-Hispanic White')
r5[, L:= paste0('[',  round(CL, d=0),'-', round(CU, d=0),']')]
r5 <- subset(r5, select=c('state','L'))
r5[, race.eth:='Non-Hispanic White']
r6 <- dcast.data.table(dt, state~q_label, value.var='Total')
r6[, L:= paste0('[',  round(CL, d=0),'-', round(CU, d=0),']')]
r6 <- subset(r6, select=c('state','L'))
r6[, race.eth:='Total']

ans <- rbind(r1,r2,r3,r4,r5,r6)
ans <- dcast(ans,state~race.eth,value.var='L')

saveRDS(ans,file='SM_table_states_primary_caregivers_CIs.RDS')

colnames(ans)[2:ncol(ans)] <- paste0(colnames(ans)[2:ncol(ans)],'_CI')
central <- merge(central,ans,by=c('state'),all=T)
central[, Hispanic:= paste0(Hispanic,' ',Hispanic_CI)]
central[, `Non-Hispanic American Indian or Alaska Native`:= paste0(`Non-Hispanic American Indian or Alaska Native`,' ',`Non-Hispanic American Indian or Alaska Native_CI`)]
central[, `Non-Hispanic Asian`:= paste0(`Non-Hispanic Asian`,' ',`Non-Hispanic Asian_CI`)]
central[, `Non-Hispanic Black`:= paste0(`Non-Hispanic Black`,' ',`Non-Hispanic Black_CI`)]
central[, `Non-Hispanic White`:= paste0(`Non-Hispanic White`,' ',`Non-Hispanic White_CI`)]
central[, `Total`:= paste0(`Total`,' ',`Total_CI`)]

central <- central[, 1:7]
central[state=='District of Columbia', `Non-Hispanic American Indian or Alaska Native`:='-']
central[state=='Delaware', `Non-Hispanic American Indian or Alaska Native`:='-']
central[state=='Hawaii', `Non-Hispanic American Indian or Alaska Native`:='-']
central[state=='Kentucky', `Non-Hispanic American Indian or Alaska Native`:='-']
central[state=='Montana', `Non-Hispanic Asian`:='-']
central[state=='North Dakota', `Non-Hispanic Asian`:='-']
central[state=='New Hampshire', `Non-Hispanic American Indian or Alaska Native`:='-']
central[state=='Rhode Island', `Non-Hispanic American Indian or Alaska Native`:='-']
central[state=='Vermont', `Non-Hispanic American Indian or Alaska Native`:='-']
central[state=='Vermont', `Non-Hispanic Asian`:='-']
central[state=='Vermont', `Hispanic`:='-']
central[state=='West Virginia', `Non-Hispanic American Indian or Alaska Native`:='-']
central[state=='Wyoming', `Non-Hispanic Asian`:='-']
central[state=='Wyoming', `Non-Hispanic Black`:='-']
saveRDS(central,file='SM_table_states_primary_caregivers_withCIs.RDS')

### Table 2: rates

# load population data
pop_m <- read.csv(file = '/rds/general/user/ablenkin/home/git/covid19orphans/data/USA/pop/usa_states_population_m.csv')
pop_f <- read.csv(file = '/rds/general/user/ablenkin/home/git/covid19orphans/data/USA/pop/usa_states_population_f.csv')

pop_m <- subset(pop_m,year==2019)
pop_f <- subset(pop_f,year==2019)
pop <- data.table(rbind(pop_m,pop_f))

pop <- subset(pop,race.eth %notin% c('Non-Hispanic Native Hawaiian or Other Pacific Islander','Non-Hispanic More than one race','Unknown'))
pop_a <- subset(pop,age.cat!='0-14')

pop_t <- pop_a[,list(pop_t=sum(population)),by=c('state')]
pop_r <- pop_a[,list(pop_r=sum(population)),by=c('state','race.eth')]
pop_r_us <- pop_a[,list(pop_r=sum(population)),by=c('race.eth')]
# get population under 18

pop_by_age <- data.table(read.csv('/rds/general/user/ablenkin/home/git/covid19orphans/data/USA/pop_US_1y.csv'))
pop_by_age[, pop:=Population*1000]
pop_min <- sum(pop_by_age$pop[pop_by_age$Age<=17])

pop_kids <- subset(pop,age.cat %in% c('0-14','15-19'))
pop_kids <- pop_kids[, list(pop=sum(population)),by=c('state')]
pop_kids[, ratio:= pop/sum(pop)]
pop_kids[, pop_kids:=round(ratio*pop_min,0)]

do <- merge(do,subset(pop_kids,select=c('state','pop_kids')),by=c('state'),all=T)
st <- merge(st,subset(pop_kids,select=c('state','pop_kids')),by=c('state'),all=T)

central <- readRDS('SM_table_states_primary_caregivers_rates.RDS')
states = unique(central$state)

st$state <- factor(st$state,levels=states)

do[, Total:=round(orphans/pop_kids*100000,0)]
st[, orphans1e5:=round(orphans/pop_kids*100000,0)]

ans <- dcast(st,REP+state~race.eth,value.var='orphans1e5')
ans <- merge(ans,subset(do,select=c('REP','state','Total')),by=c('state','REP'),all=T)
ans[, Total:=round(Total,0)]

ans <- ans[, c('REP','state','Hispanic','Non-Hispanic American Indian or Alaska Native',
               'Non-Hispanic Asian','Non-Hispanic Black','Non-Hispanic White','Total')]
ans[`Non-Hispanic Asian`<0,`Non-Hispanic Asian`:=0]

dt <- ans[, list(Hispanic= quantile(Hispanic, prob=c(0.025, 0.975),na.rm=T),
                 `Non-Hispanic American Indian or Alaska Native` = quantile(`Non-Hispanic American Indian or Alaska Native`, prob=c(0.025, 0.975),na.rm=T),
                 `Non-Hispanic Asian` = quantile(`Non-Hispanic Asian`, prob=c(0.025, 0.975),na.rm=T),
                 `Non-Hispanic Black` = quantile(`Non-Hispanic Black`, prob=c(0.025, 0.975),na.rm=T),
                 `Non-Hispanic White` = quantile(`Non-Hispanic White`, prob=c(0.025, 0.975),na.rm=T),
                 Total = quantile(Total, prob=c(0.025, 0.975),na.rm=T),
                 q_label=c('CL','CU')),by=c('state')]

r1 <- dcast.data.table(dt, state~q_label, value.var='Hispanic')
r1[, L:= paste0('[',  round(CL, d=0),'-', round(CU, d=0),']')]
r1 <- subset(r1, select=c('state','L'))
r1[, race.eth:='Hispanic']
r2 <- dcast.data.table(dt, state~q_label, value.var='Non-Hispanic American Indian or Alaska Native')
r2[, L:= paste0('[',  round(CL, d=0),'-', round(CU, d=0),']')]
r2 <- subset(r2, select=c('state','L'))
r2[, race.eth:='Non-Hispanic American Indian or Alaska Native']
r3 <- dcast.data.table(dt, state~q_label, value.var='Non-Hispanic Asian')
r3[, L:= paste0('[',  round(CL, d=0),'-', round(CU, d=0),']')]
r3 <- subset(r3, select=c('state','L'))
r3[, race.eth:='Non-Hispanic Asian']
r4 <- dcast.data.table(dt, state~q_label, value.var='Non-Hispanic Black')
r4[, L:= paste0('[',  round(CL, d=0),'-', round(CU, d=0),']')]
r4 <- subset(r4, select=c('state','L'))
r4[, race.eth:='Non-Hispanic Black']
r5 <- dcast.data.table(dt, state~q_label, value.var='Non-Hispanic White')
r5[, L:= paste0('[',  round(CL, d=0),'-', round(CU, d=0),']')]
r5 <- subset(r5, select=c('state','L'))
r5[, race.eth:='Non-Hispanic White']
r6 <- dcast.data.table(dt, state~q_label, value.var='Total')
r6[, L:= paste0('[',  round(CL, d=0),'-', round(CU, d=0),']')]
r6 <- subset(r6, select=c('state','L'))
r6[, race.eth:='Total']

ans <- rbind(r1,r2,r3,r4,r5,r6)
ans <- dcast(ans,state~race.eth,value.var='L')

saveRDS(ans,file='SM_table_states_primary_caregivers_rates_CIs.RDS')

colnames(ans)[2:ncol(ans)] <- paste0(colnames(ans)[2:ncol(ans)],'_CI')
central <- merge(central,ans,by=c('state'),all=T)
central[, Hispanic:= paste0(Hispanic,' ',Hispanic_CI)]
central[, `Non-Hispanic American Indian or Alaska Native`:= paste0(`Non-Hispanic American Indian or Alaska Native`,' ',`Non-Hispanic American Indian or Alaska Native_CI`)]
central[, `Non-Hispanic Asian`:= paste0(`Non-Hispanic Asian`,' ',`Non-Hispanic Asian_CI`)]
central[, `Non-Hispanic Black`:= paste0(`Non-Hispanic Black`,' ',`Non-Hispanic Black_CI`)]
central[, `Non-Hispanic White`:= paste0(`Non-Hispanic White`,' ',`Non-Hispanic White_CI`)]
central[, `Total`:= paste0(`Total`,' ',`Total_CI`)]

central <- central[, 1:7]
saveRDS(central,file='SM_table_states_primary_caregivers_rates_withCIs.RDS')

# exclude 14 strata with very small counts - data too limited to report

central[state=='District of Columbia', `Non-Hispanic American Indian or Alaska Native`:='-']
central[state=='Delaware', `Non-Hispanic American Indian or Alaska Native`:='-']
central[state=='Hawaii', `Non-Hispanic American Indian or Alaska Native`:='-']
central[state=='Kentucky', `Non-Hispanic American Indian or Alaska Native`:='-']
central[state=='Montana', `Non-Hispanic Asian`:='-']
central[state=='North Dakota', `Non-Hispanic Asian`:='-']
central[state=='New Hampshire', `Non-Hispanic American Indian or Alaska Native`:='-']
central[state=='Rhode Island', `Non-Hispanic American Indian or Alaska Native`:='-']
central[state=='Vermont', `Non-Hispanic American Indian or Alaska Native`:='-']
central[state=='Vermont', `Non-Hispanic Asian`:='-']
central[state=='Vermont', `Hispanic`:='-']
central[state=='West Virginia', `Non-Hispanic American Indian or Alaska Native`:='-']
central[state=='Wyoming', `Non-Hispanic Asian`:='-']
central[state=='Wyoming', `Non-Hispanic Black`:='-']
saveRDS(central,file='SM_table_states_primary_caregivers_rates_withCIs.RDS')

