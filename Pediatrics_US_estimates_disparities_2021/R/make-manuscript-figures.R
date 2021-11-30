require(data.table)
require(scales)
require(usmap)
require(cowplot)
require(ggplot2)
require(RColorBrewer )
require(ggsci)
require(geofacet)
`%notin%` <- Negate(`%in%`)


home <- file.path(args$source_dir,'figures')
codes <- data.table(read.csv('data/USA/state_codes.csv'))

####################################################################################
cat(paste0("Making figure 1: maps of orphans and cumulative orphans by state \n"))
####################################################################################

orphans <- readRDS("orphans_usa_allstates_0.RDS")

do <- data.table(orphans)
do <- subset(do,race.eth!='Unknown')
do <- data.table(state=do$state,race.eth=do$race.eth,orphans=do$primary_loss,primary_loss=do$primary_loss)
st <- do[, list(orphans=sum(orphans)),by=c('state','race.eth')]
re <- do[, list(orphans=sum(orphans)),by=c('race.eth')]
do <- do[, list(orphans=sum(orphans)),by=c('state')]

variable <- "orphans"
tmp <- do
tmp = tmp[order(get(variable))]
states = unique(tmp$state)

st$state <- factor(st$state,levels=states)
st <- st[order(state),]
st$cumorphans <- ave(st$orphans, st$race.eth, FUN=cumsum)

# for state totals
do$state <- factor(do$state,levels=states)
do <- do[order(state),]

tmp <- merge(tmp,codes,by.x='state',by.y='State',all.x=T)
tmp[, state:=NULL]
tmp[, Abbrev:=NULL]
setnames(tmp,'Code','state')

# set up cut-off values 
breaks <- c(0,250,500,1000,2500,5000,10000,17000)
# specify interval/bin labels
tags <- c("[0-250)", "[250-500)", "[500-1,000)", "[1,000-2,500)","[2,500-5,000)","[5,000-10,000)", "[10,000-17,000)")
# bucketing values into bins
tmp$orphansb <- cut(tmp$orphans, 
                    breaks=breaks, 
                    include.lowest=TRUE, 
                    right=FALSE, 
                    labels=tags)
tmp[,orphans:=NULL]
pal <- brewer.pal(n=length(breaks), name="YlGnBu")
pal.r <- c(pal_jama("default")(5)[2:5],"#D3D3D3")

st[,statep:=paste0("PLUS ",state)]
st[state==levels(st$state)[1],statep:=state]
st$statep <- factor(st$statep,levels=paste0(unique(st$statep)))
st$race.eth <- factor(st$race.eth,levels=c("Hispanic","Non-Hispanic American Indian or Alaska Native",
                                           "Non-Hispanic Asian","Non-Hispanic Black","Non-Hispanic White"))
# graph 1: cumulative orphans by state
g0 = ggplot(data=st) +
  geom_bar(aes(x=cumorphans,y=statep,fill=race.eth),stat='identity',width=0.9) +
  scale_x_continuous(expand=expansion(mult = c(0, .05)), labels = comma) +
  scale_fill_manual(values=pal.r) +
  labs(x='Cumulative loss of primary caregiver',y='',fill='') +
  theme_bw(base_size=30) +
  theme(axis.text.x=element_text(vjust = 0.5, hjust=0.5),
        legend.position="bottom",
        legend.text=element_text(size=30),
        plot.margin= unit(c(1, 1, 1, 1), "cm")) +
  guides(fill=guide_legend(nrow=5,byrow=TRUE))

g1 = plot_usmap(data = tmp, values = "orphansb") +
  scale_fill_manual(
    #name = "",
    name = "Number of children who have lost a primary caregiver", 
    values = pal, labels = tags, breaks = tags) +
  theme(legend.position = "none",
        plot.margin = unit(c(0,-0.6,-0.5, 0), "cm"),
        legend.text = element_text(size = 24), legend.title = element_text(size = 24)) 

legend_t <- cowplot::get_legend(g1 +
                                  guides(fill = guide_legend(nrow = 1,title.position="top",title.theme = element_text(
                                    size = 30,
                                    face = "bold"
                                  )), size = guide_legend(title.position="top")) +
                                  theme(legend.position = "bottom"))

## hispanic
do <- data.table(orphans)
do[,orphans:=mother+father+both]
do <- data.table(state=do$state,race.eth=do$race.eth,orphans=do$primary_loss,primary_loss=do$primary_loss)

tmp <- subset(do,race.eth=='Hispanic')
tmp <- merge(tmp,codes,by.x='state',by.y='State',all.x=T)
tmp[, state:=NULL]
tmp[, Abbrev:=NULL]
setnames(tmp,'Code','state')

# set up cut-off values 
breaks <- c(0,25,50,75,100,500,1000,5000,12000)
# specify interval/bin labels
tags <- c("[0-25)", "[25-50)", "[50-75)", "[75-100)", "[100-500)", "[500-1,000)", "[1,000-5,000)", "[5,000-12,000)")
# bucketing values into bins
tmp$orphansb <- cut(tmp$orphans, 
                    breaks=breaks, 
                    include.lowest=TRUE, 
                    right=FALSE, 
                    labels=tags)
tmp[,orphans:=NULL]
pal <- brewer.pal(n=length(breaks), name="YlOrRd")

g2 =	plot_usmap(data = tmp, values = "orphansb") +
  scale_fill_manual(name = "", values = pal, labels = tags, breaks = tags) +
  theme(legend.position = "none",
        plot.margin = unit(c(0,-0.6,-0.5, 0), "cm"),
        legend.text = element_text(size = 24), legend.title = element_text(size = 24))

legend_b <- get_legend(g2 +
                         guides(fill = guide_legend(nrow = 1)) +
                         theme(legend.position = "bottom"))

# black
tmp <- subset(do,race.eth=='Non-Hispanic Black')
tmp <- merge(tmp,codes,by.x='state',by.y='State',all.x=T)
tmp[, state:=NULL]
tmp[, Abbrev:=NULL]
setnames(tmp,'Code','state')

# bucketing values into bins
tmp$orphansb <- cut(tmp$orphans, 
                    breaks=breaks, 
                    include.lowest=TRUE, 
                    right=FALSE, 
                    labels=tags)
tmp[,orphans:=NULL]

g3 =	plot_usmap(data = tmp, values = "orphansb") +
  scale_fill_manual(name = "Number of orphans", values = pal, labels = tags, breaks = tags) +
  theme(legend.position = "none",
        plot.margin = unit(c(0,-0.6,-0.5, 0), "cm"),
        legend.text = element_text(size = 24), legend.title = element_text(size = 24)) 

#Asian
tmp <- subset(do,race.eth=='Non-Hispanic Asian')
tmp <- merge(tmp,codes,by.x='state',by.y='State',all.x=T)
tmp[, state:=NULL]
tmp[, Abbrev:=NULL]
setnames(tmp,'Code','state')

# bucketing values into bins
tmp$orphansb <- cut(tmp$orphans, 
                    breaks=breaks, 
                    include.lowest=TRUE, 
                    right=FALSE, 
                    labels=tags)
tmp[,orphans:=NULL]

g4 =	plot_usmap(data = tmp, values = "orphansb") +
  scale_fill_manual(name = "Number of orphans", values = pal, labels = tags, breaks = tags) +
  theme(legend.position = "none",
        plot.margin = unit(c(0,-0.6,-0.5, 0), "cm"),
        legend.text = element_text(size = 24), legend.title = element_text(size = 24)) 

# white
tmp <- subset(do,race.eth=='Non-Hispanic White')
tmp <- merge(tmp,codes,by.x='state',by.y='State',all.x=T)
tmp[, state:=NULL]
tmp[, Abbrev:=NULL]
setnames(tmp,'Code','state')

# bucketing values into bins
tmp$orphansb <- cut(tmp$orphans, 
                    breaks=breaks, 
                    include.lowest=TRUE, 
                    right=FALSE, 
                    labels=tags)
tmp[,orphans:=NULL]

g5 =	plot_usmap(data = tmp, values = "orphansb") +
  scale_fill_manual(name = "Number of orphans", values = pal, labels = tags, breaks = tags) +
  theme(legend.position = "none",
        plot.margin = unit(c(0,-0.6,-0.5, 0), "cm"),
        legend.text = element_text(size = 24), legend.title = element_text(size = 24)) 

# AI/AN
tmp <- subset(do,race.eth=='Non-Hispanic American Indian or Alaska Native')
tmp <- merge(tmp,codes,by.x='state',by.y='State',all.x=T)
tmp[, state:=NULL]
tmp[, Abbrev:=NULL]
setnames(tmp,'Code','state')

# bucketing values into bins
tmp$orphansb <- cut(tmp$orphans, 
                    breaks=breaks, 
                    include.lowest=TRUE, 
                    right=FALSE, 
                    labels=tags)
tmp[,orphans:=NULL]

g6 =	plot_usmap(data = tmp, values = "orphansb") +
  scale_fill_manual(name = "Number of orphans", values = pal, labels = tags, breaks = tags) +
  theme(legend.position = "none",
        plot.margin = unit(c(0,-0.6,-0.5, 0), "cm"),
        legend.text = element_text(size = 24), legend.title = element_text(size = 24)) 

cat(paste0("combine figure 1 plots \n"))
require(ggpubr)

g1 <- annotate_figure(g1,	top = text_grob("Total",size=30,vjust=2))
g2 <- annotate_figure(g2,	top = text_grob("Hispanic",size=30,vjust=2))
g3 <- annotate_figure(g3,	top = text_grob("Non-Hispanic Black",size=30,vjust=2))
g4 <- annotate_figure(g4,	top = text_grob("Non-Hispanic Asian",size=30,vjust=2))
g5 <- annotate_figure(g5,	top = text_grob("Non-Hispanic White",size=30,vjust=2))
g6 <- annotate_figure(g6,	top = text_grob("Non-Hispanic American Indian & Alaska Native",size=30,vjust=2))

gr <- ggarrange(g1,g2,g6,g4,g3,g5,ncol=2,nrow=3,align="v")
g <- ggarrange(gr,legend_t,legend_b,ncol=1,heights = c(4,0.3,0.3))
g_left <- annotate_figure(g0,fig.lab = "A", fig.lab.face = "bold",fig.lab.size=50)
g_right <- annotate_figure(g,fig.lab = "B", fig.lab.face = "bold",fig.lab.size=50)

gall <- ggarrange(g_left,g_right,ncol=2,align="hv",widths=c(1,1.5))
ggsave(file='figures/Figure1.png', gall, w=37, h=25)
ggsave(file='figures/Figure1.pdf', gall, w=37, h=25)

####################################################################################
cat(paste0("Making figure 2: map of composition of pop/deaths/orphans \n"))
####################################################################################

# load population data
pop_m <- read.csv(file = 'data/USA/pop/usa_states_population_m.csv')
pop_f <- read.csv(file = 'data/USA/pop/usa_states_population_f.csv')

pop_m <- subset(pop_m,year==2019)
pop_f <- subset(pop_f,year==2019)
pop <- data.table(rbind(pop_m,pop_f))

pop <- subset(pop,race.eth %notin% c('Non-Hispanic Native Hawaiian or Other Pacific Islander','Non-Hispanic More than one race','Unknown'))
pop_a <- subset(pop,age.cat!='0-14')

pop_t <- pop[,list(pop_t=sum(population)),by=c('state')]
pop_r <- pop[,list(pop_r=sum(population)),by=c('state','race.eth')]
pop_r_us <- pop[,list(pop_r=sum(population)),by=c('race.eth')]

pop2 <- pop_r_us[, list(race=race.eth,pct=pop_r/sum(pop_r))]
pop2 <- pop_r[, list(race=race.eth,pct=pop_r/sum(pop_r)),by=c('state')]
pop2[pop2$state %in% c('Mississippi','Louisiana')]

# get population under 18
pop_by_age <- data.table(read.csv('data/USA/pop_US_1y.csv'))
pop_by_age[, pop:=Population*1000]
pop_min <- sum(pop_by_age$pop[pop_by_age$Age<=17])

pop_kids <- subset(pop,age.cat %in% c('0-14','15-19'))
pop_kids <- pop_kids[, list(pop=sum(population)),by=c('state')]
pop_kids[, ratio:= pop/sum(pop)]
pop_kids[, pop_kids:=round(ratio*pop_min,0)]

orphans <- readRDS("orphans_usa_allstates_0.RDS")

rownames(orphans) <- NULL
do <- subset(orphans)
do <- data.table(do)
do[,orphansp:=mother+father+both]
do[,orphansgp:=sg_grandmother + sg_grandfather + sg_both + mg_grandmother + mg_grandfather + mg_both]

do <- subset(do,race.eth %notin% c('Non-Hispanic Native Hawaiian or Other Pacific Islander','Non-Hispanic More than one race','Unknown'))
do <- merge(do,pop_t,by=c('state'),all=T)
do <- merge(do,pop_r,by=c('state','race.eth'),all=T)
do <- merge(do,pop_kids,by=c('state'),all=T)

# add death data for all population
d_deaths = read.csv('data/USA/usa_states_ALL.csv', stringsAsFactors = FALSE)
dd <- data.table(d_deaths)
dd <- dd[, list(deaths=sum(deaths)),by=c('State','Race.and.Hispanic.Origin.Group')]
dd <- subset(dd,Race.and.Hispanic.Origin.Group!='Other')
setnames(dd,c('State','Race.and.Hispanic.Origin.Group'),c('state','race.eth'))
dd2 <- dd[, list(race=race.eth,pct=deaths/sum(deaths)),by=c('state')]
dd2[dd2$state %in% c('Mississippi','Louisiana')]

setnames(do,'deaths','deaths_a')

do <- merge(do,dd,by=c('state','race.eth'),all.x=T)

ans <- reshape2::melt(do,id.vars=c('state', 'race.eth'))
ans <- data.table(ans)
ans[is.na(value),value:=0]
ans[variable=='primary_loss',variable:='Loss of primary\n caregiver']
ans[variable=='orphansp',variable:='Lost parents']
ans[variable=='orphansgp',variable:='Lost grandparents']
ans[variable=='deaths',variable:='Excess deaths']
ans[variable=='pop_r',variable:='Population size']

dat <- subset(ans,variable %in% c('Population size','Excess deaths','Loss of primary\n caregiver'))
dat$variable <- factor(dat$variable,levels=c('Population size','Excess deaths','Loss of primary\n caregiver'))

dat2 <- subset(dat,race.eth %notin% c('Non-Hispanic Native Hawaiian or Other Pacific Islander','Non-Hispanic More than one race','Unknown'))
dat2 <- subset(dat2,state!='Puerto Rico')

dat3 <- copy(dat2)
dat3$race.eth <- factor(dat3$race.eth,levels=c("Hispanic","Non-Hispanic American Indian or Alaska Native",
                                               "Non-Hispanic Asian","Non-Hispanic Black","Non-Hispanic White"))

## us total
do <- subset(do,state=!'Puerto Rico')

us <- do[, list(orphans=sum(primary_loss),deaths=sum(deaths),pop_r=sum(pop_r)),by=c('race.eth')]
ans <- reshape2::melt(us,id.vars=c('race.eth'))
ans <- data.table(ans)
ans[is.na(value),value:=0]
ans[variable=='orphans',variable:='Orphans']
ans[variable=='deaths',variable:='Excess deaths']
ans[variable=='pop_r',variable:='Population size']

dat <- subset(ans,variable %in% c('Population size','Excess deaths','Orphans'))
dat$variable <- factor(dat$variable,levels=c('Population size','Excess deaths','Orphans'))

dat2 <- subset(dat,race.eth %notin% c('Non-Hispanic Native Hawaiian or Other Pacific Islander','Non-Hispanic More than one race','Unknown'))
dat2$race.eth <- factor(dat2$race.eth,levels=c("Hispanic","Non-Hispanic American Indian or Alaska Native",
                                               "Non-Hispanic Asian","Non-Hispanic Black","Non-Hispanic White"))
pal <- c(pal_jama("default")(5)[2:5],"#D3D3D3")

dat2[variable=='Excess deaths',variable:='COVID-19 associated\ndeaths']
dat2[variable=='Orphans',variable:='Loss of primary caregiver']
dat2$variable <- factor(dat2$variable,levels=c('Population size','COVID-19 associated\ndeaths','Loss of primary caregiver'))
g_all <- ggplot(data=dat2) +
  geom_bar(stat='identity',aes(x=variable,y=value,fill=race.eth),position = "fill") +
  scale_fill_manual(values=pal) +
  scale_y_continuous(expand=c(0,0),labels = scales::percent) + 
  labs(x='',y='',fill='',
       title = "A") +
  theme_bw(base_size=30) +
  theme(axis.text.x=element_text(size=30, vjust = 0, hjust=0.5),
        strip.background=element_blank(),
        axis.text.y=element_blank(),
        legend.position = "none",
        plot.title=element_text(size=60,face="bold"),
        plot.title.position = "plot") +
  guides(fill=guide_legend(nrow=2,byrow=TRUE))
ggsave('figures/Figure2_US.png',g_all,width=15,height=8)

dat3[value<0,value:=0]
dat3[variable=='Excess deaths',variable:='COVID-19 associated\ndeaths']
dat3$variable <- factor(dat3$variable,levels=c('Population size','COVID-19 associated\ndeaths','Loss of primary\n caregiver'))
g_map <- ggplot(data=dat3) +
  geom_bar(stat='identity',aes(x=variable,y=value,fill=race.eth),position = "fill") +
  scale_fill_manual(values=pal) +
  facet_geo(~state, grid="us_state_grid2") +
  scale_y_continuous(expand=c(0,0),labels = scales::percent) + 
  labs(x='',y='',fill='',
       title="B") +
  theme_bw(base_size=30) +
  theme(axis.text.x=element_text(size=24,angle=60, vjust = 0.5, hjust=0.5),
        axis.ticks=element_blank(),
        axis.text.y=element_blank(),
        strip.background=element_blank(),
        legend.position = "bottom",
        legend.text=element_text(size=34),
        plot.title=element_text(size=60,face="bold"),
        plot.title.position = "plot") +
  guides(fill=guide_legend(nrow=1,byrow=TRUE))
ggsave('figures/Figure2_map.png',width=40,height=30)


require(magick)
map <- image_read("figures/Figure2_map.png")
us <- image_read("figures/Figure2_US.png")
img <- c(us, map)
g <- image_append(img, stack = TRUE)
image_write(g, path = "figures/Figure2.png", format = "png")
image_write(g, path = "figures/Figure2.pdf", format = "pdf")

####################################################################################
cat(paste0("Making figure 3: rates of mortality and orphanhood \n"))
####################################################################################
`%notin%` <- Negate(`%in%`)
library(ggsci)
require(data.table)

## load death data
d_deaths = read.csv('data/USA/usa_states.csv', stringsAsFactors = FALSE)
dd <- data.table(d_deaths)
dd <- dd[, list(deaths=sum(deaths)),by=c('State','gender','Race.and.Hispanic.Origin.Group')]

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

pop_kids_r <- subset(pop,age.cat %in% c('0-14','15-19'))
pop_kids_r <- pop_kids_r[, list(pop=sum(population)),by=c('race.eth')]
pop_kids_r[, ratio:= pop/sum(pop)]
pop_kids_r[, pop_kids:=round(ratio*pop_min,0)]

orphans <- readRDS("orphans_usa_allstates_0.RDS")

rownames(orphans) <- NULL
do <- subset(orphans,select=c(state,race.eth,deaths,mother,father,both,all,primary_loss))
do <- data.table(do)
do[,orphans:=primary_loss]
do[orphans<0,orphans:=0]
do[deaths<0,deaths:=0]

do <- subset(do,race.eth %notin% c('Non-Hispanic Native Hawaiian or Other Pacific Islander','Non-Hispanic More than one race','Unknown'))
do <- merge(do,pop_t,by=c('state'),all=T)
do <- merge(do,pop_r,by=c('state','race.eth'),all=T)
do <- merge(do,pop_kids,by=c('state'),all=T)
do[,pct.orphaned:=(orphans/pop_kids) * 100]
do[,deaths1e5:=deaths/pop_t*1e5]
do[,orphans1e5:=orphans/pop_kids*1e5]
do[is.na(orphans1e5),orphans1e5:=0]
do[is.na(orphans),orphans:=0]

# get order of states
tot <- do[, list(deaths=sum(deaths),orphans=sum(orphans)),by=c('state')]
tot <- merge(tot,pop_t,by=c('state'))
tot <- merge(tot,subset(pop_kids,select=c('state','pop_kids')),by=c('state'))
tot <- data.table(tot)
tot[,deaths1e5:=deaths/pop_t*1e5]
tot <- tot[order(-deaths1e5)]
order <- tot$state

av_deaths <- sum(tot$deaths)/sum(tot$pop_t)*1e5
av_orphans <- sum(tot$orphans)/sum(tot$pop_kids)*1e5
av <- data.table(variable=c("Excess deaths per 100k",'Orphans (loss of primary caregiver) per 100k minors'),
                 av=c(av_deaths,av_orphans))

## melt main data
ans <- reshape2::melt(do,id.vars=c('state', 'race.eth'))
ans <- data.table(ans)
ans[is.na(value),value:=0]
ans[variable=='orphans',variable:='Orphans']
ans[variable=='deaths',variable:='Excess deaths']
ans[variable=='pop_r',variable:='Population size']
ans[variable=='pop_kids',variable:='Population 0-19']
ans[variable=='pct.orphaned',variable:='% orphans']
ans[variable=='deaths1e5',variable:='Excess deaths per 100k']
ans[variable=='orphans1e5',variable:='Loss of primary caregiver per 100k children']

dat <- subset(ans,variable %in% c('Excess deaths per 100k','Loss of primary caregiver per 100k children'))
dat$variable <- factor(dat$variable,levels=c('Excess deaths per 100k','Loss of primary caregiver per 100k children'))
dat$state <- factor(dat$state,levels=order)

dat2 <- subset(dat,race.eth %notin% c('Non-Hispanic Native Hawaiian or Other Pacific Islander','Non-Hispanic More than one race','Unknown'))
dat2 <- subset(dat2,!is.na(state))

## US overall deaths/orphans
us <- do[, list(deaths=sum(deaths),orphans=sum(orphans)),by=c('race.eth')]
us <- merge(us,pop_r_us,by=c('race.eth'))
us <- merge(us,subset(pop_kids_r,select=c('race.eth','pop_kids')),by=c('race.eth'))
us <- data.table(us)
us[,deaths1e5:=deaths/sum(pop_r)*1e5]
us[,orphans1e5:=orphans/sum(pop_kids)*1e5]
us[, state:='US total']

us <- reshape2::melt(us,id.vars=c('state', 'race.eth'))
us <- data.table(us)
us[is.na(value),value:=0]
us[variable=='deaths1e5',variable:='Excess deaths per 100k']
us[variable=='orphans1e5',variable:='Loss of primary caregiver per 100k children']
us <- subset(us,variable %in% c('Excess deaths per 100k','Loss of primary caregiver per 100k children'))
us$variable <- factor(us$variable,levels=c('Excess deaths per 100k','Loss of primary caregiver per 100k children'))
us <- subset(us,race.eth %notin% c('Non-Hispanic Native Hawaiian or Other Pacific Islander','Non-Hispanic More than one race','Unknown'))

dat4 <- rbind(us, dat2)
dat4[, group:='states']
dat4[state=='US total', group:='all']

dat4 <- subset(dat4,race.eth!='Other')
dat4$race.eth <- factor(dat4$race.eth,levels=c("Hispanic","Non-Hispanic American Indian or Alaska Native",
                                               "Non-Hispanic Asian","Non-Hispanic Black","Non-Hispanic White"))

## plot
pal <- c(pal_jama("default")(5)[2:5],"#D3D3D3")

g1 <- ggplot(data=subset(dat4,state=='US total' & variable=='Excess deaths per 100k')) +
  geom_bar(stat='identity',aes(x=state,y=value,fill=race.eth)) +
  scale_fill_manual(values=pal) +
  coord_cartesian(ylim=c(0,450*1.1)) +
  scale_y_continuous(expand = expansion(mult = c(0, .05)))  +
  labs(x='',y='',fill='') +
  theme_bw(base_size=30) +
  theme(axis.text.x=element_text(vjust = 0.5, hjust=0.5),
        strip.background=element_blank(),
        strip.text=element_blank(),
        legend.position="bottom",
        legend.text=element_text(size=30))

g2 <- ggplot(data=subset(dat4,state!='US total' & variable=='Excess deaths per 100k')) +
  geom_bar(stat='identity',aes(x=state,y=value,fill=race.eth)) +
  geom_hline(data= subset(av,variable=='Excess deaths per 100k'), aes(yintercept=av),colour="black",size=1,linetype=2) +
  scale_fill_manual(values=pal) +
  coord_cartesian(ylim=c(0,450*1.1)) +
  scale_y_continuous(expand = expansion(mult = c(0, .05)))  +
  labs(x='',y='',fill='') +
  theme_bw(base_size=30) +
  theme(axis.text.x=element_text(angle=90, vjust = 0.5, hjust=0),
        strip.background=element_blank(),
        legend.position="bottom",
        legend.text=element_text(size=30),
        strip.text=element_text(size=35))

g3 <- ggplot(data=subset(dat4,state=='US total' & variable=='Loss of primary caregiver per 100k children')) +
  geom_bar(stat='identity',aes(x=state,y=value,fill=race.eth)) +
  scale_fill_manual(values=pal) +
  coord_cartesian(ylim=c(0,300*1.1)) +
  scale_y_continuous(expand = expansion(mult = c(0, .05)))  +
  labs(x='',y='',fill='') +
  theme_bw(base_size=30) +
  theme(axis.text.x=element_text(vjust = 0.5, hjust=0.5),
        strip.background=element_blank(),
        strip.text=element_blank(),
        legend.position="bottom",
        legend.text=element_text(size=30))

g4 <- ggplot(data=subset(dat4,state!='US total' & variable=='Loss of primary caregiver per 100k children')) +
  geom_bar(stat='identity',aes(x=state,y=value,fill=race.eth)) +
  geom_hline(data= subset(av,variable=='Orphans (loss of primary caregiver) per 100k minors'), aes(yintercept=av),colour="black",size=1,linetype=2) +
  scale_fill_manual(values=pal) +
  coord_cartesian(ylim=c(0,300*1.1)) +
  scale_y_continuous(expand = expansion(mult = c(0, .05)))  +
  labs(x='',y='',fill='') +
  theme_bw(base_size=30) +
  theme(axis.text.x=element_text(angle=90, vjust = 0.5, hjust=0),
        strip.background=element_blank(),
        legend.position="bottom",
        legend.text=element_text(size=30),
        strip.text=element_text(size=35))

g_top <- ggarrange(g1,g2,widths=c(0.2,1),align="hv",common.legend=T,legend="none")
g_top <- annotate_figure(g_top,top = text_grob("COVID-19 associated mortality per 100k", face = "bold", size = 30),
                         fig.lab = "A", fig.lab.face = "bold",fig.lab.size=50)
g_bottom <- ggarrange(g3,g4,widths=c(0.2,1),align="hv",common.legend=T,legend="bottom")
g_bottom <- annotate_figure(g_bottom,top = text_grob("Loss of primary caregiver per 100k children", face = "bold", size = 30),
                            fig.lab = "B", fig.lab.face = "bold",fig.lab.size=50)
graph <- ggarrange(g_top,g_bottom,heights=c(1,1.1),align="hv",ncol=1,common.legend=T,legend="bottom")
ggsave('figures/Figure3.png',graph, width=30,height=20)
ggsave('figures/Figure3.pdf',graph, width=30,height=20)


####################################################################################
cat(paste0("Making figure 4: order states by four measures \n"))
####################################################################################

## load death data
d_deaths = read.csv('data/USA/usa_states.csv', stringsAsFactors = FALSE)
dd <- data.table(d_deaths)
dd <- dd[, list(deaths=sum(deaths)),by=c('State','gender','Race.and.Hispanic.Origin.Group')]

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

pop_kids_r <- subset(pop,age.cat %in% c('0-14','15-19'))
pop_kids_r <- pop_kids_r[, list(pop=sum(population)),by=c('race.eth')]
pop_kids_r[, ratio:= pop/sum(pop)]
pop_kids_r[, pop_kids:=round(ratio*pop_min,0)]

orphans <- readRDS("orphans_usa_allstates_0.RDS")

rownames(orphans) <- NULL
do <- subset(orphans,select=c(state,race.eth,deaths,mother,father,both,all,primary_loss))
do <- data.table(do)
do[,orphans:=primary_loss]
do[orphans<0,orphans:=0]
do[deaths<0,deaths:=0]

do <- subset(do,race.eth %notin% c('Non-Hispanic Native Hawaiian or Other Pacific Islander','Non-Hispanic More than one race','Unknown'))
do <- merge(do,pop_t,by=c('state'),all=T)
do <- merge(do,pop_r,by=c('state','race.eth'),all=T)
do <- merge(do,pop_kids,by=c('state'),all=T)
do[,pct.orphaned:=(orphans/pop_kids) * 100]
do[,deaths1e5:=deaths/pop_t*1e5]
do[,orphans1e5:=orphans/pop_kids*1e5]
do[is.na(orphans1e5),orphans1e5:=0]
do[is.na(orphans),orphans:=0]

# get order of states
tot <- do[, list(deaths=sum(deaths),orphans=sum(orphans)),by=c('state')]
tot <- merge(tot,pop_t,by=c('state'))
tot <- merge(tot,subset(pop_kids,select=c('state','pop_kids')),by=c('state'))
tot <- data.table(tot)
tot[,deaths1e5:=deaths/pop_t*1e5]
tot <- tot[order(deaths1e5)]
order <- tot$state

av_deaths <- sum(tot$deaths)/sum(tot$pop_t)*1e5
av_orphans <- sum(tot$orphans)/sum(tot$pop_kids)*1e5
av <- data.table(variable=c("Excess deaths per 100k",'Orphans (loss of primary caregiver) per 100k minors'),
                 av=c(av_deaths,av_orphans))

## melt main data
ans <- reshape2::melt(do,id.vars=c('state', 'race.eth'))
ans <- data.table(ans)
ans[is.na(value),value:=0]
ans[variable=='orphans',variable:='Orphans']
ans[variable=='deaths',variable:='Excess deaths']
ans[variable=='pop_r',variable:='Population size']
ans[variable=='pop_kids',variable:='Population 0-19']
ans[variable=='pct.orphaned',variable:='% orphans']
ans[variable=='deaths1e5',variable:='Excess deaths per 100k']
ans[variable=='orphans1e5',variable:='Loss of primary caregiver per 100k children']

dat <- subset(ans,variable %in% c('Excess deaths per 100k','Loss of primary caregiver per 100k children'))
dat$variable <- factor(dat$variable,levels=c('Excess deaths per 100k','Loss of primary caregiver per 100k children'))
dat$state <- factor(dat$state,levels=order)

dat2 <- subset(dat,race.eth %notin% c('Non-Hispanic Native Hawaiian or Other Pacific Islander','Non-Hispanic More than one race','Unknown'))
dat2 <- subset(dat2,!is.na(state))
dat2$race.eth <- factor(dat2$race.eth,levels=c("Hispanic","Non-Hispanic American Indian or Alaska Native",
                                               "Non-Hispanic Asian","Non-Hispanic Black","Non-Hispanic White"))

## plot
pal <- c(pal_jama("default")(5)[2:5],"#D3D3D3")

dat2[state %in% order[42:51] , flag:=1]
dat2[flag==1 & race.eth=='Hispanic' & variable=='Excess deaths per 100k',bar:=450*1.1]
bold <- c(rep('plain',41), rep('bold',10))
box <- data.frame(xmin=order[41],xmax=Inf,ymin=3,ymax=460)
g_deathr <- ggplot() +
  geom_bar(data=subset(dat2,variable=='Excess deaths per 100k'),stat='identity',aes(x=state,y=value,fill=race.eth)) +
  geom_rect(data=box, aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),linetype=1,color="black",fill=NA,size=2) + 
  scale_fill_manual(values=pal) +
  coord_cartesian(ylim=c(0,450*1.1)) +
  scale_y_continuous(expand = expansion(mult = c(0, .05)))  +
  labs(x='',y='Excess deaths per 100k \n over 15 years of age',fill='') +
  coord_flip() + 
  theme_bw(base_size=30) +
  theme(strip.background=element_blank(),
    legend.position="bottom",
    legend.text=element_text(size=30),
    strip.text=element_text(size=35)) 

dat5 <- subset(dat2, variable=='Loss of primary caregiver per 100k children')
dat5 <- dat5[, list(orphans=sum(value)),by=c('state')]
dat5 <- dat5[order(orphans),]
order_fig2 <- dat5$state
dat2$state <- factor(dat2$state,levels=order_fig2)

set(dat2,NULL,c('flag','bar'),NULL)
dat2[state %in% order_fig2[42:51] , flag:=1]
dat2[flag==1 & race.eth=='Hispanic' & variable=='Loss of primary caregiver per 100k children',bar:=450*1.1]
box <- data.frame(xmin=order_fig2[41],xmax=Inf,ymin=3,ymax=340)
g_orphr <- ggplot() +
  geom_bar(data=subset(dat2,variable=='Loss of primary caregiver per 100k children'),stat='identity',aes(x=state,y=value,fill=race.eth)) +
  geom_rect(data=box,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),linetype=1,color="black",fill=NA,size=2) + 
  scale_fill_manual(values=pal) +
  coord_cartesian(ylim=c(0,300*1.1)) +
  scale_y_continuous(expand = expansion(mult = c(0, .05)))  +
  coord_flip() + 
  labs(x='',y='Loss of primary caregiver \nper 100k children',fill='') +
  theme_bw(base_size=30) +
  theme(strip.background=element_blank(),
    legend.position="bottom",
    legend.text=element_text(size=30),
    strip.text=element_text(size=35))

dat2 <- subset(ans,variable %in% c('Orphans'))
dat2 <- subset(dat2,race.eth %notin% c('Non-Hispanic More than one race','Unknown'))
dat2 <- subset(dat2,!is.na(state))
dat5 <- dat2[, list(orphans=sum(value)),by=c('state')]
dat5 <- dat5[order(orphans),]
order_or <- dat5$state
dat2$state <- factor(dat2$state,levels=order_or)
dat2$race.eth <- factor(dat2$race.eth,levels=c("Hispanic","Non-Hispanic American Indian or Alaska Native",
                                               "Non-Hispanic Asian","Non-Hispanic Black","Non-Hispanic White"))
dat2[state %in% order_or[42:51] , flag:=1]
dat2[flag==1 & race.eth=='Hispanic' & variable=='Orphans',bar:=15500*1.1]
box <- data.frame(xmin=order_or[41],xmax=Inf,ymin=100,ymax=16500)
g_orphans <- ggplot() +
  geom_bar(data=subset(dat2,state!='US total' & variable=='Orphans'),stat='identity',aes(x=state,y=value,fill=race.eth)) +
  geom_rect(data=box,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),linetype=1,color="black",fill=NA,size=2) + 
  scale_fill_manual(values=pal) +
  coord_cartesian(ylim=c(0,15500*1.1)) +
  scale_y_continuous(expand = expansion(mult = c(0, .05)))  +
  labs(x='',y='Loss of primary caregiver',fill='') +
  coord_flip() + 
  theme_bw(base_size=30) +
  theme(strip.background=element_blank(),
        legend.position="bottom",
        legend.text=element_text(size=30),
        strip.text=element_text(size=35))

### rates vs NH white

## melt main data
pop_kids_rs <- subset(pop,age.cat %in% c('0-14','15-19'))
pop_kids_rs <- pop_kids_rs[, list(pop=sum(population)),by=c('state','race.eth')]
pop_kids_rs[, ratio:= pop/sum(pop)]
pop_kids_rs[, pop_kids_rs:=round(ratio*pop_min,0)]
pop_kids_rs[, pop:=NULL]
pop_kids_rs[, ratio:=NULL]

do <- merge(do, pop_kids_rs,by=c('state','race.eth'),all.x=T)
do[, race2:='Hispanic and Non-White']
do[race.eth=='Non-Hispanic White', race2:='Non-Hispanic White']
do[race.eth=='Other', race2:=NA]
do2 <- do[, list(orphans=sum(orphans),pop_kids=sum(pop_kids_rs)),by=c('state','race2')]
do2 <- subset(do2,!is.na(race2))
do2[, orphans1e5:=orphans/pop_kids*1e5]
setnames(do2,'race2','race.eth')

dat2 <- subset(do2,!is.na(state))
nhw <- subset(dat2,dat2$race.eth=='Non-Hispanic White',select=c('state','orphans1e5'))
setnames(nhw,'orphans1e5','rate_nhw')
dat5 <- merge(dat2,nhw,by=c('state'),all.x=T)
dat5 <- subset(dat5,race.eth=='Hispanic and Non-White')
dat5 <- dat5[, list(rr=round(orphans1e5/rate_nhw,digits=2)),by=c('state','race.eth')]

dat5 <- dat5[order(rr),]
order_fig3 <- dat5$state
dat5$state <- factor(dat5$state,levels=order_fig3)

## load death data
d_deaths = read.csv('data/USA/usa_states.csv', stringsAsFactors = FALSE)
dd <- data.table(d_deaths)
dd <- dd[, list(deaths=sum(deaths)),by=c('State','gender','Race.and.Hispanic.Origin.Group')]

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

pop_kids_r <- subset(pop,age.cat %in% c('0-14','15-19'))
pop_kids_r <- pop_kids_r[, list(pop=sum(population)),by=c('race.eth')]
pop_kids_r[, ratio:= pop/sum(pop)]
pop_kids_r[, pop_kids:=round(ratio*pop_min,0)]

orphans <- readRDS("orphans_usa_allstates_0.RDS")

rownames(orphans) <- NULL
do <- subset(orphans,select=c(state,race.eth,deaths, mother,father,both,all,primary_loss))
do <- data.table(do)
do[,orphans:=primary_loss]

do[, race2:='Hispanic or Non-White']
do[race.eth=='Non-Hispanic White', race2:='Non-Hispanic White']
do[race.eth=='Other', race2:=NA]
do2 <- do[, list(orphans=sum(orphans)),by=c('state','race2')]
do2 <- subset(do2,!is.na(race2))
do2[race2=='Non-Hispanic White',orphans:=orphans*(-1)]
do2$race2 <- factor(do2$race2,levels=c("Non-Hispanic White","Hispanic or Non-White"))

pop_kids_rs <- data.table(pop_kids_rs)
pop_kids_rs[,race2:='Hispanic or Non-White']
pop_kids_rs[race.eth=='Non-Hispanic White', race2:='Non-Hispanic White']
pop_kids_rs[race.eth=='Other', race2:=NA]
pop_kids_rs <- subset(pop_kids_rs,!is.na(race2))
popk <- pop_kids_rs[, list(popk=sum(pop_kids_rs)),by=c('state','race2')]
popk <- subset(popk,!is.na(race2))

do2 <- merge(do2,popk,by=c('state','race2'),all=T)
do2 <- merge(do2,pop_kids,by=c('state'),all=T)
do2[, orphans1e5:=orphans/popk*1000]

# order states by rr
nhw <- subset(do2,do2$race2=='Non-Hispanic White',select=c('state','orphans1e5'))
setnames(nhw,'orphans1e5','rate_nhw')
dat5 <- merge(do2,nhw,by=c('state'),all.x=T)
dat5 <- subset(dat5,race2=='Hispanic or Non-White')
dat5 <- dat5[, list(rr=round(orphans1e5/rate_nhw,digits=2)),by=c('state','race2')]
dat5 <- dat5[order(-rr),]
order_fig3 <- dat5$state

do2$race2 <- factor(do2$race2,levels=c("Non-Hispanic White","Hispanic or Non-White"))
pal3 <- c(pal_jama("default")(1),"#D3D3D3")
do2$state <- factor(do2$state,levels=order_fig3)

do2[state %in% order_fig3[42:51] , flag:=1]
do2[flag==1 ,bar:=6]

box <- data.frame(xmin=order_fig3[41],xmax=Inf,ymin=-6,ymax=7)
g_pyramid <- ggplot() + 
  geom_bar(data=subset(do2,race2 == "Non-Hispanic White"), aes(x = state, y = orphans1e5, fill = race2),color=pal3[1], stat = "identity") + 
  geom_bar(data=subset(do2,race2 == "Hispanic or Non-White"), aes(x = state, y = orphans1e5, fill = race2), stat = "identity") + 
  geom_rect(data=box, aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),linetype=1,color="black",fill=NA,size=2) + 
  scale_y_continuous(breaks = seq(-6, 6,2), 
                     labels = c(seq(6, 0, -2), seq(2, 6, 2))) +
  expand_limits(y = c(-6, 6)) +
  labs(y="Loss of primary caregiver per 1,000 children \nof each race/ethnicity",x="",fill="") +
  coord_flip() + 
  scale_fill_manual(values=pal3) +
  theme_bw(base_size=30) +
  theme(legend.position="bottom",
        legend.text=element_text(size=30))#,

fig4 <- ggarrange(g_deathr+theme(legend.position="none"),g_orphans+theme(legend.position="none"),
                  g_orphr+guides(fill=guide_legend(nrow=2,byrow=TRUE)),g_pyramid+guides(fill=guide_legend(nrow=2,byrow=TRUE)),ncol=2,nrow=2,align="hv",labels=c('A','B','C','D'),hjust=0,
                  font.label = list(size = 70, color = "black", face = "bold", family = NULL))
ggsave('figures/Figure4.png',fig4, width=45,height=55,limitsize=FALSE)
ggsave('figures/Figure4.pdf',fig4, width=45,height=55,limitsize=FALSE)

