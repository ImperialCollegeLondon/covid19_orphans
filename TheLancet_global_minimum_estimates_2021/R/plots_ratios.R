library(tidyverse)
library(cowplot)
library(gridExtra)

argentina <- read.csv("TheLancet_global_minimum_estimates_2021/data/Argentina/all_data.csv", stringsAsFactors = FALSE)
names(argentina) <- c("age", "gender", "deaths", "orphans")
argentina$country <- rep("Argentina*", length(argentina$age))
argentina$age[which(argentina$age == "00-19")] = "0-19"

brazil <- read.csv("TheLancet_global_minimum_estimates_2021/data/Brazil/all_data.csv", stringsAsFactors = FALSE)
names(brazil) <- c("age", "gender", "deaths", "orphans")
brazil$country <- rep("Brazil**", length(brazil$age))

colombia <- read.csv("TheLancet_global_minimum_estimates_2021/data/Colombia/all_data.csv", stringsAsFactors = FALSE)
names(colombia) <- c("age", "gender", "deaths", "orphans")
colombia$country <- rep("Colombia**", length(colombia$age))

england <- read.csv("TheLancet_global_minimum_estimates_2021/data/UK/england_wales_all_data.csv", stringsAsFactors = FALSE)
england <- select(england, age, gender, nb_deaths, nb_orphans)
names(england) <- c("age", "gender", "deaths", "orphans")
england$country <- rep("England & Wales", length(england$age))

france <- read.csv("TheLancet_global_minimum_estimates_2021/data/France/all_data.csv", stringsAsFactors = FALSE)
france <- select(france, age, gender, nb_deaths, nb_orphans)
names(france) <- c("age", "gender", "deaths", "orphans")
france$country <- rep("France", length(france$age))

germany <- read.csv("TheLancet_global_minimum_estimates_2021/data/Germany/all_data.csv", stringsAsFactors = FALSE)
names(germany) <- c("age", "gender", "deaths", "orphans")
germany$country <- rep("Germany*", length(germany$age))

india <- read.csv("TheLancet_global_minimum_estimates_2021/data/India/all_data.csv", stringsAsFactors = FALSE)
names(india) <- c("age", "gender", "deaths", "orphans")
india$country <- rep("India*", length(india$age))

iran <- read.csv("TheLancet_global_minimum_estimates_2021/data/Iran/all_data.csv", stringsAsFactors = FALSE)
names(iran) <- c("age", "gender", "deaths", "orphans")
iran$country <- rep("I.R. Iran**", length(iran$age))

italy <- read.csv("TheLancet_global_minimum_estimates_2021/data/Italy/all_data.csv", stringsAsFactors = FALSE)
italy <- select(italy, age, gender, max_deaths, nb_orphans)
names(italy) <- c("age", "gender", "deaths", "orphans")
italy$country <- rep("Italy", length(italy$age))

kenya <- read.csv("TheLancet_global_minimum_estimates_2021/data/Kenya/all_data.csv", stringsAsFactors = FALSE)
names(kenya) <- c("age", "gender", "deaths", "orphans")
kenya$country <- rep("Kenya*", length(kenya$age))

malawi <- read.csv("TheLancet_global_minimum_estimates_2021/data/Malawi/all_data.csv", stringsAsFactors = FALSE)
malawi <- select(malawi, age, gender, COVID19_deaths, nb_orphans)
names(malawi) <- c("age", "gender", "deaths", "orphans")
malawi$country <- rep("Malawi*", length(malawi$age))

mexico <- read.csv("TheLancet_global_minimum_estimates_2021/data/Mexico/all_data.csv", stringsAsFactors = FALSE)
names(mexico) <- c("age", "gender", "deaths", "orphans")
mexico$country <- rep("Mexico**", length(mexico$age))

nigeria <- read.csv("TheLancet_global_minimum_estimates_2021/data/Nigeria/all_data.csv", stringsAsFactors = FALSE)
names(nigeria) <- c("age", "gender", "deaths", "orphans")
nigeria$country <- rep("Nigeria*", length(nigeria$age))

peru <- read.csv("TheLancet_global_minimum_estimates_2021/data/Peru/all_data.csv", stringsAsFactors = FALSE)
names(peru) <- c("age", "gender", "deaths", "orphans")
peru$country <- rep("Peru**", length(peru$age))

philippines <- read.csv("TheLancet_global_minimum_estimates_2021/data/Philippines/all_data.csv", stringsAsFactors = FALSE)
names(philippines) <- c("age", "gender", "deaths", "orphans")
philippines$country <- rep("Philippines*", length(philippines$age))

poland <- read.csv("TheLancet_global_minimum_estimates_2021/data/Poland/all_data.csv", stringsAsFactors = FALSE)
poland <- select(poland, age, gender, max_deaths, nb_orphans)
names(poland) <- c("age", "gender", "deaths", "orphans")
poland$country <- rep("Poland*", length(poland$age))

russia <- read.csv("TheLancet_global_minimum_estimates_2021/data/Russia/all_data.csv", stringsAsFactors = FALSE)
names(russia) <- c("age", "gender", "deaths", "orphans")
russia$country <- rep("Russian Federation", length(russia$age))

southafrica <- read.csv("TheLancet_global_minimum_estimates_2021/data/SouthAfrica/all_data.csv", stringsAsFactors = FALSE)
names(southafrica) <- c("age", "gender", "deaths", "orphans")
southafrica$country <- rep("South Africa**", length(southafrica$age))

spain <- read.csv("TheLancet_global_minimum_estimates_2021/data/Spain/all_data.csv", stringsAsFactors = FALSE)
spain <- select(spain, age, gender, max_deaths, nb_orphans)
names(spain) <- c("age", "gender", "deaths", "orphans")
spain$country <- rep("Spain", length(spain$age))

usa <- read.csv("TheLancet_global_minimum_estimates_2021/data/USA/all_data.csv", stringsAsFactors = FALSE)
usa <- select(usa, age, gender, max_deaths, nb_orphans)
names(usa) <- c("age", "gender", "deaths", "orphans")
usa$country <- rep("USA", length(usa$age))

zimbabwe <- read.csv("TheLancet_global_minimum_estimates_2021/data/Zimbabwe/all_data.csv", stringsAsFactors = FALSE)
zimbabwe <- select(zimbabwe, age, gender, COVID19_deaths, nb_orphans)
names(zimbabwe) <- c("age", "gender", "deaths", "orphans")
zimbabwe$country <- rep("Zimbabwe*", length(zimbabwe$age))
zimbabwe$age[zimbabwe$age == "1-20"] <-"0-20"

data <- rbind(argentina, brazil, colombia, england, france, germany, india, iran, 
              italy, kenya, malawi, mexico, nigeria, peru, philippines, poland, russia, southafrica, 
              spain, usa, zimbabwe)

data$gender <- ifelse(data$gender == "female", "Female", data$gender)
data$gender <- ifelse(data$gender == "male", "Male", data$gender)

p1 <- ggplot(data) +
  geom_col(aes(age, deaths, fill = gender), position = "dodge") +
  facet_wrap(~country, scales = "free", ncol = 3) +  
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
        legend.position="bottom",
        strip.background =element_rect(fill="white"),
        plot.margin=unit(c(1,1,0,1), "cm")) +
  xlab("Age") + ylab("Deaths by age for males and females") + labs(fill = "Sex") + labs(tag = "A")

p2 <- ggplot(data %>% filter(age != "0-14")) +
  geom_col(aes(age, orphans/deaths, fill = gender), position = "dodge") +
  geom_hline(yintercept = 1, alpha = 0.8, col = "grey") + 
  facet_wrap(~country, scales = "free_x", ncol = 3) +  
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
        legend.position="bottom",
        strip.background =element_rect(fill="white"),
        plot.margin=unit(c(1,1,0,1), "cm")) +
  xlab("Age") + ylab("Ratio of orphans to parental deaths") + labs(fill = "Sex of parent") + 
  labs(tag = "B")

#extract legend
#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(p1)


p3 <- grid.arrange(arrangeGrob(p1 + theme(legend.position="none"),
                               p2 + theme(legend.position="none"),
                               nrow=1),
                   mylegend, nrow=2,heights=c(10, 1))

cowplot::save_plot("TheLancet_global_minimum_estimates_2021/figures/fig2.pdf", p3, base_height = 12, base_width = 15)

ps <- readRDS("TheLancet_global_minimum_estimates_2021/data/primary_secondary_ratios.RDS")
p <- readRDS("TheLancet_global_minimum_estimates_2021/data/primary_ratios.RDS")
pa <- readRDS("TheLancet_global_minimum_estimates_2021/data/orphanhood_ratios.RDS")

ratios <- left_join(pa, p, by = "country")
ratios <- left_join(ratios, ps, by = "country")
ratios <- ratios[order(ratios$country),]
write_csv(ratios, "TheLancet_global_minimum_estimates_2021/ratios.csv")
