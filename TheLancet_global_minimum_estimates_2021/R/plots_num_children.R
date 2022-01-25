library(tidyverse)

brazil_m <- read.csv("TheLancet_global_minimum_estimates_2021/data/Brazil/children_m.csv")
brazil_m$country <- rep("Brazil", length(brazil_m$age))
brazil_f <- read.csv("TheLancet_global_minimum_estimates_2021/data/Brazil/children_f.csv")
brazil_f$country <- rep("Brazil", length(brazil_f$age))

colombia_m <- read.csv("TheLancet_global_minimum_estimates_2021/data/Colombia/children_m.csv")
colombia_m$country <- rep("Colombia", length(colombia_m$age))
colombia_f <- read.csv("TheLancet_global_minimum_estimates_2021/data/Colombia/children_f.csv")
colombia_f$country <- rep("Colombia", length(colombia_f$age))

england_m <- read.csv("TheLancet_global_minimum_estimates_2021/data/UK/children_m.csv")
england_m$country <- rep("England & Wales", length(england_m$age))
england_f <- read.csv("TheLancet_global_minimum_estimates_2021/data/UK/children_f.csv")
england_f$country <- rep("England & Wales", length(england_f$age))

france_m <- read.csv("TheLancet_global_minimum_estimates_2021/data/France/children_m.csv")
france_m$country <- rep("France", length(france_m$age))
france_f <- read.csv("TheLancet_global_minimum_estimates_2021/data/France/children_f.csv")
france_f$country <- rep("France", length(france_f$age))

germany_m <- read.csv("TheLancet_global_minimum_estimates_2021/data/Germany/children_m.csv")
germany_m$country <- rep("Germany", length(germany_m$age))
germany_f <- read.csv("TheLancet_global_minimum_estimates_2021/data/Germany/children_f.csv")
germany_f$country <- rep("Germany", length(germany_f$age))

india_m <- read.csv("TheLancet_global_minimum_estimates_2021/data/India/children_m.csv")
india_m$country <- rep("India", length(india_m$age))
india_f <- read.csv("TheLancet_global_minimum_estimates_2021/data/India/children_f.csv")
india_f$country <- rep("India", length(india_f$age))

iran_m <- read.csv("TheLancet_global_minimum_estimates_2021/data/Iran/children_m.csv")
iran_m$country <- rep("I.R. Iran", length(iran_m$age))
iran_f <- read.csv("TheLancet_global_minimum_estimates_2021/data/Iran/children_f.csv")
iran_f$country <- rep("I.R. Iran", length(iran_f$age))

italy_m <- read.csv("TheLancet_global_minimum_estimates_2021/data/Italy/children_m.csv")
italy_m$country <- rep("Italy", length(italy_m$age))
italy_f <- read.csv("TheLancet_global_minimum_estimates_2021/data/Italy/children_f.csv")
italy_f$country <- rep("Italy", length(italy_f$age))

kenya_m <- read.csv("TheLancet_global_minimum_estimates_2021/data/Kenya/children_m.csv")
kenya_m$country <- rep("Kenya", length(kenya_m$age))
kenya_f <- read.csv("TheLancet_global_minimum_estimates_2021/data/Kenya/children_f.csv")
kenya_f$country <- rep("Kenya", length(kenya_f$age))

malawi_m <- read.csv("TheLancet_global_minimum_estimates_2021/data/Malawi/children_m.csv")
malawi_m$country <- rep("Malawi", length(malawi_m$age))
malawi_f <- read.csv("TheLancet_global_minimum_estimates_2021/data/Malawi/children_f.csv")
malawi_f$country <- rep("Malawi", length(malawi_f$age))

mexico_m <- read.csv("TheLancet_global_minimum_estimates_2021/data/Mexico/children_m.csv")
mexico_m$country <- rep("Mexico", length(mexico_m$age))
mexico_f <- read.csv("TheLancet_global_minimum_estimates_2021/data/Mexico/children_f.csv")
mexico_f$country <- rep("Mexico", length(mexico_f$age))

nigeria_m <- read.csv("TheLancet_global_minimum_estimates_2021/data/Nigeria/children_m.csv")
nigeria_m$country <- rep("Nigeria", length(nigeria_m$age))
nigeria_f <- read.csv("TheLancet_global_minimum_estimates_2021/data/Nigeria/children_f.csv")
nigeria_f$country <- rep("Nigeria", length(nigeria_f$age))

peru_m <- read.csv("TheLancet_global_minimum_estimates_2021/data/Peru/children_m.csv")
peru_m$country <- rep("Peru", length(peru_m$age))
peru_f <- read.csv("TheLancet_global_minimum_estimates_2021/data/Peru/children_f.csv")
peru_f$country <- rep("Peru", length(peru_f$age))

philippines_m <- read.csv("TheLancet_global_minimum_estimates_2021/data/Philippines/children_m.csv")
philippines_m$country <- rep("Philippines", length(philippines_m$age))
philippines_f <- read.csv("TheLancet_global_minimum_estimates_2021/data/Philippines/children_f.csv")
philippines_f$country <- rep("Philippines", length(philippines_f$age))

poland_m <- read.csv("TheLancet_global_minimum_estimates_2021/data/Poland/children_m.csv")
poland_m$country <- rep("Poland", length(poland_m$age))
poland_f <- read.csv("TheLancet_global_minimum_estimates_2021/data/Poland/children_f.csv")
poland_f$country <- rep("Poland", length(poland_f$age))

russia_m <- read.csv("TheLancet_global_minimum_estimates_2021/data/Russia/children_m.csv")
russia_m$country <- rep("Russian Federation", length(russia_m$age))
russia_f <- read.csv("TheLancet_global_minimum_estimates_2021/data/Russia/children_f.csv")
russia_f$country <- rep("Russian Federation", length(russia_f$age))

southafrica_m <- read.csv("TheLancet_global_minimum_estimates_2021/data/SouthAfrica/children_m.csv")
southafrica_m$country <- rep("South Africa", length(southafrica_m$age))
southafrica_f <- read.csv("TheLancet_global_minimum_estimates_2021/data/SouthAfrica/children_f.csv")
southafrica_f$country <- rep("South Africa", length(southafrica_f$age))

spain_m <- read.csv("TheLancet_global_minimum_estimates_2021/data/Spain/children_m.csv")
spain_m$country <- rep("Spain", length(spain_m$age))
spain_f <- read.csv("TheLancet_global_minimum_estimates_2021/data/Spain/children_f.csv")
spain_f$country <- rep("Spain", length(spain_f$age))

usa_m <- read.csv("TheLancet_global_minimum_estimates_2021/data/USA/children_m.csv")
usa_m$country <- rep("USA", length(usa_m$age))
usa_f <- read.csv("TheLancet_global_minimum_estimates_2021/data/USA/children_f.csv")
usa_f$country <- rep("USA", length(usa_f$age))

zimbabwe_m <- read.csv("TheLancet_global_minimum_estimates_2021/data/Zimbabwe/children_m.csv")
zimbabwe_m$country <- rep("Zimbabwe", length(zimbabwe_m$age))
zimbabwe_f <- read.csv("TheLancet_global_minimum_estimates_2021/data/Zimbabwe/children_f.csv")
zimbabwe_f$country <- rep("Zimbabwe", length(zimbabwe_f$age))

data = rbind(brazil_m, brazil_f,
             colombia_m, colombia_f,
             england_m, england_f,
             france_m, france_f,
             germany_m, germany_f,
             india_m, india_f,
             iran_m, iran_f,
             italy_m, italy_f,
             kenya_m, kenya_f,
             malawi_m, malawi_f,
             mexico_m, mexico_f,
             nigeria_m, nigeria_f,
             peru_m, peru_f,
             philippines_m, philippines_f,
             poland_m, poland_f,
             russia_m, russia_f,
             southafrica_m, southafrica_f,
             spain_m, spain_f,
             usa_m, usa_f,
             zimbabwe_m, zimbabwe_f)

data$gender <- ifelse(data$gender == "female", "Female", data$gender)
data$gender <- ifelse(data$gender == "male", "Male", data$gender)

p <- ggplot(data) + 
  geom_line(aes(age, children, group = interaction(gender, country), col = gender)) + 
  facet_wrap(~country, ncol=5) + 
  theme_bw() + 
  theme(legend.position="bottom",
        strip.background =element_rect(fill="white")) +
  xlab("Age") + ylab("Average number of children") + labs(col = "Sex of parent")
print(p)

ggsave("TheLancet_global_minimum_estimates_2021/figures/fig3.pdf", p, width = 8, height = 8)


