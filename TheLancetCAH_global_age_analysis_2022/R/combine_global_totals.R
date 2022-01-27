library(tidyverse)
source("TheLancetCAH_global_age_analysis_2022/R/utils.R")

parents = readRDS("TheLancetCAH_global_age_analysis_2022/data/age_outputs/reg_pa.RDS")
parents$parents = sprintf("%s [%s - %s]", 
                         format(round(parents$mean, -2), big.mark = ",", trim = TRUE), 
                         format(round.choose(parents$`2.5%`, 100, 0), big.mark = ",", trim = TRUE), 
                         format(round.choose(parents$`97.5%`, 100, 1), big.mark = ",", trim = TRUE))
parents = select(parents, who_region, parents)

primary = readRDS("TheLancetCAH_global_age_analysis_2022/data/age_outputs/reg_p.RDS")
primary$primary = sprintf("%s [%s - %s]", 
                         format(round(primary$mean, -2), big.mark = ",", trim = TRUE), 
                         format(round.choose(primary$`2.5%`, 100, 0), big.mark = ",", trim = TRUE), 
                         format(round.choose(primary$`97.5%`, 100, 1), big.mark = ",", trim = TRUE))
primary = select(primary, who_region, primary)

primary_secondary = readRDS("TheLancetCAH_global_age_analysis_2022/data/age_outputs/reg_ps.RDS")
print(sprintf("Prop SE Asia: %f", 
              primary_secondary$mean[which(primary_secondary$who_region == "South-East Asia")] / 
                sum(primary_secondary$mean)*100))
primary_secondary$primary_secondary = sprintf("%s [%s - %s]", 
                         format(round(primary_secondary$mean, -2), big.mark = ",", trim = TRUE), 
                         format(round.choose(primary_secondary$`2.5%`, 100, 0), big.mark = ",", trim = TRUE), 
                         format(round.choose(primary_secondary$`97.5%`, 100, 1), big.mark = ",", trim = TRUE))
primary_secondary = select(primary_secondary, who_region, primary_secondary)

dat = left_join(parents, primary, by = "who_region")
dat = left_join(dat, primary_secondary, by = "who_region")

tab<-xtable(dat)
print(tab, include.rownames=FALSE)

