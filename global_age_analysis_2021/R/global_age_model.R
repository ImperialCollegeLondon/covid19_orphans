# age covariates
# aim: predict proportion of orphans in each age category based on various factors
# covariates: number of orphans (get out of other model), 
#             percentage of population in different age groups,

library(tidyverse)
library(ggrepel)
library(brms)
library(matrixStats)
library(gridExtra)
library(xtable)
library(ggpubr)
source("global_age_analysis_2021/R/brms_joint_fit.R")
source("global_age_analysis_2021/R/utils.R")

set.seed(1)

month = "_oct"
data = readRDS(paste0("global_age_analysis_2021/data/age_outputs/age_data_scaled", month, ".RDS"))

# Read in TFR
covariarates = readRDS("global_age_analysis_2021/data/tfr_covariates.RDS")
covariarates = select(covariarates, country, who_region, fitting_deaths, tfr)
covariarates$country[which(covariarates$country == "I.R. Iran")] = "Iran (Islamic Republic of)"
data = left_join(data, covariarates, by = "country")
all_data = covariarates

# Read in population
population = readRDS("global_age_analysis_2021/data/bespoke_population_bands.RDS")
population_sub = select(population, Country.Area.Name, age, prop)
population_sub_wide = spread(population_sub, key = "age", value = "prop")
population_sub_wide$Country.Area.Name[which(population_sub_wide$Country.Area.Name =="United States")] <- "USA"
population_sub_wide$Country.Area.Name[which(population_sub_wide$Country.Area.Name =="Iran")] <- "Iran (Islamic Republic of)"
population_sub_wide$Country.Area.Name[which(population_sub_wide$Country.Area.Name =="Russia")] <- "Russian Federation"
population_sub_wide$Country.Area.Name[which(population_sub_wide$Country.Area.Name =="Bahamas, The")] <- "Bahamas"
population_sub_wide$Country.Area.Name[which(population_sub_wide$Country.Area.Name =="Bolivia")] <- "Bolivia (Plurinational State of)"
population_sub_wide$Country.Area.Name[which(population_sub_wide$Country.Area.Name =="Brunei")] <- "Brunei Darussalam"
population_sub_wide$Country.Area.Name[which(population_sub_wide$Country.Area.Name =="Burma")] <- "Myanmar"
population_sub_wide$Country.Area.Name[which(population_sub_wide$Country.Area.Name =="Congo (Brazzaville)")] <- "Congo"
population_sub_wide$Country.Area.Name[which(population_sub_wide$Country.Area.Name =="Congo (Kinshasa)")] <- "Democratic Republic of the Congo" 
population_sub_wide$Country.Area.Name[which(population_sub_wide$Country.Area.Name =="Côte d'Ivoire")] <- "Cote d'Ivoire"
population_sub_wide$Country.Area.Name[which(population_sub_wide$Country.Area.Name =="Czechia")] <- "Czech Republic"
population_sub_wide$Country.Area.Name[which(population_sub_wide$Country.Area.Name =="Gambia, The" )] <- "Gambia (Republic of The)" 
population_sub_wide$Country.Area.Name[which(population_sub_wide$Country.Area.Name =="Guinea-Bissau")] <- "Guinea Bissau" 
population_sub_wide$Country.Area.Name[which(population_sub_wide$Country.Area.Name =="Korea, South" )] <- "Republic of Korea" 
population_sub_wide$Country.Area.Name[which(population_sub_wide$Country.Area.Name =="Laos")] <- "Lao People's Democratic Republic" 
population_sub_wide$Country.Area.Name[which(population_sub_wide$Country.Area.Name =="Micronesia, Federated States of")] <- "Micronesia (Federated States of)"  
population_sub_wide$Country.Area.Name[which(population_sub_wide$Country.Area.Name =="Moldova")] <- "Republic of Moldova"
population_sub_wide$Country.Area.Name[which(population_sub_wide$Country.Area.Name =="Syria")] <- "Syrian Arab Republic"
population_sub_wide$Country.Area.Name[which(population_sub_wide$Country.Area.Name =="Tanzania")] <- "United Republic of Tanzania"  
population_sub_wide$Country.Area.Name[which(population_sub_wide$Country.Area.Name =="Venezuela")] <- "Venezuela (Bolivarian Republic of)"
population_sub_wide$Country.Area.Name[which(population_sub_wide$Country.Area.Name =="Vietnam" )] <- "Viet Nam" 
population_sub_wide$Country.Area.Name[which(population_sub_wide$Country.Area.Name =="Gaza Strip")] <- "Occupied Palestinian Territory"
population_sub_wide$Country.Area.Name[which(population_sub_wide$Country.Area.Name =="United Kingdom")] <- "Scotland & Northern Ireland"
data = left_join(data, population_sub_wide, by = c("country"="Country.Area.Name"))
all_data = left_join(all_data, population_sub_wide, by = c("country"="Country.Area.Name"))

# Read in GDP
gdp <- read.csv("global_age_analysis_2021/data/gdp.csv", header = FALSE, stringsAsFactors = FALSE) #(https://data.worldbank.org/indicator/NY.GDP.MKTP.CD)
gdp_2020 <- gdp[4:269, c(1, 65)]
gdp_2019 <- gdp[4:269, c(1, 64)]
gdp_2018 <- gdp[4:269, c(1, 63)]
gdp_2017 <- gdp[4:269, c(1, 62)]
gdp_2016 <- gdp[4:269, c(1, 61)]
gdp_2015 <- gdp[4:269, c(1, 60)]
gdp_2006<- gdp[4:269, c(1, 52)]
gdp <- gdp_2020
gdp$V65 <- ifelse(!is.na(gdp_2020$V65), gdp_2020$V65, 
              ifelse(!is.na(gdp_2019$V64), gdp_2019$V64, 
                     ifelse(!is.na(gdp_2018$V63), gdp_2018$V63,
                            ifelse(!is.na(gdp_2017$V62), gdp_2017$V62,
                                   ifelse(!is.na(gdp_2016$V61), gdp_2016$V61,
                                          ifelse(!is.na(gdp_2015$V60), gdp_2015$V60, gdp_2006$V52))))))
names(gdp) <- c("country", "gdp")
gdp$country[which(gdp$country =="United Kingdom")] <- "England & Wales"
gdp$country[which(gdp$country =="United States")] <- "USA"
gdp$country[which(gdp$country =="Iran, Islamic Rep.")] <- "Iran (Islamic Republic of)"
gdp$country[which(gdp$country =="Bahamas, The")] <- "Bahamas"
gdp$country[which(gdp$country =="Bolivia")] <- "Bolivia (Plurinational State of)"
gdp$country[which(gdp$country =="Congo, Rep.")] <- "Congo"
gdp$country[which(gdp$country =="Congo, Dem. Rep.")] <- "Democratic Republic of the Congo" 
gdp$country[which(gdp$country =="Egypt, Arab Rep.")] <- "Egypt" 
gdp$country[which(gdp$country =="Gambia, The" )] <- "Gambia (Republic of The)" 
gdp$country[which(gdp$country =="Guinea-Bissau")] <- "Guinea Bissau" 
gdp$country[which(gdp$country =="Korea, Rep." )] <- "Republic of Korea" 
gdp$country[which(gdp$country =="Kyrgyz Republic")] <- "Kyrgyzstan" 
gdp$country[which(gdp$country =="Lao PDR")] <- "Lao People's Democratic Republic" 
gdp$country[which(gdp$country =="Micronesia, Fed. Sts.")] <- "Micronesia (Federated States of)"  
gdp$country[which(gdp$country =="Moldova")] <- "Republic of Moldova"
gdp$country[which(gdp$country =="Syria")] <- "Syrian Arab Republic"
gdp$country[which(gdp$country =="Tanzania")] <- "United Republic of Tanzania"  
gdp$country[which(gdp$country =="Venezuela, RB")] <- "Venezuela (Bolivarian Republic of)"
gdp$country[which(gdp$country =="Vietnam" )] <- "Viet Nam" 
gdp$country[which(gdp$country =="West Bank and Gaza")] <- "Occupied Palestinian Territory"
gdp$country[which(gdp$country =="Yemen, Rep.")] <- "Yemen"
gdp$country[which(gdp$country =="Slovak Republic")] <- "Slovakia"
skn <- data.frame("country" = "Saint Kitts and Nevis", 
                  "gdp" = 17435.9)
sl <- data.frame("country" = "Saint Lucia", 
                  "gdp" = 9276.1)
svg <- data.frame("country" = "Saint Vincent and the Grenadines", 
                  "gdp" = 7297.9)
eng <- gdp[which(gdp$country =="England & Wales"),]
eng$country <- "Scotland & Northern Ireland"
gdp <- rbind(gdp, eng, skn, sl, svg)
data = left_join(data, gdp, by = "country")
all_data = left_join(all_data, gdp, by = "country")
data$gdp = data$gdp / max(all_data$gdp)
all_data$gdp = all_data$gdp / max(all_data$gdp)


# Read in orphans estimates
orphan <- readRDS("global_age_analysis_2021/data/country_estimates_pa.RDS")
orphan <- select(orphan, country, mean, lower, upper)
all_data$country[which(all_data$country == "Iran (Islamic Republic of)")] <- "I.R. Iran"
data = left_join(data, orphan, by = "country")
all_data = right_join(all_data, orphan, by = "country")
#all_data$country[which(all_data$country == "Dominica")] <- "Dominican Republic"
 
tmp <- all_data[which(is.na(all_data$mean)),]
unique(tmp$country)

names(data) <- c("category", "gender", "li_raw", "ui_raw", "li_percent",  "ui_percent", 
                 "raw", "percent", "country", "who_region", "deaths", "tfr", "pre_school",
                 "secondary_school", "parents","old_parents", "primary_school" , "grandparents",
                 "gdp", "orphans", "lower_orphans", "upper_orphans")   
names(all_data) <- c("country", "who_region", "deaths", "tfr", "pre_school",
                 "secondary_school", "parents","old_parents", "primary_school" , "grandparents",
                 "gdp", "orphans", "lower_orphans", "upper_orphans")   

data$country[which(data$country == "Iran (Islamic Republic of)")] <- "I.R. Iran"

# Fit model
response = select(data, category, gender, raw, country, parents, old_parents, grandparents,
                  pre_school, primary_school, secondary_school, gdp)
response_wide = pivot_wider(response, names_from = c("category", "gender"), values_from = raw)
response_wide[,9:14] <- round(response_wide[,9:14])
response_wide$N <- rowSums(response_wide[, 9:14])
response_wide$response <- with(response_wide, 
                               cbind(`[0-5)_Female`, `[0-5)_Male`, `[5-10)_Female`, 
                                     `[5-10)_Male`, `[10-18)_Female`, `[10-18)_Male`))
formula = response | trials(N) ~ parents + old_parents + grandparents + 
  pre_school + primary_school + secondary_school + gdp
mod = joint_fit(all_data = data, data = response_wide, formula, plot = TRUE, loo = FALSE)
mod = readRDS("global_age_analysis_2021/data/age_outputs/global_age_fit.RDS")

# Out of sample prediction
newdat = all_data
samples <- readRDS("global_age_analysis_2021/data/orphanhood_samples.RDS")
samples <- samples[samples$country %in% all_data$country,]

# Repeat number of samples for Russia from initial study
joined <- readRDS("global_age_analysis_2021/data/tfr_covariates.RDS")
joined <- joined$orphans[which(joined$country == "Russian Federation")]
samples[17,1:1000] <- rep(joined,1000)

num_samples = 1000
combined_samples = vector(mode = "list", length = num_samples)
for (i in 1:num_samples){
  print(i)
  newdat$N = round(samples[,i])
  combined_samples[[i]] = posterior_predict(mod, newdata = newdat, ndraws = 1, seed = 2)[1,,]
}

samples_array = array(as.numeric(unlist(combined_samples)), dim = c(length(newdat$N), 6, num_samples))
write.csv(samples_array, file = "global_age_analysis_2021/data/age_outputs/age_prediction_before_switching.csv", row.names=FALSE)

# Exchange out of sample prediction for bootstrapped samples for study countries
study_samples = read.csv(paste0("global_age_analysis_2021/data/age_outputs/samples_age_data_scaled", month, ".csv"))
study_samples_sub = select(study_samples, -orphans_percent)
study_samples_sub$sample = rep(1:1000, each = 6)
study_samples_wide = pivot_wider(study_samples_sub, names_from = c("category", "gender"), values_from = "orphans")
study_samples_wide = study_samples_wide %>% mutate(totals = study_samples_wide$`[0-5)_Female` + study_samples_wide$`[0-5)_Male` + 
                                                     study_samples_wide$`[5-10)_Female` + study_samples_wide$`[5-10)_Male` + 
                                                     study_samples_wide$`[10-18)_Female` + study_samples_wide$`[10-18)_Male`)
for (j in 1:num_samples){
  study_samples_wide_sub = study_samples_wide[which(study_samples_wide$sample == j),]
  samples_array[c(1:16, 18:21), , j]  <- as.matrix(study_samples_wide_sub[,3:8])
}

write.csv(samples_array, file = "global_age_analysis_2021/data/age_outputs/age_prediction.csv", row.names=FALSE)

samples_array_reorder = aperm(samples_array, c(3,1,2))
country_totals = rowSums(samples_array_reorder, dims = 2) # have already exchanged country data

# Checks where sampled orphans are different to orphans from age model <- shows difference only occurs in study countries where
# expected
# diff = t(country_totals) - as.matrix(samples[,1:1000])
# diff[diff<1]<-0

# Calculates percentages of different ages
sample_percents = samples_array_reorder
for (k in 1:6){
  sample_percents[,,k] = samples_array_reorder[,,k] / country_totals
}

# Calculates means
sample_percents_reorder = aperm(sample_percents, c(2,3,1))
sample_percents_mean = rowMeans(sample_percents_reorder, dims = 2, na.rm = TRUE)

# Switch out sample means here to be the number of samples in deterministic case
mean_data = read.csv(file = paste0("global_age_analysis_2021/data/age_outputs/age_data_scaled", month, ".csv"))
mean_data_sub = select(mean_data, country, category, gender, percent)
study_category_means = pivot_wider(mean_data_sub, names_from = c("category", "gender"), values_from = "percent")
sample_percents_mean[c(1:16, 18:21),] <- matrix(unlist(study_category_means[,2:7]), ncol = 6)/100

lower = NULL
upper = NULL
for (c in 1:length(newdat$country)){
  lower = rbind(lower, colQuantiles(sample_percents[,c,], probs = c(0.025), na.rm = TRUE))
  upper = rbind(upper, colQuantiles(sample_percents[,c,], probs = c(0.975), na.rm = TRUE))
}

# Check mean is within intervals
#sum(sample_percents_mean > upper & sample_percents_mean < lower)

# Format table for SM
percent_format = NULL
for(i in 1:6){
  percent_format = cbind(percent_format, sprintf("%.1f%% [%.1f%% - %.1f%%]", 
                                                sample_percents_mean[,i] * 100,
                                                round.choose(lower[,i]*100, 0.1, 0),
                                                round.choose(upper[,i]*100, 0.1, 1)))
}

means = data.frame(country = newdat$country,
                   mean_sample = colMeans(country_totals))

# Change out means of estimated orphans for the study countries
mean_data = read.csv(file = paste0("global_age_analysis_2021/data/age_outputs/age_data_scaled", month, ".csv"))
mean_data_sub = select(mean_data, country, category, gender, raw)
mean_data_sub = mean_data_sub %>% group_by(country) %>% summarise(mean = sum(raw))

means = left_join(means, mean_data_sub, by = "country")
means$mean_sample = ifelse(is.na(means$mean), means$mean_sample, means$mean)

g_tot <- sprintf("%s [%s - %s]", 
                 format(round(means$mean_sample, -2), big.mark = ",", trim = TRUE), 
                 format(round.choose(colQuantiles(country_totals, probs = 0.025), 100, 0), big.mark = ",", trim = TRUE), 
                 format(round.choose(colQuantiles(country_totals, probs = 0.975), 100, 1), big.mark = ",", trim = TRUE))

percent_format = data.frame(country = newdat$country, 
                            total = g_tot,
                            percent_format)
names(percent_format) <- c("Country", "Total Orphans", "Maternal 0-4", "Paternal 0-4", "Maternal 5-9", "Paternal 5-9",
             "Maternal 10-17", "Paternal 10-17")

data_save = left_join(select(orphan, country), percent_format, by = c("country"="Country"))
write.csv(data_save, file = "global_age_analysis_2021/data/age_outputs/country_specific_totals.csv", row.names=FALSE)

tab<-xtable(data_save)
print(tab, include.rownames=FALSE)

# Working out global totals ----------------------------------------------------------------------------
totals_samples = rowSums(country_totals) 

# Samples of number of children in each category
num_10_17_male <- samples_array[,6,]
num_10_17_female <- samples_array[,5,]
num_5_9_male <- samples_array[,4,]
num_5_9_female <- samples_array[,3,]
num_0_4_male <- samples_array[,2,]
num_0_4_female <- samples_array[,1,]

# Mean number of children in each age group
num_10_17_male_mean <- rowMeans(num_10_17_male)
num_10_17_female_mean <- rowMeans(num_10_17_female)
num_5_9_male_mean <- rowMeans(num_5_9_male)
num_5_9_female_mean <- rowMeans(num_5_9_female)
num_0_4_male_mean <- rowMeans(num_0_4_male)
num_0_4_female_mean <- rowMeans(num_0_4_female)

# Need to replace study countries with deterministic number of children
mean_data = read.csv(file = paste0("global_age_analysis_2021/data/age_outputs/age_data_scaled", month, ".csv"))
mean_data_sub = select(mean_data, country, category, gender, raw)
mean_data_sub_wide = pivot_wider(mean_data_sub, names_from = c("category", "gender"), values_from = "raw")
# Here have more children that previous because we don't account for double orphans
mean_data_sub_wide = mutate(mean_data_sub_wide,
                            total = `[10-18)_Male` + `[10-18)_Female`+ `[5-10)_Male`+ `[5-10)_Female`+ `[0-5)_Male`+ `[0-5)_Female`)

num_10_17_male_mean[c(1:16, 18:21)] <- mean_data_sub_wide$`[10-18)_Male`
num_10_17_female_mean[c(1:16, 18:21)] <- mean_data_sub_wide$`[10-18)_Female`
num_5_9_male_mean[c(1:16, 18:21)] <- mean_data_sub_wide$`[5-10)_Male`
num_5_9_female_mean[c(1:16, 18:21)] <- mean_data_sub_wide$`[5-10)_Female`
num_0_4_male_mean[c(1:16, 18:21)] <- mean_data_sub_wide$`[0-5)_Male`
num_0_4_female_mean[c(1:16, 18:21)] <- mean_data_sub_wide$`[0-5)_Female`

tots_country = num_10_17_male_mean + num_10_17_female_mean + num_5_9_male_mean + num_5_9_female_mean + num_0_4_male_mean + num_0_4_female_mean

# Sum them up to get means
num_10_17_male_mean_sum = sum(num_10_17_male_mean)
num_10_17_female_mean_sum = sum(num_10_17_female_mean)
num_5_9_male_mean_sum = sum(num_5_9_male_mean)
num_5_9_female_mean_sum = sum(num_5_9_female_mean)
num_0_4_male_mean_sum = sum(num_0_4_male_mean)
num_0_4_female_mean_sum = sum(num_0_4_female_mean)

# Samples of global summed
global_10_17_male <- colSums(num_10_17_male)
global_10_17_female <- colSums(num_10_17_female)
global_5_9_male <- colSums(num_5_9_male)
global_5_9_female <- colSums(num_5_9_female)
global_0_4_male <- colSums(num_0_4_male)
global_0_4_female <- colSums(num_0_4_female)

global <- rbind(global_10_17_male, global_10_17_female,
                global_5_9_male, global_5_9_female,
                global_0_4_male, global_0_4_female)

global_percent <- global
for (j in 1:ncol(global)){
  for (i in 1:nrow(global)){
    global_percent[i,j] = global[i,j] / sum(global[,j])
  }
}
li = rowQuantiles(global_percent, probs = 0.025)
ui = rowQuantiles(global_percent, probs = 0.975)
mean = c(num_10_17_male_mean_sum, num_10_17_female_mean_sum, num_5_9_male_mean_sum, num_5_9_female_mean_sum, 
         num_0_4_male_mean_sum, num_0_4_female_mean_sum)
mean_percent = mean / sum(mean)

li_0_4 <- quantile(global_0_4_female + global_0_4_male, probs = 0.025)
ui_0_4 <- quantile(global_0_4_female + global_0_4_male, probs = 0.975)
li_5_9 <- quantile(global_5_9_female + global_5_9_male, probs = 0.025)
ui_5_9 <- quantile(global_5_9_female + global_5_9_male, probs = 0.975)
li_10_17 <- quantile(global_10_17_female + global_10_17_male, probs = 0.025)
ui_10_17 <- quantile(global_10_17_female + global_10_17_male, probs = 0.975)
li_maternal <- quantile(global_0_4_female + global_5_9_female + global_10_17_female, probs = 0.025)
ui_maternal <- quantile(global_0_4_female + global_5_9_female + global_10_17_female, probs = 0.975)
li_paternal <- quantile(global_0_4_male + global_5_9_male + global_10_17_male, probs = 0.025)
ui_paternal <- quantile(global_0_4_male + global_5_9_male + global_10_17_male, probs = 0.975)

li_0_4_percent <- quantile((global_0_4_female + global_0_4_male)/totals_samples, probs = 0.025)
ui_0_4_percent <- quantile((global_0_4_female + global_0_4_male)/totals_samples, probs = 0.975)
li_5_9_percent <- quantile((global_5_9_female + global_5_9_male)/totals_samples, probs = 0.025)
ui_5_9_percent <- quantile((global_5_9_female + global_5_9_male)/totals_samples, probs = 0.975)
li_10_17_percent <- quantile((global_10_17_female + global_10_17_male)/totals_samples, probs = 0.025)
ui_10_17_percent <- quantile((global_10_17_female + global_10_17_male)/totals_samples, probs = 0.975)
li_maternal_percent <- quantile((global_0_4_female + global_5_9_female + global_10_17_female)/totals_samples, probs = 0.025)
ui_maternal_percent <- quantile((global_0_4_female + global_5_9_female + global_10_17_female)/totals_samples, probs = 0.975)
li_paternal_percent <- quantile((global_0_4_male + global_5_9_male + global_10_17_male)/totals_samples, probs = 0.025)
ui_paternal_percent <- quantile((global_0_4_male + global_5_9_male + global_10_17_male)/totals_samples, probs = 0.975)

# Calculate global totals
global_totals = data.frame("country" = "Global extrapolation",
                           "[0-5)" = sprintf("%s [%s - %s]",
                                             format(round(num_0_4_male_mean_sum + num_0_4_female_mean_sum, -2),  big.mark = ",", trim = TRUE),
                                             format(round.choose(li_0_4, 100, 0), big.mark = ",", trim = TRUE),
                                             format(round.choose(ui_0_4, 100, 1), big.mark = ",", trim = TRUE)), 
                           "[5-10)" = sprintf("%s [%s - %s]",
                                              format(round(num_5_9_male_mean_sum + num_5_9_female_mean_sum, -2),  big.mark = ",", trim = TRUE),
                                              format(round.choose(li_5_9, 100, 0), big.mark = ",", trim = TRUE),
                                              format(round.choose(ui_5_9, 100, 1), big.mark = ",", trim = TRUE)), 
                           "[10-18)" = sprintf("%s [%s - %s]",
                                               format(round(num_10_17_male_mean_sum + num_10_17_female_mean_sum, -2),  big.mark = ",", trim = TRUE),
                                               format(round.choose(li_10_17, 100, 0), big.mark = ",", trim = TRUE),
                                               format(round.choose(ui_10_17, 100, 1), big.mark = ",", trim = TRUE)),
                           "Total" = sprintf("%s [%s - %s]",
                                           format(round(sum(mean),-2),  big.mark = ",", trim = TRUE),
                                           format(round.choose(quantile(totals_samples, probs = 0.025), 100, 0), big.mark = ",", trim = TRUE),
                                           format(round.choose(quantile(totals_samples, probs = 0.975), 100, 1), big.mark = ",", trim = TRUE)),
                           "Maternal" = sprintf("%s [%s - %s]",
                                              format(round(num_0_4_female_mean_sum + num_5_9_female_mean_sum + num_10_17_female_mean_sum, -2),  big.mark = ",", trim = TRUE),
                                              format(round.choose(li_maternal, 100, 0), big.mark = ",", trim = TRUE),
                                              format(round.choose(ui_maternal, 100, 1), big.mark = ",", trim = TRUE)),
                           "Paternal" = sprintf("%s [%s - %s]",
                                              format(round(num_0_4_male_mean_sum + num_5_9_male_mean_sum + num_10_17_male_mean_sum, -2),  big.mark = ",", trim = TRUE),
                                              format(round.choose(li_paternal, 100, 0), big.mark = ",", trim = TRUE),
                                              format(round.choose(ui_paternal, 100, 1), big.mark = ",", trim = TRUE)))

global_totals_2 = data.frame("country" = "Global extrapolation percentages",
                           "[0-5)" = sprintf("%s%% [%s%% - %s%%]",
                                             format(round(100*(num_0_4_male_mean_sum + num_0_4_female_mean_sum)/sum(mean), 1),  big.mark = ",", trim = TRUE),
                                             format(round.choose(100*li_0_4_percent, 0.1, 0), big.mark = ",", trim = TRUE),
                                             format(round.choose(100*ui_0_4_percent, 0.1, 1), big.mark = ",")), 
                           "[5-10)" = sprintf("%s%% [%s%% - %s%%]",
                                              format(round(100*(num_5_9_male_mean_sum + num_5_9_female_mean_sum)/sum(mean), 1),  big.mark = ",", trim = TRUE),
                                              format(round.choose(100*li_5_9_percent, 0.1, 0), big.mark = ",", trim = TRUE),
                                              format(round.choose(100*ui_5_9_percent, 0.1, 1), big.mark = ",", trim = TRUE)), 
                           "[10-18)" = sprintf("%s%% [%s%% - %s%%]",
                                               format(round(100*(num_10_17_male_mean_sum + num_10_17_female_mean_sum)/sum(mean), 1),  big.mark = ",", trim = TRUE),
                                               format(round.choose(100*li_10_17_percent, 0.1, 0), big.mark = ",", trim = TRUE),
                                               format(round.choose(100*ui_10_17_percent, 0.1, 1), big.mark = ",", trim = TRUE)),
                           "Total" = sprintf("-"),
                           "Maternal" = sprintf("%s%% [%s%% - %s%%]",
                                                format(round(100*(num_0_4_female_mean_sum + num_5_9_female_mean_sum + num_10_17_female_mean_sum)/sum(mean), 1),  big.mark = ",", trim = TRUE),
                                                format(round.choose(100*li_maternal_percent, 0.1, 0), big.mark = ",", trim = TRUE),
                                                format(round.choose(100*ui_maternal_percent, 0.1, 1), big.mark = ",", trim = TRUE)),
                           "Paternal" = sprintf("%s%% [%s%% - %s%%]",
                                                format(round(100*(num_0_4_male_mean_sum + num_5_9_male_mean_sum + num_10_17_male_mean_sum)/sum(mean), 1),  big.mark = ",", trim = TRUE),
                                                format(round.choose(100*li_paternal_percent, 0.1, 0), big.mark = ",", trim = TRUE),
                                                format(round.choose(100*ui_paternal_percent, 0.1, 1), big.mark = ",", trim = TRUE)))
saveRDS(rbind(global_totals, global_totals_2), 
        "global_age_analysis_2021/data/age_outputs/global_extrapolation_totals.RDS")

#print(sprintf("Mean totals of deterministic model: %f", sum(mean)))

global_percent_summary <- data.frame(mean = mean_percent * 100,
                                     li = li * 100,
                                     ui = ui * 100)
global_percent_summary$format <- sprintf("%.1f%% [%.1f%% - %.1f%%]",
                                         round(global_percent_summary$mean, digits = 1),
                                         round.choose(global_percent_summary$li, 0.1, 0),
                                         round.choose(global_percent_summary$ui, 0.1, 1))

global_percent_summary$region <- "Global"
global_percent_summary$category <- rownames(global_percent_summary)


#  Regions calculations --------------------------------
# Work out regional percentages
regions <- select(all_data, country, who_region)

# Combine europe
regions$who_region[which(regions$who_region == "Eastern European")] <- "European"

reg_percent_summary <- NULL
for (r in unique(regions$who_region)){
  idx_reg <- which(regions$who_region == r)

  reg_10_17_male <- colSums(num_10_17_male[idx_reg,])
  reg_10_17_female <- colSums(num_10_17_female[idx_reg,])
  reg_5_9_male <- colSums(num_5_9_male[idx_reg,])
  reg_5_9_female <- colSums(num_5_9_female[idx_reg,])
  reg_0_4_male <- colSums(num_0_4_male[idx_reg,])
  reg_0_4_female <- colSums(num_0_4_female[idx_reg,])

  # Samples for each category in specified region
  reg <- rbind(reg_10_17_male, reg_10_17_female,
                  reg_5_9_male, reg_5_9_female,
                  reg_0_4_male, reg_0_4_female)

  reg_10_17_male_mean <- sum(num_10_17_male_mean[idx_reg])
  reg_10_17_female_mean <- sum(num_10_17_female_mean[idx_reg])
  reg_5_9_male_mean <- sum(num_5_9_male_mean[idx_reg])
  reg_5_9_female_mean <- sum(num_5_9_female_mean[idx_reg])
  reg_0_4_male_mean <- sum(num_0_4_male_mean[idx_reg])
  reg_0_4_female_mean <- sum(num_0_4_female_mean[idx_reg])
  
  reg_percent <- reg
  for (j in 1:ncol(reg)){
    for (i in 1:nrow(reg)){
      reg_percent[i,j] = reg[i,j] / sum(reg[,j])
    }
  }
  li = rowQuantiles(reg_percent, probs = 0.025)
  ui = rowQuantiles(reg_percent, probs = 0.975)
  mean = rowMeans(reg_percent)
  reg_mean = c(reg_10_17_male_mean, reg_10_17_female_mean, reg_5_9_male_mean, reg_5_9_female_mean, 
               reg_0_4_male_mean, reg_0_4_female_mean)
  reg_mean_percent = reg_mean / sum(reg_mean)
  
  reg_percent_summary_tmp <- data.frame(mean = reg_mean_percent * 100,
                                       li = li * 100,
                                       ui = ui * 100)
  reg_percent_summary_tmp$format <- sprintf("%.1f%% [%.1f%% - %.1f%%]",
                                           round(reg_percent_summary_tmp$mean, digits = 1),
                                           round.choose(reg_percent_summary_tmp$li, 0.1, 0),
                                           round.choose(reg_percent_summary_tmp$ui, 0.1, 1))
  reg_percent_summary_tmp$category <- rownames(reg_percent_summary_tmp)

  reg_percent_summary_tmp$region <- r

  reg_percent_summary <- rbind(reg_percent_summary, reg_percent_summary_tmp)

}

reg_percent_summary <- rbind(reg_percent_summary, global_percent_summary)
reg_percent_summary$category <- str_replace(reg_percent_summary$category , "reg_", "")
reg_percent_summary$category <- str_replace(reg_percent_summary$category , "global_", "")

tab <- select(reg_percent_summary, category, region, format)

tab_wide <- spread(tab, key = category, value = format)
tab_wide <- select(tab_wide, region, "0_4_female", "0_4_male", "5_9_female",
                   "5_9_male", "10_17_female", "10_17_male")
tab_wide_global <- tab_wide[tab_wide$region == "Global",]
tab_wide <- tab_wide[tab_wide$region != "Global",]
tab_wide <- rbind(tab_wide, tab_wide_global)
write.csv(tab_wide, "global_age_analysis_2021/data/age_outputs/global_age_percentages.csv", row.names=FALSE)

tab<-xtable(tab_wide)
print(tab, include.rownames=FALSE)


reg_percent_summary$gender = c("male", "female")
reg_percent_summary$age = rep(c("old", "middle", "young"), each = 2)

dat = reg_percent_summary %>% group_by(age, region) %>% 
  summarise(mean = sum(mean),
            li = sum(li), 
            ui = sum(ui))
print(dat)

reg_percent_summary$region <- factor(reg_percent_summary$region,
                                     levels = c("African ", "Americas", "Eastern Mediterranean",
                                                "European",  "South-East Asia",  "Western Pacific", "Global"))

reg_percent_summary$category <- factor(reg_percent_summary$category,
                                       levels = c("0_4_female",  "0_4_male",  "5_9_female",
                                                  "5_9_male", "10_17_female", "10_17_male"),
                                       labels = c("Maternal orphans 0-4", "Paternal orphans 0-4", "Maternal orphans 5-9",
                                                  "Paternal orphans 5-9", "Maternal orphans 10-17", "Paternal orphans 10-17"))

p_global_a <- ggplot(reg_percent_summary %>% filter(region == "Global")) +
  geom_bar(aes(region, mean, fill = category), stat = "identity") +
  geom_text(aes(region, mean, label = sprintf("%.1f", round(mean, 1))), position = position_stack(vjust = 0.5)) +
  xlab("") + ylab("Percentage of orphans")  +
  scale_fill_manual(name = "", values = c("orchid1", "lightblue1", "orchid3", "lightblue3",  "orchid4",  "lightblue4")) +
  theme_bw()

p_global_b <- ggplot(reg_percent_summary %>% filter(region != "Global")) +
  geom_bar(aes(region, mean, fill = category), stat = "identity") +
  geom_text(aes(region, mean, label = sprintf("%.1f", round(mean, 1))), position = position_stack(vjust = 0.5)) +
  xlab("") + ylab("Percentage of orphans")  +
  scale_fill_manual(name = "", values = c("orchid1", "lightblue1", "orchid3", "lightblue3",  "orchid4",  "lightblue4")) +
  theme_bw()


p_global <- ggarrange(p_global_a, p_global_b, common.legend = TRUE, 
                      labels = "AUTO", legend="bottom", widths = c(1, 3))
print(p_global)
ggsave("global_age_analysis_2021/figures/fig_3_global_orphans_percentage.pdf", p_global, height = 7)

# --- Summarise 
reg_age = reg_percent_summary %>%
  select(region, age, mean) %>%
  group_by(region, age) %>%
  summarise(mean = sum(mean))
reg_age_wide = spread(reg_age, key = region, value = mean)
reg_age_wide$age = c("5-9", "10-17", "0-4")
write.csv(file = paste0("global_age_analysis_2021/data/age_compostion_region.csv"), reg_age_wide, row.names=FALSE)

reg_gender = reg_percent_summary %>%
  select(region, gender, mean) %>%
  group_by(region, gender) %>%
  summarise(mean = sum(mean))
reg_gender_wide = spread(reg_gender, key = region, value = mean)
write.csv(file = paste0("global_age_analysis_2021/data/gender_compostion_region.csv"), reg_gender_wide, row.names=FALSE)

# ---------- Calculating total numbers of 10-17s
adolescents = global[1,] + global[2,]
print(sprintf("Adolescent orphans: %s [%s - %s]", 
              format(round(num_10_17_male_mean_sum + num_10_17_female_mean_sum, -2), big.mark = ",", trim = TRUE), 
              format(round.choose(quantile(adolescents, probs = 0.025), 100, 0), big.mark = ",", trim = TRUE),
              format(round.choose(quantile(adolescents, probs = 0.975), 100, 1), big.mark = ",", trim = TRUE)))


# --------- Fertility sensitivity
pre_school = num_0_4_male_mean_sum + num_0_4_female_mean_sum
under_one = pre_school * 0.2
under_one_2021 = under_one * 0.5
under_one_2021_reduced = under_one_2021 * 0.2

pre_school_reduced = pre_school - under_one_2021_reduced
tot_reduced = pre_school_reduced + num_5_9_male_mean_sum + num_10_17_male_mean_sum + num_5_9_female_mean_sum + num_10_17_female_mean_sum
tot = num_0_4_male_mean_sum + num_5_9_male_mean_sum + num_10_17_male_mean_sum + num_0_4_female_mean_sum + num_5_9_female_mean_sum + num_10_17_female_mean_sum

li_0_4_reduced <- quantile((global_0_4_female + global_0_4_male) * 0.2 * 0.5 * 0.2, probs = 0.025)
ui_0_4_reduced <- quantile((global_0_4_female + global_0_4_male) * 0.2 * 0.5 * 0.2, probs = 0.975)

li = quantile(((global_0_4_female + global_0_4_male) - (global_0_4_female + global_0_4_male) * 0.2 * 0.5 * 0.2 + 
           global_5_9_female + global_5_9_male + global_10_17_female + global_10_17_male) / 
           (global_0_4_female + global_0_4_male + global_5_9_female + global_5_9_male + global_10_17_female + global_10_17_male), 
         probs = 0.025) * 100
ui = quantile(((global_0_4_female + global_0_4_male) - (global_0_4_female + global_0_4_male) * 0.2 * 0.5 * 0.2 + 
           global_5_9_female + global_5_9_male + global_10_17_female + global_10_17_male) / 
  (global_0_4_female + global_0_4_male + global_5_9_female + global_5_9_male + global_10_17_female + global_10_17_male), probs = 0.975) * 100

print(sprintf("All orphans under one: %s [%s - %s]", 
              format(round(under_one_2021, -2), big.mark = ",", trim = TRUE),
              format(round.choose(quantile((global_0_4_female + global_0_4_male) * 0.2 * 0.5, probs = 0.025), 100, 0), big.mark = ",", trim = TRUE),
              format(round.choose(quantile((global_0_4_female + global_0_4_male) * 0.2 * 0.5, probs = 0.975), 100, 1), big.mark = ",", trim = TRUE)))

print(sprintf("Reduced orphans: %s [%s - %s]", 
      format(round(under_one_2021_reduced, -2), big.mark = ",", trim = TRUE),
      format(round.choose(li_0_4_reduced, 100, 0), big.mark = ",", trim = TRUE),
      format(round.choose(ui_0_4_reduced, 100, 1), big.mark = ",", trim = TRUE)))

print(sprintf("Percentage reduction in orphanhood: %.2f%% [%.2f%% - %.2f%%]",
              round(100 - tot_reduced/tot * 100, digits = 2),
              round.choose(100-li, 0.01, 0),
              round.choose(100-ui, 0.01, 1)))
