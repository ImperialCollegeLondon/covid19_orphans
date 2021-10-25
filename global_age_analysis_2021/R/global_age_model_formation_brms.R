# age covariates
# aim: predict proportion of orphans in each age category based on various factors
# covariates: number of orphans (get out of other model), 
#             percentage of population in different age groups,

library(tidyverse)
library(ggrepel)
library(nnet)
library(brms)

source("R/brms_joint_fit.R")

data = readRDS("age_data_scaled.RDS")

# Read in TFR
covariarates = readRDS("data/tfr_covariates.RDS")
covariarates = select(covariarates, country, who_region, fitting_deaths, tfr)
covariarates$country[which(covariarates$country == "I.R. Iran")] = "Iran (Islamic Republic of)"
data = left_join(data, covariarates, by = "country")

# Read in population
population = readRDS("data/bespoke_population_bands.RDS")
population_sub = select(population, Country.Area.Name, age, prop)
population_sub_wide = spread(population_sub, key = "age", value = "prop")
population_sub_wide$Country.Area.Name[which(population_sub_wide$Country.Area.Name =="United States")] <- "USA"
population_sub_wide$Country.Area.Name[which(population_sub_wide$Country.Area.Name =="Iran")] <- "Iran (Islamic Republic of)"
data = left_join(data, population_sub_wide, by = c("country"="Country.Area.Name"))

# Read in GDP
gdp <- read.csv("data/gdp.csv", header = FALSE) #(https://data.worldbank.org/indicator/NY.GDP.MKTP.CD)
gdp <- gdp[4:269, c(1, 65)]
names(gdp) <- c("country", "gdp")
gdp$country[which(gdp$country =="United Kingdom")] <- "England & Wales"
gdp$country[which(gdp$country =="United States")] <- "USA"
gdp$country[which(gdp$country =="Iran, Islamic Rep.")] <- "Iran (Islamic Republic of)"
data = left_join(data, gdp, by = "country")
data$gdp = data$gdp / max(data$gdp)

# 
# # Read in development index
# hdi <- read.csv("data/human-development-index.csv") #owid
# hdi = hdi[which(hdi$Year == 2017),]
# hdi$Entity[which(hdi$Entity =="United Kingdom")] <- "England & Wales"
# hdi$Entity[which(hdi$Entity =="United States")] <- "USA"
# hdi$Entity[which(hdi$Entity =="Iran")] <- "Iran (Islamic Republic of)"
# hdi = select(hdi, "Entity" , "Human.Development.Index..UNDP.")
# names(hdi) = c("country", "hdi")
# data = left_join(data, hdi, by = "country")
# 
# # Read in death ratio
# ratio = readRDS("data/death_ratio.RDS")
# data = left_join(data, ratio, by = "country")

names(data) <- c("category", "gender", "li_raw", "ui_raw", "li_percent",  "ui_percent", 
                 "raw", "percent", "country", "who_region", "deaths", "tfr", "pre_school",
                 "secondary_school", "parents","old_parents", "primary_school" , "grandparents",
                 "gdp")   




# response = select(data, category, gender, raw, country, parents, old_parents, grandparents)
# response_wide = pivot_wider(response, names_from = c("category", "gender"), values_from = raw)
# response_wide[,5:10] <- round(response_wide[,5:10])
# response_wide$N <- rowSums(response_wide[, 5:10])
# response_wide$response <- with(response_wide,
#                                cbind(`[0-5)_Female`, `[0-5)_Male`, `[5-10)_Female`,
#                                      `[5-10)_Male`, `[10-18)_Female`, `[10-18)_Male`))
# formula = response | trials(N) ~ parents + old_parents + grandparents
# print(paste("Model 1:", formula[3]))
# joint_fit(data, response_wide, formula, plot = FALSE)
# 
# 
# response = select(data, category, gender, raw, country, parents, old_parents, grandparents, gdp)
# response_wide = pivot_wider(response, names_from = c("category", "gender"), values_from = raw)
# response_wide[,6:11] <- round(response_wide[,6:11])
# response_wide$N <- rowSums(response_wide[, 6:11])
# response_wide$response <- with(response_wide, 
#                                cbind(`[0-5)_Female`, `[0-5)_Male`, `[5-10)_Female`, 
#                                      `[5-10)_Male`, `[10-18)_Female`, `[10-18)_Male`))
# formula = response | trials(N) ~ parents + old_parents + grandparents + gdp
# print(paste("Model 2:", formula[3]))
# joint_fit(data, response_wide, formula, plot = TRUE)


response = select(data, category, gender, raw, country, parents, old_parents, grandparents,
                  pre_school, primary_school, secondary_school, gdp)
response_wide = pivot_wider(response, names_from = c("category", "gender"), values_from = raw)
response_wide[,9:14] <- round(response_wide[,9:14])
response_wide$N <- rowSums(response_wide[, 9:14])
response_wide$response <- with(response_wide, 
                               cbind(`[0-5)_Female`, `[0-5)_Male`, `[5-10)_Female`, 
                                     `[5-10)_Male`, `[10-18)_Female`, `[10-18)_Male`))
formula = response | trials(N) ~ parents + old_parents + grandparents + pre_school + primary_school + secondary_school + gdp
print(paste("Model 3:", formula[3]))
fit = joint_fit(data, response_wide, formula, plot = TRUE)


# response = select(data, category, gender, raw, country, parents, old_parents, grandparents,
#                   pre_school, primary_school, secondary_school)
# response_wide = pivot_wider(response, names_from = c("category", "gender"), values_from = raw)
# response_wide[,8:13] <- round(response_wide[,8:13])
# response_wide$N <- rowSums(response_wide[, 8:13])
# response_wide$response <- with(response_wide, 
#                                cbind(`[0-5)_Female`, `[0-5)_Male`, `[5-10)_Female`, 
#                                      `[5-10)_Male`, `[10-18)_Female`, `[10-18)_Male`))
# formula =  response | trials(N) ~ parents + old_parents + grandparents + pre_school + primary_school + secondary_school
# print(paste("Model 4:", formula[3]))
# joint_fit(data, response_wide, formula, plot = FALSE)

