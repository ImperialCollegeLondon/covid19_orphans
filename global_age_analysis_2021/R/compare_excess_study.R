#df=read.csv("https://raw.githubusercontent.com/TheEconomist/covid-19-the-economist-global-excess-deaths-model/main/output-data/export_country_cumulative.csv")
#saveRDS(df, file = "global_age_analysis_2021/data/exces_deaths.RDS")

library(countrycode)
library(tidyverse)

# Read excess deaths
excess = readRDS("global_age_analysis_2021/data/exces_deaths.RDS")
excess$country = countrycode(excess$iso3c, "iso3c", 
                          "country.name")
excess_oct = excess[which(excess$date == as.Date("2021-11-01")),]
excess_oct$country[which(excess_oct$country == "Iran")] <- "I.R. Iran"
excess_oct$country[which(excess_oct$country == "United States")] <- "USA"
excess_oct$country[which(excess_oct$country == "United Kingdom")] <- "England & Wales"
excess_oct$country[which(excess_oct$country == "Russia")] <- "Russian Federation"

# Read study deaths
study_data =  readRDS("~/Documents/covid19/orphans/fork/covid19_orphans_fork/global_age_analysis_2021/data/tfr_covariates.RDS")
study_data$deaths[which(study_data$country == "England & Wales")] <- study_data$deaths[which(study_data$country == "England & Wales")] + 
  study_data$fitting_deaths[which(study_data$country == "Scotland & Northern Ireland")]
study_data = study_data[which(study_data$all != 0),]

deaths_study = select(study_data, country, deaths)

comb_data = left_join(deaths_study, excess_oct,by = "country")
comb_data$cumulative_estimated_daily_excess_deaths_ci_95_bot = ifelse(comb_data$cumulative_estimated_daily_excess_deaths_ci_95_bot < 0, 0, 
                                                                      comb_data$cumulative_estimated_daily_excess_deaths_ci_95_bot)
p <- ggplot(comb_data) +
  geom_point(aes(deaths, cumulative_estimated_daily_excess_deaths)) + geom_abline(slope = 1) + 
  geom_errorbar(aes(x = deaths, ymin = cumulative_estimated_daily_excess_deaths_ci_95_bot, 
                    ymax = cumulative_estimated_daily_excess_deaths_ci_95_top)) + geom_abline(slope = 1) + 
  ggrepel::geom_text_repel(aes(deaths, cumulative_estimated_daily_excess_deaths,label = country), max.overlaps  = 20) + 
  xlab("Our study deaths") + ylab("Economist excess deaths") + scale_x_log10() + scale_y_log10() + theme_bw()
print(p)
