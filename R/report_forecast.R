library(epidemia)
library(dplyr)
library(rstanarm)
library(matrixStats)
library(ggplot2)
library(COVID19)

data <- covid19(
  country = NULL,
  level = 1,
  start = "2010-01-01",
  end = "2021-04-30",
)

data <- data %>%
  group_by(date) %>%
  summarise(deaths = sum(deaths, na.rm = TRUE),
            hosp = sum(deaths,na.rm = TRUE),
            vent = sum(deaths, na.rm = TRUE),
            icu = sum(deaths,na.rm = TRUE),
            population = sum(population, na.rm = TRUE))
data$population <- max(data$population)# Should I use the actual population of the world or the ones for the countries included in this?
data$week = c(rep(1:(length(data$date)%/%7), each = 7), 
           rep(length(data$date)%/%7, length(data$date)%%7))
data$place = "World"
#data <- data[which(data$week < 20),]
data$daily_deaths = c(data$deaths[1], diff(data$deaths))
data$daily_deaths[which(data$daily_deaths < 0)] = 0 # there will be 3 spikes as 3 negative days

# Sets observation
deaths <- epiobs(formula = daily_deaths ~ 1,
                 prior_intercept = rstanarm::normal(0,0.2), # this encodes the ifr.  assumed to be between 0 and 2, with mean of 1
                 link = scaled_logit(0.02),
                 i2o = EuropeCovid$inf2death)

data$pop <- pop <-  as.numeric(7554158217)#data$population[1]


# Sets infections
inf <- epiinf(gen = EuropeCovid$si,
              seed_days = 6,
              pop_adjust = TRUE,
              pops = pop)#,
              #prior_susc = normal(0.49, 0.1), prior_seeds = normal(15e3, 2e3))

#inf <- epiinf(gen = EuropeCovid$si, seed_days=20L, pop_adjust = TRUE, pops = pop,
 #             prior_susc = normal(0.49, 0.1), prior_seeds = normal(15e3, 2e3))

# Sets rt formulation
rt <- epirt(formula = R(place, date) ~ rw(time = week, prior_scale = 0.1),
                link = 'log')

options(mc.cores = parallel::detectCores())

args <- list(rt=rt, inf=inf, obs=deaths, data=data, seed=12345,
             algorithm = "sampling",
             iter = 100, control = list(max_treedepth = 15))

print("Solving model")
fm <- do.call(epim, args)

print("Making plots")
print(plot_rt(fm, plotly=FALSE))
print(plot_obs(fm, type = "daily_deaths", plotly=FALSE))
print(plot_infections(fm, plotly=FALSE))

# Forecast
fut <- tibble(date = max(data$date) + seq_len(900), 
              deaths = -1, hosp = -1,  vent =  -1, icu = -1, population = data$population[1], pop = data$population[1],
              week = max(data$week), place = "World", daily_deaths = -1
              )
newdata <- rbind(data, fut)
print(plot_obs(fm, type = "daily_deaths", newdata = newdata, plotly=FALSE)+  ylim(0, 10e4) )

d <- plot_obs(fm, type = "daily_deaths", newdata = newdata, plotly=FALSE)
a <- d$data

print(plot_rt(fm, newdata = newdata, plotly=FALSE))

saveRDS(fm, "epidemia_fit.RDS")
