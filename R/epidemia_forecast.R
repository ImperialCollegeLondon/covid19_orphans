weeks = 139

df <- fm$data
projstart <- max(df$date)

# time points in projection
future_dates <- seq.Date(projstart+1, projstart+(7*weeks), by='day')

df_new = rbind(df, data.frame(group = rep("World", length(future_dates)),
                              date =  future_dates,
                              week = rep(max(df$week), length(future_dates)),
                              daily_deaths = rep(-1, length(future_dates))))

endTime <- nrow(df_new)


# epi parameters
si <- c(EuropeCovid$si, rep(0, endTime - length(EuropeCovid$si)))
deaths_i2o <- c(EuropeCovid$inf2death, rep(0, endTime - length(EuropeCovid$inf2death)))
cases_i2o <- c(0,0,0, rep(1/10,10), rep(0, endTime - 13))

model_infections <- posterior_infections(fm)$draws
model_rt <- posterior_rt(fm, offset=FALSE, adjusted=FALSE)$draws
model_infections <- posterior_infections(fm)$draws
nsamples <- nrow(model_infections)
model_linearpred <- epidemia::posterior_linpred(fm, offset=FALSE)$draws ## BUG - this is the linear predictor and not rt unadjusted
raw_pop <- df$pop[1]


# creating vectors of importance but making sure we do not redo the calculations already done
infections <- cbind(model_infections, matrix(0, nrow=nsamples, ncol=length(future_dates)))
deaths <- matrix(0, nrow=nsamples, ncol=endTime)
Rt_unadj <- cbind(model_linearpred, matrix(0, nrow=nsamples, ncol=length(future_dates)))
Rt <- cbind(model_rt, matrix(0, nrow=nsamples, ncol=length(future_dates)))

intercept = as.matrix(fm, pars = "daily_deaths|(Intercept)")
ifr = 0.02*invlogit(intercept)

appendStart <- nrow(df) + 1
for (i in (appendStart):endTime) {
  datenow <- df_new$date[i]
  
  rt_val <- 1#rt_all[, i - nrow(df) - 1 + deltastart]
  #pop <- pop_vac_diff %>% filter(date==datenow) %>% .$susceptibles
  #pop_vac_inc <- pop_vac_diff$dose_effective_inc[1:(i-1)]
  convolution <- infections[, 1:(i-1)] %*% si[i:2] # infections[, 1:i] %*% si[i:1]
  cumsum_before <- rowSums(infections[, 1:(i-1)])
  
  # compute unadjusted rt
  Rt[, i] <- rt_val
  Rt_unadj[, i] <- Rt[, i] / ((raw_pop - cumsum_before) / raw_pop)
  
  # susceptible adjustment
  infections[, i] <- (raw_pop - cumsum_before)*(1-exp(-Rt_unadj[, i]*convolution/raw_pop))
  
  # Calculate observed values
  deaths[, i] <- infections[, 1:(i-1)] %*% deaths_i2o[(i-1):1] * ifr
}

# append results
infections_samples <- infections[, appendStart:endTime]
deaths_samples <- deaths[, appendStart:endTime]
rt_samples <- Rt[, appendStart:endTime]


d <- data.frame(dates = c(df$date, future_dates),
                deaths = c(df$daily_deaths,colMedians(deaths_samples)),
                lower = c(df$daily_deaths,colQuantiles(deaths_samples, probs = 0.025)),
                upper = c(df$daily_deaths,colQuantiles(deaths_samples, probs = 0.975)))
ggplot(d) + geom_line(aes(x=dates, y=deaths)) + geom_ribbon(aes(x=dates, ymin = lower, ymax = upper), fill = "blue", alpha = 0.5) + theme_bw()

 
median(rowSums(deaths_samples))
