weeks = 139
rt_val <- 1.0

df <- fm$data
projstart <- max(df$date)

# time points in projection
future_dates <- seq.Date(projstart+1, projstart+(7*weeks), by='day')
df_new = rbind(df, data.frame(group = rep("World", length(future_dates)),
                              date =  future_dates,
                              week = rep(max(df$week), length(future_dates)),
                              daily_deaths = rep(-1, length(future_dates))))

endTime <- nrow(df_new)
raw_pop <- df$pop[1]

# epi parameters
si <- c(EuropeCovid$si, rep(0, endTime - length(EuropeCovid$si)))
deaths_i2o <- c(EuropeCovid$inf2death, rep(0, endTime - length(EuropeCovid$inf2death)))

model_linearpred <- epidemia::posterior_linpred(fm, offset=FALSE)$draws 
model_infections <- posterior_infections(fm)$draws
model_rt <- posterior_rt(fm, offset=FALSE, adjusted=FALSE)$draws
nsamples <- nrow(model_infections)


# creating vectors of importance but making sure we do not redo the calculations already done
infections <- cbind(model_infections, matrix(0, nrow=nsamples, ncol=length(future_dates)))
deaths <- matrix(0, nrow=nsamples, ncol=endTime)
Rt_adj <- cbind(model_linearpred, matrix(0, nrow=nsamples, ncol=length(future_dates)))
Rt <- cbind(model_rt, matrix(0, nrow=nsamples, ncol=length(future_dates)))

intercept = as.matrix(fm, pars = "daily_deaths|(Intercept)")
ifr = 0.02*invlogit(intercept)

appendStart <- nrow(df) + 1
for (i in (appendStart):endTime) {
  datenow <- df_new$date[i]

  convolution <- infections[, 1:(i-1)] %*% si[i:2] # infections[, 1:i] %*% si[i:1]
  cumsum <- rowSums(infections[, 1:(i-1)])
  
  # compute unadjusted rt
  #Rt[, i] <- rt_val
  #Rt_adj[, i] <- 1.1#Rt[, i] / ((raw_pop - cumsum) / raw_pop)
  
  # susceptible adjustment
  #infections[, i] <- (raw_pop - cumsum)*(1-exp(-Rt_adj[, i]*convolution/raw_pop))
  infections[, i] <- infections[,i] + rt_val * convolution
  
  # Calculate observed values
  deaths[, i] <- infections[, 1:(i-1)] %*% deaths_i2o[(i-1):1] * ifr
}

# append results
infections_samples <- infections[, appendStart:endTime]
deaths_samples <- deaths[, appendStart:endTime]
rt_samples <- Rt[, appendStart:endTime]
rt_adj_samples <- Rt_adj[, appendStart:endTime]


d <- data.frame(dates = c(df$date, future_dates),
                deaths = c(df$daily_deaths,colMedians(deaths_samples)),
                infections = c(df$daily_deaths,colMedians(infections_samples)),
                lower = c(df$daily_deaths,colQuantiles(deaths_samples, probs = 0.025)),
                upper = c(df$daily_deaths,colQuantiles(deaths_samples, probs = 0.975)),
                rt_adj = c(rep(NA, length(df$daily_deaths)), colMedians(rt_adj_samples)))
p <- ggplot(d) + geom_line(aes(x=dates, y=infections)) #+ geom_ribbon(aes(x=dates, ymin = lower, ymax = upper), fill = "blue", alpha = 0.5) + theme_bw()
print(p)
 
median(rowSums(deaths_samples))
