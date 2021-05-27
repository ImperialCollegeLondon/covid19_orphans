library(matrixStats)
library(tidyverse)
library(cowplot)
library(ggrepel)
library(stats)

calc_ratio <- function(alpha, beta, gamma, tfr){
  return(gamma * (exp(alpha + beta * tfr))/(1 + exp(alpha + beta * tfr)))
} 

error <- function(params, data){
  return(sum((calc_ratio(params[1], params[2], params[3], data$tfr) - data$ratio)^2))
}

round.choose <- function(x, roundTo, dir = 1) {
  if(dir == 1) {  ##ROUND UP
    x + (roundTo - x %% roundTo)
  } else {
    if(dir == 0) {  ##ROUND DOWN
      x - (x %% roundTo)
    }
  }
}

set.seed(10)

# Load in data
joined <- readRDS("data/tfr_covariates.RDS")

# Calculate country specific SD
joined$sd = (joined$tfr_u-joined$tfr_l)/(2*1.96)

# Select a subset
subset <- joined[which(joined$all != 0),]

# Exclude Iran from fit
subset = subset[which(!subset$country %in% c("I.R. Iran")),]
print(sprintf("Pearsons r^2 primary and/or seconday: %f",  
              cor(subset$tfr, subset$ratio, method = "pearson") ))


# Fit primary and secondary
pars = c(0.1, 0.1, 0.1)
output_ps = optim(pars, error, data=subset)
joined$calculated_ratio <- calc_ratio(output_ps$par[1], output_ps$par[2], 
                                      output_ps$par[3], joined$tfr)
#saveRDS(output_ps$par, "data/shiny/primary_secondary_coefficients.RDS")
print(output_ps$par)

x = seq(0, 5, 0.1)
line_all = data.frame(x = x, 
                      y = calc_ratio(output_ps$par[1], output_ps$par[2], 
                                     output_ps$par[3], x))
p_fit_ps <- ggplot(subset) + 
  geom_point(aes(tfr, ratio)) + 
  geom_line(data = line_all, aes(x, y), col = 'black') + 
  xlab("Total fertility rate") + 
  ylab("Ratio of children orphaned and/or losing caregivers (primary or secondary) \nto deaths of parents and/or caregivers (primary or secondary)") + 
  theme_bw()
p_fit_ps_label <- p_fit_ps + geom_text_repel(aes(x = tfr, y = ratio, label = country), size = 3, max.overlaps = 100)

# Calculate deterministic number of orphans
joined$calculated_ratio <- calc_ratio(output_ps$par[1], output_ps$par[2], 
                                      output_ps$par[3], joined$tfr)
joined$calculated_orphans <- joined$calculated_ratio * joined$fitting_deaths 
joined$final_orphans <- ifelse(joined$all == 0, joined$calculated_orphans, joined$all)

# Calculate uncertainty around primary and secondary estimate
n = 1000
estimates <- matrix(nrow = length(joined$country), ncol = n)
estimates_orphans <- matrix(nrow = length(joined$country), ncol = n)

rn2 = NULL
ratio_fit = NULL
for (i in 1:n){
  rn <- rnorm(length(joined$country), mean = joined$tfr, sd = joined$sd)
  rn2 = cbind(rn2, rn)
  estimates[, i] <- calc_ratio(output_ps$par[1], output_ps$par[2], output_ps$par[3], 
                               rn)
  ratio_fit = cbind(ratio_fit,  estimates[, i])
  estimates_orphans[, i] <- estimates[, i] * joined$fitting_deaths
}

min = rowMins(ratio_fit)
max = rowMaxs(ratio_fit)

d = data.frame("country" = joined$country,
               "fit" = joined$calculated_ratio,
               "min" = min,
               "max" = max)

joined$estimates <- rowMeans(estimates_orphans)

orphans_samples <- colSums(estimates_orphans)
print(sprintf("Primary and/or secondary orphans: %0.f [%0.f - %0.f]", 
              sum(joined$final_orphans), floor(quantile(orphans_samples, probs = 0.025)), 
              ceiling(quantile(orphans_samples, probs = 0.975))))

# Separate out by regions
mean = rowMeans(estimates_orphans)
li = rowQuantiles(estimates_orphans, probs = 0.025)
ui = rowQuantiles(estimates_orphans, probs = 0.975)

orphans_sample = data.frame("country" = joined$country, 
                            "mean" = joined$final_orphans, 
                            "lower" = li, 
                            "upper" = ui,
                            "text_ps" = sprintf("%.0f [%.0f - %.0f]", 
                                             signif(joined$final_orphans, 2), 
                                             round.choose(li, 100, 0), 
                                             round.choose(ui, 100, 1)), 
                            "region" = joined$who_region)

orphans_sample$text_ps <- as.character(orphans_sample$text_ps)

# Exchange out study countries
orphans_sample$text_ps[joined$all != 0] <- joined$final_orphans[joined$all != 0]
# Remove 0 countries
orphans_sample = orphans_sample[orphans_sample$mean > 0,]
# Sort
orphans_sample = orphans_sample[order(orphans_sample$region, orphans_sample$country),]
saveRDS(orphans_sample, "data/country_estimates_ps.RDS")

# Make plots
joined$colour = ifelse(joined$country == "I.R. Iran", 1, 0)
joined$colour = factor(joined$colour)
p_obs_pred_ps = ggplot(joined %>% filter(all != 0)) +
  geom_point(aes(ratio, calculated_ratio)) + 
  geom_point(data = joined %>% filter(country == "I.R. Iran"), aes(ratio, calculated_ratio), col = "red") + 
  geom_abline(slope = 1, intercept = 0) + 
  xlab("Ratio of children orphaned and/or losing caregivers (primary or secondary) \nto deaths of parents and/or caregivers (primary or secondary) (observed)") + 
  ylab("Ratio of children orphaned and/or losing caregivers (primary or secondary) \nto deaths of parents and/or caregivers (primary or secondary) (predicted)") + 
  geom_text_repel(aes(x = ratio, y = calculated_ratio, label = country, col = colour), size = 2, max.overlaps = 200) +
  scale_color_manual(values = c("black", "red")) + 
  theme_bw() + theme(legend.position = "none")

# Leave one out analysis
pars = c(0.1, 0.1, 0.1)
loo_orphans = vector(length = length(subset$country))
p <- p_fit_ps
for (i in 1:length(subset$country)){
  leave_one_out = subset[which(subset$country != subset$country[i]),]
  output_loo_ps = optim(pars, error, data=leave_one_out)
  
  x = seq(0, 5, 0.1)
  line = data.frame(x = x, 
                    y = calc_ratio(output_loo_ps$par[1], output_loo_ps$par[2], 
                                   output_loo_ps$par[3], x))
  p <- p + geom_line(data = line, aes(x, y), col = "blue", alpha = 0.2)

  joined$calculated_ratio_loo <- calc_ratio(output_loo_ps$par[1], output_loo_ps$par[2], 
                                            output_loo_ps$par[3], joined$tfr)
  joined$calculated_orphans_loo <- joined$calculated_ratio_loo * joined$fitting_deaths 
  joined$final_orphans_loo <- ifelse(joined$all == 0, joined$calculated_orphans_loo, joined$all)
  loo_orphans[i] = sum(joined$final_orphans_loo)
}

loo_combined = data.frame("country" = subset$country,
                          "orphans" = loo_orphans)
p_loo_ps <- p + geom_line(data = line_all, aes(x, y), col = 'red') + 
  geom_text_repel(aes(x = tfr, y = ratio, label = country), max.overlaps = 100)

print("Range loo")
print(loo_combined[which(loo_combined$orphans == min(loo_combined$orphans)),])
print(loo_combined[which(loo_combined$orphans == max(loo_combined$orphans)),])

save(p_fit_ps_label, p_obs_pred_ps, p_loo_ps, file = "data/extrapolate_primary_secondary.RData")

