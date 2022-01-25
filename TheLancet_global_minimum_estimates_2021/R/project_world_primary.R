library(matrixStats)
library(tidyverse)
library(cowplot)
library(ggrepel)
library(stats)

calc_ratio <- function(alpha, beta, gamma, tfr){
  return(gamma * (exp(alpha + beta * tfr))/(1 + exp(alpha + beta * tfr)))
} 

error_primary <- function(params, data){
  return(sum((calc_ratio(params[1], params[2], params[3], data$tfr) - data$primary_ratio)^2))
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
joined <- readRDS("TheLancet_global_minimum_estimates_2021/data/tfr_covariates.RDS")

# Calculate country specific SD
joined$sd = (joined$tfr_u-joined$tfr_l)/(2*1.96)

# Calculate primary ratio
joined$primary <- ifelse(is.na(joined$primary), 0, joined$primary)
joined$primary_ratio <- joined$primary_loss/joined$deaths

# Exclude Iran from fit
subset <- joined[which(joined$primary_ratio != 0),]
subset = subset[which(!subset$country %in% c("I.R. Iran")),]
print(sprintf("Pearsons r^2 primary: %f",  cor(subset$tfr, subset$primary_ratio, method = "pearson") ))

pars = c(0.1, 0.1, 0.1)
output_p = optim(pars, error_primary, data=subset)
print(output_p$par)

x = seq(0, 5, 0.1)
line_all = data.frame(x = x, 
                      y = calc_ratio(output_p$par[1], output_p$par[2], 
                                     output_p$par[3], x))
p_fit_p <- ggplot(subset) + 
  geom_point(aes(tfr, primary_ratio)) + 
  geom_line(data = line_all, aes(x, y), col = 'black') + 
  xlab("Total fertility rate") + ylab("Ratio of children orphaned or losing primary caregivers \nto deaths of parents or primary caregivers") + 
  theme_bw()
p_fit_p_label <- p_fit_p + geom_text_repel(aes(x = tfr, y = primary_ratio, label = country), size = 3, max.overlaps = 100)

# Calculate deterministic number of orphans
joined$calculated_primary_ratio <- calc_ratio(output_p$par[1], output_p$par[2], 
                                              output_p$par[3], joined$tfr)
joined$calculated_orphans <- joined$calculated_primary_ratio * joined$fitting_deaths 
joined$final_primary_orphans <- ifelse(joined$primary == 0, joined$calculated_orphans, joined$primary)


ratio_dat <- select(joined, country, primary_ratio, calculated_primary_ratio)
ratio_dat$calculated_primary_ratio <- ifelse(is.na(ratio_dat$primary_ratio), ratio_dat$calculated_primary_ratio , ratio_dat$primary_ratio)
ratio_dat$primary_ratio <- NULL
names(ratio_dat) <- c("country", "primary_ratio")
saveRDS(ratio_dat, "TheLancet_global_minimum_estimates_2021/data/primary_ratios.RDS")

# Calculate uncertainty
n = 1000
estimates_primary <- matrix(nrow = length(joined$country), ncol = n)
estimates_primary_orphans <- matrix(nrow = length(joined$country), ncol = n)

for (i in 1:n){
  rn <- rnorm(length(joined$country), mean = joined$tfr, sd = joined$sd)
  estimates_primary[, i] <- calc_ratio(output_p$par[1], output_p$par[2], output_p$par[3], rn)
  estimates_primary_orphans[, i] <- estimates_primary[, i] * joined$fitting_deaths
}

orphans_samples <- colSums(estimates_primary_orphans)
print(sprintf("Primary orphans: %0.f [%0.f - %0.f]", 
              sum(joined$final_primary_orphans), floor(quantile(orphans_samples, probs = 0.025)), 
              ceiling(quantile(orphans_samples, probs = 0.975))))

# Separate out by regions
mean = rowMeans(estimates_primary_orphans)
li = rowQuantiles(estimates_primary_orphans, probs = 0.025)
ui = rowQuantiles(estimates_primary_orphans, probs = 0.975)

orphans_sample = data.frame("country" = joined$country, 
                            "mean" = joined$final_primary_orphans, 
                            "lower" = li, 
                            "upper" = ui,
                            "text_p" = sprintf("%.0f [%.0f - %.0f]", 
                                                signif(joined$final_primary_orphans, 2), 
                                                round.choose(li, 100, 0), 
                                                round.choose(ui, 100, 1)), 
                            "region" = joined$who_region)

# Exchange out study countries
orphans_sample$text_p <- as.character(orphans_sample$text_p)
orphans_sample$text_p[joined$all != 0] <- joined$final_primary_orphans[joined$all != 0]
# Remove 0 countries
orphans_sample = orphans_sample[orphans_sample$mean > 0,]
# Sort
orphans_sample = orphans_sample[order(orphans_sample$region, orphans_sample$country),]
saveRDS(orphans_sample, "TheLancet_global_minimum_estimates_2021/data/country_estimates_p.RDS")

joined$colour = ifelse(joined$country == "I.R. Iran", 1, 0)
joined$colour = factor(joined$colour)
p_obs_pred_p = ggplot(joined %>% filter(all != 0)) +
  geom_point(aes(primary_ratio, calculated_primary_ratio)) + 
  geom_point(data = joined %>% filter(country == "I.R. Iran"), 
             aes(primary_ratio, calculated_primary_ratio), col = "red") + 
  geom_abline(slope = 1, intercept = 0) + 
  xlab("Ratio of children orphaned or losing primary caregivers \nto deaths of parents or primary caregivers (observed)") + 
  ylab("Ratio of children orphaned or losing primary caregivers \nto deaths of parents or primary caregivers (predicted)") + 
  geom_text_repel(aes(x = primary_ratio, y = calculated_primary_ratio, label = country, col = colour), 
                  size = 2, max.overlaps = 100) +
  scale_color_manual(values = c("black", "red")) + 
  theme_bw() + theme(legend.position = "none")

## Primary loo
pars = c(0.1, 0.1, 0.1)
loo_orphans = vector(length = length(subset$country))
p <- p_fit_p
for (i in 1:length(subset$country)){
  leave_one_out = subset[which(subset$country != subset$country[i]),]
  output_loo_p = optim(pars, error_primary, data=leave_one_out)
  
  x = seq(0, 5, 0.1)
  line = data.frame(x = x, 
                    y = calc_ratio(output_loo_p$par[1], output_loo_p$par[2], 
                                   output_loo_p$par[3], x))
  p <- p + geom_line(data = line, aes(x, y), col = "blue", alpha = 0.2)
  
  joined$calculated_ratio_loo_primary <- calc_ratio(output_loo_p$par[1], output_loo_p$par[2], 
                                                    output_loo_p$par[3], joined$tfr)
  joined$calculated_orphans_loo <- joined$calculated_ratio_loo_primary * joined$fitting_deaths 
  joined$final_orphans_loo <- ifelse(joined$primary == 0, joined$calculated_orphans_loo, joined$primary)
  loo_orphans[i] = sum(joined$final_orphans_loo)
}

loo_combined = data.frame("country" = subset$country,
                          "orphans" = loo_orphans)
p_loo_p <- p + geom_line(data = line_all, aes(x, y), col = 'red') + 
  geom_text_repel(aes(x = tfr, y = primary_ratio, label = country), max.overlaps = 100)

print("Range loo")
print(loo_combined[which(loo_combined$orphans == min(loo_combined$orphans)),])
print(loo_combined[which(loo_combined$orphans == max(loo_combined$orphans)),])

save(p_fit_p_label, p_obs_pred_p, p_loo_p, file = "TheLancet_global_minimum_estimates_2021/data/extrapolate_primary.RData")



