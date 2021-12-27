library(matrixStats)
library(tidyverse)
library(cowplot)
library(ggrepel)
library(stats)

calc_ratio <- function(alpha, beta, gamma, delta, tfr, europe){
  return(delta * (exp(alpha + beta * tfr + gamma * europe))/(1 + exp(alpha + beta * tfr + gamma * europe)))
} 

error <- function(params, data){
  return(sum((calc_ratio(params[1], params[2], params[3], params[4], data$tfr, data$europe) - data$ratio)^2))
}

source("global_age_analysis_2021/R/utils.R")

base::set.seed(10)
# Load in data
joined <- readRDS("global_age_analysis_2021/data/tfr_covariates.RDS")

# Calculate country specific SD
joined$sd = (joined$tfr_u-joined$tfr_l)/(2*1.96)

joined$europe = ifelse(joined$who_region == "European", 1, 0)

# Select a subset
subset <- joined[which(joined$all != 0),]

prop_double <- sum(subset$mg_both + subset$sg_both + subset$both) / sum(subset$all)
print(sprintf("Primary + secondary prop double: %f", prop_double*100))

# Exclude Iran from fit
subset = subset[which(!subset$country %in% c("I.R. Iran")),]
print(sprintf("Pearsons r^2 primary and/or seconday: %f",  
              cor(subset$tfr, subset$ratio, method = "pearson") ))


# Fit primary and secondary
pars = c(-7, 1, 1, 1)
output_ps = optim(pars, error, data=subset, control = c(abstol = 1e-12))
joined$calculated_ratio <- calc_ratio(output_ps$par[1], output_ps$par[2], 
                                      output_ps$par[3], output_ps$par[4],
                                      joined$tfr, joined$europe)
print(output_ps$par)
saveRDS(output_ps$par, "global_age_analysis_2021/data/shiny/primary_secondary_coefficients.RDS")

ratio_dat <- select(joined, country, ratio, calculated_ratio)
ratio_dat$calculated_ratio <- ifelse(is.na(ratio_dat$ratio), ratio_dat$calculated_ratio , ratio_dat$ratio)
ratio_dat$ratio <- NULL
names(ratio_dat) <- c("country", "primary_secondary_ratio")

saveRDS(ratio_dat, "global_age_analysis_2021/data/primary_secondary_ratios.RDS")

# Calculate deterministic number of orphans
joined$calculated_ratio <- calc_ratio(output_ps$par[1], output_ps$par[2], 
                                      output_ps$par[3], output_ps$par[4],
                                      joined$tfr, joined$europe)
joined$calculated_orphans <- joined$calculated_ratio * joined$fitting_deaths 
joined$final_orphans <- ifelse(joined$all == 0, joined$calculated_orphans, joined$all)


# Calculate uncertainty around primary and secondary estimate
n = 1000
estimates <- matrix(nrow = length(joined$country), ncol = n)
estimates_orphans <- matrix(nrow = length(joined$country), ncol = n)

base::set.seed(10)

rn_tot = 0
mean_tot = 0 
sd_tot = 0 
for (i in 1:n){
  rn <- stats::rnorm(length(joined$country), mean = joined$tfr, sd = joined$sd)
  rn_tot = rn_tot+sum(rn)
  mean_tot = mean_tot + sum(joined$tfr)
  sd_tot = sd_tot + sum(joined$sd)
  estimates[, i] <- calc_ratio(output_ps$par[1], output_ps$par[2], output_ps$par[3], 
                               output_ps$par[4], rn, joined$europe)
  estimates_orphans[, i] <- estimates[, i] * joined$fitting_deaths
}
print(sprintf("rn total: %f, %f, %f", rn_tot, mean_tot, sd_tot))

orphans_samples <- colSums(estimates_orphans)
formatted <- sprintf("Primary and/or secondary orphans: %0.f [%0.f - %0.f]", 
                     round(sum(joined$final_orphans), -2), round.choose(quantile(orphans_samples, probs = 0.025), 100, 0), 
                     round.choose(quantile(orphans_samples, probs = 0.975), 100, 1))
print(formatted)
formatted_double <- sprintf("Primary and/or secondary double orphans: %0.f [%0.f - %0.f]", 
                            round(sum(joined$final_orphans)*prop_double, -2), round.choose(quantile(orphans_samples, probs = 0.025)*prop_double, 100, 0), 
                            round.choose(quantile(orphans_samples, probs = 0.975)*prop_double, 100, 1))
print(formatted_double)

saveRDS(sprintf("%s [%s - %s]", 
                format(round(sum(joined$final_orphans), -2), big.mark = ",", trim = TRUE), 
                format(round.choose(quantile(orphans_samples, probs = 0.025), 100, 0), big.mark = ",", trim = TRUE), 
                format(round.choose(quantile(orphans_samples, probs = 0.975), 100, 1), big.mark = ",", trim = TRUE)), 
        file = "global_age_analysis_2021/data/formatted_primary_secondary.RDS")
saveRDS(c(sum(joined$final_orphans), quantile(orphans_samples, probs = 0.025),  quantile(orphans_samples, probs = 0.975)), 
        file = "global_age_analysis_2021/data/un_formatted_primary_secondary.RDS")

estimates_ps_orphans_swapped <- estimates_orphans
estimates_ps_orphans_swapped[1:21,] <- joined$final_orphans[1:21]
orphans_samples_swapped <- colSums(estimates_ps_orphans_swapped)
saveRDS(orphans_samples_swapped, "global_age_analysis_2021/data/ps_total_samples.RDS")

orphanhood_country <- as.data.frame(estimates_orphans)
orphanhood_country$country <- ratio_dat$country
saveRDS(orphanhood_country, "global_age_analysis_2021/data/ps_samples.RDS")

x = seq(0, 5, 0.1)
line_all = data.frame(x = x, 
                      y0 = calc_ratio(output_ps$par[1], output_ps$par[2], 
                                      output_ps$par[3], output_ps$par[4], x, 0),
                      y1 = calc_ratio(output_ps$par[1], output_ps$par[2], 
                                      output_ps$par[3], output_ps$par[4], x, 1))
p_fit_ps <- ggplot(subset) + 
  geom_point(aes(tfr, ratio)) + 
  geom_line(data = line_all, aes(x, y0), col = 'black') + 
  geom_line(data = line_all, aes(x, y1), col = 'red') + 
  xlab("Total fertility rate") + 
  ylab("Ratio of children orphaned and/or losing caregivers (primary or secondary) \nto deaths") + 
  theme_bw()
p_fit_ps_label <- p_fit_ps + geom_text_repel(aes(x = tfr, y = ratio, label = country), size = 3, max.overlaps = 100)
print(p_fit_ps_label)

all_country_fit <- sum(joined$final_orphans)

# Separate out by countries
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
saveRDS(orphans_sample, "global_age_analysis_2021/data/country_estimates_ps.RDS")

# Group data for region totals
joined$who_region <- ifelse(joined$who_region == "Eastern European", "European", joined$who_region)
estimates_orphans[which(joined$all != 0),] = joined$final_orphans[which(joined$all != 0)]
reg_mean <- joined %>% group_by(who_region) %>% summarise(mean = sum(final_orphans))
reg_samples <- cbind(data.frame(estimates_orphans), joined$who_region)
reg_samples_ <- reg_samples %>%
  group_by(joined$who_region) %>%
  summarise_all(sum)
intervals = rowQuantiles(as.matrix(reg_samples_[,2:1000]), probs = c(0.025, 0.975))
reg <- cbind(reg_mean, intervals)
saveRDS(reg, "global_age_analysis_2021/data/age_outputs/reg_ps.RDS")

# Make plots
joined$colour = ifelse(joined$country == "I.R. Iran", 1, 0)
joined$colour = factor(joined$colour)
p_obs_pred_ps = ggplot(joined %>% filter(all != 0)) +
  geom_point(aes(ratio, calculated_ratio)) + 
  geom_point(data = joined %>% filter(country == "I.R. Iran"), aes(ratio, calculated_ratio), col = "red") + 
  geom_abline(slope = 1, intercept = 0) + 
  xlab("Ratio of children orphaned and/or losing caregivers (primary or secondary) \nto deaths (observed)") + 
  ylab("Ratio of children orphaned and/or losing caregivers (primary or secondary) \nto deaths (predicted)") + 
  geom_text_repel(aes(x = ratio, y = calculated_ratio, label = country, col = colour), size = 2, max.overlaps = 200) +
  scale_color_manual(values = c("black", "red")) + 
  theme_bw() + theme(legend.position = "none")

# Leave one out analysis
pars = c(-7, 1, 1, 1)
loo_orphans = vector(length = length(subset$country))
p <- p_fit_ps
for (i in 1:length(subset$country)){
  leave_one_out = subset[which(subset$country != subset$country[i]),]
  output_loo_ps = optim(pars, error, data=leave_one_out, control = c(abstol = 1e-12))
  #print(output_loo_ps$par)
  #x = seq(0, 5, 0.1)
  #line = data.frame(x = x,
  #                  y = calc_ratio(output_loo_ps$par[1], output_loo_ps$par[2],
  #                                 output_loo_ps$par[3], x))
  #p <- p + geom_line(data = line, aes(x, y), col = "blue", alpha = 0.2)
  
  joined$calculated_ratio_loo <- calc_ratio(output_loo_ps$par[1], output_loo_ps$par[2],
                                            output_loo_ps$par[3], output_loo_ps$par[4], 
                                            joined$tfr, joined$europe)
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

save(p_fit_ps_label, p_obs_pred_ps, file = "global_age_analysis_2021/data/extrapolate_primary_secondary.RData")


mae <- abs(loo_combined$orphans - all_country_fit)
print(sprintf("MAE LOO: %0.f", mean(mae)))

print(sprintf("Prop ps orphans study: %f", sum(subset$all)/sum(joined$final_orphans)*100))
