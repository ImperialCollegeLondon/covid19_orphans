library(matrixStats)
library(tidyverse)
library(cowplot)
library(ggrepel)
library(stats)

calc_ratio <- function(alpha, beta, gamma, delta, tfr, europe){
  return(delta * (exp(alpha + beta * tfr + gamma * europe))/(1 + exp(alpha + beta * tfr + gamma * europe)))
} 

error_primary <- function(params, data){
  return(sum((calc_ratio(params[1], params[2], params[3], params[4], data$tfr, data$europe) - data$primary_ratio)^2))
}

source("TheLancetCAH_global_age_analysis_2022/R/utils.R")
base::set.seed(10)
# Load in data
joined <- readRDS("TheLancetCAH_global_age_analysis_2022/data/tfr_covariates.RDS")

# Calculate country specific SD
joined$sd = (joined$tfr_u-joined$tfr_l)/(2*1.96)

joined$europe = ifelse(joined$who_region == "European", 1, 0)

# Calculate primary ratio
joined$primary <- ifelse(is.na(joined$primary), 0, joined$primary)
joined$primary_ratio <- joined$primary_loss/joined$deaths

# Exclude Iran from fit
subset <- joined[which(joined$primary_ratio != 0),]
subset = subset[which(!subset$country %in% c("I.R. Iran")),]
print(sprintf("Pearsons r^2 primary: %f",  cor(subset$tfr, subset$primary_ratio, method = "pearson") ))

prop_double <- sum(subset$sg_both + subset$both) / sum(subset$primary)
print(sprintf("Primary prop double: %f", prop_double*100))

pars = c(-1, 1, 1, 1)
output_p = stats::optim(pars, error_primary, data=subset, control = c(abstol = 1e-12))
print(output_p$par)
saveRDS(output_p$par, "TheLancetCAH_global_age_analysis_2022/data/shiny/primary_coefficients.RDS")

# Calculate deterministic number of orphans
joined$calculated_primary_ratio <- calc_ratio(output_p$par[1], output_p$par[2], 
                                              output_p$par[3], output_p$par[4],
                                              joined$tfr, joined$europe)
joined$calculated_orphans <- joined$calculated_primary_ratio * joined$fitting_deaths 
joined$final_primary_orphans <- ifelse(joined$primary == 0, joined$calculated_orphans, joined$primary)


ratio_dat <- select(joined, country, primary_ratio, calculated_primary_ratio)
ratio_dat$calculated_primary_ratio <- ifelse(is.na(ratio_dat$primary_ratio), 
                                             ratio_dat$calculated_primary_ratio , ratio_dat$primary_ratio)
ratio_dat$primary_ratio <- NULL
names(ratio_dat) <- c("country", "primary_ratio")
saveRDS(ratio_dat, "TheLancetCAH_global_age_analysis_2022/data/primary_ratios.RDS")

# Calculate uncertainty
n = 1000
estimates_primary <- matrix(nrow = length(joined$country), ncol = n)
estimates_primary_orphans <- matrix(nrow = length(joined$country), ncol = n)
tot_rn = 0
mean_tot = 0 
sd_tot = 0 
base::set.seed(10)
for (i in 1:n){
  rn <- rnorm(length(joined$country), mean = joined$tfr, sd = joined$sd)
  tot_rn <- tot_rn + sum(rn)
  mean_tot = mean_tot + sum(joined$tfr)
  sd_tot = sd_tot + sum(joined$sd)
  estimates_primary[, i] <- calc_ratio(output_p$par[1], output_p$par[2], output_p$par[3], output_p$par[4], rn, joined$europe)
  estimates_primary_orphans[, i] <- estimates_primary[, i] * joined$fitting_deaths
}
print(sprintf("rn total: %f, %f, %f", tot_rn, mean_tot, sd_tot))

orphans_samples <- colSums(estimates_primary_orphans)
formatted = sprintf("Primary orphans: %0.f [%0.f - %0.f]", 
              round(sum(joined$final_primary_orphans), -2), round.choose(quantile(orphans_samples, probs = 0.025), 100, 0), 
              round.choose(quantile(orphans_samples, probs = 0.975), 100, 1))
print(formatted)
formatted_double <- sprintf("Primary double orphans: %0.f [%0.f - %0.f]", 
                            round(sum(joined$final_primary_orphans)*prop_double, -2), round.choose(quantile(orphans_samples, probs = 0.025)*prop_double, 100, 0), 
                            round.choose(quantile(orphans_samples, probs = 0.975)*prop_double, 100, 1))
print(formatted_double)
saveRDS(sprintf("%s [%s - %s]", 
                format(round(sum(joined$final_primary_orphans), -2), big.mark = ",", trim = TRUE), 
                format(round.choose(quantile(orphans_samples, probs = 0.025), 100, 0), big.mark = ",", trim = TRUE), 
                format(round.choose(quantile(orphans_samples, probs = 0.975), 100, 1), big.mark = ",", trim = TRUE)), 
        file = "TheLancetCAH_global_age_analysis_2022/data/formatted_primary.RDS")
saveRDS(c(sum(joined$final_primary_orphans), quantile(orphans_samples, probs = 0.025),  quantile(orphans_samples, probs = 0.975)), 
        file = "TheLancetCAH_global_age_analysis_2022/data/un_formatted_primary.RDS")

estimates_primary_orphans_swapped <- estimates_primary_orphans
estimates_primary_orphans_swapped[1:21,] <- joined$final_primary_orphans[1:21]
orphans_samples_swapped <- colSums(estimates_primary_orphans_swapped)
saveRDS(orphans_samples_swapped, "TheLancetCAH_global_age_analysis_2022/data/p_total_samples.RDS")

all_country_fit <- sum(joined$final_primary_orphans)

orphanhood_country <- as.data.frame(estimates_primary_orphans)
orphanhood_country$country <- ratio_dat$country
saveRDS(orphanhood_country, "TheLancetCAH_global_age_analysis_2022/data/p_samples.RDS")
  
x = seq(0, 5, 0.1)
line_all = data.frame(x = x, 
                      y0 = calc_ratio(output_p$par[1], output_p$par[2], 
                                      output_p$par[3], output_p$par[4], x, 0),
                      y1 = calc_ratio(output_p$par[1], output_p$par[2], 
                                      output_p$par[3], output_p$par[4], x, 1))
p_fit_p <- ggplot(subset) + 
  geom_point(aes(tfr, primary_ratio)) + 
  geom_line(data = line_all, aes(x, y0), col = 'black') + 
  geom_line(data = line_all, aes(x, y1), col = 'red') + 
  xlab("Total fertility rate") + ylab("Ratio of children orphaned or losing primary caregivers \nto deaths") + 
  theme_bw()
p_fit_p_label <- p_fit_p + geom_text_repel(aes(x = tfr, y = primary_ratio, label = country), size = 3, max.overlaps = 100)
print(p_fit_p_label)

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
saveRDS(orphans_sample, "TheLancetCAH_global_age_analysis_2022/data/country_estimates_p.RDS")

# Group data for region totals
joined$who_region <- ifelse(joined$who_region == "Eastern European", "European", joined$who_region)
estimates_primary_orphans[which(joined$all != 0),] = joined$final_primary_orphans[which(joined$all != 0)]
reg_mean <- joined %>% group_by(who_region) %>% summarise(mean = sum(final_primary_orphans))
reg_samples <- cbind(data.frame(estimates_primary_orphans), joined$who_region)
reg_samples_ <- reg_samples %>%
  group_by(joined$who_region) %>%
  summarise_all(sum)
intervals = rowQuantiles(as.matrix(reg_samples_[,2:1000]), probs = c(0.025, 0.975))
reg <- cbind(reg_mean, intervals)
saveRDS(reg, "TheLancetCAH_global_age_analysis_2022/data/age_outputs/reg_p.RDS")
saveRDS(reg_samples_, "TheLancetCAH_global_age_analysis_2022/data/age_outputs/reg_p_samples.RDS")

joined$colour = ifelse(joined$country == "I.R. Iran", 1, 0)
joined$colour = factor(joined$colour)
p_obs_pred_p = ggplot(joined %>% filter(all != 0)) +
  geom_point(aes(primary_ratio, calculated_primary_ratio)) + 
  geom_point(data = joined %>% filter(country == "I.R. Iran"), 
             aes(primary_ratio, calculated_primary_ratio), col = "red") + 
  geom_abline(slope = 1, intercept = 0) + 
  xlab("Ratio of children orphaned or losing primary caregivers \nto deaths (observed)") + 
  ylab("Ratio of children orphaned or losing primary caregivers \nto deaths (predicted)") + 
  geom_text_repel(aes(x = primary_ratio, y = calculated_primary_ratio, label = country, col = colour), 
                  size = 2, max.overlaps = 100) +
  scale_color_manual(values = c("black", "red")) + 
  theme_bw() + theme(legend.position = "none")

## Primary loo
pars = c(-5, 1, 1, 1)
loo_orphans = vector(length = length(subset$country))
#p <- p_fit_p
for (i in 1:length(subset$country)){
  leave_one_out = subset[which(subset$country != subset$country[i]),]
  output_loo_p = optim(pars, error_primary, data=leave_one_out, control = c(abstol = 1e-12))
  #print(output_loo_p$par)
  #x = seq(0, 5, 0.1)
  #line = data.frame(x = x,
  #                  y = calc_ratio(output_loo_p$par[1], output_loo_p$par[2],
  #                                 output_loo_p$par[3], x))
  #p <- p + geom_line(data = line, aes(x, y), col = "blue", alpha = 0.2)

  joined$calculated_ratio_loo_primary <- calc_ratio(output_loo_p$par[1], output_loo_p$par[2],
                                                    output_loo_p$par[3], output_loo_p$par[4],
                                                    joined$tfr, joined$europe)
  joined$calculated_orphans_loo <- joined$calculated_ratio_loo_primary * joined$fitting_deaths
  joined$final_orphans_loo <- ifelse(joined$primary == 0, joined$calculated_orphans_loo, joined$primary)
  loo_orphans[i] = sum(joined$final_orphans_loo)
}

loo_combined = data.frame("country" = subset$country,
                          "orphans" = loo_orphans)
#p_loo_p <- p + geom_line(data = line_all, aes(x, y), col = 'red') +
  #geom_text_repel(aes(x = tfr, y = primary_ratio, label = country), max.overlaps = 100)

print("Range loo")
print(loo_combined[which(loo_combined$orphans == min(loo_combined$orphans)),])
print(loo_combined[which(loo_combined$orphans == max(loo_combined$orphans)),])

save(p_fit_p_label, p_obs_pred_p, file = "TheLancetCAH_global_age_analysis_2022/data/extrapolate_primary.RData")

mae <- abs(loo_combined$orphans - all_country_fit)
print(sprintf("MAE LOO: %0.f", mean(mae)))

print(sprintf("Prop p orphans study: %f", sum(subset$primary)/sum(joined$final_primary_orphans)*100))
