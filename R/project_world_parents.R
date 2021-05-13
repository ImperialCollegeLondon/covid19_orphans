library(matrixStats)
library(tidyverse)
library(cowplot)
library(ggrepel)
library(stats)

calc_ratio <- function(alpha, beta, gamma, tfr){
  return(gamma * (exp(alpha + beta * tfr))/(1 + exp(alpha + beta * tfr)))
} 

error_parent<- function(params, data){
  return(sum((calc_ratio(params[1], params[2], params[3], data$tfr) - data$parent_ratio)^2))
}

set.seed(10)

# Load in data
joined <- readRDS("data/tfr_covariates.RDS")
joined$all_parents = joined$mother + joined$father + joined$both
joined$parent_ratio = joined$all_parents/joined$deaths

# Calculate country specific SD
joined$sd = (joined$tfr_u-joined$tfr_l)/(2*1.96)

# Exclude Iran from fit
subset <- joined[which(joined$parent_ratio != 0),]
subset = subset[which(!subset$country %in% c("I.R. Iran")),]
print(sprintf("Pearsons r^2 primary: %f",  cor(subset$tfr, subset$parent_ratio, method = "pearson") ))

pars = c(0.1, 0.1, 0.1)
output_p = optim(pars, error_parent, data=subset)
#saveRDS(output_p$par, "data/shiny/parent_coefficients.RDS")
print(output_p$par)

x = seq(0, 5, 0.1)
line_all = data.frame(x = x, 
                      y = calc_ratio(output_p$par[1], output_p$par[2], 
                                     output_p$par[3], x))
p_fit_pa <- ggplot(subset) + 
  geom_point(aes(tfr, parent_ratio)) + 
  geom_line(data = line_all, aes(x, y), col = 'black') + 
  xlab("Total fertility rate") + ylab("Ratio of children orphaned to deaths of parents") + 
  theme_bw()
p_fit_pa_label <- p_fit_pa + geom_text_repel(aes(x = tfr, y = parent_ratio, label = country), size = 3, max.overlaps = 100)

# Calculate deterministic number of orphans
joined$calculated_parent_ratio <- calc_ratio(output_p$par[1], output_p$par[2], 
                                              output_p$par[3], joined$tfr)
joined$calculated_orphans <- joined$calculated_parent_ratio * joined$fitting_deaths 
joined$final_parent_orphans <- ifelse(is.na(joined$all_parents), joined$calculated_orphans, joined$all_parents)

# Calculate uncertainty
n = 1000
estimates_parent <- matrix(nrow = length(joined$country), ncol = n)
estimates_parent_orphans <- matrix(nrow = length(joined$country), ncol = n)

for (i in 1:n){
  rn <- rnorm(length(joined$country), mean = joined$tfr, sd = joined$sd)
  estimates_parent[, i] <- calc_ratio(output_p$par[1], output_p$par[2], output_p$par[3], rn)
  estimates_parent_orphans[, i] <- estimates_parent[, i] * joined$fitting_deaths
}

joined$estimates_parent <- rowMeans(estimates_parent_orphans)
joined$final_parent_orphans_uq <- ifelse(joined$all == 0, joined$estimates_parent, joined$all_parents)

orphans_samples <- colSums(estimates_parent_orphans)
print(sprintf("Parent orphans: %0.f [%0.f - %0.f]", 
              sum(joined$final_parent_orphans), floor(quantile(orphans_samples, probs = 0.025)), 
              ceiling(quantile(orphans_samples, probs = 0.975))))

# Separate out by regions
mean = rowMeans(estimates_parent_orphans)
li = rowQuantiles(estimates_parent_orphans, probs = 0.025)
ui = rowQuantiles(estimates_parent_orphans, probs = 0.975)

orphans_sample = data.frame("country" = joined$country, 
                            "mean" = joined$final_parent_orphans, 
                            "lower" = li, 
                            "upper" = ui,
                            "text_pa" = sprintf("%.0f [%.0f - %.0f]", 
                                               signif(joined$final_parent_orphans, 2), 
                                               round.choose(li, 100, 0), 
                                               round.choose(ui, 100, 1)), 
                            "region" = joined$who_region)

# Exchange out study countries
orphans_sample$text_pa[joined$all != 0] <- joined$final_parent_orphans[joined$all != 0]
# Remove 0 countries
orphans_sample = orphans_sample[orphans_sample$mean > 0,]
# Sort
orphans_sample = orphans_sample[order(orphans_sample$region, orphans_sample$country),]
saveRDS(orphans_sample, "data/country_estimates_pa.RDS")

joined$colour = ifelse(joined$country == "I.R. Iran", 1, 0)
joined$colour = factor(joined$colour)
p_obs_pred_pa = ggplot(joined %>% filter(all != 0)) +
  geom_point(aes(parent_ratio, calculated_parent_ratio)) + 
  geom_point(data = joined %>% filter(country == "I.R. Iran"), 
             aes(parent_ratio, calculated_parent_ratio), col = "red") + 
  geom_abline(slope = 1, intercept = 0) + 
  xlab("Ratio of children orphaned to deaths of parents (observed)") + 
  ylab("Ratio of children orphaned to deaths of parents (predicted)") + 
  geom_text_repel(aes(x = parent_ratio, y = calculated_parent_ratio, label = country, col = colour),
                  size = 2, max.overlaps = 100) +
  scale_color_manual(values = c("black", "red")) + 
  theme_bw() + theme(legend.position = "none")

## Primary loo
pars = c(0.1, 0.1, 0.1)
loo_orphans = vector(length = length(subset$country))
p <- p_fit_pa
for (i in 1:length(subset$country)){
  leave_one_out = subset[which(subset$country != subset$country[i]),]
  output_loo_p = optim(pars, error_parent, data=leave_one_out)
  
  x = seq(0, 5, 0.1)
  line = data.frame(x = x, 
                    y = calc_ratio(output_loo_p$par[1], output_loo_p$par[2], 
                                   output_loo_p$par[3], x))
  p <- p + geom_line(data = line, aes(x, y), col = "blue", alpha = 0.2)
  
  joined$calculated_ratio_loo_parent <- calc_ratio(output_loo_p$par[1], output_loo_p$par[2], 
                                                    output_loo_p$par[3], joined$tfr)
  joined$calculated_orphans_loo <- joined$calculated_ratio_loo_parent * joined$fitting_deaths 
  joined$final_orphans_loo <- ifelse(is.na(joined$all_parents), joined$calculated_orphans_loo, joined$all_parents)
  loo_orphans[i] = sum(joined$final_orphans_loo)
}

loo_combined = data.frame("country" = subset$country,
                          "orphans" = loo_orphans)
p_loo_pa <- p + geom_line(data = line_all, aes(x, y), col = 'red') + 
  geom_text_repel(aes(x = tfr, y = parent_ratio, label = country), max.overlaps = 100)

print("Range loo")
print(loo_combined[which(loo_combined$orphans == min(loo_combined$orphans)),])
print(loo_combined[which(loo_combined$orphans == max(loo_combined$orphans)),])

save(p_fit_pa_label, p_obs_pred_pa, p_loo_pa, file = "data/extrapolate_parent.RData")



