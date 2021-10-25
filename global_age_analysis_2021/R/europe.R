library(matrixStats)
library(tidyverse)
library(cowplot)
library(ggrepel)
library(stats)
library(ggplot2)

set.seed(10)

calc_ratio <- function(alpha, beta, gamma, tfr){
  return(gamma * (exp(alpha + beta * tfr))/(1 + exp(alpha + beta * tfr)))
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


# Load in data
joined <- readRDS("data/tfr_covariates.RDS")
joined$all_parents = joined$mother + joined$father + joined$both
joined$parent_ratio = joined$all_parents/joined$deaths

# Calculate country specific SD
joined$sd = (joined$tfr_u-joined$tfr_l)/(2*1.96)

# Exclude Iran from fit
subset <- joined[which(joined$parent_ratio != 0),]
subset = subset[which(!subset$country %in% c("I.R. Iran")),]

subset <- subset[which(subset$country %in% c("England & Wales", "France", "Germany", "Italy", "Poland", "Russian Federation", "Spain")),]


predict_subset <- joined[which(joined$country %in% c("Sweden", "Netherlands", "Denmark", "Switzerland" )),]

mod <- lm(parent_ratio ~ tfr, subset)
tfrs =  seq(1, 2, 0.1)
new.tfr = data.frame(tfr = c(tfrs))
ratios = predict(mod, new.tfr)

dat <- data.frame(tfr = tfrs,
                  ratios = ratios)

tfrs =  seq(1, 2, 0.01)
ratios = calc_ratio(-7.875283,  2.863118,  1.955140, tfrs)
dat2 <- data.frame(tfr = tfrs,
                  ratios = ratios)

new.tfr = data.frame(tfr = c(predict_subset$tfr))
predict_subset$predicted_linear <- predict(mod, new.tfr)
predict_subset$predicted_logistic <- calc_ratio(-7.875283,  2.863118,  1.955140, predict_subset$tfr)

p <- ggplot(subset) + geom_point(aes(tfr, parent_ratio))  +
  geom_text_repel(aes(x = tfr, y = parent_ratio, label = country), size = 3, max.overlaps = 100) + 
  geom_line(data = dat, aes(tfr, ratios), col = "red") + 
  geom_line(data = dat2, aes(tfr, ratios), col = "blue") + 
  geom_point(data = predict_subset, aes(tfr, predicted_linear), col = "red")  +
  geom_point(data = predict_subset, aes(tfr, predicted_logistic), col = "blue") +
  geom_text_repel(data = predict_subset, aes(x = tfr, y = predicted_linear, label = country), col = "red", size = 3, max.overlaps = 100) + 
  geom_text_repel(data = predict_subset, aes(x = tfr, y = predicted_logistic, label = country), col = "blue", size = 3, max.overlaps = 100)
print(p)


europe <- joined[joined$who_region == "European",]
new.tfr = data.frame(tfr = c(europe$tfr))

europe$predicted_linear <- predict(mod, new.tfr)
europe$predicted_logistic <- calc_ratio(-7.875283,  2.863118,  1.955140, europe$tfr)

europe$predicted_linear <- ifelse(is.na(europe$parent_ratio), europe$predicted_linear, europe$parent_ratio)
europe$predicted_logistic <- ifelse(is.na(europe$parent_ratio), europe$predicted_logistic, europe$parent_ratio)

europe$predicted_linear_orphans <- europe$predicted_linear * europe$fitting_deaths
europe$predicted_logistic_orphans <- europe$predicted_logistic * europe$fitting_deaths

# Calculate uncertainty
n = 1000
estimates_parents <- matrix(nrow = length(europe$country), ncol = n)
estimates_parents_orphans <- matrix(nrow = length(europe$country), ncol = n)

for (i in 1:n){
  rn <- rnorm(length(europe$country), mean = europe$tfr, sd = europe$sd)
  new.tfr = data.frame(tfr = c(rn))
  estimates_parents[, i] <- predict(mod, new.tfr)
  estimates_parents_orphans[, i] <- estimates_parents[, i] * europe$fitting_deaths
}

orphans_samples <- colSums(estimates_parents_orphans)
print(sprintf("Parent orphans: %0.f [%0.f - %0.f]", 
              sum(europe$predicted_linear_orphans), floor(quantile(orphans_samples, probs = 0.025)), 
              ceiling(quantile(orphans_samples, probs = 0.975))))

# Separate out by regions
mean = rowMeans(estimates_parents_orphans)
li = rowQuantiles(estimates_parents_orphans, probs = 0.025)
ui = rowQuantiles(estimates_parents_orphans, probs = 0.975)

orphans_sample_linear = data.frame("country" = europe$country, 
                            "mean" = europe$predicted_linear_orphans, 
                            "lower" = li, 
                            "upper" = ui,
                            "text" = sprintf("%.0f [%.0f - %.0f]", 
                                               signif(europe$predicted_linear_orphans, 2), 
                                               round.choose(li, 100, 0), 
                                               round.choose(ui, 100, 1)), 
                            "region" = europe$who_region)

# Exchange out study countries
orphans_sample_linear$text <- as.character(orphans_sample_linear$text)
orphans_sample_linear$text[europe$all != 0] <- europe$predicted_linear_orphans[europe$all != 0]

# Calculate uncertainty
n = 1000
estimates_parents <- matrix(nrow = length(europe$country), ncol = n)
estimates_parents_orphans <- matrix(nrow = length(europe$country), ncol = n)

for (i in 1:n){
  rn <- rnorm(length(europe$country), mean = europe$tfr, sd = europe$sd)
  estimates_parents[, i] <- calc_ratio(-7.875283,  2.863118,  1.955140, rn)
  estimates_parents_orphans[, i] <- estimates_parents[, i] * europe$fitting_deaths
}

orphans_samples <- colSums(estimates_parents_orphans)
print(sprintf("Parent orphans: %0.f [%0.f - %0.f]",
              sum(europe$predicted_logistic_orphans), floor(quantile(orphans_samples, probs = 0.025)),
              ceiling(quantile(orphans_samples, probs = 0.975))))

