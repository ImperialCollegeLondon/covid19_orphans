source("global_age_analysis_2021/R/utils.R")
library(xtable)

format_table <- function(date){
  # Read in a format orphans data
  data = readRDS("global_age_analysis_2021/data/tfr_covariates.RDS")
  data = select(data, country, orphans, primary_loss, all)
  data = data[which(data$all!= 0),]
  data = rbind(data, 
               data.frame(country = "Study total",
                          orphans = sum(data$orphans),
                          primary_loss = sum(data$primary_loss),
                          all = sum(data$all)))
  data$orphans_format = format(round(data$orphans, -2), big.mark = ",", trim = TRUE)
  data$primary_loss_format = format(round(data$primary_loss, -2), big.mark = ",", trim = TRUE)
  data$all_format = format(round(data$all, -2), big.mark = ",", trim = TRUE)
  
  # Load in extrapolation
  ps_extrapolation = readRDS("global_age_analysis_2021/data/un_formatted_primary_secondary.RDS")
  p_extrapolation = readRDS("global_age_analysis_2021/data/un_formatted_primary.RDS")
  pa_extrapolation = readRDS("global_age_analysis_2021/data/un_formatted_parents.RDS")
  
  extrapolation_data  = data.frame(type = c("orphan", "primary", "primary_secondary"),
                                   mean = c(pa_extrapolation[1], p_extrapolation[1], ps_extrapolation[1]),
                                   li = c(pa_extrapolation[2], p_extrapolation[2], ps_extrapolation[2]),
                                   ui = c(pa_extrapolation[3], p_extrapolation[3], ps_extrapolation[3]))
  save(data, extrapolation_data, file=paste0("global_age_analysis_2021/data/age_outputs/formated_update_", date, ".RData"))
  
  if (date == "oct"){
    p = readRDS("global_age_analysis_2021/data/p_samples.RDS")
    p_sum = colSums(p[,1:1000])
    ps = readRDS("global_age_analysis_2021/data/ps_samples.RDS")
    ps_sum = colSums(ps[,1:1000])
    
    print(sprintf("Primary: %s [%s - %s]", format(round(p_extrapolation[1], -2), big.mark = ",", trim = TRUE), 
                  format(round.choose(quantile(p_sum, probs = 0.025), 100, 0), big.mark = ",", trim = TRUE), 
                  format(round.choose(quantile(p_sum, probs = 0.975), 100, 1), big.mark = ",", trim = TRUE)))
    
    diff = ps_sum - p_sum
    
    print(sprintf("Diff: %s [%s - %s]", 
            format(round(ps_extrapolation[1] - p_extrapolation[1], -2), big.mark = ",", trim = TRUE), 
            format(round.choose(quantile(diff, probs = 0.025), 100, 0), big.mark = ",", trim = TRUE), 
            format(round.choose(quantile(diff, probs = 0.975), 100, 1), big.mark = ",", trim = TRUE)))
    
  }
  
  # Saves global samples with which date it is
  ps_total <- readRDS("global_age_analysis_2021/data/ps_total_samples.RDS")
  saveRDS(ps_total, paste0("global_age_analysis_2021/data/ps_total_samples_", date, ".RDS"))
  
  p_total <- readRDS("global_age_analysis_2021/data/p_total_samples.RDS")
  saveRDS(p_total, paste0("global_age_analysis_2021/data/p_total_samples_", date, ".RDS"))
  
  pa_total <- readRDS("global_age_analysis_2021/data/pa_total_samples.RDS")
  saveRDS(pa_total, paste0("global_age_analysis_2021/data/pa_total_samples_", date, ".RDS"))
}

combine_table <- function(){
  load("global_age_analysis_2021/data/age_outputs/formated_update_apr.RData")
  apr = data
  apr_extrapolation = extrapolation_data
  load("global_age_analysis_2021/data/age_outputs/formated_update_oct.RData")
  oct = data
  oct_extrapolation = extrapolation_data
  
  # Work out percentage change.
  oct_ps = readRDS("global_age_analysis_2021/data/ps_total_samples_oct.RDS")
  april_ps = readRDS("global_age_analysis_2021/data/ps_total_samples_apr.RDS")
  diff_ps = quantile((oct_ps - april_ps) / april_ps, probs = c(0.025, 0.975))
  
  oct_p = readRDS("global_age_analysis_2021/data/p_total_samples_oct.RDS")
  april_p = readRDS("global_age_analysis_2021/data/p_total_samples_apr.RDS")
  diff_p = quantile((oct_p - april_p) / april_p,  probs = c(0.025, 0.975))
  
  oct_pa = readRDS("global_age_analysis_2021/data/pa_total_samples_oct.RDS")
  april_pa= readRDS("global_age_analysis_2021/data/pa_total_samples_apr.RDS")
  diff_pa = quantile((oct_pa - april_pa) / april_pa,  probs = c(0.025, 0.975))
  
  # Make extrapolation table.
  extrapolation = data.frame(type = c("orphan", "primary", "primary_secondary"))
  extrapolation$update_study = sprintf("%s [%s - %s]", 
                                       format(round(apr_extrapolation$mean, -2), big.mark = ",", trim = TRUE), 
                                       format(round.choose(apr_extrapolation$li, 100, 0), big.mark = ",", trim = TRUE), 
                                       format(round.choose(apr_extrapolation$ui, 100, 1), big.mark = ",", trim = TRUE))
  extrapolation$six_months = sprintf("%s [%s - %s]", 
                                       format(round(oct_extrapolation$mean - apr_extrapolation$mean, -2), big.mark = ",", trim = TRUE), 
                                       format(round.choose(oct_extrapolation$li - apr_extrapolation$li, 100, 0), big.mark = ",", trim = TRUE), 
                                       format(round.choose(oct_extrapolation$ui - apr_extrapolation$ui, 100, 1), big.mark = ",", trim = TRUE))
  extrapolation$grand_total = sprintf("%s [%s - %s]", 
                                       format(round(oct_extrapolation$mean, -2), big.mark = ",", trim = TRUE), 
                                       format(round.choose(oct_extrapolation$li, 100, 0), big.mark = ",", trim = TRUE), 
                                       format(round.choose(oct_extrapolation$ui, 100, 1), big.mark = ",", trim = TRUE))
  extrapolation$percent_increase = sprintf("%.1f%% [%.1f%% - %.1f%%]", 
                                           (oct_extrapolation$mean - apr_extrapolation$mean)/apr_extrapolation$mean * 100,
                                           round.choose(c(diff_pa[1], diff_p[1], diff_ps[1]) * 100, 0.1, 0),
                                           round.choose(c(diff_pa[2], diff_p[2], diff_ps[2]) * 100, 0.1, 1))
  write.csv(extrapolation, file = "global_age_analysis_2021/table_1_extrapolation_increase.csv", row.names=FALSE)
  
  
  dat = left_join(apr, oct, by = c("country"))
  dat$diff_orphans = dat$orphans.y - dat$orphans.x
  dat$diff_primary = dat$primary_loss.y - dat$primary_loss.x
  dat$diff_all = dat$all.y - dat$all.x
  
  dat$diff_orphans_format = format(round(dat$diff_orphans, -2), big.mark = ",", trim = TRUE)
  dat$diff_primary_loss_format = format(round(dat$diff_primary, -2), big.mark = ",", trim = TRUE)
  dat$diff_all_format = format(round(dat$diff_all, -2), big.mark = ",", trim = TRUE)
  
  dat$orphans_percent = paste0(round(dat$diff_orphans / dat$orphans.x * 100, digits = 1), "%")
  dat$primary_percent = paste0(round.choose(dat$diff_primary / dat$primary_loss.x * 100, 0.1, 0), "%")
  dat$all_percent = paste0(round.choose(dat$diff_all / dat$all.x * 100, 0.1, 1), "%")
  
  
  dat_subset = select(dat, country, orphans_format.x, primary_loss_format.x, all_format.x,
                      diff_orphans_format, diff_primary_loss_format, diff_all_format,
                      orphans_percent, primary_percent, all_percent)
  tab<-xtable(dat_subset)
  print(tab, include.rownames=FALSE)
}
  
