source("TheLancetCAH_global_age_analysis_2022/R/utils.R")
library(xtable)
library(matrixStats)

format_table <- function(date){
  # Read in a format orphans data
  data = readRDS("TheLancetCAH_global_age_analysis_2022/data/tfr_covariates.RDS")
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
  ps_extrapolation = readRDS("TheLancetCAH_global_age_analysis_2022/data/un_formatted_primary_secondary.RDS")
  p_extrapolation = readRDS("TheLancetCAH_global_age_analysis_2022/data/un_formatted_primary.RDS")
  pa_extrapolation = readRDS("TheLancetCAH_global_age_analysis_2022/data/un_formatted_parents.RDS")
  
  extrapolation_data  = data.frame(type = c("orphan", "primary", "primary_secondary"),
                                   mean = c(pa_extrapolation[1], p_extrapolation[1], ps_extrapolation[1]),
                                   li = c(pa_extrapolation[2], p_extrapolation[2], ps_extrapolation[2]),
                                   ui = c(pa_extrapolation[3], p_extrapolation[3], ps_extrapolation[3]))
  save(data, extrapolation_data, file=paste0("TheLancetCAH_global_age_analysis_2022/data/age_outputs/formated_update_", date, ".RData"))
  
  if (date == "oct"){
    p = readRDS("TheLancetCAH_global_age_analysis_2022/data/p_samples.RDS")
    p_sum = colSums(p[,1:1000])
    ps = readRDS("TheLancetCAH_global_age_analysis_2022/data/ps_samples.RDS")
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
  ps_total <- readRDS("TheLancetCAH_global_age_analysis_2022/data/ps_total_samples.RDS")
  saveRDS(ps_total, paste0("TheLancetCAH_global_age_analysis_2022/data/ps_total_samples_", date, ".RDS"))
  
  p_total <- readRDS("TheLancetCAH_global_age_analysis_2022/data/p_total_samples.RDS")
  saveRDS(p_total, paste0("TheLancetCAH_global_age_analysis_2022/data/p_total_samples_", date, ".RDS"))
  
  pa_total <- readRDS("TheLancetCAH_global_age_analysis_2022/data/pa_total_samples.RDS")
  saveRDS(pa_total, paste0("TheLancetCAH_global_age_analysis_2022/data/pa_total_samples_", date, ".RDS"))
  
  reg_ps = readRDS("TheLancetCAH_global_age_analysis_2022/data/age_outputs/reg_ps_samples.RDS")
  saveRDS(reg_ps, paste0("TheLancetCAH_global_age_analysis_2022/data/age_outputs/reg_ps_samples_", date, ".RDS"))
  
  reg_p = readRDS("TheLancetCAH_global_age_analysis_2022/data/age_outputs/reg_p_samples.RDS")
  saveRDS(reg_p, paste0("TheLancetCAH_global_age_analysis_2022/data/age_outputs/reg_p_samples_", date, ".RDS"))
  
  reg_pa = readRDS("TheLancetCAH_global_age_analysis_2022/data/age_outputs/reg_pa_samples.RDS")
  saveRDS(reg_pa, paste0("TheLancetCAH_global_age_analysis_2022/data/age_outputs/reg_pa_samples_", date, ".RDS"))
  
  reg_ps_mean = readRDS("TheLancetCAH_global_age_analysis_2022/data/age_outputs/reg_ps.RDS")
  saveRDS(reg_ps_mean, paste0("TheLancetCAH_global_age_analysis_2022/data/age_outputs/reg_ps_", date, ".RDS"))
  reg_p_mean = readRDS("TheLancetCAH_global_age_analysis_2022/data/age_outputs/reg_p.RDS")
  saveRDS(reg_p_mean, paste0("TheLancetCAH_global_age_analysis_2022/data/age_outputs/reg_p_", date, ".RDS"))
  reg_pa_mean = readRDS("TheLancetCAH_global_age_analysis_2022/data/age_outputs/reg_pa.RDS")
  saveRDS(reg_pa_mean, paste0("TheLancetCAH_global_age_analysis_2022/data/age_outputs/reg_pa_", date, ".RDS"))
  
}

combine_table <- function(){
  load("TheLancetCAH_global_age_analysis_2022/data/age_outputs/formated_update_apr.RData")
  apr = data
  apr_extrapolation = extrapolation_data
  load("TheLancetCAH_global_age_analysis_2022/data/age_outputs/formated_update_oct.RData")
  oct = data
  oct_extrapolation = extrapolation_data
  
  # Work out percentage change.
  oct_ps = readRDS("TheLancetCAH_global_age_analysis_2022/data/ps_total_samples_oct.RDS")
  april_ps = readRDS("TheLancetCAH_global_age_analysis_2022/data/ps_total_samples_apr.RDS")
  oct_p = readRDS("TheLancetCAH_global_age_analysis_2022/data/p_total_samples_oct.RDS")
  april_p = readRDS("TheLancetCAH_global_age_analysis_2022/data/p_total_samples_apr.RDS")
  
  secondary_apr = april_ps - april_p
  secondary_oct = oct_ps - oct_p
  
  apr_extrapolation = rbind(apr_extrapolation, data.frame(type = "secondary",
                                                          mean = apr_extrapolation$mean[apr_extrapolation$type == "primary_secondary"] - 
                                                            apr_extrapolation$mean[apr_extrapolation$type == "primary"] ,
                                                          li = quantile(secondary_apr, probs = 0.025),
                                                          ui = quantile(secondary_apr, probs = 0.975))) 
  
  oct_extrapolation = rbind(oct_extrapolation, data.frame(type = "secondary",
                                                          mean = oct_extrapolation$mean[oct_extrapolation$type == "primary_secondary"] - 
                                                            oct_extrapolation$mean[oct_extrapolation$type == "primary"] ,
                                                          li = quantile(secondary_oct, probs = 0.025),
                                                          ui = quantile(secondary_oct, probs = 0.975)))
  
  oct_ps = sort(oct_ps)
  april_ps = sort(april_ps)
  diff_ps = quantile((oct_ps - april_ps) / april_ps, probs = c(0.025, 0.975))
  
  oct_p = sort(oct_p)
  april_p = sort(april_p)
  diff_p = quantile((oct_p - april_p) / april_p,  probs = c(0.025, 0.975))
  
  oct_s = sort(secondary_oct)
  april_s = sort(secondary_apr)
  diff_s = quantile((oct_s - april_s) / april_s,  probs = c(0.025, 0.975))
  
  
  oct_pa = readRDS("TheLancetCAH_global_age_analysis_2022/data/pa_total_samples_oct.RDS")
  oct_pa = sort(oct_pa)
  april_pa= readRDS("TheLancetCAH_global_age_analysis_2022/data/pa_total_samples_apr.RDS")
  april_pa = sort(april_pa)
  diff_pa = quantile((oct_pa - april_pa) / april_pa,  probs = c(0.025, 0.975))
  
  # Make extrapolation table.
  extrapolation = data.frame(type = c("orphan", "primary", "primary_secondary", "secondary"))
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
                                           c(diff_pa[1], diff_p[1], diff_ps[1], diff_s[1]) * 100,
                                           c(diff_pa[2], diff_p[2], diff_ps[2], diff_s[2]) * 100)
  write.csv(extrapolation, file = "TheLancetCAH_global_age_analysis_2022/table_1_extrapolation_increase.csv", row.names=FALSE)
  
  
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
  
  
  # secondary
  secondary_oct = quantile(oct_ps - oct_p, probs = c(0.025, 0.975))
  
  # Regional percentage increase table
  reg_ps_oct = readRDS("TheLancetCAH_global_age_analysis_2022/data/age_outputs/reg_ps_samples_oct.RDS")
  reg_ps_oct[1, 2:1001] <- as.list(sort(unlist(reg_ps_oct[1, 2:1001])))
  reg_ps_oct[2, 2:1001] <- as.list(sort(unlist(reg_ps_oct[2, 2:1001])))
  reg_ps_oct[3, 2:1001] <- as.list(sort(unlist(reg_ps_oct[3, 2:1001])))
  reg_ps_oct[4, 2:1001] <- as.list(sort(unlist(reg_ps_oct[4, 2:1001])))
  reg_ps_oct[5, 2:1001] <- as.list(sort(unlist(reg_ps_oct[5, 2:1001])))
  reg_ps_oct[6, 2:1001] <- as.list(sort(unlist(reg_ps_oct[6, 2:1001])))
  reg_ps_apr = readRDS("TheLancetCAH_global_age_analysis_2022/data/age_outputs/reg_ps_samples_apr.RDS")
  reg_ps_apr[1, 2:1001] <- as.list(sort(unlist(reg_ps_apr[1, 2:1001])))
  reg_ps_apr[2, 2:1001] <- as.list(sort(unlist(reg_ps_apr[2, 2:1001])))
  reg_ps_apr[3, 2:1001] <- as.list(sort(unlist(reg_ps_apr[3, 2:1001])))
  reg_ps_apr[4, 2:1001] <- as.list(sort(unlist(reg_ps_apr[4, 2:1001])))
  reg_ps_apr[5, 2:1001] <- as.list(sort(unlist(reg_ps_apr[5, 2:1001])))
  reg_ps_apr[6, 2:1001] <- as.list(sort(unlist(reg_ps_apr[6, 2:1001])))
  
  percent_reg = (reg_ps_oct[, 2:1001] - reg_ps_apr[, 2:1001]) / reg_ps_apr[, 2:1001] * 100
  quantiles = rowQuantiles(as.matrix(percent_reg), probs = c(0.025, 0.975))
  
  mean_reg_ps_oct =  readRDS("TheLancetCAH_global_age_analysis_2022/data/age_outputs/reg_ps_oct.RDS")
  mean_reg_ps_apr =  readRDS("TheLancetCAH_global_age_analysis_2022/data/age_outputs/reg_ps_apr.RDS")
  
  mean = (mean_reg_ps_oct$mean - mean_reg_ps_apr$mean) / mean_reg_ps_apr$mean * 100
  
  format_ps = sprintf("%.1f [%.1f%% - %.1f%%]", mean, 
                      round.choose(quantiles[,1], 0.1, 0), round.choose(quantiles[,2], 0.1, 1))
  
  reg_p_oct = readRDS("TheLancetCAH_global_age_analysis_2022/data/age_outputs/reg_p_samples_oct.RDS")
  reg_p_oct[1, 2:1001] <- as.list(sort(unlist(reg_p_oct[1, 2:1001])))
  reg_p_oct[2, 2:1001] <- as.list(sort(unlist(reg_p_oct[2, 2:1001])))
  reg_p_oct[3, 2:1001] <- as.list(sort(unlist(reg_p_oct[3, 2:1001])))
  reg_p_oct[4, 2:1001] <- as.list(sort(unlist(reg_p_oct[4, 2:1001])))
  reg_p_oct[5, 2:1001] <- as.list(sort(unlist(reg_p_oct[5, 2:1001])))
  reg_p_oct[6, 2:1001] <- as.list(sort(unlist(reg_p_oct[6, 2:1001])))
  reg_p_apr = readRDS("TheLancetCAH_global_age_analysis_2022/data/age_outputs/reg_p_samples_apr.RDS")
  reg_p_apr[1, 2:1001] <- as.list(sort(unlist(reg_p_apr[1, 2:1001])))
  reg_p_apr[2, 2:1001] <- as.list(sort(unlist(reg_p_apr[2, 2:1001])))
  reg_p_apr[3, 2:1001] <- as.list(sort(unlist(reg_p_apr[3, 2:1001])))
  reg_p_apr[4, 2:1001] <- as.list(sort(unlist(reg_p_apr[4, 2:1001])))
  reg_p_apr[5, 2:1001] <- as.list(sort(unlist(reg_p_apr[5, 2:1001])))
  reg_p_apr[6, 2:1001] <- as.list(sort(unlist(reg_p_apr[6, 2:1001])))
  
  percent_reg = (reg_p_oct[, 2:1001] - reg_p_apr[, 2:1001]) / reg_p_apr[, 2:1001] * 100
  quantiles = rowQuantiles(as.matrix(percent_reg), probs = c(0.025, 0.975))
  
  mean_reg_p_oct =  readRDS("TheLancetCAH_global_age_analysis_2022/data/age_outputs/reg_p_oct.RDS")
  mean_reg_p_apr =  readRDS("TheLancetCAH_global_age_analysis_2022/data/age_outputs/reg_p_apr.RDS")
  
  mean = (mean_reg_p_oct$mean - mean_reg_p_apr$mean) / mean_reg_p_apr$mean * 100
  
  format_p = sprintf("%.1f [%.1f%% - %.1f%%]", mean, 
                      round.choose(quantiles[,1], 0.1, 0), round.choose(quantiles[,2], 0.1, 1))
  
  reg_pa_oct = readRDS("TheLancetCAH_global_age_analysis_2022/data/age_outputs/reg_pa_samples_oct.RDS")
  reg_pa_oct[1, 2:1001] <- as.list(sort(unlist(reg_pa_oct[1, 2:1001])))
  reg_pa_oct[2, 2:1001] <- as.list(sort(unlist(reg_pa_oct[2, 2:1001])))
  reg_pa_oct[3, 2:1001] <- as.list(sort(unlist(reg_pa_oct[3, 2:1001])))
  reg_pa_oct[4, 2:1001] <- as.list(sort(unlist(reg_pa_oct[4, 2:1001])))
  reg_pa_oct[5, 2:1001] <- as.list(sort(unlist(reg_pa_oct[5, 2:1001])))
  reg_pa_oct[6, 2:1001] <- as.list(sort(unlist(reg_pa_oct[6, 2:1001])))
  reg_pa_apr = readRDS("TheLancetCAH_global_age_analysis_2022/data/age_outputs/reg_pa_samples_apr.RDS")
  reg_pa_apr[1, 2:1001] <- as.list(sort(unlist(reg_pa_apr[1, 2:1001])))
  reg_pa_apr[2, 2:1001] <- as.list(sort(unlist(reg_pa_apr[2, 2:1001])))
  reg_pa_apr[3, 2:1001] <- as.list(sort(unlist(reg_pa_apr[3, 2:1001])))
  reg_pa_apr[4, 2:1001] <- as.list(sort(unlist(reg_pa_apr[4, 2:1001])))
  reg_pa_apr[5, 2:1001] <- as.list(sort(unlist(reg_pa_apr[5, 2:1001])))
  reg_pa_apr[6, 2:1001] <- as.list(sort(unlist(reg_pa_apr[6, 2:1001])))
  
  percent_reg = (reg_pa_oct[, 2:1001] - reg_pa_apr[, 2:1001]) / reg_pa_apr[, 2:1001] * 100
  quantiles = rowQuantiles(as.matrix(percent_reg), probs = c(0.025, 0.975))
  
  mean_reg_pa_oct =  readRDS("TheLancetCAH_global_age_analysis_2022/data/age_outputs/reg_pa_oct.RDS")
  mean_reg_pa_apr =  readRDS("TheLancetCAH_global_age_analysis_2022/data/age_outputs/reg_pa_apr.RDS")
  
  mean = (mean_reg_pa_oct$mean - mean_reg_pa_apr$mean) / mean_reg_pa_apr$mean * 100
  
  format_pa = sprintf("%.1f%% [%.1f%% - %.1f%%]", mean, 
                     round.choose(quantiles[,1], 0.1, 0), round.choose(quantiles[,2], 0.1, 1))
  
  df_percent = data.frame("Region" = mean_reg_pa_oct$who_region,
                          "Orphanhood" = format_pa,
                          "Primary caregiver loss" = format_p,
                          "Primary &/or secondary caregiver loss" = format_ps)
  
  tab<-xtable(df_percent)
  print(tab, include.rownames=FALSE)
}
  
