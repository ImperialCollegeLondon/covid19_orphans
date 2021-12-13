# Joint fitting
joint_fit <- function(all_data, data, formula, plot = FALSE, loo = FALSE){
  # Fit model
  set.seed(1)
  mod_full <- brm(formula, data = data, seed = 2,
                  control = list(max_treedepth = 16),
                  family = multinomial(), 
                  cores = 4,
                  file_refit = "always", 
                  file = "global_age_analysis_2021/data/age_outputs/mod_fit.rds")
  saveRDS(mod_full, "global_age_analysis_2021/data/age_outputs/global_age_fit.RDS")
  # Format data for analysis
  newdat = data
  newdat$N = 1
  
  sample_prediction <- as.data.frame(predict(mod_full, newdata = newdat)[, 1, ])
  sample_prediction_error <- sample_prediction
  names(sample_prediction) <- paste0(names(sample_prediction), "_predicted")
  sample_prediction$country <- response_wide$country
  percent = select(all_data, category, gender, percent, country)
  percent$percent = percent$percent / 100
  percent_wide = pivot_wider(percent, names_from = c("category", "gender"), values_from = percent)
  percent_wide_error <- percent_wide
  percent_wide <- left_join(percent_wide, sample_prediction,  by = "country")
  
  # Error
  sample_prediction_error$country <- response_wide$country
  sample_prediction_error_long <- gather(sample_prediction_error, key = "category", value = "percent_predicted", -country)
  percent_wide_error_long <- gather(percent_wide_error, key = "category", value = "percent_observed", -country)
  error_long <- left_join(percent_wide_error_long, sample_prediction_error_long, by = c("country", "category"))
  error_long$abs_error = abs(error_long$percent_observed - error_long$percent_predicted)
  error_long$abs_percentage_error = abs(error_long$percent_observed - error_long$percent_predicted) / error_long$percent_observed
  print(sprintf("MAE: %f", mean(error_long$abs_error)))
  print(sprintf("MAPE: %f", mean(error_long$abs_percentage_error)))
  
  if (plot == TRUE){
    # Plotting
    p_m_10_17 <- ggplot(percent_wide, aes(`[10-18)_Female`, `[10-18)_Female_predicted`)) +
      geom_segment(aes(x = 0.1, y = 0.1, xend = 0.25, yend = 0.25)) +
      geom_point() + 
      geom_text_repel(aes(label = country), max.overlaps = 21) +
      xlab("Observed maternal orphans 10-17") + ylab("Predicted maternal orphans 10-17") + 
      theme_bw() 
    
    p_p_10_17 <- ggplot(percent_wide, aes(`[10-18)_Male`, `[10-18)_Male_predicted`)) +
      geom_segment(aes(x = 0.3, y = 0.3, xend = 0.6, yend = 0.6)) +
      geom_point() + 
      geom_text_repel(aes(label = country), max.overlaps = 21) +
      xlab("Observed paternal orphans 10-17") + ylab("Predicted paternal orphans 10-17") + 
      theme_bw() 
    
    p_m_5_9 <- ggplot(percent_wide, aes(`[5-10)_Female`, `[5-10)_Female_predicted`)) +
      geom_segment(aes(x = 0, y = 0, xend = 0.1, yend = 0.1)) +
      geom_point() + 
      geom_text_repel(aes(label = country), max.overlaps = 21) +
      xlab("Observed maternal orphans 5-9") + ylab("Predicted maternal orphans 5-9") + 
      theme_bw() 
    
    p_p_5_9 <- ggplot(percent_wide, aes(`[5-10)_Male`, `[5-10)_Male_predicted`)) +
      geom_segment(aes(x = 0.1, y = 0.1, xend = 0.25, yend = 0.25)) +
      geom_point() + 
      geom_text_repel(aes(label = country), max.overlaps = 21) +
      xlab("Observed paternal orphans 5-9") + ylab("Predicted paternal orphans 5-9") + 
      theme_bw() 
    
    p_m_0_4 <- ggplot(percent_wide, aes(`[0-5)_Female`, `[0-5)_Female_predicted`)) +
      geom_segment(aes(x = 0, y = 0, xend = 0.1, yend = 0.1)) +
      geom_point() + 
      geom_text_repel(aes(label = country), max.overlaps = 21) +
      xlab("Observed maternal orphans 0-5") + ylab("Predicted maternal orphans 0-5") + 
      theme_bw() 
    
    p_p_0_4 <- ggplot(percent_wide, aes(`[0-5)_Male`, `[0-5)_Male_predicted`)) +
      geom_segment(aes(x = 0, y = 0, xend = 0.2, yend = 0.2)) +
      geom_point() + 
      geom_text_repel(aes(label = country), max.overlaps = 21) +
      xlab("Observed paternal orphans 0-5") + ylab("Predicted paternal orphans 0-5") + 
      theme_bw() 
    p <- cowplot::plot_grid(p_m_10_17, p_p_10_17,
                            p_m_5_9, p_p_5_9,
                            p_m_0_4, p_p_0_4,
                            ncol = 2, labels = "AUTO")
    ggsave("global_age_analysis_2021/figures/observed_predicted.pdf", p, width = 8, height = 10)
  }
  
  if (loo == TRUE){
    # LOO
    mean_loo <- NULL
    for (i in 1:length(data$country)){
      sub_data = data[which(data$country != data$country[i]),]
      
      mod <- brm(formula, data = data, 
                 control = list(max_treedepth = 15),
                 family = multinomial(), seed = 1, 
                 cores = getOption("mc.cores", parallel::detectCores()),
                 refresh = 0, silent = 0)
      
      sample_prediction <- as.data.frame(predict(mod, newdata = newdat)[, 1, ])
      sample_prediction_error <- sample_prediction
      names(sample_prediction) <- paste0(names(sample_prediction), "_predicted")
      sample_prediction$country <- response_wide$country
      percent = select(all_data, category, gender, percent, country)
      percent$percent = percent$percent / 100
      percent_wide = pivot_wider(percent, names_from = c("category", "gender"), values_from = percent)
      percent_wide_error <- percent_wide
      percent_wide <- left_join(percent_wide, sample_prediction,  by = "country")
      
      sample_prediction_error$country <- response_wide$country
      sample_prediction_error_long <- gather(sample_prediction_error, key = "category", value = "percent_predicted", -country)
      percent_wide_error_long <- gather(percent_wide_error, key = "category", value = "percent_observed", -country)
      error_long <- left_join(percent_wide_error_long, sample_prediction_error_long, by = c("country", "category"))
      error_long$abs_error = abs(error_long$percent_observed - error_long$percent_predicted)
      error_long$abs_percentage_error = abs(error_long$percent_observed - error_long$percent_predicted) / error_long$percent_observed
      
      mean_loo = c(mean_loo, mean(error_long$abs_error))
    }
    print(sprintf("MAE LOO: %f", mean(mean_loo)))
    
  }
  
  return (mod_full)
}