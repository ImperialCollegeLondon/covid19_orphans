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
  data$orphans = format(round(data$orphans, -2), big.mark = ",", trim = TRUE)
  data$primary_loss = format(round(data$primary_loss, -2), big.mark = ",", trim = TRUE)
  data$all = format(round(data$all, -2), big.mark = ",", trim = TRUE)
  
  # Load in extrapolation
  ps_extrapolation = readRDS("global_age_analysis_2021/data/formatted_primary_secondary.RDS")
  p_extrapolation = readRDS("global_age_analysis_2021/data/formatted_primary.RDS")
  pa_extrapolation = readRDS("global_age_analysis_2021/data/formatted_primary.RDS")
  
  data = rbind(data, 
               data.frame(country = "Global extrapolation",
                          orphans = pa_extrapolation,
                          primary_loss = p_extrapolation,
                          all = ps_extrapolation))
  saveRDS(data, file=paste0("global_age_analysis_2021/data/age_outputs/formated_update_", date, ".RDS"))
}

combine_table <- function(){
  april = readRDS("global_age_analysis_2021/data/age_outputs/formated_update_apr.RDS")
  oct = readRDS("global_age_analysis_2021/data/age_outputs/formated_update_oct.RDS")
  dat = left_join(april, oct, by = "country")
  write.csv(dat, file = "global_age_analysis_2021/tab_1.csv")
}
  
