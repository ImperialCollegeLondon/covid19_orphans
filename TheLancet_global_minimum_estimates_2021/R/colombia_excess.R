library(readxl)

calculate_colombia_excess <- function(){
  data = read_xlsx("TheLancet_global_minimum_estimates_2021/data/Colombia/colombia_excess_deaths.xlsx", sheet = 2)
  data = data[6:316, 1:3]
  colnames(data) <- c("year", "week", "deaths")
  data$deaths <- as.numeric(data$deaths)
  data = data %>% filter(week != "Total")
  data$week <- sapply(data$week, function(x) (strsplit(x, " ")[[1]])[2])
  
  data$week <- as.numeric(data$week)
  
  data_combined <- data %>%
    filter(year != 2020) %>%
    group_by(week) %>%
    summarise("mean_deaths" = mean(deaths))
  
  data_2020 <- data %>% filter(year == "2020")
  data_2020 <- select(data_2020, week, deaths)
  names(data_2020) <- c("week", "deaths_2020")
  
  data = left_join(data_combined, data_2020, by = c("week"))
  
  data$excess = data$deaths_2020 - data$mean_deaths
  
  data_subset = data %>% 
    filter(data$week > 9 & data$week <= 44)
  
  total_excess = sum(data_subset$excess)
  
  return (total_excess)              
  
}
