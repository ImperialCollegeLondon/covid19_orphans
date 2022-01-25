source(file.path("R","process_children_function.R"))
source(file.path("R","process_fertility.R"))
source(file.path("R","process_child_mortality.R"))

# USA
process_number_children_usa_bystate <- function(rep=000){
  cat(sprintf("Processing Fertility rates\n"))
  process_usa_states_fertility(rep)
  # child mortality
  cat(sprintf("Processing child mortality rates\n"))
  process_child_mortality('usa', 'United States of America')
  
  # fathers
  cat(sprintf("Processing number of children of fathers\n"))
  data_m = read.csv(paste0('data/fertility/usa_states_fertility_m_all.csv'))
  data_m$gender <- 'Male'
  data_m$fertility_rate <- data_m$fertility_rate/1000
  data_m$date = data_m$year
  # copy 2019 fert data to 2020
  d_2019 = data_m[which(data_m$date == 2019),]
  d_2020 = copy(d_2019)
  d_2020$date = 2020
  d_2020$year = 2020
  is_child_mortality_needed = 1
  
  data_m = rbind(data_m, d_2020)
  data_m <- data.table(data_m)

  states <- unique(data_m$state)
  rcat <- unique(data_m$race.eth)
  rcat <- rcat[!(grepl("More than",rcat))]
  rcat <- rcat[!(grepl("Other",rcat))]
  rcat <- rcat[!(grepl("Unknown",rcat))]
  for(s in states){
    cat(paste0("processing ",s))
    for(r in rcat){
      cat(paste0("processing ",r))
      data_m_srh <- subset(data_m,state==s & race.eth==r)
      
      process_children_father_55_plus(paste0("usa","_",gsub(' ','',s),"_",gsub(' ','',r)), data_m_srh)
      add_child_mortality(is_child_mortality_needed,  paste0("usa","_",gsub(' ','',s),"_",gsub(' ','',r)))
    }
  }
  
  # mothers
  cat(sprintf("Processing number of children of mothers\n"))
  data = read.csv(paste0('data/fertility/usa_states_fertility_f.csv'))
  data_f = copy(data)
  data_f$gender <- 'Female'
  data_f$date = data_f$year
  data_f$fertility_rate = data_f$fertility_rate/1000
  d_2019 = data_f[which(data_f$date == 2019),]
  d_2020 = copy(d_2019)
  d_2020$date = 2020
  data_f = rbind(data_f, d_2020)
  
  data_f <- data.table(data_f)

  states <- unique(data_f$state)
  rcat <- unique(data_f$race.eth)
  rcat <- rcat[!(grepl("More than",rcat))]
  rcat <- rcat[!(grepl("Other",rcat))]
  rcat <- rcat[!(grepl("Unknown",rcat))]
  for(s in states){
    cat(paste0("processing ",s))
    for(r in rcat){
      cat(paste0("processing ",r))
      data_f_srh <- subset(data_f,state==s & race.eth==r)
      
      process_children_all(paste0("usa","_",gsub(' ','',s),"_",gsub(' ','',r)), is_child_mortality_needed, data_f_srh)
      
      process_fertility_usa_states_plots(paste0("usa","_",gsub(' ','',s),"_",gsub(' ','',r)),s,r)
    }
  }
  
}

