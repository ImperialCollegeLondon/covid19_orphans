get_diff_deaths <- function(file_a, file_b, file_c, country = ""){
  
  if (country != ""){
    multipliers = read.csv("global_age_analysis_2021/data/multipliers.csv", header = FALSE)
    mult = multipliers$V2[which(multipliers$V1 == country)]
  } else {
    mult = 1
  }
  deaths_a <- read.csv(file_a)
  deaths_a <- select(deaths_a, age, gender, deaths)
  deaths_a$deaths = deaths_a$deaths * mult
  
  deaths_b <- read.csv(file_b)
  deaths_b <- select(deaths_b, age, gender, deaths)
  deaths_b$deaths = deaths_b$deaths * mult
  
  deaths <- left_join(deaths_a, deaths_b, by = c("age", "gender"))
  deaths$deaths <- deaths$deaths.y - deaths$deaths.x
  
  deaths$deaths = deaths$deaths / mult
  
  write.csv(deaths, file_c, row.names=FALSE)
}

get_diff_COVID19_deaths <- function(file_a, file_b, file_c, country = ""){
  
  if (country != ""){
    multipliers = read.csv("global_age_analysis_2021/data/multipliers.csv", header = FALSE)
    mult = multipliers$V2[which(multipliers$V1 == country)]
  } else {
    mult = 1
  }
  
  deaths_a <- read.csv(file_a)
  deaths_a <- select(deaths_a, age, gender, COVID19_deaths)
  deaths_a$COVID19_deaths = deaths_a$COVID19_deaths * mult
  
  deaths_b <- read.csv(file_b)
  deaths_b <- select(deaths_b, age, gender, COVID19_deaths)
  deaths_b$COVID19_deaths = deaths_b$COVID19_deaths * mult
  
  deaths <- left_join(deaths_a, deaths_b, by = c("age", "gender"))
  deaths$COVID19_deaths <- deaths$COVID19_deaths.y - deaths$COVID19_deaths.x
  
  deaths$COVID19_deaths =  deaths$COVID19_deaths / mult
  
  write.csv(deaths, file_c, row.names=FALSE)
}

get_diff_deaths_colombia <- function(file_a, file_b, file_c, country = ""){
  
  multipliers = read.csv("global_age_analysis_2021/data/multipliers.csv", header = FALSE)
  mult = multipliers$V2[which(multipliers$V1 == "Colombia")]
  
  deaths_a <- read.csv(file_a)
  deaths_a <- select(deaths_a, age, gender, deaths)
  deaths_a$deaths <- deaths_a$deaths * mult
  
  deaths_b <- read.csv(file_b)
  deaths_b <- select(deaths_b, age, gender, deaths)
  tmp_male = data.frame(age = "80+",
                   gender = "male",
                   deaths = deaths_b$deaths[which(deaths_b$age == "80-84" & deaths_b$gender == "male")] + 
                     deaths_b$deaths[which(deaths_b$age == "85+" & deaths_b$gender == "male")] )
  tmp_female = data.frame(age = "80+",
                        gender = "female",
                        deaths = deaths_b$deaths[which(deaths_b$age == "80-84" & deaths_b$gender == "female")] + 
                          deaths_b$deaths[which(deaths_b$age == "85+" & deaths_b$gender == "female")] )
  deaths_b <- deaths_b[which(!deaths_b$age %in% c("80-84", "85+")),]
  deaths_b <- rbind(deaths_b, tmp_male, tmp_female)
  deaths_b$deaths <- deaths_b$deaths * mult
  
  deaths <- left_join(deaths_a, deaths_b, by = c("age", "gender"))
  deaths$deaths <- deaths$deaths.y - deaths$deaths.x
  deaths$deaths = deaths$deaths / mult
  
  write.csv(deaths, file_c, row.names=FALSE)
}

get_diff_deaths_india <- function(file_a, file_b, file_c, country = ""){
  multipliers = read.csv("global_age_analysis_2021/data/multipliers.csv", header = FALSE)
  ind = multipliers$V2[which(multipliers$V1 == "India")]
  
  deaths_a <- read.csv(file_a)
  deaths_a <- select(deaths_a, age, sex, deaths)
  deaths_a$deaths = deaths_a$deaths * ind
  
  deaths_b <- read.csv(file_b)
  deaths_b <- select(deaths_b, age, sex, deaths)
  deaths_b$deaths = deaths_b$deaths * ind
  
  deaths <- left_join(deaths_a, deaths_b, by = c("age", "sex"))
  deaths$deaths <- deaths$deaths.y - deaths$deaths.x
  deaths$deaths <- deaths$deaths / ind
  write.csv(deaths, file_c, row.names=FALSE)
}

get_diff_death <- function(file_a, file_b, file_c, country = ""){
  
  if (country != ""){
    multipliers = read.csv("global_age_analysis_2021/data/multipliers.csv", header = FALSE)
    mult = multipliers$V2[which(multipliers$V1 == country)]
  } else {
    mult = 1
  }
  
  deaths_a <- read.csv(file_a)
  deaths_a <- select(deaths_a, age, gender, death)
  deaths_a$death <- deaths_a$death * mult
  
  deaths_b <- read.csv(file_b)
  deaths_b <- select(deaths_b, age, gender, death)
  deaths_b$death <- deaths_b$death * mult
  
  deaths <- left_join(deaths_a, deaths_b, by = c("age", "gender"))
  deaths$death <- deaths$death.y - deaths$death.x
  deaths$death = deaths$death / mult
  
  write.csv(deaths, file_c, row.names=FALSE)
}
