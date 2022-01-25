library(rdhs)
library(zoo)
library(lubridate)

extract_male_fertility <- function(data, country_code, year = NA){
  data$id <- paste(data$hv001, data$hv002, sep = "_")
  
  ids <- unique(data$hhid)
  
  id_household <- NULL
  id_man <- NULL
  age_father_now <- NULL
  age_child_now <- NULL
  age_mother_now <- NULL
  survey_date <- NULL
  father_status <- NULL
  father_wi <- NULL
  father_bmi <- NULL
  father_smoking <- NULL
  household_smoking <- NULL
  father_anemia <- NULL
  
  
  # Replace nas in mother's/fathers alive with don't know
  data$hv111[is.na(data$hv111)] <- 8
  data$hv113[is.na(data$hv113)] <- 8
  
  # Replace NAs in fathers/mothers line no
  data$hv114[is.na(data$hv114)] <- 99
  data$hv112[is.na(data$hv112)] <- 99
  
  for (i in 1:length(ids)){
    # Reset line number of fathers
    line_num_father <- NULL
    
    household <- data[which(data$hhid == ids[i]),]
    
    # Check household size 
    if (length(household$hv009) != household$hv009[1]){
      comb_household <- sum(unique(household$hv009))
      if (comb_household == length(household$hv009) | length(household$hv009) == 2*comb_household | length(household$hv009) == 3*comb_household |length(household$hv009) == 4*comb_household){
        print("Combined household.")
        stop()
      } else{
        print("Length household doesn't match household size.")
        stop()
      }
    }
    
    # Check if under children under 18
    children <- household[which(household$hv105 < 18),]
    if (length(children$hv105) != 0){
      for (j in 1:length(children$hv105)){
        # Extracts child data
        child <- children[j,]
        # Records fathers status for imputation later 0 - dead, 1 - alive, 8 - don't know, 9 - missing
        father_status <- c(father_status, child$hv113)
        
        # Saves id for that child
        id_household <- c(id_household, child$hhid)
        # Adds age of child to list
        age_child_now <- c(age_child_now, child$hv105)
        # Looks at age of father
        line_num_father <- c(line_num_father, child$hv114)
        # If father isn't in household or line number is missing add NA
        if (child$hv114 == 0 | child$hv114 == 99){
          age_father_now <- c(age_father_now, NA)
          survey_date <- c(survey_date, as.yearmon(as.yearmon(paste(child$hv007, child$hv006, sep = "-")), "%Y %m"))
          father_wi <- c(father_wi, NA)
          father_bmi <- c(father_bmi, NA)
          father_smoking <- c(father_smoking, NA)
          household_smoking <- c(household_smoking, NA)
          father_anemia <- c(father_anemia, NA)
        } else {
          father = household[child$hv114, ]
          survey_date <- c(survey_date, as.yearmon(as.yearmon(paste(father$hv007, father$hv006, sep = "-")), "%Y %m"))
          if (is.null(father$hv270)){
            father_wi <- c(father_wi, NA)
          } else {
            father_wi <- c(father_wi, father$hv270)
          }
          if (is.null(father$hb35)){
            father_smoking <- c(father_smoking, NA)
          } else {
            father_smoking <- c(father_smoking, father$hb35)
          }
          if(is.null(father$hb40)){
            father_bmi <- c(father_bmi, NA)
          } else {
            father_bmi <- c(father_bmi, father$hb40)
          }
          if (is.null(father$hv252)){
            household_smoking <- c(household_smoking, NA)
          } else {
            household_smoking <- c(household_smoking, father$hv252)
          }
          if (is.null(father$hb57)){
            father_anemia <- c(father_anemia, NA)
          } else {
            father_anemia <- c(father_anemia, father$hb57)
          }
          if (father$hv105 > 95){
            age_father_now <- c(age_father_now, NA)
          } else {
            age_father_now <- c(age_father_now, father$hv105)
          }
        }
        # Looks at age of mother
        # If mother isn't in household or line number is missing add NA
        if (child$hv111 == 0){ # If mother is dead set age to be 0
          age_mother_now = c(age_mother_now, 0)
        } else if (child$hv112 == 0 | child$hv112 == 99){ # If mother information is missing or not in household add NA
          age_mother_now <- c(age_mother_now, NA)
        } else {
          mother = household[child$hv112, ]
          if (mother$hv105 > 95){
            age_mother_now <- c(age_mother_now, NA)
          } else {
            age_mother_now <- c(age_mother_now, mother$hv105)
          }
        }
      }
    }
    # If have fathers need to give them ids
    if (length(line_num_father) > 0){
      # Gives each father an id
      unique_fathers <- unique(line_num_father)
      # make father ids
      if (length(id_man) == 0){
        father_ids <- 1:(1 + length(unique_fathers) - 1)
      } else {
        father_ids <- (max(id_man)+1):(max(id_man) + length(unique_fathers))
      }
      tmp_ids <- data.frame(unique_fathers, father_ids)
      
      tmp_combine_ids <- vector(length = length(line_num_father))
      for (n in 1:length(line_num_father)){
        tmp_combine_ids[n] = tmp_ids$father_ids[which(tmp_ids$unique_fathers == line_num_father[n])]
      }
      
      id_man <- c(id_man, tmp_combine_ids)
      
      
    }
    
    # Remove fathers from the household
    no_fathers <- household[which(! 1:length(household$hv001) %in% line_num_father),]
    
    # add other men to list
    age_no_father <- no_fathers$hv105[which(no_fathers$hv104 == 1)]
    if (is.null(no_fathers$hv270[which(no_fathers$hv104 == 1)])){
      wi_no_father <- rep(NA, length(age_no_father))
    } else {
      wi_no_father <- no_fathers$hv270[which(no_fathers$hv104 == 1)]
    }
    if(is.null(no_fathers$hb40[which(no_fathers$hv104 == 1)])){
      bmi_no_father <- rep(NA, length(age_no_father))
    } else {
      bmi_no_father <- no_fathers$hb40[which(no_fathers$hv104 == 1)]
    }
    if (is.null(no_fathers$hb35[which(no_fathers$hv104 == 1)])){
      smoking_no_father <- rep(NA, length(age_no_father))
    } else {
      smoking_no_father <- no_fathers$hb35[which(no_fathers$hv104 == 1)]
    }
    if (is.null(no_fathers$hv252[which(no_fathers$hv104 == 1)])){
      household_smoking_no_father <- rep(NA, length(age_no_father))
    } else {
      household_smoking_no_father <- no_fathers$hv252[which(no_fathers$hv104 == 1)]
    }
    if (is.null(no_fathers$hb57[which(no_fathers$hv104 == 1)])){
      anemia_no_father <- rep(NA, length(age_no_father))
    } else {
      anemia_no_father <- no_fathers$hb57[which(no_fathers$hv104 == 1)]
    }

    if (length(age_no_father) > 0){
      age_father_now <- c(age_father_now, age_no_father)
      age_mother_now <- c(age_mother_now, rep(NA, length(age_no_father)))
      age_child_now <- c(age_child_now, rep(NA, length(age_no_father)))
      father_status <- c(father_status, rep(NA, length(age_no_father)))
      id_household <- c(id_household, rep(ids[i], length(age_no_father)))
      survey_date <- c(survey_date, rep(as.yearmon(paste(household$hv007[1], household$hv006[1], sep = "-")), length(age_no_father)))
      father_wi <- c(father_wi, wi_no_father)
      father_bmi <- c(father_bmi, bmi_no_father)
      father_smoking <- c(father_smoking, smoking_no_father)
      household_smoking <- c(household_smoking, household_smoking_no_father)
      father_anemia <- c(father_anemia, anemia_no_father)
      if (length(id_man) == 0){
        id_man <- c(id_man, 1:(length(age_no_father)))
      } else {
        id_man <- c(id_man, ((max(id_man)+1):(max(id_man) + length(age_no_father))))
      }
      
    }
  }
  # Combines data
  father_data <- data.frame(id_household, id_man, age_father_now, age_mother_now, age_child_now, 
                            father_status, survey_date, father_wi, father_bmi, father_smoking, household_smoking,
                            father_anemia)
  if (is.na(year)){
    saveRDS(father_data, paste0("data/DHS/", country_code, "_fathers.RDS"))
  } else {
    saveRDS(father_data, paste0("data/DHS/", country_code, "_fathers_", year, ".RDS"))
  }
 
}

#------------------------------------------------------------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)

country = args[1]
print(country)

if (country == "co"){
  # Columbia
  datasets <- dhs_datasets(
    surveyIds = "CO2015DHS",
    fileFormat = "FL",
    fileType = "PR")
  downloads <- get_datasets(datasets$FileName)
  vars <- c("hhid", "hv001", "hv002", "hv009", "hv104", "hv105", "hv101", "hv113", "hv114", "hv111", 
            "hv112", "hv006", "hv007", "hv270", "ha40", "hb40", "ha35", "hb35", "ha36", 
            "hv252", "ha57", "hb57")
  questions <- search_variables(datasets$FileName, variables = vars)
  extract <- extract_dhs(questions, add_geo = TRUE)
  data <- rbind_labelled(extract$COPR72FL)
  extract_male_fertility (data = data, country_code = "co", year = "15")
  
  datasets <- dhs_datasets(
    surveyIds = "CO2005DHS",
    fileFormat = "FL",
    fileType = "PR")
  downloads <- get_datasets(datasets$FileName)
  vars <- c("hhid", "hv001", "hv002", "hv009", "hv104", "hv105", "hv101", "hv113", "hv114", "hv111", 
            "hv112", "hv006", "hv007", "hv270", "ha40", "hb40", "ha35", "hb35", "ha36", 
            "hv252", "ha57", "hb57")
  questions <- search_variables(datasets$FileName, variables = vars)
  extract <- extract_dhs(questions, add_geo = TRUE)
  data <- rbind_labelled(extract$COPR53FL)
  extract_male_fertility (data = data, country_code = "co", year = "05")
  
  datasets <- dhs_datasets(
    surveyIds = "CO2010DHS",
    fileFormat = "FL")
  downloads <- get_datasets(datasets$FileName)
  vars <- c("hhid", "hv001", "hv002", "hv009", "hv104", "hv105", "hv101", "hv113", "hv114", "hv111", 
            "hv112", "hv006", "hv007", "hv270", "ha40", "hb40", "ha35", "hb35", "ha36", 
            "hv252", "ha57", "hb57")
  questions <- search_variables(datasets$FileName, variables = vars)
  extract <- extract_dhs(questions, add_geo = TRUE)
  data <- rbind_labelled(extract$COPR61FL)
  
  extract_male_fertility (data = data, country_code = "co", year = "10")

} else if (country == "za"){
  #------------------------------------------------------------------------------------------------------------------------
  # South Africa
  datasets <- dhs_datasets(
    surveyIds = "ZA2016DHS",
    fileFormat = "FL")
  downloads <- get_datasets(datasets$FileName)
  vars <- c("hhid", "hv001", "hv002", "hv009", "hv104", "hv105", "hv101", "hv113", "hv114", "hv111",
            "hv112", "hv006", "hv007", "hv270", "ha40", "hb40", "ha35", "hb35", "ha36",
            "hv252", "ha57", "hb57")
  questions <- search_variables(datasets$FileName, variables = vars)
  extract <- extract_dhs(questions, add_geo = TRUE)
  data <- rbind_labelled(extract$ZAPR71FL)
  extract_male_fertility(data = data, country_code = "za", year  = "16")
  
} else if (country == "ia"){
  #------------------------------------------------------------------------------------------------------------------------
  # India
  print("a")
  datasets <- dhs_datasets(
    surveyIds = "IA2006DHS",
    fileFormat = "FL",
    fileType = "PR")
  downloads <- get_datasets(datasets$FileName)
  vars <- c("hhid", "hv001", "hv002", "hv009", "hv104", "hv105", "hv101", "hv113", "hv114", "hv111",
            "hv112", "hv006", "hv007", "hv270", "ha40", "hb40", "ha35", "hb35", "ha36",
            "hv252", "ha57", "hb57")
  questions <- search_variables(datasets$FileName, variables = vars)
  extract <- extract_dhs(questions, add_geo = FALSE)
  data <- rbind_labelled(extract$IAPR52FL)
  extract_male_fertility(data = data, country_code = "ia", year = "06")
  
  print("b")
  datasets <- dhs_datasets(
    surveyIds = "IA2015DHS",
    fileFormat = "FL",
    fileType = "PR")
  downloads <- get_datasets(datasets$FileName)
  vars <- c("hhid", "hv001", "hv002", "hv009", "hv104", "hv105", "hv101", "hv113", "hv114", "hv111",
            "hv112", "hv006", "hv007", "hv270", "ha40", "hb40", "ha35", "hb35", "ha36",
            "hv252", "ha57", "hb57")
  questions <- search_variables(datasets$FileName, variables = vars)
  extract <- extract_dhs(questions, add_geo = TRUE)
  data <- rbind_labelled(extract$IAPR74FL)
  extract_male_fertility(data = data, country_code = "ia", year = "15")
  
} else if (country == "pe"){
  #------------------------------------------------------------------------------------------------------------------------
  # Peru
  datasets <- dhs_datasets(
    surveyIds =  "PE2012DHS",
    fileFormat = "FL",
    fileType = "PR",
    countryIds = "PE")
  downloads <- get_datasets(datasets$FileName)
  vars <- c("hhid", "hv001", "hv002", "hv009", "hv104", "hv105", "hv101", "hv113", "hv114", "hv111",
            "hv112", "hv006", "hv007", "hv270", "ha40", "hb40", "ha35", "hb35", "ha36",
            "hv252", "ha57", "hb57")
  questions <- search_variables(datasets$FileName, variables = vars)
  extract <- extract_dhs(questions, add_geo = FALSE)
  data <- rbind_labelled(extract$PEPR6IFL)
  extract_male_fertility(data = data, country_code = "pe", year = "12")
  
  datasets <- dhs_datasets(
    surveyIds =  "PE2011DHS",
    fileFormat = "FL",
    fileType = "PR",
    countryIds = "PE")
  downloads <- get_datasets(datasets$FileName)
  vars <- c("hhid", "hv001", "hv002", "hv009", "hv104", "hv105", "hv101", "hv113", "hv114", "hv111",
            "hv112", "hv006", "hv007", "hv270", "ha40", "hb40", "ha35", "hb35", "ha36",
            "hv252", "ha57", "hb57")
  questions <- search_variables(datasets$FileName, variables = vars)
  extract <- extract_dhs(questions, add_geo = FALSE)
  data <- rbind_labelled(extract$PEPR6AFL)
  extract_male_fertility(data = data, country_code = "pe", year = "11")
  
  datasets <- dhs_datasets(
    surveyIds =  "PE2010DHS",
    fileFormat = "FL",
    fileType = "PR",
    countryIds = "PE")
  downloads <- get_datasets(datasets$FileName)
  vars <- c("hhid", "hv001", "hv002", "hv009", "hv104", "hv105", "hv101", "hv113", "hv114", "hv111",
            "hv112", "hv006", "hv007", "hv270", "ha40", "hb40", "ha35", "hb35", "ha36",
            "hv252", "ha57", "hb57")
  questions <- search_variables(datasets$FileName, variables = vars)
  extract <- extract_dhs(questions, add_geo = FALSE)
  data <- rbind_labelled(extract$PEPR61FL)
  extract_male_fertility(data = data, country_code = "pe", year = "10")
  
  datasets <- dhs_datasets(   surveyIds =  "PE2009DHS",
                              fileFormat = "FL",
                              fileType = "PR",
                              countryIds = "PE")
  downloads <- get_datasets(datasets$FileName)
  vars <- c("hhid", "hv001", "hv002", "hv009", "hv104", "hv105", "hv101", "hv113", "hv114", "hv111",
            "hv112", "hv006", "hv007", "hv270", "ha40", "hb40", "ha35", "hb35", "ha36",
            "hv252", "ha57", "hb57")
  questions <- search_variables(datasets$FileName, variables = vars)
  extract <- extract_dhs(questions, add_geo = FALSE)
  data <- rbind_labelled(extract$PEPR5IFL)
  extract_male_fertility(data = data, country_code = "pe", year = "09")
  
  datasets <- dhs_datasets(
    surveyIds =  "PE2007DHS",
    fileFormat = "FL",
    fileType = "PR",
    countryIds = "PE")
  downloads <- get_datasets(datasets$FileName)
  vars <- c("hhid", "hv001", "hv002", "hv009", "hv104", "hv105", "hv101", "hv113", "hv114", "hv111",
            "hv112", "hv006", "hv007", "hv270", "ha40", "hb40", "ha35", "hb35", "ha36",
            "hv252", "ha57", "hb57")
  questions <- search_variables(datasets$FileName, variables = vars)
  extract <- extract_dhs(questions, add_geo = FALSE)
  data <- rbind_labelled(extract$PEPR5AFL)
  extract_male_fertility(data = data, country_code = "pe", year = "07")
  
  datasets <- dhs_datasets(
    surveyIds =  "PE2004DHS",
    fileFormat = "FL",
    fileType = "PR",
    countryIds = "PE")
  downloads <- get_datasets(datasets$FileName)
  vars <- c("hhid", "hv001", "hv002", "hv009", "hv104", "hv105", "hv101", "hv113", "hv114", "hv111",
            "hv112", "hv006", "hv007", "hv270", "ha40", "hb40", "ha35", "hb35", "ha36",
            "hv252", "ha57", "hb57")
  questions <- search_variables(datasets$FileName, variables = vars)
  extract <- extract_dhs(questions, add_geo = FALSE)
  data <- rbind_labelled(extract$PEPR51FL)
  extract_male_fertility(data = data, country_code = "pe", year = "04")
  
  
} else if (country == "ke"){
  #------------------------------------------------------------------------------------------------------------------------
  # Kenya
  datasets <- dhs_datasets(
    surveyIds =  "KE2014DHS",
    fileFormat = "FL",
    fileType = "PR")
  downloads <- get_datasets(datasets$FileName)
  vars <- c("hhid", "hv001", "hv002", "hv009", "hv104", "hv105", "hv101", "hv113", "hv114", "hv111",
            "hv112", "hv006", "hv007", "hv270", "ha40", "hb40", "ha35", "hb35", "ha36",
            "hv252", "ha57", "hb57")
  questions <- search_variables(datasets$FileName, variables = vars)
  extract <- extract_dhs(questions, add_geo = FALSE)
  data <- rbind_labelled(extract$KEPR72FL)
  extract_male_fertility(data = data, country_code = "ke", year = "14")
  
  datasets <- dhs_datasets(
    surveyIds =  "KE2003DHS",
    fileFormat = "FL",
    fileType = "PR")
  downloads <- get_datasets(datasets$FileName)
  vars <- c("hhid", "hv001", "hv002", "hv009", "hv104", "hv105", "hv101", "hv113", "hv114", "hv111",
            "hv112", "hv006", "hv007", "hv270", "ha40", "hb40", "ha35", "hb35", "ha36",
            "hv252", "ha57", "hb57")
  questions <- search_variables(datasets$FileName, variables = vars)
  extract <- extract_dhs(questions, add_geo = FALSE)
  data <- rbind_labelled(extract$KEPR42FL)
  extract_male_fertility(data = data, country_code = "ke", year = "03")
  
  
} else if (country == "ng"){
  #------------------------------------------------------------------------------------------------------------------------
  # Nigeria
  datasets <- dhs_datasets(
    surveyIds =  "NG2018DHS",
    fileFormat = "FL",
    fileType = "PR")
  downloads <- get_datasets(datasets$FileName)
  vars <- c("hhid", "hv001", "hv002", "hv009", "hv104", "hv105", "hv101", "hv113", "hv114", "hv111",
            "hv112", "hv006", "hv007", "hv270", "ha40", "hb40", "ha35", "hb35", "ha36",
            "hv252", "ha57", "hb57")
  questions <- search_variables(datasets$FileName, variables = vars)
  extract <- extract_dhs(questions, add_geo = FALSE)
  data <- rbind_labelled(extract$NGPR7AFL)
  extract_male_fertility(data = data, country_code = "ng", year = "18")
  
  datasets <- dhs_datasets(
    surveyIds =  "NG2013DHS",
    fileFormat = "FL",
    fileType = "PR")
  downloads <- get_datasets(datasets$FileName)
  vars <- c("hhid", "hv001", "hv002", "hv009", "hv104", "hv105", "hv101", "hv113", "hv114", "hv111",
            "hv112", "hv006", "hv007", "hv270", "ha40", "hb40", "ha35", "hb35", "ha36",
            "hv252", "ha57", "hb57")
  questions <- search_variables(datasets$FileName, variables = vars)
  extract <- extract_dhs(questions, add_geo = FALSE)
  data <- rbind_labelled(extract$NGPR6AFL)
  extract_male_fertility(data = data, country_code = "ng", year = "13")

  datasets <- dhs_datasets(
    surveyIds =  "NG2008DHS",
    fileFormat = "FL",
    fileType = "PR")
  downloads <- get_datasets(datasets$FileName)
  vars <- c("hhid", "hv001", "hv002", "hv009", "hv104", "hv105", "hv101", "hv113", "hv114", "hv111",
            "hv112", "hv006", "hv007", "hv270", "ha40", "hb40", "ha35", "hb35", "ha36",
            "hv252", "ha57", "hb57")
  questions <- search_variables(datasets$FileName, variables = vars)
  extract <- extract_dhs(questions, add_geo = FALSE)
  data <- rbind_labelled(extract$NGPR53FL)
  extract_male_fertility(data = data, country_code = "ng", year = "08")

  datasets <- dhs_datasets(
    surveyIds =  "NG2003DHS",
    fileFormat = "FL",
    fileType = "PR")
  downloads <- get_datasets(datasets$FileName)
  vars <- c("hhid", "hv001", "hv002", "hv009", "hv104", "hv105", "hv101", "hv113", "hv114", "hv111",
            "hv112", "hv006", "hv007", "hv270", "ha40", "hb40", "ha35", "hb35", "ha36",
            "hv252", "ha57", "hb57")
  questions <- search_variables(datasets$FileName, variables = vars)
  extract <- extract_dhs(questions, add_geo = FALSE)
  data <- rbind_labelled(extract$NGPR4CFL)
  extract_male_fertility(data = data, country_code = "ng", year = "03")
  
} else if (country == "zw"){
  #------------------------------------------------------------------------------------------------------------------------
  # Zimbabwe
  datasets <- dhs_datasets(
    surveyIds =  "ZW2015DHS",
    fileFormat = "FL",
    fileType = "PR")
  downloads <- get_datasets(datasets$FileName)
  vars <- c("hhid", "hv001", "hv002", "hv009", "hv104", "hv105", "hv101", "hv113", "hv114", "hv111",
            "hv112", "hv006", "hv007", "hv270", "ha40", "hb40", "ha35", "hb35", "ha36",
            "hv252", "ha57", "hb57")
  questions <- search_variables(datasets$FileName, variables = vars)
  extract <- extract_dhs(questions, add_geo = FALSE)
  data <- rbind_labelled(extract$ZWPR72FL)
  extract_male_fertility(data = data, country_code = "zw", year = "15")
  
  datasets <- dhs_datasets(
    surveyIds =  "ZW2010DHS",
    fileFormat = "FL",
    fileType = "PR")
  downloads <- get_datasets(datasets$FileName)
  vars <- c("hhid", "hv001", "hv002", "hv009", "hv104", "hv105", "hv101", "hv113", "hv114", "hv111",
            "hv112", "hv006", "hv007", "hv270", "ha40", "hb40", "ha35", "hb35", "ha36",
            "hv252", "ha57", "hb57")
  questions <- search_variables(datasets$FileName, variables = vars)
  extract <- extract_dhs(questions, add_geo = FALSE)
  data <- rbind_labelled(extract$ZWPR62FL)
  extract_male_fertility(data = data, country_code = "zw", year = "10")
  
  datasets <- dhs_datasets(
    surveyIds =  "ZW2005DHS",
    fileFormat = "FL",
    fileType = "PR")
  downloads <- get_datasets(datasets$FileName)
  vars <- c("hhid", "hv001", "hv002", "hv009", "hv104", "hv105", "hv101", "hv113", "hv114", "hv111",
            "hv112", "hv006", "hv007", "hv270", "ha40", "hb40", "ha35", "hb35", "ha36",
            "hv252", "ha57", "hb57")
  questions <- search_variables(datasets$FileName, variables = vars)
  extract <- extract_dhs(questions, add_geo = FALSE)
  data <- rbind_labelled(extract$ZWPR52FL)
  extract_male_fertility(data = data, country_code = "zw", year = "05")
  
} else if (country == "mw"){
  # # Malawi
  datasets <- dhs_datasets(
    surveyIds =  "MW2015DHS",
    fileFormat = "FL",
    fileType = "PR")
  downloads <- get_datasets(datasets$FileName)
  vars <- c("hhid", "hv001", "hv002", "hv009", "hv104", "hv105", "hv101", "hv113", "hv114", "hv111",
            "hv112", "hv006", "hv007", "hv270", "ha40", "hb40", "ha35", "hb35", "ha36",
            "hv252", "ha57", "hb57")
  questions <- search_variables(datasets$FileName, variables = vars)
  extract <- extract_dhs(questions, add_geo = FALSE)
  data <- rbind_labelled(extract$MWPR7AFL)
  extract_male_fertility(data = data, country_code = "mw", year = "15")

  datasets <- dhs_datasets(
    surveyIds =  "MW2010DHS",
    fileFormat = "FL",
    fileType = "PR")
  downloads <- get_datasets(datasets$FileName)
  vars <- c("hhid", "hv001", "hv002", "hv009", "hv104", "hv105", "hv101", "hv113", "hv114", "hv111",
            "hv112", "hv006", "hv007", "hv270", "ha40", "hb40", "ha35", "hb35", "ha36",
            "hv252", "ha57", "hb57")
  questions <- search_variables(datasets$FileName, variables = vars)
  extract <- extract_dhs(questions, add_geo = FALSE)
  data <- rbind_labelled(extract$MWPR61FL)
  extract_male_fertility(data = data, country_code = "mw", year = "10")

  datasets <- dhs_datasets(
    surveyIds =  "MW2004DHS",
    fileFormat = "FL",
    fileType = "PR")
  downloads <- get_datasets(datasets$FileName)
  vars <- c("hhid", "hv001", "hv002", "hv009", "hv104", "hv105", "hv101", "hv113", "hv114", "hv111",
            "hv112", "hv006", "hv007", "hv270", "ha40", "hb40", "ha35", "hb35", "ha36",
            "hv252", "ha57", "hb57")
  questions <- search_variables(datasets$FileName, variables = vars)
  extract <- extract_dhs(questions, add_geo = FALSE)
  data <- rbind_labelled(extract$MWPR4EFL)
  extract_male_fertility(data = data, country_code = "mw", year = "04")
}








