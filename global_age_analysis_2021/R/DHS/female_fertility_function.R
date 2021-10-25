library(rdhs)
library(zoo)
library(lubridate)

extract_female_fertility <- function(data, country_code, year){
  data$id <- paste(data$hv001, data$hv002, sep = "_")
  
  ids <- unique(data$hhid)
  
  id_household <- NULL
  id_woman <- NULL
  age_father_now <- NULL
  age_child_now <- NULL
  age_mother_now <- NULL
  survey_date <- NULL
  mother_status <- NULL
  mother_wi <- NULL
  mother_bmi <- NULL
  mother_smoking <- NULL
  household_smoking <- NULL
  mother_anemia <- NULL
  
  # Replace nas in mother's alive with don't know
  data$hv111[is.na(data$hv111)] <- 8
  data$hv113[is.na(data$hv113)] <- 8
  
  # Replace NAs in fathers/mothers line no
  data$hv114[is.na(data$hv114)] <- 99
  data$hv112[is.na(data$hv112)] <- 99
  
  for (i in 1:length(ids)){
    # Reset line number of mothers
    line_num_mother <- NULL
    
    household <- data[which(data$hhid == ids[i]),]
    
    # Check household size 
    if (length(household$hv009) != household$hv009[1]){
      print("Length household doesn't match household size.")
    }
    
    # Check if under children under 18
    children <- household[which(household$hv105 < 18),]
    if (length(children$hv105) != 0){
      for (j in 1:length(children$hv105)){
        # Extracts child data
        child <- children[j,]
        # Records fathers status for imputation later 0 - dead, 1 - alive, 8 - don't know, 9 - missing
        mother_status <- c(mother_status, child$hv111)
        
        # Saves id for that child
        id_household <- c(id_household, child$hhid)
        # Adds age of child to list
        age_child_now <- c(age_child_now, child$hv105)
        # Looks at age of mother
        line_num_mother <- c(line_num_mother, child$hv112)
        # If mother isn't in household or line number is missing add NA
        if (child$hv112 == 0 | child$hv112 == 99){
          age_mother_now <- c(age_mother_now, NA)
          survey_date <- c(survey_date, as.yearmon(as.yearmon(paste(child$hv007, child$hv006, sep = "-")), "%Y %m"))
          mother_wi <- c(mother_wi, NA)
          mother_bmi <- c(mother_bmi, NA)
          mother_smoking <- c(mother_smoking, NA)
          household_smoking <- c(household_smoking, NA)
          mother_anemia <- c(mother_anemia, NA)
        } else {
          mother = household[child$hv112, ]
          survey_date <- c(survey_date, as.yearmon(as.yearmon(paste(mother$hv007, mother$hv006, sep = "-")), "%Y %m"))
          if (is.null(mother$hv270)){
            mother_wi <- c(mother_wi, NA)
          } else {
            mother_wi <- c(mother_wi, mother$hv270)
          }
          if (is.null(mother$ha40)){
            mother_bmi <- c(mother_bmi, NA)
          } else {
            mother_bmi <- c(mother_bmi, mother$ha40)
          }
          if (is.null(mother$ha35)){
            mother_smoking <- c(mother_smoking, NA)
          } else {
            mother_smoking <- c(mother_smoking, mother$ha35)
          }
          if (is.null(mother$hv252)){
            household_smoking <- c(household_smoking, NA)
          } else {
            household_smoking <- c(household_smoking, mother$hv252)
          }
          if (is.null(mother$ha57)){
            mother_anemia <- c(mother_anemia, NA)
          } else {
            mother_anemia <- c(mother_anemia, mother$ha57)
          }
          if (mother$hv105 > 95){
            age_mother_now <- c(age_mother_now, NA)
          } else {
            age_mother_now <- c(age_mother_now, mother$hv105)
          }
        }
        # Looks at age of father
        if (child$hv113 == 0){ # If father is dead set age to be 0
          age_father_now = c(age_father_now, 0)
        } else if (child$hv114 == 0 | child$hv114 == 99){ # If father information is missing or not in household add NA
          age_father_now <- c(age_father_now, NA)
        } else {
          father = household[child$hv114, ]
          if (father$hv105 > 95){
            age_father_now <- c(age_father_now, NA)
          } else {
            age_father_now <- c(age_father_now, father$hv105)
          }
        }
      }
    }
    # If have mothers need to give them ids
    if (length(line_num_mother) > 0){
      # Gives each mothers an id
      unique_mothers <- unique(line_num_mother)
      # make mothers ids
      if (length(id_woman) == 0){
        mother_ids <- 1:(1 + length(unique_mothers) - 1)
      } else {
        mother_ids <- (max(id_woman)+1):(max(id_woman) + length(unique_mothers))
      }
      tmp_ids <- data.frame(unique_mothers, mother_ids)
      
      tmp_combine_ids <- vector(length = length(line_num_mother))
      for (n in 1:length(line_num_mother)){
        tmp_combine_ids[n] = tmp_ids$mother_ids[which(tmp_ids$unique_mothers == line_num_mother[n])]
      }
      
      id_woman <- c(id_woman, tmp_combine_ids)
    }
    
    # Remove mothers from the household
    no_mothers <- household[which(! 1:length(household$hv001) %in% line_num_mother),]
    
    # add other women to list
    age_no_mother <- no_mothers$hv105[which(no_mothers$hv104 == 2)]
    if (is.null(no_mothers$hv270[which(no_mothers$hv104 == 2)])){
      wi_no_mother <- rep(NA, length(age_no_mother))
    } else {
      wi_no_mother <- no_mothers$hv270[which(no_mothers$hv104 == 2)]
    }
    if (is.null(no_mothers$ha40[which(no_mothers$hv104 == 2)])){
      bmi_no_mother <- rep(NA, length(age_no_mother))
    } else {
      bmi_no_mother <- no_mothers$ha40[which(no_mothers$hv104 == 2)]
    }
    if (is.null(no_mothers$ha35[which(no_mothers$hv104 == 2)])){
      smoking_no_mother <- rep(NA, length(age_no_mother))
    } else {
      smoking_no_mother <- no_mothers$ha35[which(no_mothers$hv104 == 2)]
    }
    if (is.null(no_mothers$hv252[which(no_mothers$hv104 == 2)])){
      household_smoking_no_mother <- rep(NA, length(age_no_mother))
    } else {
      household_smoking_no_mother <- no_mothers$hv252[which(no_mothers$hv104 == 2)]
    }
    if (is.null(no_mothers$ha57[which(no_mothers$hv104 == 2)])){
      anemia_no_mother <- rep(NA, length(age_no_mother))
    } else {
      anemia_no_mother <- no_mothers$ha57[which(no_mothers$hv104 == 2)]
    }
    if (length(age_no_mother) > 0){
      age_mother_now <- c(age_mother_now, age_no_mother)
      age_father_now <- c(age_father_now, rep(NA, length(age_no_mother)))
      age_child_now <- c(age_child_now, rep(NA, length(age_no_mother)))
      mother_status <- c(mother_status, rep(NA, length(age_no_mother)))
      id_household <- c(id_household, rep(ids[i], length(age_no_mother)))
      survey_date <- c(survey_date, rep(as.yearmon(paste(household$hv007[1], household$hv006[1], sep = "-")), length(age_no_mother)))
      mother_wi <- c(mother_wi, wi_no_mother)
      mother_bmi <- c(mother_bmi, bmi_no_mother)
      mother_smoking <- c(mother_smoking, smoking_no_mother)
      household_smoking <- c(household_smoking, household_smoking_no_mother)
      mother_anemia <- c(mother_anemia, anemia_no_mother)
      if (length(id_woman) == 0){
        id_woman <- c(id_woman, 1:(length(age_no_mother)))
      } else {
        id_woman <- c(id_woman, ((max(id_woman)+1):(max(id_woman) + length(age_no_mother))))
      }
      
    }
  }
  # Combines data
  mother_data <- data.frame(id_household, id_woman, age_mother_now, age_father_now, age_child_now, 
                            mother_status, survey_date, mother_wi, mother_bmi, mother_smoking,
                            mother_anemia)
  
  if (is.na(year)){
    saveRDS(mother_data, paste0("data/DHS/", country_code, "_mothers.RDS"))
  } else {
    saveRDS(mother_data, paste0("data/DHS/", country_code, "_mothers_", year, ".RDS"))
  }
}

#-----------------------------------------------------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)

country = args[1]
print(country)

## Colombia
if (country == "co"){
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
  extract_female_fertility (data = data, country_code = "co", year = "15")
  
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
  extract_female_fertility (data = data, country_code = "co", year = "05")
  
datasets <- dhs_datasets(
  surveyIds = "CO2010DHS",
  fileFormat = "FL",
  fileType = "PR")
downloads <- get_datasets(datasets$FileName)
vars <- c("hhid", "hv001", "hv002", "hv009", "hv104", "hv105", "hv101", "hv113", "hv114", "hv111", 
          "hv112", "hv006", "hv007", "hv270", "ha40", "hb40", "ha35", "hb35", "ha36", 
          "hv252", "ha57", "hb57")
questions <- search_variables(datasets$FileName, variables = vars)
extract <- extract_dhs(questions, add_geo = TRUE)
data <- rbind_labelled(extract$COPR61FL)
extract_female_fertility (data = data, country_code = "co", year = "10")
 
} else if (country == "za"){
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
  extract_female_fertility(data = data, country_code = "za")

} else if (country == "ia"){
 # India
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
  extract_female_fertility(data = data, country_code = "ia", year = "15")
  
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
  extract_female_fertility(data = data, country_code = "ia", year = "06")

} else if (country == "pe"){
# Peru
  print("2012")
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
  extract_female_fertility(data = data, country_code = "pe", year = "12")
  
  print("2011")
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
  extract_female_fertility(data = data, country_code = "pe", year = "11")
  
  print("2010")
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
  extract_female_fertility(data = data, country_code = "pe", year = "10")
  
  print("2009")
  datasets <- dhs_datasets(
    surveyIds =  "PE2009DHS",
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
  extract_female_fertility(data = data, country_code = "pe", year = "09")
  
  print("2007")
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
  extract_female_fertility(data = data, country_code = "pe", year = "07")
  
  print("2004")
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
  extract_female_fertility(data = data, country_code = "pe", year = "04")
 
} else if (country == "ke"){
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
  extract_female_fertility(data = data, country_code = "ke", year = "14")
  
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
  extract_female_fertility(data = data, country_code = "ke", year = "03")
  

} else if (country == "ng"){
  # # Nigeria
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
  extract_female_fertility(data = data, country_code = "ng", year = "18")
  
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
  extract_female_fertility(data = data, country_code = "ng", year = "13")

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
  extract_female_fertility(data = data, country_code = "ng", year = "08")

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
  extract_female_fertility(data = data, country_code = "ng", year = "03")

} else if (country == "zw"){
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
  extract_female_fertility(data = data, country_code = "zw", year = "15")

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
  extract_female_fertility(data = data, country_code = "zw", year = "10")

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
  extract_female_fertility(data = data, country_code = "zw", year = "05")

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
  extract_female_fertility(data = data, country_code = "mw", year = "15")

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
  extract_female_fertility(data = data, country_code = "mw", year = "10")

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
  extract_female_fertility(data = data, country_code = "mw", year = "04")
}

