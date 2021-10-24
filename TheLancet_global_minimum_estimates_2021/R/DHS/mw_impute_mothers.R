library(dplyr)
library(ggplot2)

set.seed(15)

# Functions -------------------------------------------------------------------
# Defines age categories that group corresponds to
labels = c("(15-20]", "(20-25]", "(25-30]", "(30-35]", "(35-40]", 
           "(40-45]", "(45-50]", "(50-55]", "(55-60]", "(60-65]", "(65-70]",
           "(70-75]", "(75-80]", "80+")

# Age groups
grouping <- function(age){
  if (age >= 15 & age < 20){
    return (1)
  } else if (age >= 20 & age < 25){
    return (2)
  } else if (age >=25 & age < 30){
    return (3)
  } else if (age >= 30 & age < 35){
    return (4)
  } else if (age >= 35 & age < 40){
    return (5)
  } else if (age >= 40 & age < 45){
    return (6)
  } else if (age >= 45 & age < 50){
    return (7)
  } else if (age >= 50 & age < 55){
    return (8)
  } else if (age >= 55 & age < 60){
    return (9)
  } else if (age >= 60 & age < 65){
    return (10)
  } else if (age >= 65 & age < 70){
    return (11)
  } else if (age >= 70 & age < 75){
    return (12)
  } else if (age >= 75 & age < 80){
    return(13)
  } else {
    return (14)
  }
}

age_spent <- function(row, y){
  id <- as.numeric(row[2])
  age_month <- as.numeric(row[9])
  age <- age_month - 5*(y-1)
  time_age_cat <- vector(mode = "numeric", length =  15)
  if (age > 960){
    if (age > 960 + 60){
      time_age_cat[15] = 60
    } else {
      age_in_category <- age - 960
      time_age_cat[15] <- age_in_category
      time_age_cat[14] <- 5*12 - age_in_category
    }
  } else if (age > 900){
    age_in_category <- age - 900
    time_age_cat[14] <- age_in_category
    time_age_cat[13] <- 5*12 - age_in_category 
  } else if (age > 840){
    age_in_category <- age - 840
    time_age_cat[13] <- age_in_category
    time_age_cat[12] <- 5*12 - age_in_category
  } else if (age > 780){
    age_in_category <- age - 780
    time_age_cat[12] <- age_in_category
    time_age_cat[11] <- 5*12 - age_in_category
  } else if (age > 720){
    age_in_category <- age - 720
    time_age_cat[11] <- age_in_category
    time_age_cat[10] <- 5*12 - age_in_category
  } else if (age > 660){
    age_in_category <- age - 660
    time_age_cat[10] <- age_in_category
    time_age_cat[9] <- 5*12 - age_in_category
  } else if (age > 600){
    age_in_category <- age - 600
    time_age_cat[9] <- age_in_category
    time_age_cat[8] <- 5*12 - age_in_category
  } else if (age > 540){
    age_in_category <- age - 540
    time_age_cat[8] <- age_in_category
    time_age_cat[7] <- 5*12 - age_in_category
  } else if (age > 480){
    age_in_category <- age - 480
    time_age_cat[7] <- age_in_category
    time_age_cat[6] <- 5*12 - age_in_category
  } else if (age > 420){
    age_in_category <- age - 420
    time_age_cat[6] <- age_in_category
    time_age_cat[5] <- 5*12 - age_in_category
  } else if (age > 360){
    age_in_category <- age - 360
    time_age_cat[5] <- age_in_category
    time_age_cat[4] <- 5*12 - age_in_category
  } else if (age > 300){
    age_in_category <- age - 300
    time_age_cat[4] <- age_in_category
    time_age_cat[3] <- 5*12 - age_in_category
  } else if (age > 240){
    age_in_category <- age - 240
    time_age_cat[3] <- age_in_category
    time_age_cat[2] <- 5*12 - age_in_category
  } else if (age > 180){
    age_in_category <- age - 180
    time_age_cat[2] <- age_in_category
    time_age_cat[1] <- 5*12 - age_in_category
  } else {
    time_age_cat[1] <- 60
  }
  if(sum(time_age_cat) != 60){
    print(id)
    print((time_age_cat))
    stop()
  }
  return (c(id, time_age_cat))
}

#-----------------------------------------------------------------------------
all_fertility <- NULL

for (y in 1:3){
  # Read in DHS data
  if (y == 1){
    all_data <- readRDS("data/DHS/mw_mothers_15.RDS")
  } else if (y == 2){
    all_data <- readRDS("data/DHS/mw_mothers_10.RDS")
  } else {
    all_data <- readRDS("data/DHS/mw_mothers_04.RDS")
  }
  
  all_data <- select(all_data, -mother_wi, -mother_bmi, -mother_smoking, -mother_anemia)
  
  # Survey year
  year <- floor(all_data$survey_date[1])
  # Add one to year to make labelling easier later
  year_tmp <- year + 1
  
  # Separate out women who are mothers
  mother_data <- all_data[which(!is.na(all_data$age_child_now)),]
  
  # Separate out full data
  full <- mother_data[which(mother_data$age_mother_now > 0 & mother_data$age_father_now >0 & mother_data$mother_status == 1),]
  not_nec_fathers <- mother_data[which(mother_data$age_mother_now > 0),]

  # Makes a null fertility vector
  fertility <- NULL
  # Repeat bootstrapping 10 times
  for (j in 1:10){
    # Assigns mothers to missing children
    for (i in 1:length(mother_data$id_household)){
      # Only adjust those with missing mother data
      if (is.na(mother_data$age_mother_now[i])){
        # What is age of this child
        age_child <- mother_data$age_child_now[i]
        # What is age of father
        age_father <- mother_data$age_father_now[i]
        # If father status is missing or unknown - work out if father should be alive
        if (mother_data$mother_status[i] > 1){
          other_children <- mother_data[which(mother_data$age_child_now == age_child),]
          if (!is.na(age_father)){
            if (age_father != 0){
              other_children <- other_children[which(other_children$age_father_now == age_father),]
            }
          }
          prop_live = sum(other_children$mother_status == 1) / length(other_children$mother_status)
          p <- runif(1, 0, 1)
          if (p < prop_live){
            mother_data$mother_status[i] = 1
          } else {
            mother_data$mother_status[i] = 0
          }
        }
        
        # Find other children with data that matches
        if (age_father == 0 | is.na(age_father)){
          others <- not_nec_fathers[which(not_nec_fathers$age_child_now == age_child),]
        } else {
          others <- full[which(full$age_child_now == age_child & full$age_father_now),]
        }
        # Won't mater if can't match up older children since subset last 5 years
        if(length(others$id_household) > 0){
          # Randomly select a child from the others list
          new_mother <- others[sample(1:length(others$id_household), 1),]
          mother_data$age_mother_now[i] <- new_mother$age_mother_now
          mother_data$new_id[i] <- new_mother$id
        }
      }
    }
    
    # Removes children with dead mother
    mother_data <- mother_data[which(mother_data$mother_status == 1),]
    # Data where every child has age of mother
    mother_data$age_mother_birth <- mother_data$age_mother_now - mother_data$age_child_now
    
    # select children born in last 5 year
    selected_children <- mother_data[which(mother_data$age_child_now < 5),]

    # Summarise number of children in each age
    summ <- selected_children %>%
      group_by(age_mother_birth) %>%
      summarize(count = n())
    
    # Plots a histogram 
    #p <- ggplot(summ, aes(age_mother_birth, count)) + geom_col() + theme_bw()
    #print(p)
    
    # Calculate number of children in each age category
    summ$group <- sapply(summ$age_mother_birth, grouping)
    band_children <- summ %>%
      group_by(group) %>%
      summarise("births" = sum(count)) 
    band_children$label <- labels[1:length(band_children$group)]
    
    # Work out exposure
    # Removes all lines where mother doesn't have an age as we match them to other mothers
    exposure <- all_data[!is.na(all_data$age_mother_now),]
    # Subset so only have one line per man
    unique_women <- exposure %>% distinct(id_woman, .keep_all= TRUE)
    # Randomly assign each woman a birth month
    unique_women$woman_age_month <- sample(0:11, length(unique_women$id_woman), replace = TRUE)
    # Work out womens age in months
    unique_women$age_months <- unique_women$age_mother_now*12 + unique_women$woman_age_month
    # Remove women (girls) under the age of 5*years
    unique_women <- unique_women[which(unique_women$age_mother_now > 5*y),]
    
    # Calculate number of years spent in each age category
    exposure_months <- t(apply(unique_women, 1, age_spent, y=y))
    exposure_years <- data.frame("exposure_years" = colSums(exposure_months[,2:16])/12)
    exposure_years$label <- c("under 15", labels)
    
    # Calculate fertility
    data <- left_join(exposure_years, band_children)
    data$fertility <- data$births/data$exposure_years
    
    fertility <- cbind(fertility, data$fertility)
  }
  
  fertility_rate = rowMeans(fertility)
  
  if (y == 2){
    for (i in 1:6){
      all_fertility <- cbind(all_fertility, fertility_rate)
    }
  } else {
    for (i in 1:5){
      all_fertility <- cbind(all_fertility, fertility_rate)
    }
  }

  #p <- ggplot(all_fertility %>% filter(!is.na(fertility_rate))) + 
  #  geom_point(aes(age, fertility_rate)) + theme_bw() + 
  #  ylab("Female fertility rate") + xlab("Age category")
  #print(p)
}

df_fertility <- data.frame(all_fertility)
names(df_fertility) <- paste0("y", 2015 - 0:15)
df_fertility$ages <- factor(exposure_years$label, levels = c("under 15", labels))

saveRDS(df_fertility, file = "data/Malawi/female_fertility.RDS")

p <- ggplot(df_fertility) + 
  geom_point(aes(ages, y2015, col = "2015")) + 
  geom_point(aes(ages, y2010, col = "2010")) +
  geom_point(aes(ages, y2004, col = "2004")) + 
  scale_color_manual(values = c("2015" = "black", "2010" = "blue", "2004" = "red")) + 
  theme_bw()
print(p)