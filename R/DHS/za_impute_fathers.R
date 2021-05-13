library(dplyr)
library(ggplot2)

set.seed(10)

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
# Read in DHS data
all_data <- readRDS("data/DHS/za_fathers_16.RDS")
all_data <- select(all_data, - father_wi, -father_bmi, -father_smoking, -father_anemia)

# Survey year
year <- floor(all_data$survey_date[1])
# Add one to year to make labelling easier later
year_tmp <- year + 1

# Separate out men who are fathers
father_data <- all_data[which(!is.na(all_data$age_child_now)),]

# Separate out full data
full <- father_data[which(father_data$age_father_now > 0 & father_data$age_mother_now >0 & father_data$father_status == 1),]
not_nec_mothers <- father_data[which(father_data$age_father_now > 0),]

all_fertility <- NULL

for (y in 1:3){
  # Makes a null fertility vector
  fertility <- NULL
  # Repeat bootstrapping 10 times
  for (j in 1:10){
    # Assigns fathers to missing children
    for (i in 1:length(father_data$id_household)){
      # Only adjust those with missing father data
      if (is.na(father_data$age_father_now[i])){
        # What is age of this child
        age_child <- father_data$age_child_now[i]
        # What is age of mother
        age_mother <- father_data$age_mother_now[i]
        # If father status is missing or unknown - work out if father should be alive
        if (father_data$father_status[i] > 1){
          other_children <- father_data[which(father_data$age_child_now == age_child),]
          if (!is.na(age_mother)){
            if (age_mother != 0){
              other_children <- other_children[which(other_children$age_mother_now == age_mother),]
            }
          }
          prop_live = sum(other_children$father_status == 1) / length(other_children$father_status)
          p <- runif(1, 0, 1)
          if (p < prop_live){
            father_data$father_status[i] = 1
          } else {
            father_data$father_status[i] = 0
          }
        }
        
        # Find other children with data that matches
        if (age_mother == 0 | is.na(age_mother)){
          others <- not_nec_mothers[which(not_nec_mothers$age_child_now == age_child),]
        } else {
          others <- full[which(full$age_child_now == age_child & full$age_mother_now),]
        }
        # Randomly select a child from the others list
        new_father <- others[sample(1:length(others$id_household), 1),]
        father_data$age_father_now[i] <- new_father$age_father_now
        father_data$new_id[i] <- new_father$id
      }
    }
    
    # Removes children with dead father
    father_data <- father_data[which(father_data$father_status == 1),]
    # Data where every child has age of father
    father_data$age_father_birth <- father_data$age_father_now - father_data$age_child_now
    
    # select children born in last 5 year
    if (y == 1){
      selected_children <- father_data[which(father_data$age_child_now < 5),]
    } else {
      selected_children <- father_data[which(father_data$age_child_now >= 5*(y-1) & father_data$age_child_now < 5*y),]
    }

    # Summarise number of children in each age
    summ <- selected_children %>%
      group_by(age_father_birth) %>%
      summarize(count = n())
    
    # Plots a histogram 
    #p <- ggplot(summ, aes(age_father_birth, count)) + geom_col() + theme_bw()
    #print(p)
    
    # Calculate number of children in each age category
    summ$group <- sapply(summ$age_father_birth, grouping)
    band_children <- summ %>%
      group_by(group) %>%
      summarise("births" = sum(count)) 
    band_children$label <- labels[1:length(band_children$group)]
    
    # Work out exposure
    # Removes all lines where father doesn't have an age as we match them to other fathers
    exposure <- all_data[!is.na(all_data$age_father_now),]
    # Subset so only have one line per man
    unique_men <- exposure %>% distinct(id_man, .keep_all= TRUE)
    # Randomly assign each man a birth month
    unique_men$man_age_month <- sample(0:11, length(unique_men$id_man), replace = TRUE)
    # Work out mens age in months
    unique_men$age_months <- unique_men$age_father_now*12 + unique_men$man_age_month
    # Remove men (boys) under the age of 5*years
    unique_men <- unique_men[which(unique_men$age_father_now > 5*y),]
    
    # Calculate number of years spent in each age category
    exposure_months <- t(apply(unique_men, 1, age_spent, y=y))
    exposure_years <- data.frame("exposure_years" = colSums( exposure_months[,2:16])/12)
    exposure_years$label <- c("under 15", labels)
    
    # Calculate fertility
    data <- left_join(exposure_years, band_children)
    data$fertility <- data$births/data$exposure_years
    
    fertility <- cbind(fertility, data$fertility)
  }
  
  fertility_rate = rowMeans(fertility)
  
  for (i in 1:5){
    year_tmp <- year_tmp - 1
    all_fertility <- cbind(all_fertility, fertility_rate)
  }
  
  #p <- ggplot(all_fertility %>% filter(!is.na(fertility_rate))) + 
  #  geom_point(aes(age, fertility_rate)) + theme_bw() + 
  #  ylab("Male fertility rate") + xlab("Age category")
  #print(p)
}

df_fertility <- data.frame(all_fertility)
names(df_fertility) <- paste0("y", year - 0:14)
df_fertility$ages <- factor(exposure_years$label, levels = c("under 15", labels))
saveRDS(df_fertility, file = "data/SouthAfrica/male_fertility.RDS")

p <- ggplot(df_fertility) + 
  geom_point(aes(ages, y2016, col = "2016")) + 
  geom_point(aes(ages, y2011, col = "2011")) +
  geom_point(aes(ages, y2006, col = "2006")) + 
  scale_color_manual(values = c("2016" = "black", "2011" = "blue", "2006" = "red")) + 
  theme_bw()
print(p)
ggsave("za_male.png", p)