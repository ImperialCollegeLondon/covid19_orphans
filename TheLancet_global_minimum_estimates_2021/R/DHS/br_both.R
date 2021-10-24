library(haven)
library(tidyverse)

data <- read_sas("data/Brazil/PES2015datasets/pes2015_lp.sas7bdat")
data_sub <- select(data, V0101, V0102, V0103, V0301, V0302, V3032, V3033, V8005, 
                   V0401,V0402, V0403, V0405, V0406, V0407, V0408)
data_sub$id <- paste(data_sub$V0102, data_sub$V0103, sep = "_")

ids <- unique(data_sub$id)

id_household_m <- NULL
id_household_f <- NULL
id_man <- NULL
id_woman <- NULL
age_father_now_m <- NULL
age_father_now_f <- NULL
age_child_now_m <- NULL
age_child_now_f <- NULL
age_mother_now_m <- NULL
age_mother_now_f <- NULL
mother_status <- NULL
birth_month_father <- NULL
birth_month_mother <- NULL

for (i in 1:length(ids)){
  # Reset line number of fathers
  line_num_mother <- NULL
  line_num_father <- NULL
  
  household <- data_sub[which(data_sub$id == ids[i]),]
  
  # Check household size 
  if (length(household$id) != as.numeric(max(household$V0301))){
    print("Length household doesn't match household size.")
  }
  
  # Extracts children 
  children <- household[which(household$V8005 < 18),]
  if (length(children$id) != 0){
    for (j in 1:length(children$id)){
      # Extracts child data
      child <- children[j,]
      # Saves id for that child
      id_household_m <- c(id_household_m, child$id)
      id_household_f <- c(id_household_f, child$id)
      # Adds age of child to list
      age_child_now_m <- c(age_child_now_m, child$V8005)
      age_child_now_f <- c(age_child_now_f, child$V8005)
      # Saves mother status
      mother_status <- c(mother_status, child$V0405)

      # if no line number for mother
      if (is.na(child$V0407) | child$V0407 == 99){
        # mother is unknown
        age_mother_now_m <- c(age_mother_now_m, NA)
        age_mother_now_f <- c(age_mother_now_f, NA)
        birth_month_mother <- c(birth_month_mother, sample(1:12, 1))
        line_num_mother <- c(line_num_mother, NA)
      # Mother line is known
      } else {
        # Finds line number of mother
        line_num_mother <- c(line_num_mother, child$V0407)
        mother <- household[which(as.numeric(household$V0301) == child$V0407),]
        age_mother_now_m <- c(age_mother_now_m, mother$V8005)
        age_mother_now_f <- c(age_mother_now_f, mother$V8005)
        birth_month_mother <- c(birth_month_mother, mother$V3032)
      }
      
      # Check if childs condition in family
      if (child$V0402 == 3){
        father <- household[which(household$V0401 == 1),]
        # Check there is a house holder
        if (length(father$id) != 1){
          stop("Don't find householder")
        }
        # If householder is male
        if (as.numeric(father$V0302) == 2){
          age_father_now_m <- c(age_father_now_m, father$V8005)
          age_father_now_f <- c(age_father_now_f, father$V8005)
          line_num_father <- c(line_num_father, father$V0403)
          birth_month_father <- c(birth_month_father, father$V3032)
        } else {
          age_father_now_m <- c(age_father_now_m, NA)
          age_father_now_f <- c(age_father_now_f, NA)
          line_num_father <- c(line_num_father, NA)
          birth_month_father <- c(birth_month_father, sample(1:12, 1))
        }
      } else {
        # Don't know who father is so leave blank
        age_father_now_m <- c(age_father_now_m, NA)
        age_father_now_f <- c(age_father_now_f, NA)
        line_num_father <- c(line_num_father, NA)
        birth_month_father <- c(birth_month_father, sample(1:12, 1))
      }
    }
  }
  
  # If have fathers need to give them ids
  if (length(children$id) > 0){
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
      if (is.na(line_num_father[n])){
        tmp_combine_ids[n] = tmp_ids$father_ids[which(is.na(tmp_ids$unique_fathers))]
      } else {
        tmp_combine_ids[n] = tmp_ids$father_ids[which(tmp_ids$unique_fathers == line_num_father[n])]
      }
    }
    
    id_man <- c(id_man, tmp_combine_ids)
  }
  
  # If have mothers need to give them ids
  if (length(line_num_mother) > 0){
    # Gives each father an id
    unique_mothers <- unique(line_num_mother)
    # make father ids
    if (length(id_woman) == 0){
      mother_ids <- 1:(1 + length(unique_mothers) - 1)
    } else {
      mother_ids <- (max(id_woman)+1):(max(id_woman) + length(unique_mothers))
    }
    tmp_ids <- data.frame(unique_mothers, mother_ids)

    tmp_combine_ids <- vector(length = length(line_num_mother))
    for (n in 1:length(line_num_mother)){
      if (is.na(line_num_mother[n])){
        tmp_combine_ids[n] = tmp_ids$mother_ids[which(is.na(tmp_ids$unique_mothers))]
      } else {
        tmp_combine_ids[n] = tmp_ids$mother_ids[which(tmp_ids$unique_mothers == line_num_mother[n])]
      }

    }

    id_woman <- c(id_woman, tmp_combine_ids)
  }
  
  # Remove fathers from the household
  no_fathers <- household[which(! 1:length(household$V0403) %in% line_num_father),]
  
  # add other men to list
  age_no_father <- no_fathers$V8005[which(no_fathers$V0302 == 2)]
  if (length(age_no_father) > 0){
    age_father_now_m <- c(age_father_now_m, age_no_father)
    age_mother_now_m <- c(age_mother_now_m, rep(NA, length(age_no_father)))
    age_child_now_m <- c(age_child_now_m, rep(NA, length(age_no_father)))
    id_household_m <- c(id_household_m, rep(ids[i], length(age_no_father)))
    birth_month_father <- c(birth_month_father, sample(1:12, length(age_no_father), replace = TRUE))
    if (length(id_man) == 0){
      id_man <- c(id_man, 1:(length(age_no_father)))
    } else {
      id_man <- c(id_man, (max(id_man)+1):(max(id_man) + length(age_no_father)))
    }
    
  }
  
  # Remove mothers from the household
  no_mothers <- household[which(! 1:length(household$V0403) %in% line_num_mother),]

  # add other men to list
  age_no_mother <- no_mothers$V8005[which(no_mothers$V0302 == 4)]
  if (length(age_no_mother) > 0){
    age_mother_now_f <- c(age_mother_now_f, age_no_mother)
    age_father_now_f <- c(age_father_now_f, rep(NA, length(age_no_mother)))
    age_child_now_f <- c(age_child_now_f, rep(NA, length(age_no_mother)))
    mother_status <- c(mother_status, rep(NA, length(age_no_mother)))
    id_household_f <- c(id_household_f, rep(ids[i], length(age_no_mother)))
    birth_month_mother <- c(birth_month_mother, sample(1:12, length(age_no_mother), replace = TRUE))
    if (length(id_woman) == 0){
      id_woman <- c(id_woman, 1:(length(age_no_mother)))
    } else {
      id_woman <- c(id_woman, (max(id_woman)+1):(max(id_woman) + length(age_no_mother)))
    }

  }
}

father_data <- data.frame(id_household = id_household_m, id_man, age_father_now = age_father_now_m, 
                          age_mother_now = age_mother_now_m, age_child_now = age_child_now_m, 
                          birth_month_father)
saveRDS(father_data, paste0("data/DHS/brazil_fathers.RDS"))
mother_data <- data.frame(id_household = id_household_f, id_woman, age_father_now = age_father_now_f, 
                          age_mother_now = age_mother_now_f, age_child_now = age_child_now_f, 
                          mother_status, birth_month_mother)
saveRDS(mother_data, paste0("data/DHS/brazil_mothers.RDS"))
