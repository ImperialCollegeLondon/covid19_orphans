ratio = NULL
country = NULL

# Argentina
dat  = read.csv("data/Argentina/all_data.csv")
dat$group = ifelse(dat$age == "60+", "old", "young")
dat = dat %>%
  group_by(group) %>%
  summarise(ratio = sum(covid19_deaths))
ratio = c(ratio, dat$ratio[dat$group == "old"]/dat$ratio[dat$group == "young"])
country = c(country, "Argentina")

# Brazil
dat  = read.csv("data/Brazil/all_data.csv")
dat$group = ifelse(dat$age == "65+", "old", "young")
dat = dat %>%
  group_by(group) %>%
  summarise(ratio = sum(covid19_deaths))
ratio = c(ratio, dat$ratio[dat$group == "old"]/dat$ratio[dat$group == "young"])
country = c(country, "Brazil")

# Colombia
dat  = read.csv("data/Colombia/all_data.csv")
dat$group = ifelse(dat$age == "65+", "old", "young")
dat = dat %>%
  group_by(group) %>%
  summarise(ratio = sum(covid19_deaths))
ratio = c(ratio, dat$ratio[dat$group == "old"]/dat$ratio[dat$group == "young"])
country = c(country, "Colombia")

# England and Wales
dat  = read.csv("data/UK/england_wales_orphans_all.csv")
dat$group = ifelse(dat$age%in% c("65-69", "70-74", "75-79", "80-84", "85-89", "90+"), "old", "young")
dat = dat %>%
  group_by(group) %>%
  summarise(ratio = sum(deaths))
ratio = c(ratio, dat$ratio[dat$group == "old"]/dat$ratio[dat$group == "young"])
country = c(country, "England & Wales")
            
# France
dat = read.csv("data/France/all_data.csv")
dat$group = ifelse(dat$age == "70+", "old", "young")
dat = dat %>%
  group_by(group) %>%
  summarise(ratio = sum(nb_deaths))
ratio = c(ratio, dat$ratio[dat$group == "old"]/dat$ratio[dat$group == "young"])
country = c(country, "France")

# Germany
dat  = read.csv("data/Germany/all_data.csv")
dat$group = ifelse(dat$age == "70+", "old", "young")
dat = dat %>%
  group_by(group) %>%
  summarise(ratio = sum(COVID19_deaths))
ratio = c(ratio, dat$ratio[dat$group == "old"]/dat$ratio[dat$group == "young"])
country = c(country, "Germany")

# India
dat  = read.csv("data/India/all_data.csv")
dat$group = ifelse(dat$age == "70+", "old", "young")
dat = dat %>%
  group_by(group) %>%
  summarise(ratio = sum(covid_deaths))
ratio = c(ratio, dat$ratio[dat$group == "old"]/dat$ratio[dat$group == "young"])
country = c(country, "India")

# Iran
dat  = read.csv("data/Iran/all_data.csv")
dat$group = ifelse(dat$age == "70+", "old", "young")
dat = dat %>%
  group_by(group) %>%
  summarise(ratio = sum(max_deaths))
ratio = c(ratio, dat$ratio[dat$group == "old"]/dat$ratio[dat$group == "young"])
country = c(country, "Iran (Islamic Republic of)")

# Italy
dat  = read.csv("data/Italy/all_data.csv")
dat$group = ifelse(dat$age == "70+", "old", "young")
dat = dat %>%
  group_by(group) %>%
  summarise(ratio = sum(max_deaths))
ratio = c(ratio, dat$ratio[dat$group == "old"]/dat$ratio[dat$group == "young"])
country = c(country, "Italy")

# Kenya
dat  = read.csv("data/Kenya/all_data.csv")
dat$group = ifelse(dat$age == "60+", "old", "young")
dat = dat %>%
  group_by(group) %>%
  summarise(ratio = sum(COVID19_deaths))
ratio = c(ratio, dat$ratio[dat$group == "old"]/dat$ratio[dat$group == "young"])
country = c(country, "Kenya")

# Malawi
dat  = read.csv("data/Malawi/all_data.csv")
dat$group = ifelse(dat$age == "70+", "old", "young")
dat = dat %>%
  group_by(group) %>%
  summarise(ratio = sum(COVID19_deaths))
ratio = c(ratio, dat$ratio[dat$group == "old"]/dat$ratio[dat$group == "young"])
country = c(country, "Malawi")

# Mexico
dat  = read.csv("data/Mexico/all_data.csv")
dat$group = ifelse(dat$age == "65+", "old", "young")
dat = dat %>%
  group_by(group) %>%
  summarise(ratio = sum(COVID19_deaths))
ratio = c(ratio, dat$ratio[dat$group == "old"]/dat$ratio[dat$group == "young"])
country = c(country, "Mexico")

# Nigeria
dat  = read.csv("data/Nigeria/all_data.csv")
dat$group = ifelse(dat$age ==  "70+", "old", "young")
dat = dat %>%
  group_by(group) %>%
  summarise(ratio = sum(COVID19_deaths))
ratio = c(ratio, dat$ratio[dat$group == "old"]/dat$ratio[dat$group == "young"])
country = c(country, "Nigeria")


# Peru
dat  = read.csv("data/Peru/all_data.csv")
dat$group = ifelse(dat$age == "70+", "old", "young")
dat = dat %>%
  group_by(group) %>%
  summarise(ratio = sum(COVID19_deaths))
ratio = c(ratio, dat$ratio[dat$group == "old"]/dat$ratio[dat$group == "young"])
country = c(country, "Peru")

# Philippines
dat  = read.csv("data/Philippines/all_data.csv")
dat$group = ifelse(dat$age == "65+", "old", "young")
dat = dat %>%
  group_by(group) %>%
  summarise(ratio = sum(max_deaths))
ratio = c(ratio, dat$ratio[dat$group == "old"]/dat$ratio[dat$group == "young"])
country = c(country, "Philippines")

# Poland
dat  = read.csv("data/Poland/all_data.csv")
dat$group = ifelse(dat$age == "70+", "old", "young")
dat = dat %>%
  group_by(group) %>%
  summarise(ratio = sum(max_deaths))
ratio = c(ratio, dat$ratio[dat$group == "old"]/dat$ratio[dat$group == "young"])
country = c(country, "Poland")

# Russia
dat  = read.csv("data/Russia/all_data.csv")
dat$group = ifelse(dat$age == "65+", "old", "young")
dat = dat %>%
  group_by(group) %>%
  summarise(ratio = sum(max_deaths))
ratio = c(ratio, dat$ratio[dat$group == "old"]/dat$ratio[dat$group == "young"])
country = c(country, "Russia")

# South Africa
dat  = read.csv("data/SouthAfrica/all_data.csv")
dat$group = ifelse(dat$age == "70+", "old", "young")
dat = dat %>%
  group_by(group) %>%
  summarise(ratio = sum(covid19_deaths))
ratio = c(ratio, dat$ratio[dat$group == "old"]/dat$ratio[dat$group == "young"])
country = c(country, "South Africa")

# Spain
dat  = read.csv("data/Spain/all_data.csv")
dat$group = ifelse(dat$age == "70+", "old", "young")
dat = dat %>%
  group_by(group) %>%
  summarise(ratio = sum(max_deaths))
ratio = c(ratio, dat$ratio[dat$group == "old"]/dat$ratio[dat$group == "young"])
country = c(country, "Spain")

# US
dat  = read.csv("data/USA/all_data.csv")
dat$group = ifelse(dat$age == "65+", "old", "young")
dat = dat %>%
  group_by(group) %>%
  summarise(ratio = sum(max_deaths))
ratio = c(ratio, dat$ratio[dat$group == "old"]/dat$ratio[dat$group == "young"])
country = c(country, "USA")

# Zimbabwe
dat  = read.csv("data/Zimbabwe/all_data.csv")
dat$group = ifelse(dat$age == "71+", "old", "young")
dat = dat %>%
  group_by(group) %>%
  summarise(ratio = sum(COVID19_deaths))
ratio = c(ratio, dat$ratio[dat$group == "old"]/dat$ratio[dat$group == "young"])
country = c(country, "Zimbabwe")

comb_dat = data.frame("country" = country,
                      "ratio" = ratio)
saveRDS(comb_dat, "data/death_ratio.RDS")
