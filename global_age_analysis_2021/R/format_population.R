library(tidyverse)
library(readxl)

# Read in and format us census data
dat = read.csv("global_age_analysis_2021/data/us_census_population.csv") 
dat = select(dat, Country.Area.Name, GROUP, Population, Male.Population, Female.Population)
dat = dat[which(dat$GROUP != "TOTAL"),]
dat$GROUP = ifelse(dat$GROUP == "100+", 100, dat$GROUP)
dat$GROUP = as.numeric(dat$GROUP)
dat$age = paste0((dat$GROUP)%/%5 * 5 ,'-',(dat$GROUP) %/% 5 *5+4)
dat$age = ifelse(dat$age == "100-104", "100+", dat$age)

# Read in and format england and wales data
eng = read_excel("global_age_analysis_2021/data/england_wales_pop.xlsx")
eng = eng[8:98,5:8]
names(eng) <- c("GROUP", "Male.Population", "Female.Population", "Population")
eng$Country.Area.Name <- "England & Wales"
eng$GROUP = ifelse(eng$GROUP == "90+", 90, eng$GROUP)
eng$GROUP = as.numeric(eng$GROUP)
eng$age = paste0((eng$GROUP)%/%5 * 5 ,'-',(eng$GROUP) %/% 5 *5+4)
eng$age = ifelse(eng$age == "90-94", "90+", dat$age)
eng$Male.Population = as.numeric(eng$Male.Population)
eng$Female.Population = as.numeric(eng$Female.Population)
eng$Population = as.numeric(eng$Population)

# Combine data sets
dat = rbind(dat, eng)

# Work out total population
total_pop = dat %>% 
  group_by(Country.Area.Name) %>%
  summarise(total_population = sum(Population))

# Group in 5 year age bands
bands_5_year = dat %>% 
  group_by(Country.Area.Name, age) %>%
  summarise(population = sum(Population),
            male_population = sum(Male.Population),
            female_population = sum(Female.Population))

bands_5_year = left_join(bands_5_year, total_pop, by = "Country.Area.Name")
bands_5_year$prop = bands_5_year$population / bands_5_year$total_population
bands_5_year$male_prop = bands_5_year$male_population /  bands_5_year$total_population
bands_5_year$female_prop = bands_5_year$female_population /  bands_5_year$total_population

bands_5_year$age = factor(bands_5_year$age, 
                          levels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34",
                                     "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69",
                                     "70-74", "75-79", "80-84", "85-89", "90-94", "95-99", "100+", "90+"))
bands_5_year$male_population = bands_5_year$male_population * -1
bands_5_year$male_prop = bands_5_year$male_prop * -1

bands_5_year_prop = select(bands_5_year, Country.Area.Name, age, male_prop, female_prop)
bands_5_year_long <- gather(bands_5_year_prop, key = "Gender", value = 'Proportion', -age, -Country.Area.Name)

# Make plot of population pyramids for study countries
study_countries = c("Argentina", "Brazil", "Colombia", "England & Wales", "France", "Germany", "India", "Iran", "Italy", "Kenya", 
                    "Malawi", "Mexico", "Nigeria", "Peru", "Philippines", "Poland", "Russia", "South Africa", "Spain", "United States", "Zimbabwe")
study_5_years <- bands_5_year_long[which(bands_5_year_long$Country.Area.Name %in% study_countries),]
n1 <- ggplot(study_5_years) + 
  geom_bar(data = subset(study_5_years, Gender == "female_prop"), stat = "identity", aes(x = age, y = Proportion, fill = Gender)) + 
  geom_bar(data = subset(study_5_years, Gender == "male_prop"), stat = "identity", aes(x = age, y = Proportion, fill = Gender)) + 
  scale_y_continuous(breaks = seq(-0.08, 0.08, 0.02), 
                     labels = paste0(as.character(c(seq(8, 0, -2), seq(2, 8, 2))), "%")) + 
  xlab("") + ylab("Percent of population") +
  coord_flip() + 
  scale_fill_brewer(name = "", palette = "Set1", label = c("Female", "Male")) + 
  theme_bw() + theme(legend.position = "bottom") + 
  facet_wrap(~Country.Area.Name, scales = "free", ncol =4)
ggsave("global_age_analysis_2021/figures/pop_pyramids.pdf", n1, height = 14, width = 8)


# Get proportions of children
children_dat = dat
children_dat$age = ifelse(children_dat$GROUP %in% c(0, 1, 2, 3, 4), "0-4",
                          ifelse(children_dat$GROUP %in% c(5, 6, 7, 8, 9), "5-9",
                                 ifelse(children_dat$GROUP %in% c(10, 11, 12, 13, 14, 15, 16, 17), "10-17", "adult")))

children_bands = children_dat %>% 
  group_by(Country.Area.Name, age) %>%
  summarise(population = sum(Population),
            male_population = sum(Male.Population),
            female_population = sum(Female.Population))

children_bands = left_join(children_bands, total_pop, by = "Country.Area.Name")
children_bands$prop = children_bands$population / children_bands$total_population
children_bands$male_prop = children_bands$male_population /  children_bands$total_population
children_bands$female_prop = children_bands$female_population /  children_bands$total_population

# Get proportions of adults
adult_dat = dat
adult_dat$age = ifelse(adult_dat$GROUP < 15, "children",
                          ifelse(adult_dat$GROUP >= 15 & adult_dat$GROUP < 45, "15-44",
                                 ifelse(adult_dat$GROUP >= 45 & adult_dat$GROUP < 65, "45-64", "65+")))

adult_bands = adult_dat %>% 
  group_by(Country.Area.Name, age) %>%
  summarise(population = sum(Population),
            male_population = sum(Male.Population),
            female_population = sum(Female.Population))

adult_bands = left_join(adult_bands, total_pop, by = "Country.Area.Name")
adult_bands$prop = adult_bands$population / adult_bands$total_population
adult_bands$male_prop = adult_bands$male_population /  adult_bands$total_population
adult_bands$female_prop = adult_bands$female_population /  adult_bands$total_population

bespoke_bands = rbind(children_bands, adult_bands)
bespoke_bands = bespoke_bands[which(! bespoke_bands$age %in% c("adult", "children")),]

saveRDS(bespoke_bands, file = "global_age_analysis_2021/data/bespoke_population_bands.RDS")


#---------
zim = dat[which(dat$Country.Area.Name == "Zimbabwe"),]
zim = sum(zim$Population[which(zim$GROUP > 9 & zim$GROUP < 18)]) / sum(zim$Population[which(zim$GROUP < 18)])

spa = dat[which(dat$Country.Area.Name == "Spain"),]
spa = sum(spa$Population[which(spa$GROUP > 9 & spa$GROUP < 18)]) / sum(spa$Population[which(spa$GROUP < 18)])


spa = dat[which(dat$Country.Area.Name == "Spain"),]
spa = sum(spa$Population[which(spa$GROUP > 4 & spa$GROUP < 10)]) / sum(spa$Population[which(spa$GROUP < 18)])

ken = dat[which(dat$Country.Area.Name == "Kenya"),]
ken = sum(ken$Population[which(ken$GROUP > 4 & ken$GROUP < 10)]) / sum(ken$Population[which(ken$GROUP < 18)])


#------
dat_study = dat[which(dat$Country.Area.Name %in% study_countries),]
dat_study = dat_study[which(dat_study$GROUP < 18),]
dat_study$category = ifelse(dat_study$GROUP < 5, "0-4", 
                            ifelse(dat_study$GROUP > 5 &  dat_study$GROUP < 10, "5-9", "10-17"))
dat_study_group = dat_study %>%
  group_by(Country.Area.Name, category) %>%
  summarise("Male" = sum(Male.Population),
            "Female" = sum(Female.Population))


dat_study_group_long <- gather(dat_study_group, key = "gender", value = "population", -category, -Country.Area.Name)
saveRDS(dat_study_group_long, "global_age_analysis_2021/data/grouped_age_sex_population.RDS")

