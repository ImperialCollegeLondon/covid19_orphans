library(ggplot2)
library(tidyverse)
library(ggpubr)
library(xtable)

source("global_age_analysis_2021/R/utils.R")

month = "_oct"
n = 1000
percentages = readRDS(paste0("global_age_analysis_2021/data/age_outputs/age_data_scaled", month, ".RDS"))
percentages_ = percentages
samples <- readRDS(paste0("global_age_analysis_2021/data/age_outputs/samples_age_data_scaled", month, ".RDS"))
samples$sample <- rep(rep(1:n, each = 6), times = 20)


# Diagram: Updated numbers  ----------------------------------------------
ps <- readRDS("global_age_analysis_2021/data/formatted_primary_secondary.RDS")
p <- readRDS("global_age_analysis_2021/data/formatted_primary.RDS")
pa <- readRDS("global_age_analysis_2021/data/formatted_parents.RDS")

dat <- data.frame(parents = pa,
                  primary = p,
                  primary_seconday = ps)
write.csv(file = paste0("global_age_analysis_2021/data/age_outputs/updated_numbers", month, ".csv"), dat, row.names=FALSE)


# Table 1: Children of different ages ----------------------------------------------
tab1 <- samples %>%
  group_by(country, sample, category) %>%
  summarise(orphans = sum(orphans))

tab1_mean <- percentages %>% 
  group_by(country, category) %>%
  summarise(mean = sum(raw))

total <- samples %>%
  group_by(country, sample) %>%
  summarise(orphans = sum(orphans))

total_mean <- percentages %>%
  group_by(country) %>%
  summarise(mean = sum(raw))

tab1 <- tab1 %>%
  group_by(country, category) %>%
  summarise(li = quantile(orphans, probs = 0.025),
            ui = quantile(orphans, probs = 0.975))
tab1 <- left_join(tab1, tab1_mean)

total <- total %>%
  group_by(country) %>%
  summarise(li = quantile(orphans, probs = 0.025),
            ui = quantile(orphans, probs = 0.975))
total <- left_join(total, total_mean)
total$category = "Total"

tab1 <- rbind(tab1, total)
tab1$format = sprintf("%s [%s - %s]", 
                      format(round(tab1$mean, -2), big.mark = ",", trim = TRUE), 
                      format(round.choose(tab1$li, 100, 0), big.mark = ",", trim = TRUE),
                      format(round.choose(tab1$ui, 100, 1), big.mark = ",", trim = TRUE))
tab1 <- select(tab1, country, category, format)
tab1 <- spread(data = tab1, key  = category, value = format)

cateogry_totals = samples %>%
  group_by(sample, category) %>%
  summarise(orphans = sum(orphans))

cateogry_totals_mean = percentages %>%
  group_by(category) %>%
  summarise(mean = sum(raw))

cateogry_totals <- cateogry_totals %>%
  group_by(category) %>%
  summarise(li = quantile(orphans, probs = 0.025),
            ui = quantile(orphans, probs = 0.975))
cateogry_totals <- left_join(cateogry_totals, cateogry_totals_mean)
cateogry_totals$country = "Total"

cateogry_totals$format = sprintf("%s [%s - %s]", 
                             format(round(cateogry_totals$mean, -2), big.mark = ",", trim = TRUE), 
                             format(round.choose(cateogry_totals$li, 100, 0), big.mark = ",", trim = TRUE),
                             format(round.choose(cateogry_totals$ui, 100, 1), big.mark = ",", trim = TRUE))

total_total = samples %>%
  group_by(sample) %>%
  summarise(orphans = sum(orphans))

total_totals_mean = data.frame(country = "Total", 
                               category = "Total",
                               mean = sum(percentages$raw))

total_total <- total_total %>%
  summarise(li = quantile(orphans, probs = 0.025),
            ui = quantile(orphans, probs = 0.975))
total_total$country = "Total"
total_total$category = "Total"
total_total <- left_join(total_total, total_totals_mean)

total_total$format = sprintf("%s [%s - %s]", 
                      format(round(total_total$mean, -2), big.mark = ",", trim = TRUE), 
                      format(round.choose(total_total$li, 100, 0), big.mark = ",", trim = TRUE),
                      format(round.choose(total_total$ui, 100, 1), big.mark = ",", trim = TRUE))

cateogry_totals = rbind(cateogry_totals, total_total)
cateogry_totals <- select(cateogry_totals, country, category, format)
cateogry_totals <- spread(data = cateogry_totals, key  = category, value = format)

tab1 <- rbind(tab1, cateogry_totals)

tab1 <- select(tab1, country,  `[0-5)`, `[5-10)`, `[10-18)`, Total)

samples_gender = samples

tab1_gender <- samples_gender %>%
  group_by(country, sample, gender) %>%
  summarise(orphans = sum(orphans))

tab1_mean_gender <- percentages %>% 
  group_by(country, gender) %>%
  summarise(mean = sum(raw))

tab1_gender <- tab1_gender %>%
  group_by(country, gender) %>%
  summarise(li = quantile(orphans, probs = 0.025),
            ui = quantile(orphans, probs = 0.975))
tab1_gender <- left_join(tab1_gender, tab1_mean_gender, by = c("country", "gender"))

tab1_gender$format = sprintf("%s [%s - %s]", 
                      format(round(tab1_gender$mean, -2), big.mark = ",", trim = TRUE), 
                      format(round.choose(tab1_gender$li, 100, 0), big.mark = ",", trim = TRUE),
                      format(round.choose(tab1_gender$ui, 100, 1), big.mark = ",", trim = TRUE))
tab1_gender <- select(tab1_gender, country, gender, format)
tab1_gender <- spread(data = tab1_gender, key  = gender, value = format)

cateogry_totals_gender = samples_gender %>%
  group_by(sample, gender) %>%
  summarise(orphans = sum(orphans))

cateogry_totals_mean_gender = percentages %>%
  group_by(gender) %>%
  summarise(mean = sum(raw))

cateogry_totals_gender <- cateogry_totals_gender %>%
  group_by(gender) %>%
  summarise(li = quantile(orphans, probs = 0.025),
            ui = quantile(orphans, probs = 0.975))
cateogry_totals_gender <- left_join(cateogry_totals_gender, cateogry_totals_mean_gender)
cateogry_totals_gender$country = "Total"

cateogry_totals_gender$format = sprintf("%s [%s - %s]", 
                                 format(round(cateogry_totals_gender$mean, -2), big.mark = ",", trim = TRUE), 
                                 format(round.choose(cateogry_totals_gender$li, 100, 0), big.mark = ",", trim = TRUE),
                                 format(round.choose(cateogry_totals_gender$ui, 100, 1), big.mark = ",", trim = TRUE))

cateogry_totals_gender <- select(cateogry_totals_gender, country, gender, format)
cateogry_totals_gender <- spread(data = cateogry_totals_gender, key  = gender, value = format)

tab1_gender_ = rbind(tab1_gender, cateogry_totals_gender)
names(tab1_gender_) = c("country", "Maternal", "Paternal")

tab_combined <- left_join(tab1, tab1_gender_, by = "country")
global_totals <- readRDS("global_age_analysis_2021/data/age_outputs/global_extrapolation_totals.RDS")
names(global_totals) <- names(tab_combined)
tab_combined_ <- rbind(tab_combined, global_totals)

write.csv(file = paste0("global_age_analysis_2021/table_2_age_breakdown", month, ".csv"), tab_combined_, row.names=FALSE)


# Table 2: Percentages of ages and types ----------------------------------------------
pretty_table <- percentages_
pretty_table$raw_format <- sprintf("%.1f%% [%.1f%% - %.1f%%]", pretty_table$percent,
                                   round.choose(pretty_table$li_percent, 0.1, 0), round.choose(pretty_table$ui_percent, 0.1, 1))

pretty_table <- select(pretty_table, country, category, gender, raw_format)

pretty_table_wide_raw <- pivot_wider(pretty_table, names_from = c(category, gender), values_from = c(raw_format))


#global_dat = read.csv("global_age_analysis_2021/data/age_outputs/global_age_percentages.csv")
#global_dat = global_dat[which(global_dat == "Global"),]
#names(global_dat) = c("country", "[0-5)_Female",  "[0-5)_Male", "[5-10)_Female", "[5-10)_Male", "[10-18)_Female", "[10-18)_Male"  )
#pretty_table_wide_raw <- rbind(pretty_table_wide_raw, global_dat)

write.csv(file = paste0("global_age_analysis_2021/data/age_outputs/age_compostion", month, ".csv"), pretty_table_wide_raw, row.names=FALSE)
tab<-xtable(pretty_table_wide_raw)
print(tab, include.rownames=FALSE)

percentages_gender <- percentages_ %>%
  select(gender, percent, country) %>%
  group_by(country, gender) %>%
  summarise(percent = sum(percent))
percentages_gender_wide = spread(percentages_gender, key = country, percent)
write.csv(file = paste0("global_age_analysis_2021/data/gender_compostion.csv"), percentages_gender_wide, row.names=FALSE)

percentages_age <- percentages_ %>%
  select(category, percent, country) %>%
  group_by(country, category) %>%
  summarise(percent = sum(percent))
percentages_age_wide = spread(percentages_age, key = country, percent)
write.csv(file = paste0("global_age_analysis_2021/data/age_compostion.csv"), percentages_age_wide, row.names=FALSE)

# Percentages pyramid ----------------------------------------------
pyramid <- percentages
pyramid$category <- ifelse(pyramid$category == "[0-5)", "0-4", 
                           ifelse(pyramid$category == "[5-10)", "5-9", "10-17"))
pyramid$category <- factor(pyramid$category, levels = c("0-4", "5-9", "10-17"))
pyramid$gender <- ifelse(pyramid$gender == "Male", "Paternal", "Maternal")
pyramid$percent <- ifelse(pyramid$gender == "Paternal", -pyramid$percent, pyramid$percent)
pyramid$li_percent <- ifelse(pyramid$gender == "Paternal", -pyramid$li_percent, pyramid$li_percent)
pyramid$ui_percent <- ifelse(pyramid$gender == "Paternal", -pyramid$ui_percent, pyramid$ui_percent)

tmp_adolescents_paternal <- pyramid[which(pyramid$gender == "Paternal" & pyramid$category == "10-17"),]
pyramid$country = factor(pyramid$country, levels = unique(tmp_adolescents_paternal$country[order(tmp_adolescents_paternal$percent)]))

n1 <- ggplot(pyramid) + 
  geom_bar(data = subset(pyramid, gender == "Paternal"), stat = "identity", aes(x = category, y = percent/100, fill = gender)) + 
  geom_bar(data = subset(pyramid, gender == "Maternal"), stat = "identity", aes(x = category, y = percent/100, fill = gender)) + 
  geom_text(aes(x = category, y = percent/100, label = abs(round(percent))), 
            position = position_stack(vjust = 0.5)) +
  geom_errorbar(aes(x=category, ymin =  li_percent/100, ymax = ui_percent/100)) + 
  scale_y_continuous(breaks = seq(-0.6, 0.6, 0.2), 
                     labels = paste0(as.character(c(seq(60, 0, -20), seq(20, 60, 20))), "%")) + 
  xlab("") + ylab("Percent of children") +
  coord_flip() + 
  scale_fill_brewer(name = "", palette = "Set1", label = c("Maternal", "Paternal")) + 
  theme_bw() + theme(legend.position = "bottom") + 
  facet_wrap(~country, ncol =4)

children_numbers <- read.csv("global_age_analysis_2021/data/numbers_of_children_smaller.csv")
children_numbers <- children_numbers[, c(1, 8, 9, 10)]
names(children_numbers) <- c("country", "0-4", "5-9", "10-17")
children_numbers$total = children_numbers$`0-4` + children_numbers$`5-9` + children_numbers$`10-17`
children_numbers$`0-4` = (children_numbers$`0-4` / children_numbers$total)/2
children_numbers$`5-9` = (children_numbers$`5-9` / children_numbers$total)/2
children_numbers$`10-17` = (children_numbers$`10-17` / children_numbers$total)/2

children_numbers_maternal = children_numbers
children_numbers_maternal$gender = "Maternal"

children_numbers_paternal = children_numbers
children_numbers_paternal$gender = "Paternal"

children_numbers = rbind(children_numbers_paternal, children_numbers_maternal)
children_numbers = select(children_numbers, -total)
children_numbers_long <- gather(children_numbers, key = "category", value = "children", -gender, -country)
children_numbers_long$children = ifelse(children_numbers_long$gender == "Paternal", -children_numbers_long$children, 
                                        children_numbers_long$children)
children_numbers_long = children_numbers_long[which(children_numbers_long$country != "Russia"),]
children_numbers_long$country = factor(children_numbers_long$country,
                                       levels = unique(tmp_adolescents_paternal$country[order(tmp_adolescents_paternal$percent)]))

n2 <- ggplot(pyramid) + 
  geom_bar(data = subset(pyramid, gender == "Paternal"), stat = "identity", aes(x = category, y = percent/100, fill = gender)) + 
  geom_bar(data = subset(pyramid, gender == "Maternal"), stat = "identity", aes(x = category, y = percent/100, fill = gender)) + 
  geom_bar(data = subset(children_numbers_long, gender == "Paternal"), stat = "identity", 
           aes(x = category, y = children, fill = gender), colour="black", alpha = 0) + 
  geom_bar(data = subset(children_numbers_long, gender == "Maternal"), stat = "identity", 
           aes(x = category, y = children, fill = gender), colour="black", alpha = 0) + 
  scale_y_continuous(breaks = seq(-0.6, 0.6, 0.2), 
                     labels = paste0(as.character(c(seq(60, 0, -20), seq(20, 60, 20))), "%")) + 
  xlab("") + ylab("Percent of children") +
  coord_flip() + 
  scale_fill_brewer(name = "", palette = "Set1", label = c("Maternal", "Paternal")) + 
  theme_bw() + theme(legend.position = "bottom") + 
  facet_wrap(~country, ncol =4)

p <- ggarrange(n1, n2, ncol = 1, labels = "AUTO", 
               common.legend = TRUE, legend = "bottom")

ggsave(paste0("global_age_analysis_2021/figures/fig_2_age_composition", month, ".pdf"),  p, height = 14,  width = 14)


# Working out proportions  ----------------------------------------------
total_sample <- samples %>% 
  group_by(sample) %>%
  summarise("total" = sum(orphans))

props <- samples %>% 
  group_by(sample, gender) %>%
  summarise("orphans" = sum(orphans))

props <- left_join(props, total_sample, by = "sample")
props$prop <- props$orphans/ props$total

male <- props[which(props$gender == "Male"),]
print(sprintf("Proportion of paternal orphans: %.1f[%.1f - %.1f]", mean(male$prop)*100,
              quantile(male$prop, probs = 0.025)*100, quantile(male$prop, probs = 0.975)*100))

# Merging regions  ----------------------------------------------
regions <- read.csv("global_age_analysis_2021/data/who_regions.csv")
regions$Country_Region[which(regions$Country_Region == "US")] <- "USA"
regions$Country_Region[which(regions$Country_Region == "I.R. Iran")] <- "Iran (Islamic Republic of)"
regions$Country_Region[which(regions$Country_Region == "United Kingdom")] <- "England & Wales"
percentages <- left_join(percentages, regions, by = c("country" = "Country_Region"))

percentages$who_region[which(percentages$who_region == "Eastern European")] <- "European"
percentages$who_region <- factor(percentages$who_region, 
                                    labels = c("African", "American", "Eastern Mediterranean", "European",
                                               "South-East Asian", "Western Pacific"))

sorting <- percentages[which(percentages$category == "[10-18)" & percentages$gender == "Male"),]
percentages$country <- factor(percentages$country, levels = sorting$country[order(sorting$percent)])
percentages$group <- paste(percentages$gender, percentages$category)
percentages$group <- factor(percentages$group, levels = c("Female [0-5)", "Male [0-5)", "Female [5-10)",
                                                           "Male [5-10)", "Female [10-18)", "Male [10-18)"),
                            labels = c("Maternal orphans 0-4", "Paternal orphans 0-4", "Maternal orphans 5-9",
                                       "Paternal orphans 5-9", "Maternal orphans 10-17", "Paternal orphans 10-17"))

# Merging with proportion of children  ----------------------------------------------
pop = readRDS("global_age_analysis_2021/data/grouped_age_sex_population.RDS")
pop_categories <- pop %>% group_by(category, Country.Area.Name) %>% summarise(population = sum(population))
pop_categories$Country.Area.Name[which(pop_categories$Country.Area.Name == "United States")] = "USA"
pop_categories$Country.Area.Name[which(pop_categories$Country.Area.Name == "Iran")] = "Iran (Islamic Republic of)"

pop_gender <- pop %>% group_by(Country.Area.Name) %>% summarise(population = sum(population))
pop_gender$Country.Area.Name[which(pop_gender$Country.Area.Name == "United States")] = "USA"
pop_gender$Country.Area.Name[which(pop_gender$Country.Area.Name == "Iran")] = "Iran (Islamic Republic of)"
pop_gender_f = pop_gender
pop_gender_f$gender = "Female"
pop_gender_m = pop_gender
pop_gender_m$gender = "Male"
pop_gender_comb = rbind(pop_gender_f,pop_gender_m)

dat_cat = percentages
dat_cat$category <- ifelse(dat_cat$category == "[0-5)", "0-4", 
                           ifelse(dat_cat$category == "[5-10)", "5-9", "10-17"))
dat_cat$category <- factor(dat_cat$category, levels = c("0-4", "5-9", "10-17"))


rates_samples <- samples
rates_samples$category <- ifelse(rates_samples$category == "[0-5)", "0-4", 
                                 ifelse(rates_samples$category == "[5-10)", "5-9", "10-17"))
rates_samples$category <- factor(rates_samples$category, levels = c("0-4", "5-9", "10-17"))
rates_samples_categories <- rates_samples %>% group_by(category, country, sample) %>% summarise(raw = sum(orphans))
rates_samples_gender <- rates_samples %>% group_by(gender, country, sample) %>% summarise(raw = sum(orphans))
rates_samples_all <- rates_samples %>% group_by(country, sample) %>% summarise(raw = sum(orphans))
rates_samples_cat <- rates_samples_categories %>% group_by(category, country) %>% 
  summarise(li = quantile(raw, probs = 0.025),
            ui = quantile(raw, probs = 0.975))
rates_samples_gen <- rates_samples_gender %>% group_by(gender, country) %>% 
  summarise(li = quantile(raw, probs = 0.025),
            ui = quantile(raw, probs = 0.975))
rates_samples_all <- rates_samples_all %>% group_by(country) %>% 
  summarise(li = quantile(raw, probs = 0.025),
            ui = quantile(raw, probs = 0.975))

rates_categories <- dat_cat %>% group_by(category, country, who_region) %>% summarise(raw = sum(raw))
rates_categories <- left_join(rates_categories, rates_samples_cat, by = c("category", "country"))
rates_gender <- dat_cat %>% group_by(gender, country, who_region) %>% summarise(raw = sum(raw))
rates_gender <- left_join(rates_gender, rates_samples_gen, by = c("gender", "country"))
rates_all <- dat_cat %>% group_by(country, who_region) %>% summarise(raw = sum(raw))
rates_all <- left_join(rates_all, rates_samples_all, by = c("country"))

rates_categories <- left_join(rates_categories, pop_categories, by = c("country"="Country.Area.Name", "category"))
rates_categories$raw_pop <- rates_categories$raw / rates_categories$population * 1000
rates_categories$raw_pop_li <- rates_categories$li / rates_categories$population * 1000
rates_categories$raw_pop_ui <- rates_categories$ui / rates_categories$population * 1000
rates_categories$category <- factor(rates_categories$category, levels = c("0-4", "5-9", "10-17"))
tmp_cat <- rates_categories[which(rates_categories$category == "10-17"),]
rates_categories$country <- factor(rates_categories$country, levels = tmp_cat$country[order(tmp_cat$raw_pop)])

rates_gender <- left_join(rates_gender, pop_gender_comb, by = c("country"="Country.Area.Name", "gender"))
rates_gender$gender <- ifelse(rates_gender$gender == "Male", "Paternal", "Maternal")
rates_gender$raw_pop <- rates_gender$raw / rates_gender$population * 1000
rates_gender$raw_pop_li <- rates_gender$li / rates_gender$population * 1000
rates_gender$raw_pop_ui <- rates_gender$ui / rates_gender$population * 1000
tmp_gender <- rates_gender[which(rates_gender$gender == "Paternal"),]
rates_gender$country <- factor(rates_gender$country, levels = tmp_gender$country[order(tmp_gender$raw_pop)])

rates_all <- left_join(rates_all, pop_gender, by = c("country"="Country.Area.Name"))
rates_all$raw_pop <- rates_all$raw / rates_all$population * 1000
rates_all$raw_pop_li <- rates_all$li / rates_all$population * 1000
rates_all$raw_pop_ui <- rates_all$ui / rates_all$population * 1000
rates_all$country <- factor(rates_all$country, levels = rates_all$country[order(rates_all$raw_pop)])

print(rates_all)
# Plot rates  ----------------------------------------------

p_cat <- ggplot(rates_categories) +
  geom_bar(aes(country, raw_pop, fill = who_region), stat = "identity") +
  geom_errorbar(aes(x=country, ymin = raw_pop_li, ymax = raw_pop_ui)) + 
  facet_grid(~category) +
  theme_bw()  + theme(legend.title = element_blank(),
                      axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
                      plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")) +
  xlab("") + ylab("Rate of orphans per 1000 children\n in each category")

p_gender <- ggplot(rates_gender) +
  geom_bar(aes(country, raw_pop, fill = who_region), stat = "identity") +
  geom_errorbar(aes(x=country, ymin =  raw_pop_li, ymax = raw_pop_ui)) + 
  facet_grid(~gender) +
  theme_bw()  + theme(legend.title = element_blank(),
                      axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
                      plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")) +
  xlab("") + ylab("Rate of orphans per 1000 children")

p_all <- ggplot(rates_all) +
  geom_bar(aes(country, raw_pop, fill = who_region), stat = "identity") +
  geom_errorbar(aes(x=country, ymin =  raw_pop_li, ymax = raw_pop_ui)) + 
  theme_bw()  + theme(legend.title = element_blank(),
                      axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
                      plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")) +
  xlab("") + ylab("Rate of orphans per 1000 children")

p <- ggarrange(p_all, p_gender, p_cat, labels = "AUTO", ncol = 1, common.legend = TRUE, legend = "bottom")

ggsave(paste0("global_age_analysis_2021/figures/orphanhood_rates_age", month, ".pdf"),  p, height = 14,  width = 10)


# Time varying figure ----------------------------------------------
percentages_diff = readRDS(paste0("global_age_analysis_2021/data/age_outputs/age_data_scaled_diff.RDS"))
percentages_diff$date = "Last 6 months"
percentages_diff$group <- paste(percentages_diff$gender, percentages_diff$category)
percentages_diff$group <- factor(percentages_diff$group, levels = c("Female [0-5)", "Male [0-5)", "Female [5-10)",
                                                          "Male [5-10)", "Female [10-18)", "Male [10-18)"),
                            labels = c("Maternal orphans 0-4", "Paternal orphans 0-4", "Maternal orphans 5-9",
                                       "Paternal orphans 5-9", "Maternal orphans 10-17", "Paternal orphans 10-17"))
percentages_diff$category <- ifelse(percentages_diff$category == "[0-5)", "0-4", 
                               ifelse(percentages_diff$category == "[5-10)", "5-9", "10-17"))


percentages_initial = readRDS(paste0("global_age_analysis_2021/data/age_outputs/age_data_scaled.RDS"))
percentages_initial$date = "Initial study"
percentages_initial$group <- paste(percentages_initial$gender, percentages_initial$category)
percentages_initial$group <- factor(percentages_initial$group, levels = c("Female [0-5)", "Male [0-5)", "Female [5-10)",
                                                                    "Male [5-10)", "Female [10-18)", "Male [10-18)"),
                                 labels = c("Maternal orphans 0-4", "Paternal orphans 0-4", "Maternal orphans 5-9",
                                            "Paternal orphans 5-9", "Maternal orphans 10-17", "Paternal orphans 10-17"))
percentages_initial$category <- ifelse(percentages_initial$category == "[0-5)", "0-4", 
                                    ifelse(percentages_initial$category == "[5-10)", "5-9", "10-17"))

percentages_all <- rbind(percentages_initial, percentages_diff)

tmp = percentages_all %>%
  group_by(country, date) %>%
  summarise(raw = sum(raw))
tmp_wide = spread(tmp, key = date, value = raw)

percentages_all <- percentages_all %>%
  group_by(category, country, date) %>%
  summarise(percent = sum(percent))


percentages_all$category = factor(percentages_all$category, levels = c("0-4", "5-9", "10-17"))

percentages_all_two = percentages_all
percentages_all_two$category <- as.character(percentages_all_two$category)
percentages_all_two$category[percentages_all_two$category %in% c("0-4", "5-9")] <- "young"
percentages_all_two = percentages_all_two %>%
  group_by(category, country, date) %>%
  summarise(percent = sum(percent))
percentages_all_two$category <- factor(percentages_all_two$category, 
                                       labels = c("Adolescent", "Child"))
percentages_all_two$category <- factor(percentages_all_two$category, 
                                       levels = c( "Child", "Adolescent"))
p2 <- ggplot(percentages_all_two %>% 
               filter(!country %in% c("Kenya", "Iran (Islamic Republic of)", "India", "Zimbabwe"))) +
  geom_bar(aes(x = date, y = percent, fill = category), stat = "identity") + 
  geom_text(aes(x = date, y = percent, label = round(percent)), 
            position = position_stack(vjust = 0.5)) +
  facet_wrap(~country) + 
  ylab("Percentage of orphans") + xlab("") + 
  theme_bw() + theme(legend.position = "bottom", legend.title = element_blank())
ggsave(paste0("global_age_analysis_2021/figures/s1_timeseries_age_composition", month, ".pdf"),  p2, height = 7,  width = 14)


