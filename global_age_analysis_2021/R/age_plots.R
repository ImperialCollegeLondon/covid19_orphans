library(ggplot2)
library(tidyverse)
source("global_age_analysis_2021/R/utils.R")

month = "_oct"
n = 1
percentages = readRDS(paste0("global_age_analysis_2021/data/age_outputs/age_data_scaled", month, ".RDS"))
samples <- readRDS(paste0("global_age_analysis_2021/data/age_outputs/samples_age_data_scaled", month, ".RDS"))
samples$sample <- rep(rep(1:n, each = 6), times = 20)


# Diagram: Updated numbers  ----------------------------------------------
ps <- readRDS("global_age_analysis_2021/data/formatted_primary_secondary.RDS")
p <- readRDS("global_age_analysis_2021/data/formatted_primary.RDS")
pa <- readRDS("global_age_analysis_2021/data/formatted_parents.RDS")

dat <- data.frame(parents = pa,
                  primary = p,
                  primary_seconday = ps)
write_csv(file = paste0("global_age_analysis_2021/data/age_outputs/updated_numbers", month, ".csv"), dat)


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

write_csv(file = paste0("global_age_analysis_2021/age_table_1", month, ".csv"), tab_combined)


# Table 2: Percentages of ages and types ----------------------------------------------
pretty_table <- percentages
pretty_table$raw_format <- sprintf("%.1f%% [%.1f%% - %.1f%%]", round(pretty_table$percent, digits = 1),
                                   round.choose(pretty_table$li_percent, 0.1, 0), round.choose(pretty_table$ui_percent, 0.1, 1))

pretty_table <- select(pretty_table, country, category, gender, raw_format)

pretty_table_wide_raw <- pivot_wider(pretty_table, names_from = c(category, gender), values_from = c(raw_format))


global_dat = read.csv("global_age_analysis_2021/data/age_outputs/global_age_percentages.csv")
global_dat = global_dat[which(global_dat == "Global"),]

names(global_dat) = c("country", "[0-5)_Female",  "[0-5)_Male", "[5-10)_Female", "[5-10)_Male", "[10-18)_Female", "[10-18)_Male"  )

pretty_table_wide_raw <- rbind(pretty_table_wide_raw, global_dat)
write_csv(file = paste0("global_age_analysis_2021/age_table_2", month, ".csv"), pretty_table_wide_raw)


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
pop_children <- read.csv("global_age_analysis_2021/data/numbers_of_children_smaller.csv")
pop_children <- pop_children[, c(1, 8, 9, 10)]
names(pop_children) <- c("country", "0-4", "5-9", "10-17")
pop_children_long <- gather(pop_children, key = "category", value = "pop", -country)
percentages$category <- ifelse(percentages$category == "[0-5)", "0-4", 
                               ifelse(percentages$category == "[5-10)", "5-9", "10-17"))

percentages <- left_join(percentages, pop_children_long, by = c("country", "category"))
percentages$raw_prop <- percentages$raw/percentages$pop * 1000
percentages$raw_prop_li <- percentages$li_raw/percentages$pop * 1000
percentages$raw_prop_ui <- percentages$ui_raw/percentages$pop * 1000

sorting <- percentages[which(percentages$category == "10-17" & percentages$gender == "Male"),]
percentages$country <- factor(percentages$country, levels = sorting$country[order(sorting$raw_prop)])
percentages$gender <- factor(percentages$gender, labels = c("Maternal", "Paternal"))
percentages$category <- factor(percentages$category, levels = c("0-4", "5-9", "10-17"))

# Plot fig 1  ----------------------------------------------
p1 <- ggplot(percentages) +
  geom_bar(aes(country, raw_prop, fill = who_region), stat = "identity") +
  geom_errorbar(aes(x=country, ymin =  raw_prop_li, ymax = raw_prop_ui)) + 
  facet_grid(category ~ gender) +
  theme_bw()  + theme(legend.title = element_blank(),
                      axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
                      plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")) +
  xlab("") + ylab("Rate of orphans per 1000 children in each category")
ggsave(paste0("global_age_analysis_2021/figures/fig_2_orphanhood_rates_age", month, ".pdf"),  p1, height = 7,  width = 14)


# TTime varying figure ----------------------------------------------
percentages_diff = readRDS(paste0("global_age_analysis_2021/data/age_outputs/age_data_scaled_diff.RDS"))
percentages_diff$date = "Last 6 months"
percentages_diff$group <- paste(percentages_diff$gender, percentages_diff$category)
percentages_diff$group <- factor(percentages_diff$group, levels = c("Female [0-5)", "Male [0-5)", "Female [5-10)",
                                                          "Male [5-10)", "Female [10-18)", "Male [10-18)"),
                            labels = c("Maternal orphans 0-4", "Paternal orphans 0-4", "Maternal orphans 5-9",
                                       "Paternal orphans 5-9", "Maternal orphans 10-17", "Paternal orphans 10-17"))
percentages_diff$category <- ifelse(percentages_diff$category == "[0-5)", "0-4", 
                               ifelse(percentages_diff$category == "[5-10)", "5-9", "10-17"))


percentages_initial = percentages
percentages_initial$date = "Initial study"

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
ggsave(paste0("global_age_analysis_2021/figures/timeseries_age_composition", month, ".pdf"),  p2, height = 7,  width = 14)


