dhs <- readRDS("global_age_analysis_2021/data/Brazil/female_fertility.RDS")
dhs2 <- readRDS("global_age_analysis_2021/data/Brazil/female_fertility_half.RDS")

ihme <- read_csv("data/fertility/IHME_both_files.csv")
ihme_br <- ihme[which(ihme$country == "Brazil"),]

ihme_br_2015 <- ihme_br[which(ihme_br$date == 2015),]
ihme_br_2015$label <- dhs$ages[2:9]
ihme_br_2014 <- ihme_br[which(ihme_br$date == 2014),]
ihme_br_2014$label <- dhs$ages[2:9]
p <- ggplot() + 
  geom_point(data = dhs[2:15, ], aes(x = ages, y = y2015*1000, col = "DHS 2015")) + 
  geom_point(data = dhs2[2:15, ], aes(x = ages, y = y2015*1000, col = "half DHS 2015")) + 
  geom_point(data = ihme_br_2015, aes(x = label, y = afr,  col = "IHME 2015")) + 
  scale_color_manual(values = c("DHS 2015" = "black", "half DHS 2015" = "green",
                               "IHME 2015" = "blue")) + 
  theme_bw() + ylab("Fertility rate per 1000 women") + labs(col = "Data source") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1)) +
  xlab("Age of woman")
print(p)
ggsave("global_age_analysis_2021/figures/br_female_2015.pdf", p, width = 6, height = 5)


