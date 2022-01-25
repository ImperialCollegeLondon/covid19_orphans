dhs <- readRDS("data/SouthAfrica/female_fertility.RDS")

ihme <- read_csv("data/fertility/IHME_both_files.csv")
ihme_sa <- ihme[which(ihme$country == "South Africa"),]

online <- read_csv("data/DHS/za_female.csv")
online <- online[2, 3:9]
online <- data.frame("rates" = as.numeric(t(online)))
online$label <- dhs$ages[2:8]

ihme_sa_2016 <- ihme_sa[which(ihme_sa$date == 2016),]
ihme_sa_2016$label <- dhs$ages[2:9]
ihme_sa_2015 <- ihme_sa[which(ihme_sa$date == 2015),]
ihme_sa_2015$label <- dhs$ages[2:9]
ihme_sa_2014 <- ihme_sa[which(ihme_sa$date == 2014),]
ihme_sa_2014$label <- dhs$ages[2:9]

p <- ggplot() + 
  geom_point(data = dhs[2:15, ], aes(x = ages, y = y2016*1000, col = "dhs 2016")) + 
  geom_point(data = online, aes(x = label, y = rates, col = "dhs online")) + 
  geom_point(data = ihme_sa_2016, aes(x = label, y = afr, col = "ihme 2016")) + 
  geom_point(data = ihme_sa_2015, aes(x = label, y = afr,  col = "ihme 2015")) + 
  geom_point(data = ihme_sa_2014, aes(x = label, y = afr, col = "ihme 2014")) + 
  scale_color_manual(values = c("dhs 2016" = "black", "dhs online" = "green", 
                                "ihme 2016" = "red", "ihme 2015" = "blue", "ihme 2014" = "purple")) + 
  theme_bw() + ylab("Female fertility rate per 1000")
print(p)
ggsave("za_female_2016.png", p)

ihme_sa_2011 <- ihme_sa[which(ihme_sa$date == 2011),]
ihme_sa_2011$label <- dhs$ages[2:9]
ihme_sa_2010 <- ihme_sa[which(ihme_sa$date == 2010),]
ihme_sa_2010$label <- dhs$ages[2:9]
ihme_sa_2009 <- ihme_sa[which(ihme_sa$date == 2009),]
ihme_sa_2009$label <- dhs$ages[2:9]
p <- ggplot() + 
  geom_point(data = dhs[2:15, ], aes(x = ages, y = y2011*1000, col = "dhs 2011")) + 
  geom_point(data = ihme_sa_2011, aes(x = label, y = afr, col = "ihme 2011")) + 
  geom_point(data = ihme_sa_2010, aes(x = label, y = afr, col = "ihme 2010")) + 
  geom_point(data = ihme_sa_2009, aes(x = label, y = afr, col = "ihme 2009")) + 
  scale_color_manual(values = c("dhs 2011" = "black", "ihme 2011" = "red", "ihme 2010" = "blue", "ihme 2009" = "purple")) + 
  theme_bw() + ylab("Female fertility rate per 1000")
print(p)
ggsave("za_female_2011.png", p)

ihme_sa_2006 <- ihme_sa[which(ihme_sa$date == 2006),]
ihme_sa_2006$label <- dhs$ages[2:9]
ihme_sa_2005 <- ihme_sa[which(ihme_sa$date == 2005),]
ihme_sa_2005$label <- dhs$ages[2:9]
ihme_sa_2004 <- ihme_sa[which(ihme_sa$date == 2004),]
ihme_sa_2004$label <- dhs$ages[2:9]
p <- ggplot() + 
  geom_point(data = dhs[2:15, ], aes(x = ages, y = y2006*1000, col = "dhs 2006")) + 
  geom_point(data = ihme_sa_2006, aes(x = label, y = afr, col = "ihme 2006")) + 
  geom_point(data = ihme_sa_2005, aes(x = label, y = afr, col = "ihme 2005")) + 
  geom_point(data = ihme_sa_2004, aes(x = label, y = afr, col = "ihme 2004")) + 
  scale_color_manual(values = c("dhs 2006" = "black", "ihme 2006" = "red", "ihme 2005" = "blue", "ihme 2004" = "purple")) + 
  theme_bw() + ylab("Female fertility rate per 1000")
print(p)
ggsave("za_female_2006.png", p)

