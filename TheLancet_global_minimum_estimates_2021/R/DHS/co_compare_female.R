dhs <- readRDS("data/Colombia/female_fertility.RDS")

ihme <- read_csv("data/fertility/IHME_both_files.csv")
ihme_co <- ihme[which(ihme$country == "Colombia"),]

online <- read_csv("data/DHS/co_female.csv")
online_a <- online[2, 4:10]
online_a <- data.frame("rates" = as.numeric(t(online_a)))
online_a$label <- dhs$ages[2:8]
online_b<- online[3, 4:10]
online_b <- data.frame("rates" = as.numeric(t(online_b)))
online_b$label <- dhs$ages[2:8]
online_c<- online[4, 4:10]
online_c <- data.frame("rates" = as.numeric(t(online_c)))
online_c$label <- dhs$ages[2:8]

ihme_co_2015 <- ihme_co[which(ihme_co$date == 2015),]
ihme_co_2015$label <- dhs$ages[2:9]
ihme_co_2014 <- ihme_co[which(ihme_co$date == 2014),]
ihme_co_2014$label <- dhs$ages[2:9]
ihme_co_2013 <- ihme_co[which(ihme_co$date == 2013),]
ihme_co_2013$label <- dhs$ages[2:9]

p <- ggplot() + 
  geom_point(data = dhs[2:15, ], aes(x = ages, y = y2015*1000, col = "dhs 2015")) + 
  geom_point(data = online_a, aes(x = label, y = rates, col = "dhs online")) + 
  geom_point(data = ihme_co_2015, aes(x = label, y = afr, col = "ihme 2015")) + 
  geom_point(data = ihme_co_2014, aes(x = label, y = afr,  col = "ihme 2014")) + 
  geom_point(data = ihme_co_2013, aes(x = label, y = afr, col = "ihme 2013")) + 
  scale_color_manual(values = c("dhs 2015" = "black", "dhs online" = "green", 
                                "ihme 2015" = "red", "ihme 2014" = "blue", "ihme 2013" = "purple")) + 
  theme_bw() + ylab("Female fertility rate per 1000")
print(p)
ggsave("co_female_2015.png", p)

ihme_co_2010 <- ihme_co[which(ihme_co$date == 2010),]
ihme_co_2010$label <- dhs$ages[2:9]
ihme_co_2009 <- ihme_co[which(ihme_co$date == 2009),]
ihme_co_2009$label <- dhs$ages[2:9]
ihme_co_2008 <- ihme_co[which(ihme_co$date == 2008),]
ihme_co_2008$label <- dhs$ages[2:9]
p <- ggplot() +
  geom_point(data = dhs[2:15, ], aes(x = ages, y = y2010*1000, col = "dhs 2010")) +
  geom_point(data = online_b, aes(x = label, y = rates, col = "dhs online")) + 
  geom_point(data = ihme_co_2010, aes(x = label, y = afr, col = "ihme 2010")) +
  geom_point(data = ihme_co_2009, aes(x = label, y = afr, col = "ihme 2009")) +
  geom_point(data = ihme_co_2008, aes(x = label, y = afr, col = "ihme 2008")) +
  scale_color_manual(values = c("dhs 2010" = "black", "dhs online" = "green", 
                                "ihme 2010" = "red", "ihme 2009" = "blue", "ihme 2008" = "purple")) +
  theme_bw() + ylab("Female fertility rate per 1000")
print(p)
ggsave("co_female_2010.png", p)

ihme_co_2005 <- ihme_co[which(ihme_co$date == 2005),]
ihme_co_2005$label <- dhs$ages[2:9]
ihme_co_2004 <- ihme_co[which(ihme_co$date == 2004),]
ihme_co_2004$label <- dhs$ages[2:9]
ihme_co_2003 <- ihme_co[which(ihme_co$date == 2003),]
ihme_co_2003$label <- dhs$ages[2:9]
p <- ggplot() +
  geom_point(data = dhs[2:15, ], aes(x = ages, y = y2005*1000, col = "dhs 2005")) +
  geom_point(data = online_c, aes(x = label, y = rates, col = "dhs online")) + 
  geom_point(data = ihme_co_2005, aes(x = label, y = afr, col = "ihme 2005")) +
  geom_point(data = ihme_co_2004, aes(x = label, y = afr, col = "ihme 2004")) +
  geom_point(data = ihme_co_2003, aes(x = label, y = afr, col = "ihme 2003")) +
  scale_color_manual(values = c("dhs 2005" = "black", "dhs online" = "green", 
                                "ihme 2005" = "red", "ihme 2004" = "blue", "ihme 2003" = "purple")) +
  theme_bw() + ylab("Female fertility rate per 1000")
print(p)
ggsave("co_female_2005.png", p)
