library(ggplot2)
library(reshape2)
library(scales)
library(tidyverse)
library(ggsci)


d <- read_csv("JAMAPeds_excess_deaths_update_2022/output/orphanhood_timeseries_who_adjusted.csv", show_col_types = FALSE)

dat <- d[d$country %in% d$country[2:7],]
regions <- c("AFRO","AMRO","EMRO","EURO","SEARO","WPRO")
dat$region <- rep(regions, length(dat$country) / length(regions))
#dat$date <- as.Date.POSIXct(dat$date)
#dat_long <- melt(data = dat, id = 'date')
dat$country[dat$country == "South-East Asia"] <- "South-East Asian" 


# truncate up to 1st May 2022
dat <- dat[dat$date <= "2022-05-01",]


p <- ggplot(dat, aes(x = date, y = primary_secondary, fill = country)) +
  geom_area() +
  ylab("Estimated number of children (millions)") +
  scale_y_continuous(expand = c(0,0),
                     breaks=seq(0,1.0e+07,1.0e+06), 
                     labels = label_number(scale = 1/1.0e+06) ) +
  scale_x_date(date_labels = "%b  %Y") +
  scale_fill_manual(values=c("#CC0000", "#0000CC", "#000000", "#006633",
                             "#E69F00","#9933CC" )) + 
  #scale_fill_jco()+
  theme_classic() +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1, size = 6.5),
        axis.text.y = element_text(size = 7),
        axis.title = element_text(size = 8),
        axis.title.x = element_blank(),
        legend.title = element_text(size = 5),
        legend.text=element_text(size=5),
        legend.key.size = unit(.35, 'cm'),
        #legend.margin=margin(t=-45),
        #legend.box.margin = margin(-3,-3,-3,-3),
        plot.title = element_text(hjust=0.5, size = 7))  +
  labs(fill = "WHO Region") #, title = paste("Pandemic Orphanhood and Caregiver Death among Children"))

#ggsave(filename = "JAMAPeds_excess_deaths_update_2022/figures/timeseries_may_2022.pdf", width = 14, height = 10, dpi = 300, units = "cm")
saveRDS(p, file = "JAMAPeds_excess_deaths_update_2022/figures/timeseries.RDS")
