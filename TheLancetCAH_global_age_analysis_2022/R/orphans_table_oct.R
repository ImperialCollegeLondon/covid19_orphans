library(tidyverse)

source("TheLancetCAH_global_age_analysis_2022/R/summary_orphans.R")

argentina = t(data.frame(combine_orphans("Argentina", process_argentina_skip_generation(month = "_oct"), month = "_oct")))
brazil = t(data.frame(combine_orphans("Brazil", process_brazil_skip_generation(month = "_oct"), month = "_oct")))
colombia = t(data.frame(combine_orphans("Colombia", process_colombia_skip_generation(month = "_oct"), month = "_oct")))
england = t(data.frame(combine_orphans(country ="EnglandWales", process_skip_generation_england_wales(month = "_oct"), month = "_oct")))
france =  t(data.frame(combine_orphans("France", process_france_skip_generation(month = "_oct"), month = "_oct")))
germany = t(data.frame(combine_orphans("Germany", process_germany_skip_generation(month = "_oct"), month = "_oct")))
india = t(data.frame(combine_orphans("India", process_india_skip_generation(month = "_oct"), month = "_oct")))
iran = t(data.frame(combine_orphans("Iran", process_iran_skip_generation(month = "_oct"), month = "_oct")))
italy = t(data.frame(combine_orphans("Italy", process_italy_skip_generation(month = "_oct"), month = "_oct" )))
kenya = t(data.frame(combine_orphans("Kenya", process_kenya_skip_generation(month = "_oct"), month = "_oct")))
malawi = t(data.frame(combine_orphans("Malawi", process_malawi_skip_generation(month = "_oct"), month = "_oct")))
mexico = t(data.frame(combine_orphans("Mexico", process_mexico_skip_generation(month = "_oct"), month = "_oct")))
nigeria = t(data.frame(combine_orphans("Nigeria", process_nigeria_skip_generation(month = "_oct"), month = "_oct")))
peru = t(data.frame(combine_orphans("Peru", process_peru_skip_generation(month = "_oct"), month = "_oct")))
philippines = t(data.frame(combine_orphans("Philippines", process_philippines_skip_generation(month = "_oct"), month = "_oct")))
poland = t(data.frame(combine_orphans("Poland", process_poland_skip_generation(month = "_oct"), month = "_oct")))
russia = t(data.frame(combine_orphans("Russia", process_russia_skip_generation(month = "_oct"), month = "_oct")))
south_africa = t(data.frame(combine_orphans("SouthAfrica", process_south_africa_skip_generation(month = "_oct"), month = "_oct")))
spain = t(data.frame(combine_orphans("Spain", process_spain_skip_generation(month = "_oct"), month = "_oct")))
usa = t(data.frame(combine_orphans("USA", process_usa_skip_generation(month = "_oct"), month = "_oct")))
zimbabwe = t(data.frame(combine_orphans("Zimbabwe", process_zimbabwe_skip_generation(month = "_oct"), month = "_oct")))

countries <- c("Argentina", "Brazil", "Colombia", "England", "France", "Germany", "India", "I.R. Iran",
               "Italy", "Kenya", "Malawi", "Mexico",
               "Nigeria", "Peru", "Philippines", "Poland", "Russian Federation", "South Africa", "Spain", "USA", "Zimbabwe")
df = rbind(argentina, brazil, colombia, england, france, germany, india, iran, italy, kenya, malawi,
           mexico, nigeria, peru, philippines, poland, russia, south_africa, spain, usa, zimbabwe)

# Set of countries for a sensitivity analysis
#countries <- c("Brazil", "Colombia", "England", "India", "I.R. Iran",
#               "Italy", "Kenya",  "Malawi", "Mexico",
#               "Nigeria", "Peru", "Philippines", "Poland", "Russian Federation", "South Africa", 
#               "Spain", "USA", "Zimbabwe")
#df = rbind(brazil, colombia, england, india, iran, italy, kenya, malawi,
#           mexico, nigeria, peru, philippines, poland, russia, south_africa, spain, usa, zimbabwe)


row.names(df) = countries
colnames(df) = c("deaths", "mother", "father", "both", "orphans", "sg_grandmother", "sg_grandfather", 
                 "sg_both", "primary_loss", "mg_grandmother", "mg_grandfather", "mg_both", "all", "ratio")

saveRDS(df, file = "TheLancetCAH_global_age_analysis_2022/orphans.RDS")

# df = as.data.frame(df)
# df$country = countries
# 
# # Table 1
# tab_1 <- select(df, "mother", "father", "both", "orphans", "sg_grandmother", "sg_grandfather", 
#                 "sg_both", "primary_loss")
# write.csv(tab_1, file = "TheLancetCAH_global_age_analysis_2022/orphans_tab1.csv", row.names=FALSE)
# 
# # Table 2
# child_pop <- read.csv("data/numbers_of_children.csv", stringsAsFactors = FALSE)
# child_pop <- select(child_pop, X, total)
# child_pop$X[which(child_pop$X == "Russia")] <- "Russian Federation"
# child_pop$X[which(child_pop$X == "Iran")] <- "I.R. Iran"
# 
# df <- left_join(df, data.frame(child_pop), by = c("country" = "X"))
# df$orphans_per_thousand = df$orphans/(df$total/1000)
# df$primary_per_thousand = df$primary_loss/(df$total/1000)
# df$all_per_thousand = df$all/(df$total/1000)
# tab_2 <- select(df, country, orphans, orphans_per_thousand, primary_loss, 
#                 primary_per_thousand, mg_grandmother, mg_grandfather, mg_both, 
#                 all, all_per_thousand)
# write.csv(tab_2, file = "TheLancetCAH_global_age_analysis_2022/orphans_tab2.csv", row.names=FALSE)