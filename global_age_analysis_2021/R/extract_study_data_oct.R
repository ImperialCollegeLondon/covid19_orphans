# This code extracts all the necessary mortality and fertility estimates required for the
# analysis and calculates the number of orphans and children who have lost care from
# grandparents due to COVID-19

source("global_age_analysis_2021/R/extraction_deaths_oct.R")
source("global_age_analysis_2021/R/process_fertility.R")
source("global_age_analysis_2021/R/process_number_children.R")
source("global_age_analysis_2021/R/calculate_orphans.R")
source("global_age_analysis_2021/R/process_skip_generation.R")
source("global_age_analysis_2021/R/summary_orphans.R")

#### Argentina ##############################################################################
cat(sprintf("Running Argentina ======\n"))
cat(sprintf("Processing Death data\n"))
process_argentina_covid19()
# fertility -- we use Colombia DHS for Argentina as have no male fertility
cat(sprintf("Processing Number of children rates\n"))
process_number_children_colombia()
cat(sprintf("Processing number of orphans\n"))
process_orphans_argentina(month = "_oct")
cat(sprintf("Accounting for double orphans\n"))
combine_orphans("Argentina", process_argentina_skip_generation(month = "_oct"), month = "_oct")


#### Brazil ##############################################################################
cat(sprintf("Running Brazil======\n"))
cat(sprintf("Processing Death data\n"))
age_range = c(seq(0,100,5), Inf)
#process_brazil(age_range) <- this line is commented out since the data file takes a long time to download from the internet.  The file that is saved by this function is provided.
cat(sprintf("Processing Number of children rates\n"))
process_number_children_brazil()
cat(sprintf("Comparing Brazilian fertilty rates\n"))
cat(sprintf("Processing number of orphans\n"))
process_orphans_brazil(month = "_oct")
cat(sprintf("Accounting for double orphans\n"))
combine_orphans("Brazil", process_brazil_skip_generation(month = "_oct"), month = "_oct")


#### Colombia ####################################################################################
cat(sprintf("Running Colombia======\n"))
cat(sprintf("Processing Death data\n"))
process_colombia_covid19()
cat(sprintf("Processing Number of children rates\n"))
process_number_children_colombia()
cat(sprintf("Processing number of orphans\n"))
process_orphans_colombia(month = "_oct")
cat(sprintf("Accounting for double orphans\n"))
combine_orphans("Colombia", process_colombia_skip_generation(month = "_oct"), month = "_oct")


####  England and Wales ####################################################################################
cat(sprintf("Running England and Wales======\n"))
cat(sprintf("Processing Death data\n"))
process_england_wales()
cat(sprintf("Processing number of children rates\n"))
process_number_children_england_wales()
cat(sprintf("Processing number of orphans\n"))
process_orphans_england_wales(month = "_oct")
combine_orphans(country ="EnglandWales", 
                process_skip_generation_england_wales(month = "_oct"), month = "_oct")


####  France #########################################################################################################
cat(sprintf("Running France======\n"))
cat(sprintf("Processing Death data\n"))
process_france()
cat(sprintf("Processing number of children rates\n"))
process_number_children_france()
cat(sprintf("Processing number of orphans\n"))
process_orphans_france(month = "_oct")
combine_orphans("France", process_france_skip_generation(month = "_oct"), month = "_oct")


####  Germany #########################################################################################################
cat(sprintf("Running Germany======\n"))
cat(sprintf("Processing Death data\n"))
process_germany()
cat(sprintf("Processing number of children rates\n"))
process_number_children_germany()
cat(sprintf("Processing number of orphans\n"))
process_orphans_germany(month = "_oct")
combine_orphans("Germany", process_germany_skip_generation(month = "_oct"), month = "_oct")


#### India #########################################################################################################
cat(sprintf("Running India======\n"))
cat(sprintf("Processing Death data\n"))
process_india_covid()
cat(sprintf("Processing number of children rates\n"))
process_number_children_india()
cat(sprintf("Processing number of orphans\n"))
process_orphans_india(month = "_oct")
combine_orphans("India", process_india_skip_generation(month = "_oct"), month = "_oct")


#### Iran #########################################################################################################
cat(sprintf("Running Iran ======\n"))
cat(sprintf("Processing Death data\n"))
process_iran()
cat(sprintf("Processing number of children rates\n"))
process_number_children_iran()
cat(sprintf("Processing number of orphans\n"))
process_orphans_iran(month = "_oct")
combine_orphans("Iran", process_iran_skip_generation(month = "_oct"), month = "_oct")


####  Italy #########################################################################################################
cat(sprintf("Running Italy======\n"))
cat(sprintf("Processing Death data\n"))
process_italy()
cat(sprintf("Processing number of children rates\n"))
process_number_children_italy()
cat(sprintf("Processing number of orphans\n"))
process_orphans_italy(month = "_oct")
combine_orphans("Italy", process_italy_skip_generation(month = "_oct"), month = "_oct")


#### Kenya #########################################################################################################
cat(sprintf("Running Kenya======\n"))
# deaths:
cat(sprintf("Processing Death data\n"))
process_kenya_covid19()
cat(sprintf("Processing number of children rates\n"))
process_number_children_kenya()
cat(sprintf("Processing number of orphans\n"))
process_orphans_kenya(month = "_oct")
combine_orphans("Kenya", process_kenya_skip_generation(month = "_oct"), month = "_oct")


####  Malawi  ####################################################################################
cat(sprintf("Running Malawi======\n"))
cat(sprintf("Processing Death data\n"))
process_malawi()
cat(sprintf("Processing number of children rates\n"))
process_number_children_malawi()
cat(sprintf("Processing number of orphans\n"))
process_orphans_malawi(month = "_oct")
combine_orphans(country ="Malawi", 
                process_malawi_skip_generation(month = "_oct"), month = "_oct")


####  Mexico #########################################################################################################
cat(sprintf("Running Mexico======\n"))
cat(sprintf("Processing Death data\n"))
process_mexico_covid19()
cat(sprintf("Processing number of children rates\n"))
process_number_children_mexico()
cat(sprintf("Processing number of orphans\n"))
process_orphans_mexico(month = "_oct")
combine_orphans("Mexico", process_mexico_skip_generation(month = "_oct"), month = "_oct")


#### Nigeria #########################################################################################################
cat(sprintf("Running Nigeria======\n"))
cat(sprintf("Processing Death data\n"))
process_nigeria_covid19()
cat(sprintf("Processing number of children rates\n"))
process_number_children_nigeria()
cat(sprintf("Processing number of orphans\n"))
process_orphans_nigeria(month = "_oct")
combine_orphans("Nigeria", process_nigeria_skip_generation(month = "_oct"), month = "_oct")


#### Peru #########################################################################################################
cat(sprintf("Running Peru======\n"))
cat(sprintf("Processing Death data\n"))
process_peru_covid19()
cat(sprintf("Processing number of children rates\n"))
process_number_children_peru()
cat(sprintf("Processing number of orphans\n"))
process_orphans_peru(month = "_oct")
combine_orphans("Peru", process_peru_skip_generation(month = "_oct"), month = "_oct")


#### Philippines #########################################################################################################
cat(sprintf("Running Philippines======\n"))
cat(sprintf("Processing Death data\n"))
#process_philippines() #<- see Readme for where to download this data.
cat(sprintf("Processing number of children rates\n"))
process_number_children_philippines()
cat(sprintf("Processing number of orphans\n"))
process_orphans_philippines(month = "_oct")
combine_orphans("Philippines", process_philippines_skip_generation(month = "_oct"), month = "_oct")


####  Poland #########################################################################################################
cat(sprintf("Running Poland======\n"))
cat(sprintf("Processing Death data\n"))
process_poland_covid19()
cat(sprintf("Processing number of children rates\n"))
process_number_children_poland()
cat(sprintf("Processing number of orphans\n"))
process_orphans_poland(month = "_oct")
combine_orphans("Poland", process_poland_skip_generation(month = "_oct"), month = "_oct")


####  Russia #########################################################################################################
cat(sprintf("Running Russian Federation======\n"))
cat(sprintf("Processing Death data\n"))
process_russia_excess()
cat(sprintf("Processing number of children rates\n"))
process_number_children_russia()
cat(sprintf("Processing number of orphans\n"))
process_orphans_russia(month = "_oct")
combine_orphans("Russia", process_russia_skip_generation(month = "_oct"), month = "_oct")


#### South Africa  #########################################################################################################
cat(sprintf("Running South Africa======\n"))
cat(sprintf("Processing Death data\n"))
process_sa_covid19()
cat(sprintf("Processing number of children rates\n"))
process_number_children_south_africa()
cat(sprintf("Processing number of orphans\n"))
process_orphans_south_africa(month = "_oct")
combine_orphans("SouthAfrica", process_south_africa_skip_generation(month = "_oct"), month = "_oct")


####  Spain ########################################################################################################################
cat(sprintf("Running Spain======\n"))
cat(sprintf("Processing Death data\n"))
process_spain()
cat(sprintf("Processing number of children rates\n"))
process_number_children_spain()
cat(sprintf("Processing number of orphans\n"))
process_orphans_spain(month = "_oct")
combine_orphans("Spain", process_spain_skip_generation(month = "_oct"), month = "_oct")


####  USA ########################################################################################################################
cat(sprintf("Running USA======\n"))
cat(sprintf("Processing Death data\n"))
process_usa()
cat(sprintf("Processing number of children rates\n"))
process_number_children_usa()
cat(sprintf("Processing number of orphans\n"))
process_orphans_usa(month = "_oct")
combine_orphans("USA", process_usa_skip_generation(month = "_oct"), month = "_oct")


####  Zimbabwe ########################################################################################################################
cat(sprintf("Running Zimbabwe ======\n"))
cat(sprintf("Processing Death data\n"))
process_zimbabwe()
cat(sprintf("Processing number of children rates\n"))
process_number_children_zimbabwe()
cat(sprintf("Processing number of orphans\n"))
process_orphans_zimbabwe(month = "_oct")
combine_orphans("Zimbabwe", process_zimbabwe_skip_generation(month = "_oct"), month = "_oct")


### Summary ###################################################################################
source("global_age_analysis_2021/R/orphans_table_oct.R")



