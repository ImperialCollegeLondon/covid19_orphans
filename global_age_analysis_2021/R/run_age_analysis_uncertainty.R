# Run the original orphans code first to extract the death data and get central estimates.
# Run global extrapolation to get global totals.
# Run this script to get uncertainty in age.

# 1) Read in fertility data - sample from it.
# 2) Sample from fertility data
# 3) Generate average child data
# 4) sample from death data
# 5) Calculate orphans.

source("global_age_analysis_2021/R/sample_deaths.R")
source("global_age_analysis_2021/R/number_orphans_age.R")
source("global_age_analysis_2021/R/process_number_children.R")
source("global_age_analysis_2021/R/get_diff_deaths.R")

n = 5

run_age_analysis <- function(month = "_oct"){
  if (month != "_diff"){
    source(paste0("global_age_analysis_2021/R/extraction_deaths", month, ".R"))
  }

  
  set.seed(3)
  
  # Argentina -----------------------------------------------------------------
  cat(sprintf("Running Argentina======\n"))
  cat(sprintf("Calculating central estimate\n"))
  if (month == "_diff"){
    get_diff_deaths('global_age_analysis_2021/data/Argentina/covid19_deaths.csv', 'global_age_analysis_2021/data/Argentina/covid19_deaths_oct.csv', 
                    'global_age_analysis_2021/data/Argentina/covid19_deaths_diff.csv')
  } else {
    process_argentina_covid19()
  }
  process_number_children_colombia(uncertainty = FALSE)
  arg_central = process_orphans_age_argentina(uncertainty = FALSE, month = month)
  
  cat(sprintf("Doing bootstrap\n"))
  arg <- NULL
  start_time <- Sys.time()
  for (i in 1:n){
    sample_deaths_argentina(month = month)
    process_number_children_colombia(uncertainty = TRUE)
    arg <- rbind(arg, process_orphans_age_argentina(uncertainty = TRUE, month = month))
  }
  
  cat(sprintf("Calculating uncertainty estimate\n"))
  arg_quantiles <- arg %>%
    group_by(category, gender) %>%
    summarise(li_raw = quantile(orphans, probs = 0.025),
              ui_raw = quantile(orphans, probs = 0.975),
              li_percent = quantile(orphans_percent, probs = 0.025),
              ui_percent = quantile(orphans_percent, probs = 0.975))
  
  arg_quantiles$raw = arg_central$orphans
  arg_quantiles$percent = arg_central$orphans_percent
  arg_quantiles$country = "Argentina"
  end_time <- Sys.time()
  print(end_time - start_time)
  
  arg$country = "Argentina"
  
  saveRDS(arg, paste0("global_age_analysis_2021/data/age_outputs/Argentina", month, ".RDS"))
  
  # Brazil -----------------------------------------------------------------
  cat(sprintf("Running Brazil======\n"))
  cat(sprintf("Calculating central estimate\n"))

  if (month == "_diff"){
    get_diff_COVID19_deaths('global_age_analysis_2021/data/Brazil/covid19_deaths.csv', 'global_age_analysis_2021/data/Brazil/covid19_deaths_oct.csv', 
                            'global_age_analysis_2021/data/Brazil/covid19_deaths_diff.csv', "Brazil")
  } else {
    #process_brazil(c(seq(0,100,5), Inf))
  }
  process_number_children_brazil(uncertainty = FALSE)
  bra_central = process_orphans_age_brazil(uncertainty = FALSE, month = month)
  
  cat(sprintf("Doing bootstrap\n"))
  bra <- NULL
  start_time <- Sys.time()
  for (i in 1:n){
    sample_deaths_brazil(month = month)
    process_number_children_brazil(uncertainty = TRUE)
    bra <- rbind(bra, process_orphans_age_brazil(uncertainty = TRUE, month = month))
  }
  
  cat(sprintf("Calculating uncertainty estimate\n"))
  bra_quantiles <- bra %>%
    group_by(category, gender) %>%
    summarise(li_raw = quantile(orphans, probs = 0.025),
              ui_raw = quantile(orphans, probs = 0.975),
              li_percent = quantile(orphans_percent, probs = 0.025),
              ui_percent = quantile(orphans_percent, probs = 0.975))
  
  bra_quantiles$raw = bra_central$orphans
  bra_quantiles$percent = bra_central$orphans_percent
  bra_quantiles$country = "Brazil"
  end_time <- Sys.time()
  print(end_time - start_time)
  
  bra$country = "Brazil"
  saveRDS(bra, paste0("global_age_analysis_2021/data/age_outputs/Brazil", month, ".RDS"))
  
  # Colombia -----------------------------------------------------------------
  cat(sprintf("Running Colombia======\n"))
  cat(sprintf("Calculating central estimate\n"))
  if (month == "_diff"){
    get_diff_deaths_colombia('global_age_analysis_2021/data/Colombia/covid19_deaths_all.csv', 'global_age_analysis_2021/data/Colombia/covid19_deaths_all_oct.csv', 
                    'global_age_analysis_2021/data/Colombia/covid19_deaths_all_diff.csv', "Colombia")
  } else {
    process_colombia_covid19()
  }
  process_number_children_colombia(uncertainty = FALSE)
  col_central = process_orphans_age_colombia(uncertainty = FALSE, month = month)
  
  cat(sprintf("Doing bootstrap\n"))
  col <- NULL
  start_time <- Sys.time()
  for (i in 1:n){
    sample_deaths_colombia(month = month)
    process_number_children_colombia(uncertainty = TRUE)
    col <- rbind(col, process_orphans_age_colombia(uncertainty = TRUE, month = month))
  }
  
  cat(sprintf("Calculating uncertainty estimate\n"))
  col_quantiles <- col %>%
    group_by(category, gender) %>%
    summarise(li_raw = quantile(orphans, probs = 0.025),
              ui_raw = quantile(orphans, probs = 0.975),
              li_percent = quantile(orphans_percent, probs = 0.025),
              ui_percent = quantile(orphans_percent, probs = 0.975))
  
  col_quantiles$raw = col_central$orphans
  col_quantiles$percent = col_central$orphans_percent
  col_quantiles$country = "Colombia"
  end_time <- Sys.time()
  print(end_time - start_time)
  
  col$country = "Colombia"
  saveRDS(col, paste0("global_age_analysis_2021/data/age_outputs/Colombia", month, ".RDS"))
  
  # England and Wales -----------------------------------------------------------------
  cat(sprintf("Running England and Wales======\n"))
  cat(sprintf("Calculating central estimate\n"))
  if (month == "_diff"){
    get_diff_deaths('global_age_analysis_2021/data/UK/deaths_all_england_wales.csv', 'global_age_analysis_2021/data/UK/deaths_all_england_wales_oct.csv', 
                    'global_age_analysis_2021/data/UK/deaths_all_england_wales_diff.csv')
  } else {
    process_england_wales()
  }
  process_number_children_england_wales(uncertainty = FALSE)
  eng_central = process_orphans_age_england_wales(uncertainty = FALSE, month = month)
  
  cat(sprintf("Doing bootstrap\n"))
  eng <- NULL
  start_time <- Sys.time()
  for (i in 1:n){
    sample_deaths_england_wales(month = month)
    process_number_children_england_wales(uncertainty = TRUE)
    eng <- rbind(eng, process_orphans_age_england_wales(uncertainty = TRUE, month = month))
  }
  
  cat(sprintf("Calculating uncertainty estimate\n"))
  eng_quantiles <- eng %>%
    group_by(category, gender) %>%
    summarise(li_raw = quantile(orphans, probs = 0.025),
              ui_raw = quantile(orphans, probs = 0.975),
              li_percent = quantile(orphans_percent, probs = 0.025),
              ui_percent = quantile(orphans_percent, probs = 0.975))
  
  eng_quantiles$raw = eng_central$orphans
  eng_quantiles$percent = eng_central$orphans_percent
  eng_quantiles$country = "England & Wales"
  end_time <- Sys.time()
  print(end_time - start_time)
  
  eng$country = "England & Wales"
  
  saveRDS(eng, paste0("global_age_analysis_2021/data/age_outputs/England", month, ".RDS"))
  
  # France -----------------------------------------------------------------
  cat(sprintf("Running France ======\n"))
  cat(sprintf("Calculating central estimate\n"))
  if (month == "_diff"){
    get_diff_deaths('global_age_analysis_2021/data/France/france_all.csv', 'global_age_analysis_2021/data/France/france_all_oct.csv',
                    'global_age_analysis_2021/data/France/france_all_diff.csv')
  } else {
    process_france()
  }
  process_number_children_france(uncertainty = FALSE)
  fra_central = process_orphans_age_france(uncertainty = FALSE, month = month)
  
  cat(sprintf("Doing bootstrap\n"))
  fra <- NULL
  start_time <- Sys.time()
  for (i in 1:n){
    sample_deaths_france(month = month)
    process_number_children_france(uncertainty = TRUE)
    fra <- rbind(fra, process_orphans_age_france(uncertainty = TRUE, month = month))
  }
  
  cat(sprintf("Calculating uncertainty estimate\n"))
  fra_quantiles <- fra %>%
    group_by(category, gender) %>%
    summarise(li_raw = quantile(orphans, probs = 0.025),
              ui_raw = quantile(orphans, probs = 0.975),
              li_percent = quantile(orphans_percent, probs = 0.025),
              ui_percent = quantile(orphans_percent, probs = 0.975))
  
  fra_quantiles$raw = fra_central$orphans
  fra_quantiles$percent = fra_central$orphans_percent
  fra_quantiles$country = "France"
  end_time <- Sys.time()
  print(end_time - start_time)
  
  fra$country = "France"
  
  saveRDS(fra, paste0("global_age_analysis_2021/data/age_outputs/France", month, ".RDS"))
  
  # Germany -----------------------------------------------------------------
  cat(sprintf("Running Germany ======\n"))
  cat(sprintf("Calculating central estimate\n"))
  if (month == "_diff"){
    get_diff_deaths('global_age_analysis_2021/data/Germany/covid19_deaths.csv', 'global_age_analysis_2021/data/Germany/covid19_deaths_oct.csv',
                    'global_age_analysis_2021/data/Germany/covid19_deaths_diff.csv')
  } else {
    process_germany()
  }
  process_number_children_germany(uncertainty = FALSE)
  ger_central = process_orphans_age_germany(uncertainty = FALSE, month = month)
  
  cat(sprintf("Doing bootstrap\n"))
  ger <- NULL
  start_time <- Sys.time()
  for (i in 1:n){
    sample_deaths_germany(month = month)
    process_number_children_germany(uncertainty = TRUE)
    ger <- rbind(ger, process_orphans_age_germany(uncertainty = TRUE, month = month))
  }
  
  cat(sprintf("Calculating uncertainty estimate\n"))
  ger_quantiles <- ger %>%
    group_by(category, gender) %>%
    summarise(li_raw = quantile(orphans, probs = 0.025),
              ui_raw = quantile(orphans, probs = 0.975),
              li_percent = quantile(orphans_percent, probs = 0.025),
              ui_percent = quantile(orphans_percent, probs = 0.975))
  
  ger_quantiles$raw = ger_central$orphans
  ger_quantiles$percent = ger_central$orphans_percent
  ger_quantiles$country = "Germany"
  end_time <- Sys.time()
  print(end_time - start_time)
  
  ger$country = "Germany"
  saveRDS(ger, paste0("global_age_analysis_2021/data/age_outputs/Germany", month, ".RDS"))
  
  # India -----------------------------------------------------------------
  cat(sprintf("Running India ======\n"))
  cat(sprintf("Calculating central estimate\n"))
  if (month == "_diff"){
    get_diff_deaths_india('global_age_analysis_2021/data/India/all_covid_deaths.csv', 'global_age_analysis_2021/data/India/all_covid_deaths_oct.csv',
                          'global_age_analysis_2021/data/India/all_covid_deaths_diff.csv', "India")
  } else {
    process_india_covid()
  }
  process_number_children_india(uncertainty = FALSE)
  ind_central = process_orphans_age_india(uncertainty = FALSE, month = month)
  
  cat(sprintf("Doing bootstrap\n"))
  ind <- NULL
  start_time <- Sys.time()
  for (i in 1:n){
    sample_deaths_india(month = month)
    process_number_children_india(uncertainty = TRUE)
    ind <- rbind(ind, process_orphans_age_india(uncertainty = TRUE, month = month))
  }
  
  cat(sprintf("Calculating uncertainty estimate\n"))
  ind_quantiles <- ind %>%
    group_by(category, gender) %>%
    summarise(li_raw = quantile(orphans, probs = 0.025),
              ui_raw = quantile(orphans, probs = 0.975),
              li_percent = quantile(orphans_percent, probs = 0.025),
              ui_percent = quantile(orphans_percent, probs = 0.975))
  
  ind_quantiles$raw = ind_central$orphans
  ind_quantiles$percent = ind_central$orphans_percent
  ind_quantiles$country = "India"
  end_time <- Sys.time()
  print(end_time - start_time)
  
  ind$country = "India"
  
  saveRDS(ind, paste0("global_age_analysis_2021/data/age_outputs/India", month, ".RDS"))
  
  # Iran -----------------------------------------------------------------
  cat(sprintf("Running Iran ======\n"))
  cat(sprintf("Calculating central estimate\n"))
  if (month == "_diff"){
    get_diff_deaths('global_age_analysis_2021/data/Iran/iran_all.csv', 'global_age_analysis_2021/data/Iran/iran_all_oct.csv',
                    'global_age_analysis_2021/data/Iran/iran_all_diff.csv', "Iran (Islamic Republic of)")
  } else {
    process_iran()
  }
  process_number_children_iran(uncertainty = FALSE)
  ira_central = process_orphans_age_iran(uncertainty = FALSE, month = month)
  
  cat(sprintf("Doing bootstrap\n"))
  ira <- NULL
  start_time <- Sys.time()
  for (i in 1:n){
    sample_deaths_iran(month = month)
    process_number_children_iran(uncertainty = TRUE)
    ira <- rbind(ira, process_orphans_age_iran(uncertainty = TRUE, month = month))
  }
  
  cat(sprintf("Calculating uncertainty estimate\n"))
  ira_quantiles <- ira %>%
    group_by(category, gender) %>%
    summarise(li_raw = quantile(orphans, probs = 0.025),
              ui_raw = quantile(orphans, probs = 0.975),
              li_percent = quantile(orphans_percent, probs = 0.025),
              ui_percent = quantile(orphans_percent, probs = 0.975))
  
  ira_quantiles$raw = ira_central$orphans
  ira_quantiles$percent = ira_central$orphans_percent
  ira_quantiles$country = "Iran (Islamic Republic of)"
  end_time <- Sys.time()
  print(end_time - start_time)
  
  ira$country = "Iran (Islamic Republic of)"
  saveRDS(ira, paste0("global_age_analysis_2021/data/age_outputs/Iran", month, ".RDS"))
  
  # Italy -----------------------------------------------------------------
  cat(sprintf("Running Italy ======\n"))
  cat(sprintf("Calculating central estimate\n"))
  if (month == "_diff"){
    get_diff_deaths('global_age_analysis_2021/data/Italy/italy_all.csv', 'global_age_analysis_2021/data/Italy/italy_all_oct.csv',
                    'global_age_analysis_2021/data/Italy/italy_all_diff.csv')
  } else {
    process_italy()
  }
  process_number_children_italy(uncertainty = FALSE)
  ita_central = process_orphans_age_italy(uncertainty = FALSE, month = month)
  
  cat(sprintf("Doing bootstrap\n"))
  ita <- NULL
  start_time <- Sys.time()
  for (i in 1:n){
    sample_deaths_italy(month = month)
    process_number_children_italy(uncertainty = TRUE)
    ita <- rbind(ita, process_orphans_age_italy(uncertainty = TRUE, month = month))
  }
  
  cat(sprintf("Calculating uncertainty estimate\n"))
  ita_quantiles <- ita %>%
    group_by(category, gender) %>%
    summarise(li_raw = quantile(orphans, probs = 0.025),
              ui_raw = quantile(orphans, probs = 0.975),
              li_percent = quantile(orphans_percent, probs = 0.025),
              ui_percent = quantile(orphans_percent, probs = 0.975))
  
  ita_quantiles$raw = ita_central$orphans
  ita_quantiles$percent = ita_central$orphans_percent
  ita_quantiles$country = "Italy"
  end_time <- Sys.time()
  print(end_time - start_time)
  
  ita$country = "Italy"
  saveRDS(ita, paste0("global_age_analysis_2021/data/age_outputs/Italy", month, ".RDS"))
  
  # Kenya -----------------------------------------------------------------
  cat(sprintf("Running Kenya ======\n"))
  cat(sprintf("Calculating central estimate\n"))
  if (month == "_diff"){
    get_diff_deaths('global_age_analysis_2021/data/Kenya/covid19_deaths.csv', 'global_age_analysis_2021/data/Kenya/covid19_deaths_oct.csv', 
                    'global_age_analysis_2021/data/Kenya/covid19_deaths_diff.csv')
  } else {
    process_kenya_covid19()
  }
  process_number_children_kenya(uncertainty = FALSE)
  ken_central = process_orphans_age_kenya(uncertainty = FALSE, month = month)
  
  cat(sprintf("Doing bootstrap\n"))
  ken <- NULL
  start_time <- Sys.time()
  for (i in 1:n){
    sample_deaths_kenya(month = month)
    process_number_children_kenya(uncertainty = TRUE)
    ken <- rbind(ken, process_orphans_age_kenya(uncertainty = TRUE, month = month))
  }
  
  cat(sprintf("Calculating uncertainty estimate\n"))
  ken_quantiles <- ken %>%
    group_by(category, gender) %>%
    summarise(li_raw = quantile(orphans, probs = 0.025),
              ui_raw = quantile(orphans, probs = 0.975),
              li_percent = quantile(orphans_percent, probs = 0.025),
              ui_percent = quantile(orphans_percent, probs = 0.975))
  
  ken_quantiles$raw = ken_central$orphans
  ken_quantiles$percent = ken_central$orphans_percent
  ken_quantiles$country = "Kenya"
  end_time <- Sys.time()
  print(end_time - start_time)
  
  ken$country = "Kenya"
  saveRDS(ken, paste0("global_age_analysis_2021/data/age_outputs/Kenya", month, ".RDS"))
  
  # Malawi -----------------------------------------------------------------
  cat(sprintf("Running Malawi ======\n"))
  cat(sprintf("Calculating central estimate\n"))
  if (month == "_diff"){
    get_diff_deaths('global_age_analysis_2021/data/Malawi/covid19_deaths.csv', 'global_age_analysis_2021/data/Malawi/covid19_deaths_oct.csv', 
                    'global_age_analysis_2021/data/Malawi/covid19_deaths_diff.csv')
  } else {
    process_malawi()
  }
  process_number_children_malawi(uncertainty = FALSE)
  mal_central = process_orphans_age_malawi(uncertainty = FALSE, month = month)
  
  cat(sprintf("Doing bootstrap\n"))
  mal <- NULL
  start_time <- Sys.time()
  for (i in 1:n){
    sample_deaths_malawi(month = month)
    process_number_children_malawi(uncertainty = TRUE)
    mal <- rbind(mal, process_orphans_age_malawi(uncertainty = TRUE, month = month))
  }
  
  cat(sprintf("Calculating uncertainty estimate\n"))
  mal_quantiles <- mal %>%
    group_by(category, gender) %>%
    summarise(li_raw = quantile(orphans, probs = 0.025),
              ui_raw = quantile(orphans, probs = 0.975),
              li_percent = quantile(orphans_percent, probs = 0.025),
              ui_percent = quantile(orphans_percent, probs = 0.975))
  
  mal_quantiles$raw = mal_central$orphans
  mal_quantiles$percent = mal_central$orphans_percent
  mal_quantiles$country = "Malawi"
  end_time <- Sys.time()
  print(end_time - start_time)
  
  mal$country = "Malawi"
  saveRDS(mal, paste0("global_age_analysis_2021/data/age_outputs/Malawi", month, ".RDS"))
  
  # Mexico -----------------------------------------------------------------
  cat(sprintf("Running Mexico ======\n"))
  cat(sprintf("Calculating central estimate\n"))
  if (month == "_diff"){
    get_diff_deaths('global_age_analysis_2021/data/Mexico/covid19_deaths.csv', 'global_age_analysis_2021/data/Mexico/covid19_deaths_oct.csv', 
                    'global_age_analysis_2021/data/Mexico/covid19_deaths_diff.csv', "Mexico")
  } else {
    process_mexico_covid19()
  }
  process_number_children_mexico(uncertainty = FALSE)
  mex_central = process_orphans_age_mexico(uncertainty = FALSE, month = month)
  
  cat(sprintf("Doing bootstrap\n"))
  mex <- NULL
  start_time <- Sys.time()
  for (i in 1:n){
    sample_deaths_mexico(month = month)
    process_number_children_mexico(uncertainty = TRUE)
    mex <- rbind(mex, process_orphans_age_mexico(uncertainty = TRUE, month = month))
  }
  
  cat(sprintf("Calculating uncertainty estimate\n"))
  mex_quantiles <- mex %>%
    group_by(category, gender) %>%
    summarise(li_raw = quantile(orphans, probs = 0.025),
              ui_raw = quantile(orphans, probs = 0.975),
              li_percent = quantile(orphans_percent, probs = 0.025),
              ui_percent = quantile(orphans_percent, probs = 0.975))
  
  mex_quantiles$raw = mex_central$orphans
  mex_quantiles$percent = mex_central$orphans_percent
  mex_quantiles$country = "Mexico"
  end_time <- Sys.time()
  print(end_time - start_time)
  
  mex$country = "Mexico"
  saveRDS(mex, paste0("global_age_analysis_2021/data/age_outputs/Mexico", month, ".RDS"))
  
  # Nigeria -----------------------------------------------------------------
  cat(sprintf("Running Nigeria ======\n"))
  cat(sprintf("Calculating central estimate\n"))
  if (month == "_diff"){
    get_diff_deaths('global_age_analysis_2021/data/Nigeria/covid19_deaths.csv', 'global_age_analysis_2021/data/Nigeria/covid19_deaths_oct.csv', 
                    'global_age_analysis_2021/data/Nigeria/covid19_deaths_diff.csv')
  } else {
    process_nigeria_covid19()
  }
  process_number_children_nigeria(uncertainty = FALSE)
  nig_central = process_orphans_age_nigeria(uncertainty = FALSE, month = month)
  
  cat(sprintf("Doing bootstrap\n"))
  nig <- NULL
  start_time <- Sys.time()
  for (i in 1:n){
    sample_deaths_nigeria(month = month)
    process_number_children_nigeria(uncertainty = TRUE)
    nig <- rbind(nig, process_orphans_age_nigeria(uncertainty = TRUE, month = month))
  }
  
  cat(sprintf("Calculating uncertainty estimate\n"))
  nig_quantiles <- nig %>%
    group_by(category, gender) %>%
    summarise(li_raw = quantile(orphans, probs = 0.025),
              ui_raw = quantile(orphans, probs = 0.975),
              li_percent = quantile(orphans_percent, probs = 0.025),
              ui_percent = quantile(orphans_percent, probs = 0.975))
  
  nig_quantiles$raw = nig_central$orphans
  nig_quantiles$percent = nig_central$orphans_percent
  nig_quantiles$country = "Nigeria"
  end_time <- Sys.time()
  print(end_time - start_time)
  
  nig$country = "Nigeria"
  saveRDS(nig, paste0("global_age_analysis_2021/data/age_outputs/Nigeria", month, ".RDS"))
  
  # Peru -----------------------------------------------------------------
  cat(sprintf("Running Peru ======\n"))
  cat(sprintf("Calculating central estimate\n"))
  if (month == "_diff"){
    get_diff_COVID19_deaths('global_age_analysis_2021/data/Peru/covid19_deaths.csv', 'global_age_analysis_2021/data/Peru/covid19_deaths_oct.csv', 
                    'global_age_analysis_2021/data/Peru/covid19_deaths_diff.csv')
  } else {
    process_peru_covid19()
  }
  process_number_children_peru(uncertainty = FALSE)
  per_central = process_orphans_age_peru(uncertainty = FALSE, month = month)
  
  cat(sprintf("Doing bootstrap\n"))
  per <- NULL
  start_time <- Sys.time()
  for (i in 1:n){
    sample_deaths_peru(month = month)
    process_number_children_peru(uncertainty = TRUE)
    per <- rbind(per, process_orphans_age_peru(uncertainty = TRUE, month = month))
  }
  
  cat(sprintf("Calculating uncertainty estimate\n"))
  per_quantiles <- per %>%
    group_by(category, gender) %>%
    summarise(li_raw = quantile(orphans, probs = 0.025),
              ui_raw = quantile(orphans, probs = 0.975),
              li_percent = quantile(orphans_percent, probs = 0.025),
              ui_percent = quantile(orphans_percent, probs = 0.975))
  
  per_quantiles$raw = per_central$orphans
  per_quantiles$percent = per_central$orphans_percent
  per_quantiles$country = "Peru"
  end_time <- Sys.time()
  print(end_time - start_time)
  
  per$country = "Peru"
  saveRDS(per, paste0("global_age_analysis_2021/data/age_outputs/Peru", month, ".RDS"))
  
  # Philippines -----------------------------------------------------------------
  cat(sprintf("Running Philippines ======\n"))
  cat(sprintf("Calculating central estimate\n"))
  if (month == "_diff"){
    get_diff_COVID19_deaths('global_age_analysis_2021/data/Philippines/covid19_deaths.csv', 'global_age_analysis_2021/data/Philippines/covid19_deaths_oct.csv', 
                            'global_age_analysis_2021/data/Philippines/covid19_deaths_diff.csv')
  } else {
    #process_philippines() <- data file too large
  }
  process_number_children_philippines(uncertainty = FALSE)
  phi_central = process_orphans_age_philippines(uncertainty = FALSE, month = month)
  
  cat(sprintf("Doing bootstrap\n"))
  phi <- NULL
  start_time <- Sys.time()
  for (i in 1:n){
    sample_deaths_philippines(month = month)
    process_number_children_philippines(uncertainty = TRUE)
    phi <- rbind(phi, process_orphans_age_philippines(uncertainty = TRUE, month = month))
  }
  
  cat(sprintf("Calculating uncertainty estimate\n"))
  phi_quantiles <- phi %>%
    group_by(category, gender) %>%
    summarise(li_raw = quantile(orphans, probs = 0.025),
              ui_raw = quantile(orphans, probs = 0.975),
              li_percent = quantile(orphans_percent, probs = 0.025),
              ui_percent = quantile(orphans_percent, probs = 0.975))
  
  phi_quantiles$raw = phi_central$orphans
  phi_quantiles$percent = phi_central$orphans_percent
  phi_quantiles$country = "Philippines"
  end_time <- Sys.time()
  print(end_time - start_time)
  
  phi$country = "Philippines"
  saveRDS(phi, paste0("global_age_analysis_2021/data/age_outputs/Philippines", month, ".RDS"))
  
  # Poland -----------------------------------------------------------------
  cat(sprintf("Running Poland ======\n"))
  cat(sprintf("Calculating central estimate\n"))
  if (month == "_diff"){
    get_diff_death('global_age_analysis_2021/data/Poland/poland_all.csv', 'global_age_analysis_2021/data/Poland/poland_all_oct.csv', 
                            'global_age_analysis_2021/data/Poland/poland_all_diff.csv')
  } else {
    process_poland_covid19()
  }
  process_number_children_poland(uncertainty = FALSE)
  pol_central = process_orphans_age_poland(uncertainty = FALSE, month = month)
  
  cat(sprintf("Doing bootstrap\n"))
  pol <- NULL
  start_time <- Sys.time()
  for (i in 1:n){
    sample_deaths_poland(month = month)
    process_number_children_poland(uncertainty = TRUE)
    pol <- rbind(pol, process_orphans_age_poland(uncertainty = TRUE, month = month))
  }
  
  cat(sprintf("Calculating uncertainty estimate\n"))
  pol_quantiles <- pol %>%
    group_by(category, gender) %>%
    summarise(li_raw = quantile(orphans, probs = 0.025),
              ui_raw = quantile(orphans, probs = 0.975),
              li_percent = quantile(orphans_percent, probs = 0.025),
              ui_percent = quantile(orphans_percent, probs = 0.975))
  
  pol_quantiles$raw = pol_central$orphans
  pol_quantiles$percent = pol_central$orphans_percent
  pol_quantiles$country = "Poland"
  end_time <- Sys.time()
  print(end_time - start_time)
  
  pol$country = "Poland"
  
  saveRDS(pol, paste0("global_age_analysis_2021/data/age_outputs/Poland", month, ".RDS"))
  
  # Russia
  # Not doing russia
  
  # Spain -----------------------------------------------------------------
  cat(sprintf("Running Spain ======\n"))
  cat(sprintf("Calculating central estimate\n"))
  if (month == "_diff"){
    get_diff_deaths('global_age_analysis_2021/data/Spain/spain_all.csv', 'global_age_analysis_2021/data/Spain/spain_all_oct.csv', 
                   'global_age_analysis_2021/data/Spain/spain_all_diff.csv')
  } else {
    process_spain()
  }
  process_number_children_spain(uncertainty = FALSE)
  spa_central = process_orphans_age_spain(uncertainty = FALSE, month = month)
  
  cat(sprintf("Doing bootstrap\n"))
  spa <- NULL
  start_time <- Sys.time()
  for (i in 1:n){
    sample_deaths_spain(month = month)
    process_number_children_spain(uncertainty = TRUE)
    spa <- rbind(spa, process_orphans_age_spain(uncertainty = TRUE, month = month))
  }
  
  cat(sprintf("Calculating uncertainty estimate\n"))
  spa_quantiles <- spa %>%
    group_by(category, gender) %>%
    summarise(li_raw = quantile(orphans, probs = 0.025),
              ui_raw = quantile(orphans, probs = 0.975),
              li_percent = quantile(orphans_percent, probs = 0.025),
              ui_percent = quantile(orphans_percent, probs = 0.975))
  
  spa_quantiles$raw = spa_central$orphans
  spa_quantiles$percent = spa_central$orphans_percent
  spa_quantiles$country = "Spain"
  end_time <- Sys.time()
  print(end_time - start_time)
  
  spa$country = "Spain"
  saveRDS(spa, paste0("global_age_analysis_2021/data/age_outputs/Spain", month, ".RDS"))
  
  # South Africa -----------------------------------------------------------------
  cat(sprintf("Running South Africa ======\n"))
  cat(sprintf("Calculating central estimate\n"))
  if (month == "_diff"){
    get_diff_COVID19_deaths('global_age_analysis_2021/data/SouthAfrica/covid19_deaths.csv', 'global_age_analysis_2021/data/SouthAfrica/covid19_deaths_oct.csv', 
                            'global_age_analysis_2021/data/SouthAfrica/covid19_deaths_diff.csv', "South Africa")
  } else {
    process_sa_covid19()
  }
  process_number_children_south_africa(uncertainty = FALSE)
  sa_central = process_orphans_age_southafrica(uncertainty = FALSE, month = month)
  
  cat(sprintf("Doing bootstrap\n"))
  sa <- NULL
  start_time <- Sys.time()
  for (i in 1:n){
    sample_deaths_south_africa(month = month)
    process_number_children_south_africa(uncertainty = TRUE)
    sa <- rbind(sa, process_orphans_age_southafrica(uncertainty = TRUE, month = month))
  }
  
  cat(sprintf("Calculating uncertainty estimate\n"))
  sa_quantiles <- sa %>%
    group_by(category, gender) %>%
    summarise(li_raw = quantile(orphans, probs = 0.025),
              ui_raw = quantile(orphans, probs = 0.975),
              li_percent = quantile(orphans_percent, probs = 0.025),
              ui_percent = quantile(orphans_percent, probs = 0.975))
  
  sa_quantiles$raw = sa_central$orphans
  sa_quantiles$percent = sa_central$orphans_percent
  sa_quantiles$country = "South Africa"
  end_time <- Sys.time()
  print(end_time - start_time)
  
  sa$country = "South Africa"
  saveRDS(sa, paste0("global_age_analysis_2021/data/age_outputs/SouthAfrica", month, ".RDS"))
  
  # USA -----------------------------------------------------------------
  cat(sprintf("Running USA ======\n"))
  cat(sprintf("Calculating central estimate\n"))
  if (month == "_diff"){
    get_diff_deaths('global_age_analysis_2021/data/USA/usa_all.csv', 'global_age_analysis_2021/data/USA/usa_all_oct.csv', 
                    'global_age_analysis_2021/data/USA/usa_all_diff.csv')
  } else {
    process_usa()
  }
  process_number_children_usa(uncertainty = FALSE)
  usa_central = process_orphans_age_usa(uncertainty = FALSE, month = month)
  
  cat(sprintf("Doing bootstrap\n"))
  usa <- NULL
  start_time <- Sys.time()
  for (i in 1:n){
    sample_deaths_usa(month = month)
    process_number_children_usa(uncertainty = TRUE)
    usa <- rbind(usa, process_orphans_age_usa(uncertainty = TRUE, month = month))
  }
  
  cat(sprintf("Calculating uncertainty estimate\n"))
  usa_quantiles <- usa %>%
    group_by(category, gender) %>%
    summarise(li_raw = quantile(orphans, probs = 0.025),
              ui_raw = quantile(orphans, probs = 0.975),
              li_percent = quantile(orphans_percent, probs = 0.025),
              ui_percent = quantile(orphans_percent, probs = 0.975))
  
  usa_quantiles$raw = usa_central$orphans
  usa_quantiles$percent = usa_central$orphans_percent
  usa_quantiles$country = "USA"
  end_time <- Sys.time()
  print(end_time - start_time)
  
  usa$country = "USA"
  saveRDS(usa, paste0("global_age_analysis_2021/data/age_outputs/USA", month, ".RDS"))
  
  # Zimbabwe -----------------------------------------------------------------
  cat(sprintf("Running Zimbabwe ======\n"))
  cat(sprintf("Calculating central estimate\n"))
  if (month == "_diff"){
    get_diff_deaths('global_age_analysis_2021/data/Zimbabwe/covid19_deaths.csv', 'global_age_analysis_2021/data/Zimbabwe/covid19_deaths_oct.csv', 
                            'global_age_analysis_2021/data/Zimbabwe/covid19_deaths_diff.csv')
  } else {
    process_zimbabwe()
  }
  process_number_children_zimbabwe(uncertainty = FALSE)
  zim_central = process_orphans_age_zimbabwe(uncertainty = FALSE, month = month)
  
  cat(sprintf("Doing bootstrap\n"))
  zim <- NULL
  start_time <- Sys.time()
  for (i in 1:n){
    sample_deaths_zimbabwe(month = month)
    process_number_children_zimbabwe(uncertainty = TRUE)
    zim <- rbind(zim, process_orphans_age_zimbabwe(uncertainty = TRUE, month = month))
  }
  
  cat(sprintf("Calculating uncertainty estimate\n"))
  zim_quantiles <- zim %>%
    group_by(category, gender) %>%
    summarise(li_raw = quantile(orphans, probs = 0.025),
              ui_raw = quantile(orphans, probs = 0.975),
              li_percent = quantile(orphans_percent, probs = 0.025),
              ui_percent = quantile(orphans_percent, probs = 0.975))
  
  zim_quantiles$raw = zim_central$orphans
  zim_quantiles$percent = zim_central$orphans_percent
  zim_quantiles$country = "Zimbabwe"
  end_time <- Sys.time()
  print(end_time - start_time)
  
  zim$country = "Zimbabwe"
  saveRDS(zim, paste0("global_age_analysis_2021/data/age_outputs/Zimbabwe", month, ".RDS"))
  
  
  # Formatting the data ----------------------------------------------
  percentages =  rbind(arg_quantiles, bra_quantiles, col_quantiles, eng_quantiles, fra_quantiles,
                       ger_quantiles, ind_quantiles, ira_quantiles, ita_quantiles, ken_quantiles, 
                       mal_quantiles, mex_quantiles, nig_quantiles, per_quantiles, phi_quantiles, 
                       pol_quantiles, sa_quantiles, spa_quantiles, usa_quantiles, zim_quantiles)
  
  samples =  rbind(arg, bra, col, eng, fra, ger, ind, ira, ita, ken, mal, mex, nig, per, phi, 
                   pol, sa, spa, usa, zim)
  
  saveRDS(percentages, paste0("global_age_analysis_2021/data/age_outputs/age_data", month, ".RDS"))
  saveRDS(samples, paste0("global_age_analysis_2021/data/age_outputs/samples_age_data", month, ".RDS"))
  
  # Including the excess deaths ----------------------------------------------
  multipliers <- read.csv("global_age_analysis_2021/data/multipliers.csv", header = FALSE)
  names(multipliers) <- c("country", "multiplier")
  multipliers <- multipliers[which(! multipliers$country %in% c("England & Wales", "France", "Italy", "Peru",
                                                                "Russian Federation", "Spain", "United States of America")),]
  
  percentages <- left_join(percentages, multipliers, by = "country")
  percentages$raw <- ifelse(!is.na(percentages$multiplier), percentages$raw * percentages$multiplier,  percentages$raw)
  percentages$li_raw <- ifelse(!is.na(percentages$multiplier), percentages$li_raw * percentages$multiplier,  percentages$li_raw)
  percentages$ui_raw <- ifelse(!is.na(percentages$multiplier), percentages$ui_raw * percentages$multiplier,  percentages$ui_raw)
  
  samples <- left_join(samples, multipliers, by = "country")
  samples$orphans <- ifelse(!is.na(samples$multiplier), samples$orphans * samples$multiplier,  samples$orphans)
  
  percentages <- select(percentages, -multiplier)
  samples <- select(samples, -multiplier)
  
  saveRDS(percentages, paste0("global_age_analysis_2021/data/age_outputs/age_data_scaled", month, ".RDS"))
  saveRDS(samples, paste0("global_age_analysis_2021/data/age_outputs/samples_age_data_scaled", month, ".RDS"))
  
}

run_age_analysis(month = "")
run_age_analysis(month = "_oct")
run_age_analysis(month = "_diff")


