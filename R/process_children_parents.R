# Functions for processing parents

# This function calculates the number of children per different aged adult for women
process_children_all = function(country, is_child_mortality_needed, data_f){
  # construct a matrix, the rownames represent the 
  children = matrix(rep(0, 100*18), nrow = 100)
  names(children) = paste0(seq(0:17), 'years')
  
  # A 66 year old would have had 1 year in 45-49 age year category in 2003,now 17 years old
  children[66,18] <- (data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2003)]) 
  #* (1- child$mortality[which(child$age == '15 years' & child$year == 2003) ]) 
  
  # A 65 year old would have had 2 year in 45-49 age year category in 2003 and 2004
  children[65,18:17] <- data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2003,2004))]
  #*(1- child$mortality[which(child$age == '15 years' & child$year %in% seq(2003, 2004)) ]) 
  
  # A 64 year old would have had 3 year in 45-49 age year category in 2003 - 2005
  children[64, 18:16] <- data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2003, 2005))]
  #*(1- child$mortality[which(child$age == '15 years' & child$year %in% seq(2003, 2005)) ]) 
  
  # A 63 year old would have had 4 year in 45-49 age year category in 2003 - 2006
  children[63, 18:15] <- (data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2003, 2006))])
  #*
  #  (1- c(child$mortality[which(child$age == '15 years' & child$year %in% seq(2003, 2005))], 
  #        child$mortality[which(child$age == '10-14 years' & child$year == 2006)])) 
  
  # A 62 year old would have had five year 
  # in 45-49 age year category in 2003 - 2007
  children[62, 18:14] <- (data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2003, 2007))])
  #*
  #  (1- c(child$mortality[which(child$age == '15 years' & child$year %in% seq(2003, 2005))], 
  #        child$mortality[which(child$age == '10-14 years' & child$year %in% seq(2006, 2007))])) 
  
  # A 61 year old would have had 1 year in 40-44 year group in 03 and five year 
  # in 45-49 age year category in 2004 - 2006
  children[61, 18:13] <- c(data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2003)], 
                           data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2004, 2008))])
  #*
  #  (1- c(child$mortality[which(child$age == '15 years' & child$year %in% seq(2003, 2005))], 
  #        child$mortality[which(child$age == '10-14 years' & child$year %in% seq(2006, 2008))])) 
  
  children[60, 18:12] <- c(data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date %in% seq(2003, 2004))],
                           data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2005, 2009))])
  #####
  
  children[59, 18:11] <- c(data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date %in% seq(2003, 2005))], 
                           data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2006, 2010))])
  
  
  children[58, 18:10] <- c(data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date %in% seq(2003, 2006))],
                           data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2007, 2011))])
  
  
  children[57, 18:9] <- c(data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date %in% seq(2003, 2007))], 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2008, 2012))])
  
  
  children[56, 18:8] <- c(data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2003)], 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date %in% seq(2004, 2008))],
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2009, 2013))])
  
  
  children[55, 18:7] <- c(data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date %in% seq(2003, 2004))], 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date %in% seq(2005, 2009))],
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2010, 2014))])
  
  children[54, 18:6] <- c(data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date %in% seq(2003, 2005))], 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date %in% seq(2006, 2010))],
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2011, 2015))])
  
  children[53, 18:5] <- c(data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date %in% seq(2003, 2006))],
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date %in% seq(2007, 2011))], 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2012, 2016))])
  
  children[52, 18:4] <- c(data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date %in% seq(2003, 2007))], 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date %in% seq(2008, 2012))],
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2013, 2017))])
  
  children[51, 18:3] <- c(data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2003)], 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date %in% seq(2004, 2008))],
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date %in% seq(2009, 2013))],
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2014, 2018))])
  
  children[50, 18:2] <- c(data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date %in% seq(2003,2004))],
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date %in% seq(2005, 2009))],
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date %in% seq(2010, 2014))], 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2015, 2019))])
  
  children[49, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2003)], 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2004)] ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2005)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2006)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2007)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2008)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2009)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2010)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2011)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2012)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2013)] ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2014)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2015)] , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2016)] , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2017)] , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2018)] , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2019)] , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2019)]) 
  
  children[48, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2003)],
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2004)] ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2005)] , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2006)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2007)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2008)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2009)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2010)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2011)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2012)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2013)] ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2014)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2015)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2016)] , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2017)] , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2018)] , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2019)] , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2019)])
  
  children[47, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2003)],
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2004)] ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2005)] , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2006)] , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2007)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2008)] ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2009)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2010)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2011)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2012)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2013)] ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2014)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2015)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2016)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2017)] , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2018)] , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2019)] , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2019)])
  
  children[46, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2003)],
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2004)] ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2005)] , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2006)] , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2007)] , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2008)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2009)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2010)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2011)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2012)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2013)] ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2014)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2015)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2016)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2017)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2018)] , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2019)] , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2019)])
  
  children[45, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2003)],
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2004)] ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2005)] , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2006)] , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2007)] , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2008)] , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2009)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2010)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2011)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2012)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2013)] ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2014)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2015)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2016)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2017)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2018)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2019)] , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2019)])
  
  children[44, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2003)] ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2004)] ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2005)] , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2006)] , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2007)] , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2008)] , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2009)] , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2010)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2011)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2012)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2013)] ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2014)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2015)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2016)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2017)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2018)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2019)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2019)])
  
  children[43, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2003)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2004)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2005)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2006)] , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2007)] , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2008)] , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2009)] , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2010)] , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2011)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2012)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2013)] ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2014)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2015)] , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2016)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2017)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2018)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2019)] , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2019)])
  
  children[42,18:1] <- c(data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2003)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2004)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2005)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2006)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2007)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2008)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2009)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2010)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2011)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2012)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2013)] ,
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2014)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2015)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2016)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2017)] , 
                         data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2018)] , 
                         data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2019)] , 
                         data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2019)])
  
  children[41,18:1] <- c(data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2003)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2004)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2005)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2006)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2007)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2008)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2009)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2010)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2011)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2012)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2013)] ,
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2014)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2015)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2016)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2017)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2018)] , 
                         data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2019)] , 
                         data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2019)])
  
  children[40,18:1] <- c(data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2003)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2004)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2005)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2006)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2007)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2008)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2009)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2010)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2011)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2012)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2013)] ,
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2014)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2015)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2016)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2017)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2018)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2019)] , 
                         data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2019)])
  
  children[39,18:1] <- c(data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2003)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2004)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2005)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2006)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2007)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2008)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2009)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2010)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2011)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2012)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2013)] ,
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2014)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2015)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2016)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2017)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2018)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2019)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2019)])
  
  children[38,18:1] <- c(data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2003)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2004)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2005)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2006)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2007)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2008)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2009)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2010)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2011)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2012)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2013)] ,
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2014)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2015)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2016)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2017)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2018)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2019)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2019)])
  
  children[37,18:1] <- c(data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2003)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2004)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2005)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2006)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2007)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2008)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2009)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2010)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2011)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2012)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2013)] ,
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2014)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2015)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2016)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2017)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2018)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2019)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2019)])
  
  children[36,18:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2003)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2004)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2005)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2006)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2007)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2008)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2009)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2010)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2011)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2012)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2013)] ,
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2014)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2015)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2016)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2017)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2018)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2019)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2019)])
  
  children[35,18:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2003)] , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2004)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2005)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2006)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2007)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2008)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2009)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2010)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2011)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2012)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2013)] ,
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2014)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2015)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2016)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2017)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2018)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2019)] , 
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2019)])
  
  children[34,18:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2003)] , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2004)] , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2005)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2006)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2007)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2008)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2009)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2010)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2011)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2012)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2013)] ,
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2014)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2015)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2016)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2017)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2018)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2019)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2019)])
  
  children[33,18:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2003)] , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2004)] , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2005)] , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2006)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2007)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2008)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2009)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2010)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2011)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2012)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2013)] ,
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2014)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2015)] , 
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2016)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2017)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2018)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2019)] , 
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2019)])
  
  children[32,18:1] <- c( data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2003)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2004)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2005)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2006)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2007)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2008)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2009)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2010)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2011)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2012)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2013)] ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2014)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2015)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2016)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2017)] , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2018)] , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2019)] , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2019)])
  
  children[31,17:1] <- c( data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2004)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2005)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2006)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2007)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2008)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2009)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2010)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2011)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2012)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2013)] ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2014)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2015)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2016)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2017)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)] , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2019)] , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2019)])
  
  children[30,16:1] <- c( data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2005)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2006)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2007)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2008)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2009)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2010)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2011)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2012)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2013)] ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2014)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2015)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2016)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2017)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2019)] , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2019)])
  
  children[29,15:1] <- c( data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2006)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2007)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2008)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2009)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2010)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2011)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2012)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2013)] ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2014)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2015)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2016)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2017)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2019)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2019)])
  
  children[28,14:1] <- c( data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2007)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2008)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2009)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2010)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2011)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2012)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2013)] ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2014)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2015)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2016)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2017)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2019)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2019)])
  
  children[27,13:1] <- c( data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2008)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2009)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2010)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2011)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2012)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2013)] ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2014)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2015)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2016)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2017)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2019)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2019)])
  
  children[26,12:1] <- c( data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2009)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2010)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2011)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2012)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2013)] ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2014)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2015)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2016)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2017)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2019)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2019)])
  
  children[25,11:1] <- c( data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2010)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2011)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2012)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2013)] ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2014)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2015)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2016)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2017)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2019)] , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2019)])
  
  children[24,10:1] <- c( data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2011)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2012)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2013)] ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2014)] , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2015)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2016)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2017)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2019)] , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2019)])
  
  children[23,9:1] <- c( data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2012)] , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2013)] ,
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2014)] , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2015)] , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2016)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2017)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2019)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2019)])
  
  children[22,8:1] <- c( data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2013)] ,
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2014)] , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2015)] , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2016)] , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2017)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2019)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2019)])
  
  children[21,7:1] <- c( data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2014)] , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2015)] , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2016)] , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2017)] , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2018)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2019)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2019)])
  
  children[20,6:1] <- c( data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2015)] , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2016)] , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2017)] , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2018)] , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2019)] , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2019)])
  
  children[19,5:1] <- c( data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2016)] , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2017)] , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2018)] , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2019)] , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2019)])
  
  children[18,4:1] <- c( data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2017)] , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2018)] , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2019)] , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2019)])
  
  children[17,3:1] <- c( data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2018)] , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2019)] , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2019)])
  
  children[16,2:1] <- c( data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2019)] , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2019)])
  
  children[15,1] <- data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2019)]
  
  
  plot(apply(children, 1, sum), xlab = "Age of mother", ylab = "Number of children")
  children = as.data.frame(children)
  names(children) = paste0(seq(0:17)-1, ' years')
  write_csv(path = paste0('data/', country, '/child_raw_f.csv'), children)
  
  plot_c = as.data.frame(as.numeric(as.character(unlist(children))))
  plot_c$mother_age = rep(1:100,18)
  plot_c$child_age =sort(rep(seq(18)-1, 100))
  setnames(plot_c, 1, 'prob')
  
  
  ###########  get the nb of children (considering the child mortality or not) ################
  if (is_child_mortality_needed){
    child_m_matrix = read.csv(paste0('data/', country,'/child_mortality_rate.csv'))
    child_and_m = as.matrix(children) * (1-as.matrix(child_m_matrix))
    child_and_m = as.data.frame(child_and_m)
    write_csv(file = paste0('data/', country, '/child_all_f.csv'), child_and_m)
    
    plot_c_and_m = as.data.frame(as.numeric(as.character(unlist(child_and_m))))
    plot_c_and_m$mother_age = rep(1:100,18)
    plot_c_and_m$child_age = sort(rep(seq(18)-1, 100))
    setnames(plot_c_and_m, 1, 'prob')
    
  } else {
    child_and_m = children
    plot_c_and_m = copy(plot_c)
    write_csv(file = paste0('data/', country, '/child_all_f.csv'), child_and_m)
  }
  
  plot_c_and_m$gender = 'female'
  write_csv(file = paste0('data/', country, '/child_all_list_f.csv'), plot_c_and_m) 
  setnames(plot_c_and_m, 'mother_age', 'parents_age')
  plott = read.csv(paste0('data/', country, '/child_all_list_m.csv')) 
  setnames(plott, 'father_age', 'parents_age')
  plot_all = rbind(plot_c_and_m, plott)
  write_csv(file = paste0('data/', country, '/child_all_list_both_sex.csv'), plot_all) 
  
  ###  plot female and male
  ddf = as.data.frame(apply(child_and_m, 1, sum))
  names(ddf) = 'children'
  ddf$gender = 'female'
  ddf$age = 1:100
  write_csv(file = paste0('data/', country, '/children_f.csv'), ddf)
  
  ddf = read.csv(paste0('data/', country, '/children_f.csv'))
  ddf_2 = read.csv(paste0('data/', country,'/children_m.csv'))
  ddf = rbind(ddf, ddf_2)
  p = ggplot(ddf) +
    geom_point(aes(x = age, y = children, colour = gender)) + 
    theme_bw()+
    xlab( 'Age of Parent') +
    ylab('Number of Children')+
    guides(color=guide_legend(title="Sex of Parent"))
  ggsave(filename = paste0("figures/children_", tolower(country), ".pdf"), p, width = 6, height = 5)
  write_csv(file = paste0('data/', country,'/children.csv'), ddf)
}

process_children_father_england_wales = function(data_f){
  ##
  children = matrix(rep(0, 100*18), nrow = 100)
  names(children) = paste0(seq(0:17), 'years')
  
  # A 81 year old would have had 1 year in 60+ age year category in 2003
  children[81, 18] <- data_f$fertility_rate[which(data_f$age == "60-64" & data_f$date == 2003)]
  # A 80 year old would have had three year in 60+ age year category in 2002 and 2005
  children[80, 18:17] <- data_f$fertility_rate[which(data_f$age == "60-64" & data_f$date %in% seq(2003,2004))] 
  # A 79 year old would have had four year in 60-64 age year category in 2002 and 2005
  children[79, 18:16] <- data_f$fertility_rate[which(data_f$age == "60-64" & data_f$date %in% seq(2003,2005))] 
  # A 78 year old would have had 5 year in 60-64 age year category in 2002 and 2006
  children[78, 18:15] <- data_f$fertility_rate[which(data_f$age == "60-64" & data_f$date %in% seq(2003,2006))] 
  # A 77 year old would have had 5 year in 60-64 age year category in 2003 and 2007, one year in 55-59 in 2003
  
  children[77, 18:14] <- data_f$fertility_rate[which(data_f$age == "60-64" & data_f$date %in% seq(2003,2007))]
  
  children[76, 18:13] <- c(data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date == 2003)],
                           data_f$fertility_rate[which(data_f$age == "60-64" & data_f$date %in% seq(2004,2008))])   
  
  children[75, 18:12] <-  c(data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date%in% seq(2003, 2004))] ,
                            data_f$fertility_rate[which(data_f$age == "60-64" & data_f$date %in% seq(2005,2009))])
  
  children[74, 18:11] <-   c(data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date%in% seq(2003, 2005))],
                             data_f$fertility_rate[which(data_f$age == "60-64" & data_f$date %in% seq(2006,2010))])   
  
  children[73, 18:10] <- c(  data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date%in% seq(2003, 2006))] ,
                             data_f$fertility_rate[which(data_f$age == "60-64" & data_f$date %in% seq(2007,2011))])   
  
  children[72, 18:9] <- c(data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date%in% seq(2003, 2007))] ,
                          data_f$fertility_rate[which(data_f$age == "60-64" & data_f$date %in% seq(2008,2012))]) 
  
  children[71, 18:8] <- c( data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date == 2003)],
                           data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date%in% seq(2004, 2008))] ,
                           data_f$fertility_rate[which(data_f$age == "60-64" & data_f$date %in% seq(2009,2013))])   
  
  children[70, 18:7] <- c( data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2003,2004))] ,
                           data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date%in% seq(2005, 2009))] ,
                           data_f$fertility_rate[which(data_f$age == "60-64" & data_f$date %in% seq(2010,2014))])   
  
  children[69, 18:6] <- c( data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2003,2005))] ,
                           data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date%in% seq(2006, 2010))] ,
                           data_f$fertility_rate[which(data_f$age == "60-64" & data_f$date %in% seq(2011,2015))]) 
  
  children[68, 18:5] <- c( data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2003,2006))] ,
                           data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date%in% seq(2007, 2011))] ,
                           data_f$fertility_rate[which(data_f$age == "60-64" & data_f$date %in% seq(2012,2016))]) 
  
  children[67, 18:4] <- c(data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2003,2007))]  ,
                          data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date%in% seq(2008, 2012))] ,
                          data_f$fertility_rate[which(data_f$age == "60-64" & data_f$date %in% seq(2013,2017))]) 
  
  children[66, 18:3] <- c( data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2003)]  ,
                           data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2004,2008))] ,
                           data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date%in% seq(2009, 2013))] ,
                           data_f$fertility_rate[which(data_f$age == "60-64" & data_f$date %in% seq(2014,2018))])
  
  children[65, 18:2] <- c( data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date%in% seq(2003,2004))]  ,
                           data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2005,2009))]  ,
                           data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date%in% seq(2010, 2014))] ,
                           data_f$fertility_rate[which(data_f$age == "60-64" & data_f$date %in% seq(2015,2019))])
  
  children[64, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date%in% seq(2003,2005))] ,
                           data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2006,2010))] ,
                           data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date%in% seq(2011, 2015))] ,
                           data_f$fertility_rate[which(data_f$age == "60-64" & data_f$date %in% seq(2016,2020))])
  
  
  children[63, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date%in% seq(2003,2006))] ,
                           data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2007,2011))]  ,
                           data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date%in% seq(2012, 2016))] ,
                           data_f$fertility_rate[which(data_f$age == "60-64" & data_f$date %in% seq(2017,2020))])
  
  # A 62 year old would havefive year 
  # in 45-49 age year category in 2003 - 2007
  children[62, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2003, 2007))],
                          data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2008,2012))]  ,
                          data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date%in% seq(2013, 2017))] ,
                          data_f$fertility_rate[which(data_f$age == "60-64" & data_f$date %in% seq(2018,2020))])
  # A 61 year old would have had 1 year in 40-44 year group in 2003 and five year
  # in 45-49 age year category in 2004 - 2008, 5 years in 50-54 age year category in 09-13
  children[61, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2003)], 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2004, 2008))],
                          data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2009,2013))]  ,
                          data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date%in% seq(2014, 2018))] ,
                          data_f$fertility_rate[which(data_f$age == "60-64" & data_f$date %in% seq(2019,2020))])
  
  children[60, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date %in% seq(2003, 2004))]  , 
                           data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2005, 2009))],
                           data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2010,2014))]  ,
                           data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date%in% seq(2015, 2019))] ,
                           data_f$fertility_rate[which(data_f$age == "60-64" & data_f$date == 2020)])
  
  
  children[59, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date %in% seq(2003, 2005))]  , 
                           data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2006, 2010))],
                           data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2011,2015))]  ,
                           data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date%in% seq(2016, 2020))])
  
  
  children[58, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date %in% seq(2003, 2006))], 
                           data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2007, 2011))],
                           data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2012,2016))]  ,
                           data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date%in% seq(2017, 2020))])
  
  
  children[57, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date %in% seq(2003, 2007))] , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2008, 2012))],
                          data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2013,2017))]  ,
                          data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date%in% seq(2018, 2020))])
  
  
  children[56, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2003)] , 
                           data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date %in% seq(2004, 2008))] , 
                           data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2009, 2013))],
                           data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2014,2018))]  ,
                           data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date%in% seq(2019, 2020))])
  
  children[55, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date %in% seq(2003, 2004))] , 
                           data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date %in% seq(2005, 2009))] , 
                           data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2010, 2014))],
                           data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2015,2019))]  ,
                           data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date == 2020)])
  
  children[54, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2015)],
                          data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2016,2020))])
  
  children[53, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2016)], 
                          data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2017,2020))])
  
  
  children[52, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2017)],
                          data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2018,2020))])
  
  
  children[51, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2018)],
                          data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2019,2020))])
  
  
  children[50, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2019)],
                          data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date == 2020)])
  
  
  children[49, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2020)])
  
  children[48, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2020)])
  
  children[47, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2020)])
  
  children[46, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2020)])
  
  children[45, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2020)])
  
  children[44, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2020)])
  
  children[43, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2020)])
  
  children[42, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2020)])
  
  children[41, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2020)])
  
  children[40, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2020)])
  
  children[39, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2020)])
  
  children[38, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2020)])
  
  children[37, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2020)])
  
  children[36, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2020)])
  
  children[35, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2020)])
  
  children[34, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2020)])
  
  children[33, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2020)])
  
  children[32, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2020)])
  
  children[31, 17:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2020)])
  
  children[30, 16:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2020)])
  
  children[29, 15:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2020)])
  
  children[28, 14:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)])
  
  children[27, 13:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)])
  
  children[26, 12:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)])
  
  children[25, 11:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)])
  
  children[24, 10:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)])
  
  children[23, 9:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2012)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2013)]  ,
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2014)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2015)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2016)]  , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2017)]  , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)]  , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)]  , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)])
  
  children[22, 8:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2013)]  ,
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2014)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2015)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2016)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2017)]  , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)]  , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)]  , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)])
  
  children[21, 7:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2014)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2015)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2016)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2017)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2018)]  , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)]  , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)])
  
  children[20, 6:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2015)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2016)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2017)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2018)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2018)]  , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)])
  
  children[19, 5:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2016)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2017)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2018)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2018)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2018)])
  
  children[18, 4:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2017)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2018)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2019)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2020)])
  
  children[17, 3:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2018)] , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2019)], 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2020)])
  
  children[16,2:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2019)]  , 
                        data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2020)])
  
  children[15,1] <- data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2020)]
  plot(apply(children, 1, sum), xlab = "Age of father", ylab = "Number of children")
  children = as.data.frame(children)
  names(children) = paste0(seq(0:17)-1, ' years')
  
  write_csv(path = paste0('data/UK/child_raw_m.csv'), children)
  plot_c = as.data.frame(as.numeric(as.character(unlist(children))))
  plot_c$father_age = rep(1:100,18)
  plot_c$child_age =sort(rep(seq(18)-1, 100))
  setnames(plot_c, 1, 'prob')
}

# dhs
process_children_father_80_plus = function(country, data_f){
  ##
  children = matrix(rep(0, 100*18), nrow = 100)
  names(children) = paste0(seq(0:17), 'years')
  
  # A 100 year old would have 18 years in 80+ in 2003 - 2020
  children[100, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "80+" & data_f$date%in% seq(2003,2020))])
  children[99, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "80+" & data_f$date%in% seq(2003,2020))])
  children[98, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "80+" & data_f$date%in% seq(2003,2020))])
  children[97, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "80+" & data_f$date%in% seq(2003,2020))])
  
  children[96, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "75-79" & data_f$date == 2003)],
                          data_f$fertility_rate[which(data_f$age == "80+" & data_f$date %in% seq(2004,2020))]) 
  
  children[95, 18:1] <-  c(data_f$fertility_rate[which(data_f$age == "75-79" & data_f$date%in% seq(2003, 2004))] ,
                           data_f$fertility_rate[which(data_f$age == "80+" & data_f$date %in% seq(2005,2020))])
  
  children[94, 18:1] <-   c(data_f$fertility_rate[which(data_f$age == "75-79" & data_f$date%in% seq(2003, 2005))],
                            data_f$fertility_rate[which(data_f$age == "80+" & data_f$date %in% seq(2006,2020))])   
  
  children[93, 18:1] <- c(  data_f$fertility_rate[which(data_f$age == "75-79" & data_f$date%in% seq(2003, 2006))] ,
                            data_f$fertility_rate[which(data_f$age == "80+" & data_f$date %in% seq(2007,2020))])   
  
  children[92, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "75-79" & data_f$date%in% seq(2003, 2007))] ,
                          data_f$fertility_rate[which(data_f$age == "80+" & data_f$date %in% seq(2008,2020))]) 
  
  children[91, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "70-74" & data_f$date == 2003)],
                           data_f$fertility_rate[which(data_f$age == "75-79" & data_f$date%in% seq(2004, 2008))] ,
                           data_f$fertility_rate[which(data_f$age == "80+" & data_f$date %in% seq(2009,2020))])   
  
  children[90, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "70-74" & data_f$date%in% seq(2003,2004))] ,
                           data_f$fertility_rate[which(data_f$age == "75-79" & data_f$date%in% seq(2005, 2009))] ,
                           data_f$fertility_rate[which(data_f$age == "80+" & data_f$date %in% seq(2010,2020))])   
  
  children[89, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "70-74" & data_f$date%in% seq(2003,2005))] ,
                           data_f$fertility_rate[which(data_f$age == "75-79" & data_f$date%in% seq(2006, 2010))] ,
                           data_f$fertility_rate[which(data_f$age == "80+" & data_f$date %in% seq(2011,2020))]) 
  
  children[88, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "70-74" & data_f$date%in% seq(2003,2006))] ,
                           data_f$fertility_rate[which(data_f$age == "75-79" & data_f$date%in% seq(2007, 2011))] ,
                           data_f$fertility_rate[which(data_f$age == "80+" & data_f$date %in% seq(2012,2020))]) 
  
  children[87, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "70-74" & data_f$date%in% seq(2003,2007))]  ,
                          data_f$fertility_rate[which(data_f$age == "75-79" & data_f$date%in% seq(2008, 2012))] ,
                          data_f$fertility_rate[which(data_f$age == "80+" & data_f$date %in% seq(2013,2020))]) 
  
  children[86, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "65-69" & data_f$date == 2003)]  ,
                           data_f$fertility_rate[which(data_f$age == "70-74" & data_f$date%in% seq(2004,2008))] ,
                           data_f$fertility_rate[which(data_f$age == "75-79" & data_f$date%in% seq(2009, 2013))] ,
                           data_f$fertility_rate[which(data_f$age == "80+" & data_f$date %in% seq(2014,2020))])
  
  children[85, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "65-69" & data_f$date%in% seq(2003,2004))]  ,
                           data_f$fertility_rate[which(data_f$age == "70-74" & data_f$date%in% seq(2005,2009))]  ,
                           data_f$fertility_rate[which(data_f$age == "75-79" & data_f$date%in% seq(2010, 2014))] ,
                           data_f$fertility_rate[which(data_f$age == "80+" & data_f$date %in% seq(2015,2020))])
  
  children[84, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "65-69" & data_f$date%in% seq(2003,2005))] ,
                           data_f$fertility_rate[which(data_f$age == "70-74" & data_f$date%in% seq(2006,2010))] ,
                           data_f$fertility_rate[which(data_f$age == "75-79" & data_f$date%in% seq(2011, 2015))] ,
                           data_f$fertility_rate[which(data_f$age == "80+" & data_f$date %in% seq(2016,2020))])
  
  
  children[83, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "65-69" & data_f$date%in% seq(2003,2006))] ,
                           data_f$fertility_rate[which(data_f$age == "70-74" & data_f$date%in% seq(2007,2011))]  ,
                           data_f$fertility_rate[which(data_f$age == "75-79" & data_f$date%in% seq(2012, 2016))] ,
                           data_f$fertility_rate[which(data_f$age == "80+" & data_f$date %in% seq(2017,2020))])
  
  children[82, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "65-69" & data_f$date %in% seq(2003, 2007))],
                          data_f$fertility_rate[which(data_f$age == "70-74" & data_f$date%in% seq(2008,2012))]  ,
                          data_f$fertility_rate[which(data_f$age == "75-79" & data_f$date%in% seq(2013, 2017))] ,
                          data_f$fertility_rate[which(data_f$age == "80+" & data_f$date %in% seq(2018,2020))])
  
  children[81, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "60-64" & data_f$date == 2003)], 
                          data_f$fertility_rate[which(data_f$age == "65-69" & data_f$date %in% seq(2004, 2008))],
                          data_f$fertility_rate[which(data_f$age == "70-74" & data_f$date%in% seq(2009,2013))]  ,
                          data_f$fertility_rate[which(data_f$age == "75-79" & data_f$date%in% seq(2014, 2018))] ,
                          data_f$fertility_rate[which(data_f$age == "80+" & data_f$date %in% seq(2019,2020))])
  
  children[80, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "60-64" & data_f$date %in% seq(2003, 2004))]  , 
                           data_f$fertility_rate[which(data_f$age == "65-69" & data_f$date %in% seq(2005, 2009))],
                           data_f$fertility_rate[which(data_f$age == "70-74" & data_f$date%in% seq(2010,2014))]  ,
                           data_f$fertility_rate[which(data_f$age == "75-79" & data_f$date%in% seq(2015, 2019))] ,
                           data_f$fertility_rate[which(data_f$age == "80+" & data_f$date == 2020)])
  
  
  children[79, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "60-64" & data_f$date %in% seq(2003, 2005))]  , 
                           data_f$fertility_rate[which(data_f$age == "65-69" & data_f$date %in% seq(2006, 2010))],
                           data_f$fertility_rate[which(data_f$age == "70-74" & data_f$date%in% seq(2011,2015))]  ,
                           data_f$fertility_rate[which(data_f$age == "75-79" & data_f$date%in% seq(2016, 2020))])
  
  
  children[78, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "60-64" & data_f$date %in% seq(2003, 2006))], 
                           data_f$fertility_rate[which(data_f$age == "65-69" & data_f$date %in% seq(2007, 2011))],
                           data_f$fertility_rate[which(data_f$age == "70-74" & data_f$date%in% seq(2012,2016))]  ,
                           data_f$fertility_rate[which(data_f$age == "75-79" & data_f$date%in% seq(2017, 2020))])
  
  
  children[77, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "60-64" & data_f$date %in% seq(2003, 2007))] , 
                          data_f$fertility_rate[which(data_f$age == "65-69" & data_f$date %in% seq(2008, 2012))],
                          data_f$fertility_rate[which(data_f$age == "70-74" & data_f$date%in% seq(2013,2017))]  ,
                          data_f$fertility_rate[which(data_f$age == "75-79" & data_f$date%in% seq(2018, 2020))])
  
  
  children[76, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date == 2003)] , 
                           data_f$fertility_rate[which(data_f$age == "60-64" & data_f$date %in% seq(2004, 2008))] , 
                           data_f$fertility_rate[which(data_f$age == "65-69" & data_f$date %in% seq(2009, 2013))],
                           data_f$fertility_rate[which(data_f$age == "70-74" & data_f$date%in% seq(2014,2018))]  ,
                           data_f$fertility_rate[which(data_f$age == "75-79" & data_f$date%in% seq(2019, 2020))])
  
  children[75, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date %in% seq(2003, 2004))] , 
                           data_f$fertility_rate[which(data_f$age == "60-64" & data_f$date %in% seq(2005, 2009))] , 
                           data_f$fertility_rate[which(data_f$age == "65-69" & data_f$date %in% seq(2010, 2014))],
                           data_f$fertility_rate[which(data_f$age == "70-74" & data_f$date%in% seq(2015,2019))]  ,
                           data_f$fertility_rate[which(data_f$age == "75-79" & data_f$date == 2020)])
  
  children[74, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "60-64" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "60-64" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "60-64" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "60-64" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "60-64" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "65-69" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "65-69" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "65-69" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "65-69" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "65-69" & data_f$date == 2015)],
                          data_f$fertility_rate[which(data_f$age == "70-74" & data_f$date%in% seq(2016,2020))])
  
  children[73, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date %in% seq(2003,2006))]  , 
                          data_f$fertility_rate[which(data_f$age == "60-64" & data_f$date %in% seq(2007,2011))],
                          data_f$fertility_rate[which(data_f$age == "65-69" & data_f$date %in% seq(2012, 2016))], 
                          data_f$fertility_rate[which(data_f$age == "70-74" & data_f$date%in% seq(2017,2020))])
  
  
  children[72, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date%in% seq(2003, 2007))] ,
                          data_f$fertility_rate[which(data_f$age == "60-64" & data_f$date %in% seq(2008,2012))],
                          data_f$fertility_rate[which(data_f$age == "65-69" & data_f$date %in% seq(2013, 2017))], 
                          data_f$fertility_rate[which(data_f$age == "70-74" & data_f$date%in% seq(2018,2020))]) 
  
  children[71, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date == 2003)],
                           data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date%in% seq(2004, 2008))] ,
                           data_f$fertility_rate[which(data_f$age == "60-64" & data_f$date %in% seq(2009,2013))],
                           data_f$fertility_rate[which(data_f$age == "65-69" & data_f$date %in% seq(2014, 2018))], 
                           data_f$fertility_rate[which(data_f$age == "70-74" & data_f$date%in% seq(2019,2020))])   
  
  children[70, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2003,2004))] ,
                           data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date%in% seq(2005, 2009))] ,
                           data_f$fertility_rate[which(data_f$age == "60-64" & data_f$date %in% seq(2010,2014))],
                           data_f$fertility_rate[which(data_f$age == "65-69" & data_f$date %in% seq(2015, 2019))], 
                           data_f$fertility_rate[which(data_f$age == "70-74" & data_f$date == 2020)]) 
  
  children[69, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2003,2005))] ,
                           data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date%in% seq(2006, 2010))] ,
                           data_f$fertility_rate[which(data_f$age == "60-64" & data_f$date %in% seq(2011,2015))],
                           data_f$fertility_rate[which(data_f$age == "65-69" & data_f$date %in% seq(2016, 2020))]) 
  
  children[68, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2003,2006))] ,
                           data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date%in% seq(2007, 2011))] ,
                           data_f$fertility_rate[which(data_f$age == "60-64" & data_f$date %in% seq(2012,2016))],
                           data_f$fertility_rate[which(data_f$age == "65-69" & data_f$date %in% seq(2017, 2020))])
  
  children[67, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2003,2007))]  ,
                          data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date%in% seq(2008, 2012))] ,
                          data_f$fertility_rate[which(data_f$age == "60-64" & data_f$date %in% seq(2013,2017))],
                          data_f$fertility_rate[which(data_f$age == "65-69" & data_f$date %in% seq(2018, 2020))]) 
  
  children[66, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2003)]  ,
                           data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2004,2008))] ,
                           data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date%in% seq(2009, 2013))] ,
                           data_f$fertility_rate[which(data_f$age == "60-64" & data_f$date %in% seq(2014,2018))],
                           data_f$fertility_rate[which(data_f$age == "65-69" & data_f$date %in% seq(2019, 2020))])
  
  children[65, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date%in% seq(2003,2004))]  ,
                           data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2005,2009))]  ,
                           data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date%in% seq(2010, 2014))] ,
                           data_f$fertility_rate[which(data_f$age == "60-64" & data_f$date %in% seq(2015,2019))],
                           data_f$fertility_rate[which(data_f$age == "65-69" & data_f$date == 2020)])
  ############
  children[64, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date%in% seq(2003,2005))] ,
                           data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2006,2010))] ,
                           data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date%in% seq(2011, 2015))] ,
                           data_f$fertility_rate[which(data_f$age == "60-64" & data_f$date %in% seq(2016,2020))])
  
  
  children[63, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date%in% seq(2003,2006))] ,
                           data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2007,2011))]  ,
                           data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date%in% seq(2012, 2016))] ,
                           data_f$fertility_rate[which(data_f$age == "60-64" & data_f$date %in% seq(2017,2020))])
  
  # A 62 year old would havefive year 
  # in 45-49 age year category in 2003 - 2007
  children[62, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2003, 2007))],
                          data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2008,2012))]  ,
                          data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date%in% seq(2013, 2017))] ,
                          data_f$fertility_rate[which(data_f$age == "60-64" & data_f$date %in% seq(2018,2020))])
  # A 61 year old would have had 1 year in 40-44 year group in 2003 and five year
  # in 45-49 age year category in 2004 - 2008, 5 years in 50-54 age year category in 09-13
  children[61, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2003)], 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2004, 2008))],
                          data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2009,2013))]  ,
                          data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date%in% seq(2014, 2018))] ,
                          data_f$fertility_rate[which(data_f$age == "60-64" & data_f$date %in% seq(2019,2020))])
  
  children[60, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date %in% seq(2003, 2004))]  , 
                           data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2005, 2009))],
                           data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2010,2014))]  ,
                           data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date%in% seq(2015, 2019))] ,
                           data_f$fertility_rate[which(data_f$age == "60-64" & data_f$date == 2020)])
  
  
  children[59, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date %in% seq(2003, 2005))]  , 
                           data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2006, 2010))],
                           data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2011,2015))]  ,
                           data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date%in% seq(2016, 2020))])
  
  
  children[58, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date %in% seq(2003, 2006))], 
                           data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2007, 2011))],
                           data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2012,2016))]  ,
                           data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date%in% seq(2017, 2020))])
  
  
  children[57, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date %in% seq(2003, 2007))] , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2008, 2012))],
                          data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2013,2017))]  ,
                          data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date%in% seq(2018, 2020))])
  
  
  children[56, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2003)] , 
                           data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date %in% seq(2004, 2008))] , 
                           data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2009, 2013))],
                           data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2014,2018))]  ,
                           data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date%in% seq(2019, 2020))])
  
  children[55, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date %in% seq(2003, 2004))] , 
                           data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date %in% seq(2005, 2009))] , 
                           data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2010, 2014))],
                           data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2015,2019))]  ,
                           data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date == 2020)])
  
  children[54, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2015)],
                          data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2016,2020))])
  
  children[53, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2016)], 
                          data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2017,2020))])
  
  
  children[52, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2017)],
                          data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2018,2020))])
  
  
  children[51, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2018)],
                          data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2019,2020))])
  
  
  children[50, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2019)],
                          data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date == 2020)])
  
  
  children[49, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2020)])
  
  children[48, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2020)])
  
  children[47, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2020)])
  
  children[46, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2020)])
  
  children[45, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2020)])
  
  children[44, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2020)])
  
  children[43, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2020)])
  
  children[42, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2020)])
  
  children[41, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2020)])
  
  children[40, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2020)])
  
  children[39, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2020)])
  
  children[38, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2020)])
  
  children[37, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2020)])
  
  children[36, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2020)])
  
  children[35, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2020)])
  
  children[34, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2020)])
  
  children[33, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2020)])
  
  children[32, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2020)])
  
  children[31, 17:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2020)])
  
  children[30, 16:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2020)])
  
  children[29, 15:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2020)])
  
  children[28, 14:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)])
  
  children[27, 13:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)])
  
  children[26, 12:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)])
  
  children[25, 11:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)])
  
  children[24, 10:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)])
  
  children[23, 9:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2012)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2013)]  ,
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2014)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2015)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2016)]  , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2017)]  , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)]  , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)]  , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)])
  
  children[22, 8:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2013)]  ,
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2014)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2015)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2016)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2017)]  , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)]  , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)]  , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)])
  
  children[21, 7:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2014)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2015)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2016)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2017)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2018)]  , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)]  , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)])
  
  children[20, 6:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2015)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2016)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2017)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2018)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2018)]  , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)])
  
  children[19, 5:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2016)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2017)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2018)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2018)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2018)])
  
  children[18, 4:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2017)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2018)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2019)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2020)])
  
  children[17, 3:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2018)] , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2019)], 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2020)])
  
  children[16,2:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2019)]  , 
                        data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2020)])
  
  children[15,1] <- data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2020)]
  #plot(apply(children, 1, sum), xlab = "Age of father", ylab = "Number of children")
  children = as.data.frame(children)
  names(children) = paste0(seq(0:17)-1, ' years')
  
  write_csv(path = paste0('data/', country, '/child_raw_m.csv'), children)
  plot_c = as.data.frame(as.numeric(as.character(unlist(children))))
  plot_c$father_age = rep(1:100,18)
  plot_c$child_age =sort(rep(seq(18)-1, 100))
  setnames(plot_c, 1, 'prob')

}

# Usa
process_children_father_55_plus = function(country, data_f){
  ##
  children = matrix(rep(0, 100*18), nrow = 100)
  names(children) = paste0(seq(0:17), 'years')
  
  # A 100 year old would have 18 years in 55+ in 2003 - 2020
  
  children[100, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2003,2020))])
  children[99, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2003,2020))])
  children[98, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2003,2020))])
  children[97, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2003,2020))])
  children[96, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2003,2020))])
  children[95, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2003,2020))])
  children[94, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2003,2020))])
  children[93, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2003,2020))])
  children[92, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2003,2020))])
  children[91, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2003,2020))])
  children[90, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2003,2020))])
  children[89, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2003,2020))])
  children[88, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2003,2020))])
  children[87, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2003,2020))])
  children[86, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2003,2020))])
  children[85, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2003,2020))])
  children[84, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2003,2020))])
  children[83, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2003,2020))])
  children[82, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2003,2020))])
  children[81, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2003,2020))])
  children[80, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2003,2020))])
  children[79, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2003,2020))])
  children[78, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2003,2020))])
  children[77, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2003,2020))])
  children[76, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2003,2020))])
  children[75, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2003,2020))])
  children[74, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2003,2020))])
  children[73, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2003,2020))])
  children[72, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2003,2020))])
  
  
  children[71, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date == 2003)],
                           data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2004, 2008))] ,
                           data_f$fertility_rate[which(data_f$age == "55+" & data_f$date %in% seq(2009,2013))],
                           data_f$fertility_rate[which(data_f$age == "55+" & data_f$date %in% seq(2014, 2018))], 
                           data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2019,2020))])   
  
  children[70, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2003,2004))] ,
                           data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2005, 2009))] ,
                           data_f$fertility_rate[which(data_f$age == "55+" & data_f$date %in% seq(2010,2014))],
                           data_f$fertility_rate[which(data_f$age == "55+" & data_f$date %in% seq(2015, 2019))], 
                           data_f$fertility_rate[which(data_f$age == "55+" & data_f$date == 2020)]) 
  
  children[69, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2003,2005))] ,
                           data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2006, 2010))] ,
                           data_f$fertility_rate[which(data_f$age == "55+" & data_f$date %in% seq(2011,2015))],
                           data_f$fertility_rate[which(data_f$age == "55+" & data_f$date %in% seq(2016, 2020))]) 
  
  children[68, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2003,2006))] ,
                           data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2007, 2011))] ,
                           data_f$fertility_rate[which(data_f$age == "55+" & data_f$date %in% seq(2012,2016))],
                           data_f$fertility_rate[which(data_f$age == "55+" & data_f$date %in% seq(2017, 2020))])
  
  children[67, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2003,2007))]  ,
                          data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2008, 2012))] ,
                          data_f$fertility_rate[which(data_f$age == "55+" & data_f$date %in% seq(2013,2017))],
                          data_f$fertility_rate[which(data_f$age == "55+" & data_f$date %in% seq(2018, 2020))]) 
  
  children[66, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2003)]  ,
                           data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2004,2008))] ,
                           data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2009, 2013))] ,
                           data_f$fertility_rate[which(data_f$age == "55+" & data_f$date %in% seq(2014,2018))],
                           data_f$fertility_rate[which(data_f$age == "55+" & data_f$date %in% seq(2019, 2020))])
  
  children[65, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date%in% seq(2003,2004))]  ,
                           data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2005,2009))]  ,
                           data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2010, 2014))] ,
                           data_f$fertility_rate[which(data_f$age == "55+" & data_f$date %in% seq(2015,2019))],
                           data_f$fertility_rate[which(data_f$age == "55+" & data_f$date == 2020)])
  ############
  children[64, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date%in% seq(2003,2005))] ,
                           data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2006,2010))] ,
                           data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2011, 2015))] ,
                           data_f$fertility_rate[which(data_f$age == "55+" & data_f$date %in% seq(2016,2020))])
  
  
  children[63, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date%in% seq(2003,2006))] ,
                           data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2007,2011))]  ,
                           data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2012, 2016))] ,
                           data_f$fertility_rate[which(data_f$age == "55+" & data_f$date %in% seq(2017,2020))])
  
  # A 62 year old would havefive year 
  # in 45-49 age year category in 2003 - 2007
  children[62, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2003, 2007))],
                          data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2008,2012))]  ,
                          data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2013, 2017))] ,
                          data_f$fertility_rate[which(data_f$age == "55+" & data_f$date %in% seq(2018,2020))])
  # A 61 year old would have had 1 year in 40-44 year group in 2003 and five year
  # in 45-49 age year category in 2004 - 2008, 5 years in 50-54 age year category in 09-13
  children[61, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2003)], 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2004, 2008))],
                          data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2009,2013))]  ,
                          data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2014, 2018))] ,
                          data_f$fertility_rate[which(data_f$age == "55+" & data_f$date %in% seq(2019,2020))])
  
  children[60, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date %in% seq(2003, 2004))]  , 
                           data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2005, 2009))],
                           data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2010,2014))]  ,
                           data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2015, 2019))] ,
                           data_f$fertility_rate[which(data_f$age == "55+" & data_f$date == 2020)])
  
  
  children[59, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date %in% seq(2003, 2005))]  , 
                           data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2006, 2010))],
                           data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2011,2015))]  ,
                           data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2016, 2020))])
  
  
  children[58, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date %in% seq(2003, 2006))], 
                           data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2007, 2011))],
                           data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2012,2016))]  ,
                           data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2017, 2020))])
  
  
  children[57, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date %in% seq(2003, 2007))] , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2008, 2012))],
                          data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2013,2017))]  ,
                          data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2018, 2020))])
  
  
  children[56, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2003)] , 
                           data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date %in% seq(2004, 2008))] , 
                           data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2009, 2013))],
                           data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2014,2018))]  ,
                           data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2019, 2020))])
  
  children[55, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date %in% seq(2003, 2004))] , 
                           data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date %in% seq(2005, 2009))] , 
                           data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2010, 2014))],
                           data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2015,2019))]  ,
                           data_f$fertility_rate[which(data_f$age == "55+" & data_f$date == 2020)])
  
  children[54, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2015)],
                          data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2016,2020))])
  
  children[53, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2016)], 
                          data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2017,2020))])
  
  
  children[52, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2017)],
                          data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2018,2020))])
  
  
  children[51, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2018)],
                          data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2019,2020))])
  
  
  children[50, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2019)],
                          data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date == 2020)])
  
  
  children[49, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2020)])
  
  children[48, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2020)])
  
  children[47, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2020)])
  
  children[46, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2020)])
  
  children[45, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2020)])
  
  children[44, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2020)])
  
  children[43, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2020)])
  
  children[42, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2020)])
  
  children[41, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2020)])
  
  children[40, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2020)])
  
  children[39, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2020)])
  
  children[38, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2020)])
  
  children[37, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2020)])
  
  children[36, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2020)])
  
  children[35, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2020)])
  
  children[34, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2020)])
  
  children[33, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2020)])
  
  children[32, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2020)])
  
  children[31, 17:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2020)])
  
  children[30, 16:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2020)])
  
  children[29, 15:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2020)])
  
  children[28, 14:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)])
  
  children[27, 13:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)])
  
  children[26, 12:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)])
  
  children[25, 11:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)])
  
  children[24, 10:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)])
  
  children[23, 9:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2012)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2013)]  ,
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2014)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2015)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2016)]  , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2017)]  , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)]  , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)]  , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)])
  
  children[22, 8:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2013)]  ,
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2014)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2015)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2016)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2017)]  , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)]  , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)]  , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)])
  
  children[21, 7:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2014)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2015)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2016)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2017)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2018)]  , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)]  , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)])
  
  children[20, 6:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2015)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2016)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2017)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2018)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2018)]  , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)])
  
  children[19, 5:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2016)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2017)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2018)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2018)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2018)])
  
  children[18, 4:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2017)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2018)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2019)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2020)])
  
  children[17, 3:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2018)] , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2019)], 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2020)])
  
  children[16,2:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2019)]  , 
                        data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2020)])
  
  children[15,1] <- data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2020)]
  plot(apply(children, 1, sum), xlab = "Age of father", ylab = "Number of children")
  children = as.data.frame(children)
  names(children) = paste0(seq(0:17)-1, ' years')
  
  write_csv(path = paste0('data/', country, '/child_raw_m.csv'), children)
  plot_c = as.data.frame(as.numeric(as.character(unlist(children))))
  plot_c$father_age = rep(1:100,18)
  plot_c$child_age =sort(rep(seq(18)-1, 100))
  setnames(plot_c, 1, 'prob')
  
  #ggplot(as.data.frame(plot_c), aes(x=child_age, y=father_age, fill=prob)) +
  #  geom_tile(color = "white")+
  #  theme(axis.text.x = element_text(angle = 90)) + 
  #  labs(x= "child age", y="father age") +
  #  scale_fill_gradient2(low = "yellow", high = "red")
}

# Russia
process_children_father_60_plus = function(country, data_f){
  ##
  children = matrix(rep(0, 100*18), nrow = 100)
  names(children) = paste0(seq(0:17), 'years')
  
  # A 100 year old would have 18 years in 80+ in 2003 - 2020
  
  children[100, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "60+" & data_f$date%in% seq(2003,2020))])
  children[99, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "60+" & data_f$date%in% seq(2003,2020))])
  children[98, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "60+" & data_f$date%in% seq(2003,2020))])
  children[97, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "60+" & data_f$date%in% seq(2003,2020))])
  children[96, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "60+" & data_f$date%in% seq(2003,2020))])
  children[95, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "60+" & data_f$date%in% seq(2003,2020))])
  children[94, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "60+" & data_f$date%in% seq(2003,2020))])
  children[93, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "60+" & data_f$date%in% seq(2003,2020))])
  children[92, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "60+" & data_f$date%in% seq(2003,2020))])
  children[91, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "60+" & data_f$date%in% seq(2003,2020))])
  children[90, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "60+" & data_f$date%in% seq(2003,2020))])
  children[89, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "60+" & data_f$date%in% seq(2003,2020))])
  children[88, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "60+" & data_f$date%in% seq(2003,2020))])
  children[87, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "60+" & data_f$date%in% seq(2003,2020))])
  children[86, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "60+" & data_f$date%in% seq(2003,2020))])
  children[85, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "60+" & data_f$date%in% seq(2003,2020))])
  children[84, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "60+" & data_f$date%in% seq(2003,2020))])
  children[83, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "60+" & data_f$date%in% seq(2003,2020))])
  children[82, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "60+" & data_f$date%in% seq(2003,2020))])
  children[81, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "60+" & data_f$date%in% seq(2003,2020))])
  children[80, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "60+" & data_f$date%in% seq(2003,2020))])
  children[79, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "60+" & data_f$date%in% seq(2003,2020))])
  children[78, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "60+" & data_f$date%in% seq(2003,2020))])
  children[77, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "60+" & data_f$date%in% seq(2003,2020))])
  
  
  children[76, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date == 2003)] , 
                           data_f$fertility_rate[which(data_f$age == "60+" & data_f$date %in% seq(2004, 2008))] , 
                           data_f$fertility_rate[which(data_f$age == "60+" & data_f$date %in% seq(2009, 2013))],
                           data_f$fertility_rate[which(data_f$age == "60+" & data_f$date%in% seq(2014,2018))]  ,
                           data_f$fertility_rate[which(data_f$age == "60+" & data_f$date%in% seq(2019, 2020))])
  
  children[75, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date %in% seq(2003, 2004))] , 
                           data_f$fertility_rate[which(data_f$age == "60+" & data_f$date %in% seq(2005, 2009))] , 
                           data_f$fertility_rate[which(data_f$age == "60+" & data_f$date %in% seq(2010, 2014))],
                           data_f$fertility_rate[which(data_f$age == "60+" & data_f$date%in% seq(2015,2019))]  ,
                           data_f$fertility_rate[which(data_f$age == "60+" & data_f$date == 2020)])
  
  children[74, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "60+" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "60+" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "60+" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "60+" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "60+" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "60+" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "60+" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "60+" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "60+" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "60+" & data_f$date == 2015)],
                          data_f$fertility_rate[which(data_f$age == "60+" & data_f$date%in% seq(2016,2020))])
  
  children[73, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date %in% seq(2003,2006))]  , 
                          data_f$fertility_rate[which(data_f$age == "60+" & data_f$date %in% seq(2007,2011))],
                          data_f$fertility_rate[which(data_f$age == "60+" & data_f$date %in% seq(2012, 2016))], 
                          data_f$fertility_rate[which(data_f$age == "60+" & data_f$date%in% seq(2017,2020))])
  
  
  children[72, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date%in% seq(2003, 2007))] ,
                          data_f$fertility_rate[which(data_f$age == "60+" & data_f$date %in% seq(2008,2012))],
                          data_f$fertility_rate[which(data_f$age == "60+" & data_f$date %in% seq(2013, 2017))], 
                          data_f$fertility_rate[which(data_f$age == "60+" & data_f$date%in% seq(2018,2020))]) 
  
  children[71, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date == 2003)],
                           data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date%in% seq(2004, 2008))] ,
                           data_f$fertility_rate[which(data_f$age == "60+" & data_f$date %in% seq(2009,2013))],
                           data_f$fertility_rate[which(data_f$age == "60+" & data_f$date %in% seq(2014, 2018))], 
                           data_f$fertility_rate[which(data_f$age == "60+" & data_f$date%in% seq(2019,2020))])   
  
  children[70, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2003,2004))] ,
                           data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date%in% seq(2005, 2009))] ,
                           data_f$fertility_rate[which(data_f$age == "60+" & data_f$date %in% seq(2010,2014))],
                           data_f$fertility_rate[which(data_f$age == "60+" & data_f$date %in% seq(2015, 2019))], 
                           data_f$fertility_rate[which(data_f$age == "60+" & data_f$date == 2020)]) 
  
  children[69, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2003,2005))] ,
                           data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date%in% seq(2006, 2010))] ,
                           data_f$fertility_rate[which(data_f$age == "60+" & data_f$date %in% seq(2011,2015))],
                           data_f$fertility_rate[which(data_f$age == "60+" & data_f$date %in% seq(2016, 2020))]) 
  
  children[68, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2003,2006))] ,
                           data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date%in% seq(2007, 2011))] ,
                           data_f$fertility_rate[which(data_f$age == "60+" & data_f$date %in% seq(2012,2016))],
                           data_f$fertility_rate[which(data_f$age == "60+" & data_f$date %in% seq(2017, 2020))])
  
  children[67, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2003,2007))]  ,
                          data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date%in% seq(2008, 2012))] ,
                          data_f$fertility_rate[which(data_f$age == "60+" & data_f$date %in% seq(2013,2017))],
                          data_f$fertility_rate[which(data_f$age == "60+" & data_f$date %in% seq(2018, 2020))]) 
  
  children[66, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2003)]  ,
                           data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2004,2008))] ,
                           data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date%in% seq(2009, 2013))] ,
                           data_f$fertility_rate[which(data_f$age == "60+" & data_f$date %in% seq(2014,2018))],
                           data_f$fertility_rate[which(data_f$age == "60+" & data_f$date %in% seq(2019, 2020))])
  
  children[65, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date%in% seq(2003,2004))]  ,
                           data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2005,2009))]  ,
                           data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date%in% seq(2010, 2014))] ,
                           data_f$fertility_rate[which(data_f$age == "60+" & data_f$date %in% seq(2015,2019))],
                           data_f$fertility_rate[which(data_f$age == "60+" & data_f$date == 2020)])
  ############
  children[64, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date%in% seq(2003,2005))] ,
                           data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2006,2010))] ,
                           data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date%in% seq(2011, 2015))] ,
                           data_f$fertility_rate[which(data_f$age == "60+" & data_f$date %in% seq(2016,2020))])
  
  
  children[63, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date%in% seq(2003,2006))] ,
                           data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2007,2011))]  ,
                           data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date%in% seq(2012, 2016))] ,
                           data_f$fertility_rate[which(data_f$age == "60+" & data_f$date %in% seq(2017,2020))])
  
  # A 62 year old would havefive year 
  # in 45-49 age year category in 2003 - 2007
  children[62, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2003, 2007))],
                          data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2008,2012))]  ,
                          data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date%in% seq(2013, 2017))] ,
                          data_f$fertility_rate[which(data_f$age == "60+" & data_f$date %in% seq(2018,2020))])
  # A 61 year old would have had 1 year in 40-44 year group in 2003 and five year
  # in 45-49 age year category in 2004 - 2008, 5 years in 50-54 age year category in 09-13
  children[61, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2003)], 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2004, 2008))],
                          data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2009,2013))]  ,
                          data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date%in% seq(2014, 2018))] ,
                          data_f$fertility_rate[which(data_f$age == "60+" & data_f$date %in% seq(2019,2020))])
  
  children[60, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date %in% seq(2003, 2004))]  , 
                           data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2005, 2009))],
                           data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2010,2014))]  ,
                           data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date%in% seq(2015, 2019))] ,
                           data_f$fertility_rate[which(data_f$age == "60+" & data_f$date == 2020)])
  
  
  children[59, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date %in% seq(2003, 2005))]  , 
                           data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2006, 2010))],
                           data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2011,2015))]  ,
                           data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date%in% seq(2016, 2020))])
  
  
  children[58, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date %in% seq(2003, 2006))], 
                           data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2007, 2011))],
                           data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2012,2016))]  ,
                           data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date%in% seq(2017, 2020))])
  
  
  children[57, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date %in% seq(2003, 2007))] , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2008, 2012))],
                          data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2013,2017))]  ,
                          data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date%in% seq(2018, 2020))])
  
  
  children[56, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2003)] , 
                           data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date %in% seq(2004, 2008))] , 
                           data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2009, 2013))],
                           data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2014,2018))]  ,
                           data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date%in% seq(2019, 2020))])
  
  children[55, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date %in% seq(2003, 2004))] , 
                           data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date %in% seq(2005, 2009))] , 
                           data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2010, 2014))],
                           data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2015,2019))]  ,
                           data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date == 2020)])
  
  children[54, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2015)],
                          data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2016,2020))])
  
  children[53, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2016)], 
                          data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2017,2020))])
  
  
  children[52, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2017)],
                          data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2018,2020))])
  
  
  children[51, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2018)],
                          data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2019,2020))])
  
  
  children[50, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2019)],
                          data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date == 2020)])
  
  
  children[49, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2020)])
  
  children[48, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2020)])
  
  children[47, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2020)])
  
  children[46, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2020)])
  
  children[45, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2020)])
  
  children[44, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2020)])
  
  children[43, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2020)])
  
  children[42, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2020)])
  
  children[41, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2020)])
  
  children[40, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2020)])
  
  children[39, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2020)])
  
  children[38, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2020)])
  
  children[37, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2020)])
  
  children[36, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2020)])
  
  children[35, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2020)])
  
  children[34, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2020)])
  
  children[33, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2020)])
  
  children[32, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2020)])
  
  children[31, 17:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2020)])
  
  children[30, 16:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2020)])
  
  children[29, 15:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2020)])
  
  children[28, 14:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)])
  
  children[27, 13:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)])
  
  children[26, 12:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)])
  
  children[25, 11:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)])
  
  children[24, 10:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)])
  
  children[23, 9:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2012)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2013)]  ,
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2014)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2015)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2016)]  , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2017)]  , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)]  , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)]  , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)])
  
  children[22, 8:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2013)]  ,
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2014)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2015)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2016)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2017)]  , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)]  , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)]  , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)])
  
  children[21, 7:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2014)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2015)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2016)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2017)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2018)]  , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)]  , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)])
  
  children[20, 6:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2015)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2016)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2017)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2018)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2018)]  , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)])
  
  children[19, 5:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2016)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2017)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2018)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2018)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2018)])
  
  children[18, 4:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2017)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2018)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2019)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2020)])
  
  children[17, 3:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2018)] , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2019)], 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2020)])
  
  children[16,2:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2019)]  , 
                        data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2020)])
  
  children[15,1] <- data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2020)]
  plot(apply(children, 1, sum), xlab = "Age of father", ylab = "Number of children")
  children = as.data.frame(children)
  names(children) = paste0(seq(0:17)-1, ' years')
  
  write_csv(path = paste0('data/Russia/child_raw_m.csv'), children)
  plot_c = as.data.frame(as.numeric(as.character(unlist(children))))
  plot_c$father_age = rep(1:100,18)
  plot_c$child_age =sort(rep(seq(18)-1, 100))
  setnames(plot_c, 1, 'prob')
  
  #ggplot(as.data.frame(plot_c), aes(x=child_age, y=father_age, fill=prob)) +
  #  geom_tile(color = "white")+
  #  theme(axis.text.x = element_text(angle = 90)) + 
  #  labs(x= "child age", y="father age") +
  #  scale_fill_gradient2(low = "yellow", high = "red")
}

# Poland
process_children_father_50_plus = function(country, data_f){
  ##
  children = matrix(rep(0, 100*18), nrow = 100)
  names(children) = paste0(seq(0:17), 'years')
  
  # A 100 year old would have 18 years in 50+ in 2003 - 2020
  
  children[100, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "50+" & data_f$date%in% seq(2003,2020))])
  children[99, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "50+" & data_f$date%in% seq(2003,2020))])
  children[98, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "50+" & data_f$date%in% seq(2003,2020))])
  children[97, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "50+" & data_f$date%in% seq(2003,2020))])
  children[96, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "50+" & data_f$date%in% seq(2003,2020))])
  children[95, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "50+" & data_f$date%in% seq(2003,2020))])
  children[94, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "50+" & data_f$date%in% seq(2003,2020))])
  children[93, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "50+" & data_f$date%in% seq(2003,2020))])
  children[92, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "50+" & data_f$date%in% seq(2003,2020))])
  children[91, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "50+" & data_f$date%in% seq(2003,2020))])
  children[90, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "50+" & data_f$date%in% seq(2003,2020))])
  children[89, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "50+" & data_f$date%in% seq(2003,2020))])
  children[88, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "50+" & data_f$date%in% seq(2003,2020))])
  children[87, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "50+" & data_f$date%in% seq(2003,2020))])
  children[86, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "50+" & data_f$date%in% seq(2003,2020))])
  children[85, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "50+" & data_f$date%in% seq(2003,2020))])
  children[84, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "50+" & data_f$date%in% seq(2003,2020))])
  children[83, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "50+" & data_f$date%in% seq(2003,2020))])
  children[82, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "50+" & data_f$date%in% seq(2003,2020))])
  children[81, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "50+" & data_f$date%in% seq(2003,2020))])
  children[80, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "50+" & data_f$date%in% seq(2003,2020))])
  children[79, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "50+" & data_f$date%in% seq(2003,2020))])
  children[78, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "50+" & data_f$date%in% seq(2003,2020))])
  children[77, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "50+" & data_f$date%in% seq(2003,2020))])
  children[76, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "50+" & data_f$date%in% seq(2003,2020))])
  children[75, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "50+" & data_f$date%in% seq(2003,2020))])
  children[74, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "50+" & data_f$date%in% seq(2003,2020))])
  children[73, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "50+" & data_f$date%in% seq(2003,2020))])
  children[72, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "50+" & data_f$date%in% seq(2003,2020))])
  
  
  children[71, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "50+" & data_f$date == 2003)],
                           data_f$fertility_rate[which(data_f$age == "50+" & data_f$date%in% seq(2004, 2008))] ,
                           data_f$fertility_rate[which(data_f$age == "50+" & data_f$date %in% seq(2009,2013))],
                           data_f$fertility_rate[which(data_f$age == "50+" & data_f$date %in% seq(2014, 2018))], 
                           data_f$fertility_rate[which(data_f$age == "50+" & data_f$date%in% seq(2019,2020))])   
  
  children[70, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "50+" & data_f$date%in% seq(2003,2004))] ,
                           data_f$fertility_rate[which(data_f$age == "50+" & data_f$date%in% seq(2005, 2009))] ,
                           data_f$fertility_rate[which(data_f$age == "50+" & data_f$date %in% seq(2010,2014))],
                           data_f$fertility_rate[which(data_f$age == "50+" & data_f$date %in% seq(2015, 2019))], 
                           data_f$fertility_rate[which(data_f$age == "50+" & data_f$date == 2020)]) 
  
  children[69, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "50+" & data_f$date%in% seq(2003,2005))] ,
                           data_f$fertility_rate[which(data_f$age == "50+" & data_f$date%in% seq(2006, 2010))] ,
                           data_f$fertility_rate[which(data_f$age == "50+" & data_f$date %in% seq(2011,2015))],
                           data_f$fertility_rate[which(data_f$age == "50+" & data_f$date %in% seq(2016, 2020))]) 
  
  children[68, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "50+" & data_f$date%in% seq(2003,2006))] ,
                           data_f$fertility_rate[which(data_f$age == "50+" & data_f$date%in% seq(2007, 2011))] ,
                           data_f$fertility_rate[which(data_f$age == "50+" & data_f$date %in% seq(2012,2016))],
                           data_f$fertility_rate[which(data_f$age == "50+" & data_f$date %in% seq(2017, 2020))])
  
  children[67, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "50+" & data_f$date%in% seq(2003,2007))]  ,
                          data_f$fertility_rate[which(data_f$age == "50+" & data_f$date%in% seq(2008, 2012))] ,
                          data_f$fertility_rate[which(data_f$age == "50+" & data_f$date %in% seq(2013,2017))],
                          data_f$fertility_rate[which(data_f$age == "50+" & data_f$date %in% seq(2018, 2020))]) 
  
  children[66, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2003)]  ,
                           data_f$fertility_rate[which(data_f$age == "50+" & data_f$date%in% seq(2004,2008))] ,
                           data_f$fertility_rate[which(data_f$age == "50+" & data_f$date%in% seq(2009, 2013))] ,
                           data_f$fertility_rate[which(data_f$age == "50+" & data_f$date %in% seq(2014,2018))],
                           data_f$fertility_rate[which(data_f$age == "50+" & data_f$date %in% seq(2019, 2020))])
  
  children[65, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date%in% seq(2003,2004))]  ,
                           data_f$fertility_rate[which(data_f$age == "50+" & data_f$date%in% seq(2005,2009))]  ,
                           data_f$fertility_rate[which(data_f$age == "50+" & data_f$date%in% seq(2010, 2014))] ,
                           data_f$fertility_rate[which(data_f$age == "50+" & data_f$date %in% seq(2015,2019))],
                           data_f$fertility_rate[which(data_f$age == "50+" & data_f$date == 2020)])
  ############
  children[64, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date%in% seq(2003,2005))] ,
                           data_f$fertility_rate[which(data_f$age == "50+" & data_f$date%in% seq(2006,2010))] ,
                           data_f$fertility_rate[which(data_f$age == "50+" & data_f$date%in% seq(2011, 2015))] ,
                           data_f$fertility_rate[which(data_f$age == "50+" & data_f$date %in% seq(2016,2020))])
  
  
  children[63, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date%in% seq(2003,2006))] ,
                           data_f$fertility_rate[which(data_f$age == "50+" & data_f$date%in% seq(2007,2011))]  ,
                           data_f$fertility_rate[which(data_f$age == "50+" & data_f$date%in% seq(2012, 2016))] ,
                           data_f$fertility_rate[which(data_f$age == "50+" & data_f$date %in% seq(2017,2020))])
  
  # A 62 year old would havefive year 
  # in 45-49 age year category in 2003 - 2007
  children[62, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2003, 2007))],
                          data_f$fertility_rate[which(data_f$age == "50+" & data_f$date%in% seq(2008,2012))]  ,
                          data_f$fertility_rate[which(data_f$age == "50+" & data_f$date%in% seq(2013, 2017))] ,
                          data_f$fertility_rate[which(data_f$age == "50+" & data_f$date %in% seq(2018,2020))])
  # A 61 year old would have had 1 year in 40-44 year group in 2003 and five year
  # in 45-49 age year category in 2004 - 2008, 5 years in 50+ age year category in 09-13
  children[61, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2003)], 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2004, 2008))],
                          data_f$fertility_rate[which(data_f$age == "50+" & data_f$date%in% seq(2009,2013))]  ,
                          data_f$fertility_rate[which(data_f$age == "50+" & data_f$date%in% seq(2014, 2018))] ,
                          data_f$fertility_rate[which(data_f$age == "50+" & data_f$date %in% seq(2019,2020))])
  
  children[60, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date %in% seq(2003, 2004))]  , 
                           data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2005, 2009))],
                           data_f$fertility_rate[which(data_f$age == "50+" & data_f$date%in% seq(2010,2014))]  ,
                           data_f$fertility_rate[which(data_f$age == "50+" & data_f$date%in% seq(2015, 2019))] ,
                           data_f$fertility_rate[which(data_f$age == "50+" & data_f$date == 2020)])
  
  
  children[59, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date %in% seq(2003, 2005))]  , 
                           data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2006, 2010))],
                           data_f$fertility_rate[which(data_f$age == "50+" & data_f$date%in% seq(2011,2015))]  ,
                           data_f$fertility_rate[which(data_f$age == "50+" & data_f$date%in% seq(2016, 2020))])
  
  
  children[58, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date %in% seq(2003, 2006))], 
                           data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2007, 2011))],
                           data_f$fertility_rate[which(data_f$age == "50+" & data_f$date%in% seq(2012,2016))]  ,
                           data_f$fertility_rate[which(data_f$age == "50+" & data_f$date%in% seq(2017, 2020))])
  
  
  children[57, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date %in% seq(2003, 2007))] , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2008, 2012))],
                          data_f$fertility_rate[which(data_f$age == "50+" & data_f$date%in% seq(2013,2017))]  ,
                          data_f$fertility_rate[which(data_f$age == "50+" & data_f$date%in% seq(2018, 2020))])
  
  
  children[56, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2003)] , 
                           data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date %in% seq(2004, 2008))] , 
                           data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2009, 2013))],
                           data_f$fertility_rate[which(data_f$age == "50+" & data_f$date%in% seq(2014,2018))]  ,
                           data_f$fertility_rate[which(data_f$age == "50+" & data_f$date%in% seq(2019, 2020))])
  
  children[55, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date %in% seq(2003, 2004))] , 
                           data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date %in% seq(2005, 2009))] , 
                           data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2010, 2014))],
                           data_f$fertility_rate[which(data_f$age == "50+" & data_f$date%in% seq(2015,2019))]  ,
                           data_f$fertility_rate[which(data_f$age == "50+" & data_f$date == 2020)])
  
  children[54, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2015)],
                          data_f$fertility_rate[which(data_f$age == "50+" & data_f$date%in% seq(2016,2020))])
  
  children[53, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2016)], 
                          data_f$fertility_rate[which(data_f$age == "50+" & data_f$date%in% seq(2017,2020))])
  
  
  children[52, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2017)],
                          data_f$fertility_rate[which(data_f$age == "50+" & data_f$date%in% seq(2018,2020))])
  
  
  children[51, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2018)],
                          data_f$fertility_rate[which(data_f$age == "50+" & data_f$date%in% seq(2019,2020))])
  
  
  children[50, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2019)],
                          data_f$fertility_rate[which(data_f$age == "50+" & data_f$date == 2020)])
  
  
  children[49, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2020)])
  
  children[48, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2020)])
  
  children[47, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2020)])
  
  children[46, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2020)])
  
  children[45, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2020)])
  
  children[44, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2020)])
  
  children[43, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2020)])
  
  children[42, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2020)])
  
  children[41, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2020)])
  
  children[40, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2020)])
  
  children[39, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2020)])
  
  children[38, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2020)])
  
  children[37, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2020)])
  
  children[36, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2020)])
  
  children[35, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2020)])
  
  children[34, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2020)])
  
  children[33, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2020)])
  
  children[32, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2020)])
  
  children[31, 17:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2020)])
  
  children[30, 16:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2020)])
  
  children[29, 15:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2020)])
  
  children[28, 14:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)])
  
  children[27, 13:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)])
  
  children[26, 12:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)])
  
  children[25, 11:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)])
  
  children[24, 10:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)])
  
  children[23, 9:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2012)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2013)]  ,
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2014)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2015)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2016)]  , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2017)]  , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)]  , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)]  , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)])
  
  children[22, 8:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2013)]  ,
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2014)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2015)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2016)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2017)]  , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)]  , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)]  , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)])
  
  children[21, 7:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2014)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2015)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2016)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2017)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2018)]  , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)]  , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)])
  
  children[20, 6:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2015)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2016)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2017)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2018)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2018)]  , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)])
  
  children[19, 5:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2016)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2017)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2018)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2018)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2018)])
  
  children[18, 4:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2017)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2018)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2019)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2020)])
  
  children[17, 3:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2018)] , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2019)], 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2020)])
  
  children[16,2:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2019)]  , 
                        data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2020)])
  
  children[15,1] <- data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2020)]
  plot(apply(children, 1, sum), xlab = "Age of father", ylab = "Number of children")
  children = as.data.frame(children)
  names(children) = paste0(seq(0:17)-1, ' years')
  
  write_csv(path = paste0('data/', country, '/child_raw_m.csv'), children)
  plot_c = as.data.frame(as.numeric(as.character(unlist(children))))
  plot_c$father_age = rep(1:100,18)
  plot_c$child_age =sort(rep(seq(18)-1, 100))
  setnames(plot_c, 1, 'prob')
  
  #ggplot(as.data.frame(plot_c), aes(x=child_age, y=father_age, fill=prob)) +
  #  geom_tile(color = "white")+
  #  theme(axis.text.x = element_text(angle = 90)) + 
  #  labs(x= "child age", y="father age") +
  #  scale_fill_gradient2(low = "yellow", high = "red")
}

process_children_father_65_plus = function(country, data_f){
  ##
  children = matrix(rep(0, 100*18), nrow = 100)
  names(children) = paste0(seq(0:17), 'years')
  
  # A 100 year old would have 18 years in 65+ in 2003 - 2020
  
  children[100, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "65+" & data_f$date%in% seq(2003,2020))])
  children[99, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "65+" & data_f$date%in% seq(2003,2020))])
  children[98, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "65+" & data_f$date%in% seq(2003,2020))])
  children[97, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "65+" & data_f$date%in% seq(2003,2020))])
  children[96, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "65+" & data_f$date%in% seq(2003,2020))])
  children[95, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "65+" & data_f$date%in% seq(2003,2020))])
  children[94, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "65+" & data_f$date%in% seq(2003,2020))])
  children[93, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "65+" & data_f$date%in% seq(2003,2020))])
  children[92, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "65+" & data_f$date%in% seq(2003,2020))])
  children[91, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "65+" & data_f$date%in% seq(2003,2020))])
  children[90, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "65+" & data_f$date%in% seq(2003,2020))])
  children[89, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "65+" & data_f$date%in% seq(2003,2020))])
  children[88, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "65+" & data_f$date%in% seq(2003,2020))])
  children[87, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "65+" & data_f$date%in% seq(2003,2020))])
  children[86, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "65+" & data_f$date%in% seq(2003,2020))])
  children[85, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "65+" & data_f$date%in% seq(2003,2020))])
  children[84, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "65+" & data_f$date%in% seq(2003,2020))])
  children[83, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "65+" & data_f$date%in% seq(2003,2020))])
  children[82, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "65+" & data_f$date%in% seq(2003,2020))])
  
  children[81, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "60-64" & data_f$date == 2003)], 
                          data_f$fertility_rate[which(data_f$age == "65+" & data_f$date %in% seq(2004, 2008))],
                          data_f$fertility_rate[which(data_f$age == "65+" & data_f$date%in% seq(2009,2013))]  ,
                          data_f$fertility_rate[which(data_f$age == "65+" & data_f$date%in% seq(2014, 2018))] ,
                          data_f$fertility_rate[which(data_f$age == "65+" & data_f$date %in% seq(2019,2020))])
  
  children[80, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "60-64" & data_f$date %in% seq(2003, 2004))]  , 
                           data_f$fertility_rate[which(data_f$age == "65+" & data_f$date %in% seq(2005, 2009))],
                           data_f$fertility_rate[which(data_f$age == "65+" & data_f$date%in% seq(2010,2014))]  ,
                           data_f$fertility_rate[which(data_f$age == "65+" & data_f$date%in% seq(2015, 2019))] ,
                           data_f$fertility_rate[which(data_f$age == "65+" & data_f$date == 2020)])
  
  
  children[79, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "60-64" & data_f$date %in% seq(2003, 2005))]  , 
                           data_f$fertility_rate[which(data_f$age == "65+" & data_f$date %in% seq(2006, 2010))],
                           data_f$fertility_rate[which(data_f$age == "65+" & data_f$date%in% seq(2011,2015))]  ,
                           data_f$fertility_rate[which(data_f$age == "65+" & data_f$date%in% seq(2016, 2020))])
  
  
  children[78, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "60-64" & data_f$date %in% seq(2003, 2006))], 
                           data_f$fertility_rate[which(data_f$age == "65+" & data_f$date %in% seq(2007, 2011))],
                           data_f$fertility_rate[which(data_f$age == "65+" & data_f$date%in% seq(2012,2016))]  ,
                           data_f$fertility_rate[which(data_f$age == "65+" & data_f$date%in% seq(2017, 2020))])
  
  
  children[77, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "60-64" & data_f$date %in% seq(2003, 2007))] , 
                          data_f$fertility_rate[which(data_f$age == "65+" & data_f$date %in% seq(2008, 2012))],
                          data_f$fertility_rate[which(data_f$age == "65+" & data_f$date%in% seq(2013,2017))]  ,
                          data_f$fertility_rate[which(data_f$age == "65+" & data_f$date%in% seq(2018, 2020))])
  
  
  children[76, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date == 2003)] , 
                           data_f$fertility_rate[which(data_f$age == "60-64" & data_f$date %in% seq(2004, 2008))] , 
                           data_f$fertility_rate[which(data_f$age == "65+" & data_f$date %in% seq(2009, 2013))],
                           data_f$fertility_rate[which(data_f$age == "65+" & data_f$date%in% seq(2014,2018))]  ,
                           data_f$fertility_rate[which(data_f$age == "65+" & data_f$date%in% seq(2019, 2020))])
  
  children[75, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date %in% seq(2003, 2004))] , 
                           data_f$fertility_rate[which(data_f$age == "60-64" & data_f$date %in% seq(2005, 2009))] , 
                           data_f$fertility_rate[which(data_f$age == "65+" & data_f$date %in% seq(2010, 2014))],
                           data_f$fertility_rate[which(data_f$age == "65+" & data_f$date%in% seq(2015,2019))]  ,
                           data_f$fertility_rate[which(data_f$age == "65+" & data_f$date == 2020)])
  
  children[74, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "60-64" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "60-64" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "60-64" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "60-64" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "60-64" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "65+" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "65+" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "65+" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "65+" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "65+" & data_f$date == 2015)],
                          data_f$fertility_rate[which(data_f$age == "65+" & data_f$date%in% seq(2016,2020))])
  
  children[73, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date %in% seq(2003,2006))]  , 
                          data_f$fertility_rate[which(data_f$age == "60-64" & data_f$date %in% seq(2007,2011))],
                          data_f$fertility_rate[which(data_f$age == "65+" & data_f$date %in% seq(2012, 2016))], 
                          data_f$fertility_rate[which(data_f$age == "65+" & data_f$date%in% seq(2017,2020))])
  
  
  children[72, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date%in% seq(2003, 2007))] ,
                          data_f$fertility_rate[which(data_f$age == "60-64" & data_f$date %in% seq(2008,2012))],
                          data_f$fertility_rate[which(data_f$age == "65+" & data_f$date %in% seq(2013, 2017))], 
                          data_f$fertility_rate[which(data_f$age == "65+" & data_f$date%in% seq(2018,2020))]) 
  
  children[71, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date == 2003)],
                           data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date%in% seq(2004, 2008))] ,
                           data_f$fertility_rate[which(data_f$age == "60-64" & data_f$date %in% seq(2009,2013))],
                           data_f$fertility_rate[which(data_f$age == "65+" & data_f$date %in% seq(2014, 2018))], 
                           data_f$fertility_rate[which(data_f$age == "65+" & data_f$date%in% seq(2019,2020))])   
  
  children[70, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2003,2004))] ,
                           data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date%in% seq(2005, 2009))] ,
                           data_f$fertility_rate[which(data_f$age == "60-64" & data_f$date %in% seq(2010,2014))],
                           data_f$fertility_rate[which(data_f$age == "65+" & data_f$date %in% seq(2015, 2019))], 
                           data_f$fertility_rate[which(data_f$age == "65+" & data_f$date == 2020)]) 
  
  children[69, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2003,2005))] ,
                           data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date%in% seq(2006, 2010))] ,
                           data_f$fertility_rate[which(data_f$age == "60-64" & data_f$date %in% seq(2011,2015))],
                           data_f$fertility_rate[which(data_f$age == "65+" & data_f$date %in% seq(2016, 2020))]) 
  
  children[68, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2003,2006))] ,
                           data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date%in% seq(2007, 2011))] ,
                           data_f$fertility_rate[which(data_f$age == "60-64" & data_f$date %in% seq(2012,2016))],
                           data_f$fertility_rate[which(data_f$age == "65+" & data_f$date %in% seq(2017, 2020))])
  
  children[67, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2003,2007))]  ,
                          data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date%in% seq(2008, 2012))] ,
                          data_f$fertility_rate[which(data_f$age == "60-64" & data_f$date %in% seq(2013,2017))],
                          data_f$fertility_rate[which(data_f$age == "65+" & data_f$date %in% seq(2018, 2020))]) 
  
  children[66, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2003)]  ,
                           data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2004,2008))] ,
                           data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date%in% seq(2009, 2013))] ,
                           data_f$fertility_rate[which(data_f$age == "60-64" & data_f$date %in% seq(2014,2018))],
                           data_f$fertility_rate[which(data_f$age == "65+" & data_f$date %in% seq(2019, 2020))])
  
  children[65, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date%in% seq(2003,2004))]  ,
                           data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2005,2009))]  ,
                           data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date%in% seq(2010, 2014))] ,
                           data_f$fertility_rate[which(data_f$age == "60-64" & data_f$date %in% seq(2015,2019))],
                           data_f$fertility_rate[which(data_f$age == "65+" & data_f$date == 2020)])
  ############
  children[64, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date%in% seq(2003,2005))] ,
                           data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2006,2010))] ,
                           data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date%in% seq(2011, 2015))] ,
                           data_f$fertility_rate[which(data_f$age == "60-64" & data_f$date %in% seq(2016,2020))])
  
  
  children[63, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date%in% seq(2003,2006))] ,
                           data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2007,2011))]  ,
                           data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date%in% seq(2012, 2016))] ,
                           data_f$fertility_rate[which(data_f$age == "60-64" & data_f$date %in% seq(2017,2020))])
  
  # A 62 year old would havefive year 
  # in 45-49 age year category in 2003 - 2007
  children[62, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2003, 2007))],
                          data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2008,2012))]  ,
                          data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date%in% seq(2013, 2017))] ,
                          data_f$fertility_rate[which(data_f$age == "60-64" & data_f$date %in% seq(2018,2020))])
  # A 61 year old would have had 1 year in 40-44 year group in 2003 and five year
  # in 45-49 age year category in 2004 - 2008, 5 years in 50-54 age year category in 09-13
  children[61, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2003)], 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2004, 2008))],
                          data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2009,2013))]  ,
                          data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date%in% seq(2014, 2018))] ,
                          data_f$fertility_rate[which(data_f$age == "60-64" & data_f$date %in% seq(2019,2020))])
  
  children[60, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date %in% seq(2003, 2004))]  , 
                           data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2005, 2009))],
                           data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2010,2014))]  ,
                           data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date%in% seq(2015, 2019))] ,
                           data_f$fertility_rate[which(data_f$age == "60-64" & data_f$date == 2020)])
  
  
  children[59, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date %in% seq(2003, 2005))]  , 
                           data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2006, 2010))],
                           data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2011,2015))]  ,
                           data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date%in% seq(2016, 2020))])
  
  
  children[58, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date %in% seq(2003, 2006))], 
                           data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2007, 2011))],
                           data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2012,2016))]  ,
                           data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date%in% seq(2017, 2020))])
  
  
  children[57, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date %in% seq(2003, 2007))] , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2008, 2012))],
                          data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2013,2017))]  ,
                          data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date%in% seq(2018, 2020))])
  
  
  children[56, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2003)] , 
                           data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date %in% seq(2004, 2008))] , 
                           data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2009, 2013))],
                           data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2014,2018))]  ,
                           data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date%in% seq(2019, 2020))])
  
  children[55, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date %in% seq(2003, 2004))] , 
                           data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date %in% seq(2005, 2009))] , 
                           data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2010, 2014))],
                           data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2015,2019))]  ,
                           data_f$fertility_rate[which(data_f$age == "55-59" & data_f$date == 2020)])
  
  children[54, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2015)],
                          data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2016,2020))])
  
  children[53, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2016)], 
                          data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2017,2020))])
  
  
  children[52, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2017)],
                          data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2018,2020))])
  
  
  children[51, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2018)],
                          data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2019,2020))])
  
  
  children[50, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2019)],
                          data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date == 2020)])
  
  
  children[49, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2020)])
  
  children[48, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2020)])
  
  children[47, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2020)])
  
  children[46, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2020)])
  
  children[45, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2020)])
  
  children[44, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2020)])
  
  children[43, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2020)])
  
  children[42, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2020)])
  
  children[41, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2020)])
  
  children[40, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2020)])
  
  children[39, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2020)])
  
  children[38, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2020)])
  
  children[37, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2020)])
  
  children[36, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2020)])
  
  children[35, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2020)])
  
  children[34, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2020)])
  
  children[33, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2020)])
  
  children[32, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2003)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2020)])
  
  children[31, 17:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2004)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2020)])
  
  children[30, 16:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2005)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2020)])
  
  children[29, 15:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2006)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2019)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2020)])
  
  children[28, 14:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2007)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)])
  
  children[27, 13:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2008)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)])
  
  children[26, 12:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2009)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)])
  
  children[25, 11:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2010)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)])
  
  children[24, 10:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2011)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2012)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2014)]  , 
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2015)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2016)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2017)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)]  , 
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)])
  
  children[23, 9:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2012)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2013)]  ,
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2014)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2015)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2016)]  , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2017)]  , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)]  , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)]  , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)])
  
  children[22, 8:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2013)]  ,
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2014)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2015)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2016)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2017)]  , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)]  , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)]  , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)])
  
  children[21, 7:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2014)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2015)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2016)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2017)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2018)]  , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)]  , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)])
  
  children[20, 6:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2015)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2016)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2017)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2018)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2018)]  , 
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)])
  
  children[19, 5:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2016)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2017)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2018)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2018)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2018)])
  
  children[18, 4:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2017)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2018)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2019)]  , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2020)])
  
  children[17, 3:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2018)] , 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2019)], 
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2020)])
  
  children[16,2:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2019)]  , 
                        data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2020)])
  
  children[15,1] <- data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2020)]
  plot(apply(children, 1, sum), xlab = "Age of father", ylab = "Number of children")
  children = as.data.frame(children)
  names(children) = paste0(seq(0:17)-1, ' years')
  
  country
  
  write_csv(path = paste0('data/', country, '/child_raw_m.csv'), children)
  plot_c = as.data.frame(as.numeric(as.character(unlist(children))))
  plot_c$father_age = rep(1:100,18)
  plot_c$child_age =sort(rep(seq(18)-1, 100))
  setnames(plot_c, 1, 'prob')
  
  #ggplot(as.data.frame(plot_c), aes(x=child_age, y=father_age, fill=prob)) +
  #  geom_tile(color = "white")+
  #  theme(axis.text.x = element_text(angle = 90)) + 
  #  labs(x= "child age", y="father age") +
  #  scale_fill_gradient2(low = "yellow", high = "red")
}









