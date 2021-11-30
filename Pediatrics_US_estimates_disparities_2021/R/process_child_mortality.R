# Process child & infant mortality

add_child_mortality = function(is_child_mortality_needed, country){
  children = read.csv(paste0('data/USA/', country, '_child_raw_m.csv'))
  plot_c = as.data.frame(as.numeric(as.character(unlist(children))))
  plot_c$father_age = rep(1:100,18)
  plot_c$child_age =sort(rep(seq(18)-1, 100))
  setnames(plot_c, 1, 'prob')
  
  if (is_child_mortality_needed){
    
    child_m_matrix = read.csv(paste0('data/USA/', 'child_mortality_rate.csv'))
    names(child_m_matrix) = paste0(seq(0:17)-1, 'years')
    child_and_m = as.matrix(children) * (1-as.matrix(child_m_matrix))
    child_and_m = as.data.frame(child_and_m)
    write_csv(path = paste0('data/USA/', country, '_child_all_m.csv'), child_and_m)
    
    plot_c_and_m = as.data.frame(as.numeric(as.character(unlist(child_and_m))))
    plot_c_and_m$father_age = rep(1:100,18)
    plot_c_and_m$child_age =sort(rep(seq(18)-1, 100))
    setnames(plot_c_and_m, 1, 'prob')
    
    
    ggplot(as.data.frame(plot_c_and_m), aes(x=child_age, y=father_age, fill=prob)) +
      geom_tile(color = "white")+
      theme(axis.text.x = element_text(angle = 90)) + 
      labs(x= "child age", y="father age") +
      scale_fill_gradient2(low = "yellow", high = "red")
    plot_c_and_m$gender = 'male'
    write_csv(path = paste0('data/USA/', country, '_child_all_list_m.csv'), plot_c_and_m)
  } else{
    child_and_m = copy(children)
    child_and_m = as.data.frame(child_and_m)
    write_csv(path = paste0('data/USA/', country, '_child_all_m.csv'), child_and_m)
    
    plot_c_and_m = copy(plot_c)
    plot_c_and_m$gender = 'male'
    write_csv(path = paste0('data/USA', country, '_child_all_list_m.csv'), plot_c_and_m)
    
  }
  child_and_m = read.csv(paste0('data/USA/', country, '_child_all_m.csv'))
  ddf = as.data.frame(apply(child_and_m, 1, sum))
  names(ddf) = 'children'
  ddf$gender = 'male'
  ddf$age = 1:100
  write_csv(path = paste0('data/USA/', country, '_children_m.csv'),ddf)
}

# Multiple countries
process_child_mortality = function(country, countries){
  # rate per children 
  child = read.csv(paste0('data/child_mortality_rate', '.csv'))
  child = child %>% filter(country == countries)
    
  child_m_matrix = matrix(rep(0, 100*18), nrow = 100)
  
  child_m_matrix[49:15,1] = child$mortality[which(child$year == 2020 & child$age == '0-4')]
  child_m_matrix[50:16,2] = child$mortality[which(child$year == 2019 & child$age == '0-4')]
  child_m_matrix[51:17,3] = child$mortality[which(child$year == 2018 & child$age == '0-4')]
  child_m_matrix[52:18,4] = child$mortality[which(child$year == 2017 & child$age == '0-4')]
  child_m_matrix[53:19,5] = child$mortality[which(child$year == 2016 & child$age == '0-4')]
  child_m_matrix[54:20,6] = child$mortality[which(child$year == 2015 & child$age == '5-9')]
  child_m_matrix[55:21,7] = child$mortality[which(child$year == 2014 & child$age == '5-9')]
  child_m_matrix[56:22,8] = child$mortality[which(child$year == 2013 & child$age == '5-9')]
  child_m_matrix[57:23,9] = child$mortality[which(child$year == 2012 & child$age == '5-9')]
  child_m_matrix[58:24,10] = child$mortality[which(child$year == 2011 & child$age == '5-9')]
  child_m_matrix[59:25,11] = child$mortality[which(child$year == 2010 & child$age == '10-14')]
  child_m_matrix[60:26,12] = child$mortality[which(child$year == 2009 & child$age == '10-14')]
  child_m_matrix[61:27,13] = child$mortality[which(child$year == 2008 & child$age == '10-14')]
  child_m_matrix[62:28,14] = child$mortality[which(child$year == 2007 & child$age == '10-14')]
  child_m_matrix[63:29,15] = child$mortality[which(child$year == 2006 & child$age == '10-14')]
  child_m_matrix[64:30,16] = child$mortality[which(child$year == 2005 & child$age == '15-19')]
  child_m_matrix[65:31,17] = child$mortality[which(child$year == 2004 & child$age == '15-19')]
  child_m_matrix[66:32,18] = child$mortality[which(child$year == 2003 & child$age == '15-19')]
  
  
  
  child_m_matrix = as.data.frame(child_m_matrix)
  names(child_m_matrix) = paste0(seq(0:17)-1, 'years')
  
  write_csv(path = paste0('data/USA/', 'child_mortality_rate.csv'), child_m_matrix)
}  




