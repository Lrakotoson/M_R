library(tidyverse)

cas <- read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/time_series/time_series_2019-ncov-Confirmed.csv')
retablis <- read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/time_series/time_series_2019-ncov-Recovered.csv')
morts <- read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/time_series/time_series_2019-ncov-Deaths.csv')


clean <- function(data){
  data <- data %>% 
    rename(State = 'Province/State', Country = 'Country/Region') %>%
    mutate(State = coalesce(State, Country))
  return(data)
}
  
cas <<- clean(cas)
retablis <<- clean(retablis)
morts <<- clean(morts)

latest <- function(){
  data <- cas %>% 
    rename(Cas = tail(names(.), 1)) %>% 
    select(State, Country, Lat, Long, Cas) %>% 
    left_join((retablis %>% 
                 rename(Retablis = tail(names(.), 1)) %>% 
                 select(State, Retablis))
    ) %>% 
    left_join((morts %>% 
                 rename(Morts = tail(names(.), 1)) %>% 
                 select(State, Morts)))
  return(data)
}

  
