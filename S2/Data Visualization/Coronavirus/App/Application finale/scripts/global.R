library(tidyverse)

T_cas <- read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv')
T_retablis <- read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv')
T_morts <- read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv')


clean <- function(data){
  data <- data %>% 
    rename(State = 'Province/State', Country = 'Country/Region') %>%
    mutate(State = coalesce(State, Country))
  return(data)
}

T_cas <<- clean(T_cas)
T_retablis <<- clean(T_retablis)
T_morts <<- clean(T_morts)

latest <- function(t = ncol(T_cas) - 4){
  #' Retourne les données les plus récentes à l'instant t
  #' t: temps, entier >= 1
  
  if (t > ncol(T_morts) - 4 | t > ncol(T_retablis) - 4){
    t <- min(ncol(T_morts), ncol(T_retablis)) - 4
  }
  
  data <- T_cas %>% 
    select(1:4, t+4) %>% 
    rename(Cas = tail(names(.), 1)) %>% 
    left_join((T_retablis %>% 
                 select(1, t+4) %>% 
                 rename(Retablis = tail(names(.), 1))
    )
    ) %>% 
    left_join((T_morts %>% 
                 select(1, t+4) %>% 
                 rename(Morts = tail(names(.), 1))
    )
    )
  return(data)
}

brief <- function(group = NULL, t = ncol(T_cas) - 4){
  #' Renvoie un résumé à un instant t
  #' group: NULL ou 'Country'
  #' t: temps, entier >= 1
  
  if(is_empty(group)){
    data <- latest(t) %>% 
      mutate(group = 'Monde')
    group <- 'group'
  } else {
    data <- latest(t) %>% 
      rename('group' = group)
  }
  data <- data %>% 
    group_by(group) %>% 
    summarise(Cas = sum(Cas, na.rm = T),
              Retablis = sum(Retablis, na.rm = T),
              Morts = sum(Morts, na.rm = T))
  return(data)
}
# pour recuperer les longitudes et latitudes de la france et de l'italie:
if (!(require(jsonlite))) install.packages("jsonlite")
geocodeGratuit <- function(adresses) {
  # adresses est un vecteur contenant toutes les adresses sous forme de chaine de
  # caracteres
  nominatim_osm <- function(address = NULL) {
    ## details: http://wiki.openstreetmap.org/wiki/Nominatim fonction nominatim_osm
    ## propos�e par D.Kisler
    if (suppressWarnings(is.null(address))) 
      return(data.frame())
    tryCatch(d <- jsonlite::fromJSON(gsub("\\@addr\\@", gsub("\\s+", "\\%20", 
                                                             address), "http://nominatim.openstreetmap.org/search/@addr@?format=json&addressdetails=0&limit=1")), 
             error = function(c) return(data.frame()))
    if (length(d) == 0) 
      return(data.frame())
    return(c(as.numeric(d$lon), as.numeric(d$lat)))
  }
  tableau <- t(sapply(adresses, nominatim_osm))
  colnames(tableau) <- c("lon", "lat")
  return(tableau)
}


