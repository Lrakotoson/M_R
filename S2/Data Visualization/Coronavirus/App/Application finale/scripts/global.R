library(tidyverse)

T_cas <- read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv')
T_retablis <- read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv')
T_morts <- read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv')
geodata <- rgdal::readOGR("custom.geo.json")
####################################################################

clean <- function(data){
  data <- data %>% 
    rename(State = 'Province/State', Country = 'Country/Region') %>%
    mutate(State = coalesce(State, Country))
  return(data)
}

T_cas <<- clean(T_cas)
T_retablis <<- clean(T_retablis)
T_morts <<- clean(T_morts)
####################################################################

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
  #data <- data[which(!is.finite(data))] <- 0
  
  return(data)
}
####################################################################


if (!(require(jsonlite))) install.packages("jsonlite")
geocodeGratuit <- function(adresses) {
  # adresses est un vecteur contenant toutes les adresses sous forme de chaine de
  # caracteres
  nominatim_osm <- function(address = NULL) {
    ## details: http://wiki.openstreetmap.org/wiki/Nominatim fonction nominatim_osm
    ## propos?e par D.Kisler
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

####################################################################

brief <- function(group = NULL, t = ncol(T_cas) - 4){
  #' Renvoie un résumé à un instant t
  #' group: NULL, 'Country' ou 'Continent'
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

####################################################################

compare_data <- function(situation, Country1, Country2, t1, t2 = ncol(T_cas)-4){
  #' Renvoie un tible de la sitation de deux/un pays pour une periode
  #' Situation: Cas/Morts/Retablis, str
  #' Country1, Country2: Pays, str
  #' t1, t2: entier t1<2
  
  if (situation == "Retablis"){
    data <- cbind(
      T_retablis[,1:4],
      T_retablis[,5:ncol(T_morts)]*100/(T_cas[,5:ncol(T_morts)] + 1)
    )
  } else if (situation == "Morts"){
    data <- cbind(
      T_morts[,1:4],
      T_morts[,5:ncol(T_morts)]*100/(T_morts[,5:ncol(T_morts)] + T_retablis[,5:ncol(T_morts)] + 1)
    )
  } else {
    data <- T_cas
  }
  
  data <- data %>%
    filter(Country %in% c(Country1, Country2)) %>%
    select(c(1:4, all_of(t1+4):all_of(t2+4))) %>%
    group_by(Country)%>%
    select(-c(1:4))%>%
    summarise_all(funs(sum)) %>%
    replace(is.na(.), 0) %>%
    gather(key = Date, value = value, 2:ncol(.)) %>% 
    spread_(key = names(.)[1],value = 'value') %>%
    mutate(Date = as.Date(Date,format="%m/%d/%y")) %>%
    arrange(Date)
  
  return(data)
}

####################################################################

fitLM <- function(date, y, Country, pol = 1, pred = 10){
  #' Renvoie un tibble des prédictions du lm
  #' date: vecteur date
  #' y: vecteur y
  #' Country: nom du pays
  #' pol: lm polynomiale 1 à 3
  #' pred: nombre de jours à prédire
  
  x <- as.numeric(date)
  x_pred <- as.numeric(unique(c(date, date+pred)))
  y <- log(y)
  
  model <- lm(y~poly(x, pol))
  y_pred <- predict.lm(model, newdata = list(x = x_pred))
  
  name <- paste0("pred.", Country)
  pred <- tibble(Date = as.Date(x_pred, origin = "1970-01-01"),
                 !!name := exp(y_pred))
  
  return(pred)
}