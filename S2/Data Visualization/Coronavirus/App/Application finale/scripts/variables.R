source('scripts/global.R')
library(tidyverse)
library(rAmCharts)
library(rjson)
library(plotly)
library(leaflet)

T_cas_dates <- T_cas[-c(1,2,3,4)]
sum_cas <- as.data.frame(apply(T_cas_dates, 2, sum))
dates <- rownames(sum_cas)
data_sum_cas<- cbind(sum_cas,dates)
names(data_sum_cas)[1] <- "cas"


T_mort_dates <- T_morts[-c(1,2,3,4)]
sum_mort <- as.data.frame(apply(T_mort_dates, 2, sum))
data_sum_mort <- cbind(sum_mort,dates)
names(data_sum_mort)[1] <- "mort"

T_retablis_dates <- T_retablis[-c(1,2,3,4)]
sum_ret <- as.data.frame(apply(T_retablis_dates, 2, sum))
data_sum_ret <- cbind(sum_ret,dates)
names(data_sum_ret)[1] <- "retablis"

data_sum <- merge(data_sum_cas,data_sum_mort,key="dates")
data_sum <- data_sum %>%
  merge(data_sum_ret,key="dates") %>%
  rename(Cas = cas,
         Retablis = retablis,
         Morts = mort)
data_sum$dates <- as.character(data_sum$dates)
data_sum$dates <- as.Date(data_sum$dates,"%m/%d/%y")
library(data.table)

trie <- setkey(as.data.table(data_sum),dates)
trie <- as.data.frame(trie)

france <- as.data.frame(geocodeGratuit("France"))
italie <- as.data.frame(geocodeGratuit("Italie"))

#############################################################

map_evolution <- function(region, time, colonne, titre = T, main = F){
  #' Renvoie un plotly en de la région à un moment t
  #' region: Région/Continent
  #' time: argument t de latest, t >= 1
  #' colonne: Cas/Retablis/Morts
  #' titre: bool, ajout du titre en fonction de la variable
  
  if (region == "Asia"){
    long <- 94
    lat <- 40
    zoom <- 1.6
  }else if (region == "North America"){
    long <- -102
    lat <- 55
    zoom <- 1.4
  }else if (region == "South America"){
    long <- -59
    lat <- -18
    zoom <- 1.2
  }else if (region == "Europe"){
    long <- 14
    lat <- 54
    zoom <- 2
  }else if (region == "Australia"){
    long <- 134
    lat <- -28
    zoom <- 2.6
  }else if (region == "Africa"){
    long <- 26
    lat <- 2
    zoom <- 1.7
  }else{
    long <- 18
    lat <- 35
    zoom <- ifelse(main, 1.2, 0.2)
  }
  
  if (colonne == "Morts"){
    color <- "sandybrown"
  } else if (colonne == "Retablis"){
    color <- "seagreen"
  } else {
    color <- "red"
  }
  
  plot <- latest(time) %>%
    mutate(info = paste0("Nombre de cas: ", Cas,
                         "\n Retablis: ", Retablis,
                         "\n Deces: ", Morts),
           value = .[[colonne]]
    ) %>%
    plot_ly(
      lat = ~Lat,
      lon = ~Long,
      marker = list(color = color, size = ~log(1+value), sizeref=0.1, opacity=0.4),
      type = 'scattermapbox',
      text = ~State,
      hovertext = ~info,
      hovertemplate = paste(
        "<b>%{text}</b><br><br>",
        "%{hovertext}",
        "<extra></extra>"
      )) %>%
    layout(
      title = ifelse(titre,
                     paste0("\n<b>", region,
                            "</b>: <b style='color:",color,"'>", colonne,
                            "</b> au <b>", as.Date(names(T_cas)[time+4], "%m/%d/%y"),
                            "</b>"),
                     ""),
      mapbox = list(
        style = 'carto-darkmatter',
        zoom = zoom,
        center = list(lon = long, lat= lat)),
      margin = list(
        l = 0, r = 0,
        b = 0, t = 0,
        pad = 0
      )
    )
  return(plot)
}
#############################################################

comparemap <- function(Country1, Country2 = "Monde", t = ncol(T_cas)){
  #' Renvoie un leaflet du monde avec les pays sélectionnés
  #' Country1, Country2: pays
  #' t: temps, entier >= 1
  
  level_key <- c(
    'Congo (Kinshasa)' = 'Democratic Republic of the Congo',
    "Cote d'Ivoire" = "Ivory Coast",
    "Czechia" = "Czech Republic",
    "Korea, South" = "South Korea",
    "North Macedonia" = "Macedonia",
    "Serbia" = "Republic of Serbia",
    "Taiwan*" = "Taiwan",
    "US" = "United States of America"
  )
  
  
  if(Country1 %in% names(level_key)){
    Country1 <- level_key[Country1]
  }
  if(Country2 %in% names(level_key)){
    Country2 <- level_key[Country2]
  }
  
  base <- geodata
  stats <- brief("Country", t)
  stats$group <- recode(stats$group, !!!level_key)
  
  base@data <- tibble(group = base$admin) %>%
    left_join(stats) %>%
    rename(admin = group) %>%
    filter(admin == Country1 | admin == Country2) %>%
    right_join(base@data) %>%
    mutate(admin = factor(admin))
  
  pal <- colorNumeric("viridis", NULL)
  
  plot <- leaflet(base) %>%
    addProviderTiles("CartoDB.DarkMatter") %>% 
    addPolygons(
      stroke = F, smoothFactor = 0.3,
      fillOpacity = 1,
      fillColor = ~pal(Cas),
      label = ~paste(admin, "|", Cas, "cas",
                     Morts, "morts", Retablis, "retablis")
    ) %>% 
    addLegend(
      pal = pal,
      values = ~(Cas)
    )
  
  return(plot)
}

#############################################################

compare_situation <- function(Situation, Country1, Country2, t1, t2, logscale = F, reg = 0, pred = 10){
  #' Renvoie un rAmCharts de l'évolution de la situation avec une régression
  #' Situation: Cas/ Retablis/ Morts
  #' Country1, Country2: pays
  #' t1, t2: période, entier t1 < t2
  #' reg: {0,1,2,3} reg polynomial, si 0 aucune reg
  #' pred: nb jours à prédire si reg > 0
  
  data <- compare_data(Situation, Country1, Country2, t1, t2)
  
  if (reg > 0){
    data <- data %>%
      right_join(fitLM(data$Date, data[[Country1]], Country1, reg, pred)) %>%
      right_join(fitLM(data$Date, data[[Country2]], Country2, reg, pred))
  }
  
  if (logscale){
    data[,2:ncol(data)] <- log(data[,2:ncol(data)] +1)
  }
  
  data <- data %>%
    mutate(Date = as.character(Date, format = "%d/%m/%Y"))%>%
    replace(., is.na(.), 0)
  
  
  plot <- amPlot(
    data$Date, data[[Country1]],
    parseDates = T, dataDateFormat = "DD/MM/YYYY",
    col = "#f1c40f", lwd = 2, type = "smoothedLine",
    zoom = T, legend = T, title = Country1,
    ylim = c(min(data[,2:3]), max(data[,2:3])),
    ylab = Situation, xlab = "Temps",
    main = paste("Situation:", Situation),
    theme = "dark"
  ) %>%
    amLines(
      data[[Country2]], col = "#C4E538",
      type = "smoothedLine", title = Country2
    )
  
  if (reg > 0){
    plot <- plot %>% 
      amLines(
        data[[paste0("pred.",Country1)]],
        col = "#ecf0f1", type = "line",
        title = paste0("pred.",Country1)
      ) %>% 
      amLines(
        data[[paste0("pred.",Country2)]],
        col = "#f7f1e3", type = "line",
        title = paste0("pred.",Country2)
      )
  }
  
  return(plot)
}
