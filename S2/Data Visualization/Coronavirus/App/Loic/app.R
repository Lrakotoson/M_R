library(shiny)
library(tidyverse)

#######################################################
source("global.R")

fusion <- function(table, name){
  table<- table %>% 
    select(-c(1:4)) %>%
    replace(is.na(.), 0) %>%
    summarise_all(funs(sum)) %>%
    gather(key = "Date") %>%
    mutate(Date = as.Date(Date,format="%m/%d/%y")) %>%
    rename(!!name := value)
  return(table)
}

data <- fusion(T_cas, "Cas") %>%
  left_join(fusion(T_retablis, "Retablis"), "Date") %>% 
  left_join(fusion(T_morts, "Morts"), "Date")
#######################################################
ui <- fluidPage(
  titlePanel("Coronavirus"),
  sidebarLayout(
    sidebarPanel(
      textInput("titre",
                "Titre du graphe",
                value = "Graphique de l'evolution du nombre de"
                
      ),
      dateRangeInput("daterange1", "Période",
                     start = head(data$Date,1),
                     end   = tail(data$Date,1),
                     min   = start,
                     max = end,
                     language = "fr",
                     separator = "-", 
                     format = "dd/mm/yy"
      ),
    )
  )
)