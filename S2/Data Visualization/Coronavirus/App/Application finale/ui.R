library(shiny)
library(DT)
library(rAmCharts)

source("scripts/variables.R")


# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Coronavirus"),
    
    # Sidebar with a slider input for number of bins 
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            textInput("titre",
                      "Titre du graphe",
                      value = "Graphique de l'evolution du nombre de"
                      
            ),
            dateRangeInput("daterange1", "Période",
                           start = head(trie$dates, 1),
                           end   = tail(trie$dates, 1),
                           min   = head(trie$dates, 1),
                           max   = tail(trie$dates, 1),
                           language = "fr",
                           separator = "-", 
                           format = "dd/mm/yy"
            ),
            colourpicker::colourInput("color",
                                      "Couleur :",
                                      value = "blue",
                                      showColour = c("both", "text", "background"),
                                      palette = c("square", "limited")
            ),
            radioButtons("columns",
                         "Colonne :",
                         choices = colnames(data_sum)[2:4]
                         
            ), 
            sliderInput(inputId = "dateslider", label = "Choix de la date", 
                        min = head(trie$dates, 1), 
                        max = tail(trie$dates, 1),
                        value = tail(trie$dates, 1), 
                        timeFormat = "%d/%m"
            )
        ),
        # Show a plot of the generated distribution
        mainPanel(
            
            # Output with tabsets
            tabsetPanel(type = "tabs",
                        tabPanel("Plot",
                                 amChartsOutput("distPlot"),
                                 plotlyOutput("worldmap"),
                                 textOutput("classe")
                                 
                        ),
                        
                        tabPanel("Summary",
                                 verbatimTextOutput("resume")
                                 
                        ),
                        tabPanel("Data",
                                 dataTableOutput("donnees")
                        )
                        
            )
            
        )
    )
    
)