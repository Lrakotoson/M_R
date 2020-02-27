library(shiny)
library(DT)
library(rAmCharts)
library(plotly)

################################################
source("scripts/variables.R")
first <- head(trie$dates, 1)
last <- tail(trie$dates, 1)

################################################


# Define UI for application that draws a histogram
ui <- fluidPage(    
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.min.css"),
              tags$title(HTML("Coronavirus")),
              tags$style(HTML("
      @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
      h1 {
        font-family: 'Lobster', cursive;
        font-weight: 500;
        line-height: 1.1;
        color: #FF0000;
      }
    "))
              ),
    
    # Application title
    titlePanel(h1("Coronavirus")),
    
    # Sidebar with a slider input for number of bins 
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            textInput("titre",
                      "Titre du graphe",
                      value = "Graphique de l'evolution du nombre de"
                      
            ),
            dateRangeInput("daterange1", "Période",
                           start = first,
                           end   = last,
                           min   = first,
                           max   = last,
                           language = "fr",
                           separator = "-", 
                           format = "dd/mm/yyyy"
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
                        min = first, 
                        max = last,
                        value = last, 
                        timeFormat = "%d/%m"
            ),
            
            
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
                                          # rajout d'une image avec img()
                                          # elle doit etre dans www
                                          img(src = "photo.jpg", width = 450),
                                          tags$hr(),
                                 verbatimTextOutput("resume")
                                 
                        ),
                        tabPanel("Data",
                                 dataTableOutput("donnees")
                        ),
                        tabPanel("France",
                                 plotlyOutput("francemap")
                                 
                        ),
                        tabPanel("Italie",
                                 plotlyOutput("italiemap")
                                 
                        )
            )
            
        ))
)
