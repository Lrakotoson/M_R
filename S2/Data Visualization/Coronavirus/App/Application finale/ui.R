library(shiny)
library(DT)
library(rAmCharts)
library(plotly)
library(leaflet)

################################################
source("scripts/variables.R")
first <- head(trie$dates, 1)
last <- tail(trie$dates, 1)

################################################

# Define UI for application that draws a histogram
ui <- navbarPage(
  theme = "bootstrap.min.css",
  title = h1("Coronavirus",
             style = "font-family: 'Lobster', cursive; font-weight: 500; line-height: 1.1; color: #FF0000; margin:0;"
             ),
  selected = "Evolution",
  tags$head(tags$style(HTML("@import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');"))),
  
  ##---------------------------EVOLUTION-----------------------------------##
    tabPanel(
    "Evolution",
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

            radioButtons("columns",
                         "Colonne :",
                         choices = colnames(data_sum)[2:4]
                         
            ),
            sliderInput(inputId = "dateslider", label = "Choix de la date", 
                        min = first, 
                        max = last,
                        value = last, 
                        timeFormat = "%d/%m"
            )
            
            
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
          amChartsOutput("distPlot"),
          br(),hr(),br(),
          plotlyOutput("worldmap"),
          br(),hr(),
          textOutput("classe")
          ))),
  tabPanel(
    "Comparaisons",
    fluidRow(
      column(4,
             selectInput(
               "Country1", "Pays A",
               c(brief("Country")$group, "Monde"),
               "France"
             ),
             selectInput(
               "Country2", "Pays B",
               c(brief("Country")$group, "Monde"),
               "Italy"
             ),
             sliderInput(
               "Comparedate", "Domaine",
               min = first, 
               max = last,
               value = c(first, last), 
               timeFormat = "%d/%m"
             )
      ),
      column(5,
             h3("Analyse des cas"),
             helpText(
               "Focus sur l'évolution du nombre de Cas.",
               "L'échelle logarithmique permet de visualiser",
               "de façon linéaire"
             ),
             checkboxInput(
               "Caslog", "Echelle logarithmique", value = F
             ),
             selectInput(
               "Regresor",
               "Régression",
               c("Aucune", "Régression linéaire")
             )
      ),
      column(3,
             colourpicker::colourInput("color",
                                       "Couleur :",
                                       value = "purple",
                                       showColour = c("both", "text", "background"),
                                       palette = c("square", "limited")
             )
      )
    ),hr(),br(),
    tabsetPanel(
      tabPanel("Compare",
               leafletOutput("comparemap")
               
      ),
      tabPanel("France",
               plotlyOutput("francemap")
               
      ),
      tabPanel("Italie",
               plotlyOutput("italiemap")
      )
    )
  ),
  tabPanel(
    "Données",
    tabsetPanel(
      tabPanel(
        "Data",
        dataTableOutput("donnees")
    ),
    tabPanel(
      "Summary",
      HTML('<center><img src="photo.jpg" style="width:450"></center><br><hr><br>'),
      verbatimTextOutput("resume"),
      amChartsOutput("hist")
      )
  ))

)
