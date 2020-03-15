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
      column(3,
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
             ),
             colourpicker::colourInput("color",
                                       "Couleur :",
                                       value = "purple",
                                       showColour = c("both", "text", "background"),
                                       palette = c("square", "limited")
             )
      ),
      column(3,
             h3("Analyse Cas"),
             checkboxInput(
               "Caslog", "Echelle logarithmique nombre Cas", value = T
             ),
             selectInput(
               "Casreg",
               "Régression Cas",
               c("Aucune" = 0,
                 "Régression linéaire" = 1,
                 "Régression poly 2" = 2,
                 "Régression poly 3" = 3
                 )
             ),
             textInput(
               "Caspred",
               "Nombre jours à prédire",
               value = 0
             )
      ),
      column(3,
             h3("Analyse Décès"),
             checkboxInput(
               "Mortslog", "Echelle logarithmique Mortalité", value = F
             ),
             selectInput(
               "Mortsreg",
               "Régression Mortalité",
               c("Aucune" = 0,
                 "Régression linéaire" = 1,
                 "Régression poly 2" = 2,
                 "Régression poly 3" = 3
               )
             ),
             textInput(
               "Mortspred",
               "Nombre jours à prédire",
               value = 0
             )
      ),
      column(3,
             h3("Analyse Rétablis"),
             checkboxInput(
               "Retablislog", "Echelle logarithmique Retablis", value = F
             ),
             selectInput(
               "Retablisreg",
               "Régression Retablis",
               c("Aucune" = 0,
                 "Régression linéaire" = 1,
                 "Régression poly 2" = 2,
                 "Régression poly 3" = 3
               )
             ),
             textInput(
               "Retablispred",
               "Nombre jours à prédire",
               value = 0
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
    ),
    fluidRow(
      column(
        6,
        amChartsOutput("Compare_cas")
      ),
      column(
        6,
        amChartsOutput("Compare_actifs")
      )
    ),
    fluidRow(
      column(
        6,
        amChartsOutput("Compare_morts")
      ),
      column(
        6,
        amChartsOutput("Compare_letalite")
      )
    ),
    fluidRow(
      column(
        6,
        amChartsOutput("Compare_retablis")
      ),
      column(
        6,
        amChartsOutput("Compare_nouveaux")
      )
    ),
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
