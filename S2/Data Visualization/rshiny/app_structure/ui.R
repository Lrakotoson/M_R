library(shiny)
library(colourpicker)
library(rAmCharts)

# Define UI for application that draws a histogram
shinyUI(
  # navbarPage
  navbarPage("Premiers pas avec shiny",
             
             # premier onglet Data
             tabPanel("Data", 
                      # table
                      dataTableOutput("table"),
                      # summary
                      verbatimTextOutput("summary")
             ), 
             
             # second onglet Visualisation
             tabPanel("Visualisation", 
                      
                      # Sidebar with a slider input for number of bins 
                      fluidRow(
                        column(width = 3,
                               wellPanel(
                                 sliderInput("bins",
                                             "Number of bins:",
                                             min = 1,
                                             max = 50,
                                             value = 30),
                                 
                                 # input pour la couleur
                                 colourInput(inputId = "color", label = "Couleur :", value = "purple"),
                                 
                                 # titre du graphique
                                 textInput(inputId = "titre", label = "Titre :", value = "Histogramme"),
                                 
                                 # selection de la colonne
                                 radioButtons(inputId = "var", label = "Variable : ", choices = colnames(faithful)),
                                 
                                 # action button
                                 actionButton("go", "Compute")
                               )
                          
                        ),
                        
                        # Show a plot of the generated distribution
                        column(width = 9,
                               tabsetPanel(type = "tabs", id = "viz",
                                           tabPanel("Histogramme",
                                                    #plotOutput("distPlot"),
                                                    amChartsOutput("distPlot")
                                                    ,
                                                    # classes (div centrée)
                                                    div(textOutput("n_bins"), align = "center")
                                           ),
                                           tabPanel("Boxplot",
                                                    plotOutput("boxPlot")

                                           )
                                 
                               )
                          
                        )
                      )
             )
  )
)