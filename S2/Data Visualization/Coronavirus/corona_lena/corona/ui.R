library(shiny)
library(DT)
library(rAmCharts)

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
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = nrow(sum_cas),
                        value = 15
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
                         
            )
        ),
        # Show a plot of the generated distribution
        mainPanel(
            
            # Output with tabsets
            tabsetPanel(type = "tabs",
                        tabPanel("Plot",
                                 plotOutput("distPlot"),
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