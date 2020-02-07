library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Premiers pas avec shiny"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      textInput("titre",
                "Titre de l'histogramme",
                value = "Histogramme"
                
        
      ),
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30
      ),
      colourpicker::colourInput("color",
                  "Couleur :",
                  value = "black",
                  showColour = c("both", "text", "background"),
                  palette = c("square", "limited")
      ),
      radioButtons("columns",
                   "Colonne :",
                   choices = colnames(faithful)
        
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