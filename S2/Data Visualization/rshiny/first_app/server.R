library(shiny)
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ####### Plot #######
  bins <- reactive(input$bins)
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    colonne <- input$columns
    x    <- faithful[,colonne]
    bins <- seq(min(x), max(x), length.out = bins() + 1)
    color <- input$color
    titre <- input$titre
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = color, border = 'white', main = titre)
  })
  
  output$classe <- renderText({
    paste("Nombre de classes : ", bins())
  })
  
  
  
  ####### Summary #######
  output$resume <- renderPrint({
    summary(faithful)
  })
  
  ####### Data #######
  output$donnees <- renderDataTable({
    faithful
  })
}