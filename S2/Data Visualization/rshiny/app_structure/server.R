library(shiny)
library(rAmCharts)

# Session

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  classe <- reactive(input$bins)
  var <- reactive(input$var)
  color <- reactive(input$color)
  titre <- reactive(input$titre)
  
  output$distPlot <- renderAmCharts({
    
    # generate bins based on input$bins from ui.R
    x    <- faithful[, var()] 
    bins <- round(seq(min(x), max(x), length.out = classe() + 1), 2)
    
    # draw the histogram with the specified number of bins
    #hist(x, breaks = bins, col = color(), border = 'white', main = titre())
    
    # use amHist
    amHist(x, control_hist = list(breaks = bins),
           col = color(), main = titre(),
           export = T, zoom = T)

    
  })
  
  output$boxPlot <- renderPlot({
    x <- faithful[, var()]
    boxplot(x, col = color())
  })
  
  # summary
  output$summary <- renderPrint({
    summary(faithful)
  })
  
  # table
  output$table <- renderDataTable({
    faithful
  })
  
  # nombre de classe
  output$n_bins <- renderText({
    paste("Nombre de classes : ", classe())
  })
  
  observeEvent(input$go, {
    updateTabsetPanel(session, inputId = "viz", selected = "Histogramme")
  }
  )
  
})