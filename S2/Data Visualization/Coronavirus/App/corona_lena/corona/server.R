library(shiny)
library(DT)
library(rAmCharts)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    ####### Plot #######
    
    bins <- reactive(input$bins)
    
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        colonne <- input$columns
        x <- trie[,colonne]
        bins <- seq(min(x), max(x), length.out = bins() + 1)
        color <- input$color
        titre <- input$titre
        
        # draw the histogram with the specified number of bins
        plot(trie$dates,x,xlab="",ylab="",col = color,type="l",main=paste("Evolution du nombre de",input$columns ))
    })
    
    
    ####### Summary #######
    output$resume <- renderPrint({
        summary(data_sum)
    })
    
    ####### Data #######
    output$donnees <- renderDataTable({datatable(
        as.data.frame(data_sum)
    )
    })
}