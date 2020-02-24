library(shiny)
library(DT)
library(rAmCharts)

source("scripts/variables.R")


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    ####### Plot #######
    
    output$distPlot <- renderAmCharts({
        colonne <- input$columns
        color <- input$color
        data <- trie %>% 
            select(dates, y = colonne) %>% 
            filter(dates>=input$daterange1[1] & dates<=input$daterange1[2])
        
        amPlot(x = as.character(data$dates, format = "%d/%m"), data$y, type = "l", col = color, xlab = "", ylab = paste("Nombre de", colonne))
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