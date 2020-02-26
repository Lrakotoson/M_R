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
    
    #  output$worldmap <- renderPlotly( 
    #    worldmap <- latest() %>% 
    #      plot_ly(
    #        lat = ~Lat,
    #        lon = ~Long,
    #        marker = list(color = 'red', size = ~log(1+Cas), sizeref=0.1, opacity=0.4),
    #        type = 'scattermapbox',
    #        text = ~State,
    #        hovertext = ~Cas,
    #        hovertemplate = paste(
    #          "<b>%{text}</b><br><br>",
    #          "Nombre de cas: %{hovertext}",
    #          "<extra></extra>"
    #        )) %>%
    #      layout(
    #        mapbox = list(
    #          style = 'carto-positron',
    #          zoom = 1.2,
    #          center = list(lon = 18, lat= 35)),
    #        margin = list(
    #          l = 0, r = 0,
    #          b = 0, t = 0,
    #          pad = 0
    #        )
    #      )
    #  )  
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