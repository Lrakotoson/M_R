library(shiny)
library(DT)
library(rAmCharts)
library(plotly)
##########################################
source("scripts/variables.R")
#plotlyStyle <- "carto-positron"
plotlyStyle <- "carto-darkmatter"
ramChartStyle <- "dark"
##########################################


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    ####### Plot #######
    
    output$distPlot <- renderAmCharts({
        colonne <- input$columns
        color <- input$color
        data <- trie %>% 
            select(dates, y = colonne) %>% 
            filter(dates>=input$daterange1[1] & dates<=input$daterange1[2])
      
        
        amPlot(x = as.character(data$dates, format = "%d/%m"), data$y,
               type = "sl",
               fill_alphas = 0.1,
               col = color,
               xlab = "",
               main=paste(input$titre,colonne),
               ylab = paste("Nombre de", colonne),color="black",
               theme = ramChartStyle)
    })
    
    output$worldmap <- renderPlotly({
      rangeDate <- seq(first, last, by = "day")
      releve <- match(as.Date(input$dateslider), rangeDate)
      worldmap <- map_evolution("World", releve, input$columns, F, T)
      worldmap
      })
    
    output$francemap <- renderPlotly(
        worldmap <- latest() %>%
            plot_ly(
                lat = ~Lat,
                lon = ~Long,
                marker = list(color = 'red', size = ~log(1+Cas), sizeref=0.1, opacity=0.4),
                type = 'scattermapbox',
                text = ~State,
                hovertext = ~Cas,
                hovertemplate = paste(
                    "<b>%{text}</b><br><br>",
                    "Nombre de cas: %{hovertext}",
                    "<extra></extra>"
                )) %>%
            layout(
                mapbox = list(
                    style = plotlyStyle,
                    zoom = 4.5,
                    center = list(lon = france$lon, lat= france$lat)),
                margin = list(
                    l = 0, r = 0,
                    b = 0, t = 0,
                    pad = 0
                )
            ))
    
    output$italiemap <- renderPlotly(
        worldmap <- latest() %>%
            plot_ly(
                lat = ~Lat,
                lon = ~Long,
                marker = list(color = 'red', size = ~log(1+Cas), sizeref=0.1, opacity=0.4),
                type = 'scattermapbox',
                text = ~State,
                hovertext = ~Cas,
                hovertemplate = paste(
                    "<b>%{text}</b><br><br>",
                    "Nombre de cas: %{hovertext}",
                    "<extra></extra>"
                )) %>%
            layout(
                mapbox = list(
                    style = plotlyStyle,
                    zoom = 5,
                    center = list(lon = italie$lon, lat= italie$lat)),
                margin = list(
                    l = 0, r = 0,
                    b = 0, t = 0,
                    pad = 0
                )
            ))
    
    
    ####### Summary #######
    output$resume <- renderPrint({
        summary(data_sum)
    })
    
    ####### Data #######
    output$donnees <- renderDataTable({
      datatable(data_sum,
                options = list(pageLength = 25)
                ) %>%
        formatStyle("Cas",color= "black") %>%
        formatStyle("Morts",color= "black") %>%
        formatStyle("Retablis",color= "black") %>%
        formatStyle("dates",color= "black")
        
    
    
    })
}


