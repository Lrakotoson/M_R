library(shiny)
library(DT)
library(rAmCharts)
library(plotly)
##########################################
source("scripts/variables.R")
#plotlyStyle <- "carto-positron"
plotlyStyle <- "carto-darkmatter"
ramChartStyle <- "dark"
first <- head(trie$dates, 1)
last <- tail(trie$dates, 1)
##########################################


# Define server logic required to draw a histogram
server <- function(input, output) {
  
    ##### Reactive #####
  rangeDate <- reactive({seq.Date(as.Date(first), as.Date(last), by = "day")})
  beg <- reactive({match(as.Date(input$Comparedate[1]), rangeDate())})
  end <- reactive({match(as.Date(input$Comparedate[2]), rangeDate())})
  Country1 <- reactive({input$Country1})
  Country2 <- reactive({input$Country2})
    
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
               col =   if (colonne == "Morts"){
                 color <- "sandybrown"
               } else if (colonne == "Retablis"){
                 color <- "seagreen"
               } else {
                 color <- "red"
               },
               xlab = "",
               main=paste(input$titre,colonne),
               ylab = paste("Nombre de", colonne),color="white",
               theme = ramChartStyle)
    })
    
    output$worldmap <- renderPlotly({
      releve <- match(as.Date(input$dateslider), rangeDate())
      worldmap <- map_evolution("World", releve, input$columns, F, T)
      worldmap
      })
    
    output$francemap <- renderPlotly(
        worldmap <- latest() %>%
            plot_ly(
                lat = ~Lat,
                lon = ~Long,
                marker = list(color = input$color, size = ~log(1+Cas), sizeref=0.1, opacity=0.4),
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
                marker = list(color = input$color, size = ~log(1+Cas), sizeref=0.1, opacity=0.4),
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
    output$hist <- renderAmCharts({
      colonne <- input$columns
      color <- input$color
      data <- trie %>% 
        select(dates, y = colonne) %>% 
        filter(dates>=input$daterange1[1] & dates<=input$daterange1[2])
      
      
      amHist(x = data$y,theme = ramChartStyle,col="rainbow",main= paste("Nombre de",input$columns),xlab=input$columns)
    })
    
    ###### Compare ######
    
    output$comparemap <- renderLeaflet({
      comparemap(Country1(), Country2(), end())
    })
    
    output$Compare_cas <- renderAmCharts({
      compare_situation(
        "Cas",
        Country1(), Country2(),
        beg(), end(),
        input$Caslog,
        as.integer(input$Casreg),
        as.integer(input$Caspred)
      )
    })
    
    output$Compare_morts <- renderAmCharts({
      compare_situation(
        "Morts",
        Country1(), Country2(),
        beg(), end(),
        input$Mortslog,
        as.integer(input$Mortsreg),
        as.integer(input$Mortspred)
      )
    })
    
    output$Compare_retablis <- renderAmCharts({
      compare_situation(
        "Retablis",
        Country1(), Country2(),
        beg(), end(),
        input$Retablislog,
        as.integer(input$Retablisreg),
        as.integer(input$Retablispred)
      )
    })
    
    output$Compare_actifs <- renderAmCharts({
      compare_situation(
        "Actifs",
        Country1(), Country2(),
        beg(), end(),
        logscale = F,
        reg = 0, pred = 0
      )
    })
    
    output$Compare_letalite <- renderAmCharts({
      compare_situation(
        "Letalite",
        Country1(), Country2(),
        beg(), end(),
        input$Mortslog,
        as.integer(input$Mortsreg),
        as.integer(input$Mortspred)
      )
    })
    
    output$Compare_nouveaux <- renderAmCharts({
      compare_new(
        Country1(), Country2(),
        beg(), end()
      )
    })
    
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


