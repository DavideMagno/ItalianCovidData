source(here::here("R/ItalianMap.R"))
source(here::here("R/Analysis.R"))

library(shiny)

shinyServer(function(input, output, session) {

    field <- reactive({
        if (grepl("Region", input$type)) {
            input$field
        } else {
            "Total"
        }
    })
    
    logscale <- reactive({if(input$log) "log" else "linear"})
    
    
    # Map Setup

    ItalyMap <- reactive({FilterAndPrepareToPlot(Data, input$date, input$type, 
                                                 input$field)})
    
    output$map <- leaflet::renderLeaflet({
        leaflet::leaflet() %>% 
            leaflet::addProviderTiles("Esri.WorldTerrain") %>% 
            leaflet::setView(10, 41.879, zoom = 5)})
    
    observe({
        italy <- ItalyMap()
        #create a color palette to fill the polygons
        bin <- seq(0, max(italy$cases) + 1, length.out = 10)
        pal <- leaflet::colorBin("Reds", bins=bin)
        
        #create a pop up (onClick)
        polygon_popup <- paste0(paste0("<strong>",input$type,": </strong>"), 
                                italy$name, "<br>", 
                                paste0("<strong>", field()," cases: </strong>"), 
                                italy$cases)
        leaflet::leafletProxy("map", data = italy, session) %>% 
            leaflet::clearShapes(.) %>% 
            leaflet::addPolygons(fillColor= ~pal(cases),
                                 fillOpacity = 0.4, 
                                 weight = 2, 
                                 color = "grey",
                                 popup = polygon_popup,
                                 label = ~name,
                                 layerId = ~name) 
    })
    
    selection <- reactiveValues(n = NA)
    
    observeEvent(input$map_shape_click, {
        selection$n <- input$map_shape_click$id
    })
    observeEvent({
        input$map_click
        input$type
    }, {
        selection$n <- NA
    })
    
    name <- reactive({if(is.na(selection$n)) "Italy" else selection$n})
    
    
    plot.data <- reactive({
        if (grepl("Region", input$type)) {
            field <- field()
        } else {
            field <- "Total"
        }
        if (is.na(selection$n)) {
            plot.data <- ExtractDataXRegion(Data$covid.regions, "Region",
                                            name(), field)
        } else {
            if(grepl("Region", input$type)){
                data <- Data$covid.regions
            } else {
                data <- Data$covid.province
            }
            plot.data <- ExtractDataXRegion(data, input$type, name(), field())
        }
        return(plot.data)
    })
    
    observe({
        if (grepl("Total", field())) {
            text <- paste(field(), "Positive Cases")
        } else {
            text <- paste("Positive Cases", field())
        }
        output$dynamic <- plotly::renderPlotly({
            plot.data() %>%
                as.data.frame() %>% 
                plotly::plot_ly(x = ~Date) %>%
                plotly::add_trace(y = ~Total, type = "scatter",
                                  mode = "lines+markers") %>%
                plotly::layout(
                    title = paste("Dynamic in", name()),
                    yaxis = list(title = text,
                                 type = logscale())
                ) %>%
                plotly::config(displayModeBar = FALSE)
        })
    })
    
    # =========== DATA ANALYSIS ===========
    
    observe({
        provinces <- if (is.null(input$regions)) character(0) else {
            dplyr::filter(Data$covid.province, Region %in% input$regions) %>%
                `$`('Province') %>%
                unique() %>%
                sort()
        }
        stillSelected <- isolate(input$provinces[input$provinces %in% provinces])
        updateSelectizeInput(session, "provinces", choices = provinces,
                             selected = stillSelected, server = TRUE)
    })
    
    analysis.table <- reactive({
        if(is.null(input$provinces)) {
            data <- Data$covid.regions
            df <- data %>%
                dplyr::filter(
                    is.null(input$regions)     | Region %in% input$regions
                ) %>% 
                dplyr::select(Date, Region, Hospitalised, `In ICU`, 
                              `Home Isolation`, Healed, Dead, Total, Tests)
        } else {
            data <- Data$covid.province
            df <- data %>%
                dplyr::filter(
                    is.null(input$regions)     | Region %in% input$regions,
                    is.null(input$provinces)   | Province %in% input$provinces
                )
        }
        if(is.na(input$date.range[1])) {
            df %<>%
                dplyr::filter(Date >= as.Date("2020-02-24"))
        } else {
            df %<>%
                dplyr::filter(Date >= input$date.range[1])
        }
        if(is.na(input$date.range[2])) {
            df %<>%
                dplyr::filter(Date <= last.date)
        } else {
            df %<>%
                dplyr::filter(Date <= input$date.range[2])
        }
    })
    
    output$analysis.table <- DT::renderDataTable({
        DT::datatable(analysis.table(), options = list(bFilter=0))
    })
})
