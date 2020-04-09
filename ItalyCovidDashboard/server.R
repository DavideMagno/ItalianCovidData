source(here::here("R/ItalianMap.R"))
source(here::here("R/Analysis.R"))
source(here::here("R/MultipleGraphs.R"))

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
    data.logscale <- reactive({if(input$data_increments | !input$data.log) "linear" else "log"})
    
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
        if (grepl("Region", input$type)) {
            bin <- c(1, quantile(italy$last.cases, 
                                   c(0, 0.025, 0.05, 0.15, 0.25, 0.3, 0.65, 0.85, 1)))
        } else {
            bin <- c(1, quantile(italy$last.cases, 
                                 c(0, 0.15, 0.30, 0.45, 0.55, 0.65, 0.85, 0.97, 1)))
        }
        if (grepl("Healed", field())) {
            pal <- leaflet::colorBin(c("#D6FFDA", "#B7EBBB", "#99D89C", 
                                       "#7BC57D", "#5CB25D", "#3E9F3E", 
                                       "#208C1F", "#027800"),
                                     domain = italy$cases,
                                     bins = bin,
                                     na.color = "#ffffff")
        } else {
            pal <- leaflet::colorBin(c("#F6EEDB", "#ffe59c", "#f4c78a", 
                                       "#e9aa78", "#df8d66", "#d47054", 
                                       "#ca5342","#9A0A10"),
                                     domain = italy$cases,
                                     bins = bin,
                                     na.color = "#ffffff")
        }
        
        
        #create a pop up (onClick)
        polygon_popup <- paste0(paste0("<strong>",input$type,": </strong>"), 
                                italy$name, "<br>", 
                                paste0("<strong>", field()," cases: </strong>"), 
                                italy$cases)
        leaflet::leafletProxy("map", data = italy, session) %>% 
            leaflet::clearShapes(.) %>% 
            leaflet::removeControl("legend") %>% 
            leaflet::addPolygons(fillColor= ~pal(cases),
                                 fillOpacity = 0.5, 
                                 weight = 2, 
                                 color = "grey",
                                 popup = polygon_popup,
                                 label = ~name,
                                 layerId = ~name) %>%
            leaflet::addLegend("bottomleft", pal = pal, 
                               values = ~cases,
                               labFormat = leaflet::labelFormat(digits = 0),
                               opacity = 0.5,
                               title = "Legend",
                               layerId = "legend")
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
    
    observe({
        fields <- if (is.null(input$provinces)) {
            c("Hospitalised", 
              "In ICU", "Home Isolation", "Dead", 
              "Healed", "Total", "Tests")
        } else {
            c("Total")
        }
        stillSelected <- isolate(input$data.field[input$data.field %in% fields])
        updateSelectizeInput(session, "data.field", choices = fields,
                             selected = stillSelected, server = TRUE)
    })
    
    analysis.table <- reactive({
        if(is.null(input$provinces)) {
            if(input$data_increments) {
                data <- Data$covid.regions.inc
            } else {
                data <- Data$covid.regions
            }
        } else {
            if(input$data_increments) {
                data <- Data$covid.province.inc
            } else {
                data <- Data$covid.province
            }
        }
        PrepDataExplorer(data, input$regions, input$provinces, input$data.field,
                         input$date.range)
    })
    
    observe({
        if(is.null(input$data.field)) {
            data.field <- "Total"
        } else {
            data.field <- input$data.field
        }
        output$plots <- renderUI({get_plot_output_list_div(data.field, 
                                                           analysis.table(),
                                                           data.logscale())})
    })
    
    output$analysis.table <- DT::renderDataTable({
        DT::datatable(analysis.table() %>% 
                          dplyr::arrange(desc(Date)), 
                      rownames = FALSE,
                      options = list(bFilter=0,autoWidth = TRUE,
                                     columnDefs = list(list(width = '200px',
                                                            className = 'dt-center',
                                                            targets = "_all"))))
    })
})
