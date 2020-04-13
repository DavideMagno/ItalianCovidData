source(here::here("R/ItalianMap.R"))
source(here::here("R/Analysis.R"))
source(here::here("R/MultipleGraphs.R"))

library(shiny)

shinyServer(function(input, output, session) {

# GLOBAL REACTIVES ---------------------------------------------------------------

    field <- reactive({
        if (grepl("Region", input$type)) {
            input$field
        } else {
            "Total"
        }
    })
    
    logscale <- reactive({if(input$log) "log" else "linear"})
    data.logscale <- reactive({if(input$data_increments | !input$data.log) 
        "linear" else "log"
        })
    
    increments <- reactive({
        if(input$data_increments) "Increment" else "Cumulative"
    })
    

# MAP ---------------------------------------------------------------------

    ItalyMap <- reactive({
        FilterAndPrepareToPlot(Data, input$date, input$type, field())
        })
    
    output$map <- leaflet::renderLeaflet({
        leaflet::leaflet() %>% 
            leaflet::addProviderTiles("Esri.WorldTerrain") %>% 
            leaflet::setView(10, 41.879, zoom = 5)})
    
    observe({
        map.features <- DrawProxyMap(ItalyMap(), input$type, field())
        leaflet::leafletProxy("map", data = ItalyMap(), session) %>% 
            leaflet::clearShapes(.) %>% 
            leaflet::removeControl("legend") %>% 
            leaflet::addPolygons(fillColor= ~map.features$pal(cases),
                                 fillOpacity = 0.5, 
                                 weight = 2, 
                                 color = "grey",
                                 popup = map.features$polygon_popup,
                                 label = ~name,
                                 layerId = ~name) %>%
            leaflet::addLegend("bottomleft", pal = map.features$pal, 
                               values = ~cases,
                               labFormat = leaflet::labelFormat(digits = 0),
                               opacity = 0.5,
                               title = "Legend",
                               layerId = "legend")
    })
    

# MAP GRAPH ---------------------------------------------------------------

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
        Extract(Data = Data, filter_by = name(), type = input$type, 
                select_field = field())
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
                plotly::add_trace(y = ~get(field()), type = "scatter",
                                  mode = "lines+markers") %>% 
                plotly::layout(
                    title = paste("Dynamic in", name()),
                    yaxis = list(title = text,
                                 type = logscale())
                ) %>%
                plotly::config(displayModeBar = FALSE)
        })
    })

# DATA ANALYSIS -----------------------------------------------------------

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
        input.data <- PrepareDataForExtraction(input$regions, input$provinces, 
                                         input$date.range, last.date, 
                                         input$data.field) 
        
        data <- Extract(Data, filter_by = input.data$filter_by, 
                        select_method = increments(), type = input.data$type, 
                        select_field = input.data$data.field, 
                        start_date = input.data$start_date, 
                        end_date = input.data$end_date)
        
        return(list(input.data = input.data, data = data))
    })
    
    output$plots <- renderUI({
        get_plot_output_list_div(analysis.table()$input.data$data.field, 
                                 analysis.table()$data,
                                 data.logscale())
    })
    
    observe({
        data <- Extract(Data, filter_by = analysis.table()$input.data$filter_by, 
                        select_method = "Ratio", 
                        type = analysis.table()$input.data$type, 
                        select_field = analysis.table()$input.data$data.field, 
                        start_date = analysis.table()$input.data$start_date, 
                        end_date = analysis.table()$input.data$end_date)
        
        output$plots.ratio <- renderUI({
            get_plot_output_list_div(analysis.table()$input.data$data.field, 
                                     data,"linear", TRUE)
            })
    })
    
    output$analysis.table <- DT::renderDataTable({
        DT::datatable(analysis.table()$data %>% 
                          dplyr::arrange(desc(Date)), 
                      rownames = FALSE,
                      options = list(bFilter=0,autoWidth = TRUE,
                                     columnDefs = list(list(width = '200px',
                                                            className = 'dt-center',
                                                            targets = "_all"))))
    })
})
