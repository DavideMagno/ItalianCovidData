library(shiny)
library(shinyjs)
library(billboarder)
library(shinycssloaders)

# Define UI for application that draws a histogram
navbarPage("Italy Covid", id="nav",
           tabPanel("Interactive Map", 
                    fluidRow(
                      column(4,
                             dateInput("date", "Select the date for the map", 
                                       last.date, 
                                       min = as.Date("2020-02-24"),
                                       max = last.date),
                             align="center"
                             ),
                      column(4,
                             radioButtons("type", "Type of data", 
                                          choices = c("Region", "Province"),
                                          selected = "Region"),
                             align="center"
                             ),
                      column(4,
                             conditionalPanel(condition = "input.type == 'Region'",
                                              selectInput("field", "Field for the map", 
                                                          choices = c("Hospitalised", "In ICU", "Home Isolation",
                                                                      "Dead", "Healed", "Total"),
                                                          selected = "Total")),
                             align="center")
                    ),
                    fluidRow(
                      column(12,
                             includeCSS(here::here("ItalyCovidDashboard/styles.css")),
                        # tags$head(
                        #   # Include our custom CSS
                        #   includeCSS(here::here("ItalyCovidDashboard/styles.css"))
                        # ),
                        leaflet::leafletOutput("map", height = 710),
                        absolutePanel(id = "controls", class = "panel panel-default", 
                                      fixed = FALSE, top = 15, draggable = TRUE, 
                                      left = "auto", right = 60, bottom = "auto",
                                      width = 400, height = 400, cursor = "default",
                                      p(), p(),
                                      plotly::plotlyOutput("dynamic", height = "300px"),
                                      checkboxInput("log", "Log scale", value = TRUE)
                        ),
                        absolutePanel(id = "bestworst", class = "panel panel-default", 
                                      fixed = FALSE, top = 15, draggable = TRUE, 
                                      left = 100, width = 400, cursor = "default",
                                      p(), p(),
                                      plotly::plotlyOutput("best.worst.plot", height = "450px")
                        )
                      )
                    )
           ),
           tabPanel("Data Explorer",
                    fluidRow(
                      column(3,
                             selectInput("regions", "Select the Regions of analysis", 
                                         c("Italy"= "", 
                                           unique(Data$covid.regions$Region)),
                                         multiple=TRUE),
                             align="center"),
                      column(3,
                             selectInput("provinces", "Select the Provinces of analysis", 
                                         c("All provinces"= ""), 
                                         multiple=TRUE),
                             align="center"
                      ),
                      column(3,
                             dateRangeInput("date.range", "Select the date range of analysis", 
                                            start = as.Date("2020-02-24"), 
                                            end = last.date,
                                            min = as.Date("2020-02-24"),
                                            max = last.date),
                             align="center"
                      ),
                      column(3,
                             selectInput("data.field", "Select the fields to analyse",
                                         c("Total" = ""),
                                         multiple=TRUE, selected = "Total"),
                             align="center"
                      )
                    ),
                    fluidRow(
                      column(1, 
                             p(strong("Absolute figures"))
                      ), 
                      column(1,
                             checkboxInput("data_increments", "Show daily increments", value = TRUE)
                      ),
                      column(1,
                             conditionalPanel(condition = "!input.data_increments",
                                              checkboxInput("data.log", "log scale", value = TRUE))
                      )
                    ),
                    fluidRow(
                      uiOutput("plots")
                    ),
                    fluidRow(
                      column(1, 
                             p(strong("Relative increments"))
                      )
                    ),
                    fluidRow(
                      uiOutput("plots.ratio")
                    ),
                    
                    fluidRow(
                      column(2, 
                             conditionalPanel(condition = "output.provinces == true",
                                              p(strong("Increments compared to Tests"))
                             )
                      ),
                      column(3, 
                             conditionalPanel(condition = "output.provinces == true",
                                              p(paste("Number of tests used is the",
                                                      "linearly decreasing weighted",
                                                      "average over previous 7 days"))
                             )
                      )
                    ),
                    fluidRow(
                      uiOutput("tests.ratio")
                    ),
                    fluidRow(
                      hr(),
                      DT::dataTableOutput("analysis_table")
                    )
           )
)