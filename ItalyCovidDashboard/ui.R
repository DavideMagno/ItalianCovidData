#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(billboarder)
library(shinycssloaders)

# Define UI for application that draws a histogram
navbarPage("Italy Covid", id="nav",
           tabPanel("Interactive Map", 
                    div(class = "outer",
                        tags$head(
                          # Include our custom CSS
                          includeCSS(here::here("ItalyCovidDashboard/styles.css"))
                        ),
                        leaflet::leafletOutput("map", height = "100%"),
                        absolutePanel(id = "controls", class = "panel panel-default", 
                                      fixed = FALSE, top = 60, draggable = TRUE, 
                                      left = "auto", right = 20, bottom = "auto",
                                      width = 330, height = "auto", cursor = "default",
                                      # HTML('<checkbox data-toggle="collapse" data-target="#demo">Collapse</checkbox>'),
                                      # tags$div(id = 'demo',  class="collapse",
                                      p(), p(),
                                      dateInput("date", "Select the date for the map", 
                                                last.date, 
                                                min = as.Date("2020-02-24")),
                                      radioButtons("type", "Type of data", 
                                                   choices = c("Region", "Province"),
                                                   selected = "Region"),
                                      conditionalPanel(condition = "input.type == 'Region'",
                                                       selectInput("field", "Field for the map", 
                                                                   choices = c("Hospitalised", "In ICU", "Home Isolation",
                                                                               "Dead", "Healed", "Total"),
                                                                   selected = "Total")),
                                      plotly::plotlyOutput("dynamic", height = "300px"),
                                      checkboxInput("log", "Log scale", value = TRUE)
                        )
                    )
           ),
           tabPanel("Data Explorer",
                    fluidRow(
                      column(4,
                             selectInput("regions", "Select the Regions of analysis", 
                                         c("All regions"= "", 
                                           unique(Data$covid.regions$Region)),
                                         multiple=TRUE)),
                      column(4,
                             selectInput("provinces", "Select the Provinces of analysis", 
                                         c("All provinces"= ""), 
                                         multiple=TRUE)
                      ),
                      column(4,
                             dateRangeInput("date.range", "Select the date range of analysis", 
                                            start = last.date, 
                                            end = last.date,
                                            min = as.Date("2020-02-24"))
                      )
                    ),
                    fluidRow(
                      hr(),
                      DT::dataTableOutput("analysis.table")
                    )
           )
)