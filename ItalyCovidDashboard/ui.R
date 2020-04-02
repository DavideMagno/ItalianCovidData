#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
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
                                      fixed = TRUE, top = 60, 
                                      left = "auto", right = 20, bottom = "auto",
                                      width = 330, height = "auto",
                                      h2("Italian Covid Data explorer"),
                                      dateInput("date", "Select the date for the data", Sys.Date() - 1, 
                                                min = as.Date("2020-02-24")),
                                      radioButtons("type", "Type of data", choices = c("Region", "Province"),
                                                   selected = "Region"),
                                      conditionalPanel(condition = "input.type == 'Region'",
                                                       selectInput("field", "", 
                                                                   choices = c("Hospitalised", "In ICU", "Home Isolation",
                                                                               "Dead", "Healed", "Total"),
                                                                   selected = "Total")),
                                      plotlyOutput("test"),
                                      checkboxInput("log", "Log scale", value = TRUE)
                        )
                    )
           ),
           tabPanel("Data Explorer",
                    fluidRow(
                      column(4,
                             selectInput("regions", "Regions", 
                                         c("All regions"= NULL, 
                                           unique(Data$covid.regions$Region)),
                                         multiple=TRUE)),
                      column(4,
                             selectInput("provinces", "Provinces", 
                                         c("All provinces"= NULL), 
                                         multiple=TRUE)
                             ),
                      column(4,
                             dateInput("data.date", "Select the date for the data", NULL , 
                                       Sys.Date() - 1,
                                       min = as.Date("2020-02-24"))
                      )
                    ),
                    fluidRow(
                      hr(),
                      DT::dataTableOutput("ziptable")
                    )
           )
)