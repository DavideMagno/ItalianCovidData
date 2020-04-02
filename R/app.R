library(shiny)

fib <- function(n) ifelse(n < 3, 1, fib(n - 1) + fib(n - 2))

ui <- fluidPage(
  selectInput("nselect", "Choose a pre-defined number", 1:10),
  numericInput("nfree", "Or type any number", 1),
  "Fib number:",
  textOutput("nthval", inline = TRUE)
)

server <- function(input, output, session) {
  values <- reactiveValues(n = 1)
  
  observeEvent(input$nselect, {
    values$n <- input$nselect
  })
  observeEvent(input$nfree, {
    values$n <- input$nfree
  })
  output$nthval <- renderText({
    fib(as.integer(values$n))
  })
}

shinyApp(ui = ui, server = server)