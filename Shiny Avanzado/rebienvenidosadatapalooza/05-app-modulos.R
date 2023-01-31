# Rescatar input en módulos input$`nombremodulo-id`
library(shiny)

distplotUI <- function(id) {

  ns <- NS(id)
  
  tagList(
    column(2, sliderInput(ns("rand"), label = "N", min = 100, max = 1000, value = 1000)),
    column(2, selectInput(ns("dist"), label = "Distribución", choices = c("norm", "exp", "unif"))),
    column(8, plotOutput(ns("plot")))
  )
  
}

distplotServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    output$plot <- renderPlot({
      
      rdist <- switch(
        input$dist,
        "norm" = rnorm,
        "exp"  = rexp,
        "unif" = runif,
      )
      
      hist(rdist(input$rand), xlim = c(-5, 5))
      
    })
    
  })
}

ui <- fluidPage(
  
  fluidRow(textOutput("info")),
  
  purrr::map(1:4, ~ fluidRow(distplotUI(paste0("c", .x))))
  
)

server <- function(input, output, session) {
  
  output$info <- renderText({
    total <- input$`c1-rand` + input$`c2-rand` + input$`c2-rand` + input$`c4-rand`
    paste("se han simulado", total, "valores")
  })
  
  distplotServer("c1")
  distplotServer("c2")
  distplotServer("c3")
  distplotServer("c4")
  
}

shinyApp(ui, server)
