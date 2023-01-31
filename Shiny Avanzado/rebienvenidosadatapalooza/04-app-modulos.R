# Módulos, ejemplo simple
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
  fluidRow(distplotUI("1")),
  fluidRow(distplotUI("2")),
  fluidRow(distplotUI("3")),
  fluidRow(distplotUI("4"))
)

server <- function(input, output, session) {
  
  distplotServer("1")
  distplotServer("2")
  distplotServer("3")
  distplotServer("4")
  
}

shinyApp(ui, server)
