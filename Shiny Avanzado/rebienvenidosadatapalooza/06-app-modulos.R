# Se pueden descomponer la componente UI del módulo y agregar parámetros adicionales.
library(shiny)

inputUI <- function(id, etiqueta = "N"){
  
  ns <- NS(id)
  
  tagList(
    column(6, sliderInput(ns("rand"), label = etiqueta, min = 100, max = 1000, value = 1000)),
    column(6, selectInput(ns("dist"), label = "Distribución", choices = c("norm", "exp", "unif")))
  )
  
}

plotUI <- function(id) {
  
  ns <- NS(id)
  
  # no es necesario el tagList dado que retornamos UN elemento
  tagList(
    plotOutput(ns("plot"))
  )
  
}

distplotServer <- function(id, color = "gray") {
  moduleServer(id, function(input, output, session) {
    
    # mismo que teníamos en un principio
    output$plot <- renderPlot({
      
      rdist <- switch(
        input$dist,
        "norm" = rnorm,
        "exp"  = rexp,
        "unif" = runif,
      )
      
      hist(rdist(input$rand), xlim = c(-5, 5), col = color)
      
    })
    
  })
}

ui <- fluidPage(
  
  fluidRow(uiOutput("info")),
  
  tags$hr(),
  
  fluidRow(
    column(3, inputUI("c1")),
    column(3, inputUI("c2", "# 2da distribucion")),
    column(3, inputUI("c3")),
    column(3, inputUI("c4")),
  ),
  
  tags$hr(),
  
  fluidRow(
    column(3, plotUI("c1")),
    column(3, plotUI("c2")),
    column(3, plotUI("c3")),
    column(3, plotUI("c4"))
  )
  
)

server <- function(input, output, session) {
  
  output$info <- renderUI({
    total <- input$`c1-rand` + input$`c2-rand` + input$`c2-rand` + input$`c4-rand`
    tags$h3(paste("Se han simulado", total, "valores"))
  })
  
  distplotServer("c1")
  distplotServer("c2", color = "red")
  distplotServer("c3")
  distplotServer("c4")
  
}

shinyApp(ui, server)
