# Un approach para que todos los módulos dependan de UN input
library(shiny)
library(shinyjs)

inputUI <- function(id, etiqueta = "N"){
  
  ns <- NS(id)
  
  tagList(
    column(6, sliderInput(ns("rand"), label = etiqueta, min = 100, max = 1000, value = 1000)),
    column(6, selectInput(ns("dist"), label = "Distribución", choices = c("norm", "exp", "unif"))),
    shinyjs::hidden(selectInput(ns("color"), label = NULL, choices = c("red", "blue", "gray"))),
  )
  
}

plotUI <- function(id) {
  
  ns <- NS(id)
  
  # no es necesario el tagList dado que retornamos UN elemento
  tagList(
    plotOutput(ns("plot"))
  )
  
}

distplotServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # mismo que teníamos en un principio
    output$plot <- renderPlot({
      
      rdist <- switch(
        input$dist,
        "norm" = rnorm,
        "exp"  = rexp,
        "unif" = runif,
      )
      
      hist(rdist(input$rand), xlim = c(-5, 5), col = input$color)
      
    })
    
  })
}

ui <- fluidPage(
  
  fluidRow(
    uiOutput("info"),
    selectInput("color", label = NULL, choices = c("red", "blue", "gray"))
    ),
  
  tags$hr(),
  
  fluidRow(
    # se puede map?! sip, map2
    column(3, inputUI("c1")),
    column(3, inputUI("c2", "# 2da distribucion")),
    column(3, inputUI("c3")),
    column(3, inputUI("c4")),
  ),
  
  tags$hr(),
  
  fluidRow(
    # un map!
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
  
  observeEvent(input$color, {
    
    col <- input$color
    
    # mejor un map, no?!
    updateSelectInput(session, inputId = NS("c1")("color"), selected = col)
    updateSelectInput(session, inputId = NS("c2")("color"), selected = col)
    updateSelectInput(session, inputId = NS("c3")("color"), selected = col)
    updateSelectInput(session, inputId = NS("c4")("color"), selected = col)
    
  })
  
  distplotServer("c1")
  distplotServer("c2")
  distplotServer("c3")
  distplotServer("c4")
  
}

shinyApp(ui, server)
