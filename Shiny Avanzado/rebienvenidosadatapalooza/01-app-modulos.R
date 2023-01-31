# Ejemplo simple
library(shiny)

ui <- fluidPage(
  
  fluidRow(
    column(2, sliderInput("nrand", label = "N", min = 100, max = 1000, value = 1000)),
    column(2, selectInput("dist", label = "Distribución", choices = c("norm", "exp", "unif"))),
    column(8, plotOutput("plot"))
  ),
  
)

server <- function(input, output, session) {
  
  output$plot <- renderPlot({
    
    rdist <- switch(
      input$dist,
      "norm" = rnorm,
      "exp" = rexp,
      "unif" = runif,
    )
    
    hist(rdist(input$nrand), xlim = c(-5, 5))
    
  })
  
}

shinyApp(ui, server)