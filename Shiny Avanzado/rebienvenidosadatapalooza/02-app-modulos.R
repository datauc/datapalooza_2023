# Ejemplo cuando se requiere repetir
library(shiny)

ui <- fluidPage(
  fluidRow(
    column(2, sliderInput("nrand_1", label = "N", min = 100, max = 1000, value = 1000)),
    column(2, selectInput("dist_1", label = "Distribución", choices = c("norm", "exp", "unif"))),
    column(8, plotOutput("plot_1"))
  ),
  fluidRow(
    column(2, sliderInput("nrand_2", label = "N", min = 100, max = 1000, value = 1000)),
    column(2, selectInput("dist_2", label = "Distribución", choices = c("norm", "exp", "unif"))),
    column(8, plotOutput("plot_2"))
  ),
  fluidRow(
    column(2, sliderInput("nrand_3", label = "N", min = 100, max = 1000, value = 1000)),
    column(2, selectInput("dist_3", label = "Distribución", choices = c("norm", "exp", "unif"))),
    column(8, plotOutput("plot_3"))
  ),
  fluidRow(
    column(2, sliderInput("nrand_4", label = "N", min = 100, max = 1000, value = 1000)),
    column(2, selectInput("dist_4", label = "Distribución", choices = c("norm", "exp", "unif"))),
    column(8, plotOutput("plot_4"))
  ),
)

server <- function(input, output, session) {
  
  output$plot_1 <- renderPlot({
    rdist <- switch(
      input$dist_1,
      "norm" = rnorm,
      "exp" = rexp,
      "unif" = runif,
    )
    
    hist(rdist(input$nrand_1), xlim = c(-5, 5))
    
  })
  
  output$plot_2 <- renderPlot({
    rdist <- switch(
      input$dist_2,
      "norm" = rnorm,
      "exp" = rexp,
      "unif" = runif,
    )
    
    hist(rdist(input$nrand_2), xlim = c(-5, 5))
    
  })
  
  output$plot_3 <- renderPlot({
    rdist <- switch(
      input$dist_3,
      "norm" = rnorm,
      "exp" = rexp,
      "unif" = runif,
    )
    
    hist(rdist(input$nrand_3), xlim = c(-5, 5))
    
  })
  
  output$plot_4 <- renderPlot({
    rdist <- switch(
      input$dist_4,
      "norm" = rnorm,
      "exp" = rexp,
      "unif" = runif,
    )
    
    hist(rdist(input$nrand_4), xlim = c(-5, 5))
    
  })
  
}

shinyApp(ui, server)
