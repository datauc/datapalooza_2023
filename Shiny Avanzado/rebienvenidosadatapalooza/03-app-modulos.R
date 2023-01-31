# Se puede automatizar...
library(shiny)

make_slider <- function(id){
  sliderInput(id, label = "N", min = 100, max = 1000, value = 1000)
}

make_input <- function(id){
  selectInput(id, label = "Distribución", choices = c("norm", "exp", "unif"))
}

get_dist <- function(dist){
  rdist <- switch(
    dist,
    "norm" = rnorm,
    "exp" = rexp,
    "unif" = runif,
  )
  rdist
}

ui <- fluidPage(
  fluidRow(
    column(2, make_slider("nrand_1")),
    column(2, make_input("dist_1")) ,
    column(8, plotOutput("plot_1"))
  ),
  fluidRow(
    column(2, make_slider("nrand_2")),
    column(2, make_input("dist_2")) ,
    column(8, plotOutput("plot_2"))
  ),
  fluidRow(
    column(2, make_slider("nrand_3")),
    column(2, make_input("dist_3")) ,
    column(8, plotOutput("plot_3"))
  ),
  fluidRow(
    column(2, make_slider("nrand_4")),
    column(2, make_input("dist_4")) ,
    column(8, plotOutput("plot_4"))
  )
)

server <- function(input, output, session) {
  
  output$plot_1 <- renderPlot({
    rdist <- get_dist(input$dist_1)
    hist(rdist(input$nrand_1), xlim = c(-5, 5))
  })
  
  output$plot_2 <- renderPlot({
    rdist <- get_dist(input$dist_2)
    hist(rdist(input$nrand_2), xlim = c(-5, 5))
  })
  
  output$plot_3 <- renderPlot({
    rdist <- get_dist(input$dist_3)
    hist(rdist(input$nrand_3), xlim = c(-5, 5))
  })
  
  output$plot_4 <- renderPlot({
    rdist <- get_dist(input$dist_4)
    hist(rdist(input$nrand_4), xlim = c(-5, 5))
  })
  
}

shinyApp(ui, server)