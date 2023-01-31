# Ejemplo simple
library(shiny)

ui <- fluidPage(
  textInput("texto", label = "Ingrese texto:", placeholder = "Acá va un texto"),
  textOutput("salida")
)

server <- function(input, output) {
  
  output$salida <- renderText({
    input$texto
  })
  
}

shinyApp(ui = ui, server = server)