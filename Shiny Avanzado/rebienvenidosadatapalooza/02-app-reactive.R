# Ejemplo simple
library(shiny)

ui <- fluidPage(
  textInput("texto_usuario", label = "Ingrese texto:", placeholder = "Acá va un texto"),
  # actionButton("go", label = "Go!"),
  textOutput("salida"),
  textOutput("salida2")
)

server <- function(input, output) {
  
  # expresion reactiva, similar a funcion! se actualiza, se ejecuta UNA vez!
  texto_reactivo <- reactive({
    input$texto_usuario
  })
  
  output$salida <- renderText({
    texto_reactivo()
  })
  
  output$salida2 <- renderText({
    texto_reactivo()
  })
  
}

shinyApp(ui = ui, server = server)