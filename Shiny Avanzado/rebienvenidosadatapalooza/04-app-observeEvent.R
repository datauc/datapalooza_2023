# Ejemplo simple
library(shiny)

ui <- fluidPage(
  actionButton("boton1", label = "botón 1"),
  actionButton("boton2", label = "botón 2", icon = icon("redo")),
)

server <- function(input, output) {

  # Solamente se gatilla por `input$boton1`
  # si quieres sumar más elementos: observeEvent(c(input$b1, input$b2), {...})
  observeEvent(input$boton1, {
    
    input$boton1
    input$boton2  
    
    showModal(
      modalDialog(
        title = "Botón clickeado",
        "Se apretó un botón"
        )
      )
    
  })
  
}

shinyApp(ui = ui, server = server)