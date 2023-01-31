# Ejemplo simple
library(shiny)

ui <- fluidPage(
  actionButton("boton1", label = "botón 1"),
  actionButton("boton2", label = "botón 2", icon = icon("redo")),
)

server <- function(input, output) {

  # se gatilla por cualquier expresion reactiva que cambie
  observe({
    
    # ??!!
    req(input$boton1+input$boton2>0)
    
    showModal(
      modalDialog(
        title = "Botón clickeado",
        "Se apretó un botón"
        )
      )
    
  })
  
}

shinyApp(ui = ui, server = server)