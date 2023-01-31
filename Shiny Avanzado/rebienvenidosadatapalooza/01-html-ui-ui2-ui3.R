library(shiny)

ui <- fluidPage(
  actionButton("boton", "Clickéame")
)

ui2 <- tags$div(
  class = "container-fluid",
  tags$button(
    "Clickéame",
    id="boton",
    type="button", 
    class="btn btn-default action-button"
    )
  )

ui3 <- HTML('<div class="container-fluid">
  <button id="boton" type="button" class="btn btn-default action-button">Clickéame</button>
</div>')


server <- function(input, output) {
  observeEvent(input$boto, {
    showModal(modalDialog("Hola!"))
  })
}

shinyApp(ui = ui, server = server)
shinyApp(ui = ui2, server = server)
shinyApp(ui = ui3, server = server)

