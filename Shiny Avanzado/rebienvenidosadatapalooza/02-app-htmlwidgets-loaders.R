# Ejemplo simple
library(shiny)
library(tidyverse)
library(lubridate)
library(rvest)
library(leaflet)
library(DT)
library(highcharter)
library(plotly)
library(shinycssloaders)
# https://github.com/daattali/shinycssloaders
# https://github.com/emitanaka/shinycustomloader
# https://github.com/JohnCoene/waiter

get_sismos <- function(fecha = Sys.Date()){
  url <- str_glue(
    "https://www.sismologia.cl/sismicidad/catalogo/{anio}/{mes}/{fecha}.html",
    anio = year(fecha), mes = format(fecha, "%m"), fecha = format(fecha, "%Y%m%d")
  )
  
  datos <- read_html(url) |>
    html_table() |>
    dplyr::nth(2) |>
    janitor::clean_names() |>
    tidyr::separate(
      latitud_longitud,
      into = c("latitud", "longitud"),
      sep = " ", convert = TRUE
    ) |> 
    mutate(
      info = str_glue("Magnitud: {magnitud_2}<br>Profundidad: {profundidad}"),
      profundidad = readr::parse_number(profundidad),
      magnitud = readr::parse_number(magnitud_2),
      fecha_utc = readr::parse_datetime(fecha_utc),
      magnitud_cat = cut(magnitud, breaks = c(1:7, Inf))
    )
  datos
  
}

ui <- fluidPage(
  
  tags$hr(),
  
  dateInput("fecha", label = NULL, value = Sys.Date(), max = Sys.Date()),
  
  fluidRow(
    column(6,  withSpinner(leafletOutput("mapa"))),
    column(6,  withSpinner(highchartOutput("grafico"))),
    column(12, withSpinner(dataTableOutput("tabla")))
  )
  
)

# input <- list(fecha = Sys.Date())
server <- function(input, output) {
  
  sismos <- reactive({
    sismos <- get_sismos(input$fecha)
    sismos
  })
  
  output$mapa <- renderLeaflet({ 
    sismos <- sismos()
    leaflet(sismos) |> 
      addTiles() |>   
      addMarkers(
        lng = ~longitud, 
        lat = ~latitud,
        popup = ~as.character(info),
        label = ~as.character(fecha_local_lugar)
      )
  })
  
  output$tabla <- renderDataTable({ 
    sismos()
  })
  
  output$grafico <- renderHighchart({ 
    
    sismos <- sismos()
    
    lvls <- c("(1,2]", "(2,3]", "(3,4]", "(4,5]", "(5,6]", "(6,7]", "(7,Inf]")
    
    d <- sismos |> 
      count(magnitud_cat) |> 
      complete(magnitud_cat = lvls) |> 
      mutate(
        n = coalesce(n, 0), 
        magnitud_cat = factor(magnitud_cat, levels = lvls)
        ) |> 
      arrange(magnitud_cat)
    
    hchart(d, "column", hcaes(magnitud_cat, n), name = "Conteo")
  })

}

shinyApp(ui = ui, server = server)
