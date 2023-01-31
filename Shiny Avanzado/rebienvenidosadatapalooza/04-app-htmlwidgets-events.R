# Ejemplo simple
library(shiny)
library(tidyverse)
library(lubridate)
library(rvest)
library(leaflet)
library(DT)
library(highcharter)
library(plotly)

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
      magnitud_cat = cut(magnitud, breaks = c(1:7, Inf)),
      id = row_number()
    )
  
  datos
  
}

ui <- fluidPage(
  
  tags$hr(),
  
  fluidRow(
    column(4, dateInput("fecha", label = NULL, value = Sys.Date(), max = Sys.Date())),
    column(8, uiOutput("salida"))
  ),
  
  fluidRow(
    column(6,  leafletOutput("mapa")),
    column(6,  highchartOutput("grafico")),
    column(12, dataTableOutput("tabla"))
  )
  
)

# input <- list(fecha = Sys.Date())
server <- function(input, output) {
  
  # inicializamos el valor
  sismos <- reactiveVal(value = get_sismos())
  
  # inicializamos otra variable reactiva
  evento <- reactiveVal(value = "Nada por ahora")
  
  output$salida <- renderUI({
    evento()
  })
  
  # input$`iddelmapa`_marker_click
  observeEvent(input$mapa_marker_click, {
    
    print(input$mapa_marker_click)
    
    evento(str_glue("Click mapa en id {input$mapa_marker_click$id}"))
    
  })
  
  observeEvent(input$tabla_rows_selected, {
    
    print(input$tabla_rows_selected)
    
    ids <- paste(input$tabla_rows_selected, collapse = ", ")
    
    evento(str_glue("Click tabla en id { ids }"))
    
  })
  
  # necesitamos un evento!, no un valor dado que debemos actualizar
  # widgets no GENERAR uno nuevo
  observeEvent(input$fecha,{
    
    evento("Se modifica la fecha")
    
    data_nueva <- get_sismos(input$fecha)

    # leaflet -----------------------------------------------------------------
    leafletProxy("mapa") |> 
      clearMarkers() |> 
      addMarkers(
        data = data_nueva,
        lng = ~longitud, 
        lat = ~latitud,
        layerId = ~id,
        popup = ~as.character(info),
        label = ~as.character(fecha_local_lugar)
      )
    
    # grafico highcharter -----------------------------------------------------
    lvls <- c("(1,2]", "(2,3]", "(3,4]", "(4,5]", "(5,6]", "(6,7]", "(7,Inf]")
    
    d2 <- data_nueva |>
      count(magnitud_cat) |>
      complete(magnitud_cat = lvls) |>
      mutate(
        n = coalesce(n, 0),
        magnitud_cat = factor(magnitud_cat, levels = lvls),
        magnitud_num = as.numeric(magnitud_cat) - 1
      ) |>
      arrange(magnitud_cat)

    highchartProxy("grafico") |>
      hcpxy_update_series(
        id = "conteo",
        data = d2 |> rename(x =  magnitud_num, y = n) |>  list_parse()
    )

    # DT ----------------------------------------------------------------------
    dataTableProxy("tabla") |> 
      replaceData(data = data_nueva)
    
  })

  output$mapa <- renderLeaflet({ 
    sismos <- sismos()
    leaflet(sismos) |> 
      addTiles() |>   
      addMarkers(
        lng = ~longitud, 
        lat = ~latitud,
        layerId = ~id,
        popup = ~as.character(info),
        label = ~as.character(fecha_local_lugar)
      )
    
  })
  
  output$tabla <- renderDataTable({ 
    sismos()
  }, selection = "single")
  
  output$grafico <- renderHighchart({ 
    
    sismos <- sismos()
    
    lvls <- c("(1,2]", "(2,3]", "(3,4]", "(4,5]", "(5,6]", "(6,7]", "(7,Inf]")
    
    d <- sismos |> 
      count(magnitud_cat) |> 
      complete(magnitud_cat = lvls) |> 
      mutate(
        n = coalesce(n, 0), 
        magnitud_cat = factor(magnitud_cat, levels = lvls),
        magnitud_num = as.numeric(magnitud_cat) - 1
        ) |> 
      arrange(magnitud_cat) 
      
    hchart(d, "column", hcaes(magnitud_num, n), name = "Conteo", id = "conteo") |> 
      hc_xAxis(categories = lvls)
  })

}

shinyApp(ui = ui, server = server)
