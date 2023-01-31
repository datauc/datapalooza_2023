library(tidyverse)
library(readxl)
library(writexl)
library(sjlabelled)
library(janitor) 
library(tibble) # viene dentro del tidyverse
library(forcats) # viene dentro del tidyverse
library(stringr) # viene dentro del tidyverse
library(tidyr) # viene dentro del tidyverse
library(purrr) # viene dentro del tidyverse

# Ver instalación de paquetes ---------------------------------------------

paquetes <- c("tidyverse", "sjlabelled", "forcats")

paquetes_instalados <- lapply(paquetes,
                              FUN = function(x) {
                                if(!require(x, character.only = TRUE)){
                                  
                                  install.packages(x, dependencies = TRUE)
                                  library(x, character.only = TRUE)
                                }
                                
                              }
)

pacman::p_load(tidyverse, sjlabelled, writexl, forcats, readr)

if(!require("tidyverse")) {
  install.packages("tidyverse")
  library(tidyverse)
}

# Lectura datos -----------------------------------------------------------

list.files("cep", pattern = "*.xlsx|.[R|r]ds")

# Si es que estamos en un formato específico de R
datos <- readr::read_rds(file = "cep/base_86.Rds")

# Si es que estamos con formato XLSX
datos <- readxl::read_xlsx(path = "cep/base_86.xlsx", col_names = TRUE, 
                           sheet = 1)

# saveRDS(datos, "cep/cep.rds")

# Otra opción que tenemos es abrir esto como una lista

archivos <- list.files("cep", full.names = TRUE, pattern = ".[R|r]ds")

lista <- purrr::map(archivos, .f = ~readr::read_rds(.))
lista <- purrr::map(lista, .f = ~janitor::clean_names(.))

lista <- purrr::set_names(x = lista, 
                          nm = c(gsub(archivos, 
                                      pattern = "cep/|.[R|r]ds", 
                                      replacement = "")))
lista$base_86

# Exploración de la encuesta ----------------------------------------------

names(datos)
length(datos)
nrow(datos)
sapply(datos, FUN = class)
dplyr::glimpse(datos)
str(datos)
dim(datos) 
class(datos)
purrr::map_dbl(datos, .f = function(x){sum(is.na(x))})

# Veamos ahora las etiquetas de la base de datos

etiquetas <- datos %>%
  purrr:::map(~attributes(.)) %>%
  purrr:::map_chr("label", .default = NA)

etiquetas <- tibble::as_tibble(etiquetas)

etiquetas$value

etiqueta <- function(x) {
  termino <- paste(x) 
  retorno <- dplyr::filter(etiquetas, 
                           stringr::str_detect(etiquetas$value, 
                                      regex(as.character(termino), 
                                            ignore_case = T)))
  return(retorno)

}

etiqueta("económ")


# Seleccionar variables (select) ---------------------------------------------------

# Rescatar una variable
datos %>% dplyr::select(region) 
datos$region
datos[c("region")] 

# Rescatar 4 variables
datos %>% dplyr::select(region, comuna, sexo, gse) 
datos[c("region", "comuna", "sexo", "gse")]

# Rescatar las posiciones de las variables 3 y 8
datos %>% dplyr::select(3, 8)
datos[c(3,8)]

# Rescatar todas aquellas que no sean esas variables
datos %>% dplyr::select(-c(3, 8))
datos[-c(3, 8)] 
datos[c(-3, -8)]

# Rescatar todas aquellas que comiencen con G.
datos %>% dplyr::select(starts_with(match = "percepcion_"))

# Seleccionar mediante un prefijo
datos %>% dplyr::select(num_range(prefix = "percepcion_", range = 2:6))

# Rescatar todas aquellas que terminen con _6.
datos %>% dplyr::select(ends_with(match = "_6"))

# Extraer todas aquellas variables que cumplan con el criterio de ser tipo nominal.
datos %>% dplyr::select(where(is.character))

# Extraer todas aquellas variables que cumplan con el criterio de ser tipo nominal.
datos %>% dplyr::select_if(is.character)

# Parametrizar a partir de una variable filtro.
variables <- names(datos)[c(6:8)]
datos %>% dplyr::select(-all_of(variables)) 

# Filtrar variables (filter) -------------------------------------------------------

# Filtrar cuando el NSE sea ABC1.
datos %>% dplyr::filter(gse == 1) 
datos[datos$gse == 1,]
subset(x = datos, subset = datos$gse == 1, select = c(gse))

# Filtrar cuando sea RM y comuna de Providencia.
datos %>% dplyr::filter(region == 13 & comuna == "PROVIDENCIA")
datos[datos$region == 13 & datos$comuna == "PROVIDENCIA", ]

# Filtrar cuando las comunas de la RM sean Providencia, Las Condes, Ñuñoa
datos %>% dplyr::filter(comuna %in% c("PROVIDENCIA", "LAS CONDES", "ÑUÑOA"))
datos[datos$comuna %in% c("PROVIDENCIA", "LAS CONDES", "ÑUÑOA"),]

# Filtrar cuando las comunas de la RM no sean sean las tres anteriores.
datos %>% dplyr::filter(region == 13, !comuna %in% c("PROVIDENCIA", "LAS CONDES", "ÑUÑOA"))
datos[datos$region == 13 & !datos$comuna %in% c("PROVIDENCIA", "LAS CONDES", "ÑUÑOA"),]

# Filtrar cuando la edad sea mayor o igual a 30.
datos %>% dplyr::filter(edad >= 30)
datos[datos$edad >= 30,]
datos[!datos$edad >= 30,]

# Filtrar cuando el sexo sea 1 o el NSE C2
datos %>% dplyr::filter(sexo == 1 | gse == 2)
datos[datos$sexo == 1 | datos$gse == 2,]

# Crear nuevas variables o modificar las ya existentes (mutate) ------------------------------------------------------------------

# Crear variable llamada criterio, que establezca buenas y malas películas.
datos %>% dplyr::mutate(percepcion_2 = 
                         case_when(percepcion_2 == 1 ~ "Muy mala",
                                   percepcion_2 == 2 ~ "Mala", 
                                   percepcion_2 == 3 ~ "Ni buena ni mala",
                                   percepcion_2 == 4 ~ "Buena",
                                   percepcion_2 == 5 ~ "Muy buena",
                                   percepcion_2 == 6 ~ "No sabe",
                                   percepcion_2 == 7 ~ "No contesta")) 

# Convertir todas las observaciones de las variables con las etiquetas
datos %>% dplyr::mutate_all(sjlabelled::as_label) 

# Cambiar los formatos de las variables, desde un tipo de naturaleza a otra 
datos %>% dplyr::mutate_if(is.character, as.factor) 

# Colapsar factores en menos categorías (agrupar variables).

datos %>% 
  dplyr::mutate_all(sjlabelled::as_label) %>% 
  dplyr::select(percepcion_2, gse) %>% 
  dplyr::mutate(percepcion_agrupada = 
                forcats::fct_collapse(percepcion_2,
                                      "Mala" = c("Muy mala", "Mala"),
                                      "Buena" = c("Muy buena", "Buena"),
                                      "No sabe no contesta" = c("No sabe", "No contesta")))

# Otros ejemplos

Ministerios <- data.frame(ID = paste("ID", seq(1, 6)),
                          Ministerio = c(1,1,2,3,4,5))

# Recodificar a partir del comando ifelse.
Ministerios %>% dplyr::mutate(Ministerio = 
                                ifelse(Ministerio == 1, "Agricultura", 
                                ifelse(Ministerio == 2, "Hacienda",
                                 ifelse(Ministerio == 3, "Economía", 
                                        "Otro Ministerio"))))

# Recoficiar mediante sustitución de patrones.
Ministerios  %>% dplyr::mutate(Ministerio= gsub(pattern = "3", 
                                                replacement = "Economía", 
                                                x = Ministerio))

# Recodificar mediante expresiones resulares.
Ministerios  %>% dplyr::mutate(Ministerio = 
                                 stringr::str_replace(string = Ministerio, 
                                                      pattern = "1", 
                                                      replacement = "Agricultura"))

# dplyr::mutate(sexo = car::recode(sexo, "0 = 'Hombre'; 1 = 'Mujer'"))  

# Agrupar variables de tipo categórico (group by) -------------------------

# De acuerdo a lo realizado hasta ahora por la Convención Constitucional, 
# Ud. votataría por: 

datos %>% 
  dplyr::mutate_all(sjlabelled::as_label) %>% 
  dplyr::select(constitucion_8) %>% 
  dplyr::group_by(constitucion_8) %>% 
  dplyr::count()

# Dos variables

datos %>% 
  dplyr::mutate_all(sjlabelled::as_label) %>% 
  dplyr::select(constitucion_8, gse) %>% 
  dplyr::group_by(constitucion_8, gse) %>% 
  dplyr::count() %>% 
  dplyr::mutate_at(vars(constitucion_8, gse), list(~stringr::str_to_sentence(.))) 

# Niveles de confianza en instituciones. 
# Advertencia: Adelantaré algunas cosas y complejizaré otras. Por ejemplo
# los tipos de formatos de tablas y la conjugación de distintos verbos o funciones.

datos %>% 
  dplyr::mutate_all(sjlabelled::as_label) %>% 
  dplyr::select(starts_with(match = "confianza_6")) %>% 
  tidyr::pivot_longer(cols = everything()) %>% 
  dplyr::group_by(value, name) %>% 
  dplyr::count() %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(name) %>% 
  dplyr::mutate(prop = n/sum(n)*100) %>% 
  dplyr::select(-n) %>% 
  tidyr::pivot_wider(names_from = value,
                     values_from = prop) %>% 
  dplyr::ungroup() %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(total = sum(c_across(2:length(.)))) %>% 
  dplyr::rename(institucion = name) %>% 
  janitor::clean_names()
  
# Resumen estadístico de datos --------------------------------------------

# Confianza en el gobierno

confianza_gob <- datos %>% 
  dplyr::mutate_all(sjlabelled::as_label) %>% 
  dplyr::select(region, edad, confianza_6_i) %>% 
  dplyr::group_by(region, confianza_6_i) %>% 
  dplyr::summarise(prom_edad = mean(edad, na.rm = TRUE),
                   sd_edad = sd(edad, na.rm = TRUE),
                   Q1 = quantile(edad, probs = 0.25, na.rm = TRUE),
                   med_edad = median(edad, na.rm = TRUE),
                   Q3 = quantile(edad, probs = 0.75, na.rm = TRUE)) %>% 
  dplyr::rename(region = region,
                confianza = confianza_6_i,
                promedio_edad = prom_edad,
                desviacion_edad = sd_edad,
                cuartil_1 = Q1,
                mediana_edad = med_edad,
                cuartil_3 = Q3)

confianza_gob

# Otros mecanismos más sofisticados, que comenté en clases:

datos %>% 
  dplyr::mutate_all(sjlabelled::as_label) %>% 
  dplyr::select(region, edad, confianza_6_i) %>%  
  dplyr::group_by(region, confianza_6_i) %>% 
  dplyr::summarise_at(vars(c("edad")), 
                      list(~mean(.x, na.rm = TRUE), 
                           ~median(.x, na.rm =TRUE))) 

# Guardar el análisis en Excel --------------------------------------------

if(!dir.exists("analisis")) dir.create("analisis")

writexl::write_xlsx(x = confianza_gob, 
                    path = "analisis/confianza_gob.xlsx",
                    col_names = TRUE)

# Opción split

confianza_region <- confianza_gob %>% dplyr::group_split(region) 

writexl::write_xlsx(list("1era region" = confianza_region[[1]], 
                         "2da region "= confianza_region[[2]], 
                         "3era region" = confianza_region[[3]], 
                         "4ta region" = confianza_region[[4]]),
                    path = "analisis/confianza_region.xlsx")

# Opción nest 

data_nest <- datos %>% dplyr::group_nest(region)

purrr::map2(data_nest$data, data_nest$region, 
            ~write.csv(.x, file = paste0("analisis/", .y, ".csv")))

# data_nest <- data %>% group_by(Comuna) %>% nest()

# Cruce de bases ----------------------------------------------------------

base_1 <- data.frame(Diputado = c("Pamela Jiles","Karol Cariola", "Diego Paulsen"), 
                     Partido = c("PH","RD", "RN"),
                     Mociones = c(4, 2, 1), 
                     Asistencia = c(20, 18, 24))

base_2 <- data.frame(Diputado = c("Pamela Jiles","Karol Cariola","Florcita Motuda"), 
                     Edad = c(60, 34, 64),
                     Transparencia = c("NO", "SI", "SI"), 
                     Indicaciones = c("14", "26","8"))

# Left_join ---------------------------------------------------------------
# Toma como referencia las observaciones de la tabla 1.

left_join(x = base_1, y = base_2, by = c("Diputado" = "Diputado"))
left_join(base_1, base_2) # Busca la variable transversal a ambas tablas.
left_join(base_1, base_2, by = "Diputado") # Otra forma
merge(x = base_1, y = base_2, all.x = T, all.y = F)

# Right_join --------------------------------------------------------------
# Toma como referencia las observaciones de la tabla 2

right_join(x = base_1, y = base_2, by = c("Diputado" = "Diputado")) 
right_join(base_1, base_2) 
right_join(base_1, base_2, by = "Diputado")

# Inner_join --------------------------------------------------------------
# Toma como referencia solamente los campos donde existan las mismas observaciones

inner_join(x = base_1, y = base_2, by = c("Diputado" = "Diputado"))
inner_join(base_1, base_2, by = "Diputado")
inner_join(x = base_1, y = base_2)

# Full_join ---------------------------------------------------------------
# Retorna todas las grabaciones en una nueva base de datos, independiente de si 
# figuran en ambas bases. 

full_join(x = base_1, y = base_2, by = c("Diputado" = "Diputado"))
full_join(x = base_1, y = base_2, by = "Diputado")
full_join(base_1, y = base_2)

# Bind_rows ---------------------------------------------------------------

Bloque_gobierno <- data.frame(Partido = c("RN","UDI", "EVOPOLI"),
                              Mociones = c(4, 2, 1), 
                              Asistencia = c(20, 18, 24))

Oposicion <- data.frame(Partido = c("PH","RD", "PS"),
                        Mociones = c(4, 2, 1), 
                        Asistencia = c(20, 18, 24))

Bloque_gobierno %>% dplyr::bind_rows(Oposicion)

# Bind_cols ---------------------------------------------------------------

Partidos <- Bloque_gobierno %>% bind_rows(Oposicion)
Partidos

Rechazos <- c(15, 20, 50, 22, 55, 70)
Indicaciones <- c(10, 15, 27, 25, 40, 50)

Proyectos <- data.frame(Rechazos, Indicaciones)

Partidos <- Partidos %>% bind_cols(Proyectos)

# Pivot_longer ------------------------------------------------------------

data_1 <- tidyr::pivot_longer(data = Partidos, 
                              cols = c("Mociones", "Asistencia", 
                                       "Rechazos", "Indicaciones"),
                       names_to = "Tramita_ley", values_to = "Frecuencia")

# Opción 2

tidyr::gather(data = Partidos, key = "Tramita_ley", value = "Frecuencia", 
              c(2:5))

tidyr::gather(data = Partidos, key = "Tramita_ley", value = "Frecuencia", 
              c("Mociones", "Asistencia", "Rechazos", "Indicaciones"))

# Pivot_wider -------------------------------------------------------------

data_1 %>% tidyr::pivot_wider(names_from = Tramita_ley, 
                              values_from = Frecuencia)

tidyr::spread(data = data_1, 
              key = Tramita_ley,
              value = Frecuencia)





