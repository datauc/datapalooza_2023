## Intro a R parte 2


# El de ayuda -------------------------------------------------------------

help(sqrt)
help(lm)

install.packages("readxl")
args(read.csv)

library(readxl)
args(read_excel)



# Tipo de datos -----------------------------------------------------------

vector_1 <- c(-1/2, 1, 0.5)
vector_1


b = matrix(1:9, ncol = 3)

##    nombre grupo_s altura_cm
## 1  Andrea      AB       165
## 2 Bastian       0       180
## 3  Camilo       A       158
## 4 Daniela       B       170


data.frame(
  nombre = c("Andrea", "Bastian", "Camilo", "Daniela"),
  grupo_s = c("AB", "0", "A", "B"),
  altura_cm = c(165,180,158,170)
)



# Carga de datos ----------------------------------------------------------

library(readxl)
df1 <- read_excel("datos/viviendasRM.xlsx")
View(df1)

head(df1)
str(df1)



# Ejercicio practico ------------------------------------------------------

#b
count(unique(df1$Comuna)) #pinche r
length(unique(df1$Comuna)) 
summary(unique(df1$Comuna)) 
nrow(unique(df1$Comuna)) #se marea porque no es matriz ni data frame, es un vector

#c
##Pt1
range(df1$N_Habitaciones, na.rm = T)
#Notar que el parámetro na.rm da la indicación de ignorar los NA para poder
#obtener el valor numérico sin problemas.

#Son 19 casos distintos. Como son tantos, basta mostrar la lógica con los primeros 3

##Pt2
hab1 = df1[df1$N_Habitaciones == 1,]
hab2 = df1[df1$N_Habitaciones == 2,]
hab3 = df1[df1$N_Habitaciones == 3,]

#Recordar que es un data.frame, por lo que es necesario colocar la ","
#¿Por qué?
#Porque el operador lógico solo nos dará las posiciones en donde es cierto, 
#tras eso le decimos que considere esas posiciones para filtrar la base


##Pt3
mean(hab1$Valor_UF, na.rm=T) ; mean(hab1$Valor_CLP, na.rm=T)
mean(hab2$Valor_UF, na.rm=T) ; mean(hab2$Valor_CLP, na.rm=T)
mean(hab3$Valor_UF, na.rm=T) ; mean(hab3$Valor_CLP, na.rm=T)

#Pueden correr dos comandos en la misma línea separando con ";"

#d
##Pt1 
uf1 = df1[df1$Valor_UF < 5000,]
uf2 = df1[df1$Valor_UF >= 5000 & df1$Valor_UF <= 10000,]
uf3 = df1[df1$Valor_UF > 10000,]

##Pt2
### promedio
mean(uf1$Total_Superficie_M2, na.rm=T)
mean(uf2$Total_Superficie_M2, na.rm=T)
mean(uf3$Total_Superficie_M2, na.rm=T)

### min - max
min(uf1$Total_Superficie_M2, na.rm=T) ; max(uf1$Total_Superficie_M2, na.rm=T) 
min(uf2$Total_Superficie_M2, na.rm=T) ; max(uf2$Total_Superficie_M2, na.rm=T) 
min(uf3$Total_Superficie_M2, na.rm=T) ; max(uf3$Total_Superficie_M2, na.rm=T) 

### variabilidad

#Acá pueden sacar el rango, o también pueden calcular estadísticos como la 
#varianza y la desviación estándar. Si no están tan familiarizados, os incito
#a buscar al respecto.

range(uf1$Total_Superficie_M2, na.rm=T) 
var(uf1$Total_Superficie_M2, na.rm=T) ; sd(uf1$Total_Superficie_M2, na.rm=T)

range(uf2$Total_Superficie_M2, na.rm=T) 
var(uf2$Total_Superficie_M2, na.rm=T) ; sd(uf2$Total_Superficie_M2, na.rm=T)

range(uf2$Total_Superficie_M2, na.rm=T) 
var(uf2$Total_Superficie_M2, na.rm=T) ; sd(uf2$Total_Superficie_M2, na.rm=T)

###Pt3
#Dato tip: Pueden conocer los valores que hay y sus cantidades con el comando table()
table(uf1$N_Estacionamientos)
dim(uf1[uf1$N_Estacionamientos != "No",])
#Nota: El comando dim() calcula la dimensión y entrega el dato filas, columnas
#Es importante notar que esto funciona para los datos que tienen filas, columnas

dim(uf2[uf2$N_Estacionamientos != "No",])
dim(uf3[uf3$N_Estacionamientos != "No",])

###Pt4 (sin trampa)
#Se pueden tomar de lo hecho anteriormente

v1 = dim(uf1[uf1$N_Estacionamientos != "No",])
v2 = dim(uf2[uf2$N_Estacionamientos != "No",])
v3 = dim(uf3[uf3$N_Estacionamientos != "No",])

#Estos son vectores, por lo que podemos seleccionar uno de sus 2 valores

v1[1] / nrow(uf1) #67.7%
v2[1] / nrow(uf2) #53.7%
v3[1] / nrow(uf3) #21.4%

#Si quieren mostrarlo directamente más bonito igual pueden.
#Indaguen el comando round



# Fin y despedida ---------------------------------------------------------

#Eso pues, muchas gracias por su atención
#muchas gracias por asistir y participar tanto, that's always good.

#Cualquier cosa me pueden contactar por Linkedin
#Que tengan muy buen año y espero que no veamos durante este viaje.
