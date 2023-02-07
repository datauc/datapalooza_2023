#Hola mundo


# Paso 1 de R -------------------------------------------------------------

5 + 5
#5 + 5

5 + 5 #comentario random


# Definir variable --------------------------------------------------------

a <- 5+5
5 +5 -> a
print(a)

a = 5
5 = a



# Vectores ----------------------------------------------------------------

poleras <- c(254,203,182,50)
poleras

(poleras <- c(254,203,182,50))
meses <- c("Ene", "Feb", "Mar", "Abr")

min(poleras)
max(poleras)

plot(poleras)
barplot(poleras, names.arg = Meses, col="orange")

getwd()


# Escribiendo data --------------------------------------------------------
cbind(meses, poleras)
write.csv(cbind(meses, poleras),"datos/poleras.csv")
rbind(meses, poleras)
write.csv(rbind(meses, poleras),"datos/poleras2.csv")


read.csv("datos/poleras.csv")

df0 = read.csv("datos/poleras.csv")



# Tidyverse ---------------------------------------------------------------

install.packages("tidyverse")
library(tidyverse)



# Calculos ----------------------------------------------------------------

5 * 100 + 5
5 + 100 * 5

poleras + 5
poleras%/%9
poleras/9
sum(poleras)


poleras[1] + 5

a = 5
a = a + 10






