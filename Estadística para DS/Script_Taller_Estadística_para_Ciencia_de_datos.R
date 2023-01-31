#---------------------------------------#
# Taller Estadística y Ciencia de datos #
#---------------------------------------#

library()
#install.packages("readr")
library(readr)
getwd()
setwd("/Users/vjleiva/Diplomados/Diplomado Mate/Datapalloza")

vuelos_csv <-read_csv("FLIGHTDELAYS.csv")
names(vuelos_csv)
str(vuelos_csv)


#install.packages("readxl")
library(readxl)
vuelos_excel <- read_excel("FLIGHTDELAYS.xlsx")




library(dplyr)
select(vuelos_csv,6:8)
select(vuelos_csv,starts_with("De"))
select(vuelos_csv,ends_with("y"))

filter(vuelos_csv, DelayCategory=="No Delay" | DelayCategory=="11+ Minutes")

vuelos_csv %>%
select(DelayCategory,DestinationType,Delay) %>%
filter(DelayCategory=="No Delay", Delay <0)



vuelos_csv %>%
select(DelayCategory,DestinationType,Delay) %>%
filter(DelayCategory %in% c("1-10 Minutes","11+ Minutes"), Delay >0)


select(vuelos_csv,Origin,Destination)%>%
mutate(ncharOrigin = nchar(Origin),
ncharDestination = nchar(Destination),
OrigenDestino = case_when(ncharOrigin > ncharDestination ~ "Mayor Nchar Origin",
ncharOrigin < ncharDestination ~ "Mayor Nchar Destino",
TRUE ~ "Iguales"
))

vuelos_csv %>%
group_by(DelayCategory) %>%
summarise(n = n(),
media = mean(Delay),
SD = sd(Delay))



#install.packages("ggplot2")
library(ggplot2)
data(diamonds)
head(diamonds)
tail(diamonds)

ggplot(diamonds,aes(carat))+
geom_density()

ggplot(diamonds,aes(carat))+
geom_density(fill="blue",alpha=0.1)



ggplot(diamonds,aes(carat))+
geom_density(adjust=5,fill="blue",alpha=0.2)

ggplot(diamonds,aes(carat,fill=cut))+
geom_density(adjust=5,alpha=0.2)




ggplot(diamonds,aes(cut))+
geom_bar(fill="red",alpha=0.3)

ggplot(diamonds,aes(cut,fill=color))+
geom_bar(alpha=0.3)



ggplot(diamonds,aes(cut,fill=color))+
geom_bar(alpha=0.3,position="dodge")


ggplot(diamonds,aes(cut,fill=color))+
geom_bar(alpha=0.3,position="dodge")+
facet_wrap(~color)+
theme(axis.text.x=element_text(angle=90,hjust=1))

ggplot(diamonds,aes(carat))+
geom_histogram(fill="red",alpha=0.3)

qqnorm(diamonds$carat)
qqline(diamonds$carat)

ggplot(diamonds,aes(carat,price,color=cut))+
geom_point(alpha=0.2,size=3)+
facet_wrap(~cut)



library(car)
data(Prestige)
head(Prestige)

ggplot(Prestige,aes(education,income))+
geom_point(color="red",alpha=0.2,size=3)+
geom_smooth(method="lm")





ggplot(Prestige,aes(education,income))+
geom_point(color="red",alpha=0.2,size=3)+
geom_smooth(span=0.3)



ggplot(Prestige,aes(type,income))+
geom_boxplot(alpha=0.3,fill="red")


