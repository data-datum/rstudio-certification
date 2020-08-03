#manipulacion 
#install.packages(vuelos)
library(datos)
library(tidyverse)
vuelos %>%
  filter(dia==1, mes==1)
vuelos %>%
  filter(mes==11 | mes==12)
filter(vuelos, mes %in% c(11, 12))
is.na(vuelos) #buscar valores perdidos
any(is.na(vuelos)) #buscar valores perdidos

#ejercicios
# 1. Encuentra todos los vuelos que:
# a. Tuvieron un retraso de llegada de dos o más horas

glimpse(vuelos)
vuelos %>%
  filter(atraso_llegada >= 120)

# b. Volaron a Houston (IAH o HOU)

vuelos %>%
  filter(destino %in% c("IAH", "HOU"))

# c. Fueron operados por United, American o Delta
vuelos %>%
  filter(aerolinea %in% c("AA", "DL", "UA")) 
#use este codigo para ver todos los nombres de las aerolineas
vuelos %>%
  select(aerolinea) %>%
  group_by(aerolinea) %>%
  summarize()
  
# d. Partieron en invierno (julio, agosto y septiembre)
vuelos %>%
  filter(mes %in% c(7, 8, 9))

# e. Llegaron más de dos horas tarde, pero no salieron tarde

vuelos %>%
  filter(atraso_llegada > 120  | atraso_salida < 0)

# f. Se retrasaron por lo menos una hora, pero repusieron más de 30 minutos en vuelo

vuelos %>%
  filter(atraso_llegada >= 60, tiempo_vuelo > 30 )

# g. Partieron entre la medianoche y las 6 a.m. (incluyente)

vuelos %>%
  filter(between(horario_salida, 0, 600))

# 2. uso de between para simplificar
# se pueden simplificar el punto g y el d

vuelos %>% 
  filter(between(mes, 7, 9))

#between sirve cuando queremos indexar entre rangos numericos, con variables numericas, no categoricas. 

#¿Cuántos vuelos tienen datos faltantes en horario_salida?

vuelos %>%
  filter(is.na(horario_salida))%>%
  count()

# ¿Qué otras variables tienen valores faltantes? 
vuelos %>%
  filter(is.na(across()))

#     