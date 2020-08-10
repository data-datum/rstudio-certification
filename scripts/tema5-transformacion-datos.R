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
  filter(atraso_llegada > 120  | atraso_salida <= 0)
#importante aca incluir el valor 0 tambien, los que salieron a tiempo

# f. Se retrasaron por lo menos una hora, pero repusieron más de 30 minutos en vuelo

vuelos %>%
  filter(atraso_llegada >= 60, tiempo_vuelo > 30 )

# g. Partieron entre la medianoche y las 6 a.m. (incluyente)

vuelos %>%
  filter(between(horario_salida, 0, 600))

#importante
#las 0 horas en el dataset esta mencionado como 2400, se puede ver en este codigo
summary(vuelos$horario_salida)
vuelos %>%
  filter(horario_llegada <= 600 | horario_salida == 2400)

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
summary(vuelos)

# ARRANGE

# poner los valores faltantes al inicio
#para ello tenemos saber que columna tiene valores faltantes
#hacemos un summary() previo
vuelos %>%
  arrange(desc(is.na(horario_salida)), horario_salida)

# encontrar los vuelos mas retrasados

vuelos %>%
  arrange(desc(atraso_llegada)) 

#encuentra los vuelos que salieron mas temprano

vuelos %>%
  arrange(atraso_salida)

# vuelos más rapidos

vuelos %>%
  arrange(tiempo_vuelo)

#si pensamos en la velocidad

vuelos %>%
  arrange(desc(distancia/tiempo_vuelo))

# ¿Cuáles vuelos viajaron más lejos?

vuelos %>%
  arrange(desc(distancia))

# ¿Cuál viajó más cerca?
vuelos %>%
  arrange(distancia)

#SELECT

#seleccionar por nombres de columnas

vuelos %>%
  select(anio, mes, dia)
# lo mismo de otra forma
vuelos %>%
  select(anio:dia)
#eliminar columnas con signo menos
vuelos %>%
  select(-(anio:dia))

#si quiero una columna particular que aparezca primero
vuelos %>%
  select(atraso_salida, everything())

#ejercicios de select

# 1. Haz una lluvia de ideas sobre tantas maneras como sea posible para 
# seleccionar horario_salida,atraso_salida,horario_llegada, yatraso_llegada de vuelos.
#opcion 1: definiendo las variables explicitamente
vuelos %>%
  select(horario_salida, atraso_salida, horario_llegada, atraso_llegada)
#opcion 2: definir mediante un vector
variables<-c("horario_salida", "atraso_salida", "horario_llegada", "atraso_llegada")
vuelos %>%
  select(variables)
#opcion 3: mediante un patron con el cual empiezan los nombres
vuelos %>%
  select(starts_with("horario"), starts_with("atraso"))
#opcion 4: mediante un patron con el cual termina el nombre
vuelos %>%
  select(ends_with("salida"), ends_with("llegada"))
#opcion 5: mediante un patron que contiene esas palabras
vuelos %>%
  select(contains("salida"), contains("llegada"))
#opcion 6: se puede indexar por el numero de la columna
vuelos %>%
  select(4, 6, 7, 9)
#opcion 7: las columnas se pueden especificar mediante strings
vuelos %>%
  select("horario_salida", "atraso_salida", "horario_llegada", "atraso_llegada")
#opcion 8: con any_of o all_of
vuelos %>%
  select(any_of(c("horario_salida", "atraso_salida", "horario_llegada", "atraso_llegada")))
vuelos %>%
  select(all_of(c("horario_salida", "atraso_salida", "horario_llegada", "atraso_llegada")))
#opcion 9: si especifico las variables como strings
variabl<-c("horario_salida", "atraso_salida", "horario_llegada", "atraso_llegada")
vuelos %>%
  select(!!variabl)
#opcion 10: con la funcion matches()
vuelos %>%
  select(matches("^(horario|atraso)_(salida|llegada)$"))

# ¿Qué sucede si incluyes el nombre de una variable varias veces en una llamada a select()?

vuelos %>%
  select(dia, dia)
# el resultado no duplica la columna

# ¿Qué hace la función one_of()? ¡¿Por qué podría ser útil en conjunto con este vector?

vars <- c ("anio", "mes", "dia", "atraso_salida", "atraso_llegada")

vuelos %>%
  select(one_of(c(vars)))

#selecciona todas esas columnas

# ¿Te sorprende el resultado de ejecutar el siguiente código? 
# ¿Cómo tratan por defecto las funciones auxiliares de select() a las palabras en mayúsculas o en minúsculas? 
# ¿Cómo puedes cambiar ese comportamiento predeterminado?
vuelos %>%
  select(contains("SALIDA"))

#MUTATE
library(tidyverse)
library(datos)
vuelos_sml<-select(vuelos, 
                   anio:dia, 
                   starts_with("atraso"),
                   distancia, 
                   tiempo_vuelo
                   )
vuelos_sml %>%
  mutate(ganancia = atraso_salida - atraso_llegada, 
         velocidad= distancia/tiempo_vuelo*60)

vuelos %>%
  transmute(ganancia=atraso_salida-atraso_llegada,
            horas=tiempo_vuelo/60, 
            ganancia_por_hora=ganancia/horas)

#EJERCICIOS MUTATE
# 2. Compara tiempo_vuelo con horario_llegada - horario_salida. 
# ¿Qué esperas ver? ¿Qué ves? ¿Qué necesitas hacer para arreglarlo?

vuelos %>%
  mutate(tiempo_vuelo_real=horario_llegada - horario_salida) %>%
  select(tiempo_vuelo_real, tiempo_vuelo, horario_llegada, horario_salida, everything())

# es necesario sobreescribir la columna tiempo_vuelo

# 3. Compara horario_salida, salida_programada, y atraso_salida. 
# ¿Cómo esperarías que esos tres números estén relacionados?

vuelos %>%
  select(horario_salida, salida_programada, atraso_salida)

# atraso_salida es la resta de horario salida - salida_programada

# 4. Encuentra los 10 vuelos más retrasados utilizando una función de ordenamiento. 
# ¿Cómo quieres manejar los empates? Lee atentamente la documentación de min_rank().


vuelos %>%
  select(atraso_salida, atraso_llegada, everything())%>%
  arrange(desc(atraso_salida))%>%
  head(10) #los 10 primeros

#este codigo NO es correcto
vuelos %>%
  select(atraso_salida, atraso_llegada, everything())%>%
  arrange(desc(atraso_salida))%>%
  slice_max(10)


#group_by() y summarise()
library(datos)
library(tidyverse)
vuelos %>%
  summarise(atraso=mean(atraso_salida, na.rm=TRUE))
  


------------------------------------------------------------------------------------------
----------------#ejercicio random--------------------------------------------------------


g <- mtcars %>%
  group_by(cyl)
g

g %>%
  summarise(
    disp=mean(disp),
    sd=sd(disp)
  )
    
g%>%
  summarise(
    data.frame(
      disp=mean(disp),
      sd=sd(disp)
    )
  )



  