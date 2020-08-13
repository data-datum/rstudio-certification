# leccion de the carpentries
# R para analisis cientificos reproducibles
# numero 13
https://swcarpentry.github.io/r-novice-gapminder-es/13-dplyr/index.html 

#diferencia entre filter() y select()
#cargo las librerías
library(dplyr)
library(gapminder)

#select
#con select elijo las columnas de un dataset que quiero conservar
gapminder %>%
  select(country, lifeExp)


#filter 
#con filter elijo filas de un dataset que siguen un criterio lógico
gapminder %>%
  filter(lifeExp>60)



