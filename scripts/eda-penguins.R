#palmer penguins

#para limpiar datos
#datos crudos
penguins_raw.csv <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-28/penguins_raw.csv')

library(tidyverse)
#clean data
penguins.csv <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-28/penguins.csv')

glimpse(penguins.csv)
is.na(penguins.csv) #verificar si hay valores perdidos
penguins.csv[!complete.cases(penguins.csv),] 
#para saber si hay datos perdidos en tidyverse
library(skimr)
skimr::skim(penguins.csv) 
#esta funcion tira valores globale
#se puede ver q hay 11 valores perdidos en la variable sexo 
#tambien hay valores perdidos en bill_length_mm(2), bill_depth_mm(2), flipper_length_mm(2) y body_mass(2)

#cuanto hay por grupo para ver si estan balanceadas
library(kableExtra)
penguins.csv %>%
  count(species, sort = TRUE)

penguins.csv %>%
  count(island, sort = TRUE)

penguins.csv %>%
  count(sex, sort = TRUE)

penguins.csv %>%
  count(species, island, sex)

#en caso de hacer clasificaciones, casi todas las categorias estan desbalanceadas
#data desbalanceada segun especie
ggplot(data = penguins.csv) +
  geom_bar(mapping = aes(x = species))
#distribuciones de variables
ggplot(data = penguins.csv) +
  geom_histogram(mapping = aes(x = bill_length_mm, colour=species)) 

ggplot(data = penguins.csv) +
  geom_histogram(mapping = aes(x = bill_depth_mm, fill=species)) 

ggplot(data = penguins.csv) +
  geom_histogram(mapping = aes(x = body_mass_g, fill=species)) 

ggplot(data = penguins.csv) +
  geom_histogram(mapping = aes(x = flipper_length_mm, fill=species)) 

ggplot(data = penguins.csv, mapping = aes(x = species, y = bill_length_mm)) +
  geom_boxplot()

ggplot(data = penguins.csv, mapping = aes(x = species, y = bill_depth_mm, fill= species)) +
  geom_boxplot()+
  labs(x="especie", y="profundidad del pico")
#tambien se puede escribir de esta manera
ggplot(data = penguins.csv) +
  geom_boxplot(aes(x = species, y = bill_depth_mm, fill= species))+
  labs(x="especie", y="profundidad del pico")+ #profundidad del pico
  facet_grid(~sex)

ggplot(data = penguins.csv) +
  geom_boxplot(aes(x = species, y = bill_length_mm, fill= species), alpha = 0.4)+ #alpha controla la transparencia
  labs(x="especie", y="longitud del pico")+ #longitud del pico 
  facet_grid(~sex)

#para ver datos perdidos
library(naniar)
penguins.csv %>%
  select(sex, bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g) %>%
  gg_miss_upset()

#para ver las distribuciones de todas las variables
#distribucion en general
penguins.csv %>%
  pivot_longer(cols=bill_length_mm:body_mass_g,
               names_to="metric",
               values_to="value") %>%
  ggplot(aes(value))+
  geom_histogram(bins=20)+
  facet_wrap(~metric, scales="free_x")

#distribucion segun especie
penguins_pivoted <- penguins.csv %>%
  pivot_longer(cols=bill_length_mm:body_mass_g,
               names_to="metric",
               values_to="value") 
penguins_pivoted %>%
  ggplot(aes(value, fill=species))+
  geom_histogram(bins=20)+
  facet_wrap(~metric, scales="free_x")


penguins_pivoted %>%
  ggplot(aes(value, fill=species))+
  geom_density(alpha=0.5)+
  facet_wrap(~metric, scales="free")


penguins_pivoted %>%
  ggplot(aes(species, value))+
  geom_boxplot()+
  facet_wrap(~metric, scales="free_y")


penguins.csv %>%
  ggplot(aes(year, fill=species))+
  geom_bar()

penguins.csv %>%
  ggplot(aes(island, fill=species))+
  geom_bar()

#analisis de correlacion
#fuente
library(corrr)

penguins.csv %>%
  select(bill_length_mm:body_mass_g) %>%
  correlate() %>%
  rearrange() %>%
  shave() %>%
  rplot(shape = 20, colours = c("indianred2", "white", "skyblue1"))

#para usar esta libreria NO DEBE HABER VALORES PERDIDOS, SINO DA ERROR. 

library(corrplot) #librería corrrplot para graficar
penguins<-penguins.csv %>% #ingreso de datos
  select(bill_length_mm:body_mass_g)%>%
  na.omit()

M <- cor(penguins) #calculo matriz de correlación 
corrplot(M, method="number") #grafico


#imputacion de valores
  

