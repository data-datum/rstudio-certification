#palmer penguins
library(tidyverse)
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


library(naniar)
penguins.csv %>%
  select(sex, bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g) %>%
  gg_miss_upset()



  



