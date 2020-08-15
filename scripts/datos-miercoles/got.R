#game of thrones
#datos de miercoles 17-04-2020

tiempo_pantalla <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-04-17/tiempo_pantalla.csv")

cambio_lealtades <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-04-17/cambio_lealtades.csv")

personajes_libros <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-04-17/personajes_libro.csv")


library(tidyverse)
glimpse(tiempo_pantalla)
library(viridis)
tiempo_pantalla %>%
  filter(episodios>10)%>%
  mutate(nombre=fct_reorder(nombre, minutos_pantalla))%>%
  ggplot(aes(x=nombre, y=minutos_pantalla, fill=episodios)) +
  geom_col()+
  coord_flip()+
  scale_fill_viridis() +
  theme_bw()+
  labs(title="Game of Thrones", x='', y='',
       subtitle="datos de miércoles 17-04-2019",
       caption="@data-datum")+
  geom_hline(yintercept=25)+
  theme(axis.title.x=element_text(size=2), axis.title.y=element_text(size=14),
        plot.title=element_text(size=24), panel.grid.major=element_blank())
