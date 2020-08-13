#gapminder dataset
#grafico de lineas

library(tidyverse)
library(gapminder)
library(ggdark)
glimpse(gapminder)
#todos
gapminder %>%
  ggplot(aes(x=year, y=lifeExp, fill=country, color=continent))+
  geom_line()
#por continente facetado
gapminder %>%
  ggplot(aes(x=year, y=lifeExp, fill=country, color=continent))+
  geom_line()+
  facet_wrap(~continent)
#por paises de america 
gapminder %>%
  filter(continent=="Americas")%>%
  ggplot(aes(x=year, y=lifeExp, color=country))+
  geom_line()+
  facet_wrap(~country)+
  dark_theme_gray()+
  labs(title="Países de Latinoamérica - gapminder dataset", 
       subtitle="datosdemiercoles 24-04-2019",
       caption="@data_datum")

library(here)
ggsave(here("gapminder.png"), height = 8, width = 15, units = "in")

#lo hacemos interactivo
g <-gapminder %>%
  filter(continent=="Americas")%>%
  ggplot(aes(x=year, y=lifeExp, color=country))+
  geom_line()+
  facet_wrap(~country)+
  dark_theme_gray()+
  labs(title="Países de Latinoamérica - gapminder dataset", 
       subtitle="datosdemiercoles 24-04-2019",
       caption="@data_datum")

library(plotly)
ggplotly(g)


#boxplot para la ver la variación intra-continente
gapminder%>%
  ggplot(aes(x=continent, y=lifeExp))+
  geom_boxplot(outlier.colour = "hotpink")+
  labs(title="Variación de la Esperanza de vida intra-continente",
       subtitle="datosdemiercoles 24-04-2019",
       caption="@data-datum")+
  geom_jitter(position = position_jitter(width = 0.1, height = 0), alpha = 1/4)

#agrego interactividad para conocer esos valores extremos

pg<-gapminder%>%
  ggplot(aes(x=continent, y=lifeExp))+
  geom_boxplot(outlier.colour = "hotpink")+
  labs(title="Variación de la Esperanza de vida intra-continente",
       subtitle="datosdemiercoles 24-04-2019",
       caption="@data-datum")+
  geom_jitter(position = position_jitter(width = 0.1, height = 0), alpha = 1/4)


ggplotly(pg)