#tidy_tuesday

#coffee dataset
coffee_ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-07/coffee_ratings.csv')
library(tidyverse)
glimpse(coffee_ratings)
coffee_ratings %>%
  count(species)

library(skimr)
coffee_ratings %>%
  skimr::skim()
coffee_ratings %>%
  count(total_cup_points)

coffee_ratings %>%
  pivot_longer(cols=aroma:moisture,
               names_to="caracteristicas_cafe",
               values_to="value") %>%
  ggplot(aes(value, total_cup_points))+
  geom_smooth(method= "gam", span=0.3, color='purple')+
  facet_wrap(~caracteristicas_cafe, scale="free_y")+
  dark_theme_gray()

library(hrbrthemes)
coffee_ratings %>%
  pivot_longer(cols=aroma:moisture,
               names_to="caracteristicas_cafe",
               values_to="value") %>%
  ggplot(aes(value, total_cup_points))+
  geom_smooth(method= "gam", span=0.3, color='purple')+
  labs(title="Relación entre el puntaje y las características del cafe", 
        subtitle="tidytuesday")+
  facet_wrap(~caracteristicas_cafe, scale="free_y")+
  theme_ft_rc()+
  theme(axis.title.x=element_text(size=14), axis.title.y=element_text(size=14),
        plot.title=element_text(size=24), panel.grid.major=element_blank())

#producción mundial de café
coffee_ratings %>%
  count(country_of_origin, species, sort=TRUE)%>%
  filter(!is.na(country_of_origin))%>%
  mutate(country_of_origin=fct_reorder(country_of_origin, n))%>%
  ggplot(aes(x=n, y=country_of_origin, fill=species))+
  geom_col()+
  labs(title="Producción mundial de café",
       subtitle="tidytuesday 07-07-2020",
       y="País de origen",
       x="#")

#no olvidar, fct_reorder() tiene 2 argumentos, la variable a ordenar, y el criterio. 


coffee_ratings %>%
  filter(!is.na(processing_method))%>%
  filter(!is.na(country_of_origin))%>%
  count(country_of_origin, processing_method, sort=TRUE)%>%
  mutate(country_of_origin=fct_reorder(country_of_origin, n, sum))%>%
  ggplot(aes(x=n, y=country_of_origin, fill=processing_method))+
  geom_col()+
  labs(title="Procesamiento del café según países",
       subtitle="tidytuesday 07-07-2020",
       y="País de origen",
       x="#")
  

#no olvidar de agregar un 3 argumento en fct_reorder(), para que se acomoden las barras.

coffee_ratings %>%
  count(variety, sort=TRUE)%>%
  filter(!is.na(variety))%>%
  
