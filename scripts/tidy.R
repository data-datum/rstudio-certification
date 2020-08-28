#tidyverse
#1
health<-read_csv("scripts/data/at_health_facilities.csv")
health %>%
  count(iso3)
#in total 100 countries reported values. 

#2
health %>%
  filter(!is.numeric(`age 15-17`))%>%
  group_by(iso3)%>%
  summarize(min=min(year),
            max=max(year))

health %>%
  mutate(`age 20-34` = as.numeric(`age 20-34`))%>%
  drop_na() %>%
  group_by(iso3)%>%
  summarize(min_year=min(year),
            max_year=max(year), 
            dif=max_year-min_year)

#3
health %>%
  count(iso3, sort=TRUE)%>%
  filter(n>=3)
#otra forma
health %>%
  group_by(iso3)%>%
  summarize(n=n()) %>%
  filter(n>=3)

#4
health%>%
  filter(`age 15-17`==100 | `age 20-34` == 100)


#ejercicio propuesto por riva
#de reproducir un boxplot
#datos

imdb <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2020/2020-02-19/ranking_imdb.csv")

glimpse(imdb)
imdb_pivot <-imdb %>%
  separate(genero, c("gen1", "gen2", "gen3"), sep=", ")%>%
  pivot_longer(cols=gen1:gen3,
               names_to="genre",
               values_to="value")%>%
  filter(!is.na(value))

library(hrbrthemes)
library(viridis)
library(viridisLite)
imdb_pivot %>%
  filter(value  %in% c("Aventura", "Comedia", "Drama", "Fantasía", "Horror"))%>%
  ggplot(aes(x=value, y=puntaje))+
  geom_boxplot(aes(fill = value, alpha = 0.3), outlier.alpha = 0.3, outlier.shape = 1)+
  coord_flip()+
  #scale_fill_viridis(discrete = TRUE, alpha=0.6)+
  theme_ipsum()



## otra forma
imdb2 <- separate_rows(imdb,genero,sep = ", ")


# Reproducción del gráfico de Riva ----------------------------------------

imdb2 %>% filter(genero %in% c("Aventura","Comedia","Drama","Fantasía","Horror")) %>% 
  ggplot(mapping=aes(x=puntaje,y=genero)) +
  geom_boxplot(aes(fill = genero, alpha = 0.3), outlier.alpha = 0.3, outlier.shape = 1) +
  theme_minimal()


# comentarios útiles
# en vez de hacer pivot_longer + filter se puede directamente hacer separate_rows


