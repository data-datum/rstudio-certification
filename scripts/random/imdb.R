#imdb movies
#url https://www.kaggle.com/orgesleka/imdbmovies/data?select=imdb.csv
#importo librerias
library(tidyverse)
movies<-read_csv("scripts/random/imdb.csv")
movies%>%
  filter(!is.na(year))%>%
  group_by(year)%>%
  summarize(total=n()) %>%
  ungroup()%>%
  ggplot(aes(x=year, y=total))+
  geom_col()



#es lo mismo que 
movies %>%
  count(year, sort=TRUE)

movies%>%
  filter(!is.na(year))%>%
  ggplot(aes(x=year))+
  geom_bar()

#hago una coercion de valores
movies2 <-movies %>% 
  mutate(rating = as.numeric(imdbRating))

movies2 %>%
  ggplot(aes(x=rating, fill=year))+
  geom_bar()
#contar los NAs
movies %>%
  purrr::map_df(~sum(is.na(.)))

movies2 %>%
  count(year, sort=TRUE)%>%
  ggplot(aes(x=year, y=n))+
  geom_col()

str(movies2)
class(movies2$year)
typeof(movies2$year)

horror4<-read.csv("scripts/random/imdb.csv")
glimpse(horror3)
#la variable imdbRatings la leyo como caracter
horror3%>%
  select(imdbRating)%>%
  as.double()


horror3$imdbRating<-as.integer(horror3$imdbRating)
horror3%>%
  count(imdbRating, sort=TRUE)

horror3 %>%
  purrr::map_df(~sum(is.na(.)))


horror3 <-horror3 %>%
  janitor::clean_names()

