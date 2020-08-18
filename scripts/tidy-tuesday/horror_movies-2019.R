#horror movies 
horror_movies <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-22/horror_movies.csv")

library(tidyverse)
horror_movies %>%
  purrr::map_df(~sum(is.na(.)))

horror_movies%>%
  count(genres)

horror_movies %>%
  count(release_country, sort=TRUE)%>%
  filter(n > 10)%>%
  ggplot(aes(x=release_country, y=n))+
  geom_col()+
  coord_flip()

horror<-horror_movies %>%
  extract(title, "year", "\\((\\d\\d\\d\\d)\\)", remove=FALSE)

horror_pivoted <-horror %>%
  separate(genres, sep="\\| ", into=c("genre1", "genre2", "genre3"))%>%
  pivot_longer(cols=genre1:genre3,
               names_to="genre",
               values_to="value")%>%
  filter(!is.na(value))%>%
  View()

#grafico en donde se ven los 3 principales generos de peliculas 
horror_pivoted %>%
  ggplot(aes(x=genre, fill=value))+
  geom_bar()

#cantidad de peliculas por año a partir de 2009
horror %>%
  count(year, sort=TRUE)%>%
  filter(year>2008)%>%
  mutate(total=n)%>%
  ggplot(aes(x=year, y=total))+
  geom_col()



  
