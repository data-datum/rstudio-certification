#TV series

library(tidyverse)
tv_series <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-08/IMDb_Economist_tv_ratings.csv')
tv_series %>%
  count(title, sort=TRUE) %>%
  mutate(temporadas = n)%>%
  filter(temporadas>10)%>%
  ggplot(aes(y=title, x=temporadas))+
  geom_col()
#como se distribuye el puntaje en los distintos generos
tv_pivoted <-tv_series %>%
  separate_rows(genres, sep=",")

tv_pivoted %>%
  ggplot(aes(x=av_rating, y=genres))+
  geom_boxplot()

#cuantas series hay por genero
tv_pivoted %>%
  count(genres, sort=TRUE)

#grafico de barras por genero y pintado por año
tv_pivoted %>%
  ggplot(aes(x=genres, fill=date))+
  geom_bar()+
  coord_flip()

by_year <-tv_pivoted %>%
  separate(date, sep="-", into = c("year", "month", "day"))
by_year %>%
  ggplot(aes(x=av_rating, y=year))+
  geom_boxplot()+
  facet_wrap(~genres)

