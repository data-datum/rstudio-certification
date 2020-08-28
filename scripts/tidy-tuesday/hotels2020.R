#hotels 
#tidy tuesday 11-02-2020
library(tidyverse)
hotels <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-11/hotels.csv')

hotels %>%
  count(hotel)

hotels %>%
  count(meal, sort=TRUE)

hotels %>%
  count(adults, sort=TRUE)

hotels %>%
  count(hotel, meal)%>%
  ggplot(aes(x=hotel, y=n, fill=meal))+
  geom_col()

hotels%>%
  count(country, sort=TRUE)


hotels%>%
  ggplot(aes(x=distribution_channel))+
  geom_histogram()