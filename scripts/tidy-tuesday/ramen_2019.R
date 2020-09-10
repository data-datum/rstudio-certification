#ramen ratings
#tidy tuesday 2019
ramen_ratings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-04/ramen_ratings.csv")

#distribucion de stars
ramen_ratings %>%
  ggplot(aes(x=stars))+
  geom_histogram(bins=50)


ramen_ratings %>%
  count(style, sort=TRUE)%>%
  drop_na()

ramen_ratings %>%
  count(country, sort=TRUE)

ramen_ratings %>%
  count(country, style, sort=TRUE)%>%
  drop_na()%>%
  mutate(n=total)%>%
  ggplot(aes(x=total, y=country, fill=style))+
  geom_col()

library(tidyverse)
ramen_ratings %>%
  select(sort(colnames(ramen_ratings)))

