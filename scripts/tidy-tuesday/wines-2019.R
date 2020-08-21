#wine ratings
#tidy tuesday 28-05-2019

wine_ratings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-28/winemag-data-130k-v2.csv")

#from where come the best wines?

wine_ratings %>%
  count(country, sort=TRUE)%>%
  ggplot(aes(x=))

wine_ratings%>%
  ggplot(aes(x=points, fill=country))+
  geom_bar()

wine_ratings %>%
  count(country, points, sort=TRUE)%>%
  mutate(country=fct_lump(country, 15))%>%
  ggplot(aes(x=points))+
  geom_bar()+
  facet_wrap(~country)