#pizza ratings
library(tidyverse)
pizza_barstool <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-01/pizza_barstool.csv")

pizza_barstool %>%
  count(city, sort=TRUE)

pizza_barstool %>%
  count(country, sort=TRUE)

pizza_barstool %>%
  count(provider_review_count, sort=TRUE)%>%
  #mutate(n=fct_reorder(n, provider_review_count))%>%
  ggplot(aes(provider_review_count, n))+
  geom_histogram()

pizza_barstool %>%
  ggplot(aes(x=provider_review_count))+
  geom_histogram()+
  scale_x_log10()


pizza_datafiniti <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-01/pizza_datafiniti.csv")

pizza_datafiniti %>%
  count(province, sort=TRUE)

usa<-map_data('usa')

ggplot() +
  geom_map(
    data = usa, map = usa,
    aes(long, lat, map_id = region),
    color = "white", fill = "gray50", size = 0.05, alpha = 0.2
  ) +
  geom_point(
    data = pizza_datafiniti,
    aes(longitude, latitude),
    alpha = 0.8
  )