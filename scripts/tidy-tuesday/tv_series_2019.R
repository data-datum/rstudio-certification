#TV series

library(tidyverse)
tv_series <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-08/IMDb_Economist_tv_ratings.csv')
tv_series %>%
  count(title, sort=TRUE) %>%
  mutate(temporadas = n)%>%
  filter(temporadas>10)%>%
  ggplot(aes(y=title, x=temporadas))+
  geom_col()

tv_series %>%
  count(title, av_rating, sort=TRUE)



  
