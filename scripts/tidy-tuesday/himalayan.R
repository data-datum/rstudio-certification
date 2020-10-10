#himalayan climbers

members <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/members.csv')
expeditions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/expeditions.csv')
peaks <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/peaks.csv')

library(tidyverse)
expeditions %>%
  count(peak_name, sort=TRUE)

expeditions %>%
  count(termination_reason, sort=TRUE)

members %>%
  count(sex, sort=TRUE)
------------------------------#termination reason-------------------------------------
#borrando Unknown
expeditions %>%
  filter(!termination_reason=="Unknown")%>%
  ggplot(aes(y=termination_reason, x=highpoint_metres, colour=members))+
  geom_boxplot()

#borrar 2 categorias Other y Unknown
expeditions %>%
  filter(!termination_reason %in% c("Other", "Unknown"))%>%
  ggplot(aes(y=termination_reason, x=highpoint_metres, colour=members))+
  geom_boxplot()


expeditions %>%
  ggplot(aes(x=highpoint_metres, y=peak_name))+
  geom_col()

peaks <- expeditions %>%
  count(highpoint_metres, peak_name, sort=TRUE)

