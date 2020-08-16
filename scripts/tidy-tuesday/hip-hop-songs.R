#rap_artists

polls <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/polls.csv')
rankings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/rankings.csv')

library(tidyverse)
rankings %>%
  count(year)
polls%>%
  count(year)%>%
  View()

#visualizacion de una linea temporal
library(ggdark)
library(hrbrthemes)
library(ggtext)

polls%>% 
  count(year)%>%
  ggplot(aes(x=year, y=n))+
  geom_line()+
  labs(title="Best hip hop songs of all time",
       subtitle="TidyTuesday 14-04-2020",
       caption="@data-datum",
       x="year",
       y="number of songs")+
  geom_vline(xintercept = 1984, linetype="dotted", 
             color = "yellow", size=0.5)+
  geom_vline(xintercept = 1996, linetype="dotted", 
             color = "yellow", size=0.5)+
  geom_vline(xintercept = 2009, linetype="dotted", 
             color = "yellow", size=0.5)+
  theme_ft_rc()+
  theme(
    axis.title.y = element_textbox_simple(
      orientation = "left-rotated",
      width = NULL,
      padding = margin(4, 4, 4, 4),
      margin = margin(4, 0, 0, 0),
      linetype = 1,
      r = grid::unit(8, "pt"),
      fill = "yellow"),
    axis.title.x = element_textbox_simple(
      width = NULL,
      padding = margin(4, 4, 4, 4),
      margin = margin(4, 0, 0, 0),
      linetype = 1,
      r = grid::unit(8, "pt"),
      fill = "yellow")
  )+
  annotate(
    geom="text", x=1980, y=43,label="Old-school Era",
    color="yellow"
  )+
  annotate(
    geom="text", x=1988, y=43, label="Golden Age",
    color="yellow"
  )+
  annotate(
    geom="text", x=2002, y=43, label="Bling-bling era",
    color="yellow"
  )+
  annotate(
    geom="text", x=2014, y=43, label="Internet Era",
    color="yellow"
  )
  
