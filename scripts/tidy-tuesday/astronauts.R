#astronauts
#tidy tuesday

astronauts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-14/astronauts.csv')

a <- astronauts %>%
  count(nationality,sex, sort=TRUE)%>%
  rename(total=n)%>%
  filter(total>4)%>%
  mutate(nationality = nationality %>% fct_infreq() %>% fct_rev()) %>%
  ggplot(aes(nationality, y=total, fill=sex)) +
  geom_col()+
  coord_flip()+
  labs(title= "Number of Astronauts by Country, 1961-2019",
       subtitle= "TidyTuesday 14-07-2020")+
  theme_void()+
  theme(
    plot.title = element_text(color="yellow",hjust = 0.5, size=20, margin=margin(0,0,8,0)),
    plot.margin = unit(c(.5,.25,.5,.5), "cm"),
    plot.subtitle = element_text(color="yellow",size=20),
    axis.text.x = element_text(color="yellow", size=18),
    axis.text.y = element_text(color="yellow", size=15),
    legend.text= element_text(color="yellow", size=12),
    legend.title= element_text(color="yellow", size=9))


#para agregar imagen de fondo estrellado
img <- "https://images.unsplash.com/photo-1520034475321-cbe63696469a?ixlib=rb-1.2.1&w=1000&q=80"
library(ggimage)
ggbackground(a, img)#add background img to plot
library(here)
ggsave(here("astronautas.png"), height = 8, width = 10)


#con tema oscuro
library(ggdark)

astronauts %>%
  count(nationality,sex, sort=TRUE)%>%
  rename(total=n)%>%
  filter(total>4)%>%
  mutate(nationality = nationality %>% fct_infreq() %>% fct_rev()) %>%
  ggplot(aes(nationality, y=total, fill=sex)) +
  geom_col()+
  coord_flip()+
  labs(title= "Number of Astronauts by Country, 1961-2019",
       subtitle= "TidyTuesday 14-07-2020")+
  theme_void()+
  dark_theme_gray()+
  theme(
    plot.title = element_text(color="yellow",hjust = 0.5, size=20, margin=margin(0,0,8,0)),
    plot.margin = unit(c(.5,.25,.5,.5), "cm"),
    plot.subtitle = element_text(color="yellow",size=20),
    axis.text.x = element_text(color="yellow", size=10),
    axis.text.y = element_text(color="yellow", size=9),
    legend.text= element_text(color="yellow", size=9),
    legend.title= element_text(color="yellow", size=9))
