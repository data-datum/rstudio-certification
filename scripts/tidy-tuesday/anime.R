#ANIME DATASET
tidy_anime <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-23/tidy_anime.csv")
library(tidyverse)
library(ggdark)
glimpse(tidy_anime)

str(tidy_anime)

head(tidy_anime)

tidy_anime %>%
  ggplot(aes(x=type, fill=type))+
  geom_bar()

tidy_anime %>%
  count(type) %>%
  mutate(Position = fct_reorder(type, n, .desc = TRUE)) %>%
  ggplot(aes(x = type, y = n)) + geom_bar(stat = 'identity')

tidy_anime %>%
  ggplot(aes(x=fct_reorder(type)), fill=type)+
  geom_bar()

tidy_anime %>%
  mutate(type = type %>% fct_infreq() %>% fct_rev()) %>%
  ggplot(aes(type, fill=type)) +
  geom_bar()+
  coord_flip()
#medio de promocion (TV, ovas, etc)
tidy_anime %>%
  mutate(type = type %>% fct_infreq() %>% fct_rev()) %>%
  ggplot(aes(type, fill=type)) +
  geom_bar()+
  coord_flip()
#genero

tidy_anime %>%
    


tidy_anime %>%
  mutate(genre = genre %>% fct_infreq() %>% fct_rev()) %>%
  ggplot(aes(genre, fill=genre)) +
  geom_bar(show.legend = F)+
  coord_flip()+
  labs(title = 'Género de anime\n', x='',y='',fill= ' ',
         caption = "Fuente: TidyTuesday")+
  dark_theme_gray()

ggsave(here("anime-genero.png"), height = 8, width = 10, units = "in", type='cairo')
  


#rating
tidy_anime %>%
  
  count(rating)
#grafico
tidy_anime %>%
  mutate(rating = rating %>% fct_infreq() %>% fct_rev()) %>%
  ggplot(aes(rating, fill=rating)) +
  geom_bar()+
  coord_flip()


tidy_anime %>%
  ggplot(aes(x=status))+
  geom_bar()+
  dark_theme_gray(base_family = "Fira Sans Condensed Light", base_size = 14) + 
  theme(plot.title = element_text(family = "Fira Sans Condensed"),
        plot.background = element_rect(fill = "grey10"),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey30", size = 0.2),
        panel.grid.minor = element_line(color = "grey30", size = 0.2),
        legend.background = element_blank(),
        axis.ticks = element_blank(),
        legend.key = element_blank(),
        legend.position = c(0.815, 0.27))
