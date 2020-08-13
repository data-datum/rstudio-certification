#xmen dataset
#tidy tuesday

characters <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-30/characters.csv')

comic_bechdel <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-30/comic_bechdel.csv')
xmen_bechdel <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-30/xmen_bechdel.csv')


library(tidyverse)
comic_bechdel %>%
  count(series) %>%
  ggplot(aes(x=series, fill=series))+
  geom_histogram(stat="count")

ggplot(comic_bechdel, aes(x=series, fill=series))+
  geom_bar()
#cuantos pasaron el test por grupo de comic
pass_bechdel <-comic_bechdel %>%
  count(series, pass_bechdel) 
pass_bechdel_df<-as.data.frame(pass_bechdel)
ggplot(pass_bechdel_df, aes(x=series, y=n, fill=pass_bechdel))+
  geom_bar(stat="identity")

#xmen bechdel test
xmen_bechdel %>%
  count(pass_bechdel)


#xmen characters appearance
character_visualization <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-30/character_visualization.csv')

char <- character_visualization %>%
  count(character)

character_visualization %>%
  #mutate(character=character %>%  fct_infreq() %>% fct_rev()) %>%
  ggplot(aes(character, fill=character)) +
  geom_bar(show.legend = F)+
  coord_flip()


characters_pivoted <- character_visualization %>%
  pivot_longer(cols=speech:depicted,
               names_to="metric",
               values_to="value") 

ggplot(characters_pivoted, aes(x=character,y=value, fill=metric))+
  geom_bar(stat="identity")
  

characters <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-30/characters.csv')