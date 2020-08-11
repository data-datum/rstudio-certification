#ANIME DATASET
tidy_anime <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-23/tidy_anime.csv")

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
  mutate(genre = genre %>% fct_infreq() %>% fct_rev()) %>%
  ggplot(aes(genre, fill=genre)) +
  geom_bar()+
  coord_flip()
#rating
tidy_anime %>%
  count(rating)
#grafico
tidy_anime %>%
  mutate(rating = rating %>% fct_infreq() %>% fct_rev()) %>%
  ggplot(aes(rating, fill=rating)) +
  geom_bar()+
  coord_flip()


