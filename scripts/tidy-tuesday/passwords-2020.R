#passwords
passwords <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-14/passwords.csv')

library(tidyverse)
glimpse(passwords)

passwords%>%
  count(category, sort=TRUE)

#eliminar los NA que son filas de más sin datos
passwords_clean <-passwords %>%
  filter(!is.na(rank))

#cada categoria de contraseñas, en que categoria de tiempo para descubrirlas
passwords_clean %>%
  ggplot(aes(x=category, fill=time_unit))+
  geom_bar()

passwords_clean %>%
  ggplot(aes(x=value))+
  geom_boxplot()+
  facet_wrap(~time_unit)


