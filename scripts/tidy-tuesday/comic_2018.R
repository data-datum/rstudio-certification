#comic book characters
library(tidyverse)
comic <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018/2018-05-29/week9_comic_characters.csv")

comic %>%
  count(publisher)

comic %>%
  filter(!is.na(align))%>%
  ggplot(aes(x=publisher, fill=align))+
  geom_bar()+
  facet_wrap(~sex)

https://fivethirtyeight.com/features/women-in-comic-books/
#voy a reproducir los graficos que hay ahi
  
#grafico 1
#grafico de barras con personajes introducidos por año
  
comic %>%
  filter(!is.na(year))%>%
  group_by(year)%>%
  ggplot(aes(x=year))+
  geom_bar()+
  facet_wrap(~publisher)+
  theme_minimal()+
  labs(title="New Comic Book Characters Introduced Per Year",
       y="",
       x="")

#grafico 2
#gender ratio a lo largo de los años

comic %>%
  filter(`NA`  %in% c(year, sex))%>%
  group_by(year, sex)%>%
  summarize()
  ggplot(aes(x=year, y=))+
  geom_bar()
  
sex_year <- comic %>%
  filter(NA %in% c(year, sex))%>%
  count(year, sex) %>%
  pivot_wider(names_from=sex, values_from=n)

library(janitor)
percent_fem <-sex_year %>%
  clean_names()%>%
  select(year, male_characters, female_characters)%>%
  #mutate(case_when(fema))
  mutate(total= male_characters + female_characters)%>%
  mutate(percent_fem= female_characters/total)%>%
  drop_na()



sex_year_publisher <- 
comic %>%
  select(year, sex, publisher) %>%
  drop_na()%>%
  count(year, sex, publisher) %>%
  pivot_wider(names_from=sex, values_from=n)%>%
  clean_names()%>%
  select(year, publisher, male_characters, female_characters)%>%
  replace_na(list(female_characters=0))%>%
  mutate(total=male_characters + female_characters)%>%
  mutate(percent_fem=female_characters/total)%>%
  ggplot(aes(x=year, y=percent_fem, color=publisher))+
  geom_smooth()+
  geom_line(aes(y = Marvel), color="red")
  
sex_year_publisher


comic %>%
  select(year, sex, publisher) %>%
  drop_na()%>%
  count(year, sex, publisher) %>%
  pivot_wider(names_from=sex, values_from=n)%>%
  pivot_wider(names_from=publisher, values_from=n)%>%
  clean_names()%>%
  select(year, publisher, male_characters, female_characters)%>%
  

#grafico 4 barras apiladas
#characters alignment
  
comic %>%
  select(publisher, sex, align)%>%
  drop_na()%>%
  group_by(align, sex)

comic %>%
  count(publisher, sex, align)
comic %>%
  count(align)


comic %>%
  count(publisher, sex, align)%>%
  drop_na()


comic %>%
  filter(sex %in% c("Male Characters", "Female Characters"))%>%
  count(publisher, sex, align)%>%
  drop_na()%>%
  ggplot(aes(x=publisher, ))




