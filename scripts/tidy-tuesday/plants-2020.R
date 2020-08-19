#plants in danger
#tidy tuesday 18-08-2020
plants_wide <- read_csv("https://raw.githubusercontent.com/Z3tt/TidyTuesday/master/data/raw_plants/plants_extinct_wide.csv")

plants <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/plants.csv')
actions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/actions.csv')
threats <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/threats.csv')
glimpse(plants)
glimpse(actions)
glimpse(threats)

#plantas extintas por continente
plants %>%
  ggplot(aes(x=red_list_category, fill=group))+
  geom_bar()+
  facet_wrap(~continent)+
  labs(title="Plantas extintas por continente",
       subtitle="tidytuesday 18-08-2020",
       caption="@data-datum")+
  xlab("Categorías de especies")+
  ylab("# de especies extintas")

plants %>%
  count(group, sort=TRUE)

plants %>%
  count(country=fct_lump(country,20), sort=TRUE)%>%
  filter(country!="Other")%>%
  mutate(country=fct_reorder(country, n))%>%
  ggplot(aes(n, country))+
  geom_col()+
  labs(title="Países con mayor amenaza para especies de plantas",
       subtitle="tidytuesday 18-08-2020",
       caption="@data-datum")

plants%>%
  count(fct_lump(country, 15), sort=TRUE)

