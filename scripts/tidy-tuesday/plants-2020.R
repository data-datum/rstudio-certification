#plants in danger
#tidy tuesday 18-08-2020
plants_wide <- read_csv("https://raw.githubusercontent.com/Z3tt/TidyTuesday/master/data/raw_plants/plants_extinct_wide.csv")

plants <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/plants.csv')
actions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/actions.csv')
threats <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/threats.csv')
glimpse(plants)
glimpse(actions)
glimpse(threats)

library(tidytext)
library(tidyverse)
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
#paises que presentan mayor amenaza para especies de plantas
plants %>%
  count(country=fct_lump(country,20), sort=TRUE)%>%
  filter(country!="Other")%>%
  mutate(country=fct_reorder(country, n))%>%
  ggplot(aes(n, country))+
  geom_col()+
  labs(title="Países con mayor amenaza para especies de plantas",
       subtitle="tidytuesday 18-08-2020",
       caption="@data-datum")

#tipos de amenazas segun continente
threats %>%
  count(continent, sort=TRUE)
threats %>%
  ggplot(aes(threat_type, fill=red_list_category))+
  geom_bar()+
  facet_wrap(~continent)
#tipos de amenazas segun continente
threats %>%
  filter(threatened==1)%>%
  count(threat_type, continent, sort=TRUE)%>%
  mutate(threat_type=fct_reorder(threat_type, n))%>%
  ggplot(aes(x=n, y=threat_type, fill=continent))+
  geom_col()
#amenaza en cada continente
#codigo de david silver tidytuesday screencast
by_continent_threat<-threats %>%
  filter(threatened==1)%>%
  count(threat_type, continent, sort=TRUE)%>%
  mutate(threat_type=fct_reorder(threat_type, n, sum))

by_continent_threat %>%
  ggplot(aes(x=n, y=threat_type))+
  geom_col()+
  facet_wrap(~continent, scales="free")

by_continent_threat %>%
  mutate(threat_type=reorder_within(threat_type, n, continent))%>%
  ggplot(aes(x=n, y=threat_type))+
  geom_col()+
  facet_wrap(~continent, scales="free")+
  labs(title="The Most Common Threats",
       x="# of plants facing this threat",
       y="", 
       fill="Continent")





--------------------#notas--------------------------------------------
#no olvidar la variable q se va a adicionar en count(), MUY IMPORTANTE
#fct_reorder se utiliza en mutate(), no en ggplot()
#libreria tidytext tiene la function reorder_within()
#scales=free en el facetado permite una mejor visualizacion
#




