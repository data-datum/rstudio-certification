#palmer penguins
#tidy tuesday

penguins.csv <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-28/penguins.csv')

library(tidyverse)
library(ggthemes)
library(RColorBrewer)
#scatterplot de palmer penguins
penguins.csv %>%
  ggplot(aes(x=bill_length_mm, y=bill_depth_mm, colour=species))+
  geom_point()+  
  scale_colour_brewer(palette = "Set1")+
  labs(title="Palmer Penguins dataset",
       subtitle="TidyTuesday 28-07-2020",
       caption="@data_datum")+
  xlab("Bill length (mm)")+
  ylab("Bill depth (mm)")
  theme_fivethirtyeight() + 
  theme(legend.position='right',legend.direction='vertical')

penguins.csv%>%
  mutate(bill_ratio=bill_length_mm/bill_depth_mm)%>%
  ggplot(aes(x=bill_ratio, fill=species))+
  geom_histogram()+
  facet_grid(~species)

  