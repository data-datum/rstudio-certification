#cocktails
#tidy tuesday 26-05-2020

cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/cocktails.csv')
boston_cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/boston_cocktails.csv')
#hubo problema con uno de los originales de kaggle, por ende baje nomas la data limpia
library(tidyverse)
glimpse(cocktails)

cocktails %>%
  count(alcoholic) 

cocktails %>%
  count(category)

cocktails <- cocktails %>% 
  mutate(alcoholic = str_replace(alcoholic, "Non (A|a)lcoholic", "Non alcoholic"))
library(RColorBrewer)
library(ggthemes)

#grafico de barras por categoria y ver si tienen o no alcohol
cocktails %>%
  mutate(category = category %>% fct_infreq() %>% fct_rev()) %>%
  ggplot(aes(category, fill=alcoholic))+
  geom_bar()+  
  coord_flip()

cocktails %>%
  count(ingredient)

cocktails <- cocktails %>%
  mutate(glass = str_replace(glass, "Champagne (F|f)lute", "Champagne flute"))%>%
  mutate(glass = str_replace(glass, "Cocktail (G|g)lass", "Cocktail glass"))%>%
  mutate(glass = str_replace(glass, "Coffee (M|m)ug", "Coffee mug"))%>%
  mutate(glass = str_replace(glass, "Collins (G|g)lass", "Collins glass"))%>%
  mutate(glass = str_replace(glass, "Old-(F|f)ashioned glass", "Old-fashioned glass"))
                             
cocktails %>%
  count(glass)
                          
cocktails%>%
  mutate(glass = glass %>% fct_infreq() %>% fct_rev()) %>%
  ggplot(aes(x=glass,fill=alcoholic))+
  geom_bar()+
  coord_flip()

cocktails %>%
  mutate(glass = glass %>% fct_infreq() %>% fct_rev()) %>%
  ggplot(aes(x=glass, fill=category))+
  geom_bar()+
  coord_flip()+
  labs(title="¿En qué vaso se sirve cada bebida?")

boston %>%
  count(ingredient, sort=TRUE)%>%
  head(20)%>%
  mutate(ingredient=fct_reorder(ingredient, n))%>%
  ggplot(aes(n, ingredient))+
  geom_col()+
  labs(title="Ingredientes más comunes en tragos de Boston")

boston%>%
  distinct(name, category)%>%
  count(category, sort=TRUE)

#codigo tidy tuesday de david robinson youtube screencast
boston %>%
  count(category, ingredient, sort=TRUE) %>%
  mutate(category = fct_lump(category, 4),
         ingredient = fct_lump(ingredient, 20)) %>%
  filter(ingredient != "Other") %>%
  mutate(ingredient = fct_reorder(ingredient, n, sum)) %>%
  ggplot(aes(n, ingredient, fill=category))+
  geom_col()+
  labs(title="Most common ingredients in Boston Recipes",
       x=" # of ingredients",
       y= "Ingredients",
       fill="Category")
  

