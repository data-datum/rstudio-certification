#R primers ggplot
library(tidyverse)
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, fill = cut))

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, fill = cut))


ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, fill = cut), width = 1)

ggplot(data=mpg)+ geom_bar(aes(x=fct_infreq(class), fill=class, alpha=0.5))


ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, fill = clarity))

#para ver en forma de histograma
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "dodge")

#stacked bars
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "stack")

#barras apiladas
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "fill")

#posicion identidad
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "identity")

## e