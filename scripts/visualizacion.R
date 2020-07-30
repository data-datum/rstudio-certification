#visualizacion con ggplot2

library(datos)
datos::millas
library(tidyverse)
#parametros en el geom_point
ggplot(data=millas)+
  geom_point(mapping=aes(x=cilindrada, y=autopista))
#agrego una tercer variable para las clases
ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista, color = clase))
#en vez de color separo por tamaño
ggplot(data=millas)+
  geom_point(aes(x=cilindrada, y=autopista, size=clase))

#izquierda
ggplot(data=millas)+
  geom_point(mapping=aes(x=cilindrada, y=autopista, alpha=clase))

#derecha
ggplot(data=millas)+
  geom_point(mapping=aes(x=cilindrada, y=autopista, shape=clase))


ggplot(data=millas)+
  geom_point(mapping=aes(x=cilindrada, y=autopista), color="blue")#color azul
#las variables esteticas solo toman valores x e y

ggplot(data=millas)+
  geom_point(mapping=aes(x=cilindrada, y=autopista, color="blue"))#color salmon

ggplot(data=millas)+aes(x=cilindrada, y=autopista, color="blue")
geom_point()

ggplot(data=millas)+
  geom_point(aes(x=cilindrada, y=autopista), color=modelo)

#facetado
ggplot(data=millas)+
  geom_point(mapping=aes(x=cilindrada, y=autopista))+
  facet_wrap(~clase, nrow=2) #para variables categoricas


ggplot(data=millas)+
  geom_point(mapping=aes(x=cilindrada, y=autopista))+
  facet_grid(traccion~cilindros) #para variables continuas

ggplot(data=millas)+
  geom_point(mapping=aes(x=cilindrada, y=autopista))+
  facet_grid(.~cilindros) #solo teniendo en cuenta cilindros

ggplot(data = millas) +
  geom_point(mapping = aes(x = traccion, y = cilindros))


ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista)) +
  facet_grid(traccion ~ .)

ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista)) +
  facet_grid(. ~ cilindros)

#ventajas del facetado
#permite visualizar todas las posibilidades de variables categóricas en diferentes gráficos
#esto permite una visualización más directa.

ggplot(data = millas) +
  geom_smooth(mapping = aes(x = cilindrada, y = autopista))


ggplot(data=millas)+
  geom_smooth(mapping=aes(x=cilindrada, y=autopista, linetype=traccion))

ggplot(data=millas)+
  geom_point(mapping=aes(x=cilindrada, y=autopista, color=traccion))+
  geom_smooth(mapping=aes(x=cilindrada, y=autopista, color=traccion))

ggplot(data = millas) +
  geom_smooth(mapping = aes(x = cilindrada, y = autopista))

ggplot(data = millas) +
  geom_smooth(mapping = aes(x = cilindrada, y = autopista, group = traccion))

ggplot(data = millas) +
  geom_smooth(
    mapping = aes(x = cilindrada, y = autopista, color = traccion),
    show.legend = FALSE
  )

ggplot(data = millas) + #datos
  geom_point(mapping = aes(x = cilindrada, y = autopista)) + #geom puntos 
  geom_smooth(mapping = aes(x = cilindrada, y = autopista)) #geom funcion
#vamos a simplificar código
ggplot(data=millas, mapping=aes(x=cilindrada, y=autopista))+
  geom_point()+
  geom_smooth()
#segunda simplificacion
ggplot(millas, aes(cilindrada, autopista))+
  geom_point()+
  geom_smooth()


ggplot(millas, aes(cilindrada, autopista, color=traccion))+ #
  geom_point()+ #scatterplot
  geom_smooth(se=FALSE) #FALSE borra los intervalos de confianza

#show.legend = es para mostrar legenda
#se en geom_smooth es para ver el intervalo de confianza

ggplot(data = millas, mapping = aes(x = cilindrada, y = autopista)) +
  geom_point() +
  geom_smooth()
#se ven igual ambos
ggplot() +
  geom_point(data = millas, mapping = aes(x = cilindrada, y = autopista)) +
  geom_smooth(data = millas, mapping = aes(x = cilindrada, y = autopista))
#se ven igual ambos pero hay una duplicacion de codigo

#recrear el codigo
#grafico 1
ggplot(millas, aes(cilindrada, autopista))+
  geom_point()+
  geom_smooth()
#grafico 2
ggplot(millas, aes(cilindrada, autopista, group=traccion))+
  geom_point()+
  geom_smooth(se=FALSE)
#grafico 3
ggplot(millas, aes(cilindrada, autopista, color=traccion))+
  geom_point()+
  geom_smooth(se=FALSE)
#grafico 4
ggplot(millas, aes(cilindrada, autopista, color=traccion))+
  geom_point()+
  geom_smooth()
#grafico 5
ggplot(millas, aes(cilindrada, autopista))+
  geom_point(aes(color=traccion))+
  geom_smooth(se=FALSE)
#grafico 6
ggplot(millas, aes(cilindrada, autopista))+
  geom_point(aes(color=traccion))+
  geom_smooth(aes(linetype=traccion), se=FALSE)
#grafico 7
ggplot(millas, aes(cilindrada, autopista))+
  geom_point(size=4, color="white")+
  geom_point(aes(color=traccion))

#graficos de barras
ggplot(data = diamantes) +
  geom_bar(mapping = aes(x = corte))

#en geom_bar() esta implicito el stat_count()
#lo mismo escrito de otra forma
ggplot(diamantes)+
  stat_count(mapping=aes(x=corte))


#transformaciones estadisticas

  
  