library(tidyverse)
library(datos)

##### I. Cómo se interpretan cadenas y patrones -----

comillas <- "esto es una comilla \""
writeLines(comillas)

# ¿Y si quiero representar una barra invertida?
# Por ejemplo, una barra invertida \, otra barra invertida \

x <- "una barra invertida \\, otra barra invertida \\"
writeLines(x)

# ¿U otro caracter especial?

punto <- "esto es un punto: ."

str_count(punto, ".")
str_length(punto)

# Lo que yo quiero que el computador interprete es: \. Entonces, tengo que escribir:

str_count(punto, "\\.")

y <-  "esta frase tiene 5 palabras; esta tiene 4 palabras"

# La expresión regular para dígitos es: \d. Por lo que tengo que escribir:

str_count(y, "\\d")

z <- "una barra invertida: /"

# Lo que quiero que interprete es \\. Entonces, tengo que escapar las dos barras. Es decir tengo que escribir CUATRO!

str_replace(z, "/", "\\\\") %>% 
  writeLines()


#### 2. Usos de patrones y expresiones regulares en funciones del paquete stringr -----


### Son útiles para filtrar cuando no recuerdo exactamente cómo está escrita la palabra que busco: (== supone una coincidencia exacta)

paises %>% 
  filter(str_detect(pais, "Per"))

paises %>% 
  filter(str_detect(pais, "^P"))

paises %>% 
  filter(str_detect(pais, "Corea(.+)Dem")) 

## ¡Sirven mucho para ordenar datos!

colores <- read_csv2("colores_favoritos.csv")

colores %>% 
  count(ciudad)

colores <- colores %>% 
  mutate(ciudad = str_replace(ciudad, "(V|v)alparaiso*", "Valparaíso"))

colores <- colores %>% 
  mutate(region = str_replace(region, "(V) (Región)", "\\2 \\1"))

colores <-colores %>% 
  mutate(codigo_pais = 56) %>% 
  mutate(numero_movil = str_extract(telefono_movil, "\\d{9}$")) %>% 
  select(-telefono_movil)

colores <- colores %>% 
  separate_rows(colores_favoritos, sep = "\\s*[:punct:]+\\s*") %>% 
  mutate(colores_favoritos = str_to_lower(colores_favoritos))

head(colores)

colores %>% 
  count(colores_favoritos) %>% 
  arrange(desc(n))

## También para hacer joins & pivotar datos

escritura <- read_csv2("escritura.csv")
escritura

matematicas <- read_csv2("matematicas.csv")
matematicas

left_join(escritura, matematicas)
full_join(escritura, matematicas)

matematicas %>% 
  filter(str_detect(nombre, "\\w+\\s\\w+"))

puntajes <- matematicas %>% 
  mutate(nombre = str_replace(nombre, "(\\w+)\\s(\\w+)", "\\2, \\1")) %>% 
  full_join(escritura)

head(puntajes)

glimpse(puntajes)

puntajes <- puntajes %>% 
  pivot_longer(starts_with("puntaje"), names_to = "prueba", values_to = "puntaje", names_pattern = "puntaje_(.+)")


# Agreguemos más datos

otros_puntajes <- read_csv2("otros_puntajes.csv")

otros_puntajes <- otros_puntajes %>% 
  pivot_longer(starts_with("puntaje"), names_to = "prueba", values_to = "puntaje", names_pattern = "puntaje_(.+)")


puntajes <- bind_rows(puntajes, otros_puntajes)
View(puntajes)

puntajes %>% 
  filter(str_detect(nombre, "Stark\\b"))

#### Otro ejemplo: seleccionar columnas de acuerdo a un patrón

camarones <- read_csv2("camarones.csv")
glimpse(camarones)

camarones %>% 
  select(comuna, starts_with("e"))

camarones %>% 
  select(comuna, matches("e\\d+"))


### Último ejemplo: limpiar un pdf

library(pdftools)

pinera <-  pdf_text("pinera2018.pdf")
pinera
str(pinera)

# juntar todo en una cadena de caracteres

pinera <- str_c(pinera, collapse = "")

str(pinera)

# chequear el encabezado: está escrito de dos formas
str_count(pinera, "Presidencia de la\n República de Chile")
str_count(pinera, "Presidencia de la\n  República de Chile")

str_count(pinera, "Presidencia de la\n[:blank:]+República de Chile")

# Ahora que tenemos el patrón que identifica ambas formas, eliminamos el encabezado

pinera <- str_remove_all(pinera, "Presidencia de la\n[:blank:]+República de Chile[:blank:]")

# ahora: borrar los número de página
# primero, identificamos el patrón:

str_count(pinera, "\n[:blank:]+[:digit:]+")

# Lo eliminamos
pinera <- str_replace_all(pinera, "\n[:blank:]+[:digit:]+", " ")

# Eliminamos espacios extras:
pinera <- str_replace_all(pinera, "[:blank:]{2,}", " ")


# ¡Listo! Ahora guardamos nuestro archivo como txt.
write_lines(pinera, "pinera2018.txt")
