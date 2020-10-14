#more exams
#1. basic operations----------------------------------------------------------

library(tidyverse)
#1
person<-read_csv("scripts/data/person.csv")
#2
person2<-person %>%
  select(family_name, personal_name)
#3
person2 %>%
  filter(family_name < "M")
#4
person %>%
  arrange(-str_length(family_name))

#2. cleaning and counting---------------------------------------------------
#1
measurements <-read_csv("scripts/data/measurements.csv")
measurements_cleaned <-measurements %>%
  drop_na() 
#drop_na() es una función de tidyr
#2 
measurements_cleaned %>%
  count(quantity)

#3
measurements_cleaned %>%
  group_by(quantity)%>%
  summarize(min=min(reading), max=max(reading))

#4
measurements_cleaned %>%
  pivot_wider(names_from="quantity",
             values_from="reading")%>%
  mutate(case_when(sal > 1 ~ sal/100,
                   TRUE ~ sal))

#otra forma
measurements_cleaned %>%
  mutate(reading = case_when(
    quantity == "sal" & reading > 1 ~ reading / 100,
    TRUE ~ as.double(reading)))

cleaned<-measurements_cleaned
#3. combining data----------------------------------------------------------
library(tidyr)
visited<-read_csv("scripts/data/visited.csv")%>%
  drop_na()

joined<-inner_join(visited, cleaned, by="visit_id")
#recordar que despues de by va comillas

joined %>%
  group_by(site_id)%>%
  filter(quantity=="rad")%>%
  summarize(highest_radiation=max(reading))
  
joined %>%
  group_by(site_id)%>%
  filter(quantity=="rad")%>%
  slice_max(reading)

#4. plotting-----------------------------------------
hra_raw <- read_csv(here::here("data", "home-range-database.csv"))

library(here)
hra_raw <-read_csv("scripts/data/home-range-database.csv")

#2.
hra <- hra_raw %>%
  mutate(class_fct = factor(class, levels = c("mammalia", "reptilia", "aves", "actinopterygii")))

glimpse(hra)

hra_raw %>%
  count(class, sort=TRUE)

#3. 
hra %>%
  ggplot(aes(x=log10.mass, y=log10.hra))+
  geom_point()


#4
hra %>%
  ggplot(aes(x=log10.mass, y=log10.hra, colour=class_fct))+
  geom_point()

#5

hra %>%
  filter(class_fct == "aves")%>%
  ggplot(aes(x=log10.mass, y=log10.hra))+
  geom_point()+
  geom_smooth(method = "lm")


#5.  Functions -----------------------------------------

summarize_table <- function (title, tibble){
  num_rows <- nrow(tibble) 
  num_cols <- ncol(tibble)
  
  result <- str_c(title,"has", num_rows, 
                  "rows and", num_cols, "columns", sep = " ")
  print(result)
}

summarize_table("iris", iris)





# PROBANDO FUNCIONES------------
mean_ci <- function(x, conf = 0.95) {
  se <- sd(x) / sqrt(length(x))
  alpha <- 1 - conf
  mean(x) + se * qnorm(c(alpha / 2, 1 - alpha / 2))
}

set.seed(123)
x <- runif(100)
mean_ci(x)

