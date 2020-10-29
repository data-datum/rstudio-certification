# modelo de examen

library(tidyverse)

health <- read_csv('at_health_facilities.csv')

health <- health %>%
  clean_names()
  
colnames(health)

str(health)
### How many countries reported data?
health %>%
  distinct(iso3)%>%
  count()

health %>%
  count(iso3)

health%>%
  group_by(iso3)%>%
  summarize(n=n())

health %>%
  group_by(iso3)%>%
  tally()

#otra forma un poco mas rebuscada y con rbase
library(magrittr)
health %$%
  length(unique(iso3))

### What is the difference between the minimum and maximum year with valid data for each country?

health%>%
  mutate(age_20_34 = as.double(age_20_34))%>%
  drop_na()%>%
  group_by(iso3)%>%
  mutate(difference = max(year) - min(year))

#solucion de patri

health %>%
  filter()

### How many countries reported data in 3 or more years?

health %>%
  count(iso3, sort=TRUE)%>%
  filter(n>=3)%>%
  view()

### Which countries reported 100% incidence for at least one year in either age group?
library(janitor)
health<-health %>%
  clean_names()

health %>%
  filter(age_15_17==100 | age_20_34 == 100) %>%
  count(iso3)


health %>%
  filter(age_15_17==100 | age_20_34 == 100) %>%
  group_by(iso3)




### ejercicio 2


file <- "infant_hiv.csv"

tidy_data <- function(file){
  
  # Read data.
  raw_data <- read_csv(file) 
  
  # Tidy data by splitting the columns using a regex expression.
  tidy <- raw_data %>%
    pivot_longer(-ISO3,
                 names_to = c("year", "stats"),
                 names_pattern = "(.*) (.*)") %>%
    mutate(value = case_when(
      value == "-" | value == ">95%" ~ NA_character_,
      TRUE ~ str_replace(value, pattern = "%", replacement = "")
    ))
  
  # Return tidy data.
  tidy
}

tidy_data(file)%>%view()