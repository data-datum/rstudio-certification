#himalayan climbers

members <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/members.csv')
expeditions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/expeditions.csv')
peaks <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/peaks.csv')

library(tidyverse)
expeditions %>%
  count(peak_name, sort=TRUE)

expeditions %>%
  count(termination_reason, sort=TRUE)

members %>%
  count(sex, sort=TRUE)
------------------------------#termination reason-------------------------------------
#borrando Unknown
expeditions %>%
  filter(!termination_reason=="Unknown")%>%
  ggplot(aes(y=termination_reason, x=highpoint_metres, colour=members))+
  geom_boxplot()

#borrar 2 categorias Other y Unknown
expeditions %>%
  filter(!termination_reason %in% c("Other", "Unknown"))%>%
  ggplot(aes(y=termination_reason, x=highpoint_metres, colour=members))+
  geom_boxplot()


#david robinson code-------------------------------------------------------
library(tidyverse)
library(tidytuesdayR)
library(scales)
theme_set(theme_light())
tt <- tt_load("2020-09-22")
peaks <- tt$peaks%>%
  rename(height_meters = height_metres)

peaks %>%
  arrange(desc(height_meters)) %>%
  head(50)%>%
  mutate(peak_name = fct_reorder(peak_name, height_meters))%>%
  ggplot(aes(height_meters, peak_name, fill=climbing_status))+
  geom_col()+
  labs(x="Height(meters)", 
       y="",
       title="Tallest peaks in the Himalayas", 
       fill="")

na_reasons <- c("Unknown", "Attempt rumoured", "Did not attempt climb", "Did not reach base camp")

expeditions <- tt$expeditions %>%
  mutate(success=case_when(str_detect(termination_reason, "Success") ~ "Success",
                           termination_reason %in% na_reasons ~ "Other",
                           TRUE ~ "Failure")) %>%
  mutate(days_to_highpoint=as.integer(highpoint_date - basecamp_date))


#importante conocer
#* Fraction of successful climbs per mountain, per year
#* Rate of death over time / per mountain (by all members or hired members)
#* Death rate by mountain and age
#* Death causes and rate of injury
#* Distribution of length of climbs vs height or vs time
#* Correlation between frequency of expeditions and death rate

expeditions %>%
  count(termination_reason, sort = TRUE)

#cuantos dias se tarda en alcazar el pico mas alto
expeditions %>%
  filter(!is.na(days_to_highpoint), !is.na(peak_name)) %>%
  filter(success == "Success") %>%
  mutate(peak_name = fct_lump(peak_name, 10),
         peak_name = fct_reorder(peak_name, days_to_highpoint)) %>%
  ggplot(aes(days_to_highpoint, peak_name)) +
  geom_boxplot() +
  labs(x = "Days from basecamp to highpoint",
       y = "",
       title = "How long does it take to get to the high point?",
       subtitle = "Successful climbs only")


summarize_expeditions <- function(tbl) {
  tbl %>%
    summarize(n_climbs = n(),
              pct_success = mean(success == "Success"),
              across(members:hired_staff_deaths, sum),
              first_climb = min(year)) %>%
    mutate(pct_death = member_deaths / members,
         pct_hired_staff_deaths = hired_staff_deaths / hired_staff)
}

peaks_summarized <- expeditions %>%
  group_by(peak_id, peak_name) %>%
  summarize_expeditions() %>%
  ungroup() %>%
  arrange(desc(n_climbs)) %>%
  inner_join(peaks %>% select(peak_id, height_meters), by = "peak_id")

#What are the deadliest mountains?
devtools::install_github("dgrtwo/ebbr")
 
library(ebbr)
peaks_eb <- peaks_summarized %>%
  filter(members >= 20) %>%
  arrange(desc(pct_death)) %>%
  add_ebb_estimate(member_deaths, members)

