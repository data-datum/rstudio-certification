#nobel_prizes

nobel_winners_raw <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-14/nobel_winners.csv")
nobel_winner_all_pubs <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-14/nobel_winner_all_pubs.csv")

library(tidyverse)
library(lubridate)
theme_set(theme_light())

nobel_winners <- distinct(full_name, prize_year, category, .keep_all = TRUE) %>%
  mutate(decade = 10 * (prize_year %/% 10),
         age = prize_year - year(birth_date))

nobel_winners <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-14/nobel_winners.csv") %>%
  distinct(full_name, prize_year, category, .keep_all = TRUE) %>%
  mutate(decade = 10 * (prize_year %/% 10),
         age = prize_year - year(birth_date))

nobel_winner_all_pubs <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-14/nobel_winner_all_pubs.csv") %>%
  mutate(prize_decade = 10 * (prize_year %/% 10))



nobel_winners %>%
  group_by(category, decade) %>%
  summarize(winners = n(),
            winners_per_year = winners / n_distinct(prize_year)) %>%
  ggplot(aes(decade, winners_per_year, color = category)) +
  geom_line() +
  expand_limits(y = 0)

nobel_winners %>%
  distinct(full_name, prize_year, category) %>%
  group_by(full_name) %>%
  mutate(prizes = n(),
         distinct_prizes = n_distinct(category)) %>%
  arrange(desc(prizes), full_name)

nobel_winners %>%
  count(decade,
        category,
        gender = coalesce(gender, laureate_type)) %>%
  group_by(decade, category) %>%
  mutate(percent = n / sum(n)) %>%
  ggplot(aes(decade, n, fill = gender)) +
  geom_col() +
  facet_wrap(~ category) +
  labs(x = "Decade",
       y = "# of nobel prize winners",
       fill = "Gender",
       title = "Nobel Prize gender distribution over time")


nobel_winners %>%
  mutate(category = fct_reorder(category, age, median, na.rm = TRUE)) %>%
  ggplot(aes(category, age)) +
  geom_boxplot() +
  coord_flip()

nobel_winners %>%
  filter(!is.na(age)) %>%
  group_by(decade, category) %>%
  summarize(average_age = mean(age),
            median_age = median(age)) %>%
  ggplot(aes(decade, average_age, color = category)) +
  geom_line()
nobel_winners %>%
  filter(prize_year >= 2010, category == "Peace") %>%
  select(full_name, age, prize)


nobel_winners %>%
  filter(!is.na(birth_country)) %>%
  count(birth_country = fct_lump(birth_country, 9),
        category,
        sort = TRUE) %>%
  mutate(birth_country = fct_reorder(birth_country, n)) %>%
  ggplot(aes(birth_country, n, fill = category)) +
  geom_col() +
  facet_wrap(~ category) +
  coord_flip()

library(WDI)
library(countrycode)
indicators_raw <- WDI(indicator = "NY.GDP.PCAP.CD",
                      start = 2016, end = 2016, extra = TRUE) %>%
  tbl_df() %>%
  select(country,
         country_code = iso2c,
         income,
         gdp_per_capita = NY.GDP.PCAP.CD)

nobel_winners_countries <- nobel_winners %>%
  mutate(country_code = countrycode(birth_country, "country.name", "iso2c")) %>%
  inner_join(indicators_raw, by = "country_code") %>%
  mutate(income = fct_relevel(income, c("Low income", "Lower middle income", "Upper middle income", "High income")))
nobel_winners_countries %>%
  filter(!is.na(income)) %>%
  count(category, income) %>%
  group_by(category) %>%
  mutate(percent = n / sum(n)) %>%
  ggplot(aes(income, percent)) +
  geom_col() +
  facet_wrap(~ category) +
  coord_flip() +
  labs(x = "Current income level of birth country",
       y = "% of this category's prizes",
       title = "Where do Nobel Prize winners come from?")


winners_summarized <- nobel_winner_all_pubs %>%
  filter(pub_year <= prize_year) %>%
  group_by(laureate_id,
           laureate_name,
           category,
           prize_year,
           prize_decade) %>%
  summarize(papers_before_prize = n(),
            papers_before_prize_5_years = sum(pub_year >= prize_year - 5),
            average_paper_age = mean(prize_year - pub_year),
            winning_paper_age = mean((prize_year - pub_year)[is_prize_winning_paper == "YES"]))

winners_summarized %>%
  group_by(category, prize_decade) %>%
  summarize(average_papers = mean(papers_before_prize),
            average_paper_age = mean(average_paper_age),
            average_winning_paper_age = mean(winning_paper_age)) %>%
  ggplot(aes(prize_decade, average_winning_paper_age, color = category)) +
  geom_line() +
  labs(x = "Prize decade",
       y = "Time between when paper was published and won prize",
       title = "Scientists have to wait longer for a Nobel Prize than ever",
       color = "Category") +
  expand_limits(y = 0)

pubs_enriched <- nobel_winner_all_pubs %>%
  group_by(laureate_id, category, prize_year) %>%
  mutate(papers_before = rank(pub_year, ties.method = "first") - 1,
         total_papers = n(),
         position_in_career =  papers_before / total_papers,
         first_pub_year = min(pub_year)) %>%
  ungroup()


nobel_winners %>%
  filter(!is.na(age),
         category %in% c("Chemistry", "Medicine", "Physics")) %>%
  group_by(decade, category) %>%
  summarize(average_age = mean(age),
            median_age = median(age)) %>%
  ggplot(aes(decade, average_age, color = category)) +
  geom_line()


pubs_enriched %>%
  filter(is_prize_winning_paper == "YES") %>%
  group_by(prize_decade, category) %>%
  summarize(average_position_in_career = mean(position_in_career)) %>%
  ggplot(aes(prize_decade, average_position_in_career, color = category)) +
  geom_line() +
  scale_y_continuous(labels = scales::percent_format())

pubs_enriched %>%
  filter(pub_year - first_pub_year < 75,
         prize_year >= 1910,
         prize_year <= 2000) %>%
  ggplot(aes(pub_year - first_pub_year, fill = is_prize_winning_paper)) +
  geom_density(alpha = .5) +
  facet_wrap(~ category) +
  labs(title = "Typical arc of a Nobel Prize winner's career",
       subtitle = "For people who won between 1910 and 2000",
       x = "Years into their publishing career")

