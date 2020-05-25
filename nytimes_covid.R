library(tidyverse)

# https://data.cdc.gov/NCHS/Provisional-COVID-19-Death-Counts-by-Sex-Age-and-W/vsak-wrfu
cdc_data <- read_csv("data/CDC_provisional_deaths_by_age.csv")

cdc_aggregated <- cdc_data %>%
   mutate(`Age Group` = factor(`Age Group`, levels = c("Under 1 year", "1-4 years", "5-14 years", "15-24 years", "25-34 years", "35-44 years", "45-54 years", "55-64 years", "65-74 years", "75-84 years", "85 years and over"))) %>%
   group_by(`Age Group`) %>%
   summarize(deaths = sum(`COVID-19 Deaths`)) %>%
   mutate(pct = round(deaths / sum(deaths), 3),
          source = "CDC")


# https://www.nytimes.com/interactive/2020/05/24/us/us-coronavirus-deaths-100000.html
nytimes_deaths <- read_csv("data/deaths.csv")

nytimes_deaths %>%
   ggplot(aes(x = Age)) +
   geom_histogram(bins = 11)

# re-create CDC bins
nytimes_deaths_aggregated <- nytimes_deaths %>%
   mutate(`Age Group` = case_when(
      Age < 1 ~ "Under 1 year",
      Age >= 1 & Age <= 4 ~ "1-4 years",
      Age >= 5 & Age <= 14 ~ "5-14 years",
      Age >= 15 & Age <= 24 ~ "15-24 years",
      Age >= 25 & Age <= 34 ~ "25-34 years",
      Age >= 35 & Age <= 44 ~ "35-44 years",
      Age >= 45 & Age <= 54 ~ "45-54 years",
      Age >= 55 & Age <= 64 ~ "55-64 years",
      Age >= 65 & Age <= 74 ~ "65-74 years",
      Age >= 75 & Age <= 84 ~ "75-84 years",
      Age >= 85 ~ "85 years and over")) %>%
   mutate(`Age Group` = factor(`Age Group`, levels = c("Under 1 year", "1-4 years", "5-14 years", "15-24 years", "25-34 years", "35-44 years", "45-54 years", "55-64 years", "65-74 years", "75-84 years", "85 years and over"))) %>%
   group_by(`Age Group`) %>%
   summarize(deaths = n()) %>%
   mutate(pct = deaths / sum(deaths),
          source = "NYT")

dada_joined <- cdc_aggregated %>%
   inner_join(nytimes_deaths_aggregated, by = "Age Group", suffix = c(".cdc", ".nyt")) %>%
   mutate(diff = pct.nyt - pct.cdc)

all_rows <- cdc_aggregated %>%
   bind_rows(nytimes_deaths_aggregated)

all_rows %>%
   ggplot(aes(x = `Age Group`, y = pct, fill = source)) +
   geom_bar(stat = "identity", position="dodge") +
   scale_y_continuous(labels = scales::percent) +
   theme_minimal() +
   labs(caption = "Source: Centers for Disease Control, New York Times reporting\nhttps://data.cdc.gov/NCHS/Provisional-COVID-19-Death-Counts-by-Sex-Age-and-W/vsak-wrfu\nhttps://www.nytimes.com/interactive/2020/05/24/us/us-coronavirus-deaths-100000.html", y = "% of deaths attributed to COVID-19")
